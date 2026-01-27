############################################################################
# Script Name: LDA.R
# Purpose: Perform PCA on TDRL posterior parameters
# Author: Kasey P. Spry
# Last Modified: December 4, 2025
############################################################################

library(MASS)

############################################################################
## Performs LDA with cross validation
############################################################################
lda <- MASS::lda(group ~ PC1 + PC2 + PC3,
                 data = df_pca_tdrl_individual_params,
                 CV = TRUE)
# Compares predicted versus actual group labels
conf_matrix <- table(Predicted = lda$class, Actual = df_pca_tdrl_individual_params$group)
conf_matrix
overall_accuracy <- mean(lda$class == df_pca_tdrl_individual_params$group)
overall_accuracy

groups <- colnames(conf_matrix)
metrics <- data.frame(Group = groups, Sensitivity = NA, Specificity = NA)

for (g in groups) {
  TP <- conf_matrix[g, g]
  FN <- sum(conf_matrix[, g]) - TP
  FP <- sum(conf_matrix[g, ]) - TP
  TN <- sum(conf_matrix) - TP - FN - FP

  metrics[metrics$Group == g, "Sensitivity"] <- TP / (TP + FN)
  metrics[metrics$Group == g, "Specificity"] <- TN / (TN + FP)
}
metrics

# Compute confusion matrix and Kappa
confusion <- caret::confusionMatrix(as.factor(lda$class), as.factor(df_pca_tdrl_individual_params$group))
conf_kappa <- confusion$overall['Kappa']


#Fits the LDA model without cross-validation
lda_model <- MASS::lda(group ~ PC1 + PC2 + PC3,
                       data = df_pca_tdrl_individual_params)
#Uses the fitted model to predict class membership and posterior probabilities for each observation
lda_pred <- predict(lda_model)

lda_df_plot <- df_pca_tdrl_individual_params
lda_df_plot$LD1 <- lda_pred$x[,1]
lda_df_plot$LD2 <- lda_pred$x[,2]
lda_df_plot$LD3 <- lda_pred$x[,3]

model_color_vec <- c(
  "Former TUD" = "#008C95",
  "Never TUD"  = "#A7A8A9",
  "Current TUD" = "#9E7E38",
  "TUD+OUD"    = "#6BA539"
)

ld_coeff <- as.data.frame(lda_model$scaling) %>%
  rownames_to_column(var = "Variable")

lda_df_plot$group <- factor(lda_df_plot$group,
                            levels = c("Never TUD", "Former TUD", "Current TUD", "TUD+OUD"))

ld_score_hist <- ggplot(lda_df_plot, aes(x = LD1, fill = group)) +
  geom_histogram(binwidth = 0.5,
                 position = "identity",
                 alpha = 0.65) +
  scale_fill_manual(values = model_color_vec) +
  theme_minimal() +
  theme(axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.position = "bottom") +
  labs(title = element_blank(),
       x = "Linear Discriminant Score", y = "Count") +
  scale_y_continuous(limits = c(0, 8))

ggsave(filename = file.path('Figures','LDA_histogram.png'),
       plot = ld_score_hist,
       width = 6, height = 6, units = "in", dpi = 400
)

ld_scores <- lda_pred$x
ld_df <- data.frame(ld_scores, group = df_pca_tdrl_individual_params$group)
group_levels <- levels(as.factor(ld_df$group))
group_colors <- setNames(model_color_vec, group_levels)
group_col <- group_colors[as.character(ld_df$group)]

png(filename = file.path("Figures", 'LDA_pairs.png'),
  width = 6,
  height = 6,
  units = "in",
  res = 400)
pairs(ld_df[, 1:ncol(ld_scores)],
      col = group_col,
      pch = 19)
dev.off()

library(pROC)
roc_summary <- data.frame(
  Class = character(),
  AUC = numeric(),
  CI_Lower = numeric(),
  CI_Upper = numeric(),
  stringsAsFactors = FALSE
)
lda_roc_df <- cbind(lda_df_plot, lda_pred$posterior)     # add posterior probabilities
classes <- colnames(lda_pred$posterior)


for (cls in classes) {
  binary_outcome <- factor(ifelse(lda_roc_df$group == cls, cls, paste0("not_", cls)))

  if (length(levels(binary_outcome)) < 2) {
    message(paste("Skipping", cls, "- only one level present"))
    next
  }

  roc_obj <- roc(binary_outcome, lda_roc_df[[cls]])
  auc_val <- auc(roc_obj)
  ci_val <- ci.auc(roc_obj)

  print(paste("Class:", cls))
  print(paste("AUC:", round(auc_val, 3)))
  print(paste("95% CI:", paste(round(ci_val, 3), collapse = " - ")))

  # Add to summary table
  roc_summary <- rbind(
    roc_summary,
    data.frame(
      Class = cls,
      AUC = as.numeric(auc_val),
      CI_Lower = as.numeric(ci_val[1]),
      CI_Upper = as.numeric(ci_val[3]),
      stringsAsFactors = FALSE
    )
  )

  auc_ci_text <- paste0(
    "AUC = ", round(auc_val, 3), "\n",
    "95% CI: (", round(ci_val[1], 3), " , ", round(ci_val[3], 3), ")"
  )

  filename <- file.path("Figures", sprintf("ROC_Curve_%s_MAP.png", cls))
  png(filename, width = 4, height = 4, units = "in", res = 600)

  plot(roc_obj, print.auc = FALSE, col = model_color_vec[cls], lwd = 2)

  usr <- par("usr")
  text(x = usr[2] + 0.1, y = usr[3] + 0.05, labels = auc_ci_text, adj = c(1, 0), cex = 1)

  dev.off()
}

############################################################################
## Saves to excel file in "Results"
############################################################################

conf_matrix_df <- data.frame(
  Predicted = rownames(conf_matrix),
  as.data.frame.matrix(conf_matrix),
  check.names = FALSE
)

overall_accuracy_df <- data.frame(Overall_Accuracy = overall_accuracy)
conf_kappa_df <- data.frame(Kappa = as.numeric(conf_kappa))

openxlsx::write.xlsx(list(
    conf_matrix = conf_matrix_df,
    overall_accuracy = overall_accuracy_df,
    metrics = metrics,
    conf_kappa = conf_kappa_df,
    ld_coeff = ld_coeff,
    roc_summary = roc_summary
  ),
  file = file.path("Results", "lda_results.xlsx")
)
