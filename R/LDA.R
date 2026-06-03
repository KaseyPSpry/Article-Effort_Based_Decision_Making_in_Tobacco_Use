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
                 CV = FALSE)
# Compares predicted versus actual group labels
lda_pred <- predict(lda)

conf_matrix <- table(
  Predicted = lda_pred$class,
  Actual = df_pca_tdrl_individual_params$group
)
conf_matrix
classification_Rate <- mean(lda_pred$class == df_pca_tdrl_individual_params$group)
classification_Rate

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

ld_coeff <- as.data.frame(lda$scaling) %>%
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

############################################################################
## Saves to excel file in "Results"
############################################################################

conf_matrix_df <- data.frame(
  Predicted = rownames(conf_matrix),
  as.data.frame.matrix(conf_matrix),
  check.names = FALSE
)

openxlsx::write.xlsx(list(
    conf_matrix = conf_matrix_df,
    classification_rate = classification_Rate,
    ld_coeff = ld_coeff
  ),
  file = file.path("Results", "lda_results.xlsx")
)


# Sensitivity Analysis ----------------------------------------------------

df_pca_tdrl_individual_params_sen <- df_pca_tdrl_individual_params %>%
  left_join(
    demographic_data %>%
      select(subject_id, age, years_of_education, race, location),
    by = c("subj_id" = "subject_id")
  )

df_pca_tdrl_individual_params_sen <- df_pca_tdrl_individual_params_sen %>%
  mutate(
    race_binary = ifelse(
      race == "white",
      "white",
      "minority"
    )
  )

lda_cov <- MASS::lda(
  group ~ PC1 + PC2 + PC3 + age + years_of_education + race,
  data = df_pca_tdrl_individual_params_sen,
  CV = FALSE
)

df_pca_tdrl_individual_params_sen_resid <- df_pca_tdrl_individual_params_sen %>%
  mutate(
    PC1_resid = resid(lm(PC1 ~ age + years_of_education + race, data = .)),
    PC2_resid = resid(lm(PC2 ~ age + years_of_education + race, data = .)),
    PC3_resid = resid(lm(PC3 ~ age + years_of_education + race, data = .))
  )

lda_resid <- MASS::lda(
  group ~ PC1_resid + PC2_resid  + PC3_resid,
  data = df_pca_tdrl_individual_params_sen_resid,
  CV = FALSE
)

lda$svd
lda_cov$svd
lda_resid$svd

lda_scores <- predict(lda)$x
df_pca_tdrl_individual_params_sen$LD1 <- lda_scores[, "LD1"]
summary(
  lm(
    LD1 ~ group + age + years_of_education + race_binary,
    data = df_pca_tdrl_individual_params_sen
  )
)

