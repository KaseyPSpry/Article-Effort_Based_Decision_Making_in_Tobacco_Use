############################################################################
# Script Name: PCA.R
# Purpose: Perform PCA on TDRL posterior parameters
# Author: Kasey P. Spry
# Last Modified: December 4, 2025
############################################################################

############################################################################
## Load data
############################################################################

tar_load(pars_TDRL)
tar_load(demographic_data)

############################################################################
## Function to get participants MAPs
############################################################################

get_MAP <- function(samples) {
  dens <- density(samples)
  dens$x[which.max(dens$y)]
}
############################################################################
## Extract participant posterior parameters and calculate participants MAPs
############################################################################

params <- pars_TDRL

learnrate_individual_level_MAP <- apply(params$learnrate, 2, get_MAP)
discount_individual_level_MAP <- apply(params$discount, 2, get_MAP)
inv_temp_individual_level_MAP <- apply(params$inv_temp, 2, get_MAP)

tdrl_individual_params <- data.frame(
  learnrate = learnrate_individual_level_MAP,
  discount = discount_individual_level_MAP,
  inv_temp = inv_temp_individual_level_MAP
)

############################################################################
## PCA Analysis
############################################################################

pca_tdrl_individual_params <- prcomp(tdrl_individual_params,
                                     center = TRUE,
                                     scale. = TRUE)
summary(pca_tdrl_individual_params)
loadings <- pca_tdrl_individual_params$rotation
print(loadings[, 1:3])

# Long data frame of loading for figures
loadings_df <- as.data.frame(loadings[, 1:3])
loadings_df$Variable <- rownames(loadings_df)
loadings_df_long <- pivot_longer(loadings_df, cols = starts_with("PC"), names_to = "Component", values_to = "Loading")
loadings_df_long$Variable <- factor(loadings_df_long$Variable,
                                    levels = c("inv_temp", "discount", "learnrate")  # bottom to top
)

# Figure of PCA loadings
pca_loadings <- ggplot(loadings_df_long, aes(x = Component, y = Variable, fill = Loading)) +
  geom_tile(color = NA) +
  geom_text(aes(label = round(Loading, 2)), color = "black", size = 5) +
  scale_fill_gradient2(
    high = "#800020",       # Burgundy
    mid = "white",
    low = "#08306b",      # Navy
    midpoint = 0,
    limits = c(-1, 1),
    name = "Principal Component
    Coefficients (a.u.)"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    panel.grid = element_blank(),
    axis.text.x.top = element_text(face = "bold",
                                   size = 12),   # top axis labels
    axis.ticks.x.top = element_line(),               # top axis ticks
    axis.text.y = element_text(face = "bold",
                               size = 12),
    axis.ticks.x = element_blank(),                  # hide bottom ticks
    legend.position = "right",
    legend.text = element_text(size = 10)
  ) +
  scale_x_discrete(position = "top") +
  scale_y_discrete(labels = c(
    "learnrate" = "α",
    "discount" = "γ",
    "inv_temp" = "β"
  ))
pca_loadings

ggsave(
  filename = file.path('Figures','PCA_loadings.png'),
  plot = pca_loadings,
  width = 4.5,
  height = 4.5,
  units = "in",
  dpi = 400
)

############################################################################
## PCA Figures
############################################################################

group_map <- c(
  "smoker" = "Current TUD",
  "nonsmoker" = "Never TUD",
  "exsmoker" = "Former TUD",
  "OUD smoker" = "TUD+OUD"
)

model_color_vec <- c(
  "Former TUD" = "#008C95",
  "Never TUD"  = "#A7A8A9",
  "Current TUD" = "#9E7E38",
  "TUD+OUD"    = "#6BA539"
)

group_labels <- group_map[demographic_data$group]
subj_ids <- demographic_data$subject_id
df_pca_tdrl_individual_params <- as.data.frame(pca_tdrl_individual_params$x)
df_pca_tdrl_individual_params$group <- group_labels
df_pca_tdrl_individual_params$group <- factor(df_pca_tdrl_individual_params$group,
                                              levels = names(model_color_vec))
df_pca_tdrl_individual_params$subj_id <- subj_ids

group_levels <- levels(as.factor(df_pca_tdrl_individual_params$group))
group_colors <- setNames(model_color_vec, group_levels)
group_col <- group_colors[as.character(df_pca_tdrl_individual_params$group)]

png(filename = file.path('Figures','PCA_pairs.png'),
  width = 6,
  height = 6,
  units = "in",
  res = 400)
pairs(df_pca_tdrl_individual_params[, 1:3], col = group_col, pch = 19)
dev.off()


pca_scree_plot <- factoextra::fviz_eig(pca_tdrl_individual_params,
                                       addlabels = TRUE,
                                       ylim = c(0, 100)) +
  theme(
    title = element_blank()
  )
pca_scree_plot

ggsave(
  filename = file.path('Figures','PCA Scree Plot.png'),
  plot = pca_scree_plot,
  width = 6, height = 4.5, units = "in", dpi = 600
)

############################################################################
## Statistics on PCA by Group
############################################################################

pc_manova <- manova(cbind(PC1, PC2, PC3) ~ group, data = df_pca_tdrl_individual_params)
manova_sum <- summary(pc_manova, test = "Pillai")
eta <- effectsize::eta_squared(pc_manova)
aov_sum <- summary.aov(pc_manova)

# PC1
pc1_posthoc <- TukeyHSD(aov(PC1 ~ group, data = df_pca_tdrl_individual_params))
pc1_posthoc

# PC2
pc2_posthoc <- TukeyHSD(aov(PC2 ~ group, data = df_pca_tdrl_individual_params))
pc2_posthoc

# PC3
pc3_posthoc <- TukeyHSD(aov(PC3 ~ group, data = df_pca_tdrl_individual_params))
pc3_posthoc

manova_multivar_df <- function(manova_summary_obj) {
  df <- as.data.frame(manova_summary_obj$stats, stringsAsFactors = FALSE)
  # make rownames an explicit column if present
  df$Effect <- rownames(df); rownames(df) <- NULL
  # Optional: reorder columns if you prefer
  df[, c("Effect", setdiff(names(df), "Effect"))]
}

aov_tables_df <- function(aov_summary_obj) {
  out <- lapply(names(aov_summary_obj), function(resp) {
    tab <- aov_summary_obj[[resp]]
    df <- as.data.frame(tab, stringsAsFactors = FALSE)
    df$Term <- rownames(df); rownames(df) <- NULL
    df$Response <- resp
    df[, c("Response", "Term", setdiff(names(df), c("Response", "Term")))]
  })
  do.call(rbind, out)
}

tukey_df <- function(tukey_obj) {
  res <- lapply(names(tukey_obj), function(fac) {
    df <- as.data.frame(tukey_obj[[fac]], stringsAsFactors = FALSE)
    df$Contrast <- rownames(df); rownames(df) <- NULL
    df$Factor <- fac
    df[, c("Factor", "Contrast", setdiff(names(df), c("Factor", "Contrast")))]
  })
  do.call(rbind, res)
}

sheets_list <- list(
  "MANOVA (Pillai)"   = manova_multivar_df(manova_sum),
  "Eta Squared"       = as.data.frame(eta),
  "Univariate ANOVAs" = aov_tables_df(aov_sum),
  "PC1 Tukey"         = tukey_df(pc1_posthoc),
  "PC2 Tukey"         = tukey_df(pc2_posthoc),
  "PC3 Tukey"         = tukey_df(pc3_posthoc)
)

openxlsx::write.xlsx(sheets_list,
  file = file.path("Results", "pca_manova_results.xlsx"),
  rowNames = TRUE,
  colNames = TRUE
)

