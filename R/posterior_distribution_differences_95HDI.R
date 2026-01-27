############################################################################
# Script Name: posterior_distribution_differences_95HDI.R
# Purpose: Calculate, summarize, and plot difference of means of posterior distributions
# Author: Kasey P. Spry
# Last Modified: December 4, 2025
############################################################################

############################################################################
## Load Posterior Distributions
############################################################################
group_labels <- c("NeverTUD", "FormerTUD", "TUD", "TUD_OUD")
tar_load(pars_TDRL)
model_fit = pars_TDRL

groupParams <- tibble(
  mu_learnrate = as.vector(model_fit$mu_learnrate_group),
  mu_discount = as.vector(model_fit$mu_discount_group),
  mu_inv_temp = as.vector(model_fit$mu_inv_temp_group),
  group = rep(group_labels, each = nrow(model_fit$mu_learnrate_group))
)

############################################################################
## Calculate 95% HDI and Credible Intervals for Group-level Difference between Means
############################################################################
# Compare: Each group to every other group for every parameter
meanDiffStats <- function(groupParams, group1, group2, param) {
  param_sym <- sym(param)
  diffMeans <- (groupParams %>% filter(group == group1) %>% pull(!!param_sym)) -
    (groupParams %>% filter(group == group2) %>% pull(!!param_sym))
  hdiMu <- HDIofMCMC(diffMeans)
  cred_greater <- (sum(diffMeans > 0) / length(diffMeans) * 100) %>% round(2) %>% format(nsmall = 2)
  cred_lesser <- (sum(diffMeans < 0) / length(diffMeans) * 100) %>% round(2) %>% format(nsmall = 2)
  cred <- paste0(cred_lesser, '% < 0 < ', cred_greater, '%')

  return(list(hdiMu, cred))
}


# 1. Never TUD vs. Former TUD
hdi_neverTUD_formerTUD <- data.frame(matrix(nrow = 3, ncol = 4))
rownames(hdi_neverTUD_formerTUD) <- c('mu_learnrate','mu_discount','mu_inv_temp')
colnames(hdi_neverTUD_formerTUD) <- c('desc','hdiL','hdiH','cred')
hdi_neverTUD_formerTUD[,'desc'] <- 'Never TUD - Former TUD'
for (param in rownames(hdi_neverTUD_formerTUD)) {
  hdi_neverTUD_formerTUD[param,2:3] <- meanDiffStats(groupParams, 'NeverTUD','FormerTUD', param)[[1]]
  hdi_neverTUD_formerTUD[param,4] <- meanDiffStats(groupParams, 'NeverTUD','FormerTUD', param)[[2]]
}

# 2. Never TUD vs. Current TUD
hdi_neverTUD_currentTUD <- data.frame(matrix(nrow = 3, ncol = 4))
rownames(hdi_neverTUD_currentTUD) <- c('mu_learnrate','mu_discount','mu_inv_temp')
colnames(hdi_neverTUD_currentTUD) <- c('desc','hdiL','hdiH','cred')
hdi_neverTUD_currentTUD[,'desc'] <- 'Never TUD - Current TUD'
for (param in rownames(hdi_neverTUD_currentTUD)) {
  hdi_neverTUD_currentTUD[param,2:3] <- meanDiffStats(groupParams, 'NeverTUD','TUD', param)[[1]]
  hdi_neverTUD_currentTUD[param,4] <- meanDiffStats(groupParams, 'NeverTUD','TUD', param)[[2]]
}

# 3. Never TUD vs. TUD+OUD
hdi_neverTUD_TUDOUD <- data.frame(matrix(nrow = 3, ncol = 4))
rownames(hdi_neverTUD_TUDOUD) <- c('mu_learnrate','mu_discount','mu_inv_temp')
colnames(hdi_neverTUD_TUDOUD) <- c('desc','hdiL','hdiH','cred')
hdi_neverTUD_TUDOUD[,'desc'] <- 'Never TUD - TUD+OUD'
for (param in rownames(hdi_neverTUD_TUDOUD)) {
  hdi_neverTUD_TUDOUD[param,2:3] <- meanDiffStats(groupParams, 'NeverTUD','TUD_OUD', param)[[1]]
  hdi_neverTUD_TUDOUD[param,4] <- meanDiffStats(groupParams, 'NeverTUD','TUD_OUD', param)[[2]]
}

# 4. Former TUD vs. Current TUD
hdi_formerTUD_currentTUD <- data.frame(matrix(nrow = 3, ncol = 4))
rownames(hdi_formerTUD_currentTUD) <- c('mu_learnrate','mu_discount','mu_inv_temp')
colnames(hdi_formerTUD_currentTUD) <- c('desc','hdiL','hdiH','cred')
hdi_formerTUD_currentTUD[,'desc'] <- 'Former TUD - Current TUD'
for (param in rownames(hdi_formerTUD_currentTUD)) {
  hdi_formerTUD_currentTUD[param,2:3] <- meanDiffStats(groupParams, 'FormerTUD','TUD', param)[[1]]
  hdi_formerTUD_currentTUD[param,4] <- meanDiffStats(groupParams, 'FormerTUD','TUD', param)[[2]]
}

# 5. Former TUD vs. TUD+OUD
hdi_formerTUD_TUDOUD <- data.frame(matrix(nrow = 3, ncol = 4))
rownames(hdi_formerTUD_TUDOUD) <- c('mu_learnrate','mu_discount','mu_inv_temp')
colnames(hdi_formerTUD_TUDOUD) <- c('desc','hdiL','hdiH','cred')
hdi_formerTUD_TUDOUD[,'desc'] <- 'Former TUD - TUD+OUD'
for (param in rownames(hdi_formerTUD_TUDOUD)) {
  hdi_formerTUD_TUDOUD[param,2:3] <- meanDiffStats(groupParams, 'FormerTUD','TUD_OUD', param)[[1]]
  hdi_formerTUD_TUDOUD[param,4] <- meanDiffStats(groupParams, 'FormerTUD','TUD_OUD', param)[[2]]
}

# 6. Current TUD vs. TUD+OUD
hdi_currentTUD_TUDOUD <- data.frame(matrix(nrow = 3, ncol = 4))
rownames(hdi_currentTUD_TUDOUD) <- c('mu_learnrate','mu_discount','mu_inv_temp')
colnames(hdi_currentTUD_TUDOUD) <- c('desc','hdiL','hdiH','cred')
hdi_currentTUD_TUDOUD[,'desc'] <- 'Current TUD - TUD+OUD'
for (param in rownames(hdi_currentTUD_TUDOUD)) {
  hdi_currentTUD_TUDOUD[param,2:3] <- meanDiffStats(groupParams, 'TUD','TUD_OUD', param)[[1]]
  hdi_currentTUD_TUDOUD[param,4] <- meanDiffStats(groupParams, 'TUD','TUD_OUD', param)[[2]]
}

############################################################################
## Figure: Difference between posterior distributions
############################################################################
plot_param_diff <- function(diff_vector, hdi_table, param_name, y_seg, y_end_seg, x_label, x_limits, fill_color = "#6BA539") {
  df <- tibble(value = diff_vector)

  ggplot(df, aes(x = value)) +
    geom_density(alpha = 0.3, aes(colour = paste("Never TUD - Former TUD")), fill = fill_color) +
    geom_vline(xintercept = 0, linetype = "dotted") +
    scale_colour_manual(values = c("black")) +
    geom_segment(aes(x = as.numeric(hdi_table[param_name, 'hdiL']),
                     xend = as.numeric(hdi_table[param_name, 'hdiH']),
                     y = y_seg,
                     yend = y_end_seg),
                 color = "black", size = 2, alpha = 0.7) +
    annotate("text",
             x = x_limits[1] * 0.9,
             y = max(density(df$value)$y) * 1.05,
             label = hdi_table[param_name, 'cred'],
             hjust = 0) +
    labs(x = x_label, y = "Posterior Density") +
    scale_x_continuous(limits = x_limits, expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, max(density(df$value)$y) * 1.1), expand = c(0, 0)) +
    theme_classic() +
    theme(
      legend.position = "none",
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      plot.margin = margin(t = 10, r = 10, b = 10, l = 20)
    )
}

# Never TUD vs. Former TUD, Difference between Mean distributions
learnrate_neverTUD_formerTUD <-
  groupParams %>% filter(group == "NeverTUD") %>% pull(mu_learnrate) -
  groupParams %>% filter(group == "FormerTUD") %>% pull(mu_learnrate)
neverTUD_formerTUD_Compare_learnrate <- plot_param_diff(
  diff_vector = learnrate_neverTUD_formerTUD,
  hdi_table = hdi_neverTUD_formerTUD,
  param_name = "mu_learnrate",
  y_seg = 0.01,
  y_end_seg = 0.01,
  x_label = "α",
  x_limits = c(-0.10, 0.10),
  fill_color = "#6BA539"
)

discount_neverTUD_formerTUD <-
  groupParams %>% filter(group == "NeverTUD") %>% pull(mu_discount) -
  groupParams %>% filter(group == "FormerTUD") %>% pull(mu_discount)
neverTUD_formerTUD_Compare_discount <- plot_param_diff(
  diff_vector = discount_neverTUD_formerTUD,
  hdi_table = hdi_neverTUD_formerTUD,
  param_name = "mu_discount",
  y_seg = 0.01,
  y_end_seg = 0.01,
  x_label = "γ",
  x_limits = c(-0.8, 0.8),
  fill_color = "#6BA539"
)

inv_temp_neverTUD_formerTUD <-
  groupParams %>% filter(group == "NeverTUD") %>% pull(mu_inv_temp) -
  groupParams %>% filter(group == "FormerTUD") %>% pull(mu_inv_temp)
neverTUD_formerTUD_Compare_inv_temp <- plot_param_diff(
  diff_vector = inv_temp_neverTUD_formerTUD,
  hdi_table = hdi_neverTUD_formerTUD,
  param_name = "mu_inv_temp",
  y_seg = 0.0001,
  y_end_seg = 0.0001,
  x_label = "β",
  x_limits = c(-100, 100),
  fill_color = "#6BA539"
)

# Never TUD vs. Current TUD, Difference between Mean distributions
learnrate_neverTUD_TUD <-
  groupParams %>% filter(group == "NeverTUD") %>% pull(mu_learnrate) -
  groupParams %>% filter(group == "TUD") %>% pull(mu_learnrate)
neverTUD_TUD_Compare_learnrate <- plot_param_diff(
  diff_vector = learnrate_neverTUD_TUD,
  hdi_table = hdi_neverTUD_currentTUD,
  param_name = "mu_learnrate",
  y_seg = 0.01,
  y_end_seg = 0.01,
  x_label = "α",
  x_limits = c(-0.10, 0.10),
  fill_color = "#008C95"
)

discount_neverTUD_TUD <-
  groupParams %>% filter(group == "NeverTUD") %>% pull(mu_discount) -
  groupParams %>% filter(group == "TUD") %>% pull(mu_discount)
neverTUD_TUD_Compare_discount <- plot_param_diff(
  diff_vector = discount_neverTUD_TUD,
  hdi_table = hdi_neverTUD_currentTUD,
  param_name = "mu_discount",
  y_seg = 0.01,
  y_end_seg = 0.01,
  x_label = "γ",
  x_limits = c(-0.8, 0.8),
  fill_color = "#008C95"
)

inv_temp_neverTUD_TUD <-
  groupParams %>% filter(group == "NeverTUD") %>% pull(mu_inv_temp) -
  groupParams %>% filter(group == "TUD") %>% pull(mu_inv_temp)
neverTUD_TUD_Compare_inv_temp <- plot_param_diff(
  diff_vector = inv_temp_neverTUD_TUD,
  hdi_table = hdi_neverTUD_currentTUD,
  param_name = "mu_inv_temp",
  y_seg = 0.0001,
  y_end_seg = 0.0001,
  x_label = "β",
  x_limits = c(-100, 100),
  fill_color = "#008C95"
)

# Never TUD vs. TUD+OUD, Difference between Mean distributions
learnrate_neverTUD_TUDOUD <-
  groupParams %>% filter(group == "NeverTUD") %>% pull(mu_learnrate) -
  groupParams %>% filter(group == "TUD_OUD") %>% pull(mu_learnrate)
neverTUD_TUDOUD_Compare_learnrate <- plot_param_diff(
  diff_vector = learnrate_neverTUD_TUDOUD,
  hdi_table = hdi_neverTUD_TUDOUD,
  param_name = "mu_learnrate",
  y_seg = 0.01,
  y_end_seg = 0.01,
  x_label = "α",
  x_limits = c(-0.10, 0.10),
  fill_color = "#861F41"
)

discount_neverTUD_TUDOUD <-
  groupParams %>% filter(group == "NeverTUD") %>% pull(mu_discount) -
  groupParams %>% filter(group == "TUD_OUD") %>% pull(mu_discount)
neverTUD_TUDOUD_Compare_discount <- plot_param_diff(
  diff_vector = discount_neverTUD_TUDOUD,
  hdi_table = hdi_neverTUD_TUDOUD,
  param_name = "mu_discount",
  y_seg = 0.01,
  y_end_seg = 0.01,
  x_label = "γ",
  x_limits = c(-0.8, 0.8),
  fill_color = "#861F41"
)

inv_temp_neverTUD_TUDOUD <-
  groupParams %>% filter(group == "NeverTUD") %>% pull(mu_inv_temp) -
  groupParams %>% filter(group == "TUD_OUD") %>% pull(mu_inv_temp)
neverTUD_TUDOUD_Compare_inv_temp <- plot_param_diff(
  diff_vector = inv_temp_neverTUD_TUDOUD,
  hdi_table = hdi_neverTUD_TUDOUD,
  param_name = "mu_inv_temp",
  y_seg = 0.0001,
  y_end_seg = 0.0001,
  x_label = "β",
  x_limits = c(-100, 100),
  fill_color = "#861F41"
)

# Former TUD vs. Current TUD, Difference between Mean distributions
learnrate_formerTUD_TUD <-
  groupParams %>% filter(group == "FormerTUD") %>% pull(mu_learnrate) -
  groupParams %>% filter(group == "TUD") %>% pull(mu_learnrate)
formerTUD_TUD_Compare_learnrate <- plot_param_diff(
  diff_vector = learnrate_formerTUD_TUD,
  hdi_table = hdi_formerTUD_currentTUD,
  param_name = "mu_learnrate",
  y_seg = 0.01,
  y_end_seg = 0.01,
  x_label = "α",
  x_limits = c(-0.10, 0.10),
  fill_color = "#9E7E38"
)

discount_formerTUD_TUD <-
  groupParams %>% filter(group == "FormerTUD") %>% pull(mu_discount) -
  groupParams %>% filter(group == "TUD") %>% pull(mu_discount)
formerTUD_TUD_Compare_discount <- plot_param_diff(
  diff_vector = discount_formerTUD_TUD,
  hdi_table = hdi_formerTUD_currentTUD,
  param_name = "mu_discount",
  y_seg = 0.01,
  y_end_seg = 0.01,
  x_label = "γ",
  x_limits = c(-0.8, 0.8),
  fill_color = "#9E7E38"
)

inv_temp_formerTUD_TUD <-
  groupParams %>% filter(group == "FormerTUD") %>% pull(mu_inv_temp) -
  groupParams %>% filter(group == "TUD") %>% pull(mu_inv_temp)
formerTUD_TUD_Compare_inv_temp <- plot_param_diff(
  diff_vector = inv_temp_formerTUD_TUD,
  hdi_table = hdi_formerTUD_currentTUD,
  param_name = "mu_inv_temp",
  y_seg = 0.0001,
  y_end_seg = 0.0001,
  x_label = "β",
  x_limits = c(-100, 100),
  fill_color = "#9E7E38"
)

# Former TUD vs. TUD+OUD, Difference between Mean distributions
learnrate_formerTUD_TUDOUD <-
  groupParams %>% filter(group == "FormerTUD") %>% pull(mu_learnrate) -
  groupParams %>% filter(group == "TUD_OUD") %>% pull(mu_learnrate)
formerTUD_TUDOUD_Compare_learnrate <- plot_param_diff(
  diff_vector = learnrate_formerTUD_TUDOUD,
  hdi_table = hdi_formerTUD_TUDOUD,
  param_name = "mu_learnrate",
  y_seg = 0.01,
  y_end_seg = 0.01,
  x_label = "α",
  x_limits = c(-0.10, 0.10),
  fill_color = "#002B49"
)

discount_formerTUD_TUDOUD <-
  groupParams %>% filter(group == "FormerTUD") %>% pull(mu_discount) -
  groupParams %>% filter(group == "TUD_OUD") %>% pull(mu_discount)
formerTUD_TUDOUD_Compare_discount <- plot_param_diff(
  diff_vector = discount_formerTUD_TUDOUD,
  hdi_table = hdi_formerTUD_TUDOUD,
  param_name = "mu_discount",
  y_seg = 0.01,
  y_end_seg = 0.01,
  x_label = "γ",
  x_limits = c(-0.8, 0.8),
  fill_color = "#002B49"
)

inv_temp_formerTUD_TUDOUD <-
  groupParams %>% filter(group == "FormerTUD") %>% pull(mu_inv_temp) -
  groupParams %>% filter(group == "TUD_OUD") %>% pull(mu_inv_temp)
formerTUD_TUDOUD_Compare_inv_temp <- plot_param_diff(
  diff_vector = inv_temp_formerTUD_TUDOUD,
  hdi_table = hdi_formerTUD_TUDOUD,
  param_name = "mu_inv_temp",
  y_seg = 0.0001,
  y_end_seg = 0.0001,
  x_label = "β",
  x_limits = c(-100, 100),
  fill_color = "#002B49"
)

# Current TUD vs. TUD+OUD, Difference between Mean distributions
learnrate_TUD_TUDOUD <-
  groupParams %>% filter(group == "TUD") %>% pull(mu_learnrate) -
  groupParams %>% filter(group == "TUD_OUD") %>% pull(mu_learnrate)
TUD_TUDOUD_Compare_learnrate <- plot_param_diff(
  diff_vector = learnrate_TUD_TUDOUD,
  hdi_table = hdi_currentTUD_TUDOUD,
  param_name = "mu_learnrate",
  y_seg = 0.01,
  y_end_seg = 0.01,
  x_label = "α",
  x_limits = c(-0.10, 0.10),
  fill_color = "#FDC314"
)

discount_TUD_TUDOUD <-
  groupParams %>% filter(group == "TUD") %>% pull(mu_discount) -
  groupParams %>% filter(group == "TUD_OUD") %>% pull(mu_discount)
TUD_TUDOUD_Compare_discount <- plot_param_diff(
  diff_vector = discount_TUD_TUDOUD,
  hdi_table = hdi_currentTUD_TUDOUD,
  param_name = "mu_discount",
  y_seg = 0.01,
  y_end_seg = 0.01,
  x_label = "γ",
  x_limits = c(-0.8, 0.8),
  fill_color = "#FDC314"
)

inv_temp_TUD_TUDOUD <-
  groupParams %>% filter(group == "TUD") %>% pull(mu_inv_temp) -
  groupParams %>% filter(group == "TUD_OUD") %>% pull(mu_inv_temp)
TUD_TUDOUD_Compare_inv_temp <- plot_param_diff(
  diff_vector = inv_temp_TUD_TUDOUD,
  hdi_table = hdi_currentTUD_TUDOUD,
  param_name = "mu_inv_temp",
  y_seg = 0.0001,
  y_end_seg = 0.0001,
  x_label = "β",
  x_limits = c(-100, 100),
  fill_color = "#FDC314"
)

############################################################################
## Figure: Difference between posterior distributions
## Put together plots & save
############################################################################
fig3a <- plot_grid(neverTUD_formerTUD_Compare_learnrate, neverTUD_formerTUD_Compare_discount, neverTUD_formerTUD_Compare_inv_temp, nrow = 1)
fig3b <- plot_grid(neverTUD_TUD_Compare_learnrate, neverTUD_TUD_Compare_discount, neverTUD_TUD_Compare_inv_temp, nrow = 1)
fig3c <- plot_grid(neverTUD_TUDOUD_Compare_learnrate, neverTUD_TUDOUD_Compare_discount, neverTUD_TUDOUD_Compare_inv_temp, nrow = 1)
fig3d <- plot_grid(formerTUD_TUD_Compare_learnrate, formerTUD_TUD_Compare_discount, formerTUD_TUD_Compare_inv_temp, nrow = 1)
fig3e <- plot_grid(formerTUD_TUDOUD_Compare_learnrate, formerTUD_TUDOUD_Compare_discount, formerTUD_TUDOUD_Compare_inv_temp, nrow = 1)
fig3f <- plot_grid(TUD_TUDOUD_Compare_learnrate, TUD_TUDOUD_Compare_discount, TUD_TUDOUD_Compare_inv_temp, nrow = 1)

fig_combined <- plot_grid(
  fig3a, fig3b, fig3c, fig3d, fig3e, fig3f,
  ncol = 1,
  labels = c("A", "B", "C", "D", "E", "F"),
  label_x = 0,          # Lower values move labels to the far left
  label_position = "left", # Places labels to the left of each plot
  align = "v",
  axis = "lr"
)


ggsave(
  filename = file.path('Figures', 'Posterior_Distribution_Differences_95HDI.png'),
  plot = fig_combined,
  width = 14.5, height = 12, units = "in", dpi = 600
)


############################################################################
## Figure: Plot Legends
############################################################################
# Redo some plots for the legends
vectors <- list(
  `Never TUD - Former TUD`  = learnrate_neverTUD_formerTUD,
  `Never TUD - Current TUD` = learnrate_neverTUD_TUD,
  `Never TUD - TUD+OUD`     = learnrate_neverTUD_TUDOUD,
  `Former TUD - Current TUD`= learnrate_formerTUD_TUD,
  `Former TUD - TUD+OUD`    = learnrate_formerTUD_TUDOUD,
  `Current TUD - TUD+OUD`   = learnrate_TUD_TUDOUD
)

legendData <- data.frame(
  value = unlist(vectors, use.names = FALSE),
  group = rep(names(vectors), times = sapply(vectors, NROW)),
  stringsAsFactors = FALSE
)

colnames(legendData) <- c('data','comparison')
legendData$comparison <- factor(legendData$comparison, levels=c('Never TUD - Former TUD',
                                                                'Never TUD - Current TUD',
                                                                'Never TUD - TUD+OUD',
                                                                'Former TUD - Current TUD',
                                                                'Former TUD - TUD+OUD',
                                                                'Current TUD - TUD+OUD'))

leg <- ggplot(legendData, aes(x=data, colour = comparison, fill = comparison)) +
  geom_density(alpha = 0.3) +
  theme_classic() +
  theme(legend.title = element_blank()) +
  scale_color_manual(values=c("black","black","black","black","black","black"))+
  scale_fill_manual(values=c('#6BA539','#008C95','#861F41','#9E7E38',"#002B49",'#FDC314'))+
  theme(legend.position="bottom",
        legend.text = element_text(size = 12))

ggsave(filename = file.path('Figures', 'Posterior_Distribution_Differences_95HDI_legend.png'),
       plot = leg,
       width = 11.5, height = 8, units = "in", dpi = 600)
