############################################################################
# Script Name: utils-modeling_figures.R
# Purpose: Creates figures from TDRL fit
# Author: Kasey P. Spry
# Last Modified: January 26, 2025
############################################################################

#' Generate Population Level Posterior Estimates for TDRL Model Fit for α
#'
#' @description This function accepts a model fit and plots the
#'   posterior density for all group-level posterior parameter samples (any
#'   parameter beginning with 'mu_'), excluding the mean (mu_pr) used in the
#'   standard normal hierarchical specification.
#' @param model_fit Posterior parameters from model fit environment
#' @param stan_data The output of `eefrt_prep_stab_data()`
#'
#' @return A ggplot
#'
generate_group_posterior_plot_tdrl_learnrate <- function(model_fit, stan_data) {

  learnrate_samples <- model_fit[["mu_learnrate_group"]]

  group_labels <- c(
    "1" = "Never TUD",     # 1
    "2" = "Former TUD",    # 2
    "3" = "Current TUD",   # 3
    "4" = "TUD+OUD"        # 4
  )

  group_levels <- unname(group_labels)

  group_levels <- unname(group_labels)

  get_MAP <- function(samples) {
    dens <- density(samples)
    dens$x[which.max(dens$y)]
  }

  post_data_individual <- tibble(
    participant = 1:ncol(model_fit$learnrate),
    participant_mean = colMeans(model_fit$learnrate),
    partcipant_median = apply(model_fit$learnrate, 2, median),
    participant_MAP = apply(model_fit$learnrate, 2, get_MAP),
    group_id = stan_data$group_id
  ) %>%
    mutate(
      group = factor(
        group_labels[as.character(group_id)],
        levels = group_levels
      )
    )

  post_data_group <- as_tibble(learnrate_samples) %>%
    setNames(group_labels) %>%
    pivot_longer(
      cols = everything(),
      names_to = "group",
      values_to = "value"
    ) %>%
    mutate(group = factor(group, levels = group_levels))

  hdi_data <- post_data_group %>%
    group_by(group) %>%
    summarise(
      lower = HDInterval::hdi(value, credMass = 0.95)[1],
      upper = HDInterval::hdi(value, credMass = 0.95)[2],
      .groups = "drop"
    ) %>%
    mutate(group = factor(group, levels = group_levels))

  model_color_vec <- c(
    "Former TUD" = "#008C95",
    "Never TUD" = "#A7A8A9",
    "Current TUD" = "#9E7E38",
    "TUD+OUD" = "#6BA539"
  )

  ggplot(post_data_group, aes(x = value, fill = group, color = group)) +
    ggdist::stat_slab(
      size = 0.75,
      normalize = "panels",
      # slab_color = "black",
      alpha = 0.5
    ) +
    geom_segment(
      data = hdi_data,
      aes(x = lower, xend = upper, y = 0, yend = 0),
      color = "black",
      size = 1.0,
      inherit.aes = FALSE
    ) +
    geom_point(
      data = post_data_individual,
      aes(x = participant_MAP, y = 0, color = group),
      shape = 21,
      color = "black",
      fill = "black",
      size = 1,
      stroke = 1,
      alpha = 0.5,
      inherit.aes = FALSE
    ) +
    scale_fill_manual(values = model_color_vec) +
    scale_color_manual(values = model_color_vec) +
    facet_wrap(~group, ncol = 1) +
    cowplot::theme_cowplot() +
    theme(
      legend.position = "none",
      legend.title = element_blank(),
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 14),
      plot.title = element_text(size = 18, face = "bold"),
      plot.margin = margin(1, 1, 1, 1, "cm"),
      strip.text = element_blank()
    ) +
    labs(
      y = "Posterior Density",
      x = "α",
      title = NULL
    ) +
    coord_cartesian(ylim = c(-0.05, 1.0))
}

#' Generate Population Level Posterior Estimates for TDRL Model Fit for γ
#'
#' @description This function accepts a model fit and plots the
#'   posterior density for all group-level posterior parameter samples (any
#'   parameter beginning with 'mu_'), excluding the mean (mu_pr) used in the
#'   standard normal hierarchical specification.
#' @param model_fit Posterior parameters from model fit environment
#' @param stan_data The output of `eefrt_prep_stab_data()`
#'
#' @return A ggplot
#'
generate_group_posterior_plot_tdrl_discount <- function(model_fit, stan_data) {

  discount_samples <- model_fit[["mu_discount_group"]]

  group_labels <- c(
    "1" = "Never TUD",     # 1
    "2" = "Former TUD",    # 2
    "3" = "Current TUD",   # 3
    "4" = "TUD+OUD"        # 4
  )

  group_levels <- unname(group_labels)

  get_MAP <- function(samples) {
    dens <- density(samples)
    dens$x[which.max(dens$y)]
  }

  post_data_individual <- tibble(
    participant = 1:ncol(model_fit$discount),
    participant_mean = colMeans(model_fit$discount),
    partcipant_median = apply(model_fit$discount, 2, median),
    participant_MAP = apply(model_fit$discount, 2, get_MAP),
    group_id = stan_data$group_id
  ) %>%
    mutate(
      group = factor(
        group_labels[as.character(group_id)],
        levels = group_levels
      )
    )

  post_data_group <- as_tibble(discount_samples) %>%
    setNames(group_labels) %>%
    pivot_longer(
      cols = everything(),
      names_to = "group",
      values_to = "value"
    ) %>%
    mutate(group = factor(group, levels = group_levels))

  hdi_data <- post_data_group %>%
    group_by(group) %>%
    summarise(
      lower = HDInterval::hdi(value, credMass = 0.95)[1],
      upper = HDInterval::hdi(value, credMass = 0.95)[2],
      .groups = "drop"
    ) %>%
    mutate(group = factor(group, levels = group_levels))

  model_color_vec <- c(
    "Former TUD" = "#008C95",
    "Never TUD" = "#A7A8A9",
    "Current TUD" = "#9E7E38",
    "TUD+OUD" = "#6BA539"
  )

  ggplot(post_data_group, aes(x = value, fill = group, color = group)) +
    ggdist::stat_slab(
      size = 0.75,
      normalize = "panels",
      # slab_color = "black",
      alpha = 0.5
    ) +
    geom_segment(
      data = hdi_data,
      aes(x = lower, xend = upper, y = 0, yend = 0),
      color = "black",
      size = 1.0,
      inherit.aes = FALSE
    ) +
    geom_point(
      data = post_data_individual,
      aes(x = participant_MAP, y = 0, color = group),
      shape = 21,
      color = "black",
      fill = "black",
      size = 1,
      stroke = 1,
      alpha = 0.5,
      inherit.aes = FALSE
    ) +
    scale_fill_manual(values = model_color_vec) +
    scale_color_manual(values = model_color_vec) +
    facet_wrap(~group, ncol = 1) +
    cowplot::theme_cowplot() +
    theme(
      legend.position = "none",
      legend.title = element_blank(),
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 14),
      plot.title = element_text(size = 18, face = "bold"),
      plot.margin = margin(1, 1, 1, 1, "cm"),
      strip.text = element_blank()
    ) +
    labs(
      y = "Posterior Density",
      x = "γ",
      title = NULL
    ) +
    coord_cartesian(ylim = c(-0.05, 1.0))
}

#' Generate Population Level Posterior Estimates for TDRL Model Fit for β
#'
#' @description This function accepts a model fit and plots the
#'   posterior density for all group-level posterior parameter samples (any
#'   parameter beginning with 'mu_'), excluding the mean (mu_pr) used in the
#'   standard normal hierarchical specification.
#' @param model_fit Posterior parameters from model fit environment
#' @param stan_data The output of `eefrt_prep_stab_data()`
#'
#' @return A ggplot
#'
generate_group_posterior_plot_tdrl_inv_temp <- function(model_fit, stan_data) {

  inv_temp_samples <- model_fit[["mu_inv_temp_group"]]

  group_labels <- c(
    "1" = "Never TUD",     # 1
    "2" = "Former TUD",    # 2
    "3" = "Current TUD",   # 3
    "4" = "TUD+OUD"        # 4
  )

  group_levels <- unname(group_labels)

  get_MAP <- function(samples) {
    dens <- density(samples)
    dens$x[which.max(dens$y)]
  }

  post_data_individual <- tibble(
    participant = 1:ncol(model_fit$inv_temp),
    participant_mean = colMeans(model_fit$inv_temp),
    partcipant_median = apply(model_fit$inv_temp, 2, median),
    participant_MAP = apply(model_fit$inv_temp, 2, get_MAP),
    group_id = stan_data$group_id
  ) %>%
    mutate(
      group = factor(
        group_labels[as.character(group_id)],
        levels = group_levels
      )
    )

  post_data_group <- as_tibble(inv_temp_samples) %>%
    setNames(group_labels) %>%
    pivot_longer(
      cols = everything(),
      names_to = "group",
      values_to = "value"
    ) %>%
    mutate(group = factor(group, levels = group_levels))

  hdi_data <- post_data_group %>%
    group_by(group) %>%
    summarise(
      lower = HDInterval::hdi(value, credMass = 0.95)[1],
      upper = HDInterval::hdi(value, credMass = 0.95)[2],
      .groups = "drop"
    ) %>%
    mutate(group = factor(group, levels = group_levels))

  model_color_vec <- c(
    "Former TUD" = "#008C95",
    "Never TUD" = "#A7A8A9",
    "Current TUD" = "#9E7E38",
    "TUD+OUD" = "#6BA539"
  )

  ggplot(post_data_group, aes(x = value, fill = group, color = group)) +
    ggdist::stat_slab(
      size = 0.75,
      normalize = "panels",
      # slab_color = "black",
      alpha = 0.5
    ) +
    geom_segment(
      data = hdi_data,
      aes(x = lower, xend = upper, y = 0, yend = 0),
      color = "black",
      size = 1.0,
      inherit.aes = FALSE
    ) +
    geom_point(
      data = post_data_individual,
      aes(x = participant_MAP, y = 0, color = group),
      shape = 21,
      color = "black",
      fill = "black",
      size = 1,
      stroke = 1,
      alpha = 0.5,
      inherit.aes = FALSE
    ) +
    scale_fill_manual(values = model_color_vec) +
    scale_color_manual(values = model_color_vec) +
    facet_wrap(~group, ncol = 1) +
    cowplot::theme_cowplot() +
    theme(
      legend.position = "none",
      legend.title = element_blank(),
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 14),
      plot.title = element_text(size = 18, face = "bold"),
      plot.margin = margin(1, 1, 1, 1, "cm"),
      strip.text = element_blank()
    ) +
    labs(
      y = "Posterior Density",
      x = "β",
      title = NULL
    ) +
    coord_cartesian(ylim = c(-0.05, 1.0))
}

#' Generate Legend for Posterior Distribution Plot
#'
#' @description This function accepts a list of groups and HEX codes
#'  to create a legend for the posterior distribution plot
#'
#' @param model_fit Posterior parameters from model fit environment
#' @param stan_data The output of `eefrt_prep_stab_data()`
#'
#' @return ggplot that can be cropped for legend only
#'
generate_group_posterior_plot_legend <- function(model_fit, stan_data) {

  inv_temp_samples <- model_fit[["mu_inv_temp_group"]]

  group_labels <- c(
    "1" = "Never TUD",     # 1
    "2" = "Former TUD",    # 2
    "3" = "Current TUD",   # 3
    "4" = "TUD+OUD"        # 4
  )

  group_levels <- unname(group_labels)

  get_MAP <- function(samples) {
    dens <- density(samples)
    dens$x[which.max(dens$y)]
  }

  post_data_individual <- tibble(
    participant = 1:ncol(model_fit$inv_temp),
    participant_mean = colMeans(model_fit$inv_temp),
    partcipant_median = apply(model_fit$inv_temp, 2, median),
    participant_MAP = apply(model_fit$inv_temp, 2, get_MAP),
    group_id = stan_data$group_id
  ) %>%
    mutate(
      group = factor(
        group_labels[as.character(group_id)],
        levels = group_levels
      )
    )
  post_data_group <- as_tibble(inv_temp_samples) %>%
    setNames(group_labels) %>%
    pivot_longer(
      cols = everything(),
      names_to = "group",
      values_to = "value"
    ) %>%
    mutate(group = factor(group, levels = group_levels))

  hdi_data <- post_data_group %>%
    group_by(group) %>%
    summarise(
      lower = HDInterval::hdi(value, credMass = 0.95)[1],
      upper = HDInterval::hdi(value, credMass = 0.95)[2],
      mean = mean(value),
      .groups = "drop"
    ) %>%
    mutate(group = factor(group, levels = group_levels))

  model_color_vec <- c(
    "Former TUD" = "#008C95",
    "Never TUD" = "#A7A8A9",
    "Current TUD" = "#9E7E38",
    "TUD+OUD" = "#6BA539"
  )

  legend <- ggplot(post_data_group, aes(x = value, fill = group, color = group)) +
    ggdist::stat_slab(
      size = 0.75,
      normalize = "none",
      # slab_color = "black",
      alpha = 0.5
    ) +
    geom_segment(
      data = hdi_data,
      aes(x = lower, xend = upper, y = 0, yend = 0, color = "black"),
      size = 1.0,
      inherit.aes = FALSE
    ) +
    geom_point(
      data = post_data_individual,
      aes(x = participant_MAP, y = 0, color = group),
      shape = 21,
      color = "black",
      fill = "black",
      size = 1,
      stroke = 1,
      alpha = 0.5,
      inherit.aes = FALSE
    ) +
    scale_fill_manual(values = model_color_vec) +
    scale_color_manual(values = model_color_vec) +
    facet_wrap(~group, ncol = 1) +
    cowplot::theme_cowplot() +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 14),
      plot.title = element_text(size = 18, face = "bold"),
      plot.margin = margin(1, 1, 1, 1, "cm"),
      strip.text = element_blank()
    ) +
    labs(
      y = "Posterior Density",
      x = "γ",
      title = NULL
    ) +
    coord_cartesian(ylim = c(-0.5, 5))
}
