############################################################################
# Script Name: utils-save_figures.R
# Purpose: Saves ggplot figures
# Author: Kasey P. Spry
# Last Modified: January 26, 2025
############################################################################

#' Save ggplot2 Figures
#'
#' @param .figure The figure to be saved.
#' @param .path The path to save the figure
#' @param width The width of the plot
#' @param height The height of the plot
#' @param units Units to save the plot in. Default is "in" (inches).
#' @param ... Additional parameters to pass into [ggplot2::ggsave].
#'
#' @return The file path for the saved object.
#'
save_figure.ggplot_post <- function(.figures,
                                    .path,
                                    width = 1.619 * height,
                                    height = 4.94,
                                    units = "in",
                                    ...) {
  combined_plot <- cowplot::plot_grid(plotlist = .figures,
                                      nrow = 1,
                                      ncol = 3
  )
  ggplot2::ggsave(
    filename = here::here(.path),
    plot = combined_plot,
    width = width,
    height = height,
    units = "in",
    ...
  )
  return(.path)
}
