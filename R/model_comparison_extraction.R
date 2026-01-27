############################################################################
# Script Name: model_comparison_extraction.R
# Purpose: Compares models using Model Evidence (Bridge Sampling) and Predictive
# Density (ELPD_LOO)
# Author: Kasey P. Spry
# Last Modified: Deceber 4, 2025
############################################################################


# Load Packages and Data --------------------------------------------------

library(dplyr)
library(tidyr)
library(writexl)
library(tidyr)

tar_load(bridge_error_TDRL)
tar_load(bridge_error_FullSV)
tar_load(bridge_error_RewardOnly)
tar_load(loo_info_TDRL)
tar_load(loo_info_FullSV)
tar_load(loo_info_RewardOnly)


# Model Evidence (Bridge Sampling) ----------------------------------------
bridge_errors <- list(
  TDRL_pooled = bridge_error_TDRL,
  FullSV_pooled = bridge_error_FullSV,
  RewardOnly_pooled = bridge_error_RewardOnly
)

bridge_error_list <- list()

for (model_name in names(bridge_errors)) {
  logml <- bridge_errors[[model_name]]$bridge_samples$logml

  # Summary
  bridge_error_list[[model_name]] <- data.frame(
    Model = model_name,
    Median = median(logml),
    IQR = IQR(logml)
  )
}

bridge_error_df <- bind_rows(bridge_error_list)


# Predictive Density (ELPD_LOO) -------------------------------------------
loo_results <- list(
  TDRL_pooled = loo_info_TDRL,
  FullSV_pooled = loo_info_FullSV,
  RewardOnly_pooled = loo_info_RewardOnly
)

loo_summary <- lapply(names(loo_results), function(model_name) {
  loo_obj <- loo_results[[model_name]]

  data.frame(
    Model = model_name,
    elpd_loo = loo_obj$loo[["estimates"]]["elpd_loo", "Estimate" ],
    se_elpd_loo = loo_obj$loo[["estimates"]]["elpd_loo", "SE" ]
  )
}) %>%
  bind_rows()


# Save to Results Excel File ----------------------------------------------

# Saves to a folder in your working directory called "Results"
  # If you need to create this folder run the following line:
  # dir.create("Results")

write_xlsx(
  list("Model Evidence - Marginal Likelihood" = bridge_error_df, "Predictive Density - ELPD LOO" = loo_summary),
  path = file.path("Results","ModelComparison_results.xlsx")
)
