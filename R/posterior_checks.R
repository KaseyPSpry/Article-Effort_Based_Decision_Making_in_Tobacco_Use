tar_load(fit_TDRL)
tar_load(fit_FullSV)
tar_load(fit_RewardOnly)

# Extract the summary information
summary_stats <- summary(fit_TDRL)$summary

summary_tbl <- as.data.frame(summary_stats) %>%
  tibble::rownames_to_column(var = "parameter")

# Rhat > 1.1
problematic_params <- summary_tbl %>% filter(!is.na(Rhat) & Rhat > 1.1)
problematic_params

# Rhat = NaN
na_rhat_params    <- summary_tbl %>% filter(is.na(Rhat))
na_rhat_params




