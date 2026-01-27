############################################################################
# Script Name: utils-model_comparison.R
# Purpose: Compares model fits using Marginal Evidence (Bridge Sampling)
# and Predictive Density (LOO)
# Author: Kasey P. Spry
# Last Modified: January 26, 2025
############################################################################

#' Get Leave One Out Cross-Validation Info for a Fit Model
#'
#' @param .model_fit An object of type "stanfit"; a fit model using
#'   [rstan::stan] or `run_stan_model()`.
#'
#' @return A list with "loglike", "ess", and "loo" information for the model
#'   fit.
#'

loo_comparison <- function(.model_fit) {

  set.seed(18)
  loglike <- loo::extract_log_lik(.model_fit, merge_chains = FALSE)
  ess <- loo::relative_eff(exp(loglike))
  loo <- loo::loo(.model_fit, r_eff = ess, save_psis = TRUE)

  list("loglike" = loglike,
       "ess" = ess,
       "loo" = loo)
}

#' Perform Bridge Sampling on a Model
#'
#' @param .model_fit An object of type "stanfit"; a fit model using
#'   [rstan::stan] or `run_stan_model()`.
#' @param stan_data The output of `eefrt_prep_stab_data()`.
#' @param stan_file Path to the Stan file - TDRL, Full SV, or Reward Only
#' @param maxiter Number of maximum iterations for bridgesampling.
#' @param reps Number of repetitions for bridgesampling.
#' @param silent Should progress of bridgesampling be printed to console?
#' @param ... Additional parameters passed to [bridgesampling::bridge_sampler]
#'
#' @return A list with element "bridge", corresponding to the output of
#'   `[bridgesampling::bridge_sampler], and element "error" corresponding to the
#'   output of [bridgesampling::error_measures] of the bridge element.
#'

get_bridge_error <- function(.model_fit,
                             stan_data,
                             stan_file,
                             maxiter = 10000,
                             reps = 10,
                             silent = TRUE, ...) {

  set.seed(18)

  model <- rstan::stan(model_code = readLines(stan_file),
                       data = stan_data,
                       chains = 0)

  cli::cli_text("Beginning Bridge Sampling")
  bridge <- bridgesampling::bridge_sampler(samples = .model_fit,
                                           stanfit_model = model,
                                           data = stan_data,
                                           maxiter = maxiter,
                                           repetitions = reps,
                                           silent = silent,
                                           ...)

  errors <- bridgesampling::error_measures(bridge_object = bridge)

  list(bridge_samples = bridge,
       error = errors)
}

extract_model_comparison <- function(
    bridge_errors,
    loo_results,
    excel_path = excel_path) {

  bridge_error_list <- list()

  for (model_name in names(bridge_errors)) {
    logml <- bridge_errors[[model_name]]$bridge_samples$logml

    bridge_error_list[[model_name]] <- data.frame(
      Model = model_name,
      Median = median(logml),
      IQR = IQR(logml)
    )
  }

  bridge_error_df <- bind_rows(bridge_error_list)

  loo_summary <- lapply(names(loo_results), function(model_name) {
    loo_obj <- loo_results[[model_name]]

    data.frame(
      Model = model_name,
      elpd_loo = loo_obj$loo[["estimates"]]["elpd_loo", "Estimate" ],
      se_elpd_loo = loo_obj$loo[["estimates"]]["elpd_loo", "SE" ]
    )
  }) %>%
    bind_rows()

  writexl::write_xlsx(
    list("Model Evidence - Marginal Likelihood" = bridge_error_df,
         "Predictive Density - ELPD LOO" = loo_summary),
    path = excel_path
  )
}

