############################################################################
# Script Name: utils-model_recovery.R
# Purpose: Performs model recovery for fitted model
# Author: Kasey P. Spry
# Last Modified: January 26, 2025
############################################################################

#' Extract MAPs for Full SV model
#'
#' @param pars_list posterior parameter list
#'
#' @returns
#'
extract_participant_MAPs_fullsv <- function(pars_list) {
  get_MAP <- function(samples) {
    dens <- density(samples)
    dens$x[which.max(dens$y)]
  }
  tibble::tibble(
    id = seq_len(ncol(pars_list$h)),
    h = apply(pars_list$h, 2, get_MAP),
    k = apply(pars_list$k, 2, get_MAP),
    inv_temp = apply(pars_list$inv_temp, 2, get_MAP)
  )
}

#' Extract MAPs for Reward Only SV model
#'
#' @param pars_list posterior parameter list
#'
#' @returns
#'
extract_participant_MAPs_rewardonly <- function(pars_list) {
  get_MAP <- function(samples) {
    dens <- density(samples)
    dens$x[which.max(dens$y)]
  }
  tibble::tibble(
    id       = seq_len(ncol(pars_list$k)),
    k        = apply(pars_list$k,        2, get_MAP),
    inv_temp = apply(pars_list$inv_temp, 2, get_MAP)
  )
}

#' Packer for SV models
#'
#' @description packs simulated data into arrays for SV models for model fitting
#'
#' @param sim_data simulated data
#' @param stan_data_template actual stan_data
#' @param default_RT sets reaction time to -1 to avoid NULL
#'
#' @returns
#'
pack_sim_into_stan_arrays_sv <- function(
    sim_data,
    stan_data_template,
    default_RT = -1
) {
  required_cols <- c("id", "trial", "choice", "prob", "hard_mag")
  missing_cols <- setdiff(required_cols, names(sim_data))
  if (length(missing_cols) > 0) {
    stop("`sim_data` is missing: ", paste(missing_cols, collapse = ", "))
  }

    sim_data <- sim_data[order(sim_data$id, sim_data$trial), ]

  num_subjects <- stan_data_template$num_subjects
  max_trials   <- stan_data_template$max_trials
  group_id     <- stan_data_template$group_id

  ids <- sort(unique(sim_data$id))
  if (length(ids) != num_subjects || !all(ids == seq_len(num_subjects))) {
    stop("sim_data$id must be 1..num_subjects to match `stan_data_template`.")
  }

  trials_by_id   <- tapply(sim_data$trial, sim_data$id, max)
  trials_per_subj <- as.integer(trials_by_id)

  choice_arr <- array(-1L, dim = c(num_subjects, max_trials))
  prob_arr   <- array(-1,  dim = c(num_subjects, max_trials))
  reward_arr <- array(-1,  dim = c(num_subjects, max_trials))
  rt_arr     <- array(default_RT, dim = c(num_subjects, max_trials))

  for (i in seq_len(num_subjects)) {
    rows_i <- sim_data[sim_data$id == i, ]
    Ti <- nrow(rows_i)
    if (Ti == 0) next
    if (Ti > max_trials) {
      stop("Subject ", i, " has ", Ti, " trials > max_trials = ", max_trials)
    }
    choice_arr[i, 1:Ti] <- as.integer(rows_i$choice)
    prob_arr[i,   1:Ti] <- as.numeric(rows_i$prob)
    reward_arr[i, 1:Ti] <- as.numeric(rows_i$hard_mag)  # reward = HARD magnitude in SV models
    rt_arr[i,     1:Ti] <- 1
  }

  list(
    max_trials      = as.integer(max_trials),
    num_subjects    = as.integer(num_subjects),
    trials_per_subj = as.array(trials_per_subj),
    choiceRT        = rt_arr,
    choice          = choice_arr,
    probability     = prob_arr,
    reward          = reward_arr,
    group_id        = as.array(as.integer(group_id))
  )
}

#' Simulates data using TDRL
#'
#' @description This function simulates data using TDRL for model recovery.
#' Wraps 'simulate_tdrl()'
#'
#' @param params_df data frame of actual posterior parameters
#' @param hard_mag_list actual hard magnitude list
#' @param prob_list actual probably list
#' @param n_reps limits number of trials. Default set to 50
#' @param initV initialize q_Vals
#' @param seed set seed
#'
#' @returns
#'
simulate_dataset_tdrl <- function(params_df,
                                  hard_mag_list,
                                  prob_list,
                                  n_reps = 50,
                                  initV = 0,
                                  seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  N <- nrow(params_df)
  sims <- vector("list", N)
  for (i in seq_len(N)) {
    id_i <- params_df$id[i]
    p_i <- list(
      learnrate = params_df$learnrate[i],
      discount  = params_df$discount[i],
      inv_temp  = params_df$inv_temp[i]
    )
    hard_i <- truncate_to_T(hard_mag_list[[id_i]], n_reps)
    prob_i <- truncate_to_T(prob_list[[id_i]], n_reps)

    sim_i <- simulate_tdrl(
      params   = p_i,
      hard_mag = hard_i,
      prob     = prob_i,
      initV    = initV
    )
    sim_i$id <- id_i
    sims[[i]] <- sim_i
  }
  dplyr::bind_rows(sims)
}

#' Simulates data using Full SV model
#'
#' @description This function simulates data using Full SV model for
#' model recovery.
#'
#' @param params_df data frame of actual posterior parameters
#' @param hard_mag_list actual hard magnitude list
#' @param prob_list actual probably list
#' @param n_reps limits number of trials. Default set to 50
#' @param initV initialize q_Vals
#' @param seed set seed
#'
#' @returns
#'
simulate_dataset_fullsv <- function(params_df,
                                    hard_mag_list,
                                    prob_list,
                                    n_reps = 50,
                                    initV = 0,
                                    seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  N <- nrow(params_df)
  sims <- vector("list", N)

  for (i in seq_len(N)) {
    id_i <- params_df$id[i]
    h        <- params_df$h[i]
    k        <- params_df$k[i]
    inv_temp <- params_df$inv_temp[i]

    hard_i <- truncate_to_T(hard_mag_list[[id_i]], n_reps)
    prob_i <- truncate_to_T(prob_list[[id_i]], n_reps)

    actions  <- integer(n_reps)
    outcomes <- numeric(n_reps)

    for (t in seq_len(n_reps)) {
      SVHard <- hard_i[t] * (prob_i[t]^h) - (k * 1.0)
      SVEasy <- 1.0       * (prob_i[t]^h) - (k * 0.3)
      option_SV <- c(SVHard, SVEasy)

      mx   <- max(inv_temp * option_SV)
      expv <- exp(inv_temp * option_SV - mx)
      p    <- expv / sum(expv)

      choice_idx   <- sample(1:2, size = 1, prob = p)  # 1=Hard, 2=Easy
      actions[t]   <- choice_idx
      outcomes[t]  <- if (choice_idx == 1) {
        rbinom(1, 1, prob_i[t]) * hard_i[t]
      } else {
        rbinom(1, 1, prob_i[t]) * 1.0
      }
    }

    sim_i <- data.frame(
      trial    = seq_len(n_reps),
      option1  = 1L,
      option2  = 2L,
      choice   = actions,
      outcome  = outcomes,
      hard_mag = hard_i,
      prob     = prob_i
    )
    sim_i$id <- id_i
    sims[[i]] <- sim_i
  }

  dplyr::bind_rows(sims)
}

#' Simulates data using Reward Only SV model
#'
#' @description This function simulates data using Reward Only SV model for
#' model recovery.
#'
#' @param params_df data frame of actual posterior parameters
#' @param hard_mag_list actual hard magnitude list
#' @param prob_list actual probably list
#' @param n_reps limits number of trials. Default set to 50
#' @param seed set seed
#'
#' @returns
#'
simulate_dataset_rewardonly <- function(params_df,
                                        hard_mag_list,
                                        prob_list,
                                        n_reps = 50,
                                        seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  N <- nrow(params_df)
  sims <- vector("list", N)

  for (i in seq_len(N)) {
    id_i <- params_df$id[i]
    k        <- params_df$k[i]
    inv_temp <- params_df$inv_temp[i]

    hard_i <- truncate_to_T(hard_mag_list[[id_i]], n_reps)
    prob_i <- truncate_to_T(prob_list[[id_i]], n_reps)

    actions  <- integer(n_reps)
    outcomes <- numeric(n_reps)

    for (t in seq_len(n_reps)) {
      SVHard <- hard_i[t] - (k * 1.0)
      SVEasy <- 1.0      - (k * 0.3)
      option_SV <- c(SVHard, SVEasy)

      mx   <- max(inv_temp * option_SV)
      expv <- exp(inv_temp * option_SV - mx)
      p    <- expv / sum(expv)

      choice_idx   <- sample(1:2, size = 1, prob = p)  # 1=Hard, 2=Easy
      actions[t]   <- choice_idx
      outcomes[t]  <- if (choice_idx == 1) {
        rbinom(1, 1, prob_i[t]) * hard_i[t]
      } else {
        rbinom(1, 1, prob_i[t]) * 1.0
      }
    }

    sim_i <- data.frame(
      trial    = seq_len(n_reps),
      option1  = 1L,
      option2  = 2L,
      choice   = actions,
      outcome  = outcomes,
      hard_mag = hard_i,
      prob     = prob_i
    )
    sim_i$id <- id_i
    sims[[i]] <- sim_i
  }

  dplyr::bind_rows(sims)
}

#' Organize LOO and bridge sampling results for model recovery
#'
#' @description This function takes in the LOO and bridge sampling results
#' from the model recovery and organizes it for export to an excel file.
#'
#' @param fits A named list of fit objects, names formatted as "GEN|FIT".
#' @param bridge_list A named list of bridge results, same names as `fits`.
#' @param loo_fun A function that takes a fit and returns a \code{loo} object
#' @param excel_path file path to export excel file
#' @return A list with elements:
#'   \itemize{
#'     \item \code{loo_table}: tidy LOO summary per GEN × FIT
#'     \item \code{bridge_table}: tidy bridge summary per GEN × FIT (if provided)
#'     \item \code{combined_table}: left-join of the two
#'     \item \code{accuracy}: recovery accuracy per generator (LOO and bridge)
#'   }
#'
organize_loo_and_bridge <- function(
    fits,
    bridge_list = mr_bridge_list,
    loo_fun = function(fit) loo_comparison(fit)$loo,
    export_excel = TRUE,
    excel_path = excel_path
) {

  parse_key <- function(key, sep = "\\|") {
    parts <- strsplit(key, sep)[[1]]
    if (length(parts) != 2)
      stop("Names must be 'GEN|FIT': ", key)
    list(generating_model = parts[[1]], fitted_model = parts[[2]])
  }

  safe_loo <- function(fit) {
    tryCatch(loo_fun(fit), error = function(e) NULL)
  }

  safe_logml <- function(x) {
    if (is.null(x)) return(NA_real_)
    if (is.numeric(x) && length(x) == 1L) return(as.numeric(x))
    candidates <- c("logml", "logml_estimate", "log_marginal_lik", "logmarglik")
    for (nm in candidates) {
      if (!is.null(x[[nm]]) && is.numeric(x[[nm]])) return(as.numeric(x[[nm]]))
    }
    NA_real_
  }

  safe_bridge_err <- function(x) {
    if (is.null(x)) return(NA_character_)
    fields <- c("error", "diagnostics", "messages", "note")
    for (nm in fields) {
      if (!is.null(x[[nm]])) {
        val <- x[[nm]]
        if (is.character(val)) return(paste(val, collapse = "; "))
        if (is.list(val)) return(paste(capture.output(str(val)), collapse = " "))
      }
    }
    NA_character_
  }

  # ---------- LOO extraction ----------
  loo_rows <- purrr::map2(
    .x = fits,
    .y = names(fits),
    .f = function(fit, key) {
      ids <- parse_key(key)
      loo_obj <- safe_loo(fit)

      if (is.null(loo_obj))
        return(tibble::tibble(
          generating_model = ids$generating_model,
          fitted_model     = ids$fitted_model,
          elpd_loo = NA_real_,
          se_elpd_loo = NA_real_,
          looic = NA_real_
        ))

      est <- loo_obj$estimates
      elpd <- est["elpd_loo", "Estimate"]
      se   <- est["elpd_loo", "SE"]

      tibble::tibble(
        generating_model = ids$generating_model,
        fitted_model     = ids$fitted_model,
        elpd_loo = as.numeric(elpd),
        se_elpd_loo = as.numeric(se),
        looic = -2 * as.numeric(elpd)
      )
    }
  ) %>% dplyr::bind_rows()

  loo_table <- loo_rows %>%
    dplyr::group_by(generating_model) %>%
    dplyr::mutate(
      rank_loo = dplyr::min_rank(dplyr::desc(elpd_loo)),
      delta_elpd = elpd_loo - max(elpd_loo, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()



  # ---------- Bridge sampling ----------
  if (!is.null(bridge_list)) {
    bridge_table <- purrr::map2_dfr(
      .x = bridge_list,
      .y = names(bridge_list),
      function(b, key) {
        ids <- parse_key(key)

        logml <- NULL

        # Most common place: $bridge_samples$logml
        if (!is.null(b$bridge_samples$logml)) {
          logml <- b$bridge_samples$logml
        } else {
          logml <- safe_logml(b)
        }

        if (is.null(logml)) logml <- NA_real_

        tibble::tibble(
          generating_model = ids$generating_model,
          fitted_model = ids$fitted_model,
          logml_median = median(logml, na.rm = TRUE),
          logml_iqr = IQR(logml, na.rm = TRUE),
          logml = median(logml, na.rm = TRUE)   # used for ranking
        )
      }
    ) %>%
      dplyr::group_by(generating_model) %>%
      dplyr::mutate(
        rank_bridge = dplyr::min_rank(dplyr::desc(logml)),
        delta_logml = logml - max(logml, na.rm = TRUE),
        BF_best_over_model = exp(max(logml, na.rm = TRUE) - logml)
      ) %>%
      dplyr::ungroup()

  } else {
    bridge_table <- NULL
  }

  # ---------- Combine ----------
  combined_table <- if (!is.null(bridge_table)) {
    dplyr::left_join(
      loo_table, bridge_table,
      by = c("generating_model", "fitted_model")
    )
  } else {
    loo_table
  }

  # ---------- recovery accuracy ----------
  top_by_loo <- combined_table %>%
    dplyr::group_by(generating_model) %>%
    dplyr::slice_max(order_by = elpd_loo, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      generating_model,
      top_fit_by_loo = fitted_model,
      correct_by_loo = fitted_model == generating_model
    )

  if (!is.null(bridge_table)) {
    top_by_bridge <- combined_table %>%
      dplyr::group_by(generating_model) %>%
      dplyr::slice_max(order_by = logml, with_ties = FALSE) %>%
      dplyr::ungroup() %>%
      dplyr::transmute(
        generating_model,
        top_fit_by_bridge = fitted_model,
        correct_by_bridge = fitted_model == generating_model
      )
  } else {
    top_by_bridge <- tibble::tibble(
      generating_model = unique(combined_table$generating_model),
      top_fit_by_bridge = NA_character_,
      correct_by_bridge = NA
    )
  }

  accuracy <- dplyr::left_join(top_by_loo, top_by_bridge, by = "generating_model")

  # ---------- Excel export ----------
  if (export_excel) {
    writexl::write_xlsx(
      list(
        LOO = loo_table,
        Bridge = bridge_table,
        Combined = combined_table,
        Accuracy = accuracy
      ),
      path = excel_path
    )
  }

  # ---------- return ----------
  list(
    loo_table = loo_table,
    bridge_table = bridge_table,
    combined_table = combined_table,
    accuracy = accuracy,
    excel_path = if (export_excel) excel_path else NULL
  )
}
