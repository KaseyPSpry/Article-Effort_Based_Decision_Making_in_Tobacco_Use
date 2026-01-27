############################################################################
# Script Name: utils-parameter_recovery.R
# Purpose: Performs parameter recovery for best fit model
# Author: Kasey P. Spry
# Last Modified: January 26, 2025
############################################################################

#' Extract participant MAPs to simulate data for parameter recovery
#'
#' @description This function takes in a posterior parameters from the
#' fitted model on real data to extract participant MAPs to then use to
#' simulate data for model and parameter recovery
#'
#' @param pars_list posterior parameters from fitted model on real data
#'
#' @returns a list of MAPs for each partipant for each parameter
#'
extract_participant_MAPs <- function(pars_list) {

  get_MAP <- function(samples) {
    dens <- density(samples)
    dens$x[which.max(dens$y)]
  }

  learnrate_MAP <- apply(pars_list$learnrate, 2, get_MAP)
  discount_MAP  <- apply(pars_list$discount,  2, get_MAP)
  inv_temp_MAP  <- apply(pars_list$inv_temp,  2, get_MAP)
  tibble::tibble(
    id = seq_along(learnrate_MAP),
    learnrate = learnrate_MAP,
    discount  = discount_MAP,
    inv_temp  = inv_temp_MAP
  )
}

#' Generates hard task magnitude and reward probability for simulated data
#'
#' @description This function takes in the stan data frame from the real data
#' that was used to originally fit the model
#'
#' @param stan_data stan data frame from real data
#' @param hard_field data frame within stan_data that contains hard reward
#' magnitude values
#' @param prob_field data frame within stan_data that contains reward
#' probability values
#'
build_hard_mag_and_prob_lists <- function(stan_data,
                                          hard_field = "reward_hard",
                                          prob_field = "probability") {

  hard_mat <- stan_data[[hard_field]]
  prob_mat <- stan_data[[prob_field]]
  if (is.null(hard_mat) || is.null(prob_mat)) stop("Missing fields.")

  N <- nrow(hard_mat)
  hard_list <- lapply(seq_len(N), function(i) hard_mat[i, ])
  prob_list <- lapply(seq_len(N), function(i) prob_mat[i, ])
  list(hard_mag_list = hard_list, prob_list = prob_list)
}

truncate_to_T <- function(x, T = 50) {
  if (length(x) < T) stop("Input shorter than desired T=", T)
  x[seq_len(T)]
}

#' Simulates TDRL
#'
#' @description This function takes in posterior parameters from TDRL, hard
#' task magnitude, and reward probability to simulate data using TDRL. This
#' function is used be `simulate_tdrl_all_participants`
#'
#' @param params list of actual posterior parameters
#' @param hard_mag output from `hard_prob_lists$hard_mag_list()`
#' @param prob output from `prob_lists$orb_list()`
#' @param initV initialize values for q_vals
#'
simulate_tdrl <- function(params, hard_mag, prob, initV = 0) {
  learnrate <- params$learnrate
  discount  <- params$discount
  inv_temp  <- params$inv_temp

  n_trials <- length(hard_mag)
  actions  <- integer(n_trials)
  outcomes <- numeric(n_trials)

  q_vals <- matrix(initV, nrow = 2, ncol = 3)

  for (t in seq_len(n_trials)) {
    opt_vals <- c(q_vals[1, 1], q_vals[2, 1])
    mx   <- max(inv_temp * opt_vals)
    expv <- exp(inv_temp * opt_vals - mx)
    p    <- expv / sum(expv)

    choice_idx <- sample(1:2, size = 1, prob = p)
    actions[t] <- choice_idx

    if (choice_idx == 1) {
      outcomes[t] <- rbinom(1, 1, prob[t]) * 1
      action <- 1
    } else {
      outcomes[t] <- rbinom(1, 1, prob[t]) * hard_mag[t]
      action <- 2
    }

    for (ep in 1:3) {
      if (ep < 3) {
        PE <- discount * q_vals[action, ep + 1] - q_vals[action, ep]
      } else {
        PE <- outcomes[t] - q_vals[action, ep]
      }
      q_vals[action, ep] <- q_vals[action, ep] + learnrate * PE
    }
  }

  data.frame(
    trial    = seq_len(n_trials),
    option1  = 1L,
    option2  = 2L,
    choice   = actions,
    outcome  = outcomes,
    hard_mag = hard_mag,
    prob     = prob
  )
}

#' Simulate TDRL for all participants
#'
#' @description This function takes in posterior parameters from TDRL, hard
#' task magnitude, and reward probability to simulate data using TDRL for all
#' participants, limiting trials to 50.
#'
#' @param stan_data stan data frame from real data
#' @param actual_params extracted MAPs for actual data from `extract+participant_MAPs()`
#' @param hard_mag_list output from `hard_prob_lists$hard_mag_list()`
#' @param prob_list output from `hard_prob_lists$prob_list()`
#' @param initV initialize values for q_vals
#' @param seed set seed
#' @param n_reps limits simulated trials. Default is 50
#'
#' @returns
#'
simulate_tdrl_all_participants <- function(
    stan_data,
    actual_params,
    hard_mag_list,
    prob_list,
    initV = 0,
    seed = NULL,
    n_reps = 50
) {
  N <- stan_data$num_subjects
  if (!is.null(seed)) set.seed(seed)

  sim_list <- vector("list", N)
  for (i in seq_len(N)) {
    params_i <- list(
      learnrate = actual_params$learnrate[i],
      discount  = actual_params$discount[i],
      inv_temp  = actual_params$inv_temp[i]
    )

    hard_mag_i <- truncate_to_T(hard_mag_list[[i]], n_reps)
    prob_i     <- truncate_to_T(prob_list[[i]],     n_reps)

    sim_i <- simulate_tdrl(
      params   = params_i,
      hard_mag = hard_mag_i,
      prob     = prob_i,
      initV    = initV
    )
    sim_i$id <- i
    sim_list[[i]] <- sim_i
  }

  dplyr::bind_rows(sim_list)
}

#' Pack simulated data into arrays for fitting model
#'
#' @param sim_data simulated data from 'simulate_tdrl_all_participants()'
#' @param stan_data_template stan_data from actual data
#' @param num_subjects number of subjects simulated (same as actual)
#' @param max_trials max number of trials
#' @param group_id group classification
#' @param default_RT fills in reaction time as -1 to not allow NULL values
#'
#' @returns
#'
pack_sim_into_stan_arrays <- function(
    sim_data,
    stan_data_template = NULL,
    num_subjects = NULL,
    max_trials = NULL,
    group_id = NULL,
    default_RT = -1
) {
  required_cols <- c("id", "trial", "option1", "option2", "choice", "outcome")
  missing_cols <- setdiff(required_cols, names(sim_data))
  if (length(missing_cols) > 0) {
    stop("`sim_data` is missing: ", paste(missing_cols, collapse = ", "))
  }

  sim_data <- sim_data[order(sim_data$id, sim_data$trial), ]
  ids <- sort(unique(sim_data$id))
  N_detected <- length(ids)
  trials_by_id <- tapply(sim_data$trial, sim_data$id, max)

  if (!is.null(stan_data_template)) {
    num_subjects <- stan_data_template$num_subjects
    max_trials   <- stan_data_template$max_trials
    group_id     <- stan_data_template$group_id
  } else {
    if (is.null(num_subjects)) num_subjects <- N_detected
    if (is.null(max_trials))   max_trials   <- max(trials_by_id)
    if (is.null(group_id))     group_id     <- rep(1L, num_subjects)
  }

  trials_per_subj <- as.integer(trials_by_id)
  if (length(trials_per_subj) != num_subjects) {
    stop("Mismatch between detected subjects and num_subjects. Ensure sim_data$id is 1..num_subjects.")
  }

  option1_arr <- array(1L, dim = c(num_subjects, max_trials))
  option2_arr <- array(2L, dim = c(num_subjects, max_trials))
  choice_arr  <- array(-1L, dim = c(num_subjects, max_trials))
  outcome_arr <- array(0,   dim = c(num_subjects, max_trials))
  rt_arr      <- array(default_RT, dim = c(num_subjects, max_trials))

  for (i in seq_len(num_subjects)) {
    rows_i <- sim_data[sim_data$id == i, ]
    Ti <- nrow(rows_i)
    if (Ti == 0) next
    if (Ti > max_trials) {
      stop("Subject ", i, " has ", Ti, " trials > max_trials = ", max_trials)
    }

    option1_arr[i, 1:Ti] <- as.integer(rows_i$option1)
    option2_arr[i, 1:Ti] <- as.integer(rows_i$option2)
    choice_arr[i,  1:Ti] <- as.integer(rows_i$choice)
    outcome_arr[i, 1:Ti] <- as.numeric(rows_i$outcome)
    rt_arr[i, 1:Ti] <- 1
  }

 list(
    num_subjects    = as.integer(num_subjects),
    max_trials      = as.integer(max_trials),
    group_id        = as.array(as.integer(group_id)),
    trials_per_subj = as.array(as.integer(trials_per_subj)),
    option1         = option1_arr,
    option2         = option2_arr,
    choice          = choice_arr,
    outcome         = outcome_arr,
    choiceRT        = rt_arr
  )

}

#' Extracts simulated posterior parameter MAPs
#'
#' @param pars_list simulated posterior parameters
#'
#' @returns
#'
extract_recovered_MAPs <- function(pars_list) {
  get_MAP <- function(samples) {
    dens <- density(samples)
    dens$x[which.max(dens$y)]
  }
  tibble::tibble(
    id = seq_len(ncol(pars_list$learnrate)),
    learnrate_rec = apply(pars_list$learnrate, 2, get_MAP),
    discount_rec  = apply(pars_list$discount,  2, get_MAP),
    inv_temp_rec  = apply(pars_list$inv_temp,  2, get_MAP)
  )
}

#' Combines actual and simulated posterior parameters
#'
#' @param actual_params actual posterior parameter
#' @param recovered_params simulated posterior parameters
#'
#' @returns
#'
merge_actual_recovered <- function(actual_params,
                                   recovered_params) {
  dplyr::inner_join(actual_params, recovered_params, by = "id")
}

compute_groupwise_recovery_stats <- function(df, actual_col, recovered_col, method = "pearson") {
  if (!all(c("group", actual_col, recovered_col) %in% names(df))) {
    stop("df must contain `group`, `", actual_col, "`, and `", recovered_col, "`.")
  }
  df %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(
      n = sum(stats::complete.cases(.data[[actual_col]], .data[[recovered_col]])),
      r = suppressWarnings(stats::cor(.data[[actual_col]], .data[[recovered_col]],
                                      method = method, use = "complete.obs")),
      p = tryCatch(
        stats::cor.test(.data[[actual_col]], .data[[recovered_col]], method = method, exact = FALSE)$p.value,
        error = function(e) NA_real_
      ),
      .groups = "drop"
    )
}

#' Add group labels
#'
#' @description This function takes in the actual stan data, extracts the group
#' labels and then add those group labels to the simulated/recovered data.
#'
#' @param stan_data actual stan_data
#' @param group_label_map list of group labels names if not contained
#' in stan_data
#'
#' @returns
#'
add_group_labels <- function(stan_data,
                             group_label_map = NULL) {
  if (is.null(stan_data$group_id)) {
    stop("stan_data$group_id is missing; cannot attach groups.")
  }
  g <- as.integer(stan_data$group_id)
  if (!is.null(group_label_map)) {
    if (!is.null(names(group_label_map))) {
      group <- unname(group_label_map[as.character(g)])
    } else {
      group <- group_label_map[g]
    }
  } else {
    group <- factor(g)
  }
  tibble::tibble(id = seq_along(g), group = group)
}

#' Exports stats on parameter recovery
#'
#' @param df recovered posterior parameters
#' @param actual_cols list of actual posterior parameter names
#' @param recovered_cols list of recovered posterior parameter names
#' @param output_path file path to export
#'
#' @returns
#'
write_recovery_stats_excel <- function(df,
                                       actual_cols,
                                       recovered_cols,
                                       output_path) {
  wb <- openxlsx::createWorkbook()

  # ---- Overall pooled stats ----
  pooled_stats <- purrr::map2_dfr(actual_cols,
                                  recovered_cols,
                                  function(actual_col, recovered_col) {
    ct <- stats::cor.test(df[[actual_col]], df[[recovered_col]], method = "pearson", exact = FALSE)
    tibble::tibble(
      parameter = actual_col,
      n = sum(stats::complete.cases(df[[actual_col]], df[[recovered_col]])),
      r = unname(ct$estimate),
      p = ct$p.value
    )
  })

  openxlsx::addWorksheet(wb, sheetName = "Pooled_Stats")
  openxlsx::writeData(wb, sheet = "Pooled_Stats", pooled_stats)

  # ---- Group-wise stats ----
  for (i in seq_along(actual_cols)) {
    actual_col <- actual_cols[i]
    recovered_col <- recovered_cols[i]

    stats_df <- df %>%
      dplyr::group_by(group) %>%
      dplyr::summarise(
        n = sum(stats::complete.cases(.data[[actual_col]], .data[[recovered_col]])),
        r = suppressWarnings(stats::cor(.data[[actual_col]], .data[[recovered_col]],
                                        method = "pearson", use = "complete.obs")),
        p = tryCatch(
          stats::cor.test(.data[[actual_col]], .data[[recovered_col]], method = "pearson", exact = FALSE)$p.value,
          error = function(e) NA_real_
        ),
        .groups = "drop"
      )

    openxlsx::addWorksheet(wb, sheetName = paste0(actual_col, "_Groupwise"))
    openxlsx::writeData(wb, sheet = paste0(actual_col, "_Groupwise"), stats_df)
  }

  openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
  return(output_path)
}
