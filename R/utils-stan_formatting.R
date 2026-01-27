############################################################################
# Script Name: utils-stan-formatting.R
# Purpose: Formats data for fitting models in Stan
# Author: Kasey P. Spry
# Last Modified: November 18, 2025
############################################################################

#' Get Maximum Number of Trials
#'
#' @description The max number of trials of any subject. Each row in the tibble
#'  is a trial, so this function counts the number of rows for each subject and
#'  then pulls the maximum number. This should be 50 since the data was sliced
#'  to the first 50 trials
#' @param .subject_data Subject data from [eefrt_process_data()]
#'
#' @return Returns a number with the maximum number of trials performed by any
#'   subject.
#'
get_max_trials <- function(.subject_data) {

  .subject_data %>%
    count(subject) %>%
    pull(n) %>%
    max()

}

#' Get Number of Subjects
#'
#' @description A data frame with the number of subjects.
#' @param .subject_data Subject data from [eefrt_process_data()]
#'
#' @return Returns the number of subjects in the data frame.
#'
get_num_subjects <- function(.subject_data) {

  .subject_data %>%
    count(subject) %>%
    nrow()

}

#' Get Number of trials per subject
#'
#' @description A data frame with the number of trials for each subject. Each
#'  row in the tibble is a trial, so this function counts the number of rows
#'  for each subject. This should be 50 since the data was sliced to the first
#'  50 trials
#' @param .subject_data Subject data from [eefrt_process_data()]
#'
#' @return Returns the number of trials per subjects in the data frame.
#'
get_trials_per_subj <- function(.subject_data){
  .subject_data %>%
    count(subject) %>%
    pull(n)
}

#' Format Choice Reaction Time Data
#'
#' @description Converts tidy data frame of subject data into a wide format
#'   where there is one row per subject and `n` columns (one for each trial).
#' @param .subject_data Subject data from [eefrt_process_data()]
#'
#' @return A wide data set with one row per subject and one column per trial
#'   with reaction time data.
#'
format_choice_rt <- function(.subject_data,
                             as_matrix = TRUE,
                             .keep_subject = FALSE) {

  .out <- .subject_data %>%
    select(trial, subject, choice_rt) %>%
    pivot_wider(
      id_cols = subject,
      names_from = trial,
      names_prefix = "trial_",
      values_from = choice_rt
    ) %>%
    mutate(
      across(
        .cols = -subject,
        ~replace_na(.x, -1)
      )
    )

  if (!.keep_subject) {
    .out <- .out %>%
      select(-subject)
  }

  if (as_matrix) {
    as.matrix(.out)
  } else if (!as_matrix) {
    .out
  }

}

#' Format Choice Hard Data
#'
#' @description Converts tidy data frame of subject data into a wide format
#'   where there is one row per subject and `n` columns (one for each trial).
#' @param .subject_data Subject data from [eefrt_process_data()]
#'
#' @return A wide data set with one row per subject and one column per trial
#'   with choice hard data (whether the subject chose the hard task).
#'
format_choice_hard <- function(.subject_data,
                               as_matrix = TRUE,
                               .keep_subject = FALSE) {

  .out <- .subject_data %>%
    select(trial, subject, choice_1_hard) %>%
    pivot_wider(
      id_cols = subject,
      names_from = trial,
      names_prefix = "trial_",
      values_from = choice_1_hard
    ) %>%
    mutate(
      across(
        .cols = -subject,
        ~replace_na(.x, -1)
      )
    )


  if (!.keep_subject) {
    .out <- .out %>%
      select(-subject)
  }

  if (as_matrix) {
    as.matrix(.out)
  } else if (!as_matrix) {
    .out
  }

}

#' Format Probability Data
#'
#' @description Converts tidy data frame of subject data into a wide format
#'   where there is one row per subject and `n` columns (one for each trial).
#' @param .subject_data Subject data from [eefrt_process_data()]
#'
#' @return A wide data set with one row per subject and one column per trial
#'   with probability data (the probability of receiving a reward).
#'
format_probability <- function(.subject_data,
                               as_matrix = TRUE,
                               .keep_subject = FALSE) {

  .out <- .subject_data %>%
    select(trial, subject, probability) %>%
    pivot_wider(
      id_cols = subject,
      names_from = trial,
      names_prefix = "trial_",
      values_from = probability
    ) %>%
    mutate(
      across(
        .cols = -subject,
        ~replace_na(.x, -1)
      )
    )

  if (!.keep_subject) {
    .out <- .out %>%
      select(-subject)
  }

  if (as_matrix) {
    as.matrix(.out)
  } else if (!as_matrix) {
    .out
  }

}

#' Format Reward Data
#'
#' @description Converts tidy data frame of subject data into a wide format
#'   where there is one row per subject and `n` columns (one for each trial).
#' @param .subject_data Subject data from [eefrt_process_data()]
#'
#' @return A wide data set with one row per subject and one column per trial
#'   with reward data.
#'
format_rm_actual <- function(.subject_data,
                             as_matrix = TRUE,
                             .keep_subject = FALSE) {

  .out <- .subject_data %>%
    select(trial, subject, actual_rm) %>%
    pivot_wider(
      id_cols = subject,
      names_from = trial,
      names_prefix = "trial_",
      values_from = actual_rm
    ) %>%
    mutate(
      across(
        .cols = -subject,
        ~replace_na(.x, -1)
      )
    )

  if (!.keep_subject) {
    .out <- .out %>%
      select(-subject)
  }

  if (as_matrix) {
    as.matrix(.out)
  } else if (!as_matrix) {
    .out
  }
}

#' Format Choice (Option1 = 1, Option2 = 2) for TDRL and VPRL
#'
#' @description Converts a tidy data frame of subjects data into a wide format
#'  where there is one row per subject and 'n' columns (one for each trial)
#' @param .subject_data Subject data from [eefrt_process_data()]
#'
#' @return A wide data set with one row per subject and one column per trial
#'  with option 2 data
#'

format_choice <- function(.subject_data,
                          as_matrix = TRUE,
                          .keep_subject = FALSE) {

  .out <- .subject_data %>%
    select(trial, subject, choice) %>%
    pivot_wider(
      id_cols = subject,
      names_from = trial,
      names_prefix = "trial_",
      values_from = choice
    ) %>%
    mutate(
      across(
        .cols = -subject,
        ~replace_na(.x, -1)
      )
    )

  if (!.keep_subject) {
    .out <- .out %>%
      select(-subject)
  }

  if (as_matrix) {
    # Convert to matrix AND integer
    return(matrix(as.integer(as.matrix(.out)), nrow = nrow(.out), ncol = ncol(.out)))
  } else {
    return(.out)
  }
}

#' Format Outcome for TDRL
#'
#' @description Converts a tidy data frame of subjects data into a wide format
#'    where there is one row per subject and 'n' columns (one for each trial)
#'    that contains the outcome (actual reward received)
#' @param .subject_data Subject data from [eefrt_process_data()]
#'
#' @return A wide data set with one row per subject and one column per trial
#'    with outcome (reward) info
#'

format_outcome <- function(.subject_data,
                           as_matrix = TRUE,
                           .keep_subject = FALSE) {

  .out <- .subject_data %>%
    select(trial, subject, actual_rm) %>%
    pivot_wider(
      id_cols = subject,
      names_from = trial,
      names_prefix = "trial_",
      values_from = actual_rm
    ) %>%
    mutate(
      across(
        .cols = -subject,
        ~replace_na(.x, -1)
      )
    )

  if (!.keep_subject) {
    .out <- .out %>%
      select(-subject)
  }

  if (as_matrix) {
    as.matrix(.out)
  } else if (!as_matrix) {
    .out
  }
}



#' Format Potential Reward Magnitude for High Effort Task
#'
#' @description Converts a tidy data frame of subjects data into a wide format
#'    where there is one row per subject and 'n' columns (one for each trial)
#'    that contains the reward magnitude for the high effort task that is
#'    told to the participant
#' @param .subject_data Subject data from [eefrt_process_data()]
#'
#' @return A wide data set with one row per subject and one column per trial
#'    with the Potential Reward Magnitude for High Effort Task data
#'

format_rm_hard <- function(.subject_data,
                           as_matrix = TRUE,
                           .keep_subject = FALSE) {

  .out <- .subject_data %>%
    select(trial, subject, rm_hard) %>%
    pivot_wider(
      id_cols = subject,
      names_from = trial,
      names_prefix = "trial_",
      values_from = rm_hard
    ) %>%
    mutate(
      across(
        .cols = -subject,
        ~replace_na(.x, -1)
      )
    )

  if (!.keep_subject) {
    .out <- .out %>%
      select(-subject)
  }

  if (as_matrix) {
    as.matrix(.out)
  } else if (!as_matrix) {
    .out
  }
}

#' Format Completed Trial Data
#'
#' @description Converts a tidy data frame of subjects data into a wide format
#'    where there is one row per subject and 'n' columns that contains if the
#'    participant compelted the trial with the proper number of button presses.
#' @param .subject_data Subject data from [eefrt_process_data()]
#'
#' @return A wide data set with one row per subject and one column per trial
#'    with data that shows if they completed the task (1 = completed)
#'
format_complete_trial <- function(.subject_data,
                                  as_matrix = TRUE,
                                  .keep_subject = FALSE) {

  .out <- .subject_data %>%
    select(trial, subject, complete_trial) %>%
    pivot_wider(
      id_cols = subject,
      names_from = trial,
      names_prefix = "trial_",
      values_from = complete_trial
    ) %>%
    mutate(
      across(
        .cols = -subject,
        ~replace_na(.x, -1)
      )
    )

  if (!.keep_subject) {
    .out <- .out %>%
      select(-subject)
  }

  if (as_matrix) {
    as.matrix(.out)
  } else if (!as_matrix) {
    .out
  }
}

#' Format Group Data
#'
#' @description Converts a tidy data frame of subjects data into a wide format
#'    where there is one row per subject and 'n' columns that contains the
#'    participant's group classification.
#' @param .subject_data Subject data from [eefrt_process_data()]
#'
#' @return A wide data set with one row per subject and one column per trial
#'   with data that shows what group the subject is in
#'

format_group_id <- function(.subject_data,
                            as_matrix = TRUE,
                            .keep_subject = FALSE) {
  .out <- .subject_data %>%
    distinct(subject, group) %>%
    mutate(
      group = case_when(
        group == "nonsmoker" ~ 1L, # Never TUD = 1
        group == "exsmoker" ~ 2L,  # Former TUD = 2
        group == "smoker" ~ 3L,    # Current TUD = 3
        group == "OUD smoker" ~ 4L,# TUD+OUD = 4
        TRUE ~ NA_integer_
      )
    ) %>%
    arrange(subject) %>%
    pull(group)
}

#' Formats data for fitting models in stan
#'
#' @description Combines all the data needed for fitting the models in stan
#'    into one data frame
#' @param processed_eefrt Subject data from [eefrt_process_data()]
#'
#' @return A list with stan data
#'
eefrt_prep_stan_data <- function(processed_eefrt) {
  list(
    max_trials = get_max_trials(processed_eefrt),
    num_subjects = get_num_subjects(processed_eefrt),
    trials_per_subj = get_trials_per_subj(processed_eefrt),
    choiceRT = format_choice_rt(processed_eefrt),
    chose_hard = format_choice_hard(processed_eefrt),
    probability = format_probability(processed_eefrt),
    reward = format_rm_actual(processed_eefrt),
    choice = format_choice(processed_eefrt),
    outcome = format_outcome(processed_eefrt),
    option1 = array(1, dim = c(get_num_subjects(processed_eefrt), get_max_trials(processed_eefrt))),
    option2 = array(2, dim = c(get_num_subjects(processed_eefrt), get_max_trials(processed_eefrt))),
    reward_hard = format_rm_hard(processed_eefrt),
    complete_trial = format_complete_trial(processed_eefrt),
    group_id = format_group_id(processed_eefrt)
  )
}

