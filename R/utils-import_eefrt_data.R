############################################################################
# Script Name: utils-import_eefrt_data.R
# Purpose: Imports data from EEfRT to R
# Author: Kasey P. Spry
# Last Modified: November 18, 2025
############################################################################

#' Read in Raw Data (.dat files)
#'
#' @description Read in all .dat files into a single data frame
#' @param .files .dat files containing subject data
#'
#' @return A tibble with the raw subject data
#' @export
#'
eefrt_read_raw_data <- function(
    .files
) {

  if (!all(grepl(".dat$", .files))) {
    stop("At least one file is not correct format (.dat).")
  }

  map_df(
    .x = .files,
    ~ read_table(.x),
    .id = "subject"
  )

}

#' Read in Demographic Data
#'
#' @param path path to Excel sheet that contains demographics
#'
#' @return a tibble with the raw demographic data with
#'   column names cleaned
#' @export
#'
eefrt_read_demographics <- function(path) {
  readxl::read_xlsx(path) %>%
    janitor::clean_names() %>%
    mutate(
      subject_id = as.character(subject_id)
    )
}

#' Process Raw EEFRT Data
#'
#' @description This function takes in the raw EEFRT data and cleans the column
#'   names, selects the first fifty trials for all subjects (excluding the first
#'   four trials which are practice trials), adds a trial column, cleans the
#'   subject name to reflect the data file, and formats the actual reward
#'   magnitude (in a column 'actual_rm') based on if they completed the trial,
#'   whether they chose easy or hard task, and if they won/lost.
#'
#' @param raw_eefrt The output of [eefrt_read_raw_data()].
#' @param demographic_eefrt the output of [eefrt_read_demographics()]
#' @param split_by_group Logical signaling whether or not the data frame should be
#'   split by group into a named list of data frames.
#'
#' @return A tibble of initially processed data.
#' @export
#'
eefrt_process_data <- function(
    raw_eefrt,
    demographic_eefrt,
    split_by_group = TRUE
) {

  initial <- raw_eefrt %>%
    # Clean column names
    clean_names() %>%
    group_by(subject) %>%
    # Exclude first four trials (practice trials)
    slice(5:54) %>%
    mutate(
      trial = row_number(),
      .before = subject
    ) %>%
    ungroup() %>%
    # Alter the subject column to remove the absolute file path, only including
    # the name of the subject's file.
    mutate(
      subject = fs::path_file(subject),
      subject_id = str_extract(subject, "[:digit:]{3}"),
      actual_rm = case_when(
        # If they did not complete the required number of presses,
        # reward is zero
        completed_1_yes == 0 ~ 0,
        # Completed, chose hard, and won = Reward Magnitude
        completed_1_yes == 1 & choice_1_hard == 1 & win_lose == "w" ~ rm_hard,
        # Lost = $0
        completed_1_yes == 1 & choice_1_hard == 1 & win_lose == "l" ~ 0,
        # Completed, chose easy, and won = $1
        completed_1_yes == 1 & choice_1_hard == 0 & win_lose == "w" ~ 1,
        # Lost = $0
        completed_1_yes == 1 & choice_1_hard == 0 & win_lose == "l" ~ 0
      ),
      choice = case_when(
        choice_1_hard == 0 ~ 1,
        choice_1_hard == 1 ~ 2
      ),
      complete_trial = completed_1_yes
    ) %>%
    left_join(
      demographic_eefrt %>%
        select(subject_id, group),
      by = "subject_id"
    )

  if (split_by_group) {
    groups <- initial %>%
      group_by(group) %>%
      group_keys() %>%
      pull(group)

    splits <- initial %>%
      group_by(group) %>%
      group_split()

    purrr::set_names(splits, groups)
  } else {
    initial
  }
}

#' Process Raw EEFRT Data for EEfRT Performance Statistics
#'
#' @description This function takes in the raw EEFRT data and cleans the column
#'   names, selects the first fifty trials for all subjects (excluding the first
#'   four trials which are practice ones), adds a trial column, cleans the
#'   subject name to reflect the data file, and formats the actual reward
#'   magnitude (in a column 'actual_rm') based on if they completed the trial,
#'   whether they chose easy or hard task, and if they won/lost.
#'
#' @param raw_eefrt The output of [eefrt_read_raw_data()].
#' @param demographic_eefrt the output of [eefrt_read_demographics()]
#' @param split_by_group Logical signaling whether or not the data frame should be
#'   split by group into a named list of data frames.
#'
#' @return A tibble of initially processed data.
#' @export
#'
#' @examples
eefrt_performance_process_data <- function(
    raw_eefrt,
    demographic_eefrt,
    split_by_group = TRUE
) {

  initial <- raw_eefrt %>%
    # Clean column names
    clean_names() %>%
    group_by(subject) %>%
    slice(-(1:4)) %>%
    mutate(
      trial = row_number(),
      .before = subject
    ) %>%
    ungroup() %>%
    # Alter the subject column to remove the absolute file path, only including
    # the name of the subject's file.
    mutate(
      subject = fs::path_file(subject),
      subject_id = str_extract(subject, "[:digit:]{3}"),
      actual_rm = case_when(
        # If they did not complete the required number of presses,
        # reward is zero
        completed_1_yes == 0 ~ 0,
        completed_1_yes == 1 & choice_1_hard == 1 & win_lose == "w" ~ rm_hard,
        completed_1_yes == 1 & choice_1_hard == 1 & win_lose == "l" ~ 0,
        completed_1_yes == 1 & choice_1_hard == 0 & win_lose == "w" ~ 1,
        completed_1_yes == 1 & choice_1_hard == 0 & win_lose == "l" ~ 0
      ),
      choice = case_when(
        choice_1_hard == 0 ~ 1,
        choice_1_hard == 1 ~ 2
      ),
      complete_trial = completed_1_yes
    ) %>%
    left_join(
      demographic_eefrt,
      by = "subject_id"
    )

  if (split_by_group) {
    grouped <- group_by(initial, group)

    group_names <- unlist(group_keys(grouped))

    grouped %>%
      group_split() %>%
      purrr::set_names(
        group_names
      )
  } else {
    initial
  }

}
