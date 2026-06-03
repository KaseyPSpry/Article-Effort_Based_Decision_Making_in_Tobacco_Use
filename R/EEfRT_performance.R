############################################################################
# Script Name: EEfRT_performance.R
# Purpose: Calculates statistics for EEfRT performance
# Author: Kasey P. Spry
# Last Modified: November 24, 2025
############################################################################

library(dplyr)
library(ggplot2)
library(tibble)


# Pull data from target objects -------------------------------------------
tar_load(eefrt_performance_data)

performance_data_TUD <- eefrt_performance_data[["smoker"]]
performance_data_formerTUD <- eefrt_performance_data[["exsmoker"]]
performance_data_neverTUD <- eefrt_performance_data[["nonsmoker"]]
performance_data_TUDOUD <- eefrt_performance_data[["OUD smoker"]]

performance_data_combined <- rbind(performance_data_TUD, performance_data_formerTUD, performance_data_neverTUD, performance_data_TUDOUD)

# Filters for the first 50 trials -----------------------------------------
performance_data_combined_first50 <- performance_data_combined %>%
  group_by(subject) %>%
  slice_head(n = 50) %>%
  ungroup()

# Filters for participants who did not have 50 or more trials -------------
subj_trials_less_than_50 <- performance_data_combined%>%
  count(subject) %>%
  filter(n < 50)
print(subj_trials_less_than_50)

# Excludes participants who did not complete at least 50 trials from dataset
performance_data_combined_filtered <- performance_data_combined %>%
  filter(!(subject %in% subj_trials_less_than_50[[1]]))

# Excludes from group datasets
performance_data_TUD_filtered <- performance_data_TUD %>%
  filter(!(subject %in% subj_trials_less_than_50[[1]]))

performance_data_formerTUD_filtered <- performance_data_formerTUD %>%
  filter(!(subject %in% subj_trials_less_than_50[[1]]))

performance_data_neverTUD_filtered <- performance_data_neverTUD %>%
  filter(!(subject %in% subj_trials_less_than_50[[1]]))

performance_data_TUDOUD_filtered <- performance_data_TUDOUD %>%
  filter(!(subject %in% subj_trials_less_than_50[[1]]))

performance_data_combined_first50_filtered <- performance_data_combined_first50 %>%
  filter(!(subject %in% subj_trials_less_than_50[[1]]))


# Combines stan data output -----------------------------------------------
tar_load(stan_data)
stan_data_list <- stan_data
group_labels <- c(
  "Never TUD",
  "Former TUD",
  "TUD",
  "TUD_OUD"
)

stan_data_list$group <- group_labels[ stan_data_list$group_id ]
stan_data_list <- bind_rows(stan_data_list)


# Number of Trials Per Subject Per Group ------------------------------------

# Average(avg) and Standard Deviation (sd) calculations
# All groups pooled
trials <- table(performance_data_combined_filtered$subject)

trials_avg <- mean(trials)
trials_avg
trials_sd <- sd(trials)
trials_sd
trials_range <- range(trials)
trials_range

# Current Tobacco Use Disorder
trials_TUD <- table(performance_data_TUD_filtered$subject)
trials_TUD_df <- data.frame(
  Group = rep("TUD", 26),
  trials_avg = trials_TUD
)
trials_avg_TUD <- mean(table(performance_data_TUD_filtered$subject))
trials_avg_TUD
trials_sd_TUD <- sd(trials_TUD)
trials_sd_TUD
trials_range_TUD <- range(trials_TUD)
trials_range_TUD

# Former Tobacco Use Disorder
trials_formerTUD <- table(performance_data_formerTUD_filtered$subject)
trials_formerTUD_df <- data.frame(
  Group = rep("Former TUD", 22),
  trials_avg = trials_formerTUD
)
trials_avg_formerTUD <- mean(table(performance_data_formerTUD_filtered$subject))
trials_avg_formerTUD
trials_sd_formerTUD <- sd(trials_formerTUD)
trials_sd_formerTUD
trials_range_formerTUD <- range(trials_formerTUD)
trials_range_formerTUD

# Never Tobacco Use Disorder
trials_neverTUD <- table(performance_data_neverTUD_filtered$subject)
trials_neverTUD_df <- data.frame(
  Group = rep("Never TUD", 23),
  trials_avg = trials_neverTUD
)
trials_avg_neverTUD <- mean(table(performance_data_neverTUD_filtered$subject))
trials_avg_neverTUD
trials_sd_neverTUD <- sd(trials_neverTUD)
trials_sd_neverTUD
trials_range_neverTUD <- range(trials_neverTUD)
trials_range_neverTUD

# TUD+OUD
trials_TUD_OUD <- table(performance_data_TUDOUD_filtered$subject)
trials_TUD_OUD_df <- data.frame(
  Group = rep("TUD+OUD", 29),
  trials_avg = trials_TUD_OUD
)
trials_avg_TUD_OUD <- mean(table(performance_data_TUDOUD_filtered$subject))
trials_avg_TUD_OUD
trials_sd_TUD_OUD <- sd(trials_TUD_OUD)
trials_sd_TUD_OUD
trials_range_TUD_OUD <- range(trials_TUD_OUD)
trials_range_TUD_OUD

# Significance Testing for Number of Trials between Groups
data_trials <- data.frame(
  Group = c("TUD", "Never TUD", "Former TUD", "TUD+OUD"),
  Trials_avg = c(trials_avg_TUD, trials_avg_neverTUD, trials_avg_formerTUD, trials_avg_TUD_OUD),
  SD = c(trials_sd_TUD, trials_sd_neverTUD, trials_sd_formerTUD, trials_sd_TUD_OUD)
)

data_trials_combined <- rbind(trials_neverTUD_df, trials_formerTUD_df, trials_TUD_df, trials_TUD_OUD_df)

anova_trials <- aov(trials_avg.Freq ~ Group, data = data_trials_combined)
summary(anova_trials)

# Barplot with Error Bars and Individual Points to Display Number of Trials per Group
model_color_vec = values = c(
  "Former TUD" = "#008C95",
  "Never TUD" = "#A7A8A9",
  "TUD" = "#9E7E38",
  "TUD+OUD" = "#6BA539"
)

fig_trials_per_group <- ggplot(data_trials,
                               aes(
                                 x = Group,
                                 y = Trials_avg,
                                 fill = Group)
                        ) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +
  geom_errorbar(aes(ymin = trials_avg - SD, ymax = Trials_avg + SD), width = 0.2) +
  geom_jitter(data = data_trials_combined,
              aes(
                x = Group,
                y = trials_avg.Freq
              ),
              width = 0.15, size = 2, alpha = 0.6, color = "black") +
  labs(title = "Number of Trials per Group",
       x = element_blank(),
       y = "Number of Trials") +
  cowplot::theme_cowplot() +
  scale_fill_manual(
    values = model_color_vec
  ) +
  scale_color_manual(
    values = model_color_vec
  ) +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = ggtext::element_textbox(
          size = 14,
          face = "bold",
          box.color = "#000000",
          linewidth = 0.6,
          halign = 0.5,
          linetype = 1,
          r = unit(5, "pt"),
          width = unit(1, "npc"),
          padding = margin(2, 0, 1, 0),
          margin = margin(3, 3, 3, 3)
        ),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        strip.text.y = element_blank(),
        #   plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        plot.background = element_blank())
fig_trials_per_group

ggsave(filename = file.path('Figures','Trials_per_Group.png'),
       plot = fig_trials_per_group,
       width = 6,
       height = 5,
       units = "in",
       dpi = 400)

# High and easy effort selections in the first 50 trials ---------------------------

#Average (avg) and Standard Deviation (sd)
# All groups pooled

# High effort task
chose_hard <- performance_data_combined_first50 %>%
  filter(choice_1_hard == 1)

chose_hard_avg <- (mean(performance_data_combined_first50$choice_1_hard))*100
chose_hard_avg

chose_hard_completed <- chose_hard$completed_1_yes
chose_hard_completed_avg <- mean(chose_hard_completed)*100
chose_hard_completed_avg

# Searches for participants who only chose the hard task
hard_only <- performance_data_combined_first50 %>%
  group_by(subject) %>%
  summarise(
    hard_only = all(choice_1_hard == 1),
  ) %>%
  filter(hard_only)
hard_only

# Low effort task
chose_easy <- performance_data_combined_first50 %>%
  filter(choice_1_hard == 0)

chose_easy_completed <- chose_easy$completed_1_yes
chose_easy_completed_avg <- mean(chose_easy_completed)*100
chose_easy_completed_avg

# Searches for participants who only chose the easy task
easy_only <- performance_data_combined_first50 %>%
  group_by(subject) %>%
  summarise(
    easy_only = all(choice_1_hard == 0),
  ) %>%
  filter(easy_only)
easy_only

# High effort selections by probability level
high_effort_selections_by_prob <- performance_data_combined_first50 %>%
  group_by(probability) %>%
  summarise(
    percent_high_effort = mean(choice_1_hard, na.rm = TRUE) * 100
  )

# High effort selections between groups
subject_summary <- performance_data_combined_first50 %>%
  group_by(subject, group) %>%  # Replace subject_id with your actual subject identifier column name
  summarise(prop_hard = mean(choice_1_hard),
            prop_easy = 1 - mean(choice_1_hard)) %>%
  ungroup()

anova_hard_selection <- aov(prop_hard ~ group, data = subject_summary)
summary(anova_hard_selection)

anova_easy_selection <- aov(prop_easy ~ group, data = subject_summary)
summary(anova_easy_selection)

# Ratio of high effort completed / selected
subject_summary_hard <- chose_hard %>%
  group_by(subject, group) %>%
  summarise(prop_completion = mean(completed_1_yes)) %>%
  ungroup()

anova_hard_ratio <- aov(prop_completion ~ group, data = subject_summary_hard)
summary(anova_hard_ratio)

# Ratio of low effort completed / selected
subject_summary_easy <- chose_easy %>%
  group_by(subject, group) %>%
  summarise(prop_completion = mean(completed_1_yes)) %>%
  ungroup()

anova_easy_ratio <- aov(prop_completion ~ group, data = subject_summary_easy)
summary(anova_easy_ratio)
# Save Results to an Excel File -------------------------------------------

EEfRT_performance_results <- list("Total Trials - Group - Average" = trials_avg,
                                  "Total Trials - Group - SD" = trials_sd,
                                  "Total Trials - Group - Range" = setNames(trials_range, c("Min", "Max")),
                                  "Total Trials - TUD - Average" = trials_avg_TUD,
                                  "Total Trials - TUD - SD" = trials_sd_TUD,
                                  "Total Trials - TUD - Range" = setNames(trials_range_TUD, c("Min", "Max")),
                                  "Total Trials - Former TUD - Average" = trials_avg_formerTUD,
                                  "Total Trials - Former TUD - SD" = trials_sd_formerTUD,
                                  "Total Trials - Former TUD - Range" = setNames(trials_range_formerTUD, c("Min", "Max")),
                                  "Total Trials - Never TUD - Average" = trials_avg_neverTUD,
                                  "Total Trials - Never TUD - SD" =  trials_sd_neverTUD,
                                  "Total Trials - Never TUD - Range" =  setNames(trials_range_neverTUD, c("Min", "Max")),
                                  "Total Trials - TUD+OUD - Average" =  trials_avg_TUD_OUD,
                                  "Total Trials - TUD+OUD - SD" =   trials_sd_TUD_OUD,
                                  "Total Trials - TUD+OUD - Range" = setNames(trials_range_TUD_OUD, c("Min", "Max"))
                                        )

EEfRT_performance_results_long <- do.call(
  rbind,
  lapply(names(EEfRT_performance_results), function(name) {
    values <- EEfRT_performance_results[[name]]

    # If no names, assign "Value"
    if (is.null(names(values))) names(values) <- "Value"

    data.frame(
      Metric = name,
      Submetric = names(values),
      Value = as.numeric(values),
      row.names = NULL
    )
  })
)

openxlsx::write.xlsx(
  EEfRT_performance_results_long,
  file = file.path("Results", "EEfRT_Performance.xlsx"),
  sheetName = "Performance"
)
