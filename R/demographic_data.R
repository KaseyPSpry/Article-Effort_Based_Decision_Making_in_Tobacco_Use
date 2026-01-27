############################################################################
# Script Name: demographic_data.R
# Purpose: Calculate and summarize demographic data and smoking histories
# Author: Kasey P. Spry
# Last Modified: 2025-06-20
############################################################################

############################################################################
## Calculate demographics for all groups
############################################################################

# Load Demographic Data ---------------------------------------------------
tar_load(demographic_data)

# Smoker Demographic Statistics -------------------------------------------

demographic_data_smoker <- demographic_data[demographic_data$group == 'smoker',]
smoker_n <- nrow(demographic_data_smoker)
smoker_sex <- demographic_data_smoker%>%
  count(sex)
smoker_age <- mean(demographic_data_smoker$age)
smoker_age_sd <- sd(demographic_data_smoker$age)
smoker_race <- demographic_data_smoker%>%
  count(race)%>%
  rbind(list("asian",0),list("american indian", 0),list("other",0))
smoker_edu <- mean(as.numeric(demographic_data_smoker$years_of_education))
smoker_edu_sd <- sd(as.numeric(demographic_data_smoker$years_of_education))
smoker_cigs_per_day <- mean(demographic_data_smoker$cigs_per_day)
smoker_cigs_per_day_sd <- sd(demographic_data_smoker$cigs_per_day)
smoker_sj_post_crave <- mean(demographic_data_smoker$s_jpost_crave)
smoker_sj_post_crave_sd <- sd(demographic_data_smoker$s_jpost_crave)
smoker_sj_post_negaff <- mean(demographic_data_smoker$s_jpost_negaff)
smoker_sj_post_negaff_sd <- sd(demographic_data_smoker$s_jpost_negaff)
smoker_sj_post_appdist <- mean(demographic_data_smoker$s_jpost_appdist)
smoker_sj_post_appdist_sd <- sd(demographic_data_smoker$s_jpost_appdist)
smoker_sj_post_arousal <- mean(demographic_data_smoker$s_jpost_arousal)
smoker_sj_post_arousal_sd <- sd(demographic_data_smoker$s_jpost_arousal)
smoker_sj_post_somatic <- mean(demographic_data_smoker$s_jpost_somatic)
smoker_sj_post_somatic_sd <- sd(demographic_data_smoker$s_jpost_somatic)
smoker_sj_post_habit <- mean(demographic_data_smoker$s_jpost_habit)
smoker_sj_post_habit_sd <- sd(demographic_data_smoker$s_jpost_habit)
smoker_SOWS <- mean(demographic_data_smoker$sows_total_score)
smoker_SOWS_sd <- sd(demographic_data_smoker$sows_total_score)

smoker_demographics <- c(smoker_n,
                         smoker_sex[1,2],
                         smoker_sex[2,2],
                         smoker_age,
                         smoker_age_sd,
                         smoker_race[1,2],
                         smoker_race[2,2],
                         smoker_race[3,2],
                         smoker_race[4,2],
                         smoker_race[5,2],
                         smoker_edu,
                         smoker_edu_sd,
                         smoker_cigs_per_day,
                         smoker_cigs_per_day_sd,
                         smoker_sj_post_crave,
                         smoker_sj_post_crave_sd,
                         smoker_sj_post_negaff,
                         smoker_sj_post_negaff_sd,
                         smoker_sj_post_appdist,
                         smoker_sj_post_appdist_sd,
                         smoker_sj_post_arousal,
                         smoker_sj_post_arousal_sd,
                         smoker_sj_post_somatic,
                         smoker_sj_post_somatic_sd,
                         smoker_sj_post_habit,
                         smoker_sj_post_habit_sd,
                         smoker_SOWS,
                         smoker_SOWS_sd
)



# Exsmoker Demographic Statistics -----------------------------------------

demographic_data_exsmoker <- demographic_data[demographic_data$group == 'exsmoker',]
exsmoker_n <- nrow(demographic_data_exsmoker)
exsmoker_sex <- demographic_data_exsmoker%>%
  count(sex)
exsmoker_age <- mean(demographic_data_exsmoker$age)
exsmoker_age_sd <- sd(demographic_data_exsmoker$age)
exsmoker_race <- demographic_data_exsmoker%>%
  count(race)%>%
  rbind(list("asian",0),list("american indian", 0),list("other",0))
exsmoker_edu <- mean(as.numeric(demographic_data_exsmoker$years_of_education))
exsmoker_edu_sd <- sd(as.numeric(demographic_data_exsmoker$years_of_education))
exsmoker_cigs_per_day <- mean(demographic_data_exsmoker$cigs_per_day)
exsmoker_cigs_per_day_sd <- sd(demographic_data_exsmoker$cigs_per_day)
exsmoker_sj_post_crave <- mean(demographic_data_exsmoker$s_jpost_crave, na.rm = TRUE)
exsmoker_sj_post_crave_sd <- sd(demographic_data_exsmoker$s_jpost_crave, na.rm = TRUE)
exsmoker_sj_post_negaff <- mean(demographic_data_exsmoker$s_jpost_negaff)
exsmoker_sj_post_negaff_sd <- sd(demographic_data_exsmoker$s_jpost_negaff)
exsmoker_sj_post_appdist <- mean(demographic_data_exsmoker$s_jpost_appdist)
exsmoker_sj_post_appdist_sd <- sd(demographic_data_exsmoker$s_jpost_appdist)
exsmoker_sj_post_arousal <- mean(demographic_data_exsmoker$s_jpost_arousal)
exsmoker_sj_post_arousal_sd <- sd(demographic_data_exsmoker$s_jpost_arousal)
exsmoker_sj_post_somatic <- mean(demographic_data_exsmoker$s_jpost_somatic)
exsmoker_sj_post_somatic_sd <- sd(demographic_data_exsmoker$s_jpost_somatic)
exsmoker_sj_post_habit <- mean(demographic_data_exsmoker$s_jpost_habit, na.rm = TRUE)
exsmoker_sj_post_habit_sd <- sd(demographic_data_exsmoker$s_jpost_habit, na.rm = TRUE)
exsmoker_SOWS <- mean(demographic_data_exsmoker$sows_total_score)
exsmoker_SOWS_sd <- sd(demographic_data_exsmoker$sows_total_score)

exsmoker_demographics <- c(exsmoker_n,
                           exsmoker_sex[1,2],
                           exsmoker_sex[2,2],
                           exsmoker_age,
                           exsmoker_age_sd,
                           exsmoker_race[1,2],
                           exsmoker_race[2,2],
                           exsmoker_race[3,2],
                           exsmoker_race[4,2],
                           exsmoker_race[5,2],
                           exsmoker_edu,
                           exsmoker_edu_sd,
                           exsmoker_cigs_per_day,
                           exsmoker_cigs_per_day_sd,
                           exsmoker_sj_post_crave,
                           exsmoker_sj_post_crave_sd,
                           exsmoker_sj_post_negaff,
                           exsmoker_sj_post_negaff_sd,
                           exsmoker_sj_post_appdist,
                           exsmoker_sj_post_appdist_sd,
                           exsmoker_sj_post_arousal,
                           exsmoker_sj_post_arousal_sd,
                           exsmoker_sj_post_somatic,
                           exsmoker_sj_post_somatic_sd,
                           exsmoker_sj_post_habit,
                           exsmoker_sj_post_habit_sd,
                           exsmoker_SOWS,
                           exsmoker_SOWS_sd
)
# Nonsmoker Demographic Statistics ----------------------------------------

demographic_data_nonsmoker <- demographic_data[demographic_data$group == 'nonsmoker',]
nonsmoker_n <- nrow(demographic_data_nonsmoker)
nonsmoker_sex <- demographic_data_nonsmoker%>%
  count(sex)
nonsmoker_age <- mean(demographic_data_nonsmoker$age)
nonsmoker_age_sd <- sd(demographic_data_nonsmoker$age)
nonsmoker_race <- demographic_data_nonsmoker%>%
  count(race)%>%
  rbind(list("asian",0),list("american indian", 0),list("other",0))
nonsmoker_edu <- mean(as.numeric(demographic_data_nonsmoker$years_of_education))
nonsmoker_edu_sd <- sd(as.numeric(demographic_data_nonsmoker$years_of_education))
nonsmoker_cigs_per_day <- mean(demographic_data_nonsmoker$cigs_per_day)
nonsmoker_cigs_per_day_sd <- sd(demographic_data_nonsmoker$cigs_per_day)
nonsmoker_sj_post_crave <- mean(demographic_data_nonsmoker$s_jpost_crave)
nonsmoker_sj_post_crave_sd <- sd(demographic_data_nonsmoker$s_jpost_crave)
nonsmoker_sj_post_negaff <- mean(demographic_data_nonsmoker$s_jpost_negaff)
nonsmoker_sj_post_negaff_sd <- sd(demographic_data_nonsmoker$s_jpost_negaff)
nonsmoker_sj_post_appdist <- mean(demographic_data_nonsmoker$s_jpost_appdist)
nonsmoker_sj_post_appdist_sd <- sd(demographic_data_nonsmoker$s_jpost_appdist)
nonsmoker_sj_post_arousal <- mean(demographic_data_nonsmoker$s_jpost_arousal)
nonsmoker_sj_post_arousal_sd <- sd(demographic_data_nonsmoker$s_jpost_arousal)
nonsmoker_sj_post_somatic <- mean(demographic_data_nonsmoker$s_jpost_somatic)
nonsmoker_sj_post_somatic_sd <- sd(demographic_data_nonsmoker$s_jpost_somatic)
nonsmoker_sj_post_habit <- mean(demographic_data_nonsmoker$s_jpost_habit)
nonsmoker_sj_post_habit_sd <- sd(demographic_data_nonsmoker$s_jpost_habit)
nonsmoker_SOWS <- mean(demographic_data_nonsmoker$sows_total_score)
nonsmoker_SOWS_sd <- sd(demographic_data_nonsmoker$sows_total_score)


nonsmoker_demographics <- c(nonsmoker_n,
                            nonsmoker_sex[1,2],
                            nonsmoker_sex[2,2],
                            nonsmoker_age,
                            nonsmoker_age_sd,
                            nonsmoker_race[1,2],
                            nonsmoker_race[2,2],
                            nonsmoker_race[3,2],
                            nonsmoker_race[4,2],
                            nonsmoker_race[5,2],
                            nonsmoker_edu,
                            nonsmoker_edu_sd,
                            nonsmoker_cigs_per_day,
                            nonsmoker_cigs_per_day_sd,
                            nonsmoker_sj_post_crave,
                            nonsmoker_sj_post_crave_sd,
                            nonsmoker_sj_post_negaff,
                            nonsmoker_sj_post_negaff_sd,
                            nonsmoker_sj_post_appdist,
                            nonsmoker_sj_post_appdist_sd,
                            nonsmoker_sj_post_arousal,
                            nonsmoker_sj_post_arousal_sd,
                            nonsmoker_sj_post_somatic,
                            nonsmoker_sj_post_somatic_sd,
                            nonsmoker_sj_post_habit,
                            nonsmoker_sj_post_habit_sd,
                            nonsmoker_SOWS,
                            nonsmoker_SOWS_sd
)

# OUDsmoker Demographic Statistics ----------------------------------------

demographic_data_OUD.smoker <- demographic_data[demographic_data$group == 'OUD smoker',]
OUD.smoker_n <- nrow(demographic_data_OUD.smoker)
OUD.smoker_sex <- demographic_data_OUD.smoker%>%
  count(sex)
OUD.smoker_age <- mean(demographic_data_OUD.smoker$age)
OUD.smoker_age_sd <- sd(demographic_data_OUD.smoker$age)
OUD.smoker_race <- demographic_data_OUD.smoker%>%
  count(race)%>%
  rbind(list("asian",0),list("american indian", 0),list("other",0))
OUD.smoker_edu <- mean(as.numeric(demographic_data_OUD.smoker$years_of_education))
OUD.smoker_edu_sd <- sd(as.numeric(demographic_data_OUD.smoker$years_of_education))
OUD.smoker_cigs_per_day <- mean(demographic_data_OUD.smoker$cigs_per_day)
OUD.smoker_cigs_per_day_sd <- sd(demographic_data_OUD.smoker$cigs_per_day)
OUD.smoker_sj_post_crave <- mean(demographic_data_OUD.smoker$s_jpost_crave)
OUD.smoker_sj_post_crave_sd <- sd(demographic_data_OUD.smoker$s_jpost_crave)
OUD.smoker_sj_post_negaff <- mean(demographic_data_OUD.smoker$s_jpost_negaff)
OUD.smoker_sj_post_negaff_sd <- sd(demographic_data_OUD.smoker$s_jpost_negaff)
OUD.smoker_sj_post_appdist <- mean(demographic_data_OUD.smoker$s_jpost_appdist)
OUD.smoker_sj_post_appdist_sd <- sd(demographic_data_OUD.smoker$s_jpost_appdist)
OUD.smoker_sj_post_arousal <- mean(demographic_data_OUD.smoker$s_jpost_arousal)
OUD.smoker_sj_post_arousal_sd <- sd(demographic_data_OUD.smoker$s_jpost_arousal)
OUD.smoker_sj_post_somatic <- mean(demographic_data_OUD.smoker$s_jpost_somatic)
OUD.smoker_sj_post_somatic_sd <- sd(demographic_data_OUD.smoker$s_jpost_somatic)
OUD.smoker_sj_post_habit <- mean(demographic_data_OUD.smoker$s_jpost_habit)
OUD.smoker_sj_post_habit_sd <- sd(demographic_data_OUD.smoker$s_jpost_habit)
OUD.smoker_SOWS <- mean(demographic_data_OUD.smoker$sows_total_score)
OUD.smoker_SOWS_sd <- sd(demographic_data_OUD.smoker$sows_total_score)

OUD.smoker_demographics <- c(OUD.smoker_n,
                             OUD.smoker_sex[1,2],
                             OUD.smoker_sex[2,2],
                             OUD.smoker_age,
                             OUD.smoker_age_sd,
                             OUD.smoker_race[1,2],
                             OUD.smoker_race[2,2],
                             OUD.smoker_race[3,2],
                             OUD.smoker_race[4,2],
                             OUD.smoker_race[5,2],
                             OUD.smoker_edu,
                             OUD.smoker_edu_sd,
                             OUD.smoker_cigs_per_day,
                             OUD.smoker_cigs_per_day_sd,
                             OUD.smoker_sj_post_crave,
                             OUD.smoker_sj_post_crave_sd,
                             OUD.smoker_sj_post_negaff,
                             OUD.smoker_sj_post_negaff_sd,
                             OUD.smoker_sj_post_appdist,
                             OUD.smoker_sj_post_appdist_sd,
                             OUD.smoker_sj_post_arousal,
                             OUD.smoker_sj_post_arousal_sd,
                             OUD.smoker_sj_post_somatic,
                             OUD.smoker_sj_post_somatic_sd,
                             OUD.smoker_sj_post_habit,
                             OUD.smoker_sj_post_habit_sd,
                             OUD.smoker_SOWS,
                             OUD.smoker_SOWS_sd
)

# Combine into a single data frame
demographics_df<-data.frame(smoker = unlist(smoker_demographics),
                            exsmoker = unlist(exsmoker_demographics),
                            neversmoker = unlist(nonsmoker_demographics),
                            OUD.smoker = unlist(OUD.smoker_demographics),
                            row.names = c("number of participants (n=)",
                                          "Sex(F)",
                                          "Sex(M)",
                                          "Age",
                                          "Age SD",
                                          "Race (Black)",
                                          "Race (White)",
                                          "Race (Asian)",
                                          "Race (American Indian)",
                                          "Race (Other)",
                                          "Years of Education",
                                          "Years of Education SD",
                                          "Cigarettes Per Day",
                                          "Cigarettes Per Day SD",
                                          "Shiffman-Jarvik Craving Score",
                                          "Shiffman-Jarvik Craving Score SD",
                                          "Shiffman-Jarvik Negative Affect Score",
                                          "Shiffman-Jarvik Negative Affect Score SD",
                                          "Shiffman-Jarvik Arousal Score",
                                          "Shiffman-Jarvik Arousal Score SD",
                                          "Shiffman-Jarvik Somatic Symptoms Score",
                                          "Shiffman-Jarvik Somatic Symptoms  Score SD",
                                          "Shiffman-Jarvik Appetite Score",
                                          "Shiffman-Jarvik Appetite Score SD",
                                          "Shiffman-Jarvik Habit Withdrawal Score",
                                          "Shiffman-Jarvik Habit Withdrawal Score SD",
                                          "SOWS Score",
                                          "SOWS Score SD")
)
demographics_df

# Data frame that excludes Never TUD
demographic_data_sud <- demographic_data[demographic_data$group != 'nonsmoker',]
demographic_data_sud

# Data frame that excludes Never TUD and TUD+OUD for Shiffman Jarvik Craving Analysis
demographic_data_sj_craving <- demographic_data[!(demographic_data$group %in% c('nonsmoker', 'OUD smoker')), ]

# Data frame that excludes TUD+OUD for Shiffman Jarvik Analysis
demographic_data_sj <- demographic_data[demographic_data$group != 'OUD smoker',]

# Saves demographics to an excel file
library("openxlsx")
write.xlsx(demographics_df,
           file = file.path("Results","Demographics_all groups.xlsx"),
           colNames = TRUE,
           rowNames = TRUE)

############################################################################
## Significance Testing for demographics and smoking histories
############################################################################

age_sig <- aov(age~group, data = demographic_data)
summary(age_sig)
TukeyHSD(age_sig)
pairwise.t.test(demographic_data$age,
                demographic_data$group,
                p.adjust.method = "bonferroni")


race_sig <- chisq.test(demographic_data$race, demographic_data$group, correct = FALSE)
head(race_sig)
race_sig$stdres

sex_sig <- chisq.test(demographic_data$sex, demographic_data$group, correct = FALSE)
head(sex_sig)

n_table <- as.table(rbind(c(nrow(demographic_data_nonsmoker),
                            nrow(demographic_data_exsmoker),
                            nrow(demographic_data_smoker),
                            nrow(demographic_data_OUD.smoker)
)
)
)
colnames(n_table) <- c("Never TUD", "Former TUD", "Current TUD", "TUD+OUD")
n_sig <- chisq.test(n_table)
head(n_sig)

cigs_per_day_sig_sud <- aov(cigs_per_day~group, data=demographic_data_sud)
summary(cigs_per_day_sig_sud)

edu_sig <- aov(years_of_education~group, data=demographic_data)
summary(edu_sig)
TukeyHSD(edu_sig)
pairwise.t.test(demographic_data$years_of_education,
                demographic_data$group,
                p.adjust.method = "bonferroni")

sj_post_craving_sig <- aov(s_jpost_crave~group, data = demographic_data_sj_craving)
summary(sj_post_craving_sig)

sj_post_negaff_sig <- aov(s_jpost_negaff~group, data = demographic_data_sj)
summary(sj_post_negaff_sig)

sj_post_arousal_sig <- aov(s_jpost_arousal~group, data = demographic_data_sj)
summary(sj_post_arousal_sig)

sj_post_somatic_sig <- aov(s_jpost_somatic~group, data = demographic_data_sj)
summary(sj_post_somatic_sig)

sj_post_appetite_sig <- aov(s_jpost_appdist~group, data = demographic_data_sj)
summary(sj_post_appetite_sig)

sj_post_habit_sig <- aov(s_jpost_habit~group, data = demographic_data_sj_craving)
summary(sj_post_habit_sig)

############################################################################
##  Analysis between TDRL parameters and demographics
############################################################################

get_MAP <- function(samples) {
  dens <- density(samples)
  dens$x[which.max(dens$y)]
}

learnrate_individual_level_MAP <- apply(pars_TDRL$learnrate, 2, get_MAP)
discount_individual_level_MAP <- apply(pars_TDRL$discount, 2, get_MAP)
inv_temp_individual_level_MAP <- apply(pars_TDRL$inv_temp, 2, get_MAP)

tdrl_individual_params <- data.frame(
  learnrate = learnrate_individual_level_MAP,
  discount = discount_individual_level_MAP,
  inv_temp = inv_temp_individual_level_MAP
)

# Group race labels
race_grouped <- tolower(trimws(demographic_data$race))
race_grouped <- ifelse(race_grouped == "white", "White", "Minority")
race_grouped <- factor(race_grouped, levels = c("White", "Minority"))

# Learnrate
individual_learnrate <- tdrl_individual_params$learnrate

corr_learnrate_age <- cor.test(individual_learnrate, demographic_data$age)
corr_learnrate_age

corr_learnrate_edu <- cor.test(individual_learnrate, demographic_data$years_of_education)
corr_learnrate_edu

aov_learnrate_race <- aov(individual_learnrate~race_grouped)
summary(aov_learnrate_race)


# discount
individual_discount <- tdrl_individual_params$discount

corr_discount_age <- cor.test(individual_discount, demographic_data$age)
corr_discount_age
df_discount_age <- data.frame(
  individual_discount = individual_discount,
  age = demographic_data$age
)
discount_age <- ggplot(df_discount_age, aes(x = age, y = individual_discount)) +
  geom_point(color = "black", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  ) +
  labs(
    title = NULL,
    subtitle = NULL,
    x = "Age",
    y = "γ"
  )
discount_age

ggsave(
  filename = file.path('Figures','Corr_plot_discount_age.png'),
  plot = discount_age,
  width = 3.5,
  height = 3.5,
  units = "in",
  dpi = 400
)

corr_discount_edu <- cor.test(individual_discount, demographic_data$years_of_education)
corr_discount_edu
df_discount_edu <- data.frame(
  individual_discount = individual_discount,
  edu = demographic_data$years_of_education
)
discount_edu <- ggplot(df_discount_edu, aes(x = edu, y = individual_discount)) +
  geom_point(color = "black", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  ) +
  scale_x_continuous(breaks = seq(min(df_discount_edu$edu),max(df_discount_edu$edu),3)) +
  labs(
    title = NULL,
    subtitle = NULL,
    x = "Years of Education",
    y = "γ"
  )
discount_edu

ggsave(
  filename = file.path('Figures', 'Corr_plot_discount_edu.png'),
  plot = discount_edu,
  width = 3.5,
  height = 3.5,
  units = "in",
  dpi = 400
)

aov_discount_race <- aov(individual_discount~race_grouped)
summary(aov_discount_race)
discount_race_fig <- ggplot(data.frame(discount = individual_discount, race = race_grouped),
                            aes(x = race, y = discount)) +
  # geom_violin(trim = FALSE, fill = "lightblue") +
  geom_boxplot(width = 0.3, fill = "gray") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  ) +
  labs(title = NULL,
       y = "γ",
       x = "Race")
discount_race_fig

ggsave(
  filename = file.path('Figures','Plot_discount_race.png'),
  plot = discount_race_fig,
  width = 3.5,
  height = 3.5,
  units = "in",
  dpi = 400
)

# inv_temp
individual_inv_temp <- tdrl_individual_params$inv_temp

corr_inv_temp_age <- cor.test(individual_inv_temp, demographic_data$age)
corr_inv_temp_age

corr_inv_temp_edu <- cor.test(individual_inv_temp, demographic_data$years_of_education)
corr_inv_temp_edu

aov_inv_temp_race <- aov(individual_inv_temp~race_grouped)
summary(aov_inv_temp_race)
inv_temp_race_fig <- ggplot(data.frame(inv_temp = individual_inv_temp, race = race_grouped),
                            aes(x = race, y = inv_temp)) +
  # geom_violin(trim = FALSE, fill = "lightblue") +
  geom_boxplot(width = 0.3, fill = "gray") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  ) +
  labs(title = NULL,
       y = "β",
       x = "Race")
inv_temp_race_fig

ggsave(
  filename = file.path('Figures','Plot_inv_temp_race.png'),
  plot = inv_temp_race_fig,
  width = 3.5,
  height = 3.5,
  units = "in",
  dpi = 400
)
