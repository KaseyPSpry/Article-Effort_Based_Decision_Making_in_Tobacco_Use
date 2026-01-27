############################################################################
# Script Name: posterior_parameters_stats.R
# Purpose: Calculates group level posterior parameter stats
# Author: Kasey P. Spry
# Last Modified: December 2, 2025
############################################################################

# Load Data
tar_load(pars_TDRL)
tar_load(stan_data)

# Function to split by group
split_by_group <- function(param_matrix, group_ids) {
  split(1:nrow(param_matrix), group_ids) |>
    lapply(function(idx) param_matrix[idx, , drop = FALSE])
}

# Create lists for each parameter
learnrate_by_group <- split_by_group(pars_TDRL$learnrate, stan_data$group_id)
discount_by_group  <- split_by_group(pars_TDRL$discount, stan_data$group_id)
inv_temp_by_group  <- split_by_group(pars_TDRL$inv_temp, stan_data$group_id)

pars_TDRL_list <- list(mu_learnrate_group = pars_TDRL$mu_learnrate_group[,1],
                                  mu_discount_group = pars_TDRL$mu_discount_group[,1],
                                  mu_inv_temp_group = pars_TDRL$mu_inv_temp_group[,1],
                                  learnrate = pars_TDRL$learnrate[,1],
                                  discount = pars_TDRL$discount[,1],
                                  inv_temp = pars_TDRL$inv_temp[,1])


get_MAP <- function(samples) {
  dens <- density(samples)
  dens$x[which.max(dens$y)]
}

############################################################################
## Group Level learning rate (α)
############################################################################
learnrate_NeverTUD <- pars_TDRL$mu_learnrate_group[,1]
learnrate_NeverTUD_MAP <- get_MAP(learnrate_NeverTUD)
learnrate_NeverTUD_hdi <- HDInterval::hdi(learnrate_NeverTUD, credMass = 0.95)

learnrate_FormerTUD <- pars_TDRL$mu_learnrate_group[,2]
learnrate_FormerTUD_MAP <- get_MAP(learnrate_FormerTUD)
learnrate_FormerTUD_hdi <- HDInterval::hdi(learnrate_FormerTUD, credMass = 0.95)

learnrate_TUD <- pars_TDRL$mu_learnrate_group[,3]
learnrate_TUD_MAP <- get_MAP(learnrate_TUD)
learnrate_TUD_hdi <- HDInterval::hdi(learnrate_TUD, credMass = 0.95)

learnrate_TUD_OUD <- pars_TDRL$mu_learnrate_group[,4]
learnrate_TUD_OUD_MAP <- get_MAP(learnrate_TUD_OUD)
learnrate_TUD_OUD_hdi <- HDInterval::hdi(learnrate_TUD_OUD, credMass = 0.95)

############################################################################
## Group Level discount factor (γ)
############################################################################

discount_NeverTUD <- pars_TDRL$mu_discount_group[,1]
discount_NeverTUD_MAP <- get_MAP(discount_NeverTUD)
discount_NeverTUD_hdi <- HDInterval::hdi(discount_NeverTUD, credMass = 0.95)

discount_FormerTUD <- pars_TDRL$mu_discount_group[,2]
discount_FormerTUD_MAP <- get_MAP(discount_FormerTUD)
discount_FormerTUD_hdi <- HDInterval::hdi(discount_FormerTUD, credMass = 0.95)

discount_TUD <- pars_TDRL$mu_discount_group[,3]
discount_TUD_MAP <- get_MAP(discount_TUD)
discount_TUD_hdi <- HDInterval::hdi(discount_TUD, credMass = 0.95)

discount_TUD_OUD <- pars_TDRL$mu_discount_group[,4]
discount_TUD_OUD_MAP <- get_MAP(discount_TUD_OUD)
discount_TUD_OUD_hdi <- HDInterval::hdi(discount_TUD_OUD, credMass = 0.95)

############################################################################
## Group Level inv_temp (τ)
############################################################################

inv_temp_NeverTUD <- pars_TDRL$mu_inv_temp_group[,1]
inv_temp_NeverTUD_MAP <- get_MAP(inv_temp_NeverTUD)
inv_temp_NeverTUD_hdi <- HDInterval::hdi(inv_temp_NeverTUD, credMass = 0.95)

inv_temp_FormerTUD <- pars_TDRL$mu_inv_temp_group[,2]
inv_temp_FormerTUD_MAP <- get_MAP(inv_temp_FormerTUD)
inv_temp_FormerTUD_hdi <- HDInterval::hdi(inv_temp_FormerTUD, credMass = 0.95)

inv_temp_TUD <- pars_TDRL$mu_inv_temp_group[,3]
inv_temp_TUD_MAP <- get_MAP(inv_temp_TUD)
inv_temp_TUD_hdi <- HDInterval::hdi(inv_temp_TUD, credMass = 0.95)

inv_temp_TUD_OUD <- pars_TDRL$mu_inv_temp_group[,4]
inv_temp_TUD_OUD_MAP <- get_MAP(inv_temp_TUD_OUD)
inv_temp_TUD_OUD_hdi <- HDInterval::hdi(inv_temp_TUD_OUD, credMass = 0.95)

############################################################################
## Combine get_MAP and HDI for all groups and all parameters
## into a table and save the table
############################################################################

NeverTUD_MAP_hdi <- c(
  learnrate_MAP = learnrate_NeverTUD_MAP,
  learnrate_hdi_lower = learnrate_NeverTUD_hdi[1],
  learnrate_hdi_upper = learnrate_NeverTUD_hdi[2],
  discount_MAP = discount_NeverTUD_MAP,
  discount_hdi_lower = discount_NeverTUD_hdi[1],
  discount_hdi_upper = discount_NeverTUD_hdi[2],
  inv_temp_MAP = inv_temp_NeverTUD_MAP,
  inv_temp_hdi_lower = inv_temp_NeverTUD_hdi[1],
  inv_temp_hdi_upper = inv_temp_NeverTUD_hdi[2]
)

FormerTUD_MAP_hdi <- c(
  learnrate_MAP = learnrate_FormerTUD_MAP,
  learnrate_hdi_lower = learnrate_FormerTUD_hdi[1],
  learnrate_hdi_upper = learnrate_FormerTUD_hdi[2],
  discount_MAP = discount_FormerTUD_MAP,
  discount_hdi_lower = discount_FormerTUD_hdi[1],
  discount_hdi_upper = discount_FormerTUD_hdi[2],
  inv_temp_MAP = inv_temp_FormerTUD_MAP,
  inv_temp_hdi_lower = inv_temp_FormerTUD_hdi[1],
  inv_temp_hdi_upper = inv_temp_FormerTUD_hdi[2]
)

TUD_MAP_hdi <- c(
  learnrate_MAP = learnrate_TUD_MAP,
  learnrate_hdi_lower = learnrate_TUD_hdi[1],
  learnrate_hdi_upper = learnrate_TUD_hdi[2],
  discount_MAP = discount_TUD_MAP,
  discount_hdi_lower = discount_TUD_hdi[1],
  discount_hdi_upper = discount_TUD_hdi[2],
  inv_temp_MAP = inv_temp_TUD_MAP,
  inv_temp_hdi_lower = inv_temp_TUD_hdi[1],
  inv_temp_hdi_upper = inv_temp_TUD_hdi[2]
)

TUD_OUD_MAP_hdi <- c(
  learnrate_MAP = learnrate_TUD_OUD_MAP,
  learnrate_hdi_lower = learnrate_TUD_OUD_hdi[1],
  learnrate_hdi_upper = learnrate_TUD_OUD_hdi[2],
  discount_MAP = discount_TUD_OUD_MAP,
  discount_hdi_lower = discount_TUD_OUD_hdi[1],
  discount_hdi_upper = discount_TUD_OUD_hdi[2],
  inv_temp_MAP = inv_temp_TUD_OUD_MAP,
  inv_temp_hdi_lower = inv_temp_TUD_OUD_hdi[1],
  inv_temp_hdi_upper = inv_temp_TUD_OUD_hdi[2]
)

df_MAP_hdi <- data.frame(NeverTUD = NeverTUD_MAP_hdi,
                         FormerTUD = FormerTUD_MAP_hdi,
                         TUD = TUD_MAP_hdi,
                         TUD_OUD = TUD_OUD_MAP_hdi)

openxlsx::write.xlsx(df_MAP_hdi,
           file = file.path("Results", "Group-level TDRL Posterior Parameters MAP and HDI.xlsx"),
           rowNames = TRUE,
           colNames = TRUE
           )
