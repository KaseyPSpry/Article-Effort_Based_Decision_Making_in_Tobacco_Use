# Article-Effort_Based_Decision_Making_in_Tobacco_Use
This repository contains the scripts and data used in "Effort and Substance Use: Differentiating Tobacco Use Through Reinforcement Learning of Effort Based Decision Making"

# Data
To obtain raw data for the EEfRT task and participant demographics, reach out to either Kasey Spry (kasey.spry@wfusm.edu) or Dr. Merideth Addicott (merideth.addicott@wfusm.edu).

# Overview of analysis
Analysis scripts have been coded in R and Stan. The required packages and libraries have been specified in _targets.R or other supporting scripts.

The R package `targets` was used as a pipeline tool. More information on `targets` can be found on the [Github package page](https://github.com/ropensci/targets). 

To run this analysis follow the following steps:
  1. Load packages in `_targets.R`
  2. Run `tar_make()`
  3. Run supports R scripts if necessary

# Scripts
`_targets.R` pipeline script to run analysis

`stan_files/stan_eefrt_full_sv.stan` stan file that runs Full SV Model 

`stan_files/stan_eefrt_rewardonly.stan` stan file that runs Reward Only SV Model

`stan_files/stan_eefrt_tdrl.stan` stan file that runs TDRL Model

`R/utils-import_eefrt_data.R` Imports and processes/cleans raw EEfRT data that is contained in .dat files and participant demographic data that is contained in an excel file

`R/utils-stan_formatting.R` Formats cleaned data for fitting models in Stan

`R/utils-stan_models.R` Runs a Stan model using rstan

`R/utils-model_comparison.R` Compares model fits using Marginal Evidence (bridge sampling) and Predictive Density (LOO) and exports to an excel file

`R/utils-modeling_figures.R` Creates figures from TDRL fit

`R/utils-save_figures.R` Saves figures

`R/utils-parameter_recovery.R` Runs parameter recovery for TDRL model and exports results to an excel file

`R/utils-model_recovery.R` Runs parameter recovery for TDRL, Full SV, and Reward Only SV and exports results to an excel file.

`R/PCA.R` Runs PCA, exports figures, and exports results to an excel file. This script must be run separately from the targets pipeline.

`R/LDA.R` Runs LDA, exports figures, and exports results to an excel file. This script must be run separately from the targets pipeline.

`R/demographic_Data.R` Calculates and summarizes demographics and smoking histories for all groups. This script must be run separately from the targets pipeline.

`R/EEfRT_performance.R` Calculates statistics for EEfRT performance. This script must be run separately from the targets pipeline.

`R/model_comparison_extraction.R` Compares models using Model Evidence (Bridge Sampling) and Predictive Density (ELPD_LOO). This script must be run separately from the targets pipeline.

`R/posterior_distribution_differences_95HDI.R` Calculate, summarize, and plot difference of means of posterior distributions. This script must be run separately from the targets pipeline.

`R/posterior_parameters_stats.R` Calculates group level posterior parameter stats. This script must be run separately from the targets pipeline.

# License
This code is released with a permissive open-source license, and the code in this repository may be used and adapted only in compliance with the terms of the license. If you make use of the code, we would appreciate that you cite the work.
