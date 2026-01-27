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
