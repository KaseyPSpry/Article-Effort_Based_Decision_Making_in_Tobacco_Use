library(tidyverse)
library(stantargets)
library(targets)
library(tarchetypes)
library(janitor)
library(here)
library(fs)
library(ggtext)
library(bridgesampling)
library(bayesplot)
library(stringr)
library(ggplot2)
library(loo)
library(stats)
library(rstan)
library(magrittr)
library(patchwork)
library(cowplot)
library(grid)
library(hBayesDM)
library(rstanarm)
library(dplyr)
library(purrr)
library(tidyr)
library(tibble)


source("R/utils-import_eefrt_data.R")
source("R/utils-stan_formatting.R")
source("R/utils-stan_models.R")
source("R/utils-model_comparison.R")
source("R/utils-modeling_figures.R")
source("R/utils-save_figures.R")
source("R/utils-parameter_recovery.R")
source("R/utils-model_recovery.R")

stem_data_import <- list(
  tar_target(
    data_path,
    dir_ls(
      here('data'),
      glob = "*.dat"
    )
  ),
  tar_target(
    raw_data,
    eefrt_read_raw_data(
      .files = data_path
    )
  ),
  tar_target(
    demographic_data,
    eefrt_read_demographics(
      path = "data/Subject_Data_GBN_DITO.xlsx"
    )
  ),
  tar_target(
    processed_data,
    eefrt_process_data(
      raw_eefrt = raw_data,
      demographic_eefrt = demographic_data,
      split_by_group = FALSE
    )
  ),
  tar_target(
    eefrt_performance_data,
    eefrt_performance_process_data(
      raw_eefrt = raw_data,
      demographic_eefrt = demographic_data,
      split_by_group = TRUE
    )
  )
)

stem_model_compilation <- list(
  tar_target(
    tdrl_model_compiled,
    rstan::stan_model("stan-files/stan_eefrt_tdrl.stan"),
    format = "rds"
  ),
  tar_target(
    fullsv_model_compiled,
    rstan::stan_model("stan-files/stan_eefrt_full_sv.stan"),
    format = "rds"
  ),
  tar_target(
    rewardonly_model_compiled,
    rstan::stan_model("stan-files/stan_eefrt_rewardonly.stan"),
    format = "rds"
  )
)

stem_stan_fitting <- list(
  tar_target(
    stan_data,
    eefrt_prep_stan_data(
      processed_eefrt = processed_data
    )
  ),
  tar_target(
    fit_TDRL,
    run_stan_model(
      stan_data,
      stan_file = "stan-files/stan_eefrt_tdrl.stan",
      chains = 4,
      cores = 4,
      num_iter = 10000,
      num_warmup  = 2000
    )
  ),
  tar_target(
    pars_TDRL,
    rstan::extract(fit_TDRL)
  ),
  tar_target(
    bridge_error_TDRL,
    get_bridge_error(fit_TDRL,
                     stan_data = stan_data,
                     stan_file = "stan-files/stan_eefrt_tdrl.stan"
    )
  ),
  tar_target(
    loo_info_TDRL,
    loo_comparison(fit_TDRL)
  ),
  tar_target(
    fit_FullSV,
    run_stan_model(
      stan_data,
      stan_file = "stan-files/stan_eefrt_full_sv.stan",
      chains = 4,
      cores = 4,
      num_iter = 10000,
      num_warmup  = 2000
    )
  ),
  tar_target(
    pars_FullSV,
    rstan::extract(fit_FullSV)
  ),
  tar_target(
    bridge_error_FullSV,
    get_bridge_error(fit_FullSV,
                     stan_data = stan_data,
                     stan_file = "stan-files/stan_eefrt_full_sv.stan"
    )
  ),
  tar_target(
    loo_info_FullSV,
    loo_comparison(fit_FullSV)
  ),
  tar_target(
    fit_RewardOnly,
    run_stan_model(
      stan_data,
      stan_file = "stan-files/stan_eefrt_rewardonly.stan",
      chains = 4,
      cores = 4,
      num_iter = 10000,
      num_warmup  = 2000
    )
  ),
  tar_target(
    pars_RewardOnly,
    rstan::extract(fit_RewardOnly)
  ),
  tar_target(
    bridge_error_RewardOnly,
    get_bridge_error(fit_RewardOnly,
                     stan_data = stan_data,
                     stan_file = "stan-files/stan_eefrt_rewardonly.stan"
    )
  ),
  tar_target(
    loo_info_RewardOnly,
    loo_comparison(fit_RewardOnly)
  ),
  tar_target(
    model_comparison_summary,
    extract_model_comparison(
      bridge_errors = list(
        TDRL_pooled = bridge_error_TDRL,
        FullSV_pooled = bridge_error_FullSV,
        RewardOnly_pooled = bridge_error_RewardOnly
      ),
      loo_results = list(
        TDRL_pooled = loo_info_TDRL,
        FullSV_pooled = loo_info_FullSV,
        RewardOnly_pooled = loo_info_RewardOnly
      ),
      excel_path = "Results/Model_Comparison_results.xlsx"
    )
  )
)

stem_stan_vis_groups <- list(
  tar_target(
    group_posterior_plot_individual_tdrl_learnrate,
    generate_group_posterior_plot_tdrl_learnrate(
      model_fit = pars_TDRL,
      stan_data = stan_data
    )
  ),
  tar_target(
    group_posterior_plot_individual_tdrl_discount,
    generate_group_posterior_plot_tdrl_discount(
      model_fit = pars_TDRL,
      stan_data = stan_data
    )
  ),
  tar_target(
    group_posterior_plot_individual_tdrl_inv_temp,
    generate_group_posterior_plot_tdrl_inv_temp(
      model_fit = pars_TDRL,
      stan_data = stan_data
    )
  ),
  tar_target(
    group_posterior_plot_legend,
    generate_group_posterior_plot_legend(
      model_fit = pars_TDRL,
      stan_data = stan_data
    )
  ),
  tar_target(
    Posterior_Distributions_nonoverlayed,
    save_figure.ggplot_post(
      .figures = list(
        group_posterior_plot_individual_tdrl_learnrate,
        group_posterior_plot_individual_tdrl_discount,
        group_posterior_plot_individual_tdrl_inv_temp
      ),
      .path = "Figures/Posterior_Distributions.png",
      width = 15,
      height = 6
    )
  ),
  tar_target(
    Posterior_Distributions_legend,
    save_figure.ggplot_post(
      .figures = list(
        group_posterior_plot_legend
      ),
      .path = "Figures/Posterior_Distributions_legend.png",
      width = 15,
      height = 6
    )
  )
)


stem_parameter_recovery <- list(
  tar_target(
    actual_params,
    extract_participant_MAPs(pars_TDRL)
  ),
  tar_target(
    hard_prob_lists,
    build_hard_mag_and_prob_lists(stan_data)
  ),
  tar_target(
    hard_mag_list,
    hard_prob_lists$hard_mag_list
  ),
  tar_target(
    prob_list,
    hard_prob_lists$prob_list
  ),
  tar_target(
    sim_data,
    simulate_tdrl_all_participants(
      stan_data = stan_data,
      actual_params = actual_params,
      hard_mag_list = hard_mag_list,
      prob_list = prob_list,
      seed = 42
    )
  ),
  tar_target(
    stan_data_sim,
    pack_sim_into_stan_arrays(sim_data,
                              stan_data_template = stan_data)
  ),
  tar_target(
    fit_TDRL_recovery,
    run_stan_model(
      stan_data_sim,
      stan_file = "stan-files/stan_eefrt_tdrl.stan",
      chains    = 4,
      cores     = 4,
      num_iter  = 10000,
      num_warmup = 2000
    )
  ),
  tar_target(
    pars_TDRL_recovery,
    rstan::extract(fit_TDRL_recovery)
  ),
  tar_target(
    recovered_params,
    extract_recovered_MAPs(pars_TDRL_recovery)
  ),
  tar_target(
    params_eval,
    merge_actual_recovered(actual_params, recovered_params)
  ),
  tar_target(
    group_labels,
    add_group_labels(stan_data)
  ),

  tar_target(
    params_eval_grouped,
    params_eval %>%
      dplyr::left_join(group_labels, by = "id")
  ),
  tar_target(
    param_recovery_stats_excel,
    write_recovery_stats_excel(
      df = params_eval_grouped,
      actual_cols = c("learnrate", "discount", "inv_temp"),
      recovered_cols = c("learnrate_rec", "discount_rec", "inv_temp_rec"),
      output_path = "Results/Parameter_Recovery_Stats.xlsx"
    ),
    format = "file"
  )
)

stem_model_recovery <- list(
  tar_target(
    actual_params_TDRL,
    extract_participant_MAPs(pars_TDRL)
  ),
  tar_target(
    actual_params_FullSV,
    extract_participant_MAPs_fullsv(pars_FullSV)
  ),
  tar_target(
    actual_params_RewardOnly,
    extract_participant_MAPs_rewardonly(pars_RewardOnly)
  ),
  tar_target(
    hard_prob_lists_mr,
    build_hard_mag_and_prob_lists(stan_data)
  ),
  tar_target(
    hard_mag_list_mr,
    hard_prob_lists_mr$hard_mag_list
  ),
  tar_target(
    prob_list_mr,
    hard_prob_lists_mr$prob_list
  ),
  tar_target(
    sim_TDRL_gen,
    simulate_dataset_tdrl(
               actual_params_TDRL,
               hard_mag_list_mr,
               prob_list_mr,
               n_reps = 50,
               seed = 1001)
  ),
  tar_target(
    sim_FullSV_gen,
    simulate_dataset_fullsv(
               actual_params_FullSV,
               hard_mag_list_mr,
               prob_list_mr,
               n_reps = 50,
               seed = 1002)
  ),
  tar_target(
    sim_RewardOnly_gen,
    simulate_dataset_rewardonly(
               actual_params_RewardOnly,
               hard_mag_list_mr,
               prob_list_mr,
               n_reps = 50,
               seed = 1003)
  ),
  tar_target(
    sd_TDRL_TDRL,
    pack_sim_into_stan_arrays(sim_TDRL_gen,
                              stan_data)
  ),
  tar_target(
    sd_TDRL_FullSV,
    pack_sim_into_stan_arrays_sv(sim_TDRL_gen,
                                 stan_data)
  ),
  tar_target(
    sd_TDRL_RewardOnly,
    pack_sim_into_stan_arrays_sv(sim_TDRL_gen,
                                 stan_data)
  ),
  tar_target(
    sd_FullSV_TDRL,
    pack_sim_into_stan_arrays(sim_FullSV_gen,
                              stan_data)
  ),
  tar_target(
    sd_FullSV_FullSV,
    pack_sim_into_stan_arrays_sv(sim_FullSV_gen,
                                 stan_data)
  ),
  tar_target(
    sd_FullSV_RewardOnly,
    pack_sim_into_stan_arrays_sv(sim_FullSV_gen,
                                 stan_data)
  ),
  tar_target(
    sd_RewardOnly_TDRL,
    pack_sim_into_stan_arrays(sim_RewardOnly_gen,
                              stan_data)
  ),
  tar_target(
    sd_RewardOnly_FullSV,
    pack_sim_into_stan_arrays_sv(sim_RewardOnly_gen,
                                 stan_data)
  ),
  tar_target(
    sd_RewardOnly_RewardOnly,
    pack_sim_into_stan_arrays_sv(sim_RewardOnly_gen,
                                 stan_data)
  ),
  tar_target(
    fit_TDRL_on_TDRLsim,
    run_stan_model(
      sd_TDRL_TDRL,
      stan_file = "stan-files/stan_eefrt_tdrl.stan",
      chains = 4,
      cores = 4,
      num_iter = 4000,
      num_warmup = 1000)
  ),
  tar_target(
    fit_FullSV_on_TDRLsim,
    run_stan_model(
      sd_TDRL_FullSV,
      stan_file = "stan-files/stan_eefrt_full_sv.stan",
      chains = 4,
      cores = 4,
      num_iter = 4000,
      num_warmup = 1000)
  ),
  tar_target(
    fit_RewardOnly_on_TDRLsim,
    run_stan_model(
      sd_TDRL_RewardOnly,
      stan_file = "stan-files/stan_eefrt_rewardonly.stan",
      chains = 4,
      cores = 4,
      num_iter = 4000,
      num_warmup = 1000)
    ),
  tar_target(
    fit_TDRL_on_FullSVsim,
    run_stan_model(
      sd_FullSV_TDRL,
      stan_file = "stan-files/stan_eefrt_tdrl.stan",
      chains = 4,
      cores = 4,
      num_iter = 4000,
      num_warmup = 1000,
      init = "0")
    ),
  tar_target(
    fit_FullSV_on_FullSVsim,
    run_stan_model(
      sd_FullSV_FullSV, stan_file = "stan-files/stan_eefrt_full_sv.stan",
      chains = 4,
      cores = 4,
      num_iter = 4000,
      num_warmup = 1000)
    ),
  tar_target(
    fit_RewardOnly_on_FullSVsim,
    run_stan_model(
      sd_FullSV_RewardOnly,
      stan_file = "stan-files/stan_eefrt_rewardonly.stan",
      chains = 4,
      cores = 4,
      num_iter = 4000,
      num_warmup = 1000)
    ),
  tar_target(
    fit_TDRL_on_RewardOnlysim,
    run_stan_model(
      sd_RewardOnly_TDRL,
      stan_file = "stan-files/stan_eefrt_tdrl.stan",
      chains = 4,
      cores = 4,
      num_iter = 4000,
      num_warmup = 1000)
    ),
  tar_target(
    fit_FullSV_on_RewardOnlysim,
    run_stan_model(
      sd_RewardOnly_FullSV,
      stan_file = "stan-files/stan_eefrt_full_sv.stan",
      chains = 4,
      cores = 4,
      num_iter = 4000,
      num_warmup = 1000)
    ),
  tar_target(
    fit_RewardOnly_on_RewardOnlysim,
    run_stan_model(
      sd_RewardOnly_RewardOnly,
      stan_file = "stan-files/stan_eefrt_rewardonly.stan",
      chains = 4,
      cores = 4,
      num_iter = 4000,
      num_warmup = 1000)
    ),
  tar_target(
    scores_on_TDRLsim,
    tibble::tibble(
      generating_model = "TDRL",
      model = c("TDRL", "FullSV", "RewardOnly"),
      elpd_loo = c(
        loo_comparison(fit_TDRL_on_TDRLsim)$loo$estimates["elpd_loo", "Estimate"],
        loo_comparison(fit_FullSV_on_TDRLsim)$loo$estimates["elpd_loo", "Estimate"],
        loo_comparison(fit_RewardOnly_on_TDRLsim)$loo$estimates["elpd_loo", "Estimate"]
        )
      )
    ),
  tar_target(
    scores_on_FullSVsim,
    tibble::tibble(
      generating_model = "FullSV",
      model = c("TDRL", "FullSV", "RewardOnly"),
      elpd_loo = c(
        loo_comparison(fit_TDRL_on_FullSVsim)$loo$estimates["elpd_loo", "Estimate"],
        loo_comparison(fit_FullSV_on_FullSVsim)$loo$estimates["elpd_loo", "Estimate"],
        loo_comparison(fit_RewardOnly_on_FullSVsim)$loo$estimates["elpd_loo", "Estimate"]
        )
      )
    ),
  tar_target(
    scores_on_RewardOnlysim,
    tibble::tibble(
      generating_model = "RewardOnly",
      model = c("TDRL", "FullSV", "RewardOnly"),
      elpd_loo = c(
        loo_comparison(fit_TDRL_on_RewardOnlysim)$loo$estimates["elpd_loo", "Estimate"],
        loo_comparison(fit_FullSV_on_RewardOnlysim)$loo$estimates["elpd_loo", "Estimate"],
        loo_comparison(fit_RewardOnly_on_RewardOnlysim)$loo$estimates["elpd_loo", "Estimate"]
        )
      )
    ),
  tar_target(
    model_recovery_scores,
    dplyr::bind_rows(scores_on_TDRLsim,
                     scores_on_FullSVsim,
                     scores_on_RewardOnlysim)
    ),
  tar_target(
    bridge_TDRL_on_TDRL,
    get_bridge_error(
      fit_TDRL_on_TDRLsim,
      stan_data = sd_TDRL_TDRL,
      stan_file = "stan-files/stan_eefrt_tdrl.stan"
    )
  ),
  tar_target(
    bridge_FullSV_on_TDRL,
    get_bridge_error(
      fit_FullSV_on_TDRLsim,
      stan_data = sd_TDRL_FullSV,
      stan_file = "stan-files/stan_eefrt_full_sv.stan"
    )
  ),
  tar_target(
    bridge_RewardOnly_on_TDRL,
    get_bridge_error(
      fit_RewardOnly_on_TDRLsim,
      stan_data = sd_TDRL_RewardOnly,
      stan_file = "stan-files/stan_eefrt_rewardonly.stan",
    )
  ),
  tar_target(
    bridge_TDRL_on_FullSV,
    get_bridge_error(
      fit_TDRL_on_FullSVsim,
      stan_data = sd_FullSV_TDRL,
      stan_file = "stan-files/stan_eefrt_tdrl.stan",
    )
  ),
  tar_target(
    bridge_FullSV_on_FullSV,
    get_bridge_error(
      fit_FullSV_on_FullSVsim,
      stan_data = sd_FullSV_FullSV,
      stan_file = "stan-files/stan_eefrt_full_sv.stan",
    )
  ),
  tar_target(
    bridge_RewardOnly_on_FullSV,
    get_bridge_error(
      fit_RewardOnly_on_FullSVsim,
      stan_data = sd_FullSV_RewardOnly,
      stan_file = "stan-files/stan_eefrt_rewardonly.stan",
    )
  ),
  tar_target(
    bridge_TDRL_on_RewardOnly,
    get_bridge_error(
      fit_TDRL_on_RewardOnlysim,
      stan_data = sd_RewardOnly_TDRL,
      stan_file = "stan-files/stan_eefrt_tdrl.stan",
    )
  ),
  tar_target(
    bridge_FullSV_on_RewardOnly,
    get_bridge_error(
      fit_FullSV_on_RewardOnlysim,
      stan_data = sd_RewardOnly_FullSV,
      stan_file = "stan-files/stan_eefrt_full_sv.stan",
    )
  ),
  tar_target(
    bridge_RewardOnly_on_RewardOnly,
    get_bridge_error(
      fit_RewardOnly_on_RewardOnlysim,
      stan_data = sd_RewardOnly_RewardOnly,
      stan_file = "stan-files/stan_eefrt_rewardonly.stan",
    )
  ),
  tar_target(
    mr_fit_list,
    list(
      "TDRL|TDRL" = fit_TDRL_on_TDRLsim,
      "TDRL|FullSV" = fit_FullSV_on_TDRLsim,
      "TDRL|RewardOnly" = fit_RewardOnly_on_TDRLsim,
      "FullSV|TDRL" = fit_TDRL_on_FullSVsim,
      "FullSV|FullSV" = fit_FullSV_on_FullSVsim,
      "FullSV|RewardOnly"= fit_RewardOnly_on_FullSVsim,
      "RewardOnly|TDRL" = fit_TDRL_on_RewardOnlysim,
      "RewardOnly|FullSV"= fit_FullSV_on_RewardOnlysim,
      "RewardOnly|RewardOnly" = fit_RewardOnly_on_RewardOnlysim
    )
  ),
  tar_target(
    mr_bridge_list,
    list(
      "TDRL|TDRL" = bridge_TDRL_on_TDRL,
      "TDRL|FullSV" = bridge_FullSV_on_TDRL,
      "TDRL|RewardOnly" = bridge_RewardOnly_on_TDRL,
      "FullSV|TDRL" = bridge_TDRL_on_FullSV,
      "FullSV|FullSV" = bridge_FullSV_on_FullSV,
      "FullSV|RewardOnly" = bridge_RewardOnly_on_FullSV,
      "RewardOnly|TDRL" = bridge_TDRL_on_RewardOnly,
      "RewardOnly|FullSV" = bridge_FullSV_on_RewardOnly,
      "RewardOnly|RewardOnly" = bridge_RewardOnly_on_RewardOnly
    )
  ),
  tar_target(
    model_recovery_summary,
    organize_loo_and_bridge(
      fits = mr_fit_list,
      bridge_list = mr_bridge_list,
      excel_path = "Results/Model_recovery_summary.xlsx"
    )
  )
)



list(
  c(
    stem_data_import,
    stem_model_compilation,
    stem_stan_fitting,
    stem_stan_vis_groups,
    stem_parameter_recovery,
    stem_model_recovery
  )
)
