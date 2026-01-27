# Parameter Recovery ------------------------------------------------------

tar_load(pars_TDRL)
fit <- pars_TDRL

get_MAP <- function(samples) {
  dens <- density(samples)
  dens$x[which.max(dens$y)]
}

learnrate_MAP <- apply(fit$learnrate, 2, get_MAP)
discount_MAP <- apply(fit$discount, 2, get_MAP)
inv_temp_MAP <- apply(fit$inv_temp, 2, get_MAP)

actual_params <- data.frame(
  id = 1:100,
  learnrate = learnrate_MAP,
  discount  = discount_MAP,
  inv_temp  = inv_temp_MAP
)

simulate_tdrl <- function(params, hard_mag, prob, initV = 0) {

  learnrate <- params$learnrate
  discount <- params$discount
  inv_temp  <- params$inv_temp

  n_trials <- length(hard_mag)

  # storage
  actions <- integer(n_trials)
  outcomes <- numeric(n_trials)

  q_vals <- matrix(initV, nrow = 2, ncol = 3)

  for(t in 1:n_trials) {
    # softmax uses Q at event 1
    opt_vals <- c(q_vals[1,1], q_vals[2,1])

    # numerical stability for softmax: subtract max
    mx <- max(inv_temp * opt_vals)
    expv <- exp(inv_temp * opt_vals - mx)
    p <- expv / sum(expv)

    # choice: 1 -> easy, 2 -> hard
    choice_idx <- sample(1:2, size = 1, prob = p)
    actions[t] <- choice_idx

    # outcome generation using same prob for both options
    if(choice_idx == 1) {
      # easy task: magnitude = 1
      outcomes[t] <- rbinom(1, 1, prob[t]) * 1
      action <- 1
    } else {
      # hard task: magnitude = hard_mag[t]
      outcomes[t] <- rbinom(1, 1, prob[t]) * hard_mag[t]
      action <- 2
    }

    for(ep in 1:3) {
      if(ep < 3) {
        PE <- discount * q_vals[action, ep+1] - q_vals[action, ep]
      } else {
        PE <- outcomes[t] - q_vals[action, ep]
      }
      q_vals[action, ep] <- q_vals[action, ep] + learnrate * PE
    }
  }

  data.frame(
    trial = seq_len(n_trials),
    option1 = 1,             # easy index for Stan compatibility
    option2 = 2,             # hard index
    choice = actions,        # recorded actions as 1 (easy) or 2 (hard)
    outcome = outcomes,
    hard_mag = hard_mag,
    prob = prob
  )
}

simulate_tdrl_all_participants <- function(N,
                                           learnrate_map,
                                           discount_map,
                                           inv_temp_map,
                                           hard_mag_list,
                                           prob_list,
                                           initV = 0,
                                           seed = NULL) {
  if(!is.null(seed)) set.seed(seed)

  sim_list <- vector("list", N)

  for(i in seq_len(N)) {
    params_i <- list(
      learnrate = learnrate_map[i],
      discount  = discount_map[i],
      inv_temp  = inv_temp_map[i]
    )

    sim_i <- simulate_tdrl(
      params = params_i,
      hard_mag = hard_mag_list[[i]],
      prob = prob_list[[i]],
      initV = initV
    )
    sim_i$id <- i
    sim_list[[i]] <- sim_i
  }
  do.call(rbind, sim_list)
}

hard_mag_list <- lapply(1:nrow(stan_data$reward_hard), function(i){
  stan_data$reward_hard[i, ]
})

prob_list <- lapply(1:nrow(stan_data$probability), function(i){
  stan_data$probability[i, ]
})

sim_data <- simulate_tdrl_all_participants(
  N = length(learnrate_MAP),
  learnrate_map = learnrate_MAP,
  discount_map  = discount_MAP,
  inv_temp_map  = inv_temp_MAP,
  hard_mag_list = hard_mag_list,
  prob_list     = prob_list,
  seed = 42
)
