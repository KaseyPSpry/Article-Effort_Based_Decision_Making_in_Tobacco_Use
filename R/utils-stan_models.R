############################################################################
# Script Name: utils-stan_models.R
# Purpose: Runs a Stan model using rstan
# Author: Kasey P. Spry
# Last Modified: January 26, 2026
############################################################################

#' Run Stan model
#'
#' @param stan_data The output of [eefrt_prep_stan_data()]
#' @param stan_file Path to the Stan file
#' @param num_iter Number of iterations for MCMC sampler.
#' @param num_warmup Number of warmups for MCMC sampler
#' @param chains Number of chains to use when fitting MCMC (can use
#'    parallel::detectCores() to determine the number of cores/chains available)
#' @param cores Number of cores to use when fitting MCMC (can use
#'    parallel::detectCores() to determine the number of cores/chains available)
#' @param ... Additional arguments to pass onto [rstan::stan]
#'
#' @return A fit Stan model
#'
run_stan_model <- function(stan_data,
                           stan_file,
                           num_iter,
                           num_warmup,
                           chains,
                           cores,
                           seed = 211,
                           adapt_delta = 0.95,
                           ...) {

  rstan::stan(file = stan_file,
              data = stan_data,
              iter = num_iter,
              warmup = num_warmup,
              chains = chains,
              cores = cores,
              seed = seed,
              control = list(adapt_delta=0.95),
              ...)

}
