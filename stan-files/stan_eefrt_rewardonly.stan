
data {
  int<lower=0> max_trials; // define maximum number of trials
  int<lower=0> num_subjects; // define number of "subjects"
  int<lower=0, upper=max_trials> trials_per_subj[num_subjects]; // define the number of trials per subject
  real<lower=-1> choiceRT[num_subjects, max_trials]; // define the reaction time
  int<lower=-1, upper=2> choice[num_subjects, max_trials]; // define whether on each trial the subject chose a hard option or not (decision on each trial, yes or no)
  real<lower=-1, upper=1> probability[num_subjects, max_trials]; // define the probability of reward at each trial
  real<lower=-1> reward[num_subjects, max_trials];  // define the magnitude of reward on each trial
  int<lower=1, upper=4> group_id[num_subjects]; // define group classification

}

parameters {
  // Hyper(group)-parameters: 4 groups, 2 parameters each
  // _pr indicates prior
  matrix[4,2] mu_pr_group;
  vector<lower=0>[2] sigma_pr;

  // Subject-level raw parameters
  vector[num_subjects] k_pr; // k: Cost of effort
  vector[num_subjects] inv_temp_pr; // temperature for softmax decision rule
}

transformed parameters {

  vector<lower=0, upper=10>[num_subjects] k;
  vector<lower=0, upper=100>[num_subjects] inv_temp;

  for (i in 1:num_subjects) {
    int g = group_id[i];
    k[i] = Phi_approx(mu_pr_group[g, 1] + sigma_pr[1] * k_pr[i]) *10;
    inv_temp[i] = Phi_approx(mu_pr_group[g, 2] + sigma_pr[2] * inv_temp_pr[i]) *100;
  }

}

model {

  // Hyperparameters
  for (g in 1:4) {
    to_vector(mu_pr_group[g]) ~ normal(0,1);
  }
  sigma_pr  ~ normal(0, 1);

  k_pr ~ normal(0, 1.0);
  inv_temp_pr ~ normal(0, 1.0);

  // Subject loop
  for (i in 1:(num_subjects)) {
    real SVHard;
    real SVEasy;
    vector[2] option_SV = [ 0, 0 ]';

      // trial loop
       for (trial in 1:(trials_per_subj[i])) {

         if (choice[i, trial] > 0) {

          // Subjective Value of the HARD option is equal to the reward on that trial
          // times the exponentiated probability of receiving the reward offset by the
          // effort discount of pressing a button 100 times (note: that button press is scaled down
          // so 1.0 is equivalent to 100 button presses).
          SVHard = reward[i,trial] - (k[i] * 1.0);

          // Subjective Value of the EASY option is equal to 1, the magnitude always given
          // times the exponentiated probability of receiving the reward offset by the
          // effort discount of pressing a button 30 times (note: that button press is scaled down
          // so 0.3 is equivalent to 30 button presses).
          SVEasy = 1.0 - (k[i] * 0.3);

          option_SV = [ SVHard, SVEasy]';

          // probability of choosing the hard task
          target +=  categorical_lpmf(choice[i, trial] | softmax( option_SV * inv_temp[i] ));

        }
       }

  }

}

generated quantities {

  vector[4] mu_k_group;
  vector[4] mu_inv_temp_group;

  for (g in 1:4) {
    mu_k_group[g] = Phi_approx(mu_pr_group[g, 1])*10;
    mu_inv_temp_group[g] = Phi_approx(mu_pr_group[g, 2]) * 100;
  }

  // log likelihood
  array[num_subjects] real log_lik;

  // posterior predictive checks (compares the fitted model and the actual observed data to see if the model is inadequate to describe the data)
  array[num_subjects, max_trials] real pred_choice;

  // set all posterior predictions
  // to -1 to avoid NULL values
  for (i in 1:num_subjects) {
    for (trial in 1:max_trials) {
      pred_choice[i,trial] = -1;
    }
  }

 { // local section to save time and space

    for (i in 1:num_subjects) {
      real SVHard;
      real SVEasy;
      vector[2] option_SV = [ 0, 0 ]';

      log_lik[i] = 0;

      for (trial in 1:(trials_per_subj[i])) {

        if (choice[i, trial] > 0 ) {

        if (choiceRT[i, trial] > 0) {

          // Subjective Value of the HARD option is equal to the reward on that trial
          // times the exponentiated probability of receiving the reward offset by the
          // effort discount of pressing a button 100 times (note: that button press is scaled down
          // so 1.0 is equivalent to 100 button presses).
          SVHard = reward[i,trial] - (k[i] * 1.0);

          // Subjective Value of the EASY option is equal to 1, the magnitude always given
          // times the exponentiated probability of receiving the reward offset by the
          // effort discount of pressing a button 30 times (note: that button press is scaled down
          // so 0.3 is equivalent to 30 button presses).
          SVEasy = 1.0 - (k[i] * 0.3);

          option_SV = [ SVHard, SVEasy]';

          // The log Bernoulli probability mass of chose_hard for each subject for each trial given chance of success inv_logit(SVHard - SVEasy)
          log_lik[i] += categorical_lpmf(choice[i, trial] | softmax( (option_SV) * inv_temp[i]));

          // generate posterior prediction for current trial
          pred_choice[i,trial] = categorical_rng( softmax( option_SV * inv_temp[i] ));

        }

      }

    }

  }

}
}
