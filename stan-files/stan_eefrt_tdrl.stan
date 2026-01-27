data {
  // specifies the data that is conditioned upon in Bayes Rule
  int<lower=1> num_subjects; // number of subjects
  int<lower=1> max_trials; // defines the maximum number of trials
  int<lower=1, upper=4> group_id[num_subjects]; // defines the group classification
  array[num_subjects] int<lower=1, upper=max_trials> trials_per_subj;   // number of trials per subject
  array[num_subjects, max_trials] int<lower=1, upper=2> option1; // defines easy task option
  array[num_subjects, max_trials] int<lower=1, upper=2> option2; // defines hard task option
  array[num_subjects, max_trials] int<lower=-1, upper=2> choice; // define whether on each trial the subject chose a hard option or not (decision on each trial, yes or no), 2 = chose hard or 1 = chose easy
  array[num_subjects, max_trials] real outcome; // defines the actual remard outcome (whether they received the reward and the magtiude they received)
  array[num_subjects, max_trials] real<lower=-1> choiceRT; // define the reaction time
}

transformed data {
  row_vector[3] initV;
  initV = rep_row_vector(0.0, 3);
}

parameters {
  // Declare all parameters as vectors for vectorizing
  // Hyper(group)-parameters: 4 groups, 3 parameters each
  matrix[4,3] mu_pr_group;
  vector<lower=0>[3] sigma_pr;

  // Subject-level raw parameters
  vector[num_subjects] learnrate_pr;
  vector[num_subjects] discount_pr;
  vector[num_subjects] inv_temp_pr;
}

transformed parameters {
  // subject-level parameters
  vector<lower=0, upper=1>[num_subjects] learnrate;
  vector<lower=0, upper=1>[num_subjects] discount;
  vector<lower=0, upper=100>[num_subjects] inv_temp;

  for (subj in 1:num_subjects) {
    int g = group_id[subj];
    learnrate[subj] = Phi_approx(mu_pr_group[g, 1] + sigma_pr[1] * learnrate_pr[subj]);
    discount[subj]  = Phi_approx(mu_pr_group[g, 2] + sigma_pr[2] * discount_pr[subj]);
    inv_temp[subj]  = Phi_approx(mu_pr_group[g, 3] + sigma_pr[3] * inv_temp_pr[subj]) * 100;
  }
}

model {
  for (g in 1:4) {
    to_vector(mu_pr_group[g]) ~ normal(0,1);
  }
  sigma_pr ~ normal(0,1);

  // individual parameters
  learnrate_pr ~ normal(0,1);
  discount_pr  ~ normal(0,1);
  inv_temp_pr  ~ normal(0,1);

  // subject loop and trial loop
  for (subj in 1:num_subjects) {
    // Q Values: 2 options, 3 events for EEfRT
    matrix[2,3] q_vals;
    int action;
    real PE;
    vector[2] option_values = [ 0, 0 ]';

    for (idx in 1:2) { q_vals[idx] = initV; }

    for (tr in 1:trials_per_subj[subj]) {

      action = (choice[subj, tr] > 1) ? option2[subj, tr] : option1[subj, tr];

      option_values = [ q_vals[option1[subj, tr], 1] , q_vals[option2[subj, tr], 1] ]';

      // compute action probabilities with softmax
      target += categorical_lpmf(choice[subj,tr] | softmax( option_values*inv_temp[subj] ));

      // Loop for each event in the task
      for (ep in 1:3) {

        if (ep < 3) {

          PE = discount[subj] * q_vals[action, ep+1] - q_vals[action, ep];
          q_vals[action, ep] += learnrate[subj] * PE;

        } else {

          PE = outcome[subj, tr] - q_vals[action, ep];
          q_vals[action, ep] += learnrate[subj] * PE;
        }
      }
    }
  }
}

generated quantities {

  vector[4] mu_learnrate_group;
  vector[4] mu_discount_group;
  vector[4] mu_inv_temp_group;

    for (g in 1:4) {
    mu_learnrate_group[g] = Phi_approx(mu_pr_group[g, 1]);
    mu_discount_group[g] = Phi_approx(mu_pr_group[g, 2]);
    mu_inv_temp_group[g] = Phi_approx(mu_pr_group[g, 3]) * 100;
  }

  // For log-likelihood values and posterior predictive check
  array[num_subjects] real log_lik;
  array[num_subjects, max_trials] real pred_choice;

  // Set all posterior predictions to -1 (avoids NULL values)
  for (subj in 1:num_subjects) {
    for (tr in 1:max_trials) {
      pred_choice[subj,tr] = -1;
    }
  }

  // subject loop and trial loop
  for (subj in 1:num_subjects) {
    matrix[2,3] q_vals;
    int action;
    real PE;
    vector[2] option_values = [ 0, 0 ]';

    log_lik[subj] = 0;

    // Loop for each trial
    for (idx in 1:2) { q_vals[idx] = initV; }

    // Loop for each trial
    for (tr in 1:trials_per_subj[subj]) {

      if (choiceRT[subj, tr] > 0 ) {


      action = (choice[subj, tr] > 1) ? option2[subj, tr] : option1[subj, tr];

      option_values = [ q_vals[option1[subj, tr], 1] , q_vals[option2[subj, tr], 1] ]';

      // compute log likelihood of current trial
      log_lik[subj] += categorical_lpmf(choice[subj, tr] | softmax( option_values*inv_temp[subj] ));

      // generate posterior prediction for current trial
      pred_choice[subj,tr] = categorical_rng( softmax( option_values*inv_temp[subj] ));

      // Loop for each event in the task
      for (ep in 1:3) {

        if (ep < 3) {

          PE = discount[subj] * q_vals[action, ep+1] - q_vals[action, ep];
          q_vals[action, ep] += learnrate[subj] * PE;

        } else {

          PE = outcome[subj, tr] - q_vals[action, ep];
          q_vals[action, ep] += learnrate[subj] * PE;
        }
      }
      }
    }
  }
}
