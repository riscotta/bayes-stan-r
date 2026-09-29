data {
  int<lower=1> N;
  int<lower=1> P;
  int<lower=1> J;
  int<lower=2> R;
  int<lower=2> T;
  array[N] int<lower=1, upper=P> pair_id;
  array[P] int<lower=1, upper=J> pair_country;
  array[P] int<lower=1, upper=T> pair_year;
  array[J] int<lower=1, upper=R> country_region;
  array[N] int<lower=0, upper=1> female;
  vector<lower=0, upper=1>[N] y;
  int<lower=0, upper=1> prior_only;
}
transformed data {
  real eps_mu = 1e-6;
  real eps_prob = 1e-6;
  real log_phi_lo = log(2.0);
  real log_phi_hi = log(200.0);
  real logit_pb_lo = logit(1e-6);
  real logit_pb_hi = logit(0.20);
}
parameters {
  real alpha;
  sum_to_zero_vector[R] region_base;
  vector[J] z_country;
  real<lower=0, upper=2.5> sigma_country;
  sum_to_zero_vector[T] year_base;
  vector[P] z_pair;
  real<lower=0, upper=2.0> sigma_pair;

  real delta_female;
  sum_to_zero_vector[R] delta_region;
  vector[J] z_delta_country;
  real<lower=0, upper=1.5> sigma_delta_country;
  sum_to_zero_vector[T] delta_year;

  real<lower=log_phi_lo, upper=log_phi_hi> log_phi;
  real<lower=logit_pb_lo, upper=logit_pb_hi> logit_p_boundary;
  real<lower=eps_prob, upper=1 - eps_prob> p_one_given_boundary;
}
transformed parameters {
  vector[J] country_effect = sigma_country * z_country;
  vector[J] delta_country = sigma_delta_country * z_delta_country;
  vector[P] pair_baseline;
  vector[N] eta;
  vector[N] mu;
  real<lower=2, upper=200> phi = exp(log_phi);
  real<lower=1e-6, upper=0.20> p_boundary = inv_logit(logit_p_boundary);

  for (p in 1:P) {
    int j = pair_country[p];
    int r = country_region[j];
    pair_baseline[p] = alpha + region_base[r] + country_effect[j]
                       + year_base[pair_year[p]] + sigma_pair * z_pair[p];
  }

  for (n in 1:N) {
    int p = pair_id[n];
    int j = pair_country[p];
    int r = country_region[j];
    real female_shift = delta_female + delta_region[r]
                        + delta_country[j] + delta_year[pair_year[p]];
    real mu_raw;
    eta[n] = pair_baseline[p] + female[n] * female_shift;
    mu_raw = inv_logit(eta[n]);
    mu[n] = eps_mu + (1 - 2 * eps_mu) * mu_raw;
  }
}
model {
  alpha ~ normal(logit(0.30), 1.2);
  region_base ~ normal(0, 0.7);
  z_country ~ std_normal();
  sigma_country ~ normal(0, 1.0);
  year_base ~ normal(0, 0.4);
  z_pair ~ std_normal();
  sigma_pair ~ normal(0, 0.7);

  delta_female ~ normal(0, 0.5);
  delta_region ~ normal(0, 0.35);
  z_delta_country ~ std_normal();
  sigma_delta_country ~ normal(0, 0.5);
  delta_year ~ normal(0, 0.25);

  log_phi ~ normal(log(20), 1.0);
  logit_p_boundary ~ normal(logit(0.01), 1.5);
  p_one_given_boundary ~ beta(1, 1);

  if (prior_only == 0) {
    for (n in 1:N) {
      if (y[n] == 0) {
        target += log(p_boundary) + log1m(p_one_given_boundary);
      } else if (y[n] == 1) {
        target += log(p_boundary) + log(p_one_given_boundary);
      } else {
        target += log1m(p_boundary)
                  + beta_lpdf(y[n] | mu[n] * phi, (1 - mu[n]) * phi);
      }
    }
  }
}
generated quantities {
  vector[N] log_lik;
  vector[N] y_rep;
  for (n in 1:N) {
    if (prior_only == 0) {
      if (y[n] == 0) {
        log_lik[n] = log(p_boundary) + log1m(p_one_given_boundary);
      } else if (y[n] == 1) {
        log_lik[n] = log(p_boundary) + log(p_one_given_boundary);
      } else {
        log_lik[n] = log1m(p_boundary)
                     + beta_lpdf(y[n] | mu[n] * phi, (1 - mu[n]) * phi);
      }
    } else {
      log_lik[n] = 0;
    }

    if (bernoulli_rng(p_boundary) == 1) {
      y_rep[n] = bernoulli_rng(p_one_given_boundary);
    } else {
      y_rep[n] = beta_rng(mu[n] * phi, (1 - mu[n]) * phi);
    }
  }
}