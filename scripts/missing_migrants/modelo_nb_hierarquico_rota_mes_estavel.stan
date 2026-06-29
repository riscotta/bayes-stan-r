data {
  int<lower=1> N;
  int<lower=1> G;
  array[N] int<lower=0> y;
  array[N] int<lower=1, upper=G> group_id;
  array[N] int<lower=1, upper=12> month_id;
  vector[N] t_years;
  vector<lower=0>[N] exposure_days;
}
transformed data {
  vector[N] log_exposure = log(exposure_days);
}
parameters {
  real alpha;
  real beta_time;
  vector[G] z_group_intercept;
  vector[G] z_group_slope;
  vector[12] month_raw;
  // Limites superiores evitam propostas numericamente explosivas durante warmup.
  // Eles não devem ficar próximos da posterior; verificar isso na E08.
  real<lower=0, upper=3> sigma_group_intercept;
  real<lower=0, upper=0.5> sigma_group_slope;
  real<lower=0, upper=2> sigma_month;
  real<lower=1e-4, upper=1e4> phi;
}
transformed parameters {
  vector[G] group_intercept = sigma_group_intercept * z_group_intercept;
  vector[G] group_slope = sigma_group_slope * z_group_slope;
  vector[12] month_effect = sigma_month * (month_raw - mean(month_raw));
  vector[N] eta;

  for (n in 1:N) {
    eta[n] = log_exposure[n]
             + alpha
             + group_intercept[group_id[n]]
             + (beta_time + group_slope[group_id[n]]) * t_years[n]
             + month_effect[month_id[n]];
  }
}
model {
  alpha ~ normal(log(0.5), 2);
  beta_time ~ normal(0, 0.3);
  z_group_intercept ~ std_normal();
  z_group_slope ~ std_normal();
  month_raw ~ std_normal();
  // Priors fracamente informativos, mas mais regularizadores que a versão inicial.
  // A escala está no preditor logarítmico; valores muito grandes implicam taxas implausíveis.
  sigma_group_intercept ~ normal(0, 0.75);
  sigma_group_slope ~ normal(0, 0.15);
  sigma_month ~ normal(0, 0.50);
  phi ~ lognormal(log(5), 1);

  y ~ neg_binomial_2_log(eta, phi);
}
generated quantities {
  array[N] int<lower=0> y_rep;
  vector[N] log_lik;
  real<lower=0> total_y_rep = 0;
  real<lower=0> mean_y_rep;
  real<lower=0> sd_y_rep;
  real<lower=0> max_y_rep = 0;
  int<lower=0> n_zero_rep = 0;
  int<lower=0> n_ge_80_rep = 0;
  real<lower=0> rate_ratio_annual_global = exp(beta_time);

  for (n in 1:N) {
    y_rep[n] = neg_binomial_2_log_rng(eta[n], phi);
    log_lik[n] = neg_binomial_2_log_lpmf(y[n] | eta[n], phi);
    total_y_rep += y_rep[n];
    if (y_rep[n] == 0) n_zero_rep += 1;
    if (y_rep[n] >= 80) n_ge_80_rep += 1;
    if (y_rep[n] > max_y_rep) max_y_rep = y_rep[n];
  }

  mean_y_rep = total_y_rep / N;
  {
    real ss = 0;
    for (n in 1:N) {
      ss += square(y_rep[n] - mean_y_rep);
    }
    sd_y_rep = sqrt(ss / (N - 1));
  }
}
