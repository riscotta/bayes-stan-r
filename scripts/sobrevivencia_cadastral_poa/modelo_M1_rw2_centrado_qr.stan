functions {
  real event_log_prob_cloglog(real eta) {
    if (eta > 20) return 0;
    return log1m_exp(-exp(eta));
  }
}

data {
  int<lower=1> G;
  int<lower=3> M;
  array[G, M] int<lower=0> risk;
  array[G, M] int<lower=0> events;
  int<lower=1> K_cohort;
  matrix[G, K_cohort] B_cohort;
  int<lower=1> K_x;
  matrix[G, K_x] X;
  int<lower=1> K_tv;
  matrix[G, K_tv] Z_tv;
  vector[M] time_basis;
}

transformed data {
  int K_fix = K_cohort + K_x;
  matrix[G, K_fix] D_fix = append_col(B_cohort, X);
  matrix[G, K_fix] Q_ast = qr_thin_Q(D_fix) * sqrt(G - 1.0);
  matrix[K_fix, K_fix] R_ast = qr_thin_R(D_fix) / sqrt(G - 1.0);
}

parameters {
  vector[M] alpha;
  vector[K_cohort + K_x] theta_fix;
  vector[K_tv] gamma_tv;
  real<lower=0> sigma_rw2;
}

transformed parameters {
  vector[K_cohort + K_x] beta_all = R_ast \ theta_fix;
  vector[K_cohort] beta_cohort = head(beta_all, K_cohort);
  vector[K_x] beta = tail(beta_all, K_x);
}

model {
  vector[G] lin_const = Q_ast * theta_fix;
  vector[G] tv_coef = Z_tv * gamma_tv;

  alpha[1] ~ normal(-4.7, 1.0);
  alpha[2] - alpha[1] ~ normal(0, 0.35);
  alpha[3:M] - 2 * alpha[2:(M - 1)] + alpha[1:(M - 2)]
    ~ normal(0, sigma_rw2);

  sigma_rw2 ~ normal(0, 0.15);
  beta_cohort ~ normal(0, 0.35);
  beta ~ normal(0, 0.50);
  gamma_tv ~ normal(0, 0.25);

  for (g in 1:G) {
    for (m in 1:M) {
      if (risk[g, m] > 0) {
        real eta = alpha[m] + lin_const[g] + tv_coef[g] * time_basis[m];
        int d = events[g, m];
        int s = risk[g, m] - d;
        if (s > 0) target += s * (-exp(eta));
        if (d > 0) target += d * event_log_prob_cloglog(eta);
      }
    }
  }
}

generated quantities {
  vector[M] baseline_hazard;
  real z_alpha_2_recon = (alpha[2] - alpha[1]) / 0.35;
  vector[M - 2] z_rw2_recon =
    (alpha[3:M] - 2 * alpha[2:(M - 1)] + alpha[1:(M - 2)]) / sigma_rw2;
  for (m in 1:M) baseline_hazard[m] = 1 - exp(-exp(alpha[m]));
}
