// modelo_principal_hierarquico_student_t.stan
// Modelo hierarquico robusto para log_import em painel pais-ano.
// Versao de teste SEM LKJ, com limites computacionais conservadores e piso positivo para sigma.
// Objetivo: evitar propostas extremas que tornam o preditor linear mu infinito
// e impedir escala zero na verossimilhanca Student-t.
// mantendo a estrutura principal: Student-t + intercepto e slope aleatorios por pais.

data {
  int<lower=1> N;
  int<lower=1> P;
  int<lower=1> J_country;
  vector[N] y;
  matrix[N, P] X;
  vector[N] year10_c;
  array[N] int<lower=1, upper=J_country> country_id;
  real y_mean;
}

parameters {
  vector<lower=-30, upper=30>[P] beta;
  vector<lower=-8, upper=8>[J_country] z_country_intercept;
  vector<lower=-8, upper=8>[J_country] z_country_slope;
  real<lower=0, upper=5> tau_country_intercept;
  real<lower=0, upper=2> tau_country_slope;
  real<lower=0.01, upper=5> sigma;
  real<lower=2, upper=100> nu;
}

transformed parameters {
  vector[J_country] b_country_intercept;
  vector[J_country] b_country_slope;
  b_country_intercept = tau_country_intercept * z_country_intercept;
  b_country_slope = tau_country_slope * z_country_slope;
}

model {
  vector[N] mu;
  beta[1] ~ normal(y_mean, 2.5);
  if (P > 1) beta[2:P] ~ normal(0, 1.0);
  z_country_intercept ~ normal(0, 1);
  z_country_slope ~ normal(0, 1);
  tau_country_intercept ~ normal(0, 1.0);
  tau_country_slope ~ normal(0, 0.2);
  // Como sigma tem lower=0.01, esta prior atua como half-normal truncada com piso numerico.
  sigma ~ normal(0, 1.0);
  (nu - 2) ~ exponential(1.0 / 10.0);
  mu = X * beta;
  for (n in 1:N) {
    mu[n] += b_country_intercept[country_id[n]] +
             b_country_slope[country_id[n]] * year10_c[n];
  }
  y ~ student_t(nu, mu, sigma);
}

generated quantities {
  vector[N] log_lik;
  vector[N] y_rep;
  vector[N] mu;
  matrix[2, 2] Omega_country;
  Omega_country = diag_matrix(rep_vector(1, 2));
  mu = X * beta;
  for (n in 1:N) {
    mu[n] += b_country_intercept[country_id[n]] +
             b_country_slope[country_id[n]] * year10_c[n];
    log_lik[n] = student_t_lpdf(y[n] | nu, mu[n], sigma);
    y_rep[n] = student_t_rng(nu, mu[n], sigma);
  }
}
