// E06_modelo_softmax_dificuldade_sensibilidade.stan
// Especificação candidata para a Etapa 07. Não foi compilada nem executada na E06.
// Uso: sensibilidade à suposição ordinal do modelo ordered_logistic.

data {
  int<lower=1> N;
  int<lower=3> K_y;
  int<lower=1> P;
  matrix[N, P] X;
  array[N] int<lower=1, upper=K_y> y;
}
transformed data {
  vector[P] zeros = rep_vector(0, P);
}
parameters {
  matrix[P, K_y - 1] beta_raw;
}
transformed parameters {
  matrix[P, K_y] beta = append_col(beta_raw, zeros);
}
model {
  to_vector(beta_raw) ~ normal(0, 1);
  for (n in 1:N) {
    y[n] ~ categorical_logit((X[n] * beta)');
  }
}
generated quantities {
  array[N] int<lower=1, upper=K_y> y_rep;
  vector[N] log_lik;
  for (n in 1:N) {
    vector[K_y] eta = (X[n] * beta)';
    y_rep[n] = categorical_logit_rng(eta);
    log_lik[n] = categorical_logit_lpmf(y[n] | eta);
  }
}
