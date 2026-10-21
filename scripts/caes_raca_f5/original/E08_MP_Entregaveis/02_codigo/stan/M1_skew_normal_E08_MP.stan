// E08_MP C1 M1. ESPECIFICADO; compilacao/ajuste pendentes de gate MP1.
// eta = media condicional, xi = eta - omega * delta * sqrt(2/pi).
data {
  int<lower=1> N;
  int<lower=1> K;
  matrix[N,K] X;
  vector[N] y;
}
parameters {
  real alpha;
  vector[K] beta;
  real<lower=0> omega;
  real lambda;
}
transformed parameters {
  real delta = lambda / sqrt(1 + square(lambda));
  real mean_shift = omega * delta * sqrt(2 / pi());
}
model {
  vector[N] xi = alpha + X * beta - rep_vector(mean_shift, N);
  alpha ~ normal(0, 1.5);
  beta ~ normal(0, 0.5);
  omega ~ normal(0, 1.0); // half-normal devido ao limite inferior
  lambda ~ normal(0, 1.5);
  y ~ skew_normal(xi, omega, lambda);
}
