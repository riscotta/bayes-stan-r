// E08_MP C1 M2. ESPECIFICADO; compilacao/ajuste pendentes de gate MP1.
// z_breed nao centrado; raca nova e integrada marginalmente no R.
data {
  int<lower=1> N;
  int<lower=1> K;
  matrix[N,K] X;
  vector[N] y;
  int<lower=1> J;
  array[N] int<lower=1,upper=J> breed;
}
parameters {
  real alpha;
  vector[K] beta;
  real<lower=0> omega;
  real lambda;
  real<lower=0> tau;
  vector[J] z_breed;
}
transformed parameters {
  real delta = lambda / sqrt(1 + square(lambda));
  real mean_shift = omega * delta * sqrt(2 / pi());
}
model {
  vector[N] xi = alpha + X * beta - rep_vector(mean_shift, N);
  alpha ~ normal(0, 1.5);
  beta ~ normal(0, 0.5);
  omega ~ normal(0, 1.0);
  lambda ~ normal(0, 1.5);
  tau ~ normal(0, 0.5);
  z_breed ~ std_normal();
  for (n in 1:N) xi[n] += tau * z_breed[breed[n]];
  y ~ skew_normal(xi, omega, lambda);
}
