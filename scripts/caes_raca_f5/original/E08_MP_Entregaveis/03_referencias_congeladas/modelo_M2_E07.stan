data {
  int<lower=1> N;
  int<lower=1> K;
  matrix[N, K] X;
  vector[N] y;
  int<lower=1> J;
  array[N] int<lower=1, upper=J> breed;
  int<lower=0, upper=1> gaussian;
  real<lower=0> tau_scale;
}
parameters {
  real alpha;
  vector[K] beta;
  real<lower=0> sigma;
  real<lower=0> tau;
  vector[J] z_breed;
}
model {
  vector[N] mu = alpha + X * beta;
  alpha ~ normal(0, 1.5);
  beta ~ normal(0, 0.5);
  sigma ~ normal(0, 1.0);
  tau ~ normal(0, tau_scale);
  z_breed ~ std_normal();
  for (n in 1:N) {
    mu[n] += tau * z_breed[breed[n]];
  }
  if (gaussian == 1) {
    y ~ normal(mu, sigma);
  } else {
    y ~ student_t(5, mu, sigma);
  }
}
