data {
  int<lower=1> N;
  int<lower=1> K;
  matrix[N, K] X;
  vector[N] y;
  int<lower=0, upper=1> gaussian;
}
parameters {
  real alpha;
  vector[K] beta;
  real<lower=0> sigma;
}
model {
  alpha ~ normal(0, 1.5);
  beta ~ normal(0, 0.5);
  sigma ~ normal(0, 1.0);
  if (gaussian == 1) {
    y ~ normal(alpha + X * beta, sigma);
  } else {
    y ~ student_t(5, alpha + X * beta, sigma);
  }
}
