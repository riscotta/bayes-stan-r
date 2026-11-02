data {
  real y;
  real<lower=0> s;
}
parameters {
  real<lower=0> mu;
}
model {
  mu ~ normal(0, 10000);
  y ~ normal(mu, s);
}
generated quantities {
  real theta = mu;
  real y_rep = normal_rng(theta, s);
}
