data {
  real y;
  real<lower=0> s;
}
parameters {
  real<lower=0, upper=1> p;
}
model {
  p ~ beta(1, 1);
  y ~ normal(100 * p, s);
}
generated quantities {
  real theta = 100 * p;
  real y_rep = normal_rng(theta, s);
}
