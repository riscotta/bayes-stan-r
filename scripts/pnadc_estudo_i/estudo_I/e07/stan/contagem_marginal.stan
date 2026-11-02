data {
  real y;
  real<lower=0> s;
  real<lower=0> populacao_14mais_mil;
}
parameters {
  real<lower=0, upper=1> q;
}
model {
  q ~ beta(1, 1);
  y ~ normal(populacao_14mais_mil * q, s);
}
generated quantities {
  real theta = populacao_14mais_mil * q;
  real y_rep = normal_rng(theta, s);
}
