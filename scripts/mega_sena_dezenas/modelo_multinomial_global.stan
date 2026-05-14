data {
  int<lower=2> D;
  int<lower=1> G;
  array[G, D] int<lower=0> y;
  array[G] int<lower=1> n_slots;
}
parameters {
  sum_to_zero_vector[D] alpha;
}
transformed parameters {
  simplex[D] p;
  p = softmax(alpha);
}
model {
  alpha ~ normal(0, 0.35);
  for (g in 1:G) {
    y[g] ~ multinomial(p);
  }
}
generated quantities {
  array[G, D] int y_rep_multinomial;
  vector[G] log_lik;
  for (g in 1:G) {
    y_rep_multinomial[g] = multinomial_rng(p, n_slots[g]);
    log_lik[g] = multinomial_lpmf(y[g] | p);
  }
}
