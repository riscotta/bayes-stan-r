data {
  int<lower=2> D;                         // number of dezenas; expected: 60
  int<lower=2> G;                         // number of levels of the modeled factor
  array[G, D] int<lower=0> y;             // counts: factor level x dezena
  array[G] int<lower=1> n_slots;          // row totals; usually 6 * number of contests in level
}
transformed data {
  int total_slots = sum(n_slots);
}
parameters {
  sum_to_zero_vector[D] alpha;            // baseline logit profile over dezenas
  array[G] sum_to_zero_vector[D] z_delta; // standardized level-specific deviations
  real<lower=0> tau_delta;                // global scale of factor x dezena deviations
}
transformed parameters {
  simplex[D] p_base;
  array[G] simplex[D] p;

  p_base = softmax(alpha);
  for (g in 1:G) {
    p[g] = softmax(alpha + tau_delta * z_delta[g]);
  }
}
model {
  alpha ~ normal(0, 0.35);
  for (g in 1:G) {
    z_delta[g] ~ normal(0, 1);
  }
  tau_delta ~ normal(0, 0.20);            // half-normal by lower bound

  for (g in 1:G) {
    y[g] ~ multinomial(p[g]);
  }
}
generated quantities {
  array[G, D] int y_rep_multinomial;
  vector[G] log_lik;
  real contraste_total_base = 0;
  real max_abs_delta_100_concursos = 0;

  for (g in 1:G) {
    y_rep_multinomial[g] = multinomial_rng(p[g], n_slots[g]);
    log_lik[g] = multinomial_lpmf(y[g] | p[g]);
    for (d in 1:D) {
      real delta = p[g][d] - p_base[d];
      real delta_100_concursos = 600 * delta;
      contraste_total_base += (n_slots[g] * 1.0 / total_slots) * square(delta) / p_base[d];
      if (abs(delta_100_concursos) > max_abs_delta_100_concursos) {
        max_abs_delta_100_concursos = abs(delta_100_concursos);
      }
    }
  }
}
