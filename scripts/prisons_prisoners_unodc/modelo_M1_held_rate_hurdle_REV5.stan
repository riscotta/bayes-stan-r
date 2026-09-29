// E07 REV5 — M1; corrigido apenas o mecanismo dos zeros.
// Grupos: 1 = paises exceto Vaticano; 2 = Vaticano (id 212).
// As duas probabilidades tem prioris independentes e Beta conjugadas.
// O modelo da magnitude positiva reproduz a especificacao da REV4.1.
data {
  int<lower=1> N;
  int<lower=1> J;
  int<lower=2> R;
  int<lower=2> T;
  array[N] int<lower=1, upper=J> country;
  array[J] int<lower=1, upper=R> country_region;
  array[N] int<lower=1, upper=T> year_id;
  vector<lower=0>[N] held_rate;
  int<lower=1, upper=J> vat_id;
  int<lower=0> n_nonvat;
  int<lower=0> n_vat;
  int<lower=0, upper=1> prior_only;
}
parameters {
  real alpha;
  sum_to_zero_vector[R] region_base;
  vector[J] z_country;
  real<lower=0> sigma_country;

  vector[T - 1] z_global_rw;
  real<lower=0> sigma_global_rw;
  array[T - 1] sum_to_zero_vector[R] z_region_rw;
  real<lower=0> sigma_region_rw;

  real<lower=0> sigma_obs;
  real<lower=0> nu_minus_two;

  // Sem efeito individual nao identificavel nos 223 paises sem zeros.
  vector<lower=0, upper=1>[2] p_zero;
}
transformed parameters {
  vector[J] country_effect = sigma_country * z_country;
  vector[T] global_time;
  matrix[R, T] region_time;
  real<lower=2> nu = nu_minus_two + 2;

  global_time[1] = 0;
  for (t in 2:T) {
    global_time[t] = global_time[t - 1]
                     + sigma_global_rw * z_global_rw[t - 1];
  }
  global_time -= mean(global_time);

  for (r in 1:R) region_time[r, 1] = 0;
  for (t in 2:T) {
    for (r in 1:R) {
      region_time[r, t] = region_time[r, t - 1]
                         + sigma_region_rw * z_region_rw[t - 1][r];
    }
  }
}
model {
  // Priors positivos identicos aos da REV4.1.
  alpha ~ normal(log(150), 0.40);
  region_base ~ normal(0, 0.20);
  z_country ~ std_normal();
  sigma_country ~ normal(0, 0.35);
  z_global_rw ~ std_normal();
  sigma_global_rw ~ normal(0, 0.07);
  for (t in 1:(T - 1)) z_region_rw[t] ~ std_normal();
  sigma_region_rw ~ normal(0, 0.05);
  sigma_obs ~ normal(0, 0.25);
  nu_minus_two ~ exponential(0.10);

  // Media previa = 1/2000 = 0.0005 fora do Vaticano.
  p_zero[1] ~ beta(1, 1999);
  // Excecao por pais exploratoria, nao interpretada como causa.
  p_zero[2] ~ beta(1, 1);

  if (prior_only == 0) {
    for (n in 1:N) {
      int j = country[n];
      int r = country_region[j];
      int g = 1;
      real eta;
      if (j == vat_id) g = 2;
      eta = alpha + region_base[r] + country_effect[j]
            + global_time[year_id[n]] + region_time[r, year_id[n]];
      if (held_rate[n] == 0) {
        target += bernoulli_lpmf(1 | p_zero[g]);
      } else {
        target += bernoulli_lpmf(0 | p_zero[g]);
        target += student_t_lpdf(log(held_rate[n]) | nu, eta, sigma_obs)
                  - log(held_rate[n]);
      }
    }
  }
}
generated quantities {
  // PPC compacto: sem N vetores de y_rep / log_lik no CSV do CmdStan.
  int<lower=0> zero_rep_nonvat = binomial_rng(n_nonvat, p_zero[1]);
  int<lower=0> zero_rep_vat = binomial_rng(n_vat, p_zero[2]);
}
