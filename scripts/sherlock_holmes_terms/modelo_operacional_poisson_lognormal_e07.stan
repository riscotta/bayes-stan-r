// Etapa 07 - modelo operacional principal
// Poisson-lognormal hierarquico para contagens obra-termo com offset de exposicao.
// Versao operacional congelada para a E07; no ambiente atual nao houve compilacao CmdStan por ausencia de R/cmdstanr/CmdStan.

data {
  int<lower=1> N;                         // celulas obra-termo
  int<lower=1> W;                         // obras
  int<lower=1> T;                         // termos
  array[N] int<lower=1, upper=W> work_id;
  array[N] int<lower=1, upper=T> term_id;
  array[N] int<lower=0> y;                // ocorrencias do termo na obra
  vector<lower=0>[N] exposure_10k;        // tokens literarios / 10000
  array[W] int<lower=0, upper=1> is_novel_work;
  vector[W] year_c_work;                  // ano centrado em decadas
}

parameters {
  real mu_alpha;
  real<lower=0> sigma_alpha;
  vector[T] alpha;                        // log taxa basal por termo

  vector[T] beta_novel;                   // efeito romance vs conto por termo
  vector[T] beta_year;                    // efeito por decada por termo

  real<lower=0> sigma_work;
  vector[W] z_work;

  vector<lower=0>[T] sigma_work_term;
  matrix[W, T] z_work_term;
}

transformed parameters {
  vector[W] a_work = sigma_work * z_work;
  matrix[W, T] a_work_term;
  for (w in 1:W) {
    for (t in 1:T) {
      a_work_term[w, t] = sigma_work_term[t] * z_work_term[w, t];
    }
  }
}

model {
  vector[N] eta;

  // Priors fracamente informativos na escala do log da taxa por 10 mil tokens.
  mu_alpha ~ normal(log(3), 2);
  sigma_alpha ~ normal(0, 1.5);
  alpha ~ normal(mu_alpha, sigma_alpha);

  beta_novel ~ normal(0, 0.75);
  beta_year ~ normal(0, 0.35);

  sigma_work ~ normal(0, 0.75);
  z_work ~ std_normal();

  sigma_work_term ~ normal(0, 1);
  to_vector(z_work_term) ~ std_normal();

  for (n in 1:N) {
    int w = work_id[n];
    int t = term_id[n];
    eta[n] = log(exposure_10k[n])
             + alpha[t]
             + beta_novel[t] * is_novel_work[w]
             + beta_year[t] * year_c_work[w]
             + a_work[w]
             + a_work_term[w, t];
  }

  y ~ poisson_log(eta);
}

generated quantities {
  vector[N] log_lik;
  vector[N] rate_per_10k;
  array[N] int y_rep;

  for (n in 1:N) {
    int w = work_id[n];
    int t = term_id[n];
    real eta_rate = alpha[t]
                    + beta_novel[t] * is_novel_work[w]
                    + beta_year[t] * year_c_work[w]
                    + a_work[w]
                    + a_work_term[w, t];
    real eta_count = log(exposure_10k[n]) + eta_rate;

    rate_per_10k[n] = exp(eta_rate);
    log_lik[n] = poisson_log_lpmf(y[n] | eta_count);

    // Protecao contra overflow do RNG em cenarios extremos.
    if (eta_count < 20) {
      y_rep[n] = poisson_log_rng(eta_count);
    } else {
      y_rep[n] = -1;
    }
  }
}
