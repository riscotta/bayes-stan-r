// E06_modelo_ordinal_dificuldade_com_equipamento_diagnostico.stan
// Especificação candidata para a Etapa 07. Não foi compilada nem executada na E06.
// Uso: diagnóstico da dominância de equipment_group sobre difficulty. Não interpretar causalmente.

data {
  int<lower=1> N;
  int<lower=3> K_y;
  array[N] int<lower=1, upper=K_y> y;
  int<lower=1> K_body;
  array[N] int<lower=1, upper=K_body> body;
  int<lower=1> K_equipment;
  array[N] int<lower=1, upper=K_equipment> equipment_group;
  int<lower=1> K_cat;
  array[N] int<lower=1, upper=K_cat> category_group;
  vector[N] z_instruction_steps;
  vector[N] z_secondary_muscles;
}
parameters {
  ordered[K_y - 1] cutpoints;
  real beta_steps;
  real beta_secondary;
  vector[K_body] z_body;
  real<lower=0> sigma_body;
  vector[K_equipment] z_equipment;
  real<lower=0> sigma_equipment;
  vector[K_cat] z_category;
  real<lower=0> sigma_category;
}
transformed parameters {
  vector[K_body] a_body = sigma_body * z_body;
  vector[K_equipment] a_equipment = sigma_equipment * z_equipment;
  vector[K_cat] a_category = sigma_category * z_category;
}
model {
  cutpoints ~ normal(0, 2);
  beta_steps ~ normal(0, 1);
  beta_secondary ~ normal(0, 1);
  z_body ~ std_normal();
  z_equipment ~ std_normal();
  z_category ~ std_normal();
  sigma_body ~ normal(0, 0.7);
  sigma_equipment ~ normal(0, 0.7);
  sigma_category ~ normal(0, 0.7);

  for (n in 1:N) {
    real eta = beta_steps * z_instruction_steps[n]
             + beta_secondary * z_secondary_muscles[n]
             + a_body[body[n]]
             + a_equipment[equipment_group[n]]
             + a_category[category_group[n]];
    y[n] ~ ordered_logistic(eta, cutpoints);
  }
}
generated quantities {
  array[N] int<lower=1, upper=K_y> y_rep;
  vector[N] log_lik;
  for (n in 1:N) {
    real eta = beta_steps * z_instruction_steps[n]
             + beta_secondary * z_secondary_muscles[n]
             + a_body[body[n]]
             + a_equipment[equipment_group[n]]
             + a_category[category_group[n]];
    y_rep[n] = ordered_logistic_rng(eta, cutpoints);
    log_lik[n] = ordered_logistic_lpmf(y[n] | eta, cutpoints);
  }
}
