functions {
  vector X_model(real time, vector y, array[] real params) {
    vector[20] dydt;
    real E_to_I_A;
    real E_to_I_B;
    real E_to_I_C;
    real E_to_I_D;
    real I_to_R_A;
    real I_to_R_B;
    real I_to_R_C;
    real I_to_R_D;
    real total_N;
    real var_psi_A;
    real var_psi_B;
    real var_psi_C;
    real var_psi_D;
    real C_in_A;
    real C_in_B;
    real C_in_C;
    real C_in_D;
    real S_to_E_A;
    real S_to_E_B;
    real S_to_E_C;
    real S_to_E_D;
    E_to_I_A = 0.5*y[5];
    E_to_I_B = 0.5*y[6];
    E_to_I_C = 0.5*y[7];
    E_to_I_D = 0.5*y[8];
    I_to_R_A = 0.5*y[9];
    I_to_R_B = 0.5*y[10];
    I_to_R_C = 0.5*y[11];
    I_to_R_D = 0.5*y[12];
    total_N = 10000+10000+10000+10000;
    var_psi_A = 0*y[9]+0*y[10]+0*y[11]+0*y[12];
    var_psi_B = 0*y[9]+0*y[10]+0*y[11]+0*y[12];
    var_psi_C = 0*y[9]+0*y[10]+0*y[11]+0*y[12];
    var_psi_D = 0*y[9]+0*y[10]+0*y[11]+0*y[12];
    C_in_A = params[1]*E_to_I_A;
    C_in_B = params[1]*E_to_I_B;
    C_in_C = params[1]*E_to_I_C;
    C_in_D = params[1]*E_to_I_D;
    S_to_E_A = var_psi_A*y[1]/total_N;
    S_to_E_B = var_psi_B*y[2]/total_N;
    S_to_E_C = var_psi_C*y[3]/total_N;
    S_to_E_D = var_psi_D*y[4]/total_N;
    dydt[1] = -S_to_E_A;
    dydt[2] = -S_to_E_B;
    dydt[3] = -S_to_E_C;
    dydt[4] = -S_to_E_D;
    dydt[5] = S_to_E_A-E_to_I_A;
    dydt[6] = S_to_E_B-E_to_I_B;
    dydt[7] = S_to_E_C-E_to_I_C;
    dydt[8] = S_to_E_D-E_to_I_D;
    dydt[9] = E_to_I_A-I_to_R_A;
    dydt[10] = E_to_I_B-I_to_R_B;
    dydt[11] = E_to_I_C-I_to_R_C;
    dydt[12] = E_to_I_D-I_to_R_D;
    dydt[13] = I_to_R_A;
    dydt[14] = I_to_R_B;
    dydt[15] = I_to_R_C;
    dydt[16] = I_to_R_D;
    dydt[17] = C_in_A;
    dydt[18] = C_in_B;
    dydt[19] = C_in_C;
    dydt[20] = C_in_D;
    return dydt;
  }
}
data {
  int<lower = 1> n_obs;
  int<lower = 1> n_params;
  int<lower = 1> n_difeq;
  array[n_obs] int y_A;
  array[n_obs] int y_B;
  array[n_obs] int y_C;
  array[n_obs] int y_D;
  real t0;
  array[n_obs] real ts;
  vector[n_difeq] x0;
}
parameters {
  real<lower = 0, upper = 1> par_rho;
}
transformed parameters{
  array[n_obs] vector[n_difeq] x; // Output from the ODE solver
  array[n_params] real params;
  array[n_obs] real delta_x_1;
  array[n_obs] real delta_x_2;
  array[n_obs] real delta_x_3;
  array[n_obs] real delta_x_4;
  params[1] = par_rho;
  x = ode_rk45(X_model, x0, t0, ts, params);
  delta_x_1[1] =  x[1, 17] - x0[17] + 1e-5;
  delta_x_2[1] =  x[1, 18] - x0[18] + 1e-5;
  delta_x_3[1] =  x[1, 19] - x0[19] + 1e-5;
  delta_x_4[1] =  x[1, 20] - x0[20] + 1e-5;
  for (i in 1:n_obs-1) {
    delta_x_1[i + 1] = x[i + 1, 17] - x[i, 17] + 1e-5;
    delta_x_2[i + 1] = x[i + 1, 18] - x[i, 18] + 1e-5;
    delta_x_3[i + 1] = x[i + 1, 19] - x[i, 19] + 1e-5;
    delta_x_4[i + 1] = x[i + 1, 20] - x[i, 20] + 1e-5;
  }
}
model {
  par_rho ~ beta(2, 2);
  y_A ~ poisson(delta_x_1);
  y_B ~ poisson(delta_x_2);
  y_C ~ poisson(delta_x_3);
  y_D ~ poisson(delta_x_4);
}
generated quantities {
  real log_lik;
  log_lik = poisson_lpmf(y_A | delta_x_1)+poisson_lpmf(y_B | delta_x_2)+poisson_lpmf(y_C | delta_x_3)+poisson_lpmf(y_D | delta_x_4);
}