// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N; // number of observations (for different CEOs/firms)
  int<lower=0> K; // number of coefficents (/predictors)
  vector[N] r; // dependent variable (reported company earnings, scaled by Rev, total revenue)
  matrix[N,K] Z; // predictor variables (main components of earnings, all scaled by Rev: 
                      // COGS2Rev, SGA2Rev, RnD2Rev with Rev2Rev = unit vector as the intercept)
  int<lower=0,upper=1> M[N];   // M = 1 for the decision to misreport; = 0 if report honestly  

// forecasts  
  int<lower=0> N_new; // number of predictions
  matrix[N_new,K] Z_new; // 
  
// GP data
  int<lower=0> G; // number of observed points from the realized GP function
  real t[G];     // ie, range of function input (ie, x of f(x)) to the realized GP function f(x)
  vector[G] m;  // misreporting extent (ie, y = f(x) the function value of the realized GP function)
}
transformed data {
// GP data
}
parameters {
  vector[K] b; // coefficients of the predictor variables
  real<lower=0> sd_r; // sd of reported earnings
  real<lower=0,upper=1> integrity;     // integrity level of the society affecting the decision to misreport or not
//  vector[N] integrity;     // integrity level of each CEO affecting the decision to misreport or not

// est'd coefficients for the GP kernel parameters
  real<lower=0> rho;
  real<lower=0> alpha;
  real<lower=0> sigma;
}
transformed parameters {
  real ilogodds;     // log odds of the society integrity level
  ilogodds = logit(integrity);   
}
model {
  matrix[G, G] Kcov;
  matrix[G, G] Lcov;

// b ~ cauchy(0, 2.5); // common prior for each b[K]
  r ~ normal(Z*b, sd_r);
  M ~ bernoulli_logit(ilogodds); 

  rho ~ inv_gamma(5,20);#30);    // (5,5) (8.91924, 34.5805);                    rho_T = 5.5
  alpha ~ gamma(1.5,0.5); // gamma(3,1)   normal(0, 3);     // (0, 2)  (0,1)    alpha_T = 3
  sigma ~ gamma(1,0.5); // gamma(2,1)   normal(0, 2);     // (0, 1.5)  (0,1)  sigma_T = 2    

// https://mc-stan.org/docs/2_18/stan-users-guide/fit-gp-section.html
// https://betanalpha.github.io/assets/case_studies/gp_part3/part3.html
  Kcov = cov_exp_quad(t, alpha, rho) + diag_matrix(rep_vector(square(sigma), G));
//                                                                  ^
  Lcov = cholesky_decompose(Kcov);

  m ~ multi_normal_cholesky(rep_vector(0, G), Lcov);   // rep_vector(0, G) is the zero mean assumed for the GP
}

generated quantities {
  vector[N_new] r_new;
  for (n in 1:N_new)
    r_new[n] = normal_rng(Z_new[n] * b, sd_r);
}



