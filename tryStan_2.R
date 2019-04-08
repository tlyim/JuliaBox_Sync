#.libPaths( c( "~/R_library/3.5", .libPaths() ) )

#library(magrittr)
#library(dplyr)
#library(tidyverse)

#load previously saved Stan model objects
#load(file = "StanObjects.rda")
#library(rstan)
rstan_options(auto_write = TRUE) # avoid recompilation of unchanged Stan programs

# The following can affect distributional computing over a cluster
#options(mc.cores = parallel::detectCores())  # 

# The following throws an error in compiling .stan
#Sys.setenv(LOCAL_CPPFLAGS = '-march=native') # For improved execution time but can throw errors for some CPUs




# Save/Load Stan objects; Simulate data and estimate underlying parameters
######################################################################################
#```{r}


N = 29*1 #5 #+ 50  #9999

#set.seed(88)  # set the seed for random generation to ensure replicability

# Z matrix is composed of the columns of earnings components
Z <- matrix(data = c(rep(1, N), rnorm(N), rnorm(N), rnorm(N)), 
            nrow = N)
colnames(Z) <- c("Rev2Rev", "COGS2Rev", "SGA2Rev", "RnD2Rev")

#set actual parameters of the data generation process of the reported earnings (to total revenue ratio) 
a = c(0.9, -0.4, -0.2, -0.05)
avec <- matrix(data = a, nrow = length(a))  # define column vector as a matrix[N,1]

# Simulate the earnings based on a linear model (Later, this model will be estimated with observed data)
sd_r = 0.15
r <- rnorm(N, mean = (Z %*% avec), sd = sd_r)  # reported company earnings, scaled by Rev, total revenue)

mod_OLS <- lm(r ~ . + 0, data = data.frame(r, Z))  # + 0 to remove intercept as Rev2Rev is the unit vector for intercept
summary(mod_OLS)

integrity = 0.8       # integrity level of the society, interpreted as likelihood to behave opportunistically
M <- rbinom(N, 1, integrity)   # the decision to misreport earnings


# Simulate GP data
rho_T <- 5.5   # set true parameters for the exponential kernel of the GP
alpha_T <- 3
sigma_T <- 2

g = 1 # approx. about g times of N
G_total = g*(N+1) + 1
t_total <- 20 * (0:(G_total - 1)) / (G_total - 1) - 10   # rescale the range of z to [-10,10]

sim_GP_data <- list(rho=rho_T, 
                  alpha=alpha_T, 
                  sigma=sigma_T, 
                  N=G_total, 
                  x=t_total)

#library(magrittr)

timeused <- system.time({
simu_fit <- stan("simu_GP.stan", data=sim_GP_data, iter=1,
            chains=1, algorithm="Fixed_param")                        #seed=987, 
})

print(timeused)
cat("\n\n")
ls() %>% print()

    
m_total <- extract(simu_fit)$y[1,]  # misreporting extent = the sample y of from the true realization GP f
# true_realization <- data.frame(f_total = extract(simu_fit)$f[1,], 
#                                t_total = t_total)
observed_t <- c(g*(1:N))  # Get N observed points evenly over the range [-10,10]
t <- t_total[observed_t]     
m <- m_total[observed_t]


# Prepare simulated earnings management (EM) data for estimation using MCMC
N_new = 3
sim_EM_data <- list(N = N, 
                 K = length(a),
                 r = r, 
                 Z = Z,
                 M = M,
                 G = N,
                 t = t,
                 m = m,
                 Z_new = Z[1:N_new,],
                 N_new = N_new
                 )

#```
######################################################################################

# Fit the debug and full model 
######################################################################################
#```{r}
# Run the debug model to make sure the Stan code complies properly 
fit0_debug <- stan("misreport.stan", data = sim_EM_data, iter = 10, chains = 1)


ls()
#save(simu_GP, misreport, file = "StanObjects.rda")
#load(file = "StanObjects.rda")


# Run the full model and refer to the debug model to save compilation time 
timeused <- system.time({
fit1 <- stan( 
#  file = "misreport.stan",  # Stan program
  data = sim_EM_data,    # named list of data
  fit = fit0_debug,   # to save compilation time if the debug model was run
  control = list(adapt_delta = 0.90),    # adjust when there're divergent transitions after warmup
  chains = 4,             # default is 4 Markov chains
#  cores = 2,  #8,
#  init = init, # where init = list(list(mu=...,sigma=...), list(mu=..., sigma=...), ...) 
                # where length of list = number of chains
#  seed = 555,
  iter = 300, #500 #2000,            # total number of iterations per chain
#  warmup = 1000,          # number of warmup iterations per chain
  refresh = 100#0          # = 0 means no progress shown
  )
}) 
print(timeused)
cat("\n\n\n")

#```
######################################################################################

# Summary of the posterior sample 
######################################################################################
#```{r}

ss_complete_pool <- extract(fit1);
# rho_T <- 5.5, alpha_T <- 3, sigma_T <- 2
print(fit1, probs = c(0.1, 0.5, 0.9), digits = 3) #, probs = c(.05,.95))

source('stan_utility.R')
check_all_diagnostics(fit1)

#```
######################################################################################


#```{r}
warnings ()
sessionInfo ()
#```


