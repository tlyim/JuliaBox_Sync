.libPaths( c( "~/R_library/3.5", .libPaths() ) )
.libPaths()

system("g++ -v")

sessionInfo()


library(rstan)
rstan_options(auto_write = TRUE) # avoid recompilation of unchanged Stan programs

# The following can affect distributional computing over a cluster
options(mc.cores = parallel::detectCores())  # 




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

simu_fit <- stan("simu_GP.stan", data=sim_GP_data, iter=1,
            chains=1, algorithm="Fixed_param")  
                      #seed=987, 

