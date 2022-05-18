# In this example we are interested in explore the performance of the lm( )
# function to estimate the parameters beta0, beta1 and sigma in a 
# linear regression model.
# The model to consider is 
#     Y ~ NO(mu = 47 - 0.008 * x, sigma = 3)
#     X ~ U(1695, 4105)

# Factors to study
# n = 10, 20, 30, ..., 980, 990, 1000

# Fixed factor:
# True parameters: beta0=47, beta1=-0.008 and sig=3
# Distribution for the covariate X ~ U(1695, 4105)

# Variables to monitor
# ^beta0, ^beta1 and ^sigma

# To perform the simulation -----------------------------------------------

library("parSim")

parSim(
  ### SIMULATION CONDITIONS
  n = seq(from=10, to=1000, by=10),
  
  reps = 1000,      # 100 repetitions
  write = TRUE,     # Writing to a file
  name = "Examples parSim/results_example_parSim_03",  # Name of the file
  nCores = 1,       # Number of cores to use
  
  expression = {
    # True parameter values
    beta0 <- 47
    beta1 <- -0.008
    sig   <- 3
    
    # Generate data:
    x <- runif(n=n, min=1695, max=4105)
    media <- beta0 + beta1 * x
    y <- rnorm(n=n, mean=media, sd=sig)
    dat <- data.frame(y=y, x=x)
    
    # Run analysis:
    mod <- lm(y ~ x, data=dat)
    
    # Store coefficients:
    beta0_hat <- coef(mod)[1]
    beta1_hat <- coef(mod)[2]
    sigma_hat <- summary(mod)$sigma
    
    # Results list:
    Results <- list(
      beta0_hat = beta0_hat,
      beta1_hat = beta1_hat,
      sigma_hat = sigma_hat
      )
    
    # Return:
    Results
  }
)

# To load the results -----------------------------------------------------

datos <- read.table("Examples parSim/results_example_parSim_03.txt", header = TRUE)

# To analize the results --------------------------------------------------

require(dplyr)  # A useful package to manage dataframes

dat <- datos %>% group_by(n) %>% 
  summarise(intercept = mean(beta0_hat),
            slope = mean(beta1_hat),
            sigma = mean(sigma_hat))

dat

# To create the graph -----------------------------------------------------

# True parameter values
beta0 <- 47
beta1 <- -0.008
sig   <- 3

par(mfrow=c(1, 3))
with(dat, plot(x=n, y=intercept, type='l', las=1, main=expression(hat(beta[0]))))
abline(h=beta0, col='dodgerblue2', lty='dashed')
with(dat, plot(x=n, y=slope, type='l', las=1, main=expression(hat(beta[1]))))
abline(h=beta1, col='dodgerblue2', lty='dashed')
with(dat, plot(x=n, y=sigma, type='l', las=1, main=expression(hat(sigma))))
abline(h=sig, col='dodgerblue2', lty='dashed')

