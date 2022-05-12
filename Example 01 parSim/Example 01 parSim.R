# This example was taken from r-bloggers
# https://www.r-bloggers.com/2019/08/simulation-studies-in-r-with-the-parsim-package/
#   
# The objective is explore the bias of beta0^ and beta1^ in a linear regression

library("parSim")

parSim(
  ### SIMULATION CONDITIONS
  sampleSize = c(50, 100, 250, 500, 1000),
  
  reps = 100,                   # 100 repetitions
  write = TRUE,                 # Writing to a file
  name = "parSim_regression",   # Name of the file
  nCores = 1,                   # Number of cores to use
  expression = {
    # True beta coefficient (random):
    trueBeta <- rnorm(1)
    
    # Generate data:
    X <- rnorm(sampleSize)
    Y <- trueBeta * X + rnorm(sampleSize)
    
    # Run analysis:
    fit <- lm(Y ~ X)
    
    # Store coefficients:
    coefs <- coef(fit)
    
    # Intercept bias (beta0 = 0):
    interceptBias <- abs(coefs[1])
    
    # Slope bias:
    slopeBias <- abs(coefs[2] - trueBeta)
    
    # Results list:
    Results <- list(
      interceptBias = interceptBias,
      slopeBias = slopeBias
    )
    
    # Return:
    Results
  }
)
