# This example was taken from the github repository
# https://github.com/SachaEpskamp/parSim
#   
# The objective is explore the bias of beta0^ and beta1^ in a linear regression

# To perform the simulation -----------------------------------------------

library("parSim")

# Some function we might use:
bias <- function(x, y) {abs(x-y)}

# Run the simulation:
Results <- parSim(
  # Any number of conditions:
  sampleSize = c(50, 100, 250),
  beta = c(0, 0.5, 1),
  sigma = c(0.25, 0.5, 1),
  
  # Number of repetitions?
  reps = 100,
  
  # Parallel?
  nCores = 1,
  
  # Write to file?
  write = TRUE,
  
  # Name of the file
  name = "results_example_parSim_02",           
  
  # Export objects (only needed when nCores > 1):
  export = c("bias"),
  
  # R expression:
  expression = {
    # Load all R packages in the expression if needed
    # library(...)
    
    # Want to debug? Enter browser() and run the function. Only works with nCores = 1!
    # browser()
    
    # Enter whatever codes you want. I can use the conditions as objects.
    X <- rnorm(sampleSize)
    Y <- beta * X + rnorm(sampleSize, sigma)
    fit <- lm(Y ~ X)
    betaEst <- coef(fit)[2]
    Rsquared <- summary(fit)$r.squared
    
    # Make a data frame with one row to return results (multple rows is also possible but not reccomended):
    data.frame(
      betaEst = betaEst,
      bias = bias(beta, betaEst),
      Rsquared = Rsquared
    )
  }
)

# To load the results -----------------------------------------------------

Results <- read.table("results_example_parSim_02.txt", header = TRUE)


# To analize the results --------------------------------------------------

library("ggplot2")
library("tidyr")

# We want to plot bias and R-squared. Let's first make it long format:
Long <- gather(Results,metric,value,bias:Rsquared)

# Make factors with nice labels:
Long$sigmaFac <- factor(Long$sigma,levels = c(0.25,0.5,1), 
                        labels = c("Sigma: 0.025", "Sigma: 0.5", "Sigma: 1"))

# Now let's plot:
g <- ggplot(Long, aes(x = factor(sampleSize), y = value, fill = factor(beta)))  +
  facet_grid(metric ~ sigmaFac, scales = "free") + 
  geom_boxplot() + 
  theme_bw() + 
  xlab("Sample size") + 
  ylab("") + 
  scale_fill_discrete("Beta")
print(g)
