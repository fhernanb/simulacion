# This example was taken from r-bloggers
# https://www.r-bloggers.com/2019/08/simulation-studies-in-r-with-the-parsim-package/
#   
# The objective is explore the bias of beta0^ and beta1^ in a linear regression


# To perform the simulation -----------------------------------------------

library("parSim")

parSim(
  ### SIMULATION CONDITIONS
  sampleSize = c(50, 100, 250, 500, 1000),
  
  reps = 100,                          # 100 repetitions
  write = TRUE,                        # Writing to a file
  name = "results_example_parSim_01",  # Name of the file
  nCores = 1,                          # Number of cores to use
  
  expression = {
    # True intercept (fixed):
    trueIntercept <- 0
    
    # True beta coefficient (random):
    trueBeta <- rnorm(1)
    
    # Generate data:
    X <- rnorm(sampleSize)
    Y <- trueIntercept + trueBeta * X + rnorm(sampleSize)
    
    # Run analysis:
    fit <- lm(Y ~ X)
    
    # Store coefficients:
    coefs <- coef(fit)
    
    # Intercept bias:
    interceptBias <- abs(coefs[1] - trueIntercept)
    
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

# To load the results -----------------------------------------------------

table <- read.table("results_example_parSim_01.txt", header = TRUE)


# To analize the results --------------------------------------------------

library("ggplot2")
library("dplyr")
library("tidyr")

# Gather results:
table_gather <- table %>% gather(measure,value,interceptBias:slopeBias)

# Plot:
ggplot(table_gather, aes(x=factor(sampleSize), y=value, fill = measure)) + 
  geom_boxplot(outlier.size = 0.5,lwd=0.5,fatten=0.5) + 
  xlab("Sample Size") + 
  ylab("Bias") + 
  theme_bw() + 
  theme( panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank()) +
  geom_vline(xintercept=seq(1.5, length(unique(table_gather$sampleSize))-0.5, 1),
             lwd=0.5, colour="black", alpha = 0.25) 
