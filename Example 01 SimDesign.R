# Example taken from
# https://cran.r-project.org/web/packages/SimDesign/vignettes/SimDesign-intro.html

# The objective is:
# How does trimming affect recovering the mean of a distribution? 
# Investigate this using different sample sizes with Gaussian and χ2 
# distributions. Also, demonstrate the effect of using the median 
# to recover the mean.

library(SimDesign)


# Define the conditions ---------------------------------------------------

Design <- createDesign(sample_size = c(30, 60, 120, 240), 
                       distribution = c('norm', 'chi'))
Design


# Define the functions ----------------------------------------------------


# We first start by defining the data generation functional component. 
# The only argument accepted by this function is condition, which will 
# always be a single row from the Design data.frame object of class data.frame. 
# Conditions are run sequentially from row 1 to the last row in Design. It is 
# also possible to pass a fixed_objects object to the function for including 
# fixed sets of population parameters and other conditions, however for this 
# simple simulation this input is not required.

Generate <- function(condition, fixed_objects = NULL) {
  N <- condition$sample_size
  dist <- condition$distribution
  if(dist == 'norm'){
    dat <- rnorm(N, mean = 3)
  } else if(dist == 'chi'){
    dat <- rchisq(N, df = 3)
  }
  dat
}

# As we can see above, Generate() will return a numeric vector of length N 
# containing the data to be analysed each with a population mean of 3 
# (because a χ2 distribution has a mean equal to its df). Next, we define the 
# analyse component to analyse said data:

Analyse <- function(condition, dat, fixed_objects = NULL) {
  M0 <- mean(dat)
  M1 <- mean(dat, trim = .1)
  M2 <- mean(dat, trim = .2)
  med <- median(dat)
  
  ret <- c(mean_no_trim=M0, mean_trim.1=M1, mean_trim.2=M2, median=med)
  ret
}

Summarise <- function(condition, results, fixed_objects = NULL) {
  obs_bias <- bias(results, parameter = 3)
  obs_RMSE <- RMSE(results, parameter = 3)
  ret <- c(bias=obs_bias, RMSE=obs_RMSE, RE=RE(obs_RMSE))
  ret
}


# Putting it all together -------------------------------------------------

res <- runSimulation(Design, replications = 1000, generate=Generate, 
                     analyse=Analyse, summarise=Summarise)

res


# Interpreting the results ------------------------------------------------

REs <- res[,grepl('RE\\.', colnames(res))]
data.frame(Design, REs)


