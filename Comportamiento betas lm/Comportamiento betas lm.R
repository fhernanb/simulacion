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

# Functions to use --------------------------------------------------------

# True parameter values
beta0 <- 47
beta1 <- -0.008
sig   <- 3

# Function to generate one dataset
gendat <- function(n) {
  x <- runif(n=n, min=1695, max=4105)
  media <- beta0 + beta1 * x
  y <- rnorm(n=n, mean=media, sd=sig)
  data.frame(y=y, x=x)
}

# Function to obtain theta vector
one.simul <- function(n) {
  dat <- gendat(n=n)
  mod <- lm(y ~ x, data=dat)
  c(coef(mod), desvi=summary(mod)$sigma)
}

# Function to simulate nsim times
simul <- function(nsim, n) {
  results <- replicate(n=nsim, one.simul(n=n))
  results <- cbind(t(results), n=n)
  write(x=t(results), 
        file='Comportamiento betas lm/results.txt', 
        ncolumns=4, append=T)
}

# To simulate automatically -----------------------------------------------

N <- seq(from=10, to=1000, by=10)
nsim <- 10000

lapply(1:length(N), function(i) {
  cat(i, " ")
  simul(nsim=nsim, n=N[i])
})

# To load the simulated data ----------------------------------------------

datos <- read.table('Comportamiento betas lm/results.txt')
colnames(datos) <- c('Intercept', 'Slope', 'Sigma', 'n')
 
require(dplyr)  # A useful package to manage dataframes
dat <- datos %>% group_by(n) %>% 
  summarise(intercept = mean(Intercept),
            slope = mean(Slope),
            sigma = mean(Sigma))

dat

# To create the graph -----------------------------------------------------

par(mfrow=c(1, 3))
with(dat, plot(x=n, y=intercept, type='l', las=1, main=expression(hat(beta[0]))))
abline(h=beta0, col='dodgerblue2', lty='dashed')
with(dat, plot(x=n, y=slope, type='l', las=1, main=expression(hat(beta[1]))))
abline(h=beta1, col='dodgerblue2', lty='dashed')
with(dat, plot(x=n, y=sigma, type='l', las=1, main=expression(hat(sigma))))
abline(h=sig, col='dodgerblue2', lty='dashed')

