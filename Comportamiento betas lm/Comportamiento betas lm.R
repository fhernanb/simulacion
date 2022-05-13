
# Functions to use --------------------------------------------------------

# True values
beta0 <- 47
beta1 <- -0.008
sig <- 3

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
nsim <- 1000

lapply(1:length(N), function(i) {
  cat(i, " ")
  simul(nsim=nsim, n=N[i])
})



# To load the simulated data ----------------------------------------------

datos <- read.table('Comportamiento estimadores en RLS/results.txt')
colnames(datos) <- c('Intercept', 'Slope', 'Sigma', 'n')
 
require(dplyr)  # A useful package to manage dataframes
dat <- datos %>% group_by(n) %>% 
  summarise(int=mean(Intercept),
            slo=mean(Slope),
            sig=mean(Sigma))

# To create the graph -----------------------------------------------------

par(mfrow=c(1, 3))
with(dat, plot(x=n, y=int, type='l', las=1,
               main=expression(hat(beta[0]))))
abline(h=beta0, col='blue', lty='dashed')
with(dat, plot(x=n, y=slo, type='l', las=1,
               main=expression(hat(beta[1]))))
abline(h=beta1, col='blue', lty='dashed')
with(dat, plot(x=n, y=sig, type='l', las=1,
               main=expression(hat(sigma))))
abline(h=sig, col='blue', lty='dashed')





