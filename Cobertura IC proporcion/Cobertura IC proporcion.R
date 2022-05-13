# URL for paper
# http://www.stat.ufl.edu/~aa/articles/agresti_caffo_2000.pdf

# Exploring ---------------------------------------------------------------

prop <- 0.7 
n <- 10

x <- rbinom(n=n, prob=prop, size=1)
x

icprop <- function(x, conf.level) {
  p <- sum(x) / length(x)
  n <- length(x)
  Z <- qnorm(p=(1-conf.level)/2, lower.tail=F)
  p + Z * c(-1, 1) * sqrt(p*(1-p)/n)
}

icprop(x=x, conf.level=0.90)


# Function to generate one IC ---------------------------------------------
one.simul <- function(prop, n, conf) {
  x <- rbinom(n=n, prob=prop, size=1)
  ic <- icprop(x=x, conf.level=conf)
  rta <- ic[1] <= prop & prop <= ic[2]
  return(rta)
}

simul <- function(nsim, prop, n, conf) {
  res <- replicate(n=nsim, one.simul(prop=prop, n=n, conf=conf))
  results <- cbind(mean(res), prop, n, conf)
  write(x=t(results), 
        file='Cobertura IC proporcion/results_prop.txt', 
        ncolumns=4, append=TRUE)
}

# To simulate -------------------------------------------------------------

props <- seq(from=0.01, to=0.99, by=0.01)
ns <- c(5, 10, 20)
confs <- c(0.95, 0.99)
nsim <- 10000

params <- expand.grid(props=props, ns=ns, confs=confs)
NROW(params)

lapply(1:NROW(params), function(i) {
  cat(i, " ")
  r <- as.numeric(unlist(params[i, ]))
  simul(nsim=nsim, prop=r[1], n=r[2], conf=r[3])
})


# To plot the results -----------------------------------------------------

datos <- read.table('Cobertura IC proporcion/results_prop.txt')
names(datos) <- c('cover', 'true.prop', 'n', 'conf')
head(datos)

# Cobertura versus verdadera prop

myplot <- function(n, conf) {
  myconf <- conf
  myn <- n
  dt <-  datos[datos$n == n & datos$conf == conf, ]
  with(dt, plot(x=true.prop, y=cover, type='l', las=1,
                ylab='Coverage rate', ylim=c(0.7, 1),
                xlab='True proportion', lty='dashed',
                main=paste('Conf level = ', myconf, 'with n = ' , myn)))
  abline(h=conf, col='red')
}

# Replicando algunas figuras del articulo

par(mfrow=c(2, 3))

myplot( 5, 0.95)
myplot(10, 0.95)
myplot(20, 0.95)

myplot( 5, 0.99)
myplot(10, 0.99)
myplot(20, 0.99)


# Cobertura versus n

dt <- subset(datos, true.prop == 0.10 & conf == 0.95)
dt <- dt[order(dt$n), ]
with(dt, plot(x=n, y=cover, type='b', las=1,
              ylab='Coverage rate'))


