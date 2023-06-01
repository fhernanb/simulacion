library(gamlss)

# Vamos a definir el directorio de trabajo donde
# se almacenaran el script, simulaciones y graficos

setwd("Estudio_simul_nueva_dist")

#-----------------------------------------------------------------
#------------------ EJEMPLO CON LA DISTRIBUCION ------------------
#----------------------- Zero inflated Poisson -------------------
#---------------------------- ZIP --------------------------------
#-----------------------------------------------------------------

# En la distribucion ZIP
# mu > 0,         por tanto usaremos funcion de enlace log
# 0 < sigma < 1,  por tanto usaremos funcion de enlace logit

# Creando las funciones de enlace inversas
logit_inv <- function(x) exp(x) / (1 + exp(x))
# No vamos a crear log_inv porque esa funcion ya existe y 
# llama exp( )

# The parameters ----------------------------------------------------------
# Los siguientes son los valores de los betas para el modelo de regresion
# debemos chequear que esos numeros den valores correctos de mu y sigma
# no podemos asignar numeros a la loca

true_b0_mu <- -2    # intercept for mu
true_b1_mu <- 1.5   # slope for mu
true_g0_si <- -1    # intercept for sigma
true_g1_si <- 1.4   # slope for sigma

# Useful functions to the simulation study --------------------------------

# Funcion para obtener mu_hat y sigma_hat para un valor fijo de n
simul_one <- function(size) {
  x1 <- runif(n=size)
  x2 <- runif(n=size)
  mu    <-       exp(true_b0_mu + true_b1_mu * x1)
  sigma <- logit_inv(true_g0_si + true_g1_si * x2)
  y <- rZIP(n=size, mu=mu, sigma=sigma)
  mod <- NULL
  mod <- try(gamlss(y~x1, sigma.fo=~x2, family='ZIP',
                    control=gamlss.control(n.cyc=2500, trace=FALSE)))
  if (class(mod)[1] == "try-error")
    res <- rep(NA, 4)
  else
    res <- c(coef(mod, what='mu'), coef(mod, what='sigma'))
  res
}

# Super function to simulate and write the estimated parameters
simul <- function(n) {
  result <- t(replicate(n=nrep, expr=simul_one(size=n)))
  result <- cbind(result, n)
  write(x=t(result), file='simul_with_cov.txt', 
        ncol=5, append=TRUE)
}

# Code to generate the simulations given n --------------------------------

# Aqui se definen los valores de tamano muestral n
# Luego se define el numero de repeticiones
n <- seq(from=20, to=300, by=20)
nrep <- 10

values <- expand.grid(n=n)
values
apply(values, 1, simul)

# Plots -------------------------------------------------------------------
dt <- read.table('simul_with_cov.txt', 
                 col.names=c('b0_mu_hat', 'b1_mu_hat', 
                             'g0_si_hat', 'g1_si_hat', 
                             'n'))

library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

# Numero de observaciones por cada n
num <- dt %>% group_by(n) %>% count()
mean(num$nn)
min(num$nn)

# Para obtener la metricas
res <- dt %>% 
  drop_na() %>% 
  group_by(n) %>% 
  summarise(mean_b0_mu=mean(b0_mu_hat),
            mean_b1_mu=mean(b1_mu_hat),
            mean_b0_si=mean(g0_si_hat),
            mean_b1_si=mean(g1_si_hat),
            mse_b0_mu=mean((true_b0_mu - b0_mu_hat)^2), 
            mse_b1_mu=mean((true_b1_mu - b1_mu_hat)^2),
            mse_b0_si=mean((true_g0_si - g0_si_hat)^2), 
            mse_b1_si=mean((true_g1_si - g1_si_hat)^2))

# Mean -----------------------------------------------------
p1 <- ggplot(data=res, aes(x=n, y=mean_b0_mu)) + 
  geom_line() + 
  labs(x="n", y=expression(hat(beta)[0]), 
       title=expression("Mean for the intercept in" ~ mu)) +
  geom_line(y=true_b0_mu, col='red', lty='dashed')

p2 <- ggplot(data=res, aes(x=n, y=mean_b1_mu)) + 
  geom_line() + 
  labs(x="n", y=expression(hat(beta)[1]),
       title=expression("Mean for the slope in" ~ mu)) +
  geom_line(y=true_b1_mu, col='red', lty='dashed')

p3 <- ggplot(data=res, aes(x=n, y=mean_b0_si)) + 
  geom_line() + 
  labs(x="n", y=expression(hat(gamma)[0]),
       title=expression("Mean for the intercept in" ~ sigma)) +
  geom_line(y=true_g0_si, col='red', lty='dashed')

p4 <- ggplot(data=res, aes(x=n, y=mean_b1_si)) + 
  geom_line() + 
  labs(x="n", y=expression(hat(gamma)[1]),
       title=expression("Mean for the slope in" ~ sigma)) +
  geom_line(y=true_g1_si, col='red', lty='dashed')

mean2 <- grid.arrange(p1, p2, p3, p4, nrow=2, ncol=2)
mean2
ggsave(filename="mean2.pdf", 
       plot=mean2, 
       width=10, height=8)

# MSE -----------------------------------------------------
p1 <- ggplot(data=res, aes(x=n, y=mse_b0_mu)) + 
  geom_line() + 
  labs(x="n", y=expression(MSE~hat(beta)[0]),
       title=expression("MSE for the intercept in" ~ mu))

p2 <- ggplot(data=res, aes(x=n, y=mse_b1_mu)) + 
  geom_line() + 
  labs(x="n", y=expression(MSE~hat(beta)[1]),
       title=expression("MSE for the slope in" ~ mu))

p3 <- ggplot(data=res, aes(x=n, y=mse_b0_si)) + 
  geom_line() + 
  labs(x="n", y=expression(MSE~hat(gamma)[0]),
       title=expression("MSE for the intercept in" ~ sigma))

p4 <- ggplot(data=res, aes(x=n, y=mse_b1_si)) + 
  geom_line() + 
  labs(x="n", y=expression(MSE~hat(gamma)[1]),
       title=expression("MSE for the slope in" ~ sigma))

mse2 <- grid.arrange(p1, p2, p3, p4, nrow=2, ncol=2)
mse2
ggsave(filename="mse2.pdf", 
       plot=mse2, 
       width=10, height=8)

