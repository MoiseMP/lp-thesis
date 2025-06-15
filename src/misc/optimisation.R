# Numerical Optimisation using R

# Random parabola
parabola <- function(x) {
  x1 <- x[1]
  return(x1^2)
}

# NOTE: optim minimises the objective function
optim(c(-1), parabola, method= 'BFGS')

# Normal Distribution
y <- rnorm(1000)

llik <- function(args, y) {
  mu <- args[1]
  sigma2 <- args[2]
  sigma2 <- abs(sigma2)
  return(sum(0.5*log(sigma2) + ((y-mu)^2)/(2*sigma2)))
}

optim(par=c(0,1), fn=llik, method= 'BFGS', y=y)

# Autoregressive model
# Joint likelihood -> factorisation

y_ar <- arima.sim(n=100, list(ar=0.8), sd=1)

llik_rec <- function(args, y) {
  phi <- args[1]
  sigma2 <- args[2]
  sigma2 <- abs(sigma2)
  
  n <- length(y)
  
  llik <- (n/2)*log(sigma2)
  
  for (t in 2:n) {
    llik <- llik + ((y[t] - phi*y[t-1])^2) / (2*sigma2)
  }
  
  return(llik)
}

optim(par=c(0.5,0.6), fn=llik_rec, y=y_ar)
