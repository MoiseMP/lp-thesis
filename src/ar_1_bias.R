# Bias AR(1) models

N <- c(100,200,500,1000)
M <- 1000
phi <- 0.9

results <- data.frame(
  'N'= N,
  'mean' = NA,
  'std' = NA,
  'median' = NA
)

sim_ar_1 <- function(phi, sigma, n) {
  y <- rep(0,n)
  y[1] <- rnorm(1,0,sigma)
  for (t in 2:n) {
    y[t] <- phi * y[t-1] + rnorm(1)
  }
  return(y)
}

estimate_ar_1 <- function(X,y) {
  phi_hat <- solve(t(X)%*%X)%*%t(X)%*%y
  return(phi_hat)
}

for (i in seq_along(N)) {
  n <- N[i]
  phi_hat_mat <- rep(0,M)
  
  for (j in 1:M) {
    y <- sim_ar_1(phi,sigma,n)
    phi_hat_mat[j] <- estimate_ar_1(y[1:length(y)-1],y[2:length(y)])
  }
  results$mean[i] <- mean(phi_hat_mat)
  results$median[i] <- median(phi_hat_mat)
  results$std[i] <- sd(phi_hat_mat)
}


plot(N, results$mean, ylim = c(0.8,1))
abline(a=phi, b=0)
arrows(N, results$mean - results$std, N, results$mean + results$std, angle=90, code=3, length=0.05)

