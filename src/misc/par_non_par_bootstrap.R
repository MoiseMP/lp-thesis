# Parametric Bootstrap in R
set.seed(123)
y <- rnorm(1000)
B <- 1000

# Goal recover median, and std_median
llik <- function(par, y) {
  mu <- par[1]
  sigma2 <- par[2]
  sigma2 <- abs(sigma2)
  l <- sum(-0.5*log(2*pi) - 0.5*log(sigma2) - ((y-mu)^2) / (2*sigma2) )
  return(-mean(l))
}

optimser <- optim(par=c(1,1), fn=llik, y=y)
bootstrap_medians <- numeric(length = 1000)
bootstrap_medians_non <- numeric(length = 1000)

bootstrap_samples <- c(100, 1000, 5000, 10000)
par_results <- matrix(nrow=2,ncol=4)
non_par_results <- matrix(nrow=2,ncol=4)

for (i in 1:4) {
  B <- bootstrap_samples[i]
  for (b in 1:B) {
    y.b <- rnorm(1000, optimser$par[1], sqrt(optimser$par[2]))
    bootstrap_medians[b] <- median(y.b)
  }
  for (b in 1:B) {
    y.b <- sample(y, 1000, replace = TRUE)
    bootstrap_medians_non[b] <- median(y.b)
  }
  par_results[1,i] <- mean(bootstrap_medians)
  par_results[2,i] <- sd(bootstrap_medians)
  non_par_results[1,i] <- mean(bootstrap_medians_non)
  non_par_results[2,i] <- sd(bootstrap_medians_non)
}

x <- 1:4
plot(x, par_results[1,], col = 'blue', pch=4, ylim=c(-0.1,0.1), xaxt='n',
     main='Comparison parametric and non-parametric bootstrap',
     xlab='Bootstrap samples',
     ylab='Bootstrap median (mean +- std)')

axis(side=1, at=x, labels=bootstrap_samples)
arrows(x, par_results[1,] - par_results[2,], x, par_results[1,] + par_results[2,], angle = 90, code = 3, length = 0.1, col='blue')
points(x + 0.01, non_par_results[1,], col = 'red', pch=4)
arrows(x + 0.01, non_par_results[1,] - non_par_results[2,], x + 0.01, non_par_results[1,] + non_par_results[2,], angle = 90, code = 3, length = 0.1, col='red')
abline(h=0, lty=2)


