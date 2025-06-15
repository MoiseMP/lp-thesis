# BIAS AR(q) -> AR(p) q<p
# Bias in the impule response function
library(lmtest)
library(lpirfs) # FOR LP's
library(dplyr)
library("sandwich")
library(vars)

N <- 1000
H <- 12
phi <- c(0.3, 0.2, -0.1, 0.4)
p <- length(phi)
critical_value <- 1.96
horizons <- seq(0, H)

# TRUE DGP AR(4)
y <- arima.sim(n = N, list(ar=phi), sd = 1)

# TRUE IRF
true_irf <- ARMAtoMA(ar=phi, lag.max = H+1)

##AR BASED IRF

B <- 1000

list_irf_ar <- vector('list', 4)

for (i in 1:4) {
  df_irf_ar <- data.frame(irf=rep(NA,H+1),lower_bound=rep(NA,H+1),upper_bound=rep(NA,H+1))
  fit_arima <- arima(y, order = c(i,0,0), include.mean = FALSE, method='CSS')
  irf_fit <- ARMAtoMA(fit_arima$coef, lag.max=H+1)
  df_irf_ar$irf <- irf_fit
  residuals <- fit_arima$residuals
  irf_bootstrap_samples <- matrix(NA,nrow=B,ncol=H+1)
  for (b in 1:B) {
    bootstrap_residuals <- sample(residuals, N, replace = TRUE)
    y_sim <- arima.sim(n = N, list(ar = fit_arima$coef), innov=bootstrap_residuals)
    fit_sim <- arima(y_sim, order = c(i,0,0), include.mean = FALSE)
    irf_sim <- ARMAtoMA(ar = fit_sim$coef, lag.max = H+1)
    irf_bootstrap_samples[b,] <- irf_sim
  }
  a_quantiles <- apply(irf_bootstrap_samples, 2, quantile, probs=c(0.025,0.975))
  df_irf_ar$lower_bound <- a_quantiles[1,]
  df_irf_ar$upper_bound <- a_quantiles[2,]
  list_irf_ar[[i]] <- df_irf_ar
}


## Local Projection Based IRF

lp_irf <- function(y,q,H) {
  # Estimate LP IRF
  df_irf_lp <- data.frame(irf = rep(NA, H+1),
                       se_irf = rep(NA, H+1))
  
  df_irf_lp$irf[1] <- 1
  df_irf_lp$se_irf[1] <- 0
  
  for (h in 1:H) {
    lp_model <- lp_fit(y,h,q)
    df_irf_lp$irf[h+1] <- as.numeric(lp_model$coefficients[1])
    df_irf_lp$se_irf[h+1] <- sqrt(diag(NeweyWest(lp_model)))[1]  # Newey west HAC robust SE
  }

    return(df_irf_lp)
}

lp_fit <- function(y, h, q) {
  # Estimate LP
  #y: univariate ts
  #h: forecast horizon
  #q: n endogenous lags
  
  N <- length(y)
  # Construct the outcome vector
  Y <- y[(h+q):N]
  # Construct the data matrix with lagged values
  X <- matrix(data=NA, nrow=(N-q-h+1), ncol=q)

  for (i in 1:q) {
    X[,i] <- y[(q-(i-1)):(N-h-(i-1))]
  }
  # Fit Model
  lp_model <- lm(Y ~ X - 1)
  
  return(lp_model)
}

list_lp_irf <- vector('list', 4)

for (q in 1:4) {
  list_lp_irf[[q]] <- lp_irf(y,q,H)
}



# Plotting
par(mfrow=c(2,2), oma = c(4, 4, 2, 1))

for (i in 1:4) {
  
  # Plot of IRFs
  plot(horizons, true_irf, col = 'black', type = 'b', lwd = 2,
       ylim = c(-0.5, 1), xlab = "", ylab = "")
  
  if (i == 2) {
    # Add legend
    legend("topright",
           legend = c("True IRF", "LP IRFs", "AR IRF"),
           col = c("black", "red", "blue"),
           lty = c(1, 2, 3), pch = c(1, 1, 1), lwd = c(2, 1, 1),
           bty = "n")
  }
  
  #LP IRF
  irf_lp <- list_lp_irf[[i]]$irf
  se_lp  <- list_lp_irf[[i]]$se_irf
  upper_lp <- irf_lp + se_lp * 1.96
  lower_lp <- irf_lp - se_lp * 1.96
  
  # Fill CI
  polygon(c(horizons, rev(horizons)),
          c(upper_lp, rev(lower_lp)),
     
               col = rgb(1,0,0,0.15), border = NA)
  # Main line
  lines(horizons, irf_lp, col = 'red', lwd = 2)
  
  # AR IRFs
  irf_ar <- list_irf_ar[[i]]$irf
  upper_ar <- list_irf_ar[[i]]$upper_bound
  lower_ar <- list_irf_ar[[i]]$lower_bound
  
  # Fill CI
  polygon(c(horizons, rev(horizons)),
          c(upper_ar, rev(lower_ar)),
          col = rgb(0,0,1,0.15), border = NA)
  # Main line
  lines(horizons, irf_ar, col = 'blue', lwd = 2)
  
  
  lines(horizons, true_irf, col = 'black', lwd = 2)
}












