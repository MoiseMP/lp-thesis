packages_to_install <- c("boot", "sandwich", "np")

for (pkg in packages_to_install) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

sim_ar <- function(phi, sd = 1, n = 95) {
  #phi: list of ar parameters
  
  y <- numeric(n)
  p <- length(phi)
  y[1:p] <- 0
  
  for (t in (p+1):n) {
    y[t] <- sum(phi*rev(y[(t-p):(t-1)])) + rnorm(1)
  }
  
  y
} 

sim_ar2_rho <- function(rho, sd = 1, n = 95) {
  # rho: vector of 2 roots
  
  if (length(rho) != 2) {
    stop("rho should be of length 2")
  }
  
  # Convert roots to AR(2) coefficients
  phi1 <- rho[1] + rho[2]
  phi2 <- -rho[1] * rho[2]
  phi <- c(phi1, phi2)
  
  # Initialize series
  y <- numeric(n)
  y[1:2] <- 0  # can also use rnorm(2) if you want random starts
  
  for (t in 3:n) {
    y[t] <- phi[1] * y[t - 1] + phi[2] * y[t - 2] + rnorm(1, sd = sd)
  }
  
  return(y)
}

lm_lp <- function(y, h, q, return_fit = FALSE, X = FALSE) {
  # Estimate LP
  #y: univariate ts
  #h: forecast horizon
  #q: n endogenous lags
  #X: optional, => add custom exporatory variables
  
  # Build data matrix
  D <- embed(y, h + q)
  n <- nrow(D)
  y_h <- D[, 1]
  
  
  if (is.logical(X)) {
    X <- D[, (h+1):(h+q), drop = FALSE]
  } else {
    y_h <- y
  }

  # Fit Model
  lp_model <- lm(y_h ~ 0 + X)

  # Return
  coef <- as.numeric(lp_model$coefficients[1])
  se_HC <- sqrt(vcovHC(lp_model, type = "HC3")[1, 1]) # with dof adjusment
  se_cosine <- sqrt(vcovHC(lp_model)[1, 1]) # cosine weightsAndrews
  se_NW <- sqrt(NeweyWest(lp_model)[1, 1])
  
  if (return_fit) {
    list(coef = coef,
      se_NW = se_NW,
      se_HC = se_HC,
      se_cosine = se_cosine,
      t_stat = NA,
      residuals = resid(lp_model),
      fitted_values = fitted(lp_model)
      )
  } else {
    c(coef = coef,
      se_NW = se_NW,
      se_HC = se_HC,
      se_cosine = se_cosine,
      t_stat = NA)
  }
}

lp_fgls <- function(y, h, q) {
  # Estimate LP using FGLS lusompa (2023)
  #y: univariate ts
  #h: forecast horizon
  #q: n endogenous lags

  beta_fgls <- numeric(h)
  se_fgls <- numeric(h)
  
  for (k in 1:h){
    
    # Fit Model
    D   <- embed(y, k + q)
    y_k <- D[, 1]
    X   <- D[, 2:(q+1), drop = FALSE]
    
    if (k > 1) {
      y_k <- y_k - beta_fgls[k-1] * X[, 1]
    }
    
    lp_model <- lm(y_k ~ 0 + X)
    beta_fgls[k] <- lp_model$coef[1]
    se_fgls[k] <- sqrt(diag(vcovHC(lp_model, type = "HC3")))[1]
                       
  }
  list(beta_fgls = beta_fgls,
    se_fgls = se_fgls)
}

true_irf_ar_2 <- function(phi, horizons) {
  # phi = c(phi1, phi2)
  max_h <- max(horizons)
  psi   <- numeric(max_h+1)
  psi[1] <- 1
  psi[2] <- phi[1]
  for(h in 3:(max_h+1)) {
    psi[h] <- phi[1]*psi[h-1] + phi[2]*psi[h-2]
  }
  psi[horizons+1]
}

