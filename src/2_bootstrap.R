#!/usr/bin/env Rscript
source('src/1_bootstrap.R')
#set.seed

packages_to_install <- c("purrr", "dplyr", "furrr", "future")

for (pkg in packages_to_install) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

args = commandArgs(trailingOnly=TRUE)

# test if there is at least one argument: if not, return an error
if (length(args)==0) {
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
}

B <- 10
M <- 10
p <- 2 # AR order
n <- 100
# TRUE GDP
phi <- c(phi1 = 0.5, phi_2 = 0.1)
horizons <- c(1,6,12,18)
q <- 2

method <- args[1]
type <- args[2]
M <- args[3]
B <- args[4]




#n_cores <- parallelly::availableCores() - 1
plan(multisession, workers = 50)

true_irf <- true_irf_ar_2(phi,horizons = horizons)
true_irf <- phi^horizons

run_instance_ar_bootstrap <- function(y, q, horizons, B, type = c('iid','wild'), alpha = 0.1) {
  
  type <- match.arg(type)
  
  # Estimate LP
  lp_fit <- map_df(horizons, function (x) lm_lp(y, x, q))
  
  # Fit AR(p) on data
  ar_model <- ar(y, aic = FALSE, order.max = q, demean = FALSE)
  u_hat <- na.omit(ar_model$res)
  u_tilde <- u_hat - mean(u_hat)
  phi_hat <- ar_model$ar
  
  # Initialize results
  results_array <- array(0, 
                         dim = c(length(horizons), 5, B),
                         dimnames = list(
                           horizon   = as.character(horizons),
                           metric    = c("coef","se_HC","se_NW","se_cosine","t_stat"),
                           bootstrap = 1:B))
  
  
  # Bootstrap Loop
  for (b in 1:B) {
    
    if (type == 'wild') {
      u_b <- u_tilde * rnorm(n-q)
    } else {
      u_b <- sample(u_tilde, size = n - q, replace = TRUE)
    }
    
    # Simulate AR(q) process
    y_b <- numeric(n)
    y_b[1:q] <- 0
    
    for (t in (q+1):n) {
      y_b[t] <- sum(phi_hat * rev(y_b[(t - q):(t - 1)])) + u_b[t - q]
    }
    
    #horizons
    results <- t(sapply(horizons, function(x) lm_lp(y_b, x, q)))
    results[ , 't_stat'] <- (results[ , 'coef'] - lp_fit$coef ) / results[ , 'se_NW']
    
    results_array[ , , b] <- results
  }
  
  result <- process_results_array(results_array, lp_fit, alpha)
  
  result
}

run_instance_block_bootstrap <- function(y, q, horizons, B, 
                                         type = c('mbb','cbb','stat'),
                                         l = 3,
                                         alpha = 0.1) {
  type <- match.arg(type)
  
  if (type == 'mbb') {
    sim_type <- 'fixed';  endcorr <- FALSE    # moving blocks,
  } else if (type == 'cbb') {
    sim_type <- 'fixed';  endcorr <- TRUE     # circular blocks
  } else { # 'stat'
    sim_type <- 'geom';   endcorr <- TRUE     # stationary bootstrap
  }
  
  # draw bootstrap data
  tsb <- tsboot(
    tseries   = y,
    statistic = function(x) 0, 
    R         = B,
    sim       = sim_type,
    l         = l,
    endcorr   = endcorr
  )

  bootstrap_samples <- boot.array(tsb)
  
  # Initialize results
  results_array <- array(0, 
                         dim = c(length(horizons), 5, B),
                         dimnames = list(
                           horizon   = as.character(horizons),
                           metric    = c("coef","se_HC","se_NW","se_cosine","t_stat"),
                           bootstrap = 1:B))
  
  lp_fit <- map_df(horizons, function (x) lm_lp(y, x, q))
  
  for (b in 1:B) {
    y_b <- y[bootstrap_samples[b,]]
    results <- t(sapply(horizons, function(x) lm_lp(y_b, x, q)))
    results[ , 't_stat'] <- (results[ , 'coef'] - lp_fit$coef ) / results[ , 'se_NW']
    results_array[ , , b] <- results
  }
  
  process_results_array(results_array, lp_fit, alpha)
}

run_instance_sieve_bootstrap <- function(y, q, horizons, B, alpha = 0.1) {
  
  lp_fit <- map_df(horizons, function(x) lm_lp(y, x, q))
  
  results_array <- array(0,
                         dim = c(length(horizons), 5, B),
                         dimnames = list(
                           horizon   = as.character(horizons),
                           metric    = c("coef", "se_HC", "se_NW", "se_cosine", "t_stat"),
                           bootstrap = 1:B))
  
  for (i in seq_along(horizons)) {
    
    h <- horizons[i]
    
    lp_fit_h <- lm_lp(y, h, q, return_fit = TRUE)
    u_hat <- lp_fit_h$residuals
    lp_fitted_h <- lp_fit_h$fitted_values
    
    # Estimate ar(p) on residuals with aic
    ar_fit <- ar(u_hat, aic = TRUE, demean = FALSE)
    phi_hat_residuals <- ar_fit$ar
    epsilon_hat <- na.omit(ar_fit$res)
    p <- ar_fit$order
    
    for (b in 1:B) {
      epsilon_star_b <- sample(epsilon_hat, size = length(u_hat), replace = TRUE)
      
      u_star_b <- numeric(length(u_hat))
      u_star_b[1:p] <- u_hat[1:p]
      
      for (t in (p + 1):length(u_hat)) {
          u_star_b[t] <- sum(phi_hat_residuals * u_star_b[(t - p):(t - 1)]) + epsilon_star_b[t]
        }

      y_star_b_dep <- lp_fitted_h + u_star_b
      
      D <- embed(y, h + q)
      n <- nrow(D)
      y_h <- D[, 1]
      X <- D[, (h+1):(h+q), drop = FALSE] 
      
      lp_fit_b <- lm_lp(y_star_b_dep, h, q, return_fit = FALSE, X)
      
      results_b <- lp_fit_b
      results_b[5] <- (results_b[1] - lp_fit$coef[i]) / results_b[3]
      
      results_array[i, , b] <- results_b
    }
  }
  
  process_results_array(results_array, lp_fit, alpha)
}

process_results_array <- function(results_array, lp_fit, alpha) {
  # Compute summary statistics
  # All columns except t_stat
  means <- apply(results_array[ , c(1:4), ], MARGIN = c(1,2),FUN = mean)
  
  # Quantiles
  percentile_interval <- apply(results_array[ , 'coef',], MARGIN = 1, FUN = function(x) quantile(abs(x), probs = c(alpha / 2, 1 - alpha/2)))
  lower_pct <- percentile_interval[1,]
  upper_pct <- percentile_interval[2,]
  
  # T-stats
  t_interval <- apply(results_array[ , 't_stat',], MARGIN = 1, FUN = function(x) quantile(abs(x), probs = c(1 - alpha)))
  equal_tail <- apply(results_array[ , 't_stat', ], MARGIN = 1, FUN = function(x) quantile(x, probs = c(alpha/2, 1 - alpha/2)))
  
  # Normal
  z_val <- qnorm(1 - alpha / 2)
  
  # Return
  tibble(
    horizon = horizons,
    true_irf = true_irf,
    theta_hat = lp_fit$coef,
    mean_coef = means[ , 'coef'],
    mean_se_HC = means[ , 'se_HC'],
    mean_se_NW = means[ , 'se_NW'],
    lower_sym = lp_fit$coef - t_interval * lp_fit$se_NW,
    upper_sym = lp_fit$coef + t_interval * lp_fit$se_NW,
    lower_eq = lp_fit$coef + equal_tail[1, ] * lp_fit$se_NW,
    upper_eq = lp_fit$coef + equal_tail[2, ] * lp_fit$se_NW,
    lower_pct = lower_pct,
    upper_pct = upper_pct,
    lower_norm = lp_fit$coef - z_val * lp_fit$se_NW,
    upper_norm = lp_fit$coef + z_val * lp_fit$se_NW
  )
}

run_mc <- function(M, phi, n, q, horizons, B, alpha = 0.1, method = c('ar', 'block', 'sieve'), type = NULL) {
    mc_results <- future_map_dfr(seq_len(M), function(x) {
      # Simulate DGP
      y <- sim_ar(phi, n = n)
      
      method <- match.arg(method, choices = c('ar', 'block', 'sieve'))
      
      
      if (method == 'ar') {
        ci_tbl <- run_instance_ar_bootstrap(y, q, horizons, B, type = type, alpha = 0.1)
      } else if (method == 'block') {
        ci_tbl <- run_instance_block_bootstrap(y, q, horizons, B, type = type,
                                            l = 3, alpha = 0.1)
      } else if (method == 'sieve') {
        ci_tbl <- run_instance_sieve_bootstrap(y, q, horizons, B, alpha = 0.1)
      }
      ci_tbl$iter <- x
      ci_tbl
    },
    .progress = TRUE,
    .options = furrr_options(seed = TRUE)
    )
    
    coverage <- mc_results |>
    mutate(
      cover_eq = (true_irf >= lower_eq) & (true_irf <= upper_eq),
      cover_sym = (true_irf >= lower_sym) & (true_irf <= upper_sym),
      cover_pct = (true_irf >= lower_pct) & (true_irf <= upper_pct),
      cover_norm = (true_irf >= lower_norm) & (true_irf <= upper_norm)
    ) |>
    group_by(horizon) |>
      summarise(
        cov_eq = mean(cover_eq),
        cov_sym = mean(cover_sym),
        cov_pct = mean(cover_pct),
        cov_norm = mean(cover_norm),
        avg_w_eq = mean(upper_eq - lower_eq),
        avg_w_sym = mean(upper_sym - lower_sym),
        avg_w_pct = mean(upper_pct - lower_pct),
        avg_w_norm = mean(upper_norm - lower_norm),
        med_w_eq = median(upper_eq - lower_eq),
        med_w_sym = median(upper_sym - lower_sym),
        med_w_pct = median(upper_pct - lower_pct),
        med_w_norm = median(upper_norm - lower_norm)
    )
    
    list(mc_table = mc_results, coverage = coverage)
}

# write to file

#run_mc(100, phi, n, q, horizons, 5, alpha = 0.01, method = 'ar', type = 'iid')
#run_mc(100, phi, n, q, horizons, 5, alpha = 0.01, method = 'block', type = 'mbb')
#run_mc(100, phi, n, q, horizons, 5, alpha = 0.01, method = 'sieve')

results <- run_mc(100, phi, n, q, horizons, 5, alpha = 0.01, method = 'ar', type = 'iid')
result_table <- results[[1]]
result_coverage <- results[[2]]

write.csv(result_table, paste0(paste0('data/result_table', method, type, sep = '_'),'.csv'), row.names = FALSE)
write.csv(result_coverage, paste0(paste0('data/result_coverage', method, type, sep = '_'),'.csv'), row.names = FALSE)
