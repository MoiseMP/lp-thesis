source('R/1_bootstrap.R')

rho1_vals <- c(0.2, 0.5, 0.9)
rho2_vals <- c(0.8, 0.5, 0.2, -0.5)
n <- 95

png("figures/ar2_grid.png", width = 9, height = 7, units = "in", res = 300)
# For plotting
par(mfrow = c(length(rho1_vals), length(rho2_vals)), mar = c(2,2,2,1))

set.seed(421) # for reproducibility

for (rho1 in rho1_vals) {
  for (rho2 in rho2_vals) {
    
    y <- sim_ar2_rho(c(rho1, rho2), n = n)
    plot(y, type = "l",
         main = bquote(rho[1] == .(rho1) ~ "," ~ rho[2] == .(rho2)),
         xlab = "", ylab = "", ylim = c(-20,20))
    for (i in 1:100) {
      lines(sim_ar2_rho(c(rho1, rho2), n = n))
    }
  }
}

dev.off()