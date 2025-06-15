library(parallel)

# Detect the number of available cores
n_cores <- as.integer(Sys.getenv("SLURM_NTASKS", unset=detectCores()))

# Example function: simple computation
myfun <- function(x) {
  Sys.sleep(1)  # Simulate some work
  return(x^2)
}

# Run in parallel
result <- mclapply(1:16, myfun, mc.cores = n_cores)

# Write output to file
writeLines(as.character(unlist(result)), con = "result.txt")
