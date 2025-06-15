#!/bin/bash
#SBATCH --job-name=renv_job
#SBATCH --output=renv_job.%j.out
#SBATCH --error=renv_job.%j.err
#SBATCH --time=01:00:00
#SBATCH --cpus-per-task=48
#SBATCH --mem=64G

module load 2024
module load R/4.4.2-gfbf-2024a

# Restore project library
R -e "renv::restore()"

# Run your analysis
Rscript scripts/my_analysis.R