#!/bin/bash
#SBATCH --job-name=renv_job
#SBATCH --output=logs_job.%j.out
#SBATCH --error=logs_job.%j.err
#SBATCH --time=01:00:00
#SBATCH --cpus-per-task=64
#SBATCH --mem=12G

#SBATCH --mail-user=m.n.mpongo@student.vu.nl
#SBATCH --mail-type=END,FAIL

module load 2024
module load R/4.4.2-gfbf-2024a

# Restore project library
R -e "renv::restore()"

# Run your analysis
Rscript scripts/my_analysis.R ar iid 10 10