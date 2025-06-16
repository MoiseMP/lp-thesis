#!/bin/bash
#SBATCH --job-name=renv_job
#SBATCH --output=logs/job.%j.out
#SBATCH --error=logs/job.%j.err
#SBATCH --time=01:00:00
#SBATCH --cpus-per-task=8
#SBATCH --mem=16G

#SBATCH --mail-user=m.n.mpongo@student.vu.nl
#SBATCH --mail-type=END,FAIL

module load 2024
module load R/4.4.2-gfbf-2024a

# Restore project library
R -e "renv::restore()"

# Run your analysis
Rscript src/2_bootstrap.R ar iid 10 10  