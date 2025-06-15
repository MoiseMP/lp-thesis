#!/bin/bash
#SBATCH -n 16
#SBATCH -t 0:10:00
#SBATCH --job-name=parallel_r_test
#SBATCH --output=parallel_r_test.out

# Load R module (adjust as needed)
module load 2022
module load R/4.2.1-foss-2022a

# Copy R script to scratch (optional, for bigger jobs)
cp $HOME/my_parallel_test.R "$TMPDIR"

# Go to scratch directory
cd "$TMPDIR"

# Run the R script
Rscript my_parallel_test.R

# Copy output back home
cp result.txt $HOME/
