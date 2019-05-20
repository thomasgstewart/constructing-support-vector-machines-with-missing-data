#!/bin/bash
#SBATCH --mail-type=FAIL
#SBATCH --ntasks=1
#SBATCH --time=04:00:00
#SBATCH --mem=4G
#SBATCH --array=0-999
#SBATCH --output=./out/out_%a.out

Rscript 02-sim.R $SLURM_ARRAY_TASK_ID
