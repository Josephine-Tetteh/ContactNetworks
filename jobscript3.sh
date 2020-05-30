#!/bin/bash
#SBATCH --job-name=vmBC
#SBATCH --partition=fuchs
#SBATCH --nodes=3
#SBATCH --ntasks=40
#SBATCH --mem-per-cpu=2500
#SBATCH --time=240:00:30
#SBATCH --mail-type=FAIL
#SBATCH --extra-node-info=2:10:1
Rscript vmodelBC.R
