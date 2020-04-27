#!/bin/bash
#SBATCH --job-name=to_cluster
#SBATCH --partition=fuchs
#SBATCH --nodes=2
#SBATCH --ntasks=40
#SBATCH --mem-per-cpu=2500
#SBATCH --time=240:00:30
#SBATCH --mail-type=FAIL
#SBATCH --extra-node-info=2:10:1
Rscript to_cluster.R
