#!/bin/bash

#SBATCH --job-name=mefContinued
##SBATCH --nodelist=stat1
#SBATCH --partition=queue3
#SBATCH --ntasks=1
#SBATCH --mem-per-cpu=5G
#SBATCH --cpus-per-task=30
#SBATCH --mail-type=end
#SBATCH --mail-user=antonio.calcagni@unipd.it

R CMD BATCH casestudy_parallel_cv_all.R
