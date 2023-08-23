#!/bin/bash
#SBATCH --time=04:00:00
#SBATCH --job-name="paper3"
#SBATCH --partition=short
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=13
#SBATCH --mem-per-cpu=8GB
#SBATCH --output=paper3.out

cd /PROJECTES/HELIX_OMICS/DATA_PREVIOUS_MIGRATION/lorenzoF/phd/papers/paper3_helixSC_neuro/paper-helixSC-neuro

module load lang/R/4.2.1-foss-2022a

Rscript run.R
