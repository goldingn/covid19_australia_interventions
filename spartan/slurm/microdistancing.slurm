#!/bin/bash

# Never edit
#SBATCH --account="punim1422"
#SBATCH --mail-user=ryange@unimelb.edu.au
#SBATCH --mail-type=ALL

# Sometimes edit
#SBATCH --partition="physical,snowy"
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=10

# megabytes per process


# Always edit
#SBATCH --job-name="job_microdistancing"
#SBATCH --output="/data/gpfs/projects/punim1422/covid/covid19_australia_interventions/spartan/slurm/slurm_output/job_microdistancing.out"

# days-hours:mins:sec
#SBATCH --time=0-3:00:00

# Location to run job from
cd /data/gpfs/projects/punim1422/covid/covid19_australia_interventions

# Modules
module load gcccore/8.3.0
module load gcc/8.3.0
module load openmpi/3.1.4
module load udunits/2.2.26
module load geos/3.7.2-python-3.7.4
module load openssl/1.1.1f
module load netcdf/4.7.1
module load gdal/3.0.2-python-3.7.4
module load rgdal/1.4-4-r-3.6.0
module load python/3.7.4
module load tensorflow/1.15.2-python-3.7.4
module load r/3.6.0

# Commands
Rscript --vanilla R/microdistancing_change.R
