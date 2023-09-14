#!/usr/bin/bash
#SBATCH --job-name=BATCHx

#SBATCH --mail-type=begin        # send email when job begins
#SBATCH --mail-type=end          # send email when job ends
#SBATCH --mail-type=fail         # send email if job fails
#SBATCH --mail-user=kldaum@ucsb.edu

#SBATCH --ntasks=15
#SBATCH --time=200:00:00
#SBATCH --output=BATCHx-%j.log

pwd; hostname; date
R -f /home/vdl/DYNAFFOR/Scripts_striker/BATCH_submitx_KD.R
date