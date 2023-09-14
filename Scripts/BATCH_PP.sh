#!/usr/bin/bash
#SBATCH --job-name=DYNAFFORE_PP
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=kldaum@ucsb.edu
#SBATCH --ntasks=15
#SBATCH --time=24:00:00
#SBATCH --output=BATCH-%j.log

pwd; hostname; date
R -f /home/vdl/DYNAFFOR/Scripts_striker/BATCH_PostProc_KD.R
date