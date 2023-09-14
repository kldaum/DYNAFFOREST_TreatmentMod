#!/bin/sh
#test_r.sh
#Slurm script to run a test r program
#Slurm directives
#


#SBATCH --partition=p_deenr_1          # Partition (job queue)

#SBATCH --requeue                 # Return job to the queue if preempted

#SBATCH --job-name=sierra_run       # Assign a short name to your job

#SBATCH --nodes=1                 # Number of nodes you require

#SBATCH --ntasks=1                # Total # of tasks across all nodes

#SBATCH --cpus-per-task=1         # Cores per task (>1 if multithread tasks)

#SBATCH --mem=64000                # Real memory (RAM) required (MB)

#SBATCH --time=48:00:00           # Total run time limit (HH:MM:SS)

#SBATCH --output=slurm.%N.%j.out  # STDOUT output file

#SBATCH --error=slurm.%N.%j.err   # STDERR output file (optional)

module load singularity/3.1.0
#Define the arguments that are fed to the model runs down below
region=sierra
nyears=337
fsmult1=1
fsmult2=1.1
fsmult3=1.2
fnmult1=1
fnmult2=5
fnmult3=10

#Delete the last output sqlite so you do not append ot the old one.
cd /scratch/wh350/western.fire/$region/outputs/
find . -type f -name output\* -exec rm {} \;
cd /home/wh350/western.fire/

#Execute the model runs feeding the arguments defined above.

#######

			#Singularity image file			#Load Rscript		#Load model itself		     #Feed/import above-defined objects into R
singularity exec /home/wh350/software/geospatial_latest.sif /usr/local/bin/Rscript forest-fire.model/development/model_3_25_2021.R $region $nyears $fsmult1 $fnmult1

		#Move/rename output file from...				#Move/rename output file to...	
mv /scratch/wh350/western.fire/$region/outputs/output.sqlite /scratch/wh350/western.fire/$region/outputs/output.size-$fsmult1.num-$fnmult1.sqlite

#######

#singularity exec /home/wh350/software/geospatial_latest.sif /usr/local/bin/Rscript forest-fire.model/development/model_3_25_2021.R $region $nyears $fsmult2 $fnmult1
#mv /scratch/wh350/western.fire/$region/outputs/output.sqlite /scratch/wh350/western.fire/$region/outputs/output.size-$fsmult2.num-$fnmult1.sqlite

#singularity exec /home/wh350/software/geospatial_latest.sif /usr/local/bin/Rscript forest-fire.model/development/model_3_25_2021.R $region $nyears $fsmult3 $fnmult1
#mv /scratch/wh350/western.fire/$region/outputs/output.sqlite /scratch/wh350/western.fire/$region/outputs/output.size-$fsmult3.num-$fnmult1.sqlite

#singularity exec /home/wh350/software/geospatial_latest.sif /usr/local/bin/Rscript forest-fire.model/development/model_3_25_2021.R $region $nyears $fsmult1 $fnmult2
#mv /scratch/wh350/western.fire/$region/outputs/output.sqlite /scratch/wh350/western.fire/$region/outputs/output.size-$fsmult1.num-$fnmult2.sqlite

#singularity exec /home/wh350/software/geospatial_latest.sif /usr/local/bin/Rscript forest-fire.model/development/model_3_25_2021.R $region $nyears $fsmult2 $fnmult2
#mv /scratch/wh350/western.fire/$region/outputs/output.sqlite /scratch/wh350/western.fire/$region/outputs/output.size-$fsmult2.num-$fnmult2.sqlite

#singularity exec /home/wh350/software/geospatial_latest.sif /usr/local/bin/Rscript forest-fire.model/development/model_3_25_2021.R $region $nyears $fsmult3 $fnmult2
#mv /scratch/wh350/western.fire/$region/outputs/output.sqlite /scratch/wh350/western.fire/$region/outputs/output.size-$fsmult3.num-$fnmult2.sqlite

#singularity exec /home/wh350/software/geospatial_latest.sif /usr/local/bin/Rscript forest-fire.model/development/model_3_25_2021.R $region $nyears $fsmult1 $fnmult3
#mv /scratch/wh350/western.fire/$region/outputs/output.sqlite /scratch/wh350/western.fire/$region/outputs/output.size-$fsmult1.num-$fnmult3.sqlite

#singularity exec /home/wh350/software/geospatial_latest.sif /usr/local/bin/Rscript forest-fire.model/development/model_3_25_2021.R $region $nyears $fsmult2 $fnmult3
#mv /scratch/wh350/western.fire/$region/outputs/output.sqlite /scratch/wh350/western.fire/$region/outputs/output.size-$fsmult2.num-$fnmult3.sqlite

#singularity exec /home/wh350/software/geospatial_latest.sif /usr/local/bin/Rscript forest-fire.model/development/model_3_25_2021.R $region $nyears $fsmult3 $fnmult3
#mv /scratch/wh350/western.fire/$region/outputs/output.sqlite /scratch/wh350/western.fire/$region/outputs/output.size-$fsmult3.num-$fnmult3.sqlite

# End of script