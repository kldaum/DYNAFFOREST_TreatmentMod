
#### FOR CLUSTER ONLY
#### This script allows the user to submit multiple experiments to the cluster from one
#### batch submission schedule. This script will separate by "run handle" and separately
#### write/submit Slurm scripts. If running on local, skip this script and run from P1.

## Cluster paths
path <- "/home/kldaum/vdl/scratch-vdl/kldaum1/DYNAFFOR/" # ending in /
path_outputs <- "/home/kldaum/vdl/scratch-vdl/kldaum1/DYNAFFOR/sierra/outputs/" # ending in /
path_params <-  "/home/kldaum/vdl/scratch-vdl/kldaum1/DYNAFFOR/Parameters_striker/" # ending in /
path_scripts <- "/home/kldaum/vdl/DYNAFFOR/Scripts_striker/" # ending in /

# Run initialization module
module_name <- "Init"
source(paste0(path_scripts, "Modules_KD.R"))

# Read experiment parameters
params_batch <- read.csv(paste0(path_params, "Params_BATCH.csv"))
script.name <- "BATCH_Model_P1_KD.R"
submitted <- NULL
script.name <- "BATCH_P1_KD.R"

# Submit experiments (defined by "handle" separately)
for (i in c(1:length(unique(params_batch$Run_handle)))){
  
  RUNINFO1 <- paste0("'", unique(params_batch$Run_handle)[i], "'")
  
  # Wait while submitting...
  path_prev <- paste0(search.files(path_outputs, "BATCH", "last"), "/Outputs")
  while(length(search.files(path_prev, "Postprocess", "")) == 0){
    Sys.sleep(60*5)
  }
  
  Sys.sleep(5)
  
  #SBATCH --job-name=", substr(batch_time, 3, nchar(batch_time)-2), "\n
  
  SCRIPT <- paste0(
"#!/usr/bin/bash\n
#SBATCH --job-name=P1_", sprintf("%03d", i), "\n
#SBATCH --mail-type=END,FAIL\n
#SBATCH --mail-user=kldaum@ucsb.edu\n
#SBATCH --ntasks=1\n
#SBATCH --time=100:00:00\n
#SBATCH --output=", path_scripts, "SLURM_RUN_P1_", sprintf("%03d", i), ".log\n\n
pwd; hostname; date\n
R -f ", path_scripts, script.name,"\n
date\n
"
)
  
  # Write and submit slurm script
  if ((r %in% submitted) == F){
    
    fileConn<-file(paste0(path_scripts, "BATCH_P1_", sprintf("%03d", i),".sh"))
    writeLines(SCRIPT, fileConn)
    close(fileConn)
    
    system(command = paste0("sbatch ", "--export=RUNINFO1=", RUNINFO1, " ",path_scripts, "BATCH_P1_", sprintf("%03d", i),".sh"))
    file.remove(paste0(path_scripts, "BATCH_P1_", sprintf("%03d", i),".sh"))
    
    submitted <- c(submitted, r)
    
  }
}
