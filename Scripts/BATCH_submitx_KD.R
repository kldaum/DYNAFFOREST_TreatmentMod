library(readr)

if (exists("BATCHx") == F){
  
  # Batch get status
  BATCH_status <- function(){
    system(command = paste0("squeue -u $LOGNAME > status.txt"))
    Sys.sleep(3)
    return(data.frame(read_table(paste0(path_scripts, "status.txt"))))
  }
  
  # Batch submit generic
  BATCH_submitx <- function(inputs, input.name, job.name, script.name, path_logs){
    submitted <- NULL
    for (i in c(1:length(inputs))){
      
      input.temp <- inputs[i]
      RUNINFOx <- paste0("'", input.name, "___", input.temp, "'")
      
      if (i > 1){
        Sys.sleep(10)
        while(nrow(BATCH_status()) > 15){
          Sys.sleep(60*5)
        }
      }
      
      Sys.sleep(5)
      
      SCRIPT <- paste0(
        "#!/usr/bin/bash\n
  #SBATCH --job-name=", job.name, "\n
  #SBATCH --mail-type=END,FAIL\n
  #SBATCH --mail-user=kldaum@ucsb.edu\n
  #SBATCH --ntasks=1\n
  #SBATCH --time=100:00:00\n
  #SBATCH --output=", path_logs, "BATCHx_", job.name, "_", sprintf("%03d", i), ".log\n\n
  pwd; hostname; date\n
  R -f ", path_scripts, script.name,"\n
  date\n
  "
      )
      
      if ((i %in% submitted) == F){
        
        path.temp <- paste0(path_scripts, "BATCHx_", sprintf("%03d", i),".sh")
        
        fileConn<-file(path.temp)
        writeLines(SCRIPT, fileConn)
        close(fileConn)
        
        system(command = paste0("sbatch ", "--export=RUNINFOx=", RUNINFOx, " ",path.temp))
        file.remove(path.temp)
        
        submitted <- c(submitted, i)
        
      }
    } 
  }
  
  platform <- "cluster"
  inputs_batch <- c("BATCH_2209010020")
  path_outputs <- "/home/kldaum/vdl/scratch-vdl/kldaum1/DYNAFFOR/sierra/outputs/" # ending in /
  path_scripts <- "/home/kldaum/vdl/DYNAFFOR/Scripts_striker/"
  
  inputs <- unlist(lapply(list.files(path_outputs, full.names = T)[grepl(inputs_batch, list.files(path_outputs))], paste0, "/"))
  input.name <- "path"
  script.name <- "BATCH_PostProc_V4KE4.R"
  job.name <- paste0(unlist(strsplit(script.name, "_"))[2], "x")
  # path_logs <- paste0(inputs[1], "Logs/")
  path_logs <- path_scripts
  
  BATCH_submitx(inputs, input.name, job.name, script.name, path_logs)

}

if (exists("BATCHx")){
  
  if (BATCHx == "POSTPROCESS"){
    
    RUNINFOx <- Sys.getenv('RUNINFOx')
    cat(paste0("\nRUNINFOx = ", RUNINFOx, "\n\n"))
    BATCH_plots <- unlist(strsplit(gsub("'", "", RUNINFOx), "___"))
    path_save <- paste0(BATCH_plots[2], "/Outputs/")
    cat(paste0("\npath = ", path_save, "\n\n"))
    
    BATCH <- T
    
    path <- "/home/kldaum/vdl/scratch-vdl/kldaum1/DYNAFFOR/" # ending in /
    path_params <-  "/home/kldaum/vdl/scratch-vdl/kldaum1/DYNAFFOR/Parameters_striker/" # ending in /
    path_inputs <- "/home/kldaum/vdl/scratch-vdl/kldaum1/DYNAFFOR/sierra/inputs/" # ending in /
    path_outputs <- "/home/kldaum/vdl/scratch-vdl/kldaum1/DYNAFFOR/sierra/outputs/" # ending in /
    path_scripts <- "/home/kldaum/vdl/DYNAFFOR/Scripts_striker/" # ending in /
    path_geospa <- "/home/kldaum/vdl/scratch-vdl/kldaum1/DYNAFFOR/Geospatial_striker/" # ending in /
    
    path.alt <- "/Users/MacBook/Desktop/Cellar/Academic/UCSB/Research/LabFees/DYNAFFORE/Model_V4/" # ending in /
    path_params.alt <-  "/Users/MacBook/Desktop/Cellar/Academic/UCSB/Research/LabFees/DYNAFFORE/Model_V4/Parameters/" # ending in /
    path_inputs.alt <- "/Users/MacBook/Desktop/Cellar/Academic/UCSB/Research/LabFees/DYNAFFORE/Model_V4/sierra/inputs/" # ending in /
    path_outputs.alt <- "/Users/MacBook/Desktop/Cellar/Academic/UCSB/Research/LabFees/DYNAFFORE/Model_V4/sierra/outputs/" # ending in /
    path_scripts.alt <- "/Users/MacBook/Desktop/Cellar/Academic/UCSB/Research/LabFees/DYNAFFORE/Model_V4/Scripts/" # ending in /
    path_geospa.alt <- "/Users/MacBook/Desktop/Cellar/Academic/UCSB/Research/LabFees/DYNAFFORE/Model_V4/Geospatial/" # ending in /
    
    KEditsV4 <- T
    module_name <- "Init"
    source(paste0(path_scripts, "BATCH_module_V4KE3.R"))
    
  }
  
  if (BATCHx == "PLOTS"){
    
    RUNINFOx <- Sys.getenv('RUNINFOx')
    cat(paste0("\nRUNINFOx = ", RUNINFOx, "\n\n"))
    BATCH_plots <- unlist(strsplit(gsub("'", "", RUNINFOx), "___"))
    path_PP <- paste0(BATCH_plots[2], "/Outputs/")
    paths_PP <- list.files(path_PP, full.names = T)[grepl("Postprocess", list.files(path_PP))]
    path_PP <- paste0(paths_PP[length(paths_PP)], "/")
    cat(paste0("\nPP path = ", path_PP, "\n\n"))
    
  }
    
}

