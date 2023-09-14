
#### LOCAL or CLUSTER
#### This script interprets the run schedule for each unique experiment (handle) in the run schedule
#### and runs in series (local) or submits each individual model run to the cluster in parallel.
#### Begin runs on local here. Requires manual input of scripts path

# Fetch output from P0 if running on the cluster, otherwise
# clear environment, log the time for running on local.
if (grepl("MacBook", toString(Sys.getenv()))){
  rm(list = ls())
  platform <- "local"
  batch_time <- format(Sys.time(), "%y%m%d%H%M")
} else {
  RUNINFO1 <- Sys.getenv('RUNINFO1') # Fetch output from P0
  cat(paste0("\nRUNINFO1 = ", RUNINFO1, "\n\n"))
  RUNINFO1 <- unlist(strsplit(gsub("'", "", RUNINFO1), "___"))
  batch_name <- RUNINFO1[1]
  batch_time <- RUNINFO1[2]
  platform <- "cluster"
}

# Scripts paths (change these)
if (grepl("MacBook", toString(Sys.getenv()))){
  platform <- "local"
  path_scripts <- "/Users/MacBook/Desktop/Cellar/Academic/UCSB/Research/LabFees/DYNAFFORE/Model_KD_Share/Scripts/"
} else {
  platform <- "cluster"
  path_scripts <- "/home/kldaum/vdl/DYNAFFOR/Scripts_striker/"
}

# Note (saved in outputs)
user_notes <- "Version 1: KD for Manette"

# Run initialization module
module_name <- "Init"
source(paste0(path_scripts, "Modules_KD.R"))

# Look for missing packages
pkgs.missing <- pkgs.required[(pkgs.required %in% (.packages())) == F]
pkgs.unavailable <- pkgs.required[(pkgs.required %in% as.vector(installed.packages()[,1])) == F]
if (length(pkgs.missing) > 0){
  cat(paste0("\nPackages failed to load (but are installed): \n"))
  cat(paste0(pkgs.missing[(pkgs.missing %in% pkgs.unavailable) == F]))
  cat(paste0("\n\nPackages need to be installed: \n"))
  cat(paste0(pkgs.unavailable))
}

# Read run/experiment parameters
params_batch.full <- read.csv(paste0(path_params, "Params_BATCH.csv"))

# Run experiments (handles) one-at-a-time if multiple present (only if on local system)
for (batch_name in unique(params_batch.full$Run_handle)){

  # Subset batch instructions and get experiment run schedule
  if (exists("batch_name")){
    params_batch <- params_batch.full[which(params_batch.full$Run_handle == batch_name),]
    run_schedule <- params_batch
  } else {
    params_batch <- params_batch.full
    run_schedule <- params_batch
    batch_name <- unique(run_schedule$Run_handle)
  }
  
  # Count number of scenarios and number of runs
  cat(paste0("\nBATCH = ", batch_name, "\n\n"))
  n_scen <- nrow(run_schedule)
  n_runs <- sum(run_schedule$Run_rep)
  
  # Check if any runs are already complete (if failed re-submission), if so...
  if (("Complete" %in% names(run_schedule))){
    run_schedule$Complete <- as.logical(run_schedule$Complete)
    if (sum(run_schedule$Complete) != 0){
      
      # If all complete, treat as a re-submission, repeat run schedule
      if (sum(run_schedule$Complete) == nrow(run_schedule)){
        
        run_schedule$Notes <- unlist(lapply(run_schedule$Notes, paste0, paste0(" (Re-simulation of ", gsub("/", "", unique(run_schedule$Run_batch)), ")")))
        run_schedule <- run_schedule[,c(1:which(names(run_schedule) == "Complete"))]
        run_schedule$Complete <- F
      
      # Otherwise, re-run incomplete runs
      } else {
        
        batch_time <- run_schedule$Run_timestamp[1]
        
        path_save <- paste0(path_outputs, unique(run_schedule$Run_batch), "/Outputs/")
        path_db <- paste0(path_outputs, unique(run_schedule$Run_batch), "/Databases/")
        path_save_ras <- paste0(path_save, "/Rasters/")
        
        compl <- length(search.files(path = path_save, terms = "run_schedule_line_COMPLETED", position = ""))
        write.csv(run_schedule[which(run_schedule$Complete == T),], paste0(path_save, "run_schedule_line_COMPLETED", (compl+1), ".csv"), row.names = F)
        run_schedule <- run_schedule[which(run_schedule$Complete == F),]
        
      }
    }
    
  # If not...
  } else {
    
    run_schedule$Complete <- F
    
  }
  
  # If not an incomplete re-submission, build folder system for outputs
  if (sum(as.logical(run_schedule$Complete)) == 0){
    
    batch_handles <- batch_name
    batch_name <- paste0("BATCH_", batch_time, "_", batch_name)
    
    run_schedule$Complete <- F
    run_schedule$Run_timestamp <- batch_time
    run_schedule$Run_batch <- 0
    run_schedule$Run_DB <- 0
    run_schedule$Run_batch <- paste0(batch_name[match(run_schedule$Run_handle, batch_handles)], "/")
    
    options(warn=2)
    
    path_source <- paste0(path_outputs, batch_name, "/")
    for (path.temp in path_source){
      dir.create(path.temp)
    }
    
    path_save <- paste0(path_outputs, batch_name, "/Outputs/")
    for (path.temp in path_save){
      dir.create(path.temp)
    }
    
    path_logs <- paste0(path_save, "/Logs/")
    for (path.temp in path_logs){
      dir.create(path.temp)
    }
    
    path_save_ras <- paste0(path_save, "/Rasters/")
    for (path.temp in path_save_ras){
      dir.create(path.temp)
    }
    
    path_db <- paste0(path_source, "/Databases/")
    for (path.temp in path_db){
      dir.create(path.temp)
    }
    
  }
  
  # If treatment area parameter in run schedule contains multiple values, expand (make a separate copy of the line in the run schedule for each listed area)
  df.temp <- data.frame(cbind(run_schedule$P_burn_area, run_schedule$P_thin_area))
  df.temp <- split(df.temp, seq(nrow(df.temp)))
  
  for (i in c(1:length(df.temp))){
    ar <- unlist(lapply(as.list(gsub(")", "", gsub("c(", "", df.temp[[i]], fixed = T), fixed = T)), strsplit, ", "), recursive = F)
    ar.len <- unlist(lapply(ar, length))
    if (sum(ar.len) > length(ar)){
      multi <- c("P_burn_area", "P_thin_area")[which(ar.len != 1)]
      df.temp.1 <- data.frame(ar[[which(ar.len != 1)]], cbind(rep(ar[[which(ar.len == 1)]], length(ar[[which(ar.len != 1)]]))))
      names(df.temp.1) <- c("P_burn_area", "P_thin_area")
      df.temp.2 <- run_schedule[c(rep(i, nrow(df.temp.1))),]
      df.temp.2[,c("P_burn_area", "P_thin_area")] <- df.temp.1
      df.temp.2$Run_group <- paste0(df.temp.2$Run_group, sprintf("%04d", as.numeric(df.temp.2[,multi])))
      run_schedule <- run_schedule[c(1:nrow(run_schedule))[which(c(1:nrow(run_schedule)) != i)],]
      run_schedule <- rbind(df.temp.2, run_schedule)
      row.names(run_schedule) <- c(1:nrow(run_schedule))
    }
  }
  
  # If biomass reduction parameter in run schedule contains multiple values, expand (make a separate copy of the line in the run schedule for each listed fraction)
  df.temp <- data.frame(cbind(run_schedule$P_burn_redux, run_schedule$P_thin_redux))
  df.temp <- split(df.temp, seq(nrow(df.temp)))
  
  for (i in c(1:length(df.temp))){
    ar <- unlist(lapply(as.list(gsub(")", "", gsub("c(", "", df.temp[[i]], fixed = T), fixed = T)), strsplit, ", "), recursive = F)
    ar.len <- unlist(lapply(ar, length))
    if (sum(ar.len) > length(ar)){
      multi <- c("P_burn_redux", "P_thin_redux")[which(ar.len != 1)]
      df.temp.1 <- data.frame(ar[[which(ar.len != 1)]], cbind(rep(ar[[which(ar.len == 1)]], length(ar[[which(ar.len != 1)]]))))
      names(df.temp.1) <- c("P_burn_redux", "P_thin_redux")
      df.temp.2 <- run_schedule[c(rep(i, nrow(df.temp.1))),]
      df.temp.2[,c("P_burn_redux", "P_thin_redux")] <- df.temp.1
      df.temp.2$Run_group <- paste0(df.temp.2$Run_group, gsub(".", "", as.character(df.temp.2[,multi]), fixed = T))
      run_schedule <- run_schedule[c(1:nrow(run_schedule))[which(c(1:nrow(run_schedule)) != i)],]
      run_schedule <- rbind(df.temp.2, run_schedule)
      row.names(run_schedule) <- c(1:nrow(run_schedule))
    }
  }
  
  # # Formatting compatibility cross-check (3/27/22)
  if (("Run_name" %in% names(run_schedule)) & (sum(c("Run_handle", "Run_group", "Run_rep") %in% names(run_schedule)) == 0)){
    run_schedule <- cbind(data.frame(lextract(run_schedule$Run_name, "_")), run_schedule)
    names(run_schedule)[c(1:3)] <- c("Run_handle", "Run_group", "Run_rep")
    run_schedule$Run_ID <- c(1:nrow(run_schedule))
  } else {
    rm(list = ls()[grepl("df.temp", ls())])
    for (r in c(1:nrow(run_schedule))){
      df.temp <- run_schedule[rep(r, run_schedule$Run_rep[r]),]
      df.temp$Run_rep <- sprintf("%02d", c(1:nrow(df.temp)))
      assign(paste0("df.temp.", r), df.temp)
    }
    run_schedule <- bind_rows(lapply(ls()[grepl("df.temp.", ls(), fixed = T)], get))
    run_schedule$Run_name <- apply(run_schedule[,c("Run_handle", "Run_group", "Run_rep")], MARGIN = 1,  FUN = paste, collapse = "_")
    run_schedule$Run_ID <- c(1:nrow(run_schedule))
    row.names(run_schedule) <- c(1:nrow(run_schedule))
  }
  
  # Complete path for snapshot initialization using local/cluster path_outputs as necessary
  run_schedule$path_snapinit <- unlist(lapply(path_outputs, paste0, run_schedule$P_snapshot_init))
  
  # Log paths used; store in run schedule path column
  for (path in ls()[grep("path_", ls())]){
    run_schedule[,path] <- get(path)
  }
  
  # Set start time
  run_schedule$Time_ini <- 0
  run_schedule$Time_fin <- 0
  run_schedule$Time_mins <- 0
  
  # Define run schedule file path, save
  path_runsched <- paste0(path_save, "run_schedule_", batch_name, ".csv")
  run_schedule$path_runsched <- path_runsched
  write.csv(run_schedule, path_runsched, row.names = F)
  write.csv(params_batch, paste0(path_save, "ParamsBATCH_", batch_name, ".csv"), row.names = F)
  if (batch_name != "All"){
    write.csv(params_batch.full, paste0(path_save, "ParamsFULL_", batch_name, ".csv"), row.names = F)
  }
  
  # Save user note
  fileConn<-file(paste0(path_save, "user_notes.txt"))
  writeLines(user_notes, fileConn)
  close(fileConn)
  
  cat(paste0("\n\nPATH SAVE = ", path_save, "\nBATCH = ", batch_name, "\n\n"))
  
  # Submit individual runs to cluster (in parallel) or run in series on local
  if (platform == "cluster"){
    
    # Cluster
    for (r in c(1:nrow(run_schedule))){
      script.name <- "BATCH_Model_P2_KD.R"
      job.name <- paste0(substr(run_schedule$Run_handle[r], 1, c(nchar(run_schedule$Run_handle[r]), 4)[(nchar(run_schedule$Run_handle[r]) > 5)+1]), "_", sprintf("%03d", r))
      BATCH_submit(r, job.name, script.name)
    }
    
  } else {
    
    # Local
    for (r in c(1:nrow(run_schedule))){
      RUNINFO <- paste0("'", path_runsched, "___", r, "'")
      source(paste0(path_scripts, "BATCH_Model_P2_KD.R"))
    }
    
  }
  
  # Generate list of names for partial run schedule entries (separate runs)
  run_schedule.ix <- unlist(lapply("run_schedule_line_", paste0, sprintf("%03d", c(1: nrow(run_schedule))), ".csv"))
  checked <- NULL
  
  if (platform == "cluster"){
    # Wait for cluster to finish batch
    while("FALSE" %in% run_schedule$Complete){
      
      # Check which runs have completed
      run_schedule$Complete[as.numeric(gsub(".csv", "", gsub("run_schedule_line_", "", run_schedule.ix[which(run_schedule.ix %in% list.files(path_save))])))] <- "TRUE"
      
      # Check for failed runs, document if terminated, save log, resubmit 
      for (r in which(as.character(run_schedule$Time_fin) == "0")){
        path.log <- search.files(path_logs, paste0(run_schedule$Run_name[r], ".log"), "first")
        if (length(path.log) == 1){
          log <- read.delim(path.log)
          if (grepl("Execution halted", log)){
            
            run_schedule$Complete[r] <- "FALSE"
            stime <- as.POSIXct(gsub(format(Sys.time(), "%Z"), "", log[2,]), format = "%a %B  %d %I:%M:%S %p  %Y")
            ftime <- as.POSIXct(gsub(format(Sys.time(), "%Z"), "", log[nrow(log),]), format = "%a %B  %d %I:%M:%S %p  %Y")
            rtime <- as.numeric(difftime(ftime, stime, units = "mins"))
            
            run_schedule$Time_ini[r] <- as.character(stime)
            run_schedule$Time_fin[r] <- "0"
            run_schedule$Time_mins[r] <- rtime
            
            n <- length(search.files(path_logs, c(paste0(run_schedule$Run_name[r], "_FAILED"), ".log"), ""))+1
            file.rename(from = path.log, to = gsub(".log", paste0("_FAILED", n, ".log"), path.log, fixed = T))
            
            cat(paste0("\nERROR: ", run_schedule$Run_name[r], " failed!\n"))
            
            if (file.exists(paste0(path_save, "run_schedule_line_", sprintf("%03d", r), ".csv"))){
              file.remove(paste0(path_save, "run_schedule_line_", sprintf("%03d", r), ".csv"))
            }
            
            script.name <- "BATCH_Model_P2_KD.R"
            job.name <- paste0(substr(run_schedule$Run_handle[r], 1, c(nchar(run_schedule$Run_handle[r]), 4)[(nchar(run_schedule$Run_handle[r]) > 5)+1]), "_", sprintf("%03d", r))
            BATCH_submit(r, job.name, script.name)
            
          }
          if (grepl("Finished run! Thank you for using DYNAFFOREST", log)){
            run_schedule$Complete[r] <- "TRUE"
            stime <- as.POSIXct(gsub(format(Sys.time(), "%Z"), "", log[2,]), format = "%a %B  %d %I:%M:%S %p  %Y")
            ftime <- as.POSIXct(gsub(format(Sys.time(), "%Z"), "", log[nrow(log),]), format = "%a %B  %d %I:%M:%S %p  %Y")
            rtime <- as.numeric(difftime(ftime, stime, units = "mins"))
            run_schedule$Time_ini[r] <- as.character(stime)
            run_schedule$Time_fin[r] <- as.character(ftime)
            run_schedule$Time_mins[r] <- rtime
            cat(paste0("\nSUCCESS: ", run_schedule$Run_name[r], " complete!\n"))
          }
        }
      }
      
      # Take a rest
      Sys.sleep(60)
      
    }
  }
  
  # Re-combine run schedule entries from each model run
  scheds <- list.files(path_save, full.names = F)[grep("run_schedule_line", list.files(path_save))]
  scheds <- as.numeric(gsub(".csv", "", gsub("run_schedule_line_", "", scheds)))
  scheds_paths <- list.files(path_save, full.names = T)[grep("run_schedule_line", list.files(path_save))]
  path_runsched <- search.files(path_save, terms = c("run_schedule", "!line"), position = "last")
  run_schedule <- read.csv(path_runsched)
  for (i in c(1:length(scheds))){
    df.temp <- read.csv(scheds_paths[i])
    run_schedule[scheds[i],] <- df.temp
    file.remove(scheds_paths[i])
    write.csv(run_schedule, path_runsched, row.names = F)
  }
  
  # If all runs are complete...
  if (sum(run_schedule$Complete) == nrow(run_schedule)){
  
    # Combine individual run outputs
    batch_time <- unique(run_schedule$Run_timestamp)
    ftypes <- list.files(path_save)[substr(list.files(path_save), 1, 3) == "Run"]
    ftypes <- unique(unlist(lapply(ftypes[which(grepl("_ras_", ftypes) == F)], strsplitsubr, "_", 1, 1)))
    files_all <- NULL
    for (ftype in ftypes){
      cat(paste0("\nAssimilating files of type ", ftype, "...\n"))
      files <- search.files(path_save, ftype, "")
      df.temp <- read.csv.list(files, batch_time)
      names(df.temp)[1] <- "BATCH"
      write.csv(df.temp, paste0(path_save, ftype), row.names = F)
      files_all <- c(files_all, files)
    }
    
    # Housekeeping
    if ((sum(ftypes %in% list.files(path_save)) == length(ftypes))){
      for (f in files_all){
        file.remove(f)
      }
    }
      
    # If local, run post-processing script. If cluster, submit Slurm script to initiate post-processing routine.
    cat("\nEND OF SIMULATION\n\n")
    cat("\nInitiating Postprocessing\n\n")
    BATCH <- T
    
    if (platform == "local"){
      source(paste0(path_scripts, "BATCH_PostProc_KD.R"))
    } else {
      BATCH_PP <- data.frame(cbind(
        1, unique(run_schedule$Run_handle), 1, F, F,
        paste0(strsplitsubr(unique(run_schedule$path_save), "/", 1, 2), "/Outputs")
      ))
      names(BATCH_PP) <- c("Run_ID", "Run_name", "Interval", "Control", "is_Control", "BATCH")
      write.csv(BATCH_PP, paste0(path_params, "PostProc_BATCH.csv"), row.names = F)
      Sys.sleep(2)
      
      
      SCRIPT <- paste0(
  "#!/usr/bin/bash\n
  #SBATCH --job-name=POSTPROC\n
  #SBATCH --mail-type=END,FAIL\n
  #SBATCH --mail-user=kldaum@ucsb.edu\n
  #SBATCH --ntasks=1\n
  #SBATCH --time=100:00:00\n
  #SBATCH --output=", path_logs, "POSTPROC", ".log\n\n
  pwd; hostname; date\n
  R -f ", path_scripts, "BATCH_PostProc_KD.R","\n
  date\n
  "
      )
      
      RUNINFO <- "blank"
      
      fileConn<-file(paste0(path_scripts, "POSTPROC.sh"))
      writeLines(SCRIPT, fileConn)
      close(fileConn)
      
      system(command = paste0("sbatch ", "--export=RUNINFO=", RUNINFO, " ",path_scripts, "POSTPROC.sh"))
      file.remove(paste0(path_scripts, "POSTPROC.sh"))
      
    }
  }
}
