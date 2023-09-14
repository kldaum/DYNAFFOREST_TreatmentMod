cat(paste0("\n\nINITIATING 'BATCH_PostProc_V4KE4.R'\n\n\n"))


####################################
##### User Input ###################
####################################

## Define key term for R to identify local OS (vs. cluster)
LOCAL <- "MacBook" #@ USER
interval <- 1

## Define script paths for local and cluster
path_scripts.local <- "/Users/MacBook/Desktop/Cellar/Academic/UCSB/Research/LabFees/DYNAFFORE/Model_V4/Scripts/" #@ USER
path_scripts.cluster <- "/home/kldaum/vdl/DYNAFFOR/Scripts_striker/" #@ USER


####################################
##### Initialize ###################
####################################

## If BATCH undefined:
# 1) clear environment & make BATCH <- F
# 2) identify/define scripts path
# 3) initiation module
if (exists("BATCH") == F){
  cat(paste0("BATCH = F"))
  # 1) make BATCH <- F
  BATCH <- F
  
  # 2) identify/define scripts path
  if (grepl(LOCAL, toString(Sys.getenv()))){
    path_scripts <- path_scripts.local
  } else {
    path_scripts <- path_scripts.cluster
  }
  
  # 3) initiation module
  KEditsV4 <- T
  module_name <- "Init"
  source(paste0(path_scripts, "BATCH_module_V4KE3.R"))
  
}

## LOCAL: If running on LOCAL and BATCH == F
# ask user for input (select data, reprocess, etc)
if ((grepl(LOCAL, toString(Sys.getenv()))) & (BATCH == F)){
  
  Sys.sleep(2)
  READ_BATCH <- menu(c("Yes", "No"), 
                     title="Read instructions from PostProc_BATCH.csv? (Enter '1', or '2')")
  while(exists("READ_BATCH") == F){Sys.sleep(1)}
  
  if (READ_BATCH == 2){
    cat("\n\nSelect Run Schedule for target Batch\n\n")
    Sys.sleep(1)
    path_rsched <- file.choose()
    run_schedule <- read.csv(path_rsched)
  }
  
  RECOMPILE <- menu(c("Yes (recompile from sqlite databases, if available)", "No (seek recent data from prior postprocessing, if available)"), 
                    title="Recompile 'sens.df' from databases? (Enter '1', or '2')")
  while(exists("RECOMPILE") == F){Sys.sleep(1)}
  
} else {
  
  RECOMPILE <- 1
  READ_BATCH <- 1
  
}

## CLUSTER: Check for submission instructions from BATCHx.sh
if (Sys.getenv('RUNINFOx') != ""){
  
  BATCHx <- "POSTPROCESS"
  source(paste0(path_scripts, "BATCH_submitx.R"))
  
}

## If BATCH == F, gather run info
if (BATCH == F){
  
  # Fetch post-processing schedule, extract directory
  if (READ_BATCH == 1){
    
    # Load postprocess schedule from parameter file
    PProc_schedule <- suppressWarnings(read.csv(paste0(path_params, "PostProc_BATCH.csv")))
    
  } else {
    
    # Create postprocess schedule manually
    PProc_schedule <- data.frame(cbind(1, 
                                       "ManualReprocess", 
                                       1, 
                                       F, 
                                       (unique(run_schedule$is_Control) == T), 
                                       paste0(unique(run_schedule$Run_batch), "/Outputs/")))
    names(PProc_schedule) <- c("Run_ID", "Run_name", "Interval", "Control", "is_Control", "BATCH")
  }
  
  # Determine increment of measurement
  inc <- which(unlist(lapply(PProc_schedule$path_BATCH, strsplitsubr, "/", 1, 1)) != "Outputs")
  if (length(inc > 0)){
    PProc_schedule$path_BATCH[inc] <- unlist(lapply(PProc_schedule$path_BATCH[inc], paste0, "/Outputs/"))
  }
  
  # Determine path
  PProc_schedule$path_BATCH <- unlist(lapply(path_outputs, paste0, PProc_schedule$BATCH, "/"))
  
  #! WORKAROUND Check for unassimilated files
  for (path.temp in PProc_schedule$path_BATCH){
    if (length(list.files(path.temp)[substr(list.files(path.temp), 1, 3) == "Run"]) > 0){
      cat(paste0("\nAssimilating file type: run_schedule...\n\n"))
      # Combine run schedules
      scheds <- list.files(path.temp, full.names = F)[grep("run_schedule_line", list.files(path.temp))]
      scheds <- as.numeric(gsub(".csv", "", gsub("run_schedule_line_", "", scheds)))
      scheds_paths <- list.files(path.temp, full.names = T)[grep("run_schedule_line", list.files(path.temp))]
      path_runsched <- search.files(path.temp, terms = c("run_schedule", "!line"), position = "last")
      run_schedule <- read.csv(path_runsched)
      for (i in c(1:length(scheds))){
        df.temp <- read.csv(scheds_paths[i])
        run_schedule[scheds[i],] <- df.temp
        #file.remove(scheds_paths[i])
        write.csv(run_schedule, path_runsched, row.names = F)
      }
    } else {
      run_schedule <- read.csv(search.files(path.temp, c("run_schedule_BATCH"), "first"))
    }
    
    # if (length(search.files(path.temp, c("^Run", "!schedule"), "")) > 0){
    # 
    #   if (sum(run_schedule$Complete) == nrow(run_schedule)){

        # Combine individual run outputs
        ftypes <- list.files(path.temp)[substr(list.files(path.temp), 1, 3) == "Run"]
        if (length(ftypes) > 0){
          cat(paste0("\nAssimilating file type: ", toString(ftypes), "...\n\n"))
          ftypes <- unique(unlist(lapply(ftypes[which(grepl("RAS", toupper(ftypes)) == F)], strsplitsubr, "_", 1, 1)))
          for (ftype in ftypes){
            cat(paste0("\nAssimilating file type: ", ftype, "...\n\n"))
            files <- search.files(path.temp, ftype, "")
            df.temp <- read.csv.list(files, gsub("BATCH_", "", strsplitsubr(path.temp, "/", 1, 1)))
            names(df.temp)[1] <- "BATCH"
            write.csv(df.temp, paste0(path.temp, ftype), row.names = F)
            for (f in files){
              #file.remove(f)
            }
          }
        

        cat("\nRUNS ASSIMILATED for:", unlist(strsplitsubr(path.temp, "/", 1, 1)), "\n(", toString(ftypes), ")\n")

        } else {
          cat("\nNO FILES TO ASSIMILATE")
        }
    #   }
    # }
  }
  
  # Fetch run_schedules
  run_schedule <- read.csv.list(search.files(paths = PProc_schedule$path_BATCH, terms = c("run_schedule", "!line", "!PProc", "!Postprocess"), ""), index = PProc_schedule$Run_name)
  names(run_schedule)[1] <- "Source"
  
  # Update DB paths in run schedule
  run_schedule$Run_ID <- c(1:nrow(run_schedule))
  run_schedule$Run_name_PProc <- paste(unlist(lapply(run_schedule$Run_name, strsplitsubr, "_", 1, 2)), unicount(unlist(lapply(run_schedule$Run_name, strsplitsubr, "_", 1, 2))), sep = "_")
  path_source <- gsub(path_outputs.alt, path_outputs, unique(run_schedule$path_save))
  #path_source <- paste0(path_outputs, paste(strsplitsubr(path_source, "/", 2, 2), collapse = "/"), "/")
  path_batch <- gsub("Outputs/", "", path_source)
  path_save_ras <- paste0(path_source, "Rasters/")
  path_db <- paste0(strsplitsubr(path_source, "/", "x", -1), "/Databases/")

  run_schedule$path_DB <- unlist(lapply(path_db, paste0, run_schedule$Run_DB))
  run_schedule$exists_DB <- file.exists(run_schedule$path_DB)
  
  # if (grepl("TEST", toupper(unique(run_schedule$Run_handle)))){
  #   interval <- floor(ceiling(min(run_schedule$Years)/30)/5)*5
  #   if (interval == 0){
  #     interval <- 1
  #   }
  # } else {
  #   interval <- 1
  # }
  
  PProc_schedule$Interval <- interval
  
  if ((sum(run_schedule$exists_DB) < nrow(run_schedule)) & (RECOMPILE == 1)){
    cat(paste0("\nNot all output.sqlite files available locally. Please revise submission.
               \nMissing run: ", run_schedule$Run_name[run_schedule$exists_DB == F], 
               "\nMissing file: ", run_schedule$Run_DB[run_schedule$exists_DB == F],
               "\n\n"))
  }
  
  # Make note...
  cat(paste0("\nPost-Processing: ", toString(PProc_schedule$BATCH), "\n\n"))
  
  # Determine output directory
  if (length(unique(PProc_schedule$path_BATCH)) > 1){
    ## (if BATCH == F, and ) length of BATCH > 1 ...   
    if (unique(PProc_schedule$is_Control) == T){
      # If is a control run
      # New output directory
      run_time <- paste0("CONTROL_", sprintf("%02d", sum(grepl("CONTROL", toupper(list.files(path_outputs))))+1))
      run_time <- format(Sys.time(), "%Y-%m-%d-%H%M%S")
      if (file.exists(paste0(path_outputs, "CONTROL")) == F){
        dir.create(paste0(path_outputs, "CONTROL"))
      }
      path_save <- paste0(path_outputs, "CONTROL/PostProc_", run_time, "_V4KE1/")
      cat(paste0("\n\nDefining new path_save: ", path_save, "\n"))
      dir.create(path_save)
      cat(paste0("complete\n"))
    } else {
      # If is not a control run
      # New output directory
      run_time <- format(Sys.time(), "%Y-%m-%d-%H%M%S")
      path_save <- paste0(path_outputs, "PostProc_", run_time, "_V4KE1/")
      cat(paste0("\n\nDefining new path_save: ", path_save, "\n"))
      dir.create(path_save)
      cat(paste0("complete\n"))
    }
  } else {
    ## (if BATCH == F, and ) length of BATCH == 1 ...
    
    # Define source path for single BATCH directory
    path_source <- paste0(PProc_schedule$path_BATCH, "/")
    path_batch <- gsub("Outputs/", "", path_source)
    path_db <- paste0(PProc_schedule$path_BATCH, "/Databases/")
    
    # Extract run name
    batch_name.full <- unlist(strsplit(PProc_schedule$BATCH, "/"))
    batch_name.full <- batch_name.full[grep("BATCH", batch_name.full)]
    batch_name <- strsplitsubr(batch_name.full, "_", 1, 1)
    
    # Extract run time
    run_time <- strsplitsubr(batch_name.full, "_", 1, 2)
    
    PProc_files <- search.files(path_source, "Postprocess-", c("last", "is.directory"))
    if (length(PProc_files) > 0){
      n_files <- as.numeric(unlist(strsplit(unlist(strsplit(strsplitsubr(PProc_files, "/", 1, 1), "-"))[2], "_"))[1])
    } else {
      n_files <- 0
    }
    
    path_save <- paste0(path_source, "Postprocess-", sprintf("%03d", (n_files+1)), "_", run_time, "_", unique(run_schedule$Run_handle), "/")
    dir.create(path_save)
    
  }
  
  write.csv(PProc_schedule, paste0(path_save, "PProc_schedule.csv"), row.names = F)
  
}

if (BATCH == T){
  
  # Extract run time
  run_time <- gsub("BATCH_", "", strsplitsubr(path_save, "/", 1, 2))
  
  # Redefine path
  if (grepl("Postprocess", path_save)){path_save <- strsplitsubr(path_save, "/", "", -1)}
  path_source <- path_save
  path_batch <- gsub("Outputs/", "", path_source)
  path_save_ras <- paste0(path_save, "Raster/")
  path_db <- paste0(strsplitsubr(path_source, "/", "x", -1), "/Databases/")
  
  n_files <- search.files(path_source, c("Postprocess-", "!csv"), c("last", "is.directory"))
  if (length(n_files) > 0){
    n_files <- as.numeric(gsub("Postprocess-", "", strsplitsubr(strsplitsubr(strsplitsubr(n_files, "/", 1, 1), "-", 1, 1), "_", 1, 2)))
  } else {
    n_files <- 0
  }
  
  # Get run schedule, check for databases
  run_schedule <- read.csv(search.files(path_source, c("run_schedule_", "!line"), ""))
  run_schedule$path_DB <- unlist(lapply(path_db, paste, run_schedule$Run_DB, sep = "/"))
  run_schedule$exists_DB <- file.exists(run_schedule$path_DB)
  
  # Generate new postprocessing output file
  path_save <- paste0(path_source, "Postprocess-", sprintf("%03d", (n_files+1)), "_", run_time, "_", unique(run_schedule$Run_handle), "/")
  if (file.exists(path_save) == F){
    dir.create(path_save)
  }
  
  # Define time series interval
  interval <- round(ceiling(min(run_schedule$Years)/30) / 5)*5
  if (interval == 0){interval <- 1}
  
  # Create new post-processing schedule
  PProc_schedule <- data.frame(cbind(1, "Default", interval, toString(unique(run_schedule$is_Control)), path_source))
  names(PProc_schedule) <- c("Run_ID", "Run_name", "Interval", "is_Control", "BATCH")
  
  cat(paste0("\nPost-Processing: ", toString(PProc_schedule$BATCH), "\n\n"))
  
}

#!#! WORKAROUND FOR UNASSIMILATED FILE TYPES
if (length(list.files(path.temp)[substr(list.files(path.temp), 1, 3) == "Run"]) > 0){
  # Combine individual run outputs
  ftypes <- list.files(path.temp)[substr(list.files(path.temp), 1, 3) == "Run"]
  ftypes <- unique(unlist(lapply(ftypes[which(grepl("RAS", toupper(ftypes)) == F)], strsplitsubr, "_", 1, 1)))
  for (ftype in ftypes){
    cat(paste0("\nAssimilating file type: ", ftype, "...\n\n"))
    files <- search.files(path.temp, ftype, "")
    df.temp <- read.csv.list(files, gsub("BATCH_", "", strsplitsubr(path.temp, "/", 1, 1)))
    names(df.temp)[1] <- "BATCH"
    write.csv(df.temp, paste0(path.temp, ftype), row.names = F)
  }
  cat("\nRUNS ASSIMILATED for:", unlist(strsplitsubr(path.temp, "/", 1, 1)), "\n(", toString(ftypes), ")\n")
}


write.csv(PProc_schedule, paste0(path_save, "PProc_schedule_", run_time, ".csv"), row.names = F)
write.csv(run_schedule, paste0(path_save, "PProc_run_schedule_", run_time, ".csv"), row.names = F)




####################################
##### PostProcess ##################
####################################


# # Get pre-spin-up information
path_spin <- paste0(path_outputs, unique(run_schedule$P_snapshot_init), "/")
run_spin <- as.numeric(gsub("_year", "", strsplitsubr(strsplitsubr(path_spin, "/", 1, 1), "-", 1, 2)))
batch_spin <- unlist(strsplit(path_spin, "/"))[grepl("BATCH", unlist(strsplit(path_spin, "/")))]
year_spin <- as.numeric(strsplitsubr(strsplitsubr(path_spin, "/", 1, 1), "-", 1, 1))
path_spin <- unlist(strsplit(path_spin, "Snapshots"))[1]
path_spinPP <- paste0(search.files(path_spin, "Postprocess-", c("last", "is.directory")), "/")
run_schedule_SPIN <- read.csv(search.files(path_spinPP, "run_schedule", "first"))[run_spin,]

if (file.exists(paste0(path_batch, "/Outputs/TreatLog.csv"))){ #!#!
  treatlog.df <- read.csv(paste0(path_batch, "/Outputs/TreatLog.csv"))
}

# Trim to relevant years
years.spin <- gen_spinup(run_schedule, F) # To modify to exclude treatment spin-up years, second arg = F


# Generate blank data frames and vectors to fill
sens.df <- data.frame()
sens.ras <- data.frame()
run_names <- vector()

file_PP <- unlist(strsplit(path_save, "/"))[length(unlist(strsplit(path_save, "/")))]

if (RECOMPILE == 2){
  
  # Find latest postprocessing directory containing both sens and sens_means
  # If possible, one containing also "Plots"
  paths.temp <- search.files(path_source, c("Postprocess-", "!csv"), c("is.directory"))
  
  path_sens <- search.files(paths.temp, c("sensitivity", c("!means", "!BACKUP")), "last")
  path_sens.means <- search.files(paths.temp, c("sensitivity", c("means", "!BACKUP")), "last")
  
  if (length(path_sens) > 0){
    # path_sens <- unlist(strsplitsubr(path_sens, "/", n, -1))
    path_sens <- gsub(unlist(strsplitsubr(path_sens, "/", n, 1)), "", path_sens)
  } else {
    path_sens.means <- NULL
  }
  
  if (length(path_sens.means) > 0){
    # path_sens.means <- unlist(strsplitsubr(path_sens.means, "/", n, -1))
    path_sens.means <- gsub(unlist(strsplitsubr(path_sens.means, "/", n, 1)), "", path_sens.means)
  } else {
    path_sens.means <- NULL
  }
  
  path_sens <- c(path_sens, path_sens.means)

  df.temp <- data.frame(table(path_sens))
  df.temp[,1] <- as.character(df.temp[,1])
  df.temp <- df.temp %>% subset(Freq > 1)
  
  path_sens.1 <- search.files(paths.temp, c("NoPreSpin"), c("last", "is.directory"))
  if (length(path_sens.1) > 0){
    for (i in c(1:length(path_sens.1))){
      path_sens.1[i] <- gsub(unlist(strsplitsubr(path_sens.1, "/", n, 1))[i], "", path_sens.1[i])
    }
    df.temp <- cbind(df.temp, (gsub("/", "", df.temp[,1]) %in% gsub("/", "", path_sens.1)))
    df.temp <- df.temp[which(df.temp[,3] == T),]
    df.temp <- df.temp[order(df.temp[,2], df.temp[,3]),]
    if (df.temp[1,3] == T){
      path_sens <- paste0(as.character(df.temp[1,1]), "NoPreSpin/")
    } else {
      path_sens <- as.character(df.temp[1,1])
    }
  } else {
    path_sens <- as.character(df.temp[max(which(df.temp$Freq == 2)),1])
  }
  
  # Load existing output data
  # sens.df <- read.csv(search.files(path_sens,  c("sensitivity", "!means"), "first"))
  # sens_means.df <- read.csv(search.files(path_sens,  c("sensitivity", "means"), "first"))
  # firestats.df <- read.csv(search.files(strsplitsubr(path_sens, "/", n, -2),  c("FireStats"), "first"))
  # flux.df <- read.csv(search.files(strsplitsubr(path_sens, "/", n, -2),  c("FluxC"), "first"))
  sens.df <- read.csv(paste0(path_sens, "sensitivity.csv"))
  sens_means.df <- read.csv(paste0(path_sens,  "sensitivity_means.csv"))
  firestats.df <- read.csv(paste0(path_sens, "FireStats.csv"))
  flux.df <- read.csv(paste0(path_sens, "FluxC.csv"))

} else {
  
  # Load existing output data
  flux.df <- read.csv(paste0(path_source, "FluxC.csv"))
  firestats.df <- read.csv(search.files(path = c(paste0(path_batch, "/Outputs/")), terms = c("FireStats.csv"), position = ""))
  
  for (f in which(run_schedule$exists_DB == T)){
    
    name_db.temp <- run_schedule$Run_DB[f]
    path_db.temp <- run_schedule$path_DB[f]
    cat(paste0("\n\nPostprocessing: '", run_schedule$Run_name[f], "' (", f, " of ", (nrow(run_schedule)),")\n\n"))
    cat(paste0("\n\nInterval = ", interval, "\n\n"))
    
    run_name <- run_schedule$Run_name[f]
    rep_name <- paste(unlist(strsplit(run_name, "_"))[1:2], collapse = "_")
    run_years <- run_schedule$Years[f]
    run_num <- run_schedule$Run_ID[f]
    interval_years <- seq(from = 0, to = run_years, by = interval)
    interval_years[1] <- 1
    
    n <- 1
    
    for (i in c(1:length(path_db.temp))){
      dbdata_name <- paste0("dbdata_", gsub(".sqlite", "", gsub("output_", "", name_db.temp)))
      if (exists(dbdata_name)){
        df.temp <- get(dbdata_name)
      } else {
        cat("\nConnecting to", strsplitsubr(path_db.temp[i], "/", 1,1), "...\n\n")
        output.db <- DBI::dbConnect(RSQLite::SQLite(), dbname=path_db.temp[i])
        
        #dbListTables(db.conn) # List tables
        
        # Get fuel/fire data
        cat("\nCollecting 'mortality' data...\n")
        dataset <- tbl(output.db, "Dead_fuel_fire")
        data.mort <- dataset%>%filter(year %in% interval_years)
        data.mort <- data.frame(data.mort)
        
        # Get stand data
        cat("\nCollecting 'stand' data...\n")
        dataset <- tbl(output.db, "Stand")
        data.stand <- dataset%>%filter(year %in% interval_years)
        data.stand <- data.frame(data.stand)
        
        df.temp <- data.frame(cbind(data.stand, data.mort))
        df.temp <- df.temp[,which(duplicated(names(df.temp)) == F)]
        
        # Substitute 0 for NA in applicable columns
        df.temp$stand.density.ha[is.na(df.temp$stand.density.ha)] <- 0
        df.temp$avail.litter.kg[is.na(df.temp$avail.litter.kg)] <- 0
        df.temp$avail.cwd.kg[is.na(df.temp$avail.cwd.kg)] <- 0
        df.temp$ck[is.na(df.temp$ck)] <- 0
        
        # Remove rows with pft = NA
        #df.temp <- df.temp[which(is.na(df.temp$pft) == F),]
        
        # Add total biomass columns
        df.temp$biomass.sum <- (df.temp$stem.biomass.kg + df.temp$leaf.biomass.kg + df.temp$branch.biomass.kg)
        df.temp$biomass_dead.sum <- (df.temp$litter.kg + df.temp$cwd.kg + df.temp$snag.kg)
        
        assign(dbdata_name, df.temp)
        DBI::dbDisconnect(output.db)
      }
      
      if (i == 1){
        dbdata <- df.temp
      } else {
        dbdata <- bind_rows(dbdata, df.temp)
      }
      
    }
    
    # Build mortality dataset
    data_grouped <- dbdata %>% dplyr::group_by(year, pft) %>% summarize(
      coverage = length(x)/n,
      mortality = sum(death)/n,
      mortality_fire = sum(fire.death)/n,
      burned_area = length(which(fire.sev != 0))/n,
      age.mean = mean(stand_age),
      age.sd = sd(stand_age),
      biomass.mean = mean(biomass.sum),
      biomass.sd = sd(biomass.sum),
      biomass_dead.mean = mean(biomass_dead.sum),
      biomass_dead.sd = sd(biomass_dead.sum)
    )
    
    data_grouped <- data.frame(cbind(run_name, toString(name_db.temp), file_PP, run_num, data_grouped))
    names(data_grouped)[1:4] <- c("treatment", "filename", "directory", "RunID")
    sens.df <- rbind(sens.df, data_grouped)
    write.csv(data.frame(sens.df), paste0(path_save, "sensitivity_BACKUP_", run_time, ".csv"), row.names = F)
    
  }

  # Extract treatment information
  sens.df[which(unlist(lapply(strsplit(sens.df$treatment, "_"), length)) < 3),"treatment"] <- 
    unlist(lapply(sens.df[which(unlist(lapply(strsplit(sens.df$treatment, "_"), length)) < 3),"treatment"], paste0, "_0"))
  sens.df$treatment_type <- unlist(strsplit(sens.df$treatment, "_"))[(c(1:(nrow(sens.df)*3)) %% 3 == T)]
  sens.df$treatment_val <- unlist(strsplit(sens.df$treatment, "_"))[(c(3:((nrow(sens.df)*3) + 3)) %% 3 == T)]
  sens.df$treatment_rep <- unlist(strsplit(sens.df$treatment, "_"))[(c(2:((nrow(sens.df)*3) + 2)) %% 3 == T)]
  
  rep_rows <- which(unlist(lapply(strsplit(sens.df$treatment, "_"), length)) == 2)
  sens.df$treatment_type[rep_rows] <- sapply(strsplit(sens.df$treatment[rep_rows], "_"),"[[",1)
  sens.df$treatment_val[rep_rows] <- sapply(strsplit(sens.df$treatment[rep_rows], "_"),"[[",2)
  sens.df$treatment_rep[rep_rows] <- 0
  
  # Get comparisons of means by treatment
  sens_means.df <- data.frame(sens.df %>% 
                                subset(treatment_rep != 0) %>%
                                group_by(treatment_type, 
                                         treatment_val,
                                         directory,
                                         year, 
                                         pft) %>% 
                                summarise(
                                  RunID = toString(unique(RunID)),
                                  treatment = paste0(unique(treatment_type), "_", unique(treatment_val)),
                                  treatment_rep = toString(unique(treatment_rep)),
                                  filename = toString(unique(filename)),
                                  coverage.mean = mean(coverage),
                                  coverage.sd = sd(coverage),
                                  mortality.mean = mean(mortality),
                                  mortality.sd = sd(mortality),
                                  mortality_fire.mean = mean(mortality_fire),
                                  mortality_fire.sd = sd(mortality_fire),
                                  burned_area.mean = mean(burned_area),
                                  burned_area.sd = sd(burned_area),
                                  age.mean = mean(age.mean),
                                  age.sd = sd(age.mean),
                                  biomass.mean = mean(biomass.mean),
                                  biomass.sd = sd(biomass.mean),
                                  biomass_dead.mean = mean(biomass_dead.mean),
                                  biomass_dead.sd = sd(biomass_dead.mean)
                                ))
  
  sens_means.df$sample_n <- unlist(lapply(lapply(strsplit(as.character(sens_means.df$RunID), ", "), as.numeric), length))
  
  sens.df <- data.frame(sens.df %>% 
                          subset(treatment_rep != 0) %>%
                          group_by(RunID,
                                   treatment_type, 
                                   treatment_val,
                                   treatment,
                                   filename,
                                   treatment_rep,
                                   directory,
                                   year, 
                                   pft) %>% 
                          summarise(
                            coverage.mean = mean(coverage),
                            coverage.sd = sd(coverage),
                            mortality.mean = mean(mortality),
                            mortality.sd = sd(mortality),
                            mortality_fire.mean = mean(mortality_fire),
                            mortality_fire.sd = sd(mortality_fire),
                            burned_area.mean = mean(burned_area),
                            burned_area.sd = sd(burned_area),
                            age.mean = mean(age.mean),
                            age.sd = sd(age.mean),
                            biomass.mean = mean(biomass.mean),
                            biomass.sd = sd(biomass.mean),
                            biomass_dead.mean = mean(biomass_dead.mean),
                            biomass_dead.sd = sd(biomass_dead.mean)
                          ))
  
  sens.df$sample_n <- unlist(lapply(lapply(strsplit(as.character(sens.df$RunID), ", "), as.numeric), length))

}

# sens.df <- read.csv(search.files(path_nopre,  c("sensitivity", "!means"), "first"))
# sens_means.df <- read.csv(search.files(path_nopre,  c("sensitivity", "means"), "first"))
# firestats.df <- read.csv(paste0(path_nopre, "FireStats.csv"))
# flux.df <- read.csv(paste0(path_nopre, "FluxC.csv"))

for (df.name in c("sens.df",
                  "sens_means.df",
                  "firestats.df",
                  "flux.df"
                  )){
  
  df.temp <- get(df.name)
  
  # lists to vectors
  vec.temp <- as.vector(apply(df.temp, 2, class))
  if (sum(grepl("list", vec.temp)) > 0){
    for (l in which(vec.temp == "list")){
      df.temp[,l] <- unlist(df.temp[,l])
      if (is.numeric(df.temp[,l])){
        df.temp[,l] <- as.numeric(df.temp[,l])
      }
    }
  }
  
  # spin up categories
  spinup.df <- gen_spinup(run_schedule, F)
  df.temp$spin_up_pre <- F
  df.temp$spin_up <- F
  df.temp <- df.temp[,!grepl("spinup", names(df.temp))]
  for (g in unique(spinup.df$Group)){
    df.temp[which((df.temp$treatment_val == g) & (df.temp$year <= spinup.df[which(spinup.df$Group == g), "Spin"])),"spin_up"] <- T
  }
  
  # grouping
  if (sum(grepl("group", tolower(names(df.temp)))) == 0){
    if ("treatment_val" %in% tolower(names(df.temp))){
      df.temp$group <- df.temp[,grep("treatment_val", tolower(names(df.temp)))]
    } else {
      df.temp$group <- unlist(strsplitsubr(df.temp[,which(tolower(names(df.temp)) == "treatment")], "_", 2, 2))
    }
  }
  
  assign(df.name, df.temp)
  
}

if (dir.exists(paste0(path_save, "NoPreSpin/")) == F){
  path_nopre <- paste0(path_save, "NoPreSpin/")
  dir.create(path_nopre)
}

write.csv(unlist_cols(sens.df), paste0(path_nopre, "sensitivity.csv"), row.names = F)
write.csv(unlist_cols(sens_means.df), paste0(path_nopre, "sensitivity_means.csv"), row.names = F)
write.csv(unlist_cols(firestats.df), paste0(path_nopre, "FireStats.csv"), row.names = F)
write.csv(unlist_cols(flux.df), paste0(path_nopre, "FluxC.csv"), row.names = F)


for (df.name in c("sens.df", "sens_means.df")){
  
  df.temp <- get(df.name)
  
  # NAs to 0
  df.temp[is.na(df.temp)] = 0
  
  # Look up PFT names
  df.temp$pft_name <- pft_names$Name[match(df.temp$pft, pft_names$ID)]
  
  # Original value reference
  cov_orig <- df.temp[which((df.temp$year == min(abs(df.temp$year))) & (df.temp$treatment == df.temp$treatment[1])), c("pft", "coverage.mean")]
  cov_orig <- rbind(cov_orig, setNames(data.frame(cbind(c(0:max(df.temp$pft))[which(c(0:max(df.temp$pft)) %in% cov_orig$pft == F)], 1)), names(cov_orig)))
  cov_orig <- cov_orig[order(cov_orig$pft),]
  cov_orig <- data.frame(apply(cov_orig, FUN = as.numeric, MARGIN = 2))
  
  # Generate values proportional to pft coverage / original pft coverage
  df.temp$mortality.by_area_orig <- df.temp$mortality.mean/cov_orig$coverage.mean[match(df.temp$pft, cov_orig$pft)]
  df.temp$mortality.by_area <- df.temp$mortality.mean/df.temp$coverage.mean
  
  df.temp$mortality_fire.by_area_orig <- df.temp$mortality_fire.mean/cov_orig$coverage[match(df.temp$pft, cov_orig$pft)]
  df.temp$mortality_fire.by_area <- df.temp$mortality_fire.mean/df.temp$coverage.mean
  
  df.temp$burned_area.by_area_orig <- df.temp$burned_area.mean/cov_orig$coverage[match(df.temp$pft, cov_orig$pft)]
  df.temp$burned_area.by_area <- df.temp$burned_area.mean/df.temp$coverage.mean
  
  df.temp$coverage.perc_orig <- (df.temp$coverage.mean/cov_orig$coverage[match(df.temp$pft, cov_orig$pft)])*100
  
  # Track values cumulatively
  df.temp <- data.frame(df.temp %>% group_by(treatment, pft) %>% mutate(burned_area.cumu = cumsum(burned_area.mean)))
  df.temp <- data.frame(df.temp %>% group_by(treatment, pft) %>% mutate(mortality.cumu = cumsum(mortality.mean)))
  df.temp <- data.frame(df.temp %>% group_by(treatment, pft) %>% mutate(mortality_fire.cumu = cumsum(mortality_fire.mean)))
  
  row.names(df.temp) <- c(1:nrow(df.temp))
  
  # Add mean and standard deviation values for columns generated above
  if (df.name == "sens_means.df"){
    newcols <- c("mortality.by_area_orig", 
                 "mortality.by_area",
                 "mortality_fire.by_area_orig",
                 "mortality_fire.by_area",
                 "burned_area.by_area_orig",
                 "burned_area.by_area",
                 "coverage.perc_orig",
                 "burned_area.cumu",
                 "mortality.cumu",
                 "mortality_fire.cumu"
    )
    newcols.ix <- unique(unlist(lapply(lapply(newcols, grepl, names(df.temp)), which)))
    
    df.temp <- df.temp[,which(c(1:ncol(df.temp)) %in% newcols.ix == F)]
    for (cnam in newcols){
      cvals <- data.frame(sens.df %>% 
                            group_by(year, treatment_val, pft) %>% 
                            dplyr::select(as.name(cnam)) %>% 
                            summarize(col.mean := mean(!! rlang::sym(cnam)),
                                      col.sd := sd(!! rlang::sym(cnam), na.rm = T)))
      if (unique(is.na(cvals$col.sd)) == T){cvals$col.sd <- 0}
      names(cvals)[c(4:5)] <- unlist(lapply(cnam, paste0, c(".mean", ".sd")))
      cvals$treatment_val <- unlist(cvals$treatment_val)
      df.temp <- right_join(df.temp, cvals, by = c("year" = "year", "pft" = "pft", "treatment_val" = "treatment_val"))
    }
  }
  
  # Resize
  minyr <- min(data.frame(df.temp %>% group_by(treatment) %>% summarize(maxyear = max(year)))[,2])
  df.temp <- df.temp %>% subset(year <= minyr)
  
  assign(df.name, df.temp)

}

run_schedule <- cbind(strsplitsubr(path_save, "/", 1, 1), run_schedule)
names(run_schedule)[1] <- "index"

sens_means.df$index <- run_schedule$index[match(sens_means.df$treatment, unlist(lapply(run_schedule$Run_name, strsplitsubr, "_", "", -1)))]
sens_means.df$index[grepl("_0", sens_means.df$treatment)] <- sens_means.df$index[match(gsub("_0", "_1", sens_means.df$treatment[grepl("_0", sens_means.df$treatment)]), sens_means.df$treatment)]

# Save 
write.csv(run_schedule, paste0(path_save, "PProc_run_schedule_", run_time, ".csv"), row.names = F)
write.csv(apply(sens.df,2,as.character), paste0(path_save, "sensitivity_", run_time, ".csv"), row.names = F)
write.csv(apply(sens_means.df,2,as.character), paste0(path_save, "sensitivity_means_", run_time, ".csv"), row.names = F)

if (unique(PProc_schedule$is_Control) == T){
  paths_aux <- search.files(paths = PProc_schedule$path_BATCH, terms = c("!.sqlite", "/Run"), "")
}


########### FLUX ###########
# Take mean values of flux
flux_means.df <- data.frame(flux.df %>% group_by(run_group, year) %>% summarize(batch = unique(BATCH),
                                                                                run_rep = 0,
                                                                                run_handle = unique(run_handle),
                                                                                treatment = paste0(run_handle, "_", run_group),
                                                                                flux_net = mean(flux_net),
                                                                                flux_pos = mean(flux_pos),
                                                                                flux_neg = mean(flux_neg),
                                                                                combust_live = mean(combust_live),
                                                                                combust_dead = mean(combust_dead),
                                                                                decomp = mean(decomp), 
                                                                                spin_up = unique(spin_up),
                                                                                spin_up_pre = toString(unique(spin_up_pre))))
flux_means.df <- flux_means.df[(duplicated(flux_means.df) == F),]
names(flux.df) <- tolower(names(flux.df))
flux_means.df <- flux_means.df[,names(flux.df)]
flux.df <- rbind(flux.df, flux_means.df)
names(flux.df)[4] <- "group"
write.csv(flux.df, paste0(path_save, "FluxCMeans.csv"), row.names = F)

# Formatting for compatibility
for (df.name in c("sens.df", "sens_means.df", "firestats.df", "treatlog.df")){ # , "array_summary")){
  if (exists(df.name)){
    df.temp <- get(df.name)
    names(df.temp) <- tolower(names(df.temp))
    names <- names(df.temp)
    if (length(unique(df.temp[,which(names == "treatment_val")])) > 1){
      df.temp[which(grepl("CONTROL", toupper(df.temp[,which(names == "treatment_val")])) & (grepl("*", df.temp[,which(names == "treatment_val")], fixed = T) == F)), which(names == "treatment_val")] <- "*CONTROL" #!#!
      df.temp$group <- df.temp[,which(names == "treatment_val")]
    } else {
      if (df.name == "sens_means.df"){
        df.temp$group <- df.temp[,which(names == "treatment_val")]
      } else {
        if (df.name == "treatlog.df"){
          df.temp <- cbind(bind_rows(lapply(lapply(strsplit(df.temp$run_name, "_"), rbind), data.frame)), df.temp)
          names(df.temp)[1:3] <- c("treatment_type", "treatment_val", "treatment_rep")
          names <- names(df.temp)
          df.temp$group <- df.temp$treatment_val
          #df.temp$group <- str_c(df.temp[,which(names == "treatment_val")], sprintf("%02d",df.temp[,which(names == "treatment_rep")]), sep = "_")
        } else {
          df.temp$group <- df.temp$treatment_val
          #df.temp$group <- str_c(df.temp[,which(names == "treatment_val")], sprintf("%02d",df.temp[,which(names == "treatment_rep")]), sep = "_")
        }
      }
    }
    
    df.temp$spin_up <- F
    for (g in unique(df.temp$group)){
      df.temp[which((df.temp$group == g) & (df.temp$year < as.numeric(years.spin$Spin[years.spin$Group == g]))), "spin_up"] <- T
      row.names(df.temp) <- c(1:nrow(df.temp))
      df.temp <- df.temp[which(row.names(df.temp) %in% which((df.temp$group == g) & (df.temp$year > as.numeric(years.spin$Max[years.spin$Group == g]))) == F),]
    }
    
    assign(df.name, df.temp)
  
  }
}

# Dummy entry for pft 0 at t1 if absent
for (df.name in c("sens.df", "sens_means.df")){
  df <- get(df.name)
  pft0 <- unique(df$treatment)[which(unique(df$treatment) %in% as.vector(unlist(unique(df %>% subset(year == min(year)) %>% subset(pft == 0) %>% dplyr::select(treatment)))) == F)]
  for (gr in unique(pft0)){
    df.temp <- data.frame(df %>% subset(treatment == gr))[1,]
    df.temp$pft <- 0
    df.temp[,c(which(names(df.temp) == "coverage.mean"):(ncol(df.temp)-3))] <- 0
    df.temp$pft_name <- pft_names$Name[1]
    df <- rbind(df.temp, df)
    assign(df.name, df)
  }
}

# Elaborate and group fire stats
firestats_cell.df <- firestats.df
firestats_cell.df$to_road <- roads.ras[firestats_cell.df$cell]
firestats_cell.df$to_wui <- WUI.ras[firestats_cell.df$cell]
firestats_cell.df$slope <- slope.ras[firestats_cell.df$cell]
if (exists("treatlog.df")){
  firestats_cell.df$in_treatzone <- F
  for (t in c(1:nrow(run_schedule))){
    treat_quants <- run_schedule[t,grep("P_quant_", names(run_schedule))]
    treat_quants <- treat_quants[,which(names(treat_quants) %in% c("P_quant_conn", "P_quant_lbio", "P_quant_dbio") == F)]
    treat_quants <- treat_quants %>% dplyr::select(which((as.character(treat_quants[1,]) %in% c("-9999", "SORT") == F) & (is.na(treat_quants[1,]) == F)))
    if (ncol(treat_quants) > 0){
      treat_factors <- gsub("P_quant_", "", names(treat_quants))
      for (i in c(1:length(treat_factors))){
        ras.temp <- list.blank[[1]]
        ras.temp1 <- list.blank[[1]]
        ras <- get(paste0(treat_factors[i], ".ras"))
        quant <- treat_quants[1,i]
        
        if (grepl("%", quant)){
          ras.ix <- which((is.na(ras) == F) & (ras != 0) & (list.blank[[1]] == 0))
          df.temp <- cbind(ras.ix, ras[ras.ix])
          df.temp <- cbind(df.temp, rank(df.temp[,2], ties.method="min"))
          ras.rank <- round(df.temp[,3]/length(df.temp[,3]) * 100)
          if (length(unique(ras.rank)) == 1){
            ras.rank[c(1:length(ras.rank))] <- 100
          }
          df.temp[,3] <- ras.rank
          ras.temp[ras.ix] <- eval(parse(text = paste0("ras.rank", gsub("%", "", quant))))
        } else {
          if (grepl("=", quant)){
            ras.temp[which(eval(parse(text = paste0("ras ", gsub("#", "", quant)))) & (list.blank[[1]] == 0))] <- 1
          } else {
            ras.temp[which(eval(parse(text = paste0("ras %in% c(", quant, ")"))) & (list.blank[[1]] == 0))] <- 1
          }
        }
        assign(paste0(treat_factors[i], ".ix"), which(ras.temp == 1))
      }
      
      treat.ix <- as.numeric(row.names(data.frame(which(table(unlist(lapply(unlist(lapply(treat_factors, paste0, ".ix")), get))) == length(treat_factors)))))
      firestats_cell.df[which(firestats_cell.df$treatment == run_schedule$Run_name[t]),"in_treatzone"] <- (firestats_cell.df[which(firestats_cell.df$treatment == run_schedule$Run_name[t]),"cell"] %in% treat.ix)
      assign(paste0("treat.ix.", t), treat.ix)
      
    }
  }
  
  treat.ix <- unique(unlist(lapply(ls()[grepl("treat.ix.", ls(), fixed = T)], get)))
  
  if (toString(unique(c(run_schedule$P_burn_area, run_schedule$P_thin_area))) == "-9999"){
    treat.ix <- which(is.na(list.blank[[1]]) == F)
  }
  
  for (t in which(run_schedule$Run_group %in% unique(treatlog.df$treatment_val) == F)){
    firestats_cell.df[which(firestats_cell.df$treatment == run_schedule$Run_name[t]),"in_treatzone"] <- (firestats_cell.df[which(firestats_cell.df$treatment == run_schedule$Run_name[t]),"cell"] %in% treat.ix)
  }
  
} else {
  firestats_cell.df$in_treatzone <- T
  treat.ix <- which(is.na(list.blank[[1]]) == F)
}

# firestats_treatment_all.df <-  data.frame(firestats_cell.df %>%
#                                          group_by(year, treatment) %>%
#                                          summarize(group = unique(group),
#                                                    treatment_val = unique(treatment_val),
#                                                    spin_up = unique(spin_up),
#                                                    burned = (length(cell)),
#                                                    fire.count = length(unique(fire_id)),
#                                                    severity.mean = mean(fire_severity),
#                                                    severity.sd = sd(fire_severity),
#                                                    to_grassshrub = sum(to_grassshrub),
#                                                    population = sum(population),
#                                                    population_severe1 = sum(population*(((crown_kill.perc > 0) & (crown_kill.perc <= 25))/100)),
#                                                    population_severe2 = sum(population*(((crown_kill.perc > 25) & (crown_kill.perc <= 50))/100)),
#                                                    population_severe3 = sum(population*(((crown_kill.perc > 50) & (crown_kill.perc <= 75))/100)),
#                                                    population_severe4 = sum(population*(((crown_kill.perc > 75) & (crown_kill.perc <= 99))/100)),
#                                                    population_severe5 = sum(population*(((crown_kill.perc > 99)/100))),
#                                                    to_wui.mean = mean(to_wui),
#                                                    biomass_dead.delta = sum(biomass_dead.delta),
#                                                    biomass_live.delta = sum(biomass_live.delta),
#                                                    biomass.delta = sum(biomass.delta),
#                                                    fire_probability.mean = mean(fire_probability),
#                                                    crown_kill.sum = sum(crown_kill.perc),
#                                                    crown_kill.mean = mean(crown_kill.perc),
#                                                    stand_kill.sum = sum(stand_kill),
#                                                    stand_kill.mean = mean(stand_kill),
#                                                    combusted_litter.sum = sum(combusted_litter),
#                                                    combusted_cwd.sum = sum(combusted_cwd),
#                                                    combusted_lbio.sum = sum(combusted_lbio),
#                                                    combusted_biomass.sum = sum(combusted_biomass)
#                                          ) %>%
#                                          group_by(year, group) %>%
#                                          summarize(treatment_val = unique(treatment_val),
#                                                    spin_up = unique(spin_up),
#                                                    burned.mean = mean(burned, na.rm = T),
#                                                    burned.sd = sd(burned, na.rm = T),
#                                                    fire_rotation = (length(as.vector(unlist(list.blank[[1]])) == 0)/mean(burned, na.rm = T)),
#                                                    fire.count.mean = mean(fire.count, na.rm = T),
#                                                    fire.count.sd = sd(fire.count, na.rm = T),
#                                                    severity.gmean = mean(severity.mean, na.rm = T),
#                                                    severity.sd = sd(severity.mean),
#                                                    to_grassshrub.mean = mean(to_grassshrub),
#                                                    to_grassshrub.sd = sd(to_grassshrub),
#                                                    population.mean = mean(population, na.rm = T),
#                                                    population.sd = sd(population, na.rm = T),
#                                                    population_severe1 = mean(population_severe1),
#                                                    population_severe2 = mean(population_severe2),
#                                                    population_severe3 = mean(population_severe3),
#                                                    population_severe4 = mean(population_severe4),
#                                                    population_severe5 = mean(population_severe5),
#                                                    to_wui.gmean = mean(to_wui.mean),
#                                                    biomass_dead.delta.mean = mean(biomass_dead.delta, na.rm = T),
#                                                    biomass_dead.delta.sd = sd(biomass_dead.delta, na.rm = T),
#                                                    biomass_live.delta.mean = mean(biomass_live.delta, na.rm = T),
#                                                    biomass_live.delta.sd = sd(biomass_live.delta, na.rm = T),
#                                                    biomass.delta.mean = mean(biomass.delta, na.rm = T),
#                                                    biomass.delta.sd = sd(biomass.delta, na.rm = T),
#                                                    fire_probability.gmean = mean(fire_probability.mean),
#                                                    crown_kill.sum.mean = mean(crown_kill.sum),
#                                                    crown_kill.gmean = mean(crown_kill.mean),
#                                                    stand_kill.sum.mean = mean(stand_kill.sum),
#                                                    stand_kill.gmean = mean(stand_kill.mean),
#                                                    combusted_litter.sum.mean = mean(combusted_litter.sum),
#                                                    combusted_cwd.sum.mean = mean(combusted_cwd.sum),
#                                                    combusted_lbio.sum.mean = mean(combusted_lbio.sum),
#                                                    combusted_biomass.sum.mean = mean(combusted_biomass.sum)
#                                          ))
# 
# # Group fire by fires within treatable area (maximum among scenarios)
# firestats_treatment_in.df <- data.frame(firestats_cell.df %>%
#                                        subset(in_treatzone == T) %>%
#                                        group_by(year, treatment) %>%
#                                        summarize(group = unique(group),
#                                                  treatment_val = unique(treatment_val),
#                                                  spin_up = unique(spin_up),
#                                                  burned = (length(cell)),
#                                                  fire_rotation = (length(treat.ix)/mean(burned, na.rm = T)),
#                                                  fire.count = length(unique(fire_id)),
#                                                  severity.mean = mean(fire_severity),
#                                                  severity.sd = sd(fire_severity),
#                                                  to_grassshrub = sum(to_grassshrub),
#                                                  population = sum(population),
#                                                  population_severe1 = sum(population*(((crown_kill.perc > 0) & (crown_kill.perc <= 25))/100)),
#                                                  population_severe2 = sum(population*(((crown_kill.perc > 25) & (crown_kill.perc <= 50))/100)),
#                                                  population_severe3 = sum(population*(((crown_kill.perc > 50) & (crown_kill.perc <= 75))/100)),
#                                                  population_severe4 = sum(population*(((crown_kill.perc > 75) & (crown_kill.perc <= 99))/100)),
#                                                  population_severe5 = sum(population*(((crown_kill.perc > 99)/100))),
#                                                  to_wui.mean = mean(to_wui),
#                                                  biomass_dead.delta = sum(biomass_dead.delta),
#                                                  biomass_live.delta = sum(biomass_live.delta),
#                                                  biomass.delta = sum(biomass.delta),
#                                                  fire_probability.mean = mean(fire_probability),
#                                                  crown_kill.sum = sum(crown_kill.perc),
#                                                  crown_kill.mean = mean(crown_kill.perc),
#                                                  stand_kill.sum = sum(stand_kill),
#                                                  stand_kill.mean = mean(stand_kill),
#                                                  combusted_litter.sum = sum(combusted_litter),
#                                                  combusted_cwd.sum = sum(combusted_cwd),
#                                                  combusted_lbio.sum = sum(combusted_lbio),
#                                                  combusted_biomass.sum = sum(combusted_biomass)
#                                        ) %>%
#                                        group_by(year, group) %>%
#                                        summarize(treatment_val = unique(treatment_val),
#                                                  spin_up = unique(spin_up),
#                                                  burned.mean = mean(burned, na.rm = T),
#                                                  burned.sd = sd(burned, na.rm = T),
#                                                  fire.count.mean = mean(fire.count, na.rm = T),
#                                                  fire.count.sd = sd(fire.count, na.rm = T),
#                                                  severity.gmean = mean(severity.mean, na.rm = T),
#                                                  severity.sd = sd(severity.mean),
#                                                  to_grassshrub.mean = mean(to_grassshrub),
#                                                  to_grassshrub.sd = sd(to_grassshrub),
#                                                  population.mean = mean(population, na.rm = T),
#                                                  population.sd = sd(population, na.rm = T),
#                                                  population_severe1 = mean(population_severe1),
#                                                  population_severe2 = mean(population_severe2),
#                                                  population_severe3 = mean(population_severe3),
#                                                  population_severe4 = mean(population_severe4),
#                                                  population_severe5 = mean(population_severe5),
#                                                  to_wui.gmean = mean(to_wui.mean),
#                                                  biomass_dead.delta.mean = mean(biomass_dead.delta, na.rm = T),
#                                                  biomass_dead.delta.sd = sd(biomass_dead.delta, na.rm = T),
#                                                  biomass_live.delta.mean = mean(biomass_live.delta, na.rm = T),
#                                                  biomass_live.delta.sd = sd(biomass_live.delta, na.rm = T),
#                                                  biomass.delta.mean = mean(biomass.delta, na.rm = T),
#                                                  biomass.delta.sd = sd(biomass.delta, na.rm = T),
#                                                  fire_probability.gmean = mean(fire_probability.mean),
#                                                  crown_kill.sum.mean = mean(crown_kill.sum),
#                                                  crown_kill.gmean = mean(crown_kill.mean),
#                                                  stand_kill.sum.mean = mean(stand_kill.sum),
#                                                  stand_kill.gmean = mean(stand_kill.mean),
#                                                  combusted_litter.sum.mean = mean(combusted_litter.sum),
#                                                  combusted_cwd.sum.mean = mean(combusted_cwd.sum),
#                                                  combusted_lbio.sum.mean = mean(combusted_lbio.sum),
#                                                  combusted_biomass.sum.mean = mean(combusted_biomass.sum)
#                                        ))

# Group fire data by fire event
firestats_fire.df <- data.frame(firestats_cell.df %>%
                               group_by(year, treatment, fire_id) %>%
                               summarize(
                                 group = unique(group),
                                 treatment_val = unique(treatment_val),
                                 treatment_rep = unique(treatment_rep),
                                 spin_up = unique(spin_up),
                                 size_km = length(cell),
                                 cdf_lookup = unique(cdf_lookup),
                                 cdf_size = unique(cdf_size),
                                 mtbs_id = unique(mtbs_id),
                                 cell_sample = cell[round(length(cell)/2)],
                                 severity.mean = mean(fire_severity),
                                 severity.sd = sd(fire_severity),
                                 crown_kill.mean = mean(crown_kill.perc),
                                 stand_kill.perc = (sum(stand_kill)/length(cell))*100,
                                 to_GrassShrub = sum(to_grassshrub),
                                 population = sum(population),
                                 to_road.mean = mean(to_road),
                                 to_wui.mean = mean(to_wui),
                                 slope.mean = mean(slope),
                                 in_treatzone.perc = (100*(length(which(in_treatzone == 1))/length(in_treatzone))),
                                 pft = toString(unique(pft)),
                                 biomass_dead.delta = sum(biomass_dead.delta),
                                 biomass_live.delta = sum(biomass_live.delta),
                                 biomass.delta = sum(biomass.delta)
                               ))

# for (c in grep(".sd", names(firestats_treatment_all.df))){
#   is.na(firestats_treatment_all.df[,c]) <- 0
# }
# 
# for (c in grep(".sd", names(firestats_treatment_in.df))){
#   is.na(firestats_treatment_in.df[,c]) <- 0
# }

for (c in grep(".sd", names(firestats_fire.df))){
  is.na(firestats_fire.df[,c]) <- 0
}

# Save grouped data 
write.csv(unlist_cols(firestats_cell.df), paste0(path_save, "firestats_cell.csv"), row.names = F)
# write.csv(unlist_cols(firestats_treatment_all.df), paste0(path_save, "firestats_treatment_all.csv"), row.names = F)
# write.csv(unlist_cols(firestats_treatment_in.df), paste0(path_save, "firestats_treatment_in.csv"), row.names = F)
write.csv(unlist_cols(firestats_fire.df), paste0(path_save, "firestats_fire.csv"), row.names = F)
write.csv(unlist_cols(sens.df), search.files(path_save, c("sensitivity", ".csv", "!means"), "first"), row.names = F)
write.csv(unlist_cols(sens_means.df), search.files(path_save, c("sensitivity", ".csv", "means"), "first"), row.names = F)
write.csv(flux.df, paste0(path_save, "FluxCMeans.csv"), row.names = F)



### RUN PLOTS
# BATCH <- T
# source(paste0(path_scripts, "BATCH_plots_V4KE6.R"))
# source(paste0(path_scripts, "BATCH_rasters_KD.R"))
