# V4KE2 PART 2

if (grepl("MacBook", toString(Sys.getenv()))){
  rm(list = ls()[grepl("RUNINFO", ls()) == F])
  platform <- "local"
} else {
  RUNINFO <- Sys.getenv('RUNINFO')
  platform <- "cluster"
}

tstart <- Sys.time()
RUNINFO <- unlist(strsplit(gsub("'", "", RUNINFO), "___"))
run_schedule <- read.csv(as.character(unlist(RUNINFO[1])))
r <- as.numeric(RUNINFO[2])

for (path in names(run_schedule)[grep("path_", names(run_schedule))]){
  path.temp <- paste0(run_schedule[1,path], "/")
  if (grepl(".csv", path.temp)){
    path.temp <- paste0(run_schedule[1,path])
  }
  assign(path, path.temp)
}

path_slurm <- list.files(path_scripts, full.names = F)[grep(".log", list.files(path_scripts))]
path_slurm <- path_slurm[length(path_slurm)]

KEditsV4 <- T

module_name <- "Init"
source(paste0(path_scripts, "Modules_KD.R"))

path <- gsub(strsplitsubr(path_inputs, "/", 1, 1), "", gsub(strsplitsubr(path_inputs, "/", 1, 2), "", path_inputs))
filesave <- paste0("output_", sprintf("%03d", r), "_", run_schedule$Run_timestamp[1], ".sqlite")
run_schedule$Run_DB <- filesave
path_save_db <- paste0(path_db, filesave)
batch_time <- unique(run_schedule$Run_timestamp)

region <- tolower(run_schedule$Region[r])
run_schedule$Time_ini[r] <- as.character(Sys.time())
run <- run_schedule$Run_ID[r]
n.years <- run_schedule$Years[r]
run_name <- run_schedule$Run_name[r]

# # Initiate log
# if (platform == "cluster"){sink(file = paste0(path_logs, "P2_Run", sprintf("%03d", run), "_log.txt"))}

for (v in names(run_schedule[grep("P_", names(run_schedule))])){
  assign(v, run_schedule[r,v])
}

if (P_snapshot_yrs == F){P_snapshot_yrs <- 9999}
P_snapshot_yrs <- as.numeric(unlist(strsplit(as.character(P_snapshot_yrs), ", ")))
take.snapshot <- (sum((P_snapshot_yrs %in% c(9999, 0)) == F) > 0) #Take a snapshot of a given year to do runs from?
start_from.snapshot <- (P_snapshot_init %in% c(F, 9999, "9999") == F) #Start from a snapshot?

fire.years <- P_fireyrs
fire.size.mult <- P_firesize_mult
fire.freq.mult <- P_firefreq_mult


if (grepl("%", P_climate_yrs)){
  vars <- c("psi", "sm_volum", "min.temp")
  var.name <- vars[unlist(lapply(vars, grepl, P_climate_yrs))]
  var <- stack(paste0(path_inputs, var.name, ".nc"))
  list.var=list()
  for (i in 1:67){ #! HARD CODE
    twixt=as.matrix(var[[i]])
    twixt = rbind(NA, cbind(NA, twixt, NA), NA)
    list.var[[i]]=twixt
  }
  var.mean <- unlist(lapply(list.var, mean, na.rm = T))
  var.rank <- rank(var.mean, ties.method="min")
  var.rank <- round(var.rank/length(var.rank) * 100)
  
  if (grepl(var.name, P_climate_yrs)){
    climate.yr.range <- which(eval(parse(text = gsub(var.name, "var.rank", gsub("%", "", P_climate_yrs)))))
  } else {
    climate.yr.range <- which(eval(parse(text = paste0("var.rank", gsub("#", "", P_climate_yrs)))))
  }
  
} else {
  P_climate_yrs <- unlist(lapply(gsub(" ", "", unlist(strsplit(P_climate_yrs, ","))), as.numeric))
  if (sum(P_climate_yrs >= 1949) == 2){
    P_climate_yrs <- (P_climate_yrs-1949)
  }
  climate.yr.range <- c(P_climate_yrs[1]:P_climate_yrs[2])
}

check_break(path_save)
cat(paste0("\n\n\n\nInitiating run ", r," of ", nrow(run_schedule), " (", run_schedule$Run_name[r], ") at ", Sys.time(), "\n\n\n\n\n"))
source(paste0(path_scripts, "model_10-10-2021_KD.R"))

run_schedule$Time_ini[r] <- as.character(tstart)
run_schedule$Time_fin[r] <- as.character(Sys.time())
run_schedule$Time_mins[r] <- as.numeric(difftime(time1 = Sys.time(), time2 = tstart, units = "mins"))
run_schedule$Complete[r] <- T
write.csv(run_schedule[r,], paste0(path_save, "run_schedule_line_", sprintf("%03d", r), ".csv"), row.names = F)

# if ((platform == "cluster") & (length(path_slurm) > 0)){
#   file.copy(from = path_slurm, to = gsub(path_scripts, path_save, path_slurm))
#   file.remove(path_slurm)
# }

# # Terminate log
# if (platform == "cluster"){sink(file = NULL)}
