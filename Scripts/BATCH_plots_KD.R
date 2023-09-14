# TO DO:
# Sensitivity to area, y = crownkill
rm(list = ls())
BATCH <- T

module_name <- "Init"
path_scripts <- "/Users/MacBook/Desktop/Cellar/Academic/UCSB/Research/LabFees/DYNAFFORE/Model_V4/Scripts/"
source(paste0(path_scripts, "BATCH_module_V4KE4.R"))

experiment_names <- c("Bio", "Area", "KnowAcc")

# path_bio <- "BATCH_2303031504_Bio"
# path_area <- "BATCH_2303031504_Area"
# path_knowacc <- "BATCH_2303031504_KnowAcc"

path_bio <- "BATCH_2303241520_Bio"
path_area <- "BATCH_2303241520_Area"
path_knowacc <- "BATCH_2303241520_KnowAcc"

for (exp.name in tolower(experiment_names)){
  path.temp <- paste0(path_outputs, get(paste0("path_", exp.name)), "/Outputs/")
  path.temp <- search.files(path.temp, "Postprocess", c("is.directory", "last"))
  assign(paste0("path_", exp.name), path.temp)
}

combine_control <- "path_knowacc"
path_control <- get(combine_control)
files.mod <- c("firestats", "FluxCMeans", "sensitivity")
files.copy <- c("PProc_")
TreatmentBase.list <- c("TreatmentBase09", "TreatmentBase1000", "TreatmentBase")

if (combine_control != ""){
  for (exp.name in experiment_names){
    path.temp <- get(paste0("path_", tolower(exp.name)))
    if (path.temp != get(combine_control)){
      path.temp.1 <- paste0(path.temp, "ctl_", gsub("path_", "", combine_control), "/")
      if (dir.exists(path.temp.1) == F){
        
        cat(paste0("\nControl Modification for Experiment: ", exp.name, "\n"))
        dir.create(path.temp.1)
        
        # Copy all relevant files...
        for (name.temp in c(files.copy, files.mod)){
          
          cat(paste0("File = ", name.temp, "\n"))
          
          files.temp <- search.files(path.temp, c(name.temp, "!BACKUP"), "name_only")
          for (file.temp in files.temp){
            file.copy(paste0(path.temp, file.temp), paste0(path.temp.1, file.temp))
          }
        }
      
        # # Substitute control data in files.mod
        # for (name.temp in search.files(path.temp.1, files.mod, c("any_match", "name_only"))){
        #   file.temp <- paste0(path.temp.1, name.temp)
        #   file.master.temp <- paste0(path_control, name.temp)
        #   
        #   df.temp <- read.csv(file.temp)
        #   df.master.temp <- read.csv(file.master.temp)
        #   
        #   ctl.temp <- unique(df.temp$group)[grepl("control", tolower(unique(df.temp$group)))]
        #   ctl.master.temp <- unique(df.master.temp$group)[grepl("control", tolower(unique(df.master.temp$group)))]
        #   
        #   df.temp <- df.temp %>% subset(group != ctl.temp)
        #   df.temp <- rbind(df.temp, df.master.temp %>% subset(group == ctl.master.temp))
        #   
        #   write.csv(df.temp, paste0(path.temp.1, name.temp))
        # }
        
        # Substitute control and treatmentbase data in files.mod
        files.temp <- search.files(path.temp.1, files.mod, c("any_match", "name_only"))
        for (i in c(1:length(files.temp))){
          name.temp <- files.temp[i]
          file.temp <- paste0(path.temp.1, name.temp)
          file.master.temp <- paste0(path_control, name.temp)
          
          df.temp <- read.csv(file.temp)
          df.master.temp <- read.csv(file.master.temp)
          
          ctl.temp <- unique(df.temp$group)[grepl("control", tolower(unique(df.temp$group)))]
          tb.temp <- unique(df.temp$group)[which(tolower(unique(df.temp$group)) %in% tolower(TreatmentBase.list))]
          #tb.temp <- unique(df.temp$group)[grepl(tolower(TreatmentBase), tolower(unique(df.temp$group)))]
          
          ctl.master.temp <- unique(df.master.temp$group)[grepl("control", tolower(unique(df.master.temp$group)))]
          tb.master.temp <- unique(df.master.temp$group)[which(tolower(unique(df.master.temp$group)) %in% tolower(TreatmentBase.list))]
          # tb.master.temp <- unique(df.master.temp$group)[grepl(tolower(TreatmentBase), tolower(unique(df.master.temp$group)))]
          
          df.master.temp.1 <- df.master.temp %>% subset(group %in% c(ctl.master.temp, tb.master.temp))
          df.temp.1 <- df.temp %>% subset((group %in% c(ctl.temp, tb.temp)) == F)
          df.temp.2 <- rbind(df.temp.1, df.master.temp.1)
          
          df.temp.2$group[which(df.temp.2$group == tb.master.temp)] <- tb.temp
          df.temp.2$group[which(df.temp.2$group == ctl.master.temp)] <- ctl.master.temp
          
          # df.temp <- df.temp %>% subset(tolower(group) != tolower(TreatmentBase))
          # df.temp <- rbind(df.temp, tb.master.temp %>% subset(tolower(group) == tolower(TreatmentBase)))
          
          write.csv(df.temp.2, paste0(path.temp.1, name.temp))
        }
        
      }
      assign(paste0("path_", tolower(exp.name)), path.temp.1)
    }
  }
}

for (exp.ix in c(1:length(experiment_names))){
  
  # rm(list =   ls()[(ls() %in% c("path_bio", "path_area", "path_knowacc", "experiment_names", "exp.ix", "BATCH")) == F])
  
  exp.name <- experiment_names[exp.ix]
  path_save <- get(paste0("path_", tolower(exp.name)))
  experiment.name <- exp.name

# experiment.name <- "KnowAcc"
# path_save <- "/Volumes/LaCie/DYNAFFORE/V4/sierra/outputs/BATCH_2301060020_KnowAcc/Outputs/Postprocess-004_2301060020_KnowAcc/" # WITH Hemlock/Cedar
# path_save <- "/Volumes/LaCie/DYNAFFORE/V4/sierra/outputs/BATCH_2302202151_KnowAcc/Outputs/Postprocess-002_2302202151_KnowAcc/" # Resimulation WITH Hemlock/Cedar
# path_save <- "/Volumes/LaCie/DYNAFFORE/V4/sierra/outputs/BATCH_2302211954_KnowAcc/Outputs/Postprocess-002_2302211954_KnowAcc/" # RERUN_20REP_Mods
# path_save <- "/Volumes/LaCie/DYNAFFORE/V4/sierra/outputs/BATCH_2303011905_KnowAcc/Postprocess-001_2303011905_KnowAcc/" # resim 25rep; >50% dbio
# path_save <- "/Volumes/LaCie/DYNAFFORE/V4/sierra/outputs/BATCH_2303031504_KnowAcc/Outputs/Postprocess-002_2303031504_KnowAcc/"  # resim 25rep; >20% dbio

#mexperiment.name <- "Area"
# path_save <- "/Volumes/LaCie/DYNAFFORE/V4/sierra/outputs/BATCH_2301121653_Area/Outputs/Postprocess-002_2301121653_Area/" # Area
# path_save <- "/Volumes/LaCie/DYNAFFORE/V4/sierra/outputs/BATCH_2303011905_Area/Outputs/Postprocess-001_2303011905_Area/" # resim 25rep; >50% dbio
# path_save <- "/Volumes/LaCie/DYNAFFORE/V4/sierra/outputs/BATCH_2303031504_Area/Outputs/Postprocess-001_2303031504_Area/" # resim 25rep; >20% dbio

# experiment.name <- "Bio"
# path_save <- "/Volumes/LaCie/DYNAFFORE/V4/sierra/outputs/BATCH_2302140049_KnowAcc/Outputs/Postprocess-002_2302140049_KnowAcc/" # WITH Hemlock/Cedar
# path_save <- "/Volumes/LaCie/DYNAFFORE/V4/sierra/outputs/BATCH_2303011905_Bio/Outputs/Postprocess-001_2303011905_Bio/" # resim 25rep; >50% dbio
# path_save <- "/Volumes/LaCie/DYNAFFORE/V4/sierra/outputs/BATCH_2303031504_Bio/Outputs/Postprocess-001_2303031504_Bio/" # resim 25rep; >20% dbio

# experiment.name <- "KnowAcc" # ALLOSWAP
# path_save <- "/Volumes/LaCie/DYNAFFORE/V4/sierra/outputs/BATCH_2301171125_KnowAcc/Outputs/Postprocess-002_2301171125_KnowAcc/" # PFT Swap w/ turnover
# path_save <- "/Volumes/LaCie/DYNAFFORE/V4/sierra/outputs/BATCH_2301141349_KnowAcc/Outputs/Postprocess-003_2301141349_KnowAcc/" # PFT Swap w/o turnover

path_PP <- list.files(path_save, full.names = T)[grepl("PProc_run_schedule_", list.files(path_save))]
run_schedule <- read.csv(path_PP)

# BATCH <- T
# experiment.name <- "Area"
# path_save <- "/Volumes/LaCie/DYNAFFORE/V4/sierra/outputs/BATCH_2210011547_Area01/Outputs/Postprocess-001_2210011547_Area01/"

if (exists("run_schedule") == F){
  cat(paste0("\nSelect 'PProc_run_schedule'\n\n"))
  Sys.sleep(.5)
  path_PP <- file.choose()
  run_schedule <- read.csv(path_PP)
  path_PP <- strsplitsubr(path_PP, "/", n, -1)
  path_save <- path_PP
  BATCH <- T
  experiment.name <- unique(run_schedule$Run_handle)
} else {
  #experiment.name <- unique(run_schedule$Run_handle)
}

####################################
##### User Input ###################
####################################



####################################
##### Initialize ###################
####################################

cat(paste0("\n\nINITIATING 'BATCH_plots_V4KE4.R'\n\n\n"))

rm(list = c(ls()[grep("df.prev", ls(), fixed = T)]))
prev <- c(ls()[grep(".df", ls(), fixed = T)])
if (length(prev) > 0){
  for (i in c(1:length(prev))){
    assign(paste0(prev[i], ".prev"), get(prev[i]))
  }
}

if (grepl("MacBook", toString(Sys.getenv()))){
  path_scripts <- "/Users/MacBook/Desktop/Cellar/Academic/UCSB/Research/LabFees/DYNAFFORE/Model_V4/Scripts/"
} else {
  path_scripts <- "/home/kldaum/vdl/DYNAFFOR/Scripts_striker/"
  Control_group <- ""
}

KEditsV4 <- T

if (exists("INIT") == F){
  module_name <- "Init"
  source(paste0(path_scripts, "BATCH_module_V4KE4.R"))
}

# library(devtools)
# devtools::install_github("an-bui/calecopal")
library(calecopal)

if (experiment.name == "KnowAcc"){
  # Control_group <- c("TreatmentBase", "CONTROL")
  Control_group <- c("CONTROL")
  Plot_order <- c("Agency", "Access", "Knowledge", "Synergy")
  Plot_colors <- c("#639e7e", "#ffa600", "#BB4430", "#6edbd8") # Alt green = #79a15f; Alt yellow = #ebcf7a
  # Plot_colors <- c("darkseagreen1", "goldenrod", "darkred", "cyan")
  # Plot_colors <- c("#7a5195", "#ffa600", "#ef5675", "#003f5c")
  CONTROL_solid <- T # Should group named 'control' always use a solid line? Else, active Control_group uses...
}

if (experiment.name == "Area"){
  Control_group <- c("CONTROL")
  Plot_order <- c("Area", "Control")
  Plot_colors <- "gradient"
  color_low <- "red"
  color_mid <- "magenta"
  color_high <- "blue"
  CONTROL_solid <- T # Should group named 'control' always use a solid line? Else, active Control_group uses...
}

if (experiment.name == "Bio"){
  Control_group <- c("CONTROL")
  Plot_order <- c("Bio", "Control")
  Plot_colors <- "gradient"
  color_low <- "orangered3"
  color_mid <- "goldenrod"
  color_high <- "darkseagreen2"
  CONTROL_solid <- T # Should group named 'control' always use a solid line? Else, active Control_group uses...
}

if (Sys.getenv('RUNINFOx') != ""){
  
  BATCHx <- "PLOTS"
  source(paste0(path_scripts, "BATCH_submitx.R"))

} else {
  
  if (exists("BATCH")){
    if (BATCH == F){
      cat("\n\nSelect any file in 'PostProcess' directory of TEST / BATCH data\n\n")
      Sys.sleep(1)
      path_PP <- paste0(file.choose()) # Select any file in directory "PostProcess" directory
      path_PP <- paste0(paste(unlist(strsplit(path_PP, "/"))[c(1:length(unlist(strsplit(path_PP, "/")))-1)], collapse = "/"), "/")
    } else {
      path_PP <- path_save
    }
  } else {
    cat("\n\nSelect any file in 'PostProcess' directory of TEST / BATCH data\n\n")
    Sys.sleep(1)
    path_PP <- paste0(file.choose()) # Select any file in directory "PostProcess" directory
    path_PP <- paste0(paste(unlist(strsplit(path_PP, "/"))[c(1:length(unlist(strsplit(path_PP, "/")))-1)], collapse = "/"), "/")
  }
  
}


path_batch <- strsplitsubr(path_PP, "/", "x", -(which(rev(grepl("Outputs", unlist(strsplit(path_PP, "/")))))-1))
name_batch <- gsub("/", "", strsplitsubr(path_PP, "/", 0, -3))
# name_batch <- strsplitsubr(path_PP, "/", 1, 3)

path_rasters <- paste0(path_batch, "Rasters/")
path_plots <- paste0(path_PP, "Plots_", length(search.files(path_PP, "Plots_", "is.directory"))+1, "/")
dir.create(path_plots)





####################################
##### Load Data ####################
####################################

PProc_schedule <- read.csv(search.files(c(path_PP), c("PProc_", "_schedule"), "last"))

# Compile run schedules
params.df <- read.csv(search.files(path_batch, c("Params", "BATCH", "!FULL"), ""))
run_schedule <- read.csv(search.files(path_PP, c("PProc_run_schedule"), ""))

# Compile sensitivity analyses
sens.df <- read.csv(search.files(path_PP, c("sensitivity", "!means", "!BACKUP"), ""))

# Compile mean sensitivity analyses
sens_means.df <- read.csv(search.files(path_PP, c("sensitivity", "means", "!BACKUP"), ""))

#
years.spin <- gen_spinup(run_schedule)

# Compile fire statistics
#if (file.exists(paste0(path_PP, "firestats_cell.csv"))){
#if (exists("firestats_cell.df") == F){
  
  firestats_cell.df <- read.csv(paste0(path_PP, "firestats_cell.csv"))
  firestats_treatment_all.df <- read.csv(paste0(path_PP, "firestats_treatment_all.csv"))
  firestats_treatment_in.df <- read.csv(paste0(path_PP, "firestats_treatment_in.csv"))
  firestats_fire.df <- read.csv(paste0(path_PP, "firestats_fire.csv"))
  
#}

# If available, load treatment log
if (file.exists(paste0(path_batch, "/TreatLog.csv"))){
  treatlog.df <- read.csv(paste0(path_batch, "TreatLog.csv"))
  treatlog.df <- cbind(strsplit_to_cols(treatlog.df$Run_name, "_", c("treatment_type", "treatment_val", "treatment_rep")), treatlog.df)
  treatlog.df$group <- treatlog.df$treatment_val  
}

# Calculate spin up period
years.spinup <- gen_spinup(run_schedule)
df.names <- c("sens.df", "sens_means.df", "firestats_cell.df", "firestats_treatment_all.df", "firestats_treatment_in.df", "firestats_fire.df")
for (df.name in df.names){
  df.temp <- get(df.name)
  if (("group" %in% names(df.temp))==F){
    df.temp$group <- df.temp$treatment_val
  }
  grp.ix <- match(df.temp$group, years.spinup$Group)
  df.temp$spin_up <- df.temp$year < years.spinup$Spin[grp.ix]
  assign(df.name, df.temp)
}

# Tidy paths
for (path.temp in ls()[grep("path_", ls())]){
  if (is.null(get(path.temp)) == F){
    assign(path.temp, monoslash(get(path.temp)))
  }
}

# Compile fluxC
flux.df <- read.csv(paste0(path_PP, "FluxCMeans.csv"))

# # Rasters
# if (length(search.files(path_rasters, ".pdf", "")) < 3){
#   source(paste0(path_scripts, "BATCH_rasters_V4KE4.R"))
#   Sys.sleep(30)
# }

# for (file in search.files(path_rasters, ".pdf", "")){
#   file.copy(file, gsub(path_rasters, path_plots, file))
# }

#! TEMPORARY COMPATIBILITY FIX, ENSURE THAT POSTPROCESS IS DOING THIS WORK
nondiagnostic <- c("!", "year", "group", "treatment_val", "spin_up", "index_1", "runid", "treatment_type",
                   "treatment", "filename", "treatment_rep", "directory", "pft", "pft_name", "batch", 
                   "run_id", "run_rep", "fire_id", "cell", "cdf_lookup", "cdf_size", "mtbs_id", "sample_n", "cld", ".sd")



# for (ctl.temp in Control_group){
  ctl.temp <- Control_group #!#!
  
  path_plots.1 <- gsub("*", "", paste0("Ctl_", ctl.temp))
  ct.temp <- sprintf("%02d", sum(grepl(path_plots.1, list.files(path_plots)))+1)
  path_plots.1 <- paste0(path_plots, path_plots.1, "_", ct.temp, "/")
  dir.create(path_plots.1)
  
  ## Substitute Control if defined by User Input
  grouped_dataframes <- ls()[grep(".df", ls(), fixed = T)]
  
  if ((ctl.temp != "") & (as.character(ctl.temp) != "9999")){
    ctl.temp <- paste0("*", gsub("*", "", ctl.temp, fixed = T))
  } else {
    ctl.temp <- "*CONTROL"
  }
  
  for (df.name in grouped_dataframes){
    df.temp <- get(df.name)
    if (("group" %in% names(df.temp)) == F){
      grouped_dataframes <- grouped_dataframes[which(grouped_dataframes != df.name)]
    } else {
      if ((grepl("CONTROL", toupper(ctl.temp))) == F){
        df.temp[grepl(toupper(gsub("*", "", ctl.temp, fixed = T)), toupper(df.temp$group)), "group"] <- ctl.temp
        df.temp[grepl("CONTROL", toupper(df.temp$group)), "group"] <- "CONTROL"
        assign(df.name, df.temp) #!#!
      } else {
        df.temp$group <- gsub("*", "", df.temp$group, fixed = T)
        df.temp[grepl("CONTROL", toupper(df.temp$group)), "group"] <- "*CONTROL"
        assign(df.name, df.temp)
      }
    }
  }
  
  
  ####################################
  ##### Prep for Plotting ############
  ####################################
  
  alphabet_upper <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z")
  alphabet_lower <- tolower(alphabet_upper)
  alphabet_lower.per <- unlist(lapply("(", paste0, lapply(as.list(alphabet_lower), paste0, ")")))
  alphabet_upper.per <- unlist(lapply("(", paste0, lapply(as.list(alphabet_upper), paste0, ")")))
  
  ctl.temp
  
  group <- unique(sens.df$group)
  plot_guide <- cbind(group, suppressWarnings(data.frame(column = group) %>% 
                                                 separate(column, 
                                                          into = c("text", "num"), 
                                                          sep = "(?<=[A-Za-z])(?=[0-9])")))
  plot_guide$control <- grepl(toupper(gsub("*", "", ctl.temp, fixed = T)), toupper(plot_guide$text), fixed = T)
  plot_guide$num[is.na(plot_guide$num)] <- 0
  
  # if (sum(plot_guide$num) > 0){
  #   plot_guide <- plot_guide[order(as.character(plot_guide$num)),]
  # } else {
    plot_guide <- plot_guide[order(plot_guide$group),]
  # }

  
  if (experiment.name == "KnowAcc"){
    grades <- c("High", "Med", "Low")
    grade_lines <- c("solid", "longdash", "dotted")
    grade_lines.ix <- c(1, 2, 3)
    grade_shapes.ix <- c(16, 1, 10)
    grade <- lapply(grades, grep, plot_guide$group)
    grades <- grades[unlist(lapply(grade, length)) > 0]
    grade <- grade[unlist(lapply(grade, length)) > 0]
    grade_lines <- grade_lines[unlist(lapply(grade, length)) > 0]
    grade_lines.ix <- grade_lines.ix[unlist(lapply(grade, length)) > 0]
    grade_shapes.ix <- grade_shapes.ix[unlist(lapply(grade, length)) > 0]
    
    plot_guide$grade <- "High"
    plot_guide$text.sub <- plot_guide$text
    plot_guide$linetype <- "soild"
    plot_guide$linetype.ix <- 1
    plot_guide$shape.ix <- 1
    for (g in which(unlist(lapply(grade, length)) > 0)){
      plot_guide$grade[grade[[g]]] <- grades[g]
      plot_guide$text.sub[grade[[g]]] <- gsub(grades[g], "", plot_guide$text[grade[[g]]])
      plot_guide$linetype[grade[[g]]] <- grade_lines[g]
      plot_guide$linetype.ix[grade[[g]]] <- grade_lines.ix[g]
      plot_guide$shape.ix[grade[[g]]] <- grade_shapes.ix[g]
    }
  } else {
    grades <- "Base"
    grade_lines <- "solid"
    plot_guide$grade <- grades
    plot_guide$text.sub <- paste0(plot_guide$text) #, plot_guide$num)
    plot_guide$linetype <- "soild"
    plot_guide$linetype.ix <- 1
    plot_guide$shape.ix <- 16
  }
  
  plot_guide$order <- NA
  if (experiment.name == "Area"){
    plot_guide <- plot_guide[order(as.numeric(plot_guide$num)),]
    row.names(plot_guide) <- c(1:nrow(plot_guide))
    plot_guide$order <- c(1:nrow(plot_guide))
  } else {
    if (toString(Plot_order) == ""){
      plot_guide[which(plot_guide$control == T),"order"] <- order(plot_guide[which(plot_guide$control == T),"text"])
      if (sum(c("High", "Low") %in% plot_guide$grade) > 1){
        ix.temp <- c(which(plot_guide$control == T),
                     which((plot_guide$control == F) & (plot_guide$grade == "Low") & (plot_guide$group %in% TreatmentBase.list == F)),
                     which(plot_guide$group %in% TreatmentBase.list == T),
                     which((plot_guide$control == F) & (plot_guide$grade == "High") & (plot_guide$group %in% TreatmentBase.list == F)))
        plot_guide <- plot_guide[ix.temp,]
        plot_guide[,"order"] <- c(1:nrow(plot_guide))
      } else {
        plot_guide[which(plot_guide$control == F),"order"] <- order(plot_guide[which(plot_guide$control == F),"text"])+sum(plot_guide$control)
      }
    } else {
      if (sum(c("High", "Low") %in% plot_guide$grade) > 1){
        ix.temp <- c(which(plot_guide$control == T), 
                     which(plot_guide$group %in% TreatmentBase.list == T), 
                     unlist(lapply(lapply(Plot_order, grepl, plot_guide$group), which)))
        plot_guide <- plot_guide[ix.temp,]
        ix.temp <- c(which(plot_guide$control == T),
                     which((plot_guide$control == F) & (plot_guide$grade == "Low") & (plot_guide$group %in% TreatmentBase.list == F)),
                     which(plot_guide$group %in% TreatmentBase.list == T),
                     which((plot_guide$control == F) & (plot_guide$grade == "High") & (plot_guide$group %in% TreatmentBase.list == F)))
        plot_guide <- plot_guide[ix.temp,]
        plot_guide[,"order"] <- c(1:nrow(plot_guide))
      } else {
        plot_guide[which(plot_guide$control == T),"order"] <- order(plot_guide[which(plot_guide$control == T),"text"])
        plot_guide[which(plot_guide$control == F),"order"] <- match(plot_guide$text.sub[which(plot_guide$control == F)], Plot_order) + sum(plot_guide$control)
        plot_guide <- plot_guide[order(plot_guide$order),]
        plot_guide$order <- c(1:nrow(plot_guide))
      }
    }
  }
  plot_guide$order.alp <- alphabet_upper[plot_guide$order]
  row.names(plot_guide) <- c(1:nrow(plot_guide))
  
  if (toString(Plot_colors) == "gradient"){
    # If continuous
    # color_low <- "red"
    # color_high <- "blue"
    # color_mid <- "magenta"
    ctl.ct <- sum(plot_guide$control)
    pal <- colorRampPalette(c(color_low, color_high))
    color <- c(rep("black", ctl.ct), pal(length(plot_guide$group)-ctl.ct))
    plot_guide$color <- color
  } else {
    plot_guide$color <- "black"
    ix.temp <- match(plot_guide$text.sub[which(plot_guide$control == F)], Plot_order)
    ix.temp <- ix.temp[which(!is.na(ix.temp))]
    colors.temp <- Plot_colors[ix.temp]
    ix.temp <- which(plot_guide$text.sub %in% Plot_order)
    plot_guide$color[ix.temp] <- colors.temp
    #plot_guide$color <- tolower(c(rep("black", sum(plot_guide$control)), Plot_colors[ix.temp]))
  }
  plot_guide$fill <- plot_guide$color
  plot_guide$display <- T
  
  plot_guide$plot_group <- 1
  ix.temp <- which(plot_guide$text %in% c(ctl.temp, TreatmentBase.list) == F)
  plot_guide$plot_group[ix.temp] <- match(plot_guide$text.sub[ix.temp], unique(plot_guide$text.sub[ix.temp]))+1
  
  # vec.temp <- which(plot_guide$plot_group == which(as.vector(table(plot_guide$plot_group) == 1)))
  # vec.temp <- vec.temp[order(plot_guide$text.sub[vec.temp])]
  # palbw <- colorRampPalette(c("black", "grey80"))
  # plot_guide$color[vec.temp] <- "black" #palbw(length(vec.temp))
  # plot_guide$fill[vec.temp] <- palbw(length(vec.temp))
  # if ((sum(grepl("CONTROL", toupper(plot_guide$group[vec.temp]))) > 0) & (CONTROL_solid == T)){
  #   vec.temp <- vec.temp[c(grep("CONTROL", toupper(plot_guide$group[vec.temp])), which(!grepl("CONTROL", toupper(plot_guide$group[vec.temp]))))]
  # }
  # if (length(vec.temp) <= length(grade_lines)){
  #   plot_guide$linetype[vec.temp] <- grade_lines[c(1:length(vec.temp))]
  #   plot_guide$linetype.ix[vec.temp] <- c(1:length(vec.temp))
  # }
  
  # plot_guide <- plot_guide[c(vec.temp, c(1:nrow(plot_guide))[which(c(1:nrow(plot_guide)) %in% vec.temp == F)]),]
  # plot_guide$order <- c(1:nrow(plot_guide))
  # plot_guide$order.alp <- alphabet_upper[plot_guide$order]
  # row.names(plot_guide) <- c(1:nrow(plot_guide))
  
  # # Modify control
  # if (sum(plot_guide$control) > 1){
  #   plot_guide$linetype[1] <- "dashed"
  #   plot_guide$linetype.ix[1] <- 2
  #   plot_guide$shape.ix[1] <- 1
  #   plot_guide$shape <- 1
  # }
  
  # Modify base treatment line
  ix.temp <- which(TreatmentBase.list %in% plot_guide$group)
  if (length(ix.temp > 0)){
    tb.temp <- TreatmentBase.list[ix.temp]
    ix.temp <- which(plot_guide$group == tb.temp)
    plot_guide$linetype[ix.temp] <- "longdash"
    plot_guide$linetype.ix[ix.temp] <- 2
    plot_guide$color[ix.temp] <- "black"
    plot_guide$fill[ix.temp] <- "#000000"
  }
  
  #!#! Workaround, lazy
  grade_shapes.ix <- c(16, 1, 10)
  plot_guide$shape.ix <- grade_shapes.ix[plot_guide$linetype.ix]
  
  plot_guide$pattern <- c("none", "stripe", "circle")[plot_guide$linetype.ix]
  plot_guide$Treatment <- plot_guide$group
  plot_guide$Scenario <- plot_guide$group
  
  shape <- plot_guide$shape
  pattern <- plot_guide$pattern
  linetype <- plot_guide$linetype
  color <- plot_guide$color
  groups <- plot_guide$group
  grp.ord <- groups
  grp.ix <- which(plot_guide$display == T)
  
  # Last minute edits...
  firestats_cell.df$pft_transition <- F
  firestats_cell.df$pft_transition[which((firestats_cell.df$pft_post != firestats_cell.df$pft) & (firestats_cell.df$pft_post != 0))] <- T
  
  
  domain_area <- sum(!is.na(list.blank[[1]]))
  total_pop <- sum(pop.ras, na.rm = T)
  total_pop.affected <- sum(firestats_cell.df$population[duplicated(firestats_cell.df$cell) == F])
  crown_kill.col <- firestats_cell.df$crown_kill.perc
  population.col <- firestats_cell.df$population
  firestats_cell.df$pop.ck50 <- firestats_cell.df$population
  firestats_cell.df$pop.ck50[firestats_cell.df$crown_kill.perc < .5] <- 0
  firestats_cell.df$pop.ck80 <- firestats_cell.df$population
  firestats_cell.df$pop.ck80[firestats_cell.df$crown_kill.perc < .8] <- 0
  firestats_cell.df$pop.ck90 <- firestats_cell.df$population
  firestats_cell.df$pop.ck90[firestats_cell.df$crown_kill.perc < .9] <- 0
  
  # firestats_cell.df$pop.ck <- (1/domain_area)*(population.col/total_pop)*crown_kill.col
  # firestats_cell.df$pop.ck <- crown_kill.col*(population.col/total_pop.affected)
  
  # df.temp <- firestats_cell.df %>% subset((population > 0) & (year > 0)) %>% dplyr::select(year, treatment_rep, group, population, crown_kill.perc)
  # sevpop.df <- df.temp %>% group_by(year, group) %>% summarize(popsev = mean(rep(crown_kill.prec, population)))
  # sevpop.df$sevpop <- 0
  # for (r in c(1:nrow(sevpop.df))){
  #   df.temp.1 <- df.temp %>% subset(year == sevpop.df$year[r])
  #   vec.temp <- NULL
  #   for (i in c(1:nrow(df.temp))){
  #     
  #   }
  #   df.temp[rep(r, df.temp$population[r]), c("crown_kill.perc")]
  #   sevpop.df$sevpop[r] <- df.temp[rep(r, df.temp$population[r]), c("year", "treatment_rep", "group", "crown_kill.perc")])
  # }
  
  # sevpop.df <- NULL
  # for (r in c(1:nrow(df.temp))){
  #   sevpop.df <- rbind(sevpop.df, df.temp[rep(r, df.temp$population[r]), c("year", "treatment_rep", "group", "crown_kill.perc")])
  # }
  
  ########################################################################
  ########################################################################
  ####################################### GENERATE PLOTS #################
  ########################################################################
  ########################################################################
  
  library(gtable)
  library(gridExtra)
  library(ggpattern)
  library(patchwork)
  library(multcompView)
  library(ggnewscale)
  
  years_plot <- list(c(1, 25), c(25, 50), c(50, 100), c(75, 100))
  xlims <- range(unlist(years_plot))
  
  # if ((experiment.name == "Area") & (nrow(plot_guide) > 6)){
  #   for (df.name in c("plot_guide", "firestats_cell.df", "firestats_fire.df", "sens.df", "sens_means.df", "treatlog.df")){
  #     df.temp <- get(df.name)
  #     if (("group" %in% names(df.temp)) == F){
  #       df.temp$group <- df.temp$treatment_val
  #     }
  #     df.temp <- df.temp %>% subset(group %in% groups[c(1, c(3:length(groups))[which(c(2:(length(groups))) %% 2 == 1)])])
  #     assign(df.name, df.temp)
  #   }
  # }
  
  
  
  if (1 == 1){
    
    rep.ct <- 1
    
    # df name
    df.name <-  c(
      
      rep(c(
      "firestats_cell.df",
      "firestats_cell.df",
      "data.frame(sens.df %>% subset(pft == 0) %>% group_by(year, treatment_rep, treatment_val, group) %>% summarize(cov.diff = sum(!is.na(list.blank[[1]])) - coverage.mean))",
      "firestats_cell.df",
      "firestats_fire.df",
      "firestats_fire.df",
      "firestats_cell.df",
      "sens.df"
      ), rep.ct)

                  )
    
    # column name
    value.name <- c(
      
      rep(c(
      "stand_kill",
      "crown_kill.perc",
      "cov.diff",
      "combusted_lbio",
      "size_km",
      "size_km",
      "stand_kill",
      "biomass.mean"
      ), rep.ct)
      
                    )
    
    # ts1 plotted value from ts.df ("diff", "cumu", "cumudiff)
    transformation <- c(
      "diff",
      "diff",
      "diff",
      "diff",
      "cumudiff",
      "cumudiff",
      "cumudiff",
      "diff"
                        )
    
    # Smoothing ("none", "linear", "loess")
    interp <- "loess"
    
    #  Moving window size (ToE)
    ToE_window <- c(
      10
      )
    
    # Precentiles for plotted ribbon
    percentiles.ribbon <- c(
      
      rep("25, 75", (length(df.name)/rep.ct))
      
    )
    
    # percentiles for ToE threshold
    percentiles.ToE <- c(
      
      rep("25, 75", (length(df.name)/rep.ct))
      
                          )
    
    # conversion
    conversion_fac <- c(
      
      rep(c(
      "1",
      "0.01",
      "1",
      "0.001/2",
      "1",
      "1",
      "1",
      "0.001/2"
      ), rep.ct)
                        )
    
    # annual_sum_or_mean
    annual_sum_or_mean <- c(
      
      rep(c(
      "sum", 
      "mean", 
      "mean", 
      "sum",
      "mean",
      "sum",
      "sum",
      "sum"
      ), rep.ct)
                            )
    
    # y label box
    ylab.bx1 <- c(
      
      rep(c(
      "Stand-Replacement Events\n(% difference)",
      "Fire Severity\n(% difference)",
      "Forest Coverage\n(% difference)",
      "Live C Loss\n(% difference)",
      "Mean Fire Size\n(% difference)",
      "Cumulative Burned Area\n(% difference)",
      "Stand-Replacement Events\n(% difference)",
      "Standing Live C \n(% difference)"
      ), rep.ct)
      
                  )
    
    # y axis box
    yax.bx1 <- c(

      "c('', 'none', 'none', '')"
      
    )
    
    # y label time series (where transformation, insert "TRANSFORMATION")
    ylab.ts1 <- c(

      rep(c(
      "Stand-Replacement Events\n(annual TRANSFORMATION, km^2)",
      "Average Fire Severity\n(annual TRANSFORMATION)",
      "Total Forest Coverage\n(TRANSFORMATION km^2)",
      "Live C Loss\n(annual TRANSFORMATION, MT C)",
      "Mean Fire Size\n(TRANSFORMATION km^2)",
      "Cumulative Burned Area\n(TRANSFORMATION km^2)",
      "Stand-Replacement Events\n(TRANSFORMATION, km^2)",
      "Standing Live C\n(TRANSFORMATION, MT C)"
      ), rep.ct)
      
                  )
    
    # box plot type
    abs_or_redux.bx <- c("abs")
    
    # time series type
    abs_or_redux.ts <- c("abs")
    
    # Legend type box plot
    legend_type <- c("list(c('none', 'none', 'none', 'none'), 'none', 'none')")

  }
  
  plots.deck.names <- c("df.name", 
                        "value.name", 
                        "transformation",
                        "interp",
                        "ToE_window",
                        "percentiles.ToE",
                        "percentiles.ribbon",
                        "conversion_fac", 
                        "annual_sum_or_mean", 
                        "ylab.bx1", 
                        "yax.bx1",
                        "ylab.ts1",
                        "abs_or_redux.bx", 
                        "abs_or_redux.ts", 
                        "legend_type")
  
  plots.deck <- data.frame(bind_cols(lapply(plots.deck.names, get)))
  names(plots.deck) <- plots.deck.names

  # PRINT.ix <- c(0, c(1:4))
  # 
  # plots.deck <- plots.deck[PRINT.ix,]
  
  # W <- 7
  # H <- 4.5
  size.ax <- 10
  #graphics.off()
  #ct <- sprintf("%03d", (length(search.files(path_plots.1, "FPLOT_COMBINED", ""))+1))
  #pdf(paste0(path_plots.1, "/FPLOT_COMBINED1.pdf"), onefile = TRUE, width = W, height = H)
  #plots.deck <- plots.deck[c(1:4),]
  for (r in c(0:nrow(plots.deck))){
    if (r == 0){
      maxvals.df <- NULL
      # grid.table(plots.deck[,c(1:3)], theme=ttheme_minimal(base_size = 4))
      write.csv(plots.deck, paste0(path_plots.1, "plots_deck.csv"))
    } else {
      for (n in c(1:ncol(plots.deck))){
        assign(names(plots.deck)[n], plots.deck[r,n])
      }
      
      var.name <- "value.perc_median"
      # var.name <- "value.mean"
      y_ribbon.plot <- c("value.perc_low", "value.perc_high")
      
      if ((ToE_window == "") | (ToE_window == "none")){
        ToE_window <- NULL
      } else {
        ToE_window <- as.numeric(ToE_window)
      }
      
      if (grepl("TRANSFORMATION", ylab.ts1)){
        transformation.name <- c("difference", "cumulative", "cumulative difference")[which(c("diff", "cumu", "cumudiff") == transformation)]
        ylab.ts1 <- gsub("TRANSFORMATION", transformation.name, ylab.ts1)
      }
      
      df.name <- plots.deck$df.name[r]
      y_label.plot <- plots.deck$ylab.ts1[r]
      xlims <- xlims
      
      if (grepl(",", legend_type, fixed = T)){
        legend_type <- eval(parse(text = legend_type))
        if (class(legend_type) == "list"){
          legend.bx <- legend_type[[1]]
          legend.ts1 <- legend_type[[2]]
          legend.ts2 <- legend_type[[3]]
        } else {
          legend.bx <- legend_type[1]
          legend.ts1 <- legend_type[2]
          legend.ts2 <- legend_type[3]
        }
      } else {
        legend.bx <- "none"
        legend.ts1 <- legend_type
        legend.ts2 <- "none"
      }
      
      absredux.bx.temp <- plots.deck$abs_or_redux.bx[r]
      absredux.bx.sign <- c(-1, 1)[(absredux.bx.temp == "abs")+1]
      absredux.bx.char <- gsub("1", "", as.character(absredux.bx.sign))
      
      absredux.ts.temp <- plots.deck$abs_or_redux.ts[r]
      absredux.ts.sign <- c(-1, 1)[(absredux.ts.temp == "abs")+1]
      absredux.ts.char <- gsub("1", "", as.character(absredux.ts.sign))
        
      diff_mean_med <- "median"
      ts.df <- calc_ts2(df.name = df.name, 
                       value.name = value.name, 
                       diff_mean_med = diff_mean_med,
                       conversion_fac = conversion_fac,
                       transformation = transformation,
                       annual_sum_or_mean = annual_sum_or_mean,
                       xlims = xlims,
                       percentiles.ribbon = percentiles.ribbon,
                       percentiles.ToE = percentiles.ToE,
                       interp = interp)
      
      trendline <- names(ts.df)[grepl(diff_mean_med, names(ts.df))][1]
      method <- 2
      bounds.ToE <- c("value.ToE_low", "value.ToE_high")
      ts.df <-  calc_ToE(ts.df = ts.df,
                         trendline = trendline,
                         bounds.ToE = bounds.ToE,
                         ToE_window = ToE_window,
                         method = method)
      
      assign(paste0("ts_", sprintf("%02d", r), ".df."), ts.df)
      
      yrs.plt <- years_plot
      y_label.plot <- ylab.bx1
      plot_abs_or_redux <- absredux.bx.temp
      legend_type <- legend.bx
      y_axis.bx <- yax.bx1
      bx.df <- calc_bx(df.name = df.name,
                       value.name = value.name,
                       conversion_fac = conversion_fac,
                       annualize = T,
                       annual_sum_or_mean = annual_sum_or_mean,
                       diff_mean_med = diff_mean_med,
                       yrs.plt = years_plot,
                       y_axis.bx = y_axis.bx,
                       y_label.plot = ylab.bx1,
                       plot_abs_or_redux = absredux.bx.temp,
                       legend_type = legend.bx)
      
      ## I don't know why I can't get the y axis to adjust but his is my kitchen sink approach
      ylims <- as.list(vector("numeric", 2))
      nudge <- as.list(vector("numeric", 2))
      for (i in c(1)){
        value.name <- value.name[i]
        col.sign <- absredux.ts.sign
        value.name <- gsub("-", "", value.name)
        ylims[[i]] <- range(ts.df[,var.name]*absredux.ts.sign)
      }
      
      for (i in c(1:length(ylims))){
        y <- unlist(ylims[i])
        for (j in c(1:2)){
          y.t <- y[j]
          if (j == 2){y.t <-  y.t + sum(abs(y[1]), abs(y.t))*0.2}
          y.sig <- sign(y.t)
          y.t <- abs(y.t)
          if (grepl(".", y.t, fixed = T)){
            y.place <- 10^(nchar(strsplit(as.character(y.t), ".", fixed = T)[[1]][1])-1)
          } else {
            y.place <- 10^(nchar(as.character(y.t))-1)
          }
          y.t.1 <- plyr::round_any(y.t, y.place, f = ceiling)*y.sig
          y[j] <- y.t.1
        }
        nudge[[i]] <- sum(abs(y))*0.1
        ylims[[i]] <- y
      }
      
      bx1 <- bx.df[[2]][[1]] +  theme(text=element_text(size = size.ax),
                                      plot.title = element_text(hjust = 0.5)) +
                                      labs(title = paste0(paste(unlist(years_plot[1]), collapse ="-"), " yrs"))
      assign(paste0("BX1_r", r), bx1)
      
      bx2 <- bx.df[[2]][[2]] +  theme(text=element_text(size = size.ax),
                                      plot.title = element_text(hjust = 0.5)) + 
                                      labs(title = paste0(paste(unlist(years_plot[2]), collapse ="-"), " yrs"))
      assign(paste0("BX2_r", r), bx2)
      
      bx3 <- bx.df[[2]][[3]] +  theme(text=element_text(size = size.ax),
                                      plot.title = element_text(hjust = 0.5)) + 
                                      labs(title = paste0(paste(unlist(years_plot[3]), collapse ="-"), " yrs"))
      assign(paste0("BX3_r", r), bx3)
      
      
      bx4 <- bx.df[[2]][[4]] +  theme(text=element_text(size = size.ax),
                                      plot.title = element_text(hjust = 0.5)) + 
        labs(title = paste0(paste(unlist(years_plot[4]), collapse ="-"), " years (mean)"))
      assign(paste0("BX4_r", r), bx4)
      
                     
     ### Time series 1
     legend <- legend.ts1
     y_label.plot <- ylab.ts1
     ts1 <-  plot_ts(ts.df = ts.df,
                     plot_guide = plot_guide,
                     y_label.plot = y_label.plot,
                     y_ribbon.plot = y_ribbon.plot, 
                     var.name = var.name,
                     legend = legend) #,
                     #ToE_window = ToE_window)
     assign(paste0("TS1_r", r), ts1)
     
     trend <- which(abs(layer_scales(ts1)$y$range$range) == max(abs(layer_scales(ts1)$y$range$range)))
     lab.x <- xlims[1]+(xlims[2]*0.04)
     lab.y <- layer_scales(ts1)$y$range$range[trend]+(nudge[[1]]*(c(1, -1)[trend]))
     lab <- alphabet_lower.per[length(years_plot)+1]
     
      # multiplot <- ((bx1 | bx2 | bx3) / ts1) + 
      #   plot_annotation(tag_prefix = '(', tag_levels = 'a', tag_suffix = ')') &
      #   theme(plot.tag = element_text(face = 'bold', size = size.ax),
      #         text = element_text(size = size.ax))

      # print(multiplot)
      
    }
  }
  # while (!is.null(dev.list()))  dev.off()
  
  for (p in c(1:(nrow(plots.deck)/4))){
  
    ix.temp <- 1:4
    cat(paste0("\n", toString(ix.temp), "\n"))
    
    for (i in c(1:4)){
      
      assign(paste0("TS", i, ".temp"), get(paste0("TS1_r", ix.temp[i])))
      
      ct <- gsub("_0", "", paste0("_", cumsum(grepl(plots.deck$value.name[i], plots.deck$value.name))[i]))
      name.temp <- paste0("TS_", plots.deck$value.name[i], ct, ".jpg")

      ggsave(plot = get(paste0("TS", i, ".temp")),
             filename = paste0(path_plots.1, name.temp),
             width = 7,
             height = 3)
      
      assign(paste0("BX", i, ".temp"), get(paste0("BX4_r", ix.temp[i])))
      
    }
    
    ggsave(filename = paste0(path_plots.1, "TS", sprintf("%02d", p), ".jpg"),
           plot = ((  TS1.temp + (BX1.temp + labs(title = "")) +
                      TS2.temp + (BX2.temp + labs(title = "")) +
                      TS3.temp + (BX3.temp + labs(title = "")) +
                      TS4.temp + (BX4.temp + labs(title = "")))  +
                     plot_layout(widths = c(3, 1)) +
                     plot_annotation(paste0("Experiment = ", exp.name, "\n\n"), 
                                     caption = paste0("\n\n",
                                                      "Ribbon = ", gsub(", ", " -> ", unique(plots.deck[ix.temp, "percentiles.ribbon"])), "\n",
                                                      "ToE Window = ", gsub(", ", " -> ", unique(plots.deck[ix.temp, "ToE_window"])), "\n",
                                                      "ToE Threshold = ", gsub(", ", " -> ", unique(plots.deck[ix.temp, "percentiles.ToE"]))), 
                                     theme = theme(plot.title=element_text(hjust=0.5))) + #?
                     plot_annotation(tag_prefix = '(', tag_levels = 'a', tag_suffix = ')  ') &
                     theme(plot.tag = element_text(face = 'bold', size = size.ax),
                           text = element_text(size = size.ax))),
           width = 10,
           height = 13)
  
  }
  
#   
#   
#   
#   # MANUAL SINGLE
#   
#   ggsave(filename = paste0(path_plots.1, "TS2", "_MeanFireSize", ".jpg"),
#          plot = ((TS1_r5 + labs(title = "100 Year Time Series") +
#                   BX4_r5) +
#                   plot_layout(widths = c(3, 1)) +
#                   plot_annotation(tag_prefix = '(', tag_levels = 'a', tag_suffix = ')  ') &
#                   theme(plot.tag = element_text(face = 'bold', size = size.ax),
#                         text = element_text(size = size.ax))),
#                 width = 10,
#                 height = 3)
#   
#   ggsave(filename = paste0(path_plots.1, "TS2", "_CumulativeBurnedArea", ".jpg"),
#          plot = ((TS1_r6 + labs(title = "100 Year Time Series") +
#                     BX4_r6) +
#                    plot_layout(widths = c(3, 1)) +
#                    plot_annotation(tag_prefix = '(', tag_levels = 'a', tag_suffix = ')  ') &
#                    theme(plot.tag = element_text(face = 'bold', size = size.ax),
#                          text = element_text(size = size.ax))),
#          width = 10,
#          height = 3)
#   
#   ggsave(filename = paste0(path_plots.1, "TS2", "_CumulativeStandReplacement", ".jpg"),
#          plot = ((TS1_r7 + labs(title = "100 Year Time Series") +
#                     BX4_r7) +
#                    plot_layout(widths = c(3, 1)) +
#                    plot_annotation(tag_prefix = '(', tag_levels = 'a', tag_suffix = ')  ') &
#                    theme(plot.tag = element_text(face = 'bold', size = size.ax),
#                          text = element_text(size = size.ax))),
#          width = 10,
#          height = 3)
#          
#   
#   # sum(firestats_cell.df$stand_kill[which(firestats_cell.df$group == "*CONTROL")])
#   # sum(firestats_cell.df$stand_kill[which(firestats_cell.df$group == "SynergyHigh")])  
#   # 
#   # mean(firestats_fire.df$size_km[which((firestats_fire.df$group == "*CONTROL") & (firestats_fire.df$year > 74))])
#   # mean(firestats_fire.df$size_km[which((firestats_fire.df$group == "SynergyHigh") & (firestats_fire.df$year > 74))])  
#   # 
#   # median(firestats_fire.df$size_km[which((firestats_fire.df$group == "*CONTROL") & (firestats_fire.df$year > 74))])
#   # median(firestats_fire.df$size_km[which((firestats_fire.df$group == "SynergyHigh") & (firestats_fire.df$year > 74))]) 
#   # 
#   # 
#   # vec.temp <- firestats_fire.df %>% subset((year > 74) & (group == "*CONTROL")) %>% group_by(group) %>% summarize(fs_mean = mean(size_km),
#   #                                                                           fs_median = median(size_km),
#   #                                                                           fs_sum = sum(size_km))
#   # 
#   # firestats_fire.df %>% subset(year > 74) %>% group_by(group) %>% summarize(fs_mean = mean(size_km) - as.numeric(vec.temp[,2]),
#   #                                                                           fs_median = median(size_km) - as.numeric(vec.temp[,3]),
#   #                                                                           fs_sum = sum(size_km) - as.numeric(vec.temp[,4]))
#   # 
#   # vec.temp <- as.numeric(firestats_cell.df %>% subset((year > 74) & (group == "*CONTROL")) %>% group_by(group) %>% summarize(fs_mean = mean(length(group))))[2]
#   # df.temp <- data.frame(firestats_cell.df %>% subset(year > 74) %>% group_by(group) %>% summarize(burned_area = vec.temp - length(group)))
#   # df.temp <- data.frame(firestats_cell.df %>% subset(year > 74) %>% group_by(group) %>% summarize(burned_area = length(group)))
#   # 
#   # df.temp <- df.temp[order(df.temp[,2]),]
#   
#   # W <- 8
#   # H <- 5
#   # size.ax <- 10
#   # # Difference time series (not cumulative)
#   # graphics.off()
#   # # ct <- sprintf("%03d", (length(search.files(path_plots.1, "FPLOT_TS2", ""))+1))
#   # pdf(paste0(path_plots.1, "/FPLOT_TS2.pdf"), onefile = TRUE, width = W, height = H)
#   # for (r in c(0:nrow(plots.deck))){
#   #   if (r == 0){
#   #     grid.table(plots.deck[,c(1:3)], theme=ttheme_minimal(base_size = 8))
#   #   } else {
#   # 
#   #     for (n in c(1:ncol(plots.deck))){
#   #       assign(names(plots.deck)[n], plots.deck[r,n])
#   #     }
#   # 
#   #     var.name <- value.plot.ts1
#   #     y_ribbon.plot <- unlist(strsplit(value.ribbon.ts1, ", "))
#   # 
#   #     df.name <- plots.deck$df.name[r]
#   #     xlims <- xlims
#   #     # conversion_facs <- eval(parse(text=conversion_facs))
#   # 
#   #     if (grepl(",", legend_type, fixed = T)){
#   #       legend_type <- eval(parse(text = legend_type))
#   #       legend.ts2 <- legend_type[3]
#   #     } else {
#   #       legend.ts2 <- legend_type
#   #     }
#   # 
#   #     y_label.plot <- plots.deck$ylab.ts1[r]
#   # 
#   #     absredux.ts.temp <- plots.deck$abs_or_redux.ts[r]
#   #     absredux.ts.sign <- c(-1, 1)[(absredux.ts.temp == "abs")+1]
#   #     absredux.ts.char <- gsub("1", "", as.character(absredux.ts.sign))
#   # 
#   #     ts.df <- calc_ts(df.name = df.name,
#   #                      value.name = value.name,
#   #                      conversion_fac = conversion_fac,
#   #                      annual_sum_or_mean = annual_sum_or_mean,
#   #                      xlims = xlims)
#   # 
#   #     ## I don't know why I can't get the y axis to adjust but his is my kitchen sink approach
#   #     ylims <- as.list(vector("numeric", 1))
#   #     nudge <- as.list(vector("numeric", 1))
#   #     for (i in c(1)){
#   #       value.name <- value.name[i]
#   #       col.sign <- absredux.ts.sign
#   #       value.name <- gsub("-", "", value.name)
#   #       ylims[[i]] <- range(ts.df[,var.name]*absredux.ts.sign)
#   #     }
#   # 
#   #     for (i in c(1:length(ylims))){
#   #       y <- unlist(ylims[i])
#   #       for (j in c(1:2)){
#   #         y.t <- y[j]
#   #         if (j == 2){y.t <-  y.t + sum(abs(y[1]), abs(y.t))*0.2}
#   #         y.sig <- sign(y.t)
#   #         y.t <- abs(y.t)
#   #         if (grepl(".", y.t, fixed = T)){
#   #           y.place <- 10^(nchar(strsplit(as.character(y.t), ".", fixed = T)[[1]][1])-1)
#   #         } else {
#   #           y.place <- 10^(nchar(as.character(y.t))-1)
#   #         }
#   #         y.t.1 <- plyr::round_any(y.t, y.place, f = ceiling)*y.sig
#   #         y[j] <- y.t.1
#   #       }
#   #       nudge[[i]] <- sum(abs(y))*0.1
#   #       ylims[[i]] <- y
#   #     }
#   # 
#   #     ### Time series 2
#   #     legend <- legend.ts1
#   #     ToE_window <- NULL
#   #     ts2 <-  plot_ts(ts.df = ts.df,
#   #                     plot_guide = plot_guide,
#   #                     y_label.plot = y_label.plot,
#   #                     y_ribbon.plot = y_ribbon.plot,
#   #                     var.name = var.name,
#   #                     legend = legend,
#   #                     ToE_window = ToE_window)
#   # 
#   #     trend <- which(abs(layer_scales(ts2)$y$range$range) == max(abs(layer_scales(ts2)$y$range$range)))
#   #     lab.x <- xlims[1]+(xlims[2]*0.04)
#   #     lab.y <- layer_scales(ts2)$y$range$range[trend]+(nudge[[1]]*(c(1, -1)[trend]))
#   #     lab <- alphabet_lower.per[length(years_plot)+1]
#   # 
#   #     # ts2 <- ts2 +
#   #     #   geom_label(aes(x = lab.x,
#   #     #                  y = lab.y,
#   #     #                  label = lab),
#   #     #              parse = T,
#   #     #              size = 5.25,
#   #     #              alpha = .9,
#   #     #              color = "black")
#   # 
#   #     ts2 <- ts2 +
#   #       theme(plot.margin = margin(1,1.25,1,1, "cm"))
#   # 
#   #     print(ts2)
#   #   }
#   # }
#   # while (!is.null(dev.list()))  dev.off()
#   # 
#   # 
#   # ####
#   # 
#   # # source(paste0(path_scripts, "BATCH_supplots_V4KE1.R"))
# 
# # }
# 
# # 
# # ########################################################################
# # ########################################################################
# # ####################################### GENERATE RASTERS ###############
# # ########################################################################
# # ########################################################################
# 
#   colors_redscale <- c("white",
#                       "red",
#                       "darkred",
#                       "purple")
# 
#   colors_rainbow <- c("cyan",
#                       "goldenrod",
#                       "red",
#                       "purple")
# 
#   colors_rainbow0 <- c("honeydew",
#                         #"white",
#                         "lemonchiffon",
#                         "gold",
#                         "red",
#                         "purple",
#                        "darkslateblue",
#                        "navyblue")
# 
# detach_package("plyr", TRUE)
# detach_package("dplyr", TRUE)
# library(ggnewscale)
# library(plyr)
# library(dplyr)
# library(png)
# library(grid)
# library(cowplot)
# library(jpeg)
# library(scales)
# 
# # Get treatment data
# ras_retreat.paths <- search.files(path_rasters, "^Rastreatburnrep", "")
# ras_treatable.paths <- search.files(path_rasters, "^Treatable", "")
# 
# # Make blank domain raster
# background.ras.melt <- as.matrix(list.blank[[1]])
# rownames(background.ras.melt) <- c(dim(background.ras.melt)[1]:1)
# colnames(background.ras.melt) <- c(1:dim(background.ras.melt)[2])
# background.ras.melt <- melt(background.ras.melt)
# background.ras.melt[which(background.ras.melt$value == 1),"value"] <- 0
# background.ras.melt <- background.ras.melt[(complete.cases(background.ras.melt) == T),]
# names(background.ras.melt) <- c("y", "x", "z")
# 
# inset.jpg <- readJPEG(paste0(path_geospa, "USA_DEM_inset3.jpg"))
# inset.png <- readPNG(paste0(path_geospa, "USA_DEM_inset3.png"))
# inset.plt <- ggdraw() +
#   draw_plot(grid::rasterGrob(inset.png, interpolate=TRUE), x = 0, y = 0, width = 1, height = 1)
# 
# # Get forest boundary and DEM
# forest_boundary <- st_read("/Volumes/LaCie/Geospatial/EXTENT_forest_boundary.shp")
# DEM <- raster(paste0(path_geospa, "DEM_subset2_CANVOR_1km.tif"))
# ras.temp <- raster(ras_retreat.paths[1])
# crs(ras.temp) <- crs(DEM)
# extent(ras.temp) <- extent(DEM)
# ras.temp <- resample(ras.temp, DEM, method = "ngb") #!?
# # DEM.coarse <- resample(DEM, ras.temp)
# DEM.coarse <- DEM
# DEM.melt <- melt(as.matrix(DEM.coarse))
# names(DEM.melt) <- c("y", "x", "z")
# DEM.melt$y <- DEM.melt$y[c(nrow(DEM.melt):1)]
# 
# sz <- 0.5
# al <- 0.5
# 
# DEM.plot <- ggplot() +
#   theme_light() +
#   theme(axis.text = element_blank()) +
#   theme(text=element_text(size=10)) +
#   labs(x = "", y = "") +
#   geom_tile(data = DEM.melt, aes(x = x, y = y, fill = z, color = z), size = sz) +
#   scale_fill_gradient2("", mid = "black", high = "white") +
#   scale_color_gradient2("", mid = "black", high = "white") +
#   guides(fill = "none", color = "none")
# 
# DEM_sb.plot <- DEM.plot +
#               geom_segment(aes(y = round(dim(DEM.coarse)[1]/20), yend = round(dim(DEM.coarse)[1]/20),
#                                x = round(dim(DEM.coarse)[2]/20), xend = round(dim(DEM.coarse)[1]/20)+100), size = 1) +
#               geom_text(aes(x = round(dim(DEM.coarse)[2]/20), y = round(dim(DEM.coarse)[1]/10),
#                             label = "100 km"), hjust = 0, fontface = "bold", size = 4)
# 
# # Process treatment data into retreatment rasters
# lims_retreat.df <- NULL
# for (i in c(0:length(plot_guide$group))){
# 
#   ## subset data
#   if (i == 0){
# 
#     ## Blank
#     # blank.ras.melt <- melt(list.blank[[1]])
#     # names(blank.ras.melt) <- c("y", "x", "z")
#     sce.temp <- "blank"
#     ras.temp <- list.blank[[1]]
# 
#   } else {
# 
#     ## raster of mean cell treatment count
#     sce.temp <- plot_guide$group[i]
#     df.temp <- treatlog.df %>% subset((group == sce.temp) & (year <= 100))
#     df.temp.1 <- data.frame(table(unlist(lapply(df.temp$cells_treated, strsplit, ", "))))
#     df.temp <- df.temp[,names(df.temp)[!grepl("cells_treated", names(df.temp))]]
# 
#     ras.temp <- list.blank[[1]]
#     if (nrow(df.temp.1) > 0){
#       df.temp.1[,1] <- as.numeric(as.character(df.temp.1[,1]))
#       df.temp.1[,2] <- as.numeric(as.character(df.temp.1[,2]))
#       df.temp.1 <- df.temp.1[order(df.temp.1[,1]),]
#       df.temp.1 <- df.temp.1[which(df.temp.1$Var1 > 0),]
#       row.names(df.temp.1) <- c(1:nrow(df.temp.1))
#       ras.temp[df.temp.1$Var1] <- signif(df.temp.1$Freq/length(unique(df.temp$treatment_rep)))
#       ras.temp[which((as.matrix(raster(ras_treatable.paths[grepl(sce.temp, ras_treatable.paths)])) == 0))] <- NA
#       df.temp.2 <- run_schedule[which(run_schedule$Run_group == sce.temp)[1],]
#       max.treat <- (floor(df.temp.2$Years/10)*10)/df.temp.2$P_treat_return
#       if (max(ras.temp, na.rm = T) > max.treat){
#         cat(paste0("\nHigh treat rep values found. Max = ", max(ras.temp[(ras.temp > max.treat) == T], na.rm = T), "\n"))
#       }
#       ras.temp[(ras.temp > max.treat) == T] <- max.treat
#     }
#   }
# 
#   ras.temp <- raster(ras.temp)
#   crs(ras.temp) <- crs(DEM.coarse)
#   extent(ras.temp) <- extent(forest_boundary)
#   ras.temp <- resample(ras.temp, DEM.coarse)
#   ras.melt <- melt(as.matrix(ras.temp))
#   names(ras.melt) <- c("y", "x", "z")
#   ras.melt$y <- ras.melt$y[c(nrow(ras.melt):1)]
#   ras.melt <- ras.melt %>% subset(!is.na(z))
#   row.names(ras.melt) <- c(1:nrow(ras.melt))
# 
#   if (sce.temp %in% c(ctl.temp, "blank") == F){
#     lims_retreat.df <- rbind(lims_retreat.df, cbind(sce.temp, min(ras.melt %>% subset(z > 0) %>% dplyr::select(z)), max(ras.melt$z)))
#   }
# 
#   assign(paste0(gsub("*", "", sce.temp, fixed = T), "_retreat.ras"), raster(ras.temp))
#   assign(paste0(gsub("*", "", sce.temp, fixed = T), "_retreat.ras.melt"), ras.melt)
# 
# }
# 
# lims_retreat.df <- data.frame(lims_retreat.df)
# names(lims_retreat.df) <- c("scenario", "min", "max")
# lims_retreat.df$min <- as.numeric(lims_retreat.df$min)
# lims_retreat.df$max <- as.numeric(lims_retreat.df$max)
# lims_retreat <- c(0, max(lims_retreat.df$max))
# 
# # Plot retreatment rasters
# for (i in c(1:length(plot_guide$group))){
# 
#   sce.temp <- plot_guide$group[i]
#   retreat.ras.melt <- get(paste0(gsub("*", "", sce.temp, fixed = T), "_retreat.ras.melt"))
# 
#   ## RE-INSERT BLANKSPACE (EDIT FEB 21)
#   # retreat.ras.melt <- retreat.ras.melt %>% subset(z > c(0, -1)[grepl("CONTROL", toupper(sce.temp))+1])
#   y_x.blank <- paste(blank_retreat.ras.melt$y, blank_retreat.ras.melt$x, sep = "_")
#   y_x.test <- paste(retreat.ras.melt$y, retreat.ras.melt$x, sep = "_")
#   retreat.ras.melt <- rbind(retreat.ras.melt, blank_retreat.ras.melt[which((y_x.blank %in% y_x.test) == F),])
#   ##
# 
#   plot.temp <- DEM.plot +
# 
#     new_scale("fill") +
# 
#     geom_tile(data = retreat.ras.melt %>% subset(ceiling(z) < 1),
#               aes(x = x, y = y, fill = z), na.rm = T, alpha = al, size = sz) +
#     scale_fill_gradientn(paste0("Treatment\nCount"),
#                          limits = c(0, 1),
#                          colors = "cyan"
#     ) +
# 
#     guides(fill = "none") +
# 
#     new_scale("fill") +
# 
#     geom_tile(data = retreat.ras.melt %>% subset(ceiling(z) >= 1),
#               aes(x = x, y = y, fill = ceiling(z)), na.rm = T, alpha = al, size = sz) +
#     scale_fill_gradientn(paste0("Treatment\nCount"),
#                          limits = c(0, ceiling(max(lims_retreat.df$max))),
#                          colors = colors_rainbow0
#                          ) +
#     # continuous_scale(
#     #   "fill", "my_pal",
#     #   my_palette(colors = colors_rainbow0,
#     #              # range = range(retreat.ras.melt %>% subset(z > 0) %>% dplyr::select(z)),
#     #              range = lims_retreat,
#     #              target = c(-1, 0.1),
#     #              replace_color = "cyan"),
#     #   limits = lims_retreat,
#     #   guide = guide_colourbar(nbin = 500) # Give guide plenty bins
#     # ) +
#     labs(fill = paste0("Retreatment\nCount"))
# 
#   ylims.temp <- layer_scales(plot.temp)$y$range$range
#   xlims.temp <- layer_scales(plot.temp)$x$range$range
# 
#   plot.temp <- plot.temp +
#     coord_cartesian(xlim = xlims.temp,
#                     ylim = ylims.temp,
#                     expand = FALSE, clip = "off")  +
#     theme(aspect.ratio = 1.75)
# 
#   assign(paste0("retreat_", gsub("*", "", sce.temp, fixed = T), ".ras.plt"), plot.temp)
# 
# }
# 
# x.range <- layer_scales(plot.temp)$x$range$range
# x.range <- x.range[2]-x.range[1]
# y.range <- layer_scales(plot.temp)$y$range$range
# y.range <- y.range[2]-y.range[1]
# 
# 
# 
# 
# # Plot biomass loading at initialization
# # Get initialization data
# path_init <- paste0(path_outputs, unique(params.df$P_snapshot_init), "/")
# if (dir.exists(path_init)){
#   spinup_data <- list.files(path_init, full.names = T)
#   spinup_data <- spinup_data[!grepl("aux.xml", spinup_data)]
#   for (i in c(1:length(spinup_data))){
#     ras.temp <- as.matrix(raster(spinup_data[i]))
#     ras.temp <- rbind(NA, cbind(NA, ras.temp, NA), NA)
#     assign(paste0("INIT_", gsub(".tif", "", strsplitsubr(spinup_data[i], "/", 1, 1)), ".ras"),
#            ras.temp)
#   }
# 
#   pft.ras <- INIT_pft_snapshot.ras
#   lbio.ras <- INIT_branch.biomass.kg_snapshot.ras + INIT_leaf.biomass.kg_snapshot.ras + INIT_stem.biomass.kg_snapshot.ras
#   dbio.ras <- INIT_cwd.kg_snapshot.ras + INIT_litter.kg_snapshot.ras + INIT_snag.kg_snapshot.ras
#   conn.ras <- connectivity.calc(INIT_leaf.biomass.kg_snapshot.ras, INIT_branch.biomass.kg_snapshot.ras, INIT_stem.biomass.kg_snapshot.ras)
#   bio.ras <- lbio.ras + dbio.ras
# 
#   vec.temp <- c("bio.ras", "lbio.ras", "dbio.ras")
#   vec.temp.1 <- as.list(rep(0, length(vec.temp)))
#   for (i in c(1:length(vec.temp))){
# 
#     name.temp <- vec.temp[i]
#     ras.temp <- get(name.temp)
# 
#     ras.temp <- ras.temp*0.001 # to metric tons
#     ras.temp <- raster(ras.temp)
#     crs(ras.temp) <- crs(DEM.coarse)
#     extent(ras.temp) <- extent(forest_boundary)
#     ras.temp <- resample(ras.temp, DEM.coarse)
#     ras.melt <- melt(as.matrix(ras.temp))
#     names(ras.melt) <- c("y", "x", "z")
#     ras.melt$y <- ras.melt$y[c(nrow(ras.melt):1)]
#     ras.melt <- ras.melt %>% subset(!is.na(z))
#     row.names(ras.melt) <- c(1:nrow(ras.melt))
# 
#     vec.temp.1[[i]] <- ceiling(max(ras.melt$z))
# 
#     name.temp.1 <- gsub(".ras", "_proj.ras", name.temp)
#     vec.temp[i] <- name.temp.1
# 
#     assign(name.temp.1, ras.temp)
#     assign(paste0(name.temp.1, ".melt"), ras.melt)
# 
#   }
# 
#   # vec.temp.1 <- "(MT/"
#   # vec.temp.2 <- ")"
#   # fill_label.temp <- paste0("~paste('", vec.temp.1,"',km^2,')',",vec.temp.2)
#   vec.temp.1.1 <- "Biomass"
#   vec.temp.1.2 <- "(MT/"
#   vec.temp.2 <- ")"
#   fill_label.temp <- paste0("atop('", vec.temp.1.1, "', paste('", vec.temp.1.2, "', ", ", km^{2},'", vec.temp.2, "'))")
#   lims_bio.df <- ceiling(unlist(vec.temp.1)/10)*10
#   for (i in c(1:length(vec.temp))){
# 
#     name.temp <- vec.temp[i]
#     ras.melt <- get(paste0(name.temp, ".melt"))
# 
#     plot.temp <- DEM.plot +
# 
#       new_scale("fill") +
#       geom_tile(data = ras.melt,
#                 aes(x = x, y = y, fill = z), na.rm = T, alpha = al) +
#       scale_fill_gradientn(parse(text = (fill_label.temp)),
#                            limits = c(0, max(lims_bio.df)),
#                            colors = colors_rainbow)
# 
#     plot.temp <- plot.temp +
#       coord_cartesian(xlim = layer_scales(plot.temp)$x$range$range,
#                       ylim = layer_scales(plot.temp)$y$range$range,
#                       expand = FALSE, clip = "off") +
#       theme(aspect.ratio = 1.75)
# 
#     if (i < length(vec.temp)){
#       plot.temp <- plot.temp + guides(fill = "none")
#     }
# 
#     # # insert inset
#     # if (i == 1){
#     #   plot.temp <- plot.temp + annotation_raster(inset.png,
#     #                                              ymin = ylims.temp[1],
#     #                                              ymax = ylims.temp[1] + (diff(ylims.temp)*0.23),
#     #                                              xmin = xlims.temp[1],
#     #                                              xmax = xlims.temp[1] + (diff(xlims.temp)*0.7))
#     # }
# 
#     assign(paste0(gsub("_proj", "", name.temp), ".plt"), plot.temp)
#   }
# 
# }
# 
# 
# 
# 
# 
# 
# 
# df.temp <- data.frame(cbind(params.df$P_burn_area, params.df$P_thin_area))
# df.temp <- split(df.temp, seq(nrow(df.temp)))
# 
# for (i in c(1:length(df.temp))){
#   ar <- unlist(lapply(as.list(gsub(")", "", gsub("c(", "", df.temp[[i]], fixed = T), fixed = T)), strsplit, ", "), recursive = F)
#   ar.len <- unlist(lapply(ar, length))
#   if (sum(ar.len) > length(ar)){
#     multi <- c("P_burn_area", "P_thin_area")[which(ar.len != 1)]
#     df.temp.1 <- data.frame(ar[[which(ar.len != 1)]], cbind(rep(ar[[which(ar.len == 1)]], length(ar[[which(ar.len != 1)]]))))
#     names(df.temp.1) <- c("P_burn_area", "P_thin_area")
#     df.temp.2 <- params.df[c(rep(i, nrow(df.temp.1))),]
#     df.temp.2[,c("P_burn_area", "P_thin_area")] <- df.temp.1
#     df.temp.2$Run_group <- paste0(df.temp.2$Run_group, "_", sprintf("%04d", as.numeric(df.temp.2[,multi])))
#     params.df <- params.df[c(1:nrow(params.df))[which(c(1:nrow(params.df)) != i)],]
#     params.df <- rbind(df.temp.2, params.df)
#     row.names(params.df) <- c(1:nrow(params.df))
#   }
# }
# 
# # Get initialization data
# # Get "treatable" rasters
# treat_factors <- c("roads", "slope", "strikes", "WUI", "PET", "agency", "lbio", "dbio", "conn")
# for (r in c(1:nrow(params.df))){
#   g <- params.df$Run_group[r]
#   for (v in names(params.df[grep("P_", names(params.df))])){
#     assign(v, params.df[r,v])
#   }
#   for (i in c(1:length(treat_factors))){
# 
#     quant <- get(paste0("P_quant_", treat_factors[i]))
# 
#     #if (quant != 9999){
# 
#     if ((quant == "SORT") | (quant == 9999)){
#       quant <- ">=0"
#     }
# 
#     ras.temp <- list.blank[[1]]
#     ras <- as.matrix(get(paste0(treat_factors[i], ".ras")))
# 
#     if (grepl("%", quant)){
#       ras.ix <- which((is.na(ras) == F) & (ras != 0) & (blank.ras == 0))
#       df.temp <- cbind(ras.ix, ras[ras.ix])
#       df.temp <- cbind(df.temp, rank(df.temp[,2], ties.method="min"))
#       ras.rank <- round(df.temp[,3]/length(df.temp[,3]) * 100)
#       if (length(unique(ras.rank)) == 1){
#         ras.rank[c(1:length(ras.rank))] <- 100
#       }
#       df.temp[,3] <- ras.rank
#       ras.temp[ras.ix] <- eval(parse(text = paste0("ras.rank", gsub("%", "", quant))))
#     } else {
#       if (grepl("=", quant)){
#         ras.temp[which(eval(parse(text = paste0("ras ", gsub("#", "", quant)))) & (list.blank[[1]] == 0))] <- 1
#       } else {
#         ras.temp[which(eval(parse(text = paste0("ras %in% c(", quant, ")"))) & (list.blank[[1]] == 0))] <- 1
#       }
#     }
#     assign(paste0(treat_factors[i], ".ix"), which(ras.temp == 1))
#     assign(paste0(treat_factors[i], ".rank"), ras.temp)
# 
#     assign(paste0("treatable_", g, "_", treat_factors[i], ".ix"), which(ras.temp == 1))
#     assign(paste0("treatable_", g, "_", treat_factors[i], ".rank"), ras.temp)
#     #}
#   }
#   vec.temp <- which(unlist(lapply(lapply(unlist(lapply("treatable_", paste0, g, "_", treat_factors, ".ix")), get), length)) > 0)
#   array.temp <- lapply(lapply(treat_factors[vec.temp], paste0, ".rank"), get)
#   treatable.ras <- Reduce("+", array.temp) == length(vec.temp)
#   assign(paste0("treatable_", g, "_ALL.ras"), treatable.ras)
# 
#   vec.temp <- vec.temp[!grepl("bio", treat_factors[vec.temp])]
#   array.temp <- lapply(lapply(treat_factors[vec.temp], paste0, ".rank"), get)
#   treatable.ras <- Reduce("+", array.temp) == length(vec.temp)
#   assign(paste0("treatable_", g, "_STATIC.ras"), treatable.ras)
#   #raster::writeRaster(raster(treatable.ras), paste0(path_rasbas, "Treatable_", params.df$Run_group[r], ".tiff"), overwrite = T)
# }
# 
# 
# # Summary data frame
# treatable.df <- data.frame(cbind(c(1:length(blank.ras)),
#                                  as.vector(INIT_pft_snapshot.ras),
#                                  as.vector(lbio.ras),
#                                  as.vector(dbio.ras),
#                                  as.vector(bio.ras),
#                                  as.vector(roads.ras),
#                                  as.vector(elev),
#                                  as.vector(WUI.ras),
#                                  as.vector(strikes.ras),
#                                  as.vector(slope.ras)
# ))
# names(treatable.df) <- c("cell_id",
#                          "pft",
#                          "lbio",
#                          "dbio",
#                          "bio",
#                          "roads",
#                          "elevation",
#                          "WUI",
#                          "strikes",
#                          "slope")
# treatable.df$pft_name <- pft_names$Name[match(treatable.df$pft, pft_names$ID)]
# 
# names.temp <- ls()[which(grepl("treatable_", ls()) & grepl(".ras", ls()) & (!grepl("RAS_", ls())))]
# for (i in c(1:length(names.temp))){
#   name.temp <- names.temp[i]
#   ras.temp <- get(name.temp)
#   group.temp <- gsub("treatable_", "", gsub(".ras", "", name.temp))
#   treatable.df <- cbind(treatable.df, as.vector(ras.temp))
#   names(treatable.df)[ncol(treatable.df)] <- group.temp
# }
# treatable.df <- treatable.df %>% subset(is.na(pft) == F)
# 
# df.temp <- NULL
# 
# vec.temp <- sort(unique(unlist(lapply(unique(params.df$Run_group), grep, names(treatable.df)))))
# for (c in vec.temp){
# 
#   scenario <- names(treatable.df)[c]
#   df.temp.1 <- treatable.df[which(treatable.df[,scenario] == T),]
#   df.temp.1 <- cbind(scenario, df.temp.1[,which(names(df.temp.1) %in% params.df$Run_group == F)])
#   df.temp <- rbind(df.temp, df.temp.1)
# 
# }
# 
# treatable.df.1 <- df.temp
# treatable.tab <- data.frame(table(treatable.df.1$scenario))
# treatable.tab$Freq <- as.numeric(treatable.tab$Freq)
# treatable.tab$Var1 <- as.character(treatable.tab$Var1)
# names(treatable.tab) <- c("Scenario", "Static_extent")
# 
# vec.return <- as.numeric(gsub("9999", "1", as.character(params.df$P_treat_return[match(treatable.tab$Scenario, params.df$Run_group)])))
# 
# vec.extent_d <- as.numeric(gsub("9999", "1", as.character(params.df$P_burn_area[match(treatable.tab$Scenario, params.df$Run_group)])))
# vec.extent_l <- as.numeric(gsub("9999", "1", as.character(params.df$P_thin_area[match(treatable.tab$Scenario, params.df$Run_group)])))
# vec.extent <- apply(data.frame(cbind(vec.extent_d, vec.extent_l)), 1, max)
# 
# vec.bio_d <- gsub("9999", ">=1%", as.character(params.df$P_quant_dbio[match(treatable.tab$Scenario, params.df$Run_group)]))
# vec.bio_l <- gsub("9999", ">=1%", as.character(params.df$P_quant_lbio[match(treatable.tab$Scenario, params.df$Run_group)]))
# for (vec in c("vec.bio_d", "vec.bio_l")){
#   vec.temp <- get(vec)
#   vec.temp.out <- vector(mode = "numeric", length = length(vec.temp))
#   for (i in c(1:length(vec.temp))){
#     v <- vec.temp[i]
#     v.num <- as.numeric(gsub("#", "", gsub("<", "", gsub(">", "", gsub("=", "", gsub("%", "", v))))))
#     v.sym <- gsub(v.num, "", v)
#     if (grepl("%", v.sym)){
#       if (grepl("=", v.sym)){
#         if (grepl(">", v.sym)){
#           v.num <- v.num-1
#         }
#         if (grepl("<", v.sym)){
#           v.num <- v.num+.1
#         }
#       }
#       if (grepl(">", v.sym)){
#         v.num <- 100 - v.num
#       }
#     }
#     v.num <- v.num*(.01)
#     vec.temp.out[i] <- v.num
#   }
#   assign(paste0(vec), vec.temp.out)
# }
# vec.bio <- apply(data.frame(cbind(vec.bio_d, vec.bio_l)), 1, min)
# 
# # vec.return, vec.extent, vec.bio
# treatable.tab$Dynamic_extent <- (treatable.tab$Static_extent*vec.bio)/(vec.return*vec.extent)
# treatable.tab$Viable <- treatable.tab$Dynamic_extent > 2
# 
# # PFT data
# pft.df <- data.frame(treatable.df %>% group_by(pft_name) %>% summarize(count = length(dbio),
#                                                                        bio.mean = mean(bio),
#                                                                        lbio.mean = mean(lbio),
#                                                                        dbio.mean = mean(dbio),
#                                                                        bio.median = median(bio),
#                                                                        lbio.median = median(lbio),
#                                                                        dbio.median = median(dbio),
#                                                                        bio.sum = sum(bio),
#                                                                        lbio.sum = sum(lbio),
#                                                                        dbio.sum = sum(dbio))
# )
# 
# df.temp <- data.frame(matrix(0, ncol = nrow(params.df), nrow = nrow(pft.df)))
# names(df.temp) <- params.df$Run_group
# pft.df <- cbind(pft.df, df.temp)
# for (pft in unique(treatable.df$pft_name)){
#   for (grp in params.df$Run_group){
#     pft.df[which(pft.df$pft_name == pft),grp] <- sum(treatable.df[which(treatable.df$pft_name == pft),grp])
#   }
# }
# 
# write.csv(pft.df, paste0(path_plots.1, "Treatable_PFT.csv"))
# write.csv(treatable.tab, paste0(path_plots.1, "Treatable_Table.csv"))
# write.csv(treatable.df, paste0(path_plots.1, "Treatable.csv"))
# 
# 
# if (experiment.name == "KnowAcc"){
#   name.temp <- "SynergyHigh"
# }
# 
# if (experiment.name %in% c("Bio", "Area")){
#   name.temp <- plot_guide$group[which(as.numeric(plot_guide$num) == max(as.numeric(plot_guide$num)))]
# }
# 
# pftfin_sd0_control.ras <- raster(search.files(path_rasters, c("!^Run", "!.pdf", "pftfin", "sd0", "control"), "ignore.case"))
# pftfin_sd0_treatmax.ras <- raster(search.files(path_rasters, c("!^Run", "!.pdf", "pftfin", "sd0", name.temp), "ignore.case"))
# 
# rasters <- c("roads.ras",
#              "slope.ras",
#              "WUI.ras",
#              "agency.ras",
#              "pft.ras",
#              "pftfin_sd0_control.ras",
#              "pftfin_sd0_treatmax.ras")
# 
# units <- c("Distance\nfrom\nRoads\n(km)",
#            "Slope\n(degrees)",
#            "Distance\nfrom\nHomes\n(km)",
#            "Land\nManagement\nAgency",
#            "PFT",
#            "PFTs",
#            "PFTs")
# 
# for (i in c(1:length(rasters))){
#   if (length(get(rasters[i])) > 0){
# 
#     ras.temp <- get(rasters[i])
# 
#     if (class(ras.temp)[1] == "RasterLayer"){
#       if (length(unique(getValues(ras.temp))) == 1){
#         module_name <- "Init"
#         source(paste0(path_scripts, "BATCH_module_V4KE4.R"))
#         ras.temp <- get(rasters[i])
#       }
#       ras.temp <- as.matrix(ras.temp)
#     }
# 
#     ras.temp[which(is.na(list.blank[[1]]))] <- NA
#     ras.temp <- raster(ras.temp)
# 
#     crs(ras.temp) <- crs(DEM.coarse)
#     extent(ras.temp) <- extent(forest_boundary)
# 
#     if ((rasters[i] == "agency.ras") | (rasters[i] =="pft.ras")){
#       ras.temp <- resample(ras.temp, DEM.coarse, method = "ngb")
#     } else {
#       ras.temp <- resample(ras.temp, DEM.coarse, method = "bilinear")
#     }
# 
#     ras.melt <- melt(as.matrix(ras.temp))
#     names(ras.melt) <- c("y", "x", "z")
#     ras.melt$y <- ras.melt$y[c(nrow(ras.melt):1)]
#     ras.melt <- ras.melt %>% subset(!is.na(z))
#     row.names(ras.melt) <- c(1:nrow(ras.melt))
# 
#     if ((rasters[i] == "agency.ras") | (rasters[i] =="pft.ras")){
#       if (rasters[i] == "agency.ras"){
#         ras.melt$z <- agency.df$Agency[match(ras.melt$z, agency.df$ID)]
#       }
# 
#       if (rasters[i] =="pft.ras"){
#         ras.melt$z <- pft_names$Name[match(ras.melt$z, pft_names$ID)]
#       }
#     } else {
#       ras.melt$z <- as.numeric(ras.melt$z)
#     }
# 
#     assign(paste0(rasters[i], "_CRS"), ras.temp)
#     assign(paste0(rasters[i], "_CRS.melt"), ras.melt)
# 
#     if (rasters[i] == "agency.ras"){
# 
#       plot.temp <- DEM.plot +
#         new_scale("fill") +
#         geom_tile(data = ras.melt %>% subset(z > c(0, -1)[grepl("CONTROL", toupper(sce.temp))+1]),
#                   aes(x = x, y = y, fill = as.character(z)), colour="#FFFFFF00", na.rm = T, alpha = al, size = sz) +
#         scale_fill_manual(units[i],
#                            values = c("palegreen3",
#                                       "khaki",
#                                       "lightseagreen",
#                                       "aquamarine2",
#                                       "darkolivegreen4",
#                                       "darkgreen",
#                                       "pink"),
#                            na.value = "white" )
# 
#     } else {
# 
#       if (rasters[i] =="pft.ras"){
# 
#         plot.temp <- DEM.plot  +
#           new_scale("fill") +
#           geom_tile(data = ras.melt %>% subset(z > c(0, -1)[grepl("CONTROL", toupper(sce.temp))+1]),
#                     aes(x = x, y = y, fill = as.character(z)), colour="#FFFFFF00", na.rm = T, alpha = al, size = sz) +
#           scale_fill_manual(units[i],
#                             values = c(pft_names$color[match(sort(unique(ras.melt$z)), pft_names$Name)]),
#                             na.value = "white" )
# 
#       } else {
# 
#         plot.temp <- DEM.plot +
#           new_scale("fill") +
#           geom_tile(data = ras.melt %>% subset(z > c(0, -1)[grepl("CONTROL", toupper(sce.temp))+1]),
#                     aes(x = x, y = y, fill = z), colour="#FFFFFF00", na.rm = T, alpha = al, size = sz) +
#           scale_fill_gradientn(units[i],
#                                limits = c(0, max(ras.melt$z)),
#                                colors = colors_rainbow)
# 
#       }
#     }
# 
#     ylims.temp <- layer_scales(plot.temp)$y$range$range
#     xlims.temp <- layer_scales(plot.temp)$x$range$range
# 
#     plot.temp <- plot.temp +
#       guides(color = "none") +
#       coord_cartesian(xlim = xlims.temp,
#                       ylim = ylims.temp,
#                       expand = FALSE,
#                       clip = "off") +
#       theme(aspect.ratio = 1.75)
# 
#     assign(paste0("BASIC_", rasters[i], ".plt"), plot.temp)
#   }
# }
# 
# 
# 
# # Forest loss rasters
# if (experiment.name == "KnowAcc"){
#   sce.temp <- "SynergyHigh"
# }
# 
# if (experiment.name %in% c("Bio", "Area")){
#   sce.temp <- plot_guide$group[which(as.numeric(plot_guide$num) == max(as.numeric(plot_guide$num)))]
# }
# 
# ctl.temp <- plot_guide$group[grepl("CONTROL", toupper(plot_guide$group))]
# 
# fcov.ras.melt <- NULL
# for (name.temp in c(sce.temp, ctl.temp)){
#   name.temp.1 <- gsub("*", "", name.temp, fixed = T)
#   ix.temp <- grep(toupper(name.temp.1), toupper(run_schedule$Run_group))
#   for (r in c(1:length(ix.temp))){
#     rid <- sprintf("%03d", run_schedule$Run_ID[ix.temp[r]])
#     ras.temp <- as.matrix(raster(search.files(path_rasters, c(paste0("^Run", rid), "pftfin", "!mean"), "ignore.case")))
#     ras.temp <- raster(ras.temp == 0)
#     # crs(ras.temp) <- crs(DEM.coarse)
#     # extent(ras.temp) <- extent(forest_boundary)
#     # ras.temp <- resample(ras.temp, DEM.coarse, method = "ngb")
#     ras.melt <- melt(as.matrix(ras.temp))
#     names(ras.melt) <- c("y", "x", "z")
#     ras.melt$y <- ras.melt$y[c(nrow(ras.melt):1)]
#     ras.melt <- ras.melt %>% subset(!is.na(z))
#     row.names(ras.melt) <- c(1:nrow(ras.melt))
#     names(ras.melt)[3] <- paste0("z_", r)
#     if (r == 1){
#       ras.melt.df <- ras.melt
#     } else {
#       ras.melt.df <- left_join(ras.melt.df, ras.melt, by = c("x", "y"))
#     }
#   }
#   ras.melt.df <- cbind(ras.melt.df[,c("x", "y")], apply(X = ras.melt.df[,grepl("z", names(ras.melt.df))], FUN = "mean", MARGIN = 1))
#   names(ras.melt.df)[3] <- paste0("z_", name.temp.1)
#   if (is_null(fcov.ras.melt)){
#     fcov.ras.melt <- ras.melt.df
#   } else {
#     fcov.ras.melt <- left_join(fcov.ras.melt, ras.melt.df, by = c("x", "y"))
#   }
# }
# 
# fcov_test.ras <- rasterFromXYZ(fcov.ras.melt[,c(1, 2, 3)])
# fcov_control.ras <- rasterFromXYZ(fcov.ras.melt[,c(1, 2, 4)])
# 
# fcov_test.blur.ras <- disaggregate(fcov_test.ras, 5, method="bilinear")
# fcov_control.blur.ras <- disaggregate(fcov_control.ras, 5, method="bilinear")
# # fcov_test.blur.ras <- raster::focal(fcov_test.ras, w = matrix(1,5,5), mean)
# # fcov_control.blur.ras <- raster::focal(fcov_control.ras, w = matrix(1,5,5), mean)
# test.mat <- as.matrix(fcov_test.blur.ras)
# control.mat <- as.matrix(fcov_control.blur.ras)
# diff.mat <- control.mat - test.mat
# 
# # fcov_diff.ras <- raster(-1*(as.matrix(fcov_test.blur.ras) / as.matrix(fcov_control.blur.ras)))
# # fcov_diff.mat <- 100*(abs(diff.mat)/((control.mat + test.mat)/2))
# fcov_diff.ras <- raster(diff.mat)
# fcov_diff.blur.ras <- disaggregate(fcov_diff.ras, 5, method="bilinear")
# 
# # fcov_diff.blur.ras <- raster::focal(ras.temp, w = matrix(1,9,9), mean)
# 
# lims <- c(range(as.vector(as.matrix(fcov_test.blur.ras)), na.rm = T),
#           range(as.vector(as.matrix(fcov_control.blur.ras)), na.rm = T))#,
#           #range(as.vector(as.matrix(fcov_diff.blur.ras)), na.rm = T))
# lims <- c(min(lims), max(lims))
# 
# for (name.temp in c("fcov_diff.blur.ras", "fcov_control.ras", "fcov_test.ras")){
# 
#   ras.temp <- get(name.temp)
#   crs(ras.temp) <- crs(DEM.coarse)
#   extent(ras.temp) <- extent(forest_boundary)
#   ras.temp <- resample(ras.temp, DEM.coarse, method = "ngb")
#   ras.melt <- melt(as.matrix(ras.temp))
#   names(ras.melt) <- c("y", "x", "z")
#   ras.melt$y <- ras.melt$y[c(nrow(ras.melt):1)]
#   ras.melt <- ras.melt %>% subset(!is.na(z))
#   row.names(ras.melt) <- c(1:nrow(ras.melt))
# 
#   plot.temp <- DEM.plot +
#     new_scale("fill") +
#     geom_tile(data = ras.melt %>%
#                 subset(!is.na(z)), aes(x = x,
#                                        y = y,
#                                        fill = z),
#                                        # alpha = ((abs(z)-min(abs(z)))/(max(abs(z))-min(abs(z))))),
#               colour="#FFFFFF00",
#               na.rm = T,
#               #alpha = al,
#               size = sz)
# 
#   ylims.temp <- layer_scales(plot.temp)$y$range$range
#   xlims.temp <- layer_scales(plot.temp)$x$range$range
# 
#   if (!grepl("diff", name.temp)){
#     plot.temp <- plot.temp +
#       scale_fill_gradientn("",
#                          limits = c(0, 1),
#                          labels = scales::percent,
#                          colors = c("transparent", "red")) +
#       guides(color = "none", alpha = "none") +
#       coord_cartesian(xlim = xlims.temp,
#                       ylim = ylims.temp,
#                       expand = FALSE,
#                       clip = "off") +
#       theme(aspect.ratio = 1.75)
#   } else {
#     plot.temp <- plot.temp +
#       scale_fill_gradientn("",
#                            colours = c("cyan","white","red"),
#                            values = rescale(c(min(ras.melt$z), 0, max(ras.melt$z))),
#                            labels = scales::percent,
#                            guide = "colorbar", limits = c(min(ras.melt$z),max(ras.melt$z))) +
#       guides(color = "none", alpha = "none") +
#       coord_cartesian(xlim = xlims.temp,
#                       ylim = ylims.temp,
#                       expand = FALSE,
#                       clip = "off") +
#       theme(aspect.ratio = 1.75)
#   }
# 
#   assign(paste0(name.temp, ".plt"), plot.temp)
# 
# }
# 
# 
# 
# 
# 
# ## Assign universal names to plots
# abbrev.key <- NULL
# for (i in c(1:nrow(plot_guide))){
#   sce.temp <- plot_guide$group[i]
#   vec.temp <- ls()[which(grepl("ras.plt", ls()) & grepl(sce.temp, ls()))]
#   for (name.temp in vec.temp){
#     name.temp.1 <- gsub(gsub("*", "", sce.temp, fixed = T), paste0("S", plot_guide$order[i]), gsub("*", "", name.temp, fixed = T))
#     abbrev.key <- data.frame(rbind(abbrev.key, cbind(name.temp, name.temp.1)))
#     assign(name.temp.1, get(name.temp))
#   }
# }
# 
# 
# scalebar_km <- 100
# scalebar_y <- round(dim(DEM.coarse)[1]/20)
# scalebar_x1 <- round(dim(DEM.coarse)[2]/20)
# scalebar_texty <- round(dim(DEM.coarse)[1]/10)
# scalebar_linesize <- 1
# scalebar_textsize <- 4
# scalebar_x2 <- scalebar_x1 + scalebar_km
# 
# inset_ymin <- ylims.temp[1]
# inset_ymax <- ylims.temp[1] + (diff(ylims.temp)*0.23)
# inset_xmin <- xlims.temp[1]
# inset_xmax <- xlims.temp[1] + (diff(xlims.temp)*0.7)
# 
# W <- 10
# H <- 7
# 
# # graphics.off()
# # pdf(paste0(path_plots.1, "/FPLOT_RASTERS.pdf"), onefile = TRUE, width = W, height = H)
# if (1 == 1){
# 
#   #######################################
#   ########################## FOREST COVER
# 
#   multiplot <- ((
# 
#     fcov_control.ras.plt +
#       scale_fill_gradientn("",
#                            limits = c(0, 1),
#                            labels = scales::percent,
#                            colors = c("transparent", "red")) +
#       guides(color = "none", alpha = "none") +
#       coord_cartesian(xlim = xlims.temp,
#                       ylim = ylims.temp,
#                       expand = FALSE,
#                       clip = "off") +
#       theme(aspect.ratio = 1.75)
# 
#   ) | (
# 
#     fcov_test.ras.plt +
#       scale_fill_gradientn("",
#                            limits = c(0, 1),
#                            labels = scales::percent,
#                            colors = c("transparent", "red")) +
#       guides(color = "none", alpha = "none") +
#       coord_cartesian(xlim = xlims.temp,
#                       ylim = ylims.temp,
#                       expand = FALSE,
#                       clip = "off") +
#       theme(aspect.ratio = 1.75)
# 
#   ) | (
# 
#     fcov_diff.blur.ras.plt +
#       scale_fill_gradientn("",
#                            colours = c("cyan","transparent","red"),
#                            values = rescale(c(-max(abs(ras.melt$z)), 0, max(abs(ras.melt$z)))),
#                            labels = scales::percent,
#                            guide = "colorbar", limits = c(-max(abs(ras.melt$z)), max(abs(ras.melt$z)))) +
#       guides(color = "none", alpha = "none") +
#       coord_cartesian(xlim = xlims.temp,
#                       ylim = ylims.temp,
#                       expand = FALSE,
#                       clip = "off") +
#       theme(aspect.ratio = 1.75)
# 
#   )) &
#     plot_annotation(tag_prefix = '(', tag_levels = 'a', tag_suffix = ')') &
#     theme(plot.tag = element_text(face = 'bold')) +
#     theme(legend.justification = "left")
# 
#   ggsave(filename = paste0(path_plots.1, "ForestCover_1.jpg"),
#          plot = multiplot,
#          width = W,
#          height = H)
# 
#   # multiplot <- ((fcov_control.ras.plt + guides(fill = "none")) |
#   #                 (fcov_test.ras.plt) |
#   #                 fcov_diff.blur.ras.plt) &
#   #   plot_layout(guides = "collect") &
#   #   plot_annotation(tag_prefix = '(', tag_levels = 'a', tag_suffix = ')') +
#   #   theme(plot.tag = element_text(face = 'bold')) +
#   #   theme(legend.justification = "left")
#   #
#   # ggsave(filename = paste0(path_plots.1, "ForestCover_1.jpg"),
#   #        plot = multiplot,
#   #        width = W,
#   #        height = H)
# 
# 
# 
# 
# 
# 
# 
# #######################################
# ############ STATIC RESTRICTION FACTORS
# 
# multiplot <- ((BASIC_roads.ras.plt +
#                  geom_segment(aes(y = scalebar_y, yend = scalebar_y,
#                                   x = scalebar_x1, xend = scalebar_x2),
#                               size = scalebar_linesize) +
#                  geom_text(aes(x = scalebar_x1, y = scalebar_texty,
#                                label = paste0(scalebar_km, " km")), hjust = 0,
#                            fontface = "bold", size = scalebar_textsize) |
#                  BASIC_slope.ras.plt) /
#                 (BASIC_WUI.ras.plt |
#                    BASIC_agency.ras.plt)) +
#   plot_annotation(tag_prefix = '(', tag_levels = 'a', tag_suffix = ')') &
#   theme(plot.tag = element_text(face = 'bold')) +
#   theme(legend.justification = "left")
# 
# ggsave(filename = paste0(path_plots.1, "StaticFactors_1.jpg"),
#        plot = multiplot,
#        width = W,
#        height = H)
# 
# 
# ###########################
# ############ PFT & PFT MODE
# 
# multiplot <- (BASIC_pft.ras.plt +
#                 geom_segment(aes(y = scalebar_y, yend = scalebar_y,
#                                  x = scalebar_x1, xend = scalebar_x2),
#                              size = scalebar_linesize) +
#                 geom_text(aes(x = scalebar_x1, y = scalebar_texty,
#                               label = paste0(scalebar_km, " km")), hjust = 0,
#                           fontface = "bold", size = scalebar_textsize) |
#                 BASIC_pftfin_sd0_control.ras.plt) +
#   plot_annotation(tag_prefix = '(', tag_levels = 'a', tag_suffix = ')') &
#   theme(plot.tag = element_text(face = 'bold')) +
#   theme(legend.justification = "left")
# 
# ggsave(filename = paste0(path_plots.1, "PFTmode_1.jpg"),
#        plot = multiplot,
#        width = W,
#        height = H)
# 
# 
# ####################
# ############ BIOMASS
# 
# multiplot <- (lbio.ras.plt +
#                 geom_segment(aes(y = scalebar_y, yend = scalebar_y,
#                                  x = scalebar_x1, xend = scalebar_x2),
#                              size = scalebar_linesize) +
#                 geom_text(aes(x = scalebar_x1, y = scalebar_texty,
#                               label = paste0(scalebar_km, " km")), hjust = 0,
#                           fontface = "bold", size = scalebar_textsize) |
#                 dbio.ras.plt |
#                 bio.ras.plt) +
#   plot_layout(guides = "collect") &
#   plot_annotation(tag_prefix = '(', tag_levels = 'a', tag_suffix = ')') &
#   theme(plot.tag = element_text(face = 'bold'))
# 
# ggsave(filename = paste0(path_plots.1, "Biomass_1.jpg"),
#        plot = multiplot,
#        width = W,
#        height = H)
# 
# multiplot <- ((lbio.ras.plt +
#                 geom_segment(aes(y = scalebar_y, yend = scalebar_y,
#                                  x = scalebar_x1, xend = scalebar_x2),
#                              size = scalebar_linesize) +
#                 geom_text(aes(x = scalebar_x1, y = scalebar_texty,
#                               label = paste0(scalebar_km, " km")), hjust = 0,
#                           fontface = "bold", size = scalebar_textsize) |
#                 dbio.ras.plt) /
#                 (bio.ras.plt | BASIC_pft.ras.plt)) +
#   plot_layout(guides = "collect") &
#   plot_annotation(tag_prefix = '(', tag_levels = 'a', tag_suffix = ')') &
#   theme(plot.tag = element_text(face = 'bold'))
# 
# ggsave(filename = paste0(path_plots.1, "Biomass_2.jpg"),
#        plot = multiplot,
#        width = H,
#        height = H)
# 
# 
# #########################
# ############ RETREAT FULL
# 
# if (experiment.name %in% c("Bio", "Area")){
#   multiplot <- ((retreat_S2.ras.plt +
#                     geom_segment(aes(y = scalebar_y, yend = scalebar_y,
#                                      x = scalebar_x1, xend = scalebar_x2),
#                                  size = scalebar_linesize) +
#                     geom_text(aes(x = scalebar_x1, y = scalebar_texty,
#                                   label = paste0(scalebar_km, " km")), hjust = 0,
#                               fontface = "bold", size = scalebar_textsize) |
#                     retreat_S6.ras.plt) /
#                    (retreat_S3.ras.plt |
#                       retreat_S4.ras.plt |
#                       retreat_S5.ras.plt))  +
#     plot_layout(guides = "collect") &
#     plot_annotation(tag_prefix = '(', tag_levels = 'a', tag_suffix = ')') &
#     theme(plot.tag = element_text(face = 'bold'))
# }
# 
# if (experiment.name == "KnowAcc"){
#   multiplot <- ((retreat_TreatmentBase.ras.plt +
#                     geom_segment(aes(y = scalebar_y, yend = scalebar_y,
#                                      x = scalebar_x1, xend = scalebar_x2),
#                                  size = scalebar_linesize) +
#                     geom_text(aes(x = scalebar_x1, y = scalebar_texty,
#                                   label = paste0(scalebar_km, " km")), hjust = 0,
#                               fontface = "bold", size = scalebar_textsize) |
#                     retreat_AccessHigh.ras.plt |
#                     retreat_AgencyHigh.ras.plt |
#                     retreat_KnowledgeHigh.ras.plt |
#                     retreat_SynergyHigh.ras.plt) /
#                    (retreat_CONTROL.ras.plt |
#                       retreat_AccessLow.ras.plt |
#                       retreat_AgencyLow.ras.plt |
#                       retreat_KnowledgeLow.ras.plt |
#                       retreat_SynergyLow.ras.plt))  +
#     plot_layout(guides = "collect") &
#     plot_annotation(tag_prefix = '(', tag_levels = 'a', tag_suffix = ')') &
#     theme(plot.tag = element_text(face = 'bold'))
# 
# }
# 
# ggsave(filename = paste0(path_plots.1, "RetreatFull_1.jpg"),
#        plot = multiplot,
#        width = W,
#        height = H)
# 
# 
# ###########################
# ############ RETREAT SELECT
# 
# if (experiment.name == "KnowAcc"){
# 
#   p1 <- retreat_SynergyLow.ras.plt +
#     geom_segment(aes(y = scalebar_y, yend = scalebar_y,
#                      x = scalebar_x1, xend = scalebar_x2),
#                  size = scalebar_linesize) +
#     geom_text(aes(x = scalebar_x1, y = scalebar_texty,
#                   label = paste0(scalebar_km, " km")), hjust = 0,
#               fontface = "bold", size = scalebar_textsize)
# 
#   p6 <- cowplot::get_legend(p1 + guides(fill = guide_colorbar(title.position = "right")))
# 
#   p1 <- p1 + guides(fill = "none")
#   p2 <- retreat_SynergyHigh.ras.plt + guides(fill = "none")
#   p3 <- retreat_TreatmentBase.ras.plt + guides(fill = "none")
#   p4 <- retreat_CONTROL.ras.plt + guides(fill = "none")
#   p5 <- inset.plt
# 
# }
# 
# if (experiment.name %in% c("Bio", "Area")){
# 
#   p1 <- retreat_S3.ras.plt +
#     geom_segment(aes(y = scalebar_y, yend = scalebar_y,
#                      x = scalebar_x1, xend = scalebar_x2),
#                  size = scalebar_linesize) +
#     geom_text(aes(x = scalebar_x1, y = scalebar_texty,
#                   label = paste0(scalebar_km, " km")), hjust = 0,
#               fontface = "bold", size = scalebar_textsize)
# 
#   p6 <- cowplot::get_legend(p1 + guides(fill = guide_colorbar(title.position = "right")))
# 
#   p1 <- p1 + guides(fill = "none")
#   p2 <- retreat_S4.ras.plt + guides(fill = "none")
#   p3 <- retreat_S6.ras.plt + guides(fill = "none")
#   p4 <- retreat_S1.ras.plt + guides(fill = "none")
#   p5 <- inset.plt
# 
# }
# 
# design <- "
#   1236
#   1235
#   "
# multiplot <- p1 + p2 + p3 + p5 + p6 +
#   plot_layout(design=design) +
#   plot_annotation(theme = theme(plot.margin = margin())) &
#   plot_annotation(tag_prefix = '(', tag_levels = 'a', tag_suffix = ')') &
#   theme(plot.tag = element_text(face = 'bold'))
# 
# ggsave(filename = paste0(path_plots.1, "RetreatSelect_1.jpg"),
#        plot = multiplot,
#        width = 9,
#        height = 4)
# 
# # design <- "
# #   126
# #   345
# #   "
# # multiplot <- p1 + p2 + p3 + p4 + p5 + p6 +
# #   plot_layout(design=design) +
# #   plot_annotation(theme = theme(plot.margin = margin())) &
# #   plot_annotation(tag_prefix = '(', tag_levels = 'a', tag_suffix = ')') &
# #   theme(plot.tag = element_text(face = 'bold'))
# #
# # ggsave(filename = paste0(path_plots.1, "RetreatSelect_2.jpg"),
# #        plot = multiplot,
# #        width = 4,
# #        height = 4)
# 
# 
# }
# 
# 
# 
# 
# ########################################################################
# ########################################################################
# ##################################### OTHER RANDOM PLOTS ###############
# ########################################################################
# ########################################################################
# 
# detach_package("plyr", TRUE)
# 
# df.temp <- treatlog.df %>% group_by(year, group) %>%
#   summarize(dbio_rm.mt.mean = mean(biomass_dead.removed)*0.001,
#             dbio_rm.mt.sd = sd(biomass_dead.removed)*0.001)
# 
# plot.temp <- ggplot(df.temp) +
#   geom_ribbon(aes(x = year,
#                   ymin = (dbio_rm.mt.mean - dbio_rm.mt.sd),
#                   ymax = (dbio_rm.mt.mean + dbio_rm.mt.sd),
#                   fill = group), alpha = 0.3) +
#   geom_line(aes(x = year, y = dbio_rm.mt.mean, color = group, linetype = group)) +
#   theme_light() +
#   theme(text=element_text(size=12)) +
#   scale_color_manual(values = plot_guide$color, breaks = plot_guide$group) +
#   scale_fill_manual(values = plot_guide$color, breaks = plot_guide$group) +
#   scale_linetype_manual(values = plot_guide$linetype.ix, breaks = plot_guide$group) +
#   labs(x = "Year", y = "Biomass Removed (MT)", color = "Scenario", linetype = "Scenario") +
#   guides(fill = "none")
# 
# 
# scale.temp <- layer_scales(plot.temp)$y$range$range
# scale.temp[2] <- signif(scale.temp[2], 2) + (signif(scale.temp[2], 2)/1000)
# plot.temp <- plot.temp +
#   coord_cartesian(xlim = c(min(df.temp$year), max(df.temp$year)),
#                   ylim = scale.temp,
#                   expand = FALSE, clip = "off")
# 
# ggsave(filename = paste0(path_plots.1, "Bio_rm_1.jpg"),
#        plot = plot.temp,
#        width = 9,
#        height = 4)
# 
# 
# 
# 
# # detach_package("plyr", TRUE)
# # df.temp <- firestats_cell.df[which(((paste(firestats_cell.df$treatment, firestats_cell.df$fire_id, sep = "_") %in%
# #          paste(firestats_fire.df$treatment, firestats_fire.df$fire_id, sep = "_")[which(firestats_fire.df$size_km < 3)]) == F) &
# #            (firestats_cell.df$year > 0)),]
# #
# # df.temp.1 <- firestats_cell.df %>%
# #   subset(year > 0) %>%
# #   group_by(group, treatment_rep, fire_id) %>%
# #   summarize(ck.mean = mean(crown_kill.perc),
# #             ck.sd = sd(crown_kill.perc))
# # df.temp.1 <- df.temp.1[complete.cases(df.temp.1),]
# #
# # PLOT_pdf_ckvar <- ggplot(df.temp.1) +
# #   geom_density(aes(ck.sd, fill = as.character(group)), alpha = .2) +
# #   labs(title = "Within-Fire Crown-Kill Variability",
# #        subtitle = "Density Distribution",
# #        x = "Standard Deviation", y = "Density", fill = "Treatment") +
# #   theme_light() +
# #   theme(text=element_text(size=20)) +
# #   scale_fill_manual(values = plot_guide$color,
# #                     breaks = plot_guide$group) +
# #   facet_wrap(~group) #+
# #   #scale_x_log10()
# 
# 
# ##### OTHER PLOT SCRIPTS
# # rasbas <- "FPLOTS"
# # source(paste0(path_scripts, "BATCH_rastersbasic_V4KE1.R"))
# #
# # if (grepl("AREA", toupper(unique(run_schedule$Run_handle)))){
# #   source(paste0(path_scripts, "BATCH_plots3D_V4KE4.R"))
# # }
# #
# # source(paste0(path_scripts, "BATCH_supplots_V4KE1.R"))
# 

}
