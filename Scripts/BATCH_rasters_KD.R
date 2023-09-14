if (grepl("MacBook", toString(Sys.getenv()))){
  path_scripts <- "/Users/MacBook/Desktop/Cellar/Academic/UCSB/Research/LabFees/DYNAFFORE/Model_V4/Scripts/"
} else {
  path_scripts <- "/home/kldaum/vdl/DYNAFFOR/Scripts_striker/"
}

if (exists("INIT") == F){
  module_name <- "Init"
  source(paste0(path_scripts, "BATCH_module_V4KE3.R"))
}

# list_rasters <- c("BATCH_2209010020_Agency", "BATCH_2209010020_Biomass", "BATCH_2209010020_Feasibility", "BATCH_2209010020_Foresight", "BATCH_2209010020_Interval", "BATCH_2209010020_Retreat")
# list_rasters <- unlist(lapply(path_outputs, paste0, list_rasters, "/Rasters/"))

#for (path_rasters in list_rasters){

if (exists("path_rasters") == F){
  
  cat("\n\nSelect any file in 'Rasters' directory of TEST / BATCH data\n\n")
  Sys.sleep(1)
  path_rasters <- folder.choose()
  
}

if (exists("path_plots")){
  path_save_ras <- path_plots
} else {
  path_save_ras <- path_rasters
}

path.temp <- paste0(paste(unlist(strsplit(path_rasters, "/"))[c(1:(length(unlist(strsplit(path_rasters, "/")))-1))], collapse = "/"), "/")
if ((strsplitsubr(path.temp, "/", 1, 1) != "Outputs") & dir.exists(paste0(path.temp, "Outputs/"))){
  path.temp <- paste0(path.temp, "Outputs/")
}
sens.df <- read.csv(search.files(search.files(path.temp, "Postprocess", c("is.directory", "last")), c("sensitivity"), "first"))
run_schedule <- read.csv(search.files(path.temp, "schedule", "last"))
  
# } else {
#   
#   if (exists("path_plots")){
#     path_save_ras <- path_plots
#   } else {
#     path_save_ras <- path_rasters
#   }
#   
#   path.temp <- paste0(paste(unlist(strsplit(path_rasters, "/"))[c(1:(length(unlist(strsplit(path_rasters, "/")))-1))], collapse = "/"), "/")
#   if (strsplitsubr(path.temp, "/", 1, 1) != "Outputs"){
#     path.temp <- paste0(path.temp, "Outputs/")
#   }
#   sens.df <- read.csv(search.files(search.files(path.temp, "Postprocess", "last"), c("sensitivity"), "first"))
#   run_schedule <- read.csv(search.files(path.temp, "schedule", "last"))
# }

years.spin <- gen_spinup(run_schedule)

background.ras <- as.matrix(list.blank[[1]])
rownames(background.ras) <- c(dim(background.ras)[1]:1)
colnames(background.ras) <- c(1:dim(background.ras)[2])
background.ras <- melt(background.ras)
background.ras[which(background.ras$value == 1),"value"] <- 0
background.ras <- background.ras[(complete.cases(background.ras) == T),]

raster_dict <- c("pftfin",            "PFT Distribution\nMode of final xSPINUPx years (xANALYSIS.SUBx)",     "PFT",
                 "biomassdead",       "Dead Biomass\nxANALYSISx of final xSPINUPx years (xANALYSIS.SUBx)",   "kg/km^2",
                 "biomasslive",       "Live Biomass\nxANALYSISx of final xSPINUPx years (xANALYSIS.SUBx)",   "kg/km^2",
                 "deadbio",           "Dead Biomass\nMean of all xYEARSx years",                      "kg/km^2",
                 "livebio",           "Live Biomass\nMean of all xYEARSx years",                      "kg/km^2",
                 
                 "fire",              "Cumulative Number of Fires in xYEARSx years",                  "Count",
                 "fireprob",          "Mean Fire Probability for all xYEARSx years",                  "Probability",
                 
                 "grassage",          "Grass/Shrubland Age (xANALYSIS.SUBx)",                         "Years",
                 #"grassto",           "Transitions to Grass/Shrub (xANALYSIS.SUBx)",                 "Count",
                 #"grassfrom",         "Transitions from Grass/Shrub (xANALYSIS.SUBx)",               "Count",
                 #"grassyear",         "Grass/Shrub Transition Year (xANALYSIS.SUBx)",                "Year",
                 
                 "treatburn",         "Latest Burn Treatment (xANALYSIS.SUBx)",                       "Year",
                 "treatthin",         "Latest Thinning Treatment (xANALYSIS.SUBx)",                   "Year",
                 
                 "treatthinrep",      "Number of Thinning Treatments (xANALYSIS.SUBx)",               "Count",
                 "treatburnrep",      "Number of Burn Treatments (xANALYSIS.SUBx)",                   "Count",
                 
                 "connectivity",      "Mean Connectivity Value",                                      "Connectivity",
                 
                 "fireseverity",      "Mean Fire Severity (xANALYSIS.SUBx)",                          "Severity",
                 "decomp",            "Mean Annual Carbon Loss to Decomposition",                     "Metric Tonnes",
                 "combusted",         "Mean Annual Carbon Loss to Combustion",                        "Metric Tonnes",
                 "combustedlive",     "Mean Annual Live Carbon Loss to Combustion",                   "Metric Tonnes",
                 "combusteddead",     "Mean Annual Dead Carbon Loss to Combustion",                   "Metric Tonnes",
                 "fluxpos",           "Mean Annual Carbon Loss to Atmosphere",                        "Metric Tonnes",
                 "fluxneg",           "Mean Annual Carbon Acquisition to Biomass",                    "Metric Tonnes",
                 "fluxnet",           "Mean Annual Net Carbon Flux",                                  "Metric Tonnes",
                 "crownkillcumu",     "Cumulative Crown-Kill in Fire Events",                         "Percent (cumulative)",
                 "stressed",          "Mean Stress",                                                  ""
                 # "thin",              "Number of Thinning Treatments",               "Count",
                 # "burn",              "Number of Burn Treatments",                   "Count"
)

raster_dict <- data.frame(cbind(raster_dict[seq(from = 1, by = 3, to = length(raster_dict)-2)],
                                raster_dict[seq(from = 2, by = 3, to = length(raster_dict)-1)],
                                raster_dict[seq(from = 3, by = 3, to = length(raster_dict))]))
names(raster_dict) <- c("file", "title", "fill")

if (sum(grepl("rasplots.csv", list.files(path_rasters))) > 0){
  
  rasters.df <- read.csv(paste0(path_rasters, "rasplots.csv"))
  cat(paste0("\nRaster Directory found...\n\n"))
  if (nrow(rasters.df) == 0){
    rm("rasters.df")
    cat(paste0("\nRaster Directory empty(!)\n\n"))
  } else {
    if (unique(rasters.df$runname) == 0){
      rm("rasters.df")
      cat(paste0("\nRaster Directory empty(!)\n\n"))
    }
  }
  
}
  
if (exists("rasters.df") == F){
  
  cat(paste0("\nCompiling Raster Bricks...\n\n"))
  
  rasters.df <- data.frame(matrix(0, ncol = 16, nrow = 2))
  names(rasters.df) <- c("index", "group", "type", "runname", "rastype", "analysis", "analysis.sub", "fill", "plotname", "title", "subtitle", "bmax", "bmin", "path", "error_gen", "error_print")

  index <- "TEST"
  
  # if (length(gsub("/", "", gsub(path_rasters, "", search.files(path_rasters, c(".tif", "!.tif.", "!Run"), "")))) == 0){
  rasters <- search.files(path_rasters, c("Run", ".tif", ".tiff", "!.tiff.", "!.tif.", "!.aux", "!.xml"), "")
  rasters <- rasters[grep("^Run", unlist(lapply(rasters, strsplitsubr, "/", 1, 1)))]
  if (length(rasters) > 0){
    
    cat(paste0("\n", (length(rasters)), " found.\n\n"))
    
    rasters <- cbind(rasters, unlist(lapply(X = rasters, FUN = strsplitsubr, "/", 1, 1)))
    rasters <- data.frame(cbind(index, rasters, substr(rasters[,2], 1, 6), substrRight(rasters[,2], -7)))
    names(rasters) <- c("index", "path", "file", "run", "type")
    
    run.ix <- sens.df %>% dplyr::select(runid, treatment_val)
    
    if (grepl("Run", run.ix[1,1]) == F){
      run.ix[,1] <- unlist(lapply("Run", paste0, sprintf("%03d", run.ix[,1])))
    }
    names(run.ix) <- tolower(names(run.ix))
    run.ix$group <- match(run.ix[,2], unique(run.ix[,2]))
    
    for (g in unique(run.ix$group)){
      cat(paste0("\nGenerating Rasters for Scenario: ", unique(run.ix$treatment_val)[g], "\n\n"))
      for (i in c(1:length(unique(rasters$type)))){
        
        cat(paste0("File = ", unique(rasters$type)[i], 
                   "\nGroup = ", g, " (", unique(run.ix$treatment_val[which(run.ix$group == g)]), ")", 
                   "\nType = ", i, " (", unlist(strsplit(unique(rasters$type)[i], ".", fixed = T))[1], ")"))
        
        rm(list = ls()[grep("ras.temp", ls())])
        for (name in names(rasters.df)){
          assign(name, NA)
        }
        
        r <- unique(rasters$type)[i]
        
        paths.temp <- unique(unlist(lapply(path_rasters, paste0, lapply(run.ix$runid[which(run.ix$group == g)], paste0, r))))
        paths.temp <- paths.temp[unlist(lapply(paths.temp, file.exists))]
        
        if (sum(unlist(lapply(paths.temp, file.exists))) > 0){
          if (strsplitsubr(paths.temp[1], "/", 1, 1) != "Rasters"){
            
            files.temp <- unlist(lapply(paths.temp, strsplitsubr, "/", 1, 1))
            
            # Get run details for rasters.df
            if (grepl("PFTfin", r)){
              rowct <- 3
            } else {
              rowct <- 2
            }
            
            if (i > 1){
              row <- c((nrow(rasters.df)+1):(nrow(rasters.df)+rowct))
              rasters.df <- append0(rasters.df, x = 0, n = rowct)
              # row <- c(nrow(rasters.df)+1, nrow(rasters.df)+2)
              # rasters.df <- rbind(rasters.df, 0, 0)
            } else {
              row <- c(1:rowct)
            }
            
            rasters.df$group[row] <- g
            rasters.df$type[row] <- i
            
            rastype <- tolower(gsub(c(".tif", ".tiff")[unlist(lapply(c("\\.tif\\b", "\\.tiff\\b"), grepl, files.temp[1]))], "", unlist(strsplit(files.temp[1], "Ras"))[2]))
            
            if (grepl("run", rastype)){
              rastype <- unlist(strsplit(tolower(gsub('[0-9]+', '', rastype)), "_"))
              rastype <- paste(rastype[which((rastype != "run") & (rastype != "ras"))], collapse = "")
            }
            
            if (grepl("mean", substrRight(rastype, 4))){
              analysis <- "mean"
            } else {
              if(grepl("sd", substrRight(rastype, 4))){
                analysis <- "sd"
              } else {
                if(grepl("mode", substrRight(rastype, 4))){
                  analysis <- "mode"
                } else {
                  analysis <- ""
                }
              }
            }
            
            if (nchar(analysis) > 0){
              rastype <- substr(rastype, 1, nchar(rastype)-nchar(analysis))
            }
            
            runname <- unique(run.ix$treatment_val[which(run.ix$group == g)])
            title <- raster_dict[which(raster_dict$file == rastype), "title"]
            fill <- raster_dict[which(raster_dict$file == rastype), "fill"]
            if (("spin_up" %in% names(sens.df)) == F){
              sens.df <- apply_spinup(sens.df, run_schedule)
            }
            
            if ("group" %in% names(sens.df) == F){
              sens.df$group <- sens.df$treatment_val
            }
            spinupx <- min(sens.df %>% subset(group == runname) %>% subset(spin_up == F) %>% dplyr::select(year))
            
            if (length(which(raster_dict$file == rastype)) > 0){
              title <- raster_dict$title[which(raster_dict$file == rastype)]
              fill <- raster_dict$fill[which(raster_dict$file == rastype)]
            } else {
              title <- filename
              fill <- ""
              cat(paste0("\nName not found...\n"))
            }
            
            run.treat <- unique(run.ix$treatment_val)[g]
            if (grepl("control", tolower(run.treat))){
              run.treat <- unique(run_schedule$Run_group)[grepl("control", tolower(unique(run_schedule$Run_group)))]
            }
            
            yrs <- unique(years.spinup$Max[which(tolower(years.spinup$Group) == tolower(run.treat))])
            title <- gsub("xSPINUPx", as.character(spinupx), gsub("xYEARSx", as.character(yrs), gsub("xANALYSISx", as.character(analysis), title, fixed = T)))
            n <- sum(grepl(runname, run_schedule$Run_name))
            runname <- gsub("*", "", runname, fixed = T)
            subtitle <- paste0("Treatment = ", runname, "\nn = ", n)
            
            # Flag process for user
            cat(paste0("\nGenerating rasters for\nRun: ", runname,
                       "\nRaster: ", rastype, 
                       "\nAnalysis: ", analysis, 
                       "\n"))
            
            # Populate rasters.df
            rasters.df$index[row] <- index
            rasters.df$runname[row] <- runname
            rasters.df$rastype[row] <- rastype
            rasters.df$analysis[row] <- analysis
            rasters.df$fill[row] <- fill
            rasters.df$title[row] <- title
            rasters.df$subtitle[row] <- subtitle
            
            for (p in c(1:length(paths.temp))){
              assign(paste0("ras.temp.", p), raster(paths.temp[p]))
            }
            
            brk <- brick(stack(lapply(ls()[grep("ras.temp.", ls(), fixed = T)], get)))
            
            if (grepl("PFT", r)){
              
              # Raster of intraensemble PFT mode
              rasters.df$plotname[row[1]] <- paste0("RAS_", rastype, analysis, "_", runname, ".mode")
              rasters.df$title[row[1]] <- gsub("xANALYSIS.SUBx", "mode", rasters.df$title[row[1]])
              rasters.df$title[row[1]] <- gsub("()", "", rasters.df$title[row[1]])
              rasters.df$analysis.sub[row[1]] <- "mode"
              path_rasters.1 <- paste0(path_rasters, gsub(" ", "", str_to_title(paste(unlist(strsplit(gsub(".tif", paste0(unique(run.ix$treatment_val[which(run.ix$group == g)]), "Mean.tif"), r), "_")), collapse = " "))))
              
              brk.mean <- calc(brk, getmode)
              if (length(array_vals3(as.matrix(brk.mean), "unique")) == 2){
                cat(paste0("BLANK RASTER (mean)\n"))
                rasters.df$error_gen[row[1]] <- "insufficient values (blank)"
              } else {
                rasters.df$error_gen[row[1]] <- "none"
                rasters.df$path[row[1]] <- path_rasters.1
                raster::writeRaster(brk.mean, path_rasters.1, overwrite = T)
              }
              
              # Raster of intraensemble PFT discord
              rasters.df$plotname[row[2]] <- paste0("RAS_", rastype, analysis, "_", runname, ".uniq")
              rasters.df$title[row[2]] <- gsub("xANALYSIS.SUBx", "unique", rasters.df$title[row[2]])
              rasters.df$title[row[2]] <- gsub("()", "", rasters.df$title[row[2]])
              rasters.df$analysis.sub[row[2]] <- "uniq"
              path_rasters.1 <- paste0(path_rasters, gsub(" ", "", str_to_title(paste(unlist(strsplit(gsub(".tif", paste0(unique(run.ix$treatment_val[which(run.ix$group == g)]), "SD.tif"), r), "_")), collapse = " "))))
              
              brk.uniq <- calc(brk, uniqcount)
              ras.temp <- melt(as.matrix(brk.uniq))
              
              if (length(unique(ras.temp$value[complete.cases(ras.temp)])) == 1){
                
                cat(paste0("BLANK RASTER (SD)\n"))
                rasters.df$error_gen[row[2]] <- "insufficient values (blank)"
                
              } else {
                
                # Calculate breaks in scale (bmin & bmax)
                b.rows <- which((rasters.df$rastype == rastype) & (rasters.df$analysis == analysis) & (rasters.df$analysis.sub == "uniq"))
                
                b <- bvals(ras.temp = ras.temp)
                
                bmin <- b[1]
                bmin.temp <- min(rasters.df$bmax[b.rows])
                if (bmin < bmin.temp){
                  rasters.df$bmin[b.rows] <- bmin
                }
                
                bmax <- b[2]
                bmax.temp <- max(rasters.df$bmax[b.rows])
                if (bmax > bmax.temp){
                  rasters.df$bmax[b.rows] <- bmax
                }
                
                raster::writeRaster(brk.uniq, path_rasters.1, overwrite = T)
                rasters.df$error_gen[row[2]] <- "none"
                rasters.df$path[row[2]] <- path_rasters.1
                
              }
              
              # Raster of intraensemble PFT discord W/O GRASS
              brk <- lapply(lapply(lapply(lapply(ls()[grep("ras.temp.", ls(), fixed = T)], get), as.matrix), function(x) replace(x,x==0,NA)), raster)
              brk <- brick(stack(brk))
              rasters.df$plotname[row[3]] <- paste0("RAS_", rastype, analysis, "_", runname, ".uniq0")
              rasters.df$title[row[3]] <- gsub("xANALYSIS.SUBx", "unique", paste0(rasters.df$title[row[3]], "\nexcluding grass/shrub"))
              rasters.df$title[row[3]] <- gsub("()", "", rasters.df$title[row[3]])
              rasters.df$analysis.sub[row[3]] <- "uniq0"
              path_rasters.1 <- paste0(path_rasters, gsub(" ", "", str_to_title(paste(unlist(strsplit(gsub(".tif", paste0(unique(run.ix$treatment_val[which(run.ix$group == g)]), "SD0.tif"), r), "_")), collapse = " "))))
              
              brk.uniq0 <- calc(brk, uniqcount)
              ras.temp <- melt(as.matrix(brk.uniq0))
              
              if (length(unique(ras.temp$value[complete.cases(ras.temp)])) == 1){
                
                cat(paste0("BLANK RASTER (uniq0)\n"))
                rasters.df$error_gen[row[3]] <- "insufficient values (blank)"
                
              } else {
                
                # Calculate breaks in scale (bmin & bmax)
                b.rows <- which((rasters.df$rastype == rastype) & (rasters.df$analysis == analysis) & (rasters.df$analysis.sub == "uniq0"))
                
                b <- bvals(ras.temp = ras.temp)
                
                bmin <- b[1]
                bmin.temp <- min(rasters.df$bmax[b.rows])
                if (bmin < bmin.temp){
                  rasters.df$bmin[b.rows] <- bmin
                }
                
                bmax <- b[2]
                bmax.temp <- max(rasters.df$bmax[b.rows])
                if (bmax > bmax.temp){
                  rasters.df$bmax[b.rows] <- bmax
                }
                
                raster::writeRaster(brk.uniq0, path_rasters.1, overwrite = T)
                rasters.df$error_gen[row[3]] <- "none"
                rasters.df$path[row[3]] <- path_rasters.1
                
              }
              
            } else {
              
              # Raster of intraensemble mean
              rasters.df$plotname[row[1]] <- paste0("RAS_", rastype, analysis, "_", runname, ".mean")
              rasters.df$title[row[1]] <- gsub("xANALYSIS.SUBx", "mean", rasters.df$title[row[1]])
              rasters.df$title[row[1]] <- gsub("()", "", rasters.df$title[row[1]])
              rasters.df$analysis.sub[row[1]] <- "mean"
              path_rasters.1 <- paste0(path_rasters, gsub(" ", "", str_to_title(paste(unlist(strsplit(gsub(".tif", paste0(unique(run.ix$treatment_val[which(run.ix$group == g)]), "Mean.tif"), r), "_")), collapse = " "))))
              
              brk.mean <- calc(brk, mean)
              ras.temp <- melt(as.matrix(brk.mean))
              
              if (length(unique(ras.temp$value[complete.cases(ras.temp)])) == 1){
                
                cat(paste0("BLANK RASTER (mean)\n"))
                rasters.df$error_gen[row[1]] <- "insufficient values (blank)"
                
              } else {
              
                # Calculate breaks in scale (bmin & bmax)
                b.rows <- which((rasters.df$rastype == rastype) & (rasters.df$analysis == analysis) & (rasters.df$analysis.sub == "mean"))
                
                b <- bvals(ras.temp = ras.temp)
                
                bmin <- b[1]
                bmin.temp <- min(rasters.df$bmax[b.rows])
                if (bmin < bmin.temp){
                  rasters.df$bmin[b.rows] <- bmin
                }
                
                bmax <- b[2]
                bmax.temp <- max(rasters.df$bmax[b.rows])
                if (bmax > bmax.temp){
                  rasters.df$bmax[b.rows] <- bmax
                }
                
                rasters.df$bmax[row[1]] <- bmax
                rasters.df$bmin[row[1]] <- bmin
                
                raster::writeRaster(brk.mean, path_rasters.1, overwrite = T)
                rasters.df$error_gen[row[1]] <- "none"
                rasters.df$path[row[1]] <- path_rasters.1
              
              }
              # # Raster of intraensemble standard deviation
              # rasters.df$plotname[row[2]] <- paste0("RAS_", rastype, analysis, "_", runname, ".sd")
              # rasters.df$title[row[2]] <- gsub("xANALYSIS.SUBx", "sd", rasters.df$title[row[2]])
              # rasters.df$title[row[2]] <- gsub("()", "", rasters.df$title[row[2]])
              # rasters.df$analysis.sub[row[2]] <- "sd"
              # path_rasters.1 <- paste0(path_rasters, gsub(" ", "", str_to_title(paste(unlist(strsplit(gsub(".tif", paste0(unique(run.ix$treatment_val[which(run.ix$group == g)]), "SD.tif"), r), "_")), collapse = " "))))
              # rasters.df$path[row[2]] <- path_rasters.1
              # 
              # brk.sd <- calc(brk, sd)
              # ras.temp <- melt(as.matrix(brk.sd))
              # b <- ceiling(max(ras.temp$value[complete.cases(ras.temp) == T])/10)*10
              # b.rows <- which((rasters.df$rastype == rastype) & (rasters.df$analysis == analysis) & (rasters.df$analysis.sub == "sd"))
              # b.temp <- max(rasters.df$bmax[b.rows])
              # if (b > b.temp){
              #   rasters.df$bmax[b.rows] <- b
              # }
              # 
              # raster::writeRaster(brk.sd, path_rasters.1, overwrite = T)
            }
            cat(paste0("\n"))
          }
        } else {
          rasters.df$error_gen[row] <- "Files not found"
          cat(paste0("\nFiles not found\n\n"))
        }
      }
    }
  }
  
  rasters.df <- rasters.df[order(rasters.df$type),]
  rasters.df <- rasters.df[order(rasters.df$group),]
  rasters.df$index <- "TEST"
  rasters.df <- rasters.df[which(rasters.df$plotname != 0),]   
  cat(paste0("\n\n\nNumber of rasters compiled: ", nrow(rasters.df), "\n"))
  write.csv(rasters.df, paste0(path_save_ras, "/rasplots.csv"))
  
}
# zip(zipfile = paste0(path_save_ras, "RASTERS.zip"), files = search.files(path_save_ras, "Run", ""))
# 
# for (file in search.files(path_save_ras, "Run", "")){
#   file.remove(file)
# }

########### RASTERS ###########

background.ras <- as.matrix(list.blank[[1]])
rownames(background.ras) <- c(dim(background.ras)[1]:1)
colnames(background.ras) <- c(1:dim(background.ras)[2])
background.ras <- melt(background.ras)
background.ras[which(background.ras$value == 1),"value"] <- 0
background.ras <- background.ras[(complete.cases(background.ras) == T),]

## outline
#outline.ras <- fortify(rasterToPolygons(raster(blank.ras), dissolve=TRUE))
# outline.ras <- rasterToPolygons(raster(blank.ras), dissolve=TRUE)
# crs(outline.ras) <- crs(r2)
# outline.ras <- sf::st_as_sf(outline.ras)
# extent(outline.ras) <- extent(r2)
# outline.ras <- setExtent(outline.ras, extent(r2))

rasters.df$index[which(grepl("CONTROL", rasters.df$runname))] <- "CONTROL"
rasters.df$index[which(grepl("CONTROL", rasters.df$runname) == F)] <- "TEST"
#rasters.df$path <- unlist(lapply(path_outputs, paste0, lapply(lapply(rasters.df$path, strsplitsubr, "/", 4, 4), paste, collapse = "/")))
rasters.df$path <- gsub(path_outputs.alt, path_outputs, rasters.df$path)
rasters.df$error_print <- "Not printed"
rasters.df$name <- ""

raspaths <- gsub("//", "/", search.files(path_rasters, c(".tif", "!.xml"), ""))
ext_files <- c(".tif", ".tiff")[unlist(lapply(c("\\.tif\\b", "\\.tiff\\b"), grepl, raspaths[1]))]
ext_df <- c(".tif", ".tiff")[unlist(lapply(c("\\.tif\\b", "\\.tiff\\b"), grepl, rasters.df$path[grep(".tif", rasters.df$path)[1]]))]

if (ext_files != ext_df){
  rasters.df$path <- gsub(ext_df, ext_files, rasters.df$path, fixed = T)
}

rasters.df$name <- ""
for (r in c(1:nrow(rasters.df))){
  
  path <- rasters.df$path[r]
  
  if (file.exists(path) == F){
    path <- rasters.df$path[r]
    name <- strsplitsubr(path, "/", 1, 1)
    path <- search.files(path_rasters, c(name, "!.aux", "!.xml"), c("ignore.case", "standardize.ext", "first"))
  }
  
  if (length(path) > 0){
    name <- strsplitsubr(path, "/", 1, 1)
    rasters.df$path[r] <- path
    rasters.df$name[r] <- name
    rasters.df$error_gen[r] <- "none"
  } else {
    rasters.df$path[r] <- "9999"
    rasters.df$name[r] <- "9999"
    rasters.df$error_gen[r] <- "File not saved"
  }
  
}

cat(paste0("\n\nSuccessful compilations: ", length(which(as.character(rasters.df$path) != "9999")), "\n\n"))

rasters.df <- rasters.df[order(rasters.df$rastype),]
row.names(rasters.df) <- c(1:nrow(rasters.df))
write.csv(rasters.df, paste0(path_save_ras, "/rasplots.csv"))

while (!is.null(dev.list()))  dev.off()

pdf(paste0(path_rasters, "/Rasters",".pdf"), onefile = TRUE, width = 8.5, height = 11)
for (r in c(1:nrow(rasters.df))){
  
  # if (r %in% c(1, seq(from = 20, to = nrow(rasters.df), by = 20))){
  #   while (!is.null(dev.list()))  dev.off()
  #   pdf(paste0(path_rasters, "/Rasters_", which(c(1, seq(from = 20, to = nrow(rasters.df), by = 20)) == r),".pdf"), onefile = TRUE, width = 8.5, height = 11)
  # }
  
  for (name in names(rasters.df)){
    assign(name, rasters.df[r,name])
  }
  
  # name <- strsplitsubr(path, "/", 1, 1)
  # path <- search.files(path_rasters, c(name, "!.aux", "!.xml"), c("ignore.case", "standardize.ext"))
  # name <- strsplitsubr(path, "/", 1, 1)
  
  if ((name %in% c("9999", "", "0")) == F){
    
    cat(paste0("\nPrinting Raster: ", plotname, "\n"))
    
    if ((grepl("PFT", toupper(rastype))) & (analysis.sub == "mode")){
      
      ras.temp <- as.matrix(raster(path))
      ras.temp[is.na(list.blank[[1]])] <- NA
      ras.temp <- melt(ras.temp)
      
      ras.temp$value <- pft_names$Name[match(ras.temp$value, pft_names$ID)]
      colors <- pft_names$color[match(sort(unique(ras.temp$value)), pft_names$Name)]
      
      plot.temp <- ggplot() +
        geom_tile(data = background.ras, aes(x = Var2, y = Var1), fill = "black") +
        geom_tile(data = ras.temp, aes(x = Var2, y = Var1[c(nrow(ras.temp):1)], fill=value), na.rm = T) +
        theme_light() +
        theme(axis.text = element_blank()) +
        scale_fill_manual(values = colors) +
        labs(x = "", y = "", fill = fill, title = paste0(title),
             subtitle = paste0(subtitle)) +
        theme(text=element_text(size=20))
      
      assign(plotname, plot.temp)
      print(plot.temp)
      
      rasters.df$error_print[r] <- "none"
      
    } else {
      
      ras.temp <- as.matrix(raster(path))
      #ras.temp[is.na(list.blank[[1]])] <- 0
      if (sum(is.na(array_vals3(ras.temp, "unique")) == F) < 2){
        cat(paste0("PRINTING FAILED: insufficient unique values\n"))
        rasters.df$error_print[r] <- "insufficient unique values"
      } else {
        if (toString(dim(ras.temp)) == toString(dim(list.blank[[1]]))){
          ras.temp[is.na(list.blank[[1]])] <- NA
          plot.temp <- ggplot() +
            geom_tile(data = background.ras, aes(x = Var2, y = Var1), fill = "black")
        } else {
          plot.temp <- ggplot()
        }
        ras.temp <- melt(ras.temp)
        
        #if (length(unique(ras.temp$value)[is.na(unique(ras.temp$value)) == F]) > 1){
        
        bmax <- as.numeric(max(rasters.df$bmax[which((rasters.df$rastype == rastype) & (rasters.df$analysis == analysis) & (rasters.df$analysis.sub == analysis.sub))]))
        bmin <- as.numeric(min(rasters.df$bmin[which((rasters.df$rastype == rastype) & (rasters.df$analysis == analysis) & (rasters.df$analysis.sub == analysis.sub))]))
        if (bmax == 0){
          bmax <- 1
        }
        
        #colors.temp <- c("darkseagreen2", "lemonchiffon1", "gold1", "brown2", "darkred", "purple")
        colors.temp <- c("darkseagreen1", "gold2", "brown2", "darkred", "purple")
        
        plot.temp <- plot.temp + 
          geom_tile(data = background.ras, aes(x = Var2, y = Var1), fill = "black") +
          geom_tile(data = ras.temp, aes(x = Var2, y = Var1[c(nrow(ras.temp):1)], fill=value), na.rm = T) +
          #geom_sf(data = outline, size = 1.5, col = "gold", fill = NA) +
          theme_light() +
          theme(axis.text = element_blank()) +
          scale_fill_gradientn(limits = c(bmin, bmax),
                               #colours=c("mistyrose", "brown1", "darkmagenta"),
                               colours=colors.temp,
                               breaks=c(bmin, bmax),
                               na.value = "transparent" ) +
          labs(x = "", y = "", fill = fill, title = paste0(title),
               subtitle = paste0(subtitle)) +
          theme(text=element_text(size=20))
        
        if (substr(strsplitsubr(path, "/", 1, 1), 4, 7) == "flux"){
          colors.temp <- c("aquamarine", "white", "red")[list(c(1:3), c(3:1))[[(grepl("fluxneg", name) + 1)]]]
          plot.temp <- plot.temp + scale_fill_gradientn(limits = c(-bmax, bmax),
                                                        colours = colors.temp,
                                                        breaks = c(-bmax, bmax),
                                                        na.value = "transparent")
        }
        
        assign(plotname, plot.temp)
        print(plot.temp)
        
        rasters.df$error_print[r] <- "none"
        
        #}
      }
    }
  } else {
    cat(paste0("\n!! FILE NOT FOUND: ", plotname, "\n"))
    rasters.df$error_print[r] <- "file not found"
  }
  
  # if (r %in% c(seq(from = 19, to = nrow(rasters.df), by = 20))){
  #   while (!is.null(dev.list()))  dev.off()
  #   pdf(paste0(path_rasters, "/Rasters_", which(c(1, seq(from = 20, to = nrow(rasters.df), by = 20)) == r),".pdf"), onefile = TRUE, width = 8.5, height = 11)
  # }
  
}

while (!is.null(dev.list()))  dev.off()

# GENERIC RASTERS
dbio.ras <- search.files(path_rasters, c("dead", "control", "!sd.", "!aux.xml"), "ignore.case")
if (length(dbio.ras) == 0){
  dbio.ras <- search.files(path_rasters, c("dead", "!sd.", "!aux.xml"), "")[1]
}
dbio.ras <- mask_landscape(blank.ras, as.matrix(raster(dbio.ras[1])))

lbio.ras <- search.files(path_rasters, c("live", "control", "!sd.", "!aux.xml"), "ignore.case")
if (length(dbio.ras) == 0){
  lbio.ras <- search.files(path_rasters, c("live", "!sd.", "!aux.xml"), "")[1]
}
lbio.ras <- mask_landscape(blank.ras, as.matrix(raster(lbio.ras[1])))

conn.ras <- search.files(path_rasters, c("connectivitymean", "control", "!sd.", "!aux.xml"), "ignore.case")
if (length(dbio.ras) == 0){
  conn.ras <- search.files(path_rasters, c("connectivitymean", "!sd.", "!aux.xml"), "")[1]
}
conn.ras <- mask_landscape(blank.ras, as.matrix(raster(conn.ras[1])))

agency1.ras <- agency.ras
agency1.ras[1] <- "TEMP"
for (a in agency.df$ID){
  agency1.ras[which(agency.ras == as.character(a))] <- agency.df$Agency[which(agency.df$ID == a)]
}

# Get "treatable" rasters
if (length(search.files(path_rasters, "Treatable", "")) == 0){
  treat_factors_static <- c("roads", "slope", "strikes", "WUI", "PET", "agency")
  for (r in c(1:nrow(params.df))){
    for (v in names(params.df[grep("P_", names(params.df))])){
      assign(v, params.df[r,v])
    }
    for (i in c(1:length(treat_factors_static))){
      ras.temp <- list.blank[[1]]
      ras <- get(paste0(treat_factors_static[i], ".ras"))
      quant <- gsub("9999", ">=0", get(paste0("P_quant_", treat_factors_static[i])))
      
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
      assign(paste0(treat_factors_static[i], ".ix"), which(ras.temp == 1))
      assign(paste0(treat_factors_static[i], ".rank"), ras.temp)
    }
    vec.temp <- which(unlist(lapply(lapply(lapply(treat_factors_static, paste0, ".ix"), get), length)) > 0)
    array.temp <- lapply(lapply(treat_factors_static[vec.temp], paste0, ".rank"), get)
    treatable.ras <- Reduce("+", array.temp) == length(vec.temp)
    raster::writeRaster(raster(treatable.ras), paste0(path_rasters, "Treatable_", params.df$Run_group[r], ".tiff"), overwrite = T)
  }
}

names.temp <- search.files(path_rasters, c("Treatable", "!.aux", "!.xml"), "")
names.temp.1 <- gsub(".tif", "", gsub(".tiff", "", unlist(lapply(lapply(names.temp, strsplitsubr, "/", 1, 1), strsplitsubr, "_", 1, 1))))
treatable.arr <- lapply(lapply(names.temp, raster), as.matrix)
names.temp.2 <- unlist(lapply("treatable_", paste0, names.temp.1, ".ras"))
titles.temp <- unlist(lapply("Areas Eligible for Fuels Treatments\nin Scenario: '", paste0, names.temp.1, "'"))
units.temp <- rep("Eligible\nAreas", length(names.temp))
colors.temp <- rep("darkseagreen2", length(names.temp))
for (i in c(1:length(treatable.arr))){
  assign(names.temp.2[i], treatable.arr[[i]] == 1)
}

# Define plot characteristics
rasters <- c("roads.ras", "slope.ras", "WUI.ras", "lbio.ras", "dbio.ras", "conn.ras", "agency1.ras", names.temp.2)
units <- c("km         ", "degrees    ", "km         ", "kg/km^2    ", "kg/km^2    ", "Connectivity", "Name", units.temp)
titles <- c("Distance from Roads", "Slope", "Distance from Homes", "Live Biomass", "Dead Biomass", "Connectivity", "Land Management Agency", titles.temp)
color2 <- c("goldenrod", "cyan", "peachpuff2", "springgreen1", "lightsalmon1", "aquamarine", "darkolivegreen4", colors.temp)

forest_boundary <- st_read("/Volumes/LaCie/Geospatial/EXTENT_forest_boundary.shp")
# dem.ras <- raster(paste0("/Volumes/LaCie/Geospatial/DEM_CANVOR.tif"))
# crs(dem.ras) <- crs(forest_boundary)
# extent(dem.ras) <- extent(forest_boundary) 

while (!is.null(dev.list()))  dev.off()
pdf(paste0(path_rasters, "/RastersBasic.pdf"), onefile = TRUE, width = 8.5, height = 11)
for (i in c(1:length(rasters))){
  if (length(get(rasters[i])) > 0){
    if (grepl("agency", rasters[i]) == F){
      # DEM Plot
      ras.temp <- get(rasters[i])
      ras.temp[which(is.na(list.blank[[1]]))] <- 0
      sum(ras.temp)
      ras.temp <- raster(ras.temp)

      crs(ras.temp) <- crs(forest_boundary)
      extent(ras.temp) <- extent(forest_boundary)
      writeRaster(ras.temp, paste0(path_rasters, "/BasicGeoTif_", gsub(".ras", "", rasters[i]), ".tif"), overwrite=TRUE)
    }
#   }
# }
    # ras.temp.sp <- rasterToPoints(ras.temp, spatial=TRUE)
    # dem.sp <- rasterToPoints(dem, spatial=TRUE)
    # y = st_as_sf(r.pts, as_points = TRUE)
    
    # Standard plot
    ras.temp <- get(rasters[i])
    ras.temp[is.na(list.blank[[1]])] <- NA
    ras.temp <- melt(ras.temp)
    
    colors <- c("black", color2[i])
    if ((rasters[i] %in% c(names.temp.2, "conn.ras", "lbio.ras", "dbio.ras")) == F){
      # if ((grepl("control", tolower(rasters[i])) == F) & (rasters[i] %in% names.temp.2)){
      #   ras.temp$value <- ras.temp$value == F
      # } else {
        colors <- colors[c(2,1)]
      #}
    }
    
    plot.temp <- ggplot() +
      geom_tile(data = background.ras, aes(x = Var2, y = Var1), fill = "black") +
      geom_tile(data = ras.temp, aes(x = Var2, y = Var1[c(nrow(ras.temp):1)], fill=value), na.rm = T) +
      theme_light() +
      theme(axis.text = element_blank()) +
      labs(x = "", y = "", fill = units[i], title = titles[i]) +
      theme(text=element_text(size=20))
    
    if (rasters[i] == "agency1.ras"){
      plot.temp <- plot.temp + scale_fill_manual(values = c("palegreen3", 
                                                            "khaki",
                                                            "lightseagreen",
                                                            "aquamarine2",
                                                            "darkolivegreen4",
                                                            "darkgreen",
                                                            "pink"),
                                                 na.value = "white" )
    } else {
      if (rasters[i] %in% names.temp.2){
        plot.temp <- plot.temp +
          scale_fill_manual(values = colors, na.value = "transparent" )
      } else {
        plot.temp <- plot.temp +
          scale_fill_gradientn(colours=colors, na.value = "transparent" )
      }
    }
    
    assign(paste0("RAS_basic_", rasters[i]), plot.temp)
    print(plot.temp)
    cat(paste0("\nPrinting: ", rasters[i], "\n"))
    
  }
}
while (!is.null(dev.list()))  dev.off()


# # C Flux bars (piggybacking on rasters)
# 
# df.temp <- rbind(data.frame(cbind("Dead Biomass Decomposed", c(1:length(unlist(array_vals3(decompose, "sort")))), unlist(array_vals3(decompose, "sort")))),
#                  data.frame(cbind("Dead Biomass Combusted", c(1:length(unlist(array_vals3(combusted.dead, "sort")))), unlist(array_vals3(combusted.dead, "sort")))),
#                  data.frame(cbind("Live Biomass Combusted", c(1:length(unlist(array_vals3(combusted.live, "sort")))), unlist(array_vals3(combusted.live, "sort")))),
#                  data.frame(cbind("Net Source (Combust + Decomp)", c(1:length(unlist(array_vals3(flux.pos, "sort")))), unlist(array_vals3(flux.pos, "sort")))),
#                  data.frame(cbind("Net Sink (∆ Live Biomass pre-Fire)", c(1:length(unlist(array_vals3(flux.neg, "sort")))), unlist(array_vals3(flux.neg, "sort"))))#,
#                  #data.frame(cbind("Net Flux (Source + Sink)", c(1:length(unlist(array_vals3(flux.net, "sort")))), unlist(array_vals3(flux.net, "sort"))))
# )


########### VALIDATION ############
cat(paste0("\n\nPrinting Validation Rasters...\n"))

background.ras <- as.matrix(list.blank[[1]])
rownames(background.ras) <- c(dim(background.ras)[1]:1)
colnames(background.ras) <- c(1:dim(background.ras)[2])
background.ras <- melt(background.ras)
background.ras[which(background.ras$value == 1),"value"] <- 0
background.ras <- background.ras[(complete.cases(background.ras) == T),]

MODIS_Forest1.ras <- as.matrix(raster(paste0(path_geospa, "MCD12Q1_LC2001_Forest.tif")))
MODIS_Forest1.ras <- rbind(NA, cbind(NA, MODIS_Forest1.ras, NA), NA)
MODIS_Forest1.ras[which(is.na(list.blank[[1]]))] <- NA

MODIS_GrassShrub1.ras <- as.matrix(raster(paste0(path_geospa, "MCD12Q1_LC2001_GrassShrub.tif")))
MODIS_GrassShrub1.ras <- rbind(NA, cbind(NA, MODIS_GrassShrub1.ras, NA), NA)
MODIS_GrassShrub1.ras[which(is.na(list.blank[[1]]))] <- NA

list_grass <- NULL
grassters <- search.files(c(path_rasters), c("Rasgrassage", "mean.tif", "!.tif.", "!.aux", "!.xml"), "ignore.case")

while (!is.null(dev.list()))  dev.off()
pdf(paste0(path_rasters, "/RastersValidation.pdf"), onefile = TRUE, width = 8.5, height = 11)
for (i in c(1:length(grassters))){
  
  path <- grassters[i]
  grass.ras <- as.matrix(raster(path))
  name <- gsub(".tif", "", gsub(".tiff", "", gsub("mean", "", gsub("Rasgrassage", "", strsplitsubr(path, "/", 1, 1)))))
  
  cat(paste0("...", name, "...\n"))
  
  group_years <- unique(sens.df$year[which(sens.df$group == unique(sens.df$group)[which(tolower(unique(sens.df$group)) == name)])])
  years.min <- as.numeric(years.spin$Spin[grepl(tolower(name), tolower(years.spin$Group))])
  years.max <- as.numeric(years.spin$Max[grepl(tolower(name), tolower(years.spin$Group))])
  
  grass.ras <- (grass.ras > years.min) + 0
  grass.ras[which((grass.ras == 1) & (MODIS_GrassShrub1.ras == 1))] <- 2
  grass.ras[which((grass.ras == 0) & (MODIS_GrassShrub1.ras == 1))] <- 3
  list_grass[[i]] <- grass.ras
  
  pft.ras <- search.files(path_rasters, c(name, "PFTfin.tif"), "ignore.case")
  if (length(pft.ras) > 0){
    pft.ras <- as.matrix(raster(pft.ras))
    bio.ras <- as.matrix(raster(search.files(path_rasters, c("Rasbiomasslive", name, "mean.tif"), "first")))
    df.temp <- melt(table(pft.ras[grass.ras == 3]))
    
    names(df.temp)[1] <- "ID"
    df.temp$pft <- pft_names$Name[match(df.temp$ID, pft_names$ID)]
    df.temp$perc <- 0
    for (r in c(1:nrow(df.temp))){
      df.temp$perc[r] <- (100*(df.temp$value[r]/sum(df.temp$value)))
    }
    df.temp.1 <- melt(pft.ras)
    
    plot.temp <- ggplot(df.temp, aes(x = "", y = value, fill = pft)) + #!!!
      geom_bar(stat = "identity", width = 1) +
      #scale_y_discrete(limits = df.temp$pft[order(as.numeric(df.temp$ID))]) +
      coord_polar("y", start = 0) + theme_light() +
      theme(axis.text = element_blank()) +
      scale_color_manual(values = pft_names$Color[match(df.temp$ID[order(df.temp$pft)], pft_names$ID)]) +
      scale_fill_manual(values = pft_names$Color[match(df.temp$ID[order(df.temp$pft)], pft_names$ID)]) +
      labs(x = "", y = "", fill = "", title = "MODIS vs. Model", subtitle = paste0("Model PFT in MODIS Non-forest\nValidation Comparison\nScenario = ", name)) +
      theme(text=element_text(size=20))
    
    #assign(paste0("PLOT_Validation2_", name), plot.temp)
    print(plot.temp)
    
  }
  
  ras.temp <- melt(grass.ras)
  if (length(unique(ras.temp$value)) < 5){
    missing <- c(0:3)[which((as.character(c(0:3) %in% unique(ras.temp$value[is.na(ras.temp$value) == F]))) == F)]
    for (m in c(1:length(missing))){
      ras.temp[(nrow(ras.temp)-m),"value"] <- missing[m]
    }
  }
  
  plot.temp <- ggplot() +
    geom_tile(data = background.ras, aes(x = Var2, y = Var1), fill = "black") +
    geom_tile(data = ras.temp, aes(x = Var2, y = Var1[c(nrow(ras.temp):1)], fill=as.character(value)), na.rm = T) +
    theme_light() +
    theme(axis.text = element_blank()) +
    # scale_fill_manual(values = c("darkgrey", "blue", "cyan", "magenta"),
    scale_fill_manual(values = c("darkgrey","cyan", "blue", "magenta"),
                      labels = c(paste0("Agreement:\nForest\n\n"), paste0("Model:\nnon-Forest\n\n"), paste0("Agreement:\nnon-Forest\n\n"), paste0("MODIS:\nnon-Forest\n\n"), "")) +
    labs(x = "", y = "", fill = "", title = "MODIS vs. Model", subtitle = paste0("Validation Comparison\nScenario = ", name)) +
    theme(text=element_text(size=20))
  
  #assign(paste0("RAS_Validation1_", name), plot.temp)
  print(plot.temp)
  
  df.temp <- melt(table(ras.temp$value))
  # df.temp <- df.temp[which(df.temp$Var1 != 0),]
  df.temp$perc <- 0
  for (r in c(1:nrow(df.temp))){
    df.temp$perc[r] <- (100*(df.temp$value[r]/sum(df.temp$value)))
  }
  
  plot.temp <- ggplot(df.temp, aes(x = "", y = value, fill = as.character(Var1))) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +    theme_light() +
    theme(axis.text = element_blank()) +
    # scale_fill_manual(values = c("blue", "cyan", "magenta"),
    #                   labels = c(paste0("Model:\nnon-Forest\n\n"),
    #                              paste0("Agreement:\nnon-Forest\n\n"),
    #                              paste0("MODIS:\nnon-Forest\n\n"), "")) +
    # scale_fill_manual(values = c("darkgrey", "blue", "cyan", "magenta"),
    scale_fill_manual(values = c("darkgrey", "cyan", "blue", "magenta"),
                      labels = c(paste0("Agreement:\nForest\n", floor(df.temp$perc[1]), "%\n"),
                                 paste0("Model:\nnon-Forest\n", floor(df.temp$perc[2]), "%\n"),
                                 paste0("Agreement:\nnon-Forest\n", floor(df.temp$perc[3]), "%\n"),
                                 paste0("MODIS:\nnon-Forest\n", floor(df.temp$perc[4]), "%\n"), "")) +
    labs(x = "", y = "", fill = "", title = "MODIS vs. Model", subtitle = paste0("Validation Comparison\nScenario = ", name)) +
    theme(text=element_text(size=20))
  
  #assign(paste0("PLOT_Validation1_", name), plot.temp)
  print(plot.temp)
  
}

while (!is.null(dev.list()))  dev.off()


######### PRINT RASTERS ########

# ras.ix <- ls()[grepl("RAS_", ls()) & (grepl("_basic_", ls()) == F)]
# while (!is.null(dev.list()))  dev.off()
# pdf(paste0(path_rasters, "/Rasters.pdf"), onefile = TRUE, width = 8.5, height = 11)
# for (i in c(1:length(ras.ix))){
#   plot.temp <- get(ras.ix[i])
#   # plot <- gsub("_", "", plot)
#   # assign(plot, plot.temp)
#   print(plot.temp)
#   cat(paste0("\nPrinting: ", ras.ix[i]))
# }
# 
# while (!is.null(dev.list()))  dev.off()
#}

cat(paste0("\n\n\nRASTERS COMPLETE\n\n\n\n"))




############## SCRATCH PAPER ################


### PFT Diff
files <- search.files(path_rasters, c("pft", "!run", "!aux.xml", "sd0"), "ignore.case")
pftfin_control.ras <- as.matrix(raster(files[grepl("CONTROL", toupper(files))]))
pftfin_control <- as.vector(pftfin_control.ras)

pftfin_diff.df <- data.frame(cbind(sort(rep(params.df$Run_group, 13)), rep(c(0:12), length(params.df$Run_group)), 0))
names(pftfin_diff.df) <- c("Scenario", "PFT_count", "Count")
pftfin_diff.df$PFT_count <- as.numeric(pftfin_diff.df$PFT_count)
pftfin_diff.df$Count <- as.numeric(pftfin_diff.df$Count)

run_groups <- params.df$Run_group
run_groups <- c(run_groups[grepl("CONTROL", toupper(run_groups))], run_groups[!grepl("CONTROL", toupper(run_groups))])

for (i in c(1:length(run_groups))){
  
  scenario <- run_groups[i]
  
  pftfin_temp.ras <- as.matrix(raster(files[grepl(toupper(scenario), toupper(files))]))
  pftfin_temp <- as.vector(pftfin_temp.ras)
  
  df.temp <- data.frame(table(pftfin_temp.ras))
  df.temp[,1] <- as.numeric(as.character(df.temp[,1]))
  df.temp[,2] <- as.numeric(as.character(df.temp[,2]))
  
  pftfin_diff.df[which((pftfin_diff.df$Scenario == scenario) & (pftfin_diff.df$PFT_count %in% df.temp[,1])), 3] <- as.numeric(df.temp[,2])
  
  if (i > 1){
    pftfin_diff.df[which(pftfin_diff.df$Scenario == scenario), 3] <- pftfin_diff.df$Count[which(pftfin_diff.df$Scenario == scenario)] - pftfin_diff.df$Count[which(pftfin_diff.df$Scenario == run_groups[1])]
  }

}

pftfin_diff.df <- pftfin_diff.df %>% subset(!grepl("CONTROL", toupper(Scenario)))

write.csv(pftfin_diff.df, paste0(path_rasters, "pftdiff.csv"))


### PFT mean
files <- search.files(path_rasters, c("pft", "!run", "!aux.xml", "mean"), "ignore.case")
pftfin_control.ras <- as.matrix(raster(files[grepl("CONTROL", toupper(files))]))
pftfin_control <- as.vector(pftfin_control.ras)

pftfin_mode.df <- data.frame(cbind(sort(rep(params.df$Run_group, 13)), rep(c(0:12), length(params.df$Run_group)), 0))
names(pftfin_mode.df) <- c("Scenario", "PFT_ID", "Count")
pftfin_mode.df$PFT_ID <- as.numeric(pftfin_mode.df$PFT_ID)
pftfin_mode.df$Count <- as.numeric(pftfin_mode.df$Count)

run_groups <- params.df$Run_group
run_groups <- c(run_groups[grepl("CONTROL", toupper(run_groups))], run_groups[!grepl("CONTROL", toupper(run_groups))])

for (i in c(1:length(run_groups))){
  
  scenario <- run_groups[i]
  
  pftfin_temp.ras <- as.matrix(raster(files[grepl(toupper(scenario), toupper(files))]))
  pftfin_temp <- as.vector(pftfin_temp.ras)
  
  df.temp <- data.frame(table(pftfin_temp.ras))
  df.temp[,1] <- as.numeric(as.character(df.temp[,1]))
  df.temp[,2] <- as.numeric(as.character(df.temp[,2]))
  
  pftfin_mode.df[which((pftfin_mode.df$Scenario == scenario) & (pftfin_mode.df$PFT_ID %in% df.temp[,1])), 3] <- as.numeric(df.temp[,2])
  
  if (i > 1){
    pftfin_mode.df[which(pftfin_mode.df$Scenario == scenario), 3] <- pftfin_mode.df$Count[which(pftfin_mode.df$Scenario == scenario)] - pftfin_mode.df$Count[which(pftfin_mode.df$Scenario == run_groups[1])]
  }
  
}

pftfin_mode.df <- pftfin_mode.df %>% subset(!grepl("CONTROL", toupper(Scenario)))
pftfin_mode.df$PFT_name <- pft_names$Name[match(pftfin_mode.df$PFT_ID, pft_names$ID)]

write.csv(pftfin_diff.df, paste0(path_rasters, "pftmode.csv"))
