#######################################################################################################################
#######################################################################################################################

# Initialization routine

if (module_name == "Init"){
  
  #### USER INPUT
  
  # Identification keyword for local (non-cluster) environment; 
  #   should be present in Sys.getenv() and unique to your local system (i.e., not on the cluster, I use "MacBook")
  LOCAL <- "MacBook"
  
  # Local paths (ending in /)
  path_params.local <-  "/Users/MacBook/Desktop/Cellar/Academic/UCSB/Research/LabFees/DYNAFFORE/Model_KD_Share/Parameters/" # Parameters source folder
  path_inputs.local <- "/Users/MacBook/Desktop/Cellar/Academic/UCSB/Research/LabFees/DYNAFFORE/Model_KD_Share/sierra/inputs/" # Inputs source folder
  path_outputs.local <- "/Users/MacBook/Desktop/Cellar/Academic/UCSB/Research/LabFees/DYNAFFORE/Model_KD_Share/sierra/outputs/" # Outputs folder
  path_scripts.local <- "/Users/MacBook/Desktop/Cellar/Academic/UCSB/Research/LabFees/DYNAFFORE/Model_KD_Share/Scripts/" # Scripts folder
  path_geospa.local <- "/Users/MacBook/Desktop/Cellar/Academic/UCSB/Research/LabFees/DYNAFFORE/Model_KD_Share/Geospatial/" # Geospatial folder
  
  # Cluster paths (ending in /)
  path_params.cluster <-  "/home/kldaum/vdl/scratch-vdl/kldaum1/DYNAFFOREST/Parameters_striker/"
  path_inputs.cluster <- "/home/kldaum/vdl/scratch-vdl/kldaum1/DYNAFFOREST/sierra/inputs/"
  path_outputs.cluster <- "/home/kldaum/vdl/scratch-vdl/kldaum1/DYNAFFOREST/sierra/outputs/"
  path_scripts.cluster <- "/home/kldaum/vdl/DYNAFFOREST/Scripts_striker/"
  path_geospa.cluster <- "/home/kldaum/vdl/scratch-vdl/kldaum1/DYNAFFOREST/Geospatial_striker/"
  
  #### Assign paths
  if (grepl(LOCAL, toString(Sys.getenv()))){
    platform <- "local"
  } else {
    platform <- "cluster"
  }
  
  # Select paths for appropriate platform (local vs cluster)
  for (path.name in ls()[grepl("path", ls()) & grepl(platform, ls())]){
    
    # Assign default path
    path.name.1 <- gsub(paste0(".", platform), "", path.name)
    assign(path.name.1, get(path.name))
    
    # Assign alt path
    path.name.alt <- paste0(path.name.1, ".", c("local", "cluster")[which(c("local", "cluster") != platform)])
    assign(paste0(path.name.1, ".alt"), get(path.name.alt))
    
    # Cat process
    cat(paste0("\n", path.name.1, " <- ", path.name,
               "\n", paste0(path.name.1, ".alt"), " <- ", path.name.alt, "\n"))
  }
  
  #### Env options
  options(scipen=999)
  options(warn = 1)
  
  #### Load libraries
  pkgs.required <- c("igraph", 
                     "sp", 
                     "dbplyr", 
                     "DBI", 
                     "tidyverse", 
                     "raster", 
                     "ncdf4", 
                     "rgdal", 
                     "rgeos", 
                     "lwgeom", 
                     "RColorBrewer", 
                     "forcats", 
                     "landscapeR", 
                     "landscapemetrics", 
                     "reshape2", 
                     "ggrepel", 
                     "sf",
                     "eva",
                     "plotly",
                     "rlang"
                     )
  pkgs.installed <- as.vector(installed.packages()[,1])
  
  for (pkg in pkgs.required){
    if ((pkg %in% pkgs.installed) == F){
      #install.packages(pkg)
    }
    eval(bquote(library(.(pkg))))
  }
  
  pkgs.missing <- pkgs.required[(pkgs.required %in% (.packages())) == F]
  pkgs.unavailable <- pkgs.required[(pkgs.required %in% as.vector(installed.packages()[,1])) == F]
  if (length(pkgs.missing) > 0){
    cat(paste0("\nPackages failed to load (but are installed): \n"))
    cat(paste0(pkgs.missing[(pkgs.missing %in% pkgs.unavailable) == F]))
    cat(paste0("\n\nPackages need to be installed: \n"))
    cat(paste0(pkgs.unavailable))
  }
      
  #### Load model functions and SQLite parameter data
  # Load up parameter values
  db.conn <- DBI::dbConnect(RSQLite::SQLite(), dbname=paste0(path_params, "pft_parameters.sqlite" ), synchronous = NULL) #!KE path, synchronous NULL
  pft.params <- DBI::dbReadTable(db.conn, "pft_parameters") 
  DBI::dbDisconnect(db.conn)
  
  db.conn <- DBI::dbConnect(RSQLite::SQLite(), dbname=paste0(path_params, "general_parameters.sqlite" ), synchronous = NULL) #!KE path, synchronous NULL
  gen.params <- DBI::dbReadTable(db.conn, "general_parameters") 
  DBI::dbDisconnect(db.conn)
    
  read.csv(paste0(path_params, "Params_BATCH.csv"))
  
  source(paste0(path_scripts, "model_functions_10-10-2021_KD.R")) # Read in the functions that run the model
  source(paste0(path_scripts, "fire_model_functions_10-10-2021_KD.R")) # read in the fire module functions
  source(paste0(path_scripts, "Functions_KD.R")) # Read in supplemental functions
  
  # if (exists("n.years") == F){
  #   ny <- 200
  # } else {
  #   ny <- n.years
  # }
  
  ny <- max(read.csv(paste0(path_params, "Params_BATCH.csv"))$Years)
  n.years <- ny
  
  # Create blank spatial array for use later...
  list.blank=raster(paste0(path_inputs, "forest_grid.tif"))
  list.blank=as.matrix(list.blank)
  list.blank[which(is.na(list.blank) == F)] <- 0
  list.blank <- rbind(NA, cbind(NA, list.blank, NA), NA)
  blank.ras <- list.blank
  list.blank=lapply(seq_len(ny), function(X) list.blank)
  r2 <- raster(paste0(path_geospa, "petmean_hires.tif"))
  raster.blank <- raster(list.blank[[1]])
  crs(raster.blank) <- crs(r2)
  extent(raster.blank) <- extent(r2)
  
  # Create PFT index
  pft_names <- c("Grass/Shrub", # 0
                 "Aspen", # 1
                 "California mixed conifer", # 2
                 "Douglas-fir coastal", # 3
                 "Douglas-fir inland", # 4
                 "Englemann Spruce/Fir", # 5
                 "Five-needle Pine", # 6
                 "Hemlock/Cedar", # 7
                 "Lodgepole Pine", # 8
                 "Mixed Fir", # 9
                 "Pinyon Pine", # 10
                 "Ponderosa Pine northern", # 11
                 "Ponderosa Pine southern") # 12
  
  pft_abbrev <- c("Shrub",
                  "Potr", 
                  "Pipo_Psme.C", 
                  "Psme.C", 
                  "Psme.I", 
                  "Pien", 
                  "Pial", 
                  "Thpl", 
                  "Pico", 
                  "Abgr", 
                  "Pied", 
                  "Pipo.N", 
                  "Pipo.S")
  
  cols <- c(
    #"Grassland/Shrubland"=
    "red",
    
    #"Aspen" = 
    "#8c510a", 
    
    #"California mixed conifer" = 
    "#1f78b4", 
    
    #"Douglas-fir coastal" = 
    "#b2df8a", 
    
    #"Douglas-fir inland"=
    "#756bb1", 
    
    #"Engelmann spruce/fir" = 
    "#33a02c",
    
    #"five-needle pine" = 
    "#fb9a99", 
    
    #"hemlock/cedar" = 
    "#f0027f", 
    
    #"lodgepole pine" = 
    "#ff7f00",
    
    #"Mixed fir" = 
    "#cab2d6", 
    
    #"Pinyon pine" = 
    "#ffff99", 
    
    #"Ponderosa pine northern" = 
    "#b15928", 
    
    #"Ponderosa pine southern" = 
    "#fdbf6f")
  
  pft_names <- data.frame(cbind(c(0:(length(pft_names)-1)), pft_names, cols, pft_abbrev))
  names(pft_names) <- c("ID", "Name", "color", "Abbrev")
  pft_names$ID <- as.numeric(pft_names$ID)
  
  
  # Load rasters
  elev = rbind(NA, cbind(NA, as.matrix(raster(paste0(path_inputs, "elev_grid.tif"))), NA), NA)
  slope_scaled.ras <- raster_matrix(path_geospa, "DEM_slope_res_RegionSierra.tif", rescale = T, reverse = T)
  slope.ras <- raster_matrix(path_geospa, "DEM_slope_res_RegionSierra.tif", rescale = F, reverse = F)
  roads_scaled.ras <- raster_matrix(path_geospa, "dist_road_km_CANVOR_clipped_res.tif", rescale = T, reverse = T)
  roads.ras <- raster_matrix(path_geospa, "dist_road_km_CANVOR_clipped_res.tif", rescale = F, reverse = F)
  pop_scaled.ras <- raster_matrix(path_geospa, "CANVOR_popdens_extent_res.tif", rescale = T, reverse = F)
  pop.ras <- raster_matrix(path_geospa, "CANVOR_popdens_extent_res.tif", rescale = F, reverse = F)
  WUI.ras <- raster_matrix(path_geospa, "CANVOR_WUI_Distance.tif", rescale = F, reverse = F)
  WUI_poly.ras <- raster_matrix(path_geospa, "CANVOR_WUI.tif", rescale = F, reverse = F)
  PET.ras <- raster_matrix(path_geospa, "petmean_hires.tif", rescale = F, reverse = F)
  strikes.ras <- raster_matrix(path_geospa, "strikes_hires.tif", rescale = F, reverse = F)
  fed.ras <- raster_matrix(path_geospa, "FedLandsRasterScale.tif", rescale = F, reverse = F)
  
  # Spatial supplements
  fed.ras.df <- read.csv(paste0(path_geospa, "FedLandsIndex.csv")) # Attribute table for vector fed lands data
  agency.ras <- list.blank[[1]]
  for (a in unique(fed.ras.df$Agency_ID)){
    temp.ix <- fed.ras.df$ID[which(fed.ras.df$Agency_ID == a)]
    agency.ras[which(as.vector(unlist(fed.ras)) %in% temp.ix)] <- a
  }
  agency.df <- data.frame(cbind(fed.ras.df$Agency, fed.ras.df$Agency_ID))
  agency.df <- rbind(data.frame(cbind("Private lands", 0)), agency.df[which(duplicated(agency.df) == F),])
  names(agency.df) <- c("Agency", "ID")
  
  cat(paste0("\n\nInitiation module completed\n\n"))

  # Store boolean "INIT" (initialized?) value in global env
  INIT <- T
}

#######################################################################################################################
#######################################################################################################################

# Misc commands for model loop
# Commands separated by semicolons and containing only single-tick quotation marks, to be run at the beginning of each model year
if (module_name == "misc"){
  cat(paste0("\nmodule: '", module_name, "'..."))
  if ("P_misc" %in% names(run_schedule)){
    commands.temp <- run_schedule$P_misc[r]
    if (as.character(commands.temp) != "-9999"){
      if (grepl(";", commands.temp)){
        commands.temp <- unlist(strsplit(commands.temp, ";"))
      }
      for (command.temp in commands.temp){
        eval(parse(text = gsub("[\u2018\u2019\u201A\u201B\u2032\u2035]", "'", commands.temp)))
      }
    }
  }
}

#######################################################################################################################
#######################################################################################################################

# Fuels treatment module
if (module_name == "treat"){
  
  cat(paste0("\nmodule: '", module_name, "'...\n"))
  
  # Create biomass rasters
  if (t == 1){
    
    # Define treatment types
    treat_types <- c("burn", "thin")
    
    # Create blank
    list.biomass_dead <- list.blank
    list.biomass_live <- list.blank
    list.fprob <- list.blank
    
    # Create fprob raster
    FireProb <- raster(list.blank[[1]])
    crs(FireProb) <- crs(r2)
    extent(FireProb) <- extent(r2)
    fprob.ras <- FireProb
    
    # Interpret "P_quant" type treatment parameters, generate rasters and preliminary treatability indexes
    treat_factors_dynamic <- c("conn", "dbio", "lbio", "fprob") # treatment factors that update annuallly
    treat_factors_static <- c("roads", "slope", "strikes", "WUI", "PET", "agency") # treatment factors that do not update annually
    treat_factors_all <- c(treat_factors_static, treat_factors_dynamic) # All treatment factors
   
    # Get initial distributions of cells conforming to experimental treatment factor thresholds...
    #   Dynamically updated rasters/spatial matrices will be assigned as blank at this stage, then recalculated during treatment loop.
    #   For static variables, these values will be referenced during treatment.
    for (i in c(1:length(treat_factors_all))){ # Loop through treatment factors...
      ras.temp <- list.blank[[1]] # Get blank domain spatial matrix
      name.temp <- paste0(treat_factors_all[i], ".ras") # Name this object
      if (exists(name.temp)){ # Check to see whether this spatial matrix already exists
        ras <- as.matrix(get(name.temp)) # If so, load...
        ras[which(is.na(ras.temp))] <- NA # Mask off out-of-domain, if necessary
      } else {
        ras <- ras.temp # If not, supply blank
        ras[which(ras == 0)] <- 1 # Apply boolean value (all domain cells eligible by default)
      }

      # Look up treatment parameter associated with this treatment factor
      if (exists(paste0("P_quant_", treat_factors_all[i]))){ # If treatment factor was defined in run schedule/parameters
        
        quant <- gsub("-9999", ">=0", get(paste0("P_quant_", treat_factors_all[i]))) # If treatment factor is undefined ("-9999"), set it to >=0; update if negative values for treatment factor exist
        
        # If factor is defined as a percentile threshold...
        if (grepl("%", quant)){
          # Determine percentile cutoff...
          quant.num <- as.numeric(unlist(rmchars(quant, c(">", "<", "%", "#", "="))))/100 # Get percentile value as a decimal (remove non-numeric characters, divide by 100)
          quant.num <- as.numeric(quantile(ras, probs = quant.num, na.rm = T)) # Get cutoff percentile value of range
          
          # Get index of eligible cells
          if (grepl(">", quant)){ # If parameter includes ">" find cells with higher values than cutoff percentile value
            if (grepl("=", quant)){ # If includes values equal to percentile cutoff...
              ras.ix <- which(ras >= quant.num)
            } else { # If not...
              ras.ix <- which(ras > quant.num)
            }
          } else {
            if (grepl("<", quant)){ # If parameter includes "<" find cells with lower values than cutoff percentile value
              if (grepl("=", quant)){ # If includes values equal to percentile cutoff...
                ras.ix <- which(ras <= quant.num)
              } else { # If not...
                ras.ix <- which(ras < quant.num)
              }
            }
          }
          ras.temp[ras.ix] <- 1 # Give eligible cells boolean
          
        } else {
          
          # If factor is defined as a numerical threshold...
          if (grepl("=", quant) | grepl("#", quant)){
            ras.temp[which(eval(parse(text = paste0("ras ", gsub("#", "", quant)))) & (list.blank[[1]] == 0))] <- 1 # Remove hash and evaluate as an expression; insert boolean result in blank spatial matrix
          } else {
            
            # If factor is a numerical sorting factor...
            if (quant == "SORT"){
              ras.temp[!is.na(ras.temp)] <- 1 # All cells eligible, sorting occurs later (during treatment step)
            } else {
              
              # If none of the above formats, interpret parameter as a list of eligible unique values (e.g., categorical PFT and agency grids)
              ras.temp[which(eval(parse(text = paste0("ras %in% c(", quant, ")"))) & (list.blank[[1]] == 0))] <- 1
              
            }
          }
        }
      }
      
      assign(paste0(treat_factors_all[i], ".ix"), which(ras.temp == 1)) # Store indexes of eligible cells
      assign(paste0(treat_factors_all[i], ".rank"), ras.temp) # Store spatial matrix of eligible cells
      
    }
    
    # Combine factors to get a mask of cells treatable according to static parameters
    vec.temp <- which(unlist(lapply(lapply(lapply(treat_factors_static, paste0, ".ix"), get), length)) > 0)
    array.temp <- lapply(lapply(treat_factors_static[vec.temp], paste0, ".rank"), get)
    treatable.ras <- Reduce("+", array.temp) == length(vec.temp)
    treatmask.ras <- treatable.ras
    treat_F <- which(as.vector(treatmask.ras) == F)
    if (length(treat_F) > 0){
      treatmask.ras[treat_F] <- NA
      treatmask.ras[is.na(treatmask.ras) == F] <- 0
    }
    
    # Determine primary and secondary treatment type
    # Secondary treatment type: if a treatment area is not defined for one of the treatment types ("burn"/"thin") but a percent reduction is, 
    # this is defined as the secondary treatment. It will be implemented in the same cells chosen for the other type (not selected independently).
    treat_type_area <- treat_types[which(unlist(lapply(as.list(unlist(lapply("P_", paste0, treat_types, "_area"))), get)) != -9999)] # Check areal extent parameter to determine primary treatment types
    treat_type_redux <- treat_types[which(unlist(lapply(as.list(unlist(lapply("P_", paste0, treat_types, "_redux"))), get)) != -9999)] # Make sure a biomass reduction parameter has also been supplied
    treat_type_prim <- treat_types[which(c(sum(treat_types[1] == c(treat_type_area, treat_type_redux)), sum(treat_types[2] == c(treat_type_area, treat_type_redux))) == 2)] # Which is defined for both
    treat_type_sec <- treat_types[which(c(sum(treat_types[1] == c(treat_type_area, treat_type_redux)), sum(treat_types[2] == c(treat_type_area, treat_type_redux))) == 1)] # Which is defined for one  
    
    # Replace null values in location selection parameters !! Change if negative numbers present
    for (quant in ls()[grep("P_quant_", ls())]){
      if (as.character(get(quant)) == "-9999"){
        assign(quant, ">=0")
      }
    }
    
    # Determine which biomass loading threshold (live or dead) is stricter...
    # ...The stricter threshold will be used to mask out non-conforming cells first. The second threshold will
    # ...be applied to the remaining cells (relevant if thresholding is based on percentiles)
    triage_pools <- T # Switch to turn this process on/off (on/off = T/F)
    primary_pool <- unlist(rmchars(c(P_quant_dbio, P_quant_lbio), c(">", "<", "%", "#", "=")))
    primary_pool <- as.numeric(primary_pool)
    vec.temp <- which(unlist(lapply(">", grepl, list(P_quant_dbio, P_quant_lbio))) == T)
    if (length(vec.temp) > 0){
      primary_pool[vec.temp] <- 100-(primary_pool[vec.temp])
    }
    primary_pool <- c("dbio", "lbio")[which(primary_pool == min(primary_pool))]
    # ...if same, default to dead biomass as primary
    if (length(primary_pool) == 2){
      primary_pool <- "dbio"
    }
    # Order masking factors (primary pool first)
    treat_factors_dynamic <- c(primary_pool, treat_factors_dynamic[!grepl(primary_pool, treat_factors_dynamic)])
    
    # Define treatment years for each treatment
    if (as.numeric(P_burn_tstart) != -9999) {years_burn <- seq(P_burn_tstart, n.years, P_burn_intvl)} else {years_burn <- NA}
    if (as.numeric(P_thin_tstart) != -9999) {years_thin <- seq(P_thin_tstart, n.years, P_thin_intvl)} else {years_thin <- NA}
    if (length(treat_type_sec) > 0){
      assign(paste0("years_", treat_type_sec), get(paste0("years_", treat_type_prim)))
    }
    
    # Create treatment log data frame
    log_treat.names <- c("Run_ID", "Run_name",
                       "year", "treatment", "km", "pfts",
                       "agency", "unit",
                       "population.mean", "population.sd",
                       "slope.mean", "slope.sd",
                       "road_distance.mean", "road_distance.sd",
                       "connectivity.mean", "connectivity.sd",
                       "elevation.mean", "elevation.sd",
                       "age.mean", "age.sd",
                       "litter.mean", "litter.sd",
                       "snag.mean", "snag.sd",
                       "cwd.mean", "cwd.sd",
                       "biomass_dead.mean",
                       "biomass_dead.sd",
                       "biomass_dead.removed",
                       "leaf.mean", "leaf.sd",
                       "branch.mean", "branch.sd",
                       "stem.mean", "stem.sd",
                       "biomass_live.mean",
                       "biomass_live.sd",
                       "biomass_live.removed",
                       "cells_treated"
    ) # Define names of columns...
    log_treat <- NULL # Set as null object for appending in loop
    for (treat_type in treat_type_prim){ # Loop through primary treatment type/s to generate entries
      df.temp <- data.frame(cbind(run, run_name, get(paste0("years_", treat_type)), treat_type))
      log_treat <- rbind(log_treat, df.temp)
    }
    log_treat <- data.frame(cbind(log_treat, matrix(data = 0, nrow = nrow(log_treat), ncol = length(log_treat.names)-ncol(log_treat)))) # Create blank entries for populating during treatment phase
    names(log_treat) <- log_treat.names # Set column names
    log_treat <- log_treat[order(log_treat$year),] # Order by year
    row.names(log_treat) <- c(1:nrow(log_treat)) # Set row names
    
    # Create (blank) rasters for logging treatment activity (based on blank forest raster)
    treat=raster(paste0(path_inputs, "forest_grid.tif"))
    treat=as.matrix(treat)
    treat[which(is.na(treat) == F)] <- 0
    treat = rbind(NA, cbind(NA, treat, NA), NA)
    list.treat=lapply(seq_len(n.years), function(X) treat)
    burnmap.ras <- list.treat[[1]]
    thinmap.ras <- list.treat[[1]]
    burnrepmap.ras <- list.treat[[1]]
    thinrepmap.ras <- list.treat[[1]]
    prevtreat_thin.ras <- list.treat[[1]]
    prevtreat_burn.ras <- list.treat[[1]]
  
  } # End year 1 initialization routine
  
  
  # Get current values for biomass (mask to make NA values consistent)
  stem.t1 <- mask_landscape(blank.ras, list.stem[[t+1]])
  branch.t1 <- mask_landscape(blank.ras, list.branch[[t+1]])
  leaf.t1 <- mask_landscape(blank.ras, list.leaf[[t+1]])
  snag.t1 <- mask_landscape(blank.ras, list.snag[[t+1]])
  cwd.t1 <- mask_landscape(blank.ras, list.cwd[[t+1]])
  litter.t1 <- mask_landscape(blank.ras, list.litter[[t+1]])
  
  # Get live/dead biomass sums
  list.biomass_live[[t+1]] <- branch.t1 + leaf.t1 + stem.t1
  list.biomass_dead[[t+1]] <- snag.t1 + litter.t1 + cwd.t1
  
  # Get fprob (& resize raster)
  fprob.ras <- FireProb
  fprob.ras <- terra::aggregate(fprob.ras, fact=2, fun=mean)
  fprob.ras <- resample(fprob.ras, raster.blank, method='bilinear')
  list.fprob[[t+1]] <- mask_landscape(blank.ras, as.matrix(fprob.ras))
  
  # Loop though treatment routine ("burn" and "thin") for whichever treatment type(s) have a defined area (primary treatment; cells will be selected independently)
  # If a secondary treatment type is defined, loop over it afterwards, else only apply current treatment type (outer loop)
  for (treat_type in c(treat_type_prim, treat_type_sec)){
    
    # Define parameters for primary treatment types
    if (treat_type %in% treat_type_prim){
    
      # Generic handle for treatment type years
      years_treat <- get(paste0("years_", treat_type))
      
      # Get treatment parameters for active treatment type
      treat_params.spec <- ls()[grepl(paste0("P_", treat_type), ls())]
      
      # Give these parameters generic names for within loop (replace "burn"/"thin" with "treat")
      for (name in treat_params.spec){
        assign(gsub(paste0("P_", treat_type), "P_treat", name), get(name))
      }
      
      # If PFTs for treatment not defined, include all
      if ("-9999" %in% as.character(P_treat_PFT)){
        P_treat_PFT <- c(1:12) # Replace null (-9999) with PFT IDs 1:12
      } else {
        if (suppressWarnings(is.na(as.numeric(P_treat_PFT)))){ # If parameter was supplied with commas...
          P_treat_PFT <- as.integer(unlist(strsplit(P_treat_PFT, ","))) # ...remove them and get numerical vector of eligible PFTs
        } else {
          P_treat_PFT <- as.numeric(P_treat_PFT) # Get numerical vector of eligible PFTs
        }
      }
      
      # Get raster of most recent treatments; update (to determine minimum re-treatment threshold adherence)
      prevtreat.ras <- get(paste0("prevtreat_", treat_type, ".ras")) # Get relevant spatial matrix
      if (sum(unique(prevtreat.ras[is.na(prevtreat.ras) == F]) > 0) > 0){ # If treatment history exists...
        prevtreat.ras[which(prevtreat.ras > 0)] <- prevtreat.ras[which(prevtreat.ras > 0)]+1 # Add a year to treated cells (update counter)
      }
      prevtreat.ras[which(prevtreat.ras > P_treat_return)] <- 0 # Reset cells that have exceeded minimum retreatment threshold
      assign(paste0("prevtreat_", treat_type, ".ras"), prevtreat.ras)
      
    } else {
      
      # Only redefine biomass reduction parameter and treatment counter for secondary type (if applicable)
      assign(paste0("prevtreat_", treat_type, ".ras"), prevtreat.ras)
      P_treat_redux <- treatsec_redux
      
    }
    
    # Define relevant biomass pools for given treatment type
    if (treat_type == "burn"){
      treat_pools <- c("snag", "litter", "cwd")
      list.biomass <- get("list.biomass_dead")
    }
    if (treat_type == "thin"){
      treat_pools <- c("leaf", "branch", "stem")
      list.biomass <- get("list.biomass_live")
    }
    
    # If this year is in a treatment year
    if ((t+1) %in% years_treat){
      
      cat(paste0("\nTreatment (", treat_type, "): year ", t+1, "\n\n"))
      
      ## Non-hierarchical filter
      prevtreat.ras <- get(paste0("prevtreat_", treat_type, ".ras"))

      # Get all treatment factors
      treat_factors <- ls()[grep("P_quant", ls(), fixed = T)]
      
      # Get sorting factors
      treat_sort <- treat_factors[which(unlist(lapply(lapply(treat_factors, get), as.character)) == "SORT")]
      
      # Determine which factors to ignore
      treat_rm <- treat_factors[which(unlist(lapply(lapply(treat_factors, get), as.character)) == "-9999")]
      treat_factors <- gsub("P_quant_", "", treat_factors[which((treat_factors %in% treat_sort == F) & (treat_factors %in% treat_rm == F))])
      
      # If a primary treatment type, determine which cells to treat
      if (treat_type %in% treat_type_prim){
        
        # Retrieve dynamic (annually re-simulated) spatial matrices
        conn.ras <-  mask_landscape(blank.ras, connectivity.calc(list.leaf[[t+1]], list.branch[[t+1]], list.stem[[t+1]]))
        dbio.ras <-  mask_landscape(blank.ras, list.biomass_dead[[t+1]])
        lbio.ras <-  mask_landscape(blank.ras, list.biomass_live[[t+1]])
        bio.ras <-   mask_landscape(blank.ras, (list.biomass_live[[t+1]] + list.biomass_dead[[t+1]]))
        fprob.ras <- mask_landscape(blank.ras, list.fprob[[t+1]])
        
        # If non-treatable PFTs are defined, mask eligible cells by PFT
        if (toString(P_treat_PFT) != "-9999"){ # If PFT parameter defined...
          for (ras.name in unlist(lapply(treat_factors_dynamic, paste0, ".ras"))){ # Loop through dynamic treatment factors...
            ras <- get(ras.name) # Get associated spatial matrix
            ras[which((list.pft[[t+1]] %in% P_treat_PFT) == F)] <- NA # Populate ineligible cells with NA
            assign(ras.name, ras) # Reassign variable name
          }
        }
        
        # If treatable age is limited, mask eligible cells by age
        if (toString(P_treat_age) != "-9999"){ # If age parameter defined...
          for (ras.name in unlist(lapply(treat_factors_dynamic, paste0, ".ras"))){ # Loop through dynamic treatment factors...
            ras <- get(ras.name) # Get associated spatial matrix
            ras[which(list.age[[t+1]] < P_treat_age)] <- NA # Populate ineligible cells with NA
            assign(ras.name, ras) # Reassign variable name
          }
        }
        
        # Update mask of treatable cells
        treatmask.temp.ras <- treatmask.ras
        treatmask.temp.ras[which(prevtreat.ras > 0)] <- NA
        
        # Re-evaluate treatment mask (eligible cells) according to dynamic variables (those re-simulated annually, e.g., biomass)
        for (i in c(1:length(treat_factors_dynamic))){
          
          # Get factor...
          fac.temp <- treat_factors_dynamic[i] # Generic variable name for factor
          ras.temp <- list.blank[[1]] # Temporary spatial matrix
          ras <- mask_landscape(treatmask.temp.ras, as.matrix(get(paste0(fac.temp, ".ras")))) # Retrieve spatial matrix associated with factor, mask by static factors + re-treatment threshold

          # Mask out ineligible cells from primary pool if applicable
          # ...this allows non-primary pool percentile values to be calculated only from cells available in the (more restrictive) primary pool
          if (triage_pools == T){
            if ((fac.temp != primary_pool) & (grepl("bio", fac.temp))){ # If we're looking at the non-primary biomass pool...
              ras.temp <- get(paste0(primary_pool, ".rank")) # Get the primary one...
              ras[which(ras.temp == 0)] <- NA # Mask the spatial matrix of the secondary pool with the eligible cells from primary pool
            }
          }
          
          # Determine treatable cells
          if (exists(paste0("P_quant_", fac.temp))){ # If treatment parameter/factor is defined...
            quant <- get(paste0("P_quant_", fac.temp)) # Apply generic name
            if (quant != "SORT"){ # Ignore sorting parameters for now... 
              if (grepl("%", quant)){ # If defined by percentile threshold...
                
                # Determine eligible cells if using a percentile cutoff value...
                quant.num <- as.numeric(unlist(rmchars(quant, c(">", "<", "%", "#", "=")))) # Get percentile value as a decimal (remove non-numeric characters, divide by 100)
                quant <- gsub(quant.num, "", quant) # Remove number from expression
                quant.num <- as.numeric(quantile(ras, probs = (quant.num/100), na.rm = T)) # Get actual cutoff percentile value of range
                quant <- gsub("%", quant.num, quant) # Revise expression to include cutoff value
                ras.temp[which(eval(parse(text = paste0("ras ", gsub("%", "", quant)))) & (list.blank[[1]] == 0))] <- 1 # Evaluate expression, mark eligible cells (boolean)

              } else {
                
                # Determine eligible cells if using a numerical cutoff value...
                if (grepl("=", quant)){
                  ras.temp[which(eval(parse(text = paste0("ras ", gsub("#", "", quant)))) & (list.blank[[1]] == 0))] <- 1 # Evaluate expression, mark eligible cells (boolean)
                }
                
              }
              
              # Save eligibility lists
              assign(paste0(treat_factors_dynamic[i], ".ix"), which(ras.temp == 1)) # Save list of eligible cell indexes by factor name
              assign(paste0(treat_factors_dynamic[i], ".rank"), ras.temp) # Save spatial matrix of eligible cells by factor name
              
            }
          }
        }
        
        # Finalize treatment list
        treat.ix <- as.numeric(row.names(data.frame(which(table(unlist(lapply(unlist(lapply(treat_factors, paste0, ".ix")), get))) == length(treat_factors))))) # Find cells that appear in all eligibility lists
        
        # Define treatment area (either set treatment area or total cell count, whichever is smaller... If latter, will get flagged)
        treat_area <- c(get("P_treat_area"), length(treat.ix))[which(c(get("P_treat_area"), length(treat.ix)) == min(c(get("P_treat_area"), length(treat.ix))))]
      
        if (sum(c(0, -9999) %in% P_treat_area) == 0){ # If treatment area is still defined and > 0
          if (length(treat_sort) > 0){ # If a sorting treatment factor exists... 
            if (length(treat_sort) > 1){ # ONLY works if applied for both biomass pools
              ras.temp <- Reduce('+', lapply(lapply(lapply(treat_sort, strsplitsubr, "_", 1,1), paste0, ".ras"), get)) # Retrieve sorting factor spatial matrices, sum
            } else {
              ras.temp <- get(paste0(strsplitsubr(treat_sort, "_", 1,1), ".ras")) # Retrieve sorting factor spatial matrix
            }
            ras.temp[is.na(ras.temp)] <- 0 
            ras.temp[which(c(1:length(ras.temp)) %in% treat.ix == F)] <- 0 # Replace ineligible cells with zeros
            treat.ix <- sort(ras.temp, index.return = T, decreasing = T)$ix[c(1:treat_area)] # Sort all cells by value (decreasing) and select treatment area from the top
          } else {
            treat.ix <- sort(sample(treat.ix, P_treat_area, replace = F)) # If a sorting factor does not exist, select pseudo-randomly from eligible cells
          }
        }
        
        # Set to zero during first pass
        biomass_redux <- c(0, 0)
        
      } # End cell selection protocol
    
      # Calculate net biomass removal for appropriate biomass pool
      biomass_redux[which(c("burn", "thin") == treat_type)] <- sum(list.biomass[[t+1]][treat.ix])*P_treat_redux
      
      # Log cells treated in re-treatment counter spatial matrix (eligibility threshold)
      prevtreat.ras[treat.ix] <- 1
      assign(paste0("prevtreat_", treat_type, ".ras"), prevtreat.ras)
      
      # Log cells treated in treatment year spatial matrix (year of most recent treatment)
      plot.temp.name <- paste0(treat_type, "map.ras")
      plot.temp <- get(plot.temp.name)
      plot.temp[treat.ix] <- t+1
      assign(plot.temp.name, plot.temp)
      
      # Log cells treated in re-treatment counter spatial matrix (cumulative re-treatment count)
      plot.temp.name <- paste0(treat_type, "repmap.ras")
      plot.temp <- get(plot.temp.name)
      plot.temp[treat.ix] <- plot.temp[treat.ix]+1
      assign(plot.temp.name, plot.temp)
  
      
      ### Compile summary stats of treated cells for treatment log
      
      treatment <- treat_type
      km <- length(treat.ix)
      pfts <- toString(list.pft[[t+1]][treat.ix])
      
      agency <- paste(unique(fed.ras.df$Agency[match(fed.ras[treat.ix[is.na(fed.ras[treat.ix]) == F]], fed.ras.df$ID)]), collapse = ", ") # Agencies
      unit <- paste(unique(fed.ras.df$unit_name[match(fed.ras[treat.ix[is.na(fed.ras[treat.ix]) == F]], fed.ras.df$ID)]), collapse = ", ")
      
      population.mean <- mean(pop.ras[treat.ix], na.rm = T) # Population
      population.sd <- sd(pop.ras[treat.ix], na.rm = T)
      
      slope.mean <- mean(slope.ras[treat.ix], na.rm = T) # Slope
      slope.sd <- sd(slope.ras[treat.ix], na.rm = T)
      
      road_distance.mean <- mean(roads.ras[treat.ix], na.rm = T) # Road distance
      road_distance.sd <- sd(roads.ras[treat.ix], na.rm = T)
      
      connectivity.mean <- mean(conn.ras[treat.ix], na.rm = T) # Connectivity
      connectivity.sd <- sd(conn.ras[treat.ix], na.rm = T)
      
      elevation.mean <- mean(elev[treat.ix], na.rm = T) # Elevation
      elevation.sd <- sd(elev[treat.ix], na.rm = T)
      
      age.mean <- mean(list.age[[t+1]][treat.ix], na.rm = T) # Age
      age.sd <- sd(list.age[[t+1]][treat.ix], na.rm = T)
      
      litter.mean <- mean(list.litter[[t+1]][treat.ix], na.rm = T) # Litter
      litter.sd <- sd(list.litter[[t+1]][treat.ix], na.rm = T) 
      
      snag.mean <- mean(list.snag[[t+1]][treat.ix], na.rm = T) # Snag
      snag.sd <- sd(list.snag[[t+1]][treat.ix], na.rm = T) 
      
      cwd.mean <- mean(list.cwd[[t+1]][treat.ix], na.rm = T) # CWD
      cwd.sd <- sd(list.cwd[[t+1]][treat.ix], na.rm = T)
      
      biomass_dead.mean <- mean(list.biomass_dead[[t+1]][treat.ix], na.rm = T) # Total dead biomass
      biomass_dead.sd <- sd(list.biomass_dead[[t+1]][treat.ix], na.rm = T)
      
      biomass_dead.removed <- biomass_redux[1] # Dead biomass removed
      
      leaf.mean <- leaf.mean <- mean(list.leaf[[t+1]][treat.ix], na.rm = T) # Leaf
      leaf.sd <- sd(list.leaf[[t+1]][treat.ix], na.rm = T) 
      
      branch.mean <- mean(list.branch[[t+1]][treat.ix], na.rm = T) # Branch
      branch.sd <- sd(list.branch[[t+1]][treat.ix], na.rm = T)
      
      stem.mean <- mean(list.stem[[t+1]][treat.ix], na.rm = T) # Stem
      stem.sd <- sd(list.stem[[t+1]][treat.ix], na.rm = T)
      
      biomass_live.mean <- mean(list.biomass_live[[t+1]][treat.ix], na.rm = T) # Total live biomass
      biomass_live.sd <- sd(list.biomass_live[[t+1]][treat.ix], na.rm = T)
      
      biomass_live.removed <- biomass_redux[2] # Live biomass removed
      
      cells_treated <- toString(sort(treat.ix)) # Treated cells
      # cells_treated <- -9999 # Can be activated to substantially reduce file size for long runs
      
      # Add new row/entry to treatment log
      row <- which((log_treat$year == t+1) & (log_treat$treatment == treat_type)) # determine row associated with this treatment
      cols <- log_treat.names[which(log_treat[row,] == 0)] # Empty columns
      log_treat[row,cols] <- c(lapply(cols, get)) # Populate
      
      # Perform treatment (reduce biomass in appropriate spatial matrices)
      if (treat_type == "burn"){
        list.cwd[[t+1]][treat.ix] <- (list.cwd[[t+1]][treat.ix])*(1-P_treat_redux)
        list.snag[[t+1]][treat.ix] <- (list.snag[[t+1]][treat.ix])*(1-P_treat_redux)
        list.litter[[t+1]][treat.ix] <- (list.litter[[t+1]][treat.ix])*(1-P_treat_redux)
      }
      if (treat_type == "thin"){
        list.branch[[t+1]][treat.ix] <- (list.branch[[t+1]][treat.ix])*(1-P_treat_redux)
        list.stem[[t+1]][treat.ix] <- (list.stem[[t+1]][treat.ix])*(1-P_treat_redux)
        list.leaf[[t+1]][treat.ix] <- (list.leaf[[t+1]][treat.ix])*(1-P_treat_redux)
      }
      
      # If not enough cells exist for treatment, make note, abort run
      if ((treat_area < P_treat_area) & (sum(log_treat$km) > 0)){
        cat(paste0("\nWarning:\nEligible treatment area < Prescribed treatment area for year ", t+1, "!\n"))
        write.csv(log_treat, paste0(path_save, "Run",  sprintf("%03d", run), "_TreatLog", ".csv"), row.names = F)
        BREAK.cmd <- paste0("Run",  sprintf("%03d", run), ": Eligible treatment area < Prescribed treatment area for year ", t+1, "...")
        write.table(BREAK.cmd, file = paste0(path_save, "BREAK_cmd.txt"), sep = "")
        if (platform == "local"){
          breakerbar <- breakerbar
        }
      }
    
      # If last year of simulation, save rasters and treatment log
      if (t+1 == n.years){
        
        # Treatment log
        write.csv(log_treat, paste0(path_save, "Run",  sprintf("%03d", run), "_TreatLog", ".csv"), row.names = F)
        
        # Rasters
        raster::writeRaster(raster(treatable.ras), paste0(path_save_ras, "Treatable_", run_schedule$Run_group[r], ".tiff"), overwrite = T)
        raster::writeRaster(raster(thinmap.ras), paste0(path_save_ras, "Run",  sprintf("%03d", run), "RasTreatThin", ".tiff"), overwrite = T)
        raster::writeRaster(raster(burnmap.ras), paste0(path_save_ras, "Run",  sprintf("%03d", run), "RasTreatBurn", ".tiff"), overwrite = T)
        raster::writeRaster(raster(thinrepmap.ras), paste0(path_save_ras, "Run",  sprintf("%03d", run), "RasTreatThinRep", ".tiff"), overwrite = T)
        raster::writeRaster(raster(burnrepmap.ras), paste0(path_save_ras, "Run",  sprintf("%03d", run), "RasTreatBurnRep", ".tiff"), overwrite = T)
        
      }
    }
  }
}


#######################################################################################################################
#######################################################################################################################

# UNIVERSAL

if (module_name == "collect_data"){
  
  cat(paste0("\n\nInitiating Module: 'collect_data'\n"))
  
  # Set which years to collect data in "universal" module
  # yrs.univ <- c(2:n.years)[c(2:n.years) %% 5 == 0]
  yrs.univ <- c(3:n.years)
  
  if (t == 1){
    
    # Spinup: This function can be modified to specify which years to omit
    # from the statistics, if treatment equilibration years are to be ignored.
    years.spinup <- gen_spinup(run_schedule, F)  # To modify to exclude treatment spin-up years, second arg = F
    t.spin <- as.numeric(years.spinup$Spin[which(years.spinup$Group == run_schedule$Run_group[r])])
    t.max <- as.numeric(years.spinup$Max[which(years.spinup$Group == run_schedule$Run_group[r])])
    
    # Forest/grassland transition rasters
    grass_to.ras <- list.blank[[1]]
    grass_from.ras <- list.blank[[1]]
    grass_year.ras <- list.blank[[1]]
    grass_age.ras <- list.blank[[1]] 
    
    # Fire occurrence raster
    fire_num.ras <- list.blank[[1]] # Number of Fires
    fire_sev.ras <- list.blank[[1]] # Fire Severity
    fire.ras <- list.blank[[1]] # Fire Occurrence
    
    # Biomass rasters
    conn.ras <- list.blank[[1]] # Connectivity
    lbio.ras <- list.blank[[1]] # Live biomass
    dbio.ras <- list.blank[[1]] # Dead biomass
    deco.ras <- list.blank[[1]] # Decomposition
    comb.ras <- list.blank[[1]] # Combusted
    combl.ras <- list.blank[[1]] # Combusted Live
    combd.ras <- list.blank[[1]] # Combusted Dead
    flux.pos.ras <- list.blank[[1]] # Positive Flux
    flux.neg.ras <- list.blank[[1]] # Negative Flux
    flux.net.ras <- list.blank[[1]] # Net Flux
    crownkill.ras <- list.blank[[1]] # Crown Kill
    firekill.ras <- list.blank[[1]] # Stand Mortality (fire)
    fireprob.ras <- list.blank[[1]] # Fire Probability
    stressed.ras <- list.blank[[1]] # Stress
    
    # Get population density raster
    if (exists("pop.ras") == F){
      pop.ras = raster(paste0(path_geospa, "CANVOR_popdens_extent_res.tif"))
      pop.ras = as.matrix(pop.ras)
      pop.ras = rbind(NA, cbind(NA, pop.ras, NA), NA)
    }
    
    fire.df <- NULL
    flux.df <- NULL
    
  }
  
  # If year is in universal years (defined above) and is not final year...
  if ((((t+1) %in% yrs.univ) == T) & (t+1 <= t.max)){
    
    Run <- r
    Year <- t+1
    
    # Snapshots
    if (take.snapshot == TRUE){
      if ((t) %in% P_snapshot_yrs){ #! KE
        snapshot.year <- (t)
        snapshot(year.write = (t)) #! KE
        cat(paste0("Snapshot generated for year ", (t), "\n")) #! KE
      } #! KE
    }
    
    # Carbon flux data compilation
    decompose <- mask_landscape(blank.ras, (list.litter[[t]]*gen.params$decomp_litter + list.cwd[[t]]*gen.params$decomp_cwd + 0))
    combusted.dead <- mask_landscape(blank.ras, (list.avail.cwd[[t+1]] + list.avail.litter[[t+1]] + 0))
    firekilled.live <- mask_landscape(blank.ras, ((list.leaf[[t]]*(list.ck[[t+1]]/100)) + (list.branch[[t]]*(list.ck[[t+1]]/100)) + 0))
    combusted.live <- mask_landscape(blank.ras, (((list.leaf[[t]]*(list.ck[[t+1]]/100))*.9) + ((list.branch[[t]]*(list.ck[[t+1]]/100))*.5) + 0))
    
    growth.leaf <-
      mask_landscape(blank.ras, (biomass(list.pft[[t]],list.blank[[1]],list.blank[[1]],list.DBH[[t+1]],leaf_a_Potr, leaf_b_Potr,leaf_a_Pipo_Psme.C,leaf_b_Pipo_Psme.C,leaf_a_Psme.C,leaf_b_Psme.C,leaf_a_Psme.I,leaf_b_Psme.I,
                            leaf_a_Pien,leaf_b_Pien,leaf_a_Pial,leaf_b_Pial,leaf_a_Thpl,leaf_b_Thpl, leaf_a_Pico,leaf_b_Pico,leaf_a_Abgr,leaf_b_Abgr,leaf_a_Pied,leaf_b_Pied,leaf_a_Pipo.N,
                            leaf_b_Pipo.N,leaf_a_Pipo.S,leaf_b_Pipo.S, list.density[[t+1]], list.leaf[[t]],list.blank[[1]], "leaf"))) - 
      mask_landscape(blank.ras, (biomass(list.pft[[t-1]],list.blank[[1]],list.blank[[1]],list.DBH[[t]],leaf_a_Potr, leaf_b_Potr,leaf_a_Pipo_Psme.C,leaf_b_Pipo_Psme.C,leaf_a_Psme.C,leaf_b_Psme.C,leaf_a_Psme.I,leaf_b_Psme.I,
                                      leaf_a_Pien,leaf_b_Pien,leaf_a_Pial,leaf_b_Pial,leaf_a_Thpl,leaf_b_Thpl, leaf_a_Pico,leaf_b_Pico,leaf_a_Abgr,leaf_b_Abgr,leaf_a_Pied,leaf_b_Pied,leaf_a_Pipo.N,
                                      leaf_b_Pipo.N,leaf_a_Pipo.S,leaf_b_Pipo.S, list.density[[t]], list.leaf[[t-1]],list.blank[[1]], "leaf")))
    growth.leaf[which(growth.leaf < 0)] <- 0
    
    growth.branch <-
      mask_landscape(blank.ras, (biomass(list.pft[[t]],list.blank[[1]],list.blank[[1]],list.DBH[[t+1]],branch_a_Potr, branch_b_Potr,branch_a_Pipo_Psme.C,branch_b_Pipo_Psme.C,branch_a_Psme.C,branch_b_Psme.C,branch_a_Psme.I,branch_b_Psme.I,
                              branch_a_Pien,branch_b_Pien,branch_a_Pial,branch_b_Pial,branch_a_Thpl,branch_b_Thpl, branch_a_Pico,branch_b_Pico,branch_a_Abgr,branch_b_Abgr,branch_a_Pied,branch_b_Pied,branch_a_Pipo.N,
                              branch_b_Pipo.N,branch_a_Pipo.S,branch_b_Pipo.S,list.density[[t+1]],list.branch[[t]],list.blank[[1]], "branch"))) - 
      mask_landscape(blank.ras, (biomass(list.pft[[t-1]],list.blank[[1]],list.blank[[1]],list.DBH[[t]],branch_a_Potr, branch_b_Potr,branch_a_Pipo_Psme.C,branch_b_Pipo_Psme.C,branch_a_Psme.C,branch_b_Psme.C,branch_a_Psme.I,branch_b_Psme.I,
                                        branch_a_Pien,branch_b_Pien,branch_a_Pial,branch_b_Pial,branch_a_Thpl,branch_b_Thpl, branch_a_Pico,branch_b_Pico,branch_a_Abgr,branch_b_Abgr,branch_a_Pied,branch_b_Pied,branch_a_Pipo.N,
                                        branch_b_Pipo.N,branch_a_Pipo.S,branch_b_Pipo.S,list.density[[t]],list.branch[[t-1]],list.blank[[1]], "branch")))
    growth.branch[which(growth.branch < 0)] <- 0
    
    
    growth.stem <-
      mask_landscape(blank.ras, (biomass(list.pft[[t]],list.blank[[1]],list.blank[[1]], list.DBH[[t+1]], stem_a_Potr, stem_b_Potr,stem_a_Pipo_Psme.C,stem_b_Pipo_Psme.C,stem_a_Psme.C,stem_b_Psme.C,stem_a_Psme.I,stem_b_Psme.I,
                                         stem_a_Pien,stem_b_Pien,stem_a_Pial,stem_b_Pial,stem_a_Thpl,stem_b_Thpl, stem_a_Pico,stem_b_Pico,stem_a_Abgr,stem_b_Abgr,stem_a_Pied,stem_b_Pied,stem_a_Pipo.N,
                                         stem_b_Pipo.N,stem_a_Pipo.S,stem_b_Pipo.S, list.density[[t+1]], list.stem[[t]], list.blank[[1]],"stem"))) - 
                                   mask_landscape(blank.ras, (biomass(list.pft[[t-1]],list.blank[[1]],list.blank[[1]], list.DBH[[t]], stem_a_Potr, stem_b_Potr,stem_a_Pipo_Psme.C,stem_b_Pipo_Psme.C,stem_a_Psme.C,stem_b_Psme.C,stem_a_Psme.I,stem_b_Psme.I,
                                           stem_a_Pien,stem_b_Pien,stem_a_Pial,stem_b_Pial,stem_a_Thpl,stem_b_Thpl, stem_a_Pico,stem_b_Pico,stem_a_Abgr,stem_b_Abgr,stem_a_Pied,stem_b_Pied,stem_a_Pipo.N,
                                           stem_b_Pipo.N,stem_a_Pipo.S,stem_b_Pipo.S, list.density[[t]], list.stem[[t-1]], list.blank[[1]],"stem")))
    growth.stem[which(growth.stem < 0)] <- 0
    
    flux.neg <- growth.leaf + growth.branch + growth.stem
    flux.pos <- decompose + combusted.dead + combusted.live
    flux.net <- flux.pos-flux.neg
      
    flux.df <- rbind(flux.df, cbind(run_schedule[r,c(1:3)], t+1, .5*sum(as.vector(flux.net), na.rm = T)/1000, 
                                                                 .5*sum(as.vector(flux.pos), na.rm = T)/1000, 
                                                                 .5*sum(as.vector(flux.neg), na.rm = T)/1000,
                                                                 .5*sum(as.vector(combusted.live), na.rm = T)/1000,
                                                                 .5*sum(as.vector(combusted.dead), na.rm = T)/1000,
                                                                 .5*sum(as.vector(decompose), na.rm = T)/1000))
    
    # If year is greater than treatment spin-up, start collecting data
    if (t+1 >= t.spin){
      
      # Update rasters / arrays
      fireprob.ras <- fireprob.ras + mask_landscape(blank.ras, list.fprob[[t+1]])
      stressed.ras <- stressed.ras + mask_landscape(blank.ras, list.stressed[[t+1]])
      conn.ras <- conn.ras + mask_landscape(blank.ras, as.matrix(connectivity.calc(list.leaf[[t+1]], list.branch[[t+1]], list.stem[[t+1]])))
    
      # Aggregate flux data
      lbio.t1 <- mask_landscape(blank.ras, list.branch[[t+1]]) + 
        mask_landscape(blank.ras, list.leaf[[t+1]]) + 
        mask_landscape(blank.ras, list.stem[[t+1]])
      lbio.ras <- lbio.ras + lbio.t1
      dbio.t1 <- mask_landscape(blank.ras, list.snag[[t+1]]) + 
        mask_landscape(blank.ras, list.litter[[t+1]]) + 
        mask_landscape(blank.ras, list.cwd[[t+1]])
      dbio.ras <- dbio.ras + dbio.t1
      deco.ras <- deco.ras + decompose
      combd.ras <- combd.ras + combusted.dead
      combl.ras <- combl.ras + combusted.live
      comb.ras <- comb.ras + combusted.dead + combusted.live
      firekill.ras <- firekill.ras + firekilled.live
      flux.pos.ras <- flux.pos.ras + flux.pos
      flux.neg.ras <- flux.neg.ras + flux.neg
      flux.net.ras <- flux.net.ras + flux.net
      
      # Tracking crown kill
      ck.t1 <- list.ck[[t+1]]
      ck.t1[which(is.na(ck.t1) & (is.na(list.blank[[1]]) == F))] <- 0
      crownkill.ras <- crownkill.ras + ck.t1
      
      # Tracking forest transitions (to or from grass PFT)
      grass_to <- which((list.pft[[t+1]] == 0) & (list.pft[[t]] != 0))
      grass_to.ras[grass_to] <-  grass_to.ras[grass_to]+1
  
      grass_from <- which((list.pft[[t+1]] != 0) & (list.pft[[t]] == 0))
      grass_from.ras[grass_from] <- grass_from.ras[grass_from]+1
      
      grass_age.ras.prev <- grass_age.ras
      grass_age.ras <- grass_age.ras + as.numeric(list.pft[[t+1]] == 0)
      
      grass_year.ras[grass_to] <- n.years - (t+1)
      
    }
    
    # # Landscape Metrics
    # landscape_all <- raster(list.pft[[t+1]])
    # landscape_bin <- list.pft[[t+1]]
    # landscape_bin[which((landscape_bin != 0) & (is.na(landscape_bin) == F))] <- 1
    # landscape_bin <- raster(landscape_bin)
    # 
    # lsm_bin_cedge <- data.frame(lsm_c_te(landscape_bin, count_boundary = T, directions = 4))
    # lsm_all_cedge <- data.frame(lsm_c_te(landscape_all, count_boundary = T, directions = 4))
    # 
    # lsm_cedge <- data.frame(cbind((t+1), rbind(as.matrix(cbind("F/NF", lsm_bin_cedge)), as.matrix(cbind("PFTs", lsm_all_cedge)))))
    # names(lsm_cedge)[1:2] <- c("year", "cats")
    # lsm_cedge <- lsm_cedge[,c("year", "cats", "class", "metric", "value")]
    # 
    # if (exists("lsm_cedge.df") == F){
    #   lsm_cedge.df <- lsm_cedge
    # } else {
    #   lsm_cedge.df <- rbind(lsm_cedge.df, lsm_cedge)
    # }
    # 
    # lsm_bin_class_enn <- data.frame(right_join(lsm_c_enn_mn(landscape_bin),
    #                                            lsm_c_enn_sd(landscape_bin),
    #                                        by = c("layer", "level", "class", "id")))
    # lsm_all_class_enn <- data.frame(right_join(lsm_c_enn_mn(landscape_all),
    #                                            lsm_c_enn_sd(landscape_all),
    #                                            by = c("layer", "level", "class", "id")))
    # 

    
    # Update fire stats
    fire.ix <- which(list.fire.severity[[t+1]] > 0)
    fire.ix <- fire.ix[which(is.na(list.blank[[1]][fire.ix]) == F)]
    fire.id <- as.matrix(clump(raster(list.fire.severity[[t+1]]), directions=4))[fire.ix]
    
    # Update fire occurrence raster
    fire.ras <- fire.ras + (list.fire.severity[[t+1]] > 0)
    fire_sev.ras <- fire_sev.ras + list.fire.severity[[t+1]]
    fire_num.ras[fire.ix] <- fire_num.ras[fire.ix]+1
    
    # Look up max fire size
    cdf_lookup <- fireSize[fire.id,"CDF.lookup"]
    cdf_size <- fireSize[fire.id,"fire.size"]
    
    # Create unique fire IDs (start counting after last ID from previous years)
    if (is.null(fire.df) == F){
      fire.id <- fire.id + max(as.numeric(fire.df$Fire_ID), na.rm = T)
    }
    
    # Look up MTBS ID
    if (t+1 >= fire.years){
      fire.MTBS <- list.fire.MTBS[[t+1]][fire.ix]
    } else {
      fire.MTBS <- NA
    }
    
    # Calculate connectivity
    if (toString(unique(unlist(lapply(lapply(list(list.leaf[[t]], list.branch[[t]], list.stem[[t]]), as.vector), unique)))) != "NA, 0"){
      connect <- connectivity.calc(list.leaf[[t]], list.branch[[t]], list.stem[[t]])
    } else {
      connect <- list.blank[[1]]
    }
    
    # Set column names for fire data
    names.fire.df <- c("Run_ID", 
                       "Treatment",
                       "Treatment_type",
                       "Treatment_val",
                       "Treatment_rep",
                       "Year", 
                       "Fire_ID", 
                       "Cell", 
                       
                       "CDF_lookup",
                       "CDF_size",
                       "MTBS_ID",
                       
                       "Fire_probability",
                       "Crown_kill.perc",
                       "Stand_kill",
                       "Fire_Severity", 
                       
                       "PFT",
                       "PFT_post",
                       
                       "to_GrassShrub",
                       "age_GrassShrub",
                       
                       "Connectivity",
                       
                       "Combusted_litter",
                       "Combusted_CWD",
                       "Combusted_lbio",
                       "FireKill_lbio",
                       "Combusted_biomass",
                       
                       "Biomass_dead.pre", 
                       "Biomass_dead.post", 
                       "Biomass_dead.delta", 
                       
                       "Biomass_live.pre", 
                       "Biomass_live.post", 
                       "Biomass_live.delta", 
                       
                       "Biomass.delta", 

                       "Agency",
                       "Population", 
                       "Elevation")
    
    # If there are recorded fires, compile information
    if (length(fire.ix) > 0){
      
      df.temp <- data.frame(cbind(run,                                 # Run_ID
                                  run_name,                            # Treatment
                                  unlist(strsplit(run_name, "_"))[1],  # Treatment_type
                                  unlist(strsplit(run_name, "_"))[2],  # Treatment_val
                                  unlist(strsplit(run_name, "_"))[3],  # Treatment_rep
                                  t,                                   # Year
                                  fire.id,                             # Fire_ID
                                  fire.ix,                             # Cell
                                  
                                  cdf_lookup,                          # CDF_lookup
                                  cdf_size,                            # CDF_size
                                  fire.MTBS,                           # MTBS_ID
                                  
                                  list.fprob[[t+1]][fire.ix],          # Fire_probability
                                  list.ck[[t+1]][fire.ix],             # Crown_kill.perc
                                  list.dead.fire[[t+1]][fire.ix],      # Stand_kill
                                  list.fire.severity[[t+1]][fire.ix],  # Fire_severity
                                  list.pft[[t]][fire.ix],              # PFT
                                  list.pft[[t+1]][fire.ix],            # PFT_post
                                  
                                  as.numeric((list.pft[[t]][fire.ix] != 0) & (list.pft[[t+1]][fire.ix] == 0)), # to_GrassShrub
                                  0,                                   # age_GrassShrub
                                  
                                  connect[fire.ix],                    # Connectivity
                                  
                                  list.avail.litter[[t+1]][fire.ix],   # Combusted_litter
                                  list.avail.cwd[[t+1]][fire.ix],      # Combusted_CWD
                                  combusted.live[fire.ix],             # Combusted_lbio
                                  firekilled.live[fire.ix],            # Live biomass killed by fire
                                  
                                  (combusted.live+combusted.dead)[fire.ix], # biomass loss
                                  
                                  list.biomass_dead[[t]][fire.ix],     # Biomass_dead.pre
                                  list.biomass_dead[[t+1]][fire.ix],   # Biomass_dead.post
                                  list.biomass_dead[[t+1]][fire.ix]-list.biomass_dead[[t]][fire.ix], # Biomass_dead.delta
                                  
                                  list.biomass_live[[t]][fire.ix],     # Biomass_live.pre
                                  list.biomass_live[[t+1]][fire.ix],   # Biomass_live.post
                                  list.biomass_live[[t+1]][fire.ix]-list.biomass_live[[t]][fire.ix], # Biomass_live.delta
                                  
                                  ((list.biomass_live[[t+1]][fire.ix]-list.biomass_live[[t]][fire.ix])+(list.biomass_dead[[t+1]][fire.ix]-list.biomass_dead[[t]][fire.ix])), # Biomass.delta
                                  
                                  
                                  agency.ras[fire.ix],                 # Agency
                                  pop.ras[fire.ix],                    # Population
                                  elev[fire.ix]                        # Elevation
                                  ))
      
      names(df.temp) <- names.fire.df # Name columns to bind
      fire.df <- rbind(fire.df, df.temp) # Add new data
      fire.df <- alpnumlog(fire.df) # Format columns
      fire.df <- fire.df[order(as.numeric(fire.df$Fire_ID)),] # Sort rows
      row.names(fire.df) <- c(1:nrow(fire.df)) # Number rows
      fire.df <- fire.df[which(c(1:nrow(fire.df)) %in% na.margin(fire.df, "rows") == F),] # Remove any NA rows
    }
    
    # Count how many years grass cells remain before succession
    grass.ix <- which(fire.df$PFT_post == 0)
    fire.df$PFT_post[grass.ix] <- list.pft[[t+1]][fire.df$Cell[grass.ix]] 
    grass.ix <- which(fire.df$PFT_post == 0)
    fire.df$age_GrassShrub[grass.ix] <- fire.df$age_GrassShrub[grass.ix]+1

  }
  
  # If  this year is the final year, tidy and save data
  if ((t+1) == t.max){
    
    # Get mean values for rasters
    years.spun <- t.max - t.spin
    conn.ras <- conn.ras/length(years.spun)
    fireprob.ras <- fireprob.ras/length(years.spun)
    stressed.ras <- stressed.ras/length(years.spun)
    lbio.ras <- lbio.ras/length(years.spun)
    dbio.ras <- dbio.ras/length(years.spun)
    deco.ras <- ((0.5*deco.ras)/length(years.spun))/1000 # Convert to mT Carbon
    comb.ras <- ((0.5*comb.ras)/length(years.spun))/1000 # Convert to mT Carbon
    combd.ras <- ((0.5*combd.ras)/length(years.spun))/1000 # Convert to mT Carbon
    combl.ras <- ((0.5*combl.ras)/length(years.spun))/1000 # Convert to mT Carbon
    flux.pos.ras <- ((0.5*flux.pos.ras)/length(years.spun))/1000 # Convert to mT Carbon
    flux.neg.ras <- ((0.5*flux.neg.ras)/length(years.spun))/1000 # Convert to mT Carbon
    flux.net.ras <- ((0.5*flux.net.ras)/length(years.spun))/1000 # Convert to mT Carbon
    crownkill.ras <- crownkill.ras
    
    pftfin.ras <- brick(stack(lapply(list.pft[c(t.spin:length(list.pft))], raster)))
    pftfin.ras <- calc(pftfin.ras, getmode)
    
    # Save rasters
    raster::writeRaster(raster(fire.ras), paste0(path_save_ras, "Run",  sprintf("%03d", run), "RasFire", ".tiff"), overwrite = T)
    raster::writeRaster(raster(fire_sev.ras), paste0(path_save_ras, "Run",  sprintf("%03d", run), "RasFireSeverity", ".tiff"), overwrite = T)
    raster::writeRaster(raster(stressed.ras), paste0(path_save_ras, "Run",  sprintf("%03d", run), "RasStressed", ".tiff"), overwrite = T)
    raster::writeRaster(raster(grass_age.ras), paste0(path_save_ras, "Run",  sprintf("%03d", run), "RasGrassAge", ".tiff"), overwrite = T)
    raster::writeRaster(pftfin.ras, paste0(path_save_ras, "Run",  sprintf("%03d", run), "RasPFTfin", ".tiff"), overwrite = T)
    raster::writeRaster(raster(conn.ras), paste0(path_save_ras, "Run", sprintf("%03d", run), "Ras", "Connectivity", "Mean.tiff"), overwrite = T)
    raster::writeRaster(raster(fireprob.ras), paste0(path_save_ras, "Run", sprintf("%03d", run), "Ras", "FireProb", "Mean.tiff"), overwrite = T)
    raster::writeRaster(raster(lbio.ras), paste0(path_save_ras, "Run", sprintf("%03d", run), "Ras", "LiveBio", "Mean.tiff"), overwrite = T)
    raster::writeRaster(raster(dbio.ras), paste0(path_save_ras, "Run", sprintf("%03d", run), "Ras", "DeadBio", "Mean.tiff"), overwrite = T)
    raster::writeRaster(raster(deco.ras), paste0(path_save_ras, "Run", sprintf("%03d", run), "Ras", "Decomp", "Mean.tiff"), overwrite = T)
    raster::writeRaster(raster(comb.ras), paste0(path_save_ras, "Run", sprintf("%03d", run), "Ras", "Combusted", "Mean.tiff"), overwrite = T)
    raster::writeRaster(raster(combd.ras), paste0(path_save_ras, "Run", sprintf("%03d", run), "Ras", "CombustedDead", "Mean.tiff"), overwrite = T)
    raster::writeRaster(raster(combl.ras), paste0(path_save_ras, "Run", sprintf("%03d", run), "Ras", "CombustedLive", "Mean.tiff"), overwrite = T)
    raster::writeRaster(raster(crownkill.ras), paste0(path_save_ras, "Run", sprintf("%03d", run), "Ras", "CrownKill", "Cumu.tiff"), overwrite = T)
    raster::writeRaster(raster(flux.pos.ras), paste0(path_save_ras, "Run", sprintf("%03d", run), "Ras", "FluxPos", "Mean.tiff"), overwrite = T)
    raster::writeRaster(raster(flux.neg.ras), paste0(path_save_ras, "Run", sprintf("%03d", run), "Ras", "FluxNeg", "Mean.tiff"), overwrite = T)
    raster::writeRaster(raster(flux.net.ras), paste0(path_save_ras, "Run", sprintf("%03d", run), "Ras", "FluxNet", "Mean.tiff"), overwrite = T)
    
    # Save universal data
    # write.csv(lsm_cedge.df, paste0(path_save, "Run", sprintf("%03d", r), "_LandClassEdge.csv"), row.names = F)
    write.csv(fire.df, paste0(path_save, "Run", sprintf("%03d", r), "_FireStats.csv"), row.names = F)
    
    flux.df <- cbind(apply(flux.df[,c(1:3)], 1 , paste, collapse = "_"), flux.df)
    names(flux.df) <- c("treatment", "run_handle", "run_group", "run_rep", "year", "flux_net", "flux_pos", "flux_neg", "combust_live", "combust_dead", "decomp")
    flux.df <- flux.df %>% 
      subset(treatment %in% unique(flux.df$treatment)[unlist(lapply(unlist(lapply(unique(flux.df$treatment), strsplit, "_"), recursive = F), length)) == 3])
    write.csv(flux.df, paste0(path_save, "Run", sprintf("%03d", r), "_FluxC.csv"), row.names = F)
    
  }
  
}

