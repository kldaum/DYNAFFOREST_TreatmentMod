BATCH_status <- function(){
  system(command = paste0("squeue -u $LOGNAME > status.txt"))
  Sys.sleep(3)
  return(data.frame(read_table(paste0(path_scripts, "status.txt"))))
}

BATCH_submit <- function(rows, job.name, script.name){
  submitted <- NULL
  for (i in c(1:length(rows))){
    
    r <- rows[i]
    
    if (platform == "cluster"){
      
      RUNINFO <- paste0("'", path_runsched, "___", r, "'")
      
      while(r > length(search.files(path_save, c("run_schedule", "line"), ""))+50){Sys.sleep(60)}
      
      Sys.sleep(5)
      
      if (length(search.files(path_save, "BREAK", "")) > 0){
        system(command = "scancel --user=kldaum")
      }
      
      #SBATCH --job-name=", substr(batch_time, 3, nchar(batch_time)-2), "\n
      
      SCRIPT <- paste0(
        "#!/usr/bin/bash\n
#SBATCH --job-name=", job.name[i], "\n
#SBATCH --mail-type=END,FAIL\n
#SBATCH --mail-user=kldaum@ucsb.edu\n
#SBATCH --ntasks=1\n
#SBATCH --time=100:00:00\n
#SBATCH --output=", path_logs, "SLURM_RUN", sprintf("%03d", r), "_", run_schedule$Run_name[r], ".log\n\n
pwd; hostname; date\n
R -f ", path_scripts, script.name,"\n
date\n
"
      )
      
      if ((r %in% submitted) == F){
        
        fileConn<-file(paste0(path_scripts, "BATCH_P2_", sprintf("%03d", r),".sh"))
        writeLines(SCRIPT, fileConn)
        close(fileConn)
        
        system(command = paste0("sbatch ", "--export=RUNINFO=", RUNINFO, " ",path_scripts, "BATCH_P2_", sprintf("%03d", r),".sh"))
        file.remove(paste0(path_scripts, "BATCH_P2_", sprintf("%03d", r),".sh"))
        
        submitted <- c(submitted, r)
        
      }
    } 
  }
}

# Check for break command
check_break <- function(path.temp){
  if (length(search.files(path.temp, "BREAK", "")) > 0){
    if (platform.get() == "cluster"){
      cat(paste0("\n\n\n!!! APPLYING BREAK !!!\n\n\n"))
      system(command = "scancel --user=kldaum")
    }
  }
}

# Batch submit generic
BATCH_submitx <- function(inputs, input.name, job.name, script.name, path_logs){
  submitted <- NULL
  for (i in c(1:length(inputs))){
    
    input.temp <- inputs[i]
    
    if (platform == "cluster"){
      
      xRUNINFO <- paste0("'", input.name, "___", input.temp, "'")
      
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
#SBATCH --output=", path_logs, "SLURM_", job.name, "_", sprintf("%03d", i), ".log\n\n
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
}

# Identify platform
platform.get <- function(){
  if (grepl("MacBook", toString(Sys.getenv()))){
    platform <- "local"
  } else {
    platform <- "cluster"
  }
  return(platform)
}

# Choose folder path
folder.choose <- function(){
  
  pathf <- file.choose()
  return(paste0(paste(unlist(strsplit(pathf, "/"))[1:(length(unlist(strsplit(pathf, "/")))-1)], collapse = "/"), "/"))
  
}

# Get path(s) to file(s) with terms match in directory
search.files <- function(paths, terms, position){
  file.out <- NULL
  for (path.f in paths){
    terms_T <- terms[which(unlist(lapply(terms, substr, 1, 1)) != "!")]
    terms_F <- terms[which(unlist(lapply(terms, substr, 1, 1)) == "!")]
    terms_F <- unlist(lapply(terms_F, substrRight, -2))
    files.master <- list.files(path.f, full.names = F)
    files <- files.master
    files <- data.frame(cbind(files, files.master))
    
    # if ("ignore.case" %in% position){
    #   terms_T <- tolower(terms_T)
    #   terms_F <- tolower(terms_F)
    #   files$files <- tolower(files$files)
    # }
    
    if (("case_sensitive" %in% position) == F){
      terms_T <- tolower(terms_T)
      terms_F <- tolower(terms_F)
      files$files <- tolower(files$files)
    }
    
    if ("standardize.ext" %in% position){
      files$files <- c(gsub(".tiff", ".tif", gsub(".Tiff", ".Tif", files$files)))
      terms_T <- c(gsub(".tiff", ".tif", gsub(".Tiff", ".Tif", terms_T)))
      terms_F <- c(gsub(".tiff", ".tif", gsub(".Tiff", ".Tif", terms_F)))
    }
    
    if (sum(grepl("^", terms_T, fixed = T)) > 0){
      terms_Tstart <- terms_T[grep("^", terms_T, fixed = T)]
      terms_T <- terms_T[grepl("^", terms_T, fixed = T) == F]
    } else {
      terms_Tstart <- NULL
    }
    
    if (sum(grepl("^", terms_F, fixed = T)) > 0){
      terms_Fstart <- terms_F[grep("^", terms_F, fixed = T)]
      terms_F <- terms_F[grepl("^", terms_F, fixed = T) == F]
    } else {
      terms_Fstart <- NULL
    }
    
    if (length(terms_Tstart) > 0){
      files <- files[which(rowSums(as.data.frame(lapply(terms_Tstart, grepl, files$files, fixed = F))) == 1),]
    }
    
    if (length(terms_Fstart) > 0){
      files <- files[which(rowSums(as.data.frame(lapply(terms_Fstart, grepl, files$files, fixed = F))) == 0),]
    }
    
    if (length(terms_T) > 0){
      if ("any_match" %in% position == F){
        query <- data.frame(table(unlist(lapply(terms_T, grep, files$files, fixed = T))))
        result <- as.numeric(as.character(query$Var1[which(query$Freq == length(terms_T))]))
      } else {
        result <- unique(unlist(lapply(lapply(terms_T, grepl, files$files), which)))
      }
      files <- files[result,]
    }
    
    if (length(terms_F) > 0){
      ix.temp.f <- which(rowSums(as.data.frame(lapply(terms_F, grepl, files$files, fixed = T))) == 0)
      files <- files[ix.temp.f,]
    }
    
    if (length(files) > 0){
      files <- unlist(lapply(path.f, paste0, "/", files$files.master))
    }
    
    if ("first" %in% position){
      file.out <- c(file.out, files[1])
    } else {
      if ("last" %in% position){
        file.out <- c(file.out, files[length(files)])
      } else {
        file.out <- c(file.out, files)
      }
    }
    
    for (f in c(1:length(file.out))){
      file.temp <- unlist(strsplit(file.out[f], "/"))
      file.out[f] <- paste0("/", paste(file.temp[which(file.temp != "")], collapse = "/"))
    }
    
    if (("is.directory" %in% position) == F){
      file.out <- file.out[dir.exists(file.out) == F]
    } else {
      file.out <- file.out[dir.exists(file.out)]
      file.out <- unlist(lapply(file.out, paste0, "/"))
    }
    
    file.out <- file.out[which(gsub("/", "", file.out) != gsub("/", "", path.f))]
    file.out <- file.out[which(file.out != "//")]
    
    if ("name_only" %in% position){
      file.out <- unlist(lapply(file.out, strsplitsubr, "/", 1, 1))
    }
    
  }
  
  return(file.out)
  
}

# Unlist columns
unlist_cols <- function(df.temp.f){
  for (f in c(1:ncol(df.temp.f))){
    df.temp.f[,f] <- unlist(df.temp.f[,f])
  }
  return(df.temp.f)
}

# vector strsplit to columns
strsplit_to_cols <- function(vec, split, col_names){
  df.temp.f <- do.call(rbind, lapply(lapply(unlist(lapply(vec, strsplit, split), recursive = F), rbind), data.frame))
  names(df.temp.f) <- col_names
  return(df.temp.f)
}

# Read and append
read.csv.list <- function(list, index){
  if (length(index) == 1){
    index <- data.frame(cbind(rep(index, length(list))))
    names(index) <- "index"
  } else {
    index <- data.frame(index)
  }
  
  for (i in 1:length(list)){
    if (i == 1){
      df.temp <- cbind(index[i,], read.csv(list[i]))
    } else {
      df.temp <- plyr::rbind.fill(df.temp, cbind(index[i,], read.csv(list[i])))
    }
  }
  names(df.temp)[1] <- "index"
  return(df.temp)
}

# Detach all packages
detachAllPackages <- function() {
  
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  
}

# Append n uniform rows to end of data frame
append0 <- function(df, x, n){
  return(force_bind(df, data.frame(matrix(data = x, nrow = n, ncol = ncol(df)))))
}

# remove designated characters from strings
rmchars <- function(strings, characters){
  strings.return <- as.list(unlist(strings))
  strings <- unlist(strings)
  characters <- unlist(characters)
  for (i.f in c(1:length(strings))){
    string <- strings[i.f]
    for (character in characters){
      string <- gsub(character, "", string)
    }
    strings.return[i.f] <- string
  }
  return(strings.return)
}

# Linear extrapolation
linear_extrap <- function(x1, y1, x2, y2, x.out){
  m <- ((y2-y1)/(x2-x1))
  b <- y1 - (m*x1)
  return((m*(x.out))+b)
}

# Linear approximation
linear_approx <- function(vec.temp){
  
  # Length vec
  len <- length(vec.temp)
  
  # NAs vec
  val.ix <- which(is.na(vec.temp) == F)
  
  # Extrapolate Y1
  x1x2 <- val.ix[c(1, 2)]
  if ((1 %in% x1x2) == F){
    y1y2 <- vec.temp[x1x2]
    vec.temp[1] <- linear_extrap(x1x2[1], y1y2[1], x1x2[2], y1y2[2], 1)
  }
  
  # Extrapolate Yn
  x1x2 <- val.ix[c((length(val.ix)-1), length(val.ix))]
  if ((len %in% x1x2) == F){
    y1y2 <- vec.temp[x1x2]
    vec.temp[len] <- linear_extrap(x1x2[1], y1y2[1], x1x2[2], y1y2[2], len)
  }
  
  # Interpolate
  vec.temp <- zoo::na.approx(vec.temp)
  
  # Return
  return(vec.temp)
  
}

# Bind and ignore column names
force_bind = function(df1, df2) {
  colnames(df2) = colnames(df1)
  return(rbind(df1, df2))
}

# Calculate mode
getmode <- function(v) {
  uniqv <- unique(v)
  return(uniqv[which.max(tabulate(match(v, uniqv)))])
}

# Gaussian curve
gauss <- function(sequence, height, center, std){
  return(height*e^(-(((sequence-center)^2)/(2*std^2))))
}

# Sum of unique types
uniqcount <- function(vector){
  return(length(unique(vector[is.na(vector) == F])))
}

# Unique values in matrix
matrix_unique <- function(matrix){
  return(sort(unique(matrix[is.na(matrix) == F])))
}

# Get unique values for each frame in array
array_vals3 <- function(input_array, sort_or_neg_or_table){
  #lapply(lapply(lapply(lapply(input_array, unlist), as.vector), function(x) x<0), sum, na.rm = T)
  if (toString(class(input_array)) != "list"){
    input_array <- list(input_array)
  }
  output <- lapply(lapply(input_array, unlist), as.vector)
  output <- output[is.na(output) == F]
  if (grepl("unique", tolower(toString(sort_or_neg_or_table)))){
    output <- lapply(output, unique)
  }
  if (grepl("sort", tolower(toString(sort_or_neg_or_table)))){
    output <- lapply(output, sort)
  }
  if (grepl("neg", tolower(toString(sort_or_neg_or_table)))){
    output <- lapply(lapply(output, function(x) x<0), sum, na.rm = T)
  }
  if (grepl("table", tolower(toString(sort_or_neg_or_table)))){
    output <- lapply(output, table)
  }
  if (grepl("sum", tolower(toString(sort_or_neg_or_table)))){
    output <- lapply(output, sum, na.rm = T)
  }
  
  if (length(output) == 1){
    output <- unlist(output)
  }
  
  return(output)
}

# Subset n characters from end of string
substrRight <- function(str, n){
  if (n > 0){
    return(substr(str, nchar(str)-n+1, nchar(str)))
  }
  if (n < 0){
    return(substr(str, abs(n), nchar(str)))
  }
}

# Unique string count
unicount <- function(vector){
  count.vec <- vector(mode = "numeric", length = length(vector))
  for (val in unique(vector)){
    count.vec[which(vector == val)] <- cumsum(vector == val)[which(vector == val)]
  }
  return(count.vec)
}

# remove multiple slashes in URLs and file paths
monoslash <- function(string){
  vec.temp <- unlist(strsplit(string, "/"))
  vec.temp <- vec.temp[which(vec.temp != "")]
  string.new <- paste0("/", paste(vec.temp, collapse = "/"), "/")
  return(string.new)
}

# Detatch package
detach_package <- function(pkg, character.only = FALSE)
{
  if(!character.only)
  {
    pkg <- deparse(substitute(pkg))
  }
  search_item <- paste("package", pkg, sep = ":")
  while(search_item %in% search())
  {
    detach(search_item, unload = TRUE, character.only = TRUE)
  }
}

# Sort based on numerics in alphanumeric
sort_alpnum <- function(vec){
  vec1 <- vec[order(as.numeric(gsub("[^0-9.-]", "", vec)))]
  vec1 <- vec1[c(grep("CONTROL", toupper(vec1)), which(grepl("CONTROL", toupper(vec1)) == F))]
  return(vec1)
}

# Test class in data frame and set to alpha/numeric/logical where appropriate
alpnumlog <- function(data_frame){
  data_frame <- data.frame(sapply(data.frame(data_frame), as.character))
  df.temp <- data.frame(rbind(head(data_frame), tail(data_frame)))
  #unique(apply(data_frame[,numcols], FUN = typeof, MARGIN = 2))
  alphcols <- suppressWarnings(which(as.numeric(colSums(apply(apply(df.temp, MARGIN = 2, FUN = as.numeric), FUN = is.na, MARGIN = 2))) != 0))
  numcols <- which(c(1:ncol(df.temp)) %in% alphcols == F)
  logcols <- which(is.na(as.numeric(colSums(apply(df.temp, FUN = as.logical, MARGIN = 2)))) == F)
  if (length(numcols) > 0){data_frame[,numcols] <- data.frame(apply(data_frame[,numcols], FUN = as.numeric, MARGIN = 2))}
  if (length(logcols) > 0){data_frame[,logcols] <- data.frame(apply(data_frame[,logcols], FUN = as.logical, MARGIN = 2))}
  return(data_frame)
}

# Which whole rows/columns are NA
na.margin <- function(dataframe, rows_or_cols){
  marg <- which(match(c("rows", "cols"), rows_or_cols) == 1)
  nar <- as.numeric(which(rowSums(apply(dataframe, 2, is.na)) == ncol(dataframe)))
  nac <- as.numeric(which(colSums(apply(dataframe, 2, is.na)) == nrow(dataframe)))
  return(c(nar, nac)[marg])
}

# Get matrix of elements in list of lists split by split
lextract <- function(x, split){
  xspl <- strsplit(x, split)
  matx <- matrix(ncol = length(xspl[[1]]), nrow = length(xspl))
  for (l in c(1:dim(matx)[2])){
    matx[,l] <- unlist(lapply(xspl, `[[`, l))
  }
  return(matx)
}


# Select n:n-k elements from end of end of string split by j
strsplitsubr <- function(strings, split, n_elements, from_right){
  
  output <- as.list(rep("", length(strings)))
  
  for (f in c(1:length(strings))){
    
    string <- strings[f]
    
    if (split == "/"){
      string <- gsub("//", "/", string)
    }
    if (substrRight(string, 1) == split){
      string <- substr(string, 1, (nchar(string)-1))
    }
    stvec <- unlist(strsplit(string, split, fixed = T))
    if (from_right > 0){
      if (is.numeric(n_elements) == F){ n_elements <- from_right }
      str.temp <- stvec[c((length(stvec)-(from_right-1)):((length(stvec)-from_right)+(n_elements)))]
    } else {
      if (is.numeric(n_elements) == F){ 
        n_start <- 1 
      } else {
        n_start <- ((length(stvec)+from_right)-n_elements)
      }
      str.temp <- paste(stvec[c(n_start:(length(stvec)+from_right))], collapse = split)
      if (split == "/") { 
        str.temp <- paste0(str.temp, "/")
      }
    }
    
    output[f] <- str.temp
    
  }
  
  if (length(output) == 1){
    output <- unlist(output)
  }
  
  return(output)
  
}

# Unique values in arrays
array_vals1 <- function(list.name){
  
  vals <- data.frame(c(1:length(list.name)))
  names(vals) <- "Frame"
  
  for (i in c(1:nrow(vals))){
    for (u in c(NA, sort(unique(list.name[[i]][is.na(list.name[[i]]) == F])))){
      if (sum(grepl(toString(u), names(vals))) == 0){
        vals <- cbind(vals, 0)
        names(vals)[ncol(vals)] <- toString(u)
      }
      if (is.na(u)){
        vals[i,toString(u)] <- sum(is.na(list.name[[i]]))
      } else {
        vals[i,toString(u)] <- length(list.name[[i]][is.na(list.name[[i]]) == F][which(list.name[[i]][is.na(list.name[[i]]) == F] == u)])
      }
    }
  }
  return(vals)
}

# Unique values in arrays
array_vals2 <- function(array.name){
  
  array.temp <- array.name
  array.vals <- data.frame()
  
  for (i in c(1:length(array.temp))){
    for (u in c(NA, sort(unique(array.temp[[i]][is.na(array.temp[[i]]) == F])))){
      array.vals <- rbind(array.vals, cbind(
        i, u, length(array.temp[[i]][is.na(array.temp[[i]]) == F][which(array.temp[[i]][is.na(array.temp[[i]]) == F] == u)])
      ))
    }
  }
  names(array.vals) <- c("year", "val", "count")
  return(array.vals)
}

# Impose landscape mask (impose NA/numeric distribution of mask matrix/raster onto another matrix/raster)
mask_landscape <- function(basemap_input, raster_input){
  raster.temp <- as.matrix(raster_input)
  basemap.temp <- as.matrix(basemap_input)
  raster.temp[is.na(basemap.temp)] <- NA
  raster.temp[which((is.na(basemap.temp) == F) & (is.na(raster.temp)))] <- 0
  if (grepl("RASTER", toupper(class(raster_input)[1]))){
    raster.temp <- raster(raster.temp)
  }
  return(raster.temp)
}

# Rescale raster
rescale_raster <- function(ras, reverse){
  if (sum(class(ras) == "matrix") > 0){
    r.min <- min(ras[which(is.na(ras) == F)])
    r.max <- max(ras[which(is.na(ras) == F)])
  } else {
    r.min <- cellStats(ras, "min")
    r.max <- cellStats(ras, "max")
  }
  ras <- ((ras - r.min) / (r.max - r.min))
  if (reverse == T){
    ras <- invert_raster(as.matrix(ras))
  }
  return(ras)
}

# Load raster, matrixify
raster_matrix <- function(path, name, rescale, reverse){
  ras <- raster(paste0(path, name))
  if (rescale == T){
    ras <- rescale_raster(ras, reverse = reverse)
  }
  ras <- rbind(NA, cbind(NA, as.matrix(ras), NA), NA)
  return(ras)
}

# Reverse index
reverse_index <- function(vector){
  vec <- as.numeric(as.vector(vector))
  vec.ix <- c(max(unique(vector)):min(unique(vector)))
  vec.ix <- vec.ix[which(vec.ix %in% unique(vector))]
  vec.ix <- data.frame(cbind(sort(unique(vector)), vec.ix))
  vec.rev <- vec.ix$vec.ix[match(vec, vec.ix$V1)]
  return(as.vector(vec.rev))
}

# Invert raster
invert_raster <- function(ras){
  rasclass <- class(ras)[1]
  ras <- max(as.matrix(ras))-as.matrix(ras)
  if (rasclass == "RasterLayer"){
    ras <- raster(ras)
  }
  return(ras)
}

# PFT color function for plotting
pft_color <- function(sorting){
  if (sorting == "alph"){
    return(pft_names$color[match(sort(unique(df.temp$pft_name)), pft_names$Name)])
  }
  if (sorting == "num"){
    return(pft_names$color[match(sort(unique(df.temp$pft)), pft_names$ID)])
  }
}

# Generate timeframe / spin up
gen_spinup <- function(run_schedule, active){
  
  years.max <- floor(run_schedule$Years/10)*10
  years.spin <- rep(50, length(years.max))
  year_treat <- apply(cbind(run_schedule$P_thin_tstart, run_schedule$P_burn_tstart), FUN = min, MARGIN = 1)
  year_treat[which(year_treat == 1)] <- 0
  year_treat <- year_treat*(year_treat != 9999)
  
  years.spin[which(years.max >= 80)] <- 50 + year_treat
  years.spin[which(years.max < 80)] <- floor((years.max+year_treat)/3)
  years.spin[which(years.max <= 10)] <- floor(years.spin/10)*10
  years.spin[grepl("CONTROL", toupper(run_schedule$Run_group))] <- min(years.spin[grepl("CONTROL", toupper(run_schedule$Run_group)) == F])
  years.spin <- unique(data.frame(cbind(run_schedule$Run_group, years.spin, years.max)))
  
  names(years.spin) <- c("Group", "Spin", "Max")
  
  if (sum(grepl("CONTROL", toupper(years.spin$Group))) > 0){
    years.spin <- rbind(years.spin, years.spin[grep("CONTROL", toupper(years.spin$Group)),])
    years.spin[nrow(years.spin), "Group"] <- "*CONTROL"
  }
  
  years.spin$Spin <- as.numeric(years.spin$Spin)
  years.spin$Max <- as.numeric(years.spin$Max)
  
  if (active == F){
    years.spin$Spin <- 1
    years.spin$Max <- max(run_schedule$Years)
  }
  
  return(years.spin)
  
}

# Apply spin_up
apply_spinup <- function(df.temp, run_schedule){
  years.spin <- gen_spinup(run_schedule)
  df.temp$spin_up <- F
  for (g in unique(df.temp$group)){
    df.temp[which((df.temp$group == g) & (df.temp$year < as.numeric(years.spin$Spin[years.spin$Group == g]))), "spin_up"] <- T
    row.names(df.temp) <- c(1:nrow(df.temp))
    df.temp <- df.temp[which(row.names(df.temp) %in% which((df.temp$group == g) & (df.temp$year > as.numeric(years.spin$Max[years.spin$Group == g]))) == F),]
  }
  return(df.temp)
}
