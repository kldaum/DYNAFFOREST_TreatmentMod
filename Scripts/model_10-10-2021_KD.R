# ###########
# ########
# ###
# ########
# ###########
# #DYNAFFOREST V.1.01
# # Copyright (C) 2020-2030 Winslow D. Hansen
# #This This file is part of DYNAFFOREST V.1.01 and comes with with ABSOLUTELY NO WARRANTY; 
# #This is free software (licensed under GPL 3.0). 
# #You are welcome to redistribute it under certain conditions. See http://www.gnu.org/copyleft/gpl.html for more details.
# ###########
# ########
# ###
# ########
# ###########
# 
# 
# 
# #Load major libraries (additional libraries are called in specific functions.)
# #list:
# #tidyverse, raster, ncdf4, DBI, RSQLITE, landscapeR, igraph (might have missed a couple but the error messages will let you know
# 
# library(tidyverse)
# library(raster)
# library(ncdf4)
# l = data.frame(Sys.info()) #define what operating system you are working on.
# os =  l[1,1]
# ###If running on cluster use this block
# if(os == "Linux"){
# path= "/scratch/wh350/western.fire/"  #Define your path here to make it easy to read in files
# source("/cache/home/wh350/western.fire/forest-fire.model/development/model_functions_7-29-2021.R") # Read in the functions that run the model
# source("/cache/home/wh350/western.fire/forest-fire.model/development/fire_model_functions_7-29-2021.R")
# db.conn <- DBI::dbConnect(RSQLite::SQLite(), dbname="/cache/home/wh350/western.fire/forest-fire.model/development/pft_parameters.sqlite" )
# pft <- DBI::dbReadTable(db.conn, "pft_parameters") 
# DBI::dbDisconnect(db.conn)
# 
# db.conn <- DBI::dbConnect(RSQLite::SQLite(), dbname="/cache/home/wh350/western.fire/forest-fire.model/development/general_parameters.sqlite")
# gen <- DBI::dbReadTable(db.conn, "general_parameters") 
# DBI::dbDisconnect(db.conn)
# 
# }else{
# #If running on desktop use this block
# path= "D:/workspace/Winslow/western_veg_fire_model/model runs/calibration regions/"  #Define your path here to make it easy to read in files
# source("D:/workspace/Winslow/western_veg_fire_model/code.repository/forest-fire.model/development/model_functions_7-29-2021.R") # Read in the functions that run the model
# source("D:/workspace/Winslow/western_veg_fire_model/code.repository/forest-fire.model/development/fire_model_functions_7-29-2021.R") # read in the fire module functions
#! Edited KD silenced block above

# # Initiate log
# if (platform == "cluster"){sink(file = paste0(path_logs, "MODEL_Run", sprintf("%03d", run), "_log.txt"))} #!K

cat(paste0("\nRunning Model_10-10-2021_KD.R\n\n"))

loop <- F #!K

db.conn <- DBI::dbConnect(RSQLite::SQLite(), dbname=paste0(path_params, "pft_parameters.sqlite" ), synchronous = NULL) #! Edited KD path, synchronous NULL
pft <- DBI::dbReadTable(db.conn, "pft_parameters")
DBI::dbDisconnect(db.conn)

db.conn <- DBI::dbConnect(RSQLite::SQLite(), dbname=paste0(path_params, "general_parameters.sqlite" ), synchronous = NULL) #! Edited KD path, synchronous NULL
gen <- DBI::dbReadTable(db.conn, "general_parameters")
DBI::dbDisconnect(db.conn)

# }

####
#Convert parameters to individual atomic values for easy use in the model
####

pft=pft %>%
  pivot_wider(names_from = shortName, values_from = c(2:27))  ##This must be updated regularly when PFt parameter changes



for (i in 1:312){   # This must be updated regularly when PFT parameter changes
nam <- names(pft[,i])
    assign(nam, as.numeric(pft[1,i]))   
}
gen=as_tibble(gen)
gen=gen%>%mutate(test=0)
for (i in 1:39){
nam <- names(gen[,i])
    assign(nam, as.numeric(gen[1,i]))
}
pft=NULL
gen=NULL


#Set options for model run
# climate.yr.range = 16:45 #Define the years that you want to use from the climate dataset. 16:45 corresponds with the years 1965 to 1984
# take.snapshot=FALSE #Take a snapshot of a given year to do runs from?
# snapshot.year=4 #Model year to take snapshot.
# start_from.snapshot=FALSE #Start from a snapshot?
###Read and generate in the forcing drivers. this turns then from netcdf rasters into lists of matrices with a boundary of NAs around the edge of the study region
#I have calculated and produced all this driver data in a different script.
# if(os == "Linux"){ #! Edited KD silenced block
# # If running on cluster use this block
# args=commandArgs(TRUE)
# region=args[1]
# n.years=as.numeric(args[2])
# fire.size.mult=as.numeric(args[3])
# fire.freq.mult=as.numeric(args[4])
# }else{
##
# If running on desktop use this block
# n.years = 337 #Run length
# region = "yellowstone" # Define the study region This is only important if you actually plan to do more than one region. Then you can run them all from here just by changing the one flag.   
# fire.size.mult = 1 # Define whether you want fire sizes as parameterized or if you want to change fire sizes to simulate no suppression signal.
# fire.freq.mult = 1# Define whether you want fire frequencies as parameterized or if you want to change fire frequency to simulate no suppression signal.
# }
### #! Edited KD silenced block above

print(paste0("Beginning initialization of region ",region, " for years ", 1949 + min(climate.yr.range)," to ", 1949 + max(climate.yr.range)))
psi=stack(paste0(path_inputs, "psi.nc")) #! Edited KD path
cdata=psi[[climate.yr.range]]
list.psi=list()
for (i in 1:length(climate.yr.range)){ #! Edited KD
  
  twixt=as.matrix(cdata[[i]])
  twixt = rbind(NA, cbind(NA, twixt, NA), NA)

list.psi[[i]]=twixt
}

list.psi  = rep(list.psi,times = ceiling(n.years/length(climate.yr.range)))  # For historical simulations ( which all we are doing so far) We have a 70 observational record of soil moisture. If runs are longer than 70 years climate records are simply repeated. 

psi.2=calc(psi[[51:70]], median, na.rm=T)
twixt=as.matrix(psi.2)
twixt = rbind(NA, cbind(NA, twixt, NA), NA)
psi.median=twixt

psi=NULL
cdata=NULL


sm=stack(paste0(path_inputs, "sm_volum.nc")) #! Edited KD path
cdata=sm[[climate.yr.range]]

list.sm=list()
for (i in 1:length(climate.yr.range)){
  twixt=as.matrix(cdata[[i]])
  twixt = rbind(NA, cbind(NA, twixt, NA), NA)

list.sm[[i]]=twixt
}
list.sm  = rep(list.sm,times = ceiling(n.years/length(climate.yr.range)))

sm.2=max(sm)
twixt=as.matrix(sm.2)
twixt = rbind(NA, cbind(NA, twixt, NA), NA)
sm.max=twixt
sm=NULL
cdata=NULL

min.temp=stack(paste0(path_inputs, "min.temp.nc"))  #! Edited KD path
cdata=min.temp[[climate.yr.range]] #!!!
list.min.temp=list()
for (i in 1:length(climate.yr.range)){
  twixt=as.matrix(cdata[[i]])
  twixt = rbind(NA, cbind(NA, twixt, NA), NA)

list.min.temp[[i]]=twixt
}
list.min.temp  = rep(list.min.temp,times = ceiling(n.years/length(climate.yr.range)))
min.temp=NULL
cdata=NULL

chill.day=stack(paste0(path_inputs, "chill.days.nc")) #! Edited KD path
cdata=chill.day[[climate.yr.range]]
list.chill.day=list()
for (i in 1:length(climate.yr.range)){
  twixt=as.matrix(cdata[[i]])
  twixt = rbind(NA, cbind(NA, twixt, NA), NA)

list.chill.day[[i]]=twixt
}
list.chill.day  = rep(list.chill.day,times = ceiling(n.years/length(climate.yr.range)))
chill.day=NULL
cdata=NULL

frost.free=stack(paste0(path_inputs, "frost.free.days.nc"))  #! Edited KD path
cdata=frost.free[[climate.yr.range]]

list.frost.free=list()
for (i in 1:length(climate.yr.range)){
  twixt=as.matrix(cdata[[i]])
  twixt = rbind(NA, cbind(NA, twixt, NA), NA)

list.frost.free[[i]]=twixt
}
list.frost.free  = rep(list.frost.free,times = ceiling(n.years/length(climate.yr.range)))
frost.free=NULL
cdata=NULL

varNames=c("GDD.Potr","GDD.Pipo_Psme.C","GDD.Psme.C","GDD.Psme.I","GDD.Pien","GDD.Pial","GDD.Thpl","GDD.Pico","GDD.Abgr","GDD.Pied","GDD.Pipo.N","GDD.Pipo.S")
for (j in levels(as.factor(varNames))){
GDD=stack(paste0(path_inputs ,j,".nc"))  #! Edited KD path
cdata=GDD[[climate.yr.range]]

d=list()
for (i in 1:length(climate.yr.range)){
  twixt=as.matrix(cdata[[i]])
  twixt = rbind(NA, cbind(NA, twixt, NA), NA)
d[[i]]=twixt
}
d=rep(d,times = ceiling(n.years/length(climate.yr.range)))
assign(paste0("list.",j), d)
}
GDD=NULL
cdata=NULL
varNames=c("nfrostdays_afterbud.Potr","nfrostdays_afterbud.Pipo_Psme.C","nfrostdays_afterbud.Psme.C","nfrostdays_afterbud.Psme.I","nfrostdays_afterbud.Pien","nfrostdays_afterbud.Pial","nfrostdays_afterbud.Thpl","nfrostdays_afterbud.Pico","nfrostdays_afterbud.Abgr","nfrostdays_afterbud.Pied","nfrostdays_afterbud.Pipo.N","nfrostdays_afterbud.Pipo.S")
for (j in levels(as.factor(varNames))){
frost.after.budburst=stack(paste0(path_inputs, j,".nc")) #! Edited KD path
cdata=frost.after.budburst[[climate.yr.range]]
d=list()
for (i in 1:length(climate.yr.range)){
  twixt=as.matrix(cdata[[i]])
  twixt = rbind(NA, cbind(NA, twixt, NA), NA)
d[[i]]=twixt
}
d=rep(d,times = ceiling(n.years/length(climate.yr.range)))
assign(paste0("list.",j), d)
}
frost.after.budburst=NULL
cdata=NULL

varNames=c("GDD_budburst.Potr","GDD_budburst.Pipo_Psme.C","GDD_budburst.Psme.C","GDD_budburst.Psme.I","GDD_budburst.Pien","GDD_budburst.Pial","GDD_budburst.Thpl","GDD_budburst.Pico","GDD_budburst.Abgr","GDD_budburst.Pied","GDD_budburst.Pipo.N","GDD_budburst.Pipo.S")
for (j in levels(as.factor(varNames))){
bud.burst=stack(paste0(path_inputs, j,".nc")) #! Edited KD path
cdata=bud.burst[[climate.yr.range]]
values(cdata)[values(cdata) > 0] = 1
d=list()
for (i in 1:length(climate.yr.range)){
  twixt=as.matrix(cdata[[i]])
  twixt = rbind(NA, cbind(NA, twixt, NA), NA)
d[[i]]=twixt
}
d=rep(d,times = ceiling(n.years/length(climate.yr.range)))
assign(paste0("list.",j), d)
}
bud.burst=NULL
cdata=NULL
print(paste0("Finished loading climate data for region ", region))
####
#Next we generate the initial conditions for the model. PFT, forest age and height were all derived from gridded datasets. The others are started at zeros.
####
#Read in forests

if(start_from.snapshot == TRUE){
  cat(paste0("\nInitiating from pre-spin-up (snapshot) of ", gsub("/", "", gsub("year-", "", strsplitsubr(P_snapshot_init, "_", 1, 1))), " years...\n\n"))
}

if(start_from.snapshot == FALSE){
  pft=raster(paste0(path_inputs, "forest_grid.tif")) #! Edited KD path
}else{
  pft=raster(paste0(path_snapinit, "pft_snapshot.tif")) #! Edited KD path
}
pft=as.matrix(pft)
pft = rbind(NA, cbind(NA, pft, NA), NA)
list.pft=lapply(seq_len(n.years), function(X) pft)

if(start_from.snapshot == FALSE){
age = raster(paste0(path_inputs, "forest_age.tif")) #! Edited KD path
}else{
  age = raster(paste0(path_snapinit, "stand_age_snapshot.tif"))  #! Edited KD path
}
age=as.matrix(age)
age = rbind(NA, cbind(NA, age, NA), NA)
list.age = lapply(seq_len(n.years), function(X) age)

if(start_from.snapshot == FALSE){
H = raster(paste0(path_inputs, "forest_height.tif")) #! Edited KD path
}else{
  H = raster(paste0(path_snapinit, "height.m_snapshot.tif"))  #! Edited KD path
}
H=as.matrix(H)
H = rbind(NA, cbind(NA, H, NA), NA)
list.H = lapply(seq_len(n.years), function(X) H)

if(start_from.snapshot == FALSE){
cwd=raster(paste0(path_inputs, "cwd_grid.tif"))  #! Edited KD path
}else{
  cwd = raster(paste0(path_snapinit, "cwd.kg_snapshot.tif"))  #! Edited KD path
}
cwd=as.matrix(cwd)
cwd = rbind(NA, cbind(NA, cwd, NA), NA)
list.cwd=lapply(seq_len(n.years), function(X) cwd)

if(start_from.snapshot == FALSE){
snag=raster(paste0(path_inputs, "snag_grid.tif"))  #! Edited KD path
}else{
  snag =  raster(paste0(path_snapinit, "snag.kg_snapshot.tif"))  #! Edited KD path
}
snag=as.matrix(snag)
snag = rbind(NA, cbind(NA,  snag, NA), NA)
list.snag =lapply(seq_len(n.years), function(X) snag)

if(start_from.snapshot == FALSE){
litter=raster(paste0(path_inputs, "fwd_grid.tif"))  #! Edited KD path
}else{
  litter = raster(paste0(path_snapinit, "litter.kg_snapshot.tif"))  #! Edited KD path
}
litter=as.matrix(litter)
litter = rbind(NA, cbind(NA, litter, NA), NA)
list.litter=lapply(seq_len(n.years), function(X) litter)

### Generate the grids for created variables

year=matrix(1, ncol=ncol(pft)-2,nrow=nrow(pft)-2)
year = rbind(NA, cbind(NA, year, NA), NA)
list.year=lapply(seq_len(n.years), function(X) year)

if(start_from.snapshot == FALSE){
DBH = matrix(0, ncol=ncol(pft)-2,nrow=nrow(pft)-2)
}else{
  DBH = raster(paste0(path_snapinit, "DBH.cm_snapshot.tif"))  #! Edited KD path
  DBH=as.matrix(DBH)
}
DBH = rbind(NA, cbind(NA, DBH, NA), NA)
list.DBH=lapply(seq_len(n.years), function(X) DBH)

if(start_from.snapshot == FALSE){
stem = matrix(0, ncol=ncol(pft)-2,nrow=nrow(pft)-2)
}else{
  stem = raster(paste0(path_snapinit, "stem.biomass.kg_snapshot.tif"))  #! Edited KD path
  stem = as.matrix(stem)
}
stem = rbind(NA, cbind(NA, stem, NA), NA)
list.stem=lapply(seq_len(n.years), function(X) stem)

if(start_from.snapshot == FALSE){
leaf = matrix(0, ncol=ncol(pft)-2,nrow=nrow(pft)-2)
}else{
  leaf = raster(paste0(path_snapinit, "leaf.biomass.kg_snapshot.tif"))  #! Edited KD path
  leaf = as.matrix(leaf)
}
leaf = rbind(NA, cbind(NA, leaf, NA), NA)
list.leaf=lapply(seq_len(n.years), function(X) leaf)

if(start_from.snapshot == FALSE){
branch = matrix(0, ncol=ncol(pft)-2,nrow=nrow(pft)-2)
}else{
  branch = raster(paste0(path_snapinit, "branch.biomass.kg_snapshot.tif"))  #! Edited KD path
  branch = as.matrix(branch)
}
branch = rbind(NA, cbind(NA, branch, NA), NA)
list.branch=lapply(seq_len(n.years), function(X) branch)

if(start_from.snapshot == FALSE){
density = matrix(NA, ncol=ncol(pft)-2,nrow=nrow(pft)-2)
}else{
  density = raster(paste0(path_snapinit, "stand.density.ha_snapshot.tif"))  #! Edited KD path
  density = as.matrix(density)
}
density = rbind(NA, cbind(NA, density, NA), NA)
list.density=lapply(seq_len(n.years), function(X) density)

stress = matrix(0, ncol=ncol(pft)-2,nrow=nrow(pft)-2)
stress = rbind(NA, cbind(NA, stress, NA), NA)
list.stress = lapply(seq_len(n.years), function(X) stress)

background = matrix(0, ncol=ncol(pft)-2,nrow=nrow(pft)-2)
background = rbind(NA, cbind(NA, background, NA), NA)
list.background = lapply(seq_len(n.years), function(X) background)

death_prob = matrix(0, ncol=ncol(pft)-2,nrow=nrow(pft)-2)
death_prob = rbind(NA, cbind(NA, death_prob, NA), NA)
list.death_prob = lapply(seq_len(n.years), function(X) death_prob)

if(start_from.snapshot == FALSE){
stressed = matrix(0, ncol=ncol(pft)-2,nrow=nrow(pft)-2)
}else{
  stressed = raster(paste0(path_snapinit, "stressed_snapshot.tif"))  #! Edited KD path
  stressed = as.matrix(stressed)
}
stressed = rbind(NA, cbind(NA, stressed, NA), NA)
list.stressed = lapply(seq_len(n.years), function(X) stressed)

if(start_from.snapshot == FALSE){
dead = matrix(0, ncol=ncol(pft)-2,nrow=nrow(pft)-2)
}else{
  dead = raster(paste0(path_snapinit, "dead_snapshot.tif"))  #! Edited KD path
  dead = as.matrix(dead)
}
dead = rbind(NA, cbind(NA, dead, NA), NA)
list.dead = lapply(seq_len(n.years), function(X) dead)

min.temp.flag=matrix(0, ncol=ncol(pft)-2,nrow=nrow(pft)-2)
min.temp.flag= rbind(NA, cbind(NA, min.temp.flag, NA), NA)
min.temp.flag1=lapply(seq_len(12), function(X) min.temp.flag)  # sequence length set to five for the number of PFTs
list.min.temp.flag=lapply(seq_len(n.years), function(X) min.temp.flag1)

GDD.flag=matrix(0, ncol=ncol(pft)-2,nrow=nrow(pft)-2)
GDD.flag= rbind(NA, cbind(NA, GDD.flag, NA), NA)
GDD.flag1=lapply(seq_len(12), function(X) GDD.flag)  # sequence length set to five for the number of PFTs
list.GDD.flag=lapply(seq_len(n.years), function(X) GDD.flag1)

chill.day.flag=matrix(0, ncol=ncol(pft)-2,nrow=nrow(pft)-2)
chill.day.flag= rbind(NA, cbind(NA, chill.day.flag, NA), NA)
chill.day.flag1=lapply(seq_len(12), function(X) chill.day.flag)  # sequence length set to five for the number of PFTs
list.chill.day.flag=lapply(seq_len(n.years), function(X) chill.day.flag1)

frost.free.flag=matrix(0, ncol=ncol(pft)-2,nrow=nrow(pft)-2)
frost.free.flag= rbind(NA, cbind(NA, frost.free.flag, NA), NA)
frost.free.flag1=lapply(seq_len(12), function(X) frost.free.flag)  # sequence length set to five for the number of PFTs
list.frost.free.flag=lapply(seq_len(n.years), function(X) frost.free.flag1)

GDD_budburst.flag=matrix(0, ncol=ncol(pft)-2,nrow=nrow(pft)-2)
GDD_budburst.flag= rbind(NA, cbind(NA, GDD_budburst.flag, NA), NA)
GDD_budburst.flag1=lapply(seq_len(12), function(X) GDD_budburst.flag)  # sequence length set to five for the number of PFTs
list.GDD_budburst.flag=lapply(seq_len(n.years), function(X) GDD_budburst.flag1)

p_estab=matrix(0, ncol=ncol(pft)-2,nrow=nrow(pft)-2)
p_estab= rbind(NA, cbind(NA, p_estab, NA), NA)

p_estab.1=lapply(seq_len(12), function(X) p_estab)  # sequence length set to five for the number of PFTs

list.p_estab.abiotic=lapply(seq_len(n.years), function(X) p_estab.1)
list.p_estab.dispersal=lapply(seq_len(n.years), function(X) p_estab.1)
dispersal=matrix(0,ncol=ncol(pft)-2,nrow=nrow(pft)-2)
dispersal= rbind(NA, cbind(NA, dispersal, NA), NA)
dispersal.1=lapply(seq_len(12), function(X) dispersal)  # sequence length set to five for the number of PFTs
list.prob.dispersal=lapply(seq_len(n.years), function(X) dispersal.1)

established=matrix(0, ncol=ncol(pft)-2,nrow=nrow(pft)-2)
established= rbind(NA, cbind(NA, established, NA), NA)
list.e=lapply(seq_len(n.years), function(X) established)
list.established=lapply(seq_len(n.years), function(X) established)
fire = matrix(0, ncol=ncol(pft)-2,nrow=nrow(pft)-2)
fire = rbind(NA, cbind(NA, fire, NA), NA)
list.fire = lapply(seq_len(n.years), function(X) fire)
fire.severity = matrix(0, ncol=ncol(pft)-2,nrow=nrow(pft)-2)
fire.severity = rbind(NA, cbind(NA, fire.severity, NA), NA)
list.fire.severity = lapply(seq_len(n.years), function(X) fire.severity)

avail.litter = matrix(0, ncol=ncol(pft)-2,nrow=nrow(pft)-2)
avail.litter = rbind(NA, cbind(NA, avail.litter, NA), NA)
list.avail.litter = lapply(seq_len(n.years), function(X) avail.litter)

avail.cwd = matrix(0, ncol=ncol(pft)-2,nrow=nrow(pft)-2)
avail.cwd = rbind(NA, cbind(NA, avail.cwd, NA), NA)
list.avail.cwd = lapply(seq_len(n.years), function(X) avail.cwd)

ck = matrix(0, ncol=ncol(pft)-2,nrow=nrow(pft)-2)
ck = rbind(NA, cbind(NA, ck, NA), NA)
list.ck = lapply(seq_len(n.years), function(X) ck)

dead.fire = matrix(0, ncol=ncol(pft)-2,nrow=nrow(pft)-2)
dead.fire = rbind(NA, cbind(NA, dead.fire, NA), NA)
list.dead.fire = lapply(seq_len(n.years), function(X) dead.fire)



#Reading in fire products

fire.MTBS=stack(paste0(path_inputs, "fires.MTBS.nc"))  #! Edited KD path  # This is the observational record of fires for the period 1984-2019. The model is setup so actual fires are actually implemented for the last 37 years of model runs. All other years will have fire dynamically simulated
forest_buffer=raster(paste0(path_inputs, "forest_buffer.tif"))  #! Edited KD path
list.fire.MTBS=list()
for (i in 1:37){
  twixt=implement.MTBS(fire.MTBS[[i]])
  list.fire.MTBS[[i]]=twixt
}
fire.MTBS=NULL
# fire.years=n.years-36 #! Edited KD commented out

p_pet.mean = raster(paste0(path_inputs, "p_pet.mean.tif"))  #! Edited KD path
pop = raster(paste0(path_inputs, "pop.tif"))  #! Edited KD path
strikes.mean = raster(paste0(path_inputs, "strikes.mean.tif"))  #! Edited KD path
slope = raster(paste0(path_inputs, "slope.tif")) #! Edited KD path
elev.std = raster(paste0(path_inputs, "elev.std.tif")) #! Edited KD path
elev.fine = raster(paste0(path_inputs, "elev_grid.tif")) #! Edited KD path
#Convert fuels data from the main model to a raster This is the stardizing table with means and SDs for each region and variable
st=read.table(paste0(path_inputs, "",region,"_predictor_mean_standard_deviation.txt"),sep=",",header = F)
fire.size.draws=read.table(paste0(path_inputs, "",region,"_Table_RandomFireSizeDraws.txt"), sep = ",", header = F)  # The Fire size draws table

#Calculate the log10 of the fire variables
if(region =="total_domain"){
  p_pet.mean.log10 = calc.log10(p_pet.mean, Q01_aridityindex_td)
  pop.log10 = calc.log10(pop, Q01_popdensity_td)
  strikes.mean.log10 = calc.log10(strikes.mean, Q01_lightning_td)
  slope.log10 = calc.log10(slope, Q01_slope_td)
  elev.std.log10 = calc.log10(elev.std, Q01_elevstd_td)
}else{
  p_pet.mean.log10 = calc.log10(p_pet.mean, Q01_aridityindex_r)
  pop.log10 = calc.log10(pop, Q01_popdensity_r)
  strikes.mean.log10 = calc.log10(strikes.mean, Q01_lightning_r)
  slope.log10 = calc.log10(slope, Q01_slope_r)
  elev.std.log10 = calc.log10(elev.std, Q01_elevstd_r)
}
#Standardize the fire variables
  slope = standardize(slope, 15)
  elev.std = standardize(elev.std, 17)
  p_pet.mean = standardize(p_pet.mean, 19)
  strikes.mean =standardize(strikes.mean, 21)
  pop = standardize(pop, 23)
  slope.log10 = standardize(slope.log10, 16)
  elev.std.log10 = standardize(elev.std.log10, 18)
  p_pet.mean.log10 = standardize(p_pet.mean.log10, 20)
  strikes.mean.log10 =standardize(strikes.mean.log10, 22)
  pop.log10 = standardize(pop.log10, 24)
print(paste0("Finished loading init files for region ", region))


####
#Run model
####
print("Beginning run loop")
ptm <- proc.time()

for (t in 1:(n.years-1)){   
  loop <- T #!K
  
  print(paste("Starting simulation of year ",t+1,"",sep = ""))
  list.year[[t+1]] = list.year[[t]]+1
  list.age[[t+1]]=   list.age[[t]]+1

  #!K MISC (commands separated by semicolon, must contain single quote marks only)
    module_name <- "misc"
    source(paste0(path_scripts, "Modules_KD.R"))
    
  # Step 1 March of T+1: Calculate what died from stress and fire in the previous year  
  #At the beginning of the growing season, did the tree die in the previous season??
  list.background[[t+1]] = prob_intrin(list.pft[[t]]) # Increasing probability of a tree dying from old age and reduced productivity and age at the end of last august
  list.stress[[t+1]] = prob_stress(list.pft[[t]],list.psi[[t]], psi.median)  # stress in last march, PSI from last growing season and pft last August
  l=matrix(runif(ncell(list.stress[[t+1]]),0,1),nrow= nrow(list.stress[[t+1]]), ncol=ncol(list.stress[[t+1]])) #These probabilities are used in the next line
  list.stressed[[t+1]] = ifelse(list.stress[[t+1]]>l,1,0)+list.stressed[[t]]  # is it officially stressed in March
  f.s= fire.severity.calc(list.fire[[t]],list.sm[[t]],sm.max,list.litter[[t]],list.cwd[[t]],list.DBH[[t]],list.pft[[t]]) # Returns the available fuels, percent crown kill and probability of tree death 
  list.avail.litter[[t+1]] = f.s[[1]]
  list.avail.cwd[[t+1]] = f.s[[2]]
  list.ck[[t+1]] = f.s[[3]]
  list.fire.severity[[t+1]] = f.s[[4]]
  die= dead.calc(list.dead[[t]], list.background[[t+1]], list.stressed[[t+1]], list.fire.severity[[t+1]], list.established[[t]]) #Is the forest now dead because of stress or fire as of march
  list.dead.fire[[t+1]]=die[[1]]  # is it dead from fire?
  list.dead[[t+1]]=die[[2]]       # is it dead from fire or stress?
  
  # Step 2 in March of T+1: Calculate all the litter that resulted from the tree deaths and the previous growing season
  list.snag[[t+1]] = ifelse(list.dead[[t+1]] == 1,  snag_pool(list.snag[[t]],e,snag_hl)+list.stem[[t]],snag_pool(list.snag[[t]],e,snag_hl))
  
  
  list.litter[[t+1]] = dead_influx(list.pft[[t]],list.dead[[t+1]],list.fire[[t]],list.litter[[t]],decomp_litter,list.leaf[[t]],turnover_leaf_Potr,
                                     turnover_leaf_Pipo_Psme.C,turnover_leaf_Psme.C,turnover_leaf_Psme.I,turnover_leaf_Pien,turnover_leaf_Pial,turnover_leaf_Thpl,
                                     turnover_leaf_Pico,turnover_leaf_Abgr,turnover_leaf_Pied,turnover_leaf_Pipo.N,turnover_leaf_Pipo.S,0.9,list.ck[[t+1]], list.avail.litter[[t+1]] )
  
  list.cwd[[t+1]] = dead_influx(list.pft[[t]],list.dead[[t+1]],list.fire[[t]],list.cwd[[t]],decomp_cwd,list.branch[[t]],turnover_branch_Potr,
                                     turnover_branch_Pipo_Psme.C,turnover_branch_Psme.C,turnover_branch_Psme.I,turnover_branch_Pien,turnover_branch_Pial,turnover_branch_Thpl,
                                     turnover_branch_Pico,turnover_branch_Abgr,turnover_branch_Pied,turnover_branch_Pipo.N,turnover_branch_Pipo.S,0.5,
                                     list.ck[[t+1]], list.avail.cwd[[t+1]])+(list.snag[[t]]-snag_pool(list.snag[[t]],e,snag_hl))  
  
  
  #step 3 August of T + 1: Now at the end of the growing season we calculate what establishes if something has died
  list.min.temp.flag[[t+1]] = establishment.threshold1.calc(list.dead[[t+1]],list.min.temp[[t+1]],min.temp.threshold_Potr,min.temp.threshold_Pipo_Psme.C,min.temp.threshold_Psme.C,
                                         min.temp.threshold_Psme.I,min.temp.threshold_Pien,min.temp.threshold_Pial,min.temp.threshold_Thpl,
                                         min.temp.threshold_Pico,min.temp.threshold_Abgr,min.temp.threshold_Pied,min.temp.threshold_Pipo.N,min.temp.threshold_Pipo.S)
  list.frost.free.flag[[t+1]] = establishment.threshold1.calc(list.dead[[t+1]],list.frost.free[[t+1]],frost.free.threshold_Potr,frost.free.threshold_Pipo_Psme.C,frost.free.threshold_Psme.C,
                                         frost.free.threshold_Psme.I,frost.free.threshold_Pien,frost.free.threshold_Pial,frost.free.threshold_Thpl,
                                         frost.free.threshold_Pico,frost.free.threshold_Abgr,frost.free.threshold_Pied,frost.free.threshold_Pipo.N,frost.free.threshold_Pipo.S)
  list.chill.day.flag[[t+1]] = establishment.threshold1.calc(list.dead[[t+1]],list.chill.day[[t+1]],chill.days.threshold_Potr,chill.days.threshold_Pipo_Psme.C,chill.days.threshold_Psme.C,
                                         chill.days.threshold_Psme.I,chill.days.threshold_Pien,chill.days.threshold_Pial,chill.days.threshold_Thpl,
                                         chill.days.threshold_Pico,chill.days.threshold_Abgr,chill.days.threshold_Pied,chill.days.threshold_Pipo.N,chill.days.threshold_Pipo.S)
  list.GDD.flag[[t+1]] = establishment.threshold2.calc(list.dead[[t+1]],  list.GDD.Potr[[t+1]], list.GDD.Pipo_Psme.C[[t+1]],list.GDD.Psme.C[[t+1]],list.GDD.Psme.I[[t+1]], list.GDD.Pien[[t+1]],list.GDD.Pial[[t+1]],
                                         list.GDD.Thpl[[t+1]],list.GDD.Pico[[t+1]],list.GDD.Abgr[[t+1]],list.GDD.Pied[[t+1]],list.GDD.Pipo.N[[t+1]],list.GDD.Pipo.S[[t+1]],
                                         GDD.threshold.min_Potr, GDD.threshold.max_Potr, GDD.threshold.min_Pipo_Psme.C,
                                         GDD.threshold.max_Pipo_Psme.C,GDD.threshold.min_Psme.C, GDD.threshold.max_Psme.C,
                                         GDD.threshold.min_Psme.I, GDD.threshold.max_Psme.I, GDD.threshold.min_Pien, GDD.threshold.max_Pien,
                                         GDD.threshold.min_Pial, GDD.threshold.max_Pial, GDD.threshold.min_Thpl, GDD.threshold.max_Thpl,
                                         GDD.threshold.min_Pico, GDD.threshold.max_Pico,GDD.threshold.min_Abgr, GDD.threshold.max_Abgr,
                                         GDD.threshold.min_Pied, GDD.threshold.max_Pied,GDD.threshold.min_Pipo.N, GDD.threshold.max_Pipo.N,
                                         GDD.threshold.min_Pipo.S, GDD.threshold.max_Pipo.S)
  list.GDD_budburst.flag[[t+1]] = establishment.threshold3.calc(list.dead[[t+1]],list.GDD_budburst.Potr[[t+1]], list.GDD_budburst.Pipo_Psme.C[[t+1]], list.GDD_budburst.Psme.C[[t+1]], list.GDD_budburst.Psme.I[[t+1]],
                                                                list.GDD_budburst.Pien[[t+1]],list.GDD_budburst.Pial[[t+1]], list.GDD_budburst.Thpl[[t+1]], list.GDD_budburst.Pico[[t+1]],
                                                                list.GDD_budburst.Abgr[[t+1]],list.GDD_budburst.Pied[[t+1]],list.GDD_budburst.Pipo.N[[t+1]],list.GDD_budburst.Pipo.S[[t+1]],
                                                                bud.burst.threshold_Potr, bud.burst.threshold_Pipo_Psme.C,bud.burst.threshold_Psme.C,bud.burst.threshold_Psme.I,
                                                                bud.burst.threshold_Pien, bud.burst.threshold_Pial, bud.burst.threshold_Thpl,bud.burst.threshold_Pico,
                                                                bud.burst.threshold_Abgr,bud.burst.threshold_Pied,bud.burst.threshold_Pipo.N,bud.burst.threshold_Pipo.S)
  list.p_estab.abiotic[[t+1]] = p_estab.abiotic.calc(list.dead[[t+1]],list.min.temp.flag[[t+1]][[1]],list.GDD.flag[[t+1]][[1]],list.chill.day.flag[[t+1]][[1]],list.GDD_budburst.flag[[t+1]][[1]],list.nfrostdays_afterbud.Potr[[t+1]],
                                                 list.min.temp.flag[[t+1]][[2]],list.GDD.flag[[t+1]][[2]],list.chill.day.flag[[t+1]][[2]],list.GDD_budburst.flag[[t+1]][[2]], list.nfrostdays_afterbud.Pipo_Psme.C[[t+1]],
                                                 list.min.temp.flag[[t+1]][[3]],list.GDD.flag[[t+1]][[3]],list.chill.day.flag[[t+1]][[3]],list.GDD_budburst.flag[[t+1]][[3]],list.nfrostdays_afterbud.Psme.C[[t+1]],
                                                 list.min.temp.flag[[t+1]][[4]],list.GDD.flag[[t+1]][[4]],list.chill.day.flag[[t+1]][[4]],list.GDD_budburst.flag[[t+1]][[4]],list.nfrostdays_afterbud.Psme.I[[t+1]],
                                                 list.min.temp.flag[[t+1]][[5]],list.GDD.flag[[t+1]][[5]],list.chill.day.flag[[t+1]][[5]],list.GDD_budburst.flag[[t+1]][[5]],list.nfrostdays_afterbud.Pien[[t+1]],
                                                 list.min.temp.flag[[t+1]][[6]],list.GDD.flag[[t+1]][[6]],list.chill.day.flag[[t+1]][[6]],list.GDD_budburst.flag[[t+1]][[6]],list.nfrostdays_afterbud.Pial[[t+1]],
                                                 list.min.temp.flag[[t+1]][[7]],list.GDD.flag[[t+1]][[7]],list.chill.day.flag[[t+1]][[7]],list.GDD_budburst.flag[[t+1]][[7]],list.nfrostdays_afterbud.Thpl[[t+1]],
                                                 list.min.temp.flag[[t+1]][[8]],list.GDD.flag[[t+1]][[8]],list.chill.day.flag[[t+1]][[8]],list.GDD_budburst.flag[[t+1]][[8]],list.nfrostdays_afterbud.Pico[[t+1]],
                                                 list.min.temp.flag[[t+1]][[9]],list.GDD.flag[[t+1]][[9]],list.chill.day.flag[[t+1]][[9]],list.GDD_budburst.flag[[t+1]][[9]],list.nfrostdays_afterbud.Abgr[[t+1]],
                                                 list.min.temp.flag[[t+1]][[10]],list.GDD.flag[[t+1]][[10]],list.chill.day.flag[[t+1]][[10]],list.GDD_budburst.flag[[t+1]][[10]],list.nfrostdays_afterbud.Pied[[t+1]],
                                                 list.min.temp.flag[[t+1]][[11]],list.GDD.flag[[t+1]][[11]],list.chill.day.flag[[t+1]][[11]],list.GDD_budburst.flag[[t+1]][[11]],list.nfrostdays_afterbud.Pipo.N[[t+1]],
                                                 list.min.temp.flag[[t+1]][[12]],list.GDD.flag[[t+1]][[12]],list.chill.day.flag[[t+1]][[12]],list.GDD_budburst.flag[[t+1]][[12]],list.nfrostdays_afterbud.Pipo.S[[t+1]],list.psi[[t+1]])
  list.p_estab.dispersal[[t+1]] = p_dispersal.calc(list.dead[[t+1]],list.dead.fire[[t+1]],list.pft[[t]],list.age[[t]])
   test= established_calc(list.dead.fire[[t+1]],list.age[[t]],list.pft[[t]],
                          list.p_estab.abiotic[[t+1]][[1]],list.p_estab.dispersal[[t+1]][[1]],
                          list.p_estab.abiotic[[t+1]][[2]],list.p_estab.dispersal[[t+1]][[2]],
                          list.p_estab.abiotic[[t+1]][[3]],list.p_estab.dispersal[[t+1]][[3]],
                          list.p_estab.abiotic[[t+1]][[4]],list.p_estab.dispersal[[t+1]][[4]],
                          list.p_estab.abiotic[[t+1]][[5]],list.p_estab.dispersal[[t+1]][[5]],
                          list.p_estab.abiotic[[t+1]][[6]],list.p_estab.dispersal[[t+1]][[6]],
                          list.p_estab.abiotic[[t+1]][[7]],list.p_estab.dispersal[[t+1]][[7]],
                          list.p_estab.abiotic[[t+1]][[8]],list.p_estab.dispersal[[t+1]][[8]],
                          list.p_estab.abiotic[[t+1]][[9]],list.p_estab.dispersal[[t+1]][[9]],
                          list.p_estab.abiotic[[t+1]][[10]],list.p_estab.dispersal[[t+1]][[10]],
                          list.p_estab.abiotic[[t+1]][[11]],list.p_estab.dispersal[[t+1]][[11]],
                          list.p_estab.abiotic[[t+1]][[12]],list.p_estab.dispersal[[t+1]][[12]])
  
  list.pft[[t+1]]= ifelse(test[[2]]==1,test[[1]],
                   ifelse(list.dead[[t+1]]==0,list.pft[[t]],0))   # reset pft at the end of August if something established
  list.established[[t+1]]=test[[2]]
  
  #Step 4 August of T+1: Tree growth and biomass calculations if trees were not pronounced dead in August based on last years PFT This is okay because areas where forest did change over are about to be reset
  list.H[[t+1]] = height_increment(list.pft[[t]],list.H[[t]]) #Update height increment based on the height last August
  list.DBH[[t+1]]= dbh_increment(list.H[[t+1]],list.pft[[t]])
  list.density[[t+1]] = density_est(list.pft[[t]],list.DBH[[t+1]])
  list.stem[[t+1]] = biomass(list.pft[[t]],list.fire[[t]],list.dead[[t+1]], list.DBH[[t+1]], stem_a_Potr, stem_b_Potr,stem_a_Pipo_Psme.C,stem_b_Pipo_Psme.C,stem_a_Psme.C,stem_b_Psme.C,stem_a_Psme.I,stem_b_Psme.I,
                               stem_a_Pien,stem_b_Pien,stem_a_Pial,stem_b_Pial,stem_a_Thpl,stem_b_Thpl, stem_a_Pico,stem_b_Pico,stem_a_Abgr,stem_b_Abgr,stem_a_Pied,stem_b_Pied,stem_a_Pipo.N,
                               stem_b_Pipo.N,stem_a_Pipo.S,stem_b_Pipo.S, list.density[[t+1]], list.stem[[t]], list.ck[[t+1]],"stem")#estimate stem biomass
  list.leaf[[t+1]] = biomass(list.pft[[t]],list.fire[[t]],list.dead[[t+1]],list.DBH[[t+1]],leaf_a_Potr, leaf_b_Potr,leaf_a_Pipo_Psme.C,leaf_b_Pipo_Psme.C,leaf_a_Psme.C,leaf_b_Psme.C,leaf_a_Psme.I,leaf_b_Psme.I,
                               leaf_a_Pien,leaf_b_Pien,leaf_a_Pial,leaf_b_Pial,leaf_a_Thpl,leaf_b_Thpl, leaf_a_Pico,leaf_b_Pico,leaf_a_Abgr,leaf_b_Abgr,leaf_a_Pied,leaf_b_Pied,leaf_a_Pipo.N,
                               leaf_b_Pipo.N,leaf_a_Pipo.S,leaf_b_Pipo.S, list.density[[t+1]], list.leaf[[t]],list.ck[[t+1]], "leaf")#estimate leaf biomass
  list.branch[[t+1]] = biomass(list.pft[[t]],list.fire[[t]],list.dead[[t+1]],list.DBH[[t+1]],branch_a_Potr, branch_b_Potr,branch_a_Pipo_Psme.C,branch_b_Pipo_Psme.C,branch_a_Psme.C,branch_b_Psme.C,branch_a_Psme.I,branch_b_Psme.I,
                               branch_a_Pien,branch_b_Pien,branch_a_Pial,branch_b_Pial,branch_a_Thpl,branch_b_Thpl, branch_a_Pico,branch_b_Pico,branch_a_Abgr,branch_b_Abgr,branch_a_Pied,branch_b_Pied,branch_a_Pipo.N,
                               branch_b_Pipo.N,branch_a_Pipo.S,branch_b_Pipo.S,list.density[[t+1]],list.branch[[t]],list.ck[[t+1]], "branch")#estimate branch biomass
  
  #!K TREATMENT
  module_name <- "treat"
  source(paste0(path_scripts, "Modules_KD.R"))
  
  #Step 5 in August T+1: Where did fires occur over the last few months (Running the fire module) .
  cat(paste0("Initiating fire module...\n"))
  if (t+1 < abs(fire.years)){
    connectivity=connectivity.calc(list.leaf[[t+1]], list.branch[[t+1]], list.stem[[t+1]])
    if(region == "total_domain"){
      holder = matrix_to_raster(connectivity,1,2, Q01_connectivity_td)
      coarse.connectivity = holder[[1]]
      coarse.connectivity.log10 = holder[[2]]
      holder = matrix_to_raster(list.cwd[[t+1]],3,4, Q01_deadbiomass_coarsewood_td)
      cwd.biomass = holder[[1]]
      cwd.biomass.log10 = holder[[2]]
      cwd.biomass.mask = holder[[3]]
      holder = matrix_to_raster(list.litter[[t+1]],5,6, Q01_deadbiomass_litter_td)
      litter.biomass = holder[[1]]
      litter.biomass.log10 = holder[[2]]
      litter.biomass.mask = holder[[3]]
      holder = matrix_to_raster(list.snag[[t+1]],7,8, Q01_deadbiomass_snag_td)
      snag.biomass = holder[[1]]
      snag.biomass.log10 = holder[[2]]
      snag.biomass.mask = holder[[3]]
      holder = matrix_to_raster(list.branch[[t+1]],9,10, Q01_livebiomass_branch_td)
      branch.biomass = holder[[1]]
      branch.biomass.log10 = holder[[2]]
      branch.biomass.mask = holder[[3]]
      holder = matrix_to_raster(list.leaf[[t+1]],11,12, Q01_livebiomass_leaf_td)
      leaf.biomass = holder[[1]]
      leaf.biomass.log10 = holder[[2]]
      leaf.biomass.mask = holder[[3]]
      holder = matrix_to_raster(list.stem[[t+1]],13,14, Q01_livebiomass_stem_td)
      stem.biomass=holder[[1]]
      stem.biomass.log10=holder[[2]]
      stem.biomass.mask = holder[[3]]
      holder=NULL
    }else{
      holder = matrix_to_raster(connectivity,1,2, Q01_connectivity_r)
      coarse.connectivity = holder[[1]]
      coarse.connectivity.log10 = holder[[2]]
      holder = matrix_to_raster(list.cwd[[t+1]],3,4, Q01_deadbiomass_coarsewood_r)
      cwd.biomass = holder[[1]]
      cwd.biomass.log10 = holder[[2]]
      cwd.biomass.mask = holder[[3]]
      holder = matrix_to_raster(list.litter[[t+1]],5,6, Q01_deadbiomass_litter_r)
      litter.biomass = holder[[1]]
      litter.biomass.log10 = holder[[2]]
      litter.biomass.mask = holder[[3]]
      holder = matrix_to_raster(list.snag[[t+1]],7,8, Q01_deadbiomass_snag_r)
      snag.biomass = holder[[1]]
      snag.biomass.log10 = holder[[2]]
      snag.biomass.mask = holder[[3]]
      holder = matrix_to_raster(list.branch[[t+1]],9,10, Q01_livebiomass_branch_r)
      branch.biomass = holder[[1]]
      branch.biomass.log10 = holder[[2]]
      branch.biomass.mask = holder[[3]]
      holder = matrix_to_raster(list.leaf[[t+1]],11,12, Q01_livebiomass_leaf_r)
      leaf.biomass = holder[[1]]
      leaf.biomass.log10 = holder[[2]]
      leaf.biomass.mask = holder[[3]]
      holder = matrix_to_raster(list.stem[[t+1]],13,14, Q01_livebiomass_stem_r)
      stem.biomass=holder[[1]]
      stem.biomass.log10=holder[[2]]
      stem.biomass.mask = holder[[3]]
      holder=NULL
    }
    
    FireProb = f.prob.calc()
    fire.oc = suppressWarnings(f.occ.calc(FireProb))
    fire.number = suppressWarnings(f.num.calc())
    FireSizeCDF=suppressWarnings(f.cdf.calc())
    fireSize = suppressWarnings(f.size.calc())
    list.fire[[t+1]] = suppressWarnings(make_fire())
    
  } else {
    # list.fire[[t+1]] = list.fire.MTBS[[(t+1)-fire.years+1]]  #this adds to one or the first layer of the actual fire.
    # print(paste0("running fire from Observed record for year",(t+1)-fire.years+1)) #! Edited KD
    
    MTBSyr <- (((t+1)-abs(fire.years)+1) %% 37)+1 #! Edited KD
    list.fire[[t+1]] = list.fire.MTBS[[MTBSyr]]  #! Edited KD # this adds to one or the first layer of the actual fire.
      
  }
  
  # Step 6: Reset the live variables to initial conditions when tree dies and something establishes To get the right values
  list.age[[t+1]] = live_reset(list.dead[[t+1]],list.established[[t+1]],list.age[[t+1]],1)
  list.stress[[t+1]] = live_reset(list.dead[[t+1]],list.established[[t+1]],list.stress[[t+1]],list.stress[[1]])
  list.background[[t+1]] = live_reset(list.dead[[t+1]],list.established[[t+1]],list.background[[t+1]],list.background[[1]])
  list.death_prob[[t+1]] = live_reset(list.dead[[t+1]],list.established[[t+1]],list.death_prob[[t+1]],list.death_prob[[1]])
  list.stressed[[t+1]] = ifelse(list.dead[[t+1]]>0 & list.established[[t+1]]==0,list.stressed[[t]],
                                live_reset(list.dead[[t+1]],list.established[[t+1]],list.stressed[[t+1]],list.stressed[[1]]))
  list.H[[t+1]] = live_reset(list.dead[[t+1]],list.established[[t+1]],list.H[[t+1]],0.1)
  list.DBH[[t+1]] = live_reset(list.dead[[t+1]],list.established[[t+1]],list.DBH[[t+1]],0)
  list.density[[t+1]] = live_reset(list.dead[[t+1]],list.established[[t+1]],list.density[[t+1]],0)
  list.stem[[t+1]] = live_reset(list.dead[[t+1]],list.established[[t+1]],list.stem[[t+1]],0)
  list.leaf[[t+1]] = live_reset(list.dead[[t+1]],list.established[[t+1]],list.leaf[[t+1]],0)
  list.branch[[t+1]] = live_reset(list.dead[[t+1]],list.established[[t+1]],list.branch[[t+1]],0)
  write.out(path, region)
  
  #!K UNIVERSAL EDIT P1
    module_name <- "collect_data"
    source(paste0(path_scripts, "Modules_KD.R"))
  
  # Dump memory to save gigs of ram
  list.age=memory.dump(list.age)
  list.stress=memory.dump(list.stress)
  list.background=memory.dump(list.background)
  list.death_prob=memory.dump(list.death_prob)
  list.stressed=memory.dump(list.stressed)
  list.H=memory.dump(list.H)
  list.DBH=memory.dump(list.DBH)
  list.density=memory.dump(list.density)
  list.stem=memory.dump(list.stem)
  list.leaf=memory.dump(list.leaf)
  list.branch=memory.dump(list.branch)
  list.dead=memory.dump(list.dead)
  list.litter=memory.dump(list.litter)
  list.cwd=memory.dump(list.cwd)
  list.snag=memory.dump(list.snag)
  list.min.temp.flag=memory.dump(list.min.temp.flag)
  list.GDD.flag=memory.dump(list.GDD.flag)
  list.chill.day.flag=memory.dump(list.chill.day.flag)
  list.frost.free.flag=memory.dump(list.frost.free.flag)
  list.GDD_budburst.flag=memory.dump(list.GDD_budburst.flag)
  list.p_estab.abiotic=memory.dump(list.p_estab.abiotic)
  list.p_estab.dispersal=memory.dump(list.p_estab.dispersal)
  
  list.established=memory.dump(list.established)
  list.avail.litter=memory.dump(list.avail.litter)
  list.avail.cwd = memory.dump(list.avail.cwd)
  list.ck = memory.dump(list.ck)
  #! list.fire.severity=memory.dump(list.fire.severity)
  list.dead.fire=memory.dump(list.dead.fire)
  
  cat(paste0("Simulation of year ", (t+1), " complete.\n")) #! Edited KD
  
}
proc.time() - ptm

# if(take.snapshot == TRUE){ #! Edited KD moved to "Universal"
#   for (snapshot.year in P_snapshot_yrs){ #! Edited KD; moved to module "collect_data"
#     snapshot(year.write=snapshot.year) #! Edited KD
#     cat(paste0("Snapshot generated for year ", snapshot.year, "\n")) #! Edited KD
#   } #! Edited KD
# } # else {
# #   snapshot.year <- n.years
# #   snapshot(year.write=snapshot.year) #! Edited KD
# # }

print("Finished run! Thank you for using DYNAFFOREST: The Dynamic Temperate and Boreal Fire and Forest-ecosystem Simulator")

# # Initiate log
# if (platform == "cluster"){sink(file = paste0(path_logs, "MODEL_Run", sprintf("%03d", run), "_log.txt"))} #!K


