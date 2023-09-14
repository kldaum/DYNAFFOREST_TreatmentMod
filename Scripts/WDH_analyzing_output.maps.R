#run notes
#1. 7/1/2020 1. Initial run done on desktop with incorrect PSI inputs
#2. 7/15/2020 2. Second run done on Columbia cluster with corrected PSI inputs, updated forest height and density and DBH parameters.
#3. 7/28/2020 3. Adjusted the fire severity calculation to compare aridity to the maximum soil moisture in the grid cell during the historical period
#4. 8/14/2020 4. Reduced tree heights and DBHs to try to reduce live biomass and subseuqently fuels to match FIA data 
#5 8/17/2020 5. Reduced SDIs to account for density increases that resulted from decreases tree heights and DBHs.
#6 8/25/2020 Reworked the fire severity equation to be correct
#7 8/31/2020 Adjusted the alg. for how is consumed by fire so it correctly consumes the right amount. This will help reduce fuel loads alot. Also adjusted down THPL dispersal ability a bit.
#8 9/4/2020 Reduced stand densities down to try to match from FIA
#9 9/9/2020 Run with adjusted fuels so it can not be below zero and starting at fuel loads of zero
#10 9/22/2020 Run with fixed NAs and automatic dispersal if tree died in grid cell.
#11 9/23/2020 Run with a reduced fire severity parameter set and a new establishment evaluation script
#12 9/25/2020 Fire severity equation is fixed (hopefully) and have updated the minimimum psi value for seedling establishment to see if that improves mixed con.
#13 9/29/2020 Running the model again to get more abilitic outputs for interior DF coastal df and hemlock
#14 11/16/2020 Running the model with updated climate regeneration parameters which should help expand the range where species can establish.
#15: 11/25/2020 Adjusted the GDD max threshold for California Mixed conifer up to meet the 95th percentile. Also adjusted down dispersal of aspen and hemlock
#16: 11/30/2020 Adjusted the probabilities (everywhere: stress, establishment, fire mortality) so that its not one value draw but one per cell. This should increase 
#                  heterogeneity. Also fixed output writeouts for death and death from fire so they align with the year the fire happened.
#17. 12/9/2020 Trying to reduce fire severity I adjusted one of the coefficients in equation 3 from -1.466 to 0.466 to account for different spatial scale Also reduced fire size in SR.
#18. 1/7/2020 TNew fire module and so much more. fwiw went back to the original fire severity equation but adjusted so the threshold for tree death was 0.95

###
#
###
library(tidyverse)
####
###
#Define the region and parameterization run
####
 reg = "sierra"
 run = 1

####
#Read in the sqlite databases for mapping and for timeseries analysis
####

db.conn <- DBI::dbConnect(RSQLite::SQLite(), dbname=path_output_db)

read.map=function(region,table){
db.conn <- DBI::dbConnect(RSQLite::SQLite(), dbname=path_output_db)

dataset <- tbl(db.conn, table)
dataset.map=dataset%>%filter(year==1|year==50|year==100|year==150|year==200|year==250|year==299)  # This filters the years to read in  from the SQlite you can change depending on what years you want to map#
dataset.map=dataset.map%>%collect()
return(dataset.map)
DBI::dbDisconnect(db.conn)
}


###
#Mapping variables
###

map=function(time,dataset,variable){
  if(variable=="forest.type"){
    cols <- c("NA" = "pink","grassland/shrubland"="red","aspen" = "#8c510a", "California mixed conifer" = "#1f78b4", "Douglas-fir coastal" = "#b2df8a", "Douglas-fir inland"="#756bb1", "Engelmann spruce/fir" = "#33a02c","five-needle pine" = "#fb9a99", "hemlock/cedar" = "#f0027f", "lodgepole pine" = "#ff7f00","mixed fir" = "#cab2d6", "pinyon pine" = "#ffff99", "Ponderosa pine northern" = "#b15928", "Ponderosa pine southern" = "#fdbf6f")
 for(i in time){
   t=dataset%>%filter(year==time)
     plot= ggplot(t)+
     geom_raster(aes_string(x = "x", y = "y", fill = variable))+
        theme(legend.title = element_blank())+
       scale_fill_manual(values = cols)+
      scale_y_continuous(trans = "reverse")+
        theme_bw()+
       labs(title = paste0(variable," Year ",time))
     print(plot)
 }
 }else{
    for(i in time){
   t=dataset%>%filter(year==time)  #This can be adjusted 
      plot= ggplot(t)+
     geom_raster(aes_string(x = "x", y = "y", fill = variable))+
        theme(legend.title = element_blank())+
       scale_fill_viridis_c()+
      scale_y_continuous(trans = "reverse")+
        theme_bw()+
       labs(title = paste0(variable," Year ",time))
     print(plot)
 }
 }
}


Stand = data.frame(read.map(reg,"Stand"))
fire = data.frame(read.map(reg,"Dead_fuel_fire"))
Stand=Stand%>%mutate(forest.type = ifelse(pft==0, "grassland/shrubland",
                                          ifelse(pft==1,"aspen",
                                           ifelse(pft==2,"California mixed conifer",
                                           ifelse(pft==3,"Douglas-fir coastal",
                                           ifelse(pft==4, "Douglas-fir inland",
                                           ifelse(pft==5, "Engelmann spruce/fir",
                                           ifelse(pft==6, "five-needle pine",
                                           ifelse(pft==7, "hemlock/cedar",
                                           ifelse(pft==8, "lodgepole pine",
                                           ifelse(pft==9, "mixed fir",
                                           ifelse(pft==10,"pinyon pine", 
                                           ifelse(pft==11,"Ponderosa pine northern","Ponderosa pine southern")))))))))))))

pdf(paste0(path,reg,"_maps_",run_name,".pdf"))  # Edited

for (i in c(1,50,100,150,200,250,299)){
map(i,Stand,"forest.type")
}
for (i in c(1,50,100,150,200,250,299)){
map(i,Stand,"height.m")
}
for (i in c(1,50,100,150,200,250,299)){
map(i,Stand,"stand_age")
}

for (i in c(1,50,100,150,200,250,299)){
map(i,Stand,"DBH.cm")
}
for (i in c(1,50,100,150,200,250,299)){
map(i,Stand,"stem.biomass.kg")
}
for (i in c(1,50,100,150,200,250,299)){
map(i,Stand,"branch.biomass.kg")
}
for (i in c(1,50,100,150,200,250,299)){
map(i,Stand,"leaf.biomass.kg")
}
for (i in c(1,50,100,150,200,250,299)){
map(i,Stand,"stand.density.ha")
}

for (i in c(1,50,100,150,200,250,299)){
map(i,fire,"litter.kg")
}
for (i in c(1,50,100,150,200,250,299)){
map(i,fire,"cwd.kg")
}
for (i in c(1,50,100,150,200,250,299)){
map(i,fire,"snag.kg")
}
for (i in c(1,50,100,150,200,250,299)){
map(i,fire,"fire.sev")
}
for (i in c(1,50,100,150,200,250,299)){
map(i,fire,"fire.death")
}
dev.off()

