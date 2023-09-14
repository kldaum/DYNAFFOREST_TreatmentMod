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
#23 2/23/2021 Running with mortality broken out and with estimates of spoil water potential minimum from Annas trait maps
#24 2/24/2021 Diagnosing what is causing mortality early on now. 
#25 2/24/2021 I think one major issue is the blotchiness of initial stand age causeing large areas to die at the same time. Rerun with random noise added to the stand age/height init maps.
#26: 2/25/2021: Fixed problems with how fire is tracked and fed into dead fuel and biomass equations. Testing how that feeds through the model. 
#27: 2/25/2021: Increased the percent of trees that make it to max age to improve the equilibrium over the run from initial conditions. Also adjusted the max age paramter to align with the init data.
#28: 2/26/2021: Increased the frost tolerance of most species where it was low, also adjusted the random number ot not draw zeros. This relly helped control things in Yellowstone. We will see the impact in other regions...

###
library(tidyverse)
library(sf)
####
###
#Define the region and parameterization run
####
#reg="sierra"
#run=1 # Which run is it? You'll see in my notes section above its helpful to track what you do each run when parameterizing. You can delete my notes and start your own.

#path = paste0(path_wd, reg, "/outputs/") # Edited
#pathsave = paste0(path, "ts/run", sprintf("%03d", run), "_", format(Sys.time(),"%Y%m%d%H%M%S")) # Edited
#dir.create(pathsave)
#pathsave <- path


####
#Read in the sqlite databases for mapping and for timeseries analysis
####

read.timeseries.pft=function(region,table){
db.conn <- DBI::dbConnect(RSQLite::SQLite(), dbname=paste0(path_output_db))  #path to your output database
     dataset <- tbl(db.conn, table)
complete=data.frame()
for(i in seq(1,300,by=2)){
     dataset.t=dataset%>%filter(year==i)%>%collect()                       
     dataset.t2=dataset.t %>% group_by(pft) %>% summarise(across(
    .cols = where(is.numeric), 
    .fns = list(mean = mean, sd = sd, median = median, Q25 = ~quantile(., 0.25,na.rm=T), Q75 = ~quantile(., 0.75,na.rm=T)), na.rm = TRUE,    # This is a function (called later in the script) that calculates summary 
    #statistics for all numeric variables by pft and year
    .names = "{col}_{fn}"))
     dataset.t2$year = i
   complete =  rbind(complete,dataset.t2)
     print(paste0("Year ",i," Complete"))
}
DBI::dbDisconnect(db.conn)
return(complete)
}
read.timeseries=function(region,table){     # This function is the same as above but does not group by PFT. It does the whole landscape
db.conn <- DBI::dbConnect(RSQLite::SQLite(), dbname=paste0(path_output_db))
     dataset <- tbl(db.conn, table)
     complete=data.frame()
for(i in seq(1,300,by=2)){
     dataset.t=dataset%>%filter(year==i)%>%collect()                       
     dataset.t2=dataset.t%>%summarise(across(
    .cols = where(is.numeric), 
    .fns = list(mean = mean, sd = sd, median = median, Q25 = ~quantile(., 0.25,na.rm=T), Q75 = ~quantile(., 0.75,na.rm=T)), na.rm = TRUE, 
    .names = "{col}_{fn}"))
     dataset.t2$year = i
   complete =  rbind(complete,dataset.t2)
     print(paste0("Year ",i," Complete"))
    }
DBI::dbDisconnect(db.conn)
return(complete)
}

read.timeseries.fire=function(region,table){  # This one produces time series of fire 
  complete=data.frame()
db.conn <- DBI::dbConnect(RSQLite::SQLite(), dbname=paste0(path_output_db))
      dataset <- tbl(db.conn, table)
     dataset.t=dataset%>%filter(fire.sev>0)%>%collect()
    dataset.t2 = dataset.t %>% group_by(year)%>%summarise(fire.sev_median = median(fire.sev,na.rm=T),
                                                          fire.sev_sd = sd(fire.sev,na.rm=T),
                                                           fire.sev.Q25 = quantile(fire.sev, 0.25, na.rm=T),
                                                           fire.sev.Q75 = quantile(fire.sev, 0.75, na.rm=T))

       DBI::dbDisconnect(db.conn)
      return(dataset.t2)
}


###
#The actual plotting functions
####
landscape.time.pft=function(dataset,variable1,variable2, variable3){      # This function produces the time series plots by PFT
    dataset$variable1 <- dataset[, variable1]
    dataset$variable2 <- dataset[, variable2]
    dataset$variable3 <- dataset[, variable3]

    plot= ggplot(dataset,aes(x=year,y=variable1))+
     geom_line(color="red")+
     geom_ribbon(aes(x=year,ymin=variable1-variable2,ymax=variable1+variable3),fill="grey",alpha=0.5)+
        theme_bw()+
       labs(title = paste0(variable1),y=paste0(variable1),x="Year")+
     facet_wrap(.~forest.type,scales = "free")
     print(plot)
}
landscape.time=function(dataset,variable1,variable2, variable3){          # This does the same but for the whole landscape
 dataset$variable1 <- dataset[, variable1]
    dataset$variable2 <- dataset[, variable2]
    dataset$variable3 <- dataset[, variable3]

    plot= ggplot(dataset,aes(x=year,y=variable1))+
     geom_line(color="red")+
     geom_ribbon(aes(x=year,ymin=variable1-variable2,ymax=variable1+variable3),fill="grey",alpha=0.5)+
        theme_bw()+
       labs(title = paste0(variable1),y=paste0(variable1),x="Year")
     print(plot)
 }

Stand.ts3= data.frame(read.timeseries.fire(reg,"Dead_fuel_fire"))
pdf(paste0(path,reg,"_ts_fire_run_",run_name,".pdf"))  # This produces a pdf and puts the plots into the PDF
landscape.time(Stand.ts3,"fire.sev_median", "fire.sev.Q25", "fire.sev.Q75")
dev.off()

Stand.ts2 = data.frame(read.timeseries(reg,"Dead_fuel_fire"))
pdf(paste0(path, reg,"_ts_run_",run_name,".pdf"))
landscape.time(Stand.ts2,"litter.kg_median","litter.kg_Q25", "litter.kg_Q75")
landscape.time(Stand.ts2,"cwd.kg_median","cwd.kg_Q25","cwd.kg_Q75" )
landscape.time(Stand.ts2,"snag.kg_median","snag.kg_Q25","snag.kg_Q75")
dev.off()

Stand.ts = data.frame(read.timeseries.pft(reg,"Stand"))
Stand.ts=Stand.ts%>%mutate(forest.type = ifelse(pft==0, "grassland/shrubland",
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


pdf(paste0(path, reg,"_ts_pft_run_",run_name,".pdf"))             
landscape.time.pft(Stand.ts,"stand_age_median","stand_age_Q25","stand_age_Q75")
landscape.time.pft(Stand.ts,"height.m_median","height.m_Q25","height.m_Q75" )
landscape.time.pft(Stand.ts,"DBH.cm_median","DBH.cm_Q25","DBH.cm_Q75")
landscape.time.pft(Stand.ts,"stress_median","stress_Q25","stress_Q75")
landscape.time.pft(Stand.ts,"psi_median","psi_Q25","psi_Q75")
landscape.time.pft(Stand.ts,"background.prob_median","background.prob_Q25","background.prob_Q75")
landscape.time.pft(Stand.ts,"stem.biomass.kg_median","stem.biomass.kg_Q25","stem.biomass.kg_Q75")
landscape.time.pft(Stand.ts,"branch.biomass.kg_median","branch.biomass.kg_Q25","branch.biomass.kg_Q75" )
landscape.time.pft(Stand.ts,"leaf.biomass.kg_median","leaf.biomass.kg_Q25","leaf.biomass.kg_Q75" )
landscape.time.pft(Stand.ts,"stand.density.ha_median","stand.density.ha_Q25","stand.density.ha_Q75" )
landscape.time.pft(Stand.ts,"stem.biomass.kg_median","stem.biomass.kg_Q25", "stem.biomass.kg_Q75")
landscape.time.pft(Stand.ts,"branch.biomass.kg_median","branch.biomass.kg_Q25","branch.biomass.kg_Q75")
landscape.time.pft(Stand.ts,"leaf.biomass.kg_median","leaf.biomass.kg_Q25","leaf.biomass.kg_Q75")
dev.off()













