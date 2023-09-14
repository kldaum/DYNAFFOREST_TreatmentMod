###########
########
###
########
###########
#DYNAFFOREST V.1.01
# Copyright (C) 2020-2030 Winslow D. Hansen
#This This file is part of DYNAFFOREST V.1.01 and comes with with ABSOLUTELY NO WARRANTY; 
#This is free software (licensed under GPL 3.0). 
#You are welcome to redistribute it under certain conditions. See http://www.gnu.org/copyleft/gpl.html for more details.
###########
########
###
########
###########

# the next three functions are to calculate our metric of forest_buffer_buffer connectivity which is used by the fire module later
calc.connect = function(x,x.2,dir.label){   
 dir=matrix( nrow = 6,ncol = 1)
    dir[1] = dir.calc(x,x.2[i,1],x.2[i,2],1,dir.label)
    if(dir[1] == 0|is.na(dir[1])){
      return(0)
    }else{
    dir[2] = dir.calc(x,x.2[i,1],x.2[i,2],2,dir.label)
    }
   if(dir[1] ==1 & dir[2] ==0|is.na(dir[2])){
    return(1)
   }else{
    dir[3] = dir.calc(x,x.2[i,1],x.2[i,2],3,dir.label) 
    }
    if(dir[2] ==1 & dir[3] ==0|is.na(dir[3])){
    return(2)
   }else{
    dir[4] = dir.calc(x,x.2[i,1],x.2[i,2],4,dir.label) 
    }
    if(dir[3] ==1 & dir[4] ==0|is.na(dir[4])){
    return(3)
    }else{
    dir[5] = dir.calc(x,x.2[i,1],x.2[i,2],5,dir.label)  
    }   
    if(dir[4] ==1 & dir[5] ==0|is.na(dir[5])){
    return(4)
    }else{
    dir[6] = dir.calc(x,x.2[i,1],x.2[i,2],6,dir.label)  
    }
    if(dir[5] ==1 & dir[6] ==0|is.na(dir[6])){
    return(5)
    }else{
      return(6)
    }
  }


dir.calc = function(x, row,col,dir.distance,dir.label){   
if(dir.label == "N"){
  return(as.vector(x[row - dir.distance, col])) 
}else if(dir.label == "S"){
    return(as.vector(x[row + dir.distance, col])) 
}else if(dir.label == "E"){
   return(as.vector(x[row, col + dir.distance]))
}else if(dir.label == "W"){
   return(as.vector(x[row, col - dir.distance]))
}else if(dir.label == "NE"){
   return(as.vector(x[row - dir.distance, col + dir.distance]))
}else if(dir.label == "NW"){
   return(as.vector(x[row - dir.distance, col - dir.distance]))
}else if(dir.label == "SE"){
   return(as.vector(x[row + dir.distance, col + dir.distance]))
}else{
   return(as.vector(x[row + dir.distance, col - dir.distance]))
}
}

connectivity.calc = function(leaf, branch, stem){
cover=leaf+branch+stem
cover[cover<40000] = 0
cover[cover>=40000] = 1

cover.2=which(cover > 0, arr.ind=TRUE)
cover.2=cbind(cover.2, matrix(NA,nrow=nrow(cover.2)),matrix(NA,nrow=nrow(cover.2)),matrix(NA,nrow=nrow(cover.2)),matrix(NA,nrow=nrow(cover.2)),matrix(NA,nrow=nrow(cover.2)),matrix(NA,nrow=nrow(cover.2)),
          matrix(NA,nrow=nrow(cover.2)),matrix(NA,nrow=nrow(cover.2)),matrix(NA,nrow=nrow(cover.2)),matrix(NA,nrow=nrow(cover.2)))

for(i in 1:nrow(cover.2)){
cover.2[i,3] = calc.connect(cover,cover.2, "N")
cover.2[i,4] = calc.connect(cover,cover.2, "S")
cover.2[i,5] = calc.connect(cover,cover.2, "E")
cover.2[i,6] = calc.connect(cover,cover.2, "W")
cover.2[i,7] = calc.connect(cover,cover.2, "NW")
cover.2[i,8] = calc.connect(cover,cover.2, "NE")
cover.2[i,9] = calc.connect(cover,cover.2, "SW")
cover.2[i,10] = calc.connect(cover,cover.2, "SE")

}
  
cover.2[,7]=ifelse(cover.2[,7]>4,4,cover.2[,7])
cover.2[,8]=ifelse(cover.2[,8]>4,4,cover.2[,8])
cover.2[,9]=ifelse(cover.2[,9]>4,4,cover.2[,9])
cover.2[,10]=ifelse(cover.2[,10]>4,4,cover.2[,10])
cover.2[,11] = rowSums(cover.2[,3:10])
cover.2[,12] = (cover.2[,11]+1)/(6*4+4*4+1)    
  


veg.connectivity=cover
veg.connectivity[as.matrix(cover.2[,c(1,2)])] <- as.double(cover.2[,12])

return(veg.connectivity)
}

#log10 is a function to calculate the log10 of input variables used in the fire module.
calc.log10 = function(var,q01.row){
 var.log10 = var
  var.log10[var.log10 < q01.row] = q01.row
  var.log10 = log10(var.log10)
  return(var.log10)
}


# Standardize is a function to standardize the input variables for the statistical fire model.
# it uses means and SD for each region read in from a table 

standardize=function(var,st.row){
  return((var-st[st.row,1])/st[st.row,2])
}

# Do a function to implement fires that are existing and give them an idea
implement.MTBS = function(data){
  data=raster::mask(data, forest_buffer)
  check=raster::clump(data, directions=8, gaps=FALSE)
  values(check)[is.na(values(check))] = 0
  twixt=as.matrix(check)                                 # convert back to matrix for ingestion in main model. Voile! We have MTBS fire.
  twixt = rbind(NA, cbind(NA, twixt, NA), NA)
return(twixt)
}


# convert the fuels data from the main model to a raster for comparison with other forcing datasets. The fuels variables are also standardized
matrix_to_raster = function(variable, row.1,row.2, qrow){
matrix.yr=variable
x.dim=ncol(forest_buffer)+2   # this and the steps below are done because we add a 1 row/ column buffer around each landscape in the main model
y.dim=nrow(forest_buffer)+2

matrix.yr=matrix.yr[-y.dim,-x.dim]
matrix.yr=matrix.yr[-1,-1]
matrix.yr.raster=raster(matrix.yr)  # Convert to a raster using the matrix to raster function in the landscapemetrics package
matrix.yr.raster=setExtent(matrix.yr.raster, forest_buffer)
crs(matrix.yr.raster)=crs(forest_buffer)
matrix.yr.raster2 = overlay(x=matrix.yr.raster, y=elev.fine, fun=function(x, y) ifelse(is.na(y), NA,x))
matrix.yr.raster2[is.na(matrix.yr.raster2)]=0
matrix.yr.agg=raster::aggregate(matrix.yr.raster2,fact= 12)  # aggregate to 12 km to match the other variables
matrix.yr.agg.log10 = calc.log10(matrix.yr.agg, qrow)
matrix.yr.agg.st = standardize(matrix.yr.agg, row.1 ) #standardize
matrix.yr.agg.log10.st = standardize(matrix.yr.agg.log10, row.2 ) #standardize
return(list(matrix.yr.agg.st,matrix.yr.agg.log10.st,matrix.yr.agg))
}




#First step is to identify whether or not a fire occurs in a grid cell 

#Calculate the models predictors by applying equations from FireProbModel_predictors.txt provided by Park. Note I'm not reading in the text file but entering the equations from the text file here. then apply model in 
#region_FireProbModel.txt

f.prob.calc = function(){
  if(region=="yellowstone"){
    predictor01 = 0.01109 + 0.0048579*cwd.biomass
    predictor02 = 0.0011017 + 0.0014626*snag.biomass - 0.00098898*snag.biomass^2 - 0.00098419*snag.biomass^3
    predictor03 = -0.0014176 + 0.0019748*strikes.mean.log10 + 0.0013172*strikes.mean.log10^2 - 0.00074075*strikes.mean.log10^3
    predictor04 = -0.0011687 - 0.00052483*elev.std + 0.0014831*elev.std^2
    predictor05 = -8.4656e-05 - 0.0016966*p_pet.mean
    predictor06 = -0.0019399 - 0.00041602*coarse.connectivity + 0.0021398*coarse.connectivity^2
FireProb = 1/(1+e^(-(-5.9633 + 120.0691*predictor01 + 56.1939*predictor02 + 49.0477*predictor03 + 103.4472*predictor04 + 67.901*predictor05 + 35.0137*predictor06)))
FireProb[FireProb<0|stem.biomass.mask+branch.biomass.mask+leaf.biomass.mask+snag.biomass.mask+cwd.biomass.mask+litter.biomass.mask==0] = 0  # only calculate a fire probability for gridcells with forest biomass
FireProb.cor = FireProb*0.864 # This corrects for calculating probabilities based on the 1985-1994 period. 
return(FireProb.cor) 
 }else if(region=="Southern_Rockies"){
   predictor01 = 0.0058524 + 0.0078874*branch.biomass.log10 + 0.0036313*branch.biomass.log10^2 + 0.00054988*branch.biomass.log10^3
   predictor02 = -0.0034432 - 0.0025175*elev.std.log10 + 0.0035642*elev.std.log10^2 + 0.0021751*elev.std.log10^3
   predictor03 = -0.00073417 + 0.0078134*strikes.mean + 0.0025868*strikes.mean^2 - 0.0019818*strikes.mean^3
   predictor04 = 0.0021657 - 0.0008096*litter.biomass - 0.0030299*litter.biomass^2
   predictor05 = 0.0040521 + 0.0028109*p_pet.mean - 0.0031894*p_pet.mean^2 + 0.00044641*p_pet.mean^3
   predictor06 = 0.003082 - 0.0019757*slope - 0.0035448*slope^2 + 0.0015163*slope^3
   predictor07 = -0.00097918 - 0.0024626*pop.log10
   predictor08 = -0.0014163 + 0.0012737*coarse.connectivity.log10 + 0.0025213*coarse.connectivity.log10^2 + 0.00081308*coarse.connectivity.log10^3
   predictor09 = -0.0011363 + 0.00025466*leaf.biomass + 0.0014462*leaf.biomass^2
FireProb = 1/(1+e^(-(-6.6564 + 123.12*predictor01 + 53.0272*predictor02 + 40.5412*predictor03 + 114.8377*predictor04 + 203.9811*predictor05 + 36.0528*predictor06 + 140.2893*predictor07 + 210.6381*predictor08 - 65.1419*predictor09)))
FireProb[FireProb<0|stem.biomass.mask+branch.biomass.mask+leaf.biomass.mask+snag.biomass.mask+cwd.biomass.mask+litter.biomass.mask==0] = 0  # only calculate a fire probability for gridcells with forest biomass
FireProb.cor = FireProb*0.52094 # This corrects for calculating probabilities based on the 1985-1994 period. 
return(FireProb.cor) 

 } else if(region=="idaho"){
   predictor01 = 0.021126 + 0.018554*slope.log10 + 0.0065213*slope.log10^2 + 0.00078912*slope.log10^3
   predictor02 = -0.0055335 + 0.0039233*coarse.connectivity + 0.0056408*coarse.connectivity^2 + 0.002466*coarse.connectivity^3
   predictor03 = -0.00010589 - 0.0054561*branch.biomass - 0.0036524*branch.biomass^2
   predictor04 = -0.0037956 - 0.0063373*pop.log10 + 0.0034054*pop.log10^2
   predictor05 = -0.0039199 - 0.0054463*leaf.biomass - 0.00028028*leaf.biomass^2 + 0.001066*leaf.biomass^3
   predictor06 = 0.0010097 + 0.0037647*elev.std.log10 + 0.00061199*elev.std.log10^2
   predictor07 = -0.0024899 + 0.0044425*cwd.biomass + 0.0018369*cwd.biomass^2
   predictor08 = 0.0026331 + 0.0044584*strikes.mean.log10 - 0.0023675*strikes.mean.log10^2 - 0.0010899*strikes.mean.log10^3
   predictor09 = 0.0010559 + 0.00094961*p_pet.mean.log10 - 0.0012959*p_pet.mean.log10^2
FireProb = 1/(1+e^(-(-4.2876 + 18.8699*predictor01 + 35.4114*predictor02 + 58.1318*predictor03 + 38.8504*predictor04 + 10.6566*predictor05 + 63.0279*predictor06 + 39.5572*predictor07 + 110.9534*predictor08 + 69.475*predictor09)))
FireProb[FireProb<0|stem.biomass.mask+branch.biomass.mask+leaf.biomass.mask+snag.biomass.mask+cwd.biomass.mask+litter.biomass.mask==0] = 0  # only calculate a fire probability for gridcells with forest biomass
FireProb.cor = FireProb*1.0193 # This corrects for calculating probabilities based on the 1985-1994 period. 
return(FireProb.cor)   
 }else if(region == "cascades"){
   predictor01 = 0.012317 - 0.0052655*pop.log10 - 0.00091761*pop.log10^2
   predictor02 = 0.0025645 - 0.0037197*p_pet.mean.log10 - 0.0021187*p_pet.mean.log10^2
   predictor03 = -0.0029257 + 0.0077374*elev.std.log10 + 0.0033663*elev.std.log10^2
   predictor04 = 0.00012252 + 0.0016135*branch.biomass
   predictor05 = 0.00060845 + 0.0014245*stem.biomass - 0.00034327*stem.biomass^2 - 0.00072842*stem.biomass^3
FireProb  = 1/(1+e^(-(-5.8141 + 67.846*predictor01 + 185.5105*predictor02 + 75.3308*predictor03 + 103.5599*predictor04 + 86.69*predictor05)))
FireProb[FireProb<0|stem.biomass.mask+branch.biomass.mask+leaf.biomass.mask+snag.biomass.mask+cwd.biomass.mask+litter.biomass.mask==0] = 0  # only calculate a fire probability for gridcells with forest biomass
FireProb.cor = FireProb*0.99184 # This corrects for calculating probabilities based on the 1985-1994 period. 
return(FireProb.cor) 
 }else if(region == "sierra"){
   predictor01 = 0.020725 + 0.011071*stem.biomass.log10 + 0.001479*stem.biomass.log10^2
   predictor02 = -0.0032121 + 0.0065196*slope.log10 + 0.0041043*slope.log10^2 + 0.00086023*slope.log10^3
   predictor03 = 0.00056122 + 0.0019412*branch.biomass - 0.00011428*branch.biomass^2 - 0.0012206*branch.biomass^3
   predictor04 = 0.0050647 - 0.0041657*p_pet.mean.log10 - 0.0043122*p_pet.mean.log10^2
   predictor05 = 0.00080003 - 0.0041028*snag.biomass - 0.0030337*snag.biomass^2 + 0.0030614*snag.biomass^3
   predictor06 = 7.9497e-06 - 0.00025105*strikes.mean + 0.00071529*strikes.mean^2 - 0.00089191*strikes.mean^3
FireProb = 1/(1+e^(-(-5.1767 + 52.2528*predictor01 + 50.687*predictor02 + 24.9699*predictor03 + 34.3918*predictor04 + 20.6358*predictor05 + 51.5094*predictor06)))
FireProb[FireProb<0|stem.biomass.mask+branch.biomass.mask+leaf.biomass.mask+snag.biomass.mask+cwd.biomass.mask+litter.biomass.mask==0] = 0  # only calculate a fire probability for gridcells with forest biomass
FireProb.cor = FireProb*0.89819 # This corrects for calculating probabilities based on the 1985-1994 period. 
return(FireProb.cor) 
 }else if(region=="total_domain"){
   predictor01 = 0.013838 + 0.0063781*stem.biomass.log10 - 0.00076226*stem.biomass.log10^2 - 0.00074993*stem.biomass.log10^3
   predictor02 = -0.00079814 - 0.00047303*strikes.mean - 0.0015752*strikes.mean^2 + 0.0013867*strikes.mean^3
   predictor03 = -0.00027213 + 0.0038643*branch.biomass + 0.00055766*branch.biomass^2 - 0.0011256*branch.biomass^3
   predictor04 = -0.00086859 + 0.0026899*slope.log10 + 0.0012483*slope.log10^2 + 0.00035076*slope.log10^3
   predictor05 = 0.0023048 - 0.0027595*p_pet.mean.log10 - 0.0027717*p_pet.mean.log10^2 + 0.00070767*p_pet.mean.log10^3
   predictor06 = 0.00031144 - 0.0014958*pop.log10
   predictor07 = -0.00063225 + 0.00041807*coarse.connectivity + 0.00074243*coarse.connectivity^2
   predictor08 = -0.0024673 - 0.0013814*snag.biomass + 0.0045844*snag.biomass^2 - 0.0011576*snag.biomass^3
   predictor09 = 0.00052638 + 0.0004353*cwd.biomass - 0.0008285*cwd.biomass^2
FireProb = 1/(1+e^(-(-5.5793 + 74.0823*predictor01 + 29.5475*predictor02 + 53.356*predictor03 + 118.7191*predictor04 + 121.9431*predictor05 + 95.6366*predictor06 + 105.6118*predictor07 + 76.007*predictor08 + 146.596*predictor09)))
FireProb[FireProb<0|stem.biomass.mask+branch.biomass.mask+leaf.biomass.mask+snag.biomass.mask+cwd.biomass.mask+litter.biomass.mask==0] = 0  # only calculate a fire probability for gridcells with forest biomass
FireProb.cor = FireProb*0.82831 # This corrects for calculating probabilities based on the 1985-1994 period. 
   return(FireProb.cor) 
 }
 }



# This calculates where a fire occurs given the probability of a fire. It compares the fire probability to a random uniform number between zero and 1
f.occ.calc = function(layer){
  template = layer
  values(template)[!is.na(values(template))] =  runif(1000000,0.0,1)
  fire.oc= overlay(x=layer,y=template, fun = function(x,y) ifelse(x>=y,1,0))
  return(fire.oc)
}


#Given a fire occurs, now we model the number of fires  by applying the equations in the text file FireFrequencyProbModel.txt provided by Park


f.num.calc = function(){
if(region == "yellowstone"){
ProbNfires.1 = 1/(1+e^(-(102.5649 + 0.010911*FireProb)))
ProbNfires.2 = 1/(1+e^(-(-3.2241 + 17.0026*FireProb)))
ProbNfires.3 = 1/(1+e^(-(-4.7009 - 53.3087*FireProb)))
ProbNfires.1.masked= overlay(x=ProbNfires.1,y=fire.oc, fun = function(x,y) ifelse(y==1,x,NA))
ProbNfires.2.masked= overlay(x=ProbNfires.2,y=fire.oc, fun = function(x,y) ifelse(y==1,x,NA))
ProbNfires.3.masked= overlay(x=ProbNfires.3,y=fire.oc, fun = function(x,y) ifelse(y==1,x,NA))
template = FireProb
values(template)[!is.na(values(template))] =  runif(1000000,0.0,1)
fire.number= overlay(x=ProbNfires.2.masked,y=ProbNfires.3.masked,z=template, fun = function(x,y,z) ifelse(y>=z,3,
                                                                                                       ifelse(x>=z,2,1)))
fire.number=fire.number*fire.freq.mult
print(paste0("Fire frequency multiplier is ",fire.freq.mult))
return(fire.number)
 }else if(region == "Southern_Rockies"){
ProbNfires.1 = 1/(1+e^(-(102.5675 - 0.0027936*FireProb)))
ProbNfires.2 = 1/(1+e^(-(-3.4563 + 16.444*FireProb)))
ProbNfires.3 = 1/(1+e^(-(-9.2617 + 66.7244*FireProb)))
ProbNfires.4 = 1/(1+e^(-(-9.2617 + 66.7244*FireProb)))
ProbNfires.1.masked= overlay(x=ProbNfires.1,y=fire.oc, fun = function(x,y) ifelse(y==1,x,NA))
ProbNfires.2.masked= overlay(x=ProbNfires.2,y=fire.oc, fun = function(x,y) ifelse(y==1,x,NA))
ProbNfires.3.masked= overlay(x=ProbNfires.3,y=fire.oc, fun = function(x,y) ifelse(y==1,x,NA))
ProbNfires.4.masked= overlay(x=ProbNfires.4,y=fire.oc, fun = function(x,y) ifelse(y==1,x,NA))

template = FireProb
values(template)[!is.na(values(template))] =  runif(1000000,0.0,1)
fire.number= overlay(x=ProbNfires.2.masked,y=ProbNfires.3.masked,c=ProbNfires.4.masked, z=template, fun = function(x,y,c,z) ifelse(c>=z,4,
                                                                                                                                    ifelse(y>=z,3,
                                                                                                                                    ifelse(x>=z,2,1))))
fire.number=fire.number*fire.freq.mult
print(paste0("Fire frequency multiplier is ",fire.freq.mult))
return(fire.number)
}else if(region == "idaho"){
ProbNfires.1 = 1/(1+e^(-(102.5659 - 0.001236*FireProb)))
ProbNfires.2 = 1/(1+e^(-(-1.9474 + 0.71423*FireProb)))
ProbNfires.3 = 1/(1+e^(-(-4.2211 + 11.3852*FireProb)))
ProbNfires.4 = 1/(1+e^(-(-4.3044 - 24.6391*FireProb)))
ProbNfires.5 = 1/(1+e^(-(-2.979 - 193.5183*FireProb)))
ProbNfires.1.masked= overlay(x=ProbNfires.1,y=fire.oc, fun = function(x,y) ifelse(y==1,x,NA))
ProbNfires.2.masked= overlay(x=ProbNfires.2,y=fire.oc, fun = function(x,y) ifelse(y==1,x,NA))
ProbNfires.3.masked= overlay(x=ProbNfires.3,y=fire.oc, fun = function(x,y) ifelse(y==1,x,NA))
ProbNfires.4.masked= overlay(x=ProbNfires.4,y=fire.oc, fun = function(x,y) ifelse(y==1,x,NA))
ProbNfires.5.masked= overlay(x=ProbNfires.5,y=fire.oc, fun = function(x,y) ifelse(y==1,x,NA))
template = FireProb
values(template)[!is.na(values(template))] =  runif(1000000,0.0,1)
fire.number= overlay(x=ProbNfires.2.masked, y=ProbNfires.3.masked, c=ProbNfires.4.masked, d = ProbNfires.5.masked, z=template, fun = function(x,y,c,d,z) ifelse(d >=z,5,
                                                                                                                                                                  ifelse(c >=z,4,
                                                                                                                                                                   ifelse(y >=z,3,
                                                                                                                                                                   ifelse(x >=z,2,1)))))
fire.number=fire.number*fire.freq.mult
print(paste0("Fire frequency multiplier is ",fire.freq.mult))
return(fire.number)
}else if(region == "cascades"){
ProbNfires.1 = 1/(1+e^(-(102.5664 + 0.0038945*FireProb)))
ProbNfires.2 = 1/(1+e^(-(-2.9779 + 28.9926*FireProb)))
ProbNfires.3 = 1/(1+e^(-(-4.925 + 18.1416*FireProb)))
ProbNfires.4 = 1/(1+e^(-(-6.5516 + 39.3368*FireProb)))
ProbNfires.1.masked= overlay(x=ProbNfires.1,y=fire.oc, fun = function(x,y) ifelse(y==1,x,NA))
ProbNfires.2.masked= overlay(x=ProbNfires.2,y=fire.oc, fun = function(x,y) ifelse(y==1,x,NA))
ProbNfires.3.masked= overlay(x=ProbNfires.3,y=fire.oc, fun = function(x,y) ifelse(y==1,x,NA))
ProbNfires.4.masked= overlay(x=ProbNfires.4,y=fire.oc, fun = function(x,y) ifelse(y==1,x,NA))

template = FireProb
values(template)[!is.na(values(template))] =  runif(1000000,0.0,1)
fire.number= overlay(x=ProbNfires.2.masked,y=ProbNfires.3.masked,c=ProbNfires.4.masked, z=template, fun = function(x,y,c,z) ifelse(c>=z,4,
                                                                                                                                    ifelse(y>=z,3,
                                                                                                                                    ifelse(x>=z,2,1))))
fire.number=fire.number*fire.freq.mult
print(paste0("Fire frequency multiplier is ",fire.freq.mult))
return(fire.number)
}else if(region == "sierra"){
ProbNfires.1 = 1/(1+e^(-(102.5674 + 0.0015407*FireProb)))
ProbNfires.2 = 1/(1+e^(-(-2.3796 - 1.1323*FireProb)))
ProbNfires.3 = 1/(1+e^(-(-3.9866 - 17.6288*FireProb)))
ProbNfires.4 = 1/(1+e^(-(-4.6056 - 32.675*FireProb)))
ProbNfires.5 = 1/(1+e^(-(-4.9493 - 35.4317*FireProb)))
ProbNfires.6 = 1/(1+e^(-(-5.4227 - 45.463*FireProb)))
ProbNfires.7 = 1/(1+e^(-(-5.4227 - 45.463*FireProb)))
ProbNfires.1.masked= overlay(x=ProbNfires.1,y=fire.oc, fun = function(x,y) ifelse(y==1,x,NA))
ProbNfires.2.masked= overlay(x=ProbNfires.2,y=fire.oc, fun = function(x,y) ifelse(y==1,x,NA))
ProbNfires.3.masked= overlay(x=ProbNfires.3,y=fire.oc, fun = function(x,y) ifelse(y==1,x,NA))
ProbNfires.4.masked= overlay(x=ProbNfires.4,y=fire.oc, fun = function(x,y) ifelse(y==1,x,NA))
ProbNfires.5.masked= overlay(x=ProbNfires.5,y=fire.oc, fun = function(x,y) ifelse(y==1,x,NA))
ProbNfires.6.masked= overlay(x=ProbNfires.6,y=fire.oc, fun = function(x,y) ifelse(y==1,x,NA))
ProbNfires.7.masked= overlay(x=ProbNfires.7,y=fire.oc, fun = function(x,y) ifelse(y==1,x,NA))

template = FireProb
values(template)[!is.na(values(template))] =  runif(1000000,0.0,1)
fire.number= overlay(x=ProbNfires.2.masked, y=ProbNfires.3.masked, c=ProbNfires.4.masked, d = ProbNfires.5.masked, e = ProbNfires.6.masked, f = ProbNfires.7.masked, z=template, fun = function(x,y,c,d,e,f,z) ifelse(f >=z,7,
                                                                                                                                                                                                               ifelse(e >=z,6,
                                                                                                                                                                                                               ifelse(d >=z,5,
                                                                                                                                                                                                               ifelse(c >=z,4,
                                                                                                                                                                                                               ifelse(y >=z,3,
                                                                                                                                                                                                               ifelse(x >=z,2,1)))))))
fire.number=fire.number*fire.freq.mult
print(paste0("Fire frequency multiplier is ",fire.freq.mult))
return(fire.number)
}else if(region == "total_domain"){
  ProbNfires.1 = 1/(1+e^(-(102.5598 + 0.0011262*FireProb)))
  ProbNfires.2 = 1/(1+e^(-(-2.7046 + 10.8842*FireProb)))
  ProbNfires.3 = 1/(1+e^(-(-4.6655 + 12.9793*FireProb)))
  ProbNfires.4 = 1/(1+e^(-(-6.0918 + 15.2014*FireProb)))
  ProbNfires.5 = 1/(1+e^(-(-7.3244 - 9.7949*FireProb)))
  ProbNfires.6 = 1/(1+e^(-(-6.2527 - 172.5121*FireProb)))
  ProbNfires.7 = 1/(1+e^(-(-6.2527 - 172.5121*FireProb)))
  ProbNfires.1.masked= overlay(x=ProbNfires.1,y=fire.oc, fun = function(x,y) ifelse(y==1,x,NA))
  ProbNfires.2.masked= overlay(x=ProbNfires.2,y=fire.oc, fun = function(x,y) ifelse(y==1,x,NA))
  ProbNfires.3.masked= overlay(x=ProbNfires.3,y=fire.oc, fun = function(x,y) ifelse(y==1,x,NA))
  ProbNfires.4.masked= overlay(x=ProbNfires.4,y=fire.oc, fun = function(x,y) ifelse(y==1,x,NA))
  ProbNfires.5.masked= overlay(x=ProbNfires.5,y=fire.oc, fun = function(x,y) ifelse(y==1,x,NA))
  ProbNfires.6.masked= overlay(x=ProbNfires.6,y=fire.oc, fun = function(x,y) ifelse(y==1,x,NA))
  ProbNfires.7.masked= overlay(x=ProbNfires.7,y=fire.oc, fun = function(x,y) ifelse(y==1,x,NA))
  template = FireProb
  values(template)[!is.na(values(template))] =  runif(1000000,0.0,1)
  fire.number= overlay(x=ProbNfires.2.masked, y=ProbNfires.3.masked, c=ProbNfires.4.masked, d = ProbNfires.5.masked, e = ProbNfires.6.masked, 
                       f = ProbNfires.7.masked, z=template, fun = function(x,y,c,d,e,f,z) ifelse(f >=z,7,
                                                                                                 ifelse(e >=z,6,
                                                                                                        ifelse(d >=z,5,
                                                                                                               ifelse(c >=z,4,
                                                                                                                      ifelse(y >=z,3,
                                                                                                                             ifelse(x >=z,2,1)))))))
  fire.number=fire.number*fire.freq.mult
  print(paste0("Fire frequency multiplier is ",fire.freq.mult))
  return(fire.number)
}
}



#Finally we estimate fire sizes for each fire by applying the equations in the text file FireSizeCDFModel.txt provided by Park

# Convert fire number from a raster to a dataframe 
f.conver = function(var.name){
final=data.frame()
s=as.data.frame(fire.number,xy=T)
names(s)[3] = var.name
final = s %>% filter(!is.na(s[3]))
  return(final)
}


#First estimate the fire size CDF values for each fire
f.cdf.calc = function(){
  if(region == "yellowstone"){
    FireSizeCDF = round(0.49592 + 0.072181*coarse.connectivity - 0.0040372*slope - 0.0099917*slope^2 + 0.015828*slope^3 - 
                          0.090246*stem.biomass - 0.012319*stem.biomass^2 + 0.013497*stem.biomass^3 + 0.055854*litter.biomass, digits=2)*100  # Estimate the cdf values
    
  }else if(region == "Southern_Rockies"){
    FireSizeCDF = round(0.40686 + 0.028711*stem.biomass.log10 + 0.023766*stem.biomass.log10^2 - 0.018203*stem.biomass.log10^3 + 0.070421*p_pet.mean - 0.061598*p_pet.mean^2 + 
                          0.013374*p_pet.mean^3 - 0.0014182*slope + 0.040738*slope^2 - 0.0083991*slope^3 + 0.085047*coarse.connectivity.log10, digits = 2)*100
    
  } else if (region =="idaho"){
  FireSizeCDF = round(0.51464 - 0.012978*p_pet.mean - 0.042286*p_pet.mean^2 + 0.011267*p_pet.mean^3 + 0.056402*coarse.connectivity.log10 + 0.011115*coarse.connectivity.log10^2 + 
                        0.0021807*slope - 0.014742*slope^2 + 0.011634*slope^3 - 0.018787*elev.std, digits = 2)*100
  
  }else if(region =="cascades"){
    FireSizeCDF = round(0.26246 + 0.05666*slope - 0.34938*pop + 1.2855*pop^2 - 0.54687*pop^3 + 0.12236*coarse.connectivity - 0.085865*coarse.connectivity^2 - 
                          0.047397*p_pet.mean - 0.072295*leaf.biomass + 0.12698*leaf.biomass^2 - 0.02341*leaf.biomass^3 + 0.23963*snag.biomass.log10 +
                          0.029315*snag.biomass.log10^2 - 0.0036136*snag.biomass.log10^3 - 0.15348*cwd.biomass, digits = 2)*100
    
  }else if(region == "sierra"){
    FireSizeCDF = round(0.45492 - 0.010929*coarse.connectivity.log10 - 0.049567*coarse.connectivity.log10^2 - 0.019174*coarse.connectivity.log10^3 + 0.041434*p_pet.mean - 0.022673*p_pet.mean^2 - 0.015765*snag.biomass + 
                          0.068007*snag.biomass^2 - 0.028581*snag.biomass^3 - 0.11059*leaf.biomass + 0.079676*branch.biomass + 0.012387*slope + 0.046218*slope^2 - 
                          0.010835*slope^3 + 0.0013571*elev.std - 0.043136*elev.std^2 + 0.0082833*elev.std^3 + 0.085641*stem.biomass.log10, digits = 2) *100
    
  }else if(region == "total_domain"){  
    FireSizeCDF = round(0.50253 + 0.035175*stem.biomass.log10 - 0.0063833*stem.biomass.log10^2 - 0.0070098*stem.biomass.log10^3 + 0.048122*coarse.connectivity.log10 + 0.015665*p_pet.mean - 
                          0.022717*p_pet.mean^2 + 0.0026279*p_pet.mean^3 - 0.069717*cwd.biomass.log10 + 0.021261*slope - 0.013646*elev.std - 0.011948*pop + 0.042071*branch.biomass - 
                          0.078166*litter.biomass.log10 - 0.033722*litter.biomass.log10^2 + 0.057624*snag.biomass.log10 + 0.015506*snag.biomass.log10^2 + 0.017874*leaf.biomass, digits = 2) *100
  }
 FireSizeCDF.masked= overlay(x=FireSizeCDF,y=fire.oc, fun = function(x,y) ifelse(y==1,x,NA))
    return(FireSizeCDF.masked)
  }






## In this next step we compare the fire size CDF estimates to the CDF lookup table to randoml draw a fire size that corresponds to that CDF category.

f.size.calc = function(){
number.df = f.conver("fire.number")
size.df = f.conver("CDF")

data = left_join(number.df,size.df, by = c("x","y"))
data.exp <- data[rep(row.names(data), data$fire.number), 1:4]
data.exp$CDF = ifelse(data.exp$CDF >101, 101,data.exp$CDF)
data.exp$fire.number = suppressWarnings(ifelse(data.exp$fire.number>1,1,data.exp$fire.number)) # This is to convert the places that have two fires to one when we duplicate the rows. (we are going longer so now each row =1 fire.)
if(length(data.exp$fire.number) == 0){
  print(paste0("No fire occurred in year ",t))
return(data.exp)
  }else{
  data.exp$CDF.lookup=1
  for(i in 1:length(data.exp$fire.number)){
  k=data.exp[i,4]
  s=round(runif(1,1,200),digits=0)                       # draw the random number and grab the corresponding value from the CDF lookup table
 data.exp[i,5] = fire.size.draws[s,k] 
  }

#then conver from cdf value to fire size using a generalized pareto distribution.
  param = read.table(paste0(path,region,"/inputs/",region,"_GeneralizedPeretoParameters_1985_1994.txt"), sep = ",", header = F)  # read in the generalized pareto parameters
  data.exp$fire.size = eva::qgpd(data.exp$CDF.lookup, shape =param[,1], scale = param[,2], loc = param[,3])
  data.exp = data.exp %>%mutate(fire.size = fire.size*fire.size.mult)#fire.size.mult is the multiplier for testing how past suppression affects outcomes. See main model. 
  print(paste0("Fire size multiplier is ",fire.size.mult))
  data.exp = data.exp %>% mutate( fire.size = ifelse(fire.size<100, 100, fire.size))
  return(data.exp)
  }
}





#Produce a fire product for ingestion back into the main model by taking the values calculating above and assigning the fires to random forest_buffered 1km pixels and then growing the firest

s = function(){
  return(ifelse(runif(1,0,1)>=0.5,500,-500))    # A function to calculate the offset randomly when going from the middle of a 12 km pixel to the nearest 1km pixels
}

dist <- function(dataset, a, b){
                dt <- data.table::data.table((dataset$x-a)^2+(dataset$y-b)^2)   # function for identifying the nearest neighbor. This is used when our first random fire assignment does not land in a 1km pixel with forest_buffer
                return(which.min(dt$V1))
                }
# First estimate which 1km pixel within the 12 km pixel gets the fire.
make_fire = function(){
 if(nrow(fireSize)>0){
   final=matrix(nrow = nrow(fireSize),ncol = 2)   
for (i in 1:nrow(final)) {
    final[i,1] = fireSize[i,1] + round(runif(1,-5000,5000),-3)+s()
    final[i,2] = fireSize[i,2] + round(runif(1,-5000,5000),-3)+s()
    }

colnames(final) <- c("x", "y")
template=forest_buffer
  values(template)[values(template) >=0] = 0
  forest_buffer.df=as.data.frame(template, xy=T)
final.check=left_join(x=data.frame(final),y=forest_buffer.df, by =c("x","y"))        
forest_buffer.df.small = forest_buffer.df %>% filter(!is.na(forest_buffer))
for (i in 1:nrow(final)) {
  idx=dist(forest_buffer.df.small,final.check[i,1],final.check[i,2])     # for fires that don't land in forest_buffer, then find the nearest forest_buffered pixel to place the fire.
    final[i,1] =  forest_buffer.df.small[idx,1]
    final[i,2] = forest_buffer.df.small[idx,2]
    }

data =landscapeR::makeClass(template,sum(fireSize$fire.number),fireSize$fire.size/100,pts = final)  #then implement fires on the landscape using landscapeR package
  check=raster::clump(data, directions=8, gaps=FALSE)
  values(check)[is.na(values(check))] = 0
  twixt=as.matrix(check)                                 # convert back to matrix for ingestion in main model. Voile! We have fire.
  twixt = rbind(NA, cbind(NA, twixt, NA), NA)
return(twixt)
  }else{
 
  template=forest_buffer
  values(template)[values(template) >=0] = 0
  check=raster::clump(template, directions=8, gaps=FALSE)
  values(check)[is.na(values(check))] = 0
  twixt=as.matrix(check)                                 # convert back to matrix for ingestion in main model. Voile! We have fire.
  twixt = rbind(NA, cbind(NA, twixt, NA), NA)
return(twixt)
}
}





