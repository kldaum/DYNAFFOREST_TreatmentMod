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


#Define a height increment function

height_increment= function(pft,Ht) {
  ifelse(pft == 0, NA,
         ifelse(pft == 1, Hmax_Potr*(1-(1-(Ht/Hmax_Potr)^(1/3))*e^-g_Potr)^3,
         ifelse(pft == 2, Hmax_Pipo_Psme.C*(1-(1-(Ht/Hmax_Pipo_Psme.C)^(1/3))*e^-g_Pipo_Psme.C)^3,
         ifelse(pft == 3, Hmax_Psme.C*(1-(1-(Ht/Hmax_Psme.C)^(1/3))*e^-g_Psme.C)^3,
         ifelse(pft == 4, Hmax_Psme.I*(1-(1-(Ht/Hmax_Psme.I)^(1/3))*e^-g_Psme.I)^3,
         ifelse(pft == 5, Hmax_Pien*(1-(1-(Ht/Hmax_Pien)^(1/3))*e^-g_Pien)^3,
         ifelse(pft == 6, Hmax_Pial*(1-(1-(Ht/Hmax_Pial)^(1/3))*e^-g_Pial)^3,
         ifelse(pft == 7, Hmax_Thpl*(1-(1-(Ht/Hmax_Thpl)^(1/3))*e^-g_Thpl)^3,
         ifelse(pft == 8, Hmax_Pico*(1-(1-(Ht/Hmax_Pico)^(1/3))*e^-g_Pico)^3,
         ifelse(pft == 9, Hmax_Abgr*(1-(1-(Ht/Hmax_Abgr)^(1/3))*e^-g_Abgr)^3,
         ifelse(pft == 10, Hmax_Pied*(1-(1-(Ht/Hmax_Pied)^(1/3))*e^-g_Pied)^3,
         ifelse(pft == 11, Hmax_Pipo.N*(1-(1-(Ht/Hmax_Pipo.N)^(1/3))*e^-g_Pipo.N)^3,
         ifelse(pft == 12, Hmax_Pipo.S*(1-(1-(Ht/Hmax_Pipo.S)^(1/3))*e^-g_Pipo.S)^3,NA)))))))))))))
}



#define a dbh increment function
dbh_increment= function(Ht,pft){
  ifelse(pft == 0, NA,  
         ifelse(Ht < 1.37,0,
                ifelse(pft == 1,Ht/HD_Potr*100, 
                ifelse(pft == 2,Ht/HD_Pipo_Psme.C*100,
                ifelse(pft == 3,Ht/HD_Psme.C*100,
                ifelse(pft == 4,Ht/HD_Psme.I*100,
                ifelse(pft == 5,Ht/HD_Pien*100,
                ifelse(pft == 6,Ht/HD_Pial*100,
                ifelse(pft == 7,Ht/HD_Thpl*100,
                ifelse(pft == 8,Ht/HD_Pico*100,
                ifelse(pft == 9,Ht/HD_Abgr*100,
                ifelse(pft == 10,Ht/HD_Pied*100,
                ifelse(pft == 11,Ht/HD_Pipo.N*100,
                ifelse(pft == 12,Ht/HD_Pipo.S*100,NA))))))))))))))
}



#Define stand density function from Reineke's r
density_est= function(pft,DBH) {
ifelse(pft == 0, NA,
       ifelse(DBH == 0, 0, 
              ifelse(pft == 1, SDI_Potr*2.47105*(DBH/25)^-1.605,
              ifelse(pft == 2, SDI_Pipo_Psme.C*2.47105*(DBH/25)^-1.605,
              ifelse(pft == 3, SDI_Psme.C*2.47105*(DBH/25)^-1.605,
              ifelse(pft == 4, SDI_Psme.I*2.47105*(DBH/25)^-1.605,
              ifelse(pft == 5, SDI_Pien*2.47105*(DBH/25)^-1.605,
              ifelse(pft == 6, SDI_Pial*2.47105*(DBH/25)^-1.605,
              ifelse(pft == 7, SDI_Thpl*2.47105*(DBH/25)^-1.605,
              ifelse(pft == 8, SDI_Pico*2.47105*(DBH/25)^-1.605,
              ifelse(pft == 9, SDI_Abgr*2.47105*(DBH/25)^-1.605,
              ifelse(pft == 10, SDI_Pied*2.47105*(DBH/25)^-1.605,
              ifelse(pft == 11, SDI_Pipo.N*2.47105*(DBH/25)^-1.605,
              ifelse(pft == 12, SDI_Pipo.S*2.47105*(DBH/25)^-1.605,NA))))))))))))))

}


#Define biomass allocation function
biomass= function(pft,fire, dead, DBH,a.Potr,b.Potr,a.Pipo_Psme.C,b.Pipo_Psme.C,a.Psme.C,b.Psme.C,a.Psme.I,b.Psme.I,
                  a.Pien,b.Pien,a.Pial,b.Pial,a.Thpl,b.Thpl,a.Pico,b.Pico,
                  a.Abgr,b.Abgr,a.Pied,b.Pied,a.Pipo.N,b.Pipo.N,a.Pipo.S,b.Pipo.S,density, live_pool.past, ck, component) {
b = ifelse(pft == 0, NA,
       ifelse(DBH == 0, 0,
              ifelse(pft == 1, ((a.Potr*DBH^b.Potr)*density),
              ifelse(pft == 2, ((a.Pipo_Psme.C*DBH^b.Pipo_Psme.C)*density),
              ifelse(pft == 3, ((a.Psme.C*DBH^b.Psme.C)*density),
              ifelse(pft == 4, ((a.Psme.I*DBH^b.Psme.I)*density),
              ifelse(pft == 5, ((a.Pien*DBH^b.Pien)*density),
              ifelse(pft == 6, ((a.Pial*DBH^b.Pial)*density),
              ifelse(pft == 7, ((a.Thpl*DBH^b.Thpl)*density),
              ifelse(pft == 8, ((a.Pico*DBH^b.Pico)*density),
              ifelse(pft == 9, ((a.Abgr*DBH^b.Abgr)*density),
              ifelse(pft == 10, ((a.Pied*DBH^b.Pied)*density),
              ifelse(pft == 11, ((a.Pipo.N*DBH^b.Pipo.N)*density),
              ifelse(pft == 12, ((a.Pipo.S*DBH^b.Pipo.S)*density),NA))))))))))))))
if(component == "stem"){
  b.2 = b
}else{
  b.2 = ifelse(fire >= 1 & dead == 0, b - (live_pool.past*(ck/100)),b)
}
  return(b.2)
}


#Define function probability of intrinsic tree death Currently this doesn't change with age, but it probably should
prob_intrin = function(pft){
  ifelse(pft == 0, 0,
         ifelse(pft == 1, 1-Plucky_Potr^(1/max_age_Potr),
         ifelse(pft == 2, 1-Plucky_Pipo_Psme.C^(1/max_age_Pipo_Psme.C),
         ifelse(pft == 3, 1-Plucky_Psme.C^(1/max_age_Psme.C),
         ifelse(pft == 4, 1-Plucky_Psme.I^(1/max_age_Psme.I),
         ifelse(pft == 5, 1-Plucky_Pien^(1/max_age_Pien),
         ifelse(pft == 6, 1-Plucky_Pial^(1/max_age_Pial),
         ifelse(pft == 7, 1-Plucky_Thpl^(1/max_age_Thpl),
         ifelse(pft == 8, 1-Plucky_Pico^(1/max_age_Pico),
         ifelse(pft == 9, 1-Plucky_Abgr^(1/max_age_Abgr),
         ifelse(pft == 10, 1-Plucky_Pied^(1/max_age_Pied),
         ifelse(pft == 11, 1-Plucky_Pipo.N^(1/max_age_Pipo.N),
         ifelse(pft == 12, 1-Plucky_Pipo.S^(1/max_age_Pipo.S), NA)))))))))))))
}




#Define function for probability of tree death due to low soil moisture
prob_stress = function(pft, psi.year.current, psi.median){

  ifelse(pft == 0, 0,
         ifelse(pft == 1,  round(1/(1+e^(-(drought_a*(psi.year.current - (psi.median - HSM_Potr/drought_b))))), digits = 2),
         ifelse(pft == 2, round(1/(1+e^(-(drought_a*(psi.year.current - (psi.median - HSM_Pipo_Psme.C/drought_b))))), digits = 2),
         ifelse(pft == 3, round(1/(1+e^(-(drought_a*(psi.year.current - (psi.median - HSM_Psme.C/drought_b))))), digits = 2),
         ifelse(pft == 4, round(1/(1+e^(-(drought_a*(psi.year.current - (psi.median - HSM_Psme.I/drought_b))))), digits = 2),
         ifelse(pft == 5, round(1/(1+e^(-(drought_a*(psi.year.current - (psi.median - HSM_Pien/drought_b))))), digits = 2),
         ifelse(pft == 6, round(1/(1+e^(-(drought_a*(psi.year.current - (psi.median - HSM_Pial/drought_b))))), digits = 2),
         ifelse(pft == 7,  round(1/(1+e^(-(drought_a*(psi.year.current - (psi.median - HSM_Thpl/drought_b))))), digits = 2),
         ifelse(pft == 8, round(1/(1+e^(-(drought_a*(psi.year.current - (psi.median - HSM_Pico/drought_b))))), digits = 2),
         ifelse(pft == 9, round(1/(1+e^(-(drought_a*(psi.year.current - (psi.median - HSM_Abgr/drought_b))))), digits = 2),
         ifelse(pft == 10, round(1/(1+e^(-(drought_a*(psi.year.current - (psi.median - HSM_Pied/drought_b))))), digits = 2),
         ifelse(pft == 11, round(1/(1+e^(-(drought_a*(psi.year.current - (psi.median - HSM_Pipo.N/drought_b))))), digits = 2),
         ifelse(pft == 12, round(1/(1+e^(-(drought_a*(psi.year.current - (psi.median - HSM_Pipo.S/drought_b))))), digits = 2),NA)))))))))))))
} 




#Define function for calculating the dead pools for litter and cwd when the trees are alive and when the trees are dead either by fire or some other caused
dead_influx = function(pft,dead,fire, dead_pool,decomp,live_pool,turnover.Potr,turnover.Pipo_Psme.C,turnover.Psme.C,turnover.Psme.I,turnover.Pien,turnover.Pial,turnover.Thpl,
                       turnover.Pico,turnover.Abgr,turnover.Pied,turnover.Pipo.N,turnover.Pipo.S,live.consumed,ck, dead_avail){
  
 b= ifelse(dead == 1 & fire == 0, (dead_pool-dead_pool*decomp)+(live_pool),
    ifelse(dead ==1 & fire >= 1 , (dead_pool-dead_pool*decomp-dead_avail) + (live_pool-(live_pool*live.consumed)),
    ifelse(dead > 1, dead_pool - dead_pool*decomp,
    ifelse(dead == 0 & pft == 1, (dead_pool - dead_pool*decomp) + live_pool*turnover.Potr,
    ifelse(dead == 0 & pft == 2, (dead_pool - dead_pool*decomp) + live_pool*turnover.Pipo_Psme.C,
    ifelse(dead == 0 & pft == 3, (dead_pool - dead_pool*decomp) + live_pool*turnover.Psme.C,
    ifelse(dead == 0 & pft == 4, (dead_pool - dead_pool*decomp) + live_pool*turnover.Psme.I,
    ifelse(dead == 0 & pft == 5, (dead_pool - dead_pool*decomp) + live_pool*turnover.Pien,
    ifelse(dead == 0 & pft == 6, (dead_pool - dead_pool*decomp) + live_pool*turnover.Pial,
    ifelse(dead == 0 & pft == 7, (dead_pool - dead_pool*decomp) + live_pool*turnover.Thpl,
    ifelse(dead == 0 & pft == 8, (dead_pool - dead_pool*decomp) + live_pool*turnover.Pico,
    ifelse(dead == 0 & pft == 9, (dead_pool - dead_pool*decomp) + live_pool*turnover.Abgr,
    ifelse(dead == 0 & pft == 10, (dead_pool - dead_pool*decomp) + live_pool*turnover.Pied,
    ifelse(dead == 0 & pft == 11, (dead_pool - dead_pool*decomp) + live_pool*turnover.Pipo.N,
    ifelse(dead == 0 & pft == 12, (dead_pool - dead_pool*decomp) + live_pool*turnover.Pipo.S, NA)))))))))))))))
 
 b2 =  ifelse(dead ==0 & fire >= 1 , b - dead_avail + (live_pool*(ck/100) - live_pool*(ck/100)*live.consumed), b)
 b2=ifelse(b2<0,0,b2)
 return(b2)
}





#Define a function for snag fall
snag_pool = function(snag_pool,e,snag_hl){
  snag_pool*e^(-log(2)/snag_hl)
}


#Define a function to reset live variables to initial values when tree death occurs
live_reset = function(dead.now,established,live.t,live.init){
  ifelse(dead.now == 0,live.t,
         ifelse(established == 0,0,live.init))
}




#Define function to estimate establishment probability due to climate conditions
establishment.threshold1.calc = function(dead, phenology,phenology.threshold.Potr,phenology.threshold.Pipo_Psme.C,phenology.threshold.Psme.C,
                                         phenology.threshold.Psme.I,phenology.threshold.Pien,phenology.threshold.Pial,phenology.threshold.Thpl,
                                         phenology.threshold.Pico,phenology.threshold.Abgr,phenology.threshold.Pied,phenology.threshold.Pipo.N,phenology.threshold.Pipo.S){
  establishment.threshold.1.Potr = ifelse(dead>0 & phenology > phenology.threshold.Potr,1,0)
  establishment.threshold.1.Pipo_Psme.C= ifelse(dead>0 & phenology > phenology.threshold.Pipo_Psme.C,1,0)
  establishment.threshold.1.Psme.C= ifelse(dead>0 & phenology > phenology.threshold.Psme.C,1,0)
  establishment.threshold.1.Psme.I= ifelse(dead>0 & phenology > phenology.threshold.Psme.I,1,0)
  establishment.threshold.1.Pien= ifelse(dead>0 & phenology > phenology.threshold.Pien,1,0)
  establishment.threshold.1.Pial= ifelse (dead>0 & phenology > phenology.threshold.Pial,1,0)
  establishment.threshold.1.Thpl= ifelse(dead>0 & phenology > phenology.threshold.Thpl,1,0)
  establishment.threshold.1.Pico= ifelse(dead>0 & phenology > phenology.threshold.Pico,1,0)
  establishment.threshold.1.Abgr= ifelse(dead>0 & phenology > phenology.threshold.Abgr,1,0)
  establishment.threshold.1.Pied= ifelse(dead>0 & phenology > phenology.threshold.Pied,1,0)
  establishment.threshold.1.Pipo.N= ifelse(dead>0 & phenology > phenology.threshold.Pipo.N,1,0)
  establishment.threshold.1.Pipo.S= ifelse(dead>0 & phenology > phenology.threshold.Pipo.S,1,0)
  return(list(establishment.threshold.1.Potr, establishment.threshold.1.Pipo_Psme.C,establishment.threshold.1.Psme.C,
              establishment.threshold.1.Psme.I,establishment.threshold.1.Pien,establishment.threshold.1.Pial,establishment.threshold.1.Thpl,
              establishment.threshold.1.Pico,establishment.threshold.1.Abgr,establishment.threshold.1.Pied,
              establishment.threshold.1.Pipo.N,establishment.threshold.1.Pipo.S))
}


establishment.threshold2.calc = function(dead, phenology.Potr, phenology.Pipo_Psme.C,phenology.Psme.C,phenology.Psme.I, phenology.Pien,phenology.Pial, 
                                         phenology.Thpl,phenology.Pico,phenology.Abgr,phenology.Pied,phenology.Pipo.N,phenology.Pipo.S,
                                         phenology.threshold.min.Potr, phenology.threshold.max.Potr, phenology.threshold.min.Pipo_Psme.C, 
                                         phenology.threshold.max.Pipo_Psme.C,phenology.threshold.min.Psme.C, phenology.threshold.max.Psme.C,
                                         phenology.threshold.min.Psme.I, phenology.threshold.max.Psme.I, phenology.threshold.min.Pien, phenology.threshold.max.Pien,
                                         phenology.threshold.min.Pial, phenology.threshold.max.Pial, phenology.threshold.min.Thpl, phenology.threshold.max.Thpl,
                                         phenology.threshold.min.Pico, phenology.threshold.max.Pico,phenology.threshold.min.Abgr, phenology.threshold.max.Abgr,
                                         phenology.threshold.min.Pied, phenology.threshold.max.Pied,phenology.threshold.min.Pipo.N, phenology.threshold.max.Pipo.N,
                                         phenology.threshold.min.Pipo.S, phenology.threshold.max.Pipo.S){
  
  establishment.threshold.2.Potr=ifelse(dead > 0 & phenology.Potr > phenology.threshold.min.Potr & phenology.Potr < phenology.threshold.max.Potr,1,0)
  establishment.threshold.2.Pipo_Psme.C=ifelse(dead > 0 & phenology.Pipo_Psme.C > phenology.threshold.min.Pipo_Psme.C & phenology.Pipo_Psme.C < phenology.threshold.max.Pipo_Psme.C,1,0)
  establishment.threshold.2.Psme.C=ifelse(dead > 0 & phenology.Psme.C > phenology.threshold.min.Psme.C & phenology.Psme.C < phenology.threshold.max.Psme.C,1,0)
  establishment.threshold.2.Psme.I=ifelse(dead > 0 & phenology.Psme.I > phenology.threshold.min.Psme.I & phenology.Psme.I < phenology.threshold.max.Psme.I,1,0)
  establishment.threshold.2.Pien=ifelse(dead > 0 & phenology.Pien > phenology.threshold.min.Pien & phenology.Pien < phenology.threshold.max.Pien,1,0)
  establishment.threshold.2.Pial=ifelse(dead > 0 & phenology.Pial > phenology.threshold.min.Pial & phenology.Pial < phenology.threshold.max.Pial,1,0)
  establishment.threshold.2.Thpl=ifelse(dead > 0 & phenology.Thpl > phenology.threshold.min.Thpl & phenology.Thpl < phenology.threshold.max.Thpl,1,0)
  establishment.threshold.2.Pico=ifelse(dead > 0 & phenology.Pico > phenology.threshold.min.Pico & phenology.Pico < phenology.threshold.max.Pico,1,0)
  establishment.threshold.2.Abgr=ifelse(dead > 0 & phenology.Abgr > phenology.threshold.min.Abgr & phenology.Abgr < phenology.threshold.max.Abgr,1,0)
  establishment.threshold.2.Pied=ifelse(dead > 0 & phenology.Pied > phenology.threshold.min.Pied & phenology.Pied < phenology.threshold.max.Pied,1,0)
  establishment.threshold.2.Pipo.N=ifelse(dead > 0 & phenology.Pipo.N > phenology.threshold.min.Pipo.N & phenology.Pipo.N < phenology.threshold.max.Pipo.N,1,0)
  establishment.threshold.2.Pipo.S=ifelse(dead > 0 & phenology.Pipo.S > phenology.threshold.min.Pipo.S & phenology.Pipo.S < phenology.threshold.max.Pipo.S,1,0)
   return(list(establishment.threshold.2.Potr, establishment.threshold.2.Pipo_Psme.C,establishment.threshold.2.Psme.C,establishment.threshold.2.Psme.I,
            establishment.threshold.2.Pien,establishment.threshold.2.Pial,establishment.threshold.2.Thpl,establishment.threshold.2.Pico,establishment.threshold.2.Abgr,
            establishment.threshold.2.Pied,establishment.threshold.2.Pipo.N,establishment.threshold.2.Pipo.S))
  
}

#Define function to estimate establishment probability due to climate conditions
establishment.threshold3.calc = function(dead, phenology.Potr, phenology.Pipo_Psme.C, phenology.Psme.C, phenology.Psme.I, phenology.Pien, phenology.Pial, 
                                         phenology.Thpl, phenology.Pico, phenology.Abgr,phenology.Pied,phenology.Pipo.N,phenology.Pipo.S,
                                         phenology.threshold.Potr, phenology.threshold.Pipo_Psme.C,phenology.threshold.Psme.C,phenology.threshold.Psme.I, 
                                         phenology.threshold.Pien, phenology.threshold.Pial, phenology.threshold.Thpl,phenology.threshold.Pico,
                                         phenology.threshold.Abgr,phenology.threshold.Pied,phenology.threshold.Pipo.N,phenology.threshold.Pipo.S){
  establishment.threshold.3.Potr= ifelse(dead>0 & phenology.Potr > 0,1,0)
  establishment.threshold.3.Pipo_Psme.C= ifelse(dead>0 & phenology.Pipo_Psme.C > 0,1,0)
  establishment.threshold.3.Psme.C= ifelse(dead>0 & phenology.Psme.C > 0,1,0)   #Changed these to zero instead of the block below since this is how park gave me this one He calculated the days past when the threshold was crossed, it scores a zero if the threshold was not crossed..
  establishment.threshold.3.Psme.I= ifelse(dead>0 & phenology.Psme.I > 0,1,0)
  establishment.threshold.3.Pien= ifelse(dead>0 & phenology.Pien > 0,1,0)
  establishment.threshold.3.Pial= ifelse(dead>0 & phenology.Pial > 0,1,0)
  establishment.threshold.3.Thpl= ifelse(dead>0 & phenology.Thpl > 0,1,0)
  establishment.threshold.3.Pico= ifelse(dead>0 & phenology.Pico > 0,1,0)
  establishment.threshold.3.Abgr= ifelse(dead>0 & phenology.Abgr > 0,1,0)
  establishment.threshold.3.Pied= ifelse(dead>0 & phenology.Pied > 0,1,0)
  establishment.threshold.3.Pipo.N= ifelse(dead>0 & phenology.Pipo.N > 0,1,0)
  establishment.threshold.3.Pipo.S= ifelse(dead>0 & phenology.Pipo.S > 0,1,0)
 
  
  
  return(list(establishment.threshold.3.Potr, establishment.threshold.3.Pipo_Psme.C,establishment.threshold.3.Psme.C,
              establishment.threshold.3.Psme.I, establishment.threshold.3.Pien, establishment.threshold.3.Pial, 
              establishment.threshold.3.Thpl, establishment.threshold.3.Pico,establishment.threshold.3.Abgr,
              establishment.threshold.3.Pied,establishment.threshold.3.Pipo.N,establishment.threshold.3.Pipo.S))
}

p_estab.abiotic.calc = function(dead, min.temp.flag.Potr, GDD.flag.Potr, chill.flag.Potr, bud_burst.flag.Potr,frost_after_bud.Potr,
                                min.temp.flag.Pipo_Psme.C, GDD.flag.Pipo_Psme.C, chill.flag.Pipo_Psme.C, bud_burst.flag.Pipo_Psme.C,frost_after_bud.Pipo_Psme.C,
                                min.temp.flag.Psme.C, GDD.flag.Psme.C, chill.flag.Psme.C, bud_burst.flag.Psme.C,frost_after_bud.Psme.C,
                                min.temp.flag.Psme.I, GDD.flag.Psme.I, chill.flag.Psme.I, bud_burst.flag.Psme.I,frost_after_bud.Psme.I,
                                min.temp.flag.Pien, GDD.flag.Pien, chill.flag.Pien, bud_burst.flag.Pien, frost_after_bud.Pien,
                                min.temp.flag.Pial, GDD.flag.Pial, chill.flag.Pial, bud_burst.flag.Pial,frost_after_bud.Pial,
                                min.temp.flag.Thpl, GDD.flag.Thpl, chill.flag.Thpl, bud_burst.flag.Thpl,frost_after_bud.Thpl,
                                min.temp.flag.Pico, GDD.flag.Pico, chill.flag.Pico, bud_burst.flag.Pico,frost_after_bud.Pico,
                                min.temp.flag.Abgr, GDD.flag.Abgr, chill.flag.Abgr, bud_burst.flag.Abgr,frost_after_bud.Abgr,
                                min.temp.flag.Pied, GDD.flag.Pied, chill.flag.Pied, bud_burst.flag.Pied,frost_after_bud.Pied,
                                min.temp.flag.Pipo.N, GDD.flag.Pipo.N, chill.flag.Pipo.N, bud_burst.flag.Pipo.N,frost_after_bud.Pipo.N,
                                min.temp.flag.Pipo.S, GDD.flag.Pipo.S, chill.flag.Pipo.S, bud_burst.flag.Pipo.S,frost_after_bud.Pipo.S, psi){
  
  grass.scaler=ifelse(((grass.max+1)-dead)/grass.max>grass.estab.min,((grass.max+1)-dead)/grass.max,grass.estab.min) #this scales grass inhibition effects linearly from 0 to where grass has its maximum effect. 
  
  p_estab.Potr=ifelse(dead==0, 0,
                      ifelse(min.temp.flag.Potr ==1 & GDD.flag.Potr == 1 & chill.flag.Potr == 1 & bud_burst.flag.Potr == 1,
                             pmin(pmax(frost.tolerance_Potr^sqrt(frost_after_bud.Potr) * pmin((psi - psi.min.seedling_Potr)/(-0.015 - psi.min.seedling_Potr),1) * grass.scaler,0),1),0))
  p_estab.Pipo_Psme.C=ifelse(dead==0, 0,
                        ifelse(min.temp.flag.Pipo_Psme.C ==1 & GDD.flag.Pipo_Psme.C == 1 & chill.flag.Pipo_Psme.C == 1 & bud_burst.flag.Pipo_Psme.C== 1,
                               pmin(pmax(frost.tolerance_Pipo_Psme.C^sqrt(frost_after_bud.Pipo_Psme.C) * pmin((psi - psi.min.seedling_Pipo_Psme.C)/(-0.015 - psi.min.seedling_Pipo_Psme.C),1) * grass.scaler,0),1),0))
  p_estab.Psme.C=ifelse(dead==0, 0,
                        ifelse(min.temp.flag.Psme.C ==1 & GDD.flag.Psme.C == 1 & chill.flag.Psme.C == 1 & bud_burst.flag.Psme.C == 1,
                               pmin(pmax(frost.tolerance_Psme.C^sqrt(frost_after_bud.Psme.C) * pmin((psi - psi.min.seedling_Psme.C)/(-0.015 - psi.min.seedling_Psme.C),1) * grass.scaler,0),1),0))
 
  p_estab.Psme.I=ifelse(dead==0, 0,
                    ifelse(min.temp.flag.Psme.I ==1 & GDD.flag.Psme.I == 1 & chill.flag.Psme.I == 1 & bud_burst.flag.Psme.I == 1,
                           pmin(pmax(frost.tolerance_Psme.I^sqrt(frost_after_bud.Psme.I) * pmin((psi - psi.min.seedling_Psme.I)/(-0.015 - psi.min.seedling_Psme.I),1) * grass.scaler,0),1),0))
  p_estab.Pien=ifelse(dead==0, 0,
                     ifelse(min.temp.flag.Pien ==1 & GDD.flag.Pien == 1 & chill.flag.Pien == 1 & bud_burst.flag.Pien == 1,
                            pmin(pmax(frost.tolerance_Pien^sqrt(frost_after_bud.Pien) * pmin((psi - psi.min.seedling_Pien)/(-0.015 - psi.min.seedling_Pien),1) *grass.scaler,0),1),0))
  
  p_estab.Pial=ifelse(dead==0, 0,
                      ifelse(min.temp.flag.Pial ==1 & GDD.flag.Pial == 1 & chill.flag.Pial == 1 & bud_burst.flag.Pial == 1,
                             pmin(pmax(frost.tolerance_Pial^sqrt(frost_after_bud.Pial) * pmin((psi - psi.min.seedling_Pial)/(-0.015 - psi.min.seedling_Pial),1) * grass.scaler,0),1),0))
  p_estab.Thpl=ifelse(dead==0, 0,
                      ifelse(min.temp.flag.Thpl ==1 & GDD.flag.Thpl == 1 & chill.flag.Thpl == 1 & bud_burst.flag.Thpl == 1,
                             pmin(pmax(frost.tolerance_Thpl^sqrt(frost_after_bud.Thpl) * pmin((psi - psi.min.seedling_Thpl)/(-0.015 - psi.min.seedling_Thpl),1) * grass.scaler,0),1),0))
  
  p_estab.Pico=ifelse(dead==0, 0,
                      ifelse(min.temp.flag.Pico ==1 & GDD.flag.Pico == 1 & chill.flag.Pico == 1 & bud_burst.flag.Pico == 1,
                             pmin(pmax(frost.tolerance_Pico^sqrt(frost_after_bud.Pico) * pmin((psi - psi.min.seedling_Pico)/(-0.015 - psi.min.seedling_Pico),1) * grass.scaler,0),1),0))
  p_estab.Abgr=ifelse(dead==0, 0,
                      ifelse(min.temp.flag.Abgr ==1 & GDD.flag.Abgr == 1 & chill.flag.Abgr == 1 & bud_burst.flag.Abgr == 1,
                             pmin(pmax(frost.tolerance_Abgr^sqrt(frost_after_bud.Abgr) * pmin((psi - psi.min.seedling_Abgr)/(-0.015 - psi.min.seedling_Abgr),1) * grass.scaler,0),1),0))
  p_estab.Pied=ifelse(dead==0, 0,
                      ifelse(min.temp.flag.Pied ==1 & GDD.flag.Pied == 1 & chill.flag.Pied == 1 & bud_burst.flag.Pied == 1,
                             pmin(pmax(frost.tolerance_Pied^sqrt(frost_after_bud.Pied) * pmin((psi - psi.min.seedling_Pied)/(-0.015 - psi.min.seedling_Pied),1) * grass.scaler,0),1),0))
  p_estab.Pipo.N=ifelse(dead==0, 0,
                      ifelse(min.temp.flag.Pipo.N ==1 & GDD.flag.Pipo.N == 1 & chill.flag.Pipo.N == 1 & bud_burst.flag.Pipo.N == 1,
                             pmin(pmax(frost.tolerance_Pipo.N^sqrt(frost_after_bud.Pipo.N) * pmin((psi - psi.min.seedling_Pipo.N)/(-0.015 - psi.min.seedling_Pipo.N),1) * grass.scaler,0),1),0))
  p_estab.Pipo.S=ifelse(dead==0, 0,
                      ifelse(min.temp.flag.Pipo.S ==1 & GDD.flag.Pipo.S == 1 & chill.flag.Pipo.S == 1 & bud_burst.flag.Pipo.S == 1,
                             pmin(pmax(frost.tolerance_Pipo.S^sqrt(frost_after_bud.Pipo.S) * pmin((psi - psi.min.seedling_Pipo.S)/(-0.015 - psi.min.seedling_Pipo.S),1) * grass.scaler,0),1),0))
 
   return(list(p_estab.Potr, p_estab.Pipo_Psme.C, p_estab.Psme.C, p_estab.Psme.I, p_estab.Pien, p_estab.Pial, p_estab.Thpl, p_estab.Pico, p_estab.Abgr, p_estab.Pied, p_estab.Pipo.N, p_estab.Pipo.S))
  
  
}

p_dispersal.calc = function(dead,dead.fire,pft,age){
  
  x=which( dead > 0, arr.ind=TRUE)
  
  if(dim(x)[1]==0){
    return(list(list.prob.dispersal[[1]][[1]],list.prob.dispersal[[1]][[1]],list.prob.dispersal[[1]][[1]],
                list.prob.dispersal[[1]][[1]],list.prob.dispersal[[1]][[1]],list.prob.dispersal[[1]][[1]],
                list.prob.dispersal[[1]][[1]],list.prob.dispersal[[1]][[1]],list.prob.dispersal[[1]][[1]],
                list.prob.dispersal[[1]][[1]],list.prob.dispersal[[1]][[1]],list.prob.dispersal[[1]][[1]]))
  }else{
    x.use=x
  }
  
  final=matrix(nrow = nrow(x.use),ncol = 29)        #This adjusts with number of PFTS
  neigh=matrix( nrow = 8,ncol = 1)
  
  for (i in 1:nrow(x.use)) {
    row = x.use[i,1]
    col = x.use[i,2]
    
    neigh[1] = as.vector(pft[row - 1, col    ])                              #PFT in the N direction from the cell
    neigh[2] = as.vector(pft[row - 1, col + 1])    #PFT in the NE direction from the cell
    neigh[3] = as.vector(pft[row    , col + 1])    #PFT in the E direction from the cell
    neigh[4] = as.vector(pft[row + 1, col + 1])    #PFT in the SE direction from the cell
    neigh[5] = as.vector(pft[row + 1, col    ])   #PFT in the S direction from the cell
    neigh[6] = as.vector(pft[row + 1, col - 1])   #PFT in the sW direction from the cell
    neigh[7] = as.vector(pft[row    , col - 1])   #PFT from the W direction of the cell
    neigh[8] = as.vector(pft[row - 1, col - 1])   #PFT in the NW direction of the cell
    
    
    
    
    Potr=sum(neigh == 1, na.rm=TRUE)
    Pipo_Psme.C=sum(neigh == 2, na.rm=TRUE)
    Psme.C=sum(neigh == 3, na.rm=TRUE)
    Psme.I=sum(neigh == 4, na.rm=TRUE)
    Pien=sum(neigh == 5, na.rm=TRUE)
    Pial=sum(neigh == 6, na.rm=TRUE)
    Thpl=sum(neigh == 7, na.rm=TRUE)
    Pico=sum(neigh == 8, na.rm=TRUE)
    Abgr=sum(neigh == 9, na.rm=TRUE)
    Pied=sum(neigh == 10, na.rm=TRUE)
    Pipo.N=sum(neigh == 11, na.rm=TRUE)
    Pipo.S=sum(neigh == 12, na.rm=TRUE)
    CELL=pft[row, col]
    TIME = age[row, col]
    Killed.fire = dead.fire[row,col]
    final[i,1] = row
    final[i,2] = col
    final[i,3] = Potr
    final[i,4] = Pipo_Psme.C
    final[i,5] = Psme.C
    final[i,6] = Psme.I
    final[i,7] = Pien
    final[i,8] = Pial
    final[i,9] = Thpl
    final[i,10] = Pico
    final[i,11] = Abgr
    final[i,12] = Pied
    final[i,13] = Pipo.N
    final[i,14] = Pipo.S
    final[i,15] = CELL
    final[i,16] = TIME
    final[i,17] = Killed.fire
    final[i,18] = if(final[i,15]==1 & final[i,16]>age.repr_Potr){
      1 #min(final[i,3]*0.4981667 + 0.4168973,1)  The arrival probability for aspen
    }else{
      min(final[i,3]*0.4981667,1)
    }                                    
    final[i,19] = if(final[i,15]==2 & final[i,16]>age.repr_Pipo_Psme.C){
      1 #min(final[i,4]*0.03530807 + 0.1275197,1)  The arrival probability for California conifer
    }else{
      min(final[i,4]*0.03530807,1)
    }       
    final[i,20] = if(final[i,15]==3 & final[i,16]>age.repr_Psme.C){
      1 #min(final[i,5]*0.03530807 +  0.1275197,1) The arrival probability for Coastal Douglas fir
    }else{
      min(final[i,5]*0.03530807,1)
    }       
    
    final[i,21] = if(final[i,15]==4 & final[i,16]>age.repr_Psme.I){
      1 #min(final[i,6]*0.03530807 + 0.1275197,1)  The arrival probability for interior Douglas fir
    }else{
      min(final[i,6]*0.03530807,1)
    }               
    
    final[i,22] = if(final[i,15]==5 & final[i,16]>age.repr_Pien){
      1 #min(final[i,7]*0.01046722 + 0.6383027,1)  The arrival probability for Pien
    }else{
      min(final[i,7]*0.01046722,1)
    }               
   
     final[i,23] = if(final[i,15]==6 & final[i,16]>age.repr_Pial){
      1 #min(final[i,8]*0.0016 + 0.4843284,1)  The arrival probability for Pial
    }else{
      min(final[i,8]*0.0016,1)
    }          
     
     final[i,24] = if(final[i,15]==7 & final[i,16]>age.repr_Thpl){
       1 #min(final[i,9]*0.3769089 + 0.96,1)  The arrival probability for Thpl MUsT UPDATE
     }else{
       min(final[i,9]*0.3769089,1)
     }          
     
     final[i,25] = ifelse(final[i,15]==8 & final[i,16]>age.repr_Pico & final[i,17]==0, 1,
                          ifelse(final[i,15]==8 & final[i,16]>age.repr_Pico & final[i,17]==1 , 1, min(final[i,10]*0.08345929,1)))
#min(final[i,10]*0.08345929 + 0.6041416,1)
     
     final[i,26] = if(final[i,15]==9 & final[i,16]>age.repr_Abgr){
       1 #min(final[i,11]*0.04030679 + 0.3990382,1)  The arrival probability for Abgr MUsT UPDATE
     }else{
       min(final[i,11]*0.04030679,1)
     }         
     
     final[i,27] = if(final[i,15]==10 & final[i,16]>age.repr_Pied){
       1 #min(final[i,12]*0.01518456 + 0.4227218,1)  The arrival probability for Pied
     }else{
       min(final[i,12]*0.01518456,1)
     }         
     
     final[i,28] = if(final[i,15]==11 & final[i,16]>age.repr_Pipo.N){
       1 #min(final[i,13]*0.006076401 + 0.06061082,1)  The arrival probability for Pipo.N MUsT UPDATE
     }else{
       min(final[i,13]*0.006076401,1)
     }         
     
     final[i,29] = if(final[i,15]==12 & final[i,16]>age.repr_Pipo.S){
       1 #min(final[i,14]*0.006076401 + 0.06061082,1)  The arrival probability for Pipo.S MUsT UPDATE
     }else{
       min(final[i,14]*0.006076401,1)
     }         
     
    
  }
  prob.dispersal.Potr=list.prob.dispersal[[1]][[1]]
  prob.dispersal.Pipo_Psme.C=list.prob.dispersal[[1]][[1]]
  prob.dispersal.Psme.C=list.prob.dispersal[[1]][[1]]
  prob.dispersal.Psme.I=list.prob.dispersal[[1]][[1]]
  prob.dispersal.Pien=list.prob.dispersal[[1]][[1]]
  prob.dispersal.Pial=list.prob.dispersal[[1]][[1]]
  prob.dispersal.Thpl=list.prob.dispersal[[1]][[1]]
  prob.dispersal.Pico=list.prob.dispersal[[1]][[1]]  
  prob.dispersal.Abgr=list.prob.dispersal[[1]][[1]]
  prob.dispersal.Pied=list.prob.dispersal[[1]][[1]]
  prob.dispersal.Pipo.N=list.prob.dispersal[[1]][[1]]
  prob.dispersal.Pipo.S=list.prob.dispersal[[1]][[1]]#Here these are just creating blanks to fill in in the next step
  
  prob.dispersal.Potr[as.matrix(final[,c(1,2)])] <- as.double(final[,18])
  prob.dispersal.Pipo_Psme.C[as.matrix(final[,c(1,2)])] <- as.double(final[,19])
  prob.dispersal.Psme.C[as.matrix(final[,c(1,2)])] <- as.double(final[,20])
  prob.dispersal.Psme.I[as.matrix(final[,c(1,2)])] <- as.double(final[,21])
  prob.dispersal.Pien[as.matrix(final[,c(1,2)])] <- as.double(final[,22])
  prob.dispersal.Pial[as.matrix(final[,c(1,2)])] <- as.double(final[,23])
  prob.dispersal.Thpl[as.matrix(final[,c(1,2)])] <- as.double(final[,24])
  prob.dispersal.Pico[as.matrix(final[,c(1,2)])] <- as.double(final[,25])
  prob.dispersal.Abgr[as.matrix(final[,c(1,2)])] <- as.double(final[,26])
  prob.dispersal.Pied[as.matrix(final[,c(1,2)])] <- as.double(final[,27])
  prob.dispersal.Pipo.N[as.matrix(final[,c(1,2)])] <- as.double(final[,28])
  prob.dispersal.Pipo.S[as.matrix(final[,c(1,2)])] <- as.double(final[,29])
  
  
  return(list(prob.dispersal.Potr, prob.dispersal.Pipo_Psme.C,prob.dispersal.Psme.C,prob.dispersal.Psme.I, prob.dispersal.Pien, prob.dispersal.Pial, 
              prob.dispersal.Thpl,prob.dispersal.Pico,prob.dispersal.Abgr,prob.dispersal.Pied,prob.dispersal.Pipo.N,prob.dispersal.Pipo.S))

}





maxn <- function(n) function(x) order(x, decreasing = TRUE)[n]
max1 = maxn(1)
max2 = maxn(2)## This needs to be updated for the number of PFTs
max3 = maxn(3)
max4 = maxn(4)
max5 = maxn(5)
max6 = maxn(6)
max7 = maxn(7)
max8 = maxn(8)
max9 = maxn(9)
max10 = maxn(10)
max11 = maxn(11)
max12 = maxn(12)




established_calc = function(dead.fire,age,pft, P_estab.abiotic.Potr, P_estab.dispersal.Potr,P_estab.abiotic.Pipo_Psme.C, P_estab.dispersal.Pipo_Psme.C,
                            P_estab.abiotic.Psme.C, P_estab.dispersal.Psme.C,P_estab.abiotic.Psme.I, P_estab.dispersal.Psme.I, P_estab.abiotic.Pien, P_estab.dispersal.Pien,
                             P_estab.abiotic.Pial, P_estab.dispersal.Pial, P_estab.abiotic.Thpl, P_estab.dispersal.Thpl,P_estab.abiotic.Pico, P_estab.dispersal.Pico,
                             P_estab.abiotic.Abgr, P_estab.dispersal.Abgr,P_estab.abiotic.Pied, P_estab.dispersal.Pied,P_estab.abiotic.Pipo.N, P_estab.dispersal.Pipo.N,
                             P_estab.abiotic.Pipo.S, P_estab.dispersal.Pipo.S){
  
  
  P_estab.Potr = matrix(nrow = nrow(P_estab.abiotic.Potr), ncol=ncol(P_estab.abiotic.Potr))
  P_estab.Pipo_Psme.C = matrix(nrow = nrow(P_estab.abiotic.Pipo_Psme.C), ncol = ncol(P_estab.abiotic.Pipo_Psme.C))
  P_estab.Psme.C = matrix(nrow = nrow(P_estab.abiotic.Psme.C), ncol = ncol(P_estab.abiotic.Psme.C))
  P_estab.Psme.I = matrix(nrow = nrow(P_estab.abiotic.Psme.I), ncol = ncol(P_estab.abiotic.Psme.I))
  P_estab.Pien = matrix(nrow = nrow(P_estab.abiotic.Pien), ncol = ncol(P_estab.abiotic.Pien))
  P_estab.Pial = matrix(nrow = nrow(P_estab.abiotic.Pial), ncol = ncol(P_estab.abiotic.Pial))
  P_estab.Thpl = matrix(nrow = nrow(P_estab.abiotic.Thpl), ncol = ncol(P_estab.abiotic.Thpl))
  P_estab.Pico = matrix(nrow = nrow(P_estab.abiotic.Pico), ncol = ncol(P_estab.abiotic.Pico))
  P_estab.Abgr = matrix(nrow = nrow(P_estab.abiotic.Abgr), ncol = ncol(P_estab.abiotic.Abgr))
  P_estab.Pied = matrix(nrow = nrow(P_estab.abiotic.Pied), ncol = ncol(P_estab.abiotic.Pied))
  P_estab.Pipo.N = matrix(nrow = nrow(P_estab.abiotic.Pipo.N), ncol = ncol(P_estab.abiotic.Pipo.N))
  P_estab.Pipo.S = matrix(nrow = nrow(P_estab.abiotic.Pipo.S), ncol = ncol(P_estab.abiotic.Pipo.S))
  
  
  P_estab.Potr = ifelse(dead.fire ==1 & age > age.repr_Potr & pft==1,1, P_estab.abiotic.Potr * P_estab.dispersal.Potr)  # Aspen regenerate asexually following fire regardless of environmental conditions.
  P_estab.Pipo_Psme.C = P_estab.abiotic.Pipo_Psme.C * P_estab.dispersal.Pipo_Psme.C
  P_estab.Psme.C = P_estab.abiotic.Psme.C * P_estab.dispersal.Psme.C
  P_estab.Psme.I = P_estab.abiotic.Psme.I * P_estab.dispersal.Psme.I
  P_estab.Pien = P_estab.abiotic.Pien * P_estab.dispersal.Pien
  P_estab.Pial = P_estab.abiotic.Pial * P_estab.dispersal.Pial
  P_estab.Thpl = P_estab.abiotic.Thpl * P_estab.dispersal.Thpl
  P_estab.Pico = P_estab.abiotic.Pico * P_estab.dispersal.Pico
  P_estab.Abgr = P_estab.abiotic.Abgr * P_estab.dispersal.Abgr
  P_estab.Pied = P_estab.abiotic.Pied * P_estab.dispersal.Pied
  P_estab.Pipo.N = P_estab.abiotic.Pipo.N * P_estab.dispersal.Pipo.N
  P_estab.Pipo.S = P_estab.abiotic.Pipo.S * P_estab.dispersal.Pipo.S
  
  

  x = which( P_estab.Potr > 0|P_estab.Pipo_Psme.C>0|P_estab.Psme.C>0|P_estab.Psme.I>0|
             P_estab.Pien>0|P_estab.Pial>0|P_estab.Thpl>0|P_estab.Pico>0|P_estab.Abgr>0|
               P_estab.Pied>0|P_estab.Pipo.N>0|P_estab.Pipo.S>0, arr.ind=TRUE)
  if(dim(x)[1]==0){
    return(list(P_estab.Potr,P_estab.Potr,P_estab.Potr,P_estab.Potr,P_estab.Potr,
                P_estab.Potr,P_estab.Potr,P_estab.Potr,P_estab.Potr,P_estab.Potr,
                P_estab.Potr,P_estab.Potr))
  }else{
    x.use=x
  }
  
  establish=matrix(nrow = nrow(x.use),ncol = 28)  # This will need to be updated with the number of PFTs

  for (i in 1:nrow(x.use)) {
    row = x.use[i,1]
    col = x.use[i,2]
    establish[i,1] = row                                #Row
    establish[i,2] = col
    establish[i,3]  = P_estab.Potr[row, col]      #Potr
    establish[i,4]  = P_estab.Pipo_Psme.C[row, col]      #Cali Coni
    establish[i,5]  = P_estab.Psme.C[row, col]      #Psme.C
    establish[i,6]  = P_estab.Psme.I[row, col]      #Psme.I
    establish[i,7]  = P_estab.Pien[row , col]  #Pien
    establish[i,8]  = P_estab.Pial[row , col]  #Pial
    establish[i,9]  = P_estab.Thpl[row, col]      #Thpl
    establish[i,10]  = P_estab.Pico[row , col]  #Pico
    establish[i,11]  = P_estab.Abgr[row , col]  #ABGR
    establish[i,12]  = P_estab.Pied[row , col]  #Pied
    establish[i,13]  = P_estab.Pipo.N[row , col]  #Pipo.N
    establish[i,14]  = P_estab.Pipo.S[row , col]  #Pipo.S
    establish[i, 15] = max1(establish[i, 3:14])           #Which PFT is the highest probability
    establish[i, 16] = max2(establish[i, 3:14])           #which PFT is the second highest probability
    establish[i, 17] = max3(establish[i, 3:14])           #whih PFT is the third highest probability
    establish[i, 18] = max4(establish[i, 3:14])          #whih PFT is the 4th highest probability
    establish[i, 19] = max5(establish[i, 3:14])           #whih PFT is the 5th highest probability
    establish[i, 20] = max6(establish[i, 3:14])           #whih PFT is the 6th highest probability
    establish[i, 21] = max7(establish[i, 3:14])           #whih PFT is the 7th highest probability
    establish[i, 22] = max8(establish[i, 3:14])           #whih PFT is the 8th highest probability
    establish[i, 23] = max9(establish[i, 3:14])           #whih PFT is the 9th highest probability
    establish[i, 24] = max10(establish[i, 3:14])           #whih PFT is the 10th highest probability
    establish[i, 25] = max11(establish[i, 3:14])           #whih PFT is the 11th highest probability
    establish[i, 26] = max12(establish[i, 3:14])           #whih PFT is the 12th highest probability
    
  }
  
establish[,27] = ifelse(establish[, 15] == 1 & establish[,3] > round(runif(nrow(establish),0,1), digits=2),1,
                   ifelse(establish[, 15] == 2 & establish[, 4] > round(runif(nrow(establish),0,1), digits=2),2,
                   ifelse(establish[, 15] == 3 & establish[, 5] > round(runif(nrow(establish),0,1), digits=2),3,
                   ifelse(establish[, 15] == 4 & establish[, 6] > round(runif(nrow(establish),0,1), digits=2),4,
                   ifelse(establish[, 15] == 5 & establish[, 7] > round(runif(nrow(establish),0,1), digits=2),5,
                   ifelse(establish[, 15] == 6 & establish[,8] > round(runif(nrow(establish),0,1), digits=2),6,
                   ifelse(establish[, 15] == 7 & establish[, 9] > round(runif(nrow(establish),0,1), digits=2),7,
                   ifelse(establish[, 15] == 8 & establish[, 10] > round(runif(nrow(establish),0,1), digits=2),8,
                   ifelse(establish[, 15] == 9 & establish[, 11] > round(runif(nrow(establish),0,1), digits=2),9,
                   ifelse(establish[, 15] == 10 & establish[, 12] > round(runif(nrow(establish),0,1), digits=2),10,
                   ifelse(establish[, 15] == 11 & establish[, 13] > round(runif(nrow(establish),0,1), digits=2),11,
                   ifelse(establish[, 15] == 12 & establish[, 14] > round(runif(nrow(establish),0,1), digits=2),12,0))))))))))))
                       
 establish[,27] = ifelse(establish[, 27] > 0,establish[,27],
                  ifelse(establish[, 16] == 1 & establish[,3] > round(runif(nrow(establish),0,1), digits=2),1,
                  ifelse(establish[, 16] == 2 & establish[, 4] > round(runif(nrow(establish),0,1), digits=2),2,
                  ifelse(establish[, 16] == 3 & establish[, 5] > round(runif(nrow(establish),0,1), digits=2),3,
                  ifelse(establish[, 16] == 4 & establish[, 6] > round(runif(nrow(establish),0,1), digits=2),4,
                  ifelse(establish[, 16] == 5 & establish[, 7] > round(runif(nrow(establish),0,1), digits=2),5,
                  ifelse(establish[, 16] == 6 & establish[,8] > round(runif(nrow(establish),0,1), digits=2),6,
                  ifelse(establish[, 16] == 7 & establish[, 9] > round(runif(nrow(establish),0,1), digits=2),7,
                  ifelse(establish[, 16] == 8 & establish[, 10] > round(runif(nrow(establish),0,1), digits=2),8,
                  ifelse(establish[, 16] == 9 & establish[, 11] > round(runif(nrow(establish),0,1), digits=2),9,
                  ifelse(establish[, 16] == 10 & establish[, 12] > round(runif(nrow(establish),0,1), digits=2),10,
                  ifelse(establish[, 16] == 11 & establish[, 13] > round(runif(nrow(establish),0,1), digits=2),11,
                  ifelse(establish[, 16] == 12 & establish[, 14] > round(runif(nrow(establish),0,1), digits=2),12,0)))))))))))))

 establish[,27] = ifelse(establish[, 27] > 0,establish[,27],
                  ifelse(establish[, 17] == 1 & establish[,3] > round(runif(nrow(establish),0,1), digits=2),1,
                  ifelse(establish[, 17] == 2 & establish[, 4] > round(runif(nrow(establish),0,1), digits=2),2,
                  ifelse(establish[, 17] == 3 & establish[, 5] > round(runif(nrow(establish),0,1), digits=2),3,
                  ifelse(establish[, 17] == 4 & establish[, 6] > round(runif(nrow(establish),0,1), digits=2),4,
                  ifelse(establish[, 17] == 5 & establish[, 7] > round(runif(nrow(establish),0,1), digits=2),5,
                   ifelse(establish[, 17] == 6 & establish[,8] > round(runif(nrow(establish),0,1), digits=2),6,
                   ifelse(establish[, 17] == 7 & establish[, 9] > round(runif(nrow(establish),0,1), digits=2),7,
                   ifelse(establish[, 17] == 8 & establish[, 10] > round(runif(nrow(establish),0,1), digits=2),8,
                   ifelse(establish[, 17] == 9 & establish[, 11] > round(runif(nrow(establish),0,1), digits=2),9,
                   ifelse(establish[, 17] == 10 & establish[, 12] > round(runif(nrow(establish),0,1), digits=2),10,
                   ifelse(establish[, 17] == 11 & establish[, 13] > round(runif(nrow(establish),0,1), digits=2),11,
                   ifelse(establish[, 17] == 12 & establish[, 14] > round(runif(nrow(establish),0,1), digits=2),12,0)))))))))))))               
                                           
  establish[,27] = ifelse(establish[, 27] > 0,establish[,27],
                   ifelse(establish[, 18] == 1 & establish[,3] > round(runif(nrow(establish),0,1), digits=2),1,
                   ifelse(establish[, 18] == 2 & establish[, 4] > round(runif(nrow(establish),0,1), digits=2),2,
                   ifelse(establish[, 18] == 3 & establish[, 5] > round(runif(nrow(establish),0,1), digits=2),3,
                   ifelse(establish[, 18] == 4 & establish[, 6] > round(runif(nrow(establish),0,1), digits=2),4,
                   ifelse(establish[, 18] == 5 & establish[, 7] > round(runif(nrow(establish),0,1), digits=2),5,
                   ifelse(establish[, 18] == 6 & establish[,8] > round(runif(nrow(establish),0,1), digits=2),6,
                   ifelse(establish[, 18] == 7 & establish[, 9] > round(runif(nrow(establish),0,1), digits=2),7,
                   ifelse(establish[, 18] == 8 & establish[, 10] > round(runif(nrow(establish),0,1), digits=2),8,
                   ifelse(establish[, 18] == 9 & establish[, 11] > round(runif(nrow(establish),0,1), digits=2),9,
                   ifelse(establish[, 18] == 10 & establish[, 12] > round(runif(nrow(establish),0,1), digits=2),10,
                   ifelse(establish[, 18] == 11 & establish[, 13] > round(runif(nrow(establish),0,1), digits=2),11,
                   ifelse(establish[, 18] == 12 & establish[, 14] > round(runif(nrow(establish),0,1), digits=2),12,0)))))))))))))
                                                  
 establish[,27] =  ifelse(establish[, 27] > 0,establish[,27],
                   ifelse(establish[, 19] == 1 & establish[,3] > round(runif(nrow(establish),0,1), digits=2),1,
                   ifelse(establish[, 19] == 2 & establish[, 4] > round(runif(nrow(establish),0,1), digits=2),2,
                   ifelse(establish[, 19] == 3 & establish[, 5] > round(runif(nrow(establish),0,1), digits=2),3,
                   ifelse(establish[, 19] == 4 & establish[, 6] > round(runif(nrow(establish),0,1), digits=2),4,
                   ifelse(establish[, 19] == 5 & establish[, 7] > round(runif(nrow(establish),0,1), digits=2),5,
                   ifelse(establish[, 19] == 6 & establish[,8] > round(runif(nrow(establish),0,1), digits=2),6,
                   ifelse(establish[, 19] == 7 & establish[, 9] > round(runif(nrow(establish),0,1), digits=2),7,
                   ifelse(establish[, 19] == 8 & establish[, 10] > round(runif(nrow(establish),0,1), digits=2),8,
                   ifelse(establish[, 19] == 9 & establish[, 11] > round(runif(nrow(establish),0,1), digits=2),9,
                   ifelse(establish[, 19] == 10 & establish[, 12] > round(runif(nrow(establish),0,1), digits=2),10,
                   ifelse(establish[, 19] == 11 & establish[, 13] > round(runif(nrow(establish),0,1), digits=2),11,
                   ifelse(establish[, 19] == 12 & establish[, 14] > round(runif(nrow(establish),0,1), digits=2),12,0)))))))))))))
                                                          
 establish[,27] =  ifelse(establish[, 27] > 0,establish[,27],
                   ifelse(establish[, 20] == 1 & establish[,3] > round(runif(nrow(establish),0,1), digits=2),1,
                   ifelse(establish[, 20] == 2 & establish[, 4] > round(runif(nrow(establish),0,1), digits=2),2,
                   ifelse(establish[, 20] == 3 & establish[, 5] > round(runif(nrow(establish),0,1), digits=2),3,
                   ifelse(establish[, 20] == 4 & establish[, 6] > round(runif(nrow(establish),0,1), digits=2),4,
                   ifelse(establish[, 20] == 5 & establish[, 7] > round(runif(nrow(establish),0,1), digits=2),5,
                   ifelse(establish[, 20] == 6 & establish[,8] > round(runif(nrow(establish),0,1), digits=2),6,
                   ifelse(establish[, 20] == 7 & establish[, 9] > round(runif(nrow(establish),0,1), digits=2),7,
                   ifelse(establish[, 20] == 8 & establish[, 10] > round(runif(nrow(establish),0,1), digits=2),8,
                   ifelse(establish[, 20] == 9 & establish[, 11] > round(runif(nrow(establish),0,1), digits=2),9,
                   ifelse(establish[, 20] == 10 & establish[, 12] > round(runif(nrow(establish),0,1), digits=2),10,
                   ifelse(establish[, 20] == 11 & establish[, 13] > round(runif(nrow(establish),0,1), digits=2),11,
                   ifelse(establish[, 20] == 12 & establish[, 14] > round(runif(nrow(establish),0,1), digits=2),12, 0)))))))))))))
                                                                 
 establish[,27] =  ifelse(establish[, 27] > 0,establish[,27],
                   ifelse(establish[, 21] == 1 & establish[,3] > round(runif(nrow(establish),0,1), digits=2),1,
                   ifelse(establish[, 21] == 2 & establish[, 4] > round(runif(nrow(establish),0,1), digits=2),2,
                   ifelse(establish[, 21] == 3 & establish[, 5] > round(runif(nrow(establish),0,1), digits=2),3,
                   ifelse(establish[, 21] == 4 & establish[, 6] > round(runif(nrow(establish),0,1), digits=2),4,
                   ifelse(establish[, 21] == 5 & establish[, 7] > round(runif(nrow(establish),0,1), digits=2),5,
                   ifelse(establish[, 21] == 6 & establish[,8] > round(runif(nrow(establish),0,1), digits=2),6,
                   ifelse(establish[, 21] == 7 & establish[, 9] > round(runif(nrow(establish),0,1), digits=2),7,
                   ifelse(establish[, 21] == 8 & establish[, 10] > round(runif(nrow(establish),0,1), digits=2),8,
                   ifelse(establish[, 21] == 9 & establish[, 11] > round(runif(nrow(establish),0,1), digits=2),9,
                   ifelse(establish[, 21] == 10 & establish[, 12] > round(runif(nrow(establish),0,1), digits=2),10,
                   ifelse(establish[, 21] == 11 & establish[, 13] > round(runif(nrow(establish),0,1), digits=2),11,
                   ifelse(establish[, 21] == 12 & establish[, 14] > round(runif(nrow(establish),0,1), digits=2),12,0)))))))))))))
                                                                        
 establish[,27] =  ifelse(establish[, 27] > 0,establish[,27],
                   ifelse(establish[, 22] == 1 & establish[,3] > round(runif(nrow(establish),0,1), digits=2),1,
                   ifelse(establish[, 22] == 2 & establish[, 4] > round(runif(nrow(establish),0,1), digits=2),2,
                   ifelse(establish[, 22] == 3 & establish[, 5] > round(runif(nrow(establish),0,1), digits=2),3,
                   ifelse(establish[, 22] == 4 & establish[, 6] > round(runif(nrow(establish),0,1), digits=2),4,
                   ifelse(establish[, 22] == 5 & establish[, 7] > round(runif(nrow(establish),0,1), digits=2),5,
                   ifelse(establish[, 22] == 6 & establish[,8] > round(runif(nrow(establish),0,1), digits=2),6,
                   ifelse(establish[, 22] == 7 & establish[, 9] > round(runif(nrow(establish),0,1), digits=2),7,
                   ifelse(establish[, 22] == 8 & establish[, 10] > round(runif(nrow(establish),0,1), digits=2),8,
                   ifelse(establish[, 22] == 9 & establish[, 11] > round(runif(nrow(establish),0,1), digits=2),9,
                   ifelse(establish[, 22] == 10 & establish[, 12] > round(runif(nrow(establish),0,1), digits=2),10,
                   ifelse(establish[, 22] == 11 & establish[, 13] > round(runif(nrow(establish),0,1), digits=2),11,
                   ifelse(establish[, 22] == 12 & establish[, 14] > round(runif(nrow(establish),0,1), digits=2),12,0)))))))))))))
 
establish[,27] =   ifelse(establish[, 27] > 0,establish[,27],
                   ifelse(establish[, 23] == 1 & establish[,3] > round(runif(nrow(establish),0,1), digits=2),1,
                   ifelse(establish[, 23] == 2 & establish[, 4] > round(runif(nrow(establish),0,1), digits=2),2,
                   ifelse(establish[, 23] == 3 & establish[, 5] > round(runif(nrow(establish),0,1), digits=2),3,
                   ifelse(establish[, 23] == 4 & establish[, 6] > round(runif(nrow(establish),0,1), digits=2),4,
                   ifelse(establish[, 23] == 5 & establish[, 7] > round(runif(nrow(establish),0,1), digits=2),5,
                   ifelse(establish[, 23] == 6 & establish[,8] > round(runif(nrow(establish),0,1), digits=2),6,
                   ifelse(establish[, 23] == 7 & establish[, 9] > round(runif(nrow(establish),0,1), digits=2),7,
                   ifelse(establish[, 23] == 8 & establish[, 10] > round(runif(nrow(establish),0,1), digits=2),8,
                   ifelse(establish[, 23] == 9 & establish[, 11] > round(runif(nrow(establish),0,1), digits=2),9,
                   ifelse(establish[, 23] == 10 & establish[, 12] > round(runif(nrow(establish),0,1), digits=2),10,
                   ifelse(establish[, 23] == 11 & establish[, 13] > round(runif(nrow(establish),0,1), digits=2),11,
                   ifelse(establish[, 23] == 12 & establish[, 14] > round(runif(nrow(establish),0,1), digits=2),12,0)))))))))))))

establish[,27] =   ifelse(establish[, 27] > 0,establish[,27],
                   ifelse(establish[, 24] == 1 & establish[,3] > round(runif(nrow(establish),0,1), digits=2),1,
                   ifelse(establish[, 24] == 2 & establish[, 4] > round(runif(nrow(establish),0,1), digits=2),2,
                   ifelse(establish[, 24] == 3 & establish[, 5] > round(runif(nrow(establish),0,1), digits=2),3,
                   ifelse(establish[, 24] == 4 & establish[, 6] > round(runif(nrow(establish),0,1), digits=2),4,
                   ifelse(establish[, 24] == 5 & establish[, 7] > round(runif(nrow(establish),0,1), digits=2),5,
                   ifelse(establish[, 24] == 6 & establish[,8] > round(runif(nrow(establish),0,1), digits=2),6,
                   ifelse(establish[, 24] == 7 & establish[, 9] > round(runif(nrow(establish),0,1), digits=2),7,
                   ifelse(establish[, 24] == 8 & establish[, 10] > round(runif(nrow(establish),0,1), digits=2),8,
                   ifelse(establish[, 24] == 9 & establish[, 11] > round(runif(nrow(establish),0,1), digits=2),9,
                   ifelse(establish[, 24] == 10 & establish[, 12] > round(runif(nrow(establish),0,1), digits=2),10,
                   ifelse(establish[, 24] == 11 & establish[, 13] > round(runif(nrow(establish),0,1), digits=2),11,
                   ifelse(establish[, 24] == 12 & establish[, 14] > round(runif(nrow(establish),0,1), digits=2),12,0)))))))))))))

  establish[,27] = ifelse(establish[, 27] > 0,establish[,27],
                   ifelse(establish[, 25] == 1 & establish[,3] > round(runif(nrow(establish),0,1), digits=2),1,
                   ifelse(establish[, 25] == 2 & establish[, 4] > round(runif(nrow(establish),0,1), digits=2),2,
                   ifelse(establish[, 25] == 3 & establish[, 5] > round(runif(nrow(establish),0,1), digits=2),3,
                   ifelse(establish[, 25] == 4 & establish[, 6] > round(runif(nrow(establish),0,1), digits=2),4,
                   ifelse(establish[, 25] == 5 & establish[, 7] > round(runif(nrow(establish),0,1), digits=2),5,
                   ifelse(establish[, 25] == 6 & establish[,8] > round(runif(nrow(establish),0,1), digits=2),6,
                   ifelse(establish[, 25] == 7 & establish[, 9] > round(runif(nrow(establish),0,1), digits=2),7,
                   ifelse(establish[, 25] == 8 & establish[, 10] > round(runif(nrow(establish),0,1), digits=2),8,
                   ifelse(establish[, 25] == 9 & establish[, 11] > round(runif(nrow(establish),0,1), digits=2),9,
                   ifelse(establish[, 25] == 10 & establish[, 12] > round(runif(nrow(establish),0,1), digits=2),10,
                   ifelse(establish[, 25] == 11 & establish[, 13] > round(runif(nrow(establish),0,1), digits=2),11,
                   ifelse(establish[, 25] == 12 & establish[, 14] > round(runif(nrow(establish),0,1), digits=2),12,0)))))))))))))
  
  establish[,27] = ifelse(establish[, 27] > 0,establish[,27],
                   ifelse(establish[, 26] == 1 & establish[,3] > round(runif(nrow(establish),0,1), digits=2),1,
                   ifelse(establish[, 26] == 2 & establish[, 4] > round(runif(nrow(establish),0,1), digits=2),2,
                   ifelse(establish[, 26] == 3 & establish[, 5] > round(runif(nrow(establish),0,1), digits=2),3,
                   ifelse(establish[, 26] == 4 & establish[, 6] > round(runif(nrow(establish),0,1), digits=2),4,
                   ifelse(establish[, 26] == 5 & establish[, 7] > round(runif(nrow(establish),0,1), digits=2),5,
                   ifelse(establish[, 26] == 6 & establish[,8] > round(runif(nrow(establish),0,1), digits=2),6,
                   ifelse(establish[, 26] == 7 & establish[, 9] > round(runif(nrow(establish),0,1), digits=2),7,
                   ifelse(establish[, 26] == 8 & establish[, 10] > round(runif(nrow(establish),0,1), digits=2),8,
                   ifelse(establish[, 26] == 9 & establish[, 11] > round(runif(nrow(establish),0,1), digits=2),9,
                   ifelse(establish[, 26] == 10 & establish[, 12] > round(runif(nrow(establish),0,1), digits=2),10,
                   ifelse(establish[, 26] == 11 & establish[, 13] > round(runif(nrow(establish),0,1), digits=2),11,
                   ifelse(establish[, 26] == 12 & establish[, 14] > round(runif(nrow(establish),0,1), digits=2),12,0)))))))))))))
  
    establish[, 28] = ifelse(establish[,27] ==0,0,1) 
  establish[establish[,27] == 0] = NA
  establish = na.omit(establish)
  pft=list.pft[[t]]
  pft[establish[,c(1,2)]] <- as.double(establish[,27])
  establishment=list.e[[1]]
  establishment[establish[,c(1,2)]] <- as.double(establish[,28])
  return(list(pft,establishment,establish))
  
  
}

# fire = list.fire[[t-1]]
# aridity = list.sm[[t-1]]
# aridity.max = sm.max
# litter.total = list.litter[[t-1]]
# cwd.total = list.cwd[[t-1]]
# dbh = list.DBH[[t-1]]
# pft = list.pft[[t-1]]

#Available fuel loads and fire severity
fire.severity.calc = function(fire,aridity,aridity.max,litter.total,cwd.total,dbh,pft){
  av.litter = ifelse(fire>=1,(fire_kfc1+fire_kfc2*(1-(aridity/aridity.max)))*(litter.total/1000),NA) # We are just using soil moisture here. litter and coarse wood are divided by 1000 to put it in metric tons per ha.
  av.cwd = ifelse(fire>=1,(fire_kfc3*(1-(0.7*aridity/aridity.max)))*(cwd.total/1000),NA)
  av.fuel = av.litter+av.cwd
  ck = ifelse(fire>=1 & dbh < dbhthreshold,100*(fire_kck1+fire_kck2*dbh)*av.fuel,
              ifelse(fire>=1 & dbh>=dbhthreshold,100*(fire_kck1+fire_kck2*dbhthreshold)*av.fuel,NA))
  ck=ifelse(ck>100,100,ck)
  ck=ifelse(ck<0,0,ck)
  
 prob.mort = round(ifelse(fire >= 1 & pft == 1, 1/(1+e^(-1.466+1.91*(bark.thickness_Potr*dbh) - 0.1775*(bark.thickness_Potr*dbh)*(bark.thickness_Potr*dbh)-0.000541*ck*ck)),
                     ifelse(fire >= 1 & pft == 2, 1/(1+e^(-1.466+1.91*(bark.thickness_Pipo_Psme.C *dbh) - 0.1775*(bark.thickness_Pipo_Psme.C*dbh)*(bark.thickness_Pipo_Psme.C*dbh)-0.000541*ck*ck)),
                     ifelse(fire >= 1 & pft == 3,1/(1+e^(-1.466+1.91*(bark.thickness_Psme.C*dbh) - 0.1775*(bark.thickness_Psme.C*dbh)*(bark.thickness_Psme.C*dbh)-0.000541*ck*ck)),
                     ifelse(fire >= 1 & pft == 4, 1/(1+e^(-1.466+1.91*(bark.thickness_Psme.I*dbh) - 0.1775*(bark.thickness_Psme.I*dbh)*(bark.thickness_Psme.I*dbh)-0.000541*ck*ck)),
                     ifelse(fire >= 1 & pft == 5, 1/(1+e^(-1.466+1.91*(bark.thickness_Pien*dbh) - 0.1775*(bark.thickness_Pien*dbh)*(bark.thickness_Pien*dbh)-0.000541*ck*ck)),
                     ifelse(fire >= 1 & pft == 6, 1/(1+e^(-1.466+1.91*(bark.thickness_Pial*dbh) - 0.1775*(bark.thickness_Pial*dbh)*(bark.thickness_Pial*dbh)-0.000541*ck*ck)),
                     ifelse(fire >= 1 & pft == 7, 1/(1+e^(-1.466+1.91*(bark.thickness_Thpl*dbh) - 0.1775*(bark.thickness_Thpl*dbh)*(bark.thickness_Thpl*dbh)-0.000541*ck*ck)),
                     ifelse(fire >= 1 & pft == 8, 1/(1+e^(-1.466+1.91*(bark.thickness_Pico*dbh) - 0.1775*(bark.thickness_Pico*dbh)*(bark.thickness_Pico*dbh)-0.000541*ck*ck)),
                     ifelse(fire >= 1 & pft == 9, 1/(1+e^(-1.466+1.91*(bark.thickness_Abgr*dbh) - 0.1775*(bark.thickness_Abgr*dbh)*(bark.thickness_Abgr*dbh)-0.000541*ck*ck)),
                     ifelse(fire >= 1 & pft == 10,1/(1+e^(-1.466+1.91*(bark.thickness_Pied*dbh) - 0.1775*(bark.thickness_Pied*dbh)*(bark.thickness_Pied*dbh)-0.000541*ck*ck)),
                     ifelse(fire >= 1 & pft == 11, 1/(1+e^(-1.466+1.91*(bark.thickness_Pipo.N*dbh) - 0.1775*(bark.thickness_Pipo.N*dbh)*(bark.thickness_Pipo.N*dbh)-0.000541*ck*ck)),
                     ifelse(fire >= 1 & pft == 12, 1/(1+e^(-1.466+1.91*(bark.thickness_Pipo.S*dbh) - 0.1775*(bark.thickness_Pipo.S*dbh)*(bark.thickness_Pipo.S*dbh)-0.000541*ck*ck)), 0)))))))))))),digits=2)

  return(list(av.litter*1000,av.cwd*1000,ck,prob.mort)) #This is multiplied by 1000 to convert back to kg per ha from metric tons per ha
}

#Calculate dead
dead.calc = function(dead.past, background, stress, fire.sev, established.past){
  x=list.dead[[1]]
  x = ifelse(fire.sev >= 0.95, 1, 0)
  l=matrix(runif(ncell(background),0,1),nrow= nrow(background), ncol=ncol(background)) #These probabilities are used in the next line
  b=list.dead[[1]]
  b=ifelse(background > l,1,0)
  s=list.dead[[1]]
  s = ifelse(b==1 | stress > stress.threshold | x == 1,1,0)  #s = did it die this last year?
  s2=list.dead[[1]]
  s2 = ifelse(s==1|dead.past==0, 0,
              ifelse(dead.past ==0, 0,
                     ifelse(dead.past>0 & established.past == 0, dead.past+1, 0)))#s2 is calculating was it dead previously and if so did establishment occur
  dead=list.dead[[1]]
  dead = s + s2
  return(list(x,dead))
}


#Loop for writing out and dumping from memory
write.out = function(path, region){
LIVE_STAND=matrix(nrow = NROW(list.pft[[1]])*NCOL(list.pft[[1]]),ncol = 18)
LIVE_STAND[,1] = as.vector(col(list.pft[[1]]))
LIVE_STAND[,2] = as.vector(row(list.pft[[1]]))
LIVE_STAND[,3] = as.vector(list.H[[t]])
LIVE_STAND[,4] = as.vector(list.age[[t]])
LIVE_STAND[,5] = as.vector(list.DBH[[t]])
LIVE_STAND[,6] = as.vector(list.stem[[t]])
LIVE_STAND[,7] = as.vector(list.leaf[[t]])
LIVE_STAND[,8] =as.vector(list.branch[[t]])
LIVE_STAND[,9] =as.vector(list.density[[t]])
LIVE_STAND[,10] =as.vector(list.sm[[t]])
LIVE_STAND[,11] =as.vector(list.psi[[t]])
LIVE_STAND[,12] =as.vector(list.stress[[t+1]])          #calculated in March for the next season
LIVE_STAND[,13] =  as.vector(list.background[[t+1]])    #calculated in March for the next season
LIVE_STAND[,14] =as.vector(list.death_prob[[t+1]])      #calculated in March for the next season 
LIVE_STAND[,15] =as.vector(list.stressed[[t+1]])       #calculated in March for the next season
LIVE_STAND[,16] =as.vector(list.dead[[t+1]])            #calculated in March for the next season
LIVE_STAND[,17] =as.vector(list.pft[[t]])
LIVE_STAND[,18] =t
LIVE_STAND = LIVE_STAND[complete.cases(LIVE_STAND[ , 11]),]
colnames(LIVE_STAND) <- c("x", "y", "height.m", "stand_age", "DBH.cm", "stem.biomass.kg","leaf.biomass.kg","branch.biomass.kg","stand.density.ha","soil.moisture", "psi",
                            "stress", "background.prob","death_prob", "stressed","dead" , "pft","year")
LIVE_STAND <<- round(LIVE_STAND, digits=2) #!KE
if(t==1){
db.conn <<- DBI::dbConnect(RSQLite::SQLite(), dbname=path_save_db)  #!KE 
  DBI::dbWriteTable(db.conn, name="Stand", as.data.frame(LIVE_STAND),row.names=F,overwrite=T)
DBI::dbDisconnect(db.conn)
}else{
db.conn <<- DBI::dbConnect(RSQLite::SQLite(), dbname=path_save_db)  #!KE 
  DBI::dbWriteTable(db.conn, name="Stand", as.data.frame(LIVE_STAND),row.names=F,append=T)
DBI::dbDisconnect(db.conn)
}


DEAD_FUELS=matrix(nrow = NROW(list.pft[[1]])*NCOL(list.pft[[1]]),ncol = 17)
DEAD_FUELS[,1] = as.vector(col(list.pft[[1]]))
DEAD_FUELS[,2] = as.vector(row(list.pft[[1]]))
DEAD_FUELS[,3] = as.vector(list.litter[[t]])
DEAD_FUELS[,4] = as.vector(list.cwd[[t]])
DEAD_FUELS[,5] = as.vector(list.snag[[t]])
DEAD_FUELS[,6] = as.vector(list.pft[[t]])
DEAD_FUELS[,7] = as.vector(list.fire[[t]])
DEAD_FUELS[,8] = as.vector(list.avail.litter[[t+1]])
DEAD_FUELS[,9] = as.vector(list.avail.cwd[[t+1]])
DEAD_FUELS[,10] = as.vector(list.ck[[t+1]])
DEAD_FUELS[,11] = as.vector(list.fire.severity[[t+1]])    #calculated in March for the next season
DEAD_FUELS[,12] =as.vector(list.dead.fire[[t+1]])         #calculated in March for the next season
DEAD_FUELS[,13] =as.vector(list.dead[[t+1]])            #calculated in March for the next season
DEAD_FUELS[,14] =t
DEAD_FUELS[,15] = as.vector(list.psi[[t]])
DEAD_FUELS[,16] = as.vector(list.sm[[t]])
DEAD_FUELS[,17] = as.vector(sm.max)

DEAD_FUELS = DEAD_FUELS[complete.cases(DEAD_FUELS[ , 15]),]
colnames(DEAD_FUELS) <- c("x", "y", "litter.kg", "cwd.kg", "snag.kg", "pft","fire.id", "avail.litter.kg", "avail.cwd.kg", "ck", "fire.sev","fire.death","death",
                          "year","psi","sm.annual","sm.max")
DEAD_FUELS <<- round(DEAD_FUELS,digits = 2) #!KE
if(t==1){
db.conn <<- DBI::dbConnect(RSQLite::SQLite(), dbname=path_save_db)  #!KE 
  DBI::dbWriteTable(db.conn, name="Dead_fuel_fire", as.data.frame(DEAD_FUELS),row.names=F,overwrite=T)
DBI::dbDisconnect(db.conn)
}else{
db.conn <<- DBI::dbConnect(RSQLite::SQLite(), dbname=path_save_db)  #!KE 
  DBI::dbWriteTable(db.conn, name="Dead_fuel_fire", as.data.frame(DEAD_FUELS),row.names=F,append=T)
DBI::dbDisconnect(db.conn)
}







ESTAB.ABIOTIC=matrix(nrow = NROW(list.pft[[1]])*NCOL(list.pft[[1]]),ncol = 34)
ESTAB.ABIOTIC[,1] = as.vector(col(list.pft[[1]]))
ESTAB.ABIOTIC[,2] = as.vector(row(list.pft[[1]]))
ESTAB.ABIOTIC[,3] = as.vector(list.p_estab.abiotic[[t]][[1]])
ESTAB.ABIOTIC[,4] = as.vector(list.p_estab.abiotic[[t]][[2]])
ESTAB.ABIOTIC[,5] = as.vector(list.p_estab.abiotic[[t]][[3]])
ESTAB.ABIOTIC[,6] = as.vector(list.p_estab.abiotic[[t]][[4]])
ESTAB.ABIOTIC[,7] = as.vector(list.p_estab.abiotic[[t]][[5]])
ESTAB.ABIOTIC[,8] = as.vector(list.p_estab.abiotic[[t]][[6]])
ESTAB.ABIOTIC[,9] = as.vector(list.p_estab.abiotic[[t]][[7]])
ESTAB.ABIOTIC[,10] = as.vector(list.p_estab.abiotic[[t]][[8]])
ESTAB.ABIOTIC[,11] = as.vector(list.p_estab.abiotic[[t]][[9]])
ESTAB.ABIOTIC[,12] = as.vector(list.p_estab.abiotic[[t]][[10]])
ESTAB.ABIOTIC[,13] = as.vector(list.p_estab.abiotic[[t]][[11]])
ESTAB.ABIOTIC[,14] = as.vector(list.p_estab.abiotic[[t]][[12]])
ESTAB.ABIOTIC[,15] = as.vector(list.established[[t]])
ESTAB.ABIOTIC[,16] = as.vector(list.pft[[t]])
ESTAB.ABIOTIC[,17] = t
ESTAB.ABIOTIC[,18] = as.vector(list.psi[[t]])
ESTAB.ABIOTIC[,19] = as.vector(list.dead[[t+1]])
ESTAB.ABIOTIC[,20] = as.vector(list.min.temp.flag[[t]][[3]])
ESTAB.ABIOTIC[,21] = as.vector(list.GDD.flag[[t]][[3]])
ESTAB.ABIOTIC[,22] = as.vector(list.chill.day.flag[[t]][[3]])
ESTAB.ABIOTIC[,23] = as.vector(list.frost.free.flag[[t]][[3]])
ESTAB.ABIOTIC[,24] = as.vector(list.GDD_budburst.flag[[t]][[3]])
ESTAB.ABIOTIC[,25] = as.vector(list.min.temp.flag[[t]][[7]])
ESTAB.ABIOTIC[,26] = as.vector(list.GDD.flag[[t]][[7]])
ESTAB.ABIOTIC[,27] = as.vector(list.chill.day.flag[[t]][[7]])
ESTAB.ABIOTIC[,28] = as.vector(list.frost.free.flag[[t]][[7]])
ESTAB.ABIOTIC[,29] = as.vector(list.GDD_budburst.flag[[t]][[7]])
ESTAB.ABIOTIC[,30] = as.vector(list.min.temp.flag[[t]][[2]])
ESTAB.ABIOTIC[,31] = as.vector(list.GDD.flag[[t]][[2]])
ESTAB.ABIOTIC[,32] = as.vector(list.chill.day.flag[[t]][[2]])
ESTAB.ABIOTIC[,33] = as.vector(list.frost.free.flag[[t]][[2]])
ESTAB.ABIOTIC[,34] = as.vector(list.GDD_budburst.flag[[t]][[2]])




ESTAB.ABIOTIC = ESTAB.ABIOTIC[complete.cases(ESTAB.ABIOTIC[ , 16]),]

colnames(ESTAB.ABIOTIC) <- c("x", "y", "E.abiotic.Potr", "E.abiotic.Pipo_Psme.C","E.abiotic.Psme.C","E.abiotic.Psme.I","E.abiotic.Pien",
                               "E.abiotic.Pial","E.abiotic.Thpl","E.abiotic.Pico","E.abiotic.Abgr","E.abiotic.Pied","E.abiotic.Pipo.N","E.abiotic.Pipo.S","established","pft","year","psi","dead",
                               "min.temp.flag.Psme.C","GDD.flag.Psme.C","chill.day.flag.Psme.C","frost.free.flag.Psme.C","GDD_Budburst.Psme.C",
                                "min.temp.flag.Thpl","GDD.flag.Thpl","chill.day.flag.Thpl","frost.free.flag.Thpl","GDD_Budburst.Thpl",
                                 "min.temp.flag.Pipo_Psme.C","GDD.flag.Pipo_Psme.C","chill.day.flag.Pipo_Psme.C","frost.free.flag.Pipo_Psme.C","GDD_Budburst.Pipo_Psme.C")
ESTAB.ABIOTIC=round(ESTAB.ABIOTIC,digits = 2)
if(t==1){
db.conn <<- DBI::dbConnect(RSQLite::SQLite(), dbname=path_save_db)  #!KE 
  DBI::dbWriteTable(db.conn, name="ESTAB.ABIOTIC",as.data.frame(ESTAB.ABIOTIC),row.names=F,overwrite=T)
DBI::dbDisconnect(db.conn)
}else{
db.conn <<- DBI::dbConnect(RSQLite::SQLite(), dbname=path_save_db)  #!KE 
  DBI::dbWriteTable(db.conn, name="ESTAB.ABIOTIC",as.data.frame(ESTAB.ABIOTIC),row.names=F,append=T)
DBI::dbDisconnect(db.conn)

}



ESTAB.DISPERSAL=matrix(nrow = NROW(list.pft[[1]])*NCOL(list.pft[[1]]),ncol = 19)
ESTAB.DISPERSAL[,1] = as.vector(col(list.pft[[1]]))
ESTAB.DISPERSAL[,2] = as.vector(row(list.pft[[1]]))
ESTAB.DISPERSAL[,3] = as.vector(list.p_estab.dispersal[[t]][[1]])
ESTAB.DISPERSAL[,4] = as.vector(list.p_estab.dispersal[[t]][[2]])
ESTAB.DISPERSAL[,5] = as.vector(list.p_estab.dispersal[[t]][[3]])
ESTAB.DISPERSAL[,6] = as.vector(list.p_estab.dispersal[[t]][[4]])
ESTAB.DISPERSAL[,7] = as.vector(list.p_estab.dispersal[[t]][[5]])
ESTAB.DISPERSAL[,8] = as.vector(list.p_estab.dispersal[[t]][[6]])
ESTAB.DISPERSAL[,9] = as.vector(list.p_estab.dispersal[[t]][[7]])
ESTAB.DISPERSAL[,10] = as.vector(list.p_estab.dispersal[[t]][[8]])
ESTAB.DISPERSAL[,11] = as.vector(list.p_estab.dispersal[[t]][[9]])
ESTAB.DISPERSAL[,12] = as.vector(list.p_estab.dispersal[[t]][[10]])
ESTAB.DISPERSAL[,13] = as.vector(list.p_estab.dispersal[[t]][[11]])
ESTAB.DISPERSAL[,14] = as.vector(list.p_estab.dispersal[[t]][[12]])
ESTAB.DISPERSAL[,15] = as.vector(list.established[[t]])
ESTAB.DISPERSAL[,16] = as.vector(list.pft[[t]])
ESTAB.DISPERSAL[,17] = t
ESTAB.DISPERSAL[,18] = as.vector(list.psi[[t]])
ESTAB.DISPERSAL[,19] = as.vector(list.dead[[t+1]])

ESTAB.DISPERSAL = ESTAB.DISPERSAL[complete.cases(ESTAB.DISPERSAL[ , 18]),]

colnames(ESTAB.DISPERSAL) <- c("x", "y","E.dispersal.Potr", "E.dispersal.Pipo_Psme.C","E.dispersal.Psme.C","E.dispersal.Psme.I","E.dispersal.Pien",
                               "E.dispersal.Pial","E.dispersal.Thpl","E.dispersal.Pico","E.dispersal.Abgr","E.dispersal.Pied","E.dispersal.Pipo.N",
                               "E.dispersal.Pipo.S","established","pft","year","psi","dead")
if(t==1){
db.conn <<- DBI::dbConnect(RSQLite::SQLite(), dbname=path_save_db)  #!KE 
  DBI::dbWriteTable(db.conn, name="ESTAB.DISPERSAL",as.data.frame(ESTAB.DISPERSAL),row.names=F,overwrite=T)
DBI::dbDisconnect(db.conn)
}else{
db.conn <<- DBI::dbConnect(RSQLite::SQLite(), dbname=path_save_db)  #!KE 
  DBI::dbWriteTable(db.conn, name="ESTAB.DISPERSAL",as.data.frame(ESTAB.DISPERSAL),row.names=F,append=T)
DBI::dbDisconnect(db.conn)
 }

  
}


memory.dump = function(grid){
  if (t>1){
  grid[t-1] <- 0
  return(grid)
  }else{
    grid
  }
  
}

snapshot = function(year.write){  
  forest=raster(paste0(path_inputs, "forest_grid.tif")) #!KE
  x.dim=ncol(forest)+2
  y.dim=nrow(forest)+2
  db.conn <- DBI::dbConnect(RSQLite::SQLite(), dbname=path_save_db)
  dead_fuel_fire <- tbl(db.conn, "Dead_fuel_fire")
  dataset.d=dead_fuel_fire%>%filter(year==year.write)%>%dplyr::select(x,y,"snag.kg","cwd.kg","litter.kg")%>%collect()
  stand <- tbl(db.conn, "Stand")
  dataset.l=stand%>%filter(year==year.write)%>%dplyr::select(x,y,"pft","stand_age","DBH.cm","stand.density.ha", "height.m","stem.biomass.kg","leaf.biomass.kg","branch.biomass.kg","stressed","dead")%>%collect()
  DBI::dbDisconnect(db.conn)
  x <- seq(1, x.dim, by=1)
  y <- seq(1, y.dim, by=1)
  d1 <- data.frame(expand.grid(x = x, y = y))
  dataset.expand=left_join(d1,dataset.l,by=c("x","y"))%>%filter(x!=1&x!=x.dim)%>%filter(y!=1&y!=y.dim)
  dataset.expand=left_join(dataset.expand,dataset.d,by=c("x","y"))%>%filter(x!=1&x!=x.dim)%>%filter(y!=1&y!=y.dim)
  xyz <- rasterFromXYZ(dataset.expand)
  xyz=flip(xyz,direction = "y")
  extent(xyz)=extent(forest)
  crs(xyz)= crs(forest)
  origin(xyz)=origin(forest)
  
  if (file.exists(paste0(path_save, "Snapshots")) == F) { #!KE moved to path_saved, conditional simplified
    dir.create(paste0(path_save, "Snapshots")) #!KE moved to path_saved, conditional simplified
  }
  
  path_snap <- paste0(path_save, "Snapshots/", "Run-", sprintf("%03d", Run), "_year-", snapshot.year, "/") #!KE subfolder added
  
  if (file.exists(path_snap) == F) { #!KE subfolder added
    dir.create(path_snap) #!KE subfolder added
  }
  
  for(i in 1:nlayers(xyz)){
    names = names(xyz[[i]])
    writeRaster(xyz[[i]], paste0(path_snap, names, "_snapshot.tif"), overwrite=T) #!KE path
  }
}

























