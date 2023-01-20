library(dplyr)
library(stringr)
library(reshape2) #used for dcast
library(ggplot2)
library(naniar)
library(data.table) #used for setDT
library(sf)
library(openair)
library(dplyr)
library(stringr)
#library(plyr)
library(tidyverse) 

########
#Load Pop Data
########
Pop<-read.csv("/Users/sara/Documents/EDF/EnvJust/nhgis0010_ds244_20195_tract.csv")
#Pop<-read.csv("/Users/sara/Documents/Population/NHGIS_IPUMS/NoHeaders/nhgis0003_csv/nhgis0003_csv/nhgis0003_ds244_20195_tract.csv")
Pop_sub <- Pop[Pop$STATE %in% c("Illinois", "Wisconsin","Indiana","Michigan"), ]
Pop_sub_Clean<-Pop_sub %>%
  select_if(~ !all(is.na(.))) #remove the ones that are all NA (select if not all NA)

Pop_sub_Clean$COUNTYA<-str_pad(Pop_sub_Clean$COUNTYA, 3, pad = "0")
Pop_sub_Clean$STATE_COUNTY <- paste0(Pop_sub_Clean$STATEA,Pop_sub_Clean$COUNTYA)

########
#Load CMAQ Data
########
AQ_Annual_allTrans <- st_read("/Users/sara/Library/CloudStorage/OneDrive-NorthwesternUniversity/All_Trans/BaseDiff_allTrans_Annual_allPoll_EC.shp")

#####
#Roads
####
Roads_all <-st_read("/Users/sara/Documents/EDF/EnvJust/tl_2016_us_primaryroads/tl_2016_us_primaryroads.shp")
Roads_allsub <- Roads_all[Roads_all$MTFCC %in% c("S1100"), ]
AQ_Annual_allTrans_trans <- st_transform(AQ_Annual_allTrans, crs = st_crs(Roads_allsub))


########
#Load Indust. Economic Data
#######
Data.in <- read.csv("/Users/sara/Documents/Health_data/IndustrialEconomic/BenMAP_Ready_Tract_Incidence_CountyRatio_Edited.csv", sep=",", header=T)
sapply(Data.in,class)
class(Data.in)

Data.in$TractFIPS<-str_pad(Data.in$TractFIPS, 6, pad = "0") #uses stringr package
Data.in$StateCountyFIPS<-str_pad(Data.in$StateCountyFIPS, 5, pad = "0")
Data.in$FIPS <-str_c(Data.in$StateCountyFIPS,'',Data.in$TractFIPS) #concatenate

IndEcon_trans <- dcast(setDT(Data.in), FIPS ~ 
                            End.Age, value.var = c("Value") )

######
#Start Merge
#####
#Merge Pop to CMAQ
Pop_sub_Clean$COUNTYA<-str_pad(Pop_sub_Clean$COUNTYA, 3, pad = "0")
Pop_sub_Clean$STATE_COUNTY <- paste0(Pop_sub_Clean$STATEA,Pop_sub_Clean$COUNTYA)
Pop_Poll<-merge(AQ_Annual_allTrans,Pop_sub_Clean,by="GISJOIN")
Pop_Poll$TRACTA<-str_pad(Pop_Poll$TRACTA, 6, pad = "0")
Pop_Poll$FIPS <-str_c(Pop_Poll$STATE_COUNTY,'',Pop_Poll$TRACTA)

#Merge to IndEconomic
Pop_DR_CDC <- merge(Pop_Poll,IndEcon_trans,by="FIPS",all=FALSE) 

###
#Baseline Mort
##
#####
#MALE
Pop_DR_CDC$BM_M4under <- (Pop_DR_CDC$ALT0E003 *Pop_DR_CDC$`4`)
Pop_DR_CDC$BM_M5 <- (Pop_DR_CDC$ALT0E004 *Pop_DR_CDC$`14`)
Pop_DR_CDC$BM_M10 <- (Pop_DR_CDC$ALT0E005 *Pop_DR_CDC$`14`  )
Pop_DR_CDC$BM_M15 <- (Pop_DR_CDC$ALT0E006 *Pop_DR_CDC$`24`  )
Pop_DR_CDC$BM_M18 <- (Pop_DR_CDC$ALT0E007 *Pop_DR_CDC$`24` )
Pop_DR_CDC$BM_M20 <- (Pop_DR_CDC$ALT0E008 *Pop_DR_CDC$`24`  )
Pop_DR_CDC$BM_M21 <- (Pop_DR_CDC$ALT0E009 *Pop_DR_CDC$`24`  )
Pop_DR_CDC$BM_M22 <- (Pop_DR_CDC$ALT0E010 *Pop_DR_CDC$`24`  )
Pop_DR_CDC$BM_M25 <- (Pop_DR_CDC$ALT0E011 *Pop_DR_CDC$`34` )
Pop_DR_CDC$BM_M30 <- (Pop_DR_CDC$ALT0E012 *Pop_DR_CDC$`34`)
Pop_DR_CDC$BM_M35 <- (Pop_DR_CDC$ALT0E013 *Pop_DR_CDC$`44` )
Pop_DR_CDC$BM_M40 <- (Pop_DR_CDC$ALT0E014 *Pop_DR_CDC$`44` )
Pop_DR_CDC$BM_M45 <- (Pop_DR_CDC$ALT0E015 *Pop_DR_CDC$`54` )
Pop_DR_CDC$BM_M50 <- (Pop_DR_CDC$ALT0E016 *Pop_DR_CDC$`54`  )
Pop_DR_CDC$BM_M55 <- (Pop_DR_CDC$ALT0E017 *Pop_DR_CDC$`64` )
Pop_DR_CDC$BM_M60 <- (Pop_DR_CDC$ALT0E018 *Pop_DR_CDC$`64` )
Pop_DR_CDC$BM_M62 <- (Pop_DR_CDC$ALT0E019 *Pop_DR_CDC$`64` )
Pop_DR_CDC$BM_M65 <- (Pop_DR_CDC$ALT0E020 *Pop_DR_CDC$`74`)
Pop_DR_CDC$BM_M67 <- (Pop_DR_CDC$ALT0E021 *Pop_DR_CDC$`74`)
Pop_DR_CDC$BM_M70 <- (Pop_DR_CDC$ALT0E022 *Pop_DR_CDC$`74`)
Pop_DR_CDC$BM_M75 <- (Pop_DR_CDC$ALT0E023 *Pop_DR_CDC$`84` )
Pop_DR_CDC$BM_M80 <- (Pop_DR_CDC$ALT0E024 *Pop_DR_CDC$`84`)
Pop_DR_CDC$BM_M85 <- (Pop_DR_CDC$ALT0E025 *Pop_DR_CDC$`99`  )

#FEMALE
Pop_DR_CDC$BM_F4under <- (Pop_DR_CDC$ALT0E027 *Pop_DR_CDC$`4`)
Pop_DR_CDC$BM_F5 <- (Pop_DR_CDC$ALT0E028 *Pop_DR_CDC$`14`)
Pop_DR_CDC$BM_F10 <- (Pop_DR_CDC$ALT0E029 *Pop_DR_CDC$`14` )
Pop_DR_CDC$BM_F15 <- (Pop_DR_CDC$ALT0E030 *Pop_DR_CDC$`24`  )
Pop_DR_CDC$BM_F18 <- (Pop_DR_CDC$ALT0E031 *Pop_DR_CDC$`24`  )
Pop_DR_CDC$BM_F20 <- (Pop_DR_CDC$ALT0E032 *Pop_DR_CDC$`24` )
Pop_DR_CDC$BM_F21 <- (Pop_DR_CDC$ALT0E033 *Pop_DR_CDC$`24`  )
Pop_DR_CDC$BM_F22 <- (Pop_DR_CDC$ALT0E034 *Pop_DR_CDC$`24`  )
Pop_DR_CDC$BM_F25 <- (Pop_DR_CDC$ALT0E035 *Pop_DR_CDC$`34` )
Pop_DR_CDC$BM_F30 <- (Pop_DR_CDC$ALT0E036 *Pop_DR_CDC$`34`)
Pop_DR_CDC$BM_F35 <- (Pop_DR_CDC$ALT0E037 *Pop_DR_CDC$`44`)
Pop_DR_CDC$BM_F40 <- (Pop_DR_CDC$ALT0E038 *Pop_DR_CDC$`44` )
Pop_DR_CDC$BM_F45 <- (Pop_DR_CDC$ALT0E039 *Pop_DR_CDC$`54` )
Pop_DR_CDC$BM_F50 <- (Pop_DR_CDC$ALT0E040 *Pop_DR_CDC$`54`)
Pop_DR_CDC$BM_F55 <- (Pop_DR_CDC$ALT0E041 *Pop_DR_CDC$`64` )
Pop_DR_CDC$BM_F60 <- (Pop_DR_CDC$ALT0E042 *Pop_DR_CDC$`64`)
Pop_DR_CDC$BM_F62 <- (Pop_DR_CDC$ALT0E043 *Pop_DR_CDC$`64` )
Pop_DR_CDC$BM_F65 <- (Pop_DR_CDC$ALT0E044 *Pop_DR_CDC$`74`)
Pop_DR_CDC$BM_F67 <- (Pop_DR_CDC$ALT0E045 *Pop_DR_CDC$`74`)
Pop_DR_CDC$BM_F70 <- (Pop_DR_CDC$ALT0E046 *Pop_DR_CDC$`74` )
Pop_DR_CDC$BM_F75 <- (Pop_DR_CDC$ALT0E047 *Pop_DR_CDC$`84` )
Pop_DR_CDC$BM_F80 <- (Pop_DR_CDC$ALT0E048 *Pop_DR_CDC$`84`)
Pop_DR_CDC$BM_F85 <- (Pop_DR_CDC$ALT0E049 *Pop_DR_CDC$`99`  )


#####
#Population Totals
#####

#####
#eHDV_byRACE
######
Pop_DR_CDC = Pop_DR_CDC%>% mutate(POP_tot = rowSums(.[,c(24:46,48:70),drop=TRUE], na.rm = TRUE)) 
Pop_DR_CDC = Pop_DR_CDC%>% mutate(Pop_30p = rowSums(.[,c(33:46,57:70),drop=TRUE], na.rm = TRUE)) #30 over
######


######
#Baseline Mort Totals
#####


#######
#eHDV_byRACE NB THESE ARE BM TOTALS
######
Pop_DR_CDC = Pop_DR_CDC%>% mutate(S_allAges = rowSums(.[,176:221,drop=TRUE], na.rm = TRUE)) ##BM ALL AGES 0-85
Pop_DR_CDC = Pop_DR_CDC%>% mutate(S_25p = rowSums(.[,c(184:198,207:221),drop=TRUE], na.rm = TRUE)) #BM 25 over
Pop_DR_CDC = Pop_DR_CDC%>% mutate(S_30p = rowSums(.[,c(185:198,208:221),drop=TRUE], na.rm = TRUE)) #BM 30 over

sum(Pop_DR_CDC$S_30p)
Pop_DR_CDC_trans <- st_transform(Pop_DR_CDC, crs = st_crs(Roads_allsub)) 
st_bbox(Pop_DR_CDC_trans) 


#######
#Calculate the Att. Frac.
#######
Pop_DR_CDC = Pop_DR_CDC%>% mutate(AF_NO2 = (1-exp(-0.002*Pop_DR_CDC$no2_diff)))
Pop_DR_CDC = Pop_DR_CDC%>% mutate(AF_PM2p5_K = (1-exp(-0.005827*Pop_DR_CDC$pm_diff)))
Pop_DR_CDC = Pop_DR_CDC%>% mutate(AF_PM2p5_L = (1-exp(-0.014842*Pop_DR_CDC$pm_diff)))
Pop_DR_CDC = Pop_DR_CDC%>% mutate(AF_mda8O3_B = (1-exp(-0.0002613*Pop_DR_CDC$mo3_diff)))
Pop_DR_CDC = Pop_DR_CDC%>% mutate(AF_mda8O3_I = (1-exp(-0.000532*Pop_DR_CDC$mo3_diff)))

######
##New CRFs
######
#AF Diff - NO2
#####
Pop_DR_CDC = Pop_DR_CDC%>% mutate(AF_NO2_HEI = (1-exp(-0.00778*Pop_DR_CDC$no2_diff)))
Pop_DR_CDC = Pop_DR_CDC%>% mutate(AF_NO2_HEI_L = (1-exp(-0.00195*Pop_DR_CDC$no2_diff)))
Pop_DR_CDC = Pop_DR_CDC%>% mutate(AF_NO2_HEI_H = (1-exp(-0.01167*Pop_DR_CDC$no2_diff)))
#Baseline
Pop_DR_CDC = Pop_DR_CDC%>% mutate(AF_NO2B_HEI = (1-exp(-0.00778*Pop_DR_CDC$no2_base)))
Pop_DR_CDC = Pop_DR_CDC%>% mutate(AF_NO2B_HEI_L = (1-exp(-0.00195*Pop_DR_CDC$no2_base)))
Pop_DR_CDC = Pop_DR_CDC%>% mutate(AF_NO2B_HEI_H = (1-exp(-0.01167*Pop_DR_CDC$no2_base)))

######
#AF Diff - PM2p5
######
Pop_DR_CDC = Pop_DR_CDC%>% mutate(AF_PM2p5_HEI = (1-exp(-0.006*Pop_DR_CDC$pm_diff)))
Pop_DR_CDC = Pop_DR_CDC%>% mutate(AF_PM2p5_HEI_L = (1-exp(-0.002*Pop_DR_CDC$pm_diff)))
Pop_DR_CDC = Pop_DR_CDC%>% mutate(AF_PM2p5_HEI_H = (1-exp(-0.01*Pop_DR_CDC$pm_diff)))
#Baseline
Pop_DR_CDC = Pop_DR_CDC%>% mutate(AF_PM2p5B_HEI = (1-exp(-0.006*Pop_DR_CDC$pm_base)))
Pop_DR_CDC = Pop_DR_CDC%>% mutate(AF_PM2p5B_HEI_L = (1-exp(-0.002*Pop_DR_CDC$pm_base)))
Pop_DR_CDC = Pop_DR_CDC%>% mutate(AF_PM2p5B_HEI_H = (1-exp(-0.01*Pop_DR_CDC$pm_base)))

######
#AF Diff - MDA8O3
#####
Pop_DR_CDC = Pop_DR_CDC%>% mutate(AF_mda8O3_T = (1-exp(-0.002*Pop_DR_CDC$mo3_diff)))
Pop_DR_CDC = Pop_DR_CDC%>% mutate(AF_mda8O3_T_L = (1-exp(-0.001*Pop_DR_CDC$mo3_diff)))
Pop_DR_CDC = Pop_DR_CDC%>% mutate(AF_mda8O3_T_H = (1-exp(-0.004*Pop_DR_CDC$mo3_diff)))
#Baseline
Pop_DR_CDC = Pop_DR_CDC%>% mutate(AF_mda8O3B_T = (1-exp(-0.002*Pop_DR_CDC$mo3_base)))
Pop_DR_CDC = Pop_DR_CDC%>% mutate(AF_mda8O3B_T_L = (1-exp(-0.001*Pop_DR_CDC$mo3_base)))
Pop_DR_CDC = Pop_DR_CDC%>% mutate(AF_mda8O3B_T_H = (1-exp(-0.004*Pop_DR_CDC$mo3_base)))

######
#Calculate the Att. Mort.
######
Pop_DR_CDC = Pop_DR_CDC%>% mutate(Att_Mort_NO2_allAges = Pop_DR_CDC$S_allAges * Pop_DR_CDC$AF_NO2) 
Pop_DR_CDC = Pop_DR_CDC%>% mutate(Att_Mort_PM2p5_K_30p = Pop_DR_CDC$S_30p * Pop_DR_CDC$AF_PM2p5_K) 
Pop_DR_CDC = Pop_DR_CDC%>% mutate(Att_Mort_PM2p5_L_25p = Pop_DR_CDC$S_25p * Pop_DR_CDC$AF_PM2p5_L) 
Pop_DR_CDC = Pop_DR_CDC%>% mutate(Att_Mort_mda8o3_B_allAges = Pop_DR_CDC$S_allAges * Pop_DR_CDC$AF_mda8O3_B) 
Pop_DR_CDC = Pop_DR_CDC%>% mutate(Att_Mort_mda8o3_I_allAges = Pop_DR_CDC$S_allAges * Pop_DR_CDC$AF_mda8O3_I) 

######
#New CRFs
#####
Pop_DR_CDC = Pop_DR_CDC%>% mutate(Att_Mort_NO2_HEI_allAges = Pop_DR_CDC$S_allAges * Pop_DR_CDC$AF_NO2_HEI) 
Pop_DR_CDC = Pop_DR_CDC%>% mutate(Att_Mort_NO2_HEI_30p = Pop_DR_CDC$S_30p * Pop_DR_CDC$AF_NO2_HEI) 
Pop_DR_CDC = Pop_DR_CDC%>% mutate(Att_Mort_NO2_HEI_30p_L = Pop_DR_CDC$S_30p * Pop_DR_CDC$AF_NO2_HEI_L) 
Pop_DR_CDC = Pop_DR_CDC%>% mutate(Att_Mort_NO2_HEI_30p_H = Pop_DR_CDC$S_30p * Pop_DR_CDC$AF_NO2_HEI_H) 
#Baseline
Pop_DR_CDC = Pop_DR_CDC%>% mutate(Att_Mort_NO2B_HEI_30p = Pop_DR_CDC$S_30p * Pop_DR_CDC$AF_NO2B_HEI) 
Pop_DR_CDC = Pop_DR_CDC%>% mutate(Att_Mort_NO2B_HEI_30p_L = Pop_DR_CDC$S_30p * Pop_DR_CDC$AF_NO2B_HEI_L) 
Pop_DR_CDC = Pop_DR_CDC%>% mutate(Att_Mort_NO2B_HEI_30p_H = Pop_DR_CDC$S_30p * Pop_DR_CDC$AF_NO2B_HEI_H) 

Pop_DR_CDC = Pop_DR_CDC%>% mutate(Att_Mort_PM2p5_HEI_30p = Pop_DR_CDC$S_30p * Pop_DR_CDC$AF_PM2p5_HEI) 
Pop_DR_CDC = Pop_DR_CDC%>% mutate(Att_Mort_PM2p5_HEI_30p_L = Pop_DR_CDC$S_30p * Pop_DR_CDC$AF_PM2p5_HEI_L) 
Pop_DR_CDC = Pop_DR_CDC%>% mutate(Att_Mort_PM2p5_HEI_30p_H = Pop_DR_CDC$S_30p * Pop_DR_CDC$AF_PM2p5_HEI_H) 
#Baseline
Pop_DR_CDC = Pop_DR_CDC%>% mutate(Att_Mort_PM2p5B_HEI_30p = Pop_DR_CDC$S_30p * Pop_DR_CDC$AF_PM2p5B_HEI) 
Pop_DR_CDC = Pop_DR_CDC%>% mutate(Att_Mort_PM2p5B_HEI_30p_L = Pop_DR_CDC$S_30p * Pop_DR_CDC$AF_PM2p5B_HEI_L) 
Pop_DR_CDC = Pop_DR_CDC%>% mutate(Att_Mort_PM2p5B_HEI_30p_H = Pop_DR_CDC$S_30p * Pop_DR_CDC$AF_PM2p5B_HEI_H)

Pop_DR_CDC = Pop_DR_CDC%>% mutate(Att_Mort_PM2p5_HEI_allAges = Pop_DR_CDC$S_allAges * Pop_DR_CDC$AF_PM2p5_HEI) 
Pop_DR_CDC = Pop_DR_CDC%>% mutate(Att_Mort_mda8o3_T_30p = Pop_DR_CDC$S_30p * Pop_DR_CDC$AF_mda8O3_T) 
Pop_DR_CDC = Pop_DR_CDC%>% mutate(Att_Mort_mda8o3_T_30p_L = Pop_DR_CDC$S_30p * Pop_DR_CDC$AF_mda8O3_T_L) 
Pop_DR_CDC = Pop_DR_CDC%>% mutate(Att_Mort_mda8o3_T_30p_H = Pop_DR_CDC$S_30p * Pop_DR_CDC$AF_mda8O3_T_H) 
#Baseline
Pop_DR_CDC = Pop_DR_CDC%>% mutate(Att_Mort_mda8o3B_T_30p = Pop_DR_CDC$S_30p * Pop_DR_CDC$AF_mda8O3B_T) 
Pop_DR_CDC = Pop_DR_CDC%>% mutate(Att_Mort_mda8o3B_T_30p_L = Pop_DR_CDC$S_30p * Pop_DR_CDC$AF_mda8O3B_T_L) 
Pop_DR_CDC = Pop_DR_CDC%>% mutate(Att_Mort_mda8o3B_T_30p_H = Pop_DR_CDC$S_30p * Pop_DR_CDC$AF_mda8O3B_T_H) 



sum(Pop_DR_CDC$Att_Mort_NO2_allAges,na.rm=TRUE) 
sum(Pop_DR_CDC$Att_Mort_PM2p5_K_30p,na.rm=TRUE) 
sum(Pop_DR_CDC$Att_Mort_PM2p5_L_25p,na.rm=TRUE) 
sum(Pop_DR_CDC$Att_Mort_mda8o3_B_allAges,na.rm=TRUE) 
sum(Pop_DR_CDC$Att_Mort_mda8o3_I_allAges,na.rm=TRUE) 

sum(Pop_DR_CDC$Att_Mort_NO2_HEI_allAges,na.rm=TRUE) 
sum(Pop_DR_CDC$Att_Mort_NO2_HEI_30p,na.rm=TRUE) 
sum(Pop_DR_CDC$Att_Mort_PM2p5_HEI_30p,na.rm=TRUE) 
sum(Pop_DR_CDC$Att_Mort_PM2p5_HEI_allAges,na.rm=TRUE) 
sum(Pop_DR_CDC$Att_Mort_mda8o3_T_30p,na.rm=TRUE) 

#Diff - Using New Coeff
sum(Pop_DR_CDC$Att_Mort_NO2_HEI_30p,na.rm=TRUE) 
sum(Pop_DR_CDC$Att_Mort_NO2_HEI_30p_L,na.rm=TRUE) 
sum(Pop_DR_CDC$Att_Mort_NO2_HEI_30p_H,na.rm=TRUE)

sum(Pop_DR_CDC$Att_Mort_PM2p5_HEI_30p,na.rm=TRUE) 
sum(Pop_DR_CDC$Att_Mort_PM2p5_HEI_30p_L,na.rm=TRUE) 
sum(Pop_DR_CDC$Att_Mort_PM2p5_HEI_30p_H,na.rm=TRUE)

sum(Pop_DR_CDC$Att_Mort_mda8o3_T_30p,na.rm=TRUE) 
sum(Pop_DR_CDC$Att_Mort_mda8o3_T_30p_L,na.rm=TRUE) 
sum(Pop_DR_CDC$Att_Mort_mda8o3_T_30p_H,na.rm=TRUE) 


#Baseline - Using New Coeff
sum(Pop_DR_CDC$Att_Mort_NO2B_HEI_30p,na.rm=TRUE) 
sum(Pop_DR_CDC$Att_Mort_NO2B_HEI_30p_L,na.rm=TRUE) 
sum(Pop_DR_CDC$Att_Mort_NO2B_HEI_30p_H,na.rm=TRUE)

sum(Pop_DR_CDC$Att_Mort_PM2p5B_HEI_30p,na.rm=TRUE) 
sum(Pop_DR_CDC$Att_Mort_PM2p5B_HEI_30p_L,na.rm=TRUE) 
sum(Pop_DR_CDC$Att_Mort_PM2p5B_HEI_30p_H,na.rm=TRUE) 

sum(Pop_DR_CDC$Att_Mort_mda8o3B_T_30p,na.rm=TRUE) 
sum(Pop_DR_CDC$Att_Mort_mda8o3B_T_30p_L,na.rm=TRUE) 
sum(Pop_DR_CDC$Att_Mort_mda8o3B_T_30p_H,na.rm=TRUE) 

mean(Pop_DR_CDC$no2_diff)
mean(Pop_DR_CDC$AF_NO2_HEI * 100)
mean(Pop_DR_CDC$AF_NO2_HEI_L * 100)
mean(Pop_DR_CDC$AF_NO2_HEI_H * 100)
mean(Pop_DR_CDC$no2_base)
mean(Pop_DR_CDC$AF_NO2B_HEI * 100)
mean(Pop_DR_CDC$AF_NO2B_HEI_L * 100)
mean(Pop_DR_CDC$AF_NO2B_HEI_H * 100)

mean(Pop_DR_CDC$pm_diff)
mean(Pop_DR_CDC$AF_PM2p5_HEI * 100)
mean(Pop_DR_CDC$AF_PM2p5_HEI_L * 100)
mean(Pop_DR_CDC$AF_PM2p5_HEI_H * 100)
mean(Pop_DR_CDC$pm_base)
mean(Pop_DR_CDC$AF_PM2p5B_HEI * 100)
mean(Pop_DR_CDC$AF_PM2p5B_HEI_L * 100)
mean(Pop_DR_CDC$AF_PM2p5B_HEI_H * 100)

mean(Pop_DR_CDC$mo3_diff)
mean(Pop_DR_CDC$AF_mda8O3_T * 100)
mean(Pop_DR_CDC$AF_mda8O3_T_L * 100)
mean(Pop_DR_CDC$AF_mda8O3_T_H * 100)
mean(Pop_DR_CDC$mo3_base)
mean(Pop_DR_CDC$AF_mda8O3B_T * 100)
mean(Pop_DR_CDC$AF_mda8O3B_T_L * 100)
mean(Pop_DR_CDC$AF_mda8O3B_T_H * 100)

#Pop-Weighting ie Sum(Pop_Tot for each CT * Poll )/Pop_Tot
Sum_PopTot=sum(Pop_DR_CDC$POP_tot,na.rm=TRUE) 
Sum_Pop30p=sum(Pop_DR_CDC$Pop_30p,na.rm=TRUE) 

#Diff
Pop_DR_CDC = Pop_DR_CDC%>% mutate(mo3_pop = (Pop_DR_CDC$POP_tot * Pop_DR_CDC$mo3_diff)) 
Pop_DR_CDC = Pop_DR_CDC%>% mutate(no2_pop = (Pop_DR_CDC$POP_tot * Pop_DR_CDC$no2_diff)) 
Pop_DR_CDC = Pop_DR_CDC%>% mutate(pm2p5_pop = (Pop_DR_CDC$POP_tot * Pop_DR_CDC$pm_diff))

#BASELINE
Pop_DR_CDC = Pop_DR_CDC%>% mutate(mo3B_pop = (Pop_DR_CDC$POP_tot * Pop_DR_CDC$mo3_base)) 
Pop_DR_CDC = Pop_DR_CDC%>% mutate(no2B_pop = (Pop_DR_CDC$POP_tot * Pop_DR_CDC$no2_base)) 
Pop_DR_CDC = Pop_DR_CDC%>% mutate(pm2p5B_pop = (Pop_DR_CDC$POP_tot * Pop_DR_CDC$pm_base))



#PopW Diff
sum(Pop_DR_CDC$mo3_pop/Sum_PopTot)
sum(Pop_DR_CDC$no2_pop/Sum_PopTot)
sum(Pop_DR_CDC$pm2p5_pop/Sum_PopTot)

#CT mean BASELINE
mean(Pop_DR_CDC$mo3_base)
mean(Pop_DR_CDC$no2_base)
mean(Pop_DR_CDC$pm_base)

#PopW BASELINE
sum(Pop_DR_CDC$mo3B_pop/Sum_PopTot)
sum(Pop_DR_CDC$no2B_pop/Sum_PopTot)
sum(Pop_DR_CDC$pm2p5B_pop/Sum_PopTot)


