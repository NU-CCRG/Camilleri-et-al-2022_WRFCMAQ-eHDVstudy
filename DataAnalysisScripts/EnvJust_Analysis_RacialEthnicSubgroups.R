library(dplyr)
library(stringr)
library(ggplot2)
library(sf)
library(openair)
library(dplyr)
library(stringr)
library(tidyverse) 
library(raster)
library(reshape2)
library(tibble)
library(RColorBrewer)
library(colorspace) 
library(vtable)


########
#Load Pop Data
########
Pop<-read.csv("/Users/sara/Documents/EDF/EnvJust/nhgis0010_ds244_20195_tract.csv")
Pop_sub <- Pop[Pop$STATE %in% c("Illinois", "Wisconsin","Indiana","Michigan"), ]

#####
#Roads
####
Roads_all <-st_read("/Users/sara/Documents/EDF/EnvJust/tl_2016_us_primaryroads/tl_2016_us_primaryroads.shp")
Roads_allsub <- Roads_all[Roads_all$MTFCC %in% c("S1100"), ]
Roads_MSA <- st_crop(Roads_allsub, xmin = -88.94215, xmax = -86.92935 , ymax = 42.66991, ymin = 40.73651)
Roads_Chicago <-st_crop(Roads_allsub, xmin = -87.94011, xmax = -87.52414 , ymax = 42.02304, ymin = 41.64454)   
Roads_Cook <-st_crop(Roads_allsub, xmin = -88.26364, xmax = -87.52416 , ymax = 42.15429, ymin = 41.46971)   
Roads_Domain <- st_crop(Roads_allsub, xmin = -90.43882 , xmax = -84.36776 , ymax = 44.18358 , ymin = 40.07649)
Roads_7CMAP <- st_crop(Roads_allsub, xmin = -88.70738 , xmax = -87.52416, ymax =42.49564 , ymin = 41.20157)

MSA_all <- st_read("/Users/sara/Documents/EDF/EnvJust/tl_2019_us_cbsa/tl_2019_us_cbsa.shp") #https://catalog.data.gov/dataset/tiger-line-shapefile-2019-nation-u-s-current-metropolitan-statistical-area-micropolitan-statist
MSA_sub <- MSA_all[MSA_all$CBSAFP %in% c("16980"), ] #https://www2.census.gov/programs-surveys/cps/methodology/2015%20Geography%20Cover.pdf
MSA_sub_trans <- st_transform(MSA_sub, crs = st_crs(Roads_allsub))

#####
#Load Chicago Shapefiles
#####
Chicago <- st_read("/Users/sara/Documents/EDF/EnvJust/Boundaries - City/geo_export_a58daedd-6479-483a-9b3a-ed2953304791.shp") #https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-City/ewy2-6yfk
plot(st_geometry(Chicago))
Chicago_trans <- st_transform(Chicago, crs = st_crs(Roads_allsub))
st_bbox(Chicago_trans)


######
#Load CMAQ 
#####
Base <- st_read("/Users/sara/Documents/EDF/EnvJust/BaseOnly_Annual_Newv2.shp") #baseline air pollution concentrations from CMAQ
Base_trans <- st_transform(Base, crs = st_crs(Roads_allsub))

AQ_Annual_allTrans <- st_read("/Users/sara/Documents/All_Trans/eAT_BaseDiff_NewWint_allPollEC_NEW.shp") #sensitivity air pollution concentrations from CMAQ
AQ_trans <- st_transform(AQ_Annual_allTrans, crs = st_crs(Roads_allsub))


########
#StartMerge Pop to AQ
#######
Pop_CensusB<-merge(AQ_trans,Pop_sub,by="GISJOIN")
rm(AQ_trans)
rm(Pop_sub)
plot(st_geometry(Pop_CensusB))

#######Use the below to extract calculated Mortality impacts from the Pop_DR_CDC dataframe from Att_Mort_Calc_IndEcon.R script
myvars <- c("Att_Mort_NO2_HEI_30p", "Att_Mort_PM2p5_HEI_30p", "Att_Mort_mda8o3_T_30p","S_30p","GISJOIN")
newdata <- Pop_DR_CDC[myvars]
newdata_NOgeom <- newdata %>% st_drop_geometry()

####Merge extracted variables to the Population and Pollution data Pop_CensusB for all the modeling domain and other spatial extents of interest e.g. Chicago MSA etc ...
Pop_CensusB_AttMort<-merge(Pop_CensusB,newdata_NOgeom,by="GISJOIN")
Pop_PollB_MSA_AttMort <- st_intersection(Pop_CensusB_AttMort, MSA_sub_trans)
Pop_PollB_Chic_AttMort <- st_intersection(Pop_CensusB_AttMort, Chicago_trans)
Pop_Cook_AttMort<- Pop_CensusB_AttMort[Pop_CensusB_AttMort$COUNTY %in% c("Cook County"), ]


#######
#This loop determines the racial and ethnic composition for each no2 difference decile 
######
list.dfs <- list(All=Pop_CensusB_AttMort,MSA=Pop_PollB_MSA_AttMort,Cook=Pop_Cook_AttMort,Chicago=Pop_PollB_Chic_AttMort)

vectMort <- c(195,196,197,198,199,200,201,202) 
vectRace<- c(2,7,105,106,107,108,109,110,111,112,113,114,199) 

new.lst <- list() # list of dataframes for pollutant columns
ave.list <- list() # list of column averages 
df2_no2diff_eHDVquant_byRaceEth <- NULL
#df2_NO2DiffAT <- NULL
df3 <- NULL
colnames(df2_no2diff_eHDVquant_byRaceEth) <- c('first','second','third','forth','fifth','sixth','seventh','eight','ninth','tenth','region')
for (i in 1:length(list.dfs)){
  print(i)
  #Pop_PercTotL= list.dfs[[i]]
  Pop_PercTotL= list.dfs[[i]] %>% mutate(across(c(ALUKE003:ALUKE012), .fns = ~./ALUKE001*100)) #Fraction of race/ethnicity within each CT compared to total population "ALUKE001"
  #q=unname(quantile(Pop_PercTotL$AttMortNO2HEI30p_RATE, probs = c(.10,.20,.30,.40,.50,.60,.70,.80, .90,1),na.rm=TRUE)) 
  q=unname(quantile(Pop_PercTotL$no2_diff, probs = c(.10,.20,.30,.40,.50,.60,.70,.80, .90,1),na.rm=TRUE)) 
  Poll_1 = Pop_PercTotL[Pop_PercTotL$no2_diff <= q[1],]
  Poll_2 = Pop_PercTotL[Pop_PercTotL$no2_diff > q[1] & Pop_PercTotL$no2_diff <= q[2],]
  Poll_3 = Pop_PercTotL[Pop_PercTotL$no2_diff > q[2] & Pop_PercTotL$no2_diff <= q[3],]
  Poll_4 = Pop_PercTotL[Pop_PercTotL$no2_diff > q[3] & Pop_PercTotL$no2_diff <= q[4],]
  Poll_5 = Pop_PercTotL[Pop_PercTotL$no2_diff > q[4] & Pop_PercTotL$no2_diff <= q[5],]
  Poll_6 = Pop_PercTotL[Pop_PercTotL$no2_diff > q[5] & Pop_PercTotL$no2_diff <= q[6],]
  Poll_7 = Pop_PercTotL[Pop_PercTotL$no2_diff > q[6] & Pop_PercTotL$no2_diff <= q[7],]
  Poll_8 = Pop_PercTotL[Pop_PercTotL$no2_diff > q[7] & Pop_PercTotL$no2_diff <= q[8],]
  Poll_9 = Pop_PercTotL[Pop_PercTotL$no2_diff > q[8] & Pop_PercTotL$no2_diff <= q[9],]
  Poll_10 = Pop_PercTotL[Pop_PercTotL$no2_diff >q[9] ,]
  
  list.dfs2 <- list(Poll_1,Poll_2,Poll_3,Poll_4,Poll_5,Poll_6,Poll_7,Poll_8,Poll_9,Poll_10)
  
  for(j in 1:length(list.dfs2)){
    print (j)
    new.lst[[j]] <- list.dfs2[[j]][, vectRace] #if I write LowL_W[, vect] I get all 3 vars CHANGE TO VECT3 for ATT_MORT
    ave.list[[j]] <- colMeans(new.lst[[j]][,c(1:13),drop=TRUE], na.rm = TRUE)
  }
  tmp <- as.data.frame.list(ave.list)
  tmp$Region <- c(names(list.dfs[i]))
  colnames(tmp) <- c('first','second','third','forth','fifth','sixth','seventh','eight','ninth','tenth','Region')
  df2_no2diff_eHDVquant_byRaceEth <- rbind(df2_no2diff_eHDVquant_byRaceEth,tmp)
  
}


#######
#Cleaning up loop output, removing unwanted variables and leaving only the racial/ethnic composition data
######
row_names_df_to_remove<-c("no2_base.x","no2_diff.x","AttMortNO2HEI30p_NEW9p4_RATE","no2_base.x1","no2_diff.x1","AttMortNO2HEI30p_NEW9p4_RATE1","no2_base.x2","no2_diff.x2","AttMortNO2HEI30p_NEW9p4_RATE2","no2_base.x3","no2_diff.x3","AttMortNO2HEI30p_NEW9p4_RATE3","no2_base.x4","no2_diff.x4","AttMortNO2HEI30p_NEW9p4_RATE4")
Subs<- df2_no2diff_eHDVquant_byRaceEth[!(row.names(df2_no2diff_eHDVquant_byRaceEth) %in% row_names_df_to_remove),]

All <- Subs[Subs$Region %in% c("All"), ]
rows2race <- c("ALUKE010","ALUKE011")
All <- All[!(row.names(All) %in% rows2race),]
sum(All$first)

Chicago <- Subs[Subs$Region %in% c("Chicago"), ] 
rows2race <- c("ALUKE0103","ALUKE0113")
Chicago <- Chicago[!(row.names(Chicago) %in% rows2race),]
sum(Chicago$first)

Cook <- Subs[Subs$Region %in% c("Cook"), ]
rows2race <- c("ALUKE0102","ALUKE0112")
Cook <- Cook[!(row.names(Cook) %in% rows2race),]
sum(Cook$first)

MSA <- Subs[Subs$Region %in% c("MSA"), ] 
rows2race <- c("ALUKE0101","ALUKE0111")
MSA <- MSA[!(row.names(MSA) %in% rows2race),]
sum(MSA$first)

CMAP7 <- Subs[Subs$Region %in% c("CMAP7"), ] 
rows2race <- c("ALUKE0104","ALUKE0114")
CMAP7 <- CMAP7[!(row.names(CMAP7) %in% rows2race),]
sum(CMAP7$first)


######
#Plotting Stacked bar plots horizontally
#####
Transpose <- t(Cook)
Transpose <- head(Transpose, -1)
Melt<-melt(as.matrix(Transpose))

Melt$value <- as.numeric(Melt$value)

ggplot(data = Melt, aes(x = Var1, y = value,fill=Var2)) + 
  geom_bar(stat='identity',position="stack") + coord_flip()+ 
  scale_fill_brewer(palette = "Paired")+ #display.brewer.all(colorblindFriendly = TRUE)
  theme_bw()

ggsave("/path/to/save/newPlot.pdf")




