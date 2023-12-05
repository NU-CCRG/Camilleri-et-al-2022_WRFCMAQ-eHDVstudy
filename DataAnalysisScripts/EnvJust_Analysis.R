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
library(colorspace) #https://cran.r-project.org/web/packages/colorspace/colorspace.pdf
library(vtable)
########
#Load Pop Data
########
#Pop<-read.csv("/Users/sara/Documents/EDF/EnvJust/nhgis0006_ds239_20185_tract.csv") #No hispanic
Pop<-read.csv("/Users/sara/Documents/EDF/EnvJust/nhgis0010_ds244_20195_tract.csv")
Pop_sub <- Pop[Pop$STATE %in% c("Illinois", "Wisconsin","Indiana","Michigan"), ]

#####
#Roads
####
Roads_all <-st_read("/Users/sara/Documents/EDF/EnvJust/tl_2016_us_primaryroads/tl_2016_us_primaryroads.shp")
Roads_allsub <- Roads_all[Roads_all$MTFCC %in% c("S1100"), ]
#st_bbox(MSA_sub)
Roads_MSA <- st_crop(Roads_allsub, xmin = -88.94215, xmax = -86.92935 , ymax = 42.66991, ymin = 40.73651)
Roads_Chicago <-st_crop(Roads_allsub, xmin = -87.94011, xmax = -87.52414 , ymax = 42.02304, ymin = 41.64454)   
Roads_Cook <-st_crop(Roads_allsub, xmin = -88.26364, xmax = -87.52416 , ymax = 42.15429, ymin = 41.46971)   
Roads_Domain <- st_crop(Roads_allsub, xmin = -90.43882 , xmax = -84.36776 , ymax = 44.18358 , ymin = 40.07649)

#####
#Load MSA Shapefiles
#####
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
Base <- st_read("/Users/sara/Documents/EDF/EnvJust/BaseOnly_Annual_Newv2.shp")
crs(Base)
Base_trans <- st_transform(Base, crs = st_crs(Roads_allsub))

AQ_Ann_eHDV <- st_read("/Users/sara/Documents/EDF/EnvJust/eHDV/BaseDiff_Annual_allPoll_EC.shp")
AQ_trans <- st_transform(AQ_Ann_eHDV, crs = st_crs(Roads_allsub))
st_crs(AQ_trans) == st_crs(Roads_allsub) #TRUE
plot(st_geometry(AQ_trans))


########
#StartMerge Pop to AQ
#######
Pop_CensusB<-merge(AQ_trans,Pop_sub,by="GISJOIN")
plot(st_geometry(Pop_CensusB))


#MSA
Pop_PollB_MSA <- st_intersection(Pop_CensusB, MSA_sub_trans)
#Chicago
Pop_PollB_Chic <- st_intersection(Pop_CensusB, Chicago_trans)
#Cook
Pop_Cook<- Pop_CensusB[Pop_CensusB$COUNTY %in% c("Cook County"), ]

#######Use the below with Pop_DR_CDC from Att_Mort_Calc_IndEcon.R
#myvars <- c("Att_Mort_NO2_HEI_30p", "Att_Mort_PM2p5_HEI_30p", "Att_Mort_mda8o3_T_30p","GISJOIN")
##NV S_30p is Baseline_Mort 30p
myvars <- c("Att_Mort_NO2_HEI_30p", "Att_Mort_PM2p5_HEI_30p", "Att_Mort_mda8o3_T_30p","S_30p","GISJOIN")
newdata <- Pop_DR_CDC[myvars]
newdata_NOgeom <- newdata %>% st_drop_geometry()
#####Use the above with Pop_DR_CDC from Att_Mort_Calc_IndEcon.R
sum(newdata_NOgeom$S_30p)


Pop_CensusB_AttMort<-merge(Pop_CensusB,newdata_NOgeom,by="GISJOIN")
Pop_PollB_MSA_AttMort <- st_intersection(Pop_CensusB_AttMort, MSA_sub_trans)
Pop_PollB_Chic_AttMort <- st_intersection(Pop_CensusB_AttMort, Chicago_trans)
Pop_Cook_AttMort<- Pop_CensusB_AttMort[Pop_CensusB_AttMort$COUNTY %in% c("Cook County"), ]

#######
#Most/Least Whites/Black loop 
#Creates 2 dataframes with mean and popWmean for each poll at the respective demographic quantiles
######



list.dfs <- list(All=Pop_CensusB_AttMort,MSA=Pop_PollB_MSA_AttMort,Chicago=Pop_PollB_Chic_AttMort,Cook=Pop_Cook_AttMort)

vect2 <- c(2,4,5,6,7,8,9,11,103) #for pollutants and tot population
vect4 <- c(195,196,197,198)
new.lst <- list() # list of dataframes for pollutant columns
ave.list <- list() # list of column averages 
new.lst_pw <- list()
pw.list<- list()
df2_MorteHDV <- NULL
df3 <- NULL
colnames(df2_MorteHDV) <- c('LowL_W','HighL_W','LowL_B','HighL_B','Region')
colnames(df3) <- c('LowL_W','HighL_W','LowL_B','HighL_B','Region')
#colnames(df2) <- c('Low_Poll','High_Poll','NA','NA','Region')
#colnames(df3) <- c('Low_Poll','High_Poll','NA','NA','Region')
for (i in 1:length(list.dfs)){
  print(i)
  Pop_PercTotL= list.dfs[[i]] %>% mutate(across(c(ALUKE003:ALUKE012), .fns = ~./ALUKE001*100)) #Fraction of race within each CT ##105 to 114 are White (ALUKE003) to Hispanic latino (ALUKE012)
  qw=unname(quantile(Pop_PercTotL$ALUKE003, probs = c(.10, .90),na.rm=TRUE)) #10th and 90th percentile of white population fraction
  #qhp=unname(quantile(Pop_PercTotL$PWF_no2_diff_XAWpop_TW, probs = c(.10, .90),na.rm=TRUE)) #10th and 90th percentile of white population fraction
  #cat(qw[1],qw[2])
  #qw <- c(2.47,87.70) #MSA WHITE PERCENTILES
  LowL_W = Pop_PercTotL[Pop_PercTotL$ALUKE003 < qw[1],]
  HighL_W = Pop_PercTotL[Pop_PercTotL$ALUKE003 > qw[2],]
  #Low_Poll = Pop_PercTotL[Pop_PercTotL$PWF_no2_diff_XAWpop_TW < qhp[1],]
  #High_Poll = Pop_PercTotL[Pop_PercTotL$PWF_no2_diff_XAWpop_TW > qhp[2],]
  #print(qw[1])
  #print(qw[2])
  
  qb=unname(quantile(Pop_PercTotL$ALUKE004, probs = c(.10, .90),na.rm=TRUE))
  LowL_B = Pop_PercTotL[Pop_PercTotL$ALUKE004 < qb[1],]
  HighL_B = Pop_PercTotL[Pop_PercTotL$ALUKE004 > qb[2],]
  list.dfs2 <- list(LowL_W,HighL_W,LowL_B,HighL_B)
  #list.dfs2 <- list(Low_Poll,High_Poll,LowL_B,HighL_B)
  

  #cat(qb[1],qb[2])
  for(j in 1:length(list.dfs2)){
    print (j)
    #z=1
    new.lst[[j]] <- list.dfs2[[j]][, vect4] #if I write LowL_W[, vect] I get all 3 vars CHANGE TO VECT3 for ATT_MORT
    #colnames(new.lst[[j]]) <- c('NO2','PM2.5','MDA8O3','geometry')
    #print(list.dfs2[[j]])
    ###Use next line for ATT_Mort SUM
    ave.list[[j]] <- colSums(new.lst[[j]][,c(1:3),drop=TRUE], na.rm = TRUE) #summing Att_Mort from new.lst for %cile White
    #ave.list[[j]] <- colMeans(new.lst[[j]][,c(1:8),drop=TRUE], na.rm = TRUE) #1,2,3 columns drop=True for sf objects
    #ave.list[[j]] <- colSums(new.lst[[j]][,c(1:4),drop=TRUE], na.rm = TRUE) #S_30p
    #print(new.lst[[j]])
    #print(ave.list[[j]])
    #tmp <- as.data.frame.list(ave.list[[j]])
    #print(tmp)
    #tmp$Region <- c(names(list.dfs[i]))
    #df2 <- rbind(df2,tmp)
    
    ####POP WEIGHTING
    #new.lst_pw[[j]] <- list.dfs2[[j]][, vect2]
    #new.lst_pw[[j]] <- new.lst_pw[[j]]%>% mutate(across(c(1:8), .fns = ~.*ALUKE001,.names = "{.col}_Xpop")) #ALUKE001==ALT0E001
    #pw.list[[j]] <- colSums(new.lst_pw[[j]][,c(11:18),drop=TRUE], na.rm = TRUE)/sum(new.lst_pw[[j]]$ALUKE001,na.rm=TRUE)
    ##tmp2 <- as.data.frame.list(pw.list)
    ##tmp2$Region <- c(names(list.dfs[i]))
    ##df3 <- rbind(df3,tmp2)
  }
  #print(i)
  tmp <- as.data.frame.list(ave.list)
  tmp$Region <- c(names(list.dfs[i]))
  colnames(tmp) <- c('LowL_W','HighL_W','LowL_B','HighL_B','Region')
  df2_MorteHDV <- rbind(df2_MorteHDV,tmp)
  ##print(tmp)
  ##tmp$Region <- c(names(list.dfs[i]))
  ##print(tmp[i])
  ##df2 <- rbind(df2,tmp)
  #tmp2 <- as.data.frame.list(pw.list)
  #tmp2$Region <- c(names(list.dfs[i]))
  #colnames(tmp2) <- c('LowL_W','HighL_W','LowL_B','HighL_B','Region')
  #df3 <- rbind(df3,tmp2)
}
#####


#####
#Plot
#####
df2_eHDV <- tibble::rownames_to_column(df2_eHDV, "VALUE")
df2_m_eHDV <- melt(df2_eHDV, id=c("Region","VALUE"))
df2_W_eHDV <- df2_m_eHDV %>% filter(str_detect(df2_m_eHDV$variable,"_W"))
df2_base_eHDV <- df2_W_eHDV %>% filter(str_detect(df2_W_eHDV$VALUE,"base"))
df2_base_eHDV$VALUE <-sub("\\_.*", "", df2_base_eHDV$VALUE)
df2_diff_eHDV <- df2_W_eHDV %>% filter(str_detect(df2_W_eHDV$VALUE,"diff"))
df2_diff_eHDV$VALUE <-sub("\\_.*", "", df2_diff_eHDV$VALUE)

####
#AttMort
#####
df2_MorteHDV <- tibble::rownames_to_column(df2_MorteHDV, "VALUE")
df2_Mort_m_eHDV <- melt(df2_MorteHDV, id=c("Region","VALUE"))
df2_Mort_W_eHDV <- df2_Mort_m_eHDV %>% filter(str_detect(df2_Mort_m_eHDV$variable,"_W"))
#df2_Mort_base <- df2_W %>% filter(str_detect(df2_W$VALUE,"base"))
df2_Mort_W_eHDV$VALUE <-sub("\\p.*", "", df2_Mort_W_eHDV$VALUE)
#df2_Mort_diff <- df2_W %>% filter(str_detect(df2_W$VALUE,"diff"))
#df2_Mort_diff$VALUE <-sub("\\_.*", "", df2_diff$VALUE)


#df2_W$VALUE <- sub("\\_.*","\\_",df2_W$VALUE)
#df2_W$VALUE <-sub("\\_.*", "", df2_W$VALUE) #Use this to get all characters before the _
#df2_W$VALUE = substr(df2_W$VALUE,1,nchar(df2_W$VALUE)-1) #since we changed the col names in loop we only need to remove last character now 

df3 <- tibble::rownames_to_column(df3, "VALUE")
df3_m <- melt(df3, id=c("Region","VALUE"))
df3_W <- df3_m %>% filter(str_detect(df3_m$variable,"_W"))
df3_base <- df3_W %>% filter(str_detect(df3_W$VALUE,"base"))
df3_base$VALUE <-sub("\\_.*", "", df3_base$VALUE)
df3_diff <- df3_W %>% filter(str_detect(df3_W$VALUE,"diff"))
df3_diff$VALUE <-sub("\\_.*", "", df3_diff$VALUE)

#df2_W$VALUE <- sub("\\_.*","\\_",df2_W$VALUE)
#df3_W$VALUE = substr(df3_W$VALUE,1,nchar(df3_W$VALUE)-1)

#tbl <- c(LowL_W = "Least Whites 25%",HighL_W = "Most Whites 75%")


#####
#Plot %White %Black geom point graphs for each pollutant
####
#pdf("/Users/sara/Library/CloudStorage/OneDrive-NorthwesternUniversity/R_plots/EDF_allReg_10_90_PW_EC_Base_v2.pdf") #borderlines issue
#pdf("/Users/sara/Library/CloudStorage/OneDrive-NorthwesternUniversity/R_plots/EDF_3quesPLot.pdf") #borderlines issue
pdf("/Users/sara/Library/CloudStorage/OneDrive-NorthwesternUniversity/R_plots/eHDV_Basline_PollNO2lim0_20_disparities_noPWsedac_allDoms.pdf") #borderlines issue
#https://stackoverflow.com/questions/37975602/connect-two-points-with-a-line-in-r
ggplot(df2_base_eHDV, aes(x = value, y = Region, color = variable, group = VALUE)) + 
  geom_point() + #geom_line(color = "black") + 
  theme_light()+
  #scale_colour_brewer(type = "seq", palette = "Spectral")+
  facet_grid(rows = vars(VALUE))+
  #theme(strip.background =element_rect(fill="red"))+
  theme(strip.text = element_text(colour = 'black'))+
  scale_x_continuous(limits = c(0, 20))+ #AirPop limits c(-1.8, 0.8); AttMort c(-140, 16)
  #labs(fill = "Quantiles")
  ggtitle("eHDV Changes in Baseline Poll") 
  #scale_fill_discrete(name = "Dose",labels=c('Least Whites 25%', 'Most Whites 75%'))
dev.off()

#########
#Plot White, Hispanic and Black population Density
#######
Pop_CensusB = Pop_CensusB %>% mutate(area_km = as.numeric(st_area(Pop_CensusB)/1000000))
Pop_CensusB = Pop_CensusB %>% mutate(Pop_denW = ALUKE003/area_km)
Pop_CensusB = Pop_CensusB %>% mutate(Pop_denB = ALUKE004/area_km)
Pop_CensusB = Pop_CensusB %>% mutate(Pop_denH = ALUKE012/area_km)
Pop_CensusB = Pop_CensusB %>% mutate(area = as.numeric(st_area(Pop_CensusB)))


