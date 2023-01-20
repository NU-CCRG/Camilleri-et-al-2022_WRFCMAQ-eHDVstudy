library(ggplot2)
library(maps)
library(ggforce)


SO2_EGUs<-read.csv("/Users/sara/Library/CloudStorage/OneDrive-NorthwesternUniversity/eHDV/SO2_EGUs_wFuelType_NOTpreciseMatch.csv")
CO_EGUs<-read.csv("/Users/sara/Library/CloudStorage/OneDrive-NorthwesternUniversity/eHDV/CO_EGUs_wFuelType_NOTpreciseMatch.csv")

LADCO_EGUs_AT<-read.csv("/Users/sara/Library/CloudStorage/OneDrive-NorthwesternUniversity/All_Trans/AT_ForSara_LADCO_NOSolWindHydro.csv")

CONUS_EGUs_AT<-read.csv("/Users/sara/Library/CloudStorage/OneDrive-NorthwesternUniversity/All_Trans/AT_ForSara_NoSolWindHydro.csv")

NERC_df<-st_read("/Users/sara/Library/CloudStorage/OneDrive-NorthwesternUniversity/All_Trans/NERC_Regions/NERC_REGIONS_EIA.shp")
EGUs_byFuelType_Nerc<-read.csv("/Users/sara/Library/CloudStorage/OneDrive-NorthwesternUniversity/All_Trans/csv_for_Sara_noRenewables.csv")


pdf("/Users/sara/Library/CloudStorage/OneDrive-NorthwesternUniversity/R_plots/AT_EGUs_CONUS_AddElect_FuelType_NoSolarHydro_BarPlot.pdf")
ggplot(data=EGUs_byFuelType_Nerc, aes(x=NERC_Region, y=Add_Elec, fill=Fuel.type)) +
  scale_fill_manual(values=c("#a6cee3", "#1f78b4", "#b2df8a",'#33a02c','#fb9a99','#e31a1c','#fdbf6f'))+
  theme_bw()+
  #geom_bar()
  geom_bar(stat="identity")
dev.off()

ggplot(data = NERC_df) + 
  geom_sf( aes(fill=NERC),lwd=0,colour=NA,alpha=0.8)+
  scale_fill_manual(values=c("#ffffe5", "#fff7bc", "#fee391",'#fec44f','#fe9929','#ec7014','#cc4c02','#8c2d04'))+
  #scale_fill_viridis_d(option = "C")+ #NB THIS IS SCALE_FILL NOT SCALE_COLOR
  theme_bw()+
  coord_sf() 

base_world <- map_data('state')

base_States <- map_data('state', region = c('illinois', 'michigan', 'indiana','wisconsin'))

#map('state', region = c('illinois', 'michigan', 'indiana','wisconsin'))	# map of three states
#'new york', 'new jersey', 'penn'

#########
#Step 1
#########
p <- ggplot() + coord_fixed() +
  xlab("") + ylab("")

#Add map to base plot
base_world_messy <- p + geom_polygon(data=base_States, aes(x=long, y=lat, group=group), 
                                     colour="light green", fill="light green")
base_world_messy

#Strip the map down so it looks super clean (and beautiful!)
cleanup <- 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = 'white', colour = 'white'), 
        axis.line = element_line(colour = "white"), #legend.position="none",
        axis.ticks=element_blank(), axis.text.x=element_blank(),
        axis.text.y=element_blank())

#base_world <- base_world_messy + cleanup

#base_world

#map_data <- 
  #base_world +
  #geom_point(data=SO2_EGUs, aes(x=x, y=y,size=diff_EGU), colour="Deep Pink", fill="Pink",pch=21, alpha=I(0.7))
  #coord_map("albers",  lat0 = 45.5, lat1 = 29.5)

#map_data

#########
#Step 2
########
#pdf("/Users/sara/Library/CloudStorage/OneDrive-NorthwesternUniversity/R_plots/eHDV_EGUs_LADCO_FuelType_PercEleGen_NoSolarHydro_Zoomed.pdf")
#pdf("/Users/sara/Library/CloudStorage/OneDrive-NorthwesternUniversity/R_plots/eHDV_EGUs_LADCO_SO2FuelType_NoSolarHydro_NewCol_Boutline.pdf")
#pdf("/Users/sara/Library/CloudStorage/OneDrive-NorthwesternUniversity/R_plots/eHDVEGUs_CONUS_Perc_FuelType_NoSolarHydro_NOBlackOut_NOTtrans_NewData80.pdf")
pdf("/Users/sara/Library/CloudStorage/OneDrive-NorthwesternUniversity/R_plots/AT_EGUs_CONUS_Perc_FuelType_NoSolarHydro_100.pdf")

p <- ggplot() + coord_fixed() +
  xlab("") + ylab("")

#ggplot() + 
#  geom_polygon(data=base_States, aes(x=long, y=lat, group=group, fill=region)) +
#  geom_point(data=SO2_EGUs, aes(x=x, y=y,size=diff_EGU), alpha=I(0.7))+ #, size=5,
#  coord_map("albers",  lat0 = 45.5, lat1 = 29.5)+
#  #theme_bw()
#  theme_nothing()

plot <- p + geom_polygon(data=base_world, aes(x=long, y=lat, group=group), color='grey',fill='white')+#color='white',fill='grey95'
  coord_equal()+
  ##scale_fill_brewer(palette = 'terrain_hcl')+ #antiquewhite to colour by region fill=region inside aes #OUTSIDE of aes, color='white',fill='grey95'
  geom_point(data=CONUS_EGUs_AT, aes(x=Lon, y=Lat,size=Add_Perc_Inc,colour=Fuel.type), alpha=1)+scale_size_continuous(limits = c(0,100),range = c(1, 3))+ #,range = c(1, 4)#, size=5, #range is actual size of circles #alpha 0 transparent and 1 solid
  scale_color_manual(values=c("#a6cee3", "#1f78b4", "#b2df8a",'#33a02c','#fb9a99','#e31a1c','#fdbf6f'))+
  ##geom_point(data=CONUS_EGUs,color='black' ,aes(x=Lon, y=Lat,size=Perc,color=Fuel.type), alpha=1)
  #geom_point(data=CONUS_EGUs, aes(x=Lon, y=Lat,size=Add_Perc_Inc),shape = 1,colour = "black", stroke=0.5)+scale_size_continuous(limits = c(0,40),range = c(1, 5))+ #This creates the border 
  coord_equal()

  ##geom_point(data=SO2_EGUs, aes(x=Lon, y=Lat,size=SO2_EGU), alpha=0.6)+scale_size_continuous(limits = c(0,40),range = c(1, 5)) #, size=5, #range is actual size of circles
  #geom_point(data=SO2_EGUs, aes(x=Lon, y=Lat,size=SO2_EGU,colour=Fuel_Type), alpha=0.7)+scale_size_continuous(range = c(2, 6))+ #, size=5, #range is actual size of circles
  ##geom_point(data=SO2_EGUs, aes(x=Lon, y=Lat))
  #geom_point(data=SO2_EGUs, aes(x=Lon, y=Lat,size=SO2_EGU),shape = 1,colour = "black")+scale_size_continuous(range = c(2, 6))+ #This creates the border 
  #scale_color_manual(values=c("#a6cee3", "#1f78b4", "#b2df8a",'#e31a1c'))
  
##coord_map("albers",  lat0 = 45.5, lat1 = 29.5)

#plot_clean <- plot + theme_nothing()
plot_clean <- plot + cleanup #does same thing as above but this needs definition of  cleanup
plot_clean

P_Final <-plot_clean+coord_sf(xlim = c(-89.65, -84.81), ylim = c(40.40,43.72)) #Ladco Zoom
  #facet_zoom(xlim = c(-90, -84), ylim=c(40,44)) #doesn't work well
P_Final

dev.off()

