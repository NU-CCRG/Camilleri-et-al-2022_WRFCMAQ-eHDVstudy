
from datetime import datetime
import os
os.environ['PROJ_LIB'] = '/home/scg9491/.conda/envs/myenv/share/proj'
from netCDF4 import Dataset
import PseudoNetCDF as pnc
#import netCDF4 as nc
import numpy as np
import pandas as pd
from pandas import Grouper
from pandas import DataFrame
import matplotlib.pyplot as plt
import matplotlib.colors as mc
from matplotlib import pyplot
#from mpl_toolkits.basemap import Basemap
import cartopy.crs as ccrs
import cartopy.feature as cfeature
import cartopy.io.shapereader as shpreader
#import proplot
from mpl_toolkits.axes_grid1 import make_axes_locatable
import matplotlib as mpl
import cartopy as cart
from matplotlib.colors import ListedColormap, LinearSegmentedColormap

######
#Seasons
#####
d_1 = '/projects/b1045/wrf-cmaq/output/Chicago_LADCO/output_BASE_FINAL_spring_1.33km_sf_rrtmg_5_8_1_v3852/postprocess'
base_VOC= Dataset(d_1+'/all_201904_VOC.nc')['VOC']
base_NOx= Dataset(d_1+'/all.nc')['NOX']
#base_VOCavg=np.average(base_VOC,axis=0)
#base_NOxavg=np.average(base_NOx,axis=0)
#print(base_VOCavg.shape,base_NOxavg.shape)
#print(np.min(base_VOCavg),np.max(base_VOCavg))
#print(np.min(base_NOxavg),np.max(base_NOxavg))

d_2 = '/projects/b1045/wrf-cmaq/output/Chicago_LADCO/output_Maxime_eHDV_spring_1.33km_sf_rrtmg_5_8_1_v3852/postprocess'
eHDV_VOC= Dataset(d_2+'/all_201904.nc')['VOC'] #only for summer is the VOC in a different nc 
eHDV_NOx= Dataset(d_2+'/all_201904.nc')['NOX']
#eHDV_VOCavg=np.average(eHDV_VOC,axis=0)
#eHDV_NOxavg=np.average(eHDV_NOx,axis=0)
#print(np.min(eHDV_VOCavg),np.max(eHDV_VOCavg))
#print(np.min(eHDV_NOxavg),np.max(eHDV_NOxavg))

d_3 = '/projects/b1045/wrf-cmaq/output/Chicago_LADCO/output_BASE_FINAL_wint_1.33km_sf_rrtmg_5_8_1_v3852/postprocess'
base_VOCw= Dataset(d_3+'/all_201901_VOC.nc')['VOC']
base_NOxw= Dataset(d_3+'/all.nc')['NOX']
#base_VOCavgw=np.average(base_VOCw,axis=0)
#base_NOxavgw=np.average(base_NOxw,axis=0)
#print(base_VOCavg.shape,base_NOxavg.shape)
#print(np.min(base_VOCavg),np.max(base_VOCavg))
#print(np.min(base_NOxavg),np.max(base_NOxavg))

d_4 = '/projects/b1045/wrf-cmaq/output/Chicago_LADCO/output_Maxime_eHDV_wint_1.33km_sf_rrtmg_5_8_1_v3852/postprocess'
eHDV_VOCw= Dataset(d_4+'/all_201901.nc')['VOC'] #only for summer is the VOC in a different nc 
eHDV_NOxw= Dataset(d_4+'/all_201901.nc')['NOX']
#eHDV_VOCavgw=np.average(eHDV_VOCw,axis=0)
#eHDV_NOxavgw=np.average(eHDV_NOxw,axis=0)
#print(np.min(eHDV_VOCavg),np.max(eHDV_VOCavg))
#print(np.min(eHDV_NOxavg),np.max(eHDV_NOxavg))

d_5 = '/projects/b1045/wrf-cmaq/output/Chicago_LADCO/output_BASE_FINAL_fall_1.33km_sf_rrtmg_5_8_1_v3852/postprocess'
base_VOCf= Dataset(d_5+'/all_201810_VOC.nc')['VOC']
base_NOxf= Dataset(d_5+'/all.nc')['NOX']
#base_VOCavgf=np.average(base_VOCf,axis=0)
#base_NOxavgf=np.average(base_NOxf,axis=0)
#print(base_VOCavg.shape,base_NOxavg.shape)
#print(np.min(base_VOCavg),np.max(base_VOCavg))
#print(np.min(base_NOxavg),np.max(base_NOxavg))

d_6 = '/projects/b1045/wrf-cmaq/output/Chicago_LADCO/output_Maxime_eHDV_fall_1.33km_sf_rrtmg_5_8_1_v3852/postprocess'
eHDV_VOCf= Dataset(d_6+'/all_201810.nc')['VOC'] #only for summer is the VOC in a different nc 
eHDV_NOxf= Dataset(d_6+'/all_201810.nc')['NOX']
#eHDV_VOCavgf=np.average(eHDV_VOCf,axis=0)
#eHDV_NOxavgf=np.average(eHDV_NOxf,axis=0)
#print(np.min(eHDV_VOCavg),np.max(eHDV_VOCavg))
#print(np.min(eHDV_NOxavg),np.max(eHDV_NOxavg))

d_7 = '/projects/b1045/wrf-cmaq/output/Chicago_LADCO/output_BASE_FINAL_1.33km_sf_rrtmg_5_8_1_v3852/postprocess'
base_VOCsu= Dataset(d_7+'/all_201808_VOC.nc')['VOC']
base_NOxsu= Dataset(d_7+'/all.nc')['NOX']
#base_VOCavgsu=np.average(base_VOCsu,axis=0)
#base_NOxavgsu=np.average(base_NOxsu,axis=0)
#print(base_VOCavg.shape,base_NOxavg.shape)
#print(np.min(base_VOCavg),np.max(base_VOCavg))
#print(np.min(base_NOxavg),np.max(base_NOxavg))

d_8 = '/projects/b1045/wrf-cmaq/output/Chicago_LADCO/output_Maxime_eHDV_1.33km_sf_rrtmg_5_8_1_v3852/postprocess'
eHDV_VOCsu= Dataset(d_8+'/all_201808_VOC.nc')['VOC'] #only for summer is the VOC in a different nc 
eHDV_NOxsu= Dataset(d_8+'/all_201808.nc')['NOX']
#eHDV_VOCavgsu=np.average(eHDV_VOCsu,axis=0)
#eHDV_NOxavgsu=np.average(eHDV_NOxsu,axis=0)
#print(np.min(eHDV_VOCavg),np.max(eHDV_VOCavg))
#print(np.min(eHDV_NOxavg),np.max(eHDV_NOxavg))
print('eHDV VOC summer',eHDV_VOCsu.shape)
'''
ann_B_VOC = np.concatenate((base_VOCavg,base_VOCavgw,base_VOCavgf,base_VOCavgsu))
ann_B_NOx = np.concatenate((base_NOxavg,base_VOCavgw,base_NOxavgf,base_NOxavgsu))
ann_H_VOC = np.concatenate((eHDV_VOCavg,eHDV_VOCavgw,eHDV_VOCavgf,eHDV_VOCavgsu))
ann_H_NOx = np.concatenate((eHDV_NOxavg,eHDV_VOCavgw,eHDV_NOxavgf,eHDV_NOxavgsu))
print('concate check',ann_B_VOC.shape,ann_H_VOC.shape)
'''
ann_B_VOC = np.concatenate((base_VOC,base_VOCw,base_VOCf,base_VOCsu))
ann_B_NOx = np.concatenate((base_NOx,base_NOxw,base_NOxf,base_NOxsu))
ann_H_VOC = np.concatenate((eHDV_VOC,eHDV_VOCw,eHDV_VOCf,eHDV_VOCsu))
ann_H_NOx = np.concatenate((eHDV_NOx,eHDV_NOxw,eHDV_NOxf,eHDV_NOxsu))
print('concate check',ann_B_VOC.shape,ann_H_VOC.shape)


ann_B_VOCavg = np.average(ann_B_VOC,axis=0)
ann_B_NOxavg = np.average(ann_B_NOx,axis=0)
ann_H_VOCavg = np.average(ann_H_VOC,axis=0)
ann_H_NOxavg = np.average(ann_H_NOx,axis=0)
print('B_VOC',np.min(ann_B_VOCavg),np.max(ann_B_VOCavg))
print('B_NOx',np.min(ann_B_NOxavg),np.max(ann_B_NOxavg))
print('H_VOC other',np.min(ann_H_VOCavg),np.max(ann_H_VOCavg))
print('H_NOx',np.min(ann_H_NOxavg),np.max(ann_H_NOxavg))
#exit()
ann_B_VOCdivNOx = np.divide(ann_B_VOCavg,ann_B_NOxavg)
ann_H_VOCdivNOx = np.divide(ann_H_VOCavg,ann_H_NOxavg)
print('B_ratio',np.min(ann_B_VOCdivNOx),np.max(ann_B_VOCdivNOx))
print('H_ratio',np.min(ann_H_VOCdivNOx),np.max(ann_H_VOCdivNOx))

diff_VOC = ann_H_VOCavg - ann_B_VOCavg
diff_NOx = ann_H_NOxavg - ann_B_NOxavg
diff_VOCdivNOX = ann_H_VOCdivNOx - ann_B_VOCdivNOx
vmin = np.min(abs(diff_VOCdivNOX))
vmax = np.max(abs(diff_VOCdivNOX))
print(vmin, vmax)
print(np.min(diff_VOCdivNOX),np.max(diff_VOCdivNOX))
print('diff ratio shape',diff_VOCdivNOX.shape)
#exit()

####Load cmaq lats and lons
grid_path = '/projects/b1045/wrf-cmaq/output/Chicago_LADCO/output_BASE_FINAL_1.33km_sf_rrtmg_5_8_1_v3852/latlon.nc'
grid = pnc.pncopen(grid_path, format='ioapi')
#cmaq lats and lons
cmaq_lon = grid.variables['lon']
#print('units = %s, values = %s' % (cmaq_lon.units, cmaq_lon[:]))
cmaq_lat = grid.variables['lat']
#print('units = %s, values = %s' % (cmaq_lat.units, cmaq_lat[:]))

#Var =[ann_B_VOCavg,ann_H_VOCavg,ann_B_NOxavg,ann_H_NOxavg]
#Var =[diff_VOC,diff_NOx]
#Var =[diff_VOCdivNOX]
#Var =[ann_B_VOCdivNOx,ann_H_VOCdivNOx]
Var =[ann_B_VOCdivNOx]

#Titles = ['B_VOC (ppbC)','H_VOC (ppbC)','B_NOx (ppbV)','H_NOx (ppbV)']
#Titles = ['eHDV-Base VOC (ppbC)','eHDV-Base NOx (ppbV)']
#Titles = ['diff_VOC/NOx (ppbC/ppbV)']
#Titles = ['Base VOC/NOx (ppbC/ppbV)','eHDV VOC/NOx (ppbC/ppbV)']
Titles = ['Base VOC/NOx (ppbC/ppbV)']

k=1
#fig = plt.figure(figsize=(24,24)) #used for GASES
fig=plt.figure(figsize=(8,6))
cmap=mpl.cm.plasma
#cmap=mpl.cm.seismic
#vmin2=np.percentile(base[0,0,:,:],q=1)
#vmax2=np.percentile(base[0,0,:,:],q=99)
for i in Var:
	#for k in range(len(Var)):
	#print(i)
	#i=str(i)
	#print(shape.i)
	T=Titles[k-1]
	print(k)
	vmin = np.min(abs(i[0]))
	vmax = np.max(abs(i[0]))
	#ax = fig.add_subplot(1,2,k,projection=ccrs.LambertConformal(central_longitude=-96, central_latitude=39))
	ax = fig.add_subplot(1,1,k,projection=ccrs.LambertConformal(central_longitude=-96, central_latitude=39))
	ax.set_extent([np.amin(cmaq_lon)+0.35,np.amax(cmaq_lon)-0.55,np.amin(cmaq_lat)+0.355,np.amax(cmaq_lat)-0.35])
	#PC=plt.pcolormesh(cmaq_lon,cmaq_lat,i[0],transform=ccrs.PlateCarree(),cmap=cmap,vmin=vmin, vmax=vmax) #3 and -3 for VOC/NOX ratio seasons and 2,-2 for annual VOC ratio diff
	plt.pcolormesh(cmaq_lon,cmaq_lat,i[0],transform=ccrs.PlateCarree(),cmap=cmap,vmin=0, vmax=16)
        #ax.set_title('eHDV-Base '+ i,fontsize=12)
	ax.set_title('Annual '+T,fontsize=12)
	ax.add_feature(cfeature.STATES.with_scale('10m'),edgecolor='black',alpha=0.4)
	bounds = [0,2,4,6,8,10,12,14,16]
	norm = mpl.colors.BoundaryNorm(bounds, cmap.N)
	PC = mpl.cm.ScalarMappable(norm=norm, cmap=cmap)
	#cbar=plt.colorbar(PC, fraction=.08, pad=0.04, shrink=0.5, aspect=12) #use when you have 2X2 subplot
	cbar=plt.colorbar(PC, extend='max') #use when you have 2X2 subplot

	#plt.colorbar()
	k=k+1

plt.savefig('Annual_Base_VOCNOxRatio_plasma.png')
                                                            
