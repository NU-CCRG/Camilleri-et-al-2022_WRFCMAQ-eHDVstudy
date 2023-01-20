
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
import cartopy.crs as ccrs
import cartopy.feature as cfeature
import cartopy.io.shapereader as shpreader
from mpl_toolkits.axes_grid1 import make_axes_locatable
import matplotlib as mpl
import cartopy as cart
from matplotlib.colors import ListedColormap, LinearSegmentedColormap

######read the shapefile for counties
reader = shpreader.Reader('/home/scg9491/countyl010g.shp')
counties = list(reader.geometries())

d_1 = '/projects/b1045/scamilleri'


####Load cmaq lats and lons
grid_path = '/projects/b1045/wrf-cmaq/output/Chicago_LADCO/output_BASE_FINAL_1.33km_sf_rrtmg_5_8_1_v3852/latlon.nc'
grid = pnc.pncopen(grid_path, format='ioapi')
#cmaq lats and lons
cmaq_lon = grid.variables['lon']
cmaq_lat = grid.variables['lat']


#Var =['NO2','NO','NOX','PM25_TOT']
Var =['O3']

k=1
fig=plt.figure(figsize=(8,6))
#cmap=mpl.cm.plasma #used for baseline
cmap=mpl.cm.seismic

for i in Var:
        #for k in range(len(Var)):
	print(i)
	i=str(i)
	print(k)
	base= Dataset(d_1+'/Baseline_Daily_MDA8O3.nc')[i] # 123,1,row,col
	print (base) 
	print('base shape ',base.shape)
	mo3_base_avg=np.average(base,axis=0) #time average
	print('mo3_base_avg',mo3_base_avg.shape)
	vmax_b = np.max(mo3_base_avg[0,:,:])
	vmin_b = np.min(mo3_base_avg[0,:,:])
	print(vmax_b, vmin_b)
	AT= Dataset(d_1+'/allTrans_allTime_MDA8O3.nc')[i]
	AT_avg=np.average(AT,axis=0)
	print('AT shape',AT.shape)
	print('AT_avg shape',AT_avg.shape)
	vmax_h = np.max(AT_avg[0,:,:])
	vmin_h = np.min(AT_avg[0,:,:])
	print(vmax_h, vmin_h)
	diff = AT_avg[0,:,:]-mo3_base_avg[0,:,:] 
	vmin_ab = np.min(abs(diff))
	vmax_ab = np.max(abs(diff))
	vmax_d = np.max(diff)
	vmin_d = np.min(diff)
	print('vmin_ab','vmax_ab','vmax_d','vmin_d')
	print(vmin_ab,vmax_ab,vmax_d,vmin_d)
	print('Diff shape', diff.shape)
	
	ax = fig.add_subplot(1,1,k,projection=ccrs.LambertConformal(central_longitude=-96, central_latitude=39))
	ax.set_extent([np.amin(cmaq_lon)+0.35,np.amax(cmaq_lon)-0.55,np.amin(cmaq_lat)+0.355,np.amax(cmaq_lat)-0.35])
	plt.pcolormesh(cmaq_lon,cmaq_lat,diff,transform=ccrs.PlateCarree(),cmap=cmap,vmin=-2, vmax=2)	
	ax.set_title('AT Annual '+ i,fontsize=9)
	ax.add_feature(cfeature.STATES.with_scale('10m'),edgecolor='black',alpha=0.4)
	bounds = [-2,-1.5,-1.0,-0.5,0,0.5,1.0,1.5,2]
	norm = mpl.colors.BoundaryNorm(bounds, cmap.N, extend='both')
	plt.colorbar(mpl.cm.ScalarMappable(norm=norm, cmap=cmap),label="ppbV")
	
	k=k+1
plt.savefig('AT_MDA8O3_diff.png')
