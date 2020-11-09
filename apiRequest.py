import os
import numpy as np
import pandas as pd
import geopandas
import xarray
import rioxarray
import matplotlib.pyplot as plt
from shapely.geometry import mapping

# sampleStranding = pd.read_csv("./sampleStranding.csv")

# sampleStranding['dateRange'] = sampleStranding['date_hour'] + '/' + sampleStranding['date_add']
# sampleStranding['areaRange'] = (sampleStranding['latS'] + 0.5).apply(str) + '/' + (sampleStranding['lonS'] - 0.5).apply(str) + '/' + (sampleStranding['latS'] - 0.5).apply(str) + '/' + (sampleStranding['lonS'] + 0.5).apply(str)print(sampleStranding)

# ## Connect with api
# c = cdsapi.Client()
# for i in range(len(sampleStranding)):
#         c.retrieve(        
#         'reanalysis-era5-single-levels', {            
#             'variable':[                    
#                 '2m_temperature','10m_u_component_of_wind',                    
#                 '10m_v_component_of_wind','surface_pressure',                    
#                 'mean_wave_direction', 'mean_wave_period',                     
#                 'significant_height_of_wind_waves',                
#                 ],            
#                 "product_type": "reanalysis",                
#                 'area': sampleStranding['areaRange'][i],                
#                 'date': sampleStranding['dateRange'][i],
#                 'time':[
#                     '00:00','06:00','12:00','18:00'
#                 ],                
#                 'grid':[
#                     '0.125/0.125'
#                 ],            
#                 "format": "netcdf"        
#             }, 'WTUVP'+str(sampleStranding['ID'][i])+'.nc')

geodf = geopandas.read_file("./qgislayers/estados_2010.shp")
# geodf.plot()
geodf = geodf.set_crs(epsg=4326, inplace=True, allow_override=True)


xds = xarray.open_dataset("./WTUVP4.nc", decode_times=False)
print(xds)
xds.rio.set_spatial_dims(x_dim="longitude", y_dim="latitude", inplace=True)
xds.rio.write_crs("epsg:4326", inplace=True)
clipped = xds.rio.clip(geodf.geometry.apply(mapping), geodf.crs, drop = True, invert=True)
clipped.to_netcdf(path="./test.nc")

##
lat = clipped.variables['latitude'][:]
lon = clipped.variables['longitude'][:]
time = clipped.variables['time'][:]
t2 = clipped.variables['t2m'][:] # 2 meter temperature



# ## check variables specifications:
# print(clipped['t2m'])

# ## plot:
# clipped['t2m'].plot()

# plt.show()

