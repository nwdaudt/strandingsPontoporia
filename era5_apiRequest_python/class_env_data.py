import os
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import cdsapi
from netCDF4 import Dataset
import geopandas
import cartopy.crs as ccrs
import xarray as xr
import rioxarray
from shapely.geometry import mapping
import re

class ApiRequest(object):
    def __init__(self, dir_save_file = "./testClass", input_df = "./testClass/pontoporia.csv"
                 , download_data = True, column_id = "id_individual", clip_shape = "./qgislayers/estados_2010.shp"):
        self.dir_save_file = dir_save_file
        self.input_df = input_df
        self.download_data = download_data
        self.column_id = column_id
        self.clip_shape = clip_shape

    def wrangling_and_download_data(self):
        sampleStranding = pd.read_csv(self.input_df, sep=",")
        # Add date range to a new column:
        sampleStranding['dateRange'] = sampleStranding['back_date'] + '/' + sampleStranding['date']
        # Calculate spatial range to a new column: Zonas 1 - 3 : 50km;  Zona 2 : 100km
        sampleStranding['areaRange'] = (np.where(sampleStranding['zone'] == 2,
                                                 (sampleStranding['lat'] + 0.5).apply(str) + '/' + (
                                                             sampleStranding['long'] - 0.5).apply(str) + '/' + (
                                                             sampleStranding['lat'] - 0.5).apply(str) + '/' + (
                                                             sampleStranding['long'] + 0.5).apply(str)
                                                 , (sampleStranding['lat'] + 0.25).apply(str) + '/' + (
                                                             sampleStranding['long'] - 0.25).apply(str) + '/' + (
                                                             sampleStranding['lat'] - 0.25).apply(str) + '/' + (
                                                             sampleStranding['long'] + 0.25).apply(str)))

        ## Check:
        print(sampleStranding['areaRange'], sampleStranding['dateRange'])

        if self.download_data == True:
            # Connect with api
            c = cdsapi.Client()

            for i in range(len(sampleStranding)):
                if os.path.isfile(os.path.join(self.dir_save_file,
                                               'WTUVP' + str(sampleStranding[self.column_id][i]) + '.nc')):
                    pass
                    # print ("file exists: WTUVP" +str(sampleStranding['id_individual'][i])+'.nc')
                else:
                    print("file doesn't exist: WTUVP" + str(sampleStranding[self.column_id][i]) + '.nc')
                    c.retrieve(
                        'reanalysis-era5-single-levels',
                        {
                            'variable': [
                                '10m_u_component_of_wind',
                                '10m_v_component_of_wind',
                                'mean_wave_direction', 'mean_wave_period',
                                'significant_height_of_wind_waves',
                            ],
                            "product_type": "reanalysis",
                            'area': sampleStranding['areaRange'][i],
                            'date': sampleStranding['dateRange'][i],
                            'time': [
                                '00:00', '06:00', '12:00', '18:00'
                            ],
                            'grid': [
                                '0.125/0.125'
                            ],
                            "format": "netcdf"
                        },
                        os.path.join(self.dir_save_file,
                                     'WTUVP' + str(sampleStranding[self.column_id][i]) + '.nc'))

    def clip_data_and_summarise(self):
        ## Read shapefile with brazilian states
        states = geopandas.read_file(self.clip_shape)
        states = states.set_crs(epsg=4326, inplace=True, allow_override=True)
        # states.plot()

        ## get all file names:
        eraFiles = os.listdir(self.dir_save_file)
        print(os.listdir(self.dir_save_file))

        ## read all .nc and clip with BR states shapefile
        eraFilesList = []
        clip = []
        for item in eraFiles:
            uniqueFile = xr.open_dataset(self.dir_save_file + item)
            uniqueFile.rio.set_spatial_dims(x_dim="longitude", y_dim="latitude", inplace=True)
            uniqueFile.rio.write_crs("epsg:4326", inplace=True)
            # uniqueFile = uniqueFile.rio.clip(states.geometry.apply(mapping), states.crs, drop=True, invert=True)
            u10mean = (uniqueFile.variables['u10'][:, :, :]).mean()  # u component
            v10mean = (uniqueFile.variables['v10'][:, :, :]).mean()  # v component
            mwdmean = (uniqueFile.variables['mwd'][:, :, :]).mean()  # v component
            mwpmean = (uniqueFile.variables['mwp'][:, :, :]).mean()  # v component
            shwwmean = (uniqueFile.variables['shww'][:, :, :]).mean()  # v component
            ## Min:
            u10min = (uniqueFile.variables['u10'][:, :, :]).min()  # u component
            v10min = (uniqueFile.variables['v10'][:, :, :]).min()  # v component
            mwdmin = (uniqueFile.variables['mwd'][:, :, :]).min()  # v component
            mwpmin = (uniqueFile.variables['mwp'][:, :, :]).min()  # v component
            shwwmin = (uniqueFile.variables['shww'][:, :, :]).min()  # v component
            ## Max:
            u10max = (uniqueFile.variables['u10'][:, :, :]).max()  # u component
            v10max = (uniqueFile.variables['v10'][:, :, :]).max()  # v component
            mwdmax = (uniqueFile.variables['mwd'][:, :, :]).max()  # v component
            mwpmax = (uniqueFile.variables['mwp'][:, :, :]).max()  # v component
            shwwmax = (uniqueFile.variables['shww'][:, :, :]).max()  # v component
            ## appending
            eraFilesList.append([item, u10mean, v10mean, mwdmean, mwpmean, shwwmean, u10max,
                                 v10max, mwdmax, mwpmax, shwwmax, u10min, v10min, mwdmin, mwpmin, shwwmin])
            clip.append(uniqueFile)

        eraFilesList = pd.DataFrame(eraFilesList, columns=["fileName", "u10mean", "v10mean",
                                                           "mwdmean", "mwpmean", "shwwmean", "u10max", "v10max",
                                                           "mwdmax", "mwpmax",
                                                           "shwwmax", "u10min", "v10min", "mwdmin", "mwpmin",
                                                           "shwwmin"])
        print(eraFilesList)

        ## Save as .csv
        pd.DataFrame.to_csv(eraFilesList, os.path.join(self.dir_save_file, str(self.input_df) + "envData"))




