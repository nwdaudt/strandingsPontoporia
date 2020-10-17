# Code for consultancy of Pontoporia strandings

# Libraries ####
library(mgcv)
library(tidyverse)
library(mapview)
library(sf)
library(ecmwfr)
library(ncdf4)
library(reticulate)

# Open file and visualize ###
# rm(list = ls())
setwd("C:/Users/jessicas/Documents/strandingsPontoporia/")

## open pontoporia dataset:
pontoporia <- readxl::read_xlsx("./Pontoporia PMP 2015_08_24 a 2020_06_11 SC_PR_SP_RJ.xlsx", sheet = 2)
# filter columns
pontoporia <- 
  pontoporia %>% 
  select(Código, `Data/Hora`, `Condição da carcaça` ,`Ponto - Lat`, `Ponto - Long`)
#Rename columns
names(pontoporia) <- c("id", "date_hour", "cod_carcaca", "lat", "long")
## transform dataframe into a geospatial feature
pontoporia.sp <- st_as_sf(pontoporia, coords = c("long", "lat"), crs = 4326)
pontoporia.mapview <- mapview(pontoporia.sp, cex = 0.2) #add visualization into a df

## open drift dataset:
drift <- read.csv("./woodDrift.csv")
# transform dataframe into a geospatial feature
drift.sp <- st_as_sf(drift, coords = c("Y", "X"), crs = 4326)
drift.mapview <- mapview(drift.sp, zcol = "ï..Campanha" ) #add visualization into a df

## open isobath dataset:
isobath <- st_read("./linhasbatimetricas/linhas_lim_200m.shp")
isobath.mapview <- mapview(filter(isobath, ELEV==50)) #add visualization into a df

## visualize all 
drift.mapview + isobath.mapview + pontoporia.mapview

## import monitored beach segments:
## 01 - OPEN DATA AND MERGE ALL SHAPEFILES INTO ONE ####
# create a list with all subdirectories containing the shapefiles:
ff <- as.list(list.files(path=".", pattern="linha.shp$", recursive=TRUE, full.names=TRUE))

## Function to open the shps in subdirectories
open_shp <- function(ff){
  linha <- read_sf(ff[i])
  linha
}

## A looping creating a list with all shapefiles
linhas <- list()
for (i in 1:length(ff)) {
  linhas[[i]] <- open_shp(ff)
}

# Merge all monitoring lines in one shapefile
merged.lines <- do.call(rbind, linhas)
# visualize beach segments
mapview(filter(merged.lines, beach_name != "Praia não identificada"))


####### Starting data collection from ERA5: ###########
# check for unique ids:
pontoporia %>% group_by(id) %>% count()

## start downloading data:

wf_set_key("user" = "jessica.leiria@gmail.com",
  "key" = "afc55855c5156df018fa0173630fe672",
  "service" = "webapi")

request <- list("dataset_short_name" = "reanalysis-era5-pressure-levels",
                "product_type"   = "reanalysis",
                "variable"       = "temperature",
                "pressure_level" = "850",
                "year"           = "2000",
                "month"          = "04",
                "day"            = "04",
                "time"           = "00:00",
                "area"           = "70/-20/30/60",
                "format"         = "netcdf",
                "target"         = "era5-demo.nc")

# Start downloading the data, the path of the file
# will be returned as a variable (ncfile)
ncfile <- wf_request(user = "2088",
                     request = request,   
                     transfer = TRUE,  
                     path = "~",
                     verbose = FALSE)
