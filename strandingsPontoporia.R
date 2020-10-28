## Code for: 
## Fransiscana dolphin 'Pontoporia blainvillei' strandings in 
## southeast-south Brazil, Fransiscana Management Area (FMA) II, 
## between 2015-2020
## 
## Code by Jonatas H F do Prado, Jessica L Schattschneider, Nicholas W Daudt
## 

### General tidy rules - - - - - 
# variable_names <- write with lower case and underlines
# dataframeNames <- cammelCase style (startLowerCaseAndEveryNewWorldStartWithCapital)
# package::function notation, unless from 'base' packages
# Sessions delimited by "# Session name ####"
# Every step within the Session, use ## and a quick comment
###- - - - - - - - - - - - - - - 

# rm(list = ls())

# Libraries ####
library(plyr)
library(tidyverse)
library(mapview)
library(sf)
# library(lwgeom)
library(ecmwfr)
library(ncdf4)
library(reticulate)
# library(mgcv)

# Open files and visualize ####

# setwd("C:/Users/jessicas/Documents/strandingsPontoporia/")
# Lembrando, Jony - se tens um "projeto", o setwd ta automatico pra ti...

#
### Open Pontoporia dataset
pontoporia <- as.data.frame(
  readxl::read_xlsx("./Pontoporia PMP 2015_08_24 a 2020_06_11 SC_PR_SP_RJ.xlsx", 
                    sheet = 2))

## Filter columns
pontoporia <- 
  pontoporia %>% 
  dplyr::select(Código, `Identificador do indivíduo`, Estado, Praia, Trecho, 
         `Estratégia do trecho`, `Tipo do monitoramento`, `Data/Hora`, 
         `Ponto - Lat`, `Ponto - Long`, `Condição da carcaça`, `OFAI - Sexo`)

## Rename columns and levels
names(pontoporia) <- c("id", "id_individual", "state", "beach",
                       "strech_name", "strech_scheme", "monitoring_type",
                       "date_hour", "lat", "long", "cod_decomposition", "sex")

pontoporia$strech_scheme <- as.factor(pontoporia$strech_scheme)
levels(pontoporia$strech_scheme) <- list(daily = "Diário", 
                                         weekly = "Semanal", 
                                         fortnightly = "Diário 15", 
                                         call = "Acionamento")

pontoporia$monitoring_type <- as.factor(pontoporia$monitoring_type)
levels(pontoporia$monitoring_type) <- list(regular = "Regular", 
                                           call = "Acionamento")

pontoporia$sex <- as.factor(pontoporia$sex)
levels(pontoporia$sex) <- list(female = "Fêmea", 
                               male = "Macho", 
                               unkwown = "Indefinido")

## Transform df into a geospatial feature
pontoporiaSpatial <- 
  sf::st_as_sf(pontoporia, coords = c("long", "lat"), crs = 4326)

#
### Open drift_experiment dataset
drift <- utils::read.csv("./drift_experiment_data.csv", sep=";")

## Transform dataframe into a geospatial feature
# Need to remove observations with NA in "lat/long" and "id"
# Create an "id_distance" for calculate time between Release and Stranding
driftSpatial <- 
  drift %>% 
  dplyr::filter(lat != is.na(lat)) %>% 
  dplyr::filter(id != is.na(id)) %>%
  dplyr::mutate(id_distance = paste(campaign, id, release_stranding)) %>%
  sf::st_as_sf(coords = c("long", "lat"), crs = 4326)

driftSpatial$date <- lubridate::dmy(driftSpatial$date)
driftSpatial$campaign <- as.factor(driftSpatial$campaign)
driftSpatial$id_distance <- as.factor(driftSpatial$id_distance)

#
### Open isobath dataset
isobath <- sf::st_read("./linhasbatimetricas/linhas_lim_200m.shp")

# Initial visualizations -- strandings, drifts ####

# Mapview Pontoporia strandings
pontoporiaMapview <- # Run this line to add visualization into a df
  mapview::mapview(pontoporiaSpatial, cex = 0.2)

# Mapview Release Stations from drift experiment
driftReleaseStationsMapview <- # Run this line to add visualization into a df
  mapview::mapview(dplyr::filter(driftSpatial, id_data == "release"), 
                   zcol = "campaign")

# Mapview isobath 50m
isobath50Mapview <- # Run this line to add visualization into a df
  mapview::mapview(dplyr::filter(isobath, ELEV == 50))

## Visualize them all
driftReleaseStationsMapview + isobath50Mapview + pontoporiaMapview

# Drift experiment - Mean distance & time between release and strandings ####

## Create 'release' and 'strandings' vectors to use as basis for calculations
vectorRelease <- 
  driftSpatial %>% 
  as.data.frame() %>% 
  dplyr::select(id_distance) %>% 
  dplyr::filter(stringr::str_detect(id_distance, "R$")) %>% 
  droplevels()

vectorRelease <- as.character(vectorRelease[['id_distance']])

vectorStranding <- as.character(as.factor(stringr::str_replace(as.character(vectorRelease), "R", "S")))

## Calculate the distance between release and stranding location
driftDist <- list()

for(i in 1:27)
  driftDist[[i]] <- 
  sf::st_distance(driftSpatial$geometry[driftSpatial$id_distance == vectorRelease[i]], 
                  driftSpatial$geometry[driftSpatial$id_distance == vectorStranding[i]], 
                  by_element = FALSE)

mean(driftDist[[i]]); sd(driftDist[[i]]); min(driftDist[[i]]); max(driftDist[[i]])

# Duration between release and stranding
driftTime <- list()

for(i in 1:27)
  driftTime[[i]] <- 
  (driftSpatial$date[driftSpatial$id_distance == vectorRelease[i]]) -
  (driftSpatial$date[driftSpatial$id_distance == vectorStranding[i]])

mean(driftTime[[i]]); sd(driftTime[[i]]); min(driftTime[[i]]); max(driftTime[[i]])

# Considering distance between stranding and release position 
# for drifting time less than 10 days


# Monitored beach segments - OPEN DATA AND MERGE ALL SHAPEFILES INTO ONE ####

## Create a list with all sub-directories containing the shapefiles:
ff <- as.list(list.files(path = ".", 
                         pattern = "linha.shp$", 
                         recursive = TRUE, 
                         full.names = TRUE))

## Function to open the shps in sub-directories
open_shp <- function(ff){
  linha <- sf::read_sf(ff[i])
  linha
}

## A loop creating a list with all shapefiles
linhas <- list()
for (i in 1:length(ff)) {
  linhas[[i]] <- open_shp(ff)
}

## Merge all monitoring lines in one shapefile
mergedLines <- do.call(rbind, linhas)

## Visualize beach segments
mapview::mapview(filter(mergedLines, beach_name != "Praia não identificada"))


# Starting data collection from ERA5: ####

## Check for unique ids:
pontoporia %>% dplyr::group_by(id) %>% count()

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

## Start downloading the data, the path of the file
# will be returned as a variable (ncfile)
ncfile <- wf_request(user = "2088", 
                     request = request, 
                     transfer = TRUE, 
                     path = "~", 
                     verbose = FALSE)

#################