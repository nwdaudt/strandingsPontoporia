## Code for: 
## Fransiscana dolphin 'Pontoporia blainvillei' strandings in 
## southeast-south Brazil, Fransiscana Management Area (FMA) II, 
## between 2015-2020
## 
## Code by Jonatas H F do Prado, Jessica L Schattschneider, Nicholas W Daudt
## 

### General tidy rules - - - - - 
# variable_names <- write with lower case and underlines
# dataframeNames <- cammelCase style (startLowerCaseAndEveryNewWordStartWithCapital)
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

#
## Open Pontoporia dataset
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

pontoporia$state <- as.factor(pontoporia$state)

## Remove records from "state" == Rio de Janeiro & "cod_decomposition" == 5 
pontoporia <- 
  pontoporia %>% 
  dplyr::filter(state != "Rio de Janeiro") %>% 
  dplyr::filter(cod_decomposition != 5)

## Create column "date" and "back_date", 
## for future environment variables gathering
pontoporia <- 
  pontoporia %>% 
  dplyr::mutate(date = lubridate::as_date(date_hour)) %>% 
  dplyr::mutate(back_date = lubridate::as_date(ifelse(cod_decomposition == 2, 
                                                      date - 1, 
                                                      date - 6)))

## Transform df into a geospatial feature
pontoporiaSpatial <- 
  sf::st_as_sf(pontoporia, coords = c("long", "lat"), crs = 4326)

#
## Open drift_experiment dataset
drift <- utils::read.csv("./drift_experiment_data.csv", sep=";")

## Filter and set up Date columns
drift <- 
  drift %>% 
  dplyr::filter(lat_s != is.na(lat_s)) %>% 
  dplyr::filter(date_s != is.na(date_s))
  
drift$date_r <- lubridate::dmy(drift$date_r)
drift$date_s <- lubridate::dmy(drift$date_s)

## Create "zone" column -- spatial-similar release stations 
drift <- 
  drift %>% 
  dplyr::mutate(
    zone = 
      ifelse(lat_r > -23.8, "1", 
             ifelse(-23.8 > lat_r & lat_r > -24.4,"2", 
                    ifelse(-24.4 > lat_r & lat_r > -26, "3", "4"))))

drift$zone <- as.factor(drift$zone)

driftSpatial <- 
  drift %>% sf::st_as_sf(coords = c("long_r", "lat_r"), crs = 4326)

#
## Open isobath dataset
isobath <- sf::st_read("./linhas_batimetricas/linhas_lim_200m.shp")

# Mapview -- quick spatial check on strandings and drifts ####

## Mapview Pontoporia strandings
pontoporiaMapview <- # Run this line to add visualization into a df
  mapview::mapview(pontoporiaSpatial, cex = 0.2)

## Mapview Release Stations from drift experiment
driftReleaseStationsMapview <- # Run this line to add visualization into a df
  mapview::mapview(driftSpatial, zcol = "zone")

## Mapview isobath 30m
isobath30Mapview <- # Run this line to add visualization into a df
  mapview::mapview(dplyr::filter(isobath, ELEV == 30))

## Visualize them all
driftReleaseStationsMapview + isobath30Mapview + pontoporiaMapview

# Drift experiment - Mean distance & time between release and strandings ####

## Create a link to calculate time and distances, 
## between (r)elease and (s)tranding
driftSpatial_r <- 
  drift %>% 
  dplyr::select(campaign,id,(ends_with("r"))) %>% 
  sf::st_as_sf(coords = c("long_r", "lat_r"), crs = 4326)

driftSpatial_s <- 
  drift %>% 
  dplyr::select(campaign,id,(ends_with("s"))) %>% 
  sf::st_as_sf(coords = c("long_s", "lat_s"), crs = 4326)

## Calculate distance and time between 'r' and 's'
driftDistTime <- 
  drift %>% 
  dplyr::select(campaign, id, state, zone, 
                date_r, date_s, 
                lat_r, long_r, 
                lat_s, long_s) %>% 
  dplyr::mutate(dist = sf::st_distance(
    driftSpatial_r$geometry, driftSpatial_s$geometry, by_element = T)) %>% 
  dplyr::mutate(time_lag = (date_s) - (date_r))

#
## Summaries

driftSummary_ID_Campaign <- 
  driftDistTime %>% 
  group_by(campaign, id) %>% 
  dplyr::summarise(n = n(),
                   n_percentage = round(((n()/33)*100), digits = 1),
                   meanDist = mean((as.numeric(dist)/1000)),
                   sdDist = sd((as.numeric(dist)/1000)),
                   minDist = min((as.numeric(dist)/1000)),
                   maxDist = max((as.numeric(dist)/1000)),
                   meanTime = mean(as.numeric(time_lag)),
                   sdTime = sd(as.numeric(time_lag)),
                   minTime = min(as.numeric(time_lag)),
                   maxTime = max(as.numeric(time_lag)))

driftSummary_State <- 
  driftDistTime %>% 
  group_by(state) %>% 
  dplyr::summarise(n = n(),
                   n_percentage = round(((n()/297)*100), digits = 1),
                   meanDist = mean((as.numeric(dist)/1000)),
                   sdDist = sd((as.numeric(dist)/1000)),
                   minDist = min((as.numeric(dist)/1000)),
                   maxDist = max((as.numeric(dist)/1000)),
                   meanTime = mean(as.numeric(time_lag)),
                   sdTime = sd(as.numeric(time_lag)),
                   minTime = min(as.numeric(time_lag)),
                   maxTime = max(as.numeric(time_lag)))

## Summaries from less than 10 days drifting

driftSummary_ID_Campaign_10 <- 
  driftDistTime %>% 
  dplyr::filter(time_lag < 10) %>% 
  group_by(campaign, id) %>% 
  dplyr::summarise(n = n(),
                   n_percentage = round(((n()/33)*100), digits = 1),
                   meanDist = mean((as.numeric(dist)/1000)),
                   sdDist = sd((as.numeric(dist)/1000)),
                   minDist = min((as.numeric(dist)/1000)),
                   maxDist = max((as.numeric(dist)/1000)),
                   meanTime = mean(as.numeric(time_lag)),
                   sdTime = sd(as.numeric(time_lag)),
                   minTime = min(as.numeric(time_lag)),
                   maxTime = max(as.numeric(time_lag)))

driftSummary_State_10 <- 
  driftDistTime %>% 
  dplyr::filter(time_lag < 10) %>% 
  group_by(state) %>% 
  dplyr::summarise(n = n(),
                   n_percentage = round(((n()/297)*100), digits = 1),
                   meanDist = mean((as.numeric(dist)/1000)),
                   sdDist = sd((as.numeric(dist)/1000)),
                   minDist = min((as.numeric(dist)/1000)),
                   maxDist = max((as.numeric(dist)/1000)),
                   meanTime = mean(as.numeric(time_lag)),
                   sdTime = sd(as.numeric(time_lag)),
                   minTime = min(as.numeric(time_lag)),
                   maxTime = max(as.numeric(time_lag)))

driftSummary_Zone_10 <- 
  driftDistTime %>% 
  dplyr::filter(time_lag < 10) %>% 
  dplyr::group_by(zone) %>% 
  dplyr::summarise(n = n(),
                   meanDist = mean((as.numeric(dist)/1000)),
                   sdDist = sd((as.numeric(dist)/1000)),
                   minDist = min((as.numeric(dist)/1000)),
                   maxDist = max((as.numeric(dist)/1000)),
                   meanTime = mean(as.numeric(time_lag)),
                   sdTime = sd(as.numeric(time_lag)),
                   minTime = min(as.numeric(time_lag)),
                   maxTime = max(as.numeric(time_lag)))

## Plot time x days
plot(driftDistTime$time_lag, driftDistTime$dist)
plot(log(as.numeric(driftDistTime$time_lag)), log(driftDistTime$dist))

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

## Check for unique ids
pontoporia %>% dplyr::group_by(id) %>% count()

## Start downloading data

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

## Start downloading data
# The path of the file will be returned as a variable (ncfile)
ncfile <- wf_request(user = "2088", 
                     request = request, 
                     transfer = TRUE, 
                     path = "~", 
                     verbose = FALSE)

#################