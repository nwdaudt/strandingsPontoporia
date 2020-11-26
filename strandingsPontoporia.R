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
# library(ecmwfr)
# library(ncdf4)
# library(reticulate)
# library(mgcv)

# Open Pontoporia stranding data, drift experiment data, and isobath ####

#
## Open Pontoporia dataset
pontoporia <- as.data.frame(
  readxl::read_xlsx("./data/Pontoporia PMP 2015_08_24 a 2020_06_11 SC_PR_SP_RJ.xlsx", 
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

## Create column "date", "back_date", and "zone"
## for future environment variables gathering
pontoporia <- 
  pontoporia %>% 
  dplyr::mutate(date = lubridate::as_date(date_hour)) %>% 
  dplyr::mutate(back_date = lubridate::as_date(ifelse(cod_decomposition == 2, 
                                                      date - 1, 
                                                      date - 6))) %>% 
  dplyr::mutate(zone = 
                  ifelse(lat > -23.75, "1", 
                         ifelse(-23.75 > lat & lat > -26,"2", "3")))

# PS - "zones" were defined based on summaries related to mean drifting 
# distances from the 'drift_experiment'.
## See next steps, and summary df 'driftSummary_Zone_10'

## Transform df into a geospatial feature
pontoporiaSpatial <- 
  sf::st_as_sf(pontoporia, coords = c("long", "lat"), crs = 4326)

#
## Open drift_experiment dataset
drift <- utils::read.csv("./data/drift_experiment_data.csv", sep = ";")

## Filter and set up Date columns
drift <- 
  drift %>% 
  dplyr::filter(lat_s != is.na(lat_s)) %>% 
  dplyr::filter(date_s != is.na(date_s))
  
drift$date_r <- lubridate::dmy(drift$date_r)
drift$date_s <- lubridate::dmy(drift$date_s)

## Create "zone" column - In this case, means spatial-similar release stations 
drift <- 
  drift %>% 
  dplyr::mutate(
    zone = 
      ifelse(lat_r > -23.8, "1", 
             ifelse(-23.8 > lat_r & lat_r > -24.4,"2", 
                    ifelse(-24.4 > lat_r & lat_r > -26, "3", "4"))))

drift$zone <- as.factor(drift$zone)

## Transform df into a geospatial feature
driftSpatial <- 
  drift %>% sf::st_as_sf(coords = c("long_r", "lat_r"), crs = 4326)

#
## Open isobath dataset
isobath <- sf::st_read("./bathymetry/linhas_lim_200m.shp")

# Mapview - quick spatial check on strandings and drifts ####

## Mapview Pontoporia strandings
pontoporiaMapview <- # Run this line to add visualization into a df
  mapview::mapview(pontoporiaSpatial, cex = 0.2)

## Mapview Release Stations from drift_experiment
driftReleaseStationsMapview <- # Run this line to add visualization into a df
  mapview::mapview(driftSpatial, zcol = "zone")

## Mapview isobath 30m
isobath30Mapview <- # Run this line to add visualization into a df
  mapview::mapview(dplyr::filter(isobath, ELEV == 30))

## Visualize them all
driftReleaseStationsMapview + isobath30Mapview + pontoporiaMapview

rm(pontoporiaSpatial, driftSpatial, 
  driftReleaseStationsMapview, isobath30Mapview, pontoporiaMapview)

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
    driftSpatial_r$geometry, driftSpatial_s$geometry, by_element = TRUE)) %>% 
  dplyr::mutate(time_lag = (date_s) - (date_r))

#
## Summaries

driftSummary_ID_Campaign <- 
  driftDistTime %>% 
  dplyr::group_by(campaign, id) %>% 
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
  dplyr::group_by(state) %>% 
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
  dplyr::group_by(campaign, id) %>% 
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
  dplyr::group_by(state) %>% 
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

driftSummary_Zone_10 <-  ## This was the basis for "zones" in 'pontoporia' df.
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
plot(log(as.numeric(driftDistTime$time_lag)), log(driftDistTime$dist))

rm(driftSpatial_r, driftSpatial_s, 
   driftSummary_ID_Campaign, driftSummary_ID_Campaign_10, 
   driftSummary_State, driftSummary_State_10)
# Keep 'driftSummary_Zone_10' for easiest check, if needed

# Effort - monitored beach segments & Sectors polygon ####
ef_SP <- read.csv2("data/Effort_SP_aug2019_jul_2020.csv",
                   header = TRUE, encoding = "UTF-8")
ef_SC_PR <- read.csv2("data/Effort_SC_PR_aug2019_jul_2020.csv",
                      header = TRUE, encoding = "UTF-8")
ef_SP_PR_SC <- read.csv2("data/Effort_SC_PR_SP_aug2015_aug_2019.csv",
                         header = TRUE, encoding = "UTF-8")

eff <- rbind(ef_SP,ef_SC_PR,ef_SP_PR_SC)

rm(list = ls(pattern = "ef_"))

# split data and hour
eff$initialDate <- lubridate::dmy(
  sapply(strsplit(as.character(eff$Data.Hora.início), " "), "[", 1))
eff$initialTime <- 
  sapply(strsplit(as.character(eff$Data.Hora.início), " "), "[", 2)

# Removing unused columns
eff[c(2:3,10:12,15:16)] <- list(NULL) 

# Rename columns
colnames(eff) <- c("code","state","city","beach","stretch","type","strategy",
                   "initialLat","initialLong",
                   "complete", "initialDate", "initialTime")

# write.table(eff, "./data/Effort_complete.txt", dec = ".", sep = ";")

# Create a spatial point polygon
effSpatial <- 
  eff %>% 
  sf::st_as_sf(coords = c("initialLong","initialLat"), 
               crs = 4674)

#
## Open shapefiles (shp) with beach monitoring lines

## Create a list with all subdirectories containing the shp
ff <- as.list(list.files(path = ".", pattern = "linha.shp$", 
                         recursive = TRUE, full.names = TRUE))

## Function to open the shp in subdirectories
open_shp <- function(ff){
  linha <- sf::read_sf(ff[i])
  linha
}

## Loop creating a list with all shp
lines <- list()
for (i in 1:length(ff)) {
  lines[[i]] <- open_shp(ff)
}

## Merge all beach monitoring lines into one shp
merged.lines <- do.call(rbind, lines)

# sf::st_write(merged.lines,"./merged_lines.shp")  ## saving the shp

merged.lines <- 
  dplyr::filter(merged.lines, compriment != 0) %>% 
  dplyr::mutate(id = row_number()) %>% 
  sf::st_cast("MULTILINESTRING")

merged.lines <- 
  merged.lines %>% 
  sf::st_cast("LINESTRING", warn = TRUE, do_split = TRUE)

# mapview::mapview(merged.lines)

#
## Create a 30 latitudinal-band Sector polygon,
## which will be the basis for final analysis

# Latitudinal definition
n <- diff(sf::st_bbox(merged.lines)[c(2, 4)])/30
# Longitudinal definition
m <- diff(sf::st_bbox(merged.lines)[c(1, 3)])

## Create the sectors based on 'merged.lines' extend
sectors <- sf::st_make_grid(merged.lines, cellsize = c(m, n))

## Set the correct spatial attributes for "sectors" layer
# Define sectors as single feature
sectors <- sf::st_sf(sectors)

# Create an identifier for each sector
sectors$id <- 1:nrow(sectors)

# Define sectors as a multipolygon
sectors <- 
  sectors %>% 
  sf::st_cast("MULTIPOLYGON")

# Create a multiline feature based on "sectors" id limits
sectors_lines <- sf::st_cast(sectors, "MULTILINESTRING", group_or_split = FALSE)

# mapview::mapview(sectors_lines)

#
## Look for intersections between the 30 latitudinal-band 'sectors' and
## the monitored beaches 'merged.lines'
## Cut features and calculate each segment length, for calculate effort (km)

# Calculate intersection between 'merged.lines' and 'sectors'
line_intersection_join <- 
  sf::st_intersection(merged.lines, sectors)

## Define each line segment as an unique feature
line_intersection_join <- 
  line_intersection_join %>% 
  sf::st_cast("LINESTRING")

# Add a new id for each segment line "uid"
line_intersection_join$uid <- 1:nrow(line_intersection_join)

# mapview::mapview(line_intersection_join) + sectors_lines

## Calculate individual segment lengths
line_intersection_join$length <- sf::st_length(line_intersection_join)

## Merge the beach transect into the starting point of each monitoring
## Finds the closest line to each point and adds the line id as a new column
effSpatial$closestBeach <- sf::st_nearest_feature(effSpatial, 
                                                  line_intersection_join)
effSpatial <- merge(effSpatial, sf::st_drop_geometry(line_intersection_join), 
                    by.x = "closestBeach", by.y = "uid", all.y = F)

## checking
mapview(filter(line_intersection_join, 
               beach_name == "Vila/Itapirubá Norte")) + sectors_lines + effSpatial[1:1000,]
mapview(filter(line_intersection_join, 
               uid == 5)) + sectors_lines + filter(effSpatial, closestBeach == 5)

# Environment data collection from ERA5 ####
##
## This step was based on a Python routine 
## files = {apiRequest.py} + {requirements.txt}
##

###################