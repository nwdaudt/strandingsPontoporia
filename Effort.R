########################

##Manipulation of effort data

#rm(list = ls())
library(sf)
library(mapview)
library(plyr)
library(lwgeom)
library(lubridate)
library(tidyverse)

#01- Open data effort
ef_SP<-read.csv2("data/Effort_SP_aug2019_jul_2020.csv",header = T, encoding = "UTF-8")
ef_SC_PR<-read.csv2("data/Effort_SC_PR_aug2019_jul_2020.csv",header = T, encoding = "UTF-8")
ef_SP_PR_SC<-read.csv2("data/Effort_SC_PR_SP_aug2015_aug_2019.csv",header = T, encoding = "UTF-8")

eff <- rbind(ef_SP,ef_SC_PR,ef_SP_PR_SC)
dim(eff)

#split data and hour
eff$EndDate <- sapply(strsplit(as.character(eff$Data.Hora.término), " "), "[", 1)
eff$EndTime <- sapply(strsplit(as.character(eff$Data.Hora.término), " "), "[", 2)

eff$InitialDate <- sapply(strsplit(as.character(eff$Data.Hora.início), " "), "[", 1)
eff$InitialTime <- sapply(strsplit(as.character(eff$Data.Hora.início), " "), "[", 2)

rm(list = ls(pattern = "ef_"))

#date format
eff$InitialDate<-dmy(eff$InitialDate) 
eff$EndDate<-dmy(eff$EndDate)

#removing some columns
eff[c(2:3,10:12,15:16,18:19)] <- list(NULL) 

#rename column names
cols <- c("code","state","city","beach","tretch","type","strategy",
          "initiallat","initiallong",
          "complete", "InitialDate", "InitialTime")
colnames(eff) <- cols
# checking:
head(eff)

# write.table(eff,"data/Effort_complete.txt",dec=".",sep=";")

#03 - CRIATION OF SPATIAL POINT POLYGON
effSpatial <- eff %>% 
  sf::st_as_sf(coords = c("initiallong","initiallat"), 
               crs =4674) ## J: crs was wrong
mapview(effSpatial[1:1000,])

## 04 - OPEN DATA AND MERGE ALL SHAPEFILES INTO ONE ####
# create a list with all subdirectories containing the shapefiles:
ff <- as.list(list.files(path=".", pattern="linha.shp$", 
                         recursive=TRUE, full.names=TRUE))

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
# st_write(merged.lines,"./merged_lines.shp")  ## saving the shp

merged.lines = 
  filter(merged.lines, compriment != 0) ## J: clean empty geometries that 
                                      ## cause error to the visualization
mapview(merged.lines)

## 02 - CREATE A NEW SPATIAL FEATURE BASED ON 30 LATITUDINAL SECTORS #### 

n<-diff(st_bbox(merged.lines)[c(2, 4)])/30 #Latitudianl definition 
m<-diff(st_bbox(merged.lines)[c(1, 3)])    #Longitudinal definition

## create 30 latitudinal sectors based on merged.lines extend
sectors<-st_make_grid(merged.lines, cellsize = c(m,n))

### Set the correct spatial attributes for "sectors" layer:
sectors = st_sf(sectors)              ## define sectors as  single feature
sectors$id <- 1:nrow(sectors)         ## create an identifier for each sector
sectors <- 
  sectors %>% st_cast("MULTIPOLYGON")  ## define sectors as a multipolygon

## create a multiline feature based on "sectors" id limits:
sectors_lines <- st_cast(sectors, "MULTILINESTRING", group_or_split = FALSE)
# mapview(sectors_lines)

## 03 - LOOK FOR INTERSECTION BETWEEN THE 30 SECTORS AND ALL MONITORED LINES, CUT IN THIS POINT AND CALCULATE THE SEGMENT LENGTH #### 
# calculating intersection between merged.lines and the individual sectors
line_intersection_join <- 
  st_intersection(merged.lines,sectors) ## calculate the intersection

## id sector for each segmented line
line_intersection_join<-
  line_intersection_join %>% st_cast("LINESTRING") ## define each line 
                                                  ##segment as an unique feature 
line_intersection_join$uid <- 1:nrow(line_intersection_join)## add an id
mapview(line_intersection_join)+sectors_lines     ## view the product

## Calculate individual segment lengths
line_intersection_join$length <- st_length(line_intersection_join)
mapview(line_intersection_join)+sectors_lines

## Merge the beach transect into the starting point of each monitoring: ####

## finds the closest line to each point and adds the line id as a new column:
effSpatial$closestBeach <- st_nearest_feature(effSpatial, line_intersection_join)   
effSpatial <- merge(effSpatial, st_drop_geometry(line_intersection_join), 
                    by.x = "closestBeach", by.y = "uid", all.y = F)

## checking:
mapview(filter(line_intersection_join, beach_name == "Vila/Itapirubá Norte")) + sectors_lines + effSpatial[1:1000,]
mapview(filter(line_intersection_join, uid == 5)) + sectors_lines + filter(effSpatial, closestBeach == 5)

