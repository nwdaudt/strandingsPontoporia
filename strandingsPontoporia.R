# Code for consultancy of Pontoporia strandings

# Libraries ####
library(mgcv)
library(tidyverse)
library(mapview)
library(sf)

# Open file and visualize ###
# rm(list = ls())
setwd("C:/Users/jessicas/Downloads/")
pontoporia = readxl::read_xlsx("./Pontoporia PMP 2015_08_24 a 
                               2020_06_11 SC_PR_SP_RJ.xlsx", sheet = 2)

pontoporia <- 
  pontoporia %>% 
  select(Código, `Data/Hora`, `Ponto - Lat`, `Ponto - Long`)
###
names(pontoporia) <- c("cod", "date_hour", "lat", "long")

## transform dataframe into a geospatial feature:
pontoporia.sp <- st_as_sf(pontoporia, coords = c("long", "lat"), crs = 4326)
# mapview(pontoporia.sp)  # visualize
