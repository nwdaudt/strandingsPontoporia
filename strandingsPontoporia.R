# Code for: 
## Fransiscana dolphin 'Pontoporia blainvillei' stranding patterns in 
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

rm(list = ls())

# Libraries ####
library(plyr)
library(reshape2)
library(tidyverse)
library(mapview)
library(sf)
library(ggspatial)
library(janitor)
library(KernSmooth)
library(raster)
library(maptools)

# local functions: ##
source("./functionEnvData.R")

################################################################################
############################## Dataset manipulation ############################
################################################################################

##----------------------------------------------------------------------------##
##                              Stranding dataset                             ##
##----------------------------------------------------------------------------##

## Open stranding dataset
pontoporia <- as.data.frame(
  readxl::read_xlsx("./data/Pontoporia_PMP_2015_08_24_a_2020_06_11 SC_PR_SP_RJ.xlsx", 
                    sheet = 2))

## Filter columns
pontoporia <- 
  pontoporia %>% 
  dplyr::select(`Identificador do indivíduo`, Estado, Praia, Trecho, 
                `Estratégia do trecho`, `Tipo do monitoramento`, `Data/Hora`, 
                `Ponto - Lat`, `Ponto - Long`, `Condição da carcaça`, 
                `OFAI - Sexo`)

## Rename columns and levels
names(pontoporia) <- c("id_individual", "state", "beach",
                       "stretch", "stretch_scheme", "monitoring_type",
                       "date_hour", "lat", "long", "cod_decomposition", "sex")

pontoporia$stretch_scheme <- as.factor(pontoporia$stretch_scheme)
levels(pontoporia$stretch_scheme) <- list(daily = "Diário", 
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

## Remove stretch_scheme == "call", monitoring_type == "call" & 
## stretch_scheme == "weekly", monitoring_type == "call" 
## & stretch_scheme == "fortnightly" and "date_hour" column;
## Create columns date, year, month, day, week, semester, year_n, season and fortnight_month;
## Filter survey effort from date > "September2015" and <= "2020-06-07".
pontoporia <- 
  pontoporia %>%
  dplyr::filter(stretch_scheme!= "call") %>% 
  dplyr::filter(!((monitoring_type == "call" & stretch_scheme == "weekly") |
                    (monitoring_type == "call" & 
                       stretch_scheme == "fortnightly"))) %>%
  dplyr::mutate(date = lubridate::as_date(date_hour)) %>% 
  dplyr::select(- date_hour) %>% 
  dplyr::filter(date > "2015-08-31" & date <= "2020-06-07") %>%
  dplyr::mutate(year = lubridate::year(date), 
                month = lubridate::month(date), 
                day = lubridate::day(date),
                week = lubridate::week(date),
                semester = lubridate::semester(date, with_year = TRUE),
                season =
                  ifelse(month >= 1 & month <=3, "summer",
                         ifelse(month >= 4 & month <= 6, "autumn",
                                ifelse(month >= 7 & month <= 9, "winter", "spring"))),
                year_n = 
                  ifelse(date > "2015-08-31" & date < "2016-09-01", "Year1",
                         ifelse(date > "2016-08-31" & date < "2017-09-01", "Year2",
                                ifelse(date > "2017-08-31" & date < "2018-09-01", "Year3",
                                       ifelse(date > "2018-08-31" & date < "2019-09-01", "Year4",
                                              "Year5")))),
                fortnight_month = as.numeric(ifelse(day <= 15, "1", "2")))

## Create a continuous id for fortnight periods through the year
fortnight_df <- 
  pontoporia %>% 
  dplyr::select(id_individual, month, fortnight_month)

fortnight_df <- 
  fortnight_df %>% 
  dplyr::group_by(month, fortnight_month) %>% 
  dplyr::mutate(fortnight_id = dplyr::cur_group_id()) %>%
  dplyr::ungroup() %>% 
  dplyr::select(id_individual, fortnight_id)

## Join 'fortnight_df' into 'pontoporia'
pontoporia <- 
  dplyr::left_join(pontoporia, fortnight_df, by = "id_individual")

## Create column back_date and zone for environment variables gathering
pontoporia <- 
  pontoporia %>% 
  dplyr::mutate(back_date = 
                  lubridate::as_date(ifelse(cod_decomposition == 2,date - 1,date - 6))) %>% 
  dplyr::mutate(zone = 
                  ifelse(lat > -23.75, "1", 
                         ifelse(lat < -23.75 & lat > -26.2, "2", "3"))) 

#- - - - - - -
## Zones were defined based on summaries related to mean drifting 
## distances from the 'drift_experiment'.
## See in general results 'table4'.
#- - - - - - -

## Assign mesoregions

# Transform 'pontoporia' into a geospatial feature
pontoporia <-
  pontoporia %>%
  dplyr::mutate(long1 = long)%>%
  dplyr::mutate(lat1 = lat)

pontoporiaSpatial <- 
  pontoporia %>%
  sf::st_as_sf(coords = c("long1","lat1"), crs = 4326)

# Read mesoregions shapefile
mesoregions <- sf::read_sf("./data/shp_mesoregiao/poligonos_meso.shp")

# Filtering and selecting to make it lighter to run
mesoregions <- 
  mesoregions %>% 
  dplyr::filter(PROF == "0 - 20") %>% 
  dplyr::select(MESORREGIA, geometry) %>% 
  sf::st_transform(crs = 4326)

# mapview::mapview(mesoregions)

# Merge 'pontoporiaSpatial' with 'mesoregions'
pontoporiaSpatial <- sf::st_join(pontoporiaSpatial, mesoregions) 

# Check if there is any 'NA' coerced after joining attributes
# plyr::count(is.na(pontoporiaSpatial$MESORREGIA))

# mapview::mapview(pontoporiaSpatial, zcol = "MESORREGIA")

## Manual assignment of "MESORREGIA" (mesoregion) for the 'NA' records
# South of Tubarao city, inside Babitonga Bay, and
# id_individual == "156651" & "167513" (North Sao Paulo), 
# were not assigned to any 'mesoregion' because in a next step they 
# will be dropped out.

# Close to Barra do Sai
pontoporiaSpatial$MESORREGIA[pontoporiaSpatial$id_individual == "061824"] <- "Litoral Paranaense"

# Border between Central/North SP regions
pontoporiaSpatial$MESORREGIA[pontoporiaSpatial$id_individual == "184419" |
                               pontoporiaSpatial$id_individual == "159986"] <- "Litoral Norte Paulista"

## Return spatial feature to data.frame format
pontoporia <- 
  pontoporiaSpatial %>% 
  as.data.frame() %>% 
  dplyr::select(- geometry)

## Remove records from state == "Rio de Janeiro", cod_decomposition == "5",
## and date == "NA".
pontoporia <- 
  pontoporia %>% 
  dplyr::filter(state != "Rio de Janeiro") %>% 
  dplyr::filter(cod_decomposition != 5) %>% 
  dplyr::filter(!is.na(date))

## Remove records distant more than 300 m from the coast line

# Open shapefiles (shp) of beach monitoring lines
# Create a list with all subdirectories containing the shp
ff <- as.list(list.files(path = "./data", pattern = "linha.shp$", 
                         recursive = TRUE, full.names = TRUE))

# Function to open the shp in subdirectories
open_shp <- function(ff){
  linha <- sf::read_sf(ff[i])
  linha
}

# Loop creating a list with all shp
lines <- list()
for (i in 1:length(ff)) {
  lines[[i]] <- open_shp(ff)
}

# Merge all beach monitoring lines into one shp
originalStretches <- do.call(rbind, lines)

# Remove streches with strategy == "noneeffort", compriment == "0",  
# and beach_name == "Ponta do Itaguá"; create an id for streches.
originalStretches <- 
  originalStretches %>%
  dplyr::filter(stretch_st != "noneffort") %>% 
  dplyr::filter(compriment != 0) %>%
  dplyr::rename(original_length = compriment) %>%
  dplyr::filter(beach_name != "Ponta do Itaguá") %>%
  dplyr::rename(beach = beach_name) %>%
  dplyr::rename(stretch = stretch_na) %>%
  dplyr::mutate(id_original = dplyr::row_number()) %>% 
  sf::st_cast("MULTILINESTRING")

# Create a buffer of 300 m around the beach monitoring lines
originalStretches <- 
  sf::st_transform(originalStretches, crs = 31982)
buffer <- 
  sf::st_buffer(originalStretches, dist = 300)

# Transform 'pontoporia' into a geospatial feature
pontoporia <-
  pontoporia %>%
  dplyr::mutate(long1 = long)%>%
  dplyr::mutate(lat1 = lat)

pontoporiaSpatial <- 
  pontoporia %>%
  sf::st_as_sf(coords = c("long1","lat1"), crs = 4326) %>%
  sf::st_transform(pontoporiaSpatial, crs = 31982) # Same 'crs' as 'originalStreches' 

# Remove records outside the buffer
pontoporiaSpatial$on_buffer <- 
  lengths(sf::st_within(pontoporiaSpatial, buffer))

pontoporiaSpatial <-
  pontoporiaSpatial %>%  
  dplyr::filter(on_buffer > 0) %>% 
  dplyr::select(- on_buffer)

# mapview::mapview(buffer) + pontoporiaSpatial

## Clean environment
rm(list = "buffer", "ff", "lines", "i")

##----------------------------------------------------------------------------##
##                             Survey effort dataset                          ##
##----------------------------------------------------------------------------##

## Open survey effort dataset
ef_SP <- read.csv2("data/Effort_SP_aug2019_jul_2020.csv", 
                   header = TRUE, encoding = "UTF-8")
ef_SC_PR <- read.csv2("data/Effort_SC_PR_aug2019_jul_2020.csv", 
                      header = TRUE, encoding = "UTF-8")
ef_SP_PR_SC <- read.csv2("data/Effort_SC_PR_SP_aug2015_aug_2019.csv", 
                         header = TRUE, encoding = "UTF-8")

eff <- rbind(ef_SP, ef_SC_PR, ef_SP_PR_SC)

rm(list = ls(pattern = "ef_"))

## Split data and hour
eff$date <- 
  lubridate::dmy(
    sapply(strsplit(as.character(eff$Data.Hora.início), " "), "[", 1))

## Removing unused columns
eff[c(2:3, 10:12, 15:16)] <- list(NULL)

## Rename columns and levels
colnames(eff) <- c("code", "state", "city", "beach", "stretch", "type", 
                   "stretch_scheme", "initial_lat", "initial_long", "complete",
                   "date")

eff$stretch_scheme <- as.factor(eff$stretch_scheme)
levels(eff$stretch_scheme) <- list(daily = "Diário", 
                                   weekly = "Semanal", 
                                   fortnightly = "Diário 15", 
                                   call = "Acionamento")

eff$type <- as.factor(eff$type)
levels(eff$type) <- list(boat = "Embarcado", 
                         land = "Terrestre")

eff$complete <- as.factor(eff$complete)
levels(eff$complete) <- list(yes = "Sim", 
                             no = "Não")

## Filter survey effort from date > "September2015" and <= "2020-06-07";
## Remove stretch_scheme == "call" and complete == "NA";
## Create columns year, month, day, week, semester, season, year_n and fortnight_month
eff <-
  eff %>%
  dplyr::filter(date > "2015-08-31" & date <= "2020-06-07") %>%
  dplyr::filter(stretch_scheme != "call") %>% 
  dplyr::filter(complete != "NA") %>%
  dplyr::mutate(year = lubridate::year(date), 
                month = lubridate::month(date), 
                day = lubridate::day(date), 
                week = lubridate::week(date),
                semester = lubridate::semester(date, with_year = TRUE),
                season =
                  ifelse(month >= 1 & month <= 3, "summer",
                         ifelse(month >= 4 & month <= 6, "autumn",
                                ifelse(month >= 7 & month <= 9, "winter", "spring"))),
                year_n = 
                  ifelse(date > "2015-08-31" & date < "2016-09-01", "Year1",
                         ifelse(date > "2016-08-31" & date < "2017-09-01", "Year2",
                                ifelse(date > "2017-08-31" & date < "2018-09-01", "Year3",
                                       ifelse(date > "2018-08-31" & date < "2019-09-01", "Year4",
                                              "Year5")))),
                fortnight_month = ifelse(day <= 15, "1", "2"))

## Create a continuous id for fortnight periods through the year
fortnight_df_eff <- 
  eff %>% 
  dplyr::select(code, month, fortnight_month)

fortnight_df_eff <- 
  fortnight_df_eff %>% 
  dplyr::group_by(month, fortnight_month) %>% 
  dplyr::mutate(fortnight_id = dplyr::cur_group_id()) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(code, fortnight_id)

## Join 'fortnight_df_eff' into 'eff'
eff <- dplyr::left_join(eff, fortnight_df_eff, by = "code")

##----------------------------------------------------------------------------## 
##               Standardize the numbers and names of beach surveyed          ##
##                         in eff and originalStretches                       ##
##----------------------------------------------------------------------------##

## Removing excess of spaces between characters
eff$beach <- gsub("\\s+", " ", eff$beach)
eff$stretch <-gsub("\\s+", " ", eff$stretch)

originalStretches$beach <- gsub("\\s+", " ", originalStretches$beach)
originalStretches$stretch <- gsub("\\s+", " ", originalStretches$stretch)

## Identify beaches and stretches in 'eff' 
## that are not in 'originalStretches' and vice verse
identify1 <- dplyr::setdiff(eff$beach, originalStretches$beach)
identify2 <- dplyr::setdiff(originalStretches$beach, eff$beach)
identify3 <- dplyr::setdiff(eff$stretch, originalStretches$stretch)
identify4 <- dplyr::setdiff(originalStretches$stretch, eff$stretch)

## Select and view the beaches and stretches in the above conditions
## Transform 'eff' into a geospatial feature
effSpatial <- 
  eff %>% 
  sf::st_as_sf(coords = c("initial_long", "initial_lat"), crs = 4326)

select1 <-
  effSpatial %>% 
  dplyr::filter(beach == "Bal. Barra do Sul" | beach == "Itararé")

select3 <-
  effSpatial %>% 
  dplyr::filter(stretch == "antigo D15- Ilha Comprida" | 
                  stretch == "Antigo Superagui Sul e trapiche-Rio" | 
                  stretch == "Ipanema" | stretch == "Matinhos 1" | 
                  stretch == "Matinhos 2" | stretch == "Pontal do Sul" | 
                  stretch == "Praia da Barra N" | stretch == "Praia da Barra S" | 
                  stretch == "Praia de Garopaba N" | stretch =="Praia de Garopaba S" | 
                  stretch == "Praia de Leste")

# mapview::mapview(originalStretches) + select1
# mapview::mapview(originalStretches) + select3

## Verify duplicated beach names
eff_BeachNames <-
  effSpatial %>%
  dplyr::group_by(beach, stretch, city, state) %>% 
  dplyr::summarise(n = n()) %>% 
  dplyr::group_by(beach) %>%
  dplyr::filter(n() > 1)

## Select and view the duplicated beaches
# select_duplicated_BeachNames <-
#   originalStretches %>%
#   dplyr::filter(beach == "...") # insert the beach name in "..." to visualize

# mapview::mapview(select_duplicated_BeachNames)

#---------------
# Conclusions:

# C1: Same beaches with different names in 'eff' and 'originalStretches':
# "Bal. Barra do Sul" == ""Barra do Sul - 2 - 395 - 396 - 67" and
# "Itararé" == "Praia do José Menino, Praia do Gonzaga, Praia do Boqueirão,
# Praia do Embaré, Praia Aparecida, Ponta da Praia".

# C2: Different beaches identified with the same name in 'eff' and 
# 'originalStretches': "Armação", "Brava","Camburizinho","Estaleiro","Pereque",
# "Praia Brava","Praia Grande".

# C3: Beaches with two or more stretches in 'eff' and one in 'originalStretches': 
# "Praia da Barra N" and "Praia da Barra S" == "Praia da Barra";
# "Praia de Garopaba N" and "Praia de Garopaba S" == "Praia de Garopara";
# "antigo D15- Ilha Comprida" and "Ilha Comprida" == "Ilha Comprida";
# "Ipanema","Matinhos,Matinhos 2", "Pontal do Sul","Pontal do Sul/ Flamingo" and
# "Praia de Leste" == "Pontal do Sul/ Flamingo".

# C3.1: The stretch name "antigo D15- Ilha Comprida" was the unique stretch used
# for "Ilha Comprida" beach until 2017/03/23. Since then it was 
# rename to "Ilha Comprida".

# C3.2: The stretch "Pontal do Sul/ Flamingo" was used from spetember2015 to 
# 2019/08/19.Since then it was split in five new strecthes: "Ipanema","Matinhos,
# Matinhos 2", "Pontal do Sul" and "Praia de Leste".

# C4:The stretch name "Antigo Superagui Sul e trapiche-Rio" was the unique 
# stretch used for "Ilha do Superagui" beach until 2017/10/21. Since then it was
# split in two new stretches: "Superagui Trapiche-Rio Novo" and 
# "Superagui Sul Novo.
#---------------

## Standardize beaches and stretches in 'eff' and 'originalStretches' according 
## to above conclusions

## C1: rename beaches "Bal. Barra do Sul" and "Itararé" as in 'originalStreches'
effStandardize <-
  eff %>%
  dplyr::mutate(beach = recode_factor(beach, 
                                      "Bal. Barra do Sul" = "Barra do Sul - 2 - 395 - 396 - 67", 
                                      "Itararé" = "Praia do José Menino, Praia do Gonzaga, Praia do Boqueirão, Praia do Embaré, Praia Aparecida, Ponta da Praia"))

## C2: rename different beaches identified with the same name in 'eff' and 
## 'originalStreches'
effStandardize$beach <- as.character(effStandardize$beach)
effStandardize$city <- as.character(effStandardize$city)

effStandardize <-
  effStandardize %>%
  dplyr::mutate(beach = 
                  ifelse(beach == "Armação" & city == "Florianópolis", "ArmaçãoFloripa",
                         ifelse(beach == "Armação" & city == "Ilhabela", "ArmaçãoIlhaBela",
                                ifelse(beach == "Armação" & city == "Penha", "ArmaçãoPenha",
                                       ifelse(beach == "Brava" & city == "Florianópolis", "BravaFloripa",
                                              ifelse(beach == "Brava" & city == "Balneário Camboriú", "BravaItajaí",
                                                     ifelse(beach == "Brava" & city == "Balneário Camboriú, Itajaí", "BravaItajaí",
                                                            ifelse(beach == "Camburizinho" & city == "Guarujá", "CamburizinhoGuarujá",
                                                                   ifelse(beach == "Camburizinho" & city == "São Sebastião", "CamburizinhoSebastião",
                                                                          ifelse(beach == "Estaleiro" & city == "Balneário Camboriú", "EstaleiroCamburiú",
                                                                                 ifelse(beach == "Estaleiro" & city == "Ubatuba", "EstaleiroUbatuba",
                                                                                        ifelse(beach == "Pereque" & city == "Guarujá", "PerequeGarujá",
                                                                                               ifelse(beach == "Pereque" & city == "Ilhabela", "PerequeIlhaBela",
                                                                                                      ifelse(beach == "Pereque" & city == "Porto Belo", "PerequePortoBelo",
                                                                                                             ifelse(beach == "Praia Brava" & city == "Ilhabela", "Praia BravaIlhaBela",
                                                                                                                    ifelse(beach == "Praia Brava" & city == "Matinhos", "Praia BravaMatinhos",
                                                                                                                           ifelse(beach == "Praia Grande" & city == "Praia Grande","Praia Grande",
                                                                                                                                  ifelse(beach == "Praia Grande" & city == "Penha","Praia GrandePenha", 
                                                                                                                                         beach))))))))))))))))))

originalStretchesStandardize <-
  originalStretches %>%
  dplyr::mutate(beach = 
                  ifelse(beach == "Armação" & id_original == 282, "ArmaçãoFloripa",
                         ifelse(beach == "Armação" & id_original ==  89, "ArmaçãoIlhaBela",
                                ifelse(beach == "Armação" & id_original == 339, "ArmaçãoPenha",
                                       ifelse(beach == "Brava" & id_original == 287, "BravaFloripa",
                                              ifelse(beach == "Brava" & id_original == 334, "BravaItajaí",
                                                     ifelse(beach == "Brava" & id_original == 371, "BravaItajaí",
                                                            ifelse(beach == "Camburizinho" & id_original == 234, "CamburizinhoGuarujá",
                                                                   ifelse(beach == "Camburizinho" & id_original == 75, "CamburizinhoSebastião",
                                                                          ifelse(beach == "Estaleiro" & id_original == 338, "EstaleiroCamburiú",
                                                                                 ifelse(beach == "Estaleiro" & id_original == 146, "EstaleiroUbatuba",
                                                                                        ifelse(beach == "Pereque" & id_original == 254, "PerequeGarujá",
                                                                                               ifelse(beach == "Pereque" & id_original == 77, "PerequeIlhaBela",
                                                                                                      ifelse(beach == "Pereque" & id_original == 359, "PerequePortoBelo",
                                                                                                             ifelse(beach == "Praia Brava" & id_original == 202, "Praia BravaIlhaBela",
                                                                                                                    ifelse(beach == "Praia Brava" & id_original == 312, "Praia BravaMatinhos",
                                                                                                                           ifelse(beach == "Praia Grande" & id_original == 222, "Praia Grande",
                                                                                                                                  ifelse(beach == "Praia Grande" & id_original == 352, "Praia GrandePenha", 
                                                                                                                                         beach))))))))))))))))))

## C3: For those beaches with multiple stretches in the same period, choose one 
## stretches, rename it as in 'originalStretches' and remove the remain stretches.
## For those beaches with multiple stretches, but in different periods 
## (e.g. "Ilha Comprida"), just rename the stretch as in 'originalStretches'. 

## Step1: remove the stretches  
effStandardize <-
  effStandardize %>%
  dplyr::filter(stretch != "Praia da Barra N") %>%
  dplyr::filter(stretch != "Praia de Garopaba N") %>%
  dplyr::filter(stretch != "Matinhos 1") %>%
  dplyr::filter(stretch != "Matinhos 2") %>%
  dplyr::filter(stretch != "Ipanema") %>%
  dplyr::filter(stretch != "Praia de Leste")

## Step2: rename the stretches as in originalStretches
effStandardize <-
  effStandardize %>%
  dplyr::mutate(stretch = 
                  ifelse(beach == "Praia da Barra" & stretch == "Praia da Barra S", "Praia da Barra",
                         ifelse(beach == "Praia de Garopaba" & stretch == "Praia de Garopaba S", "Praia de Garopaba",
                                ifelse(beach == "Pontal do Sul/ Flamingo" & stretch == "Pontal do Sul", "Pontal do Sul/ Flamingo",
                                       ifelse(beach == "Ilha Comprida" & stretch == "antigo D15- Ilha Comprida", "Ilha Comprida",
                                              stretch)))))

## C4: split "Antigo Superagui Sul e trapiche-Rio" in two stretches: 
## "Superagui Sul Novo" and "Superagui Trapiche-Rio Novo".

## Step1: remove repeted date for stretch == "Antigo Superagui Sul e 
## trapiche-Rio" in 'eff'
Superagui_df <-
  effStandardize %>%
  dplyr::filter(stretch == "Antigo Superagui Sul e trapiche-Rio")  

Superagui_df <- 
  Superagui_df[!duplicated(Superagui_df$date), ]

## Step2: rename "Antigo Superagui Sul e trapiche-Rio" as "Superagui Sul Novo" 
Superagui_df$stretch <- 
  as.factor(Superagui_df$stretch)

Superagui_df_SulNovo <- 
  Superagui_df %>%
  dplyr::mutate(stretch = recode_factor(stretch, 
                                        "Antigo Superagui Sul e trapiche-Rio" = "Superagui Sul Novo")) 

## Step3: rename "Antigo Superagui Sul e trapiche-Rio" as 
## "Superagui Trapiche-Rio Novo"
Superagui_df_TrapicheNovo <- 
  Superagui_df %>% 
  dplyr::mutate(stretch = recode_factor(stretch, 
                                        "Antigo Superagui Sul e trapiche-Rio" = "Superagui Trapiche-Rio Novo")) 

## Step4: combine "Superagui_df_TrapicheNovo" and "Superagui_df_SulNovo"
Superagui_df <- 
  rbind(Superagui_df_SulNovo, Superagui_df_TrapicheNovo)

## Step5: combine "dfSuperagui" and "effStandardize"
effStandardize <- 
  effStandardize %>% 
  dplyr::filter(stretch != "Antigo Superagui Sul e trapiche-Rio")

effStandardize <- 
  rbind(effStandardize, Superagui_df)  

## Create a id for 'effStandardize' and 'originaStrechesStandardize' based on 
## "beach" and "stretch" columns.
eff2 <-
  effStandardize %>%
  dplyr::mutate(beach_id = paste(beach, stretch, sep = "/")) %>% 
  dplyr::relocate(stretch, .before = beach) %>% 
  dplyr::relocate(beach_id, .after = beach)

originalStretches2 <- 
  originalStretchesStandardize %>% 
  dplyr::select(-c(beach_id, pmp_id, stretch_id, executing_, geometry, 
                   executin_1, OBJECTID, stretch__1)) %>%
  dplyr::mutate(beach_id = paste(beach, stretch, sep = "/")) %>% 
  dplyr::select(id_original, executing1, stretch_st, stretch_ty, 
                stretch, beach, beach_id, original_length, geometry)

## sf::st_write(originalStretches2, "./data_out/originalStretchesFinal.shp")

## Check beaches and stretches in 'eff2' 
## that are not in 'originalStretches2' and vice versa
identify1 <- dplyr::setdiff(eff2$beach_id, 
                            originalStretches2$beach_id)
identify2 <- dplyr::setdiff(originalStretches2$beach_id,
                            eff2$beach_id)

## Clean environment
rm(list = "effStandardize", "eff_BeachNames", "fortnight_df", "fortnight_df_eff",
   "originalStretchesStandardize", "select_duplicated_BeachNames",
   "select1", "select3", "Superagui_df", "Superagui_df_SulNovo", "Superagui_df_TrapicheNovo")

##----------------------------------------------------------------------------##
##            Split monitored beach segments based on polygon sectors         ##
##----------------------------------------------------------------------------##

## Create a 30 latitudinal-band Sector polygon,
## which will be the basis for final analysis

## Transform crs
originalStretches <- 
  sf::st_transform(originalStretches2, crs = 4326)

# mapview(originalStretches)

## Latitudinal definition
n <- 
  diff(sf::st_bbox(originalStretches)[c(2, 4)])/30
## Longitudinal definition
m <- 
  diff(sf::st_bbox(originalStretches)[c(1, 3)])

## Create the 'sectorsPolygon' based on 'originalStretches' extend
sectorsPolygon <- 
  sf::st_make_grid(originalStretches, cellsize = c(m, n))

## Set the correct spatial attributes for 'sectorsPolygon' layer
## Define 'sectorsPolygon' as single feature
sectorsPolygon <- 
  sf::st_sf(sectorsPolygon)

## Create an identifier for each sector
sectorsPolygon$id_polygon <- 1:nrow(sectorsPolygon)

## Define 'sectorsPolygon' as a multipolygon
sectorsPolygon <- 
  sectorsPolygon %>% 
  sf::st_cast("MULTIPOLYGON")

# mapview::mapview(sectorsPolygon)+originalStretches
# sf::st_write(sectorsPolygon, "./data_out/sectorsPolygon.shp")

## Create a multiline feature based on 'sectorsPolygon' id limits
sectorsPolygon_lines <- 
  sf::st_cast(sectorsPolygon, "MULTILINESTRING", 
              group_or_split = FALSE)

# mapview::mapview(sectorsPolygon_lines)
# sf::st_write(sectorsPolygon_lines, "./data_out/sectorsPolygon_linesFinal.shp")

#------------
# Look for intersections between the 30 latitudinal-band 'sectorsPolygon' and
# the monitored beaches 'originalStretches'
# Cut features and calculate each segment length, for calculate effort (m)
#------------

## Calculate intersection between 'originalStretches' and 'sectorsPolygon'
newStretches <- 
  sf::st_intersection(originalStretches, sectorsPolygon)

## Add a new id for each segment line, called "id_newStretches"
newStretches$id_newStretches <- 1:nrow(newStretches)

## Calculate individual segment lengths
newStretches$new_length <- sf::st_length(newStretches)

## Set "new_length" and "original_length" in kilometers and obtain the
## difference between them
newStretches <- 
  newStretches %>%
  dplyr::mutate(new_length = as.numeric(new_length / 1000), 
                original_length = as.numeric(original_length / 1000)) %>%
  dplyr::mutate_if(is.numeric, round, digits = 2) %>%
  dplyr::mutate(difference = original_length - new_length)

difference <-
  newStretches %>%
  dplyr::filter(difference > 0)

# mapview::mapView(difference) + sectorsPolygon_lines

## Clean 'newStretches' and and rename some columns.
newStretches <- 
  newStretches %>% 
  dplyr::select(id_original, id_newStretches,
                id_polygon, executing1, beach,
                stretch, beach_id, stretch_st,
                new_length, geometry) %>%
  dplyr::rename(stretch_scheme = stretch_st,
                new_length_km = new_length) %>%
  sf::st_as_sf() %>% 
  sf::st_set_crs(4326)

# mapview::mapview(sectorsPolygon) + newStretches

##----------------------------------------------------------------------------##
##                  Join 'pontoporia' & 'eff' with 'newStretches'              ##
##----------------------------------------------------------------------------##

## Transform crs
pontoporiaSpatial <- 
  sf::st_transform(pontoporiaSpatial, crs = 4326)

## Join attributes from 'newStretches' into 'pontoporiaSpatial'
pontoporiaSpatialJoin <- 
  sf::st_join(pontoporiaSpatial, newStretches, 
              join = sf::st_nearest_feature)

## Return to df format clean, rename and reorganize the columns.
pontoporia <- 
  pontoporiaSpatialJoin %>% 
  as.data.frame() %>% 
  dplyr::rename(beach = beach.x, 
                stretch = stretch.x, 
                stretch_scheme = stretch_scheme.x) %>%
  dplyr::select(id_original, id_newStretches, id_polygon, zone, 
                id_individual, state, beach, stretch, beach_id, new_length_km, 
                stretch_scheme, monitoring_type, lat, long, 
                date, year, year_n, semester, month, season, fortnight_id, week, day, 
                back_date, cod_decomposition, sex)

## Join attributes from 'newStretches' into 'eff2'
effJoin <- 
  merge(eff2, newStretches, by = "beach_id", all = TRUE)

## Clean, rename and reorganize the columns 
eff <- 
  effJoin %>% 
  dplyr::rename(beach = beach.x, 
                stretch = stretch.x, 
                stretch_scheme = stretch_scheme.x) %>% 
  dplyr::select(code, id_original, id_newStretches, id_polygon, executing1, 
                state, beach, stretch, beach_id, new_length_km, 
                stretch_scheme, type, initial_lat, initial_long, complete, 
                date, year, year_n, semester, month, season, fortnight_id, week, day) %>%
  dplyr::arrange(date)

## write.csv2(eff,"./data_out/effFinal.csv")

## Clean environment
rm(list = "difference", "effJoin", "eff2", "effSpatial", 
   "pontoporiaSpatial", "pontoporiaSpatialJoin")

##----------------------------------------------------------------------------##
##            Stranding records collected in complete-made stretches          ##
##----------------------------------------------------------------------------##

## Create 'eff_i' and "date_id_newStretches" column
eff_i <- 
  eff %>% 
  dplyr::filter(complete != "yes") %>% 
  dplyr::mutate(date_id_newStretches = paste(date, id_newStretches)) 

## Create "date_id_newStretches" column in 'pontoporia'
pontoporia <- 
  pontoporia %>% 
  dplyr::mutate(date_id_newStretches = paste(date, id_newStretches)) 

## Remove strandings in 'pontoporia' recorded during incomplete beach surveys. 
pontoporia <- 
  dplyr::anti_join(pontoporia, eff_i, by = "date_id_newStretches")

## Remove "date_id_newStretches" from 'pontoporia'
pontoporia <-
  pontoporia %>% 
  dplyr::select(- date_id_newStretches)

# write.csv2(pontoporia,"./data_out/pontoporia_stranding.csv")

##----------------------------------------------------------------------------##
##                  Join environment data with pontoporia df                  ##
##----------------------------------------------------------------------------##

## The environmental data was collected from ERA5
## through a Python routine files = {download_and_process_era5_data.py} + {requirements.txt}
## two different files are used in this step because the zone of 44 stranding 
## was updated after reviewing the methodology during the the process of this 
## consultancy. This step used source("./functionEnvData.R") to clean env data 
## and merge with pontoporia df

envVariables_old_zones <- processing_env_data(
  env_file = "./data/environmental_data/envVariables_old_zone.csv")
envVariables_new_zones <- processing_env_data(
  env_file = "./data/environmental_data/envVariables_new_zone.csv"
)

## Substitute the 44 strandings with old Zones (between lat -26/ -26.2) by the 
## the new and correct data (downloaded using the new Zone) 
envVariables <- rbind((envVariables_old_zones %>% filter(!(id_individual %in% 
              envVariables_new_zones$id_individual))) , 
              envVariables_new_zones)

## remove the two initial environmental dfs:
rm(list = "envVariables_new_zones", "envVariables_old_zones")

## save final environmental df:
# write.csv(envVariables, "./data_out/strandingAndEnv.csv")


##----------------------------------------------------------------------------##
##                Summarize by polygon, month, fortnight and week             ##
##----------------------------------------------------------------------------##

## Obtain completed survey effort df
eff_c <- 
  eff %>% 
  dplyr::filter(complete != "no")

## Summarise 'pontoporia' and 'eff_c':

## Per "week"
ponWeek <-
  pontoporia %>% 
  dplyr::group_by(year_n, week, id_polygon) %>%
  dplyr::summarise(y = n()) %>%
  dplyr::mutate_at(c(1:3), as.factor)

effWeek <-
  eff_c %>%
  dplyr::group_by(year_n, id_polygon, week) %>%
  dplyr::summarise(effort_km = sum(new_length_km)) %>%
  dplyr::mutate_at(c(1:3), as.factor)

ponWeekFinal <- 
  dplyr::left_join(effWeek, ponWeek, 
                   by = c("id_polygon", "year_n", "week")) %>% 
  replace(., is.na(.), 0)

## Per "fortnight"
ponFortnight <- 
  pontoporia %>% 
  dplyr::group_by(year_n, id_polygon, fortnight_id) %>%
  dplyr::summarise(y = n()) %>%
  dplyr::mutate_at(c(1:3), as.factor)

effFortnight <-
  eff_c %>%
  dplyr::group_by(year_n, id_polygon, fortnight_id) %>%
  dplyr::summarise(effort_km = sum(new_length_km)) %>%
  dplyr::mutate_at(c(1:3), as.factor)

ponFortnightFinal <- 
  dplyr::left_join(effFortnight, ponFortnight, 
                   by = c("id_polygon", "year_n", "fortnight_id")) %>%
  replace(., is.na(.), 0)

## Per "month"
ponMonth <- 
  pontoporia %>% 
  dplyr::group_by(id_polygon, year_n, month) %>%
  dplyr::summarise(y = n()) %>%
  dplyr::mutate_at(c(1:3), as.factor)

effMonth <-
  eff_c %>%
  dplyr::group_by(year_n, id_polygon, month) %>% 
  dplyr::summarise(effort_km = sum(new_length_km)) %>%
  dplyr::mutate_at(c(1:3), as.factor)

ponMonthFinal <- 
  dplyr::left_join(effMonth, ponMonth, 
                   by = c("id_polygon", "year_n", "month")) %>%
  replace(., is.na(.), 0)

## Per "season"
ponSeason <- 
  pontoporia %>% 
  dplyr::group_by(id_polygon, year_n, season) %>%
  dplyr::summarise(y = n()) %>%
  dplyr::mutate_at(c(1:3), as.factor)

effSeason <-
  eff_c %>%
  dplyr::group_by(year_n, id_polygon, season) %>%
  dplyr::summarise(effort_km = sum(new_length_km)) %>%
  dplyr::mutate_at(c(1:3), as.factor)

ponSeasonFinal <- 
  dplyr::left_join(effSeason, ponSeason, 
                   by = c("id_polygon","year_n", "season")) %>%
  replace(., is.na(.), 0)

# write.csv2(ponWeekFinal,"./data_out/ponWeekFinal.csv")
# write.csv2(ponFortnightFinal,"./data_out/ponFortnightFinal.csv")
# write.csv2(ponMonthFinal,"./data_out/ponMonthFinal.csv")
# write.csv2(ponSeasonFinal,"./data_out/ponSeasonFinal.csv")

##----------------------------------------------------------------------------##
##                Create fake stranding in the middle                         ##
##                        of each polygon                                     ##
##                    using ponFortnightFinal                                 ##
##----------------------------------------------------------------------------##


## Task 01: find start and end date of fortnight: ####
empty_fortnight <- read.csv("./data_out/ponFortnightFinal.csv", sep = ";") %>%
  filter(y==0)

## to select fortnight
filter_dates <- pontoporia %>% dplyr::select(contains(c("year", "month", "fortnight_id"))) %>%
  group_by(year_n, year, month) %>% distinct(fortnight_id)

empty_fortnight = empty_fortnight %>% 
  mutate(day_start = (ifelse(fortnight_id%%2 == 0, "16", "1"))) %>%
  mutate(day_end = (ifelse(fortnight_id%%2 == 0, "30", "15"))) %>%
  left_join(., filter_dates,  by = c("year_n", "fortnight_id"), suffix(".y") ) %>%
  mutate(day_end = (ifelse(month == 2, "28", day_end))) %>% 
  mutate(date = lubridate::ymd(paste(year,month,day_start, sep = "-"))) %>%
  mutate(back_date = lubridate::ymd(paste(year, month,day_end, sep = "-"))) %>%
  dplyr::select(!starts_with(c("month", "day_"))) %>% 
  dplyr::select(!ends_with("year")) %>% dplyr::rename(id=X)

## Task 2: find middle point of coastline ("fake stranding") for each polygon sector and merge with task 2: ####

# Open coastline, segment it using sectors and cast to linestring:
coastline <- 
  sf::st_intersection(st_read("./data/coastline_S_SE/lc_ilhas_tratada_UTM.shp"), sf::st_transform(sectorsPolygon, crs = 31982)) %>%
  st_cast("MULTILINESTRING") %>% st_cast("LINESTRING", group_or_split = TRUE)

# calculate the length and select the longest stripe of coastline along each
# polygon sector and extract its middle point:
coastline_middle_pt <- coastline %>% 
  mutate(new_length = st_length(.)) %>%
  mutate(new_length = as.numeric(new_length)) %>% 
  group_by(id_polygon) %>% filter(new_length == max(new_length))

# extract middle point:
coastline_middle_pt <-  st_as_sf(maptools::SpatialLinesMidPoints(as(coastline_middle_pt, "Spatial")))

# extract lat, long column
coastline_middle_pt = coastline_middle_pt %>%
  mutate(long = st_coordinates(st_transform(coastline_middle_pt, 4326))[,1]) %>%
  mutate(lat = st_coordinates(st_transform(coastline_middle_pt, 4326))[,2])

# join
empty_fortnight_download <- left_join(empty_fortnight, (coastline_middle_pt %>% dplyr::select(contains(c("lat", "lon", "id_poly")))))
empty_fortnight_download$zone <- "2"

# viz:
# mapview(coastline_middle_pt) + coastline
# write.csv(empty_fortnight_download, "./data_out/empty_fortnight_download_era5.csv")

## The "empty_fortnight_download_era5.csv" created in the step above was used as 
## input in the download_and_process_era5_data.py script. And the output file generated after running
## the code is called "envVariables_without_stranding_fortnight.csv" to be
## used in the next session

## Clean environment
rm(list = "ponWeek", "effWeek", "ponWeekFinal", 
   "ponFortnight", "effFortnight", "ponFortnightFinal", 
   "ponMonth", "effMonth", "ponMonthFinal",
   "ponSeason", "effSeason", "ponSeasonFinal",
   "eff_i", "eff_c", "empty_fortnight_download", "empty_fortnight")

##----------------------------------------------------------------------------##
##                    Process the env data downloaded for                     ##
##                    "fake stranding" (session above) and                    ##
##                  join it to the initial ponFortnightFinal df               ##
##----------------------------------------------------------------------------##

envVariables_empty_fortnight <- processing_env_data(
  env_file = "./data/environmental_data/envVariables_without_stranding_fortnight.csv",
  corresponding_df = read.csv2("./data_out/ponFortnightFinal.csv"), id_column = "X")

##----------------------------------------------------------------------------##
##                                Drift dataset                               ##
##----------------------------------------------------------------------------##

## Open drift experiment dataset
drift <- read.csv("./data/drift_experiment_data.csv", sep = ";")

# cf. if you need to run this -- just to fix bug on a variable name
drift <- 
  drift %>%
  dplyr::rename(campaign=ï..campaign)

## Create "ReleaseStation" column 
drift <- 
  drift %>% 
  dplyr::mutate(ReleaseStation = 
                  ifelse(lat_r > -23.8, "1", 
                         ifelse(lat_r < -23.8 & lat_r > -24.4,"2", 
                                ifelse(lat_r < -24.4 & lat_r > -26, "3", "4"))))

drift$ReleaseStation <- as.factor(drift$ReleaseStation)

#------------
# PS: The 'ReleaseStation' represent the places where the drifters were released.
#------------

## Filter and set up date columns
drift_i <- 
  drift %>% 
  dplyr::filter(lat_s != is.na(lat_s)) %>% 
  dplyr::filter(date_s != is.na(date_s))

drift_i$date_r <- lubridate::dmy(drift_i$date_r)
drift_i$date_s <- lubridate::dmy(drift_i$date_s)

################################################################################
############################ Figures and Tables ################################
########################### Material and Methods ###############################
################################################################################

## Open shapefile
brazil <- 
  sf::read_sf(dsn = "./shape", layer = "brasil") %>% 
  sf::st_set_crs(4326)

# brazil <- sf::read_sf("./data/shp_brasil/brasil.shp") ## Nico PC

originalStretchesFinal <-
  sf::read_sf(dsn = "./data_out", layer = "originalStretchesFinal") %>% 
  sf::st_transform(crs = 4326)

sectorsPolygon_linesFinal <-
  sf::read_sf(dsn = "./data_out", layer = "sectorsPolygon_linesFinal")

isobath <- sf::st_read("./shape/linhas_lim_200m.shp")
iso20 <-
  isobath %>%
  dplyr::filter(ELEV == 20)

# isobath <- sf::st_read("./data/bathymetry/linhas_lim_200m.shp") ## Nico PC
# iso20 <-
#   isobath %>%
#   dplyr::filter(ELEV == 20)

## Figure 1. Study area
figure1 <- 
  ggplot() + 
  ggspatial::geom_sf(data = brazil) +
  geom_sf(data = originalStretchesFinal, color = "red", show.legend = FALSE) + 
  labs(x = "Longitude", y = "Latitude") + 
  coord_sf(xlim = c(-50.5, -44), ylim = c(-28.5, -23)) + 
  theme_bw() + 
  theme(axis.title = element_text(colour = "black", size = 16), 
        axis.text = element_text(colour = "black", size = 10), 
        axis.line = element_line(colour = "black")) + 
  ggspatial::annotation_scale(location = "br", width_hint = 0.3) +
  ggspatial::annotation_north_arrow(location = "br", which_north = "true", 
                                    pad_x = unit(0.45, "in"), pad_y = unit(0.3, "in"),
                                    style = north_arrow_fancy_orienteering) + 
  annotate(geom = "text", x = -46, y = -26, label = "Atlantic Ocean", 
           color = "grey22", size = 3) +
  annotate(geom = "text", x = -47.5, y = -23.58, label = "São Paulo", 
           color = "grey22", size = 3) +
  annotate(geom = "text", x = -49.8, y = -25, label = "Paraná", 
           color = "grey22", size = 3) +
  annotate(geom = "text", x = -49.8, y = -27.3, label = "Santa Catarina", 
           color = "grey22", size = 3)

## Figure 2. Latitudinal division of the study area. 
figure2 <- 
  ggplot() + 
  ggspatial::geom_sf(data = brazil) + 
  geom_sf(data = originalStretchesFinal, 
          color = "red", show.legend = FALSE) + 
  geom_sf(data = sectorsPolygon_linesFinal , 
          color = "black", show.legend = FALSE) + 
  labs(x = "Longitude", y = "Latitude") + 
  coord_sf(xlim = c(-50.5, -44), ylim = c(-28.5, -23)) + 
  theme_bw() + 
  theme(axis.title = element_text(colour = "black", size = 16), 
        axis.text = element_text(colour = "black", size = 10), 
        axis.line = element_line(colour = "black")) + 
  ggspatial::annotation_scale(location = "br", width_hint = 0.3,
                              pad_x = unit(0.1, "in"), pad_y = unit(0.05, "in")) + 
  ggspatial::annotation_north_arrow(location = "br", which_north = "true", 
                                    pad_x = unit(0.10, "in"), pad_y = unit(0.2, "in"),
                                    style = north_arrow_fancy_orienteering) + 
  annotate(geom = "text", x = -46, y = -26, label = "Atlantic Ocean", 
           color = "grey22", size = 3) + 
  annotate(geom = "text", x = -47.5, y = -23.20, label = "São Paulo", 
           color = "grey22", size = 3) + 
  annotate(geom = "text", x = -49.8, y = -25, label = "Paraná", 
           color = "grey22", size = 3) + 
  annotate(geom = "text", x = -49.8, y = -27.3, label = "Santa Catarina", 
           color = "grey22", size = 3)

# Figure 3. Release station map 
driftSpatial <-
  drift %>%
  dplyr::group_by(campaign, id, ReleaseStation) %>%
  dplyr::filter(!duplicated(lat_r)) %>%
  sf::st_as_sf(coords = c("long_r", "lat_r"), crs = 4326) %>%
  dplyr::mutate(campaign = as.factor(campaign),
                ReleaseStation = as.factor(ReleaseStation))

# sf::st_write(driftSpatial,"./data_out/driftSpatialFinal.shp")
# mapview::mapview(driftSpatial)

figure3 <- 
  ggplot() + 
  ggspatial::geom_sf(data = brazil) + 
  ggspatial::geom_sf(data = iso20) + 
  ggspatial::geom_sf(data = driftSpatial, aes(color = ReleaseStation), 
                     show.legend = TRUE) + 
  coord_sf(xlim = c(-50.5, -44), ylim = c(-28.5, -23)) + 
  labs(x = "Longitude", y = "Latitude") + 
  theme_bw() + 
  theme(axis.title = element_text(colour = "black", size = 16), 
        axis.text = element_text(colour = "black", size = 10), 
        axis.line = element_line(colour = "black")) + 
  ggspatial::annotation_scale(location = "br", width_hint = 0.3) +
  ggspatial::annotation_north_arrow(location = "br", which_north = "true", 
                                    pad_x = unit(0.45, "in"), pad_y = unit(0.3, "in"),
                                    style = north_arrow_fancy_orienteering) + 
  annotate(geom = "text", x = -46, y = -26, label = "Atlantic Ocean", 
           color = "grey22", size = 3) +
  annotate(geom = "text", x = -47.5, y = -23.58, label = "São Paulo", 
           color = "grey22", size = 3) +
  annotate(geom = "text", x = -49.8, y = -25, label = "Paraná", 
           color = "grey22", size = 3) +
  annotate(geom = "text", x = -49.8, y = -27.3, label = "Santa Catarina", 
           color = "grey22", size = 3) + 
  ggplot2::geom_rect(data = brazil, 
                     aes(xmin = -44,5, xmax = -49, ymin = -23.75, ymax = -23.4), 
                     colour = "red", fill = "NA") + 
  ggplot2::geom_rect(data = brazil, 
                     aes(xmin = -44,5, xmax = -49, ymin = -26.2, ymax = -23.75), 
                     colour = "red", fill = "NA") + 
  ggplot2::geom_rect(data = brazil, 
                     aes(xmin = -44,5, xmax = -49, ymin = -28.6, ymax = -26.2), 
                     colour = "red", fill = "NA") +
  annotate(geom = "text", x = -44.2, y = -23.58, label = "1", 
           color = "red", size = 5) +
  annotate(geom = "text", x = -44.2, y = -25, label = "2", 
           color = "red", size = 5) +
  annotate(geom = "text", x = -44.2, y = -27.3, label = "3", 
           color = "red", size = 5)

## Table1. Drifters release  by "campaign", "id" and "ReleaseStation". 
table1 <-
  drift %>% 
  dplyr::select(c(1:7, ReleaseStation)) %>% 
  dplyr::group_by(campaign, id, ReleaseStation) %>% 
  dplyr::filter(!duplicated(lat_r)) %>%
  dplyr::ungroup() %>%
  dplyr::select(- hour_r) %>%
  dplyr::mutate(release = c(rep(33, 18), 30, rep(33, 8))) %>% # number of drifters released
  dplyr::relocate(release, .after = id)

names(table1) <- c("Campaign", "State", "Release position","No. drifters",
                   "Date", "Lat", "Long","Release station")
# write.csv2(table1, "./results/table1.csv", row.names = FALSE)

################################################################################
############################## General Results #################################
################################################################################

##----------------------------------------------------------------------------##
##   Drift experiment - Mean distance & time between release and strandings   ##           
##----------------------------------------------------------------------------##

# - - - - -
# Each campaign had 33 drifters released. 
# Only in campaing == "3" and id == "C1" 30 drifters were released.
# - - - - -

## Create a link to calculate time and distances, 
## between (r)elease and (s)tranding
driftSpatial_r <- 
  drift_i %>% 
  dplyr::select(campaign , id, (ends_with("r"))) %>% 
  sf::st_as_sf(coords = c("long_r", "lat_r"), crs = 4326)

driftSpatial_s <- 
  drift_i %>% 
  dplyr::select(campaign , id, (ends_with("s"))) %>% 
  sf::st_as_sf(coords = c("long_s", "lat_s"), crs = 4326)

## Calculate distance and time between 'r' and 's'
driftDistTime <- 
  drift_i %>% 
  dplyr::select(campaign , id, state, ReleaseStation, 
                date_r, date_s, 
                lat_r, long_r, 
                lat_s, long_s) %>% 
  dplyr::mutate(dist = sf::st_distance(driftSpatial_r$geometry, 
                                       driftSpatial_s$geometry, 
                                       by_element = TRUE)) %>% 
  dplyr::mutate(time_lag = (date_s)-(date_r))

##
## General summary
##

## Table 2. Drift by "campaign" and "id"
driftSummary_Campaign <- 
  driftDistTime %>% 
  dplyr::group_by(campaign, state, id) %>% 
  dplyr::summarise(stranding = n(),
                   meanDist = round(mean((as.numeric(dist)/1000)), digits = 1),
                   sdDist = round(sd((as.numeric(dist)/1000)), digits = 1), 
                   minDist = round (min((as.numeric(dist)/1000)), digits = 1), 
                   maxDist = round(max((as.numeric(dist)/1000)), digits = 1), 
                   meanTime = round(mean(as.numeric(time_lag)), digits = 1), 
                   sdTime = round(sd(as.numeric(time_lag)), digits = 1), 
                   minTime = round (min(as.numeric(time_lag)), digits = 1), 
                   maxTime = round(max(as.numeric(time_lag)), digits = 1)) %>%
  as.data.frame()

# Adding campaigns without stranding records
df <- tibble::tibble(campaign = c("1","1","1","1","1","1","1"), 
                     state = c("SC","SC","SC","PR","PR","PR","SP"), 
                     id = c("C1","C2","C3","P1","P2","P3","S3"), 
                     stranding = c(rep(0,7)), 
                     meanDist = c(rep(0,7)), sdDist = c(rep(0,7)), 
                     minDist = c(rep(0,7)), maxDist = c(rep(0,7)), 
                     meanTime = c(rep(0,7)), sdTime = c(rep(0,7)), 
                     minTime =c(rep(0,7)), maxTime = c(rep(0,7))) %>%
  as.data.frame()

# Combine 'df' with 'driftSummary_Campaign'
driftSummary_Campaign <- rbind(df, driftSummary_Campaign)

# Create a column "release" and "percentage"
table2 <-
  driftSummary_Campaign %>%
  dplyr::arrange(campaign, id) %>%
  dplyr::mutate(release = c(rep(33, 18), 30, rep(33, 8))) %>%
  dplyr::mutate(percentage = round(stranding/release*100, digits = 1)) %>%
  dplyr::relocate(release, .after = id) %>%
  dplyr::relocate(percentage, .after = stranding) %>%
  dplyr::mutate_all(~replace(., . == 0, NA)) %>%
  janitor::adorn_totals(where = "row")

# write.csv2(table2, "./results/table2.csv", row.names = FALSE)

## Table 3A. Drift by "state"
table3A <- 
  table2 %>%
  dplyr::slice(-c(28)) %>%
  dplyr::mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::group_by(state) %>%
  dplyr::summarise(release = sum(release),
                   stranding = sum(stranding)) %>%
  dplyr::mutate(percentage = round(stranding/release*100, digits = 1))

# write.csv2(table3A, "./results/table3A.csv",row.names = FALSE)

## Table 4A. Drift by "ReleaseStation"
driftSummary_ReleaseStation <- 
  driftDistTime %>% 
  dplyr::group_by(ReleaseStation) %>% 
  dplyr::summarise(stranding = n(), 
                   meanDist = round(mean((as.numeric(dist)/1000)), digits = 1), 
                   sdDist = round(sd((as.numeric(dist)/1000)), digits = 1), 
                   minDist = round(min((as.numeric(dist)/1000)), digits = 1), 
                   maxDist = round(max((as.numeric(dist)/1000)), digits = 1), 
                   meanTime = round(mean(as.numeric(time_lag)), digits = 1), 
                   sdTime = round(sd(as.numeric(time_lag)), digits = 1), 
                   minTime = round(min(as.numeric(time_lag)), digits = 1), 
                   maxTime = round(max(as.numeric(time_lag)), digits = 1))

# Create a column "release" and "percentage"
release_ReleaseStation <-
  driftDistTime %>%
  dplyr::group_by(ReleaseStation, id, campaign) %>% 
  dplyr::summarise(n = n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(release = c(rep(33, 15), 30, rep(33, 4))) %>% 
  dplyr::group_by(ReleaseStation) %>%
  dplyr::summarise(release = sum(release))

table4A <-
  driftSummary_ReleaseStation %>%
  dplyr::left_join(release_ReleaseStation, by = "ReleaseStation") %>% 
  dplyr::relocate(release, .after = ReleaseStation) %>%
  dplyr::mutate(percentage = round(stranding/release*100, digits = 1)) %>% 
  dplyr::relocate(percentage, .after = stranding)

# write.csv2(table4A, "./results/table4A.csv", row.names = FALSE)

##
## Summaries from less than 10 days drifting
##

## Drift by "campaign" and "id".Results not present in the text.
driftSummary_Campaign_10 <- 
  driftDistTime %>% 
  dplyr::filter(time_lag < 10) %>% 
  dplyr::group_by(campaign, state, id) %>% 
  dplyr::summarise(stranding = n(), 
                   meanDist = round(mean((as.numeric(dist)/1000)), digits = 1),
                   sdDist = round(sd((as.numeric(dist)/1000)), digits = 1), 
                   minDist = round (min((as.numeric(dist)/1000)), digits = 1), 
                   maxDist = round(max((as.numeric(dist)/1000)), digits = 1), 
                   meanTime = round(mean(as.numeric(time_lag)), digits = 1), 
                   sdTime = round(sd(as.numeric(time_lag)), digits = 1), 
                   minTime = round (min(as.numeric(time_lag)), digits = 1), 
                   maxTime = round(max(as.numeric(time_lag)), digits = 1)) %>% 
  as.data.frame()

# Adding campaigns without stranding records
df <- tibble::tibble(campaign = c("1","1","1","1","1","1","1","1","2"), 
                     state = c("SC","SC","SC","PR","PR","PR","SP","SP","SC"), 
                     id = c("C1","C2","C3","P1","P2","P3","S1","S3","C2"), 
                     stranding = c(rep(0,9)), 
                     meanDist = c(rep(0,9)), sdDist = c(rep(0,9)), 
                     minDist = c(rep(0,9)), maxDist = c(rep(0,9)), 
                     meanTime = c(rep(0,9)), sdTime = c(rep(0,9)),
                     minTime =c(rep(0,9)),maxTime = c(rep(0,9))) %>% 
  as.data.frame()

# Combine df with driftSummary_Campaign_10
driftSummary_Campaign_10 <- rbind(df, driftSummary_Campaign_10)

driftSummary_Campaign_10 <- 
  driftSummary_Campaign_10 %>% 
  dplyr::arrange(campaign, id) %>% 
  dplyr::mutate(release = c(rep(33, 18), 30, rep(33, 8)))%>%
  dplyr::mutate_all(~replace(., . == 0, NA)) %>%
  dplyr::mutate(percentage = round(stranding/release*100, digits = 1)) %>%
  dplyr::relocate(release, .after = id) %>%
  dplyr::relocate(percentage, .after = stranding)

# Table 3B. Drift by "state".
table3B <- 
  driftSummary_Campaign_10 %>%
  dplyr::mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::group_by(state) %>%
  dplyr::summarise(release = sum(release),
                   stranding = sum(stranding)) %>%
  dplyr::mutate(percentage = round(stranding/release*100, digits = 1)) 

# write.csv2(table3B, "./results/table3B.csv", row.names = FALSE)

## Table 4B. Drift by zoneExp
driftSummary_ReleaseStation_10 <-
  driftDistTime %>% 
  dplyr::filter(time_lag < 10) %>% 
  dplyr::group_by(ReleaseStation) %>% 
  dplyr::summarise(stranding = n(), 
                   meanDist = round(mean((as.numeric(dist)/1000)), digits = 1), 
                   sdDist = round(sd((as.numeric(dist)/1000)), digits = 1), 
                   minDist = round(min((as.numeric(dist)/1000)), digits = 1), 
                   maxDist = round(max((as.numeric(dist)/1000)), digits = 1), 
                   meanTime = round(mean(as.numeric(time_lag)), digits = 1), 
                   sdTime = round(sd(as.numeric(time_lag)), digits = 1), 
                   minTime = round(min(as.numeric(time_lag)), digits = 1), 
                   maxTime = round(max(as.numeric(time_lag)), digits = 1))

# Create a column "release" and "percentage"
release_ReleaseStation_10 <-
  driftDistTime %>%
  dplyr::group_by(ReleaseStation, id, campaign) %>%
  summarise(n = n()) %>%
  dplyr::ungroup() %>%
  mutate(release = c(rep(33, 15), 30, rep(33, 4))) %>%
  dplyr::group_by(ReleaseStation) %>%
  dplyr::summarise(release = sum(release))

table4B <-    #### This is the basis for "zones" in 'pontoporia' df.
  driftSummary_ReleaseStation_10 %>%
  dplyr::left_join(release_ReleaseStation_10, by = "ReleaseStation") %>%
  dplyr::relocate(release, .after = ReleaseStation) %>%
  dplyr::mutate(percentage = round(stranding/release*100, digits = 1)) %>%
  dplyr::relocate(percentage, .after = stranding)

# write.csv2(table4B, "./results/table4B.csv", row.names = FALSE)

##----------------------------------------------------------------------------##
##     Summary of survey effort and stranded records by polygons and year     ##           
##----------------------------------------------------------------------------##
effFinal <- read.csv2("./data_out/effFinal.csv")
pontoporiaFinal <- read.csv2("./data_out/pontoporia_stranding.csv")

##
## Table 5. Per polygon
## 

# Summary of survey effort
step1 <- 
  effFinal %>% 
  dplyr::filter(complete == "yes") %>% 
  dplyr::group_by(id_polygon, id_newStretches, new_length_km) %>% 
  dplyr::summarise(monitoring = n()) %>% 
  dplyr::group_by(id_polygon) %>% 
  dplyr::mutate(Beach_surveyed = new_length_km * monitoring) %>% 
  dplyr::summarise(N_of_beaches = n(), 
                   Stretch_polygon_km = sum(new_length_km),
                   Total_beach_surveyed = sum(Beach_surveyed),
                   mean_monitoring=round(mean(monitoring), digits = 0))

# Summary of strandings
step2 <- 
  pontoporiaFinal %>% 
  dplyr::group_by(id_polygon, cod_decomposition) %>% 
  dplyr::summarise(strandings = n()) %>%
  tidyr::pivot_wider(names_from = cod_decomposition, 
                     names_prefix = "COD", 
                     values_from = strandings, values_fill = 0) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Total_stranding = COD1 + COD2 + COD3 + COD4)

table5 <- 
  step1 %>% 
  dplyr::left_join(step2, by = "id_polygon") %>%
  janitor::adorn_totals(where = "row") %>%
  dplyr::mutate(str_rate = as.numeric(Total_stranding * 1000 / Total_beach_surveyed))

names(table5) <- c("Polygon", "No of beaches", "Stretch polygon (km)",
                   "Total beach surveyed (km)", "Mean  number of beaches surveyed",
                   "COD1", "COD2", "COD3", "COD4", "Total strandings", "Stranding rate") 

# write.csv2(table5, "./results/table5.csv", row.names = FALSE, quote = FALSE)

##
## Table 6. Per year
## 

# Summary of survey effort
step1 <- 
  effFinal %>% 
  dplyr::filter(complete == "yes") %>% 
  dplyr::group_by(year_n, id_newStretches, new_length_km) %>% 
  dplyr::summarise(monitoring = n()) %>% 
  dplyr::group_by(year_n) %>% 
  dplyr::mutate(Beach_surveyed = new_length_km * monitoring) %>% 
  dplyr::summarise(Total_beach_surveyed = sum(Beach_surveyed))

# Summary of strandings
step2 <-
  pontoporiaFinal %>% 
  dplyr::group_by(year_n) %>% 
  dplyr::summarise(stranding = n()) 

table6 <- 
  step1 %>% 
  dplyr::left_join(step2, by = "year_n") %>%
  dplyr::mutate(str_rate = as.numeric(stranding * 1000 / Total_beach_surveyed))

names(table6) <- c("Year", "Total beach surveyed (km)",
                   "Total strandings", "Stranding rate")

# write.csv2(table6, "./results/table6.csv", row.names = FALSE)

##----------------------------------------------------------------------------##
##                       Spatial and temporal stranding rate                  ##           
##----------------------------------------------------------------------------##

dfSeason <- read.csv2("./data_out/ponSeasonFinal.csv")
dfMonth <- read.csv2("./data_out/ponMonthFinal.csv")
dfFortnight <- read.csv2("./data_out/ponFortnightFinal.csv")
dfWeek <- read.csv2("./data_out/ponWeekFinal.csv")

##
## Figure 5. Temporal pattern
##

## Per "Season"

strRateSeason <- 
  dfSeason %>%
  dplyr::group_by(season, year_n) %>% 
  dplyr::summarise(effort_km = sum(effort_km), 
                   y = sum(y)) %>%
  dplyr::mutate(str_rate = as.numeric(y * 1000 / effort_km))

str_rate_season_plot <-
  ggplot(strRateSeason, aes(x = season, y = str_rate, fill = year_n)) +
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_brewer(palette = "Pastel1") + 
  guides(fill = guide_legend(reverse = TRUE)) +
  xlab("Season") + ylab("Stranding rate (ind./1000 km)") + 
  theme(legend.position = "right") +
  theme(legend.title = element_blank()) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank()) + 
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA)) + 
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))+
  theme(strip.text.x = element_text(size = 18, face = "italic"))

## Per "month"

# Create the column "monthf", gruop by monthf and create the column "srt_rate" 
dfMonthf <-
  dfMonth %>%
  dplyr::mutate(monthf = 
                  factor(month, levels = as.character(1:12), 
                         labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                         ordered = FALSE)) %>% 
  dplyr::relocate(monthf, .before = "month")%>%
  dplyr::mutate(monthf = as.factor(monthf)) %>%
  dplyr::group_by(year_n, monthf) %>%
  dplyr::summarise(y = sum(y), 
                   effort_km = sum(effort_km)) %>% 
  dplyr::mutate(str_rate = as.numeric(y * 1000 / effort_km))

str_rate_month_plot <- 
  ggplot2::ggplot(dfMonthf, aes(x = monthf, y = str_rate, group = year_n)) + 
  geom_line(aes(color = year_n)) + 
  xlab("Month") + ylab("Stranding rate (ind./1000 km)") + 
  theme(legend.position = "NULL") +
  theme(legend.title = element_blank()) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank()) + 
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA)) + 
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))+
  theme(strip.text.x = element_text(size = 18, face = "italic"))

## Per "fortnight"

strRateFortnight <- 
  dfFortnight %>%
  dplyr::group_by(fortnight_id, year_n) %>% 
  dplyr::summarise(effort_km = sum(effort_km), 
                   y = sum(y)) %>%
  dplyr::mutate(str_rate = as.numeric (y * 1000 / effort_km))

str_rate_fortnight_plot <-
  ggplot2::ggplot(strRateFortnight , 
                  aes(x = fortnight_id, y = str_rate, group = year_n)) + 
  geom_line(aes(color = year_n)) + 
  xlab("Fortnight") + ylab("Stranding rate (ind./1000 km)") + 
  theme(legend.position = "right") +
  theme(legend.title = element_blank()) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank()) + 
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA)) + 
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))+
  theme(strip.text.x = element_text(size = 18, face = "italic"))

## Per "week"

strRateWeek <- 
  dfWeek %>%
  dplyr::group_by(week, year_n) %>% 
  dplyr::summarise(effort_km = sum(effort_km), 
                   y = sum(y)) %>%
  dplyr::mutate(str_rate = as.numeric(y * 1000 / effort_km))

str_rate_week_plot <-
  ggplot2::ggplot(strRateWeek, 
                  aes(x = week, y = str_rate, group = year_n)) + 
  geom_line(aes(color = year_n)) +
  xlab("Week") + ylab("Stranding rate (ind./1000 km)") + 
  theme(legend.position = "NULL") +
  theme(legend.title = element_blank()) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank()) + 
  theme(axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA)) + 
  theme(axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10))+
  theme(strip.text.x = element_text(size = 18, face = "italic"))

figure5 <- 
  gridExtra::grid.arrange(str_rate_month_plot, str_rate_fortnight_plot, 
                          str_rate_week_plot, str_rate_season_plot, 
                          ncol = 2)

##
## Spatial pattern
##

dfMonth$id_polygon <- as.factor(dfMonth$id_polygon)

# Figure 6. Per year
strRatePoly <- 
  dfMonth %>% 
  dplyr::group_by(id_polygon, year_n) %>% 
  dplyr::summarise(effort_km = sum(effort_km), 
                   y = sum(y)) %>%
  dplyr::mutate(str_rate = as.numeric(y * 1000 / effort_km))

figure6 <-
  ggplot2::ggplot(strRatePoly, 
                  aes(x = id_polygon, y = str_rate, group = year_n)) + 
  geom_line(aes(color = year_n)) + 
  xlab("Polygon") + ylab("Stranding rate (ind./1000 km)")+ 
  theme(legend.position = "right") +
  theme(legend.title = element_blank()) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank()) + 
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))+ 
  theme(panel.border = element_rect(colour = "black", fill = NA))+ 
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))+
  theme(strip.text.x = element_text(size = 18, face = "italic"))

# Figure 7. Per month
dfMonth$month <- as.factor(dfMonth$month)

strRatePoly <- 
  dfMonth %>% 
  dplyr::mutate(monthf = 
                  factor(month, levels = as.character(1:12), 
                         labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                         ordered = FALSE)) %>%
  dplyr::group_by(id_polygon, monthf) %>% 
  dplyr::summarise(effort_km = sum(effort_km), 
                   y = sum(y)) %>%
  dplyr::mutate(str_rate = as.numeric(y * 1000 / effort_km))

figure7 <-
  ggplot2::ggplot(strRatePoly, 
                  aes(x = id_polygon, y = str_rate, group = monthf)) + 
  geom_line(aes(color = monthf)) + 
  xlab("Polygon") + ylab("Stranding rate (ind./1000 km)")+ 
  theme(legend.position = "right") +
  theme(legend.title = element_blank()) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank()) + 
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))+ 
  theme(panel.border = element_rect(colour = "black", fill = NA))+ 
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))+
  theme(strip.text.x = element_text(size = 18, face = "italic"))

## Kernel density
# Open pontoporia stranding and ponMOnthFinal dataset
kernel1 <- read.csv2("./data_out/pontoporia_stranding.csv")
kernel2 <- read.csv2("./data_out/ponMonthFinal.csv")

# Create an id for kerne1 and kernel2
kernel1 <-
  kernel1 %>%
  dplyr::mutate(kernel_id = paste(id_polygon, year_n, month))

kernel2 <-
  kernel2 %>%
  dplyr::mutate(kernel_id = paste(id_polygon, year_n, month))

# Join effort_km from 'kernel2' into 'kernel1'
kernel <- merge(kernel1, kernel2, by = "kernel_id", all = TRUE)

# Clean and remove NAs 
kernel <-
  kernel %>%
  tidyr::drop_na() %>%
  dplyr::select(-c(id_polygon.y, year_n.y, month.y, y, X.y, X.x)) %>%
  dplyr::rename(id_polygon = id_polygon.x, 
                year_n = year_n.x, 
                month = month.x)

# Open brazil shapefile
coast <- sf::read_sf("./data/shp_brasil/brasil.shp")
plot(coast, xlim = c(-48.84, -47.00), ylim = c(-28.50, -25.00), 
     col = "gray", main = "")

# Kernel density 
kernel <- 
  kernel %>%
  dplyr::select(long, lat, effort_km)

dens <- KernSmooth::bkde2D(kernel, 
                           bandwidth = c(0.05, 0.05), 
                           gridsize = c(2000, 2000), 
                           range.x = list(c(-49.00, -44.00), c(-29.00, -23.00))) 

# Transform to raster
dens_raster <- raster::raster(list(x = dens$x1, y = dens$x2, z = dens$fhat))

par(mfrow = c(1, 1))
plot(dens_raster, xlim = c(-50, -44), ylim = c(-29, -23))
plot(coast, bg = "transparent", add = TRUE, col = 'grey')

## Clean environment
rm(list = "coast", "dens", "dens.raster", "df", "dfFortnight", "dfMonth", "dfMonthf", 
   "dfSeason", "dfWeek", "drift", "drift_i", "driftDistTime", "driftSpatial", 
   "driftSpatial_r", "driftSpatial_s", "driftSummary_Campaign", "driftSummary_Campaign_10", 
   "driftSummary_ReleaseStation", "driftSummary_ReleaseStation_10", "eff", "kernel", 
   "kernel1", "kernel2", "release_ReleaseStation", "release_ReleaseStation_10", 
   "step1", "step2", "strRateFortnight", "strRateSeason", "strRateWeek", "strRatePoly")

##----------------------------------------------------------------------------##
##                   Fisheries effort 'versus' Pontoporia strandings          ##
##----------------------------------------------------------------------------##
rm(list = ls())

## Read fishing effort data
fishingEffort <- 
  sf::read_sf("./data/PMAP_TOTALdiasdepesca/PMAP_TOTALdiasdepesca_final.shp") %>%
  sf::st_transform(crs = 4326)

## Use 'mesoregions' shapefile to crop fishing effrot data within 0-50m depth
clipFishingEffort <- 
  sf::read_sf("./data/shp_mesoregiao/poligonos_meso.shp") %>% 
  sf::st_transform(crs = 4326)

# Filtering and selecting to make it lighter to run
clipFishingEffort <- 
  clipFishingEffort %>% 
  dplyr::filter(PROF == "0 - 20" | PROF == "20 - 50") %>% 
  dplyr::select(MESORREGIA, geometry)

# mapview::mapview(clipFishingEffort)

## Crop the intersection between both spatial features
fishingEffort <- sf::st_intersection(fishingEffort, clipFishingEffort)

# mapview::mapview(fishingEffort)

## Clean 'fishingEffort' to make it lighter
fishingEffort <- 
  fishingEffort %>% 
  dplyr::select(X2017.1, X2017.2, 
                X2018.1, X2018.2, 
                X2019.1, X2019.2., 
                TOTAL., geometry) %>% 
  dplyr::rename(s2017_1 = X2017.1, 
                s2017_2 = X2017.2, 
                s2018_1 = X2018.1, 
                s2018_2 = X2018.2, 
                s2019_1 = X2019.1, 
                s2019_2 = X2019.2., 
                fishing_days_total = TOTAL.) %>% 
  sf::st_transform(crs = 4326)

## Read sectorPolygon_lines shapefile
sectorsPolygon <- 
  sf::read_sf("./data_out/sectorsPolygon.shp") %>% 
  sf::st_transform(crs = 4326)

## Join attributes from 'polygons' to 'fishingEffort'
fishingEffort <- sf::st_join(fishingEffort, sectorsPolygon)

# mapview::mapview(fishingEffort_test) + sectorsPolygon

plyr::count(is.na(fishingEffort$id_polygon)) ## 6 NA -- delete them

fishingEffort <- 
  fishingEffort %>% 
  as.data.frame() %>% 
  dplyr::filter(!is.na(id_polygon)) %>% 
  dplyr::select(-geometry) %>%
  tidyr::pivot_longer(!id_polygon, names_to = "semester", values_to = "fishing_days_effort") %>% 
  dplyr::filter(!semester == "fishing_days_total")

fishingEffort$semester <- as.factor(fishingEffort$semester)

## Open 'Pontoporia' dataset
pontoporia <- read.csv2("./data_out/pontoporia_stranding.csv")

pontoporia$semester <- as.character(pontoporia$semester)
pontoporia$semester <- stringr::str_replace(pontoporia$semester, "[:punct:]", "_")
pontoporia$semester <- paste0("s", pontoporia$semester)
pontoporia$semester <- as.factor(pontoporia$semester)

pontoporiaFishing <-
  pontoporia %>% 
  dplyr::filter(year >= 2017 & year < 2020) %>%
  droplevels()

## Summarise and combine data
pontoporiaFishingSummarised <- 
  pontoporiaFishing %>% 
  dplyr::group_by(id_polygon, semester) %>% 
  dplyr::summarise(n = n())

fishingEffortSummarised <- 
  fishingEffort %>% 
  dplyr::group_by(id_polygon, semester) %>% 
  dplyr::summarise(cumulative_fishing_effort = sum(fishing_days_effort))

pontoporiaVSfishing <- 
  dplyr::left_join(fishingEffortSummarised, pontoporiaFishingSummarised, 
                   by = c("id_polygon", "semester")) %>%
  replace(., is.na(.), 0)

## Plot -- all data
pontoporiaVSfishing_plot <- 
  ggplot2::qplot(x = cumulative_fishing_effort, y = n, color = semester,
                 data = pontoporiaVSfishing, geom = "point") +
  theme_bw()
