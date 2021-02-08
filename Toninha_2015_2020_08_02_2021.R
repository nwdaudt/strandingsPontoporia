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
library(KernSmooth)
library(raster)

################################################################################
############################## Dataset manipulation ############################
################################################################################

##----------------------------------------------------------------------------##
##                              Stranding dataset                             ##
##----------------------------------------------------------------------------##

## Open stranding dataset
pontoporia <- as.data.frame(
  readxl::read_xlsx("./data/Pontoporia PMP 2015_08_24 a 2020_06_11 SC_PR_SP_RJ.xlsx", 
                    sheet = 2))

## Filter columns
pontoporia <- 
  pontoporia %>% 
  dplyr::select(`Identificador do indivíduo`, Estado, Praia, Trecho, 
                `Estratégia do trecho`, `Tipo do monitoramento`, `Data/Hora`, 
                `Ponto - Lat`, `Ponto - Long`, `Condição da carcaça`, `OFAI - Sexo`)

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

## Remove stretch_scheme == "call", monitoring_type == "call" & stretch_scheme == "weekly", 
## monitoring_type == "call" & stretch_scheme == "fortnightly" and "date_hour" column;
## Create columns date, year, month, day, week, year_n, season and 
## fortnight_month;
## Filter survey effort from date > "September2015" and <= "2020-06-07".
pontoporia <- 
  pontoporia %>%
  dplyr::filter(stretch_scheme!= "call") %>% 
  dplyr::filter(!((monitoring_type == "call" & stretch_scheme == "weekly") |
                 (monitoring_type == "call" & stretch_scheme == "fortnightly"))) %>%
  dplyr::mutate(date = lubridate::as_date(date_hour)) %>% 
  dplyr::select(- date_hour) %>% 
  dplyr::filter(date > "2015-08-31" & date <= "2020-06-07") %>%
  dplyr::mutate(year = lubridate::year(date), 
                month = lubridate::month(date), 
                day = lubridate::day(date),
                week = lubridate::week(date)) %>%
  dplyr::mutate(season =
                  ifelse(month >= 1 & month <=3, "summer",
                  ifelse(month >= 4 & month <= 6, "autumn",
                  ifelse(month >= 7 & month <= 9, "winter", "spring")))) %>%
  dplyr::mutate(year_n = 
                  ifelse(date > "2015-08-31" & date < "2016-09-01", "Year1",
                  ifelse(date > "2016-08-31" & date < "2017-09-01", "Year2",
                  ifelse(date > "2017-08-31" & date < "2018-09-01", "Year3",
                  ifelse(date > "2018-08-31" & date < "2019-09-01", "Year4",
                                                                "Year5"))))) %>%
  dplyr::mutate(fortnight_month = as.numeric(
    ifelse(day <= 15, "1", "2"))) 

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
  dplyr::mutate(back_date = lubridate::as_date(ifelse(cod_decomposition == 2, 
                                                      date - 1, 
                                                      date - 6))) %>% 
  dplyr::mutate(zone = 
                ifelse(lat > -23.75, "1", 
                ifelse(lat < -23.75 & lat > -26, "2", "3"))) 

#- - - - - - -
## PS - zones were defined based on summaries related to mean drifting 
## distances from the 'drift_experiment'.
## See in general results, summary 'table5'.
#- - - - - - -

## Create column "macroregions" defined as in Cantor et al. (2020).


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
originalStretches <- do.call(rbind, lines)

## Remove streches with strategy == "noneeffort", compriment == "0",  
## and beach_name == "Ponta do Itaguá"; 
## Create an id for streches.
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

## Create a buffer of 300 m around the beach monitoring lines
originalStretches <- sf::st_transform(originalStretches, crs = 31982)
buffer <- sf::st_buffer(originalStretches, dist = 300)

## Transform 'pontoporia' into a geospatial feature
pontoporia <-
  pontoporia %>%
  dplyr::mutate(long1 = long)%>%
  dplyr::mutate(lat1 = lat)

pontoporiaSpatial <- 
  pontoporia %>%
  sf::st_as_sf(coords = c("long1","lat1"), crs = 4326)

## Trasform pontoporiaSpatial crs equal to originalStreches 
pontoporiaSpatial <- sf::st_transform(pontoporiaSpatial, crs = 31982)

## Remove records outside the buffer
pontoporiaSpatial$on_buffer <- lengths(sf::st_within(pontoporiaSpatial, buffer))
  
pontoporiaSpatial <-
  pontoporiaSpatial %>%  
  dplyr::filter(on_buffer > 0) %>% 
  dplyr::select(- on_buffer)

# mapview::mapview(buffer) + pontoporiaSpatial

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
## Create columns year, month, day, week, season and year_n.  
eff <-
  eff %>%
  dplyr::filter(date > "2015-08-31" & date <= "2020-06-07") %>%
  dplyr::filter(stretch_scheme != "call") %>% 
  dplyr::filter(complete != "NA") %>%
  dplyr::mutate(year = lubridate::year(date), 
                month = lubridate::month(date), 
                day = lubridate::day(date), 
                week = lubridate::week(date)) %>% 
  dplyr::mutate(season =
                  ifelse(month >= 1 & month <= 3, "summer",
                  ifelse(month >= 4 & month <= 6, "autumn",
                  ifelse(month >= 7 & month <= 9, "winter", "spring")))) %>%
  dplyr::mutate(year_n = 
                  ifelse(date > "2015-08-31" & date < "2016-09-01", "Year1",
                  ifelse(date > "2016-08-31" & date < "2017-09-01", "Year2",
                  ifelse(date > "2017-08-31" & date < "2018-09-01", "Year3",
                  ifelse(date > "2018-08-31" & date < "2019-09-01", "Year4",
                                                                "Year5"))))) %>%
  dplyr::mutate(fortnight_month = ifelse(day <= 15, "1", "2"))

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

## Removing excess of spaces between caracters
eff$beach <- gsub("\\s+", " ", eff$beach)
eff$stretch <-gsub("\\s+", " ", eff$stretch)

originalStretches$beach <- gsub("\\s+", " ", originalStretches$beach)
originalStretches$stretch <- gsub("\\s+", " ", originalStretches$stretch)

## Identify beaches and stretches in 'eff' 
## that are not in 'originalStretches' and vice verse
identify1 <- dplyr::setdiff(eff$beach,originalStretches$beach)
identify2 <- dplyr::setdiff(originalStretches$beach,eff$beach)
identify3 <- dplyr::setdiff(eff$stretch,originalStretches$stretch)
identify4 <- dplyr::setdiff(originalStretches$stretch,eff$stretch)

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
                stretch == "Praia de Garopaba N" | stretch=="Praia de Garopaba S" | 
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
select_duplicated_BeachNames <-
  originalStretches %>%
  dplyr::filter(beach == "...") # insert the beach name in "..." to visualize

# mapview::mapview(select_duplicated_BeachNames)

#---------------
# Conclusions:

# C1: Same beaches with different names in 'eff' and 'originalStretches':
# "Bal. Barra do Sul" == ""Barra do Sul - 2 - 395 - 396 - 67" and
# "Itararé" == "Praia do José Menino, Praia do Gonzaga, Praia do Boqueirão, Praia
# do Embaré, Praia Aparecida, Ponta da Praia".

# C2: Different beaches identified with the same name in 'eff' and 'originalStretches': 
#"Armação", "Brava","Camburizinho","Estaleiro","Pereque","Praia Brava","Praia Grande".

# C3: Beaches with two or more stretches in 'eff' and one in 'originalStretches': 
# "Praia da Barra N" and "Praia da Barra S" == "Praia da Barra";
# "Praia de Garopaba N" and "Praia de Garopaba S" == "Praia de Garopara";
# "antigo D15- Ilha Comprida" and "Ilha Comprida" == "Ilha Comprida";
# "Ipanema","Matinhos,Matinhos 2", "Pontal do Sul","Pontal do Sul/ Flamingo" and
# "Praia de Leste" == "Pontal do Sul/ Flamingo".

# C3.1: The stretch name "antigo D15- Ilha Comprida" was the unique stretch used
# for "Ilha Comprida" beach until 2017/03/23. Since then it was 
# rename to "Ilha Comprida".

# C3.2: The stretch "Pontal do Sul/ Flamingo" was used from spetember2015 to 2019/08/19.
# Since then it was split in five new strecthes: "Ipanema","Matinhos,Matinhos 2",
# "Pontal do Sul" and "Praia de Leste".
  
# C4:The stretch name "Antigo Superagui Sul e trapiche-Rio" was the unique stretch used
# for "Ilha do Superagui" beach until 2017/10/21. Since then it was split in two new stretches:
# "Superagui Trapiche-Rio Novo" and "Superagui Sul Novo.
#---------------

## Standardize beaches and stretches in 'eff' and 'originalStretches' according 
## to above conclusions

## C1: rename beaches "Bal. Barra do Sul" and "Itararé" as in 'originalStreches'.
effStandardize <-
  eff %>%
  dplyr::mutate(beach = recode_factor(beach, 
  "Bal. Barra do Sul" = "Barra do Sul - 2 - 395 - 396 - 67", 
  "Itararé" = "Praia do José Menino, Praia do Gonzaga, Praia do Boqueirão, Praia do Embaré, Praia Aparecida, Ponta da Praia"))

## C2: rename different beaches identified with the same name in 'eff' and 'originalStreches'
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
         ifelse(beach == "Praia Brava" & city == "Matinhos","Praia BravaMatinhos",
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
   dplyr::filter(stretch !="Praia de Garopaba N") %>%
   dplyr::filter(stretch != "Matinhos 1") %>%
   dplyr::filter(stretch != "Matinhos 2") %>%
   dplyr::filter(stretch != "Ipanema") %>%
   dplyr::filter(stretch != "Praia de Leste")

## Step2: rename the stretches as in originalStretches
effStandardize <-
   effStandardize %>%
   dplyr::mutate(stretch = 
           ifelse(beach == "Praia da Barra" & stretch == "Praia da Barra S",
                  "Praia da Barra",
           ifelse(beach == "Praia de Garopaba" & stretch == "Praia de Garopaba S",
                  "Praia de Garopaba",
           ifelse(beach == "Pontal do Sul/ Flamingo" & stretch == "Pontal do Sul",
                  "Pontal do Sul/ Flamingo",
           ifelse(beach == "Ilha Comprida" & stretch =="antigo D15- Ilha Comprida",
                  "Ilha Comprida",
                  stretch)))))

## C4: split "Antigo Superagui Sul e trapiche-Rio" in two stretches: "Superagui Sul Novo" and
## "Superagui Trapiche-Rio Novo".

## Step1: remove repeted date for stretch == "Antigo Superagui Sul e trapiche-Rio" in 'eff'
Superagui_df <-
  effStandardize %>%
  dplyr::filter(stretch == "Antigo Superagui Sul e trapiche-Rio")  
  
Superagui_df <- Superagui_df[!duplicated(Superagui_df$date), ]

## Step2: rename "Antigo Superagui Sul e trapiche-Rio" as "Superagui Sul Novo" 
Superagui_df$stretch <- as.factor(Superagui_df$stretch)

Superagui_df_SulNovo <- 
  Superagui_df %>%
  dplyr::mutate(stretch = recode_factor(stretch, 
            "Antigo Superagui Sul e trapiche-Rio" = "Superagui Sul Novo")) 
                  
## Step3: rename "Antigo Superagui Sul e trapiche-Rio" as "Superagui Trapiche-Rio Novo"
Superagui_df_TrapicheNovo <- 
  Superagui_df %>% 
  dplyr::mutate(stretch = recode_factor(stretch, 
        "Antigo Superagui Sul e trapiche-Rio" = "Superagui Trapiche-Rio Novo")) 

## Step4: combine "Superagui_df_TrapicheNovo" and "Superagui_df_SulNovo"
Superagui_df <- rbind(Superagui_df_SulNovo, Superagui_df_TrapicheNovo)

## Step5: combine "dfSuperagui" and "effStandardize"
effStandardize <- 
  effStandardize %>% 
  dplyr::filter(stretch != "Antigo Superagui Sul e trapiche-Rio")

effStandardize <- rbind(effStandardize, Superagui_df)  
 
## Create a id for 'effStandardize' and 'originaStrechesStandardize' based on 
## "beach" and "stretch" columns.
eff2 <-
  effStandardize %>%
  dplyr::mutate(beach_id = paste(beach, stretch,sep = "/")) %>% 
  dplyr::relocate(stretch, .before = beach) %>% 
  dplyr::relocate(beach_id, .after = beach)

originalStretches2 <- 
  originalStretchesStandardize %>% 
  dplyr::select(-c(beach_id, pmp_id, stretch_id, executing_, geometry, executin_1,
                   OBJECTID, stretch__1)) %>%
  dplyr::mutate(beach_id = paste(beach, stretch, sep = "/")) %>% 
  dplyr::select(id_original, executing1, stretch_st, stretch_ty, 
                stretch, beach, beach_id, original_length, geometry)

## Identify beaches and stretches in 'eff2' 
## that are not in 'originalStretches2' and vice verse
identify1 <- dplyr::setdiff(eff2$beach_id, 
                            originalStretches2$beach_id)
identify2 <- dplyr::setdiff(originalStretches2$beach_id,
                            eff2$beach_id)

##----------------------------------------------------------------------------##
##            Split monitored beach segments based on polygon sectors         ##
##----------------------------------------------------------------------------##

## Create a 30 latitudinal-band Sector polygon,
## which will be the basis for final analysis

## Trasform crs
originalStretches <- sf::st_transform(originalStretches2, crs = 4326)

# mapview(originalStretches)

## Latitudinal definition
n <- diff(sf::st_bbox(originalStretches)[c(2, 4)])/30
## Longitudinal definition
m <- diff(sf::st_bbox(originalStretches)[c(1, 3)])

## Create the 'sectorsPolygon' based on 'originalStretches' extend
sectorsPolygon <- sf::st_make_grid(originalStretches, cellsize = c(m, n))

## Set the correct spatial attributes for 'sectorsPolygon' layer
## Define 'sectorsPolygon' as single feature
sectorsPolygon <- sf::st_sf(sectorsPolygon)

## Create an identifier for each sector
sectorsPolygon$id_polygon <- 1:nrow(sectorsPolygon)

## Define 'sectorsPolygon' as a multipolygon
sectorsPolygon <- 
  sectorsPolygon %>% 
  sf::st_cast("MULTIPOLYGON")

# mapview::mapview(sectorsPolygon)+originalStretches

## Create a multiline feature based on 'sectorsPolygon' id limits
sectorsPolygon_lines <- sf::st_cast(sectorsPolygon, "MULTILINESTRING", 
                                    group_or_split = FALSE)

# mapview::mapview(sectorsPolygon_lines)

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

## Set "new_length" and "original_length" in kilometers and Obtained the
## difference between them.
newStretches <- 
  newStretches %>%
  dplyr::mutate(new_length = as.numeric(new_length/1000)) %>%
  dplyr::mutate(original_length = as.numeric(original_length/1000)) %>%
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
  dplyr::rename(stretch_scheme = stretch_st) %>%
  dplyr::rename(new_length_km = new_length) %>%
  sf::st_as_sf() %>% 
  sf::st_set_crs(4326)

# mapview::mapview(sectorsPolygon) + newStretches2

##----------------------------------------------------------------------------##
##                  Join'pontoporia' and eff with 'newStretches'              ##
##----------------------------------------------------------------------------##

## Trasform crs
pontoporiaSpatial <- sf::st_transform(pontoporiaSpatial, crs = 4326)

## Join attributes from 'newStretches' into 'pontoporiaSpatial'
pontoporiaSpatialJoin <- 
  sf::st_join(pontoporiaSpatial, newStretches, 
         join = sf::st_nearest_feature)

## Return to df format; clean, rename and reorganize the columns.
pontoporia <- 
  pontoporiaSpatialJoin %>% 
  as.data.frame() %>% 
  dplyr::rename(beach = beach.x) %>% 
  dplyr::rename(stretch = stretch.x) %>% 
  dplyr::rename(stretch_scheme = stretch_scheme.x) %>%
  dplyr::select(id_original, id_newStretches, id_polygon, zone, 
                id_individual, state, beach, stretch, beach_id, new_length_km, 
                stretch_scheme, monitoring_type, lat, long, 
                date, year, year_n, month, season, fortnight_id, week, day, 
                back_date, cod_decomposition, sex)

## Join attributes from 'newStretches' into 'eff2'
effJoin <- merge(eff2, newStretches, by= "beach_id", all = T)

## Clean, rename and reorganize the columns 
eff <- 
  effJoin %>% 
  dplyr::rename(beach = beach.x) %>% 
  dplyr::rename(stretch = stretch.x) %>% 
  dplyr::rename(stretch_scheme = stretch_scheme.x) %>% 
  dplyr::select(code, id_original, id_newStretches, id_polygon, executing1, 
                state, beach, stretch, beach_id, new_length_km, 
                stretch_scheme, type, initial_lat, initial_long, complete, 
                date, year, month, season, fortnight_id, week, day, year_n) %>%
  dplyr::arrange(date)

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

## Remove strandings in 'pontoporia' recorded during incomplet beach surveys. 
pontoporia <- dplyr::anti_join(pontoporia, eff_i, by = "date_id_newStretches")

## Remove "date_id_newStretches" from 'pontoporia'
pontoporia <-
  pontoporia %>% 
  dplyr::select(- date_id_newStretches)

# write.csv2(pontoporia,"./data_out/pontoporia_stranding.csv")

##----------------------------------------------------------------------------##
##                  Join environment data with pontoporia df                  ##
##----------------------------------------------------------------------------##

## The enviromental data was collected from ERA5
## through a Python routine files = {apiRequest.py} + {requirements.txt}

## Open environment variables dataset
envVariables <- as.data.frame(
  utils::read.csv("./data/environmental_data/envVariables.csv"))

## Remove id column
envVariables <- envVariables %>% dplyr::select(-1)

## Melt env variables
envVariables_melt <- reshape2::melt(envVariables, id.vars = "fileName")

## Extract only the info of interest (numeric part)
envVariables_melt <- 
  envVariables_melt %>% 
  dplyr::mutate(value = gsub("dtype=float32", "", value)) %>% 
  dplyr::mutate(value = gsub("<xarray.Variable", "", value)) %>% 
  dplyr::mutate(value = gsub("[^0-9.-]", "", value)) %>% 
  dplyr::mutate(value = as.numeric(value)) %>% 
  dplyr::mutate(fileName = gsub("WTUVP", "", fileName)) %>% 
  dplyr::mutate(fileName = gsub(".nc", "", fileName))

## Return df to wide format
envVariablesNew <- tidyr::pivot_wider(envVariables_melt, 
                                      names_from = variable, 
                                      values_from = value)

## Set equal ID's prior to merge
pontoporia$id_individual <- as.character(as.numeric(pontoporia$id_individual))
envVariablesNew$fileName <- as.character(as.numeric(envVariablesNew$fileName))

## Merge
pontoporiaAndEnv <- merge(pontoporia, envVariablesNew, 
                               by.x = "id_individual", by.y = "fileName")

## Renaming Env columns
pontoporia <- 
  pontoporiaAndEnv %>% 
  dplyr::rename(u_wind = u10mean,
                u_wind_min = u10min,
                u_wind_max = u10max,
                v_wind = v10mean,
                v_wind_min = v10min,
                v_wind_max = v10max,
                mean_wave_dir = mwdmean,
                mean_wave_dir_max = mwdmax,
                mean_wave_dir_min = mwdmin,
                mean_wave_period = mwpmean,
                mean_wave_period_min = mwpmin,
                mean_wave_peiod_max = mwpmax,
                sig_height_wind_wave = shwwmean,
                sig_height_wind_wave_min = shwwmin,
                sig_height_wind_wave_max = shwwmax)

##----------------------------------------------------------------------------##
##                Summarize by polygon, month, fortnight and week             ##
##----------------------------------------------------------------------------##

## Obtain completed survey effort df
eff_c <- 
  eff %>% 
  dplyr::filter(complete != "no")

## Summarise 'pontoporia' and 'eff_c':

#-------------------
#categorical<- pontoporia[18:20]
#vnames <- colnames(categorical)
#pon <- list()
#j = 1
#for (i in categorical) {
#  pon[[j]] <- 
#    pontoporia %>% 
#    group_by(year_n,id_polygon, {{i}}) %>% 
#    summarise(y = n())%>%
#    dplyr::mutate_at(c(1:3),as.factor)
#  pon[[j]]$vnames = vnames[j]
#  j = j + 1
#}

#for (i in seq(pon))
#  assign(paste0("pon", i), pon[[i]])
#----------------------

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
##                                Drift dataset                               ##
##----------------------------------------------------------------------------##

## Open drift experiment dataset
drift <- utils::read.csv("./data/drift_experiment_data_2.csv", sep = ";")

# drift <-     ### Jony
#   drift %>%
#   dplyr::rename(campaign=ï..campaign)

## Create "zoneExp" column 
drift <- 
  drift %>% 
  dplyr::mutate(zoneExp = 
      ifelse(lat_r > -23.8, "1", 
      ifelse(lat_r < -23.8 & lat_r > -24.4,"2", 
      ifelse(lat_r < -24.4 & lat_r > -26, "3", "4"))))

drift$zoneExp <- as.factor(drift$zoneExp)

#------------
# The limits of the zoneExp was defined based on the distance between the 
# release station and the coast line using the 20 m isobath as a reference.
#------------

## View the zoneExp 

## Transform df into a geospatial feature
driftSpatial <- 
  drift %>% sf::st_as_sf(coords = c("long_r", "lat_r"), crs = 4326)

## Create the 'sectorsZoneExp' based on 'DriftSpatial' extend

# mapview(sectorsZoneExp)

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

## Open the Brazilian coast shapefile
brazil <- 
  sf::read_sf(dsn = "./data/shp_brasil", layer = "brasil") %>% 
  sf::st_set_crs(4326)

## Figure 1. Study area
figure1 <- 
  ggplot() + 
  ggspatial::geom_sf(data = brazil) +
  geom_sf(data = originalStretches, color = "red", show.legend = F) + 
  labs(x = "Longitude", y = "Latitude") + 
  coord_sf(xlim = c(-50.5, -44), ylim = c(-28.5, -23)) + 
  theme_bw() + 
  theme(axis.title = element_text(colour = "black", size = 16), 
        axis.text = element_text(colour = "black", size = 10), 
        axis.line = element_line(colour = "black")) + 
  annotation_scale(location = "br", width_hint = 0.3) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.45, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) + 
  annotate(geom = "text", x = -46, y = -26, label = "Atlantic Ocean", 
           color = "grey22", size = 5) +
  annotate(geom = "text", x = -47.5, y = -23.58, label = "São Paulo", 
           color = "grey22", size = 5) +
  annotate(geom = "text", x = -49.8, y = -25, label = "Paraná", 
           color = "grey22", size = 5) +
  annotate(geom = "text", x = -49.8, y = -27.3, label = "Santa Catarina", 
           color = "grey22", size = 5)

## Figure 2. Latitudinal division of the study area. 
figure2 <- 
  ggplot() + 
  ggspatial::geom_sf(data = brazil) + 
  geom_sf(data = originalStretches, 
          color = "red", show.legend = F) + 
  geom_sf(data = sectorsPolygon_lines, 
          color = "black", show.legend = F) + 
  labs(x = "Longitude", y = "Latitude") + 
  coord_sf(xlim = c(-50.5, -44), ylim = c(-28.5, -23)) + 
  theme_bw() + 
  theme(axis.title = element_text(colour = "black", size = 16), 
        axis.text = element_text(colour = "black", size = 10), 
        axis.line = element_line(colour = "black")) + 
  annotation_scale(location = "br", width_hint = 0.3,
                   pad_x = unit(0.1, "in"), pad_y = unit(0.05, "in")) + 
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.10, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) + 
  annotate(geom = "text", x = -46, y = -26, label = "Atlantic Ocean", 
           color = "grey22", size = 5) + 
  annotate(geom = "text", x = -47.5, y = -23.20, label = "São Paulo", 
           color = "grey22", size = 5) + 
  annotate(geom = "text", x = -49.8, y = -25, label = "Paraná", 
           color = "grey22", size = 5) + 
  annotate(geom = "text", x = -49.8, y = -27.3, label = "Santa Catarina", 
           color = "grey22", size = 5)

# Figure 3. Release station map 
driftSpatial <-
  drift %>%
  dplyr::group_by(campaign, id, zoneExp) %>%
  dplyr::filter(!duplicated(lat_r)) %>%
  sf::st_as_sf(coords = c("long_r", "lat_r"), crs = 4326) %>%
  dplyr::mutate(campaign = as.factor(campaign))

figure3 <-       #### Conferir, nao esta plotando os eixos 'Lat/Long'
  ggplot() + 
  ggspatial::geom_sf(data = brazil) + 
  ggspatial::geom_sf(data = driftSpatial, aes(color = campaign), 
                     show.legend = T) + 
  coord_sf(xlim = c(-50.5, -44), ylim = c(-28.5, -23)) + 
  labs(x = "Longitude", y = "Latitude") + 
  theme_bw() + 
  theme(axis.title = element_text(colour = "black", size = 0), 
        axis.text = element_text(colour = "black", size = 10), 
        axis.line = element_line(colour = "black")) + 
  annotation_scale(location = "br", width_hint = 0.3) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.45, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) + 
  annotate(geom = "text", x = -46, y = -26, label = "Atlantic Ocean", 
           color = "grey22", size = 5) +
  annotate(geom = "text", x = -47.5, y = -23.58, label = "São Paulo", 
           color = "grey22", size = 5) +
  annotate(geom = "text", x = -49.8, y = -25, label = "Paraná", 
           color = "grey22", size = 5) +
  annotate(geom = "text", x = -49.8, y = -27.3, label = "Santa Catarina", 
           color = "grey22", size = 5)

## Figure 4. Create zoneExp map

## Figure 5. Create zone map

## Table1. Release station by "campaign", "id", "zoneExp". 
table1 <-
  drift %>% 
  dplyr::select(c(1:7, zoneExp)) %>% 
  dplyr::group_by(campaign, id, zoneExp) %>% 
  dplyr::filter(!duplicated(lat_r)) %>%
  dplyr::ungroup() %>%
  dplyr::select(- hour_r) %>%
  dplyr::mutate(release = c(rep(33, 18), 30, rep(33, 8))) %>% # number of drifters released
  dplyr::relocate(release, .after = id)

names(table1) <- c("Campign", "State", "Release station"," No.drifters",
                   "Date", "Lat", "Long","Drift zone")

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
  dplyr::select(campaign , id, state, zoneExp, 
                 date_r, date_s, 
                 lat_r, long_r, 
                 lat_s, long_s) %>% 
  dplyr::mutate(dist = sf::st_distance(driftSpatial_r$geometry, 
                                       driftSpatial_s$geometry, 
                                       by_element = TRUE)) %>% 
  dplyr::mutate(time_lag = (date_s) - (date_r))


## General summary

## Table 2. Drift by "campaign" and "id"
driftSummary_Campaign <- 
  driftDistTime %>% 
  dplyr::group_by(campaign , id) %>% 
  dplyr::summarise(stranding = n(),
                   meanDist = round(mean((as.numeric(dist)/1000)),digits = 1),
                   sdDist = round(sd((as.numeric(dist)/1000)),digits = 1), 
                   minDist = round (min((as.numeric(dist)/1000)),digits = 1), 
                   maxDist = round(max((as.numeric(dist)/1000)),digits = 1), 
                   meanTime = round(mean(as.numeric(time_lag)),digits = 1), 
                   sdTime = round(sd(as.numeric(time_lag)),digits = 1), 
                   minTime = round (min(as.numeric(time_lag)),digits = 1), 
                   maxTime = round(max(as.numeric(time_lag)),digits = 1))

## Adding campaigns without stranding records
df <- tibble::tibble(campaign = c("1","1","1","1","1","1","1"), 
                     id = c("C1","C2","C3","P1","P2","P3","S3"),       
                     stranding = c(rep(0,7)), 
                     meanDist = c(rep(0,7)), sdDist = c(rep(0,7)), 
                     minDist = c(rep(0,7)), maxDist = c(rep(0,7)), 
                     meanTime = c(rep(0,7)), sdTime = c(rep(0,7)),
                     minTime =c(rep(0,7)), maxTime = c(rep(0,7)))

## Combine df with 'driftSummary_Campaign'
driftSummary_Campaign <- rbind(df, driftSummary_Campaign)

## Create a column "release" and "percentage"
release_campaign <- c(rep(33, 18), 30, rep(33, 8)) 

table2 <-
    driftSummary_Campaign %>%
    dplyr::arrange(campaign, id) %>%
    dplyr::mutate(release = release_campaign) %>%
    dplyr::mutate(percentage = round(stranding/release*100, digits = 1)) %>%
    dplyr::relocate(release, .after = id) %>%
    dplyr::relocate(percentage, .after = stranding)

## Table 3. Drift by "zoneExp"
driftSummary_ZoneExp <- 
  driftDistTime %>% 
  dplyr::group_by(zoneExp) %>% 
  dplyr::summarise(stranded = n(), 
                   meanDist = round(mean((as.numeric(dist)/1000)),digits = 1), 
                   sdDist = round(sd((as.numeric(dist)/1000)),digits = 1), 
                   minDist = round(min((as.numeric(dist)/1000)),digits = 1), 
                   maxDist = round(max((as.numeric(dist)/1000)),digits = 1), 
                   meanTime = round(mean(as.numeric(time_lag)),digits = 1), 
                   sdTime = round(sd(as.numeric(time_lag)),digits = 1), 
                   minTime = round(min(as.numeric(time_lag)),digits = 1), 
                   maxTime = round(max(as.numeric(time_lag)),digits = 1))

## Create a column "release" and "percentage"
release_zoneExp <-
  driftDistTime %>%
  dplyr::group_by(zoneExp, id, campaign) %>% 
  dplyr::summarise(n = n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(released = c(rep(33, 15), 30, rep(33, 4))) %>% 
  dplyr::group_by(zoneExp) %>%
  dplyr::summarise(released = sum(released))

table3 <-
  driftSummary_ZoneExp %>%
  dplyr::left_join(release_zoneExp, by = "zoneExp") %>% 
  dplyr::relocate(released, .after = zoneExp) %>%
  dplyr::mutate(percentage = round(stranded/released*100, digits = 1)) %>% 
  dplyr::relocate(percentage, .after = stranded)

## Summaries from less than 10 days drifting

## Table 4. Drift by "campaign" and "id"
driftSummary_Campaign_10 <- 
  driftDistTime %>% 
  dplyr::filter(time_lag < 10) %>% 
  dplyr::group_by(campaign, id) %>% 
  dplyr::summarise(stranded = n(), 
                   meanDist = round(mean((as.numeric(dist)/1000)), digits = 1),
                   sdDist = round(sd((as.numeric(dist)/1000)), digits = 1), 
                   minDist = round (min((as.numeric(dist)/1000)), digits = 1), 
                   maxDist = round(max((as.numeric(dist)/1000)), digits = 1), 
                   meanTime = round(mean(as.numeric(time_lag)), digits = 1), 
                   sdTime = round(sd(as.numeric(time_lag)), digits = 1), 
                   minTime = round (min(as.numeric(time_lag)), digits = 1), 
                   maxTime = round(max(as.numeric(time_lag)), digits = 1))

## Adding campaigns without stranding records
df <- tibble::tibble(campaign = c("1","1","1","1","1","1","1","1","2"), 
                     id = c("C1","C2","C3","P1","P2","P3","S1","S3","C2"),       
                     stranded = c(rep(0,9)), 
                     meanDist = c(rep(0,9)), sdDist = c(rep(0,9)), 
                     minDist = c(rep(0,9)), maxDist = c(rep(0,9)), 
                     meanTime = c(rep(0,9)), sdTime = c(rep(0,9)),
                     minTime =c(rep(0,9)),maxTime = c(rep(0,9)))

## Combine df with driftSummary_Campaign_10
driftSummary_Campaign_10 <- rbind(df, driftSummary_Campaign_10)

table4 <- 
  driftSummary_Campaign_10 %>% 
  dplyr::arrange(campaign, id) %>% 
  dplyr::mutate(released = release_campaign)%>%
  dplyr::mutate(percentage = round(stranded/released*100, digits = 1)) %>%
  dplyr::relocate(released, .after = id) %>%
  dplyr::relocate(percentage, .after = stranded)

## Table 5. Drift by zoneExp
driftSummary_ZoneExp_10 <-
  driftDistTime %>% 
  dplyr::filter(time_lag < 10) %>% 
  dplyr::group_by(zoneExp) %>% 
  dplyr::summarise(stranded = n(), 
                   meanDist = round(mean((as.numeric(dist)/1000)),digits = 1), 
                   sdDist = round(sd((as.numeric(dist)/1000)),digits = 1), 
                   minDist = round(min((as.numeric(dist)/1000)),digits = 1), 
                   maxDist = round(max((as.numeric(dist)/1000)),digits = 1), 
                   meanTime = round(mean(as.numeric(time_lag)),digits = 1), 
                   sdTime = round(sd(as.numeric(time_lag)),digits = 1), 
                   minTime = round(min(as.numeric(time_lag)),digits = 1), 
                   maxTime = round(max(as.numeric(time_lag)),digits = 1))

## Create a column "release" and "percentage"
release_zoneExp_10 <-
  driftDistTime %>%
  dplyr::group_by(zoneExp, id, campaign) %>%
  summarise(n = n()) %>%
  dplyr::ungroup() %>%
  mutate(released = c(rep(33, 15), 30, rep(33, 4))) %>%
  dplyr::group_by(zoneExp) %>%
  dplyr::summarise(released = sum(released))

table5 <-    #### This was the basis for "zones" in 'pontoporia' df.
  driftSummary_ZoneExp_10 %>%
  dplyr::left_join(release_zoneExp_10, by = "zoneExp") %>%
  dplyr::relocate(released, .after = zoneExp) %>%
  dplyr::mutate(percentage = round(stranded/released*100, digits = 1)) %>%
  dplyr::relocate(percentage, .after = stranded)

##----------------------------------------------------------------------------##
##                       Spatial and temporal stranding rate                  ##           
##----------------------------------------------------------------------------##

season <- read.csv2("./data_out/ponSeasonFinal.csv")
df_month <- read.csv2("./data_out/ponMonthFinal.csv")
fortnight <- read.csv2("./data_out/ponFortnightFinal.csv")
week <- read.csv2("./data_out/ponWeekFinal.csv")

##
## Temporal pattern
##

## Per "Season"

strRateSeason <- 
  season %>%
  dplyr::group_by(season, year_n) %>% 
  dplyr::summarise(effort_km = sum(effort_km), y = sum(y)) %>%
  dplyr::mutate(str_rate = as.numeric(y*1000/effort_km))

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
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA)) + 
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  theme(strip.text.x = element_text(size = 18, face = "italic"))

## Per "month"

# Create the column "monthf", gruop by monthf and create the column "srt_rate" 
df_month <-
  ponMonthFinal %>%
  dplyr::mutate(monthf = 
         factor(month, levels = as.character(1:12), 
                       labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                                  ordered = FALSE)) %>% 
  dplyr::relocate(monthf, .before = "month")%>%
  dplyr::mutate(monthf = as.factor(monthf)) %>%
  dplyr::group_by(year_n, monthf) %>%
  dplyr::summarise(y = sum(y), effort_km = sum(effort_km)) %>% 
  dplyr::mutate(str_rate = as.numeric(y*1000/effort_km))

str_rate_month_plot <- 
  ggplot2::ggplot(df_month, aes(x = monthf, y = str_rate, group = year_n)) + 
  geom_line(aes(color = year_n)) + 
  xlab("Month") + ylab("Stranding rate (ind./1000 km)") + 
  theme(legend.position = "right") +
  theme(legend.title = element_blank()) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank()) + 
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA)) + 
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  theme(strip.text.x = element_text(size = 18, face = "italic"))

## Per "fortnight"

strRateFortnight <- 
  fortnight %>%
  dplyr::group_by(fortnight_id, year_n) %>% 
  dplyr::summarise(effort_km = sum(effort_km), y = sum(y)) %>%
  dplyr::mutate(str_rate = as.numeric (y*1000/effort_km))

str_rate_fortnight_plot <-
  ggplot2::ggplot(str_rate_fortnight, 
                  aes(x = fortnight_id, y = str_rate, group = year_n)) + 
  geom_line(aes(color = year_n)) + 
  xlab("Fortnight") + ylab("Stranding rate (ind./1000 km)") + 
  theme(legend.position = "right") +
  theme(legend.title = element_blank()) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank()) + 
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA)) + 
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  theme(strip.text.x = element_text(size = 18, face = "italic"))

## Per "week"

str_rate_week <- 
  week %>%
  dplyr::group_by(week, year_n) %>% 
  dplyr::summarise(effort_km = sum(effort_km), y = sum(y)) %>%
  dplyr::mutate(str_rate = as.numeric(y*1000/effort_km))

str_rate_week_plot <-
  ggplot2::ggplot(str_rate_week, 
                  aes(x = week, y = str_rate, group = year_n)) + 
  geom_line(aes(color = year_n)) +
  xlab("Week") + ylab("Stranding rate (ind./1000 km)") + 
  theme(legend.position = "right") +
  theme(legend.title = element_blank()) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank()) + 
  theme(axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA)) + 
  theme(axis.title.x = element_text(size = 15), 
        axis.title.y = element_text(size = 15))+
  theme(strip.text.x = element_text(size = 18, face = "italic"))

##
## Spatial pattern
##

str_rate_poly <- 
  ponMonthFinal %>% 
  dplyr::group_by(id_polygon, year_n) %>% 
  dplyr::summarise(effort_km = sum(effort_km), y = sum(y)) %>%
  dplyr::mutate(str_rate = as.numeric(y*1000/effort_km))

str_rate_poly_plot <-
  ggplot2::ggplot(str_rate_poly, 
                  aes(x = id_polygon, y = str_rate, group = year_n)) + 
  geom_line(aes(color = year_n)) + 
  xlab("Polygon") + ylab("Stranding rate (ind./1000 km)")+ 
  theme(legend.position = "right") +
  theme(legend.title = element_blank()) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank()) + 
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))+ 
  theme(panel.border = element_rect(colour = "black", fill = NA))+ 
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  theme(strip.text.x = element_text(size = 18, face = "italic"))

## Kernel density
# Open pontoporia stranding and ponMOnthFinal dataset
kernel1 <- read.csv2("./data_out/pontoporia_stranding.csv")
kernel2 <- read.csv2("./data_out/ponMonthFinal.csv")

# Create an id for kerne1 and kernel2
kernel1 <-
  kernel1 %>%
  dply::mutate(kernel_id = paste(id_polygon, year_n, month))

kernel2 <-
  kernel2 %>%
  dply::mutate(kernel_id = paste(id_polygon, year_n, month))

# Join effort_km from 'kernel2' into 'kernel1'
kernel <- merge(kernel1, kernel2, by = "kernel_id", all = T)

# Clean and remove NAs 
kernel <-
  kernel %>%
  tidyr::drop_na() %>%
  dplyr::select(-c(id_polygon.y, year_n.y, month.y, y, X.y, X.x)) %>%
  dplyr::rename(id_polygon = id_polygon.x) %>%
  dplyr::rename(year_n = year_n.x) %>%
  dplyr::rename(month = month.x)

# Open brazil shapefile
coast <- read_sf("./data/shp_brasil/brasil.shp")
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
dens.raster <- raster::raster(list(x = dens$x1, y = dens$x2, z = dens$fhat))

par(mfrow = c(1, 1))
plot(dens.raster, xlim = c(-50, -44), ylim = c(-29, -23))
plot(coast, bg = "transparent", add = TRUE, col = 'grey')
