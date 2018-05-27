#### TFM: ANÁLISIS Y PREDICCIÓN DEL RESTRASO EN LOS VUELOS ####

#### 2. DATOS DE ENTRADA: Normalización de los datos de entrada ####


# *************************************************************************************************
##### 2.1. Bloque de carga de librerias #####

list.of.packages <- c("data.table", "dplyr", "tidyr","lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(data.table)
library(dplyr)
# *************************************************************************************************


# *************************************************************************************************
##### 2.2. Bloque de carga de datos #####
# Selección de ruta, en mi caso: "/Users/ellorentesj/repostGitHub/TFM/data/"
setwd("/Users/ellorentesj/repostGitHub/TFM/data/")
flights <- fread("flights.csv", header=T, sep=',')
# *************************************************************************************************


# *************************************************************************************************
##### 2.3. Bloque de revisión basica del dataset #####
str(flights)
summary(flights)
# *************************************************************************************************


# *************************************************************************************************
#### 2.4.4. Normalización  ####
# Procedo a analizar las variables que contienen NA's y decidir que hacer con ellas 
colSums(is.na(flights))>0

#### 2.4.4.2. WeatherDelay  ####
flights %>% 
  filter(is.na(WeatherDelay)) %>% 
  nrow()
# Asumo que si la variable no contiene información en alguno de sus campos es por que no tiene 
# retraso, por tanto cumplimento los NA's con 0's
flights <- flights %>%
  mutate(WeatherDelay = coalesce(as.integer(WeatherDelay),0L))

#### 2.4.4.2. NASDelay  ####
flights %>% 
  filter(is.na(NASDelay)) %>% 
  nrow()
# Asumo que si la variable no contiene información en alguno de sus campos es por que no tiene 
# retraso, por tanto cumplimento los NA's con 0's
flights <- flights %>%
  mutate(NASDelay = coalesce(as.integer(NASDelay),0L))

#### 2.4.4.2. SecurityDelay  ####
flights %>% 
  filter(is.na(SecurityDelay)) %>% 
  nrow()
# Asumo que si la variable no contiene información en alguno de sus campos es por que no tiene 
# retraso, por tanto cumplimento los NA's con 0's
flights <- flights %>%
  mutate(SecurityDelay = coalesce(as.integer(SecurityDelay),0L))

#### 2.4.4.2. LateAircraftDelay  ####
flights %>% 
  filter(is.na(LateAircraftDelay)) %>% 
  nrow()
# Asumo que si la variable no contiene información en alguno de sus campos es por que no tiene 
# retraso, por tanto cumplimento los NA's con 0's
flights <- flights %>%
  mutate(LateAircraftDelay = coalesce(as.integer(LateAircraftDelay),0L))

# *************************************************************************************************

# Guardo el dataset
write.table(flights, file = "finalFlights.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)

# Top 3 de destinos que tienen más vuelos retrasados, 
flights %>% 
  group_by(Dest) %>% 
  summarise(totalArrDel15 = sum(ArrDel15==1), ProRetDel = (sum(ArrDel15==1)/nrow(flights))*100) %>% 
  arrange(-totalArrDel15) %>% 
  top_n(3)
#   Origin totalDel15 ProRetDel totalArrDel15  ProRetArr
#   <fct>       <int>     <dbl>         <int>      <dbl>
# 1 ORD         16437      1.74         17119       1.81
# 2 ATL         17053      1.81         15283       1.62
# 3 DEN         14102      1.49         13929       1.48
flights %>% 
  count(Dest) %>% 
  arrange(-n) %>% 
  top_n(3)
flights %>% 
  count(Origin) %>% 
  arrange(-n) %>% 
  top_n(3)

