#### TFM: ANÁLISIS Y PREDICCIÓN DEL RESTRASO EN LOS VUELOS ####

#### 1. PREPROCESAMIENTO DE LOS FICHEROS: Lectura de ficheros y primera limpieza de variables innecesarias ####

# Los datos de los vuelos se obtienen de: https://www.transtats.bts.gov/DL_SelectFields.asp

# *************************************************************************************************
##### 1.1. Bloque de carga de librerias #####
list.of.packages <- c("data.table","utils","dplyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(data.table)
library(utils)
library(dplyr)
# *************************************************************************************************


# *************************************************************************************************
##### 1.2. Bloque de carga de datos #####
# ¡CUIDADO! Es necesario modificar la ruta por la correspondiente en el equipo donde se vaya a 
# ejecutar el código para su correcto funcionamiento
# Selección de ruta donde están los ficheros, en mi caso: "/Users/ellorentesj/repostGitHub/TFM/data/"
setwd("/Users/ellorentesj/repostGitHub/TFM/data/")

# Declaro variables 'tempDec' y 'tempJan' como archivos temporeales
tempDec <- tempfile()
tempJan <- tempfile()

# Asigno las variables a los archivos
tempDec = "On_Time_On_Time_Performance_2013_12.csv.zip"
tempJan = "On_Time_On_Time_Performance_2014_1.csv.zip"

# Utilizo la función unzip para extraer los archivos CSV
unzip(tempDec,"On_Time_On_Time_Performance_2013_12.csv")
unzip(tempJan, "On_Time_On_Time_Performance_2014_1.csv")

# Introduzco los datos de los CSV en dataframes
dfDecember <- fread("On_Time_On_Time_Performance_2013_12.csv", header=T, sep=',')
dfJanuary <- fread("On_Time_On_Time_Performance_2014_1.csv", header=T, sep=',')

# Elimino la referencia a los archivos temporales
rm(tempDec)
rm(tempJan)

flightsAux <- rbind(dfDecember,dfJanuary)
# *************************************************************************************************


# *************************************************************************************************
##### 1.3. Bloque de limpieza de datos #####
# Limpieza del dataset, elimino las variables que tienen >99.99 perdida de información, puesto que
# esto indica que es una variable sin datos válidos. Y elimino los datos no significativos que no 
# aportan ningún tipo de información para el análisis.
clenEmptyData <- function (dfFlightsAux){
  
  dfFlightsAux$Quarter <- NULL
  dfFlightsAux$TailNum <- NULL
  dfFlightsAux$OriginStateFips <- NULL
  dfFlightsAux$OriginStateName <- NULL
  dfFlightsAux$OriginWac <- NULL
  dfFlightsAux$DestStateFips <- NULL
  dfFlightsAux$DestStateName <- NULL
  dfFlightsAux$DestWac <- NULL
  dfFlightsAux$CRSDepTime <- NULL
  dfFlightsAux$DepartureDelayGroups <- NULL
  dfFlightsAux$DepTimeBlk <- NULL
  dfFlightsAux$WheelsOff <- NULL
  dfFlightsAux$WheelsOn <- NULL
  dfFlightsAux$CRSArrTime <- NULL
  dfFlightsAux$ArrivalDelayGroups <- NULL
  dfFlightsAux$ArrTimeBlk <- NULL
  dfFlightsAux$Diverted <- NULL
  dfFlightsAux$CRSElapsedTime <- NULL
  dfFlightsAux$ActualElapsedTime <- NULL
  dfFlightsAux$Flights <- NULL
  dfFlightsAux$DistanceGroup <- NULL
  dfFlightsAux$FirstDepTime <- NULL
  dfFlightsAux$TotalAddGTime <- NULL
  dfFlightsAux$LongestAddGTime <- NULL
  dfFlightsAux$DivAirportLandings <- NULL
  dfFlightsAux$DivReachedDest <- NULL
  dfFlightsAux$DivActualElapsedTime <- NULL
  dfFlightsAux$DivArrDelay <- NULL
  dfFlightsAux$DivDistance <- NULL
  dfFlightsAux$Div1Airport <- NULL
  dfFlightsAux$Div1AirportID <- NULL
  dfFlightsAux$Div1AirportSeqID <- NULL
  dfFlightsAux$Div1WheelsOn <- NULL
  dfFlightsAux$Div1TotalGTime <- NULL
  dfFlightsAux$Div1LongestGTime <- NULL
  dfFlightsAux$Div1WheelsOff <- NULL
  dfFlightsAux$Div1TailNum <- NULL
  dfFlightsAux$Div2Airport <- NULL
  dfFlightsAux$Div2AirportID <- NULL
  dfFlightsAux$Div2AirportSeqID <- NULL
  dfFlightsAux$Div2WheelsOn <- NULL
  dfFlightsAux$Div2TotalGTime <- NULL
  dfFlightsAux$Div2LongestGTime <- NULL
  dfFlightsAux$Div2WheelsOff <- NULL
  dfFlightsAux$Div2TailNum <- NULL
  dfFlightsAux$Div3Airport <- NULL
  dfFlightsAux$Div3AirportID <- NULL
  dfFlightsAux$Div3AirportSeqID <- NULL
  dfFlightsAux$Div3WheelsOn <- NULL
  dfFlightsAux$Div3TotalGTime <- NULL
  dfFlightsAux$Div3LongestGTime <- NULL
  dfFlightsAux$Div3WheelsOff <- NULL
  dfFlightsAux$Div3TailNum <- NULL
  dfFlightsAux$Div4Airport <- NULL
  dfFlightsAux$Div4AirportID <- NULL
  dfFlightsAux$Div4AirportSeqID <- NULL
  dfFlightsAux$Div4WheelsOn <- NULL
  dfFlightsAux$Div4TotalGTime <- NULL
  dfFlightsAux$Div4WheelsOff <- NULL
  dfFlightsAux$Div4LongestGTime <- NULL
  dfFlightsAux$Div4TailNum <- NULL
  dfFlightsAux$Div5Airport <- NULL
  dfFlightsAux$Div5AirportID <- NULL
  dfFlightsAux$Div5AirportSeqID <- NULL
  dfFlightsAux$Div5WheelsOn <- NULL
  dfFlightsAux$Div5TotalGTime <- NULL
  dfFlightsAux$Div5LongestGTime <- NULL
  dfFlightsAux$Div5WheelsOff <- NULL
  dfFlightsAux$Div5TailNum <- NULL
  dfFlightsAux$V110 <- NULL
  
  return (dfFlightsAux)
}

flights <- clenEmptyData(flightsAux)
# *************************************************************************************************


# write.table(flights, file = "flights.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)

# Elimino los datos no necesarios
rm(dfDecember)
rm(dfJanuary)
rm(flightsAux)