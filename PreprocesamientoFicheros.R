#### TFM: ANÁLISIS Y PREDICCIÓN DEL RESTRASO EN LOS VUELOS ####

#### 1. PREPROCESAMIENTO DE LOS FICHEROS: Lectura de ficheros y primera limpieza de variables innecesarias ####

list.of.packages <- c("data.table")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(data.table)

# Selección de ruta donde están los ficheros, en mi caso: "/Users/ellorentesj/Desktop/TFM/data"
setwd("/Users/ellorentesj/Desktop/TFM/data/")

limpiarFichero <- function (mesFichero){
  
  # Lectura del fichero
  vueDF <- fread(mesFichero, header=T, sep=',')
  
  # Limpieza de los datos no significativos. Se eliminan los datos que no aportan ningún tipo de de información para el análisis de predicción. 
  vueDF$Quarter <- NULL 
  vueDF$TailNum <- NULL
  vueDF$OriginStateFips <- NULL
  vueDF$OriginStateName <- NULL
  vueDF$OriginWac <- NULL
  vueDF$DestStateFips <- NULL
  vueDF$DestStateName <- NULL
  vueDF$DestWac <- NULL
  vueDF$CRSDepTime <- NULL
  vueDF$DepartureDelayGroups <- NULL
  vueDF$DepTimeBlk <- NULL
  vueDF$WheelsOff <- NULL
  vueDF$WheelsOn <- NULL
  vueDF$CRSArrTime <- NULL
  vueDF$ArrivalDelayGroups <- NULL
  vueDF$ArrTimeBlk <- NULL
  vueDF$Diverted <- NULL
  vueDF$CRSElapsedTime <- NULL
  vueDF$ActualElapsedTime <- NULL
  vueDF$AirTime <- NULL
  vueDF$Flights <- NULL
  vueDF$DistanceGroup <- NULL
  vueDF$FirstDepTime <- NULL
  vueDF$TotalAddGTime <- NULL
  vueDF$LongestAddGTime <- NULL
  vueDF$DivAirportLandings <- NULL
  vueDF$DivReachedDest <- NULL
  vueDF$DivActualElapsedTime <- NULL
  vueDF$DivArrDelay <- NULL
  vueDF$DivDistance <- NULL
  vueDF$Div1Airport <- NULL
  vueDF$Div1AirportID <- NULL
  vueDF$Div1AirportSeqID <- NULL
  vueDF$Div1WheelsOn <- NULL
  vueDF$Div1TotalGTime <- NULL
  vueDF$Div1LongestGTime <- NULL
  vueDF$Div1WheelsOff <- NULL
  vueDF$Div1TailNum <- NULL
  vueDF$Div2Airport <- NULL
  vueDF$Div2AirportID <- NULL
  vueDF$Div2AirportSeqID <- NULL
  vueDF$Div2WheelsOn <- NULL
  vueDF$Div2TotalGTime <- NULL
  vueDF$Div2LongestGTime <- NULL
  vueDF$Div2WheelsOff <- NULL
  vueDF$Div2TailNum <- NULL
  vueDF$Div3Airport <- NULL
  vueDF$Div3AirportID <- NULL
  vueDF$Div3AirportSeqID <- NULL
  vueDF$Div3WheelsOn <- NULL
  vueDF$Div3TotalGTime <- NULL
  vueDF$Div3LongestGTime <- NULL
  vueDF$Div3WheelsOff <- NULL
  vueDF$Div3TailNum <- NULL
  vueDF$Div4Airport <- NULL
  vueDF$Div4AirportID <- NULL
  vueDF$Div4AirportSeqID <- NULL
  vueDF$Div4WheelsOn <- NULL
  vueDF$Div4TotalGTime <- NULL
  vueDF$Div4WheelsOff <- NULL
  vueDF$Div4LongestGTime <- NULL
  vueDF$Div4TailNum <- NULL
  vueDF$Div5Airport <- NULL
  vueDF$Div5AirportID <- NULL
  vueDF$Div5AirportSeqID <- NULL
  vueDF$Div5WheelsOn <- NULL
  vueDF$Div5TotalGTime <- NULL
  vueDF$Div5LongestGTime <- NULL
  vueDF$Div5WheelsOff <- NULL
  vueDF$Div5TailNum <- NULL
  vueDF$V110 <- NULL
  
  return (vueDF)
}

dic = "On_Time_On_Time_Performance_2013_12.csv"
ene = "On_Time_On_Time_Performance_2014_1.csv"

# ficCSV <- list.files('data/2013/', pattern = '*.csv', full.names = T)
# vuelos <- lapply(ficCSV, fread)

# Limpieza del mes de Diciembre para escribirlo en el csv
dicdf <- limpiarFichero(dic)
write.table(dicdf, file = "vuelos.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
enedf <- limpiarFichero(ene)
write.table(enedf, file = "vuelos.csv", append = TRUE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = FALSE)

# lismes <- c(ene,feb)
# 
# for (i in lismes){
#   
#   mesdf <- limpiarFichero(i)
#   write.table(mesdf, file = "vuelos.csv", append = TRUE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = FALSE)
#   
# }
