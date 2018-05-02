#### TFM: ANÁLISIS Y PREDICCIÓN DEL RESTRASO EN LOS VUELOS ####

#### 0. PREPROCESAMIENTO DE LOS FICHEROS: Eliminación variables innecesarias ####

# Selección de ruta, en mi caso: "/Users/ellorentesj/Desktop/TFM/MI_TFM"
ruta <- getwd()  
ruta
if (ruta != "/Users/ellorentesj/Desktop/TFM/MI_TFM") ruta <- "/Users/ellorentesj/Desktop/TFM/MI_TFM"

limpiarFichero <- function (mesFichero){
  
  # Lectura del fichero
  vueDF <- read.table(mesFichero, header = T, sep = ',')
  
  # Limpieza de los datos no significativos. Se eliminan los datos que no aportan ningún tipo de de información para el análisis de predicción. 
  vueDF$TailNum <- NULL
  vueDF$CRSDepTime <- NULL
  vueDF$DepartureDelayGroups <- NULL
  vueDF$DepTimeBlk <- NULL
  vueDF$TaxiOut <- NULL
  vueDF$WheelsOff <- NULL
  vueDF$WheelsOn <- NULL
  vueDF$TaxiIn <- NULL
  vueDF$CRSArrTime <- NULL
  vueDF$ArrivalDelayGroups <- NULL
  vueDF$ArrTimeBlk <- NULL
  vueDF$Diverted <- NULL
  vueDF$CRSElapsedTime <- NULL
  vueDF$ActualElapsedTime <- NULL
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
  vueDF$X <- NULL
  
  return (vueDF)
}

ene = "data/2013/On_Time_On_Time_Performance_2013_1.csv"
feb = "data/2013/On_Time_On_Time_Performance_2013_2.csv"
mar = "data/2013/On_Time_On_Time_Performance_2013_3.csv"
abr = "data/2013/On_Time_On_Time_Performance_2013_4.csv"
may = "data/2013/On_Time_On_Time_Performance_2013_5.csv"
jun = "data/2013/On_Time_On_Time_Performance_2013_6.csv"
jul = "data/2013/On_Time_On_Time_Performance_2013_7.csv"
ago = "data/2013/On_Time_On_Time_Performance_2013_8.csv"
sep = "data/2013/On_Time_On_Time_Performance_2013_9.csv"
oct = "data/2013/On_Time_On_Time_Performance_2013_10.csv"
nov = "data/2013/On_Time_On_Time_Performance_2013_11.csv"
dic = "data/2013/On_Time_On_Time_Performance_2013_12.csv"

# Limpieza del mes de enero para escribirlo en el csv
enedf <- limpiarFichero(ene)
write.table(enedf, file = "data/2013/2013.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)

lismes <- c(feb,mar,abr,may,jun,jul,ago,sep,oct,nov,dic)

for (i in lismes){
  
  mesdf <- limpiarFichero(i)
  write.table(mesdf, file = "data/2013/2013.csv", append = TRUE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = FALSE)
}