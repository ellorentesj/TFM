#### TFM: ANÁLISIS Y PREDICCIÓN DEL RESTRASO EN LOS VUELOS ####

#### 3. DATOS DE ENTRADA: Análisis y Normalización de los datos de entrada ####


# *************************************************************************************************
##### 3.1. Bloque de carga de librerias #####

# *************************************************************************************************


# *************************************************************************************************
##### 3.2. Bloque de carga de datos #####
# Selección de ruta, en mi caso: "/Users/ellorentesj/repostGitHub/TFM/data/"
setwd("/Users/ellorentesj/repostGitHub/TFM/")
# flights <- fread("flights.csv", header=T, sep=',')
# *************************************************************************************************


# *************************************************************************************************
##### 3.3. Bloque de revisión basica del dataset #####
str(flights)
summary(flights)
# *************************************************************************************************


# *************************************************************************************************
#### 3.4. Eliminación variables  ####
# Procedo a eliminar variables que no tiene sentido mantener después del análisis

#### 3.4.1. Year ####
# La variable Year tiene el mismo valor en todos los campos, 2014, puesto que no se van a analizar 
# más años, se prescinde de ella
flights$Year <- NULL

#### 3.4.2. FlightDate ####
# Elimino la variable FlightDate ya que esta se encuentra dividida en las variables Year, Month y 
# DayofMonth
flights$FlightDate <- NULL

#### 3.4.3. OriginCityMarketID, DestCityMarketID ####
# Elimino estas variables puesto que no estamos analizando una zona de mercado concreta, y cada 
# zona puede corresponder a varios aeropuertos
flights$OriginCityMarketID <- NULL
flights$DestCityMarketID <- NULL

#### 3.4.4. CRSDepTime ####
# Elimino la variable CRSDepTime ya que tenemos la hora actual de despegue en la variable DepTime
flights$CRSDepTime <- NULL

#### 3.4.5. CRSArrTime ####
# Elimino la variable CRSArrTime ya que tenemos la hora actual de aterrizaje en la variable ArrTime
flights$CRSArrTime <- NULL

#### 3.4.6. CRSElapsedTime ####
# Elimino la variable CRSElapsedTime ya que tenemos la hora actual de tiempo total transcurido en
# ActualElapsedTime
flights$CRSElapsedTime <- NULL

#### 3.4.7. TaxiOut, AirTime, TaxiIn  ####
# La variable ActualElapsedTime contiene en minutos el tiempo de vuelo total, esto significa que es
# la suma del tiempo Taxi-Out de la salida del vuelo, más el tiempo que está en el aire, más el 
# tiempo de Taxi-In de entrada
TimeRoute = flights$TaxiOut+flights$AirTime+flights$TaxiIn
# Comparo ambas variables 
flights %>% filter(ActualElapsedTime!=TimeRoute) %>% nrow() # Los valores coinciden
# Por tanto tenemos el tiempo total de vuelo en una única variable ActualElapsedTime, podemos 
# prescindir de TaxiOut, AirTime, TaxiIn
flights$TaxiOut <- NULL
flights$AirTime <- NULL
flights$TaxiIn <- NULL
rm(TimeRoute)

#### 3.4.8. WheelsOff, WheelsOn  ####
# La variables DepTime y ArriTime contienen las horas de despegue y aterrizaje y la variable 
# ActualElapsedTime el tiempo total de vuelo transcurrido, las variables WheelsOff, WheelsOn que 
# corresponden a la hora en la que la aronave deja de tocar pista y toca pista respectivamente no
# son necesarias
flights$WheelsOff <- NULL
flights$WheelsOn <- NULL

#### 3.4.9. Flights  ####
# La variable Flights contiene un 1 en todos sus campos, se puede prescindir de ella
flights$Flights <- NULL
# *************************************************************************************************



# *************************************************************************************************
#### 3.5. Normalización  ####

# Procedo a analizar las variables que contienen NA's y decidir que hacer con ellas 
colSums(is.na(flights))>0

#### 3.5.1. CarrierDelay  ####
flights %>% filter(is.na(CarrierDelay)) %>% nrow()
# Asumo que si la variable no contiene información en alguno de sus campos es por que no tiene 
# retraso, por tanto cumplimento los NA's con 0's
flights <- flights %>% mutate(CarrierDelay = coalesce(as.integer(CarrierDelay),0L))

#### 3.5.2. WeatherDelay  ####
flights %>% filter(is.na(WeatherDelay)) %>% nrow()
# Asumo que si la variable no contiene información en alguno de sus campos es por que no tiene 
# retraso, por tanto cumplimento los NA's con 0's
flights <- flights %>% mutate(WeatherDelay = coalesce(as.integer(WeatherDelay),0L))

#### 3.5.3. NASDelay  ####
flights %>% filter(is.na(NASDelay)) %>% nrow()
# Asumo que si la variable no contiene información en alguno de sus campos es por que no tiene 
# retraso, por tanto cumplimento los NA's con 0's
flights <- flights %>% mutate(NASDelay = coalesce(as.integer(NASDelay),0L))

#### 3.5.4. SecurityDelay  ####
flights %>% filter(is.na(SecurityDelay)) %>% nrow()
# Asumo que si la variable no contiene información en alguno de sus campos es por que no tiene 
# retraso, por tanto cumplimento los NA's con 0's
flights <- flights %>% mutate(SecurityDelay = coalesce(as.integer(SecurityDelay),0L))

#### 3.5.5. LateAircraftDelay  ####
flights %>% filter(is.na(LateAircraftDelay)) %>% nrow()
# Asumo que si la variable no contiene información en alguno de sus campos es por que no tiene 
# retraso, por tanto cumplimento los NA's con 0's
flights <- flights %>% mutate(LateAircraftDelay = coalesce(as.integer(LateAircraftDelay),0L))
# *************************************************************************************************



# Guardo el dataset
write.table(flights, file = "finalFlights.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)



# *************************************************************************************************
#### 3.6. Asignación de pesos a las variables categóricas  ####

# Realizo una copia de flights por si fuese necesario recuperarla
# flights2 <- flights
flights <- flights2

# Se han reutilizado las funciones de https://github.com/semartin3/TFM/blob/master/4%20-DescripcionDatosEntrada_02.R
# Función que calcula el retraso medio de los valores indicados en la variable de entrada numFlights
# y devuelve la cantidad de vuelos realizados. El dataframe que devuelve contiene el valor de entrada
# el retraso medio y el número de apariciones
averageDelays <- function(numFlights, sampleFlights){
  
  codes <- vector()
  delays <- vector()
  numberFlights <- vector()
  
  ## bucle for por cada codigo de vuelo
  for(i in 1:length(numFlights)){
    
    flight <- sampleFlights[sampleFlights[,2]==numFlights[i],]
    retrasoMedio <- 0
    
    if(length(flight[,1])>0){
      retrasoMedio <- mean(flight[,1])
    }
    
    codes[i] = as.character(numFlights[i])
    delays[i] = as.numeric(retrasoMedio)    
    numberFlights[i] <- length(flight[,1])
    
  }
  df <- as.data.frame(list(codes,delays,numberFlights), col.names = c("Level","AvgDelay","CountFlights"))
  return(df)
}

# Funcion que asigna pesos a las variables en funcion de su retraso medio. Cuanto menor sea su 
# retraso medio menor será el peso asignado
assignWeights <- function(dfAvgDelay){
  
  dfOrderAvgDelay <- dfAvgDelay[order(dfAvgDelay[,2]),]
  v <- 1:length(dfOrderAvgDelay[,2])
  dfOrderAvgDelay[,4] <- v
  
  return(dfOrderAvgDelay)
}


# Funcion asigna el peso correspondiente que tiene cada valor en el dataframe
assignWeightsDF <- function(codes, dfGroupsCodes){
  ## codes -> vector del dataframe total con los levels 
  ## dfGroupsCodes -> df con los códigos y su correspondiente retraso
  ## La funcion devuelve un vector indicando el peso que tiene cada valor del vector codes
  
  weight <- vector()
  flightCodes <- as.character(codes)
  vectorCodes <- as.character(dfGroupsCodes[,1])
  vectorGroups <- dfGroupsCodes[,2]
  
  for (i in 1:length(flightCodes)){
    for(j in 1:length(vectorCodes)){
      if (flightCodes[i] == vectorCodes[j]){
        weight[i] = vectorGroups[j]
      }
    }
  }
  return(as.integer(weight))
}

# Genero los pesos por categoría en función de la media de retraso (ArrDelay)
#### 3.6.1. Month ####
# Selecciono la columna de categoría y el retraso del que me voy a servir para la asignación de 
# pesos
MonthWeights <- subset(flights, select = c("ArrDelay","Month"))
levels <- unique(MonthWeights$Month) # Guardo los distintos levels de la categoría
MeanArrDelay <- averageDelays(levels, MonthWeights) # Almaceno la media de cada level
# Se añade una columna de pesos para cada categoría
MeanArrDelay$weight <- 0 # Inicializo una nueva variable peso
MeanArrDelay <- assignWeights(MeanArrDelay) # Le asigno los pesos
# Añado el nuevo vector al dataframe
GroupCode <- MeanArrDelay
GroupCode$AvgDelay <- NULL
GroupCode$CountFlights <- NULL
# Añado la nueva columna de pesos al dataframe flights
flights$MonthWeights <- assignWeightsDF(flights$Month,GroupCode)
# Elimino las variables para volver a reutilizarlas
rm(levels)
rm(MeanArrDelay)
rm(GroupCode)

#### 3.6.2. DayofMonth ####
DayofMonthWeights <- subset(flights, select = c("ArrDelay","DayofMonth"))
levels <- unique(DayofMonthWeights$DayofMonth) 
MeanArrDelay <- averageDelays(levels, DayofMonthWeights) 
MeanArrDelay$weight <- 0 
MeanArrDelay <- assignWeights(MeanArrDelay) 
GroupCode <- MeanArrDelay
GroupCode$AvgDelay <- NULL
GroupCode$CountFlights <- NULL
flights$DayofMonthWeights <- assignWeightsDF(flights$DayofMonth,GroupCode)
rm(levels)
rm(MeanArrDelay)
rm(GroupCode)

#### 3.6.3. DayOfWeek ####
DayOfWeekWeights <- subset(flights, select = c("ArrDelay","DayOfWeek"))
levels <- unique(DayOfWeekWeights$DayOfWeek) 
MeanArrDelay <- averageDelays(levels, DayOfWeekWeights) 
MeanArrDelay$weight <- 0 
MeanArrDelay <- assignWeights(MeanArrDelay) 
GroupCode <- MeanArrDelay
GroupCode$AvgDelay <- NULL
GroupCode$CountFlights <- NULL
flights$DayOfWeekWeights <- assignWeightsDF(flights$DayOfWeek,GroupCode)
rm(levels)
rm(MeanArrDelay)
rm(GroupCode)

#### 3.6.4. UniqueCarrier ####
UniqueCarrierWeights <- subset(flights, select = c("ArrDelay","UniqueCarrier"))
levels <- unique(UniqueCarrierWeights$UniqueCarrier) 
MeanArrDelay <- averageDelays(levels, UniqueCarrierWeights) 
MeanArrDelay$weight <- 0 
MeanArrDelay <- assignWeights(MeanArrDelay) 
GroupCode <- MeanArrDelay
GroupCode$AvgDelay <- NULL
GroupCode$CountFlights <- NULL
flights$UniqueCarrierWeights <- assignWeightsDF(flights$UniqueCarrier,GroupCode)
rm(levels)
rm(MeanArrDelay)
rm(GroupCode)

#### 3.6.5. TailNum #### 
TailNumWeights <- subset(flights, select = c("ArrDelay","TailNum"))
levels <- unique(TailNumWeights$TailNum) 
MeanArrDelay <- averageDelays(levels, TailNumWeights) 
MeanArrDelay$weight <- 0 
MeanArrDelay <- assignWeights(MeanArrDelay) 
GroupCode <- MeanArrDelay
GroupCode$AvgDelay <- NULL
GroupCode$CountFlights <- NULL
flights$TailNumWeights <- assignWeightsDF(flights$TailNum,GroupCode)
rm(levels)
rm(MeanArrDelay)
rm(GroupCode)

#### 3.6.6. FlightNum ####
FlightNumWeights <- subset(flights, select = c("ArrDelay","FlightNum"))
levels <- unique(FlightNumWeights$FlightNum) 
MeanArrDelay <- averageDelays(levels, FlightNumWeights) 
MeanArrDelay$weight <- 0 
MeanArrDelay <- assignWeights(MeanArrDelay) 
GroupCode <- MeanArrDelay
GroupCode$AvgDelay <- NULL
GroupCode$CountFlights <- NULL
flights$FlightNumWeights <- assignWeightsDF(flights$FlightNum,GroupCode)
rm(levels)
rm(MeanArrDelay)
rm(GroupCode)

#### 3.6.7. OriginAirportSeqID ####
OriginAirportSeqIDWeights <- subset(flights, select = c("ArrDelay","OriginAirportSeqID"))
levels <- unique(OriginAirportSeqIDWeights$OriginAirportSeqID) 
MeanArrDelay <- averageDelays(levels, OriginAirportSeqIDWeights) 
MeanArrDelay$weight <- 0 
MeanArrDelay <- assignWeights(MeanArrDelay) 
GroupCode <- MeanArrDelay
GroupCode$AvgDelay <- NULL
GroupCode$CountFlights <- NULL
flights$OriginAirportSeqIDWeights <- assignWeightsDF(flights$OriginAirportSeqID,GroupCode)
rm(levels)
rm(MeanArrDelay)
rm(GroupCode)

#### 3.6.8. Origin ####
OriginWeights <- subset(flights, select = c("ArrDelay","Origin"))
levels <- unique(OriginWeights$Origin) 
MeanArrDelay <- averageDelays(levels, OriginWeights) 
MeanArrDelay$weight <- 0 
MeanArrDelay <- assignWeights(MeanArrDelay) 
GroupCode <- MeanArrDelay
GroupCode$AvgDelay <- NULL
GroupCode$CountFlights <- NULL
flights$OriginWeights <- assignWeightsDF(flights$Origin,GroupCode)
rm(levels)
rm(MeanArrDelay)
rm(GroupCode)

#### 3.6.9. OriginCityName ####
# Como esta variable contiene la información extendida de Origin, prescindo de ella
flights$OriginCityName <- NULL

#### 3.6.10. OriginState ####
OriginStateWeights <- subset(flights, select = c("ArrDelay","OriginState"))
levels <- unique(OriginStateWeights$OriginState) 
MeanArrDelay <- averageDelays(levels, OriginStateWeights) 
MeanArrDelay$weight <- 0 
MeanArrDelay <- assignWeights(MeanArrDelay) 
GroupCode <- MeanArrDelay
GroupCode$AvgDelay <- NULL
GroupCode$CountFlights <- NULL
flights$OriginStateWeights <- assignWeightsDF(flights$OriginState,GroupCode)
rm(levels)
rm(MeanArrDelay)
rm(GroupCode)

#### 3.6.11. OriginStateName ####
# Como esta variable contiene la información extendida de OriginState, prescindo de ella
flights$OriginStateName <- NULL

#### 3.6.12. DestAirportSeqID ####
DestAirportSeqIDWeights <- subset(flights, select = c("ArrDelay","DestAirportSeqID"))
levels <- unique(DestAirportSeqIDWeights$DestAirportSeqID) 
MeanArrDelay <- averageDelays(levels, DestAirportSeqIDWeights) 
MeanArrDelay$weight <- 0 
MeanArrDelay <- assignWeights(MeanArrDelay) 
GroupCode <- MeanArrDelay
GroupCode$AvgDelay <- NULL
GroupCode$CountFlights <- NULL
flights$DestAirportSeqIDWeights <- assignWeightsDF(flights$DestAirportSeqID,GroupCode)
rm(levels)
rm(MeanArrDelay)
rm(GroupCode)

#### 3.6.13. Dest ####
DestWeights <- subset(flights, select = c("ArrDelay","Dest"))
levels <- unique(DestWeights$Dest) 
MeanArrDelay <- averageDelays(levels, DestWeights) 
MeanArrDelay$weight <- 0 
MeanArrDelay <- assignWeights(MeanArrDelay) 
GroupCode <- MeanArrDelay
GroupCode$AvgDelay <- NULL
GroupCode$CountFlights <- NULL
flights$DestWeights <- assignWeightsDF(flights$Dest,GroupCode)
rm(levels)
rm(MeanArrDelay)
rm(GroupCode)

#### 3.6.14. DestCityName ####
# Como esta variable contiene la información extendida de Dest, prescindo de ella
flights$DestCityName <- NULL

#### 2.6.13. DestState ####
DestStateWeights <- subset(flights, select = c("ArrDelay","DestState"))
levels <- unique(DestStateWeights$DestState) 
MeanArrDelay <- averageDelays(levels, DestStateWeights) 
MeanArrDelay$weight <- 0 
MeanArrDelay <- assignWeights(MeanArrDelay) 
GroupCode <- MeanArrDelay
GroupCode$AvgDelay <- NULL
GroupCode$CountFlights <- NULL
flights$DestStateWeights <- assignWeightsDF(flights$DestState,GroupCode)
rm(levels)
rm(MeanArrDelay)
rm(GroupCode)

#### 3.6.15. DestStateName ####
# Como esta variable contiene la información extendida de DestState, prescindo de ella
flights$DestStateName <- NULL

#### 3.6.16. DepartureDelayGroups ####
DepartureDelayGroupsWeights <- subset(flights, select = c("ArrDelay","DepartureDelayGroups"))
levels <- unique(DepartureDelayGroupsWeights$DepartureDelayGroups) 
MeanArrDelay <- averageDelays(levels, DepartureDelayGroupsWeights) 
MeanArrDelay$weight <- 0 
MeanArrDelay <- assignWeights(MeanArrDelay) 
GroupCode <- MeanArrDelay
GroupCode$AvgDelay <- NULL
GroupCode$CountFlights <- NULL
flights$DepartureDelayGroupsWeights <- assignWeightsDF(flights$DepartureDelayGroups,GroupCode)
rm(levels)
rm(MeanArrDelay)
rm(GroupCode)

#### 3.6.17. DepTimeBlk ####
DepTimeBlkWeights <- subset(flights, select = c("ArrDelay","DepTimeBlk"))
levels <- unique(DepTimeBlkWeights$DepTimeBlk) 
MeanArrDelay <- averageDelays(levels, DepTimeBlkWeights) 
MeanArrDelay$weight <- 0 
MeanArrDelay <- assignWeights(MeanArrDelay) 
GroupCode <- MeanArrDelay
GroupCode$AvgDelay <- NULL
GroupCode$CountFlights <- NULL
flights$DepTimeBlkWeights <- assignWeightsDF(flights$DepTimeBlk,GroupCode)
rm(levels)
rm(MeanArrDelay)
rm(GroupCode)

#### 3.6.18. ArrivalDelayGroups ####
ArrivalDelayGroupsWeights <- subset(flights, select = c("ArrDelay","ArrivalDelayGroups"))
levels <- unique(ArrivalDelayGroupsWeights$ArrivalDelayGroups) 
MeanArrDelay <- averageDelays(levels, ArrivalDelayGroupsWeights) 
MeanArrDelay$weight <- 0 
MeanArrDelay <- assignWeights(MeanArrDelay) 
GroupCode <- MeanArrDelay
GroupCode$AvgDelay <- NULL
GroupCode$CountFlights <- NULL
flights$ArrivalDelayGroupsWeights <- assignWeightsDF(flights$ArrivalDelayGroups,GroupCode)
rm(levels)
rm(MeanArrDelay)
rm(GroupCode)

#### 3.6.19. ArrTimeBlk ####
ArrTimeBlkWeights <- subset(flights, select = c("ArrDelay","ArrTimeBlk"))
levels <- unique(ArrTimeBlkWeights$ArrTimeBlk) 
MeanArrDelay <- averageDelays(levels, ArrTimeBlkWeights) 
MeanArrDelay$weight <- 0 
MeanArrDelay <- assignWeights(MeanArrDelay) 
GroupCode <- MeanArrDelay
GroupCode$AvgDelay <- NULL
GroupCode$CountFlights <- NULL
flights$ArrTimeBlkWeights <- assignWeightsDF(flights$ArrTimeBlk,GroupCode)
rm(levels)
rm(MeanArrDelay)
rm(GroupCode)

#### 3.6.20. DistanceGroup ####
DistanceGroupWeights <- subset(flights, select = c("ArrDelay","DistanceGroup"))
levels <- unique(DistanceGroupWeights$DistanceGroup) 
MeanArrDelay <- averageDelays(levels, DistanceGroupWeights) 
MeanArrDelay$weight <- 0 
MeanArrDelay <- assignWeights(MeanArrDelay) 
GroupCode <- MeanArrDelay
GroupCode$AvgDelay <- NULL
GroupCode$CountFlights <- NULL
flights$DistanceGroupWeights <- assignWeightsDF(flights$DistanceGroup,GroupCode)
rm(levels)
rm(MeanArrDelay)
rm(GroupCode)
# *************************************************************************************************



# *************************************************************************************************
#### 3.7. Guardo la relación de las variables categóricas y sus pesos ####
# Creo nuevo directorio para guardar los ficheros
dir.create("CategoriesWeights")

write.table(MonthWeights, file = "CategoriesWeights/MonthWeights.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(DayofMonthWeights, file = "CategoriesWeights/DayofMonthWeights.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(DayOfWeekWeights, file = "CategoriesWeights/DayOfWeekWeights.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(UniqueCarrierWeights, file = "CategoriesWeights/UniqueCarrierWeights.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(TailNumWeights, file = "CategoriesWeights/TailNumWeights.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(FlightNumWeights, file = "CategoriesWeights/FlightNumWeights.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(OriginAirportSeqIDWeights, file = "CategoriesWeights/OriginAirportSeqIDWeights.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(OriginWeights, file = "CategoriesWeights/OriginWeights.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(OriginStateWeights, file = "CategoriesWeights/OriginStateWeights.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(DestAirportSeqIDWeights, file = "CategoriesWeights/DestAirportSeqIDWeights.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(DestWeights, file = "CategoriesWeights/DestWeights.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(DestStateWeights, file = "CategoriesWeights/DestStateWeights.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(DepartureDelayGroupsWeights, file = "CategoriesWeights/DepartureDelayGroupsWeights.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(DepTimeBlkWeights, file = "CategoriesWeights/DepTimeBlkWeights.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(ArrivalDelayGroupsWeights, file = "CategoriesWeights/ArrivalDelayGroupsWeights.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(ArrTimeBlkWeights, file = "CategoriesWeights/ArrTimeBlkWeights.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(DistanceGroupWeights, file = "CategoriesWeights/DistanceGroupWeights.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
# *************************************************************************************************



# *************************************************************************************************
#### 3.8. Elimino variables categóricas y asignaciones innecesarias ####
rm(DayofMonthWeights)
rm(DayOfWeekWeights)
rm(FlightNumWeights)
rm(ArrivalDelayGroupsWeights)
rm(ArrTimeBlkWeights)
rm(DepartureDelayGroupsWeights)
rm(DepTimeBlkWeights)
rm(DestAirportSeqIDWeights)
rm(DestStateWeights)
rm(DestWeights)
rm(DistanceGroupWeights)
rm(flights2)
rm(MeanArrDel15)
rm(MonthWeights)
rm(OriginAirportSeqIDWeights)
rm(OriginStateWeights)
rm(OriginWeights)
rm(TailNumWeights)
rm(UniqueCarrierWeights)
flights$Month <- NULL
flights$DayofMonth <- NULL
flights$DayOfWeek <- NULL
flights$UniqueCarrier <- NULL
flights$TailNum <- NULL
flights$FlightNum <- NULL
flights$OriginAirportSeqID <- NULL
flights$Origin <- NULL
flights$OriginState <- NULL
flights$DestAirportSeqID <- NULL
flights$Dest <- NULL
flights$DestState <- NULL
flights$DepartureDelayGroups <- NULL
flights$DepTimeBlk <- NULL
flights$ArrivalDelayGroups <- NULL
flights$ArrTimeBlk <- NULL
flights$DistanceGroup <- NULL
# *************************************************************************************************



# Guardo el dataset totalmente normalizado
write.table(flights, file = "standardFlights.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)


