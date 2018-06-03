#### TFM: ANÁLISIS Y PREDICCIÓN DEL RESTRASO EN LOS VUELOS ####



#### 2. DATOS DE ENTRADA: Análisis de los datos de entrada y asignación de pesos ####



# *************************************************************************************************
##### 2.1. Bloque de carga de librerias #####

# *************************************************************************************************



# *************************************************************************************************
##### 2.2. Bloque de carga de datos #####
# Selección de ruta, en mi caso: "/Users/ellorentesj/repostGitHub/TFM/data/"
setwd("/Users/ellorentesj/repostGitHub/TFM/")
# flights <- fread("flights.csv", header=T, sep=',')
# *************************************************************************************************



# *************************************************************************************************
##### 2.3. Bloque de revisión basica del dataset #####
str(flights)
summary(flights)
# *************************************************************************************************



# *************************************************************************************************
#### 2.4. Eliminación variables  ####
# Procedo a eliminar variables que no tiene sentido mantener después del análisis


#### 2.4.1. Year ####
# La variable Year tiene el mismo valor en todos los campos, 2014, puesto que no se van a analizar 
# más años, se prescinde de ella
flights$Year <- NULL


#### 2.4.2. FlightDate ####
# Elimino la variable FlightDate ya que esta se encuentra dividida en las variables Year, Month y 
# DayofMonth
flights$FlightDate <- NULL


#### 2.4.3. OriginCityMarketID, DestCityMarketID ####
# Elimino estas variables puesto que no estamos analizando una zona de mercado concreta, y cada 
# zona puede corresponder a varios aeropuertos
flights$OriginCityMarketID <- NULL
flights$DestCityMarketID <- NULL


#### 2.4.4. CRSDepTime ####
# Elimino la variable CRSDepTime ya que tenemos la hora actual de despegue en la variable DepTime
flights$CRSDepTime <- NULL


#### 2.4.5. CRSArrTime ####
# Elimino la variable CRSArrTime ya que tenemos la hora actual de aterrizaje en la variable ArrTime
flights$CRSArrTime <- NULL


#### 2.4.6. CRSElapsedTime ####
# Elimino la variable CRSElapsedTime ya que tenemos la hora actual de tiempo total transcurido en
# ActualElapsedTime
flights$CRSElapsedTime <- NULL


#### 2.4.7. TaxiOut, AirTime, TaxiIn  ####
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


#### 2.4.8. WheelsOff, WheelsOn  ####
# La variables DepTime y ArriTime contienen las horas de despegue y aterrizaje y la variable 
# ActualElapsedTime el tiempo total de vuelo transcurrido, las variables WheelsOff, WheelsOn que 
# corresponden a la hora en la que la aronave deja de tocar pista y toca pista respectivamente no
# son necesarias
flights$WheelsOff <- NULL
flights$WheelsOn <- NULL


#### 2.4.9. Flights  ####
# La variable Flights contiene un 1 en todos sus campos, se puede prescindir de ella
flights$Flights <- NULL
# *************************************************************************************************



# *************************************************************************************************
#### 2.5. Limpieza de NA's  ####

# Procedo a analizar las variables que contienen NA's y decidir que hacer con ellas 
colSums(is.na(flights))>0


#### 2.5.1. CarrierDelay  ####
flights %>% filter(is.na(CarrierDelay)) %>% nrow()
# Asumo que si la variable no contiene información en alguno de sus campos es por que no tiene 
# retraso, por tanto cumplimento los NA's con 0's
flights <- flights %>% mutate(CarrierDelay = coalesce(as.integer(CarrierDelay),0L))


#### 2.5.2. WeatherDelay  ####
flights %>% filter(is.na(WeatherDelay)) %>% nrow()
# Asumo que si la variable no contiene información en alguno de sus campos es por que no tiene 
# retraso, por tanto cumplimento los NA's con 0's
flights <- flights %>% mutate(WeatherDelay = coalesce(as.integer(WeatherDelay),0L))


#### 2.5.3. NASDelay  ####
flights %>% filter(is.na(NASDelay)) %>% nrow()
# Asumo que si la variable no contiene información en alguno de sus campos es por que no tiene 
# retraso, por tanto cumplimento los NA's con 0's
flights <- flights %>% mutate(NASDelay = coalesce(as.integer(NASDelay),0L))


#### 2.5.4. SecurityDelay  ####
flights %>% filter(is.na(SecurityDelay)) %>% nrow()
# Asumo que si la variable no contiene información en alguno de sus campos es por que no tiene 
# retraso, por tanto cumplimento los NA's con 0's
flights <- flights %>% mutate(SecurityDelay = coalesce(as.integer(SecurityDelay),0L))


#### 2.5.5. LateAircraftDelay  ####
flights %>% filter(is.na(LateAircraftDelay)) %>% nrow()
# Asumo que si la variable no contiene información en alguno de sus campos es por que no tiene 
# retraso, por tanto cumplimento los NA's con 0's
flights <- flights %>% mutate(LateAircraftDelay = coalesce(as.integer(LateAircraftDelay),0L))
# *************************************************************************************************



# Guardo el dataset
finalFlights <- flights
write.table(finalFlights, file = "data/finalFlights.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)



# *************************************************************************************************
#### 2.6. Asignación de pesos a las variables categóricas  ####


#### 2.6.1. Month ####
# Selecciono la columna de categoría Month y el retraso del que me voy a servir para la asignación de 
# pesos ArrDel15
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDel15 = 1)
MonthWeights <- flights %>% group_by(Month) %>% summarise(MonthW = mean(ArrDel15==1))
# Realizo un join en la columna Month del dataset flights con los pesos calculados en MonthWeigths
flights$Month <- inner_join(flights, MonthWeights, by = "Month")$MonthW


#### 2.6.2. DayofMonth ####
# Selecciono la columna de categoría DayofMonth y el retraso del que me voy a servir para la
# asignación de pesos ArrDel15
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDel15 = 1)
DayofMonthWeights <- flights %>% group_by(DayofMonth) %>% summarise(DayofMonthW = mean(ArrDel15==1))
# Realizo un join en la columna DayofMonth del dataset flights con los pesos calculados en DayofMonthWeights
flights$DayofMonth <- inner_join(flights, DayofMonthWeights, by = "DayofMonth")$DayofMonthW


#### 2.6.3. DayOfWeek ####
# Selecciono la columna de categoría DayOfWeek y el retraso del que me voy a servir para la
# asignación de pesos ArrDel15
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDel15 = 1)
DayOfWeekWeights <- flights %>% group_by(DayOfWeek) %>% summarise(DayOfWeekW = mean(ArrDel15==1))
# Realizo un join en la columna DayOfWeek del dataset flights con los pesos calculados en 
# DayOfWeekWeights
flights$DayOfWeek <- inner_join(flights, DayOfWeekWeights, by = "DayOfWeek")$DayOfWeekW


#### 2.6.4. UniqueCarrier ####
# Selecciono la columna de categoría UniqueCarrier y el retraso del que me voy a servir para la
# asignación de pesos ArrDel15
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDel15 = 1)
UniqueCarrierWeights <- flights %>% group_by(UniqueCarrier) %>% summarise(UniqueCarrierW = mean(ArrDel15==1))
# Realizo un join en la columna UniqueCarrier del dataset flights con los pesos calculados en 
# UniqueCarrierWeights
flights$UniqueCarrier <- inner_join(flights, UniqueCarrierWeights, by = "UniqueCarrier")$UniqueCarrierW


#### 2.6.5. TailNum #### 
# Selecciono la columna de categoría TailNum y el retraso del que me voy a servir para la asignación
# de pesos ArrDel15
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDel15 = 1)
TailNumWeights <- flights %>% group_by(TailNum) %>% summarise(TailNumW = mean(ArrDel15==1))
# Realizo un join en la columna TailNum del dataset flights con los pesos calculados en TailNumWeights
flights$TailNum <- inner_join(flights, TailNumWeights, by = "TailNum")$TailNumW


#### 2.6.6. FlightNum ####
# Selecciono la columna de categoría FlightNum y el retraso del que me voy a servir para la asignación
# de pesos ArrDel15
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDel15 = 1)
FlightNumWeights <- flights %>% group_by(FlightNum) %>% summarise(FlightNumW = mean(ArrDel15==1))
# Realizo un join en la columna FlightNum del dataset flights con los pesos calculados en FlightNumWeights
flights$FlightNum <- inner_join(flights, FlightNumWeights, by = "FlightNum")$FlightNumW


#### 2.6.7. OriginAirportSeqID ####
# Selecciono la columna de categoría OriginAirportSeqID y el retraso del que me voy a servir para 
# la asignación de pesos ArrDel15
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDel15 = 1)
OriginAirportSeqIDWeights <- flights %>% group_by(OriginAirportSeqID) %>% summarise(OriginAirportSeqIDW = mean(ArrDel15==1))
# Realizo un join en la columna OriginAirportSeqID del dataset flights con los pesos calculados en 
# OriginAirportSeqIDWeights
flights$OriginAirportSeqID <- inner_join(flights, OriginAirportSeqIDWeights, by = "OriginAirportSeqID")$OriginAirportSeqIDW


#### 2.6.8. Origin ####
# Selecciono la columna de categoría OriginAirportSeqID y el retraso del que me voy a servir para 
# la asignación de pesos ArrDel15
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDel15 = 1)
OriginWeights <- flights %>% group_by(Origin) %>% summarise(OriginW = mean(ArrDel15==1))
# Realizo un join en la columna Origin del dataset flights con los pesos calculados en OriginWeights
flights$Origin <- inner_join(flights, OriginWeights, by = "Origin")$OriginW


#### 2.6.9. OriginCityName ####
# Como esta variable contiene la información extendida de Origin y ya no es necesaria, prescindo de
# ella pu
flights$OriginCityName <- NULL


#### 2.6.10. OriginState ####
# Selecciono la columna de categoría OriginState y el retraso del que me voy a servir para la asignación
# de pesos ArrDel15
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDel15 = 1)
OriginStateWeights <- flights %>% group_by(OriginState) %>% summarise(OriginStateW = mean(ArrDel15==1))
# Realizo un join en la columna OriginState del dataset flights con los pesos calculados en OriginStateWeights
flights$OriginState <- inner_join(flights, OriginStateWeights, by = "OriginState")$OriginStateW


#### 2.6.11. OriginStateName ####
# Como esta variable contiene la información extendida de OriginState y ya no es necesaria, prescindo
# de ella
flights$OriginStateName <- NULL


#### 2.6.12. DestAirportSeqID ####
# Selecciono la columna de categoría DestAirportSeqID y el retraso del que me voy a servir para la asignación
# de pesos ArrDel15
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDel15 = 1)
DestAirportSeqIDWeights <- flights %>% group_by(DestAirportSeqID) %>% summarise(DestAirportSeqIDW = mean(ArrDel15==1))
# Realizo un join en la columna DestAirportSeqID del dataset flights con los pesos calculados en DestAirportSeqIDWeights
flights$DestAirportSeqID <- inner_join(flights, DestAirportSeqIDWeights, by = "DestAirportSeqID")$DestAirportSeqIDW


#### 2.6.13. Dest ####
# Selecciono la columna de categoría Dest y el retraso del que me voy a servir para la asignación de
# pesos ArrDel15
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDel15 = 1)
DestWeights <- flights %>% group_by(Dest) %>% summarise(DestW = mean(ArrDel15==1))
# Realizo un join en la columna Dest del dataset flights con los pesos calculados en DestWeights
flights$Dest <- inner_join(flights, DestWeights, by = "Dest")$DestW


#### 2.6.14. DestCityName ####
# Como esta variable contiene la información extendida de Dest y ya no es necesaria, prescindo de ella
flights$DestCityName <- NULL


#### 2.6.15. DestState ####
# Selecciono la columna de categoría DestState y el retraso del que me voy a servir para la asignación
# de pesos ArrDel15
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDel15 = 1)
DestStateWeights <- flights %>% group_by(DestState) %>% summarise(DestStateW = mean(ArrDel15==1))
# Realizo un join en la columna DestState del dataset flights con los pesos calculados en DestStateWeights
flights$DestState <- inner_join(flights, DestStateWeights, by = "DestState")$DestStateW


#### 2.6.16. DestStateName ####
# Como esta variable contiene la información extendida de DestState y ya no es necearia, prescindo 
# de ella
flights$DestStateName <- NULL


#### 2.6.17. DepartureDelayGroups ####
# Selecciono la columna de categoría DepartureDelayGroups y el retraso del que me voy a servir para
# la asignación de pesos ArrDel15
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDel15 = 1)
DepartureDelayGroupsWeights <- flights %>% group_by(DepartureDelayGroups) %>% summarise(DepartureDelayGroupsW = mean(ArrDel15==1))
# Realizo un join en la columna DepartureDelayGroups del dataset flights con los pesos calculados en DepartureDelayGroupsWeights
flights$DepartureDelayGroups <- inner_join(flights, DepartureDelayGroupsWeights, by = "DepartureDelayGroups")$DepartureDelayGroupsW


#### 2.6.18. DepTimeBlk ####
# Selecciono la columna de categoría DepTimeBlk y el retraso del que me voy a servir para la asignación
# de pesos ArrDel15
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDel15 = 1)
DepTimeBlkWeights <- flights %>% group_by(DepTimeBlk) %>% summarise(DepTimeBlkW = mean(ArrDel15==1))
# Realizo un join en la columna DepTimeBlk del dataset flights con los pesos calculados en DepTimeBlkWeights
flights$DepTimeBlk <- inner_join(flights, DepTimeBlkWeights, by = "DepTimeBlk")$DepTimeBlkW


#### 2.6.19. ArrivalDelayGroups ####
# Selecciono la columna de categoría ArrivalDelayGroups y el retraso del que me voy a servir para la 
# asignación de pesos ArrDel15
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDel15 = 1)
ArrivalDelayGroupsWeights <- flights %>% group_by(ArrivalDelayGroups) %>% summarise(ArrivalDelayGroupsW = mean(ArrDel15==1))
# Realizo un join en la columna ArrivalDelayGroups del dataset flights con los pesos calculados en ArrivalDelayGroupsWeights
flights$ArrivalDelayGroups <- inner_join(flights, ArrivalDelayGroupsWeights, by = "ArrivalDelayGroups")$ArrivalDelayGroupsW


#### 2.6.20. ArrTimeBlk ####
# Selecciono la columna de categoría ArrTimeBlk y el retraso del que me voy a servir para la 
# asignación de pesos ArrDel15
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDel15 = 1)
ArrTimeBlkWeights <- flights %>% group_by(ArrTimeBlk) %>% summarise(ArrTimeBlkW = mean(ArrDel15==1))
# Realizo un join en la columna ArrTimeBlk del dataset flights con los pesos calculados en ArrTimeBlkWeights
flights$ArrTimeBlk <- inner_join(flights, ArrTimeBlkWeights, by = "ArrTimeBlk")$ArrTimeBlkW


#### 2.6.21. DistanceGroup ####
# Selecciono la columna de categoría DistanceGroup y el retraso del que me voy a servir para la 
# asignación de pesos ArrDel15
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDel15 = 1)
DistanceGroupWeights <- flights %>% group_by(DistanceGroup) %>% summarise(DistanceGroupW = mean(ArrDel15==1))
# Realizo un join en la columna DistanceGroup del dataset flights con los pesos calculados en DistanceGroupWeights
flights$DistanceGroup <- inner_join(flights, DistanceGroupWeights, by = "DistanceGroup")$DistanceGroupW
# *************************************************************************************************



# *************************************************************************************************
#### 2.7. Guardo la relación de las variables categóricas y sus pesos ####
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
#### 2.8. Elimino variables categóricas y asignaciones innecesarias ####
rm(ArrivalDelayGroupsWeights)
rm(ArrTimeBlkWeights)
rm(DayofMonthWeights)
rm(DayOfWeekWeights)
rm(DepartureDelayGroupsWeights)
rm(DepTimeBlkWeights)
rm(DestAirportSeqIDWeights)
rm(DestStateWeights)
rm(DestWeights)
rm(DistanceGroupWeights)
rm(FlightNumWeights)
rm(MonthWeights)
rm(OriginAirportSeqIDWeights)
rm(OriginStateWeights)
rm(OriginWeights)
rm(TailNumWeights)
rm(UniqueCarrierWeights)
rm(TimeRoute)
# *************************************************************************************************



# *************************************************************************************************
#### 2.9. Guardo el dataframe resultante de la normalización ####
# Guardo el dataset totalmente normalizado
flightsWeights <- flights
write.table(flightsWeights, file = "flightsWeights.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
# *************************************************************************************************



