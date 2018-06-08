#### TFM: ANÁLISIS Y PREDICCIÓN DEL RESTRASO EN LOS VUELOS ####


#### 4. MODELOS DE REGRESIÓN: Modelos de regresión y su comparación ####



# *************************************************************************************************
##### 4.1. Bloque de carga de librerias #####
# varImp
if(!require("caret")){
  install.packages("caret")
  library(caret)
}
# Random Forest
if(!require("randomForest")){
  install.packages("randomForest")
  library(randomForest)
}
# *************************************************************************************************



# Predicción del tiempo de retraso cuando se sabe que un vuelo ya está retrasado #



# *************************************************************************************************
#### 4.2. Generación del dataset y análisis de outliers  ####
# Es necesario generar un nuevo dataframe únicamente con los vuelos retrasados (ArrDel15=1) y volver
# a recalcular sus pesos, el último dataframe que se guardó antes de calcular los pesos para la 
# Clasificación es finalFlights:
# Genero un nuevo dataframe únicamente con los vuelos retrasados
flightsDelay <- finalFlights %>% filter(ArrDel15==1)

# Los retrasos se agrupan por franjas de 15 minutos hasta un máximo de 180 minutos.
# Voy a ver cuantos vuelos quedan por encima del percentil 95
quantile(flightsDelay$ArrDelayMinutes, .95)
# 95% 
# 179 
# Todos aquellos vuelos que tienen un retraso de más de 179 minutos. Voy a ver de cuantos vuelos 
# se tratan
flightsDelay %>% filter(ArrDelayMinutes>180) %>% nrow() # 10819 son outilers
# Prescindo de ellos
outliers <- flightsDelay %>% filter(ArrDelayMinutes>180) 
# Se eliminan los outliers ya que estos influyen en el cálculo del error de los modelos puesto 
# que estos son muy elevados.
# En este caso los outliers desvirtuan el modelo puesto que se tratan de retrasos que están por 
# encima de lo normal.
flightsDelay <- flightsDelay %>% filter(!ArrDelayMinutes>180) 
# Ahora se puede prescindir de la variable ArrDel15, están seleccionados todoso los vuelos con 
# ArrDel15 = 1
flightsDelay$ArrDel15 <- NULL
# *************************************************************************************************



# *************************************************************************************************
#### 4.3. Asignación de pesos a las variables categóricas y normalización de los datos  ####


#### 4.3.1. Month ####
# Calculo la media de los pesos de cada valor en función de ArrDelayMinutes 
MonthWeightsR <- flightsDelay %>% group_by(Month) %>% summarise(MonthW = mean(ArrDelayMinutes)) 
# Normalizo los pesos
MonthWeightsR$NRM <- rescale(MonthWeightsR$MonthW)
# Realizo un join en la columna Month del dataset flights con los pesos calculados en MonthWeigths
flightsDelay$Month <- inner_join(flightsDelay, MonthWeightsR, by = "Month")$NRM


#### 4.3.2. DayofMonth ####
# Calculo la media de los pesos de cada valor en función de ArrDelayMinutes 
DayofMonthWeightsR <- flightsDelay %>% group_by(DayofMonth) %>% summarise(DayofMonthW = mean(ArrDelayMinutes))
# Normalizo los pesos
DayofMonthWeightsR$NRM <- rescale(DayofMonthWeightsR$DayofMonthW)
# Realizo un join en la columna DayofMonth del dataset flightsDelay con los pesos calculados en DayofMonthWeightsR
flightsDelay$DayofMonth <- inner_join(flightsDelay, DayofMonthWeightsR, by = "DayofMonth")$NRM


#### 4.3.3. DayOfWeek ####
# Calculo la media de los pesos de cada valor en función de ArrDelayMinutes 
DayOfWeekWeightsR <- flightsDelay %>% group_by(DayOfWeek) %>% summarise(DayOfWeekW = mean(ArrDelayMinutes))
# Normalizo los pesos
DayOfWeekWeightsR$NRM <- rescale(DayOfWeekWeightsR$DayOfWeekW)
# Realizo un join en la columna DayOfWeek del dataset flightsDelay con los pesos calculados en DayofMonthWeightsR
flightsDelay$DayOfWeek <- inner_join(flightsDelay, DayOfWeekWeightsR, by = "DayOfWeek")$NRM


#### 4.3.4. UniqueCarrier ####
# Calculo la media de los pesos de cada valor en función de ArrDelayMinutes
UniqueCarrierWeightsR <- flightsDelay %>% group_by(UniqueCarrier) %>% summarise(UniqueCarrierW = mean(ArrDelayMinutes))
# Normalizo los pesos
UniqueCarrierWeightsR$NRM <- rescale(UniqueCarrierWeightsR$UniqueCarrierW)
# Realizo un join en la columna UniqueCarrier del dataset flightsDelay con los pesos calculados en 
# UniqueCarrierWeightsR
flightsDelay$UniqueCarrier <- inner_join(flightsDelay, UniqueCarrierWeightsR, by = "UniqueCarrier")$NRM


#### 4.3.5. TailNum #### 
# Calculo la media de los pesos de cada valor en función de ArrDelayMinutes
TailNumWeightsR <- flightsDelay %>% group_by(TailNum) %>% summarise(TailNumW = mean(ArrDelayMinutes))
# Normalizo los pesos
TailNumWeightsR$NRM <- rescale(TailNumWeightsR$TailNumW)
# Realizo un join en la columna TailNum del dataset flightsDelay con los pesos calculados en TailNumWeightsR
flightsDelay$TailNum <- inner_join(flightsDelay, TailNumWeightsR, by = "TailNum")$NRM


#### 4.3.6. FlightNum ####
# Calculo la media de los pesos de cada valor en función de ArrDelayMinutes
FlightNumWeightsR <- flightsDelay %>% group_by(FlightNum) %>% summarise(FlightNumW = mean(ArrDelayMinutes))
# Normalizo los pesos
FlightNumWeightsR$NRM <- rescale(FlightNumWeightsR$FlightNumW)
# Realizo un join en la columna FlightNum del dataset flightsDelay con los pesos calculados en FlightNumWeightsR
flightsDelay$FlightNum <- inner_join(flightsDelay, FlightNumWeightsR, by = "FlightNum")$NRM


#### 4.3.7. OriginAirportSeqID ####
# Calculo la media de los pesos de cada valor en función de ArrDelayMinutes
OriginAirportSeqIDWeightsR <- flightsDelay %>% group_by(OriginAirportSeqID) %>% summarise(OriginAirportSeqIDW = mean(ArrDelayMinutes))
# Normalizo los pesos
OriginAirportSeqIDWeightsR$NRM <- rescale(OriginAirportSeqIDWeightsR$OriginAirportSeqIDW)
# Realizo un join en la columna OriginAirportSeqID del dataset flightsDelay con los pesos calculados en 
# OriginAirportSeqIDWeightsR
flightsDelay$OriginAirportSeqID <- inner_join(flightsDelay, OriginAirportSeqIDWeightsR, by = "OriginAirportSeqID")$NRM


#### 4.3.8. Origin ####
# Calculo la media de los pesos de cada valor en función de ArrDelayMinutes
OriginWeightsR <- flightsDelay %>% group_by(Origin) %>% summarise(OriginW = mean(ArrDelayMinutes))
# Normalizo los pesos
OriginWeightsR$NRM <- rescale(OriginWeightsR$OriginW)
# Realizo un join en la columna Origin del dataset flightsDelay con los pesos calculados en OriginWeightsR
flightsDelay$Origin <- inner_join(flightsDelay, OriginWeightsR, by = "Origin")$NRM


#### 4.3.9. OriginState ####
# Calculo la media de los pesos de cada valor en función de ArrDelayMinutes
OriginStateWeightsR <- flightsDelay %>% group_by(OriginState) %>% summarise(OriginStateW = mean(ArrDelayMinutes))
# Normalizo los pesos
OriginStateWeightsR$NRM <- rescale(OriginStateWeightsR$OriginStateW)
# Realizo un join en la columna OriginState del dataset flightsDelay con los pesos calculados en OriginStateWeightsR
flightsDelay$OriginState <- inner_join(flightsDelay, OriginStateWeightsR, by = "OriginState")$NRM


#### 4.3.10. DestAirportSeqID ####
# Calculo la media de los pesos de cada valor en función de ArrDelayMinutes
DestAirportSeqIDWeightsR <- flightsDelay %>% group_by(DestAirportSeqID) %>% summarise(DestAirportSeqIDW = mean(ArrDelayMinutes))
# Normalizo los pesos
DestAirportSeqIDWeightsR$NRM <- rescale(DestAirportSeqIDWeightsR$DestAirportSeqIDW)
# Realizo un join en la columna DestAirportSeqID del dataset flightsDelay con los pesos calculados en DestAirportSeqIDWeightsR
flightsDelay$DestAirportSeqID <- inner_join(flightsDelay, DestAirportSeqIDWeightsR, by = "DestAirportSeqID")$NRM


#### 4.3.11. Dest ####
# Calculo la media de los pesos de cada valor en función de ArrDelayMinutes
DestWeightsR <- flightsDelay %>% group_by(Dest) %>% summarise(DestW = mean(ArrDelayMinutes))
# Normalizo los pesos
DestWeightsR$NRM <- rescale(DestWeightsR$DestW)
# Realizo un join en la columna Dest del dataset flightsDelay con los pesos calculados en DestWeightsR
flightsDelay$Dest <- inner_join(flightsDelay, DestWeightsR, by = "Dest")$NRM


#### 4.3.12. DestState ####
# Calculo la media de los pesos de cada valor en función de ArrDelayMinutes
DestStateWeightsR <- flightsDelay %>% group_by(DestState) %>% summarise(DestStateW = mean(ArrDelayMinutes))
# Normalizo los pesos
DestStateWeightsR$NRM <- rescale(DestStateWeightsR$DestStateW)
# Realizo un join en la columna DestState del dataset flightsDelay con los pesos calculados en DestStateWeightsR
flightsDelay$DestState <- inner_join(flightsDelay, DestStateWeightsR, by = "DestState")$NRM


#### 4.3.13. DepTime ####
# Calculo la media de los pesos de cada valor en función de ArrDelayMinutes
DepTimeWeightsR <- flightsDelay %>% group_by(DepTime) %>% summarise(DepTimeW = mean(ArrDelayMinutes))
# Normalizo los pesos
DepTimeWeightsR$NRM <- rescale(DepTimeWeightsR$DepTimeW)
# Realizo un join en la columna DestState del dataset flightsDelay con los pesos calculados en DepTimeWeightsR
flightsDelay$DepTime <- inner_join(flightsDelay, DepTimeWeightsR, by = "DepTime")$NRM


#### 4.3.14. DepDelay ####
# Calculo la media de los pesos de cada valor en función de ArrDelayMinutes
DepDelayWeightsR <- flightsDelay %>% group_by(DepDelay) %>% summarise(DepDelayW = mean(ArrDelayMinutes))
# Normalizo los pesos
DepDelayWeightsR$NRM <- rescale(DepDelayWeightsR$DepDelayW)
# Realizo un join en la columna DestState del dataset flightsDelay con los pesos calculados en DepDelayWeightsR
flightsDelay$DepDelay <- inner_join(flightsDelay, DepDelayWeightsR, by = "DepDelay")$NRM


#### 4.3.15. DepDelayMinutes ####
# Calculo la media de los pesos de cada valor en función de ArrDelayMinutes
DepDelayMinutesWeightsR <- flightsDelay %>% group_by(DepDelayMinutes) %>% summarise(DepDelayMinutesW = mean(ArrDelayMinutes))
# Normalizo los pesos
DepDelayMinutesWeightsR$NRM <- rescale(DepDelayMinutesWeightsR$DepDelayMinutesW)
# Realizo un join en la columna DestState del dataset flightsDelay con los pesos calculados en DepDelayMinutesWeightsR
flightsDelay$DepDelayMinutes <- inner_join(flightsDelay, DepDelayMinutesWeightsR, by = "DepDelayMinutes")$NRM


#### 4.3.16. DepDel15 ####
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDelayMinutes)
DepDel15WeightsR <- flightsDelay %>% group_by(DepDel15) %>% summarise(DepDel15W = mean(ArrDelayMinutes))
# Normalizo los pesos
DepDel15WeightsR$NRM <- rescale(DepDel15WeightsR$DepDel15W)
# Realizo un join en la columna DepDel15 del dataset flightsDelay con los pesos calculados en DepDel15WeightsR
flightsDelay$DepDel15 <- inner_join(flightsDelay, DepDel15WeightsR, by = "DepDel15")$NRM


#### 4.3.17. DepartureDelayGroups ####
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDelayMinutes)
DepartureDelayGroupsWeightsR <- flightsDelay %>% group_by(DepartureDelayGroups) %>% summarise(DepartureDelayGroupsW = mean(ArrDelayMinutes))
# Normalizo los pesos
DepartureDelayGroupsWeightsR$NRM <- rescale(DepartureDelayGroupsWeightsR$DepartureDelayGroupsW)
# Realizo un join en la columna DepartureDelayGroups del dataset flightsDelay con los pesos calculados en DepartureDelayGroupsWeightsR
flightsDelay$DepartureDelayGroups <- inner_join(flightsDelay, DepartureDelayGroupsWeightsR, by = "DepartureDelayGroups")$NRM


#### 4.3.18. DepTimeBlk ####
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDelayMinutes)
DepTimeBlkWeightsR <- flightsDelay %>% group_by(DepTimeBlk) %>% summarise(DepTimeBlkW = mean(ArrDelayMinutes))
# Normalizo los pesos
DepTimeBlkWeightsR$NRM <- rescale(DepTimeBlkWeightsR$DepTimeBlkW)
# Realizo un join en la columna DepTimeBlk del dataset flightsDelay con los pesos calculados en DepTimeBlkWeightsR
flightsDelay$DepTimeBlk <- inner_join(flightsDelay, DepTimeBlkWeightsR, by = "DepTimeBlk")$NRM


#### 4.3.19. ArrTime ####
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDelayMinutes)
ArrTimeWeightsR <- flightsDelay %>% group_by(ArrTime) %>% summarise(ArrTimeW = mean(ArrDelayMinutes))
# Normalizo los pesos
ArrTimeWeightsR$NRM <- rescale(ArrTimeWeightsR$ArrTimeW)
# Realizo un join en la columna ArrTime del dataset flightsDelay con los pesos calculados en ArrTimeWeightsR
flightsDelay$ArrTime <- inner_join(flightsDelay, ArrTimeWeightsR, by = "ArrTime")$NRM


#### 4.3.20. ArrDelay ####
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDelayMinutes)
ArrDelayWeightsR <- flightsDelay %>% group_by(ArrDelay) %>% summarise(ArrDelayW = mean(ArrDelayMinutes))
# Normalizo los pesos
ArrDelayWeightsR$NRM <- rescale(ArrDelayWeightsR$ArrDelayW)
# Realizo un join en la columna ArrDelay del dataset flightsDelay con los pesos calculados en ArrDelayWeightsR
flightsDelay$ArrDelay <- inner_join(flightsDelay, ArrDelayWeightsR, by = "ArrDelay")$NRM


#### 4.3.22. ArrivalDelayGroups ####
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDelayMinutes)
ArrivalDelayGroupsWeightsR <- flightsDelay %>% group_by(ArrivalDelayGroups) %>% summarise(ArrivalDelayGroupsW = mean(ArrDelayMinutes))
# Normalizo los pesos
ArrivalDelayGroupsWeightsR$NRM <- rescale(ArrivalDelayGroupsWeightsR$ArrivalDelayGroupsW)
# Realizo un join en la columna ArrivalDelayGroups del dataset flightsDelay con los pesos calculados en ArrivalDelayGroupsWeightsR
flightsDelay$ArrivalDelayGroups <- inner_join(flightsDelay, ArrivalDelayGroupsWeightsR, by = "ArrivalDelayGroups")$NRM


#### 4.3.23. ArrTimeBlk ####
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDelayMinutes)
ArrTimeBlkWeightsR <- flightsDelay %>% group_by(ArrTimeBlk) %>% summarise(ArrTimeBlkW = mean(ArrDelayMinutes))
# Normalizo los pesos
ArrTimeBlkWeightsR$NRM <- rescale(ArrTimeBlkWeightsR$ArrTimeBlkW)
# Realizo un join en la columna ArrTimeBlk del dataset flightsDelay con los pesos calculados en ArrTimeBlkWeightsR
flightsDelay$ArrTimeBlk <- inner_join(flightsDelay, ArrTimeBlkWeightsR, by = "ArrTimeBlk")$NRM

#### 4.3.24. ActualElapsedTime ####
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDelayMinutes)
ActualElapsedTimeWeightsR <- flightsDelay %>% group_by(ActualElapsedTime) %>% summarise(ActualElapsedTimeW = mean(ArrDelayMinutes))
# Normalizo los pesos
ActualElapsedTimeWeightsR$NRM <- rescale(ActualElapsedTimeWeightsR$ActualElapsedTimeW)
# Realizo un join en la columna ActualElapsedTime del dataset flightsDelay con los pesos calculados en ActualElapsedTimeWeightsR
flightsDelay$ActualElapsedTime <- inner_join(flightsDelay, ActualElapsedTimeWeightsR, by = "ActualElapsedTime")$NRM


#### 4.3.25. Distance ####
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDelayMinutes)
DistanceWeightsR <- flightsDelay %>% group_by(Distance) %>% summarise(DistanceW = mean(ArrDelayMinutes))
# Normalizo los pesos
DistanceWeightsR$NRM <- rescale(DistanceWeightsR$DistanceW)
# Realizo un join en la columna Distance del dataset flightsDelay con los pesos calculados en DistanceWeightsR
flightsDelay$Distance <- inner_join(flightsDelay, DistanceWeightsR, by = "Distance")$NRM


#### 4.3.26. DistanceGroup ####
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDelayMinutes)
DistanceGroupWeightsR <- flightsDelay %>% group_by(DistanceGroup) %>% summarise(DistanceGroupW = mean(ArrDelayMinutes))
# Normalizo los pesos
DistanceGroupWeightsR$NRM <- rescale(DistanceGroupWeightsR$DistanceGroupW)
# Realizo un join en la columna DistanceGroup del dataset flightsDelay con los pesos calculados en DistanceGroupWeightsR
flightsDelay$DistanceGroup <- inner_join(flightsDelay, DistanceGroupWeightsR, by = "DistanceGroup")$NRM


#### 4.3.27. CarrierDelay ####
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDelayMinutes)
CarrierDelayWeightsR <- flightsDelay %>% group_by(CarrierDelay) %>% summarise(CarrierDelayW = mean(ArrDelayMinutes))
# Normalizo los pesos
CarrierDelayWeightsR$NRM <- rescale(CarrierDelayWeightsR$CarrierDelayW)
# Realizo un join en la columna CarrierDelay del dataset flightsDelay con los pesos calculados en CarrierDelayWeightsR
flightsDelay$CarrierDelay <- inner_join(flightsDelay, CarrierDelayWeightsR, by = "CarrierDelay")$NRM


#### 4.3.28. WeatherDelay ####
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDelayMinutes)
WeatherDelayWeightsR <- flightsDelay %>% group_by(WeatherDelay) %>% summarise(WeatherDelayW = mean(ArrDelayMinutes))
# Normalizo los pesos
WeatherDelayWeightsR$NRM <- rescale(WeatherDelayWeightsR$WeatherDelayW)
# Realizo un join en la columna WeatherDelay del dataset flightsDelay con los pesos calculados en WeatherDelayWeightsR
flightsDelay$WeatherDelay <- inner_join(flightsDelay, WeatherDelayWeightsR, by = "WeatherDelay")$NRM


#### 4.3.29. NASDelay ####
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDelayMinutes)
NASDelayWeightsR <- flightsDelay %>% group_by(NASDelay) %>% summarise(NASDelayW = mean(ArrDelayMinutes))
# Normalizo los pesos
NASDelayWeightsR$NRM <- rescale(NASDelayWeightsR$NASDelayW)
# Realizo un join en la columna NASDelay del dataset flightsDelay con los pesos calculados en NASDelayWeightsR
flightsDelay$NASDelay <- inner_join(flightsDelay, NASDelayWeightsR, by = "NASDelay")$NRM


#### 4.3.30. SecurityDelay ####
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDelayMinutes)
SecurityDelayWeightsR <- flightsDelay %>% group_by(SecurityDelay) %>% summarise(SecurityDelayW = mean(ArrDelayMinutes))
# Normalizo los pesos
SecurityDelayWeightsR$NRM <- rescale(SecurityDelayWeightsR$SecurityDelayW)
# Realizo un join en la columna SecurityDelay del dataset flightsDelay con los pesos calculados en SecurityDelayWeightsR
flightsDelay$SecurityDelay <- inner_join(flightsDelay, SecurityDelayWeightsR, by = "SecurityDelay")$NRM


#### 4.3.31. LateAircraftDelay ####
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDelayMinutes)
LateAircraftDelayWeightsR <- flightsDelay %>% group_by(LateAircraftDelay) %>% summarise(LateAircraftDelayW = mean(ArrDelayMinutes))
# Normalizo los pesos
LateAircraftDelayWeightsR$NRM <- rescale(LateAircraftDelayWeightsR$LateAircraftDelayW)
# Realizo un join en la columna LateAircraftDelay del dataset flightsDelay con los pesos calculados en LateAircraftDelayWeightsR
flightsDelay$LateAircraftDelay <- inner_join(flightsDelay, LateAircraftDelayWeightsR, by = "LateAircraftDelay")$NRM
# *************************************************************************************************


# *************************************************************************************************
#### 4.4. Guardo el dataframe normalizado ####
# Guardo el dataset totalmente normalizado
flightsDelaysWeights <- flightsDelay
write.table(flightsDelaysWeights, file = "data/flightsWeightsRegres.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
# *************************************************************************************************



# *************************************************************************************************
#### 4.5. Guardo la relación de las variables categóricas y sus pesos ####
write.table(MonthWeightsR, file = "CategoriesWeights/MonthWeightsR.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(DayofMonthWeightsR, file = "CategoriesWeights/DayofMonthWeightsR.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(DayOfWeekWeightsR, file = "CategoriesWeights/DayOfWeekWeightsR.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(UniqueCarrierWeightsR, file = "CategoriesWeights/UniqueCarrierWeightsR.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(TailNumWeightsR, file = "CategoriesWeights/TailNumWeightsR.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(FlightNumWeightsR, file = "CategoriesWeights/FlightNumWeightsR.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(OriginAirportSeqIDWeightsR, file = "CategoriesWeights/OriginAirportSeqIDWeightsR.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(OriginWeightsR, file = "CategoriesWeights/OriginWeightsR.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(OriginStateWeightsR, file = "CategoriesWeights/OriginStateWeightsR.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(DestAirportSeqIDWeightsR, file = "CategoriesWeights/DestAirportSeqIDWeightsR.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(DestWeightsR, file = "CategoriesWeights/DestWeightsR.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(DestStateWeightsR, file = "CategoriesWeights/DestStateWeightsR.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(DepTimeWeightsR, file = "CategoriesWeights/DepTimeWeightsR.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(DepDelayWeightsR, file = "CategoriesWeights/DepDelayWeightsR.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(DepDelayMinutesWeightsR, file = "CategoriesWeights/DepDelayMinutesWeightsR.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(DepDel15WeightsR, file = "CategoriesWeights/DepDel15WeightsR.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(DepartureDelayGroupsWeightsR, file = "CategoriesWeights/DepartureDelayGroupsWeightsR.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(DepTimeBlkWeightsR, file = "CategoriesWeights/DepTimeBlkWeightsR.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(ArrTimeWeightsR, file = "CategoriesWeights/ArrTimeWeightsR.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(ArrDelayWeightsR, file = "CategoriesWeights/ArrDelayWeightsR.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(ArrivalDelayGroupsWeightsR, file = "CategoriesWeights/ArrivalDelayGroupsWeightsR.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(ArrTimeBlkWeightsR, file = "CategoriesWeights/ArrTimeBlkWeightsR.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(ActualElapsedTimeWeightsR, file = "CategoriesWeights/ActualElapsedTimeWeightsR.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(DistanceWeightsR, file = "CategoriesWeights/DistanceWeightsR.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(DistanceGroupWeightsR, file = "CategoriesWeights/DistanceGroupWeightsR.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(CarrierDelayWeightsR, file = "CategoriesWeights/CarrierDelayWeightsR.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(WeatherDelayWeightsR, file = "CategoriesWeights/WeatherDelayWeightsR.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(NASDelayWeightsR, file = "CategoriesWeights/NASDelayWeightsR.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(SecurityDelayWeightsR, file = "CategoriesWeights/SecurityDelayWeightsR.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(LateAircraftDelayWeightsR, file = "CategoriesWeights/LateAircraftDelayWeightsR.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
# *************************************************************************************************



# *************************************************************************************************
#### 5.6. Elimino variables categóricas ####
rm(ActualElapsedTimeWeightsR)
rm(ArrDelayWeightsR)
rm(ArrivalDelayGroupsWeightsR)
rm(ArrTimeBlkWeightsR)
rm(ArrTimeWeightsR)
rm(CarrierDelayWeightsR)
rm(DayofMonthWeightsR)
rm(DayOfWeekWeightsR)
rm(DepartureDelayGroupsWeightsR)
rm(DepDel15WeightsR)
rm(DepDelayMinutesWeightsR)
rm(DepTimeWeightsR)
rm(DestAirportSeqIDWeightsR)
rm(DestStateWeightsR)
rm(DepDelayWeightsR)
rm(DepTimeBlkWeightsR)
rm(DestWeightsR)
rm(DistanceGroupWeightsR)
rm(DistanceWeightsR)
rm(FlightNumWeightsR)
rm(LateAircraftDelayWeightsR)
rm(MonthWeightsR)
rm(NASDelayWeightsR)
rm(OriginAirportSeqIDWeightsR)
rm(OriginStateWeightsR)
rm(OriginWeightsR)
rm(SecurityDelayWeightsR)
rm(TailNumWeightsR)
rm(UniqueCarrierWeightsR)
rm(WeatherDelayWeightsR)
# *************************************************************************************************



# *************************************************************************************************
##### 4.7. Bloque de modelización #####


# Predicción del tiempo de retraso #
# He escogido para este problema de regresión, los algoritmos de Regresión Lineal y Random Fores

# Lo que se puede indicar, gracias a la realización de los modelos de clasificación, es que para 
# la realización del modelo es necesario prescindir de:
# * Las características que nos aportan información del retraso: ArrDelay, ArrivalDelayGroups, 
#   ArrDelayMinutes, ArrTime, ActualElapsedTime y ArrTimeBlk.
# * Las características que indican el grupo de retraso y el tiempo: CarrierDelay, LateAircraftDelay, 
#   NASDelay,WeatherDelay y SecurityDelay; puesto que ya existe un DepDelay y un DepDelayMinutes que nos indica
#   si sale con retraso el vuelo...
# Con esto, voy realizar de primeras un modelo con el resto de las características:
# DepDel15+DepartureDelayGroups+DepDelayMinutes+DepDelay+DistanceGroup+DayofMonth+DestState+DepTime+
# UniqueCarrier+DepTimeBlk+Distance+OriginState+FlightNum+Month+TailNum+Dest+DestAirportSeqID+
# OriginAirportSeqID+Origin+DayOfWeek

# La finalidad es encontrar el modelo que tenga el error más bajo y prediga mejor el tiempo de 
# retraso.

# RegressionModel: DepDel15+DepartureDelayGroups+DepDelayMinutes+DepDelay+DistanceGroup+DayofMonth+
#                   DestState+DepTime+UniqueCarrier+DepTimeBlk+Distance+OriginState+FlightNum+Month+
#                   TailNum+Dest+DestAirportSeqID+OriginAirportSeqID+Origin+DayOfWeek

# Establezco la semilla para que la misma muestra pueda reproducirse también en el futuro
set.seed(7) 
# Selecciono el 80% de los datos como muestra del total de las filas de los datos  
sampleRegression <- sample.int(n = nrow(flightsDelay), size = floor(.80*nrow(flightsDelay)), replace = F)
trainRegression <- flightsDelay[sampleRegression, ] # Conjunto de entrenamiento
testRegression  <- flightsDelay[-sampleRegression, ] # Conjunto de test

hist(flightsDelay$ArrDelayMinutes, breaks = 100)


#### 4.7.1 Regresión Lineal ####
modLRRegres <- lm(ArrDelayMinutes~DepDel15+DepartureDelayGroups+DepDelayMinutes+DepDelay+DistanceGroup+
                     DayofMonth+DestState+DepTime+UniqueCarrier+DepTimeBlk+Distance+OriginState+
                     FlightNum+Month+TailNum+Dest+DestAirportSeqID+OriginAirportSeqID+Origin+DayOfWeek, 
                   data = trainRegression)
summary(modLRRegres)
# Parece que las variables más correlacionadas son: DepDel15, DepDelay, DistanceGroup, DayofMonth, 
# DestState, DepTime, UniqueCarrier, DepTimeBlk, Distance, OriginState, FlightNum, Month, TailNum, 
# Dest, DestAirportSeqID, DayOfWeek.
# Predicción
predictionLRRegres <- predict.lm(modLRRegres,testRegression)
# Real
actual <- testRegression$ArrDelayMinutes
# MAPE
mapeLRRegres <- mean(abs((predictionLRRegres - actual))/actual)
mapeLRRegres
totalLRRegres <- (abs((predictionLRRegres - actual))/actual)
hist(totalLRRegres, breaks=100)
# 0.268138 de error
# RMSE -> Debe de ser lo más cercano a 0
errorLRRegres <- actual - predictionLRRegres
rmseLRRegres <- sqrt(mean(errorLRRegres^2))
rmseLRRegres
# 14.51123
name <- "LinearRegressionRegressionModel"
metricsMapeRMSE <- data.frame(name, mapeLRRegres,rmseLRRegres)
colnames(metricsMapeRMSE) <- c("Model","MAPE","RMSE")

#### 4.7.2. Random Forest ####
modRFRegres <- randomForest(ArrDelayMinutes~DepDel15+DepartureDelayGroups+DepDelayMinutes+DepDelay+
                               DistanceGroup+DayofMonth+DestState+DepTime+UniqueCarrier+DepTimeBlk+
                               Distance+OriginState+FlightNum+Month+TailNum+Dest+DestAirportSeqID+
                               OriginAirportSeqID+Origin+DayOfWeek, data = trainRegression, ntree=25)
# Predicción
predictionRFRegres <- predict(modRFRegres,testRegression)
# MAPE
mapeRFRegres <- mean(abs((predictionRFRegres - actual))/actual)
mapeRFRegres
totalRFRegres <- (abs((predictionRFRegres - actual))/actual)
hist(totalRFRegres, breaks=100)
# 0.2726795 de error
# RMSE -> Debe de ser lo más cercano a 0
errorRFRegres <- actual - predictionRFRegres
rmseRFRegres <- sqrt(mean(errorRFRegres^2))
rmseRFRegres
# 14.26581
name <- "RandomForestRegressionModel"
df <- data.frame(name,mapeRFRegres,rmseRFRegres)
colnames(df) <- c("Model","MAPE","RMSE")
metricsMapeRMSE <- rbind(metricsMapeRMSE,df)
# Conclusión: La media del porcentaje de error es ligeramente menor en la Regresión Lineal. Al 
#             testear el modelo, tiene un 26% de error.
# *************************************************************************************************

