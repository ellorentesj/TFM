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
# Random Forest
if(!require("scales")){
  install.packages("scales")
  library(scales)
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

flightsDelay %>% filter(DepDelayMinutes>180) %>% nrow() 
# *************************************************************************************************




# *************************************************************************************************
#### 4.2. Asignación de pesos a las variables categóricas y normalización de los datos  ####

#### 4.2.1. Month ####
# Calculo la media de los pesos de cada valor en función de ArrDelayMinutes 
MonthWeightsR <- flightsDelay %>% group_by(Month) %>% summarise(MonthW = mean(ArrDelayMinutes)) 
# Normalizo los pesos
MonthWeightsR$NRM <- rescale(MonthWeightsR$MonthW)
# Realizo un join en la columna Month del dataset flights con los pesos calculados en MonthWeigths
flightsDelay$Month <- inner_join(flightsDelay, MonthWeightsR, by = "Month")$NRM

#### 4.2.2. DayofMonth ####
# Calculo la media de los pesos de cada valor en función de ArrDelayMinutes 
DayofMonthWeightsR <- flightsDelay %>% group_by(DayofMonth) %>% summarise(DayofMonthW = mean(ArrDelayMinutes))
# Normalizo los pesos
DayofMonthWeightsR$NRM <- rescale(DayofMonthWeightsR$DayofMonthW)
# Realizo un join en la columna DayofMonth del dataset flightsDelay con los pesos calculados en DayofMonthWeightsR
flightsDelay$DayofMonth <- inner_join(flightsDelay, DayofMonthWeightsR, by = "DayofMonth")$NRM

#### 4.2.3. DayOfWeek ####
# Calculo la media de los pesos de cada valor en función de ArrDelayMinutes 
DayOfWeekWeightsR <- flightsDelay %>% group_by(DayOfWeek) %>% summarise(DayOfWeekW = mean(ArrDelayMinutes))
# Normalizo los pesos
DayOfWeekWeightsR$NRM <- rescale(DayOfWeekWeightsR$DayOfWeekW)
# Realizo un join en la columna DayOfWeek del dataset flightsDelay con los pesos calculados en DayofMonthWeightsR
flightsDelay$DayOfWeek <- inner_join(flightsDelay, DayOfWeekWeightsR, by = "DayOfWeek")$NRM

#### 4.2.4. UniqueCarrier ####
# Calculo la media de los pesos de cada valor en función de ArrDelayMinutes
UniqueCarrierWeightsR <- flightsDelay %>% group_by(UniqueCarrier) %>% summarise(UniqueCarrierW = mean(ArrDelayMinutes))
# Normalizo los pesos
UniqueCarrierWeightsR$NRM <- rescale(UniqueCarrierWeightsR$UniqueCarrierW)
# Realizo un join en la columna UniqueCarrier del dataset flightsDelay con los pesos calculados en 
# UniqueCarrierWeightsR
flightsDelay$UniqueCarrier <- inner_join(flightsDelay, UniqueCarrierWeightsR, by = "UniqueCarrier")$NRM

#### 4.2.5. TailNum #### 
# Calculo la media de los pesos de cada valor en función de ArrDelayMinutes
TailNumWeightsR <- flightsDelay %>% group_by(TailNum) %>% summarise(TailNumW = mean(ArrDelayMinutes))
# Normalizo los pesos
TailNumWeightsR$NRM <- rescale(TailNumWeightsR$TailNumW)
# Realizo un join en la columna TailNum del dataset flightsDelay con los pesos calculados en TailNumWeightsR
flightsDelay$TailNum <- inner_join(flightsDelay, TailNumWeightsR, by = "TailNum")$NRM

#### 4.2.6. FlightNum ####
# Calculo la media de los pesos de cada valor en función de ArrDelayMinutes
FlightNumWeightsR <- flightsDelay %>% group_by(FlightNum) %>% summarise(FlightNumW = mean(ArrDelayMinutes))
# Normalizo los pesos
FlightNumWeightsR$NRM <- rescale(FlightNumWeightsR$FlightNumW)
# Realizo un join en la columna FlightNum del dataset flightsDelay con los pesos calculados en FlightNumWeightsR
flightsDelay$FlightNum <- inner_join(flightsDelay, FlightNumWeightsR, by = "FlightNum")$NRM

#### 4.2.7. OriginAirportSeqID ####
# Calculo la media de los pesos de cada valor en función de ArrDelayMinutes
OriginAirportSeqIDWeightsR <- flightsDelay %>% group_by(OriginAirportSeqID) %>% summarise(OriginAirportSeqIDW = mean(ArrDelayMinutes))
# Normalizo los pesos
OriginAirportSeqIDWeightsR$NRM <- rescale(OriginAirportSeqIDWeightsR$OriginAirportSeqIDW)
# Realizo un join en la columna OriginAirportSeqID del dataset flightsDelay con los pesos calculados en 
# OriginAirportSeqIDWeightsR
flightsDelay$OriginAirportSeqID <- inner_join(flightsDelay, OriginAirportSeqIDWeightsR, by = "OriginAirportSeqID")$NRM

#### 4.2.8. Origin ####
# Calculo la media de los pesos de cada valor en función de ArrDelayMinutes
OriginWeightsR <- flightsDelay %>% group_by(Origin) %>% summarise(OriginW = mean(ArrDelayMinutes))
# Normalizo los pesos
OriginWeightsR$NRM <- rescale(OriginWeightsR$OriginW)
# Realizo un join en la columna Origin del dataset flightsDelay con los pesos calculados en OriginWeightsR
flightsDelay$Origin <- inner_join(flightsDelay, OriginWeightsR, by = "Origin")$NRM

#### 4.2.9. OriginCityName ####
# Como esta variable contiene la información extendida de Origin y ya no es necesaria, prescindo de
# ella 
flightsDelay$OriginCityName <- NULL

#### 4.2.10. OriginState ####
# Calculo la media de los pesos de cada valor en función de ArrDelayMinutes
OriginStateWeightsR <- flightsDelay %>% group_by(OriginState) %>% summarise(OriginStateW = mean(ArrDelayMinutes))
# Normalizo los pesos
OriginStateWeightsR$NRM <- rescale(OriginStateWeightsR$OriginStateW)
# Realizo un join en la columna OriginState del dataset flightsDelay con los pesos calculados en OriginStateWeightsR
flightsDelay$OriginState <- inner_join(flightsDelay, OriginStateWeightsR, by = "OriginState")$NRM

#### 4.2.11. OriginStateName ####
# Como esta variable contiene la información extendida de OriginState y ya no es necesaria, prescindo
# de ella
flightsDelay$OriginStateName <- NULL

#### 4.2.12. DestAirportSeqID ####
# Calculo la media de los pesos de cada valor en función de ArrDelayMinutes
DestAirportSeqIDWeightsR <- flightsDelay %>% group_by(DestAirportSeqID) %>% summarise(DestAirportSeqIDW = mean(ArrDelayMinutes))
# Normalizo los pesos
DestAirportSeqIDWeightsR$NRM <- rescale(DestAirportSeqIDWeightsR$DestAirportSeqIDW)
# Realizo un join en la columna DestAirportSeqID del dataset flightsDelay con los pesos calculados en DestAirportSeqIDWeightsR
flightsDelay$DestAirportSeqID <- inner_join(flightsDelay, DestAirportSeqIDWeightsR, by = "DestAirportSeqID")$NRM

#### 4.2.13. Dest ####
# Calculo la media de los pesos de cada valor en función de ArrDelayMinutes
DestWeightsR <- flightsDelay %>% group_by(Dest) %>% summarise(DestW = mean(ArrDelayMinutes))
# Normalizo los pesos
DestWeightsR$NRM <- rescale(DestWeightsR$DestW)
# Realizo un join en la columna Dest del dataset flightsDelay con los pesos calculados en DestWeightsR
flightsDelay$Dest <- inner_join(flightsDelay, DestWeightsR, by = "Dest")$NRM

#### 4.2.14. DestCityName ####
# Como esta variable contiene la información extendida de Dest y ya no es necesaria, prescindo de ella
flightsDelay$DestCityName <- NULL

#### 4.2.15. DestState ####
# Calculo la media de los pesos de cada valor en función de ArrDelayMinutes
DestStateWeightsR <- flightsDelay %>% group_by(DestState) %>% summarise(DestStateW = mean(ArrDelayMinutes))
# Normalizo los pesos
DestStateWeightsR$NRM <- rescale(DestStateWeightsR$DestStateW)
# Realizo un join en la columna DestState del dataset flightsDelay con los pesos calculados en DestStateWeightsR
flightsDelay$DestState <- inner_join(flightsDelay, DestStateWeightsR, by = "DestState")$NRM

#### 4.2.16. DestStateName ####
# Como esta variable contiene la información extendida de DestState y ya no es necearia, prescindo 
# de ella
flightsDelay$DestStateName <- NULL

#### 4.2.17. DepTime ####
# Calculo la media de los pesos de cada valor en función de ArrDelayMinutes
DepTimeR <- flightsDelay %>% group_by(DepTime) %>% summarise(DepTimeW = mean(ArrDelayMinutes))
# Normalizo los pesos
DepTimeR$NRM <- rescale(DepTimeR$DepTimeW)
# Realizo un join en la columna DestState del dataset flightsDelay con los pesos calculados en DepTimeR
flightsDelay$DepTime <- inner_join(flightsDelay, DepTimeR, by = "DepTime")$NRM

#### 4.2.18. DepDelay ####
# Calculo la media de los pesos de cada valor en función de ArrDelayMinutes
DepDelayR <- flightsDelay %>% group_by(DepDelay) %>% summarise(DepDelayW = mean(ArrDelayMinutes))
# Normalizo los pesos
DepDelayR$NRM <- rescale(DepDelayR$DepDelayW)
# Realizo un join en la columna DestState del dataset flightsDelay con los pesos calculados en DepDelayR
flightsDelay$DepDelay <- inner_join(flightsDelay, DepDelayR, by = "DepDelay")$NRM

#### 4.2.19. DepDelayMinutes ####
# Calculo la media de los pesos de cada valor en función de ArrDelayMinutes
DepDelayMinutesR <- flightsDelay %>% group_by(DepDelayMinutes) %>% summarise(DepDelayMinutesW = mean(ArrDelayMinutes))
# Normalizo los pesos
DepDelayMinutesR$NRM <- rescale(DepDelayMinutesR$DepDelayMinutesW)
# Realizo un join en la columna DestState del dataset flightsDelay con los pesos calculados en DepDelayMinutesR
flightsDelay$DepDelayMinutes <- inner_join(flightsDelay, DepDelayMinutesR, by = "DepDelayMinutes")$NRM

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
##### 4.2. Bloque de modelización #####
##### 3.2.2. Predicción: Tiempo de retraso cuando un vuelo está retrasado ####


set.seed(7) 
# Selecciono el 80% de los datos como muestra del total de las filas de los datos  
sample2 <- sample.int(n = nrow(flightsDelay), size = floor(.80*nrow(flightsDelay)), replace = F)
train2 <- flightsDelay[sample2, ] # Conjunto de entrenamiento
test2  <- flightsDelay[-sample2, ] # Conjunto de test







#### 3.2.2.1. Modelo completo ####

## Regresión Lineal ##
modLinRComplete <- lm(ArrDelayMinutes~DepTime+DepDelayMinutes+Distance+DayofMonthWeights+DayOfWeekWeights+
                        UniqueCarrierWeights+TailNumWeights+FlightNumWeights+OriginWeights, data = train2)
summary(modLinRComplete)
hist(flightsDelay$ArrDelayMinutes, breaks = 100)
predicted <- predict.lm(modLinRComplete,test2)
actual <- test2$ArrDelayMinutes

mape <- mean(abs((predicted - actual))/actual)
mape
# 0.3268298 de error

error <- actual - predicted
rmse <- sqrt(mean(error^2))
rmse
# 17.76245

# Quitamos los outliers y repetimos el proceso.
outliers <- flightsDelay %>% filter(ArrDelayMinutes>180)
flightsWithoutOutliers <- flightsDelay %>% filter(ArrDelayMinutes<=180) 
sample3 <- sample.int(n = nrow(flightsWithoutOutliers), size = floor(.80*nrow(flightsWithoutOutliers)), replace = F)
train3 <- flightsWithoutOutliers[sample3, ] # Conjunto de entrenamiento
test3  <- flightsWithoutOutliers[-sample3, ] # Conjunto de test
modLinROutliers <- lm(ArrDelayMinutes~DepTime+DepDelayMinutes+Distance+DayofMonthWeights+DayOfWeekWeights+
                        UniqueCarrierWeights+TailNumWeights+FlightNumWeights+OriginWeights, data = train3)
summary(modLinRComplete)
predicted <- predict.lm(modLinROutliers,test3)
actual <- test3$ArrDelayMinutes
mape <- mean(abs((predicted - actual))/actual)
mape
# 0.3217586 de error
error <- actual - predicted
rmse <- sqrt(mean(error^2))
rmse
# 16.05506

## Random Forest ##
modRFDelayComplete <- randomForest(ArrDelayMinutes~DepTime+DepDelayMinutes+Distance+DayofMonthWeights+
                                     DayOfWeekWeights+UniqueCarrierWeights+TailNumWeights+FlightNumWeights+
                                     OriginWeights, data = train2, ntree=20)
# He probado con ntree = 100 y ntree = 20. Finalmente lo he dejado establecido en 20 puesto que el error es 
# prácticamente igual: 0.2760714 con ntree = 20 y 0.2707187 con ntree=100.
# rmse tampoco tiene mucha mejora, para ntree = 20 su valor es 15.91736 y para ntree = 100 no disminuye a más de
# 15.71088
predicted <- predict(modRFDelayComplete, newdata = test2)
actual <- test2$ArrDelayMinutes

mape <- mean(abs((predicted - actual))/actual)
mape
# 32% de error

error <- actual - predicted
rmse <- sqrt(mean(error^2))
rmse

## XGBoost ##
modXGBDelay <- train(ArrDelayMinutes~DepTime+DepDelayMinutes+Distance+DayofMonthWeights+
                       DayOfWeekWeights+UniqueCarrierWeights+TailNumWeights+FlightNumWeights+
                       OriginWeights, data = train2, method = "xgbLinear", verbose = FALSE)


predicted <- predict(modXGBDelay, newdata = test2)
actual <- test2$ArrDelayMinutes

mape <- mean(abs((predicted - actual))/actual)
mape
# 32% de error

error <- actual - predicted
rmse <- sqrt(mean(error^2))
rmse
