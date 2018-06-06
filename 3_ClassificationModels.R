#### TFM: ANÁLISIS Y PREDICCIÓN DEL RESTRASO EN LOS VUELOS ####



#### 3. MODELOS DE CLASIFICACIÓN: Modelos de clasificación y su comparación ####



# *************************************************************************************************
##### 3.1. Bloque de carga de librerias #####
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



# *************************************************************************************************
#### 3.2. Asignación de pesos a las variables categóricas. Normalización de los datos  ####


#### 3.2.1. Month ####
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDel15 = 1)
MonthWeights <- flights %>% group_by(Month) %>% summarise(MonthW = mean(ArrDel15==1))
# Realizo un join en la columna Month del dataset flights con los pesos calculados en MonthWeigths
flights$Month <- inner_join(flights, MonthWeights, by = "Month")$MonthW


#### 3.2.2. DayofMonth ####
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDel15 = 1)
DayofMonthWeights <- flights %>% group_by(DayofMonth) %>% summarise(DayofMonthW = mean(ArrDel15==1))
# Realizo un join en la columna DayofMonth del dataset flights con los pesos calculados en DayofMonthWeights
flights$DayofMonth <- inner_join(flights, DayofMonthWeights, by = "DayofMonth")$DayofMonthW


#### 3.2.3. DayOfWeek ####
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDel15 = 1)
DayOfWeekWeights <- flights %>% group_by(DayOfWeek) %>% summarise(DayOfWeekW = mean(ArrDel15==1))
# Realizo un join en la columna DayOfWeek del dataset flights con los pesos calculados en 
# DayOfWeekWeights
flights$DayOfWeek <- inner_join(flights, DayOfWeekWeights, by = "DayOfWeek")$DayOfWeekW


#### 3.2.4. UniqueCarrier ####
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDel15 = 1)
UniqueCarrierWeights <- flights %>% group_by(UniqueCarrier) %>% summarise(UniqueCarrierW = mean(ArrDel15==1))
# Realizo un join en la columna UniqueCarrier del dataset flights con los pesos calculados en 
# UniqueCarrierWeights
flights$UniqueCarrier <- inner_join(flights, UniqueCarrierWeights, by = "UniqueCarrier")$UniqueCarrierW


#### 3.2.5. TailNum #### 
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDel15 = 1)
TailNumWeights <- flights %>% group_by(TailNum) %>% summarise(TailNumW = mean(ArrDel15==1))
# Realizo un join en la columna TailNum del dataset flights con los pesos calculados en TailNumWeights
flights$TailNum <- inner_join(flights, TailNumWeights, by = "TailNum")$TailNumW


#### 3.2.6. FlightNum ####
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDel15 = 1)
FlightNumWeights <- flights %>% group_by(FlightNum) %>% summarise(FlightNumW = mean(ArrDel15==1))
# Realizo un join en la columna FlightNum del dataset flights con los pesos calculados en FlightNumWeights
flights$FlightNum <- inner_join(flights, FlightNumWeights, by = "FlightNum")$FlightNumW


#### 3.2.7. OriginAirportSeqID ####
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDel15 = 1)
OriginAirportSeqIDWeights <- flights %>% group_by(OriginAirportSeqID) %>% summarise(OriginAirportSeqIDW = mean(ArrDel15==1))
# Realizo un join en la columna OriginAirportSeqID del dataset flights con los pesos calculados en 
# OriginAirportSeqIDWeights
flights$OriginAirportSeqID <- inner_join(flights, OriginAirportSeqIDWeights, by = "OriginAirportSeqID")$OriginAirportSeqIDW


#### 3.2.8. Origin ####
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDel15 = 1)
OriginWeights <- flights %>% group_by(Origin) %>% summarise(OriginW = mean(ArrDel15==1))
# Realizo un join en la columna Origin del dataset flights con los pesos calculados en OriginWeights
flights$Origin <- inner_join(flights, OriginWeights, by = "Origin")$OriginW


#### 3.2.9. OriginState ####
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDel15 = 1)
OriginStateWeights <- flights %>% group_by(OriginState) %>% summarise(OriginStateW = mean(ArrDel15==1))
# Realizo un join en la columna OriginState del dataset flights con los pesos calculados en OriginStateWeights
flights$OriginState <- inner_join(flights, OriginStateWeights, by = "OriginState")$OriginStateW


#### 3.2.10. DestAirportSeqID ####
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDel15 = 1)
DestAirportSeqIDWeights <- flights %>% group_by(DestAirportSeqID) %>% summarise(DestAirportSeqIDW = mean(ArrDel15==1))
# Realizo un join en la columna DestAirportSeqID del dataset flights con los pesos calculados en DestAirportSeqIDWeights
flights$DestAirportSeqID <- inner_join(flights, DestAirportSeqIDWeights, by = "DestAirportSeqID")$DestAirportSeqIDW


#### 3.2.11. Dest ####
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDel15 = 1)
DestWeights <- flights %>% group_by(Dest) %>% summarise(DestW = mean(ArrDel15==1))
# Realizo un join en la columna Dest del dataset flights con los pesos calculados en DestWeights
flights$Dest <- inner_join(flights, DestWeights, by = "Dest")$DestW


#### 3.2.12. DestState ####
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDel15 = 1)
DestStateWeights <- flights %>% group_by(DestState) %>% summarise(DestStateW = mean(ArrDel15==1))
# Realizo un join en la columna DestState del dataset flights con los pesos calculados en DestStateWeights
flights$DestState <- inner_join(flights, DestStateWeights, by = "DestState")$DestStateW


#### 3.2.13. DepTime ####
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDel15 = 1)
DepTimeWeights <- flights %>% group_by(DepTime) %>% summarise(DepTimeW = mean(ArrDel15==1))
# Realizo un join en la columna DepTime del dataset flights con los pesos calculados en DepTimeWeights
flights$DepTime <- inner_join(flights, DepTimeWeights, by = "DepTime")$DepTimeW


#### 3.2.14. DepDelay ####
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDel15 = 1)
DepDelayWeights <- flights %>% group_by(DepDelay) %>% summarise(DepDelayW = mean(ArrDel15==1))
# Realizo un join en la columna DepDelay del dataset flights con los pesos calculados en DepDelayWeights
flights$DepDelay <- inner_join(flights, DepDelayWeights, by = "DepDelay")$DepDelayW


#### 3.2.15. DepDelayMinutes ####
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDel15 = 1)
DepDelayMinutesWeights <- flights %>% group_by(DepDelayMinutes) %>% summarise(DepDelayMinutesW = mean(ArrDel15==1))
# Realizo un join en la columna DepDelayMinutes del dataset flights con los pesos calculados en DepDelayMinutesWeights
flights$DepDelayMinutes <- inner_join(flights, DepDelayMinutesWeights, by = "DepDelayMinutes")$DepDelayMinutesW


#### 3.2.16. DepDel15 ####
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDel15 = 1)
DepDel15Weights <- flights %>% group_by(DepDel15) %>% summarise(DepDel15W = mean(ArrDel15==1))
# Realizo un join en la columna DepDelayMinutes del dataset flights con los pesos calculados en DepDelayMinutesWeights
flights$DepDel15 <- inner_join(flights, DepDel15Weights, by = "DepDel15")$DepDel15W


#### 3.2.17. DepartureDelayGroups ####
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDel15 = 1)
DepartureDelayGroupsWeights <- flights %>% group_by(DepartureDelayGroups) %>% summarise(DepartureDelayGroupsW = mean(ArrDel15==1))
# Realizo un join en la columna DepartureDelayGroups del dataset flights con los pesos calculados en DepartureDelayGroupsWeights
flights$DepartureDelayGroups <- inner_join(flights, DepartureDelayGroupsWeights, by = "DepartureDelayGroups")$DepartureDelayGroupsW


#### 3.2.18. DepTimeBlk ####
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDel15 = 1)
DepTimeBlkWeights <- flights %>% group_by(DepTimeBlk) %>% summarise(DepTimeBlkW = mean(ArrDel15==1))
# Realizo un join en la columna DepTimeBlk del dataset flights con los pesos calculados en DepTimeBlkWeights
flights$DepTimeBlk <- inner_join(flights, DepTimeBlkWeights, by = "DepTimeBlk")$DepTimeBlkW


#### 3.2.19. ArrTime ####
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDel15 = 1)
ArrTimeWeights <- flights %>% group_by(ArrTime) %>% summarise(ArrTimeW = mean(ArrDel15==1))
# Realizo un join en la columna ArrTime del dataset flights con los pesos calculados en ArrTimeWeights
flights$ArrTime <- inner_join(flights, ArrTimeWeights, by = "ArrTime")$ArrTimeW


#### 3.2.20. ArrDelay ####
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDel15 = 1)
ArrDelayWeights <- flights %>% group_by(ArrDelay) %>% summarise(ArrDelayW = mean(ArrDel15==1))
# Realizo un join en la columna ArrDelay del dataset flights con los pesos calculados en ArrDelayWeights
flights$ArrDelay <- inner_join(flights, ArrDelayWeights, by = "ArrDelay")$ArrDelayW


#### 3.2.21. ArrDelayMinutes ####
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDel15 = 1)
ArrDelayMinutesWeights <- flights %>% group_by(ArrDelayMinutes) %>% summarise(ArrDelayMinutesW = mean(ArrDel15==1))
# Realizo un join en la columna ArrDelayMinutes del dataset flights con los pesos calculados en ArrDelayMinutesWeights
flights$ArrDelayMinutes <- inner_join(flights, ArrDelayMinutesWeights, by = "ArrDelayMinutes")$ArrDelayMinutesW

#### 3.2.22. ArrivalDelayGroups ####
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDel15 = 1)
ArrivalDelayGroupsWeights <- flights %>% group_by(ArrivalDelayGroups) %>% summarise(ArrivalDelayGroupsW = mean(ArrDel15==1))
# Realizo un join en la columna ArrivalDelayGroups del dataset flights con los pesos calculados en ArrivalDelayGroupsWeights
flights$ArrivalDelayGroups <- inner_join(flights, ArrivalDelayGroupsWeights, by = "ArrivalDelayGroups")$ArrivalDelayGroupsW


#### 3.2.23. ArrTimeBlk ####
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDel15 = 1)
ArrTimeBlkWeights <- flights %>% group_by(ArrTimeBlk) %>% summarise(ArrTimeBlkW = mean(ArrDel15==1))
# Realizo un join en la columna ArrTimeBlk del dataset flights con los pesos calculados en ArrTimeBlkWeights
flights$ArrTimeBlk <- inner_join(flights, ArrTimeBlkWeights, by = "ArrTimeBlk")$ArrTimeBlkW


#### 3.2.24. ActualElapsedTime ####
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDel15 = 1)
ActualElapsedTimeWeights <- flights %>% group_by(ActualElapsedTime) %>% summarise(ActualElapsedTimeW = mean(ArrDel15==1))
# Realizo un join en la columna ActualElapsedTime del dataset flights con los pesos calculados en ActualElapsedTimeWeights
flights$ActualElapsedTime <- inner_join(flights, ActualElapsedTimeWeights, by = "ActualElapsedTime")$ActualElapsedTimeW


#### 3.2.25. Distance ####
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDel15 = 1)
DistanceWeights <- flights %>% group_by(Distance) %>% summarise(DistanceW = mean(ArrDel15==1))
# Realizo un join en la columna Distance del dataset flights con los pesos calculados en DistanceWeights
flights$Distance <- inner_join(flights, DistanceWeights, by = "Distance")$DistanceW


#### 3.2.26. DistanceGroup ####
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDel15 = 1)
DistanceGroupWeights <- flights %>% group_by(DistanceGroup) %>% summarise(DistanceGroupW = mean(ArrDel15==1))
# Realizo un join en la columna DistanceGroup del dataset flights con los pesos calculados en DistanceGroupWeights
flights$DistanceGroup <- inner_join(flights, DistanceGroupWeights, by = "DistanceGroup")$DistanceGroupW


#### 3.2.27. CarrierDelay ####
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDel15 = 1)
CarrierDelayWeights <- flights %>% group_by(CarrierDelay) %>% summarise(CarrierDelayW = mean(ArrDel15==1))
# Realizo un join en la columna CarrierDelay del dataset flights con los pesos calculados en CarrierDelayWeights
flights$CarrierDelay <- inner_join(flights, CarrierDelayWeights, by = "CarrierDelay")$CarrierDelayW


#### 3.2.28. WeatherDelay ####
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDel15 = 1)
WeatherDelayWeights <- flights %>% group_by(WeatherDelay) %>% summarise(WeatherDelayW = mean(ArrDel15==1))
# Realizo un join en la columna WeatherDelay del dataset flights con los pesos calculados en WeatherDelayWeights
flights$WeatherDelay <- inner_join(flights, WeatherDelayWeights, by = "WeatherDelay")$WeatherDelayW


#### 3.2.29. NASDelay ####
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDel15 = 1)
NASDelayWeights <- flights %>% group_by(NASDelay) %>% summarise(NASDelayW = mean(ArrDel15==1))
# Realizo un join en la columna NASDelay del dataset flights con los pesos calculados en NASDelayWeights
flights$NASDelay <- inner_join(flights, NASDelayWeights, by = "NASDelay")$NASDelayW


#### 3.2.30. SecurityDelay ####
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDel15 = 1)
SecurityDelayWeights <- flights %>% group_by(SecurityDelay) %>% summarise(SecurityDelayW = mean(ArrDel15==1))
# Realizo un join en la columna SecurityDelay del dataset flights con los pesos calculados en SecurityDelayWeights
flights$SecurityDelay <- inner_join(flights, SecurityDelayWeights, by = "SecurityDelay")$SecurityDelayW


#### 3.2.31. LateAircraftDelay ####
# Calculo la media de los pesos de cada valor en función de los vuelos retrasados (ArrDel15 = 1)
LateAircraftDelayWeights <- flights %>% group_by(LateAircraftDelay) %>% summarise(LateAircraftDelayW = mean(ArrDel15==1))
# Realizo un join en la columna LateAircraftDelay del dataset flights con los pesos calculados en LateAircraftDelayWeights
flights$LateAircraftDelay <- inner_join(flights, LateAircraftDelayWeights, by = "LateAircraftDelay")$SLateAircraftDelayW
# *************************************************************************************************



# *************************************************************************************************
#### 2.3. Guardo el dataframe normalizado ####
# Guardo el dataset totalmente normalizado
flightsWeights <- flights
write.table(flightsWeights, file = "data/flightsWeightsClassi.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
# *************************************************************************************************



# *************************************************************************************************
##### 3.3. Bloque de modelización #####


# Predicción si un vuelo puede o no llegar con retraso #
# He escogido para este problema de clasificación, los algoritmos de Regresión Logística y Random Forest


##### 3.3.1. Modelo completo ####


# Establezco la semilla para que la misma muestra pueda reproducirse también en el futuro
set.seed(7) 
# Selecciono el 80% de los datos como muestra del total de las filas de los datos  
sampleClassification <- sample.int(n = nrow(flightsWeights), size = floor(.80*nrow(flightsWeights)), replace = F)
trainClassification <- flightsWeights[sampleClassification, ] # Conjunto de entrenamiento
testClassification  <- flightsWeights[-sampleClassification, ] # Conjunto de test

# Análisis de variables que influyen en qué un vuelo se retrase o no:
# Según se ha observado en el análisis de las gráficas en Tableau, en un primer vistazo, parece que
# las variables que más influyen en el que un vuelo esté o no retrasado son:
# * DepDelay/DepDelayMinutes
# * UniqueCarrier
# * Origin & Dest
# * Distance
# * DayOfWeek
# * DayofMonth
# * TailNum
# * DepTimeBlk/DepTime
# Voy a comprobar si esto es así y si hay más variables que influyen en que un vuelo se retrase o no.

# Para la realización del modelo completo, la finalidad es ver, que características del modelo tienen
# más importancia.


##### 3.3.1.1. Regresión Logística #####

# Realizo un modelo con todas las características con Regresión Logística, para visualizar las
# características que más influyen en el modelo:
modLRClassiComplete = glm(ArrDel15~., family=binomial(link='logit'), data = trainClassification)
summary(modLRClassiComplete)
# Voy a ver la importancia de cada característica en el modelo:
varImpModLRClassiComplete <- varImp(modLRClassiComplete)
overall <- as.double(varImpModLRClassiComplete$Overall)
names <- rownames(varImpModLRClassiComplete)
df <- data.frame(names)
df$overall <- overall
arrange(df, desc(df$overall))
ggplot(df, aes(x = df$names, y = df$overall)) + geom_col(fill="blue") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Names", y = "Overall", title = "Importance of features using Logistic Regression")
# Parece que la característica que más influye para que un vuelo se retrase o no según el modelo
# de Regresión Logística es ArrDelay
# Tiene sentido por que es justo la característica que contiene retraso en la llegada, por tanto, se 
# debe de prescindir de las variables que dan el retraso de la llegada de alguna manera para la 
# realización del modelo.

##### 3.3.1.2. Random Forest #####
# Comparo este resultado realizando un modelo de todas las características con Random Forest:
modRFClassiComplete <- randomForest(ArrDel15~., data = trainClassification, ntree=25)
# Importancia de las características:
importanceModRFClassiComplete <- importance(modRFClassiComplete)
meanDecreaseGini <- as.double(importanceModRFClassiComplete)
names <- rownames(importanceModRFClassiComplete)
df <- data.frame(names)
df$meanDecreaseGini <- meanDecreaseGini
arrange(df, desc(df$meanDecreaseGini))
ggplot(df, aes(x = df$names, y = df$meanDecreaseGini)) + geom_col(fill="blue") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Names", y = "MeanDecreaseGini", title = "Importance of features using Random Forest")
# Realizando un modelo Random Forest con todas las características, se visualizan mejor aquellas
# que tienen más importancia en el modelo.
# Por tanto, de momento lo que sí se puede indicar, es que para la realización del modelo es necesario
# prescindir de:
# * Las características que nos aportan información de si un vuelo se retrasa en la llegada: ArrDelay,
#   ArrivalDelayGroups, ArrDelayMinutes, ArrTime, ActualElapsedTime y ArrTimeBlk.
# * Las características que indican el grupo de retraso: CarrierDelay, LateAircraftDelay, NASDelay,
#   WeatherDelay y SecurityDelay; puesto que ya existe un DepDelay y un DepDelayMinutes que nos indica
#   si sale con retraso el vuelo...
# Con esto, voy realizar de primeras un modelo con el resto de las características:
# DepDel15+DepartureDelayGroups+DepDelayMinutes+DepDelay+DistanceGroup+DayofMonth+DestState+DepTime+
# UniqueCarrier+DepTimeBlk+Distance+OriginState+FlightNum+Month+TailNum+Dest+DestAirportSeqID+
# OriginAirportSeqID+Origin+DayOfWeek


##### 3.3.2. Modelo 1 #####

# ClassificationModel1: DepDel15+DepartureDelayGroups+DepDelayMinutes+DepDelay+DistanceGroup+
#                       DayofMonth+DestState+DepTime+UniqueCarrier+DepTimeBlk+Distance+OriginState+
#                       FlightNum+Month+TailNum+Dest+DestAirportSeqID+OriginAirportSeqID+Origin+DayOfWeek


##### 3.3.2.1. Regresión Logística #####
# Realizo el modelo de nuevo de Regresión Logística con todas las características seleccionadas
modLRClassi1 <- glm(ArrDel15~DepDel15+DepartureDelayGroups+DepDelayMinutes+DepDelay+DistanceGroup+
                      DayofMonth+DestState+DepTime+UniqueCarrier+DepTimeBlk+Distance+OriginState+
                      FlightNum+Month+TailNum+Dest+DestAirportSeqID+OriginAirportSeqID+Origin+DayOfWeek, 
                    family=binomial(link='logit'), data = trainClassification)
summary(modLRClassi1)
# Del modelo se obtiene que ArrDel15 es significativo a más del 99,9% junto con el resto de 
# características a excepción de DestState, OriginAirportSeqID y Origin
# Importancia de las características seleccionadas:
varImpModLRClassi1 <- varImp(modLRClassi1)
overall <- as.double(varImpModLRClassi1$Overall)
names <- rownames(varImpModLRClassi1)
df <- data.frame(names)
df$overall <- overall
arrange(df, desc(df$overall))
ggplot(df, aes(x = df$names, y = df$overall)) + geom_col(fill="blue") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Names", y = "Overall", title = "Importance of features using Logistic Regression")
# Para el Modelo 2 voy a usar: DayofMonth+DepDel15+FlightNum+UniqueCarrier+TailNum+DayOfWeek+
#                              OriginState+DepTimeBlk+DepTime/+Dest
# Predicción
predictionLRClassi1 <- round(predict(modLRClassi1, newdata = testClassification, type = "response"))
# Matriz de confusión
confusionMatrix(data = as.factor(predictionLRClassi1), reference = testClassification$ArrDel15)
#           Reference
# Prediction      0      1
#          0 121406  11917
#          1   3323  32427
# Precisión/Recall
precisionLRClassi1 <- posPredValue(as.factor(predictionLRClassi1), testClassification$ArrDel15, positive="1")
recallLRClassi1 <- sensitivity(as.factor(predictionLRClassi1), testClassification$ArrDel15, positive="1")
c(precisionLRClassi1,recallLRClassi1)
# [1] 0.9070490 0.7312601
# Conclusión: De todos los vuelos que tienen retraso, se clasifican correctamente como retraso el 73%.
# Almaceno los valores para compararlos con el resto:
name <- "LogisticRegresionClassificationModel1"
metricsPreRec <- data.frame(name, precisionLRClassi1,recallLRClassi1)
colnames(metricsPreRec) <- c("Model","Precision","Recall")

##### 3.3.2.2. Random Forest #####
# Realizo una primera vuelta del modelo utilizando Random Forest con ntree=25, 50 y 75 para detectar
# y evitar el punto donde el modelo comience a realizar overfiting, para ntree=509 el modelo comienza
# a predecir peor, se produce overfiting, ajusto el modelo con ntree=25
modRFClassi1 <- randomForest(ArrDel15~DepDel15+DepartureDelayGroups+DepDelayMinutes+DepDelay+DistanceGroup+
                               DayofMonth+DestState+DepTime+UniqueCarrier+DepTimeBlk+Distance+OriginState+
                               FlightNum+Month+TailNum+Dest+DestAirportSeqID+OriginAirportSeqID+Origin+DayOfWeek, 
                             data = trainClassification, ntree=25)
# Importancia de las características seleccionadas:
importanceModRFClassi1 <- importance(modRFClassi1)
meanDecreaseGini <- as.double(importanceModRFClassi1)
names <- rownames(importanceModRFClassi1)
df <- data.frame(names)
df$meanDecreaseGini <- meanDecreaseGini
arrange(df, desc(df$meanDecreaseGini))
ggplot(df, aes(x = df$names, y = df$meanDecreaseGini)) + geom_col(fill="blue") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Names", y = "MeanDecreaseGini", title = "Importance of features using Random Forest")
# Para el Modelo 3 voy a usar: DepDelayMinutes+TailNum+FlightNum+DepTime+DayofMonth+Distance+DayOfWeek+Dest+
#                              Origin
predictionRFClassi1 <- predict(modRFClassi1, newdata = testClassification)
# Matriz de confusión
confusionMatrix(data = as.factor(predictionRFClassi1), reference = testClassification$ArrDel15)
#           Reference
# Prediction      0      1
#          0 120774  11119
#          1   3955  33225
# Precisión/Recall 
precisionRFClassi1 <- posPredValue(as.factor(predictionRFClassi1), testClassification$ArrDel15, positive="1")
recallRFClassi1 <- sensitivity(as.factor(predictionRFClassi1), testClassification$ArrDel15, positive="1")
c(precisionRFClassi1,recallRFClassi1)
# [1] 0.8936256 0.7492558
# Conclusión: De todos los vuelos que tienen retraso, se clasifican correctamente como retraso casi el 75%.
# Guardo los resultados en el metricsPreRec
name <- "RandomForestClassificationModel1"
df <- data.frame(name,precisionRFClassi1,recallRFClassi1)
colnames(df) <- c("Model","Precision","Recall")
metricsPreRec <- rbind(metricsPreRec,df)
# Conclusión: Para las características del Modelo 1, la predicción de Random Forest es 1,7% mejor.


##### 3.3.3. Modelo 2 #####

# ClassificationModel2: DayofMonth+DepDel15+FlightNum+UniqueCarrier+TailNum+DayOfWeek+OriginState+
#                       DepTimeBlk

##### 3.3.3.1. Regresión Logística #####
modLRClassi2 <- glm(ArrDel15~DayofMonth+DepDel15+FlightNum+UniqueCarrier+TailNum+DayOfWeek+OriginState+
                      DepTimeBlk, family=binomial(link='logit'), data = trainClassification)
summary(modLRClassi2)
# Predicción
predictionLRClassi2 <- round(predict(modLRClassi2, newdata = testClassification, type = "response"))
# Matriz de confusión
confusionMatrix(data = as.factor(predictionLRClassi2), reference = testClassification$ArrDel15)
#           Reference
# Prediction     0     1
#          0 116990   9287
#          1   7739  35057
# Precisión/Recall 
precisionLRClassi2 <- posPredValue(as.factor(predictionLRClassi2), testClassification$ArrDel15, positive="1")
recallLRClassi2 <- sensitivity(as.factor(predictionLRClassi2), testClassification$ArrDel15, positive="1")
c(precisionLRClassi2,recallLRClassi2)
# [1] 0.8191653 0.7905692
# Conclusión: De todos los vuelos que tienen retraso, se clasifican correctamente como retraso el 79%
name <- "LogisticRegresionClassificationModel2"
df <- data.frame(name,precisionLRClassi2,recallLRClassi2)
colnames(df) <- c("Model","Precision","Recall")
metricsPreRec <- rbind(metricsPreRec,df)

##### 3.3.3.2. Random Forest #####
modRFClassi2 <- randomForest(ArrDel15~DayofMonth+DepDel15+FlightNum+UniqueCarrier+TailNum+DayOfWeek+OriginState+
                               DepTimeBlk, data = trainClassification, ntree=25)
# Predicción
predictionRFClassi2 <- predict(modRFClassi2, newdata = testClassification)
# Matriz de confusión
confusionMatrix(data = as.factor(predictionRFClassi2), reference = testClassification$ArrDel15)
#           Reference
# Prediction     0     1
#          0 117093   9324
#          1   7636  35020
# Precisión/Recall 
precisionRFClassi2 <- posPredValue(as.factor(predictionRFClassi2), testClassification$ArrDel15, positive="1")
recallRFClassi2 <- sensitivity(as.factor(predictionRFClassi2), testClassification$ArrDel15, positive="1")
c(precisionRFClassi2,recallRFClassi2)
# [1] 0.8209865 0.7897348
# Conclusión: De todos los vuelos que tienen retraso, se clasifican correctamente como retraso el 78%
name <- "RandomForestClassificationModel2"
df <- data.frame(name,precisionRFClassi2,recallRFClassi2)
colnames(df) <- c("Model","Precision","Recall")
metricsPreRec <- rbind(metricsPreRec,df)
# Conclusión: Para las características del Modelo 1, la predicción de Regresión Logística es 0,7% mejor.


##### 3.3.4. Modelo 3 #####

# ClassificationModel3: DepDelayMinutes+TailNum+FlightNum+DepTime+DayofMonth+Distance+DayOfWeek+Dest+
#                       Origin 

##### 3.3.4.1. Regresión Logística #####
modLRClassi3 <- glm(ArrDel15~DepDelayMinutes+TailNum+FlightNum+DepTime+DayofMonth+Distance+DayOfWeek+
                      Dest+Origin, family=binomial(link='logit'), data = trainClassification)
summary(modLRClassi3)
# Predicción
predictionLRClassi3 <- round(predict(modLRClassi3, newdata = testClassification, type = "response"))
# Matriz de confusión
confusionMatrix(data = as.factor(predictionLRClassi3), reference = testClassification$ArrDel15)
#           Reference
# Prediction     0     1
#          0 120822  11372
#          1   3907  32972
# Precisión/Recall 
precisionLRClassi3 <- posPredValue(as.factor(predictionLRClassi3), testClassification$ArrDel15, positive="1")
recallLRClassi3 <- sensitivity(as.factor(predictionLRClassi3), testClassification$ArrDel15, positive="1")
c(precisionLRClassi3,recallLRClassi3)
# [1] 0.8940589 0.7435504
name <- "LogisticRegresionClassificationModel3"
df <- data.frame(name,precisionLRClassi3,recallLRClassi3)
colnames(df) <- c("Model","Precision","Recall")
metricsPreRec <- rbind(metricsPreRec,df)

##### 3.3.4.2. Random Forest #####
modRFClassi3 <- randomForest(ArrDel15~DepDelayMinutes+TailNum+FlightNum+DepTime+DayofMonth+Distance+DayOfWeek+
                               Dest+Origin, data = trainClassification, ntree=25)
# Predicción
predictionRFClassi3 <- predict(modRFClassi3, newdata = testClassification)
# Matriz de confusión
confusionMatrix(data = as.factor(predictionRFClassi3), reference = testClassification$ArrDel15)
#           Reference
# Prediction     0     1
#          0 120784  11305
#          1   3945  33039
# Precisión/Recall 
precisionRFClassi3 <- posPredValue(as.factor(predictionRFClassi3), testClassification$ArrDel15, positive="1")
recallRFClassi3 <- sensitivity(as.factor(predictionRFClassi3), testClassification$ArrDel15, positive="1")
c(precisionRFClassi3,recallRFClassi3)
# [1] 0.8933323 0.7450613
name <- "RandomForestClassificationModel3"
df <- data.frame(name,precisionRFClassi3,recallRFClassi3)
colnames(df) <- c("Model","Precision","Recall")
metricsPreRec <- rbind(metricsPreRec,df)

# Resultados de las métricas
metricsPreRec

# El mejor precision/recall lo tiene la Regresión Logística y Random Forest del modelo 2, que de todos los vuelos 
# que tienen retraso, se clasifican correctamente como retraso el 79% de los vuelos.
varImp(modLRClassi2)
# Conclusión: Las características que más influyen en la predicción son: 
#             - El retraso del vuelo de salida.
#             - El día del mes.
#             - El estado del aeropuerto de Origen.
#             - El número de vuelo.
#             - La aeronave.
# *************************************************************************************************



# *************************************************************************************************
#### 3.4. Guardo la relación de las variables categóricas y sus pesos ####
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
write.table(DepTimeWeights, file = "CategoriesWeights/DepTimeWeights.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(DepDelayWeights, file = "CategoriesWeights/DepDelayWeights.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(DepDelayMinutesWeights, file = "CategoriesWeights/DepDelayMinutesWeights.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(DepDel15Weights, file = "CategoriesWeights/DepDel15Weights.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(DepartureDelayGroupsWeights, file = "CategoriesWeights/DepartureDelayGroupsWeights.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(DepTimeBlkWeights, file = "CategoriesWeights/DepTimeBlkWeights.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(ArrTimeWeights, file = "CategoriesWeights/ArrTimeWeights.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(ArrDelayWeights, file = "CategoriesWeights/ArrDelayWeights.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(ArrDelayMinutesWeights, file = "CategoriesWeights/ArrDelayMinutesWeights.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(ArrivalDelayGroupsWeights, file = "CategoriesWeights/ArrivalDelayGroupsWeights.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(ArrTimeBlkWeights, file = "CategoriesWeights/ArrTimeBlkWeights.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(ActualElapsedTimeWeights, file = "CategoriesWeights/ActualElapsedTimeWeights.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(DistanceWeights, file = "CategoriesWeights/DistanceWeights.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(DistanceGroupWeights, file = "CategoriesWeights/DistanceGroupWeights.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(CarrierDelayWeights, file = "CategoriesWeights/CarrierDelayWeights.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(WeatherDelayWeights, file = "CategoriesWeights/WeatherDelayWeights.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(NASDelayWeights, file = "CategoriesWeights/NASDelayWeights.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(SecurityDelayWeights, file = "CategoriesWeights/SecurityDelayWeights.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
write.table(LateAircraftDelayWeights, file = "CategoriesWeights/LateAircraftDelayWeights.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
# *************************************************************************************************



# *************************************************************************************************
#### 3.5. Elimino variables categóricas y asignaciones innecesarias ####
rm(ActualElapsedTimeWeights)
rm(ArrDelayMinutesWeights)
rm(ArrDelayWeights)
rm(ArrivalDelayGroupsWeights)
rm(ArrTimeBlkWeights)
rm(ArrTimeWeights)
rm(CarrierDelayWeights)
rm(DayofMonthWeights)
rm(DayOfWeekWeights)
rm(DepartureDelayGroupsWeights)
rm(DepDel15Weights)
rm(DepDelayMinutesWeights)
rm(DepDelayWeights)
rm(DepTimeBlkWeights)
rm(DepTimeWeights)
rm(DestAirportSeqIDWeights)
rm(DestStateWeights)
rm(DestWeights)
rm(DistanceGroupWeights)
rm(DistanceWeights)
rm(FlightNumWeights)
rm(LateAircraftDelayWeights)
rm(MonthWeights)
rm(NASDelayWeights)
rm(OriginAirportSeqIDWeights)
rm(OriginStateWeights)
rm(OriginWeights)
rm(SecurityDelayWeights)
rm(TailNumWeights)
rm(UniqueCarrierWeights)
rm(WeatherDelayWeights)
rm(df)
rm(name)
rm(names)
rm(overall)
rm(meanDecreaseGini)
# *************************************************************************************************



# *************************************************************************************************
#### 3.6. Guardo el dataframe resultante de la normalización ####
# Guardo el dataset totalmente normalizado
flightsWeights <- flights
write.table(flightsWeights, file = "data/flightsWeights.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
# *************************************************************************************************


