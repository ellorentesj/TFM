#### TFM: ANÁLISIS Y PREDICCIÓN DEL RESTRASO EN LOS VUELOS ####



#### 3. DATOS DE ENTRADA: Modelos y su comparación ####

 

# *************************************************************************************************
##### 3.1. Bloque de carga de librerias #####
# Predict
if(!require("caret")){
  install.packages("caret")
  library(caret)
}
# Decision Tree
if(!require("rpart")){
  install.packages("rpart")
  library(rpart)
}
# SVM
if(!require("e1071")){
  install.packages("e1071")
  library(e1071)
}
# Random Forest
if(!require("randomForest")){
  install.packages("randomForest")
  library(randomForest)
}
# *************************************************************************************************



# *************************************************************************************************
##### 3.2. Bloque de modelización #####


##### 3.2.1. Predicción: Si un vuelo puede o no llegar con retraso ####

str(standardFlights)

# https://stackoverflow.com/questions/17200114/how-to-split-data-into-training-testing-sets-using-sample-function
# Establezco la semilla para que la misma muestra pueda reproducirse también en el futuro
set.seed(7) 
# Selecciono el 80% de los datos como muestra del total de las filas de los datos  
sample <- sample.int(n = nrow(standardFlights), size = floor(.80*nrow(standardFlights)), replace = F)
train <- standardFlights[sample, ] # Conjunto de entrenamiento
test  <- standardFlights[-sample, ] # Conjunto de test

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
# * DepTimeBlk
# Voy a comprobar si esto es así y si hay más variables que influyen en que un vuelo se retrase o no.


##### 3.2.1.1. Modelo Completo #####

### Regresión Logística ###
# Realizo un modelo con todas las características con Regresión Logística:
modLRComplete = glm(ArrDel15~., family=binomial(link='logit'), data = train)
# 
plot(modLRComplete$residuals)
plot(modLRComplete$coefficients)
# Predicción
prediccionLRComplete <- round(predict(modLRComplete, newdata = test, type = "response"))
# Matriz de confusión
confusionMatrix(data = as.factor(prediccionLRComplete), reference = test$ArrDel15)
# Precisión/Recall 
precisionLRComplete <- posPredValue(as.factor(prediccionLRComplete), test$ArrDel15, positive="1")
recallLRComplete <- sensitivity(as.factor(precisionLRComplete), test$ArrDel15, positive="1")
c(precisionLRComplete,recallLRComplete)
# [1] 1 1 --> Evidentemente al introducir todas las características al modelo, precisión/recall
# es de 1.
# Voy a ver la importancia de cada variable en el modelo, graficándola
dfmodLRComplete <- varImp(modLRComplete)
overall <- as.double(dfmodLRComplete$Overall)
names <- rownames(dfmodLRComplete)
df <- data.frame(names)
df$overall <- overall
arrange(df, desc(df$overall))
ggplot(df, aes(x = df$names, y = df$overall)) + geom_col(fill="blue") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Names", y = "Overall", title = "Importance of features using Logistic Regression")
# Parece que las características que más influyen para que un vuelo se retrase o no son:
# * ArrDelay
# * DepDelayMinutes
# * NASDelay
# * ArrivalDelayGroupsWeights
# * DepDelay...
# Tiene sentido que la característica que más importancia tiene sea ArrDelay por que es justo la 
# característica que contiene el tiempo de retraso, por tanto, se debe de prescindir de las variables 
# que dan el retraso de alguna manera para la realización del modelo.

### Random Forest ###
# Comparo este resultado realizando un modelo de todas las características con Random Forest:
modRFComplete <- randomForest(ArrDel15~., data = train, ntree=100)
# Predicción
prediccionRFComplete <- predict(modRFComplete, newdata = test)
# Matriz de confusión
confusionMatrix(data = as.factor(prediccionRFComplete), reference = test$ArrDel15)
# Precisión/Recall 
precisionRFComplete <- posPredValue(as.factor(prediccionRFComplete), test$ArrDel15, positive="1")
recallRFComplete <- sensitivity(as.factor(prediccionRFComplete), test$ArrDel15, positive="1")
c(precisionRFComplete,recallRFComplete)
# Voy a ver la importancia de cada una de las características gráficamente:
dfmodRFComplete <- importance(modRFComplete)
meanDecreaseGini <- as.double(dfmodRFComplete)
names <- rownames(dfmodRFComplete)
df <- data.frame(names)
df$meanDecreaseGini <- meanDecreaseGini
arrange(df, desc(df$meanDecreaseGini))
ggplot(df, aes(x = df$names, y = df$meanDecreaseGini)) + geom_col(fill="blue") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Names", y = "MeanDecreaseGini", title = "Importance of features using Random Forest")
# Las características son prácticamentes las mismas que en la Regresión Logística.
# Por tanto, de momento lo que sí se puede indicar, es que para la realización del modelo es necesario
# prescindir de:
# * Las características que nos aportan información de si un vuelo se retrasa en la llegada: ArrTime,
#   ArrDelay, ArrDelayMinutes, ArrDelayGroupsWeights, ArrTimeBlkWeights y ActualElapsedTime.
# * Las características que indican el grupo de retraso: CarrierDelay, LateAircraftDelay, NASDelay,
#   WeatherDelay, SecurityDelay; puesto que ya existe un DepDelay y un DepDelayMinutes que nos indica
#   si sale con retraso el vuelo...


##### 3.2.1.2. Segundo Modelo #####

# Descarto las caracterísiticas indicadas en el análisis del punto 4.2.1.1.:

### Regresión Logística ###
# Realizo el modelo de nuevo de Regresión Logística con todas las características seleccionadas
modLR2 <- glm(ArrDel15~DepTime+DepDelay+DepDelayMinutes+DepDel15+Distance+MonthWeights+DayofMonthWeights+
                DayOfWeekWeights+UniqueCarrierWeights+TailNumWeights+FlightNumWeights+OriginAirportSeqIDWeights+
                OriginWeights+OriginStateWeights+DestAirportSeqIDWeights+DestWeights+DestStateWeights+
                DepartureDelayGroupsWeights+DepTimeBlkWeights+DistanceGroupWeights, family=binomial(link='logit'), 
              data = train)
#step(modLR2,direction="both",trace=1)
summary(modLR2)
# Del modelo se obtiene que ArrDel15 es significativo a más del 99,9% junto con: DepTime, DepDelay, 
# DepDelayMinutes, DepDel15, DayofMonthWeights, UniqueCarrierWeights, TailNumWeights, FlightNumWeights,
# OriginAirportSeqIDWeights, OriginWeights, OriginStateWeights, DepartureDelayGroupsWeights, 
# DistanceGroupWeights
# Predicción
prediccionLR2 <- round(predict(modLR2, newdata = test, type = "response"))
# Matriz de confusión
confusionMatrix(data = as.numeric(prediccionLR2>0.5), reference = test$ArrDel15)
#           Reference
# Prediction     0     1
#          0 21498  2030
#          1   654  5818
# Precisión/Recall 
precisionLR2 <- posPredValue(as.factor(prediccionLR2), test$ArrDel15, positive="1")
recallLR2 <- sensitivity(as.factor(prediccionLR2), test$ArrDel15, positive="1")
c(precisionLR2,recallLR2)
# [1] 0.8989493 0.7413354
# Conclusión: De todos los vuelos que tienen retraso, se clasifican correctamente como retraso el 74%.
# Reresento la importancia de las variables del modelo
dfmodLR2 <- varImp(modLR2)
overall <- as.double(dfmodLR2$Overall)
names <- rownames(dfmodLR2)
df <- data.frame(names)
df$overall <- overall
arrange(df, desc(df$overall))
ggplot(df, aes(x = df$names, y = df$overall)) + geom_col(fill="blue") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Names", y = "Overall", title = "Importance of features using Logistic Regression")
# Según este segundo modelo, las características más importantes son:
#                          names     overall
# 1            DayofMonthWeights 19.8947734
# 2              DepDelayMinutes 12.9569283
# 3           OriginStateWeights 12.7732226
# Seguidas con menos importancia de:
# 4             FlightNumWeights 11.7820681
# 5         UniqueCarrierWeights  9.1692181
# 6                     DepDelay  8.1162689
# 7               TailNumWeights  7.5185906
# 8                      DepTime  7.4907681
# 9             DayOfWeekWeights  6.4818470
# 10 DepartureDelayGroupsWeights  5.0445371

### Random Forest ###
# Vuelvo a comparar este resultado con un modelo Random Forest:
modRF2 <- randomForest(ArrDel15~DepTime+DepDelay+DepDelayMinutes+DepDel15+Distance+MonthWeights+
                         DayofMonthWeights+DayOfWeekWeights+UniqueCarrierWeights+TailNumWeights+
                         FlightNumWeights+OriginAirportSeqIDWeights+OriginWeights+OriginStateWeights+
                         DestAirportSeqIDWeights+DestWeights+DestStateWeights+DepartureDelayGroupsWeights+
                         DepTimeBlkWeights+DistanceGroupWeights, data = train, ntree=100)
# Predicción
prediccionRF2 <- predict(modRF2, newdata = test)
# Matriz de confusión
confusionMatrix(data = as.factor(prediccionRF2), reference = test$ArrDel15)
#           Reference
# Prediction     0     1
#          0 21528  2011
#          1   624  5837
# A primera vista parece que Random Forest realiza una mejor predicción de los vuelos retrasados,
# voy a ver precisión/recall
# Precisión/Recall 
precisionRF2 <- posPredValue(as.factor(prediccionRF2), test$ArrDel15, positive="1")
recallRF2 <- sensitivity(as.factor(prediccionRF2), test$ArrDel15, positive="1")
c(precisionRF2,recallRF2)
# [1] 0.9034205 0.7437564
# Conclusión: Clasifica unas décimas mejor que Regresión Logística
# Voy a ver la importancia de cada una de las características gráficamente:
dfmodRF2 <- importance(modRF2)
meanDecreaseGini <- as.double(dfmodRF2)
names <- rownames(dfmodRF2)
df <- data.frame(names)
df$meanDecreaseGini <- meanDecreaseGini
arrange(df, desc(df$meanDecreaseGini))
ggplot(df, aes(x = df$names, y = df$meanDecreaseGini)) + geom_col(fill="blue") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Names", y = "MeanDecreaseGini", title = "Importance of features using Random Forest")
#                          names meanDecreaseGini
# 1                     DepDelay       10030.2646
# 2              DepDelayMinutes        9071.5796
# 3                     DepDel15        5606.2858
# 4  DepartureDelayGroupsWeights        5174.6921
# 5               TailNumWeights        1790.4401
# 6                      DepTime        1733.3430
# 7             FlightNumWeights        1596.2568
# 8            DayofMonthWeights        1501.8431
# 9                     Distance        1396.5430
# 10               OriginWeights         935.8618
# Conclusiones:
# * Ambos modelos sólo tienen en común 5 características. Se puede realizar otra vuelta más aún,
#   prescindiendo de variables que contienen el retraso en la salida como: DepDelay, DepDel15, 
#   DepartureDelayGroupsWeights. 
# * Conservo DepDelayMinutes por tener relativa importancia en ambos modelos.
# * También voy a quitar del modelo la característica DepTimeBlkWeights, puesto que la información de 
#   la hora de salida ya la tiene DepTime y tiene más importancia.
# * De momento voy a conservar para al siguiente modelo:
#   - TailNumWeights, DepTime, FlightNumWeights, DayofMonthWeights por haber salido como variables con
#     importancia en los dos modelos y con,
#   - OriginStateWeights, OriginWeights, Distance, UniqueCarrierWeights, DayOfWeekWeights por haber
#     salido con importancia en uno de los dos modelos.


##### 3.2.1.3. Tercer Modelo #####

# Realizo el modelo con las caracterísiticas indicadas en las conclusiones del punto 4.2.1.2.:

### Regresión Logística ###
modLR3 <- glm(ArrDel15~DepTime+DepDelayMinutes+Distance+DayofMonthWeights+DayOfWeekWeights+
                UniqueCarrierWeights+TailNumWeights+FlightNumWeights+OriginWeights+OriginStateWeights, 
              family=binomial(link='logit'), data = train)
summary(modLR3)
# Del modelo se obtiene que tanto Distance como Origin no son significativos, voy a ver como 
# es la predicción
# Predicción
prediccionLR3 <- round(predict(modLR3, newdata = test, type = "response"))
# Matriz de confusión
confusionMatrix(data = as.numeric(prediccionLR3>0.5), reference = test$ArrDel15)
#           Reference
# Prediction     0     1
#          0 21481  2020
#          1   671  5828
# Parece que predice un poco mejor los vuelos que siendo retrasados se van a retrasar pero no
# predice mejor de los no retrasados los que no se retrasan. 
# Precisión/Recall 
precisionLR3 <- posPredValue(as.factor(prediccionLR3), test$ArrDel15, positive="1")
recallLR3 <- sensitivity(as.factor(prediccionLR3), test$ArrDel15, positive="1")
c(precisionLR3,recallLR3)
# [1] 0.8967533 0.7426096
# [1] 0.8989493 0.7413354 --> modLR2
# [1] 0.9034205 0.7437564 --> modRF2
# Conclusión: Su recall es un poco más preciso, pero no más que el de Random Forest del segundo modelo, 
# y el predicción es un poco peor que en el segundo modelo.
# Reresento la importancia de las variables del modelo
dfmodLR3 <- varImp(modLR3)
overall <- as.double(dfmodLR3$Overall)
names <- rownames(dfmodLR3)
df <- data.frame(names)
df$overall <- overall
arrange(df, desc(df$overall))
ggplot(df, aes(x = df$names, y = df$overall)) + geom_col(fill="blue") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Names", y = "Overall", title = "Importance of features using Logistic Regression")
# Según este segundo modelo, las características más importantes son:
#                   names     overall
# 1       DepDelayMinutes 144.3243223
# 2     DayofMonthWeights  20.6466199
# 3    OriginStateWeights  13.1251287
# 4      FlightNumWeights  12.9707646
# 5               DepTime  12.8784095
# 6        TailNumWeights   8.4410150
# 7  UniqueCarrierWeights   8.1328024
# 8      DayOfWeekWeights   6.9675022
# 9              Distance   0.3884345
# 10        OriginWeights   0.2540165

### Random Forest ###
modRF3 <- randomForest(ArrDel15~DepTime+DepDelayMinutes+Distance+DayofMonthWeights+DayOfWeekWeights+
                         UniqueCarrierWeights+TailNumWeights+FlightNumWeights+OriginWeights+OriginStateWeights, 
                       data = train, ntree=100)
# Predicción
prediccionRF3 <- predict(modRF3, newdata = test)
# Matriz de confusión
confusionMatrix(data = as.factor(prediccionRF3), reference = test$ArrDel15)
#           Reference
# Prediction     0     1
#          0 21528  2011
#          1   624  5837
# A primera vista parece que Random Forest realiza una mejor predicción de los vuelos retrasados,
# voy a ver precisión/recall
# Precisión/Recall 
precisionRF3 <- posPredValue(as.factor(prediccionRF3), test$ArrDel15, positive="1")
recallRF3 <- sensitivity(as.factor(prediccionRF3), test$ArrDel15, positive="1")
c(precisionRF3,recallRF3)
# [1] 0.9034205 0.7437564
# Conclusión: Clasifica unas décimas mejor que Regresión Logística
# Voy a ver la importancia de cada una de las características gráficamente:
dfmodRF3 <- importance(modRF3)
meanDecreaseGini <- as.double(dfmodRF3)
names <- rownames(dfmodRF3)
df <- data.frame(names)
df$meanDecreaseGini <- meanDecreaseGini
arrange(df, desc(df$meanDecreaseGini))
ggplot(df, aes(x = df$names, y = df$meanDecreaseGini)) + geom_col(fill="blue") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Names", y = "MeanDecreaseGini", title = "Importance of features using Random Forest")

## XGBoost ##
modXGB3 <- train(ArrDel15~DepTime+DepDelayMinutes+Distance+DayofMonthWeights+DayOfWeekWeights+
                       UniqueCarrierWeights+TailNumWeights+FlightNumWeights+OriginWeights, data = train, 
                     method = "xgbLinear", verbose = FALSE)
prediccionXGB3 <- predict(modXGB3, newdata = test)
confusionMatrix(data = as.factor(prediccionXGB3), reference = test$ArrDel15)
precisionXGB3 <- posPredValue(as.factor(prediccionXGB3), test$ArrDel15, positive="1")
recallXGB3 <- sensitivity(as.factor(prediccionXGB3), test$ArrDel15, positive="1")
c(precisionXGB3,recallXGB3)



modRF4 <- randomForest(ArrDel15~DepTime+DepDelayMinutes+Distance+DayofMonthWeights+DayOfWeekWeights+
                         TailNumWeights+OriginStateWeights, data = train, ntree=100)
prediccionRF4 <- predict(modRF4, newdata = test)
confusionMatrix(data = as.factor(prediccionRF4), reference = test$ArrDel15)
precisionRF4 <- posPredValue(as.factor(prediccionRF4), test$ArrDel15, positive="1")
recallRF4 <- sensitivity(as.factor(prediccionRF4), test$ArrDel15, positive="1")
c(precisionRF4,recallRF4)


##### 3.2.2. Predicción: Tiempo de retraso cuando un vuelo está retrasado ####
# Genero un nuevo dataframe únicamente con los vuelos retrasados
flightsDelay <- flights %>% filter(ArrDel15==1)

set.seed(7) 
# Selecciono el 80% de los datos como muestra del total de las filas de los datos  
sample2 <- sample.int(n = nrow(flightsDelay), size = floor(.80*nrow(flightsDelay)), replace = F)
train2 <- flightsDelay[sample2, ] # Conjunto de entrenamiento
test2  <- flightsDelay[-sample2, ] # Conjunto de test

# Guardar los resultados incluyendo los outliers, eliminar los outliers y volver a repetir los entrenamientos guardando los resultados.
quantile(flightsDelay$ArrDelayMinutes, .95)
# outliers <- flights %>% 
#   filter(ArrDelayMinutes>180) %>% nrow()
# Se eliminan los outliers ya que estos influyen en el cálculo del error de los modelos puesto que estos son muy elevados.
# En este caso los outliers desvirtuan el modelo puesto que se tratan de retrasos que están por encima de lo normal.


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




##### 2.6.1.1.2. Modelo teniendo en cuenta únicamente el Origen #####

##### 2.6.1.2. Decision Tree #####
modDT <- rpart(ArrDel15~DayofMonth+DayOfWeek+TailNum+Origin+Dest+DepTimeBlk+Distance+ActualElapsedTime, data = train, method = "class")
summary(modDT)

# Predicción
prediccionDT <- predict(modDT, newdata = test, type="class")

# Matriz de confusión
confusionMatrix(data = as.factor(prediccionDT), reference = test$ArrDel15)

# Precisión/Recall 
# https://stackoverflow.com/questions/8499361/easy-way-of-counting-precision-recall-and-f1-score-in-r
precisionDT <- posPredValue(as.factor(prediccionDT), test$ArrDel15, positive="1")
recallDT <- sensitivity(as.factor(prediccionDT), test$ArrDel15, positive="1")

c(precisionDT,recallDT)

##### 2.3.3. SVM (Support Vector Machine) #####
modSVM <- svm(ArrDel15~DayofMonth+DayOfWeek+UniqueCarrier+Origin+DepTimeBlk+Distance, data = train)
summary(modSVM)

# Predicción
prediccionSVM <- predict(modSVM, newdata = test)

# Matriz de confusión
confusionMatrix(data = as.factor(prediccionSVM), reference = test$ArrDel15)

# Precisión/Recall 
precisionSVM <- posPredValue(as.factor(prediccionSVM), test$ArrDel15, positive="1")
recallRFSVM <- sensitivity(as.factor(prediccionSVM), test$ArrDel15, positive="1")

c(precisionSVM,recallRFSVM)


# *************************************************************************************************
# ¿Hay relación entre los minutos de retraso en la salida y los minutos de retraso de llegada
# al destino?
# plot(flights$DepDelayMinutes,flights$ArrDelayMinutes, main = "Diagrama de dispersión", 
#      xlab = "Departure Delay Minutes", ylab = "Arrival Delay Minutes", col="red")
# abline(lm(flights$ArrDelay~flights$DepDelay, data=flights))
# grid()
ggplot(flights, aes(x = flights$DepDelayMinutes, y = flights$ArrDelayMinutes)) + geom_point() + 
  geom_smooth(method = "lm", se=TRUE, color="red", formula = y ~ x)
# Como se observa, hay una relación lienal entre los minutos de retraso de salida de un vuelo y los
# minutos de retraso en la llegada. Esto representa en que la propagación de los retrasos es lineal
modelo1=lm(flights$ArrDelayMinutes~flights$DepDelayMinutes, data = flights)
summary(modelo1)
# Call:
#   lm(formula = flights$ArrDelayMinutes ~ flights$DepDelayMinutes, 
#      data = flights)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -74.179  -3.541  -0.835   0.299 282.950 
# 
# Coefficients:
#                            Estimate Std. Error t value Pr(>|t|)    
# (Intercept)               0.8351843  0.0126344    66.1   <2e-16 ***
#   flights$DepDelayMinutes 0.9816002  0.0002829  3469.4   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 11.28 on 940454 degrees of freedom --> 11,28% de ruido que no se sabe de 
#                                                                 donde viene
# Multiple R-squared:  0.9275,	Adjusted R-squared:  0.9275 --> Coinciden
# F-statistic: 1.204e+07 on 1 and 940454 DF,  p-value: < 2.2e-16
plot(modelo1$residuals)
hist(modelo1$residuals) # asimétrico hacia la izquierda
qqnorm(modelo1$residuals)
qqline(modelo1$residuals, col=2)
modelo1$coefficients
confint(modelo1)
confint(modelo1,level = 0.95)
confint(modelo1,level = 0.999)

cor(modelo1$residuals,flights$DepDelayMinutes)

boxplot(modelo1$residuals~flights$DepDelayMinutes)
aggregate(modelo1$residuals~flights$DepDelayMinutes, FUN=mean)

anova(modelo1)
plot(modelo1)


# ¿Un vuelo de llegada se puede retrasar si sale con retraso?
ggplot(flights, aes(x = flights$Carrier, y = flights$ArrDelayMinutes)) + geom_point() + 
  geom_smooth(method = "lm", se=TRUE, color="red", formula = y ~ x)

modelo2 = lm(flights$ArrDelayMinutes~flights$Carrier, data = flights)
summary(modelo2)
# *************************************************************************************************




# *************************************************************************************************
# Estimación tiempo retraso en la llegada
flightsAux <- flights %>% 
  sample_n(100000)

# https://stackoverflow.com/questions/17200114/how-to-split-data-into-training-testing-sets-using-sample-function
set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(flightsAux), size = floor(.80*nrow(flightsAux)), replace = F)
train <- flightsAux[sample, ]
test  <- flightsAux[-sample, ]

moli = lm(ArrDelay~DayofMonth+DayOfWeek+Origin+Dest+Distance+UniqueCarrier+DepTimeBlk, data = train)
summary(moli)
predict.lm(moli,test)

mape <- mean(abs((predict.lm(moli,test) - test$ArrDelay))/test$ArrDelay)  
mape

# *************************************************************************************************