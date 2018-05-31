#### TFM: ANÁLISIS Y PREDICCIÓN DEL RESTRASO EN LOS VUELOS ####



#### 4. DATOS DE ENTRADA: Modelos y su comparación ####



# *************************************************************************************************
##### 4.1. Bloque de carga de librerias #####
# Confusion Matrix
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
##### 4.2. Bloque de modelización #####

##### 4.2.1. Predicción: Si un vulelo puede o no llegar con retraso ####

str(flights)

# Genero una muestra con 150000 registros aleatorios del dataset flights
flightsAux <- flights %>% sample_n(150000)

# https://stackoverflow.com/questions/17200114/how-to-split-data-into-training-testing-sets-using-sample-function
# Establezco la semilla para que la misma muestra pueda reproducirse también en el futuro
set.seed(7) 
# Selecciono el 80% de los datos como muestra del total de las filas de los datos  
sample <- sample.int(n = nrow(flightsAux), size = floor(.80*nrow(flightsAux)), replace = F)
train <- flightsAux[sample, ] # Conjunto de entrenamiento
test  <- flightsAux[-sample, ] # Conjunto de test

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

##### 4.2.1.2. Modelo Completo #####

# Realizo un modelo con todas las características con Regresión Logística:
modLRComplete = glm(ArrDel15~., family=binomial(link='logit'), data = train)
# El modelo reporta el siguiente Warning
# Warning messages:
#   1: glm.fit: algorithm did not converge 
#   2: glm.fit: fitted probabilities numerically 0 or 1 occurred 
summary(modLRComplete)
# Voy a ver la importancia de cada variable en el modelo, graficándola
dfmodLRComplete <- varImp(modLRComplete)
overall <- as.double(dfmodLRComplete$Overall)
names <- rownames(dfmodLRComplete)
df <- data.frame(names)
df$overall <- overall
ggplot(df, aes(x = df$names, y = df$overall)) + geom_col(fill="blue") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Names", y = "Overall", title = "Importance of features using Logistic Regression")
# Parece que las características que más influyen para que un vuelo se retrase o no son:
# * CarrierDelay
# * DepDela15
# * LateAircraftDelay
# * NASDelay
# * WeatherDelay
# Tiene sentido puesto que todas estas características indican las razones de retraso en la salida y
# si hay retraso o no en la salida, por tanto, deberíamos de prescindir de estas variables para la
# realización del modelo, sobre todo de las caracterísiticas de los retrasos.
rm(df)
rm(names)
rm(overall)

# Comparo este resultado realizando un modelo de todas las características con Random Forest:
modRFComplete <- randomForest(ArrDel15~., data = train, ntree=100)
# Voy a ver la importancia de cada una de las características gráficamente:
dfmodRFComplete <- importance(modRFComplete)
meanDecreaseGini <- as.double(dfmodRFComplete)
names <- rownames(dfmodLRComplete)
df <- data.frame(names)
df$meanDecreaseGini <- meanDecreaseGini
ggplot(df, aes(x = df$names, y = df$meanDecreaseGini)) + geom_col(fill="blue") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Names", y = "MeanDecreaseGini", title = "Importance of features using Random Forest")
# Obtenemos las mismas características, por tanto, de momento lo que sí se puede indicar, es que 
# para la realización del modelo es necesario prescindir de:
# * Las características que nos aportan información del retraso: ArrDelay, ArrDelayMinutes, 
#   ArrDelayGroupsWeights
# * Las características que indican el grupo de retraso: CarrierDelay, LateAircraftDelay, NASDelay,
#   WeatherDelay; puesto que ya existe un DepDelay y un DepDelayMinutes que nos indica si sale 
#   con retraso el vuelo.

##### 4.2.1.2. Segundo Modelo #####











# https://stackoverflow.com/questions/46028360/confusionmatrix-for-logistic-regression-in-r
# Predicción
prediccionMRL <- round(predict(modLR, newdata = test, type = "response"))

# Matriz de confusión
confusionMatrix(data = as.numeric(prediccionMRL>0.5), reference = test$ArrDel15)

# Precisión/Recall 
# https://stackoverflow.com/questions/8499361/easy-way-of-counting-precision-recall-and-f1-score-in-r
precisionMRL <- posPredValue(as.factor(prediccionMRL), test$ArrDel15, positive="1")
recallMRL <- sensitivity(as.factor(prediccionMRL), test$ArrDel15, positive="1")

c(precisionMRL,recallMRL)
# [1] 0.6891134 0.3539585
# Conclusión: De todos los vuelos que tienen retraso, sólo se clasifican como retraso el 35%.

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

##### 2.3.4. Random Forest #####
# Modelo
modRF <- randomForest(ArrDel15~DepTime+DepDelay+DepDelayMinutes+DepDel15+ActualElapsedTime+Distance+
                        MonthWeights+DayofMonthWeights+DayOfWeekWeights+UniqueCarrierWeights+
                        FlightNumWeights+OriginWeights+OriginStateWeights+DestWeights+DestStateWeights, data = train, ntree=100)

importance(modRF)
# DepTime                         4.687132e+01
# DepDelay                        4.011176e+03
# DepDelayMinutes                 4.418337e+03
# DepDel15                        9.644844e+02
# ArrTime                         9.523433e-01
# ArrDelay                        9.587953e+03
# ArrDelayMinutes                 1.156015e+04
# ActualElapsedTime               9.131268e+00
# Distance                        2.727709e+00
# CarrierDelay                    1.214974e+03
# WeatherDelay                    6.714168e+01
# NASDelay                        2.316888e+03
# SecurityDelay                   4.606492e-01
# LateAircraftDelay               3.355065e+03
# MonthWeights                    2.285714e-02
# DayofMonthWeights               3.997846e+01
# DayOfWeekWeights                1.818182e-02
# UniqueCarrierWeights            5.989579e-01
# FlightNumWeights                1.144841e+01
# OriginAirportSeqIDWeights       8.557458e+00
# OriginWeights                   1.058045e+01
# OriginStateWeights              2.561749e+00
# DestAirportSeqIDWeights         7.960155e-01
# DestWeights                     2.360634e-01
# DestStateWeights                2.789167e-01
# DepartureDelayGroupsWeights     3.521442e+03
# DepTimeBlkWeights               8.781565e+00
# ArrivalDelayGroupsWeights       1.100253e+04
# ArrTimeBlkWeights               1.559291e+00
# DistanceGroupWeights            2.042958e+00

# Predicción
prediccionRF <- predict(modRF, newdata = test)

# Matriz de confusión
confusionMatrix(data = as.factor(prediccionRF), reference = test$ArrDel15)

# Precisión/Recall 
precisionRF <- posPredValue(as.factor(prediccionRF), test$ArrDel15, positive="1")
recallRF <- sensitivity(as.factor(prediccionRF), test$ArrDel15, positive="1")

c(precisionRF,recallRF)

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


# Hay retraso?
flightsAux <- flights %>% 
  sample_n(100000)

set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(flightsAux), size = floor(.80*nrow(flightsAux)), replace = F)
train <- flightsAux[sample, ]
test  <- flightsAux[-sample, ]

mologit = glm(ArrDel15~DayofMonth+DayOfWeek+Origin+Dest+Distance+UniqueCarrier+DepTimeBlk,family=binomial(link='logit'),data=train)
summary(mologit)

require(caret)
library(caret)
# https://stackoverflow.com/questions/46028360/confusionmatrix-for-logistic-regression-in-r
# Use your model to make predictions, in this example newdata = training set, but replace with your test set    
pdata <- predict(mologit, newdata = test, type = "response")

# use caret and compute a confusion matrix
confusionMatrix(data = as.numeric(pdata>0.5), reference = test$ArrDel15)

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