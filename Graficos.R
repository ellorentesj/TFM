#### TFM: ANÁLISIS Y PREDICCIÓN DEL RESTRASO EN LOS VUELOS ####

#### 3. ANÁLISIS VISUAL DE LOS DATOS: Visualización mediante gráficos ####

# *************************************************************************************************
##### 3.1. Bloque de carga de librerias #####
list.of.packages <- c("graphics","stats","ggplot2","lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(graphics)
library(stats)
library(ggplot2)
library(lubridate)
# *************************************************************************************************


# *************************************************************************************************
##### 2.2. Bloque de análisis visual de datos #####

# ¿Hay algún tipo de relación entre la hora de salida de un vuelo y la creación de los retrasos en 
# el aeropuerto de destino?
ggplot(flights, aes(x = as.integer(flights$DepTime), y = flights$ArrDelayMinutes)) + 
  geom_point(alpha = 0.5, colour = "blue") + 
  labs(x = "Departure Time", y = "Arrival Delay", title = "Arrival Delay vs Departure Time") +
  geom_smooth(color = "red", se = T, level = 0.95)

# Frecuencia en relación con el rendimiento de los vuelos realizados en el tiempo programado y los 
# que poseen retraso de más de 15 minutos
ggplot(flights, aes(x = flights$DepDel15)) + geom_bar(fill = "blue") +
  labs(x = "Delay > 15", y = "Frequency", title = "Frequecy vs Delay > 15")

# Frecuencia de vuelos realizados por las compañías aéreas
ggplot(flights, aes(x = flights$Carrier)) + geom_bar(fill = "blue") +
  labs(x = "Company", y = "Frequency", title = "Frequecy vs Company")
# La compañía Sothwest Airlines (WN) es la que realiza un mayor número de vuelos para el período de
# estudio

# Frecuencia de vuelos realizados a cada aeropuerto
ggplot(flights, aes(x = flights$Dest)) + geom_bar(fill = "blue") +
  labs(x = "Destiny", y = "Frequency", title = "Frequecy vs Destiny", hjust)
# Aunque prácticamente no se aprecia, el mayor tránsito de vuelos se realiza en ATL (Atlanta), con
# 59725 vuelos operados de salida y 59792 de llegada.

# Representación de la media de los retrasos tanto en las salidas como en las llegadas por compañía
# aérea
ggplot(flights, aes(x = flights$Carrier, y = flights$DepDelayMinutes)) + 
  geom_bar(fill = "blue")


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
ggplot(flights, aes(x = flights$DepDelayMinutes, y = flights$ArrDel15)) + geom_point() + 
  geom_smooth(method = "lm", se=TRUE, color="red", formula = y ~ x)
modelo2 = lm(flights$ArrDel15~flights$DepDelayMinutes + flights$Distance + flights$AirTime +
               flights$DepTime, data = flights)
summary(modelo2)

# *************************************************************************************************

