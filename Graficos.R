#### TFM: ANÁLISIS Y PREDICCIÓN DEL RESTRASO EN LOS VUELOS ####

#### 3. ANÁLISIS VISUAL DE LOS DATOS: Visualización mediante gráficos ####

list.of.packages <- c("graphics","stats")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(graphics)
library(stats)

# ¿Hay algún tipo de relación lineal entre la hora de salida de un vuelo y la creación de los 
# retrasos en el aeropuerto de destino?
scatter.smooth(hm(flights$DepTime),flights$ArrDelay, xlab = "Arrival Delay Minutes", 
               ylab = "Departure Time",col = "red")
abline(lm(hm(flights$DepTime)~flights$ArrDelay, data=flights))
grid()

#install.packages("graphics")

library(graphics)

# Relación entre los minutos de retraso en la salida y como influyen en la llegada
plot(flights$DepDelay,flights$ArrDelay, xlab = "Arrival Delay Minutes", 
     ylab = "Departure Delay Minutes",col="red")
abline(lm(flights$DepDelay~flights$ArrDelay, data=flights))
grid()

plot(flights$DepDelay,flights$AirTime, xlab = "Air Time", 
     ylab = "Departure Delay Minutes",col="red")
abline(lm(flights$DepDelay~flights$AirTime, data=flights))
grid()


