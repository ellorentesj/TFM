#### TFM: ANÁLISIS Y PREDICCIÓN DEL RESTRASO EN LOS VUELOS ####

#### 2. ANÁLISIS VISUAL DE LOS DATOS: Análisis de las gráficas de Tableau ####

# *************************************************************************************************
##### 2.1. Bloque de carga de librerias #####
if(!require("graphics")){
  install.packages("graphics")
  library(graphics)
}
if(!require("stats")){
  install.packages("stats")
  library(stats)
}
if(!require("ggplot2")){
  install.packages("ggplot2")
  library(ggplot2)
}
if(!require("lubridate")){
  install.packages("lubridate")
  library(lubridate)
}
# *************************************************************************************************


# *************************************************************************************************
##### 2.2. Bloque de análisis visual de datos #####

# Se quiere predecir la variable ArrDel15, por tanto vamos a visualizar la relación que tienen con 
# el resto de variables

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


#### 2.6. Análisis gráficas Tableau ####

#### 2.6.1. Media de vuelos retrasados por Compañía ####
flights %>% 
  group_by(UniqueCarrier) %>% 
  summarise(TotalDelayFligths = sum(ArrDel15==1), AvgDelArr = (sum(ArrDel15==1)/nrow(flights))*100) %>% 
  arrange(-AvgDelArr)
# La compañía B6 es quien acumula más promedio de minutos de retraso. Sin embargo, la compañía WN
# es la compañía que más vuelos tiene retrasados.

#### 2.6.2. Media de retraso por Aeropuerto ####
flights %>% 
  group_by(Origin) %>% 
  summarise(TotalDelayFligths = sum(ArrDel15==1), AvgDelArr = mean(ArrDelayMinutes), 
            AvgDelDep= mean(DepDelayMinutes)) %>% 
  arrange(-AvgDelArr)
# Top 3 de destinos que tienen más vuelos retrasados, 
flights %>% 
  group_by(Dest) %>% 
  summarise(totalArrDel15 = sum(ArrDel15==1), AvgRetDel = (sum(ArrDel15==1)/nrow(flights))*100) %>% 
  arrange(-totalArrDel15) %>% 
  top_n(3)
#   Origin totalDel15 ProRetDel totalArrDel15  ProRetArr
#   <fct>       <int>     <dbl>         <int>      <dbl>
# 1 ORD         16437      1.74         17119       1.81
# 2 ATL         17053      1.81         15283       1.62
# 3 DEN         14102      1.49         13929       1.48
flights %>% 
  count(Dest) %>% 
  arrange(-n) %>% 
  top_n(3)
flights %>% 
  count(Origin) %>% 
  arrange(-n) %>% 
  top_n(3)


