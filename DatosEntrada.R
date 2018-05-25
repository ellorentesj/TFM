#### TFM: ANÁLISIS Y PREDICCIÓN DEL RESTRASO EN LOS VUELOS ####

#### 2. DATOS DE ENTRADA: Análisis y Exploración del dataset de entrada ####


# *************************************************************************************************
##### 2.1. Bloque de carga de librerias #####

list.of.packages <- c("data.table", "dplyr", "tidyr","lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(data.table)
library(dplyr)
library(lubridate)
library(tidyr)

# *************************************************************************************************


# *************************************************************************************************
##### 2.2. Bloque de carga de datos #####
# Selección de ruta, en mi caso: "/Users/ellorentesj/repostGitHub/TFM/data/"
setwd("/Users/ellorentesj/repostGitHub/TFM/data/")
flights <- fread("flights.csv", header=T, sep=',')
# *************************************************************************************************


# *************************************************************************************************
##### 2.3. Bloque de revisión basica del dataset #####
str(flights)
summary(flights)
# *************************************************************************************************


# *************************************************************************************************
##### 2.4. Bloque de tratamiento de variables #####

#### 2.4.1. Transformación de variables a factor  ####
flights$UniqueCarrier = as.factor(flights$UniqueCarrier)
flights$AirlineID = as.factor(flights$AirlineID)
flights$Carrier = as.factor(flights$Carrier)
flights$FlightNum = as.factor(flights$FlightNum)
flights$OriginAirportID = as.factor(flights$OriginAirportID)
flights$OriginAirportSeqID = as.factor(flights$OriginAirportSeqID)
flights$OriginCityMarketID = as.factor(flights$OriginCityMarketID)
flights$Origin = as.factor(flights$Origin)
flights$OriginCityName = as.factor(flights$OriginCityName)
flights$OriginState = as.factor(flights$OriginState)
flights$DestAirportID = as.factor(flights$DestAirportID)
flights$DestAirportSeqID = as.factor(flights$DestAirportSeqID)
flights$DestCityMarketID = as.factor(flights$DestCityMarketID)
flights$Dest = as.factor(flights$Dest)
flights$DestCityName = as.factor(flights$DestCityName)
flights$DestState = as.factor(flights$DestState)
flights$DepDel15 = as.factor(flights$DepDel15)
flights$ArrDel15 = as.factor(flights$ArrDel15)
flights$Cancelled = as.factor(flights$Cancelled)
flights$CancellationCode = as.factor(flights$CancellationCode)

# Reviso por si me he dejado alguna variable sin transformar a factor
summary(flights)

#### 2.4.2. Transformación de variables a fecha  ####
flights$FlightDate = ymd(flights$FlightDate)

#### 2.4.3. Transformación de variables de tipo hora  ####
flights$DepTime = format(strptime(flights$DepTime, format = "%H%M"), format = "%H%M")
flights$ArrTime = format(strptime(flights$ArrTime, format = "%H%M"), format = "%H%M")

#### 2.4.4. Transformación de variables de tipo integer  ####
flights$DepDelay = as.integer(flights$DepDelay)
flights$DepDelayMinutes = as.integer(flights$DepDelayMinutes)
flights$TaxiOut = as.integer(flights$TaxiOut)
flights$TaxiIn = as.integer(flights$TaxiIn)
flights$ArrDelay = as.integer(flights$ArrDelay)
flights$ArrDelayMinutes = as.integer(flights$ArrDelayMinutes)
flights$AirTime = as.integer(flights$AirTime)
flights$Distance = as.integer(flights$Distance)
flights$CarrierDelay = as.integer(flights$CarrierDelay)
flights$WeatherDelay = as.integer(flights$WeatherDelay)
flights$NASDelay = as.integer(flights$NASDelay)
flights$SecurityDelay = as.integer(flights$SecurityDelay)
flights$LateAircraftDelay = as.integer(flights$LateAircraftDelay)

# Reviso por si me he dejado alguna variable sin transformar
str(flights)

#### 2.4.4. Limpieza de datos no significativos  ####
# Procedo a eliminar los datos que no ofrecen ningún tipo de información para el análisis de la 
# predicción. Para ello visualizo tanto las variables que tienen campos nulos como los que no 
colSums(is.na(flights))>0

### DEPTIME ###
flights %>% 
  filter(is.na(DepTime)) %>% 
  nrow() # [1] 44553
# Elimino los registros relacionados con la hora de despegue (DepTime) que no contienen ningún 
# tipo de información, es decir, son campos vacíos que corresponden a vuelos cancelados. Estos no 
# son de utilidad para analizar y predecir los retrasos en los vuelos puesto que son operaciones no
# realizadas. Un vuelo está cancelado cuando en su campo indica un 1, y un 0 cuando el vuelo no ha
# sido cancelado
flights %>% 
  filter(is.na(DepTime), Cancelled == 1) %>% 
  nrow() # [1] 44553
# Guardo los vuelos cancelados en un dataframe aparte
canceled <- flights %>% filter(is.na(DepTime))
# Elimino del dataframe vuelos los vuelos cancelados
flights <- flights %>% 
  drop_na(DepTime)
### DEPTIME ###

### CANCELLED & CANCELLATIONCODE ###
# Respectivamente si un vuelo no ha sido cancelado, el campo Cancellation_Code estará vacío, es 
# decir, no contrendrá un código informativo, con lo cual el campo será nulo. Por tanto, podemos 
# prescindir de las variables Cancelled y CancellationCode
flights$Cancelled <- NULL
flights$CancellationCode <- NULL
### CANCELLED & CANCELLATIONCODE ###

colSums(is.na(flights))>0

### AIRTIME ###
flights %>% 
  filter(is.na(AirTime)) %>% 
  nrow() # [1] 3679
# Elimino estos vuelos puesto que no sabemos el tiempo de vuelo haste el destino y cálculo es tedioso
# y supone un 0.38% del dataset
flights <- flights %>% 
  drop_na(AirTime)
### AIRTIME ###

colSums(is.na(flights))>0

### TAXIOUT, TAXIIN, ARRDELAY, ARRDELAYMINUTES, ARRDEL15, CARRIERDELAY, WEATHERDELAY, NASDELAY, 
# SECURITYDELAY, LATEAIRCRAFTDELAY ###
# flights %>% 
#   filter(is.na(TaxiOut)) %>% 
#   nrow() # [1] 723
# flights %>% 
#   filter(is.na(TaxiIn)) %>% 
#   nrow() # [1] 1984
# flights %>% 
#   filter(is.na(ArrDelay)) %>% 
#   nrow() # [1] 3679
# flights %>% 
#   filter(is.na(ArrDelayMinutes)) %>% 
#   nrow() # [1] 3679
# flights %>% 
#   filter(is.na(ArrDel15)) %>% 
#   nrow() # [1] 3679
flights %>% 
  filter(is.na(CarrierDelay)) %>% 
  nrow() # [1] 675504
flights %>% 
  filter(is.na(WeatherDelay)) %>% 
  nrow() # [1] 675504
flights %>% 
  filter(is.na(NASDelay)) %>% 
  nrow() # [1] 675504
flights %>% 
  filter(is.na(SecurityDelay)) %>% 
  nrow() # [1] 675504
flights %>% 
  filter(is.na(LateAircraftDelay)) %>% 
  nrow() # [1] 675504
# El siguiente objetivo es hacer que las variables TaxiOut, TaxiIn, ArrDelay, ArrDelayMinutes, 
# ArrDel15, CRSElapsedTime, ActualElapsedTime, AirTime, CarrierDelay, WeatherDelay, NASDelay, 
# SecurityDelay, LateAircraftDelay que contienen campos nulos, es hacer que contengan algún tipo de
# información ya que se requieren para poder analizar los retrasos en los vuelos. Se procede a 
# cambiar los campos nulos(NA) de estas variables por valores con el número 0.
# flights <- flights %>% 
#   mutate(TaxiOut = coalesce(as.integer(TaxiOut),0L))
# flights <- flights %>% 
#   mutate(TaxiIn = coalesce(as.integer(TaxiIn),0L))
# flights <- flights %>% 
#   mutate(ArrDelay = coalesce(as.integer(ArrDelay),0L))
# flights <- flights %>% 
#   mutate(ArrDelayMinutes = coalesce(as.integer(ArrDelayMinutes),0L))
# flights <- flights %>% 
#   mutate(ArrDel15 = coalesce(as.integer(ArrDel15),0L))
flights <- flights %>%
  mutate(CarrierDelay = coalesce(as.integer(CarrierDelay),0L))
flights <- flights %>% 
  mutate(WeatherDelay = coalesce(as.integer(WeatherDelay),0L))
flights <- flights %>% 
  mutate(NASDelay = coalesce(as.integer(NASDelay),0L))
flights <- flights %>% 
  mutate(SecurityDelay = coalesce(as.integer(SecurityDelay),0L))
flights <- flights %>% 
  mutate(LateAircraftDelay = coalesce(as.integer(LateAircraftDelay),0L))
### TAXIOUT, TAXIIN, ARRDELAY, ARRDELAYMINUTES, ARRDEL15, CARRIERDELAY, WEATHERDELAY, NASDELAY, 
# SECURITYDELAY, LATEAIRCRAFTDELAY ###

### ARRDELAY ###
# Sustituyo los valores que contiene el campo CarrierDelay por los valores que contiene el campo 
# ArrDelay únicamente en las filas donde la suma de las variables de los campos con retraso den 0 y
# el valor de ArrDelay sea mayor que 0. Uso el campo ArrDelay ya que es el campo que refistra el 
# total de los retrasos obtenidos en un vuelos.
# Este paso es necesario, de lo contrario, los campos de las causas de los retrasos, no contendrían
# la información completa de los retrasos, omitiéndo información útil de si los vuelos han llegado 
# a su destino con retraso.
# Con esto se consigue saber correctamente si el vuelo realizado ha llegado a su destino con algún
# tipo de retraso o ha llegado puntual.
flights %>% 
  filter(ArrDelay>0, (CarrierDelay + WeatherDelay + NASDelay + SecurityDelay + LateAircraftDelay) == 0) %>% 
  nrow() # [1] 195719
# Añado una columna donde se sumen las variables con retraso 
totalDelay <- flights$CarrierDelay+flights$WeatherDelay+flights$NASDelay+flights$SecurityDelay+flights$LateAircraftDelay
# La añaado al dataset
flights <- cbind(flights, totalDelay)
# Modifico los CarrierDelay donde ArrDelay > 0 & (CarrierDelay + WeatherDelay + NASDelay + SecurityDelay + LateAircraftDelay) == 0
flights[flights$ArrDelay>0 & flights$totalDelay==0,"CarrierDelay"] <- flights[flights$ArrDelay>0 & flights$totalDelay == 0, "ArrDelay"]
rm(totalDelay)
### ARRDELAY ###

# Reviso que no quede ninguna variable más por limpiar
colSums(is.na(flights))>0
# *************************************************************************************************

# Guardo el dataset
write.table(flights, file = "finalFlights.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)

# Top 3 de destinos que tienen más vuelos retrasados, 
flights %>% 
  group_by(Dest) %>% 
  summarise(totalArrDel15 = sum(ArrDel15==1), ProRetDel = (sum(ArrDel15==1)/nrow(flights))*100) %>% 
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
