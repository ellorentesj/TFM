#### TFM: ANÁLISIS Y PREDICCIÓN DEL RESTRASO EN LOS VUELOS ####

#### 1. DATOS DE ENTRADA: Análisis y Exploración del dataset de entrada ####

# Los datos de los vuelos se obtienen de: https://www.transtats.bts.gov/Tables.asp?DB_ID=120 tiene
# datos desde 1987 hasta 2018, actualizándose mensualmente


##### 1.1. Bloque de carga de librerias #####

list.of.packages <- c("data.table", "dplyr", "lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


##### 1.2. Bloque de carga de datos #####

# Selección de ruta, en mi caso: "/Users/ellorentesj/Desktop/TFM/MI_TFM"
setwd("/Users/ellorentesj/Desktop/TFM/MI_TFM")

library(data.table)

# Al tratarse de un fichero con más de 6 millones de registros, he medido los tiempos de lectura de
# las diferentes funciones y finalmente el menor tiempo de lectura lo realiza la función fread de 
# data.table, sin embargo, se ha utilizado la función read.table, por devolver los datos como 
# int y factor y solucionar así el tratamiento de las variables de manera independiente teniendo
# que recorrer el la tabla en cada pasada de cambio de tipo:
# 1. Lectura con read.table de data.table 
#    timeReadTable <- proc.time()
vuelos <- read.table("data/2013/2013.csv", header=T, sep=',')
#    proc.time() - timeReadTable
#    user      system  elapsed 
#    168.039   19.946  215.736
# 2. Lectura con fread de data.table
#    timeReadTable <- proc.time()
#    vuelos <- fread("data/2013/2013.csv", header=T, sep=',')
#    proc.time() - timeReadTable
#    user     system elapsed 
#    11.880   3.106  34.972 
# 3. Lectura con read_csv de readr
#    timeReadTable <- proc.time()
#    vuelos <- read_csv("data/2013/2013.csv", col_names = T, progress = T)
#    proc.time() - timeReadTable
#    user    system  elapsed 
#    23.197   5.426  36.355
# 4. Lectura con read.csv en base R
#    timeReadTable <- proc.time()
#    vuelos <- read.csv("data/2013/2013.csv", header=T, sep=',')
#    proc.time() - timeReadTable
#    user  system elapsed 
#    167.924  13.573 185.828 

#weather <- read.table("weather.csv",header=T,sep=',')

# trabajo sobre una copia por si acaso
vuelos2 <- vuelos

##### 1.3. Bloque de revisión basica del dataset #####

str(vuelos)
# 'data.frame':	6369482 obs. of  52 variables:
# $ Year              : int  2013 2013 2013 2013 2013 2013 2013 2013 2013 2013 ...
# $ Quarter           : int  1 1 1 1 1 1 1 1 1 1 ...
# $ Month             : int  1 1 1 1 1 1 1 1 1 1 ...
# $ DayofMonth        : int  18 18 18 18 18 18 18 18 18 18 ...
# $ DayOfWeek         : int  5 5 5 5 5 5 5 5 5 5 ...
# $ FlightDate        : Factor w/ 365 levels "2013-01-01","2013-01-02",..: 18 18 18 18 18 18 18 18 18 18 ...
# $ UniqueCarrier     : Factor w/ 16 levels "9E","AA","AS",..: 5 5 5 5 5 5 5 5 5 5 ...
# $ AirlineID         : int  19790 19790 19790 19790 19790 19790 19790 19790 19790 19790 ...
# $ Carrier           : Factor w/ 16 levels "9E","AA","AS",..: 5 5 5 5 5 5 5 5 5 5 ...
# $ FlightNum         : int  1497 1498 1499 1500 1501 1502 1503 1504 1504 1505 ...
# $ OriginAirportID   : int  14100 11697 12953 14869 10721 11193 15304 10397 12264 13487 ...
# $ OriginAirportSeqID: int  1410002 1169703 1295302 1486903 1072102 1119302 1530402 1039705 1226402 1348702 ...
# $ OriginCityMarketID: int  34100 32467 31703 34614 30721 33105 33195 30397 30852 31650 ...
# $ Origin            : Factor w/ 320 levels "ABE","ABI","ABQ",..: 240 118 183 288 42 81 304 21 151 219 ...
# $ OriginCityName    : Factor w/ 315 levels "Aberdeen, SD",..: 235 107 214 258 37 63 290 18 301 196 ...
# $ OriginState       : Factor w/ 53 levels "AK","AL","AR",..: 38 9 34 46 19 17 9 10 47 23 ...
# $ OriginStateFips   : int  42 12 36 49 25 21 12 13 51 27 ...
# $ OriginStateName   : Factor w/ 53 levels "Alabama","Alaska",..: 38 9 32 47 21 17 9 10 49 23 ...
# $ OriginWac         : int  23 33 22 87 13 52 33 34 38 63 ...
# $ DestAirportID     : int  13487 12953 10397 10397 10397 15304 11193 12264 10397 14771 ...
# $ DestAirportSeqID  : int  1348702 1295302 1039705 1039705 1039705 1530402 1119302 1226402 1039705 1477101 ...
# $ DestCityMarketID  : int  31650 31703 30397 30397 30397 33195 33105 30852 30397 32457 ...
# $ Dest              : Factor w/ 318 levels "ABE","ABI","ABQ",..: 217 182 21 21 21 302 81 150 21 277 ...
# $ DestCityName      : Factor w/ 313 levels "Aberdeen, SD",..: 194 212 18 18 18 288 62 299 18 260 ...
# $ DestState         : Factor w/ 53 levels "AK","AL","AR",..: 23 34 10 10 10 9 17 47 10 5 ...
# $ DestStateFips     : int  27 36 13 13 13 12 21 51 13 6 ...
# $ DestStateName     : Factor w/ 53 levels "Alabama","Alaska",..: 23 32 10 10 10 9 17 49 10 5 ...
# $ DestWac           : int  63 22 34 34 34 33 52 38 34 91 ...
# $ CRSDepTime        : int  805 704 1700 940 715 900 1300 1406 1636 1740 ...
# $ DepTime           : int  758 657 1657 953 711 856 1248 1440 1647 1738 ...
# $ DepDelay          : int  -7 -7 -3 13 -4 -4 -12 34 11 -2 ...
# $ DepDelayMinutes   : int  0 0 0 13 0 0 0 34 11 0 ...
# $ DepDel15          : int  0 0 0 0 0 0 0 1 0 0 ...
# $ TaxiOut           : int  24 17 50 49 23 31 13 10 29 32 ...
# $ TaxiIn            : int  4 18 13 17 10 3 5 7 8 9 ...
# $ CRSArrTime        : int  1015 959 1943 1521 1019 1118 1511 1555 1830 2001 ...
# $ ArrTime           : int  1002 949 2007 1604 1002 1121 1452 1608 1849 1948 ...
# $ ArrDelay          : int  -13 -10 24 43 -17 3 -19 13 19 -13 ...
# $ ArrDelayMinutes   : int  0 0 24 43 0 3 0 13 19 0 ...
# $ ArrDel15          : int  0 0 1 1 0 0 0 0 1 0 ...
# $ Cancelled         : int  0 0 0 0 0 0 0 0 0 0 ...
# $ CancellationCode  : Factor w/ 5 levels "","A","B","C",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ CRSElapsedTime    : int  190 175 163 221 184 138 131 109 114 261 ...
# $ ActualElapsedTime : int  184 172 190 251 171 145 124 88 122 250 ...
# $ AirTime           : int  156 137 127 185 138 111 106 71 85 209 ...
# $ Flights           : int  1 1 1 1 1 1 1 1 1 1 ...
# $ Distance          : int  980 1076 762 1590 946 773 773 534 534 1589 ...
# $ CarrierDelay      : int  NA NA 0 0 NA NA NA NA 0 NA ...
# $ WeatherDelay      : int  NA NA 0 13 NA NA NA NA 0 NA ...
# $ NASDelay          : int  NA NA 24 30 NA NA NA NA 8 NA ...
# $ SecurityDelay     : int  NA NA 0 0 NA NA NA NA 0 NA ...
# $ LateAircraftDelay : int  NA NA 0 0 NA NA NA NA 11 NA ... 

summary(vuelos)


##### 1.4. Bloque de tratamiento de variables #####

library(dplyr)
#library(lubridate)


#### 1.4.1. Variables que se transforman a factor  ####
vuelos$Year = as.factor(vuelos$Year)
# La transformo a factor ya que se tratan de cuatrimestres
vuelos$Quarter = as.factor(vuelos$Quarter)
# Transformándolo en factor, tenemos una cuenta rápida de los vuelos que hay en cada uno de los meses
vuelos$Month = as.factor(vuelos$Month)
vuelos$DayofMonth = as.factor(vuelos$DayofMonth)
vuelos$DayOfWeek = as.factor(vuelos$DayOfWeek)
vuelos$UniqueCarrier = as.factor(vuelos$UniqueCarrier)
vuelos$AirlineID = as.factor(vuelos$AirlineID)
vuelos$Carrier = as.factor(vuelos$Carrier)
vuelos$FlightNum = as.factor(vuelos$FlightNum)
vuelos$OriginAirportID = as.factor(vuelos$OriginAirportID)
vuelos$OriginAirportSeqID = as.factor(vuelos$OriginAirportSeqID)
vuelos$OriginCityMarketID = as.factor(vuelos$OriginCityMarketID)
vuelos$OriginStateFips = as.factor(vuelos$OriginStateFips)
vuelos$OriginWac = as.factor(vuelos$OriginWac)
vuelos$DestAirportID = as.factor(vuelos$DestAirportID)
vuelos$DestAirportSeqID = as.factor(vuelos$DestAirportSeqID)
vuelos$DestCityMarketID = as.factor(vuelos$DestCityMarketID)
vuelos$DestStateFips = as.factor(vuelos$DestStateFips)
vuelos$DestWac = as.factor(vuelos$DestWac)
vuelos$DepDel15 = as.factor(vuelos$DepDel15)
vuelos$ArrDel15 = as.factor(vuelos$ArrDel15)
vuelos$Cancelled = as.factor(vuelos$Cancelled)
vuelos$Flights = as.factor(vuelos$Flights)
summary(vuelos)

#### 1.4.2. Variables que se transforman en fecha  ####
vuelos$FlightDate = as.Date(vuelos$FlightDate)
str(vuelos$FlightDate)

# Realizo una copia del DF en este momento por si tuviese que volver a recuperarlo
vuelos3 <- vuelos

#### 1.4.3. Variables que se transforman en hora  ####
#as.ITime(format(strptime(vuelos$DepTime, format = "%H%M"), format = "%H:%M"))
vuelos$CRSDepTime = format(strptime(vuelos$CRSDepTime, format = "%H%M"), format = "%H:%M")
vuelos$DepTime = format(strptime(vuelos$DepTime, format = "%H%M"), format = "%H:%M")
vuelos$CRSArrTime = format(strptime(vuelos$CRSArrTime, format = "%H%M"), format = "%H:%M")
vuelos$ArrTime = format(strptime(vuelos$ArrTime, format = "%H%M"), format = "%H:%M")

# Se convierte a factor 
# Revisando la ejecución de summary(vuelos), vemos que existen variables con contenido NA's
vuelos %>% 
  filter(DepTime == "NA") %>% 
  top_n(10)

# Top 3 de aeropuertos que tienen más vuelos retrasados
vuelos %>% 
  group_by(Origin) %>% 
  summarise(totalDel15 = sum(DepDel15, na.rm = T)) %>% 
  arrange(-totalDel15) %>% 
  top_n(3)
# Origin totalDel15
#   <fct>       <int>
# 1 ORD         81882
# 2 ATL         79937
# 3 DFW         68549

vuelos %>% 
  filter(OriginState == "CA", DepDel15 > 15) 