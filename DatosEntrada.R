#### TFM: ANÁLISIS Y PREDICCIÓN DEL RESTRASO EN LOS VUELOS ####

#### 1. DATOS DE ENTRADA: Análisis y Exploración del dataset de entrada ####

# Los datos de los vuelos se obtienen de: https://www.transtats.bts.gov/Tables.asp?DB_ID=120 tiene
# datos desde 1987 hasta 2018, actualizándose mensualmente


##### 1.1. Bloque de carga de librerias #####

list.of.packages <- c("data.table", "dplyr", "tidyr","lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


##### 1.2. Bloque de carga de datos #####

# Selección de ruta, en mi caso: "/Users/ellorentesj/Desktop/TFM/MI_TFM"
setwd("/Users/ellorentesj/Desktop/TFM/MI_TFM")

library(data.table)

# Al tratarse de un fichero con más de 6 millones de registros, he medido los tiempos de lectura de
# las diferentes funciones y finalmente el menor tiempo de lectura lo realiza la función fread de 
# data.table:
# 1. Lectura con read.table de data.table 
#    timeReadTable <- proc.time()
#    vuelos <- read.table("data/2013/2013.csv", header=T, sep=',')
#    proc.time() - timeReadTable
#    user      system  elapsed 
#    168.039   19.946  215.736
# 2. Lectura con fread de data.table
#    timeReadTable <- proc.time()
vuelos <- fread("data/2013/2013.csv", header=T, sep=',')
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
class(vuelos)

#weather <- read.table("weather.csv",header=T,sep=',')

# Realizo una copia por si tuviese que recuperarla
dfCompleto <- vuelos

##### 1.3. Bloque de revisión basica del dataset #####

str(vuelos)
# Classes ‘data.table’ and 'data.frame':	6369482 obs. of  52 variables:
# $ Year              : int  2013 2013 2013 2013 2013 2013 2013 2013 2013 2013 ...
# $ Quarter           : int  1 1 1 1 1 1 1 1 1 1 ...
# $ Month             : int  1 1 1 1 1 1 1 1 1 1 ...
# $ DayofMonth        : int  18 18 18 18 18 18 18 18 18 18 ...
# $ DayOfWeek         : int  5 5 5 5 5 5 5 5 5 5 ...
# $ FlightDate        : chr  "2013-01-18" "2013-01-18" "2013-01-18" "2013-01-18" ...
# $ UniqueCarrier     : chr  "DL" "DL" "DL" "DL" ...
# $ AirlineID         : int  19790 19790 19790 19790 19790 19790 19790 19790 19790 19790 ...
# $ Carrier           : chr  "DL" "DL" "DL" "DL" ...
# $ FlightNum         : chr  "1497" "1498" "1499" "1500" ...
# $ OriginAirportID   : int  14100 11697 12953 14869 10721 11193 15304 10397 12264 13487 ...
# $ OriginAirportSeqID: int  1410002 1169703 1295302 1486903 1072102 1119302 1530402 1039705 1226402 1348702 ...
# $ OriginCityMarketID: int  34100 32467 31703 34614 30721 33105 33195 30397 30852 31650 ...
# $ Origin            : chr  "PHL" "FLL" "LGA" "SLC" ...
# $ OriginCityName    : chr  "Philadelphia, PA" "Fort Lauderdale, FL" "New York, NY" "Salt Lake City, UT" ...
# $ OriginState       : chr  "PA" "FL" "NY" "UT" ...
# $ OriginStateFips   : chr  "42" "12" "36" "49" ...
# $ OriginStateName   : chr  "Pennsylvania" "Florida" "New York" "Utah" ...
# $ OriginWac         : int  23 33 22 87 13 52 33 34 38 63 ...
# $ DestAirportID     : int  13487 12953 10397 10397 10397 15304 11193 12264 10397 14771 ...
# $ DestAirportSeqID  : int  1348702 1295302 1039705 1039705 1039705 1530402 1119302 1226402 1039705 1477101 ...
# $ DestCityMarketID  : int  31650 31703 30397 30397 30397 33195 33105 30852 30397 32457 ...
# $ Dest              : chr  "MSP" "LGA" "ATL" "ATL" ...
# $ DestCityName      : chr  "Minneapolis, MN" "New York, NY" "Atlanta, GA" "Atlanta, GA" ...
# $ DestState         : chr  "MN" "NY" "GA" "GA" ...
# $ DestStateFips     : chr  "27" "36" "13" "13" ...
# $ DestStateName     : chr  "Minnesota" "New York" "Georgia" "Georgia" ...
# $ DestWac           : int  63 22 34 34 34 33 52 38 34 91 ...
# $ CRSDepTime        : chr  "0805" "0704" "1700" "0940" ...
# $ DepTime           : chr  "0758" "0657" "1657" "0953" ...
# $ DepDelay          : int  -7 -7 -3 13 -4 -4 -12 34 11 -2 ...
# $ DepDelayMinutes   : int  0 0 0 13 0 0 0 34 11 0 ...
# $ DepDel15          : int  0 0 0 0 0 0 0 1 0 0 ...
# $ TaxiOut           : int  24 17 50 49 23 31 13 10 29 32 ...
# $ TaxiIn            : int  4 18 13 17 10 3 5 7 8 9 ...
# $ CRSArrTime        : chr  "1015" "0959" "1943" "1521" ...
# $ ArrTime           : chr  "1002" "0949" "2007" "1604" ...
# $ ArrDelay          : int  -13 -10 24 43 -17 3 -19 13 19 -13 ...
# $ ArrDelayMinutes   : int  0 0 24 43 0 3 0 13 19 0 ...
# $ ArrDel15          : int  0 0 1 1 0 0 0 0 1 0 ...
# $ Cancelled         : int  0 0 0 0 0 0 0 0 0 0 ...
# $ CancellationCode  : chr  "" "" "" "" ...
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
# - attr(*, ".internal.selfref")=<externalptr> 

summary(vuelos)


##### 1.4. Bloque de tratamiento de variables #####

library(dplyr)

#### 1.4.1. Transformación de variables a factor  ####

vuelos$Year = as.factor(vuelos$Year)
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
vuelos$Origin = as.factor(vuelos$Origin)
vuelos$OriginCityName = as.factor(vuelos$OriginCityName)
vuelos$OriginState = as.factor(vuelos$OriginState)
vuelos$OriginStateFips = as.factor(vuelos$OriginStateFips)
vuelos$OriginStateName = as.factor(vuelos$OriginStateName)
vuelos$OriginWac = as.factor(vuelos$OriginWac)
vuelos$DestAirportID = as.factor(vuelos$DestAirportID)
vuelos$DestAirportSeqID = as.factor(vuelos$DestAirportSeqID)
vuelos$DestCityMarketID = as.factor(vuelos$DestCityMarketID)
vuelos$Dest = as.factor(vuelos$Dest)
vuelos$DestCityName = as.factor(vuelos$DestCityName)
vuelos$DestState = as.factor(vuelos$DestState)
vuelos$DestStateFips = as.factor(vuelos$DestStateFips)
vuelos$DestStateName = as.factor(vuelos$DestStateName)
vuelos$DestWac = as.factor(vuelos$DestWac)
vuelos$DepDel15 = as.factor(vuelos$DepDel15)
vuelos$ArrDel15 = as.factor(vuelos$ArrDel15)
vuelos$Cancelled = as.factor(vuelos$Cancelled)
vuelos$CancellationCode = as.factor(vuelos$CancellationCode)
vuelos$Flights = as.factor(vuelos$Flights)

# Reviso por si me he dejado alguna variable sin transformar a factor
summary(vuelos)

dfFactorizado <- vuelos
#rm(dfFactorizado)

#### 1.4.2. Transformación de variables a fecha  ####

library(lubridate)

vuelos$FlightDate = ymd(vuelos$FlightDate)
str(vuelos$FlightDate)

# Realizo una copia del DF en este momento por si tuviese que volver a recuperarlo
dfTratadoFD <- vuelos


#### 1.4.3. Transformación de variables de tipo hora  ####

vuelos$CRSDepTime = format(strptime(vuelos$DepTime, format = "%H%M"), format = "%H:%M")
str(vuelos$CRSDepTime)
vuelos$DepTime = format(strptime(vuelos$DepTime, format = "%H%M"), format = "%H:%M")
str(vuelos$DepTime)
vuelos$CRSArrTime = format(strptime(vuelos$CRSArrTime, format = "%H%M"), format = "%H:%M")
str(vuelos$CRSArrTime)
vuelos$ArrTime = format(strptime(vuelos$ArrTime, format = "%H%M"), format = "%H:%M")
str(vuelos$ArrTime)

# Reviso por si me he dejado alguna variable sin transformar
summary(vuelos)

# Realizo una copia del DF por si tuviese que volver a recuperarlo
dfTratadoFDH <- vuelos

#### 1.4.4. Limpieza de datos no significativos  ####
# Procedo a eliminar los datos que no ofrecen ningún tipo de información para el análisis de la 
# predicción. Para ello visualizo tanto las variables que tienen campos nulos como los que no 
vuelos[,colSums(is.na(vuelos))>0]
#            Year            Quarter              Month         DayofMonth          DayOfWeek         FlightDate      UniqueCarrier          AirlineID 
#           FALSE              FALSE              FALSE              FALSE              FALSE              FALSE              FALSE              FALSE 
#         Carrier          FlightNum    OriginAirportID OriginAirportSeqID OriginCityMarketID             Origin     OriginCityName        OriginState 
#           FALSE              FALSE              FALSE              FALSE              FALSE              FALSE              FALSE              FALSE 
# OriginStateFips    OriginStateName          OriginWac      DestAirportID   DestAirportSeqID   DestCityMarketID               Dest       DestCityName 
#           FALSE              FALSE              FALSE              FALSE              FALSE              FALSE              FALSE              FALSE 
#       DestState      DestStateFips      DestStateName            DestWac         CRSDepTime            DepTime           DepDelay    DepDelayMinutes 
#           FALSE              FALSE              FALSE              FALSE               TRUE               TRUE               TRUE               TRUE 
#        DepDel15            TaxiOut             TaxiIn         CRSArrTime            ArrTime           ArrDelay    ArrDelayMinutes           ArrDel15 
#            TRUE               TRUE               TRUE              FALSE               TRUE               TRUE               TRUE               TRUE 
#       Cancelled   CancellationCode     CRSElapsedTime  ActualElapsedTime            AirTime            Flights           Distance       CarrierDelay 
#           FALSE              FALSE               TRUE               TRUE               TRUE              FALSE              FALSE               TRUE 
#    WeatherDelay           NASDelay      SecurityDelay  LateAircraftDelay 
#           TRUE                TRUE               TRUE               TRUE
# Los variables que contienen campos TRUE, informan que hay filas nulas(NA), las cuales no 
# contienen ningún tipo de información.

# Elimino los registros relacionados con la hora programada de despegue (CRSDepTime) que no contienen 
# ningún tipo de información, es decir, son campos vacíos que corresponden a vuelos 
# cancelados. Estos no son de utilidad para analizar y predecir los retrasos en los vuelos puesto
# que son operaciones no realizadas. Un vuelo está cancelado cuando en su campo indica un 1, y un 0
# cuando el vuelo no ha sido cancelado
vuelos %>% 
  filter(is.na(CRSDepTime), Cancelled == 1) %>% 
  nrow() # [1] 91681

# Guardo los vuelos cancelados en un dataframe aparte
Cancelados <- vuelos %>% filter(is.na(CRSDepTime))

# Elimino del dataframe vuelos los vuelos cancelados
library(tidyr)
vuelos <- vuelos %>% 
  drop_na(CRSDepTime)

# Respectivamente si un vuelo no ha sido cancelado, el campo Cancellation_Code estará vacío, es 
# decir, no contrendrá un código informativo, con lo cual el campo será nulo. Por tanto, podemos 
# prescindir de las variables Cancelled y CancellationCode
vuelos$Cancelled <- NULL
vuelos$CancellationCode <- NULL

# Hacemos una copia del dataframe sin los cancelados
dfSinCan <- vuelos

# Vuelvo a visualizar las variables con campos nulos
vuelos[,colSums(is.na(vuelos))>0]
#              Year            Quarter              Month         DayofMonth          DayOfWeek         FlightDate      UniqueCarrier 
#             FALSE              FALSE              FALSE              FALSE              FALSE              FALSE              FALSE 
#         AirlineID            Carrier          FlightNum    OriginAirportID OriginAirportSeqID OriginCityMarketID             Origin 
#             FALSE              FALSE              FALSE              FALSE              FALSE              FALSE              FALSE 
#    OriginCityName        OriginState    OriginStateFips    OriginStateName          OriginWac      DestAirportID   DestAirportSeqID 
#             FALSE              FALSE              FALSE              FALSE              FALSE              FALSE              FALSE 
#  DestCityMarketID               Dest       DestCityName          DestState      DestStateFips      DestStateName            DestWac 
#             FALSE              FALSE              FALSE              FALSE              FALSE              FALSE              FALSE 
#        CRSDepTime            DepTime           DepDelay    DepDelayMinutes           DepDel15            TaxiOut             TaxiIn 
#             FALSE              FALSE              FALSE              FALSE              FALSE               TRUE               TRUE 
#        CRSArrTime            ArrTime           ArrDelay    ArrDelayMinutes           ArrDel15     CRSElapsedTime  ActualElapsedTime 
#             FALSE               TRUE               TRUE               TRUE               TRUE               TRUE               TRUE 
#           AirTime            Flights           Distance       CarrierDelay       WeatherDelay           NASDelay      SecurityDelay 
#              TRUE              FALSE              FALSE               TRUE               TRUE               TRUE               TRUE 
# LateAircraftDelay 
#              TRUE
# El siguiente objetivo es la variable nula ArrTime tenga un valor temporal y que las 
# campos nulos de las variables ArrDelay, ArrDelayMinutes y ArrDel15 tengan valor o valor 0,
# en función del valor que se cumplimente en ArrTime.
# Como no conocemos la hora estimada de llegada del vuelo, suponemos que llegó en hora, para los
# campos NA de ArrTime copiamos el valor que tiene en CRSArrTime, que es la hora programada de
# llegada del vuelo. 
vuelos <- vuelos %>%
  mutate(ArrTime = coalesce(ArrTime, CRSArrTime))
vuelos %>% 
  filter(is.na(ArrTime)) %>% 
  nrow() # [1] 0
# Para la variable ArrDelay deberíamos calcular su valor, restandole los tiempos ArrTime-CRSArrTime
# pero como no vamos a hacer uso de estas variables para tener en cuenta para el retraso de la 
# salida en el origen, los eliminamos del dataset (Revisar)
# vuelos %>% 
#   filter(is.na(ArrDelay)) %>% 
#   nrow() # [1] 18491
# # Casi está, para que calcule bien los cambios de día, es necesario tener en cuenta también la 
# # fecha de FlightsDate
# vuelos %>% 
#   filter(is.na(ArrDelay)) %>% 
#   mutate(ArrDelay = coalesce(hour(hm(ArrTime) - hm(CRSArrTime))*60+minute(hm(ArrTime) - hm(CRSArrTime)))) %>% 
#   View()
# vuelos <- vuelos %>%
#   mutate(ArrDelay = coalesce(ArrDelay, -ArrTime))
# vuelos <- vuelos %>%
#   mutate(ArrDelayMinutes = coalesce(ArrDelayMinutes, 0L))
# vuelos <- vuelos %>%
#   mutate(ArrDel15 = coalesce(ArrDel15, 0L))

# El último paso es que los campos nulos de las variables CarrierDelay, WeatherDelay, NASDelay, 
# SecurityDelay y LateAircraftDelay tengan valor 0
vuelos <- vuelos %>% 
  mutate(CarrierDelay = coalesce(CarrierDelay, 0L))
vuelos <- vuelos %>% 
  mutate(WeatherDelay = coalesce(WeatherDelay, 0L))
vuelos <- vuelos %>% 
  mutate(NASDelay = coalesce(NASDelay, 0L))
vuelos <- vuelos %>% 
  mutate(SecurityDelay = coalesce(SecurityDelay, 0L))
vuelos <- vuelos %>% 
  mutate(LateAircraftDelay = coalesce(LateAircraftDelay, 0L))

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