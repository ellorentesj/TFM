#### TFM: ANÁLISIS Y PREDICCIÓN DEL RESTRASO EN LOS VUELOS ####

#### 1. DATOS DE ENTRADA: Análisis y Exploración del dataset de entrada ####

# Los datos de los vuelos se obtienen de: https://www.transtats.bts.gov/Tables.asp?DB_ID=120


# *************************************************************************************************
##### 1.1. Bloque de carga de librerias #####

list.of.packages <- c("data.table", "dplyr", "tidyr","lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(data.table)
library(dplyr)
library(lubridate)
library(tidyr)

# *************************************************************************************************


# *************************************************************************************************
##### 1.2. Bloque de carga de datos #####

# Selección de ruta, en mi caso: "/Users/ellorentesj/repostGitHub/TFM/data/"
setwd("/Users/ellorentesj/repostGitHub/TFM/data/")

vuelos <- fread("vuelos.csv", header=T, sep=',')
class(vuelos) # [1] "data.table" "data.frame"
# *************************************************************************************************

# Realizo una copia por si tuviese que recuperarla
# dfCompleto <- vuelos

# *************************************************************************************************
##### 1.3. Bloque de revisión basica del dataset #####
str(vuelos)
summary(vuelos)
# *************************************************************************************************


# *************************************************************************************************
##### 1.4. Bloque de tratamiento de variables #####

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
vuelos$DestAirportID = as.factor(vuelos$DestAirportID)
vuelos$DestAirportSeqID = as.factor(vuelos$DestAirportSeqID)
vuelos$DestCityMarketID = as.factor(vuelos$DestCityMarketID)
vuelos$Dest = as.factor(vuelos$Dest)
vuelos$DestCityName = as.factor(vuelos$DestCityName)
vuelos$DestState = as.factor(vuelos$DestState)
vuelos$DepDel15 = as.factor(vuelos$DepDel15)
vuelos$ArrDel15 = as.factor(vuelos$ArrDel15)
vuelos$Cancelled = as.factor(vuelos$Cancelled)
vuelos$CancellationCode = as.factor(vuelos$CancellationCode)
# vuelos$Flights = as.factor(vuelos$Flights)

# Reviso por si me he dejado alguna variable sin transformar a factor
summary(vuelos)

# Realizo una copia del dataset
# dfFactorizado <- vuelos

#### 1.4.2. Transformación de variables a fecha  ####

vuelos$FlightDate = ymd(vuelos$FlightDate)
str(vuelos$FlightDate)

# Realizo una copia del DF en este momento por si tuviese que volver a recuperarlo
# dfTratadoFD <- vuelos

#### 1.4.3. Transformación de variables de tipo hora  ####

vuelos$DepTime = format(strptime(vuelos$DepTime, format = "%H%M"), format = "%H:%M")
str(vuelos$DepTime)
vuelos$ArrTime = format(strptime(vuelos$ArrTime, format = "%H%M"), format = "%H:%M")
str(vuelos$ArrTime)

# Reviso por si me he dejado alguna variable sin transformar
summary(vuelos)

# Realizo una copia del DF por si tuviese que volver a recuperarlo
# dfTratadoFDH <- vuelos

#### 1.4.4. Limpieza de datos no significativos  ####

# Procedo a eliminar los datos que no ofrecen ningún tipo de información para el análisis de la 
# predicción. Para ello visualizo tanto las variables que tienen campos nulos como los que no 
colSums(is.na(vuelos))>0

### DEPTIME ###
vuelos %>% 
  filter(is.na(DepTime)) %>% 
  nrow() # [1] 44553
# Elimino los registros relacionados con la hora de despegue (DepTime) que no contienen ningún 
# tipo de información, es decir, son campos vacíos que corresponden a vuelos cancelados. Estos no 
# son de utilidad para analizar y predecir los retrasos en los vuelos puesto que son operaciones no
# realizadas. Un vuelo está cancelado cuando en su campo indica un 1, y un 0 cuando el vuelo no ha
# sido cancelado
vuelos %>% 
  filter(is.na(DepTime), Cancelled == 1) %>% 
  nrow() # [1] 44553
# Guardo los vuelos cancelados en un dataframe aparte
Cancelados <- vuelos %>% filter(is.na(DepTime))
# Elimino del dataframe vuelos los vuelos cancelados
vuelos <- vuelos %>% 
  drop_na(DepTime)
### DEPTIME ###

### CANCELLED & CANCELLATIONCODE ###
# Respectivamente si un vuelo no ha sido cancelado, el campo Cancellation_Code estará vacío, es 
# decir, no contrendrá un código informativo, con lo cual el campo será nulo. Por tanto, podemos 
# prescindir de las variables Cancelled y CancellationCode
vuelos$Cancelled <- NULL
vuelos$CancellationCode <- NULL
### CANCELLED & CANCELLATIONCODE ###

colSums(is.na(vuelos))>0

# Hacemos una copia del dataframe sin los cancelados
# dfSinCan <- vuelos

### TAXIOUT, TAXIIN, ARRDELAY, ARRDELAYMINUTES, ARRDEL15, CARRIERDELAY, WEATHERDELAY, NASDELAY, 
# SECURITYDELAY, LATEAIRCRAFTDELAY ###
vuelos %>% 
  filter(is.na(TaxiOut)) %>% 
  nrow() # [1] 723
vuelos %>% 
  filter(is.na(TaxiIn)) %>% 
  nrow() # [1] 1984
vuelos %>% 
  filter(is.na(ArrDelay)) %>% 
  nrow() # [1] 3679
vuelos %>% 
  filter(is.na(ArrDelayMinutes)) %>% 
  nrow() # [1] 3679
vuelos %>% 
  filter(is.na(ArrDel15)) %>% 
  nrow() # [1] 3679
vuelos %>% 
  filter(is.na(CarrierDelay)) %>% 
  nrow() # [1] 679183
vuelos %>% 
  filter(is.na(WeatherDelay)) %>% 
  nrow() # [1] 679183
vuelos %>% 
  filter(is.na(NASDelay)) %>% 
  nrow() # [1] 679183
vuelos %>% 
  filter(is.na(SecurityDelay)) %>% 
  nrow() # [1] 679183
vuelos %>% 
  filter(is.na(LateAircraftDelay)) %>% 
  nrow() # [1] 679183
# El siguiente objetivo es hacer que las variables TaxiOut, TaxiIn, ArrDelay, ArrDelayMinutes, 
# ArrDel15, CRSElapsedTime, ActualElapsedTime, AirTime, CarrierDelay, WeatherDelay, NASDelay, 
# SecurityDelay, LateAircraftDelay que contienen campos nulos, es hacer que contengan algún tipo de
# información ya que se requieren para poder analizar los retrasos en los vuelos. Se procede a 
# cambiar los campos nulos(NA) de estas variables por valores con el número 0.
vuelos <- vuelos %>% 
  mutate(TaxiOut = coalesce(TaxiOut,0L))
vuelos <- vuelos %>% 
  mutate(TaxiIn = coalesce(TaxiIn,0L))
vuelos <- vuelos %>% 
  mutate(ArrDelay = coalesce(ArrDelay,0L))
vuelos <- vuelos %>% 
  mutate(ArrDelayMinutes = coalesce(ArrDelayMinutes,0L))
vuelos <- vuelos %>% 
  mutate(ArrDel15 = coalesce(ArrDel15,0L))
vuelos <- vuelos %>%
  mutate(CarrierDelay = coalesce(CarrierDelay,0L))
vuelos <- vuelos %>% 
  mutate(WeatherDelay = coalesce(WeatherDelay,0L))
vuelos <- vuelos %>% 
  mutate(NASDelay = coalesce(NASDelay,0L))
vuelos <- vuelos %>% 
  mutate(SecurityDelay = coalesce(SecurityDelay,0L))
vuelos <- vuelos %>% 
  mutate(LateAircraftDelay = coalesce(LateAircraftDelay,0L))
### TAXIOUT, TAXIIN, ARRDELAY, ARRDELAYMINUTES, ARRDEL15, CARRIERDELAY, WEATHERDELAY, NASDELAY, 
# SECURITYDELAY, LATEAIRCRAFTDELAY ###

# Realizo copia 
# dfCasiLimpio <- vuelos

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
vuelos %>% 
  filter(ArrDelay>0, (CarrierDelay + WeatherDelay + NASDelay + SecurityDelay + LateAircraftDelay) == 0) %>% 
  nrow() # [1] 195719
# Añado una columna donde se sumen las variables con retraso 
totalDelay <- vuelos$CarrierDelay+vuelos$WeatherDelay+vuelos$NASDelay+vuelos$SecurityDelay+vuelos$LateAircraftDelay
# La añaado al dataset
vuelos <- cbind(vuelos, totalDelay)
# Modifico los CarrierDelay donde ArrDelay > 0 & (CarrierDelay + WeatherDelay + NASDelay + SecurityDelay + LateAircraftDelay) == 0
vuelos[vuelos$ArrDelay>0 & vuelos$totalDelay==0,"CarrierDelay"] <- vuelos[vuelos$ArrDelay>0 & vuelos$totalDelay == 0, "ArrDelay"]
### ARRDELAY ###

### ARRTIME ###
vuelos %>% 
  filter(is.na(ArrTime)) %>% 
  nrow() # [1] 1984
# Se podría intentar calcular la hora de llegada por el número de vuelo y el destino, pero hay que
# tener en cuenta el tiempo de vueloy el cambio de hora, como tampoco tenemos la información de 
# estos retrasos, asignamos a estos vuelos por defecto las "00:00"
vuelos <- vuelos %>% 
  mutate(ArrTime = coalesce(ArrTime,"00:00"))
### ARRTIME ###

# Reviso que no quede ninguna variable más por limpiar
colSums(is.na(vuelos))>0
# *************************************************************************************************

# Guardo el dataset
write.table(vuelos, file = "VuelosFinales.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)

# Top 3 de aeropuertos que tienen más vuelos retrasados, 
vuelos %>% 
  group_by(Origin) %>% 
  summarise(totalDel15 = sum(DepDel15==1), ProRetDel = (sum(DepDel15==1)/nrow(vuelos))*100, totalArrDel15 = sum(ArrDel15==1),
            ProRetArr = (sum(ArrDel15==1)/nrow(vuelos)*100)) %>% 
  arrange(-totalArrDel15) %>% 
  top_n(3)
#   Origin totalDel15 ProRetDel totalArrDel15  ProRetArr
#   <fct>       <int>     <dbl>         <int>      <dbl>
# 1 ORD         16437      1.74         17119       1.81
# 2 ATL         17053      1.81         15283       1.62
# 3 DEN         14102      1.49         13929       1.48

# ranking de retrasos por meses
vuelos %>% 
  group_by(Month) %>% 
  summarise(retSal = sum(DepDel15==1,na.rm = T), retLle = sum(ArrDel15==1, na.rm = T)) %>% 
  arrange(-retLle) 
head(vuelos$ArrDel15)
#   Month retSal retLle
#   <fct>  <int>  <int>
# 1 12    141585 144958
# 2 1     118275 119994