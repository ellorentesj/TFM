#### TFM: ANÁLISIS Y PREDICCIÓN DEL RESTRASO EN LOS VUELOS ####

#### 1. DATOS DE ENTRADA: Análisis y Exploración del dataset de entrada ####

# Los datos de los vuelos se obtienen de: https://www.transtats.bts.gov/Tables.asp?DB_ID=120


# *************************************************************************************************
##### 1.1. Bloque de carga de librerias #####

list.of.packages <- c("data.table", "dplyr", "tidyr","lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# *************************************************************************************************


# *************************************************************************************************
##### 1.2. Bloque de carga de datos #####

# Selección de ruta, en mi caso: "/Users/ellorentesj/Desktop/TFM/"
setwd("/Users/ellorentesj/Desktop/TFM/data/")

library(data.table)

vuelos <- fread("vuelos.csv", header=T, sep=',')
class(vuelos)
#weather <- read.table("weather.csv",header=T,sep=',')
# *************************************************************************************************


# Realizo una copia por si tuviese que recuperarla
dfCompleto <- vuelos


# *************************************************************************************************
##### 1.3. Bloque de revisión basica del dataset #####

str(vuelos)
# Classes ‘data.table’ and 'data.frame':	4655421 obs. of  49 variables:
# $ Year              : int  2013 2013 2013 2013 2013 2013 2013 2013 2013 2013 ...
# $ Month             : int  6 6 6 6 6 6 6 6 6 6 ...
# $ DayofMonth        : int  22 23 24 25 26 27 28 29 30 12 ...
# $ DayOfWeek         : int  6 7 1 2 3 4 5 6 7 3 ...
# $ FlightDate        : chr  "2013-06-22" "2013-06-23" "2013-06-24" "2013-06-25" ...
# $ UniqueCarrier     : chr  "MQ" "MQ" "MQ" "MQ" ...
# $ AirlineID         : int  20398 20398 20398 20398 20398 20398 20398 20398 20398 20398 ...
# $ Carrier           : chr  "MQ" "MQ" "MQ" "MQ" ...
# $ FlightNum         : chr  "3189" "3189" "3189" "3189" ...
# $ OriginAirportID   : int  14576 14576 14576 14576 14576 14576 14576 14576 14576 10693 ...
# $ OriginAirportSeqID: int  1457604 1457604 1457604 1457604 1457604 1457604 1457604 1457604 1457604 1069302 ...
# $ OriginCityMarketID: int  34576 34576 34576 34576 34576 34576 34576 34576 34576 30693 ...
# $ Origin            : chr  "ROC" "ROC" "ROC" "ROC" ...
# $ OriginCityName    : chr  "Rochester, NY" "Rochester, NY" "Rochester, NY" "Rochester, NY" ...
# $ OriginState       : chr  "NY" "NY" "NY" "NY" ...
# $ OriginStateFips   : chr  "36" "36" "36" "36" ...
# $ OriginStateName   : chr  "New York" "New York" "New York" "New York" ...
# $ OriginWac         : int  22 22 22 22 22 22 22 22 22 54 ...
# $ DestAirportID     : int  13930 13930 13930 13930 13930 13930 13930 13930 13930 13930 ...
# $ DestAirportSeqID  : int  1393002 1393002 1393002 1393002 1393002 1393002 1393002 1393002 1393002 1393002 ...
# $ DestCityMarketID  : int  30977 30977 30977 30977 30977 30977 30977 30977 30977 30977 ...
# $ Dest              : chr  "ORD" "ORD" "ORD" "ORD" ...
# $ DestCityName      : chr  "Chicago, IL" "Chicago, IL" "Chicago, IL" "Chicago, IL" ...
# $ DestState         : chr  "IL" "IL" "IL" "IL" ...
# $ DestStateFips     : chr  "17" "17" "17" "17" ...
# $ DestStateName     : chr  "Illinois" "Illinois" "Illinois" "Illinois" ...
# $ DestWac           : int  41 41 41 41 41 41 41 41 41 41 ...
# $ DepTime           : chr  "1522" "1504" "1522" "1740" ...
# $ DepDelay          : int  17 -1 17 155 -3 -2 104 89 -6 15 ...
# $ DepDelayMinutes   : int  17 0 17 155 0 0 104 89 0 15 ...
# $ DepDel15          : int  1 0 1 1 0 0 1 1 0 1 ...
# $ TaxiOut           : int  8 12 9 9 20 10 15 10 13 11 ...
# $ TaxiIn            : int  13 7 14 26 6 34 21 3 29 6 ...
# $ ArrTime           : chr  "1603" "1540" "1605" "1837" ...
# $ ArrDelay          : int  3 -20 5 157 -11 21 118 73 2 6 ...
# $ ArrDelayMinutes   : int  3 0 5 157 0 21 118 73 2 6 ...
# $ ArrDel15          : int  0 0 0 1 0 1 1 1 0 0 ...
# $ Cancelled         : int  0 0 0 0 0 0 0 0 0 0 ...
# $ CancellationCode  : chr  "" "" "" "" ...
# $ CRSElapsedTime    : int  115 115 115 115 115 115 115 115 115 100 ...
# $ ActualElapsedTime : int  101 96 103 117 107 138 129 99 123 91 ...
# $ AirTime           : int  80 77 80 82 81 94 93 86 81 74 ...
# $ Flights           : int  1 1 1 1 1 1 1 1 1 1 ...
# $ Distance          : int  528 528 528 528 528 528 528 528 528 409 ...
# $ CarrierDelay      : int  NA NA NA 0 NA 0 0 51 NA NA ...
# $ WeatherDelay      : int  NA NA NA 0 NA 0 0 0 NA NA ...
# $ NASDelay          : int  NA NA NA 20 NA 21 38 0 NA NA ...
# $ SecurityDelay     : int  NA NA NA 0 NA 0 0 0 NA NA ...
# $ LateAircraftDelay : int  NA NA NA 137 NA 0 80 22 NA NA ...
# - attr(*, ".internal.selfref")=<externalptr

summary(vuelos)
# Year          Month          DayofMonth      DayOfWeek      FlightDate        UniqueCarrier        AirlineID       Carrier         
# Min.   :2013   Min.   : 1.000   Min.   : 1.00   Min.   :1.000   Length:4655421     Length:4655421     Min.   :19393   Length:4655421    
# 1st Qu.:2013   1st Qu.: 6.000   1st Qu.: 8.00   1st Qu.:2.000   Class :character   Class :character   1st Qu.:19790   Class :character  
# Median :2013   Median : 8.000   Median :16.00   Median :4.000   Mode  :character   Mode  :character   Median :19977   Mode  :character  
# Mean   :2013   Mean   : 7.483   Mean   :15.69   Mean   :3.935                                         Mean   :20029                     
# 3rd Qu.:2013   3rd Qu.:10.000   3rd Qu.:23.00   3rd Qu.:6.000                                         3rd Qu.:20366                     
# Max.   :2014   Max.   :12.000   Max.   :31.00   Max.   :7.000                                         Max.   :21171                     
# 
# FlightNum         OriginAirportID OriginAirportSeqID OriginCityMarketID    Origin          OriginCityName     OriginState        OriginStateFips   
# Length:4655421     Min.   :10135   Min.   :1013503    Min.   :30070      Length:4655421     Length:4655421     Length:4655421     Length:4655421    
# Class :character   1st Qu.:11292   1st Qu.:1129202    1st Qu.:30647      Class :character   Class :character   Class :character   Class :character  
# Mode  :character   Median :12889   Median :1288903    Median :31453      Mode  :character   Mode  :character   Mode  :character   Mode  :character  
# Mean   :12650   Mean   :1265036    Mean   :31721                                                                                 
# 3rd Qu.:13930   3rd Qu.:1393003    3rd Qu.:32467                                                                                 
# Max.   :16218   Max.   :1621801    Max.   :35991                                                                                 
# 
# OriginStateName      OriginWac     DestAirportID   DestAirportSeqID  DestCityMarketID     Dest           DestCityName        DestState        
# Length:4655421     Min.   : 1.00   Min.   :10135   Min.   :1013503   Min.   :30070    Length:4655421     Length:4655421     Length:4655421    
# Class :character   1st Qu.:34.00   1st Qu.:11292   1st Qu.:1129202   1st Qu.:30647    Class :character   Class :character   Class :character  
# Mode  :character   Median :52.00   Median :12889   Median :1288903   Median :31453    Mode  :character   Mode  :character   Mode  :character  
# Mean   :55.64   Mean   :12650   Mean   :1265048   Mean   :31721                                                            
# 3rd Qu.:81.00   3rd Qu.:13930   3rd Qu.:1393003   3rd Qu.:32467                                                            
# Max.   :93.00   Max.   :16218   Max.   :1621801   Max.   :35991                                                            
# 
# DestStateFips      DestStateName         DestWac        DepTime             DepDelay       DepDelayMinutes      DepDel15         TaxiOut      
# Length:4655421     Length:4655421     Min.   : 1.00   Length:4655421     Min.   :-112.00   Min.   :   0.00   Min.   :0.00     Min.   :  1.00  
# Class :character   Class :character   1st Qu.:34.00   Class :character   1st Qu.:  -5.00   1st Qu.:   0.00   1st Qu.:0.00     1st Qu.: 10.00  
# Mode  :character   Mode  :character   Median :52.00   Mode  :character   Median :  -1.00   Median :   0.00   Median :0.00     Median : 13.00  
# Mean   :55.64                      Mean   :  11.15   Mean   :  13.72   Mean   :0.22     Mean   : 15.65  
# 3rd Qu.:81.00                      3rd Qu.:  11.00   3rd Qu.:  11.00   3rd Qu.:0.00     3rd Qu.: 18.00  
# Max.   :93.00                      Max.   :1975.00   Max.   :1975.00   Max.   :1.00     Max.   :233.00  
# NA's   :103522    NA's   :103522    NA's   :103522   NA's   :106184  
#     TaxiIn         ArrTime             ArrDelay       ArrDelayMinutes     ArrDel15        Cancelled     CancellationCode   CRSElapsedTime 
# Min.   :  1.00   Length:4655421     Min.   :-112.00   Min.   :   0     Min.   :0.00     Min.   :0.000   Length:4655421     Min.   : 19.0  
# 1st Qu.:  4.00   Class :character   1st Qu.: -11.00   1st Qu.:   0     1st Qu.:0.00     1st Qu.:0.000   Class :character   1st Qu.: 81.0  
# Median :  6.00   Mode  :character   Median :  -3.00   Median :   0     Median :0.00     Median :0.000   Mode  :character   Median :114.0  
# Mean   :  6.95                      Mean   :   7.65   Mean   :  14     Mean   :0.22     Mean   :0.023                      Mean   :133.5  
# 3rd Qu.:  8.00                      3rd Qu.:  11.00   3rd Qu.:  11     3rd Qu.:0.00     3rd Qu.:0.000                      3rd Qu.:164.0  
# Max.   :346.00                      Max.   :1983.00   Max.   :1983     Max.   :1.00     Max.   :1.000                      Max.   :670.0  
# NA's   :109339                      NA's   :118463    NA's   :118463   NA's   :118463                                      NA's   :5      
# ActualElapsedTime    AirTime          Flights     Distance     CarrierDelay      WeatherDelay        NASDelay       SecurityDelay     LateAircraftDelay
# Min.   : 11.0     Min.   :  5.0    Min.   :1   Min.   :  17   Min.   :   0      Min.   :   0      Min.   :   0      Min.   :  0       Min.   :   0     
# 1st Qu.: 78.0     1st Qu.: 57.0    1st Qu.:1   1st Qu.: 345   1st Qu.:   0      1st Qu.:   0      1st Qu.:   0      1st Qu.:  0       1st Qu.:   0     
# Median :111.0     Median : 88.0    Median :1   Median : 599   Median :   2      Median :   0      Median :   2      Median :  0       Median :   8     
# Mean   :130.4     Mean   :107.8    Mean   :1   Mean   : 771   Mean   :  17      Mean   :   3      Mean   :  13      Mean   :  0       Mean   :  25     
# 3rd Qu.:160.0     3rd Qu.:137.0    3rd Qu.:1   3rd Qu.: 997   3rd Qu.:  17      3rd Qu.:   0      3rd Qu.:  17      3rd Qu.:  0       3rd Qu.:  32     
# Max.   :775.0     Max.   :688.0    Max.   :1   Max.   :4983   Max.   :1975      Max.   :1451      Max.   :1287      Max.   :573       Max.   :1437     
# NA's   :118463    NA's   :118463                              NA's   :3654812   NA's   :3654812   NA's   :3654812   NA's   :3654812   NA's   :3654812 
# *************************************************************************************************


# *************************************************************************************************
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

# Realizo una copia del dataset
# dfFactorizado <- vuelos

#### 1.4.2. Transformación de variables a fecha  ####

library(lubridate)

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
#              Year              Month         DayofMonth          DayOfWeek         FlightDate      UniqueCarrier          AirlineID            Carrier 
#             FALSE              FALSE              FALSE              FALSE              FALSE              FALSE              FALSE              FALSE 
#         FlightNum    OriginAirportID OriginAirportSeqID OriginCityMarketID             Origin     OriginCityName        OriginState    OriginStateFips 
#             FALSE              FALSE              FALSE              FALSE              FALSE              FALSE              FALSE              FALSE 
#   OriginStateName          OriginWac      DestAirportID   DestAirportSeqID   DestCityMarketID               Dest       DestCityName          DestState 
#             FALSE              FALSE              FALSE              FALSE              FALSE              FALSE              FALSE              FALSE 
#     DestStateFips      DestStateName            DestWac            DepTime           DepDelay    DepDelayMinutes           DepDel15            TaxiOut 
#             FALSE              FALSE              FALSE               TRUE               TRUE               TRUE               TRUE               TRUE 
#            TaxiIn            ArrTime           ArrDelay    ArrDelayMinutes           ArrDel15          Cancelled   CancellationCode     CRSElapsedTime 
#              TRUE               TRUE               TRUE               TRUE               TRUE              FALSE              FALSE               TRUE 
# ActualElapsedTime            AirTime            Flights           Distance       CarrierDelay       WeatherDelay           NASDelay      SecurityDelay 
#              TRUE               TRUE              FALSE              FALSE               TRUE               TRUE               TRUE               TRUE 
# LateAircraftDelay 
#              TRUE 
# Los variables que contienen campos TRUE, informan que hay filas nulas(NA), las cuales no 
# contienen ningún tipo de información.

### DEPTIME ###
vuelos %>% 
  filter(is.na(DepTime)) %>% 
  nrow() # [1] 103522
# Elimino los registros relacionados con la hora de despegue (DepTime) que no contienen ningún 
# tipo de información, es decir, son campos vacíos que corresponden a vuelos cancelados. Estos no 
# son de utilidad para analizar y predecir los retrasos en los vuelos puesto que son operaciones no
# realizadas. Un vuelo está cancelado cuando en su campo indica un 1, y un 0 cuando el vuelo no ha
# sido cancelado
vuelos %>% 
  filter(is.na(DepTime), Cancelled == 1) %>% 
  nrow() # [1] 103522
# Guardo los vuelos cancelados en un dataframe aparte
Cancelados <- vuelos %>% filter(is.na(DepTime))
# Elimino del dataframe vuelos los vuelos cancelados
library(tidyr)
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

# Hacemos una copia del dataframe sin los cancelados
# dfSinCan <- vuelos

### TAXIOUT, TAXIIN, ARRDELAY, ARRDELAYMINUTES, ARRDEL15, CRSELAPSEDTIME, ACTUALELAPSEDTIME, AIRTIME, CARRIERDELAY, WEATHERDELAY, NASDELAY, SECURITYDELAY, LATEAIRCRAFTDELAY ###
vuelos %>% 
  filter(is.na(TaxiOut)) %>% 
  nrow() # [1] 2662
vuelos %>% 
  filter(is.na(TaxiIn)) %>% 
  nrow() # [1] 5817
vuelos %>% 
  filter(is.na(ArrDelay)) %>% 
  nrow() # [1] 14941
vuelos %>% 
  filter(is.na(ArrDelayMinutes)) %>% 
  nrow() # [1] 14941
vuelos %>% 
  filter(is.na(ArrDel15)) %>% 
  nrow() # [1] 14941
vuelos %>% 
  filter(is.na(CRSElapsedTime)) %>% 
  nrow() # [1] 4
vuelos %>% 
  filter(is.na(ActualElapsedTime)) %>% 
  nrow() # [1] 14941
vuelos %>% 
  filter(is.na(AirTime)) %>% 
  nrow() # [1] 14941
vuelos %>% 
  filter(is.na(CarrierDelay)) %>% 
  nrow() # [1] 3551290
vuelos %>% 
  filter(is.na(WeatherDelay)) %>% 
  nrow() # [1] 3551290
vuelos %>% 
  filter(is.na(NASDelay)) %>% 
  nrow() # [1] 3551290
vuelos %>% 
  filter(is.na(SecurityDelay)) %>% 
  nrow() # [1] 3551290
vuelos %>% 
  filter(is.na(LateAircraftDelay)) %>% 
  nrow() # [1] 3551290
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
  mutate(CRSElapsedTime = coalesce(CRSElapsedTime,0L))
vuelos <- vuelos %>% 
  mutate(ActualElapsedTime = coalesce(ActualElapsedTime,0L))
vuelos <- vuelos %>% 
  mutate(AirTime = coalesce(AirTime,0L))
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
### TAXIOUT, TAXIIN, ARRDELAY, ARRDELAYMINUTES, ARRDEL15, CRSELAPSEDTIME, ACTUALELAPSEDTIME, AIRTIME, CARRIERDELAY, WEATHERDELAY, NASDELAY, SECURITYDELAY, LATEAIRCRAFTDELAY ###

# Realizo copia 
dfCasiLimpio <- vuelos

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
  filter(ArrDelay > 0, (CarrierDelay + WeatherDelay + NASDelay + SecurityDelay + LateAircraftDelay) == 0) %>% 
  nrow() # [1] 915019
# Añado una columna donde se sumen las variables con retraso 
totalDelay <- vuelos$CarrierDelay+vuelos$WeatherDelay+vuelos$NASDelay+vuelos$SecurityDelay+vuelos$LateAircraftDelay
# La añaado al dataset
vuelos <- cbind(vuelos, totalDelay)
# Modifico los CarrierDelay donde ArrDelay > 0 & (CarrierDelay + WeatherDelay + NASDelay + SecurityDelay + LateAircraftDelay) == 0
vuelos[vuelos$ArrDelay>0 & vuelos$totalDelay==0,"CarrierDelay"] <- vuelos[vuelos$ArrDelay>0 & vuelos$totalDelay == 0, "ArrDelay"]
vuelos %>% 
  filter(ArrDelay > 0, (CarrierDelay + WeatherDelay + NASDelay + SecurityDelay + LateAircraftDelay) == 0) %>% 
  nrow() # [1] 0
### ARRDELAY ###

### ARRTIME ###
vuelos %>% 
  filter(is.na(ArrTime)) %>% 
  nrow() # [1] 5817
# Se podría intentar calcular la hora de llegada por el número de vuelo y el destino, pero hay que
# tener en cuenta el tiempo de vueloy el cambio de hora, como tampoco tenemos la información de 
# estos retrasos, asignamos a estos vuelos por defecto las "00:00"
vuelos <- vuelos %>% 
  mutate(ArrTime = coalesce(ArrTime,"00:00"))
### ARRTIME ###

# Reviso que no quede ninguna variable más por limpiar
colSums(is.na(vuelos))>0
#            Year              Month         DayofMonth          DayOfWeek         FlightDate      UniqueCarrier          AirlineID            Carrier 
#           FALSE              FALSE              FALSE              FALSE              FALSE              FALSE              FALSE              FALSE 
#       FlightNum    OriginAirportID OriginAirportSeqID OriginCityMarketID             Origin     OriginCityName        OriginState    OriginStateFips 
#           FALSE              FALSE              FALSE              FALSE              FALSE              FALSE              FALSE              FALSE 
# OriginStateName          OriginWac      DestAirportID   DestAirportSeqID   DestCityMarketID               Dest       DestCityName          DestState 
#           FALSE              FALSE              FALSE              FALSE              FALSE              FALSE              FALSE              FALSE 
#   DestStateFips      DestStateName            DestWac            DepTime           DepDelay    DepDelayMinutes           DepDel15            TaxiOut 
#           FALSE              FALSE              FALSE              FALSE              FALSE              FALSE              FALSE              FALSE 
#          TaxiIn            ArrTime           ArrDelay    ArrDelayMinutes           ArrDel15     CRSElapsedTime  ActualElapsedTime            AirTime 
#           FALSE              FALSE              FALSE              FALSE              FALSE              FALSE              FALSE              FALSE 
#         Flights           Distance       CarrierDelay       WeatherDelay           NASDelay      SecurityDelay  LateAircraftDelay         totalDelay 
#           FALSE              FALSE              FALSE              FALSE              FALSE              FALSE              FALSE              FALSE
# *************************************************************************************************

# Guardamos el fichero
write.table(vuelos, file = "VuelosFinales.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)

# Top 3 de aeropuertos que tienen más vuelos retrasados
vuelos %>% 
  group_by(Origin) %>% 
  summarise(totalDel15 = sum(DepDel15==1)) %>% 
  arrange(-totalDel15) %>% 
  top_n(3)
# A tibble: 3 x 2
# Origin totalDel15
#   <fct>       <int>
# 1 ATL         66969
# 2 ORD         61844
# 3 DFW         51141

vuelos %>% 
  filter(OriginState == "CA", DepDel15 > 15) 