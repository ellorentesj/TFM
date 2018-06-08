#### TFM: ANÁLISIS Y PREDICCIÓN DEL RESTRASO EN LOS VUELOS ####



#### 1. PREPROCESAMIENTO DE LOS FICHEROS: Lectura de ficheros. Limpieza, análisis y tratamiento de datos ####



# Los datos de los vuelos se obtienen de: https://www.transtats.bts.gov/DL_SelectFields.asp



# *************************************************************************************************
##### 1.1. Bloque de carga de librerias #####
if(!require("data.table")){
  install.packages("data.table")
  library(data.table)
}
if(!require("ggplot2")){
  install.packages("ggplot2")
  library(ggplot2)
}
if(!require("dplyr")){
  install.packages("dplyr")
  library(dplyr)
}
if(!require("lubridate")){
  install.packages("lubridate")
  library(lubridate)
}
# *************************************************************************************************



# *************************************************************************************************
##### 1.2. Bloque de carga de datos #####
# ¡CUIDADO! Es necesario modificar la ruta por la correspondiente en el equipo donde se vaya a 
# ejecutar el código para su correcto funcionamiento
# Selección de ruta donde están los ficheros, en mi caso: "/Users/ellorentesj/repostGitHub/TFM"
# setwd("/Users/ellorentesj/repostGitHub/TFM")

# Declaro variables 'tempDec' y 'tempJan' como archivos temporeales
tempJan <- tempfile()
tempFeb <- tempfile()

# Asigno las variables a los archivos
tempJan = "data/On_Time_On_Time_Performance_2014_1.zip"
tempFeb = "data/On_Time_On_Time_Performance_2014_2.zip"

# Utilizo la función unzip para extraer los archivos CSV
unzip(tempJan,"On_Time_On_Time_Performance_2014_1.csv")
unzip(tempFeb, "On_Time_On_Time_Performance_2014_2.csv")

# Introduzco los datos de los CSV en dataframes
dfJanuary <- fread("On_Time_On_Time_Performance_2014_1.csv", header=T, sep=',')
dfFebruary <- fread("On_Time_On_Time_Performance_2014_2.csv", header=T, sep=',')

flightsAux <- rbind(dfJanuary,dfFebruary)
# *************************************************************************************************



# *************************************************************************************************
##### 1.3. Bloque de limpieza de datos #####

# Primera visualización de los datos
str(flightsAux)
summary(flightsAux)

# Porcentaje de información inexistente
percent <- as.double((colSums(is.na(flightsAux))/nrow(flightsAux))*100)
names <- colnames(flightsAux)
df <- data.frame(names)
df$percent <- percent
# Visualización gráfica
ggplot(df, aes(x = df$names, y = df$percent)) + geom_col(fill="blue") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Names", y = "Percentage", title = "Information loss percent")
# Visualización de los datos >= 99%
df %>% filter(percent>=99) %>% arrange(percent) %>% View()

# Limpieza del dataset, elimino las variables que tienen >99 % perdida de información, puesto que
# esto indica que es una variable sin datos válidos.
cleanEmptyData <- function (dfFlightsAux){
  
  dfFlightsAux$TotalAddGTime <- NULL # 99.34585% de pérdida de información
  dfFlightsAux$LongestAddGTime <- NULL # 99.34585% de pérdida de información
  dfFlightsAux$Div1AirportID <- NULL # 99.67470% de pérdida de información
  dfFlightsAux$Div1AirportSeqID <- NULL # 99.67470% de pérdida de información
  dfFlightsAux$Div1TotalGTime <- NULL # 99.67470% de pérdida de información
  dfFlightsAux$Div1LongestGTime <- NULL # 99.67470% de pérdida de información
  dfFlightsAux$DivReachedDest <- NULL # 99.70982% de pérdida de información
  dfFlightsAux$DivDistance <- NULL # 99.71115% de pérdida de información
  dfFlightsAux$DivActualElapsedTime <- NULL # 99.82195% de pérdida de información
  dfFlightsAux$DivArrDelay <- NULL # 99.82195% de pérdida de información
  dfFlightsAux$Div2AirportID <- NULL # 99.99247% de pérdida de información
  dfFlightsAux$Div2AirportSeqID <- NULL # 99.99247% de pérdida de información
  dfFlightsAux$Div2TotalGTime <- NULL # 99.99247% de pérdida de información
  dfFlightsAux$Div2LongestGTime <- NULL # 99.99247% de pérdida de información
  dfFlightsAux$Div3AirportID <- NULL # 100.00000% de pérdida de información
  dfFlightsAux$Div3AirportSeqID <- NULL # 100.00000% de pérdida de información
  dfFlightsAux$Div3TotalGTime <- NULL # 100.00000% de pérdida de información
  dfFlightsAux$Div3LongestGTime <- NULL # 100.00000% de pérdida de información
  dfFlightsAux$Div4AirportID <- NULL # 100.000000% de pérdida de información
  dfFlightsAux$Div4AirportSeqID <- NULL # 100.000000% de pérdida de información
  dfFlightsAux$Div4TotalGTime <- NULL # 100.000000% de pérdida de información
  dfFlightsAux$Div4LongestGTime <- NULL # 100.000000% de pérdida de información
  dfFlightsAux$Div5AirportID <- NULL # 100.000000% de pérdida de información
  dfFlightsAux$Div5AirportSeqID <- NULL # 100.000000% de pérdida de información
  dfFlightsAux$Div5TotalGTime <- NULL # 100.000000% de pérdida de información
  dfFlightsAux$Div5LongestGTime <- NULL # 100.000000% de pérdida de información
  dfFlightsAux$V110 <- NULL # 100.000000% de pérdida de información
  
  return (dfFlightsAux)
}

flights <- cleanEmptyData(flightsAux)
# *************************************************************************************************



# *************************************************************************************************
##### 1.4. Bloque de análisis del dataset #####

str(flights)
head(flights)
tail(flights)
summary(flights)

# Dentro de los vuelos operados, existen vuelos cancelados y desviados
flights %>% filter(Cancelled==1) %>% nrow() # 54571 vuelos cancelados
flights %>% filter(Diverted==1) %>% nrow() # 2619 vuelos desviados

##### 1.4.1. Análisis de los vuelos cancelados #####
# Los vuelos cancelados no son de utilidad para analizar y predecir los retrasos en los vuelos 
# puesto que son operaciones no realizadas. Un vuelo está cancelado cuando en su campo indica un 1, 
# y un 0 cuando el vuelo no ha sido cancelado.
flights <- flights %>% filter(Cancelled==0)
# Se puede prescindir de las columnas Cancelled y CancelationCode
flights$Cancelled <- NULL
flights$CancellationCode <- NULL

##### 1.4.2. Análisis de los vuelos desviados #####
# Al tratarse de vuelos no programados (puesto que no aterrizan en el destino), no almacenan el 
# retraso ya que no hay manera de calcularlo. Por tanto los vuelos desviados no son de utilidad 
# para analizar y predecir los retrasos en los vuelos. Un vuelo está desviado cuando en su campo 
# indica un 1, y un 0 cuando el vuelo no ha sido desviado.
flights <- flights %>% filter(Diverted==0)
# Se puede prescindir de las columnas asociadas a los desvios
flights$Diverted <- NULL
flights$DivAirportLandings <- NULL
flights$Div1Airport <- NULL
flights$Div1WheelsOn <- NULL
flights$Div1WheelsOff <- NULL
flights$Div1TailNum <- NULL
flights$Div2Airport <- NULL
flights$Div2WheelsOn <- NULL
flights$Div2WheelsOff <- NULL
flights$Div2TailNum <- NULL
flights$Div3Airport <- NULL
flights$Div3WheelsOn <- NULL
flights$Div3WheelsOff <- NULL
flights$Div3TailNum <- NULL
flights$Div4Airport <- NULL
flights$Div4WheelsOn <- NULL
flights$Div4WheelsOff <- NULL
flights$Div4TailNum <- NULL
flights$Div5Airport <- NULL
flights$Div5WheelsOn <- NULL
flights$Div5WheelsOff <- NULL
flights$Div5TailNum <- NULL
# *************************************************************************************************



# *************************************************************************************************
##### 1.5. Bloque de tratamiento de variables #####


##### 1.5.1. Time Period #####

##### 1.5.1.1. Year #####
str(flights$Year) 
# Esta variable contiene el año
# Transformo a factor
flights$Year <- as.factor(flights$Year)
summary(flights$Year)
# Compruebo si tiene NA's
flights %>% filter(is.na(Year)) %>% nrow() # La variable no contiene ningún NA

##### 1.5.1.2. Quarter #####
str(flights$Quarter) 
# Esta variable contiene el cuarto de año al que corresponde el mes
summary(flights$Quarter)
# Esta variable no es relevante para el estudio puesto únicamente informa del cuarto de año en el 
# que se encuentra el mes de estudio. Prescindo de ella
flights$Quarter <- NULL

##### 1.5.1.3. Month #####
str(flights$Month) 
# Esta variable contiene el mes.
# La transformo a factor
flights$Month <- as.factor(flights$Month)
summary(flights$Month)
# Compruebo si tiene NA's
flights %>% filter(is.na(Month)) %>% nrow() # La variable no contiene ningún NA

##### 1.5.1.4. DayofMonth #####
str(flights$DayofMonth) 
# Esta variable contiene el día del mes.
# La transformo a factor
flights$DayofMonth <- as.factor(flights$DayofMonth)
summary(flights$DayofMonth)
# Compruebo si tiene NA's
flights %>% filter(is.na(DayofMonth)) %>% nrow() # La variable no contiene ningún NA

##### 1.5.1.5. DayofWeek #####
str(flights$DayofWeek) 
# Esta variable contiene el día de la semana.
# La transformo a factor
flights$DayOfWeek <- as.factor(flights$DayOfWeek)
summary(flights$DayOfWeek)
# Compruebo si tiene NA's
flights %>% filter(is.na(DayOfWeek)) %>% nrow() # la variable no contiene ningún NA

##### 1.5.1.6. FlightDate #####
str(flights$FlightDate) 
# Esta varieble contiene la fecha del vuelo en formato yyyymmdd
# La transformo en fecha
flights$FlightDate = ymd(flights$FlightDate)
summary(flights$FlightDate)
# Compruebo si tiene NA's
flights %>% filter(is.na(FlightDate)) %>% nrow() # la variable no contiene ningún NA


##### 1.5.2. Airline #####

##### 1.5.2.1. UniqueCarrier #####
str(flights$UniqueCarrier) 
# Esta variable contiene el código único del operador. La página de US DOT indica que se use este 
# campo para el análisis en un rango de años puesto que cuando varios operadores han utilizado el 
# mismo código, se usa un sufijo numérico para usuarios anteriores, por ejemplo PA, PA (1), PA (2).
# Lo transformo a factor
flights$UniqueCarrier <- as.factor(flights$UniqueCarrier)
summary(flights$UniqueCarrier)
# Compruebo si tiene NA's
flights %>% filter(is.na(UniqueCarrier)) %>% nrow() # la variable no contiene ningún NA

##### 1.5.2.2. AirlineID #####
str(flights$AirlineID)
# Esta variable contiene un número de identificación asignado por el DOT de los EE.UU. Para
# identificar una aerolínea única.
# Lo transformo a factor
flights$AirlineID <- as.factor(flights$AirlineID)
summary(flights$AirlineID)
# Compruebo si cada UniqueCarrier tiene un único AirlineID asociado.
str(flights$UniqueCarrier) # 14 levels
str(flights$AirlineID) # 14 levels
flights %>% group_by(UniqueCarrier) %>% summarise(AirlineID = unique(AirlineID)) 
# Cada UniqueCarrier tiene un único AirlineID asociado, por tanto se puede prescindir de la variable
# AirlineID
flights$AirlineID <- NULL

##### 1.5.2.3. Carrier #####
str(flights$Carrier)
# Esta variable contiene el código asignado por la IATA comúnmente utilizado para identiticar una 
# compañía. Como el mismo código puede haber sido asignado a diferentes operadores a lo largo del 
# tiempo, el código no siempre es único. La página de US DOT indica que para el análisis, se use 
# el Código único de operador (Unique Carrier Code).
# Lo transformo en factor:
flights$Carrier <- as.factor(flights$Carrier)
summary(flights$Carrier)
# Comparo las columnas UniqueCarrier y Carrier:
str(flights$UniqueCarrier) # 14 levels
str(flights$Carrier) # 14 levels
flights %>% group_by(UniqueCarrier) %>% summarise(Carrier = unique(Carrier)) 
# Contienen la misma información y podemos prescindir de esta variable.
flights$Carrier <- NULL

##### 1.5.2.4. TailNum #####
str(flights$TailNum)
# Esta variable contiene el número de cola (matrícula), de la aeronave.
# Lo transformo en factor:
flights$TailNum <- as.factor(flights$TailNum)
summary(flights$TailNum)
str(flights$TailNum) # 4445 levels
# Compruebo si tiene NA's
flights %>% filter(is.na(TailNum)) %>% nrow() # la variable no contiene ningún NA

##### 1.5.2.5. FlightNum #####
str(flights$FlightNum)
# Esta variable contiene el número de vuelo.
# Lo transformo en factor:
flights$FlightNum <- as.factor(flights$FlightNum)
summary(flights$FlightNum) 
str(flights$FlightNum) # 6338 levels
# Compruebo si tiene NA's
flights %>% filter(is.na(FlightNum)) %>% nrow() # la variable no contiene ningún NA


##### 1.5.3. Origin #####

##### 1.5.3.1. OriginAirportID #####
str(flights$OriginAirportID)
# Esta variable contiene el ID del aeropuerto de Origen. Un número de identificación asignado por
# el DOT de los EE.UU. para identificar un aeropuerto único. La página indica que se use este campo
# para el análisis de aeropuertos en un rango de años porque un aeropuerto puede cambiar su código
# de aeropuerto y los códigos de aeropuerto pueden reutilizarse.
# Lo transformo en factor:
flights$OriginAirportID <- as.factor(flights$OriginAirportID)
summary(flights$OriginAirportID)
str(flights$OriginAirportID) # 301 levels
# Compruebo si tiene NA's 
flights %>% filter(is.na(OriginAirportID)) %>% nrow() # La variable no contiene ningún NA

##### 1.5.3.2. OriginAirportSeqID #####
str(flights$OriginAirportSeqID)
# Esta variable contiene el ID de secuencia del aeropuerto de Origen. Un número de identificación
# asignado por el DOT de los EE.UU. para identificar un aeropuerto único en un punto dado de tiempo.
# Los atributos del aeropuerto, como el nombre o las coordenadas del aeropuerto, pueden cambiar con
# el tiempo.
# Lo transformo en factor:
flights$OriginAirportSeqID <- as.factor(flights$OriginAirportSeqID)
summary(flights$OriginAirportSeqID)
str(flights$OriginAirportSeqID) # 305 levels
# Compruebo si tiene NA's 
flights %>% filter(is.na(OriginAirportSeqID)) %>% nrow() # La variable no contiene ningún NA

##### 1.5.3.3. OriginCityMarketID #####
str(flights$OriginCityMarketID)
# Esta variable contiene un número de identificación asignado por el DOT de los EE.UU. para 
# identificar un mercado de la ciudad. La página indica que este campo se use para consolidar los
# aeropuertos que prestan servicios en el mismo mercado de la ciudad.
# Lo transformo en factor:
flights$OriginCityMarketID <- as.factor(flights$OriginCityMarketID)
summary(flights$OriginCityMarketID)
str(flights$OriginCityMarketID) # 280 levels
# Compruebo si tiene NA's 
flights %>% filter(is.na(OriginCityMarketID)) %>% nrow() # La variable no contiene ningún NA

##### 1.5.3.4. Origin #####
str(flights$Origin)
# Esta variable contiene el aeropuerto de Origen
# Lo transormo en factor:
flights$Origin <- as.factor(flights$Origin)
summary(flights$Origin)
str(flights$Origin) # 301 levels
# Compruebo si cada Origin tiene un único OriginAirportID asociado.
flights %>% group_by(Origin) %>% summarise(OriginAirportID = unique(OriginAirportID))
# Cada Origin tiene un único OriginAirportID asociado, y como no se está realizando un análisis en 
# un rango de años se puede prescindir de la variable OriginAirportID
flights$OriginAirportID <- NULL
# Compruebo si tiene NA's
flights %>% filter(is.na(Origin)) %>% nrow() # la variable no contiene ningún NA

##### 1.5.3.5. OriginCityName #####
str(flights$OriginCityName)
# Esta variable contiene el nombre de la Ciudad del aeropuerto de Origen.
# Lo transformo en factor:
flights$OriginCityName <- as.factor(flights$OriginCityName)
summary(flights$OriginCityName)
str(flights$OriginCityName) # 297 levels
# Compruebo si tiene NA's
flights %>% filter(is.na(OriginCityName)) %>% nrow() # la variable no contiene ningún NA

##### 1.5.3.6. OriginState #####
str(flights$OriginState)
# Esta variable contiene el código del estado del aeropuerto de Origen.
# Lo transformo en factor:
flights$OriginState <- as.factor(flights$OriginState)
summary(flights$OriginState)
str(flights$OriginState) # 53 levels
# Compruebo si tiene NA's
flights %>% filter(is.na(OriginState)) %>% nrow() # la variable no contiene ningún NA

##### 1.5.3.7. OriginStateFips #####
str(flights$OriginStateFips)
# Esta variable contiene el código FIPS del aeropuerto de Origen. FIPS (Federal Information
# Processing Standards) se refiere a un código asignado a cualquier de una variedad de entidades
# geográficas (por ejemplo, condados, estados, áreas metropolitanas, etc.). Los códigos FIPS están
# destinados a simplificar la recopilación, el procesamiento y la difusión de datos y recursos del
# Gobierno Federal.
# Lo transformo en factor:
flights$OriginStateFips <- as.factor(flights$OriginStateFips)
summary(flights$OriginStateFips)
str(flights$OriginStateFips) # 53 levels
# Compruebo si cada OriginState tiene un único OriginStateFips asociado.
flights %>% group_by(OriginState) %>% summarise(OriginStateFips = unique(OriginStateFips)) 
# Cada OriginState tiene un único OriginStateFips asociado, por tanto se puede prescindir de la 
# variable OriginStateFips
flights$OriginStateFips <- NULL

##### 1.5.3.8. OriginStateName #####
str(flights$OriginStateName)
# Esta variable contiene el nombre del estado del aeropuerto de Origen.
# Lo transformo en factor:
flights$OriginStateName <- as.factor(flights$OriginStateName)
summary(flights$OriginStateName)
str(flights$OriginStateName) # 53 levels
# Compruebo si tiene NA's
flights %>% filter(is.na(OriginStateName)) %>% nrow() # la variable no contiene ningún NA

##### 1.5.3.9. OriginWac #####
str(flights$OriginWac)
# Esta variable contiene el World Area Code (WAC) del aeropuerto de Origen. Son códigos numéricos
# para identificar áreas geopolíticas como países, estados (EE.UU), provincias (Canadá) y territorios
# o posesiones de determinados países. Se utilizan dentro de diversos bancos de datos mantenidos por
# la oficina de Office or Airline Information (OAI) y son creados por OAI.
# Lo transformo en factor:
flights$OriginWac <- as.factor(flights$OriginWac)
summary(flights$OriginWac)
str(flights$OriginWac) # 53 levels
# Compruebo si cada OriginState tiene un único OriginWac asociado.
flights %>% group_by(OriginState) %>% summarise(OriginWac = unique(OriginWac)) 
# Cada OriginState tiene un único OriginWac asociado, por tanto se puede prescindir de la variable 
# OriginWac
flights$OriginWac <- NULL


##### 1.5.4. Destination #####

##### 1.5.4.1. DestAirportID #####
str(flights$DestAirportID)
# Esta variable contiene el ID del aeropuerto de Destino. Un número de identificación asignado por
# el DOT de los EE.UU. para identificar un aeropuerto único. La página indica que se use este campo
# para el análisis de aeropuertos en un rango de años porque un aeropuerto puede cambiar su código
# de aeropuerto y los códigos de aeropuerto pueden reutilizarse.
# Lo transformo en factor:
flights$DestAirportID <- as.factor(flights$DestAirportID)
summary(flights$DestAirportID)
str(flights$DestAirportID) # 301 levels
# Compruebo si tiene NA's
flights %>% filter(is.na(DestAirportID)) %>% nrow() # la variable no contiene ningún NA

##### 1.5.4.2. DestAirportSeqID #####
str(flights$DestAirportSeqID)
# Esta variable contiene el ID de secuencia del aeropuerto de Destino. Un número de identificación
# asignado por el DOT de los EE.UU. para identificar un aeropuerto único en un punto dado de tiempo.
# Los atributos del aeropuerto, como el nombre o las coordenadas del aeropuerto, pueden cambiar con
# el tiempo.
# Lo transformo en factor:
flights$DestAirportSeqID <- as.factor(flights$DestAirportSeqID)
summary(flights$DestAirportSeqID)
str(flights$DestAirportSeqID) # 305 levels
# Compruebo si tiene NA's
flights %>% filter(is.na(DestAirportSeqID)) %>% nrow() # la variable no contiene ningún NA

##### 1.5.4.3. DestCityMarketID #####
str(flights$DestCityMarketID)
# Esta variable contiene un número de identificación asignado por el DOT de los EE.UU. para 
# identificar un mercado de la ciudad. La página indica que este campo se use para consolidar los
# aeropuertos que prestan servicios en el mismo mercado de la ciudad.
# Lo transformo en factor:
flights$DestCityMarketID <- as.factor(flights$DestCityMarketID)
summary(flights$DestCityMarketID)
str(flights$DestCityMarketID) # 280 levels
# Compruebo si tiene NA's
flights %>% filter(is.na(DestCityMarketID)) %>% nrow() # la variable no contiene ningún NA

##### 1.5.4.4. Dest #####	
str(flights$Dest)
# Esta variable contiene el aeropuerto de Destino
# Lo transormo en factor:
flights$Dest <- as.factor(flights$Dest)
summary(flights$Dest)
str(flights$Dest) # 301 levels
# Compruebo si cada Dest tiene un único DestAirportID asociado.
flights %>% group_by(Dest) %>% summarise(DestAirportID = unique(DestAirportID))
# Cada Dest tiene un único DestAirportID asociado, y como no se está realizando un análisis en un
# rango de años se puede prescindir de la variable DestAirportID
flights$DestAirportID <- NULL
# Compruebo si tiene NA's
flights %>% filter(is.na(Dest)) %>% nrow() # la variable no contiene ningún NA

##### 1.5.4.5. DestCityName #####	
str(flights$DestCityName)
# Esta variable contiene el nombre de la Ciudad del aeropuerto de Destino.
# Lo transformo en factor:
flights$DestCityName <- as.factor(flights$DestCityName)
summary(flights$DestCityName)
str(flights$DestCityName) # 297 levels
# Compruebo si tiene NA's
flights %>% filter(is.na(DestCityName)) %>% nrow() # la variable no contiene ningún NA

##### 1.5.4.6. DestState #####	
str(flights$DestState)
# Esta variable contiene el código del estado del aeropuerto de Destino.
# Lo transformo en factor:
flights$DestState <- as.factor(flights$DestState)
summary(flights$DestState)
str(flights$DestState) # 53 levels
# Compruebo si tiene NA's
flights %>% filter(is.na(DestState)) %>% nrow() # la variable no contiene ningún NA

##### 1.5.4.7. DestStateFips #####
str(flights$DestStateFips)
# Esta variable contiene el código FIPS (explicado ya en el punto 1.5.3.7.) del aeropuerto de Destino.
# Lo transformo en factor:
flights$DestStateFips <- as.factor(flights$DestStateFips)
summary(flights$DestStateFips)
str(flights$DestStateFips) # 53 levels
# Compruebo si cada DestState tiene un único DestStateFips asociado.
flights %>% group_by(DestState) %>% summarise(DestStateFips = unique(DestStateFips)) 
# Cada DestState tiene un único DestStateFips asociado, por tanto se puede prescindir de la variable
# DestStateFips
flights$DestStateFips <- NULL

##### 1.5.4.8. DestStateName #####	
str(flights$DestStateName)
# Esta variable contiene el nombre del estado del aeropuerto de Destino.
# Lo transformo en factor:
flights$DestStateName <- as.factor(flights$DestStateName)
summary(flights$DestStateName)
str(flights$DestStateName) # 53 levels
# Compruebo si tiene NA's
flights %>% filter(is.na(DestStateName)) %>% nrow() # la variable no contiene ningún NA

##### 1.5.4.9. DestWac #####	
str(flights$DestWac)
# Esta variable contiene el World Area Code (WAC) del aeropuerto de Destino (explicado ya en el punto
# 1.5.3.9.). 
# Lo transformo en factor:
flights$DestWac <- as.factor(flights$DestWac)
summary(flights$DestWac)
str(flights$DestWac) # 53 levels
# Compruebo si cada DestState tiene un único DestWac asociado.
flights %>% group_by(DestState) %>% summarise(DestWac = unique(DestWac)) 
# Cada DestState tiene un único DestWac asociado, por tanto se puede prescindir de la variable 
# DestWac
flights$DestWac <- NULL


##### 1.5.4. Departure Performance #####

##### 1.5.4.1. CRSDepTime #####
str(flights$CRSDepTime)
# Esta variable contiene la hora de salida del vuelo según Computer Reservarion System (CRS) en 
# hora local: hhmm.
# Proporciona información sobre horarios de las líneas aéreas, tarifas y disponibilidad de asientos
# a las agencias de viajes y permite a los agentes reservar asientos y emitir tickets.
# Transformo la variable en tipo integer
flights$CRSDepTime <- as.integer(flights$CRSDepTime)
summary(flights$CRSDepTime)
# Compruebo si tiene NA's
flights %>% filter(is.na(CRSDepTime)) %>% nrow() # la variable no contiene ningún NA

##### 1.5.4.2. DepTime #####
str(flights$DepTime)
# Esta variable contiene la hora real de despegue en hora local: hhmm.
# Transformo la variable en tipo integer
flights$DepTime <- as.integer(flights$DepTime)
summary(flights$DepTime)
flights %>% filter(is.na(DepTime)) %>% nrow() # la variable no contiene ningún NA
# Formas de trabajar con las horas por si fuese necesario
# hour(hm(flights$DepTime))
# class(as.difftime(flights$DepTime, format="%H",units="hours"))

##### 1.5.4.3. DepDelay #####
str(flights$DepDelay)
# Esta variable contiene la diferencia en minutos entre la hora de salida programada y la real. Las
# salidas anticipadas muestran números negativos.
# Transformo la variable en tipo integer
flights$DepDelay <- as.integer(flights$DepDelay)
summary(flights$DepDelay)
# Compruebo si tiene NA's
flights %>% filter(is.na(DepDelay)) %>% nrow() # la variable no contiene ningún NA

##### 1.5.4.4. DepDelayMinutes #####
str(flights$DepDelayMinutes)
# Esta variable contiene la diferencia en minutos entre la hora de salida programada y la real. 
# Salidas anticipadas configuradas en 0.
# Transformo la variable en tipo integer
flights$DepDelayMinutes <- as.integer(flights$DepDelayMinutes)
summary(flights$DepDelayMinutes)
# Compruebo si tiene NA's
flights %>% filter(is.na(DepDelayMinutes)) %>% nrow() # la variable no contiene ningún NA

##### 1.5.4.5. DepDel15 #####
str(flights$DepDel15)
# Esta variable contiene un indicador de retraso de salida, 15 minutos o más (1=sí).
# Transformo la variable en factor
flights$DepDel15 <- as.factor(flights$DepDel15)
summary(flights$DepDel15)
# Compruebo si tiene NA's
flights %>% filter(is.na(DepDel15)) %>% nrow() # la variable no contiene ningún NA

##### 1.5.4.6. DepartureDelayGroups #####
str(flights$DepartureDelayGroups)
# Esta variable contiene intervalos de retardo de salida, cada 15 minutos desde <-15 hasta > 180
# Transformo la variable en factor
flights$DepartureDelayGroups <- as.factor(flights$DepartureDelayGroups)
summary(flights$DepartureDelayGroups)
# Compruebo si tiene NA's
flights %>% filter(is.na(DepDel15)) %>% nrow() # la variable no contiene ningún NA

##### 1.5.4.7. DepTimeBlk #####
str(flights$DepTimeBlk)
# Esta variable contiene el bloque de tiempo de salida de CRS de intervalos por hora
# Transformo la variable en factor
flights$DepTimeBlk <- as.factor(flights$DepTimeBlk)
summary(flights$DepTimeBlk)
# Compruebo si tiene NA's
flights %>% filter(is.na(DepTimeBlk)) %>% nrow() # la variable no contiene ningún NA

##### 1.5.4.8. TaxiOut #####
str(flights$TaxiOut)
# Esta variable contiene el tiempo de Taxi en minutos. Taxi-Out es el tiempo transcurrido entre la
# salida del stand en el aeropuerto de origen y el momento en que las ruedas no tocan pista.
# Transformo la variable en tipo integer
flights$TaxiOut <- as.integer(flights$TaxiOut)
summary(flights$TaxiOut)
# Compruebo si tiene NA's
flights %>% filter(is.na(TaxiOut)) %>% nrow() # la variable no contiene ningún NA

##### 1.5.4.9. WheelsOff #####
str(flights$WheelsOff)
# Esta variable contiene la hora de despegue en el aeropuerto de Origen en hora local: hhmm
# Transformo la variable en tipo integer
flights$WheelsOff <- as.integer(flights$WheelsOff)
summary(flights$WheelsOff)
# Compruebo si tiene NA's
flights %>% filter(is.na(WheelsOff)) %>% nrow() # la variable no contiene ningún NA


##### 1.5.5. Arrival Performance #####

##### 1.5.5.1. WheelsOn #####
str(flights$WheelsOn)
# Esta variable contiene la hora de aterrizaje en el aeropuerto de Destino en hora local: hhmm
# Transformo la variable en tipo integer
flights$WheelsOn <- as.integer(flights$WheelsOn)
summary(flights$WheelsOn)
# Compruebo si tiene NA's
flights %>% filter(is.na(WheelsOn)) %>% nrow() # la variable no contiene ningún NA

##### 1.5.5.2. TaxiIn #####
str(flights$TaxiIn)
# Esta variable contiene el tiempo de Taxi en minutos. Taxi-In es el tiempo transcurrido entre el
# momento en que las ruedas tocan pista y la llegada al stand en el aeropuerto de destino.
# Transformo la variable en tipo integer
flights$TaxiIn <- as.integer(flights$TaxiIn)
summary(flights$TaxiIn)
# Compruebo si tiene NA's
flights %>% filter(is.na(TaxiIn)) %>% nrow() # la variable no contiene ningún NA

##### 1.5.5.3. CRSArrTime #####
str(flights$CRSArrTime)
# Esta variable contiene la hora de llegada del vuelo según Computer Reservarion System (CRS) en 
# hora local: hhmm.
# Transformo la variable en tipo integer
flights$CRSArrTime <- as.integer(flights$CRSArrTime)
summary(flights$CRSArrTime)
# Compruebo si tiene NA's
flights %>% filter(is.na(CRSArrTime)) %>% nrow() # la variable no contiene ningún NA

##### 1.5.5.4. ArrTime #####
str(flights$ArrTime)
# Esta variable contiene la hora real de aterrizaje en hora local: hhmm.
# Transformo la variable en tipo integer
flights$ArrTime <- as.integer(flights$ArrTime)
summary(flights$ArrTime)
flights %>% filter(is.na(ArrTime)) %>% nrow() # la variable no contiene ningún NA

##### 1.5.5.5.  ArrDelay	 #####
str(flights$ArrDelay)
# Esta variable contiene la diferencia en minutos entre la hora de llegada programada y la real. Las
# llegadas anticipadas muestran números negativos.
# Transformo la variable en tipo integer
flights$ArrDelay <- as.integer(flights$ArrDelay)
summary(flights$ArrDelay)
# Compruebo si tiene NA's
flights %>% filter(is.na(ArrDelay)) %>% nrow() # la variable no contiene ningún NA 

##### 1.5.5.6. ArrDelayMinutes #####
str(flights$ArrDelayMinutes)
# Esta variable contiene la diferencia en minutos entre la hora de llegada programada y la real. 
# Salidas anticipadas configuradas en 0.
# Transformo la variable en tipo integer
flights$ArrDelayMinutes <- as.integer(flights$ArrDelayMinutes)
summary(flights$ArrDelayMinutes)
# Compruebo si tiene NA's
flights %>% filter(is.na(ArrDelayMinutes)) %>% nrow() # la variable no contiene ningún NA

##### 1.5.5.7. ArrDel15 #####
str(flights$ArrDel15)
# Esta variable contiene un indicador de retraso de llegada, 15 minutos o más (1=sí).
# Transformo la variable en factor
flights$ArrDel15 <- as.factor(flights$ArrDel15)
summary(flights$ArrDel15)
# Compruebo si tiene NA's
flights %>% filter(is.na(ArrDel15)) %>% nrow() # la variable no contiene ningún NA

##### 1.5.5.8. ArrivalDelayGroups #####
str(flights$ArrivalDelayGroups)
# Esta variable contiene intervalos de retardo de llegada, cada 15 minutos desde <-15 hasta > 180
# Transformo la variable en factor
flights$ArrivalDelayGroups <- as.factor(flights$ArrivalDelayGroups)
summary(flights$ArrivalDelayGroups)
# Compruebo si tiene NA's
flights %>% filter(is.na(ArrivalDelayGroups)) %>% nrow() # la variable no contiene ningún NA

##### 1.5.5.9. ArrTimeBlk #####
str(flights$ArrTimeBlk)
# Esta variable contiene el bloque de tiempo de llegada de CRS de intervalos por hora
# Transformo la variable en factor
flights$ArrTimeBlk <- as.factor(flights$ArrTimeBlk)
summary(flights$ArrTimeBlk)
# Compruebo si tiene NA's
flights %>% filter(is.na(ArrTimeBlk)) %>% nrow() # la variable no contiene ningún NA


##### 1.5.6. Flight Summaries #####

##### 1.5.6.1. CRSElapsedTime #####
str(flights$CRSElapsedTime)
# Esta variable contiene tiempo de vuelo transcurrido en minutos de la CRS.
# Transformo la variable en integer
flights$CRSElapsedTime <- as.integer(flights$CRSElapsedTime)
summary(flights$CRSElapsedTime)
# Compruebo si tiene NA's
flights %>% filter(is.na(CRSElapsedTime)) %>% nrow() # la variable no contiene ningún NA

##### 1.5.6.2. ActualElapsedTime #####
str(flights$ActualElapsedTime)
# Esta variable contiene tiempo actual de vuelo transcurrido en minutos.
# Transformo la variable en integer
flights$ActualElapsedTime <- as.integer(flights$ActualElapsedTime)
summary(flights$ActualElapsedTime)
# Compruebo si tiene NA's
flights %>% filter(is.na(ActualElapsedTime)) %>% nrow() # la variable no contiene ningún NA

##### 1.5.6.3. AirTime #####
str(flights$AirTime)
# Esta variable contiene el tiempo de vuelo en minutos.
# Transformo la variable en integer
flights$AirTime <- as.integer(flights$AirTime)
summary(flights$AirTime)
# Compruebo si tiene NA's
flights %>% filter(is.na(AirTime)) %>% nrow() # la variable no contiene ningún NA

##### 1.5.6.4. Flights #####
str(flights$Flights)
# Esta variable contiene el número de vuelos.
# Transformo la variable en integer
flights$Flights <- as.integer(flights$Flights)
summary(flights$Flights)
# Compruebo si tiene NA's
flights %>% filter(is.na(Flights)) %>% nrow() # la variable no contiene ningún NA

##### 1.5.6.5. Distance #####
str(flights$Distance)
# Esta variable contiene la distancia entre aeropuertos en millas.
# Transformo la variable en integer
flights$Distance <- as.integer(flights$Distance)
summary(flights$Distance)
# Compruebo si tiene NA's
flights %>% filter(is.na(Distance)) %>% nrow() # la variable no contiene ningún NA

##### 1.5.6.6. DistanceGroup #####
str(flights$DistanceGroup)
# Esta variable contiene intervalos de distancia, cada 250 millas, para el segmento de vuelo.
# Transformo la variable en factor
flights$DistanceGroup <- as.factor(flights$DistanceGroup)
summary(flights$DistanceGroup)
# Compruebo si tiene NA's
flights %>% filter(is.na(DistanceGroup)) %>% nrow() # la variable no contiene ningún NA


##### 1.5.7. Cause of Delay #####

##### 1.5.7.1. CarrierDelay #####
str(flights$CarrierDelay)
# Esta variable contiene el retraso de la compañía en minutos.
# Transformo la variable en integer
flights$CarrierDelay <- as.integer(flights$CarrierDelay)
summary(flights$CarrierDelay)
# Compruebo si tiene NA's
flights %>% filter(is.na(CarrierDelay)) %>% nrow() # la variable contiene 675504 NA's

##### 1.5.7.2. WeatherDelay #####
str(flights$WeatherDelay)
# Esta variable contiene el retraso por condiciones meteorolóficas en minutos.
# Transformo la variable en integer
flights$WeatherDelay <- as.integer(flights$WeatherDelay)
summary(flights$WeatherDelay)
# Compruebo si tiene NA's
flights %>% filter(is.na(WeatherDelay)) %>% nrow() # la variable contiene 675504 NA's

##### 1.5.7.3. NASDelay #####
str(flights$NASDelay)
# Esta variable contiene el retraso por National Air System en minutos.
# Transformo la variable en integer
flights$NASDelay <- as.integer(flights$NASDelay)
summary(flights$NASDelay)
# Compruebo si tiene NA's
flights %>% filter(is.na(NASDelay)) %>% nrow() # la variable contiene 675504 NA's

##### 1.5.7.4. SecurityDelay #####
str(flights$SecurityDelay)
# Esta variable contiene el retraso por temas de seguridad en minutos.
# Transformo la variable en integer
flights$SecurityDelay <- as.integer(flights$SecurityDelay)
summary(flights$SecurityDelay)
# Compruebo si tiene NA's
flights %>% filter(is.na(SecurityDelay)) %>% nrow() # la variable contiene 675504 NA's

##### 1.5.7.4. LateAircraftDelay #####
str(flights$LateAircraftDelay)
# Esta variable contiene el retraso acumulado o reaccionario de la aeronave en minutos.
# Transformo la variable en integer
flights$LateAircraftDelay <- as.integer(flights$LateAircraftDelay)
summary(flights$LateAircraftDelay)
# Compruebo si tiene NA's
flights %>% filter(is.na(LateAircraftDelay)) %>% nrow() # la variable contiene 355878 NA's


##### 1.5.8. Gate Return Information at Origin Airport  #####

##### 1.5.8.1. FirstDepTime  #####
str(flights$FirstDepTime)
# Esta variable contiene el retraso acumulado o reaccionario de la aeronave en minutos.
summary(flights$FirstDepTime)
flights$FirstDepTime <- as.integer(flights$FirstDepTime)
# Compruebo si tiene NA's
flights %>% filter(is.na(FirstDepTime)) %>% nrow() # la variable contiene 840187 NA's
# Miro qué porcentaje de pérdida de información tiene esta variable:
flights %>% filter(is.na(FirstDepTime)) %>% nrow()/nrow(flights)*100
# Esta variable tienen una pérdida de información del 99.38795% >99%, la elimino del dataset
flights$FirstDepTime <- NULL
# *************************************************************************************************



# *************************************************************************************************
#### 1.6. Guardo el dataframe resultante de un análisis, limpieza y tratamiento previo ####
flightsFileDataProcesing <- flights
write.table(flightsFileDataProcesing, file = "data/flights.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
# *************************************************************************************************



# *************************************************************************************************
#### 1.7. Elimino referencias innecesarias #####
rm(df)
rm(dfFebruary)
rm(dfJanuary)
rm(flightsAux)
rm(names)
rm(percent)
rm(tempJan)
rm(tempFeb)
rm(cleanEmptyData)
# *************************************************************************************************



