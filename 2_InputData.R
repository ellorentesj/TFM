#### TFM: ANÁLISIS Y PREDICCIÓN DEL RESTRASO EN LOS VUELOS ####



#### 2. DATOS DE ENTRADA: Análisis de los datos de entrada ####



# *************************************************************************************************
##### 2.1. Bloque de carga de librerias #####

# *************************************************************************************************



# *************************************************************************************************
##### 2.2. Bloque de carga de datos #####
# Selección de ruta, en mi caso: "/Users/ellorentesj/repostGitHub/TFM/data/"
setwd("/Users/ellorentesj/repostGitHub/TFM/")
# flights <- fread("flights.csv", header=T, sep=',')
# *************************************************************************************************



# *************************************************************************************************
##### 2.3. Bloque de revisión basica del dataset #####
str(flights)
summary(flights)
# *************************************************************************************************



# *************************************************************************************************
#### 2.4. Eliminación variables  ####

# Procedo a eliminar variables que no tiene sentido mantener 


#### 2.4.1. Year ####
# La variable Year tiene el mismo valor en todos los campos, 2014, puesto que no se van a analizar 
# más años, se prescinde de ella
flights$Year <- NULL


#### 2.4.2. FlightDate ####
# Elimino la variable FlightDate ya que esta se encuentra dividida en las variables Year, Month y 
# DayofMonth
flights$FlightDate <- NULL


#### 2.4.3. OriginCityMarketID, DestCityMarketID ####
# Elimino estas variables puesto que no estamos analizando una zona de mercado concreta, y cada 
# zona puede corresponder a varios aeropuertos
flights$OriginCityMarketID <- NULL
flights$DestCityMarketID <- NULL


#### 2.4.4. OriginCityName, DestCityName ####
# Como estas variable contienen la información extendida de Origin y Dest respectivamente ya no son
# necesarias, prescindo de ellas
flights$OriginCityName <- NULL
flights$DestCityName <- NULL


#### 2.4.5. OriginStateName ####
# Como estas variable contienen la información extendida de OriginState y DestState respectivamente
# ya no son necesarias, prescindo de ellas
flights$OriginStateName <- NULL
flights$DestStateName <- NULL


#### 2.4.6. CRSDepTime ####
# Elimino la variable CRSDepTime ya que tenemos la hora actual de despegue en la variable DepTime
flights$CRSDepTime <- NULL


#### 2.4.7. CRSArrTime ####
# Elimino la variable CRSArrTime ya que tenemos la hora actual de aterrizaje en la variable ArrTime
flights$CRSArrTime <- NULL


#### 2.4.8. CRSElapsedTime ####
# Elimino la variable CRSElapsedTime ya que tenemos la hora actual de tiempo total transcurido en
# ActualElapsedTime
flights$CRSElapsedTime <- NULL


#### 2.4.9. TaxiOut, AirTime, TaxiIn  ####
# La variable ActualElapsedTime contiene en minutos el tiempo de vuelo total, esto significa que es
# la suma del tiempo Taxi-Out de la salida del vuelo, más el tiempo que está en el aire, más el 
# tiempo de Taxi-In de entrada
TimeRoute = flights$TaxiOut+flights$AirTime+flights$TaxiIn
# Comparo ambas variables 
flights %>% filter(ActualElapsedTime!=TimeRoute) %>% nrow() # Los valores coinciden
# Por tanto tenemos el tiempo total de vuelo en una única variable ActualElapsedTime, podemos 
# prescindir de TaxiOut, AirTime, TaxiIn
flights$TaxiOut <- NULL
flights$AirTime <- NULL
flights$TaxiIn <- NULL


#### 2.4.10. WheelsOff, WheelsOn  ####
# La variables DepTime y ArriTime contienen las horas de despegue y aterrizaje y la variable 
# ActualElapsedTime el tiempo total de vuelo transcurrido, las variables WheelsOff, WheelsOn que 
# corresponden a la hora en la que la aronave deja de tocar pista y toca pista respectivamente no
# son necesarias
flights$WheelsOff <- NULL
flights$WheelsOn <- NULL


#### 2.4.11. Flights  ####
# La variable Flights contiene un 1 en todos sus campos, se puede prescindir de ella
flights$Flights <- NULL
# *************************************************************************************************



# *************************************************************************************************
#### 2.5. Limpieza de NA's  ####

# Procedo a analizar las variables que contienen NA's y decidir que hacer con ellas 
colSums(is.na(flights))>0


#### 2.5.1. CarrierDelay  ####
flights %>% filter(is.na(CarrierDelay)) %>% nrow()
# Asumo que si la variable no contiene información en alguno de sus campos es por que no tiene 
# retraso, por tanto cumplimento los NA's con 0's
flights <- flights %>% mutate(CarrierDelay = coalesce(as.integer(CarrierDelay),0L))


#### 2.5.2. WeatherDelay  ####
flights %>% filter(is.na(WeatherDelay)) %>% nrow()
# Asumo que si la variable no contiene información en alguno de sus campos es por que no tiene 
# retraso, por tanto cumplimento los NA's con 0's
flights <- flights %>% mutate(WeatherDelay = coalesce(as.integer(WeatherDelay),0L))


#### 2.5.3. NASDelay  ####
flights %>% filter(is.na(NASDelay)) %>% nrow()
# Asumo que si la variable no contiene información en alguno de sus campos es por que no tiene 
# retraso, por tanto cumplimento los NA's con 0's
flights <- flights %>% mutate(NASDelay = coalesce(as.integer(NASDelay),0L))


#### 2.5.4. SecurityDelay  ####
flights %>% filter(is.na(SecurityDelay)) %>% nrow()
# Asumo que si la variable no contiene información en alguno de sus campos es por que no tiene 
# retraso, por tanto cumplimento los NA's con 0's
flights <- flights %>% mutate(SecurityDelay = coalesce(as.integer(SecurityDelay),0L))


#### 2.5.5. LateAircraftDelay  ####
flights %>% filter(is.na(LateAircraftDelay)) %>% nrow()
# Asumo que si la variable no contiene información en alguno de sus campos es por que no tiene 
# retraso, por tanto cumplimento los NA's con 0's
flights <- flights %>% mutate(LateAircraftDelay = coalesce(as.integer(LateAircraftDelay),0L))
# *************************************************************************************************



# *************************************************************************************************
#### 2.6. Guardo el dataframe analizado ####
# Guardo el dataset totalmente normalizado
finalFlights <- flights
write.table(finalFlights, file = "data/finalFlights.csv", append = FALSE, sep = ",", eol = "\n", row.names = FALSE, na = "", col.names = TRUE)
# *************************************************************************************************



# *************************************************************************************************
#### 2.7. Elimino signaciones innecesarias ####
rm(TimeRoute)
# *************************************************************************************************


