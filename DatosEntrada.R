#### TFM: ANÁLISIS DEL IMPACTO DEL CLIMA EN LOS VUELOS ####


#### Análisis del dataset de entrada ####

# Los datos de los vuelos se obtienen de: https://www.transtats.bts.gov/Tables.asp?DB_ID=120 tiene datos desde 1987 hasta 2018, actualizándose
# mensualmente
# Overview: This database contains scheduled and actual departure and arrival times reported by certified U.S. air carriers that account for at least 
#           one percent of domestic scheduled passenger revenues. The data is collected by the Office of Airline Information, Bureau of 
#           Transportation Statistics (BTS).
# Coverage: US certified air carriers that account for at least one percent of domestic scheduled passenger revenues.
# Availability: 
#               First Year:	1987
#               Last Year: 	2018
#               Frequency: 	Monthly
# Data Tables:  On-Time Performance
#               This table contains on-time arrival data for non-stop domestic flights by major air carriers, and provides such additional items as 
#               departure and arrival delays, origin and destination airports, flight numbers, scheduled and actual departure and arrival times, 
#               cancelled or diverted flights, taxi-out and taxi-in times, air time, and non-stop distance.
# Terms and Definitions
# Actual Arrival Times:	Gate arrival time is the instance when the pilot sets the aircraft parking brake after arriving at the airport gate or 
#                       passenger unloading area.  If the parking brake is not set, record the time for the opening of the passenger door.  Also, 
#                       carriers using a Docking Guidance System (DGS) may record the official gate-arrival time when the aircraft is stopped at the 
#                       appropriate parking mark.
# Actual Departure Times: Gate departure time is the instance when the pilot releases the aircraft parking brake after passengers have loaded and 
#                         aircraft doors have been closed. In cases where the flight returned to the departure gate before wheels-off time and 
#                         departed a second time, report the last gate departure time before wheels-off time.  In cases of an air return, report the 
#                         last gate departure time before the gate return.  If passengers were boarded without the parking brake being set, record 
#                         the time that the passenger door was closed.  Also, carriers using a Docking Guidance System may record the official 
#                         gate-departure time based on aircraft movement.  For example, one DGS records gate departure time when the aircraft moves 
#                         more than 1 meter from the appropriate parking mark within 15 seconds.  Fifteen seconds is then subtracted from the 
#                         recorded time to obtain the appropriate out time.
# Airline ID: An identification number assigned by US DOT to identify a unique airline (carrier). A unique airline (carrier) is defined as one 
#             holding and reporting under the same DOT certificate regardless of its Code, Name, or holding company/corporation. Use this field for 
#             analysis across a range of years.
# Airport Code: A three character alpha-numeric code issued by the U.S. Department of Transportation which is the official designation of the airport. 
#               The airport code is not always unique to a specific airport because airport codes can change or can be reused.
# Airport ID: An identification number assigned by US DOT to identify a unique airport. Use this field for airport analysis across a range of years 
#             because an airport can change its airport code and airport codes can be reused.
# Arrival Delay:  Arrival delay equals the difference of the actual arrival time minus the scheduled arrival time. A flight is considered on-time 
#                 when it arrives less than 15 minutes after its published arrival time.
# CRS:  Computer Reservation System. CRS provide information on airline schedules, fares and seat availability to travel agencies and allow agents to 
#       book seats and issue tickets.
# Cancelled Flight: A flight that was listed in a carrier's computer reservation system during the seven calendar days prior to scheduled departure 
#                   but was not operated.
# Carrier Code: Code assigned by IATA and commonly used to identify a carrier. As the same code may have been assigned to different carriers over 
#               time, the code is not always unique.
# Certificate Of Public Convenience And Necessity:  A certificate issued to an air carrier under 49 U.S.C. 41102, by the Department of Transportation 
#                                                   authorizing the carrier to engage in air transportation.
# Certificated Air Carrier: An air carrier holding a Certificate of Public Convenience and Necessity issued by DOT to conduct scheduled services 
#                           interstate. Nonscheduled or charter operations may also be conducted by these carriers. (same as Certified Air Carrier)
# Certified Air Carrier:  An air carrier holding a Certificate of Public Convenience and Necessity issued by DOT to conduct scheduled services 
#                         interstate. Nonscheduled or charter operations may also be conducted by these carriers. (same as Certificated Air Carrier)
# City Market ID: An identification number assigned by US DOT to identify a city market. Use this field to consolidate airports serving the same city 
#                 market.
# Departure Delay:  The difference between the scheduled departure time and the actual departure time from the origin airport gate.
# Diverted Flight:  A flight that is required to land at a destination other than the original scheduled destination for reasons beyond the control 
#                   of the pilot/company.
# Domestic Operations:  All air carrier operations having destinations within the 50 United States, the District of Columbia, the Commonwealth of 
#                       Puerto Rico, and the U.S. Virgin Islands.
# Elapsed Time: The time computed from gate departure time to gate arrival time.
# FIPS: Federal Information Processing Standards. Usually referring to a code assigned to any of a variety of geographic entities (e.g. counties, 
#       states, metropolitan areas, etc). FIPS codes are intended to simplify the collection, processing, and dissemination of data and resources of 
#       the Federal Government.
# Flight Number:  A one to four character alpha-numeric code for a particular flight.
# In-Flight Time: The total time an aircraft is in the air between an origin-destination airport pair, i.e. from wheels-off at the origin airport to 
#                 wheels-down at the destination airport.
# Late Flight:  A flight arriving or departing 15 minutes or more after the scheduled time.
# Passenger Revenues: Revenues from the air transportation of passengers.
# Scheduled Departure Time: The scheduled time that an aircraft should lift off from the origin airport.
# Scheduled Time Of Arrival:  The scheduled time that an aircraft should cross a certain point (landing or metering fix).
# Taxi-In Time: The time elapsed between wheels down and arrival at the destination airport gate.
# Taxi-Out Time:  The time elapsed between departure from the origin airport gate and wheels off.
# Unique Carrier: Unique Carrier Code. It is the Carrier Code most recently used by a carrier. A numeric suffix is used to distinguish duplicate 
#                 codes, for example, PA, PA (1), PA (2). Use this field to perform analysis of data reported by one and only one carrier.
# World Area Code (WAC):  Numeric codes used to identify geopolitical areas such as countries, states (U.S.), provinces (Canada), and territories or 
#                         possessions of certain countries. The codes are used within the various data banks maintained by the Office of Airline 
#                         Information (OAI) and are created by OAI.




# Trabajamos sobre el directorio donde están los datos: "/Users/ellorentesj/Desktop/TFM/MI_TFM"
vuelos <- read.table("On_Time_On_Time_Performance_2018_1.csv",header=T,sep=',')

str(vuelos)
# 'data.frame':	570131 obs. of  110 variables: (int, Factor, num, logi)
# Year:	                Year
# Quarter:	            Quarter (1-4)
# Month:	              Month
# DayofMonth:	          Day of Month
# DayOfWeek:	          Day of Week
# FlightDate:	          Flight Date (yyyymmdd)
# UniqueCarrier:	      Unique Carrier Code. When the same code has been used by multiple carriers, a numeric suffix is used for earlier users, for 
#                       example, PA, PA(1), PA(2). Use this field for analysis across a range of years.
# AirlineID:    	      An identification number assigned by US DOT to identify a unique airline (carrier). A unique airline (carrier) is defined as 
#                       one holding and reporting under the same DOT certificate regardless of its Code, Name, or holding company/corporation.
# Carrier:        	    Code assigned by IATA and commonly used to identify a carrier. As the same code may have been assigned to different carriers 
#                       over time, the code is not always unique. For analysis, use the Unique Carrier Code.
# TailNum:      	      Tail Number
# FlightNum:    	      Flight Number
# OriginAirportID:	    Origin Airport, Airport ID. An identification number assigned by US DOT to identify a unique airport. Use this field for 
#                       airport analysis across a range of years because an airport can change its airport code and airport codes can be reused.
# OriginAirportSeqID:	  Origin Airport, Airport Sequence ID. An identification number assigned by US DOT to identify a unique airport at a given 
#                       point of time. Airport attributes, such as airport name or coordinates, may change over time.
# OriginCityMarketID	  Origin Airport, City Market ID. City Market ID is an identification number assigned by US DOT to identify a city market. Use 
#                       this field to consolidate airports serving the same city market.
# Origin:           	  Origin Airport
# OriginCityName:   	  Origin Airport, City Name
# OriginState:      	  Origin Airport, State Code
# OriginStateFips:  	  Origin Airport, State Fips
# OriginStateName:  	  Origin Airport, State Name
# OriginWac:        	  Origin Airport, World Area Code
# DestAirportID:    	  Destination Airport, Airport ID. An identification number assigned by US DOT to identify a unique airport. Use this field for 
#                       airport analysis across a range of years because an airport can change its airport code and airport codes can be reused.
# DestAirportSeqID: 	  Destination Airport, Airport Sequence ID. An identification number assigned by US DOT to identify a unique airport at a given 
#                       point of time. Airport attributes, such as airport name or coordinates, may change over time.
# DestCityMarketID: 	  Destination Airport, City Market ID. City Market ID is an identification number assigned by US DOT to identify a city market. 
#                       Use this field to consolidate airports serving the same city market.
# Dest:             	  Destination Airport
# DestCityName:     	  Destination Airport, City Name
# DestState:        	  Destination Airport, State Code
# DestStateFips:    	  Destination Airport, State Fips
# DestStateName:    	  Destination Airport, State Name
# DestWac:          	  Destination Airport, World Area Code
# CRSDepTime:       	  CRS Departure Time (local time: hhmm)
# DepTime:          	  Actual Departure Time (local time: hhmm)
# DepDelay:         	  Difference in minutes between scheduled and actual departure time. Early departures show negative numbers.
# DepDelayMinutes:  	  Difference in minutes between scheduled and actual departure time. Early departures set to 0.
# DepDel15:         	  Departure Delay Indicator, 15 Minutes or More (1=Yes)
# DepartureDelayGroups:	Departure Delay intervals, every (15 minutes from <-15 to >180)
# DepTimeBlk:         	CRS Departure Time Block, Hourly Intervals
# TaxiOut:            	Taxi Out Time, in Minutes
# WheelsOff:          	Wheels Off Time (local time: hhmm)
# WheelsOn:           	Wheels On Time (local time: hhmm)
# TaxiIn:             	Taxi In Time, in Minutes
# CRSArrTime:         	CRS Arrival Time (local time: hhmm)
# ArrTime:            	Actual Arrival Time (local time: hhmm)
# ArrDelay:           	Difference in minutes between scheduled and actual arrival time. Early arrivals show negative numbers.
# ArrDelayMinutes:    	Difference in minutes between scheduled and actual arrival time. Early arrivals set to 0.
# ArrDel15:           	Arrival Delay Indicator, 15 Minutes or More (1=Yes)
# ArrivalDelayGroups: 	Arrival Delay intervals, every (15-minutes from <-15 to >180)
# ArrTimeBlk:         	CRS Arrival Time Block, Hourly Intervals
# Cancelled:          	Cancelled Flight Indicator (1=Yes)
# CancellationCode:   	Specifies The Reason For Cancellation
# Diverted:           	Diverted Flight Indicator (1=Yes)
# CRSElapsedTime:     	CRS Elapsed Time of Flight, in Minutes
# ActualElapsedTime:  	Elapsed Time of Flight, in Minutes
# AirTime:            	Flight Time, in Minutes
# Flights:            	Number of Flights
# Distance:           	Distance between airports (miles)
# DistanceGroup:      	Distance Intervals, every 250 Miles, for Flight Segment
# CarrierDelay:       	Carrier Delay, in Minutes
# WeatherDelay:       	Weather Delay, in Minutes
# NASDelay:           	National Air System Delay, in Minutes
# SecurityDelay:      	Security Delay, in Minutes
# LateAircraftDelay:  	Late Aircraft Delay, in Minutes
# FirstDepTime:       	First Gate Departure Time at Origin Airport
# TotalAddGTime:      	Total Ground Time Away from Gate for Gate Return or Cancelled Flight
# LongestAddGTime:    	Longest Time Away from Gate for Gate Return or Cancelled Flight
# DivAirportLandings: 	Number of Diverted Airport Landings
# DivReachedDest:     	Diverted Flight Reaching Scheduled Destination Indicator (1=Yes)
# DivActualElapsedTime:	Elapsed Time of Diverted Flight Reaching Scheduled Destination, in Minutes. The ActualElapsedTime column remains NULL for all 
#                       diverted flights.
# DivArrDelay:        	Difference in minutes between scheduled and actual arrival time for a diverted flight reaching scheduled destination. The 
#                       ArrDelay column remains NULL for all diverted flights.
# DivDistance:        	Distance between scheduled destination and final diverted airport (miles). Value will be 0 for diverted flight reaching 
#                       scheduled destination.
# Div1Airport:        	Diverted Airport Code1
# Div1AirportID:      	Airport ID of Diverted Airport 1. Airport ID is a Unique Key for an Airport
# Div1AirportSeqID:   	Airport Sequence ID of Diverted Airport 1. Unique Key for Time Specific Information for an Airport
# Div1WheelsOn:       	Wheels On Time (local time: hhmm) at Diverted Airport Code1
# Div1TotalGTime:     	Total Ground Time Away from Gate at Diverted Airport Code1
# Div1LongestGTime:   	Longest Ground Time Away from Gate at Diverted Airport Code1
# Div1WheelsOff:      	Wheels Off Time (local time: hhmm) at Diverted Airport Code1
# Div1TailNum:        	Aircraft Tail Number for Diverted Airport Code1
# Div2Airport:        	Diverted Airport Code2
# Div2AirportID:      	Airport ID of Diverted Airport 2. Airport ID is a Unique Key for an Airport
# Div2AirportSeqID:   	Airport Sequence ID of Diverted Airport 2. Unique Key for Time Specific Information for an Airport
# Div2WheelsOn:       	Wheels On Time (local time: hhmm) at Diverted Airport Code2
# Div2TotalGTime:     	Total Ground Time Away from Gate at Diverted Airport Code2
# Div2LongestGTime:   	Longest Ground Time Away from Gate at Diverted Airport Code2
# Div2WheelsOff:      	Wheels Off Time (local time: hhmm) at Diverted Airport Code2
# Div2TailNum:        	Aircraft Tail Number for Diverted Airport Code2
# Div3Airport:        	Diverted Airport Code3
# Div3AirportID:      	Airport ID of Diverted Airport 3. Airport ID is a Unique Key for an Airport
# Div3AirportSeqID:   	Airport Sequence ID of Diverted Airport 3. Unique Key for Time Specific Information for an Airport
# Div3WheelsOn:       	Wheels On Time (local time: hhmm) at Diverted Airport Code3
# Div3TotalGTime:     	Total Ground Time Away from Gate at Diverted Airport Code3
# Div3LongestGTime:   	Longest Ground Time Away from Gate at Diverted Airport Code3
# Div3WheelsOff:      	Wheels Off Time (local time: hhmm) at Diverted Airport Code3
# Div3TailNum:        	Aircraft Tail Number for Diverted Airport Code3
# Div4Airport:        	Diverted Airport Code4
# Div4AirportID:      	Airport ID of Diverted Airport 4. Airport ID is a Unique Key for an Airport
# Div4AirportSeqID:   	Airport Sequence ID of Diverted Airport 4. Unique Key for Time Specific Information for an Airport
# Div4WheelsOn:       	Wheels On Time (local time: hhmm) at Diverted Airport Code4
# Div4TotalGTime:     	Total Ground Time Away from Gate at Diverted Airport Code4
# Div4LongestGTime:   	Longest Ground Time Away from Gate at Diverted Airport Code4
# Div4WheelsOff:      	Wheels Off Time (local time: hhmm) at Diverted Airport Code4
# Div4TailNum:        	Aircraft Tail Number for Diverted Airport Code4
# Div5Airport:        	Diverted Airport Code5
# Div5AirportID:      	Airport ID of Diverted Airport 5. Airport ID is a Unique Key for an Airport
# Div5AirportSeqID:   	Airport Sequence ID of Diverted Airport 5. Unique Key for Time Specific Information for an Airport
# Div5WheelsOn:       	Wheels On Time (local time: hhmm) at Diverted Airport Code5
# Div5TotalGTime:     	Total Ground Time Away from Gate at Diverted Airport Code5
# Div5LongestGTime:   	Longest Ground Time Away from Gate at Diverted Airport Code5
# Div5WheelsOff:      	Wheels Off Time (local time: hhmm) at Diverted Airport Code5
# Div5TailNum:        	Aircraft Tail Number for Diverted Airport Code5

##### 1. Visualización de los datos #####
summary(vuelos)

#### 2. Eliminación de variables que no contienen información ####
vuelos$Div3Airport <- NULL
vuelos$Div3AirportID <- NULL
vuelos$Div3AirportSeqID <- NULL
vuelos$Div3WheelsOn <- NULL
vuelos$Div3TotalGTime <- NULL
vuelos$Div3LongestGTime <- NULL
vuelos$Div3WheelsOff <- NULL
vuelos$Div3TailNum <- NULL
vuelos$Div4Airport <- NULL
vuelos$Div4AirportID <- NULL
vuelos$Div4AirportSeqID <- NULL
vuelos$Div4WheelsOn <- NULL
vuelos$Div4TotalGTime <- NULL
vuelos$Div4WheelsOff <- NULL
vuelos$Div4LongestGTime <- NULL
vuelos$Div4TailNum <- NULL
vuelos$Div5Airport <- NULL
vuelos$Div5AirportID <- NULL
vuelos$Div5AirportSeqID <- NULL
vuelos$Div5WheelsOn <- NULL
vuelos$Div5TotalGTime <- NULL
vuelos$Div5LongestGTime <- NULL
vuelos$Div5WheelsOff <- NULL
vuelos$Div5TailNum <- NULL
vuelos$X <- NULL

#### 3. Revisión del resto de variables ####
# Reducido a 85 variables, de las cuales nos quedamos con:
# Retrasos mayores de 15 minutos
vuelosDepDel <- vuelos[vuelos$DepDel15==1,] #116490 registros
vuelosArrDel <- vuelos[vuelos$ArrDel15==1,] #116392 registros
# Vuelos cancelados
vuelosCan <- vuelos[vuelos$Cancelled==1,] #17175 registros
# Vuelos desviados
vuelosDiv <- vuelos[vuelos$Diverted==1,] #1255 registros

#### 3.1. Vuelos retrasados ####
str(vuelosDepDel)
summary(vuelosDepDel$DepDelay)
summary(vuelosDepDel$DepDelayMinutes)
