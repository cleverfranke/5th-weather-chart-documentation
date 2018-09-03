library(plyr)
library(lubridate)
library(sp)
library(rgeos)
library(magrittr)
library(dplyr)

# Get CSV
getCSV <- function(pathToFile) {
  file <- read.csv(pathToFile, header=TRUE, sep=",")
  return(file)
}


# Get Daily weather from KNMI
getKNMIDailyData <- function(startYear, startMonth, startDay, endYear, endMonth, endDay){
  url0 <- 'http://projects.knmi.nl/klimatologie/daggegevens/getdata_dag.cgi?stns='
  url1 <- '&vars='
  url2 <- '&byear='
  url3 <- '&bmonth='
  url4 <- '&bday='
  url5 <- '&eyear='
  url6 <- '&emonth='
  url7 <- '&eday='


  url <- paste(url0, 'ALL', url1, 'all', url2, startYear, url3, startMonth, url4, startDay, url5, endYear, url6, endMonth, url7, endDay, sep="")
  print(url)

  data <- read.csv(url, header=FALSE, sep=",", comment.char="#", na.strings=" ")
  # View(data)

  colnames(data) <- c('STN','YYYYMMDD','DDVEC','FHVEC','FG','FHX','FHXH','FHN','FHNH','FXX','FXXH','TG','TN','TNH','TX','TXH','T10N','T10NH','SQ','SP','Q','DR','RH','RHX','RHXH','EV24','PG','PX','PXH','PN','PNH','VVN','VVNH','VVX','VVXH','NG','UG','UX','UXH','UN','UNH')

  return(data)
}

# Get KNMI Hourly data
getKNMIHourlyData <- function(startDate, endDate){
  url0 <- 'http://projects.knmi.nl/klimatologie/uurgegevens/getdata_uur.cgi?stns='
  url1 <- '&vars='
  url2 <- '&start='
  url3 <- '&end'

  url <- paste(url0, 'ALL', url1, 'all', url2, startDate, url3, endDate, sep="")
  print(url)

  data <- read.csv(url, header=FALSE, sep=",", comment.char="#", na.strings=" ")

  colnames(data) <- c('STN', 'YYYYMMDD',   'HH',   'DD',   'FH',   'FF',   'FX',    'T',  'T10',   'TD',   'SQ',    'Q',   'DR',   'RH',    'P',   'VV',    'N',    'U',   'WW',   'IX',    'M',    'R',    'S',    'O',    'Y')

  return(data)
}

# Merge station data (lat long etc.) and weather data
mergeWeatherAndStation <- function(weather, stations, stationId) {
  data <- merge(weather, stations, by=stationId)
  return(data)
}


# Format NL weather data
formatNLWeatherDailyData <- function(weather){
  # Date
  weather$date <- as.Date(as.character(weather$YYYYMMDD), "%Y%m%d")
  
  # Select fields we need
  weather <- weather[c('STN', 'date', 'FG', 'FHN', 'FHX', 'TG', 'TN', 'TX', 'SQ', 'DR', 'RH', 'LON', 'LAT')]

  # Units (m/s to km/h, 0.1mm to mm, and 0.1C to C)
  weather$FG <- weather$FG * 0.1 * 3.6
  weather$FHN <- weather$FHN * 0.1 * 3.6
  weather$FHX <- weather$FHX * 0.1 * 3.6

  weather$TG <- weather$TG * 0.1
  weather$TN <- weather$TN * 0.1
  weather$TX <- weather$TX * 0.1

  weather$SQ <- weather$SQ * 0.1

  weather$DR <- weather$DR * 0.1
  weather$RH <- weather$RH * 0.1

  weather$RH[weather$RH < 0] <- 0

  # Round data for future categorization
  weather$roundedFG <- floor(weather$FG)
  weather$roundedFHN <- floor(weather$FHN)
  weather$roundedFHX <- floor(weather$FHX)

  weather$roundedTG <- floor(weather$TG)
  weather$roundedTN <- floor(weather$TN)
  weather$roundedTX <- floor(weather$TX)

  weather$roundedSQ <- floor(weather$SQ)

  weather$roundedDR <- floor(weather$DR)
  weather$roundedRH <- floor(weather$RH)

  return(weather)
}

# Format NL Hourly data
formatNLWeatherHourlyData <- function(weather){
  # Date
  weather$date <- as.Date(as.character(weather$YYYYMMDD), "%Y%m%d")
  weather$hour <- weather$HH -1

  # Select fields we need
  weather <- weather[c('STN', 'date', 'hour', 'T', 'RH', 'DR', 'SQ', 'M', 'R', 'S', 'O', 'Y')]

  # Units (m/s to km/h, 0.1mm to mm, and 0.1C to C)
  weather$T <- weather$T * 0.1
  weather$SQ <- weather$SQ * 0.1

  weather$DR <- weather$DR * 0.1
  weather$RH <- weather$RH * 0.1

  weather$RH[weather$RH < 0] <- 0

  # Round data for future categorization
  weather$roundedT <- floor(weather$T)
  weather$roundedSQ <- floor(weather$SQ)
  weather$roundedDR <- floor(weather$DR)
  weather$roundedRH <- floor(weather$RH)

  return(weather)
}

# Get NL weather per day, formatted and merged with station data
getNLWeatherDailyData <- function(startYear, startMonth, startDay, endYear, endMonth, endDay) {
  print('Getting KNMI Daily data')
  nl_stations <- getCSV('all_station_KNMI_cleaned.csv')

  getKNMIDailyData(startYear, startMonth, startDay, endYear, endMonth, endDay) %>%
    mergeWeatherAndStation(nl_stations, 'STN') %>%
    formatNLWeatherDailyData() %>%
    return
}

# Get NL weather per hour, formatted and merged with station data
getNLWeatherHourlyData <- function(startDate, endDate) {
  print('Getting KNMI Hourly data')
  nl_stations <- getCSV('all_station_KNMI_cleaned.csv')

  getKNMIHourlyData(startDate, endDate) %>%
    mergeWeatherAndStation(nl_stations, 'STN') %>%
    formatNLWeatherHourlyData() %>%
    return
}

# Get closest station to location
# Location and StationLocation MUST have a LON and LAT field with this specific name.
getClosestWeatherStation <- function(location, stationLocation) {
  # Geocode locations
  sp.location <- location
  coordinates(sp.location) <- ~LON+LAT

  sp.stations <- stationLocation
  coordinates(sp.stations) <- ~LON+LAT

  # Get distance between all points
  d <- gDistance(sp.stations, sp.location, byid=TRUE)

  # Get minimum distance to point and merge it with corresponding weather station
  min.d <- apply(d, 1, function(x) order(x, decreasing=F)[1])
  closest_stations <- cbind(location, stationLocation[min.d,], apply(d, 1, function(x) sort(x, decreasing=F)[1]))

  return(closest_stations)
}
