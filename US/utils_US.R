library(plyr)
library(lubridate)
library(sp)
library(rgeos)
library(magrittr)
library(reshape)
library(plyr)

# GET THE DATA #
get_us_stations_data <- function() {
    # US STATIONS #
    us_stations <-  read.fwf(file="../data/us_stations.txt", header=FALSE, widths=c(11, 10, 10, 7, 30))
    # Put right header
    colnames(us_stations) <- c('station_id', 'lat', 'lon', 'alt', 'station_name')
    us_stations <- us_stations[ us_stations$lat > 24 & us_stations$lat < 49 & us_stations$lon < -66 & us_stations$lon > -125, ]

    return(us_stations)
}


# Get CSV
getCSV <- function(pathToFile) {
  file <- read.csv(pathToFile, header=TRUE, sep=",")
  return(file)
}


get_us_weather_data <- function(year) {
    # US WEATHER #
    # Go there ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/by_year/ to download another year
    weather_data <- read.csv(paste(year,".csv", sep=""), header=FALSE)

    colnames(weather_data) <- c("station_id", "date", 'type', 'value')

    weather_data$date <- as.Date(as.character(weather_data$date), "%Y%m%d")

    weather_data <- weather_data[c('station_id', 'date', 'type', 'value')]
    return(weather_data)
}


avg_weather_data <- function(weather_data) {
    weather_data <- aggregate(x=weather_data$value, by=list(weather_data$date,weather_data$type), FUN=mean)
    colnames(weather_data) <- c('date', 'type', 'value')
    weather_data <- reshape(weather_data,timevar="type",idvar="date",direction="wide")
    return(weather_data)
}
