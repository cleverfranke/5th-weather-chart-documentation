C°F 5th Weather Chart - Weather, (USA) Analysis
================================================

About
-----

Weather data by day can be downloaded from the NOAA FTP. There is one file per year, and each file contains several millions rows. Therefore, manipulating these files can take time.

The treatment of the weather data is similar accross the various analysis we made:

-   Download yearly data from the NOAA FTP
-   Merge with weather station data to have geolocation information, and potentially filter out stations that are not in the area of analysis
-   Reshape weather data to have one weather variable per column
-   Format the date and convert the weather variable to the desired units
-   Aggregate per time variable (day, week, year), or aggregate per weather variable (temperature, rain etc.)

In the following section you will find more details for each step.

Sources
-------

-   NOAA Weather: <ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/by_year/>
-   NOAA Weather stations: <ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt>
-   NOAA Weather type definitions: <ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/by_year/ghcn-daily-by_year-format.rtf>

Menne, M.J., I. Durre, R.S. Vose, B.E. Gleason, and T.G. Houston, 2012: An overview of the Global Historical Climatology Network-Daily Database. Journal of Atmospheric and Oceanic Technology, 29, 897-910, <doi:10.1175/JTECH-D-11-00103.1>.

Methodology
-----------

``` r
source('../utils_US.R')
library(plyr)
library(reshape)
library(dplyr)
```

### The weather data

First let's take a look at a raw file: we will use as an example the dataset from 2015, previously downloaded from the NOAA FTP.

``` r
weather_data <- read.csv('../data/2015.csv', header=FALSE)
head(weather_data)
```

    ##            V1       V2   V3   V4 V5 V6 V7  V8
    ## 1 US1FLSL0019 20150101 PRCP  173        N  NA
    ## 2 US1TXTV0133 20150101 PRCP  119        N  NA
    ## 3 USC00178998 20150101 TMAX  -33        7 700
    ## 4 USC00178998 20150101 TMIN -167        7 700
    ## 5 USC00178998 20150101 TOBS  -67        7 700
    ## 6 USC00178998 20150101 PRCP    0        7 700

``` r
nrow(weather_data)
```

    ## [1] 34327840

The first column is the weather station ID, then the date, then the type of weather variable, and finally the corresponding value. The following columns are not needed. Let's format the column names and and date values

``` r
colnames(weather_data) <- c("station_id", "date", 'type', 'value')
weather_data$date <- as.Date(as.character(weather_data$date), "%Y%m%d")
weather_data <- weather_data[c('station_id', 'date', 'type', 'value')]
```

### The weather stations data

The weather stations ID, name, and location can be found on the NOAA FTP as well.

``` r
us_stations <-  read.fwf(file="../data/us_stations.txt", header=FALSE, widths=c(11, 10, 10, 7, 30))
colnames(us_stations) <- c('station_id', 'lat', 'lon', 'alt', 'station_name')
head(us_stations)
```

    ##    station_id     lat      lon   alt                   station_name
    ## 1 ACW00011604 17.1167 -61.7833  10.1    ST JOHNS COOLIDGE FLD      
    ## 2 ACW00011647 17.1333 -61.7833  19.2    ST JOHNS                   
    ## 3 AE000041196 25.3330  55.5170  34.0    SHARJAH INTER. AIRP        
    ## 4 AEM00041194 25.2550  55.3640  10.4    DUBAI INTL                 
    ## 5 AEM00041217 24.4330  54.6510  26.8    ABU DHABI INTL             
    ## 6 AEM00041218 24.2620  55.6090 264.9    AL AIN INTL

### Format the weather data to one type per column

To be able to merge the weather data with another dataset later on, we want to have the types in the `type` column as columns, and the `value` column as row values. For this, we will use the `reshape` function from the *reshape* package.

This operation costs a lot of time, and therefore it is better to filter the `weather_data` dataset ahead with only the needed data. For example, if the data is needed for only a certain geographic area, like a state or a city, we would filter the data to get only the weather data from the weather stations of the area. Similarly, if we only need temperature data, we could filter the `type` column ahead with only `TMIN` and `TMAX` values.

``` r
# Filter on TMIN and TMAX for example
weather_data_temp <- weather_data[weather_data$type %in% (c('TMIN', 'TMAX')), ]
nrow(weather_data_temp)
```

    ## [1] 9084168

``` r
# Merge weather data with stations data
weather_data_temp_stn <- merge(weather_data_temp, us_stations)

# Filter on Florida stations for example
weather_data_temp_stn$code <- substr(weather_data_temp_stn$station_name, 1, 2)
weather_data_temp_florida <- weather_data_temp_stn[ weather_data_temp_stn$code == 'FL', ]

head(weather_data_temp_florida)
```

    ##          station_id       date type value     lat      lon alt
    ## 3753715 USC00080211 2015-03-21 TMAX   267 29.7258 -85.0206 6.1
    ## 3753716 USC00080211 2015-07-27 TMIN   239 29.7258 -85.0206 6.1
    ## 3753717 USC00080211 2015-08-13 TMIN   256 29.7258 -85.0206 6.1
    ## 3753718 USC00080211 2015-12-19 TMIN    33 29.7258 -85.0206 6.1
    ## 3753719 USC00080211 2015-06-29 TMIN   244 29.7258 -85.0206 6.1
    ## 3753720 USC00080211 2015-03-29 TMIN    72 29.7258 -85.0206 6.1
    ##                           station_name code
    ## 3753715 FL APALACHICOLA  AIRPORT         FL
    ## 3753716 FL APALACHICOLA  AIRPORT         FL
    ## 3753717 FL APALACHICOLA  AIRPORT         FL
    ## 3753718 FL APALACHICOLA  AIRPORT         FL
    ## 3753719 FL APALACHICOLA  AIRPORT         FL
    ## 3753720 FL APALACHICOLA  AIRPORT         FL

Now we can reshape the dataset:

``` r
weather_data_temp_florida <- weather_data_temp_florida[c('station_id', 'date', 'type', 'value')]
weather_data_res <- reshape(weather_data_temp_florida,timevar="type",idvar=c("station_id", "date"),direction="wide")
head(weather_data_res)
```

    ##          station_id       date value.TMAX value.TMIN
    ## 3753715 USC00080211 2015-03-21        267        200
    ## 3753716 USC00080211 2015-07-27        350        239
    ## 3753717 USC00080211 2015-08-13        344        256
    ## 3753718 USC00080211 2015-12-19        156         33
    ## 3753719 USC00080211 2015-06-29        328        244
    ## 3753720 USC00080211 2015-03-29        183         72

You can see now that the dataset has an acceptable structure to work with.

### Format values

The weather type definition file defines the units of the data. Depending on what weather variables are needed, it might be helpful to convert the units. For example, converting the temperature from 0.1C to 1C unit:

``` r
weather_data_res$value.TMIN <- weather_data_res$value.TMIN * 0.1
weather_data_res$value.TMAX <- weather_data_res$value.TMAX * 0.1
```

### Aggregate weather per day

When the geographical area of the analysis was small enough, we would average the weather data from all the surrounding weather stations, per day. For example, we consider a city like New York to be small enough to approximate the weather of the whole city as the mean of all the weather data from the stations around the city. On the contrary, we consider that an entire state is too big for such an approximation.

Just for the example, this is how we would do it :

``` r
weather_data_perday <- weather_data_res %>% group_by(date) %>% summarise(meanTMIN = mean(value.TMIN, na.rm=TRUE), meanTMAX = mean(value.TMAX, na.rm=TRUE))
head(weather_data_perday)
```

    ## # A tibble: 6 x 3
    ##         date meanTMIN meanTMAX
    ##       <date>    <dbl>    <dbl>
    ## 1 2015-01-01 14.65988 21.73434
    ## 2 2015-01-02 15.73550 23.68876
    ## 3 2015-01-03 18.32381 26.00417
    ## 4 2015-01-04 18.52308 27.29822
    ## 5 2015-01-05 14.68092 25.03584
    ## 6 2015-01-06 11.88166 23.39277

Note
----

The US weather data is stored in .csv files that weight around 1GB for each year. Processing these files can take some time. Therefore, in some of the data analysis documentation, you will not see the weather data treatment from the raw data: instead we load a preprocessed data file. The pre-processing consists of: - Subset the data for the desired weather stations - Reshape the structure of the data file - Subset the data for the relevant weather variables
