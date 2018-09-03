C°F 5th Weather Chart - Bike Shares, (US, New York) Analysis
=============================================================

About
-----

In this analysis, we compared the number of bike shares with temperature, rainfall and snow depth.

The bike share data comes from Citibike: "Citi Bike is New York City's bike share system. \[...\] Citi Bike is available for use 24 hours/day, 7 days/week, 365 days/year, and riders have access to thousands of bikes at hundreds of stations across Manhattan, Brooklyn, Queens and Jersey City. [1]"

The steps taken for this analysis are:

-   Get bike share data
-   Count the number of bike share per day, and total duration of all the trips per day
-   Merge with weather data
-   Calculate average number of bike shares per day and average trip duration per day when there is no rain and when there is rain
-   Calculate average number of bike shares per day and average trip duration per day when there is no snow on the ground and when there is no snow on the ground
-   Calculate temperature terciles

In the following section you will find more details for each step.

Sources
-------

-   Citibike System Data: <https://www.citibikenyc.com/system-data>
-   NOAA Weather : <ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/by_year/>

Methodology
-----------

``` r
source('../utils_US.R')
library(plyr)
library(ggplot2)
library(lubridate)
```

### Get citibike data

Citibike datasets are available by month. We used 2015 data only. After binding the different months dataset together, the citibike dataset looks like this:

``` r
citibike <- read.csv('../data/US_NY_Citibike_all.csv')
citibike$date <- as.Date(citibike$starttime, "%m/%d/%Y")
head(citibike)
```

    ##   X tripduration     starttime      stoptime start.station.id
    ## 1 1         1346 1/1/2015 0:01 1/1/2015 0:24              455
    ## 2 2          363 1/1/2015 0:02 1/1/2015 0:08              434
    ## 3 3          346 1/1/2015 0:04 1/1/2015 0:10              491
    ## 4 4          182 1/1/2015 0:04 1/1/2015 0:07              384
    ## 5 5          969 1/1/2015 0:05 1/1/2015 0:21              474
    ## 6 6          496 1/1/2015 0:07 1/1/2015 0:15              512
    ##        start.station.name start.station.latitude start.station.longitude
    ## 1         1 Ave & E 44 St               40.75002               -73.96905
    ## 2         9 Ave & W 18 St               40.74317               -74.00366
    ## 3    E 24 St & Park Ave S               40.74096               -73.98602
    ## 4 Fulton St & Waverly Ave               40.68318               -73.96596
    ## 5         5 Ave & E 29 St               40.74517               -73.98683
    ## 6         W 29 St & 9 Ave               40.75007               -73.99839
    ##   end.station.id            end.station.name end.station.latitude
    ## 1            265    Stanton St & Chrystie St             40.72229
    ## 2            482             W 15 St & 7 Ave             40.73936
    ## 3            505             6 Ave & W 33 St             40.74901
    ## 4            399 Lafayette Ave & St James Pl             40.68852
    ## 5            432           E 7 St & Avenue A             40.72622
    ## 6            383  Greenwich Ave & Charles St             40.73524
    ##   end.station.longitude bikeid   usertype birth.year gender       date
    ## 1             -73.99148  18660 Subscriber       1960      2 2015-01-01
    ## 2             -73.99932  16085 Subscriber       1963      1 2015-01-01
    ## 3             -73.98848  20845 Subscriber       1974      1 2015-01-01
    ## 4             -73.96476  19610 Subscriber       1969      1 2015-01-01
    ## 5             -73.98380  20197 Subscriber       1977      1 2015-01-01
    ## 6             -74.00027  20788 Subscriber       1969      2 2015-01-01

Then we count the amount of bike trips per day, and the total duration of all the trips per day. Trip durations are given in seconds.

``` r
citibike_count <- ddply( citibike , .(date) , summarise , Count=length(date), sumTime=sum(tripduration))
head(citibike_count)
```

    ##         date Count sumTime
    ## 1 2015-01-01  5317 4263206
    ## 2 2015-01-02 11304 8265948
    ## 3 2015-01-03  4478 2934363
    ## 4 2015-01-04  7849 5333821
    ## 5 2015-01-05 14506 9252098
    ## 6 2015-01-06  8739 5548587

### Get the weather

For more details on how the weather data was treated, see the related reference. As we are considering the city center of New York, we decided to average the weather data of that area, by date.

``` r
weather_data <- read.csv('../data/weather_NY_2015.csv')
weather_data$date <- as.Date(weather_data$date, format="%Y-%m-%d")
```

### Merge citibike and weather

``` r
citibike_weather <- merge(citibike_count, weather_data, by='date')
head(citibike_weather)
```

    ##         date Count sumTime X  PRCP TMAX TMIN  AWND SNOW SNWD
    ## 1 2015-01-01  5317 4263206 1  0.00  3.9 -2.7 11.52    0    0
    ## 2 2015-01-02 11304 8265948 2  0.00  5.6  1.7 11.52    0    0
    ## 3 2015-01-03  4478 2934363 3  9.00  5.6  0.6 10.44    0    0
    ## 4 2015-01-04  7849 5333821 4 16.65 13.3  5.0 10.44    0    0
    ## 5 2015-01-05 14506 9252098 5  1.65  9.4 -6.0 16.92    0    0
    ## 6 2015-01-06  8739 5548587 6  1.30 -5.5 -7.1  6.48   25    0

### Snow

``` r
# Amount of trips depending on weather - Snow depth > 0
mean(citibike_weather[citibike_weather$SNWD > 0, 'Count'])
```

    ## [1] 7514.981

``` r
# Amount of trips depending on weather - Snow depth = 0
mean(citibike_weather[citibike_weather$SNWD == 0, 'Count'])
```

    ## [1] 30650.03

``` r
# Average trip duration per day - Snow depth > 0
(sum(as.numeric(citibike_weather[citibike_weather$SNWD > 0, 'sumTime'])) / sum(citibike_weather[citibike_weather$SNWD > 0, 'Count'])) / 60
```

    ## [1] 11.5203

``` r
# Average trip duration per day - Snow depth = 0
(sum(as.numeric(citibike_weather[citibike_weather$SNWD == 0, 'sumTime'])) / sum(citibike_weather[citibike_weather$SNWD == 0, 'Count'])) / 60
```

    ## [1] 16.33125

### Rain

``` r
# Amount of trips depending on weather - No rain < 0.01 inches (which is 0.254mm)
mean(citibike_weather[citibike_weather$PRCP < 0.254, 'Count'])
```

    ## [1] 29912.89

``` r
# Amount of trips depending on weather - Rain >= 0.01 inches (which is 0.254mm)
mean(citibike_weather[citibike_weather$PRCP >= 0.254, 'Count'])
```

    ## [1] 22133.24

``` r
# Average trip duration per day  - No rain < 0.01 inches (which is 0.254mm)
(sum(as.numeric(citibike_weather[citibike_weather$PRCP < 0.254, 'sumTime'])) / sum(citibike_weather[citibike_weather$PRCP < 0.254, 'Count'])) / 60
```

    ## [1] 16.39649

``` r
# Average trip duration per day  - Rain >= 0.01 inches (which is 0.254mm)
(sum(as.numeric(citibike_weather[citibike_weather$PRCP >= 0.254, 'sumTime'])) / sum(citibike_weather[citibike_weather$PRCP >= 0.254, 'Count'])) / 60
```

    ## [1] 15.46391

### Temperature

"The nth percentile of an observation variable is the value that cuts off the first n percent of the data values when it is sorted in ascending order."[2] Maximum temperature was sorted in ascending order and then cut into three parts.

-   The 1st part has 122 days
-   The 2nd part has 122 days
-   The 3rd part has 121 days

``` r
# Terciles
q <- quantile(citibike_weather$TMAX, seq(0,1, by=1/3))
q
```

    ##        0% 33.33333% 66.66667%      100% 
    ##      -7.1      13.3      25.6      36.1

``` r
# Order by max temperature
ordered <- citibike_weather[order(citibike_weather$TMAX),]

# Get the three sections
first <- ordered[1:122,]
second <- ordered[123:244,]
third <- ordered[245:365,]

# Amount of trips depending on weather - First
mean(first$Count)
```

    ## [1] 13571.01

``` r
# Amount of trips depending on weather - Second 
mean(second$Count)
```

    ## [1] 31548.52

``` r
# Amount of trips depending on weather - Third
mean(third$Count)
```

    ## [1] 36639.56

``` r
# Average trip duration per day - First
(sum(as.numeric(first$sumTime)) / sum(first$Count)) / 60
```

    ## [1] 13.39021

``` r
# Average trip duration per day - Second 
(sum(as.numeric(second$sumTime)) / sum(second$Count)) / 60
```

    ## [1] 16.81981

``` r
# Average trip duration per day - Third
(sum(as.numeric(third$sumTime)) / sum(third$Count)) / 60
```

    ## [1] 16.56506

### Map data

The map shows the amount of bike shares which started from a Citibike station. [3]

``` r
map_data <- ddply( citibike , .(start.station.id) , summarise , Count=length(start.station.id), lat=mean(start.station.latitude), lon=mean(start.station.longitude))
head(map_data)
```

    ##   start.station.id Count      lat       lon
    ## 1               72 27639 40.76727 -73.99393
    ## 2               79 22808 40.71912 -74.00667
    ## 3               82  9883 40.71117 -74.00017
    ## 4               83 11140 40.68383 -73.97632
    ## 5              116 46381 40.74178 -74.00150
    ## 6              119  1858 40.69609 -73.97803

Notes
-----

-   On the final graph, temperature is in Fahrenheit, rainfall and snow depth are in inches.
-   On the final graph, trip durations are converted to minutes and seconds.

[1] From <https://www.citibikenyc.com/about>

[2] From <http://www.r-tutor.com/elementary-statistics/numerical-measures/percentile>

[3] Because we group by station ID, the latitude and longitude are the same inside the groups, therefore taking the average of the latitude remains the latitude of the station.
