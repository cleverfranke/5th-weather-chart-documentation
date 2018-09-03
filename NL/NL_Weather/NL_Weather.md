C°F 5th Weather Chart - Weather (NL) Analysis
==============================================

About
-----

The Royal Netherlands Meteorological Institute (KNMI) is the Dutch national weather service. It is possible to retrieve hourly and daily weather data for a given date range and a list of desired weather stations.

The treatment of the weather data is similar accross the various analysis we made:

-   Request hourly or daily data from KNMI endpoints
-   Merge with weather station data to have geolocation information, and potentially filter out stations that are not in the area of the analysis
-   Format the date field and convert the weather variables fields to the desired units
-   Aggregate per time variable (day, week, year) and/or aggregate per weather variable (temperature, rain etc.)

In the following section you will find more details for each step.

Sources
-------

-   KNMI Hourly weather : <http://www.knmi.nl/nederland-nu/klimatologie/uurgegevens>
-   KNMI Daily weather : <http://www.knmi.nl/nederland-nu/klimatologie/daggegevens>
-   KNMI Retrieving data by script documentation : <http://www.knmi.nl/kennis-en-datacentrum/achtergrond/data-ophalen-vanuit-een-script>
-   KNMI weather code definitions :
    -   Daily <http://projects.knmi.nl/klimatologie/daggegevens/selectie.cgi>
    -   Hourly <http://projects.knmi.nl/klimatologie/uurgegevens/selectie.cgi>
-   KNMI Weather stations data is included in the daily and hourly weather requests.

Methodology
-----------

``` r
library(plyr)
library(lubridate)
library(sp)
library(rgeos)
library(magrittr)
```

### Get weather data

First let's take a look at a raw file: we will use as an example the dataset from 2015. The URL for the data request can be constructed with a list of weather stations code and a date range. By default we always request all the stations available. The date range is defined as a parameter in a function called `getKNMIDailyData`.

``` r
# Retrieve daily weather data from KNMI
getKNMIDailyData <- function(startYear, startMonth, startDay, endYear, endMonth, endDay) {
  
  # Construct URL
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
  
  # Fetch data
  data <- read.csv(url, header=FALSE, sep=",", comment.char="#", na.strings=" ")
  
  # Format column names
  colnames(data) <- c('STN','YYYYMMDD','DDVEC','FHVEC','FG','FHX','FHXH','FHN','FHNH','FXX','FXXH','TG','TN','TNH','TX','TXH','T10N','T10NH','SQ','SP','Q','DR','RH','RHX','RHXH','EV24','PG','PX','PXH','PN','PNH','VVN','VVNH','VVX','VVXH','NG','UG','UX','UXH','UN','UNH')
  
  return(data)
 
}

# Example
weather_data <- getKNMIDailyData('2015', '01', '01', '2015', '12', '30')
```

    ## [1] "http://projects.knmi.nl/klimatologie/daggegevens/getdata_dag.cgi?stns=ALL&vars=all&byear=2015&bmonth=01&bday=01&eyear=2015&emonth=12&eday=30"

``` r
head(weather_data)
```

    ##   STN YYYYMMDD DDVEC FHVEC FG FHX FHXH FHN FHNH FXX FXXH TG TN TNH TX TXH
    ## 1 391 20150101   205    42 43  70   22  20    1 120   22 25 -9   6 54  14
    ## 2 391 20150102   242    51 55  80    1  30   10 140    4 58 25  24 87  12
    ## 3 391 20150103   228    29 33  50    8  10   16  90    9 29 18  20 38   7
    ## 4 391 20150104   256    26 28  40    1  10   19  70   11 21 -5   8 65  14
    ## 5 391 20150105   214    24 27  60   12  20    5 100   12 23  7   5 38  21
    ## 6 391 20150106   190    21 29  40   16  20    1  80   22 25  0   7 45  24
    ##   T10N T10NH SQ SP   Q DR RH RHX RHXH EV24 PG PX PXH PN PNH VVN VVNH VVX
    ## 1  -29     6 56 71 372  0  0   0    1    4 NA NA  NA NA  NA  NA   NA  NA
    ## 2   12    24 27 34 246 33 32  14    5    3 NA NA  NA NA  NA  NA   NA  NA
    ## 3   11    24  0  0  69 54 29   8   11    1 NA NA  NA NA  NA  NA   NA  NA
    ## 4  -27    24 67 84 408  0  0   0    1    5 NA NA  NA NA  NA  NA   NA  NA
    ## 5    3    12 28 35 221  0  0   0    1    3 NA NA  NA NA  NA  NA   NA  NA
    ## 6   -5    12 36 45 295 35 18  10   22    3 NA NA  NA NA  NA  NA   NA  NA
    ##   VVXH NG UG  UX UXH UN UNH
    ## 1   NA NA 82 100   5 65  24
    ## 2   NA NA 78  97   9 62  14
    ## 3   NA NA 91  99  17 80   2
    ## 4   NA NA 92  99  20 78  12
    ## 5   NA NA 90 100   1 79  24
    ## 6   NA NA 83  98  24 66  14

``` r
nrow(weather_data)
```

    ## [1] 17472

The function to request hourly data is similar, except that we need to add the starting and ending hour. Minimum starting hour is 1 and maximum is 24.

``` r
# Retrieve hourly weather data from KNMI
getKNMIHourlyData <- function(startDate, endDate) {
  
  # Construct URL
  url0 <- 'http://projects.knmi.nl/klimatologie/uurgegevens/getdata_uur.cgi?stns='
  url1 <- '&vars='
  url2 <- '&start='
  url3 <- '&end'
  
  url <- paste(url0, 'ALL', url1, 'all', url2, startDate, url3, endDate, sep="")
  print(url)
  
  # Fetch data
  data <- read.csv(url, header=FALSE, sep=",", comment.char="#", na.strings=" ")
  
  # Format column names
  colnames(data) <- c('STN', 'YYYYMMDD',   'HH',   'DD',   'FH',   'FF',   'FX',    'T',  'T10',   'TD',   'SQ',    'Q',   'DR',   'RH',    'P',   'VV',    'N',    'U',   'WW',   'IX',    'M',    'R',    'S',    'O',    'Y')
  
  return(data)
  
}

# Example
weather_hour_data <- getKNMIHourlyData('2015010101', '2015123024')
```

    ## [1] "http://projects.knmi.nl/klimatologie/uurgegevens/getdata_uur.cgi?stns=ALL&vars=all&start=2015010101&end2015123024"

``` r
head(weather_hour_data)
```

    ##   STN YYYYMMDD HH  DD FH FF FX  T T10 TD SQ Q DR RH  P VV  N   U WW IX  M
    ## 1 391 20150101  1 190 20 20 40  2  NA  1  0 0  0  0 NA NA NA  99 NA  6 NA
    ## 2 391 20150101  2 200 30 30 40 -1  NA -2  0 0  0  0 NA NA NA  99 NA  6 NA
    ## 3 391 20150101  3 190 30 30 50  1  NA  0  0 0  0  0 NA NA NA  99 NA  6 NA
    ## 4 391 20150101  4 220 30 20 50  5  NA  3  0 0  0  0 NA NA NA  99 NA  6 NA
    ## 5 391 20150101  5 210 20 10 30 -5  NA -5  0 0  0  0 NA NA NA 100 NA  6 NA
    ## 6 391 20150101  6 190 20 30 40 -1 -29 -3  0 0  0  0 NA NA NA  98 NA  6 NA
    ##    R  S  O  Y
    ## 1 NA NA NA NA
    ## 2 NA NA NA NA
    ## 3 NA NA NA NA
    ## 4 NA NA NA NA
    ## 5 NA NA NA NA
    ## 6 NA NA NA NA

### Merge with stations data

Each record correspond to a date and a specific station. The list of weather stations with their metadata (latitude, longitude, name, altitude) is available in the header of the file returned by KNMI data service. We manually copied it from an example file and cleaned into a usable CSV. The station code `STN` is the field that will allow us to merge the weather and the station data together.

``` r
nl_stations <- read.csv('../data/all_station_KNMI_cleaned.csv', header=TRUE, sep=",")
nl_stations
```

    ##     STN   LON    LAT ALT.m.             NAME
    ## 1   127 3.314 47.975  999.9          BUCHTEN
    ## 2    32 4.526 51.576    1.2       OUDENBOSCH
    ## 3   206 4.012 54.118   43.4            F16-A
    ## 4   331 4.193 51.480    0.0           THOLEN
    ## 5   200 4.355 52.041  999.9         YPENBURG
    ## 6    18 3.314 47.975  999.9       PHILIPPINE
    ## 7   125 5.054 51.784    1.4            ANDEL
    ## 8    16 3.314 47.975  999.9     HEINKENSZAND
    ## 9   609 4.318 51.891  999.9  R'DAM-GEULHAVEN
    ## 10  272 5.850 52.616    3.6          RAMSPOL
    ## 11  161 5.975 53.391  999.9         TERNAARD
    ## 12  998 5.177 52.101    2.0               DE
    ## 13  316 3.694 51.657    0.0           SCHAAR
    ## 14  313 3.242 51.505    0.0           VLAKTE
    ## 15   20 6.700 51.969   34.1      WINTERSWIJK
    ## 16  163 5.476 52.263    0.2          NIJKERK
    ## 17  551 2.066 56.400  999.9              AUK
    ## 18  148 6.191 51.356   30.0            VENLO
    ## 19  343 4.313 51.893    3.5  R'DAM-GEULHAVEN
    ## 20   35 3.314 47.975  999.9        HAAMSTEDE
    ## 21  208 5.942 53.493   40.5            AWG-1
    ## 22  275 5.873 52.056   48.2           DEELEN
    ## 23  325 3.931 51.654   -4.0        ZIERIKZEE
    ## 24   29 5.044 52.650  999.9            HOORN
    ## 25  350 4.936 51.566   14.9      GILZE-RIJEN
    ## 26  203 3.342 52.361   41.8            P11-B
    ## 27  138 5.676 50.843  999.9       MAASTRICHT
    ## 28  211 2.945 53.825   45.7             J6-A
    ## 29  153 5.356 52.886    2.6         STAVOREN
    ## 30  321 3.275 51.999   19.0     EUROPLATFORM
    ## 31  340 4.342 51.449   19.2      WOENSDRECHT
    ## 32  311 3.672 51.379    0.0       HOOFDPLAAT
    ## 33  247 4.555 52.422   18.0      BLOEMENDAAL
    ## 34  204 3.628 53.271   41.8        K14-FA-1C
    ## 35  165 3.314 47.975  999.9             KAAP
    ## 36  139 5.121 52.458   -0.8           MARKEN
    ## 37  129 5.780 52.712   -3.7        EMMELOORD
    ## 38   17 3.892 51.529    0.7   WILHELMINADORP
    ## 39  320 3.670 51.927   22.0      LICHTEILAND
    ## 40  147 5.611 52.658  999.9              URK
    ## 41  280 6.585 53.125    5.2            EELDE
    ## 42  273 5.888 52.703   -3.3        MARKNESSE
    ## 43  323 3.884 51.527    1.4   WILHELMINADORP
    ## 44  268 5.436 52.531  999.9          HOUTRIB
    ## 45  249 4.979 52.644   -2.4         BERKHOUT
    ## 46  168 5.906 50.764  155.0             EPEN
    ## 47  202 4.362 52.206 9999.9               MP
    ## 48  135 5.338 53.070    3.0   KORNWERDERZAND
    ## 49  348 4.926 51.970   -0.7           CABAUW
    ## 50  319 3.861 51.226    1.7        WESTDORPE
    ## 51  285 6.399 53.575    0.0       HUIBERTGAT
    ## 52  385 5.933 51.550 9999.9               DE
    ## 53  617 6.196 51.498   19.0            ARCEN
    ## 54  212 4.151 52.919   50.9          HOORN-A
    ## 55  344 4.447 51.962   -4.3        ROTTERDAM
    ## 56  126 4.079 51.596    1.0               ST
    ## 57  553 3.276 51.999  999.9     EUROPLATFORM
    ## 58  159 4.057 51.251    2.4      KAPELLEBRUG
    ## 59  251 5.346 53.392    0.7            HOORN
    ## 60  253 2.067 56.400  999.9         AUK-ALFA
    ## 61  279 6.574 52.750   15.8        HOOGEVEEN
    ## 62    8 5.528 52.453   -4.2         LELYSTAD
    ## 63  209 4.518 52.465    0.0           IJMOND
    ## 64   37 5.675 51.976    2.0       WAGENINGEN
    ## 65  270 5.752 53.224    1.2       LEEUWARDEN
    ## 66  391 6.197 51.498   19.5            ARCEN
    ## 67  170 5.715 50.794   50.0             OOST
    ## 68   33 5.677 51.549   13.2           GEMERT
    ## 69  554 3.592 51.681  999.9         MEETPOST
    ## 70  552 3.213 56.547  999.9          EKOFISK
    ## 71  227 4.655 52.774    0.5           PETTEN
    ## 72  324 4.006 51.596    0.0       STAVENISSE
    ## 73  244 5.044 52.650  999.9            HOORN
    ## 74  162 5.222 52.418  999.9 OOSTVAARDERSDIEP
    ## 75  255 1.160 61.234  999.9            NORTH
    ## 76  240 4.790 52.318   -3.3         SCHIPHOL
    ## 77  999 6.575 52.750   15.6        HOOGEVEEN
    ## 78  108 3.314 47.975  999.9   HELLEVOETSLUIS
    ## 79  230 4.759 52.967  999.9              DEN
    ## 80  604 6.262 52.435    1.4        HERWIJNEN
    ## 81  377 5.763 51.198   30.0              ELL
    ## 82  277 6.200 53.413    2.9       LAUWERSOOG
    ## 83  201 2.936 54.327   42.7         D15-FA-1
    ## 84  379 5.700 50.794   42.0    OOST-MAARLAND
    ## 85  550 3.220 53.218  999.9              K13
    ## 86  152 3.558 51.356    1.2      SCHOONDIJKE
    ## 87  225 4.555 52.463    4.4         IJMUIDEN
    ## 88  142 3.314 47.975  999.9             OUDE
    ## 89  207 4.961 53.616   44.0          L9-FF-1
    ## 90  330 4.122 51.992   11.9             HOEK
    ## 91  263 5.177 52.101    2.0               DE
    ## 92  266 5.338 53.070    3.0   KORNWERDERZAND
    ## 93  167 6.850 53.381  999.9           BIERUM
    ## 94  995 3.596 51.442    8.0       VLISSINGEN
    ## 95  214 6.042 54.038   42.5           Gemini
    ## 96  133 5.342 53.398   -1.0            HOORN
    ## 97  290 6.891 52.274   34.8          TWENTHE
    ## 98  615 4.296 52.274   12.0        HOOGEVEEN
    ## 99   39 4.156 51.988  999.9               H.
    ## 100 210 4.430 52.171   -0.2       VALKENBURG
    ## 101  41 5.526 52.458   -4.0         LELYSTAD
    ## 102 258 5.401 52.649    7.3      HOUTRIBDIJK
    ## 103 312 3.622 51.768    0.0    OOSTERSCHELDE
    ## 104 229 4.720 52.996   10.0        TEXELHORS
    ## 105  19 3.314 47.975  999.9           NIEUWE
    ## 106 260 5.180 52.100    1.9               DE
    ## 107 370 5.377 51.451   22.6        EINDHOVEN
    ## 108 315 3.998 51.447    0.0        HANSWEERT
    ## 109 166 6.238 53.583    4.0  SCHIERMONNIKOOG
    ## 110 375 5.707 51.659   22.0           VOLKEL
    ## 111 380 5.762 50.906  114.3       MAASTRICHT
    ## 112 616 4.907 52.367  999.9        AMSTERDAM
    ## 113 300 2.567 51.650  999.9       LICHTSCHIP
    ## 114 128 6.443 52.593    6.0      DEDEMSVAART
    ## 115 252 3.220 53.219   37.7              K13
    ## 116  28 5.820 52.961   -0.4            JOURE
    ## 117 286 7.150 53.196   -0.2            NIEUW
    ## 118  40 5.837 51.856  999.9         NIJMEGEN
    ## 119 283 6.657 52.069   29.1           HUPSEL
    ## 120 310 3.596 51.442    8.0       VLISSINGEN
    ## 121 250 5.147 53.360   31.4     TERSCHELLING
    ## 122 614 6.920 53.337  999.9        EEMSHAVEN
    ## 123 308 3.379 51.381    0.0          CADZAND
    ## 124 215 4.437 52.141   -1.1      VOORSCHOTEN
    ## 125 254 4.296 52.274  999.9         MEETPOST
    ## 126 278 6.259 52.435    3.6            HEINO
    ## 127 271 5.356 52.888    2.6         STAVOREN
    ## 128 605 6.196 53.409    3.0       LAUWERSOOG
    ## 129 130 3.314 47.975  999.9             EPEN
    ## 130 328 3.821 51.668  999.9      ROGGENPLAAT
    ## 131 239 4.696 54.855   50.6          F3-FB-1
    ## 132 122 6.303 52.158    9.0            ALMEN
    ## 133 267 5.384 52.898   -1.3         STAVOREN
    ## 134 143 4.393 51.855    0.2       POORTUGAAL
    ## 135 205 3.810 55.401   48.4          A12-CPP
    ## 136 158 3.314 47.975  999.9            SPIJK
    ## 137 269 5.520 52.458   -3.7         LELYSTAD
    ## 138  13 3.314 47.975  999.9       WISSEKERKE
    ## 139 235 4.781 52.928    1.2               DE
    ## 140 257 4.603 52.506    8.5             WIJK
    ## 141  36 3.314 47.975  999.9       NUMANSDORP
    ## 142 248 5.174 52.634    0.8         WIJDENES
    ## 143  38 3.314 47.975  999.9        NAALDWIJK
    ## 144 356 5.146 51.859    0.7        HERWIJNEN
    ## 145  34 6.887 52.785   24.1            EMMEN
    ## 146 265 5.274 52.130   13.9      SOESTERBERG
    ## 147 169 5.304 53.392    0.4         FORMERUM
    ## 148 164 5.909 51.092   34.1             ECHT
    ## 149 603 6.189 52.311  999.9       DIEPENVEEN
    ## 150 242 4.921 53.241   10.8         VLIELAND

``` r
weather_stn <- merge(weather_data, nl_stations, by='STN')
head(weather_stn)
```

    ##   STN YYYYMMDD DDVEC FHVEC  FG FHX FHXH FHN FHNH FXX FXXH TG TN TNH TX TXH
    ## 1 209 20150101   203   133 133 170   20 100    4 220   20 NA NA  NA NA  NA
    ## 2 209 20150102   263   127 144 180    1 120    7 230    2 NA NA  NA NA  NA
    ## 3 209 20150103   265    74  95 140    8  60   15 180    8 NA NA  NA NA  NA
    ## 4 209 20150104   290    68  77 100    1  50   14 140    9 NA NA  NA NA  NA
    ## 5 209 20150105   208    70  78 100    2  50   24 120    2 NA NA  NA NA  NA
    ## 6 209 20150106   199    41  67 100   19  50    1 120   19 NA NA  NA NA  NA
    ##   T10N T10NH SQ SP  Q DR RH RHX RHXH EV24 PG PX PXH PN PNH VVN VVNH VVX
    ## 1   NA    NA NA NA NA NA NA  NA   NA   NA NA NA  NA NA  NA  NA   NA  NA
    ## 2   NA    NA NA NA NA NA NA  NA   NA   NA NA NA  NA NA  NA  NA   NA  NA
    ## 3   NA    NA NA NA NA NA NA  NA   NA   NA NA NA  NA NA  NA  NA   NA  NA
    ## 4   NA    NA NA NA NA NA NA  NA   NA   NA NA NA  NA NA  NA  NA   NA  NA
    ## 5   NA    NA NA NA NA NA NA  NA   NA   NA NA NA  NA NA  NA  NA   NA  NA
    ## 6   NA    NA NA NA NA NA NA  NA   NA   NA NA NA  NA NA  NA  NA   NA  NA
    ##   VVXH NG UG UX UXH UN UNH   LON    LAT ALT.m.   NAME
    ## 1   NA NA NA NA  NA NA  NA 4.518 52.465      0 IJMOND
    ## 2   NA NA NA NA  NA NA  NA 4.518 52.465      0 IJMOND
    ## 3   NA NA NA NA  NA NA  NA 4.518 52.465      0 IJMOND
    ## 4   NA NA NA NA  NA NA  NA 4.518 52.465      0 IJMOND
    ## 5   NA NA NA NA  NA NA  NA 4.518 52.465      0 IJMOND
    ## 6   NA NA NA NA  NA NA  NA 4.518 52.465      0 IJMOND

### Format weather data

The weather data provided contains all the weather variables available. We can select the type of variables we are interested in, and convert their units: for example the rainfall is given in 0.1mm and we will convert it to 1mm.

``` r
# Subset weather variables and convert their units
formatNLWeatherDailyData <- function(weather) {
  
  # Date
  weather$date <- as.Date(as.character(weather$YYYYMMDD), "%Y%m%d")
  
  # Select weather variables needed
  weather <- weather[c('STN', 'date', 'FG', 'FHN', 'FHX', 'TG', 'TN', 'TX', 'SQ', 'DR', 'RH', 'LON', 'LAT')]
  
  # Convert units (m/s to km/h, 0.1mm to mm, and 0.1C to C)
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
  
  # Rounded data for potential categorization
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

weather_format <- formatNLWeatherDailyData(weather_stn)
head(weather_format)
```

    ##   STN       date    FG  FHN  FHX TG TN TX SQ DR RH   LON    LAT roundedFG
    ## 1 209 2015-01-01 47.88 36.0 61.2 NA NA NA NA NA NA 4.518 52.465        47
    ## 2 209 2015-01-02 51.84 43.2 64.8 NA NA NA NA NA NA 4.518 52.465        51
    ## 3 209 2015-01-03 34.20 21.6 50.4 NA NA NA NA NA NA 4.518 52.465        34
    ## 4 209 2015-01-04 27.72 18.0 36.0 NA NA NA NA NA NA 4.518 52.465        27
    ## 5 209 2015-01-05 28.08 18.0 36.0 NA NA NA NA NA NA 4.518 52.465        28
    ## 6 209 2015-01-06 24.12 18.0 36.0 NA NA NA NA NA NA 4.518 52.465        24
    ##   roundedFHN roundedFHX roundedTG roundedTN roundedTX roundedSQ roundedDR
    ## 1         36         61        NA        NA        NA        NA        NA
    ## 2         43         64        NA        NA        NA        NA        NA
    ## 3         21         50        NA        NA        NA        NA        NA
    ## 4         18         36        NA        NA        NA        NA        NA
    ## 5         18         36        NA        NA        NA        NA        NA
    ## 6         18         36        NA        NA        NA        NA        NA
    ##   roundedRH
    ## 1        NA
    ## 2        NA
    ## 3        NA
    ## 4        NA
    ## 5        NA
    ## 6        NA

### Get closest weather station to a location

When we needed to find the closest weather station to a given location, we used the following function:

``` r
# Get closest station to location
# Location and StationLocation MUST have their longitude column named `LON` and their latitude column named `LAT`
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
```

This function requires the packages `sp` and `rgeos`.
