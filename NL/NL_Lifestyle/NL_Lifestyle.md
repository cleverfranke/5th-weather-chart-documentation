C°F 5th Weather Chart - Lifestyle (NL, Friesland, Groningen, Drenthe) Analysis
===============================================================================

About
-----

Lifelines is a multi-disciplinary prospective population-based cohort study examining in a unique three-generation design the health and health-related behaviors of more than 167,000 persons living in the North of The Netherlands. [1] It is a biobank database that allowed us to investigate the potential effect of weather on people's health and habits. You can read more about the organisation on their website <https://www.lifelines.nl/>.

Because the data tracks individuals, it is also strictly private. Therefore we will only expose the methodology we used to analyze it.

Methodology
-----------

Lifelines data consists mainly in questionnaire and health metrics that are filled in by each participant at several points in time.

### Assumptions

The assumption is that the sample of records per weather value is representative of the whole population. For example, we assume that the group of people who made a visit to their physician on the days where the temperature was 30 degrees is as representative as the group of people who made their visit on the days where the temperature was 15 degrees, which is by extent also assumed to be representative of the whole population.

### Step by step

1.  For each record, get the date of the physician visit and the postcode of the participant
2.  Merge with the weather data for the given postcode (closest weather station) and the given date of the visit
3.  Aggregate all the data by average against a weather code, in order to get, for a given weather value, the average variable value. For example, when it's 30 degrees, the average weight of people is this many kilos.
4.  In the aggregated data, select only the values that are at least an aggregation of 10 participant data point.

### Qualitative response

Sometimes the response of the participant to a questionnaire was qualitative.

-   When the response of a question was qualitative, they were converted to a numeric scale. For example, if the question was 'how often did you eat ice-cream in the past 4 weeks?', the type of responses were '1-2 days' or '2 days per week'. In this case, the first one would be converted to `1/28` and the second one to `2*4/28`.
-   When the question was related to the past month, we considered that a month was also made of 28 days.

### Weather date range

When the data was related to a measurement, we used the weather data from the day of the participant visit. For example, the blood pressure data will use the temperature value of the day when the blood pressure was taken.

When the data was related to a time period previous to the visit, we averaged the weather data over that time period. For example, the ice-cream consumption over the past 4 weeks has been related to the average temperature over the past 4 weeks.

### Age group and sex

For each variable that we analysed, we also did an analysis by age group (0-20, 20-40, 40-60, 60-80), by sex (male and female), and by both of them.

[1] See <https://www.lifelines.nl/researcher>
