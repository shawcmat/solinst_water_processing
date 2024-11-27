# Overview

The objective of this repository is to provide a standardized and simplified process to convert measurements of raw pressure and conductivity collected by water loggers into measurements of NAVD88 water height and water salinity. This is done through a simple graphical user interface created using R-shiny. Required inputs are an unprocessed data file collected by a solonist water logger in a .csv file format, and a table of metadata associated with each logger deployment (also in a .csv file format), and measurements of barometric data (air pressure) covering the time range associated with each deployment.

The solonist water loggers record total pressure (atmospheric and water) as meters of water-equivalent pressure. These values are recorded with the column heading *level*. So, a *level* value of 10m is the amount of pressure given 10 meters of water, at the preprogrammed water density. The actual processing code convert these *level* values into NAVD88 water height with the following transformations.

1.   From meters of water-equivalent pressure we calculate total pressure in Kilopascals (Kpa). We do this using a standard pressure formula *P = pgh*, where *P* is pressure in pascals, *p* is the given density of the water, g is acceleration due to gravity, and h is the height of the water column in meters.
2.  From total pressure (Kpa) we calculate water pressure (Kpa) by subtracting barometric pressure (measured in a separate barometric dataset) for each observation.
3.  From water pressure (Kpa) we calculate the water level above the sensor, again using a standard pressure formula P = pgh.
4.  From height above the sensor, we calculate NAVD88 water height by adding the sensor height (recorded by RTK at time of retrieval and included in the deployment metadata) to the height above the sensor.

Salinity is calculated from conductivity using this formula:

-    *salinity =* 0.012 + (-0.2174 \* (*conductivity* / 53.087)\^1.5)  + (25.3283 \* (*conductivity*/ 53.087) \^1) + (13.7714 \* (*conductivity / 53.087)\^1.5) + (-6.4788 \* (conductivity/53.087)\^2) + (2.5842 \* (conductivity/53.087)\^2.5)*

# Water processing logic, step-by-step.

## A. Water logger data is loaded and prepared.

1.  Data is loaded as raw text using readLines to capture the header information. Size of the header is derived by searching for the column names, and the real data is loaded using read.csv, skipping the header.
2.  Header data is extracted and converted into an easily usable dataframe for reference. Convert encoding to UTF-8 to correctly preserve units information. Using the pattern of the header, we extract the names and the values, and use these to build a header data frame containing all header information. If there is no header, this step is skipped.
3.  Check the conductivity unit listed in the header data. If the unit is microsiemens, we convert conductivity into millisiemens by dividing by 1000. (1 mS = 1 uS  \* .001)

## B. Barometeric data is loaded and processed.

1.  Barometric data can be collected using a baro-logger or from a nearby tower sensor (airport, etc.). Baro type is entered by the user as either "logger" or "tower". The expected unit is in KiloPascals (Kpa).
2.  Tower data is loaded as a dataframe.
3.  *time* column is converted into a POSIXct string in the format month/day/year hour:minute.
4.  *air_pressure* column is renamed as *baro_pressure.*
5.  *air_temp* column is renamed as temperature.

## C. Clean and standardize water data.

1.  Data is **filtered** by the *LEVEL* column to exclude rows with levels less than 0m and greater than 20m.
2.  The *time* column is converted into a POSIXct string in the format month/day/year hour:minute:second.
3.  Some older solonist water loggers (the Levelogger Gold series) used a set zero-point at 9.5m. This results in the data measured by these instruments being offset by 9.5m (See [here](https://www.solinst.com/products/dataloggers-and-telemetry/3001-levelogger-series/operating-instructions/user-guide/1-introduction/1-2-1-level.php)). The script checks for this by checking the maximum LEVEL value. If the maximum *LEVEL* value is less than 9.5m, then 9.5m is **added** to the measurements to standardize these older sensors. There are not currently any Gold series sensors used in the Davis lab, but this code was kept just to maintain compatibility with older data. There is a theoretically possible case where the data measured by a standard sensor has a maximum value below 9.5, resulting in inflated pressure values, but this should be rare.
4.  *LEVEL* column is renamed to *level.*
5.  *TEMPERATURE* is renamed to *temperature.*
6.  All of our loggers should measure conductivity, but just in case they don't, we check here. If the *CONDUCTIVITY* column exists, we rename it to *conductivity.* if it does not exists, we create a *conductivity* column and set all values to NA.
7.  Check for errors and throw warnings.
    -   Count the number of rows excluded in step C1. If this value is more than zero, throw a warning that rows were excluded for levels outside of 0, 20, include the excluded rows, and continue processing.

    -   Check *conductivity* values. If any *conductivity* values are greater than 100, throw a warning that values greater than 100 suggest a logger error, and continue processing.
8.  Check for any duplicate timestamps in the *time* column. If any are detected, throw a warning that the Time data might not be correct, and the .csv should be checked for duplicates and formatting, then continue processing.

## D. Calculate total pressure and salinity.

1.  The solonist water loggers record total pressure (atmospheric and water) as meters of water-equivalent pressure. So, a *level* value of 10m is the amount of pressure given 10 meters of water, at the preprogrammed water density (should be 1000). We need to standardize these values to a more usable unit of measurement. We do this using a standard pressure formula *P = pgh*, where *P* is pressure in pascals, *p* is the given density of the water, g is acceleration due to gravity, and h is the height of the water column in meters. We also convert the pascals into kilopascals by dividing the result by 1000. The resulting formula looks like this:
    -   *total_pressure (in Kpa)* = *level*  \* programmed.h2o.density \* 9.80665/1000
2.   We also convert the *conductivity* measurements into and *salinity* using the following formula.
    -   *salinity =* 0.012 + (-0.2174 \* (*conductivity* / 53.087)\^1.5)  + (25.3283 \* (*conductivity*/ 53.087) \^1) + (13.7714 \* (*conductivity / 53.087)\^1.5) + (-6.4788 \* (conductivity/53.087)\^2) + (2.5842 \* (conductivity/53.087)\^2.5)*

## E. Calculate water level above sensor using barometry data.

1.  water_accessory.csv table is opened as a dataframe. A new column named *alt_name* is created as a standardized reference field by removing all spaces from the *logger* column. (When new metadata table is created this won't be neccessary).
2.  Check for issues in the water logger data and throw warnings if detected.
    -   Check if the water logger data has the correct columns. If not, stop with a warning that logger data requires *time* and *total_pressure* columns.

    -   Check that barometric data has the correct columns. If not, stop with a warning that the baro data requires *time* and *baro_pressure* columns.

    -   Check class the of logger and baro *time* columns. They should both be POSIXct. If not, stop with a warning.

    -   Check to see if the h2o.density retrieved from the water.accessory table is between 900 and 1100. If not, stop with a warning.

    -   Check to see if the time range of the water logger data is contained within the time range of the baro data. If it is not, process will quit. New baro data will need to be retrieved.

    -   Check to see if the range of the *total_pressure* column in the water logger data falls between 90 and 150. If not contained in this range, it could be a sign that the units of pressure are incorrect. (Should be kPa).

    -   Check to see if the range of the *baro_pressure* column in the barometric data falls between 90 and 150. If not contained in this range, it could be a sign that the units of pressure are incorrect. (Should be kPa).
3.   *water_pressure* is calculated from *total_pressure* using the following steps:
    -   Water level data is filtered to exclude rows where time or total pressure is NA.

    -   Barometric data is filtered to exclude rows where time or barometric pressure is NA.

    -   Using a “nearest” rolling join, join water level data and barometric data. Each observation of water level data is joined to the nearest available observation of barometric data.

    -   *water_pressure* is calculated by subtracting *baro_pressure* from *total_pressure.*
4.  *water_level_above_sensor* is calculated using the following formula: h = P/pg, where h is the height of the water column, P is the pressure in pascals, p is the density of the water, and g is acceleration due to gravity. In our code we also convert from Kpa to pa in the same formula, so the result looks like this:
    -   *water_level_above_sensor* = *water_pressure* \* 1000 / (h2o.density \* 9.80665)

## F. Calculate NAVD88 water height.

1.  Using the newly calculated *water_level_above_sensor* column and the RTK measurement found in the metadata table (*sensor_navd88)*, we can calculate the NAVD88 height of the water at every observation. This is calculated using the following formula.
    -   *Water_level_NAVD88 = water_level_above_sensor + sensor_navd88*

## G. Final steps.

1.  *i.temperature* and *temperature* are renamed as *water_temp* and *air_temp* respectively.
2.  Completed data is saved in the *processed* folder, and also appended to the long term data file for this site.