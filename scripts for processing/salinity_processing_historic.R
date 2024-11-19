

# BASIC ODYSSEY PROCESSING ----------------------------------------------------

# If you want, you can set your working directory to the logger folder.
# This lets you use shorter path names.

# for this example, we're using Petaluma
# note the slash direction here. You only want to use this kind. '/'

library(dplyr)
library(wlTools)

setwd('V:/Davis/Thorne/Data Files/2 Climate Change/3 Projects/8 Water Quality/1 Raw data/Petaluma')

clean.odyssey = function (odyssey.file) 
{
  odyssey.header <- readLines(odyssey.file, n = 15)
  skipn = grep("RAW VALUE | Raw Value", odyssey.header) + 
    1
  logger.id <- grep("Logger S", odyssey.header, value = TRUE)
  sn <- stringr::str_extract(logger.id, "\\d{4}")
  colnames <- c("scan", "date", "time", "temperature.raw", 
                "temperature.calibrated", "spcond.raw", "spcond.calibrated")
  odyssey.csv <- read.csv(odyssey.file, col.names = colnames, 
                          header = FALSE, strip.white = TRUE, skip = skipn)
  results <- odyssey.csv %>% mutate(date.time = as.POSIXct(paste(date, 
                                                                 time), format = "%d/%m/%Y %H:%M",
                                                           tz = "Etc/GMT+8"), date = as.POSIXct(date, 
                                                                                                                      format = "%d/%m/%Y"), time = as.POSIXct(time, 
                                                                                                                                                              format = "%H:%M"), month = strftime(date, format = "%Y-%m"), 
                                    serial.number = sn, salinity = CalcSalinity(spcond.calibrated)) %>% 
    select(c(serial.number, month, date, time, date.time, 
             temperature.calibrated, spcond.calibrated, salinity))
  results
}


# load the files that you would like to look at
ody1 <- clean.odyssey(
  odyssey.file = '13 PetalumaOdy_1262_072817_100517.csv')
ody2 <- clean.odyssey(
  odyssey.file = '14 PetalumaODY_1262_100517_010818.csv')
ody3 <- clean.odyssey(
  odyssey.file = '15 PetalumaOdy_1262_010818_040418.csv')
ody4 <- clean.odyssey(
  odyssey.file = '16 PetalumaOdy_1262_040418_052318.csv')
ody5 <- clean.odyssey(
  odyssey.file = '18 PetalumaOdy_2162_091318_121718.csv')
ody6 <- clean.odyssey(
  odyssey.file = '19 PetalumaOdy_2162_121718_041019.csv')

# Bind your water dataframes together using the time/date column 
my_ody <- rbind(ody1, ody2, ody3, ody4, ody5, ody6)

# now take a look at the cleaned up file. It has nicely formatted dates and
# the salinity has been calculated

head(my_ody)
str(my_ody)

filter_ody = function (ody_csv, waterlevel_csv, height.offset, output.dir, 
          ody_df, waterlevel_df, return.data = FALSE) 
{
  if (missing(height.offset)) 
    stop("Odyssey height offset required to filter water levels!")
  if (!missing(ody_csv) && !missing(ody_df)) {
    warning("Odyssey data must be either csv or data frame, not both", 
            call. = FALSE, immediate. = TRUE)
  }
  if (!missing(waterlevel_csv) && !missing(waterlevel_df)) {
    warning("Water Level Above Sensor data must be either csv or data frame, not both", 
            call. = FALSE, immediate. = TRUE)
  }
  if (!missing(ody_csv)) {
    ody.data <- read.csv(ody_csv) %>% mutate(date.time = as.POSIXct(date.time, 
                                                                    tz = "Etc/GMT+8"))
  }
  else ody.data <- ody_df
  if (!("salinity" %in% names(ody.data)) && !("salinity.calc" %in% 
                                              names(ody.data))) {
    stop("Odyssey data must contain the field \"salinity or salinity.calc\"")
  }
  if ("calc.salinity" %in% names(ody.data)) {
    ody.data <- rename(ody.data, salinity = salinity.calc)
  }
  if (!missing(waterlevel_csv)) {
    wll.data <- read.csv(waterlevel_csv) %>% mutate(time = as.POSIXct(time, 
                                                                      tz = "Etc/GMT+8"))
  }
  else wll.data <- waterlevel_df
  if (!("water.level.above.sensor" %in% names(wll.data))) {
    stop("Water Level data must contain the field \"water.level.above.sensor\"")
  }
  if (min(wll.data$time) > min(ody.data$date.time) | max(wll.data$time) < max(ody.data$date.time)) 
  {
    warning(sprintf(paste0("URGENT: Time range for odyssey data (%s - %s)", 
                           "is not fully contained within time range for water level data (%s - %s).", 
                           " Data will be trimmed. Use leve data from another time period to recover", 
                           " trimmed section."), format(min(ody.data$date.time), 
                                                        "%Y-%m-%d %H:%M"), format(max(ody.data$date.time), 
                                                                                  "%Y-%m-%d %H:%M"), format(min(wll.data$time), "%Y-%m-%d %H:%M"), 
                    format(max(wll.data$time), "%Y-%m-%d %H:%M"), collapse = ""), 
            call. = FALSE, immediate. = TRUE)
  }
  if (height.offset < 0.05) {
    warning("Minimum threshold of 5cm will be used to filter water levels.", 
            call. = FALSE, immediate. = TRUE)
  }
  filtered.water <- wll.data %>% filter(water.level.above.sensor > 
                                          max(c(0.05, height.offset))) %>% transmute(date.time = time, 
                                                                                     water.level.above.wll = water.level.above.sensor)
  filtered.ody <- inner_join(ody.data, filtered.water, by = "date.time")
  print(sprintf("Filtering complete. %s observations removed from odyssey data", 
                nrow(ody.data) - nrow(filtered.ody)))
  if (!missing(output.dir)) {
    if (!missing(ody_csv)) {
      ody.title <- basename(tools::file_path_sans_ext(ody_csv))
    }
    else {
      ody.title <- strftime(Sys.time(), format = "%Y-%m-%d-%H%M%S")
    }
    write.csv(filtered.ody, file.path(output.dir, sprintf("%s_%sm_filtered.csv", 
                                                          ody.title, max(c(0.05, height.offset)))))
  }
  if (return.data) 
    return(filtered.ody)
}

# filter odyssey file
filtered.ody <- filter_ody(ody_df = ody_alt, # we can't have both! If you enter a dataframe, it will name the file automatically with a timestamp.
                           waterlevel_csv = 'Pet01_2018.12.17_2019.04.10_processed.csv', #use processed water data, must have water level above sensor
                           height.offset = 0.307, # how far in the vertical the two sensors are from each other
                           return.data = TRUE) # return data frame
            
write.csv(filtered.ody, "petaluma_ody_072817_041019.csv")

# quick look at monthly stats
filtered.ody %>%
  group_by(month) %>%
  summarise(mean_sal = mean(salinity, na.rm = TRUE),
            max_sal = max(salinity, na.rm = TRUE))
