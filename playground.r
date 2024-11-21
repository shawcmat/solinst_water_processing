#library(wlTools)
library(dplyr)
library(ggplot2)
library(data.table)
library(readxl)
library(lubridate)
library(tools)
library(stringr)
library(fs)


standardize.tower.baro = function(baro_path) {
  tower.csv <- read.csv(baro_path, stringsAsFactors = FALSE)
  results <- tower.csv %>% 
    transmute(time = as.POSIXct(Time, format = "%m/%d/%Y %H:%M", tz = "Etc/GMT+8"), 
              baro_pressure = air_pressure, 
              temperature = air_temp)
  results
}

# standardize solinst (UPDATED 8-2022 SFJ) (Updated 11/20/2024 MS)
standardize.solinst = function (solinst.data, programmed.h2o.density = 1000) {
  if (programmed.h2o.density < 900 | programmed.h2o.density >  1100) 
    stop("H2O density should be between 950 and 1050 kg/m^3")
  clean.solinst <- function(solinist.data) {
    results <- solinst.data %>% 
      filter(LEVEL >= 0, LEVEL <= 20) %>% 
      transmute(time = if (any(grepl("M", Time))) as.POSIXct(paste(Date, Time), format = "%m/%d/%Y %I:%M:%S %p", tz = "Etc/GMT+8")
                       else as.POSIXct(paste(Date, Time), format = "%m/%d/%Y %H:%M:%S", tz = "Etc/GMT+8"), 
                level = if (max(LEVEL) < 9.5) LEVEL + 9.5
                        else LEVEL, 
                temperature = TEMPERATURE, 
                conductivity = if (any(names(solinst.data) == "CONDUCTIVITY")) CONDUCTIVITY
                               else NA)
    rows.excluded <- sum((solinst.data$LEVEL < 0) | (solinst.data$LEVEL > 20), na.rm = TRUE)
    if (rows.excluded > 0) 
      warning(sprintf("%i rows excluded for levels outside of [0, 20]", rows.excluded))
    if (any(names(solinst.data) == "CONDUCTIVITY") && any(solinst.data$CONDUCTIVITY > 100)) 
      warning("Specific conductance values greater than 100, suggesting logger error.")
    if (nrow(as.data.frame(unique(results$time))) < nrow(results))
      warning("Time data not processed correctly. Check .csv Time column for duplicates and formatting.")
    results
  }

  unit.conversion <- function(solinst.data, programmed.h2o.density) {
    solinst.data %>% 
      mutate(total_pressure = level * programmed.h2o.density * 9.80665/1000, 
             salinity = 0.012 + (-0.2174 * ((conductivity)/53.087)^0.5) + (25.3283 * ((conductivity)/53.087)^1) + 
               (13.7714 * ((conductivity)/53.087)^1.5) + (-6.4788 * ((conductivity)/53.087)^2) + (2.5842 * ((conductivity)/53.087)^2.5)) %>% 
      select(-level, -conductivity)
  }

  solinst.data <- clean.solinst(solinst.data)
  unit.conversion(solinst.data, programmed.h2o.density)
}


# total pressure to water level above sensor (UPDATED 8-2022 SFJ)
tp.to.wlas = function (logger.data, baro.data, h2o.density) {

  baro.compensation <- function(logger.data, baro.data) {
    logger.dt <- logger.data %>% filter(!is.na(time), !is.na(total_pressure)) %>% 
      data.table(key = "time")
    baro.dt <- data.table(filter(baro.data, !is.na(time), !is.na(baro_pressure)), key = "time")
    logger.baro.dt <- baro.dt[logger.dt, roll = "nearest"]
    logger.baro.dt %>% 
      mutate(water_pressure = total_pressure - baro_pressure) %>% 
      select(-total_pressure, -baro_pressure)
  }

  level.conversion <- function(logger.data, h2o.density) {
    logger.data %>% 
      mutate(water_level_above_sensor = water_pressure * 1000/(h2o.density * 9.80665)) %>% 
      select(-water_pressure)
  }

  logger.data.columns <- c("time", "total_pressure")
  baro.data.columns <- c("time", "baro_pressure")
  if (!length(intersect(logger.data.columns, colnames(logger.data))) == length(logger.data.columns)) 
    stop("Logger data must have columns: \"time\", \"total_pressure\"")
  if (!length(intersect(baro.data.columns, colnames(baro.data))) == length(baro.data.columns)) 
    stop("Baro data must have columns: \"time\", \"baro_pressure\"")
  if (!any(class(logger.data$time) == "POSIXct")) 
    stop("Logger data time must be POSIXct")
  if (!any(class(baro.data$time) == "POSIXct")) 
    stop("Baro data time must be POSIXct")
  if (h2o.density < 900 || h2o.density > 1100) 
    stop("H2O density should be between 950 and 1050 kg/m^3")
  if (min(logger.data$time) < min(baro.data$time) | max(logger.data$time) > max(baro.data$time)) 
    warning(sprintf("URGENT: Time range for logger data (%s - %s) is not contained within time range for baro data (%s - %s). Data will be incorrectly compensated.", 
                    format(min(logger.data$time), "%Y-%m-%d %H:%M"), 
                    format(max(logger.data$time), "%Y-%m-%d %H:%M"), 
                    format(min(baro.data$time), "%Y-%m-%d %H:%M"), 
                    format(max(baro.data$time), "%Y-%m-%d %H:%M")))
  if (min(na.omit(logger.data$total_pressure)) < 90 | max(na.omit(logger.data$total_pressure > 150)))
    warning("Logger data total pressure not contained in range 90:150. Check units (should be kPa).")
  if (min(na.omit(baro.data$baro_pressure)) < 90 | max(na.omit(baro.data$baro_pressure > 150)))
    warning("Baro pressure not contained in range 90:150. Check units (should be kPa).")
  logger.data %>% baro.compensation(baro.data) %>% level.conversion(h2o.density)
}







droot <- ("C:/Users/mshawcroft/water_processing data test/")

#Function parameters (Entered in Application.)
baro_path <- file.path(droot, "raw data", "baro", "Combined_2021.09.02_2023.05.01.csv")
baro_type <- "tower"
level_path <- file.path(droot, "raw data", "BigBreak_2022.02.23_2022.09.28.csv")
level_type <- "solonist"
level_processed_path <- file.path(droot, "processed data")
level_QAQC_path <- file.path(droot, "QAQC data", "BigBreak_2022.02.23_2022.09.28_QAQC_Final.csv")
water_accessory_path <- file.path(droot, "water_accessory.csv")


#Derived parameters

name_elements <- strsplit(tools::file_path_sans_ext(basename(level_path)), split = "_")[[1]]
logger_name   <- name_elements[1] 

level_start <- name_elements[2] %>% str_replace_all("\\.", "_")
level_end   <- name_elements[3] %>% str_replace_all("\\.", "_")
year_end    <-  paste0(as.character(lubridate::year(as.Date(level_end, format = "%Y_%m_%d")) + 1), "_01_01")

level_processed_file_name <- paste0(paste(logger_name, name_elements[2], name_elements[3], sep = "_"), ".csv")

level_QAQC <- read.csv(level_QAQC_path)
level_QAQC$time <- ymd_hms(level_QAQC$time, tz = Sys.timezone())
pre_year_start <- paste0(year(min(level_QAQC$time, na.rm = T)) - 1, "-12-31")
exact_start_date <- format(min(level_QAQC$time, na.rm = T), "%Y-%m-%d")

dirname(level_QAQC_path)
# Convert microsiemens to millisiemens. ------------------------------------------------------

inputheader = read.csv(level_path)

removeheader <- function(level_path) {
    headers <- readLines(level_path, n = 25)
    header.count = suppressWarnings(min(grep("^Date,Time,.*", headers)) - 1)
    real_data <- read.csv(level_path, stringsAsFactors = FALSE, skip = header.count)

    if(header.count > 1){
        header_data <- readLines(level_path, n = header.count)
        readr::guess_encoding(level_path)
        header_data <- iconv(header_data, from = "ISO-8859-1", to = "UTF-8")
        offset <- header_data[grep(pattern = "^Offset", header_data)]
        header_data_clean <- header_data[-grep(pattern = "^Offset", header_data)]
        num_pairs <- length(header_data_clean) %/% 2
        header_data_clean <- paste0(header_data_clean[seq(1, length(header_data_clean), by = 2)], header_data_clean[seq(2, length(header_data_clean), by = 2)])
        header_data_clean <- str_split(header_data_clean, pattern = ":")

        # Convert to named list
        names_vector  <- sapply(header_data_clean, `[[`, 1)
        values_vector <- sapply(header_data_clean, `[[`, 2)
    
        header_data_clean <- data.frame(as.list(values_vector))
        names(header_data_clean) <- names_vector

        return(list("header" = header_data_clean, "data" = real_data))
    }else{
        return(list("header" = "", "data" = real_data))
    }

}

out <- removeheader(level_path)

outputdata <- out$data
headerdata <- out$header

if(headerdata$CONDUCTIVITYUNIT == " µS/cm"){
    outputdata$CONDUCTIVITY = outputdata$CONDUCTIVITY/1000
}


# Actual water level processing -------------------------------------------------------

# read in and process baro data
baro.fxn = function(baro_type){
    if(baro_type == "logger"){
      standardize.baro(baro_path) # TODO: Currently not a function explicitly written in the script.
    }else{
      standardize.tower.baro(baro_path)
    }
}

baro = baro.fxn(baro_type)

# Read in and process water level data
level.fxn = function(level_type){
  if(level_type == "hobo"){
    standardize.hobo(outputdata) # TODO: Currently not a function explicitly written in the script.
  }else{
    standardize.solinst(outputdata)
  } 
} 

level = level.fxn(level_type)

# Pull in water accessory information look-up table
water_accessory = read.csv(water_accessory_path)

water_accessory$alt_name <- str_replace_all(water_accessory$logger, pattern = " ", replacement = "")

# Convert total pressure to water level above sensor using local water density & barometric pressure
level_barocomp = tp.to.wlas(level, baro, water_accessory$water_density[water_accessory$alt_name==logger_name]) # TODO: Might be an issue with pressure range.

# Add final water level NAVD88 column by adding sensor elevation in NAVD88 to water level above sensor
level_barocomp$water_level_NAVD88 = level_barocomp$water_level_above_sensor + 
  water_accessory$sensor_navd88[water_accessory$alt_name==logger_name]

# Re-name temperature columns to be more clear
final_level = level_barocomp %>%
  rename(water_temp = i.temperature, air_temp = temperature)

# Export processed dataset
write.csv(final_level, file.path(level_processed_path, level_processed_file_name))

# Join newly processed data with the long term site dataset.-------------------------------------------------------------

# Combine newly processed data with historic QAQCed data
level_processed = read.csv(file.path(level_processed_path, level_processed_file_name))
level_longterm  = read.csv(level_QAQC_path)
combined_level  = bind_rows(level_longterm, level_processed)

#View(combined_level)

#Get new start and end dates for the full dataset.
combined_level$time <- ymd_hms(combined_level$time, tz = Sys.timezone())
new_start_date <- format(min(combined_level$time, na.rm = T), "%Y-%m-%d") %>% str_replace_all("-", ".")
new_end_date   <- format(max(combined_level$time, na.rm = T), "%Y-%m-%d") %>% str_replace_all("-", ".")


dirname(level_QAQC_path)

level_working_file_name <- paste0(paste(logger_name, new_start_date, new_end_date, sep = "_"), "_working.csv")
level_working_path <- file.path(dirname(level_QAQC_path), level_working_file_name) # Output file for new working water level file ready for QAQC. Combining new processed data with longterm QAQC data.

# Export updated long-term dataset
write.csv(combined_level, level_working_path)

# Move old long-term file into the history folder.
fs::file_move(level_QAQC_path, file.path(dirname(level_QAQC_path), "history",))
