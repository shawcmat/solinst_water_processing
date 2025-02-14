#library(wlTools)
library(dplyr)
library(ggplot2)
library(data.table)
library(readxl)
library(lubridate)
library(tools)
library(stringr)
library(fs)


data_root = "C:/Users/mshawcroft/water_processing data test" 
baro_path = "C:/Users/mshawcroft/water_processing data test/raw data/baro/Combined_2021.09.02_2023.05.01.csv"
baro_type = "tower"
level_path = "C:/Users/mshawcroft/water_processing data test/raw data/BigBreak_2022.02.23_2022.09.28.csv"
level_type = "solinst"
level_QAQC_path = "C:/Users/mshawcroft/water_processing data test/QAQC data/BigBreak_2022.02.23_2022.09.29_working.csv"
water_accessory_path = "C:/Users/mshawcroft/water_processing data test/water_accessory.csv"
trim_days = TRUE

test_inputs <- function(data_root,
                         baro_path,
                          baro_type,
                           level_path, 
                            level_type, 
                             level_QAQC_path,
                              water_accessory_path){  
return(paste(data_root,
              baro_path,
               baro_type,
                level_path,
                 level_type,
                  level_QAQC_path,
                   water_accessory_path))    
}


check_baro_dates <- function(baro_path){
  dat <- standardize.tower.baro(baro_path)
  min_time <- min(dat$time)
  max_time <- max(dat$time)
  min_time_str <- format(min_time, "%m/%d/%Y")
  max_time_str <- format(max_time, "%m/%d/%Y")
  return(paste("First date:", min_time_str, "Last date:", max_time_str))
}

check_level_dates <- function(solinst_path){
  dat <- removeheader(solinst_path)
  dat <- dat$data
  dat <- transmute(dat,
   time = if(any(grepl("M", Time))){as.POSIXct(paste(Date, Time), format = "%m/%d/%Y %I:%M:%S %p", tz = "Etc/GMT+8")}
   else{as.POSIXct(paste(Date, Time), format = "%m/%d/%Y %H:%M:%S", tz = "Etc/GMT+8")})
  min_time <- min(dat$time)
  max_time <- max(dat$time)
  min_time_str <- format(min_time, "%m/%d/%Y")
  max_time_str <- format(max_time, "%m/%d/%Y")
  return(paste("First date:", min_time_str, "Last date:", max_time_str))
}

check_longterm_dates <- function(longterm_QAQC_path){
  dat <- read.csv(longterm_QAQC_path)
  min_time <- min(dat$time)
  max_time <- max(dat$time)
  min_time_str <- format(as.Date(min_time), "%m/%d/%Y")
  max_time_str <- format(as.Date(max_time), "%m/%d/%Y")
  return(paste("First date:", min_time_str, "Last date:", max_time_str))
}

standardize.tower.baro = function(baro_path) {
  tower.csv <- read.csv(baro_path, stringsAsFactors = FALSE)
  results <- tower.csv %>% 
    transmute(time = as.POSIXct(Time, format = "%m/%d/%Y %H:%M", tz = "Etc/GMT+8"), 
              baro_pressure = air_pressure, 
              temperature = air_temp)
  results
}

# standardize solinst (UPDATED 8-2022 SFJ) (Updated 11/20/2024 MS)
standardize.solinst = function (solinst.data, programmed.h2o.density = 1000, log_file_path) {
  if (programmed.h2o.density < 900 | programmed.h2o.density >  1100) {
    msg <- "Processing error in standardize.solinst: H2O density should be between 950 and 1050 kg/m^3"
    log_file <- file(log_file_path, open = "at")
    writeLines(msg, log_file)
    close(log_file)
    stop(msg)
  }
  
  clean.solinst <- function(solinst.data) {
    results <- solinst.data %>% 
      filter(LEVEL >= 0, LEVEL <= 20) %>% 
      transmute(time = if(any(grepl("M", Time))){as.POSIXct(paste(Date, Time), format = "%m/%d/%Y %I:%M:%S %p", tz = "Etc/GMT+8")
                        }else{as.POSIXct(paste(Date, Time), format = "%m/%d/%Y %H:%M:%S", tz = "Etc/GMT+8")}, 
                level = if(max(LEVEL) < 9.5){
                        LEVEL + 9.5
                          }else{LEVEL}, 
                temperature = TEMPERATURE, 
                conductivity = if (any(names(solinst.data) == "CONDUCTIVITY")) CONDUCTIVITY
                               else NA)
    rows.excluded <- sum((solinst.data$LEVEL < 0) | (solinst.data$LEVEL > 20), na.rm = TRUE)
    if (rows.excluded > 0) {
      msg <- sprintf("Warning in clean.solinst: %i rows excluded for levels outside of [0, 20]", rows.excluded)
      warning(msg)
      log_file <- file(log_file_path, open = "at")
      writeLines(msg, log_file)
      close(log_file)
    }
    if (any(names(solinst.data) == "CONDUCTIVITY") && any(solinst.data$CONDUCTIVITY > 100)) {
      msg <- "Warning in clean.solinst: Specific conductance values greater than 100, suggesting logger error."
      warning(msg)
      log_file <- file(log_file_path, open = "at")
      writeLines(msg, log_file)
      close(log_file)
    }
    if (nrow(as.data.frame(unique(results$time))) < nrow(results)) {
      msg <- "Warning in clean.solinst: Time data not processed correctly. Check .csv Time column for duplicates and formatting."
      warning(msg)
      log_file <- file(log_file_path, open = "at")
      writeLines(msg, log_file)
      close(log_file)
    }
    results
  }

  unit.conversion <- function(solinst.data, programmed.h2o.density) {
    solinst.data %>% 
      mutate(total_pressure = level * programmed.h2o.density * 9.80665/1000, #
             salinity = 0.012 + (-0.2174 * ((conductivity)/53.087)^0.5) + (25.3283 * ((conductivity)/53.087)^1) + 
               (13.7714 * ((conductivity)/53.087)^1.5) + (-6.4788 * ((conductivity)/53.087)^2) + (2.5842 * ((conductivity)/53.087)^2.5)) %>% 
      select(-level, -conductivity)
  }

  solinst.data <- clean.solinst(solinst.data)
  unit.conversion(solinst.data, programmed.h2o.density)
}


# total pressure to water level above sensor (UPDATED 8-2022 SFJ)
tp.to.wlas = function (logger.data, baro.data, h2o.density, log_file_path) {

  baro.compensation <- function(logger.data, baro.data) {
    logger.dt <- logger.data %>% filter(!is.na(time), !is.na(total_pressure)) %>% # Water level data is filtered to exclude rows where time or total pressure is NA.
      data.table(key = "time")
    baro.dt <- data.table(filter(baro.data, !is.na(time), !is.na(baro_pressure)), key = "time") #Barometric data is filtered to exclude rows where time or barometric pressure is NA.
    logger.baro.dt <- baro.dt[logger.dt, roll = "nearest"] # Join water level data and barometric data. Joined to the nearest available observation of barometric data.
    logger.baro.dt %>% 
      mutate(water_pressure = total_pressure - baro_pressure) %>% # water_pressure  is calculated by subtracting baro_pressure from total_pressure.
      select(-total_pressure, -baro_pressure)
  }

  level.conversion <- function(logger.data, h2o.density) {
    logger.data %>% 
      mutate(water_level_above_sensor = water_pressure * 1000/(h2o.density * 9.80665)) %>% # Convert water pressure and h2o.density to water_level_above_sensor.
      select(-water_pressure)
  }

  logger.data.columns <- c("time", "total_pressure")
  baro.data.columns <- c("time", "baro_pressure")
  if (!length(intersect(logger.data.columns, colnames(logger.data))) == length(logger.data.columns)) {
    msg <- "Processing error in tp.to.wlas: Logger data must have columns: \"time\", \"total_pressure\""
    log_file <- file(log_file_path, open = "at")
    writeLines(msg, log_file)
    close(log_file)
    stop(msg)
  }
  if (!length(intersect(baro.data.columns, colnames(baro.data))) == length(baro.data.columns)) {
    msg <- "Processing error in tp.to.wlas: Baro data must have columns: \"time\", \"baro_pressure\""
    log_file <- file(log_file_path, open = "at")
    writeLines(msg, log_file)
    close(log_file)
    stop(msg)
  }
  if (!any(class(logger.data$time) == "POSIXct")) {
    msg <- "Processing error in tp.to.wlas: Logger data time must be POSIXct"
    log_file <- file(log_file_path, open = "at")
    writeLines(msg, log_file)
    close(log_file)
    stop(msg)
  }
  if (!any(class(baro.data$time) == "POSIXct")) {
    msg <- "Processing error in tp.to.wlas: Baro data time must be POSIXct"
    log_file <- file(log_file_path, open = "at")
    writeLines(msg, log_file)
    close(log_file)
    stop(msg)
  }
  if (h2o.density < 900 || h2o.density > 1100) {
    msg <- "Processing error in tp.to.wlas: H2O density should be between 950 and 1050 kg/m^3"
    log_file <- file(log_file_path, open = "at")
    writeLines(msg, log_file)
    close(log_file)
    stop(msg)
  }
  if (min(logger.data$time) < min(baro.data$time) | max(logger.data$time) > max(baro.data$time)) {
    msg <- sprintf("Warning in tp.to.wlas: Time range for logger data (%s - %s) is not contained within time range for baro data (%s - %s). Data will be incorrectly compensated.",
                   format(min(logger.data$time), "%Y-%m-%d %H:%M"),
                   format(max(logger.data$time), "%Y-%m-%d %H:%M"),
                   format(min(baro.data$time), "%Y-%m-%d %H:%M"),
                   format(max(baro.data$time), "%Y-%m-%d %H:%M"))
    log_file <- file(log_file_path, open = "at")
    writeLines(msg, log_file)
    close(log_file)
    warning(msg)
  }
  if (min(na.omit(logger.data$total_pressure)) < 90 | max(na.omit(logger.data$total_pressure > 150))) {
    msg <- "Warning in tp.to.wlas: Logger data total pressure not contained in range 90:150. Check units (should be kPa)."
    log_file <- file(log_file_path, open = "at")
    writeLines(msg, log_file)
    close(log_file)
    warning(msg)
  }
  if (min(na.omit(baro.data$baro_pressure)) < 90 | max(na.omit(baro.data$baro_pressure > 150))) {
    msg <- "Warning in tp.to.wlas: Baro pressure not contained in range 90:150. Check units (should be kPa)."
    log_file <- file(log_file_path, open = "at")
    writeLines(msg, log_file)
    close(log_file)
    warning(msg)
  }
  logger.data %>% baro.compensation(baro.data) %>% level.conversion(h2o.density)
}

# Splits header and data from a solinst data file.
removeheader <- function(level_path) {
        headers <- readLines(level_path, n = 25) #Load lines of text, capturing header.
        header.count = suppressWarnings(min(grep("^Date,Time,.*", headers)) - 1) # Derive the size of the header but searching for the column names.
        real_data <- read.csv(level_path, stringsAsFactors = FALSE, skip = header.count) # Load actual data, skipping header.

        if(header.count > 1){
            header_data <- readLines(level_path, n = header.count) #load just the header
            header_data <- iconv(header_data, from = "ISO-8859-1", to = "UTF-8") #Convert encoding to UTF-9 to preserve units information.
            offset <- header_data[grep(pattern = "^Offset", header_data)] #Extract offset, as format is different than other sections.
            header_data_clean <- header_data[-grep(pattern = "^Offset", header_data)] # Remove offset section from the main header data.
            num_pairs <- length(header_data_clean) %/% 2 # used in the following code, which uses AB matching to split data into names and values.
            header_data_clean <- paste0(header_data_clean[seq(1, length(header_data_clean), by = 2)], header_data_clean[seq(2, length(header_data_clean), by = 2)])
            header_data_clean <- str_split(header_data_clean, pattern = ":")
            names_vector  <- sapply(header_data_clean, `[[`, 1) #Header labels
            values_vector <- sapply(header_data_clean, `[[`, 2) #Header values
            header_data_clean <- data.frame(as.list(values_vector)) #Create dataframe
            names(header_data_clean) <- names_vector # Set names.

            return(list("header" = header_data_clean, "data" = real_data))

        }else{

            return(list("header" = "", "data" = real_data))
        }

    }


perform_auto_QAQC <- function(final_level, water_accessory, logger_name, log_file_path, trim_days){
  # Open the log file and add a line noting the QA/QC details.
  log_file <- file(log_file_path, open = "at")
  writeLines("\nQA/QC details", log_file)
  close(log_file)
  
  # If option was selected, trim first and last days from the level file.
  if(trim_days){
    temp <- final_level
    temp$mdy <- format(temp$time, "%m/%d/%y")
    first_day <- min(temp$mdy)
    last_day <- max(temp$mdy)
    num_removed_first_day <- sum(temp$mdy == first_day)
    num_removed_last_day <- sum(temp$mdy == last_day)
    final_level <- temp %>%
    filter(mdy != first_day & mdy != last_day) %>%
    select(-mdy)

    # Open the log file again and add a line noting the dates and number of observations removed.
    log_file <- file(log_file_path, open = "at")
    writeLines(c(
    paste("Removed first day:", first_day, paste0("(", num_removed_first_day, " Cut)")),
    paste("Removed last day:", last_day,  paste0("(", num_removed_last_day, " Cut)"))
    ), log_file)
    close(log_file)
  }

  final_level$water_temp[1] <- -1
  final_level$water_temp[10] <- -1

  #check for water temp observations below zero.
  if (any(final_level$water_temp < 0, na.rm = TRUE)) {
    time_codes <- final_level$time[final_level$water_temp < 0]
    final_level$water_temp[final_level$water_temp < 0] <- NA
    log_file <- file(log_file_path, open = "at")
    writeLines("Water temperature observations below zero found and set to NA. Time codes of affected observations:", log_file)
    writeLines(, log_file)
    writeLines(as.character(time_codes), log_file)
    close(log_file)
  }

  #Check for water levels less than the long-term cutoff ("exposure_height" in the water accessory metadata) and set to NA.
  exposure_height <- water_accessory$exposure_height[water_accessory$alt_name == logger_name]
  if (is.na(exposure_height)) {
    log_file <- file(log_file_path, open = "at")
    writeLines("No long-term cutoff (exposure height) is available in the metadata.", log_file)
    close(log_file)
  } else {
    if (any(final_level$water_level_NAVD88 < exposure_height, na.rm = TRUE)) {
      time_codes <- final_level$time[final_level$water_level_NAVD88 < exposure_height]
      final_level$water_level_NAVD88[final_level$water_level_NAVD88 < exposure_height] <- NA
      log_file <- file(log_file_path, open = "at")
      writeLines("Water level observations below exposure height found and set to NA. Time codes of affected observations:", log_file)
      writeLines(as.character(time_codes), log_file)
      close(log_file)
    }
  }

  #Check for extreme values in water temp, salinity, and water level.
  # Check for outliers in water_temp, salinity, and water_level_NAVD88
  check_outliers <- function(data, column_name, log_file_path) {
    lower_bound <- quantile(data[[column_name]], 0.01, na.rm = TRUE)
    upper_bound <- quantile(data[[column_name]], 0.99, na.rm = TRUE)
    outliers <- data %>% filter(data[[column_name]] < lower_bound | data[[column_name]] > upper_bound)
    if (nrow(outliers) > 0) {
      outlier_counts <- outliers %>% 
        mutate(date = as.Date(time)) %>% 
        group_by(date) %>% 
        summarise(count = n())
      log_file <- file(log_file_path, open = "at")
      writeLines(paste0("Outliers detected in ", column_name, " (lower bound: ", lower_bound, " Upper bound: ", upper_bound, ") and set to NA. Number of occurrences each day:"), log_file)
      writeLines(apply(outlier_counts, 1, function(row) paste(row[1], ":", row[2])), log_file)
      close(log_file)
      data[[column_name]][data[[column_name]] < lower_bound | data[[column_name]] > upper_bound] <- NA
    }
    return(data)
  }

  final_level <- check_outliers(final_level, "water_temp", log_file_path)
  final_level <- check_outliers(final_level, "salinity", log_file_path)
  final_level <- check_outliers(final_level, "water_level_NAVD88", log_file_path)


  return(final_level)

}


#-----------------------------------------------------------------------------------------------------
# ------------ Primary processing logic starts here --------------------------------------------------
#----------------------------------------------------------------------------------------------------

process_data <- function(data_root,
              baro_path,
              baro_type,
                level_path, 
                level_type, 
                  level_QAQC_path,
                  water_accessory_path,
                    trim_days){

  # Derive parameters from inputs.

  print("Deriving parameters")
  level_processed_path <- file.path(data_root, "processed data")
  name_elements        <- strsplit(tools::file_path_sans_ext(basename(level_path)), split = "_")[[1]]
  logger_name          <- name_elements[1]

  level_start <- name_elements[2] %>% str_replace_all("\\.", "_")
  level_end   <- name_elements[3] %>% str_replace_all("\\.", "_")
  year_end    <-  paste0(as.character(lubridate::year(as.Date(level_end, format = "%Y_%m_%d")) + 1), "_01_01")

  level_processed_file_name <- paste0(paste(logger_name, name_elements[2], name_elements[3], sep = "_"), ".csv")

  level_QAQC <- read.csv(level_QAQC_path)
  level_QAQC$time <- ymd_hms(level_QAQC$time, tz = Sys.timezone())
  pre_year_start <- paste0(year(min(level_QAQC$time, na.rm = T)) - 1, "-12-31")
  exact_start_date <- format(min(level_QAQC$time, na.rm = T), "%Y-%m-%d")

  print("Running processing script with the following parameters:")
  print(paste("baro_path: ", baro_path))
  print(paste("baro_type: ", baro_type))
  print(paste("level_path: ", level_path))
  print(paste("processed_path: ", level_processed_path))
  print(paste("processed_file_name: ", level_processed_file_name))
  print(paste("level_type: ", level_type))
  print(paste("level_QAQC_path: ", level_QAQC_path))
  print(paste("water_accessory_path: ", water_accessory_path))
  print(paste("name_elements: ", name_elements))
  print(paste("logger_name: ", logger_name))
  print(paste("level_start: ", level_start))
  print(paste("level_end: ", level_end))
  
  # Create a log file with the described parameters
  log_file_name <- paste0(tools::file_path_sans_ext(level_processed_file_name), "_log.txt")
  log_file_path <- file.path(level_processed_path, log_file_name)
  log_file <- file(log_file_path, open = "wt")

  writeLines(c(
    "Processing Log",
    paste("Date:", Sys.Date()),
    paste("baro_path:", baro_path),
    paste("baro_type:", baro_type),
    paste("level_path:", level_path),
    paste("processed_path:", level_processed_path),
    paste("processed_file_name:", level_processed_file_name),
    paste("level_type:", level_type),
    paste("level_QAQC_path:", level_QAQC_path),
    paste("water_accessory_path:", water_accessory_path),
    paste("name_elements:", paste(name_elements, collapse = ", ")),
    paste("logger_name:", logger_name),
    paste("level_start:", level_start),
    paste("level_end:", level_end)
  ), log_file)

  close(log_file)

  # 1.  Load logger data. Convert microsiemens to millisiemens. ------------------------------------------------------
  print("1. Loading logger data.")
  
  out <- removeheader(level_path)

  leveldata <- out$data
  headerdata <- out$header

  if(headerdata$CONDUCTIVITYUNIT == " µS/cm"){ #Check units listed in the header data. 
    leveldata$CONDUCTIVITY = leveldata$CONDUCTIVITY/1000 # If it is microsiemens, (uS), we convert to millisiemens by dividing by 1000.
    # Open the log file again and add a line noting the conversion.
    log_file <- file(log_file_path, open = "at")
    writeLines("Converted conductivity unit to millisiemens.", log_file)
    close(log_file)
  }


  # 2. Read and process baro data. --------------------------------------------------------------------
  print("2. Loading and processing baro data.")
  baro.fxn = function(baro_type, baro_path){ 
    if(baro_type == "logger"){ # Check baro type. If logger, use logger standardization function.
      standardize.baro(baro_path) # TODO: Currently not a function explicitly written in the script.
    }else{
      standardize.tower.baro(baro_path) #If not logger, assumes it is tower, and calls that function instead.
    }
  }

  baro = baro.fxn(baro_type, baro_path) # load the designated processed baro data.

  # 3. standardize the water level data. ------------------------------------------------------------
  print("3. standardizing water level data.")
  level.fxn = function(level_type, leveldata){
    if(level_type == "hobo"){ # Check leve_type. If it is a hobo, use the standardize hobo function.
    standardize.hobo(leveldata) # TODO: Currently not a function explicitly written in the script.
    }else{
    standardize.solinst(leveldata) # If not a hobo, use the standardize solonist function to standardize data.
    } 
  } 

  level = level.fxn(level_type, leveldata) # Standardize the water level data.

  water_accessory = read.csv(water_accessory_path) # Pull in water accessory information look-up table

  water_accessory$alt_name <- str_replace_all(water_accessory$logger, pattern = " ", replacement = "") # standardize logger names by removing spaces.

  # Convert total pressure to water level above sensor using local water density & barometric pressure
  level_barocomp = tp.to.wlas(level, baro, water_accessory$water_density[water_accessory$alt_name==logger_name], log_file_path) # TODO: Might be an issue with pressure range.

  # Add final water level NAVD88 column by adding sensor elevation in NAVD88 to water level above sensor
  level_barocomp$water_level_NAVD88 = level_barocomp$water_level_above_sensor + 
    water_accessory$sensor_navd88[water_accessory$alt_name==logger_name]

  # Re-name temperature columns to be more clear
  final_level = level_barocomp %>%
    rename(water_temp = i.temperature, air_temp = temperature)

  # Perform Auto QAQC
  final_level_cleaned = perform_auto_QAQC(final_level, water_accessory, logger_name, log_file_path, trim_days)

  # Export processed dataset
  write.csv(final_level_cleaned, file.path(level_processed_path, level_processed_file_name))

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
  fs::file_move(level_QAQC_path, file.path(dirname(level_QAQC_path), "history"))

  if (any(grepl("Warning", readLines(log_file_path)))) {
    return("Processing complete with some warnings.")
  } else {
    return("Processing complete with no errors.")
  }
}
