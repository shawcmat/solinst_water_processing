library(dplyr)
library(ggplot2)
library(data.table)
library(readxl)
library(lubridate)
library(tools)
library(stringr)
library(fs)
library(pracma)

test_inputs <- function(data_root,
                         baro_path,
                          baro_type,
                           raw_data_path, 
                            level_type, 
                             level_QAQC_path,
                              water_metadata_path){  
return(paste(data_root,
              baro_path,
               baro_type,
                raw_data_path,
                 level_type,
                  level_QAQC_path,
                   water_metadata_path))    
}

#longterm_data_path <- "C:/Users/mshawcroft/water_processing data test/Dutch Slough (template)/BigBreak/QAQC data/BigBreak_2022.02.23_2023.04.05_QAQC_longterm.csv"
#ind_data_path <- "C:/Users/mshawcroft/water_processing data test/Dutch Slough (template)/BigBreak/QAQC data/individual/BigBreak_2022.02.23_2022.09.28_QAQC.csv"

generate_longterm_plot <- function(longterm_data_path, ind_data_path, visualize_with_new){

  longterm_dat <- read.csv(longterm_data_path) %>% select(time, air_temp, water_temp, salinity, water_level_above_sensor, water_level_NAVD88)
  ind_dat <- read.csv(ind_data_path) %>% select(time, air_temp, water_temp, salinity, water_level_above_sensor, water_level_NAVD88)

  dat1 <- rbind(longterm_dat, ind_dat) #Both data sets together.
  dat2 <- longterm_dat #Just the existing longterm dataset.

  dat1$month <- floor_date(as.Date(dat1$time), "month")
  dat2$month <- floor_date(as.Date(dat2$time), "month")

  monthly_counts1 <- dat1 %>% group_by(month) %>% summarise(count = n())
  monthly_counts2 <- dat2 %>% group_by(month) %>% summarise(count = n())

  if(visualize_with_new){
    outplot <- ggplot() +
               geom_bar(data = monthly_counts1, aes(x = month, y = count, fill = "Combined Data"), stat = "identity", alpha = 1) +
               geom_bar(data = monthly_counts2, aes(x = month, y = count, fill = "Long-term Data"), stat = "identity", alpha = 1) +
               labs(title = "Number of Observations per Month", x = "Month", y = "Number of Observations") +
               scale_fill_manual(name = "Dataset", values = c("Long-term Data" = "blue", "Combined Data" = "red")) +
               theme_minimal()
  }else{
    outplot <- ggplot() +
           geom_bar(data = monthly_counts2, aes(x = month, y = count, fill = "Long-term Data"), stat = "identity", alpha = 1) +
           labs(title = "Number of Observations per Month", x = "Month", y = "Number of Observations") +
           scale_fill_manual(name = "Dataset", values = c("Long-term Data" = "blue")) +
           theme_minimal()

  }
  return(outplot)
}

generate_level_plot <- function(primary_data_path, secondary_data_path = NULL, view_start = NULL, view_end = NULL, start_trim, end_trim, variable_name = "water_level_NAVD88", mixed = FALSE) {
  print(paste("view_start:", view_start))
  print(paste("view_end:", view_end))

  if(mixed){
    dat1 <- read.csv(primary_data_path)
    dat2 <- read.csv(secondary_data_path)
    dat2 <- dat2[,c("time", variable_name)]

    temp_name <- paste0(variable_name, "2")
    names(dat2) <- c("time", temp_name)

    dat <- left_join(dat1, dat2, by = join_by(time == time), multiple = "first")
    
  }else{
    dat <- read.csv(primary_data_path)
  }

  dat$ymd <- as.Date(dat$time)

  # Trim the data
  if(!is.null(start_trim)){
    if(!start_trim == 0){
      start_date <- min(dat$ymd) + days(start_trim)
    }else{
      start_date <- min(dat$ymd)
    }
  }else{
    start_date <- min(dat$ymd)
  }
  if(!is.null(end_trim)){
    if(!end_trim == 0){
      end_date <- max(dat$ymd) - days(end_trim)
    }else{
      end_date <- max(dat$ymd)
    }
  }else{#
    end_date <- max(dat$ymd)
  }

  trimmed_data <- dat %>% filter(ymd >= start_date) %>% filter(ymd <= end_date)

  print("Data trimmed.")

  view_start <- as.Date(view_start, format = "%m/%d/%Y")
  view_end <- as.Date(view_end, format = "%m/%d/%Y")

  # Set view range
  if(is.null(view_start)){
    view_start <- min(trimmed_data$ymd)
  }
    
  if(view_start < min(trimmed_data$ymd) | view_start > max(trimmed_data$ymd)){
    view_start <- min(trimmed_data$ymd)
  }

  if(is.null(view_end)){
    view_end <- max(trimmed_data$ymd)
  }

  if(view_end > max(trimmed_data$ymd) | view_end < min(trimmed_data$ymd)){
    view_end <- max(trimmed_data$ymd)
  }

  view_start <- as.Date(view_start)
  view_end <- as.Date(view_end)

  print("View range set.")

  # Filter data based on view range
  view_data <- trimmed_data %>% filter(ymd >= view_start & ymd <= view_end)
  print("Data filtered based on view range.")
  # Set time as POSIX for time series
  view_data$time <- as.POSIXct(view_data$time, format="%Y-%m-%d %H:%M:%S")
  # Generate the plot
  if(mixed == TRUE & !is.null(secondary_data_path)){
    this_plot <- ggplot(data = view_data, aes(x = time)) +
                  geom_line(aes(y = .data[[variable_name]], color = "pre-autoQAQC")) +
                  geom_line(aes(y = .data[[temp_name]], color = "post-autoQAQC"))

  }else{
    this_plot <- ggplot(data = view_data, aes(x = time, y = .data[[variable_name]])) + geom_line() + ggtitle("Time series", subtitle = variable_name)
  } 
  print("plot generated.")
  return(this_plot)
  
}

check_baro_dates <- function(baro_path){
  dat <- standardize.tower.baro(baro_path)
  min_time <- min(dat$time)
  max_time <- max(dat$time)
  min_time_str <- format(min_time, "%m/%d/%Y")
  max_time_str <- format(max_time, "%m/%d/%Y")
  message <- paste("First date:", min_time_str, "Last date:", max_time_str)
  return(list("start_date" = min_time_str, "end_date" = max_time_str, "message" = message))
}

check_level_dates <- function(solinst_path){

  name_elements        <- strsplit(tools::file_path_sans_ext(basename(solinst_path)), split = "_")[[1]]
  logger_name          <- name_elements[1]

  dat <- removeheader(solinst_path)
  dat <- dat$data
  dat <- transmute(dat,
   time = if(any(grepl("M", Time))){as.POSIXct(paste(Date, Time), format = "%m/%d/%Y %I:%M:%S %p", tz = "Etc/GMT+8")}
   else{as.POSIXct(paste(Date, Time), format = "%m/%d/%Y %H:%M:%S", tz = "Etc/GMT+8")})
  min_time <- min(dat$time)
  max_time <- max(dat$time)
  min_time_str <- format(min_time, "%m/%d/%Y")
  max_time_str <- format(max_time, "%m/%d/%Y")
  
  message <- paste("First date:", min_time_str, "Last date:", max_time_str, "     File name:", basename(solinst_path), "     Logger Name:", logger_name)

  return(list("start_date" = min_time_str, "end_date" = max_time_str, "message" = message))
}

check_longterm_dates <- function(longterm_QAQC_path){
  dat <- read.csv(longterm_QAQC_path)
  min_time <- min(dat$time)
  max_time <- max(dat$time)
  min_time_str <- format(as.Date(min_time), "%m/%d/%Y")
  max_time_str <- format(as.Date(max_time), "%m/%d/%Y")
  message <- paste("First date:", min_time_str, "Last date:", max_time_str)

  return(list("start_date" = min_time_str, "end_date" = max_time_str, "message" = message))
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
  
  clean.solinst <- function(solinst.data, log_file_path) {
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

  solinst.data <- clean.solinst(solinst.data, log_file_path)
  unit.conversion(solinst.data, programmed.h2o.density)
}


# total pressure to water level above sensor (UPDATED 8-2022 SFJ)
tp.to.wlas = function (logger.data, baro.data, h2o.density = 1000, log_file_path) {

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
removeheader <- function(raw_data_path) {
  headers <- readLines(raw_data_path, n = 25) #Load lines of text, capturing header.
  header.count = suppressWarnings(min(grep("^Date,Time,.*", headers)) - 1) # Derive the size of the header but searching for the column names.
  real_data <- read.csv(raw_data_path, stringsAsFactors = FALSE, skip = header.count) # Load actual data, skipping header.
  if(header.count > 1){
      header_data <- readLines(raw_data_path, n = header.count) #load just the header
      header_data <- iconv(header_data, from = "ISO-8859-1", to = "UTF-8") #Convert encoding to UTF-9 to preserve units information.
      offset <- header_data[grep(pattern = "^Offset", header_data)] #Extract offset, as format is different than other sections.
      header_data_clean <- header_data[-grep(pattern = "^Offset", header_data)] # Remove offset section from the main header data.
      num_pairs <- length(header_data_clean) %/% 2 # used in the following code, which uses AB matching to split data into names and values.
      header_data_clean <- paste0(header_data_clean[seq(1, length(header_data_clean), by = 2)], header_data_clean[seq(2, length(header_data_clean), by = 2)])
      header_data_clean <- str_split(header_data_clean, pattern = ":")
      names_vector  <- sapply(header_data_clean, `[[`, 1) #Header labels
      values_vector <- sapply(header_data_clean, `[[`, 2) #Header values
      header_data_clean <- data.frame(as.list(values_vector)) #Create dataframe
      names(header_data_clean) <- names_vector # Set name
      return(list("header" = header_data_clean, "data" = real_data))
  }else{
      return(list("header" = "", "data" = real_data))
  }
}

#-----------------------------------------------------------------------------------------------------
# ------------ Primary processing logic starts here --------------------------------------------------
#----------------------------------------------------------------------------------------------------

process_data <- function(data_root,
                          baro_path,
                           baro_type,
                            raw_data_path, 
                              level_type,
                                  water_metadata_path){


  print("Running processing script with the following parameters:")
  print(paste("data_root: ", data_root))
  print(paste("baro_path: ", baro_path))
  print(paste("baro_type: ", baro_type))
  print(paste("raw_data_path: ", raw_data_path))
  print(paste("level_type: ", level_type))
  print(paste("water_metadata_path: ", water_metadata_path))


  # Derive parameters from inputs.
  print("Deriving parameters")
  level_processed_path <- file.path(data_root, "processed data")
  dir.create(level_processed_path, showWarnings = FALSE)
  name_elements        <- strsplit(tools::file_path_sans_ext(basename(raw_data_path)), split = "_")[[1]]
  logger_name          <- name_elements[1]

  dates_output <- check_level_dates(raw_data_path)

  level_start <- dates_output$start_date
  level_end   <- dates_output$end_date
  year_end    <-  paste0(as.character(lubridate::year(as.Date(level_end, format = "%m/%d/%Y")) + 1), "/01/01")

  level_processed_file_name <- paste0(paste(logger_name, name_elements[2], name_elements[3], "processed", sep = "_"), ".csv")

  print("Additional derived parameters:")
  print(paste("processed_path: ", level_processed_path))
  print(paste("processed_file_name: ", level_processed_file_name))
  print(paste("name_elements: ", name_elements))
  print(paste("logger_name: ", logger_name))
  print(paste("level_start: ", level_start))
  print(paste("level_end: ", level_end))
  
  # Create a log file with the described parameters
  logs_directory <- file.path(data_root, ".logs")
  dir.create(logs_directory, showWarnings = FALSE)
  log_file_name <- paste0(paste(logger_name, name_elements[2], name_elements[3], sep = "_"), "_log.txt")
  log_file_path <- file.path(logs_directory, log_file_name)
  log_file <- file(log_file_path, open = "wt")

  writeLines(c(
    "Processing Log",
    paste("Date:", Sys.Date()),
    paste("baro_path:", baro_path),
    paste("baro_type:", baro_type),
    paste("raw_data_path:", raw_data_path),
    paste("processed_path:", level_processed_path),
    paste("processed_file_name:", level_processed_file_name),
    paste("level_type:", level_type),
    paste("water_metadata_path:", water_metadata_path),
    paste("name_elements:", paste(name_elements, collapse = ", ")),
    paste("logger_name:", logger_name),
    paste("level_start:", level_start),
    paste("level_end:", level_end)
  ), log_file)

  close(log_file)

  # 1.  Load logger data. Convert microsiemens to millisiemens. ------------------------------------------------------
  print("1. Loading logger data.")
  
  out <- removeheader(raw_data_path)

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
  
  if(level_type != "solinst"){
    log_file <- file(log_file_path, open = "at")
    writeLines("Processing error: Currently only data from Solinst instruments have been tested with this tool.", log_file)
    close(log_file)
    stop("Processing error: Currently only Solinst instruments are supported with this tool.")
  }

  level = standardize.solinst(leveldata, log_file_path = log_file_path) # Standardize the water level data.
 
 # Pull in water accessory information look-up table
  water_metadata = read_excel(water_metadata_path, sheet = "logger metadata") %>% select(loggerID, dataStart, dataEnd, sensorElev, waterDensity, exposureHeight )
  water_metadata$dataStart <- format(as.Date(water_metadata$dataStart, format = "%Y-%m-%d"), format = "%m/%d/%Y")
  water_metadata$dataEnd <- format(as.Date(water_metadata$dataEnd, format = "%Y-%m-%d"), format = "%m/%d/%Y")
   
  #Find the record that matches the deployment.
  water_metadata = water_metadata %>% filter(loggerID == logger_name) %>% filter(as.character(dataStart) == level_start)
 
  # Convert total pressure to water level above sensor using local water density & barometric pressure
  level_barocomp = tp.to.wlas(level, baro, water_metadata$waterDensity, log_file_path) # TODO: Might be an issue with pressure range.

  # Add final water level NAVD88 column by adding sensor elevation in NAVD88 to water level above sensor
  level_barocomp$water_level_NAVD88 = level_barocomp$water_level_above_sensor + water_metadata$sensorElev

  # Re-name temperature columns to be more clear
  final_level = level_barocomp %>%
    rename(water_temp = i.temperature, air_temp = temperature)

  # Fix time field
  final_level$time <- format(final_level$time, "%Y-%m-%d %H:%M:%S")

  # Export processed dataset
  processed_file_out <- file.path(level_processed_path, level_processed_file_name)
  write.csv(final_level, processed_file_out )

  if (any(grepl("Warning", readLines(log_file_path)))) {
    status = paste("Processing complete with some warnings. Output saved to", processed_file_out)
  } else {
    status = paste("Processing complete with no errors. Output saved to", processed_file_out)
  }

  print("processing complete")

  return(list("processed_fileout" = processed_file_out,
              "status" = status))

  
}


data_root <- "C:/Users/mshawcroft/water_processing data test/testing/" 
level_processed_path <- "C:/Users/mshawcroft/water_processing data test/testing/raw data/BigBreakGW_2022.02.23_2022.09.28.csv"
trim_days_start <- 1
trim_days_end <- NULL
auto_outlier_detection <- TRUE
check_data_gaps <- TRUE
water_metadata_path <- "C:\\Users\\mshawcroft\\water_processing data test\\waterlogger metadata.xlsx"
  
#tout <- perform_auto_QAQC(data_root, level_processed_path, water_metadata_path, trim_days_start, trim_days_end, auto_outlier_detection, check_data_gaps)

perform_auto_QAQC <- function(data_root,
                              level_processed_path,
                              water_metadata_path,
                              trim_days_start,
                              trim_days_end,
                              auto_outlier_detection,
                              check_data_gaps,
                              waterLevel_zscore_threshold = 5,
                              salinity_zscore_threshold = 5,
                              waterTemp_zscore_threshold = 5){

  print("Performing auto QAQC.")
  name_elements <- strsplit(tools::file_path_sans_ext(basename(level_processed_path)), split = "_")[[1]]
  logger_name <- name_elements[1]                              
  level_start <- name_elements[2] %>% str_replace_all("\\.", "_")
  level_end   <- name_elements[3] %>% str_replace_all("\\.", "_")

  QAQC_directory <- file.path(data_root, "QAQC data" )
  dir.create(QAQC_directory, showWarnings = FALSE)
  QAQC_ind_directory <- file.path(QAQC_directory, "individual")
  dir.create(QAQC_ind_directory, showWarnings = FALSE)

  level_QAQC_ind_name <- paste0(paste(logger_name, name_elements[2], name_elements[3], "QAQC", sep = "_"), ".csv")


  # Pull in water accessory information look-up table
  
  check_start <- as.Date(level_start, format = "%Y_%m_%d") %>% format("%m/%d/%Y")
  check_end <- as.Date(level_end, format = "%Y_%m_%d") %>% format("%m/%d/%Y")

  water_metadata = read_excel(water_metadata_path, sheet = "logger metadata") %>% select(loggerID, dataStart, dataEnd, sensorElev, waterDensity, exposureHeight )
  water_metadata$dataStart <- format(as.Date(water_metadata$dataStart, format = "%Y-%m-%d"), format = "%m/%d/%Y")
  water_metadata$dataEnd <- format(as.Date(water_metadata$dataEnd, format = "%Y-%m-%d"), format = "%m/%d/%Y")
   
  #Find the record that matches the deployment.
  water_metadata = water_metadata %>% filter(loggerID == logger_name) %>% filter(as.character(dataStart) == check_start) %>% filter(as.character(dataEnd) == check_end)
 
  processed_data <- read.csv(level_processed_path) %>% select(time, air_temp, water_temp, salinity, water_level_above_sensor, water_level_NAVD88)
  #processed_data$time[4290:4302]
  processed_data$time <- as.POSIXct(processed_data$time, format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT+8")

  # Open the log file and add a line noting the QA/QC details.
  logs_directory <- file.path(data_root, ".logs")
  log_file_name <- paste0(paste(logger_name, name_elements[2], name_elements[3], sep = "_"), "_log.txt")
  log_file_path <- file.path(logs_directory, log_file_name)
  log_file <- file(log_file_path, open = "at")
  writeLines(c(
    "\nQA/QC details",
    paste("trim_days_start:", trim_days_start),
    paste("trim_days_end:", trim_days_end),
    paste("auto_outlier_detection:", auto_outlier_detection),
    paste("check_data_gaps:", check_data_gaps)
  ), log_file)
  close(log_file)

  # Check if trim_days_start or trim_days_end is null and set to 0.
  if (is.null(trim_days_start)) {
    trim_days_start <- 0
  }
  if (is.null(trim_days_end)) {
    trim_days_end <- 0
  }
  
  # Trim start and end days according to specification.
  print("Trimming start and end days according to specification.")
  if(!(trim_days_start == 0 & trim_days_end == 0)){
    log_file <- file(log_file_path, open = "at")
    writeLines("\nStart/End day trimming:\n", log_file)
    close(log_file)

    start_date <- min(as.Date(processed_data$time) + days(trim_days_start))
    end_date <- max(as.Date(processed_data$time) - days(trim_days_end))
    num_removed_start <- sum(as.Date(processed_data$time) < start_date)
    num_removed_end <- sum(as.Date(processed_data$time) > end_date)
    removed_start_dates <- unique(as.Date(processed_data$time[as.Date(processed_data$time) < start_date]))
    removed_end_dates <- unique(as.Date(processed_data$time[as.Date(processed_data$time) > end_date]))
    processed_data <- processed_data %>% 
      filter(as.Date(time) >= start_date & as.Date(time) <= end_date)

    # Open the log file and add lines noting the dates and number of observations removed.
    log_file <- file(log_file_path, open = "at")
    writeLines(c(
      paste("Removed first", trim_days_start, "days.", paste0("(", num_removed_start, " observations removed)")),
      paste("Dates removed:", paste(removed_start_dates, collapse = ", ")),
      paste("Removed last", trim_days_end, "days.", paste0("(", num_removed_end, " observations removed)")),
      paste("Dates removed:", paste(removed_end_dates, collapse = ", "))
    ), log_file)
    close(log_file)
  }

  # Run basic outlier detection using z-scores.
  print("Running basic outlier detection using Z-scores.")
  if(auto_outlier_detection){
    log_file <- file(log_file_path, open = "at")
    writeLines("\nBasic outlier detection using modified Z-score:\n", log_file)
    close(log_file)

  check_outliers <- function(data, var_name, threshold, log_file_path) {
    x <- data[[var_name]]
    x_med <- median(x, na.rm = T)
    MAD <- median(abs(x - x_med))
    mod_zscore <- 0.6745 * (x - x_med) / MAD
   
    data[["mod_zscore"]] <- mod_zscore

    num_changed <- sum(data[["mod_zscore"]] > threshold | data[["mod_zscore"]] < (0 - threshold), na.rm = TRUE)
    outliers <- data %>% filter(mod_zscore > threshold | mod_zscore < 0-threshold)
    data_mean <- mean(x, na.rm = TRUE)
    data <- data %>% mutate(zfiltered = ifelse(mod_zscore > threshold | mod_zscore < (0-threshold), NA, data[[var_name]]))

    if (num_changed > 0) {
      outlier_counts <- outliers %>% 
        mutate(date = as.Date(time)) %>% 
        group_by(date) %>% 
        summarise(count = n())
      log_file <- file(log_file_path, open = "at")
      writeLines(paste0("Outliers detected in ", var_name, " using a threshold of ", threshold, ". Number of occurrences each day:"), log_file)
      writeLines(apply(outlier_counts, 1, function(row) paste(row[1], ":", row[2])), log_file)
      close(log_file)
      data[[var_name]] <- data$zfiltered
    }
    return(data)
  }

  processed_data <- check_outliers(processed_data, "water_level_NAVD88", threshold = waterLevel_zscore_threshold, log_file_path)
  processed_data <- check_outliers(processed_data, "salinity", threshold = salinity_zscore_threshold, log_file_path)
  processed_data <- check_outliers(processed_data, "water_temp", threshold = waterTemp_zscore_threshold, log_file_path)

  }

  # Check for data gaps.
  print("Checking for data gaps.")
  if (check_data_gaps) {
    log_file <- file(log_file_path, open = "at")
    writeLines("\nChecking for data gaps:\n", log_file)
    close(log_file)
    
    processed_data$time <- as.POSIXct(processed_data$time, format="%Y-%m-%d %H:%M:%S")
    time_diffs <- diff(processed_data$time)
    expected_diff <- as.difftime(360, units = "secs")
    gaps <- which(time_diffs > expected_diff)
    
    if (length(gaps) > 0) {
      log_file <- file(log_file_path, open = "at")
      for (gap in gaps) {
        if(time_diffs[gap] != expected_diff){
            missing_count <- as.numeric(time_diffs[gap]) / as.numeric(expected_diff) - 1
            start_time <- processed_data$time[gap]
            end_time <- processed_data$time[gap + 1]
            writeLines(paste(missing_count, "observation(s) missing between", format(start_time, "%m/%d/%Y %I:%M:%S %p"), "and", format(end_time, "%m/%d/%Y %I:%M:%S %p")), log_file)
        }

      }
      close(log_file)
    } else {
      log_file <- file(log_file_path, open = "at")
      writeLines("No data gaps detected.", log_file)
      close(log_file)
    }
  }

  log_file <- file(log_file_path, open = "at")
  writeLines("\nAdditional data checks:", log_file)
  close(log_file)

  #check for water temp observations below zero.
  print("Checking for temp observations below zero.")
  if (any(processed_data$water_temp < 0, na.rm = TRUE)) {
    time_codes <- processed_data$time[processed_data$water_temp < 0]
    processed_data$water_temp[processed_data$water_temp < 0] <- NA
    log_file <- file(log_file_path, open = "at")
    writeLines("\nWater temperature observations below zero found and set to NA. Time codes of affected observations:", log_file)
    writeLines(as.character(time_codes), log_file)
    close(log_file)
  } else {
    log_file <- file(log_file_path, open = "at")
    writeLines("\nNo water temperature observations below zero found.", log_file)
    close(log_file)
  }

  #Check for water levels less than the long-term cutoff ("exposure_height" in the water accessory metadata) and set to NA.
  print("Checking for water levels less than long-term cutoff.")
  exposure_height <- water_metadata$exposureHeight
  print(exposure_height)
  if (is.na(exposure_height) | exposure_height == 0) {
    print("no exposure height.")
    log_file <- file(log_file_path, open = "at")
    writeLines("\nNo long-term cutoff (exposure height) is available in the metadata. Skipping check.", log_file)
    close(log_file)
  } else {
    print("exposure height found.")
    if (any(processed_data$water_level_NAVD88 < exposure_height, na.rm = TRUE)) {
      print("There is data to trim.")
      time_codes <- processed_data$time[processed_data$water_level_NAVD88 < exposure_height]
      processed_data$water_level_NAVD88[processed_data$water_level_NAVD88 < exposure_height] <- NA
      log_file <- file(log_file_path, open = "at")
      writeLines("\nWater level observations below exposure height found and set to NA. Time codes of affected observations:", log_file)
      writeLines(as.character(time_codes), log_file)
      close(log_file)
    }else{
      print("There is no data to trim.")
      log_file <- file(log_file_path, open = "at")
      writeLines("\nNo water level observations were found to be below the exposure height.", log_file)
      close(log_file)
    }
  }

  # Fix time field
  print("Fixing time field.")
  processed_data$time <- format(processed_data$time, "%Y-%m-%d %H:%M:%S")

  ind_QAQC_path <- file.path(QAQC_ind_directory, level_QAQC_ind_name)
  
  print("Saving results.")
  write.csv(processed_data, ind_QAQC_path)
  print("Processing complete.")
  return(ind_QAQC_path)
  
}

#tout <- perform_auto_QAQC(data_root, level_processed_path, water_metadata_path, trim_days_start, trim_days_end, auto_outlier_detection, check_data_gaps)

#data_root = "C:/Users/mshawcroft/water_processing data test/"
#ind_QAQC_path = "C:/Users/mshawcroft/water_processing data test/QAQC data/individual/BigBreak_2022.02.23_2022.09.28_QAQC.csv"
#longterm_QAQC_path = character(0)
#longterm_QAQC_path = "C:/Users/mshawcroft/water_processing data test/QAQC data/BigBreak_2022.02.23_2022.09.26_QAQC_longterm.csv"
#attach_to_longterm(data_root, ind_QAQC_path, longterm_QAQC_path)

attach_to_longterm <- function(data_root, ind_QAQC_path, longterm_QAQC_path = character(0)){

  print("attaching to longterm file")
  name_elements <- strsplit(tools::file_path_sans_ext(basename(ind_QAQC_path)), split = "_")[[1]]
  logger_name <- name_elements[1]                              
  # Join newly processed data with the long term site dataset.-------------------------------------------------------------
  # Combine newly processed data with historic QAQCed data

  ind_QAQC      <- read.csv(ind_QAQC_path) %>% select(time, air_temp, water_temp, salinity, water_level_above_sensor, water_level_NAVD88)

  if(length(longterm_QAQC_path) < 1){
    longterm_QAQC <- ind_QAQC[0,]
  }else{
    longterm_QAQC <- read.csv(longterm_QAQC_path) %>% select(time, air_temp, water_temp, salinity, water_level_above_sensor, water_level_NAVD88)
  }


  combined_QAQC <- rbind(longterm_QAQC, ind_QAQC)
  combined_QAQC$time <- as.POSIXct(combined_QAQC$time, format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT+8")

  #Get new start and end dates for the full dataset.

  new_start_date <- min(as.Date(combined_QAQC$time)) %>% str_replace_all("-", ".")
  new_end_date   <- max(as.Date(combined_QAQC$time)) %>% str_replace_all("-", ".")

  #Fix the time field
  combined_QAQC$time <- format(combined_QAQC$time, "%Y-%m-%d %H:%M:%S")

  new_QAQC_filename <- paste0(paste(logger_name, new_start_date, new_end_date, sep = "_"), "_QAQC_longterm.csv")
  QAQC_basedir      <- file.path(data_root, "QAQC data")
  new_QAQC_fullpath <- file.path(QAQC_basedir, new_QAQC_filename)

  print("Moving previous long-term file into the history folder.")
  # Move old long-term file into the longterm_history folder.
  if(length(longterm_QAQC_path) > 0){
    dir.create(file.path(QAQC_basedir, "longterm_history"), showWarnings = FALSE)
    fs::file_move(longterm_QAQC_path, file.path(QAQC_basedir, "longterm_history"))
  }

  # Export updated long-term dataset
  write.csv(combined_QAQC, new_QAQC_fullpath)

  return(new_QAQC_fullpath)

}
