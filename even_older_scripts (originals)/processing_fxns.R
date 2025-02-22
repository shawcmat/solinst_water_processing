# standardize tower baro data
standardize.tower.baro = function(baro_path) {
  tower.csv <- read.csv(baro_path, stringsAsFactors = FALSE)
  results <- tower.csv %>% 
    transmute(time = as.POSIXct(Time, format = "%m/%d/%Y %H:%M", tz = "Etc/GMT+8"), 
              baro_pressure = air_pressure, 
              temperature = air_temp)
  results
}

# standardize solinst (UPDATED 8-2022 SFJ)
standardize.solinst = function (level_path, programmed.h2o.density = 1000) {
  if (programmed.h2o.density < 900 | programmed.h2o.density >  1100) 
    stop("H2O density should be between 950 and 1050 kg/m^3")
  clean.solinst <- function(level_path) {
    solinst.headers <- readLines(level_path, n = 25)
    header.count = suppressWarnings(min(grep("^Date,Time,.*", solinst.headers)) - 1)
    solinst.csv <- read.csv(level_path, stringsAsFactors = FALSE, skip = header.count)
    results <- solinst.csv %>% 
      filter(LEVEL >= 0, LEVEL <= 20) %>% 
      transmute(time = if (any(grepl("M", Time))) as.POSIXct(paste(Date, Time), format = "%m/%d/%Y %I:%M:%S %p", tz = "Etc/GMT+8")
                       else as.POSIXct(paste(Date, Time), format = "%m/%d/%Y %H:%M:%S", tz = "Etc/GMT+8"), 
                level = if (max(LEVEL) < 9.5) LEVEL + 9.5
                        else LEVEL, 
                temperature = TEMPERATURE, 
                conductivity = if (any(names(solinst.csv) == "CONDUCTIVITY")) CONDUCTIVITY
                               else NA)
    rows.excluded <- sum((solinst.csv$LEVEL < 0) | (solinst.csv$LEVEL > 20), na.rm = TRUE)
    if (rows.excluded > 0) 
      warning(sprintf("%i rows excluded for levels outside of [0, 20]", rows.excluded))
    if (any(names(solinst.csv) == "CONDUCTIVITY") && any(solinst.csv$CONDUCTIVITY > 100)) 
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
  solinst.data <- clean.solinst(level_path)
  unit.conversion(solinst.data, programmed.h2o.density)
}

# total pressure to water level above sensor (UPDATED 8-2022 SFJ)
tp.to.wlas = function (logger.data, baro.data, h2o.density) 
{
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
