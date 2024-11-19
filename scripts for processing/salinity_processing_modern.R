# read in and clean up salinity data
salinity_data = clean.odyssey(salinity.path)

# read in water accessory dataframe
water.accessory = read.csv("water.accessory.csv")

# read in QAQCed water level data
level_data = readRDS(level.QAQC.path)

# overwrite WlTools filter_ody function to fix problems
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

# filter out dry/low water salinity/temp readings
filtered = filter_ody(ody_df = salinity_data, waterlevel_df = level_data, 
                      height.offset = water.accessory$offset[water.accessory$logger==logger.name], return.data = TRUE)

# remove columns we don't need and rename for ease of interpretation
filtered.final = filtered %>% 
  select(-serial.number, -month, -date, -time) %>%
  rename(time = date.time, water.temp = temperature.calibrated, conductivity = spcond.calibrated)

# Export processed dataset
saveRDS(filtered.final, salinity.processed.path)