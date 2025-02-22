# read in and process baro data
baro.fxn = function(baro_type) {
  if (baro_type == "logger") standardize.baro(baro_path)
  else (standardize.tower.baro(baro_path))
}

baro = baro.fxn(baro_type)

# Read in and process water level data
level.fxn = function(level_type) {
  if (level_type == "hobo") standardize.hobo(level_path)
  else standardize.solinst(level_path)
} 

level = level.fxn(level_type)

# Pull in water accessory information look-up table

water_accessory = read.csv("../2 Processing & analysis/water_accessory.csv")

# Convert total pressure to water level above sensor using local water density & barometric pressure
level_barocomp = tp.to.wlas(level, baro, water_accessory$water_density[water_accessory$logger==logger_name])

# Add final water level NAVD88 column by adding sensor elevation in NAVD88 to water level above sensor
level_barocomp$water_level_NAVD88 = level_barocomp$water_level_above_sensor + 
  water_accessory$sensor_navd88[water_accessory$logger==logger_name]

# Re-name temperature columns to be more clear
final_level = level_barocomp %>%
  rename(water_temp = i.temperature, air_temp = temperature)

# Export processed dataset
saveRDS(final_level, level_processed_path)
