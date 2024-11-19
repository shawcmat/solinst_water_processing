# read in working processed file
data_working = readRDS(working.path)

# read in accessory file
water.accessory = read.csv("water.accessory.csv")

# trim water temperature and level data to remove any readings when water level is less than long-term cutoff
data_working = data_working %>%
  mutate(water.temp = ifelse(time >= start.date & water.level.NAVD88 < water.accessory$exposure.height[water.accessory$logger==logger.name], NA, water.temp), 
         water.level.NAVD88 = ifelse(time >= start.date & water.level.NAVD88 < water.accessory$exposure.height[water.accessory$logger==logger.name], NA, water.level.NAVD88))
# further trim any erroneous temperature readings below 0
data_working$water.temp = ifelse(data_working$water.temp < 0, NA, data_working$water.temp)

# save the final qaqced file
saveRDS(data_working, qaqc.path)