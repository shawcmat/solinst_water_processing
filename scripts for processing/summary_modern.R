# import QAQCed data
level.data = readRDS(level.path)
salinity.data = readRDS(salinity.path)

## add in dates/times that are missing, to complete record
test = miner_working %>% complete(time = seq(min(time), max(time), by = "6 min"))


# summarize by day, week, and month
level.data_daily = level.data %>% 
  group_by(day = floor_date(time, "day")) %>% 
  summarize(level.sd = sd(water.level.NAVD88, na.rm = TRUE), water.level.NAVD88 = mean(water.level.NAVD88, na.rm = TRUE), 
            watertemp.sd = sd(water.temp, na.rm = TRUE), water.temp = mean(water.temp, na.rm = TRUE), 
            airtemp.sd = sd(air.temp, na.rm = TRUE), air.temp = mean(air.temp, na.rm = TRUE))
level.data_weekly = level.data %>% 
  group_by(week = floor_date(time, "week")) %>% 
  summarize(level.sd = sd(water.level.NAVD88, na.rm = TRUE), water.level.NAVD88 = mean(water.level.NAVD88, na.rm = TRUE), 
            watertemp.sd = sd(water.temp, na.rm = TRUE), water.temp = mean(water.temp, na.rm = TRUE), 
            airtemp.sd = sd(air.temp, na.rm = TRUE), air.temp = mean(air.temp, na.rm = TRUE))
level.data_monthly = level.data %>% 
  group_by(month = floor_date(time, "month")) %>% 
  summarize(level.sd = sd(water.level.NAVD88, na.rm = TRUE), water.level.NAVD88 = mean(water.level.NAVD88, na.rm = TRUE), 
            watertemp.sd = sd(water.temp, na.rm = TRUE), water.temp = mean(water.temp, na.rm = TRUE), 
            airtemp.sd = sd(air.temp, na.rm = TRUE), air.temp = mean(air.temp, na.rm = TRUE))

# summarize salinity
salinity.data_daily = salinity.data %>% 
  group_by(day = floor_date(time, "day")) %>% 
  summarize(salinity.sd = sd(salinity, na.rm = TRUE), salinity = mean(salinity, na.rm = TRUE), 
            watertemp.sd = sd(water.temp, na.rm = TRUE), water.temp = mean(water.temp, na.rm = TRUE))
salinity.data_weekly = salinity.data %>% 
  group_by(week = floor_date(time, "week")) %>% 
  summarize(salinity.sd = sd(salinity, na.rm = TRUE), salinity = mean(salinity, na.rm = TRUE), 
            watertemp.sd = sd(water.temp, na.rm = TRUE), water.temp = mean(water.temp, na.rm = TRUE))
salinity.data_monthly = salinity.data %>% 
  group_by(month = floor_date(time, "month")) %>% 
  summarize(salinity.sd = sd(salinity, na.rm = TRUE), salinity = mean(salinity, na.rm = TRUE), 
            watertemp.sd = sd(water.temp, na.rm = TRUE), water.temp = mean(water.temp, na.rm = TRUE))












