library(dplyr)
library(ggplot2)


# crms1024----
crms1024 = read.csv("CRMS1024.csv")
crms1024_flood = crms1024 %>% 
  mutate(datetime = as.POSIXct(datetime, format = "%m/%d/%Y %H:%M"), 
         level = Adjusted.Water.Elevation.to.Datum..ft. * 0.3048) %>% 
  filter(level != "NA") %>% 
  mutate(flood_neg0.4 = ifelse(level > -0.4, 1, 0), 
         inundation_neg0.4 = sum(flood_neg0.4) / n(), 
         flood_neg0.3 = ifelse(level > -0.3, 1, 0), 
         inundation_neg0.3 = sum(flood_neg0.3) / n(), 
         flood_neg0.2 = ifelse(level > -0.2, 1, 0), 
         inundation_neg0.2 = sum(flood_neg0.2) / n(), 
         flood_neg0.1 = ifelse(level > -0.1, 1, 0), 
         inundation_neg0.1 = sum(flood_neg0.1) / n(), 
         flood_0.0 = ifelse(level > 0.0, 1, 0), 
         inundation_0.0 = sum(flood_0.0) / n(), 
         flood_0.1 = ifelse(level > 0.1, 1, 0), 
         inundation_0.1 = sum(flood_0.1) / n(), 
         flood_0.2 = ifelse(level > 0.2, 1, 0), 
         inundation_0.2 = sum(flood_0.2) / n(), 
         flood_0.3 = ifelse(level > 0.3, 1, 0), 
         inundation_0.3 = sum(flood_0.3) / n(), 
         flood_0.4 = ifelse(level > 0.4, 1, 0), 
         inundation_0.4 = sum(flood_0.4) / n(), 
         flood_0.5 = ifelse(level > 0.5, 1, 0), 
         inundation_0.5 = sum(flood_0.5) / n(), 
         flood_0.6 = ifelse(level > 0.6, 1, 0), 
         inundation_0.6 = sum(flood_0.6) / n())

crms1024_curve = data.frame(elevation = seq(-0.4, 0.6, 0.1), 
                            inundation = c(crms1024_flood[1,15], crms1024_flood[1,17], crms1024_flood[1,19], 
                                           crms1024_flood[1,21], crms1024_flood[1,23], crms1024_flood[1,25], 
                                           crms1024_flood[1,27], crms1024_flood[1,29], crms1024_flood[1,31], 
                                           crms1024_flood[1,33], crms1024_flood[1,35]))

crms1024_model = lm(data = crms1024_curve, inundation ~ poly(elevation, 3, raw = TRUE))
summary(crms1024_model)

crms1024_fxn = function(x) {
  -1.46216 * x -1.23020 * x^2 + 2.67327 * x^3 + 0.75626
}
crms1024_fxn(0.2)

ggplot(data = crms1024_curve, aes(x = elevation, y = inundation)) + geom_point() + stat_function(fun = crms1024_fxn)

crms1024_summary = crms1024 %>% 
  summarize(temp = mean(Water.Temperature...C., na.rm = TRUE), 
            salinity = mean(Salinity..ppt., na.rm = TRUE), 
            level = mean(Adjusted.Water.Elevation.to.Datum..ft. * 0.3048, na.rm = TRUE))

# crms0147----
crms0147 = read.csv("CRMS0147.csv")
crms0147_flood = crms0147 %>% 
  mutate(datetime = as.POSIXct(datetime, format = "%m/%d/%Y %H:%M"), 
         level = Adjusted.Water.Elevation.to.Datum..ft. * 0.3048) %>% 
  filter(level != "NA") %>% 
  mutate(flood_neg0.4 = ifelse(level > -0.4, 1, 0), 
         inundation_neg0.4 = sum(flood_neg0.4) / n(), 
         flood_neg0.3 = ifelse(level > -0.3, 1, 0), 
         inundation_neg0.3 = sum(flood_neg0.3) / n(), 
         flood_neg0.2 = ifelse(level > -0.2, 1, 0), 
         inundation_neg0.2 = sum(flood_neg0.2) / n(), 
         flood_neg0.1 = ifelse(level > -0.1, 1, 0), 
         inundation_neg0.1 = sum(flood_neg0.1) / n(), 
         flood_0.0 = ifelse(level > 0.0, 1, 0), 
         inundation_0.0 = sum(flood_0.0) / n(), 
         flood_0.1 = ifelse(level > 0.1, 1, 0), 
         inundation_0.1 = sum(flood_0.1) / n(), 
         flood_0.2 = ifelse(level > 0.2, 1, 0), 
         inundation_0.2 = sum(flood_0.2) / n(), 
         flood_0.3 = ifelse(level > 0.3, 1, 0), 
         inundation_0.3 = sum(flood_0.3) / n(), 
         flood_0.4 = ifelse(level > 0.4, 1, 0), 
         inundation_0.4 = sum(flood_0.4) / n(), 
         flood_0.5 = ifelse(level > 0.5, 1, 0), 
         inundation_0.5 = sum(flood_0.5) / n(), 
         flood_0.6 = ifelse(level > 0.6, 1, 0), 
         inundation_0.6 = sum(flood_0.6) / n())

crms0147_curve = data.frame(elevation = seq(-0.4, 0.6, 0.1), 
                            inundation = c(crms0147_flood[1,15], crms0147_flood[1,17], crms0147_flood[1,19], 
                                           crms0147_flood[1,21], crms0147_flood[1,23], crms0147_flood[1,25], 
                                           crms0147_flood[1,27], crms0147_flood[1,29], crms0147_flood[1,31], 
                                           crms0147_flood[1,33], crms0147_flood[1,35]))

crms0147_model = lm(data = crms0147_curve, inundation ~ poly(elevation, 3, raw = TRUE))
summary(crms0147_model)

crms0147_fxn = function(x) {
  -1.481636 * x -0.899091 * x^2 + 2.517692 * x^3 + 0.689592
}

ggplot(data = crms0147_curve, aes(x = elevation, y = inundation)) + geom_point() + stat_function(fun = crms0147_fxn)

crms0147_summary = crms0147 %>% 
  summarize(temp = mean(Water.Temperature...C., na.rm = TRUE), 
            salinity = mean(Salinity..ppt., na.rm = TRUE), 
            level = mean(Adjusted.Water.Elevation.to.Datum..ft. * 0.3048, na.rm = TRUE))


# crms0173----
crms0173 = read.csv("CRMS0173.csv")
crms0173_flood = crms0173 %>% 
  mutate(datetime = as.POSIXct(datetime, format = "%m/%d/%Y %H:%M"), 
         level = Adjusted.Water.Elevation.to.Datum..ft. * 0.3048) %>% 
  filter(level != "NA") %>% 
  mutate(flood_neg0.3 = ifelse(level > -0.3, 1, 0), 
         inundation_neg0.3 = sum(flood_neg0.3) / n(), 
         flood_neg0.2 = ifelse(level > -0.2, 1, 0), 
         inundation_neg0.2 = sum(flood_neg0.2) / n(), 
         flood_neg0.1 = ifelse(level > -0.1, 1, 0), 
         inundation_neg0.1 = sum(flood_neg0.1) / n(), 
         flood_0.0 = ifelse(level > 0.0, 1, 0), 
         inundation_0.0 = sum(flood_0.0) / n(), 
         flood_0.1 = ifelse(level > 0.1, 1, 0), 
         inundation_0.1 = sum(flood_0.1) / n(), 
         flood_0.2 = ifelse(level > 0.2, 1, 0), 
         inundation_0.2 = sum(flood_0.2) / n(), 
         flood_0.3 = ifelse(level > 0.3, 1, 0), 
         inundation_0.3 = sum(flood_0.3) / n(), 
         flood_0.4 = ifelse(level > 0.4, 1, 0), 
         inundation_0.4 = sum(flood_0.4) / n(), 
         flood_0.5 = ifelse(level > 0.5, 1, 0), 
         inundation_0.5 = sum(flood_0.5) / n())

crms0173_curve = data.frame(elevation = seq(-0.3, 0.5, 0.1), 
                            inundation = c(crms0173_flood[1,15], crms0173_flood[1,17], crms0173_flood[1,19], 
                                           crms0173_flood[1,21], crms0173_flood[1,23], crms0173_flood[1,25], 
                                           crms0173_flood[1,27], crms0173_flood[1,29], crms0173_flood[1,31]))

crms0173_model = lm(data = crms0173_curve, inundation ~ poly(elevation, 3, raw = TRUE))
summary(crms0173_model)

crms0173_fxn = function(x) {
  -1.865296 * x -1.495352 * x^2 + 5.046064 * x^3 + 0.689928
}

ggplot(data = crms0173_curve, aes(x = elevation, y = inundation)) + geom_point() + stat_function(fun = crms0173_fxn)

crms0173_summary = crms0173 %>% 
  summarize(temp = mean(Water.Temperature...C., na.rm = TRUE), 
            salinity = mean(Salinity..ppt., na.rm = TRUE), 
            level = mean(Adjusted.Water.Elevation.to.Datum..ft. * 0.3048, na.rm = TRUE))


# crms0178----
crms0178 = read.csv("CRMS0178.csv")
crms0178_flood = crms0178 %>% 
  mutate(datetime = as.POSIXct(datetime, format = "%m/%d/%Y %H:%M"), 
         level = Adjusted.Water.Elevation.to.Datum..ft. * 0.3048) %>% 
  filter(level != "NA") %>% 
  mutate(flood_neg0.1 = ifelse(level > -0.1, 1, 0), 
         inundation_neg0.1 = sum(flood_neg0.1) / n(), 
         flood_0.0 = ifelse(level > 0.0, 1, 0), 
         inundation_0.0 = sum(flood_0.0) / n(), 
         flood_0.1 = ifelse(level > 0.1, 1, 0), 
         inundation_0.1 = sum(flood_0.1) / n(), 
         flood_0.2 = ifelse(level > 0.2, 1, 0), 
         inundation_0.2 = sum(flood_0.2) / n(), 
         flood_0.3 = ifelse(level > 0.3, 1, 0), 
         inundation_0.3 = sum(flood_0.3) / n(), 
         flood_0.4 = ifelse(level > 0.4, 1, 0), 
         inundation_0.4 = sum(flood_0.4) / n(), 
         flood_0.5 = ifelse(level > 0.5, 1, 0), 
         inundation_0.5 = sum(flood_0.5) / n())

crms0178_curve = data.frame(elevation = seq(-0.1, 0.5, 0.1), 
                            inundation = c(crms0178_flood[1,15], crms0178_flood[1,17], crms0178_flood[1,19], 
                                           crms0178_flood[1,21], crms0178_flood[1,23], crms0178_flood[1,25], 
                                           crms0178_flood[1,27]))

crms0178_model = lm(data = crms0178_curve, inundation ~ poly(elevation, 3, raw = TRUE))
summary(crms0178_model)

crms0178_fxn = function(x) {
  -1.452338 * x -4.392829 * x^2 + 7.957610 * x^3 + 0.861137
}

ggplot(data = crms0178_curve, aes(x = elevation, y = inundation)) + geom_point() + stat_function(fun = crms0178_fxn)

crms0178_summary = crms0178 %>% 
  summarize(temp = mean(Water.Temperature...C., na.rm = TRUE), 
            salinity = mean(Salinity..ppt., na.rm = TRUE), 
            level = mean(Adjusted.Water.Elevation.to.Datum..ft. * 0.3048, na.rm = TRUE))


# crms0336----
crms0336 = read.csv("CRMS0336.csv")
crms0336_flood = crms0336 %>% 
  mutate(datetime = as.POSIXct(datetime, format = "%m/%d/%Y %H:%M"), 
         level = Adjusted.Water.Elevation.to.Datum..ft. * 0.3048) %>% 
  filter(level != "NA") %>% 
  mutate(flood_neg0.2 = ifelse(level > -0.2, 1, 0), 
         inundation_neg0.2 = sum(flood_neg0.2) / n(), 
         flood_neg0.1 = ifelse(level > -0.1, 1, 0), 
         inundation_neg0.1 = sum(flood_neg0.1) / n(), 
         flood_0.0 = ifelse(level > 0.0, 1, 0), 
         inundation_0.0 = sum(flood_0.0) / n(), 
         flood_0.1 = ifelse(level > 0.1, 1, 0), 
         inundation_0.1 = sum(flood_0.1) / n(), 
         flood_0.2 = ifelse(level > 0.2, 1, 0), 
         inundation_0.2 = sum(flood_0.2) / n(), 
         flood_0.3 = ifelse(level > 0.3, 1, 0), 
         inundation_0.3 = sum(flood_0.3) / n(), 
         flood_0.4 = ifelse(level > 0.4, 1, 0), 
         inundation_0.4 = sum(flood_0.4) / n(), 
         flood_0.5 = ifelse(level > 0.5, 1, 0), 
         inundation_0.5 = sum(flood_0.5) / n())

crms0336_curve = data.frame(elevation = seq(-0.2, 0.5, 0.1), 
                            inundation = c(crms0336_flood[1,15], crms0336_flood[1,17], crms0336_flood[1,19], 
                                           crms0336_flood[1,21], crms0336_flood[1,23], crms0336_flood[1,25], 
                                           crms0336_flood[1,27], crms0336_flood[1,29]))

crms0336_model = lm(data = crms0336_curve, inundation ~ poly(elevation, 3, raw = TRUE))
summary(crms0336_model)

crms0336_fxn = function(x) {
  -1.707808 * x -1.982242 * x^2 + 5.265367 * x^3 + 0.715839
}

ggplot(data = crms0336_curve, aes(x = elevation, y = inundation)) + geom_point() + stat_function(fun = crms0336_fxn)

crms0336_summary = crms0336 %>% 
  summarize(temp = mean(Water.Temperature...C., na.rm = TRUE), 
            salinity = mean(Salinity..ppt., na.rm = TRUE), 
            level = mean(Adjusted.Water.Elevation.to.Datum..ft. * 0.3048, na.rm = TRUE))


# crms0374----
crms0374 = read.csv("CRMS0374.csv")
crms0374_flood = crms0374 %>% 
  mutate(datetime = as.POSIXct(datetime, format = "%m/%d/%Y %H:%M"), 
         level = Adjusted.Water.Elevation.to.Datum..ft. * 0.3048) %>% 
  filter(level != "NA") %>% 
  mutate(flood_neg0.2 = ifelse(level > -0.2, 1, 0), 
         inundation_neg0.2 = sum(flood_neg0.2) / n(), 
         flood_neg0.1 = ifelse(level > -0.1, 1, 0), 
         inundation_neg0.1 = sum(flood_neg0.1) / n(), 
         flood_0.0 = ifelse(level > 0.0, 1, 0), 
         inundation_0.0 = sum(flood_0.0) / n(), 
         flood_0.1 = ifelse(level > 0.1, 1, 0), 
         inundation_0.1 = sum(flood_0.1) / n(), 
         flood_0.2 = ifelse(level > 0.2, 1, 0), 
         inundation_0.2 = sum(flood_0.2) / n(), 
         flood_0.3 = ifelse(level > 0.3, 1, 0), 
         inundation_0.3 = sum(flood_0.3) / n(), 
         flood_0.4 = ifelse(level > 0.4, 1, 0), 
         inundation_0.4 = sum(flood_0.4) / n(), 
         flood_0.5 = ifelse(level > 0.5, 1, 0), 
         inundation_0.5 = sum(flood_0.5) / n())

crms0374_curve = data.frame(elevation = seq(-0.2, 0.5, 0.1), 
                            inundation = c(crms0374_flood[1,15], crms0374_flood[1,17], crms0374_flood[1,19], 
                                           crms0374_flood[1,21], crms0374_flood[1,23], crms0374_flood[1,25], 
                                           crms0374_flood[1,27], crms0374_flood[1,29]))

crms0374_model = lm(data = crms0374_curve, inundation ~ poly(elevation, 3, raw = TRUE))
summary(crms0374_model)

crms0374_fxn = function(x) {
  -1.832523 * x -1.834945 * x^2 + 5.463156 * x^3 + 0.706756
}

ggplot(data = crms0374_curve, aes(x = elevation, y = inundation)) + geom_point() + stat_function(fun = crms0374_fxn)

crms0374_summary = crms0374 %>% 
  summarize(temp = mean(Water.Temperature...C., na.rm = TRUE), 
            salinity = mean(Salinity..ppt., na.rm = TRUE), 
            level = mean(Adjusted.Water.Elevation.to.Datum..ft. * 0.3048, na.rm = TRUE))

