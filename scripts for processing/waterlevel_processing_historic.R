library(wlTools)
library(dplyr)
remove(list=ls())

# read in and process baro data within the site folder of interest
# replace csv filename and folder path as necessary
baro = standardize.baro("../1 Site data/Petaluma/1 Raw data/Baro/PetBaro_2018.09.13_2018.12.17.csv")

#if doing a batch, can load several baros and then combine as below
#baro2<- standardize.baro("./Petaluma/1 Raw data/Baro/PetBaro_2018.12.17_2019.04.10.csv")
#baro3<- standardize.baro("./Petaluma/1 Raw data/Baro/PetBaro_2018.12.17_2019.04.10.csv")
#baro<- rbind(baro1, baro2, baro3, baro4, baro5, baro6, baro7, baro8)

# Read in and process water level - remember to call ".solinst" or ".hobo" 
# replace csv filename and folder path as necessary
level = standardize.hobo("../1 Site data/Petaluma/1 Raw data/Water level/Pet01_2018.09.13_2018.12.17.csv")

#if doing a batch, can load several loggers and then combine as below
#level2 <- standardize.hobo("./Petaluma/1 Raw data/Water level/Pet01_2017.10.05_2018.01.08.csv")
#level3 <- standardize.hobo("./Petaluma/1 Raw data/Water level/Pet01_2018.01.08_2018.04.04.csv")
#level <- rbind(level1, level2, level3, level4, level5)

# Convert total pressure to water level above sensor (water level, baro, Local Water Density)
# Pull in water density look-up table
water.accessory = read.csv("water.accessory.csv")
# Input density into tp.to.wlas function manually View(water.density) or use code below
final_level = tp.to.wlas(level, baro, 
                         water.accessory$water.density[water.accessory$site=="Petaluma"])

# Add final water level NAVD88 column by adding sensor elevation in NAVD88 to water level above sensor
# For historic deployments and processing, look in underlying data folder
# Add sensor navd88 manually View(sensor.navd88) or use code below
final_level$water.level.NAVD88 = final_level$water.level.above.sensor + 
  water.accessory$sensor.navd88[water.accessory$site=="Petaluma"]

# Export csv to the processed folder. Update file name and path as necessary
write.csv(final_level, "../1 Site data/Petaluma/2 Processed data/Pet01_2018.09.13_2018.12.17.csv")