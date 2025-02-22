

processed_data <- read.csv("C:/Users/mshawcroft/water_processing data test/processed data/BigBreak_2022.02.23_2022.09.28_processed.csv")
qaqc_data <- read.csv("C:/Users/mshawcroft/water_processing data test/QAQC data/individual/BigBreak_2022.02.23_2022.09.28_QAQC.csv")

hampel_test <- function(data, var_name, k, t0){

    
    hampel_dat <- hampel(data[[var_name]], k = k, t0 = t0)
    data[["hampel"]] <- hampel_dat$y

    num_changed <- sum(data[["hampel"]] - data[[var_name]] != 0)


    p1 <- ggplot(data = data, aes(x = time, y = .data[[var_name]])) + geom_line() + ggtitle("raw data")
    p2 <- ggplot(data = data, aes(x = time, y = hampel)) + geom_line() + ggtitle(paste0("hampel filter, ", num_changed, " changed"), subtitle = k)
    
    
    p3 <- ggplot(data = data, aes(x = time)) +
     geom_line(aes(y = .data[[var_name]], color = 'raw data')) +
      geom_line(aes(y = hampel, color = "hampel"))


    return(list("p1" = p1, "p2" = p2, "p3" = p3))

}

zscore_test <- function(data, var_name, threshold) {
    data$time <- as.POSIXct(data$time, format="%Y-%m-%d %H:%M:%S")
    z_scores <- scale(data[[var_name]], center = TRUE, scale = TRUE)
    data[["zscore"]] <- z_scores
    data_mean <- mean(data[[var_name]])
    num_changed <- sum(data[["zscore"]] > threshold | data[["zscore"]] < (0-threshold))

    outliers <- data$zscore > threshold
    data <- data %>% mutate(zfiltered = ifelse(zscore > threshold, NA, data[[var_name]]))
    
    p1 <- ggplot(data = data, aes(x = time, y = .data[[var_name]])) + geom_line() + ggtitle("raw data")
    p2 <- ggplot(data = data, aes(x = time, y = zfiltered)) + geom_line() + ggtitle(paste0("z filter, ", num_changed, " changed"), subtitle = threshold)
    p3 <- ggplot(data = data, aes(x = time)) +
     geom_line(aes(y = .data[[var_name]], color = 'raw data')) +
      geom_line(aes(y = zfiltered, color = "zfiltered"))
    
    p4 <- ggplot(data = data, aes(x = time, y = zscore)) + geom_line() + ggtitle("zscore")

    return(list("p1" = p1, "p2" = p2, "p3" = p3, "p4" = p4))

  }


data <- processed_data
mod_zscore_test <- function(data, var_name, threshold) {
    data$time <- as.POSIXct(data$time, format="%Y-%m-%d %H:%M:%S")
    x <- data[[var_name]]
    x_med <- median(x, na.rm = T)
    MAD <- median(abs(x - x_med))
    mod_zscore <- 0.6745 * (x - x_med) / MAD
   
    data[["mod_zscore"]] <- mod_zscore

    num_changed <- sum(data[["mod_zscore"]] > threshold | data[["mod_zscore"]] < (0 - threshold))

    data_mean <- mean(x, na.rm = TRUE)
    data <- data %>% mutate(zfiltered = ifelse(mod_zscore > threshold | mod_zscore < (0-threshold), NA, data[[var_name]]))
    
    p1 <- ggplot(data = data, aes(x = time, y = .data[[var_name]])) + geom_line() + ggtitle("raw data")
    p2 <- ggplot(data = data, aes(x = time, y = zfiltered)) + geom_line() + ggtitle(paste0("mod z filter, ", num_changed, " changed"), subtitle = threshold)
    p3 <- ggplot(data = data, aes(x = time)) +
     geom_line(aes(y = .data[[var_name]], color = 'raw data')) +
      geom_line(aes(y = zfiltered, color = "zfiltered"))
    
    p4 <- ggplot(data = data, aes(x = time)) +
     geom_line(aes(y = .data[[var_name]], color = 'raw data')) +
      geom_line(aes(y = mod_zscore, color = "mod zscore"))

    p5 <- ggplot(data = data, aes(x = time, y = mod_zscore)) + geom_line() + ggtitle("mod_zscore")

    return(list("p1" = p1, "p2" = p2, "p3" = p3, "p4" = p4))

}


head(data.frame(raw = data$water_level_NAVD88, zscore = data$mod_zscore))


hout  <- hampel_test(level_working,     "water_level_NAVD88", k = 1001, t0 = 12)
zout  <- zscore_test(level_working,     "water_level_NAVD88", threshold = 3)
mzout <- mod_zscore_test(level_working, "water_level_NAVD88", threshold = 3)

hout$p1
hout$p2
hout$p3

zout$p1
zout$p2
zout$p3
zout$p4

mzout$p1
mzout$p2
mzout$p3
mzout$p4


# Trim the data
start <- as.Date("2022-07-01")
end <- as.Date("2022-08-01")
level_working_trim <- level_working %>% filter(time >= start & time <= end)

zout_jul <- zscore_test(level_working_trim, "water_level_NAVD88", threshold = 3)

zout_jul$p1
zout_jul$p2
zout_jul$p4



# Double plot
variable_name <- "water_level_NAVD88"

dat1 <- processed_data
dat2 <- qaqc_data[,c("time", variable_name)]

temp_name <- paste0(variable_name, "2")
names(dat2) <- c("time", temp_name)

dat <- left_join(dat1, dat2, by = join_by(time == time), multiple = "first")
dat$ymd <- as.Date(dat$time)

dat$time <- 

p3 <- ggplot(data = dat, aes(x = time)) +
 geom_line(aes(y = .data[[variable_name]], color = "pre-autoQAQC")) +
 geom_line(aes(y = .data[[temp_name]], color = "post-autoQAQC"))
