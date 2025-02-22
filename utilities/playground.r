

date1 <- "2/24/2022 00:00:00"
date2 <- "2/24/2022 23:54:00"

posix_date1 <- as.POSIXct(date1, format = "%m/%d/%Y %H:%M:%S", tz = "Etc/GMT+8")
posix_date2 <- as.POSIXct(date2, format = "%m/%d/%Y %H:%M:%S", tz = "Etc/GMT+8")

posix_date1 <- format(posix_date1, "%Y-%m-%d %H:%M:%S")
posix_date2 <- format(posix_date2, "%Y-%m-%d %H:%M:%S")

posix_date1
posix_date2

date1 <- "2022-03-13 01:54:00"
date2 <- "2022-03-13 12:48:00"
as.POSIXct(date1, format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT+8")
as.POSIXct(date2, format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT+8")
