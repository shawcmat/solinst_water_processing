library(tools)

setwd("N:/Davis/Thorne/Data Files/3 Projects/8 Water Quality/1 Site data")

sites <- c("Dutch Slough",
           "Lower Joice Island",
           "Morro Bay SLR")
first <- TRUE
for(site_name in sites){
    print(site_name)
    raw_path <- file.path(site_name, "1 Raw data")
    full_list <- list.files(raw_path, pattern = ".csv$")
    converted <- list.files(raw_path, pattern = "mS.csv")
    originals <- setdiff(full_list, converted)
    logger_names <- sapply(originals, FUN = function(x){strsplit(file_path_sans_ext(x), split = "_")[[1]][1]})
    date_start <- sapply(originals, FUN = function(x){strsplit(file_path_sans_ext(x), split = "_")[[1]][2]})
    date_end <- sapply(originals, FUN = function(x){strsplit(file_path_sans_ext(x), split = "_")[[1]][3]})

    results <- data.frame(site_name = site_name,
                          logger_name = logger_names,
                          date_start = date_start,
                          date_end = date_end,
                          file_name = originals,
                          row.names = NULL)
    if(first){
        full_results <- results
        first <- FALSE
    }else{
        full_results <- rbind(full_results, results)
    }

}  

write.csv(full_results, "Inventory and Tracking.csv")
