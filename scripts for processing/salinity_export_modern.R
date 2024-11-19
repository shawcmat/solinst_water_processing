# read in working processed file
data_working = readRDS(working.path)

# save the final qaqced file
saveRDS(data_working, qaqc.path)