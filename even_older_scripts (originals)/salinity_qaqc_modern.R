# Combine newly processed data with historic QAQCed data
salinity_processed = readRDS(salinity.processed.path)
salinity_longterm = readRDS(salinity.QAQC.path)
combined_sal = bind_rows(salinity_longterm, salinity_processed)

# Export updated long-term dataset
saveRDS(combined_sal, salinity.working.path)

# load working file for visualization
salinity_working = readRDS(salinity.working.path)