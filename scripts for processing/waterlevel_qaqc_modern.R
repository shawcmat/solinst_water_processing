# Combine newly processed data with historic QAQCed data
level_processed = readRDS(level.processed.path)
level_longterm = readRDS(level.QAQC.path)
combined_level = bind_rows(level_longterm, level_processed)

# Export updated long-term dataset
saveRDS(combined_level, level.working.path)

# load working file for visualization
level_working = readRDS(level.working.path)