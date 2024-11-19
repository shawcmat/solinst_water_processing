#percent inundation from QAQC database
pet.longterm = pet.longterm %>% 
  mutate(is.flooded = ifelse(water.level.NAVD88 > 1.5, 1, 0))

pet.monthly.inundation = pet.longterm %>% 
  group_by(month = floor_date(time, "month")) %>% 
  summarize(flood.intervals = sum(is.flooded, na.rm = T), total.intervals = n()) %>% 
  mutate(inundation = flood.intervals / total.intervals)


# example for Seal Beach of weekly, etc. summaries
ggplot(data=seal_deep_monthly, aes(x=month, y=water.temp)) + 
  geom_line() + geom_point() + 
#  geom_errorbar(aes(ymin = water.temp - 1.96*temp.sd, ymax = water.temp + 1.96*temp.sd), width = 0) +
  theme_bw() + theme(text = element_text(size = 14)) + 
  labs(x="Time", y="Monthly water temperature (C)")


# merge for comparisons
seal_all = merge(seal_eel, seal_deep, by = "time")

seal_all$airdiff = seal_all$air.temp.x - seal_all$air.temp.x
seal_all$tempdiff = seal_all$water.temp.x - seal_all$water.temp.y
seal_all$leveldiff = seal_all$water.level.NAVD88.x - seal_all$water.level.NAVD88.y

ggplot(data=seal_all, aes(x=time, y=leveldiff)) + 
  geom_line() + 
  theme_bw() + theme(text = element_text(size = 14)) + 
  labs(x="Time", y="Water temperature Eel - Deep (C)")



