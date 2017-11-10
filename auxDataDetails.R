## auxiliary data details

# time span
# observations
# missing days
# mean accuracy
# sd accuracy

# variables constructed in the playing around with thicken.R file

datadetails<- data.frame(span = paste0("From ", min(mHour$time_hour_day), " to ", max(mHour$time_hour_day)),
           observations = sum(mHour$measurements),
           days_missing = nrow(mHour[which(mHour$hoursMissing == 24),]),
           mean_accuracy = mean(all$accuracy),
           sd_accuracy   = sd(all$accuracy)
           )

saveRDS(datadetails, "datadetails.rds")
