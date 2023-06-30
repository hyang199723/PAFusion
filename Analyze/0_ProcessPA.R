# Process Raw Purple Air Data
# The raw data comes distributed by month.
# This script combines all month and produce a .RData
rm(list = ls())
setwd("/Users/hongjianyang/Desktop/California2021/")
month1 = read.csv("Raw_Data/jul_aug_2021.csv")
month2 = read.csv("Raw_Data/sep_oct_2021.csv")
month3 = read.csv("Raw_Data/oct_2021_cleaned.csv")
cols = c("sensor_index", "humidity_a", "temperature_a", "pm2.5_atm_a", 
         "pm2.5_atm_b", "date_time_utc")
month1 = month1[, cols]; month2 = month2[, cols]
month3 = month3[, cols]
df = rbind(month1, month2, month3)
colnames(df) = c("index", "humidity", "temp", "pm2.5_a", "pm2.5_b", 
                 "time")
location = read.csv("Data/locations.csv")
location = location[, -1]
df = merge(x = df, y = location, by = 'index')
colnames(df) = c("index", "humidity", "temp", "pm2.5_a", "pm2.5_b", 
                 "time", "lat", "lon")
# Complete PA reading with location
save(df, file = "Data/PA2021.RData")
