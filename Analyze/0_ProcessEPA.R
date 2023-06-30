rm(list = ls())
library(dplyr)
setwd("/Users/hongjianyang/Desktop/California2021/")
data = read.csv("Raw_Data/hourly_88101_2021.csv")
# ca <- data %>% filter()
ca = data[data['State.Name'] == "California", ]
cols = c("Latitude", "Longitude", 
         "Date.GMT", "Time.GMT", "Sample.Measurement")
ca <- ca[, cols]
colnames(ca) = c("lat", "lon", "GMTDate", 
                 "GMTTime", "PM2.5")
aqs1 = ca
# Get 07/01/2021 - 10/31/2021
aqs1 = subset(aqs1, (GMTDate >= "2021-07-01") & (GMTDate <= "2021-10-31"))
# Add index to EPA data
idx = unique(aqs1[, c("lat", "lon")])
n1 = dim(idx)[1]
idx = cbind(1:n1, idx)
aqs1 = merge(x = aqs1, y = idx, by = c("lat", "lon"), all.x = T)
colnames(aqs1) = c("lat", "lon", "GMTDate", 
                 "GMTTime", "PM2.5", "index")
save(aqs1, file = "Data/AQS2021.RData")
