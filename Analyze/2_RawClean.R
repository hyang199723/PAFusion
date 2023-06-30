rm(list = ls())
setwd("/Users/hongjianyang/Research/PAStudy/PA/Data/California2021/RData/")
load("PASMoke_new.RData")
load("AQSSMoke_new.RData")
library(geodist)
# load("HMS2021.RData")
library(dplyr)
library(tidyr)

aqs = df2
pa = df1

# Process AQS data
aqs$LS = aqs$LS[, 1]
aqs$PM2.5 = ifelse(aqs$PM2.5 < 0.5, 0.5, aqs$PM2.5)
aqs_day <- aqs %>% 
  group_by(index, Date) %>%
  summarise(lat = mean(lat), lon = mean(lon), count = n(), 
            PM2.5 = mean(PM2.5), LS = max(LS), MS = max(MS), HS = max(HS))

len1 <- dim(aqs_day)[1]
# Keep daily reading with at least 23 readings
aqs_day$PM2.5 <- ifelse(aqs_day$count < 23, NA, aqs_day$PM2.5)

aqs_day$LS <- ifelse((aqs_day$LS & aqs_day$MS), FALSE, aqs_day$LS)
aqs_day$MS <- ifelse((aqs_day$MS & aqs_day$HS), FALSE, aqs_day$MS)

pa$LS <- pa$LS[, 1]
# Process Purple Air data
# PA is five hours behind AQS data
# five hours = 18000 seconds
lag = 18000
pa$date_time = pa$date_time + lag
day <- as.Date(pa$date_time, format = "%Y-%m-%d")
pa$day <- day
pa$PM2.5 = (pa$PM2.5a + pa$PM2.5b) / 2
pa_day <- pa %>%
  group_by(index, day) %>%
  summarise(humidity = mean(humidity), temperature = mean(temperature), count = n(),
            PM2.5a = mean(PM2.5a), PM2.5b = mean(PM2.5b), lat = max(lat), lon = max(lon),
            LS = max(LS), MS = max(MS), HS = max(HS), PM2.5 = mean(PM2.5))
# Remove PM2.5 reading when PM2.5a and  PM2.5b diff is huge
pa_day$PM2.5 <- ifelse(abs(pa_day$PM2.5a - pa_day$PM2.5b) >= 200, NA, pa_day$PM2.5)
# Keep daily reading with at least 23 readings

pa_day$PM2.5 <- ifelse(pa_day$count < 23, NA, pa_day$PM2.5)
pa_day$LS <- ifelse((pa_day$LS & pa_day$MS), FALSE, pa_day$LS)
pa_day$MS <- ifelse((pa_day$MS & pa_day$HS), FALSE, pa_day$MS)

pa_day$LS <- as.logical(pa_day$LS); pa_day$MS <- as.logical(pa_day$MS); pa_day$HS <- as.logical(pa_day$HS)
aqs_day$LS <- as.logical(aqs_day$LS); aqs_day$MS <- as.logical(aqs_day$MS); aqs_day$HS <- as.logical(aqs_day$HS)

pa_day <- subset(pa_day, select = -count)
aqs_day <- subset(aqs_day, select = -count)


# a = geodist(coords, measure = 'geodesic' )/1000

# Remove sites that are at the same location
# Pa close sites
c_aqs <- unique(cbind(aqs_day$index, aqs_day$lon, aqs_day$lat))
c_pa <- unique(cbind(pa_day$index, pa_day$lon, pa_day$lat))
n1 = dim(c_aqs)[1]; n2 = dim(c_pa)[1]
c <- rbind(c_aqs, c_pa)
coords <- c[, 2:3]
d <- as.matrix(dist(coords)); diag(d) = 1
d[d == 0] = NA
d12 = d[, (n1+1):(n1+n2)]
a = is.na(colSums(d12))
bad_idx = c_pa[a, 1]
pa_day <- subset(pa_day, !index %in% bad_idx)
pa <- subset(pa, !index %in% bad_idx)

# Order data
aqs_day <- aqs_day[order(aqs_day$index, aqs_day$Date),]
pa_day <- pa_day[order(pa_day$index, pa_day$day),] 

save(pa_day, file = "PASmoke_day.RData")
save(aqs_day, file = "AQSSmoke_day.RData")