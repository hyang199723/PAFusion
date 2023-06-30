# Impute missing temperature and humidity, get data into desired format
# This script takes two datasets, AQSSmoke_day and PASmoke_day
# Which are daily level PM2.5 data
rm(list = ls())
library(dplyr)
library(tidyr)
library(lubridate)
setwd("/Users/hongjianyang/Research/California2021/RData/")
load("AQSSmoke_day.RData")
load("PASmoke_day.RData")

aqs = aqs_day
pa = pa_day
# When index <= 110: type 1 data
# Otherwise type 2 data
# 2021-6-1: 18779
# 2021-7-1: 18809
# 2021-7-31: 18839
# 2021-8-1: 18840
# 2021-9-30: 18900
# Select data
start = 18809; end = 18931
subaqs <- subset(aqs, Date >= start & Date <= end)
subpa <- subset(pa, day >= start & day <= end)
subaqs$temperature <- NA
subaqs$humidity <- NA

# Look at fire percentage by month
aqs$month <- month(aqs$Date)
pa$month <- month(pa$day)
aqs_month <- aqs %>% group_by(month) %>% summarise(cLS = sum(LS),
                                                   cMS = sum(MS),
                                                   cHS = sum(HS),
                                                   pm25 = mean(PM2.5, na.rm = T))

pa_month <- pa %>% group_by(month) %>% summarise(cLS = sum(LS),
                                                 cMS = sum(MS),
                                                 cHS = sum(HS),
                                                 pm25 = mean(PM2.5, na.rm = T))

sub <- data.frame(index = c(subaqs$index, subpa$index), 
                  date = c(subaqs$Date, subpa$day),
                  lon = c(subaqs$lon, subpa$lon),
                  lat = c(subaqs$lat, subpa$lat),
                  pm = c(subaqs$PM2.5, subpa$PM2.5),
                  LS = c(subaqs$LS, subpa$LS),
                  MS = c(subaqs$MS, subpa$MS),
                  HS = c(subaqs$HS, subpa$HS),
                  tmp = c(subaqs$temperature, subpa$temperature),
                  humidity = c(subaqs$humidity, subpa$humidity))
X_LS <- as.data.frame(sub %>% pivot_wider(id_cols = index, 
                                          names_from = date, values_from = LS))
X_MS <- as.data.frame(sub %>% pivot_wider(id_cols = index, 
                                          names_from = date, values_from = MS))
X_HS <- as.data.frame(sub %>% pivot_wider(id_cols = index, 
                                          names_from = date, values_from = HS))
X_LS[is.na(X_LS)] <- FALSE
X_MS[is.na(X_MS)] <- FALSE
X_HS[is.na(X_HS)] <- FALSE
LS <- X_LS[, -1]; MS <- X_MS[, -1]; HS <- X_HS[, -1]
LSs <- rowSums(LS); MSs <- rowSums(MS); HSs <- rowSums(HS)
count = LSs + MSs + HSs

# PM2.5 (Y)
Y <- sub %>% pivot_wider(id_cols = index, 
                         names_from = date, values_from = pm)
Y <- as.data.frame(Y)
# Y = Y[count >= 30, ] # Remove this one
Ymiss = Y[rowSums(is.na(Y)) <= 18, ]
# Remove super high Pm2.5 readings
for (i in 2:dim(Ymiss)[2]) {Ymiss[,i][Ymiss[,i] >= 1000] <- NA}
idx <- Ymiss$index
# idx <- idx[1:1200]
# Book keeping
n1 = sum(idx <= 110)
n2 = sum(idx >= 110)
ns = n1 + n2
nt = dim(Ymiss)[2] - 1
# Distance matrix
sub <- subset(sub, index %in% idx)
coords <- unique(cbind(sub$index, sub$lon, sub$lat))
coords <- coords[, 2:3]

#library(raster)
#rdist.earth
#fields

d <- as.matrix(dist(coords))
diag(d) = 1

X_temp <- as.data.frame(sub %>% pivot_wider(id_cols = index, 
                                            names_from = date, values_from = tmp))
X_hum <- as.data.frame(sub %>% pivot_wider(id_cols = index, 
                                           names_from = date, values_from = humidity))
X_LS <- as.data.frame(sub %>% pivot_wider(id_cols = index, 
                                          names_from = date, values_from = LS))
X_MS <- as.data.frame(sub %>% pivot_wider(id_cols = index, 
                                          names_from = date, values_from = MS))
X_HS <- as.data.frame(sub %>% pivot_wider(id_cols = index, 
                                          names_from = date, values_from = HS))
# Complete smoke data
X_LS[is.na(X_LS)] <- FALSE
X_MS[is.na(X_MS)] <- FALSE
X_HS[is.na(X_HS)] <- FALSE
X_temp[X_temp <= -20]  = NA
boxplot(X_temp[, -1], na.rm = T, 
        main = "Temperature boxplot by day, before imputation")
boxplot(X_hum[, -1], na.rm = T, 
        main = "Humidity boxplot by day, before imputation")
# Complete temperature and humidity data
k = 10
for (i in 1:nt) {
  cur_tmp <- X_temp[, i+1]
  for (j in 1:ns) {
    if(is.na(cur_tmp[j])) {
      distance = sort(d[j, ])
      close_idx = as.integer(names(distance[1:k]))
      X_temp[j, i+1] = mean(cur_tmp[close_idx], na.rm = T)
    }
  }
}

# Complete temperature and humidity data
k = 10
for (i in 1:nt) {
  cur_hum <- X_hum[, i+1]
  for (j in 1:ns) {
    if(is.na(cur_hum[j])) {
      distance = sort(d[j, ])
      close_idx = as.integer(names(distance[1:k]))
      X_hum[j, i+1] = mean(cur_hum[close_idx], na.rm = T)
    }
  }
}

boxplot(X_temp[, -1], 
        main = "Temperature boxplot by day, after imputation")
boxplot(X_hum[, -1], 
        main = "Humidity boxplot by day, after imputation")

# Create covariates
p = 6
X=array(NA,dim=c(n1+n2, nt, p))
X[,,1]=matrix(rep(1,ns*nt),ncol = nt)
X[,,2]=as.matrix(X_temp[, -1]) # Temperature
X[,,3]=as.matrix(X_hum[, -1]) # Humidity
X[,,4]=as.matrix(X_LS[, -1])  # LS
X[,,5]=as.matrix(X_MS[, -1]) # MS
X[,,6]=as.matrix(X_HS[, -1])  # HS
# Save un-normalized version
#save(X, file = "X_original.RData")
# Normalize temperature and humidty
X[,,2] = as.matrix(scale(X[,,2]))
X[,,3] = as.matrix(scale(X[,,3]))

Y_final <- Ymiss[, -1]
Y1 <- as.matrix(Y_final[1:n1, ])
Y2 <- as.matrix(Y_final[(n1+1):(n1+n2), ])
s1 <- as.matrix(coords[1:n1, ])
s2 <- as.matrix(coords[(n1+1):(n1+n2), ])

inspectRaw = F
if (inspectRaw) {
  Y1_star <- matrix(0, nrow = n1, ncol = nt)
  Y2_star <- matrix(0, nrow = n2, ncol = nt)
  m1 <- is.na(Y1)
  m2 <- is.na(Y2)
  Y1[m1] <- mean(Y1, na.rm = T)
  Y2[m2] <- mean(Y2, na.rm = T)
  for(j in 1:n1){Y1_star[j,] <- fft_real(as.numeric(Y1[j,]))}
  for(j in 1:n2){Y2_star[j,] <- fft_real(as.numeric(Y2[j,]))}
}
boxplot(Y1, na.rm = T)
boxplot(Y2, na.rm = T)

setwd("/Users/hongjianyang/Research/PAStudy/PA/Data/California2021/RData/")
save(Y1, file = "Y1_new.RData")
save(Y2, file = "Y2_new.RData")
save(X, file = "X_new.RData")
save(s1, file = "s1_new.RData")
save(s2, file = "s2_new.RData")

