rm(list = ls())
library(usmap)
library(ggplot2)
library(sp)
library(maps)
library(tidyverse)
library(rgeos)
library(sf)
library(lubridate)
setwd("/Users/hongjianyang/Desktop/California2021/")
# Load fire and smoke data
load("Data/HMS2021.RData")
smoke_heavy[[366]] = list()
load("Data/PA2021.RData")
loc = subset(df, select = c("index", "lat", "lon"))
loc = unique(loc)
load("Data/AQS2021.RData")
aqs = as.data.frame(aqs1)
loc1 = subset(aqs, select = c("index", "lat", "lon"))
loc1 = unique(loc1)

loc = rbind(loc, loc1)
rownames(loc) = NULL
N = dim(loc)[1]
days = 1:365
rownames(loc) = NULL
loc = loc[order(loc$index),] 
loc = cbind(rep(days, each = N), loc); rownames(loc) = NULL
colnames(loc) = c("day", "index", "lat", "lon")
LS <- MS <- HS <- rep(0, N * 365)
# SMOKE DATA
sf::sf_use_s2(FALSE)
# Test:
Ntot = 365
for (i in 1:Ntot)
{
  cur_loc = subset(loc, day == i)
  idx = rownames(cur_loc)
  LS <- MS <- HS <- 0
  p <- data.frame(lon = cur_loc$lon, lat = cur_loc$lat)
  p_sf <- p %>% rowid_to_column() %>% st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
  if(!is.null(smoke_light[[i]]))
  {
    sl=st_sf(smoke_light[[i]])
    LS=st_intersects(p_sf, sl, sparse = FALSE)
  } else {
    LS = FALSE
  }
  loc[idx, 'LS'] = LS
  
  if(!is.null(smoke_medium[[i]]))
  {
    sm=st_sf(smoke_medium[[i]])
    MS = st_intersects(p_sf, sm,sparse = FALSE)  
  } else{
    MS = FALSE
  }
  loc[idx, 'MS'] = MS
  
  if(!is.null(smoke_heavy[[i]]))
  {
    sh=st_sf(smoke_heavy[[i]])
    HS = st_intersects(p_sf, sh,sparse = FALSE)
  } else {
    HS = FALSE
  }
  loc[idx, 'HS'] = HS
}
temp = loc
temp = replace(temp, is.na(temp), FALSE)
og = as.Date("2020-12-31", tz = "UTC")
temp$day = as.Date(temp$day, origin = og)
temp = temp[, c("day", "index", "LS", "MS", "HS")]
# Process PurpleAir data
# PA is five hours behind AQS data
# five hours = 18000 seconds
lag = 18000
df$time = as.POSIXct(df$time, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
df$time = df$time + lag
df$day = as.Date(df$time)
df1 <- merge(x = df, y = temp, by = c("index", "day"), all.x = TRUE)
# Process EPA data
aqs$Date = as.Date(aqs$GMTDate)
df2 <- merge(x = aqs, y = temp, by.x = c("index", "Date"), 
             by.y = c("index", "day"), all.x = TRUE)

df2 = subset(df2, !is.na(LS))

save(df1, file = "Data/PASmoke.RData")
save(df2, file = "Data/AQSSmoke.RData")
