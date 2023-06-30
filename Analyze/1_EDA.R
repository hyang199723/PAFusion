rm(list = ls())
setwd("/Users/hongjianyang/Desktop/California2021/")
load("Data/PASmoke_day.RData")
load("Data/AQSSmoke_day.RData")
load("Data/HMS2021.RData")
library(maps)
library(ggplot2)
library(ellipse)
library(sf)
library(dplyr)
library(tidyr)
pa <- pa_day
aqs <- aqs_day
# Keep only valid sites
pa <- subset(pa, !is.na(pa$PM2.5))
aqs <- subset(aqs, !is.na(aqs$PM2.5))
# Boxplot
paLS <- subset(pa, LS)
paMS <- subset(pa, MS)
paHS <- subset(pa, HS)
paLS <- subset(pa, LS)
paNone <- subset(pa, !LS & !MS & !HS)

paLS <- data.frame(pm = paLS$PM2.5, type = 'LS')
paMS <- data.frame(pm = paMS$PM2.5, type = 'MS')
paHS <- data.frame(pm = paHS$PM2.5, type = 'HS')
paNone <- data.frame(pm = paNone$PM2.5, type = 'None')

padf <- rbind(paNone, paLS, paMS, paHS)

# AQS site
aqsLS <- subset(aqs, LS)
aqsMS <- subset(aqs, MS)
aqsHS <- subset(aqs, HS)
aqsLS <- subset(aqs, LS)
aqsNone <- subset(aqs, !LS & !MS & !HS)

aqsLS <- data.frame(pm = aqsLS$PM2.5, type = 'LS')
aqsMS <- data.frame(pm = aqsMS$PM2.5, type = 'MS')
aqsHS <- data.frame(pm = aqsHS$PM2.5, type = 'HS')
aqsNone <- data.frame(pm = aqsNone$PM2.5, type = 'None')

aqsdf <- rbind(aqsNone, aqsLS, aqsMS, aqsHS)

padf$pm = log(padf$pm)
aqsdf$pm = log(aqsdf$pm)

padf$type[padf$type == "LS"] <- "Low"
padf$type[padf$type == "MS"] <- "Medium"
padf$type[padf$type == "HS"] <- "High"

aqsdf$type[aqsdf$type == "LS"] <- "Low"
aqsdf$type[aqsdf$type == "MS"] <- "Medium"
aqsdf$type[aqsdf$type == "HS"] <- "High"

padf$type = factor(padf$type, levels = c("None", "Low", "Medium", "High"))
aqsdf$type = factor(aqsdf$type, levels = c("None", "Low", "Medium", "High"))
#pdf(file = "/Users/hongjianyang/Desktop/Plots/BoxPlot.pdf")
boxplot(pm ~ type, data = padf, outline = F, ylim = c(0,5.5),
        xlab= "HMS smoke plume level", ylab = "Log fine particulate matter",
        main = "PurpleAir")
boxplot(pm ~ type, data = aqsdf, outline = F, ylim = c(0,5.5),
        xlab= "HMS smoke plume level", ylab = "Log fine particulate matter",
        main = "Air Quality System")
#dev.off()


# Select a day for the analysis
HSday = unique(subset(aqs, HS)$Date)
MSday = unique(subset(aqs, MS)$Date)
LSday = unique(subset(aqs, LS)$Date)
union(union(HSday, MSday), LSday)
# 2021-8-12 -> 224
# 2021-8-4 -> 216
a <- subset(aqs, HS)
a <- unique(a$Date) 
diff <- 18627
b <- as.integer(a)
b <- b - diff
for (j in b) {
  hs <- smoke_heavy[[j]]
  ms <- smoke_medium[[j]]
  ls <- smoke_light[[j]]
  map('usa')
  title(j)
  
  for (i in 1:length(ls)) {
    a <- st_cast(ls, "POLYGON")
    coord <- a[[i]][[1]][, 1:2]
    # coord <- coord[map.where("state",x=coord[,1],y=coord[,2])=="california", ]
    polygon(coord, col = 2)
  }
  
  for (i in 1:length(ms)) {
    a <- st_cast(ms, "POLYGON")
    coord <- a[[i]][[1]][, 1:2]
    # coord <- coord[map.where("state",x=coord[,1],y=coord[,2])=="california", ]
    polygon(coord, col = 3)
  }
  
  
  for (i in 1:length(hs)) {
    a <- st_cast(hs, "POLYGON")
    coord <- a[[i]][[1]][, 1:2]
    # coord <- coord[map.where("state",x=coord[,1],y=coord[,2])=="california", ]
    polygon(coord, col = 4)
  }
}

# Final Choice: 263

# 217; 237
# 235; 218
# 263; 225
# 242; 222
# 270; 269
# 267
# dev.new(width=15, height=5, unit="in")
plot  = T
if (plot) {
  # 235, 263, 271, 265, 207, 243, 210, 267
  j = 264
  hs <- smoke_heavy[[j]]
  ms <- smoke_medium[[j]]
  ls <- smoke_light[[j]]
  map('state', region = c("California"))
  # map('usa')
  # title(j)
  
  for (i in 1:length(ls[[1]])) {
    a <- st_cast(ls, "POLYGON")
    coord <- a[[i]][[1]][, 1:2]
    # coord <- coord[map.where("state",x=coord[,1],y=coord[,2])=="california", ]
    polygon(coord, col = 2)
  }
  
  for (i in 1:length(ms[[1]])) {
    a <- st_cast(ms, "POLYGON")
    coord <- a[[i]][[1]][, 1:2]
    # coord <- coord[map.where("state",x=coord[,1],y=coord[,2])=="california", ]
    polygon(coord, col = 3)
  }
  
  
  # # High smoke
  for (i in 1:length(hs[[1]])) {
    a <- st_cast(hs, "POLYGON")
    coord <- a[[i]][[1]][, 1:2]
    # coord <- coord[map.where("state",x=coord[,1],y=coord[,2])=="california", ]
    polygon(coord, col = 4)
  }
}
library(dplyr)
# 2021-9-20 matches 264??
plotSymbol = T
if (plotSymbol) {
  smoke_day <- as.Date("2021-09-20")
  
  pa_index <- pa$index
  aqs_index <- aqs$index
  pa_count = count(pa, index)
  aqs_count = count(aqs, index)
  
  pa_idx <- subset(pa_count, n >= 57)$index
  pa_idx <- pa_idx[1:100]
  aqs_idx <- subset(aqs_count, n >= 60)$index
  
  pa_sub = subset(pa, day == smoke_day)
  aqs_sub = subset(aqs, Date == smoke_day)
  pa_day <- pa_sub %>% 
    group_by(index, day) %>% 
    summarise(lat = mean(lat), lon = mean(lon), 
              LS = max(LS), MS =  max(MS), HS =  max(HS))
  aqs_day <- aqs_sub %>% 
    group_by(index, Date) %>% 
    summarise(lat = mean(lat), lon = mean(lon), 
              LS = max(LS), MS =  max(MS), HS =  max(HS))
  
  pa_day <- subset(pa_day, index %in% pa_idx)
  aqs_day <- subset(aqs_day, index %in% aqs_idx)
  
  pa_day$type = ifelse(pa_day$LS, "LS",
                       ifelse(pa_day$MS, "MS", 
                              ifelse(pa_day$HS, "HS", "None")))
  
  aqs_day$type = ifelse(aqs_day$LS, "LS",
                        ifelse(aqs_day$MS, "MS", 
                               ifelse(aqs_day$HS, "HS", "None")))
  
  cols = c("index", "lon", "lat", "LS", "MS", "HS")
  pacoord <- pa[, cols]
  aqscoord <- aqs[, cols]
  map("county","California",add=TRUE)
  
  points(x = pa_day$lon, y = pa_day$lat, pch=pa_day$type,col="gray",cex=.4)
  
  points(x = aqs_day$lon, y = aqs_day$lat, pch=aqs_day$type, cex=.4)
}

map("county","California",add=TRUE)
pa_coords <- cbind(pa$lon, pa$lat)
pa_unique <- unique(pa_coords)
aqs_coords <- cbind(aqs$lon, aqs$lat)
aqs_unique <- unique(aqs_coords)
points(x = pa_unique[, 1], y = pa_unique[, 2], ,col="purple",cex=.02)

points(x = aqs_unique[, 1], y = aqs_unique[, 2], cex=.2)


legend("topright",c("Low","Median","High"),
       col=2:4,pch=19,bty="n", cex = 0.7, title="HMS plume")


#legend("bottomleft",c("PA","AQS"),
#       col=c("gray","black"),pch=19,bty="n",title="PM2.5 monitor")
