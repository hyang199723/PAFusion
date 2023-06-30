# This code downloads data from this website
#      https://www.ospo.noaa.gov/Products/land/hms.html#data
rm(list=ls())
library(sf)
library(dplyr)
library(lubridate)

# Download HMS fire location data
year = "2021"

all_date <- seq(as.Date("2021-01-01"), as.Date("2021-12-31"), by="days")
all_date <- as.character(all_date)
month <- as.character(month(all_date))
day <- as.character(day(all_date))

month = sprintf("%02d", as.numeric(month))
day = sprintf("%02d", as.numeric(day))

# Load smoke data
#   Plumes are classified as light, medium and heavy smoke
#   and these classes are downloaded separately

filename<- function(m,d,y=year){
  dir <- "https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/KML/"
  fl  <- paste0(dir,y,"/",m,"/hms_smoke",y,m,d,".kml")
  return(fl)}
dest = "/Users/hongjianyang/Desktop/California2021/Raw_Data/"
smoke_light <- list()
smoke_medium <- list()
smoke_heavy <- list()
for(doy in 1:length(month)){
  m    <- month[doy]
  d    <- day[doy]
  fl   <- filename(m,d)
  fn <- paste0("hms_smoke2021", m, d, ".kml")
  local_file <- paste0(dest, fn)
  # download.file(fl, destfile = local_file)
  temp_light <- try(read_sf(local_file,layer="Smoke (Light)"), silent = TRUE,
                    outFile = getOption("try.outFile", default = stderr()))
  if(class(temp_light)[1]=="sf"){
    smoke_light[[doy]]  <- st_combine(temp_light)
  } else{
    smoke_light[[doy]] = NULL
  }
  temp_medium <- try(read_sf(local_file,layer="Smoke (Medium)"), silent = TRUE,
                     outFile = getOption("try.outFile", default = stderr()))
  if(class(temp_medium)[1]=="sf"){
    smoke_medium[[doy]]  <- st_combine(temp_medium)
  } else {
    smoke_medium[[doy]] = NULL
  }
  temp_heavy <- try(read_sf(local_file,layer="Smoke (Heavy)"), silent = TRUE,
                    outFile = getOption("try.outFile", default = stderr()))
  if(class(temp_heavy)[1]=="sf"){
    smoke_heavy[[doy]]  <- st_combine(temp_heavy)
  } else {
    smoke_heavy[[doy]] = NULL
  }
}
smoke_day   <- day
smoke_month <- month
outfile     <- "/Users/hongjianyang/Desktop/California2021/Data/HMS2021.RData"
save(smoke_day,smoke_month,smoke_light,smoke_medium,smoke_heavy,file=outfile)