# Clean PA data based on Development and Application of a United States wide correction for PM2. 5 data collected with the PurpleAir sensor
# PM2.5 = 0.524 * PA - 0.0862 * RH + 5.75
# Impute missing temperature and humidity, get data into desired format
# Source: https://pubmed.ncbi.nlm.nih.gov/34504625/
rm(list = ls())
library(dplyr)
library(tidyr)
library(lubridate)
setwd("/Users/hongjianyang/Research/California2021/RData/")
load("Y1_new.RData")
load("Y2_new.RData")
load("X_original.RData")
load("s1_new.RData")
load("s2_new.RData")
n1 = dim(Y1)[1]; n2 = dim(Y2)[1]
RH = X[1:n2 + n1,,3]

Y2 = 0.524 * Y2 - 0.0862 * RH + 5.75

setwd("/Users/hongjianyang/Research/California2021/RData/")
save(Y1, file = "Y1_corrected.RData")
save(Y2, file = "Y2_corrected.RData")
save(X, file = "X_corrected.RData")
save(s1, file = "s1_corrected.RData")
save(s2, file = "s2_corrected.RData")

