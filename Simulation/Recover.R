# Simulation study to test correctness of the full conditionals
rm(list = ls())
library(coda)
# Path
setwd("/Users/hongjianyang/Desktop/PaperCode/Simulation/")
source("LMC_Cali2021_A.R")
load("Comparison.RData")

s1 = coords1
s2 = coords2

A = 0.2

iters = 8000
burn = 5000
#Y1_test = log(Y1)
#Y2 = log(Y2)
out = LMC_fit(Y1, Y2, X, s1, s2, initA = A, iters = iters)
# Store RMSE, 95% coverage, and prediction variance
betau = out$betau
betav = out$betav
sig1 = out$sig1
sig2 = out$sig2

plot(betau[1, burn:iters], type = 'l')
abline(2, 0, col = 'red')

plot(betau[2, burn:iters], type = 'l', 
     main = "Temperature", ylab = "Value", xlab = "Iteration")
abline(0.118, 0, col = 'red')
plot(betau[3, burn:iters], type = 'l',
     main = "Humidity", ylab = "Value", xlab = "Iteration")
abline(0.064, 0, col = 'red')
plot(betau[4, burn:iters], type = 'l',
     main = "Low Smoke Plume", ylab = "Value", xlab = "Iteration")
abline(0.007, 0, col = 'red')

plot(betau[5, burn:iters], type = 'l',
     main = "Medium Smoke Plume", ylab = "Value", xlab = "Iteration")
abline(0.022, 0, col = 'red')

plot(betau[6, burn:iters], type = 'l',
     main = "High Smoke Plume", ylab = "Value", xlab = "Iteration")
abline(0.049, 0, col = 'red')



