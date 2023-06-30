# HPC script for cross-validation
rm(list = ls())

# Read in the arguments
args = commandArgs(trailingOnly = TRUE)
# Throw error if missing or extra parameters
argslen = length(args)
if (argslen > 2) stop('Error: too many arguments')
if (argslen < 2) stop('Error: too few arguments')
# Define the fixed A and batch number
inputA = as.integer(args[1])
As<- c(0.1, 0.2, 0.4, 0.6, 0.8)
A <- As[inputA]

inputBatch = as.integer(args[2])
batches = 1:5
batch = batches[inputBatch]

# Changing variables for different batch
out_name = paste0("A", A, "_", batch, "_", "Result.RData")
dat = paste0("test", batch, ".RData")
tst = paste0("ind", batch, ".RData")
iA = A

# Path
setwd("/share/bjreich/hyang23/Cali2021/HPC_Corrected_multiple/")
source("LMC_Cali2021_A.R")
load("Y1.RData")
load(dat)
load(tst)
load("Y2.RData")
load("X.RData")
load("s1.RData")
load("s2.RData")

# Loading
Y1_test = get(paste0("Y1_test_", batch))
Y1_test = log(Y1_test)
test_set = get(paste0("test", batch))

iters = 8000
burn = 5000
Y2 = log(Y2)
out = LMC_fit(Y1_test, Y2, X, s1, s2, iters = iters, initA = iA)
# Store RMSE, 95% coverage, and prediction variance
z1_out = out$Y1
# Test set
Y_test = log(Y1[test_set, ])
z1 = rowMeans(z1_out[,,burn:iters], dims = 2)
z1_test = z1[test_set, ]
z1_test[is.na(Y_test)] = NA
rmse = sqrt(sum((z1_test - Y_test)^2, na.rm = T) / sum(!is.na(Y_test)))

# Calculate 95% coverage prob
z1_out_test = z1_out[test_set, ,burn:iters]
z1_out_mean = rowMeans(z1_out_test, dims = 2)
z1_out_sd = array(data = NA, dim = dim(z1_out_mean))
ntest = sum(test_set)
for (i in 1:ntest) {
  for (j in 1:123) {
    z1_out_sd[i, j] = sd(z1_out_test[i, j, ])
  }
}
lower = qnorm(0.025, mean = z1_out_mean, sd = z1_out_sd)
upper = qnorm(1-0.025, mean = z1_out_mean, sd = z1_out_sd)
lower[is.na(Y_test)] = NA
upper[is.na(Y_test)] = NA
cvr = sum(lower <= Y_test & upper >= Y_test, na.rm= T) / sum(!is.na(Y_test))

# Calculate prediction variance
pv = mean(z1_out_sd^2)
# Save outputs
out = data.frame(rmse = rmse, cvr = cvr, pv = pv)
save(out, file = out_name)
