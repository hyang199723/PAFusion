# HPC script for cross-validation
# Data fusion approach for varying A, cv- 1
# HPC script for cross-validation
rm(list = ls())
# Read in the arguments
args = commandArgs(trailingOnly = TRUE)
# Throw error if missing or extra parameters
argslen = length(args)
if (argslen > 2) stop('Error: too many arguments')
if (argslen < 2) stop('Error: too few arguments')
# Define batch number
inputSearch = as.integer(args[1])
inputBatch = as.integer(args[2])
batches = 1:5
batch = batches[inputBatch]

# Changing variables for different batch
dat = paste0("test", batch, ".RData")
tst = paste0("ind", batch, ".RData")

# Changing slope and intercept
int_array = seq(0.5, 0.9, 0.1)
slope_array = seq(-0.1, 0.1, 0.01)
combo = expand.grid(int_array, slope_array)
intercept = combo[inputSearch, 1]
slope = combo[inputSearch, 2]
out_name = paste0(intercept, "_", slope, "_", batch, ".RData")

# Path
#setwd("/Users/hongjianyang/Research/California2021/CV_Al/")
setwd("/share/bjreich/hyang23/Cali2021/CV_Al/")
source("LMC_Al.R")
load("Y1.RData")
load(dat)
load(tst)
load("Y2.RData")
load("X.RData")
load("s1.RData")
load("s2.RData")

# Loading
Y1_test = get(paste0("Y1_test_", batch))
test_set = get(paste0("test", batch))
iters = 8000
burn = 5000
Y1_test = log(Y1_test)
Y2 = log(Y2)
out = LMC_fit(Y1_test, Y2, X, s1, s2, 
              intercept = intercept, slope = slope,
              iters = iters)
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
