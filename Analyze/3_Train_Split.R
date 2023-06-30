# Split dataset into training and testing
# Cross-Validation of data fusion model
rm(list = ls())
setwd("/Users/hongjianyang/Research/California2021/RData/")
load("Y1_new.RData")
load("Y2_new.RData")
load("X_new.RData")
load("s1_new.RData")
load("s2_new.RData")

# Subset data into 5-folds
n1 = dim(Y1)[1]
k = 5
s = floor(n1/k)
cluster = sample(c(rep(1:k, each = n1/k), c(3, 5)))
# Prepare for HPC run
full = cbind(Y1, cluster)
test1 = cluster == 1
test2 = cluster == 2
test3 = cluster == 3
test4 = cluster == 4
test5 = cluster == 5
Y1_test_1 = Y1_test_2 = Y1_test_3 = Y1_test_4 = Y1_test_5 = Y1

Y1_test_1[test1, ] = NA
Y1_test_2[test2, ] = NA
Y1_test_3[test3, ] = NA
Y1_test_4[test4, ] = NA
Y1_test_5[test5, ] = NA
setwd("/Users/hongjianyang/Research/California2021/")
# Save data sets
save(Y1_test_1, file = "HPC/test1.RData")
save(Y1_test_2, file = "HPC/test2.RData")
save(Y1_test_3, file = "HPC/test3.RData")
save(Y1_test_4, file = "HPC/test4.RData")
save(Y1_test_5, file = "HPC/test5.RData")
# Save indicators
save(test1, file = "HPC/ind1.RData")
save(test2, file = "HPC/ind2.RData")
save(test3, file = "HPC/ind3.RData")
save(test4, file = "HPC/ind4.RData")
save(test5, file = "HPC/ind5.RData")
