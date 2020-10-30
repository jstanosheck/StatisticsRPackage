library(Rcpp)
library(RcppArmadillo)
library(MASS)
library(mvtnorm)
library(dplyr)
#library(dplyr)

##############################################
##             Tests for K-Means            ##
##############################################

# Test with iris data
set.seed(123)
data("iris")
X <- iris[, -5]
X <- as.matrix(X)
K = 3
M <- X[ sample( 1:nrow(X), K ), , drop = F ]
n = nrow(X)

sourceCpp("./src/kmeans.cpp")
identical(MyKmeans_c(X, K, M), MyKmeans2(X, K, M) - 1 )

microbenchmark::microbenchmark(MyKmeans_R(X, K, M), MyKmeans_c(X, K, M), kmeans(X, K), times = 5)




# Test with Zipcode data
sourceCpp("./src/kmeans.cpp")
source("R_homework_functions/FunctionsKmeans - JS.R")
source('R/Kmeans_wrapper.R')
#sourceCpp('src/kmeans_test.cpp')

zipcode <- read.table("C:\\Users\\John\\Documents\\GitHub\\STAT600\\HW\\HW_02\\ZIPCODE.txt", header = F)
Y <- zipcode[ , 1]
X <- as.matrix(zipcode[ , -1])
K = 10
M <- X[ sample( 1:nrow(X), K ), , drop = F ]

microbenchmark::microbenchmark(MyKmeans_R(X, K, M), MyKmeans_c(X, K, M), kmeans(X, K), times = 10)

tmp = kmeans(X, K)
identical(tmp$cluster, MyKmeans_c(X, K, M)) 

microbenchmark::microbenchmark(MyKmeans_c(X, K, M))




# test with multivariate normals
sourceCpp("./src/kmeans.cpp")
source("R_homework_functions/FunctionsKmeans - JS.R")
source('R/Kmeans_wrapper.R')
K = 3 
X <- rbind(matrix(rnorm(1000, mean = 1, sd = 0.2), ncol = 2),
           matrix(rnorm(1000, mean = 3, sd = .15), ncol = 2),
           matrix(rnorm(1000, mean = 5, sd = 0.2), ncol = 2))
M <- X[ sample( 1:nrow(X), K ), , drop = F ]

kmeans_colors_C <- MyKmeans_c(X, K, M)
kmeans_colors_R <- MyKmeans_R(X, K)
plot(X, col = kmeans_colors_C+1)
plot(X, col = kmeans_colors_R)


MyKmeans_c(X, K, M)
MyKmeans_R(X, K, M) - 1
identical(MyKmeans_c(X, K, M), MyKmeans_R(X, K, M) - 1 )

microbenchmark::microbenchmark(MyKmeans_c(X, K, M), MyKmeans_R(X, K, M), kmeans(X, K))





##############################################
##          Tests for LRMultiClass          ##
##############################################


