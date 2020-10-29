# Tests for K-Means
library(Rcpp)
library(RcppArmadillo)
library(MASS)
library(mvtnorm)
library(dplyr)
#library(dplyr)

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

microbenchmark::microbenchmark(MyKmeans_c(X, K, M), MyKmeans2(X, K, M), kmeans(X, K))






# Test with Zipcode data
sourceCpp("./src/kmeans.cpp")
source("R_homework_functions/FunctionsKmeans - JS.R")
zipcode <- read.table("C:\\Users\\John\\Documents\\GitHub\\STAT600\\HW\\HW_02\\ZIPCODE.txt", header = F)
Y <- zipcode[ , 1]
X <- as.matrix(zipcode[ , -1])
K = 10
M <- X[ sample( 1:nrow(X), K ), , drop = F ]

microbenchmark::microbenchmark(MyKmeans(X, K, M), MyKmeans2(X, K, M), kmeans(X, K), times = 5)
tmp = kmeans(X, K)
identical(tmp$cluster, MyKmeans_c(X, K, M)) 




# Test with built-in dataset from kmeans





# test with multivariate normals
# source code: https://www.r-bloggers.com/2018/11/generate-datasets-to-understand-some-clustering-algorithms-behavior/
sourceCpp("./src/kmeans.cpp")
source("R_homework_functions/FunctionsKmeans - JS.R")

generateGaussianData <- function(n, center, sigma, label) {
  data = mvtnorm::rmvnorm(n, mean = center, sigma = sigma)
  data = data.frame(data)
  names(data) = c("x", "y")
  data = data %>% mutate(class=factor(label))
  data
}

# cluster 1
n = 1000
center = c(5, 5)
sigma = matrix(c(1, 0, 0, 1), nrow = 2)
data1 = generateGaussianData(n, center, sigma, 1)
# cluster 2
n = 1000
center = c(1, 1)
sigma = matrix(c(1, 0, 0, 1), nrow = 2)
data2 = generateGaussianData(n, center, sigma, 2)
# cluster 3
n = 1000
center = c(1, 8)
sigma = matrix(c(1, 0, 0, 1), nrow = 2)
data3 = generateGaussianData(n, center, sigma, 2)
# all data
data = bind_rows(data1, data2, data3)
data

X <- as.matrix(data[, 1:2])
plot(X)

K = 3
M <- X[ sample( 1:nrow(X), K ), , drop = F ]


MyKmeans_c(X, K, M)
MyKmeans2(X, K, M) - 1
identical(MyKmeans_c(X, K, M), MyKmeans2(X, K, M) - 1 )

microbenchmark::microbenchmark(MyKmeans_c(X, K, M), MyKmeans2(X, K, M), kmeans(X, K))
