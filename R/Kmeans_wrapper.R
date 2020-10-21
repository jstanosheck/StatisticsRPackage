#' K-Means Clustering
#'
#' @param X Initial Data matrix consisting of numeric entries
#' @param K A scalar corresponding to the number of clusters
#' @param M A Matrix of cluster centers
#' @param numIter A scalar that determines the maximum number of iterations for the algorithm in order to prevent an infinite loop. 
#'
#' @return A vector of cluster assignments (1 to K). The returned output will have the same length as the initial data matrix X
#' @export
#'
#' @examples
#' K = 10
#' M = NULL
#' numIter = 50
#' data(iris)
#' iris_data <- iris[, -5]
#' MyKmeans(iris_data, K = 3)
#' 
MyKmeans <- function(X, K, M = NULL, numIter = 100){
  
  n = nrow(X) # number of rows in X
  X <- as.matrix(X)
  
  # Check whether M is NULL or not. If NULL, initialize based on K random points from X. If not NULL, check for compatibility with X dimensions.
  if ( is.null(M) ){
    M <- X[ sample( 1:nrow(X), K ), , drop = F ] # K randomly selected M cluster centers
  } else {
    if ( dim( as.matrix(M) )[2] != ncol(X) ){
      
      stop( "Unequal number of columns between X and M" )
      
    }else if ( dim( as.matrix(M) )[1] > nrow(X) ){
      
      stop( "Number of M clusters exceeds the number of rows of X" )
      
    }else if ( any( duplicated(M) ) ){
      
      stop( "Duplicated clusters" )
      
    }
  } # Assuming no error, we now have a given X matrix, a pre-selected K number of clusters, and a valid M centroid matrix
  
  
  # Call C++ MyKmeans_c function to implement the algorithm
  Y = MyKmeans_c(X, K, M, numIter)
  
  # Return the class assignments
  return(Y)
}