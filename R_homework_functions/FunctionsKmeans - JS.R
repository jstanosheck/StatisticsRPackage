# Function that implements K-means algorithm. The default number of maximal iterations is 100.
MyKmeans_R <- function(X, K, M = NULL, numIter = 100){
  
  X <- as.matrix( X, drop = F ) # ensure that X is of the proper form
  
  if ( is.character(numIter) ) { 
    stop( "numIter cannot be character value" ) # ensure that user supplied is a valid integer
  } else if( !is.na(numIter) ) {
    numIter <- as.integer( as.character(numIter) )
  } 
  
  # Check whether M is NULL or not. 
  # If NULL, initialize based on K randomly selected points from X. 
  # If not NULL, check for compatibility with X dimensions.
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
  
  # Implement K-means algorithm. It should stop when either 
  # (i) the centroids don't change from one iteration to the next, or 
  # (ii) the maximal number of iterations was reached. 
  
  all_eucdist <- list()
  
  for( i in 1 : numIter ) {
    
    mean_init <- M 

    # Expand Euclidean distance calculation in the form of an n x p matrix
    xTx <- matrix( rowSums(X^2), nrow(X), K ) # ||X||^2
    mTm <- matrix( rowSums(M^2), nrow(X), K, byrow = T ) # ||M||^2
    xmT <- 2 * X %*% t(M) 
    
    all_eucdist <- xTx + mTm - xmT # finally found the golden ticket!
    
    Y <- as.matrix( max.col( -all_eucdist, ties.method = "first" ), drop = F )
    
    for (p in 1:K){
      M[p, ] <- t( colMeans( X[which(Y == p), , drop = F ] ) )
    }
    
    if ( abs( sum( mean_init - M ) ) ==  0 ) {
      print(paste("Clusters converged after ", i, " iterations", sep = ""))
      break
      }
  
  }
  
  # Return the vector of assignments Y
  return(Y)
}
