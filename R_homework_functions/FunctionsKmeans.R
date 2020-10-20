#function to assign points to a cluster

#modified sweep function to vectorize subtraction
test_sweep <- function (x, STATS){
  FUN <- match.fun("-") #Sets the function as minus
  
  #permorms the subtraction on the array
  FUN(x, STATS)
}


#function to assign each point to a specific cluster
assign_clusters <- function(X, mtest){
  #set local variables
  n <- nrow(X)
  p <- ncol(X)
  k <- nrow(mtest)
  dims <- dim(X)
  
  #set a distance_to_centroid matrix of NA to dimensions of n x K
  distance_to_centroid <-matrix(NA, n, k)
  cluster_labels <- rep(NA, n) #create an empty vector for each classification
  
  #for loop that gets distance between each point and M and classifies points
  for (i in 1:k){
    
    #set the array for test sweep to decrease computational time
    stat_array <- array(mtest[i, ], dims[c(2, 1)])
    perm_Matrix <- matrix(stat_array, n, p, byrow = TRUE)
    
    #gets the distance for individual values of k
    inside <- sqrt(rowSums((test_sweep(X, perm_Matrix)^2)))
    
    #assigns columnwise to the diestance matrix
    distance_to_centroid[ , i] <- inside 
  }
  for (f in 1:n){
    cluster_labels[as.numeric(f)] <- which.min(distance_to_centroid[f, ])
  }
  return(cluster_labels)
}


#Uses the assigned clusters to regenerate the centroids
generate_centroids <- function(X, cluster_labels, k){
  #make local variables
  p <- ncol(X)
  
  #makes the new centroids to be tested against
  mtest2 <- matrix(0, k, p) #generates matrix to store changed values
  
  #loop through K to assign M2 values from classification
  for (cls in 1:k){
    if (is.na(colMeans(X[which(cluster_labels == cls), , drop = FALSE])[1])){
      mtest2[cls, ] <- 0
    }else{
      mtest2[cls, ] <- colMeans(X[which(cluster_labels == cls), , drop = FALSE])
    }
    #mtest2[cls, ] <- colMeans(X[which(cluster_labels == cls), , drop = FALSE])
  }
  return(mtest2)
}




# Function that implements K-means algorithm. The default number of maximal iterations is 100.
MyKmeans3 <- function(X, K, M = NULL, numIter = 100){
  
  # Check whether M is NULL or not. If NULL, initialize based on K randomly selected points from X. If not NULL, check for compatibility with X dimensions.
  
  #If M is NULL then generate a K x p matrix for the centroids 
  if (is.null(M)) {
    xSize <- dim(X) #gets dimentions of matrix X vector with 2 variables
    
    M <- matrix(rnorm(K * xSize[2]), K, xSize[2])
  }
  
  #generate random normal matrix. Make sure the size of the matrix is K x p
  else if (dim(M)[1] == c(K) & dim(M)[2] == dim(X)[2]) { #checks for the correct dim of input M

    M <- M #Keep M as input
  }
  
  #Error statement for M
  else {
    stop("The dimensions of M are not correct. Please fix M or do not use argument.")
  }
  
  # Implement K-means algorithm. It should stop when either (i) the centroids don't change from one iteration to the next, or (ii) the maximal number of iterations was reached. 
  
  #overall while loop to run for the number of iterations
  iter <- 0 #variable to track the iterations
  #tol = 0.000001 #this is the assigned tolerance 
  differences <- c()
  
  while(numIter > iter && all(differences > 0)){
    iter <- iter + 1 #records number of iterations so far
    
    #generate cluster labels for the dataset
    cluster_labels <- assign_clusters(X, M)
    
    #generate the centroids from the cluster labels
    M2 <- generate_centroids(X, cluster_labels, K)
    
    #Check difference between M and M2 
    differences <- rowSums(abs(M - M2))
    
    #assign the new centroids to M from M2
    M <- M2
  }
  
  Y <- assign_clusters(X, M)
  
  # Return the vector of assignments Y
  return(Y)
}


