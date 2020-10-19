# Function that implements multi-class logistic regression.
#############################################################
# Description of supplied parameters:
# X - n x p training data, 1st column should be 1s to account for intercept
# y - a vector of size n of class labels, from 0 to K-1 (n x 1)
# Xt - ntest x p testing data, 1st column should be 1s to account for intercept
# yt - a vector of size ntest of test class labels, from 0 to K-1
# numIter - number of FIXED iterations of the algorithm, default value is 50
# eta - learning rate, default value is 0.1
# lambda - ridge parameter, default value is 0.1
# beta_init - (optional) initial starting values of beta for the algorithm, should be p x K matrix 

## Return output
##########################################################################
# beta - p x K matrix of estimated beta values after numIter iterations
# error_train - (numIter + 1) length vector of training error % at each iteration (+ starting value)
# error_test - (numIter + 1) length vector of testing error % at each iteration (+ starting value)
# objective - (numIter + 1) length vector of objective values of the function that we are minimizing at each iteration (+ starting value)
LRMultiClass <- function(X, y, Xt, yt, numIter = 50, eta = 0.1, lambda = 1, beta_init = NULL){
  ## Check the supplied parameters as described. You can assume that X, Xt are matrices; y, yt are vectors; and numIter, eta, lambda are scalars. You can assume that beta_init is either NULL (default) or a matrix.
  ###################################
  # Check that the first column of X and Xt are 1s, if not - display appropriate message and stop execution.
  X <- as.matrix(X)
  Xt <- as.matrix(Xt)
  
  Y <- as.vector(y)
  Y <- Y + 1
  Yt <- as.vector(yt)
  Yt <- Yt + 1
  
  #K <- max(Y) + 1 # Faster Alternative but not necessarily always correct
  K <- length( unique(Y) )
  p <- ncol(X)
  
  numIter <- as.numeric(numIter)
  eta <- as.numeric(eta)
  lambda <- as.numeric(lambda)
  
  # Check to ensure that the first column of the supplied X / Xt are all 1 for intercept
  if( ( all( X[ , 1] == 1 ) & all( Xt[ , 1] == 1 ) ) == FALSE ) {
    stop( 'The first column of either X or Xt is invalid. First column must be all 1s.' ) # should this automatically add the column of 1s?
  } 
  
  # Check for compatibility of dimensions between X and Y
  if( nrow(X) != length(Y) ){
    stop( 'The number of rows of Y does not match the number of rows of X' )
  }
  
  # Check for compatibility of dimensions between Xt and Yt
  if( nrow(Xt) != length(Yt) ){
    stop( 'The number of rows of Yt does not match the number of rows of Xt' )
  }
  
  # Check for compatibility of dimensions between X and Xt
  if( ncol(X) != ncol(Xt) ){
    stop( 'The number of columns of X does not match the number of columns of Xt' )
  }
  
  # Check eta is positive
  if( eta <= 0 ){
    stop('eta must be positive ( > 0)')
  }
  
  # Check lambda is non-negative
  if( lambda < 0 ){
    stop( 'lambda must be non-negative' )
  } 
  
  # Check whether beta_init is NULL. If NULL, initialize beta with p x K matrix of zeroes. If not NULL, check for compatibility of dimensions with what has been already supplied.
  if( is.null( beta_init ) ){
    beta_init = matrix( 0, nrow = ncol( X ), ncol = K )
  }else if( ( dim( beta_init )[1] != ncol( X ) ) | ( dim( beta_init )[2] != K ) ){ 
    stop('Incorrect dimensions of user-supplied beta_init')
  }
  
  
  
  ## Calculate corresponding pk, objective value f(beta_init), training error and testing error given the starting point beta_init
  ##########################################################################
  
  pk <- function(X, beta_init){
    
    xTb <- exp( X %*% beta_init) # This is an n x k matrix resulting from X_i'B_k
    pk <- xTb / rowSums(xTb) # Divide every element by its corresponding rowsum
    
    return(pk)
  }
  
  
  obj <- function(X, Y, beta_init, pk_mat){
    
    fb_list <- list()
    
    for(k in 1:(K)){
      
      fb <- -1 * sum( log( pk_mat[which(Y == k), k, drop = F] ) ) + 
        (lambda / 2) * crossprod(beta_init[, k, drop = F], beta_init[, k, drop = F]) 
      fb_list[k] <- fb
      
    }
    
    fb_all <- sum(unlist(fb_list)) 
    
    return( fb_all )
  }
  
  
  ## Newton's method cycle - implement the update EXACTLY numIter iterations
  ##########################################################################
  
  
  # Within one iteration: perform the update, calculate updated objective function and training/testing errors in %
  
  beta_update <- function(X, K, Y, beta_init, pk_mat){
    
    for(k in 1:K){
      
      wx <- (pk_mat[, k] * (1 - pk_mat[, k])) * X
      xTwx <- crossprod(X, wx)
      diag(xTwx) <- diag(xTwx) + lambda
      
      b_2 <- crossprod(X, (pk_mat[, k] - (Y == k) ) ) + lambda * beta_init[, k, drop = F]
      
      beta_init[, k] <- beta_init[ , k, drop = F] - eta * solve(xTwx, b_2)
      
      #pk_mat <- pk(X, beta_init)
    }
    
    return(beta_init)

  }
  

  
  # Begin iterations for numerical approximations and convergence
  pk_mat <- pk(X, beta_init)
  objective <- rep( obj(X, Y, beta_init, pk_mat ), numIter + 1) # initialize objective vector with starting f(B)
  error_train <- list( (1 - ( (sum(max.col(pk(X, beta_init)) == Y)) / length(Y) ) ) * 100  ) # initialize with starting beta_init and X
  error_test <- list( (1 - ( (sum(max.col(pk(Xt, beta_init)) == Yt)) / length(Yt) ) ) * 100  ) # initialize with starting beta_init and Xt
  
  
  for(i in 1:numIter){
    
    beta_new <- beta_update(X = X, K = K, Y = Y, beta_init = beta_init, pk_mat = pk_mat)
    pk_mat <- pk(X, beta_new)
    objective[i + 1] <- obj(X = X, Y = Y, beta_init = beta_new, pk_mat = pk_mat)
    
    prob_train <- pk(X, beta_new)
    prob_test <- pk(Xt, beta_new)

    error_train[i + 1] <- (1 - ( (sum(max.col(prob_train) == Y)) / length(Y) ) ) * 100 # error_train * 100 per HW requirements
    error_test[i + 1] <- (1 - ( (sum(max.col(prob_test) == Yt)) / length(Yt) ) ) * 100 # error_test * 100 per HW requirements

    beta_init <- beta_new
  }
  
  beta <- beta_new
  
  error_train <- unlist(error_train)
  error_test <- unlist(error_test)
  
  ## Return output
  ##########################################################################
  # beta - p x K matrix of estimated beta values after numIter iterations
  # error_train - (numIter + 1) length vector of training error % at each iteration (+ starting value)
  # error_test - (numIter + 1) length vector of testing error % at each iteration (+ starting value)
  # objective - (numIter + 1) length vector of objective values of the function that we are minimizing at each iteration (+ starting value)
  return(list(beta = beta, error_train = error_train, error_test = error_test, objective =  objective))
}