#softmax probability function
soft_max <- function(beta, X){
  
  xbeta <- X %*% beta #inner product of Xt * beta
  
  softmax_denom <- rowSums(exp(xbeta))#deonominator 
  
  softmax <- exp(xbeta) / softmax_denom #probabilities
  
  return(softmax)
}


#objective function 
objective_function <- function(beta, probability, lambda, X, y){
  K <- max(y) +1 #sets K param
  n <- dim(X)[1]
  
  #lambda term in objective function
  lambda_sums <- (lambda / 2) * sum(colSums(beta^2)) #finds the sum of beta ^2
  
  #for loop for each K 
  inner_obj <- rep(NA, n)
  for (i in 1:K){
    index <- which(y == i - 1)
    
    inner_obj[index] <- log(probability[index, i])
  }
  object <- -sum(inner_obj) + lambda_sums
  
  return(object)
}

#log gradient function
log_gradient <- function(beta, X, y, lambda, k){
  n <- nrow(X)
  old_p <- soft_max(beta, X)
  prob_mat <- matrix(NA, n, k)
  for (j in 1:k){
    index <- which(y == j - 1)#gets index of all y = K
    
    prob_mat[ , j] <- old_p[ , j] 
    
    prob_mat[index, j] <- prob_mat[index, j] - 1
    
  }
  gradient <- t(X) %*% prob_mat + lambda * beta
  
  return(gradient)
}

#update beta function
update_beta <- function(beta, X, y, eta, lambda){
  #define K
  K <- max(y) +1
  p <- ncol(X)
  
  #generate the probabilities
  prob <- soft_max(beta, X)
  
  #set lambda as a vector of length p and initialize new_beta
  lam <- diag(lambda, p, p)
  new_beta <- matrix(NA, p, K)
  
  #for loop to set new beta
  for (i in 1:K){
    #probability for instance k
    w <- prob[ , i] * (1 - prob[ , i])
    
    H <- crossprod(X, X * w) + lam #hessian for instance K
    
    #gradient
    gradient <- log_gradient(beta, X, y, lambda, K)[ , i, drop = F]
    
    #updated beta for instance K
    new_beta[ , i] <- beta[ , i, drop = FALSE] - eta * solve(H, gradient)
  }
  
  return(new_beta)
}


# Function that implements multi-class logistic regression.
#############################################################
# Description of supplied parameters:
# X - n x p training data, 1st column should be 1s to account for intercept
# y - a vector of size n of class labels, from 0 to K-1
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
  #set K as parameter
  K <- max(unique(y)) +1
  p <- dim(X)[2]
  n <- dim(X)[1]
  ## Check the supplied parameters as described. You can assume that X, Xt are matrices; y, yt are vectors; and numIter, eta, lambda are scalars. You can assume that beta_init is either NULL (default) or a matrix.
  ###################################
  # Check that the first column of X and Xt are 1s, if not - display appropriate message and stop execution.
  if (all(X[ , 1] != 1 ) || all(Xt[ , 1] != 1)){
    stop("Check first column of X and Xt. Must all be 1")
  }
  # Check for compatibility of dimensions between X and y
  if (n != length(y)){ #rows of X and length of y
    stop("Dimention mismatch: Check compatability of X and y")
  }
  # Check for compatibility of dimensions between Xt and yt
  if (dim(Xt)[1] != length(yt)){ #rows of Xt and length yt
    stop("Dimention mismatch: Check compatability of Xt and yt")
  }
  # Check for compatibility of dimensions between X and Xt
  if (p != dim(Xt)[2]){
    stop("Dimention mismatch: Check compatability of X and Xt")
  }
  # Check eta is positive
  if (eta < 0){
    stop("Invalid value: eta must be positive")
  }
  # Check lambda is non-negative
  if (lambda < 0){
    stop("Invalid value: lambda must be non-negative")
  }
  # Check whether beta_init is NULL. If NULL, initialize beta with p x K matrix of zeroes. If not NULL, check for compatibility of dimensions with what has been already supplied.
  if (is.null(beta_init)){
    beta_init <- matrix(0, p, K)
  }else if (dim(beta_init)[1] != p || dim(beta_init)[2] != K){
    stop("Dimention mismatch: Check compatability of beta_init")
  }
  ## Calculate corresponding pk, objective value f(beta_init), training error and testing error given the starting point beta_init
  ##########################################################################
  
  ## Newton's method cycle - implement the update EXACTLY numIter iterations
  ##########################################################################
 #initial testing of the data
  train_guess <- soft_max(beta_init, X)#train probability initial
  test_guess <- soft_max(beta_init, Xt)#test probability initial
  
  yHat <-  max.col(train_guess) - 1 #guess for initial yHat
  ytHat <- max.col(test_guess) - 1 #guess for initial ytHat
  
  train_error <- (1 - sum(y == yHat) / length(y)) * 100 #training error initial
  test_error <- (1 - sum(yt == ytHat) / length(yt)) * 100 #testing error initial
  
  error_train <- rep(train_error, numIter + 1)#train error vector initialization
  error_test <- rep(test_error, numIter + 1)#test error vector initialization
  
  init_prob <- soft_max(beta_init, X)
  
  objective <- rep(objective_function(beta_init, init_prob, lambda, X, y), numIter + 1)
  beta <- beta_init
  
  #start for loop to run from 1:numInter
  for (s in 1:numIter){
    #update beta
    beta_new <- update_beta(beta, X, y, eta, lambda)
    
    #probability with new beta
    new_probability <- soft_max(beta_new, X)
    
    #update objective function with beta_new
    objective[s + 1] <- objective_function(beta_new, new_probability, lambda, X, y)
    
    ##################################
    #calculate train and test error
    test_guess <- soft_max(beta_new, Xt)#test probability initial
    
    yHat <-  max.col(new_probability) - 1 #guess for initial yHat
    ytHat <- max.col(test_guess) - 1 #guess for initial ytHat
    
    train_error <- (1 - sum(y == yHat) / length(y)) * 100 #training error initial
    test_error <- (1 - sum(yt == ytHat) / length(yt)) * 100 
    ####################################
    #update return errors and beta
    error_train[s + 1] <- train_error #update error_train
    error_test[s + 1] <- test_error #update error_test
    beta <- beta_new #update beta
}
  
  
  # Within one iteration: perform the update, calculate updated objective function and training/testing errors in %
  
  
  ## Return output
  ##########################################################################
  # beta - p x K matrix of estimated beta values after numIter iterations
  # error_train - (numIter + 1) length vector of training error % at each iteration (+ starting value)
  # error_test - (numIter + 1) length vector of testing error % at each iteration (+ starting value)
  # objective - (numIter + 1) length vector of objective values of the function that we are minimizing at each iteration (+ starting value)
  return(list(beta = beta, error_train = error_train, error_test = error_test, objective =  objective))
}