#' Multi-Class Logistic Regression
#'
#' @param X Matrix of n x p of the training data set. Where n is the number of instances and p is the number of variables
#' @param y Vector of length n with the ground truth class labels for each instance. The class labels must be 0 to K-1.
#' @param numIter The number of iterations the function will run before reporting a result. The default number is 50.
#' @param eta The learning rate of the function. The default value is 0.1.
#' @param lambda The ridge optimization parameter. The default value is 1
#' @param beta_init (Optional) Matrix of p x K that is the initial values for beta in the algorithm. The default value is a zero matrix.
#'
#' @return
#' @export
#'
#' @examples
#' # Example #1
#' set.seed(123)
#' #set the initial size parameters
#' n <- 10
#' p <- 2
#' k <- 2
#' X <- matrix(runif(n * p), n, p)
#' X[ , 1] <- 1
#' y <- sample(0:(k-1), n, replace = TRUE)
#' betaInit <- matrix(runif(p * k), p, k)
#' 
#' LRMultiClass(X, y, beta_init = betaInit)
#' 
#' #Output:
#' #          [,1]       [,2]
#' # [1,] 0.3337088  0.2369798
#' # [2,] 0.4432530 -0.1145357
#' 
#' 
#' # Example #2
#' X <- cbind( 1, matrix( sample( 2:12, size = 900, replace = T, prob = rep(1/33, 11)), 100, 9) )
#' Y <- matrix( sample( 1:5, size = 100, replace = T, prob = rep(1/33, 5)), 100, 1)
#' K = length( unique(Y) )
#' #beta_init2 = matrix( 0, nrow = ncol( X ), ncol = K)
#' 
#' LRMultiClass(X = X, y = Y)
#' 
LRMultiClass <- function(X, y, numIter = 50, eta = 0.1, lambda = 1, beta_init = NULL){
  
  # Ensure that X is of matrix type and that y is of type vector
  X <- as.matrix(X)
  y <- as.vector(y)
  
  #set initial dimension parameters
  K <- max(unique(y)) +1
  p <- dim(X)[2]
  n <- dim(X)[1]
  
  # Compatibility checks from HW3 and initialization of beta_init
  ###############################################################
  # Check that the first column of X and Xt are 1s, if not - display appropriate message and stop execution.
  if( ( all( X[ , 1] == 1 ) ) == FALSE ) {
    stop( 'The first column of either X or Xt is invalid. First column must be all 1s.' ) # should this automatically add the column of 1s?
  } 
  # Check for compatibility of dimensions between X and y
  if (n != length(y)){ #rows of X and length of y
    stop("Dimention mismatch: Check compatability of X and y")
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
  ###############################################################
  # Call C++ LRMultiClass_c function to implement the algorithm
  out = LRMultiClass_c(X, y, beta_init, numIter, eta, lambda)
  
  # Return the class assignments
  return(out)
}

