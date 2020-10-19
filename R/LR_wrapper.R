
#' Multi-Class Logistic Regression
#'
#' @param X -Matrix of n x p of the training data set. Where n is the number of instances and p is the number of variables
#' @param y -Vector of length n with the ground truth class labels for each instance. The class labels must be 0 to K-1.
#' @param numIter -The number of iterations the function will run before reporting a result. The default number is 50.
#' @param eta -The learning rate of the function. The default value is 0.1.
#' @param lambda -The ridge optimization parameter. The default value is 1
#' @param beta_init -(Optional) Matrix of p x K that is the initial values for beta in the algorithm. The default value is a zero matrix.
#'
#' @return
#' @export
#'
#' @examples
#' # Give example
LRMultiClass <- function(X, y, numIter = 50, eta = 0.1, lambda = 1, beta_init = NULL){
  
  #set initial dimension parameters
  K <- max(unique(y)) +1
  p <- dim(X)[2]
  n <- dim(X)[1]
  
  # Compatibility checks from HW3 and initialization of beta_init
  ###############################################################
  # Check that the first column of X and Xt are 1s, if not - display appropriate message and stop execution.
  if (all(X[ , 1] != 1 )){
    stop("Check first column of X. Must all be 1")
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
  out = LRMultiClass_c(X, y, numIter, eta, lambda, beta_init)
  
  # Return the class assignments
  return(out)
}