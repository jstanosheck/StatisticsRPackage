// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"

// via the depends attribute we tell Rcpp to create hooks for
// RcppArmadillo so that the build process will know what to do
//
// [[Rcpp::depends(RcppArmadillo)]]

//Create soft_max_c function that gets the probability in a matrix form
arma::mat soft_max_c(arma::mat X, arma::mat beta){
    
    //multilply x and beta to get xbeta
    arma::mat xbeta = X * beta;
    
    //find the sum of the rows of exp(xbeta)
    arma::mat denom = arma::diagmat(1 / sum(exp(xbeta), 1));
    
    
    //calculates the probabilities for each element in the matrix
    arma::mat softmax = exp(xbeta) * denom;
    return(softmax);
}

//Create objective function. Will have for loop
double objective_function(arma::mat beta, arma::mat probability,
                             double lambda, arma::mat& X, arma::uvec& Y){
    //initialize local variables
    int K = max(Y) +1; //gets the number of classes in Y when classes are 0 to K-1
    int n = X.rows(); //gets the number of rows in X
    arma::vec inner_obj(n, 0); //vector of size n with all values=0
    
}

// For simplicity, no test data, only training data, and no error calculation.
// X - n x p data matrix
// y - n length vector of classes, from 0 to K-1
// numIter - number of iterations, default 50
// eta - damping parameter, default 0.1
// lambda - ridge parameter, default 1
// beta_init - p x K matrix of starting beta values (always supplied in right format)
// [[Rcpp::export]]
Rcpp::List LRMultiClass_c(const arma::mat& X, const arma::uvec& y, const arma::mat& beta_init,
                               int numIter = 50, double eta = 0.1, double lambda = 1){
    // All input is assumed to be correct
    
    // Initialize some parameters
    int K = max(y) + 1; // number of classes
    int p = X.n_cols;
    int n = X.n_rows;
    arma::mat beta = beta_init; // to store betas and be able to change them if needed
    arma::vec objective(numIter + 1); // to store objective values
    
    // Initialize anything else that you may need
    
    // Newton's method cycle - implement the update EXACTLY numIter iterations
    
    
    // Create named list with betas and objective values
    return Rcpp::List::create(Rcpp::Named("beta") = beta,
                              Rcpp::Named("objective") = objective);
}
