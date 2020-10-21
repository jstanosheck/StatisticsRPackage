// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"

// via the depends attribute we tell Rcpp to create hooks for
// RcppArmadillo so that the build process will know what to do
//
// [[Rcpp::depends(RcppArmadillo)]]

// X - n x p matrix
// K - number of clusters
// M - K x p cluster centers (always given)
// numIter - maximal number of iterations
// [[Rcpp::export]]
//arma::uvec
arma::mat MyKmeans_c(const arma::mat& X, int K,
                            const arma::mat& M, int numIter = 100){
    // All input is assumed to be correct
    
    // Initialize some parameters
    int n = X.n_rows;
    int p = X.n_cols;
    arma::uvec Y(n); // to store cluster assignments
    
    // Initialize any additional parameters if needed
    
    // Expand Squared Euclidean Distance into ||X||^2 , ||M||^2 , and -2 * X * t(M)
    arma::mat xTx(X.n_rows, K);//, sum(pow(X,2), 1));
    
    int n_col_euc = K;
    for(int ii = 0; ii < n_col_euc; ii++){
        xTx.col(ii) = sum(pow(X,2), 1);
    }

    
    
    // For loop with kmeans algorithm
    
    
    // Returns the vector of cluster assignments
    return(xTx); //Y);
}

