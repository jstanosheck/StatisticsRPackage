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
    arma::mat M_loop = M;
    
    // For loop with kmeans algorithm
    
    arma::mat clust_init = M_loop;
    
    // Expand Squared Euclidean Distance into ||X||^2 , ||M||^2 , and -2 * X * t(M)
    arma::mat xTx(n, K, arma::fill::zeros); // Initialize for ||X||^2
    arma::mat mTm(n, K, arma::fill::zeros); // Initialize for ||M||^2
    arma::mat xmT(n, K, arma::fill::zeros); // Initialize for 2 * X * t(M)
    
    for(int ii = 0; ii < K; ii++){
        xTx.col(ii) = sum( pow(X, 2), 1);
        mTm.each_row() = sum( pow(M_loop, 2), 1).t();
        xmT = 2 * (X * M_loop.t());
    }

    arma::mat euc_dist = xTx + mTm - xmT;
    Y = index_max(-euc_dist, 1);
    
    //insert for loop here for each kk cluster
    for(double kk = 0; kk < K; kk++){
        M_loop.row(kk) = mean( X.rows(find(Y == kk)), 0);
    }
    
    arma::mat converge_diff = clust_init - M_loop; // why is this not calculating properly??
    // put if statement for monitoring convergence level here
    
    // Returns the vector of cluster assignments
    return(converge_diff);
}

