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
arma::uvec MyKmeans_c(const arma::mat& X, int K,
                            const arma::mat& M, int numIter = 100){
    // All input is assumed to be correct
    
    // Initialize some parameters
    int n = X.n_rows;
    // int p = X.n_cols;
    arma::uvec Y(n); // to store cluster assignments
    
    // Initialize any additional parameters if needed
    arma::mat M_loop = M;
    
    // Expand Squared Euclidean Distance into ||X||^2 , ||M||^2 , and -2 * X * t(M)
    arma::mat xTx(n, K, arma::fill::zeros); // Initialize for ||X||^2
    arma::mat mTm(n, K, arma::fill::zeros); // Initialize for ||M||^2
    arma::mat xmT(n, K, arma::fill::zeros); // Initialize for 2 * X * t(M)
    
    // For loop with kmeans algorithm
    //Loop through either until numIter is reached, or until there is no change
    //in the centroids between 2 iterations
    int aa = 0;
    while(aa < numIter){
        
        // initialize clust_init
        arma::mat clust_init = M_loop;
        
        xTx.zeros();
        mTm.zeros();
        xmT.zeros();
        
        xTx.each_col() = sum( X % X, 1);
        mTm.each_row() = sum( M_loop % M_loop, 1).t();
        xmT = 2 * (X * M_loop.t());
        
        arma::mat euc_dist = xTx + mTm - xmT;
        Y = index_max(-euc_dist, 1);
        
        // Loop over each kk cluster
        for(double kk = 0; kk < K; kk++){
            M_loop.row(kk) = mean( X.rows(arma::find(Y == kk)), 0);
        }
        
        clust_init -= M_loop;
        double converge_diff = std::fabs( accu(clust_init) );
        
        // if statement to monitor convergence level
        if(converge_diff < 0.000001){
            break;
        }
        
        //update the iteration count
        aa++;
        
    }
    
    // Returns the vector of cluster assignments
    return(Y);
}
