// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"

// via the depends attribute we tell Rcpp to create hooks for
// RcppArmadillo so that the build process will know what to do
//
// [[Rcpp::depends(RcppArmadillo)]]

//Create softmax_c function that gets the probability in a matrix form
arma::mat softmax_c(arma::mat X, arma::mat beta){
    
    //multilply x and beta to get xbeta
    arma::mat xbeta = X * beta;
    
    //find the sum of the rows of exp(xbeta)
    arma::mat denom = arma::diagmat(1 / sum(exp(xbeta), 1));
    
    
    //calculates the probabilities for each element in the matrix
    arma::mat softmax = exp(xbeta) * denom;
    return(softmax);
}

//Create objective function. Will have for loop
double objective_function(arma::mat& beta, arma::mat probability, double lambda,
                          arma::mat& X, arma::uvec& Y, int K, int n){
    //initialize local variables
    arma::vec inner_obj(n, 0); //vector of size n with all values=0
    
    //lambda term in objective function 
    double lambda_sum = (lambda / 2) * arma::sum(arma::sum(beta^2, 0), 1);
    
    //loop to get log probability for each K value
    for (int i = 0; i < K; i++){
        //finds the indexes of Y that follow the logical function
        arma::uvec index = arma::find(Y == i);
        
        //finds the log(probability) for each index 
        inner_obj(index) = log(probability(index, i));
    }
    //adds the sum of the inner object and the lambda sum to get the objective value
    double objective = -sum(inner_obj) + lambda_sum;
    
    return(objective);
}

//generate the gradient for the logistic function
arma::mat logistic_gradient(arma::mat&X, arma::uvec& Y, arma::mat& beta,
                            double lambda, int K, int n){
    //initialize probability matrix with old probabilities
    arma::mat prob_mat = softmax_c(X, beta); //initializes n x K matrix of 0's
    
    //loop to generate prob_mat values
    for (int i = 0; i < K; i++){
        //get indexes for all values of Y == i for K
        arma::uvec index = arma::find(Y == i);
        
        //Reduce the value of prob_mat by one for all indexes of Y==i
        prob_mat(index, i) = prob_mat(index, i) - 1;
    }
    //calculate gradient with new probability matrix
    gradient = X.t() * prob_mat + lambda * beta;
    
    return(gradient);
}

//create update beta function loop through K
arma::mat update_beta(arma::mat& beta, arma::mat& X, arma::uvec& Y,
                      double eta, double lambda, int K, int p, int n){
    //initialize local variables
    arma::mat prob = softmax_c(X, beta); //gets the probability for the current beta
    arma::vec lambda_vec(p, lambda); //generates vector of length p with values lambda
    arma::mat lam = arma::diagmat(lambda_vec); //generates diagonal matrix 
    arma::mat new_beta = arma::zeros(p, K); //generates p x K matrix of 0's 
    
    //loop to set new_beta
    for (int i = 0; i < K; i++){
        //weight for instance K
        arma::colvec w = prob.col(i) * (1 - prob.col(i));
        
        //multiply X * w to get the weighted X to find Hessian
        arma::mat weighted_X = X * w;
        
        //find Hessian by multiplying xTxw + lam
        arma::mat H = X.t() * weighted_X + lam;
        
        //find gradient 
        beta_gradient = gradient(X, Y, beta, lambda, K, n);
        
        //update column i of new_beta matrix
        new_beta.col(i) = beta.col(i) - eta * arma::solve(H, beta_gradient);
    }
    
    return(new_beta);
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
    
    // Starting value of objective function and initial probability
    arma::mat initial_probability = softmax_c(X, beta);
    objective(0) = objective_function(beta, initial_probability, lambda, X, Y, K, n);
    
    // Newton's method cycle - implement the update EXACTLY numIter iterations
    for (int s = 1; s < numIter; s++){
        //update the beta to the new beta
        beta = update_beta(beta,  X, Y, eta, lambda, K, p, n);
        
        //generates new probability with the updated beta
        arma::mat new_probability = softmax_c(X, beta);
        
        //find new objective function value and store in objective vector
        abjective(s) = objective_function(veat, new_probability, lambda, X, Y, K, n);
    }
    
    // Create named list with betas and objective values
    return Rcpp::List::create(Rcpp::Named("beta") = beta,
                              Rcpp::Named("objective") = objective);
}
