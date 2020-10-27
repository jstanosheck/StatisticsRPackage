// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"

// via the depends attribute we tell Rcpp to create hooks for
// RcppArmadillo so that the build process will know what to do
//
// [[Rcpp::depends(RcppArmadillo)]]

//Create softmax_c function that gets the probability in a matrix form
// [[Rcpp::export]]
arma::mat softmax_c(arma::mat X, arma::mat beta){
    
    //multilply x and beta to get xbeta
    arma::mat xbeta = X * beta;
    
    //find the sum of the rows of exp(xbeta)
    arma::colvec denom = sum(exp(xbeta), 1);
    
    //finds e^xbeta for each element
    arma::mat expXbeta = exp(xbeta);
    
    //calculates the probabilities for each element in the matrix
    arma::mat softmax = expXbeta.each_col() / denom;
    return(softmax);
}

//Create objective function. Will have for loop
// [[Rcpp::export]]
double objective_function_c(arma::mat& beta, arma::mat& probability, double lambda,
                          const arma::mat& X, const arma::uvec& Y, int K, int n){
    //initialize local variables
    arma::colvec inner_obj(n); //vector of size n
    
    //finds the sum of beta^2 and multiplies it by lambda/2
    double lambda_sum = (lambda / 2) * arma::accu(arma::pow(beta, 2));
    
    //loop to get log probability for each K value
    for (int i = 0; i < K; i++){
        //finds the indexes of Y that follow the logical function
        arma::uvec index = arma::find(Y == i);

        //transforms i into a uvec for indexing
        arma::uvec column(1);
        column = i;

        //finds the log(probability) for each index in column i
        inner_obj(index) = arma::log(probability.submat(index, column));
    }
    //adds the sum of the inner object and the lambda sum to get the objective value
    double objective = -1 * arma::accu(inner_obj) + lambda_sum;

    return(objective);
}

//generate the gradient for the logistic function
// [[Rcpp::export]]
arma::mat logistic_gradient(const arma::mat& X, const arma::uvec& Y, arma::mat& beta,
                            double lambda, int K, int n){
    //initialize probability matrix with old probabilities
    arma::mat prob_mat = softmax_c(X, beta); //initializes n x K matrix of 0's

    //loop to generate prob_mat values
    for (int i = 0; i < K; i++){
        //get indexes for all values of Y == i for K
        arma::uvec index = arma::find(Y == i);
        
        //transforms i into a uvec for indexing
        arma::uvec column(1);
        column = i;
        
        //Reduce the value of prob_mat by one for all indexes of Y==i
        prob_mat.submat(index, column) -= 1;
    }
    //calculate gradient with new probability matrix
    arma::mat gradient = X.t() * prob_mat + lambda * beta;

    return(gradient);
}

//create update beta function loop through K
// [[Rcpp::export]]
arma::mat update_beta_c(arma::mat& beta, const arma::mat& X, const arma::uvec& Y,
                      double eta, double lambda, int K, int p, int n){
    //initialize local variables
    arma::mat prob = softmax_c(X, beta); //gets the probability for the current beta
    arma::vec lambda_vec (p); //generates vector of length p
    lambda_vec.fill(lambda);//fills lambda_vec with values lambda
    arma::mat lam = arma::diagmat(lambda_vec); //generates diagonal matrix
    arma::mat new_beta = arma::zeros(p, K); //generates p x K matrix of 0's
    
    //loop to set new_beta
    for (int i = 0; i < K; i++){
        //weight for instance K
        arma::colvec w = prob.col(i) % (1 - prob.col(i));
        //multiply X * w to get the weighted X to find Hessian
        arma::mat weighted_X = X.each_col() % w;
     
         //find Hessian by multiplying xTxw + lam
        arma::mat H = X.t() * weighted_X + lam;

        //find gradient
        arma::colvec beta_gradient = logistic_gradient(X, Y, beta, lambda, K, n).col(i);

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
    objective(0) = objective_function_c(beta, initial_probability, lambda, X, y, K, n);

    // Newton's method cycle - implement the update EXACTLY numIter iterations
    for (int s = 1; s < numIter + 1; s++){
        //update the beta to the new beta
        beta = update_beta_c(beta,  X, y, eta, lambda, K, p, n);

        //generates new probability with the updated beta
        arma::mat new_probability = softmax_c(X, beta);

        //find new objective function value and store in objective vector
        objective(s) = objective_function_c(beta, new_probability, lambda, X, y, K, n);
    }

    // Create named list with betas and objective values
    return Rcpp::List::create(Rcpp::Named("beta") = beta,
                              Rcpp::Named("objective") = objective);
}
