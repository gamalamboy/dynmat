//' @useDynLib dynmat

#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

//' The Cobb-Douglas Production Function
//' 
//' This implements a naive version of the Cobb-Douglas Production
//' Function for N sectors. Its form consists of \deqn{
//' \prod_{n=1}^{N} A C_{i}^{\alpha_{i}} }, where \eqn{A} is a technology
//' index, \eqn{C_{i}} is a commodity, and \eqn{\alpha_{i}} is an elasticity
//' of substitution.
//' 
//' @param inputs a vector of inputs (all must be greater than 0).
//' @param elasts a vector of elasticities.
//' @param tech a scalar.
//' 
//' @return a scalar with the expected Cobb-Douglas output for that form.
//' @export
// [[Rcpp::export]]
arma::vec CobbDouglas( arma::vec & inputs, arma::vec & elasts, 
						   double tech = 1)
{
	arma::vec out = log(inputs).t() * elasts + log(tech);
	return exp(out);
}

