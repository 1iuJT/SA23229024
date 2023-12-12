#include <Rcpp.h>
using namespace Rcpp;

extern "C" {
  SEXP gibbs_sampling_iter(SEXP last_sample, SEXP sigma, SEXP mu);
  SEXP Gibbs_sampling_cpp(SEXP N, SEXP burnin, SEXP sigma, SEXP mu);
}

//' @title Gibbs sampling iteration
//' @name gibbs_sampling
//' @description Perform one iteration of Gibbs sampling
//' @param last_sample A numeric vector representing the last sample
//' @param sigma Covariance matrix for the bivariate normal distribution
//' @param mu Mean vector for the bivariate normal distribution
//' @return A numeric vector representing the new sample
//' @export
// [[Rcpp::export]]
NumericVector gibbs_sampling_iter(const NumericVector& last_sample, const NumericMatrix& sigma, const NumericVector& mu) {
   NumericVector sample(2);
   double sigma11 = sigma(0, 0);
   double sigma12 = sigma(0, 1);
   double sigma22 = sigma(1, 1);
   double mu1 = mu(0);
   double mu2 = mu(1);

   sample(0) = R::rnorm(mu1 + sigma12 * (last_sample(1) - mu2) / sigma22,
          std::sqrt(sigma11 - sigma12 * sigma12 / sigma22));
   sample(1) = R::rnorm(mu2 + sigma12 * (sample(0) - mu1) / sigma11,
          std::sqrt(sigma22 - sigma12 * sigma12 / sigma11));
   return sample;
 }


//' @title A Gibbs sampler using Rcpp
//' @description A Gibbs sampler using Rcpp
//' @param N Total number of samples to generate
//' @param burnin Number of initial samples to discard
//' @param sigma Covariance matrix for the bivariate normal distribution
//' @param mu Mean vector for the bivariate normal distribution
//' @return A numeric matrix with each row being a sample from the distribution
//' @examples
//' \dontrun{
//' # Define the mean vector and covariance matrix for a bivariate normal distribution
//' mu <- c(0, 0)
//' sigma <- matrix(c(1, 0.5, 0.5, 1), nrow = 2, byrow = TRUE)
//' # Run Gibbs sampling to generate samples
//' set.seed(123) # Set seed for reproducibility
//' samples <- Gibbs_sampling_cpp(N = 1000, burnin = 100, sigma = sigma, mu = mu)
//' }
//' @export
// [[Rcpp::export]]
NumericMatrix Gibbs_sampling_cpp(int N, int burnin, NumericMatrix sigma, NumericVector mu) {
   NumericMatrix chain(N, 2);
   chain(0, 0) = R::rnorm(mu(0), sigma(0, 0));
   chain(0, 1) = R::rnorm(mu(1), sigma(1, 1));

   // Perform the Gibbs sampling
   for (int i = 1; i < N + burnin; i++) {
     // For actual sample use the last row of the chain, for burn-in use the last sampled values
     NumericVector last_sample = i < burnin ? NumericVector::create(chain(i - 1, 0), chain(i - 1, 1)) : chain(i - burnin, _);
     NumericVector sample = gibbs_sampling_iter(last_sample, sigma, mu);

     if (i >= burnin) {
       chain(i - burnin, _) = sample;
     }
   }

   return chain;
}


