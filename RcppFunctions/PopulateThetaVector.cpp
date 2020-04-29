#include <Rcpp.h>
// This function REPLACES for (ii in 1:NN)	ThetaVector[IObsMat[ii, 1:IndN[ii]] ] = Theta[ii]
// Goal is to put estimate of theta in vector with every single observation
// Theta for each country-year appears multiple times - for every observation from that country-year

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector PopulateThetaVector(NumericMatrix M, NumericVector Theta, NumericVector IndN, int length) {
  int nrow = M.nrow();
  NumericVector out(length);
  for (int i = 0; i < nrow; ++i) {
  for (int j = 0; j < IndN(i); ++j) {
      out[M(i, j)-1] = Theta[i];
    }
  }
  return out;
}