#include <Rcpp.h>
// This function REPLACES for (tt in 1:TT)	VarBeta[tt] 	= 1/(	sum(ThetaVector[VoteStartEnd[tt,1]:VoteStartEnd[tt,2]]^2)	+ 1/S2BetaPrior)
// Goal is to create vector of variance of beta estimates
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector VarBetaFunction(NumericMatrix M, NumericVector ThetaVector, double S2BetaPrior) {
using namespace std;  
int nrow = M.nrow();
  NumericVector out(nrow);
 
   for (int i = 0; i < nrow; ++i) {
   for (int j = 0; j < M(i, 1)- M(i, 0)+1; ++j) {
       out[i] = out[i] + pow(ThetaVector[M(i, 0)+j-1], 2);
     }
     }
  return 1/(out+S2BetaPrior);
}