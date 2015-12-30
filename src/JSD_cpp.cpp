#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

// Below is a function for calculating JSD in C++

// [[Rcpp::export]]
double JSD_cpp(NumericVector p, NumericVector q) {
  int n = p.length();
    
    // Add 10^(-4) so that there are no zero entries in p or q
    NumericVector p2 = p + 0.0001; // define p2 and q2 to avoid modifying input arguments in R workspace
    NumericVector q2 = q + 0.0001;
    
    // reweight so that p and q sum to one again
    double p_tot = 0;
    double q_tot = 0;
    
    for( int j = 0; j < n; ++j ){
        p_tot = p_tot + p2[ j ];
        q_tot = q_tot + q2[ j ];
    }
    
    for( int j = 0; j < n; ++j ){
        p2[ j ] = p2[ j ] / p_tot;
        q2[ j ] = q2[ j ] / q_tot;
    }
    
    // initialize m
    NumericVector m = 0.5 * (p2 + q2);
    
    // get the KL divergence for p||m and q||m
    double kl_pm = 0;
    double kl_qm = 0;
    
    
    for( int j = 0; j < n; ++j ){
        kl_pm = kl_pm + std::log(p2[ j ] / m[ j ]) * p2[ j ];
        kl_qm = kl_qm + std::log(q2[ j ] / m[ j ]) * q2[ j ];
    }
    
    // get the final JSD result
     
    
   return 0.5 * (kl_pm + kl_qm);
}