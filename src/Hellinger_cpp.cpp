#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;


// [[Rcpp::export]]
double Hellinger_cpp(NumericVector p, NumericVector q){
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
        
    // get the final result
    double result = 0.0;
    
    for( int j = 0; j < n; j++ ){
      result = result + (sqrt(p2[ j ]) - sqrt(q2[ j ])) * (sqrt(p2[ j ]) - sqrt(q2[ j ]));
    }
    
    result = sqrt(0.5) * sqrt(result);
  
  return result;
}