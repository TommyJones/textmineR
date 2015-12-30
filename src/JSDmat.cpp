#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix JSDmat(NumericMatrix A){
    
   NumericMatrix A2 = A; // don't overwrite inputs
    
    int rows = A2.nrow();
    int cols = A2.ncol();
    NumericMatrix answer(rows,rows);
    
    // add 10^(-4) so that there are no zero entries
    for( int j = 0; j < rows; j++){
      for(int k = 0; k < cols; k++){
        A2(j, k) = A2(j, k) + 0.0001;
      }
    }
    
    // reweight so each row sums to one
    for( int j = 0; j < rows; j++){
      double row_tot = 0.0;
      
      for(int k = 0; k < cols; k++){
       row_tot = row_tot + A2(j, k);
      }
      
      for(int k = 0; k < cols; k++){
        A2(j , k) = A2(j , k) / row_tot;
      }
    }
    
    
    // Do the main calculations
    for(int j = 0; j < rows - 1; j++){
      
      for(int k = j + 1; k < rows; k++){
        
        NumericVector p(cols), q(cols);
        
        for(int i = 0; i < cols; i++){
          p[ i ] = A2(j , i);
          q[ i ] = A2(k , i);
        }
        
        NumericVector m = 0.5 * (p + q);
        
        // get the KL divergence for p||m and q||m
        double kl_pm = 0;
        double kl_qm = 0;
        
        for( int i = 0; i < cols; ++i ){
          kl_pm = kl_pm + std::log(p[ i ] / m[ i ]) * p[ i ];
          kl_qm = kl_qm + std::log(q[ i ] / m[ i ]) * q[ i ];
        }
        
        answer(j , k) = 0.5 * (kl_pm + kl_qm);
        
      }
    }
    
    return(answer);
}