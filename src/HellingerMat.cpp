#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix HellingerMat(NumericMatrix A){
    
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
        
        double result = 0.0;
        
        for(int i = 0; i < cols; i++){
          result = result + (std::sqrt(double(A2(j , i))) - 
            std::sqrt(double(A2(k , i)))) * (std::sqrt(double(A2(j , i))) - 
            std::sqrt(double(A2(k , i))));
        }
                
        answer(j , k) = std::sqrt(double(0.5)) * std::sqrt(result);
        
      }
    }
    
    return(answer);
}