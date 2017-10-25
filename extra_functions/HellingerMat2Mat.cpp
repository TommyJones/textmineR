#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix HellingerMat2Mat(NumericMatrix A, NumericMatrix B){
  
  NumericMatrix A2 = A; // don't overwrite inputs
  NumericMatrix B2 = B;
  
  int rows = A2.nrow();
  int cols = A2.ncol();
  
  int rows_b = B2.nrow();
  int cols_b = B2.ncol();
  
  NumericMatrix answer(rows,rows_b);
  
  // add 10^(-4) so that there are no zero entries
  for(int j = 0; j < rows; j++){
    for(int k = 0; k < cols; k++){
      A2(j, k) = A2(j, k) + 0.0001;
    }
  }
  
  for(int j = 0; j < rows_b; j++){
    for(int k = 0; k < cols_b; k++){
      B2(j, j ) = B2(j, k) + 0.0001;
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
  
  
  for( int j = 0; j < rows_b; j++){
    double row_tot = 0.0;
    
    for(int k = 0; k < cols_b; k++){
      row_tot = row_tot + B2(j, k);
    }
    
    for(int k = 0; k < cols_b; k++){
      B2(j , k) = B2(j , k) / row_tot;
    }
  }
  
  
  // Do the main calculations
  for(int j = 0; j < rows; j++){
    
    for(int k = 0; k < rows_b; k++){
      
      double result = 0.0;
      
      for(int i = 0; i < cols; i++){
        result = result + (std::sqrt(double(A2(j , i))) - 
          std::sqrt(double(B2(k , i)))) * (std::sqrt(double(A2(j , i))) - 
          std::sqrt(double(B2(k , i))));
      }
      
      answer(j , k) = std::sqrt(double(0.5)) * std::sqrt(result);
      
    }
  }
  
  return(answer);
}