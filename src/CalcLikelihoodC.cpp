// [[Rcpp::depends("RcppArmadillo", "RcppProgress")]]
#include <RcppArmadillo.h>
#include <cmath>
#define ARMA_64BIT_WORD
using namespace Rcpp ;

// [[Rcpp::export]]
double CalcLikelihoodC( arma::sp_mat dtm, NumericMatrix phi, NumericMatrix theta) {
    int ndocs = dtm.n_rows;
    int ntopics = theta.ncol();
    int nwords = phi.ncol();
    double result = 0;
    
    // for each document...
    for(int d=0; d < ndocs; d++){

        // get document length
        int n = 0;
        
        for(int v = 0; v < nwords; v++){
            
            n = n + dtm( d , v);
            
        }
        
        // get "a" first element in the log likelihood
        
        double a = 0; // first element in the log likelihood
        
       for( int nd = 0; nd < n; nd++ ){

           a = a + std::log( double(n - nd) );
           
        }
        
        // get "b" second element in the log likelihood
        double b = 0;
       
       for( int v =0; v < nwords; v++ ){
           // probablity of words in this document
           double p_word = 0; 
           
           for(int k = 0; k < ntopics; k++){
               
               p_word = p_word + theta( d , k ) * phi( k , v );
               
           }
           
           b = b + dtm( d , v) * std::log(p_word); 
           
       }
       
       // get "c" third element in the log likelihood
       double c = 0; 
       
       for(int v = 0; v < nwords; v++ ){
         
           double x_dv = dtm( d, v );
           
           for( int j = 0; j < x_dv; j++){
               
               c = c + std::log(double(x_dv - j));
               
           }
       }
      
      result = result + (a + b - c);
    }
    return result;
}


