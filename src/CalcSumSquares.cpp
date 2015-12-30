// [[Rcpp::depends("RcppArmadillo", "RcppProgress")]]
#include <RcppArmadillo.h>
#include <cmath>
#define ARMA_64BIT_WORD
using namespace Rcpp ;

// [[Rcpp::export]]
NumericVector CalcSumSquares( arma::sp_mat dtm, NumericMatrix phi, NumericMatrix theta, NumericVector ybar) {
    
    int ndocs = dtm.n_rows; // number of documents
    int ntopics = theta.ncol(); // number of topics
    int nwords = phi.ncol(); // number of words
    NumericVector result(2); // final result
    double SSE = 0; // sum of squared errors across all documents
    double SST = 0; // total sum of squares across all documents
    
    
    // for each document...
    for(int d=0; d<ndocs; d++){
               
       // get document length
       int n = 0; // document length
        for(int v = 0; v < nwords; v++){
           
           n = n + dtm( d , v);
           
       }
       
       // a single loop to optimize calculation of sse and sst
       double sse = 0;
       double sst = 0;
       
       for( int v = 0; v < nwords; v++ ){
           double yhat = 0;
           
           for( int k = 0; k < ntopics; k++ ){
               yhat = yhat + theta( d , k ) * phi( k , v );
           }
           
           sse = sse + ((dtm( d , v) - n * yhat) * (dtm( d , v) - n * yhat));
           
           sst = sst + ((dtm( d , v) - ybar[ v ]) * (dtm( d , v) - ybar[ v ]));
               
       }
       
       SSE = SSE + sse;
       
       SST = SST + sst;
    }
    
    result[ 0 ] = SSE;
    result[ 1 ] = SST;
    
    return result;

    
}
