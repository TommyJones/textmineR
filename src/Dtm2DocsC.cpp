// [[Rcpp::depends("RcppArmadillo", "RcppProgress")]]
#include <RcppArmadillo.h>
#include <string>
#include <progress.hpp>
#define ARMA_64BIT_WORD
using std::string;
using namespace Rcpp ;

// [[Rcpp::export]]
List Dtm2DocsC(arma::sp_mat dtm, std::vector< std::string> vocab){
	
	// initialize some variables here
	int n_docs = dtm.n_rows;
	int n_words = dtm.n_cols;
	int n = 0;
	List result(n_docs);
	
//	// loop over documents & vocab to repeat words
	for(int d = 0; d < n_docs; d++){
//    // Check for user interrupt every 256 iterations
//    if (d % 256 == 0){
//  		Rcpp::checkUserInterrupt();
//    }
    string tmp = "";
		for(int v = 0; v < n_words; v++){
			n = dtm( d, v );
			for(int j = 0; j < n; j++){
				tmp = tmp + " " + vocab[ v ] + " ";
			}
		}
    result[ d ] = tmp;
	}

	return result;
}