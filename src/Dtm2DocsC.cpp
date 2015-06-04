#include <RcppArmadillo.h>
#include <math.h>
// [[Rcpp::depends("RcppArmadillo", "RcppProgress")]]
//#include <progress.hpp>
using namespace Rcpp ;



//' Convert a DTM to a Character Vector of documents
//' 
//' @description This function takes a sparse matrix (DTM) as input and returns a character vector
//' whose length is equal to the number of rows of the input DTM.
//' @param dtm A sparse Matrix from the matrix package whose rownames correspond to documents and colnames correspond to words
//' @param parallel Do you want to parallelize this function using snowfall? Default is FALSE 
//' @param cpus If parallel is TRUE, the number of threads to use. (Recommendation is 4, for memory's sake)
//' @export
//' @examples
//' Dtm2Docs(dtm=mydtm, parallel=TRUE, cpus=4)
//' @export
// [[Rcpp::export]]
std::vector< std::string> Dtm2DocsC(arma::sp_mat dtm, std::vector< std::string> vocab){
	
	// initialize some variables here
	int n_docs = dtm.n_rows;
	int n_words = dtm.n_cols;
	int n = 0;
	std::vector< std::string > result;
	
	// loop over documents & vocab to repeat words
	for(int d = 0; d < n_docs; d++){
		result[ d ] = "";
		for(int v = 0; v < n_words; v++){
			n = dtm[ d, v ];
			if(n > 0){
				for(int j = 0; j < n; j++){
					result[ d ] = result[ d ] + " " + vocab[ v ];
				}
			}
		}
	}
	return result;
}