// [[Rcpp::depends("RcppArmadillo", "RcppProgress")]]
#include <RcppArmadillo.h>
#include <string>
#define ARMA_64BIT_WORD
using std::string;
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
List Dtm2DocsC(arma::sp_mat dtm, std::vector< std::string> vocab){
	
	// initialize some variables here
	int n_docs = dtm.n_rows;
	int n_words = dtm.n_cols;
	int n = 0;
	List result(n_docs);
	
//	// loop over documents & vocab to repeat words
	for(int d = 0; d < n_docs; d++){
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