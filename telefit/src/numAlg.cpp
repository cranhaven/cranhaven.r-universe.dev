#include "numAlg.h"

using namespace arma;


mat mcstat2::dgemkmm(const mat& A, const mat& B, const mat& C) {
	int m = A.n_rows;
	int n = A.n_cols;
	int p = B.n_rows;
	int q = B.n_cols;
	int r = C.n_cols;
	
	mat res;
	res = mat(m*p, r, fill::zeros);
	
	mat resBlock;
	resBlock.set_size(q, r);
	
	resBlock = A.at(0,0) * C.rows( 0, q-1 );
	for(int j=1; j<n; j++)
		resBlock += A.at(0,j) * C.rows( j*q, (j+1)*q-1 );
	
	res.rows( 0, p-1 ) = B * resBlock;
	
	for(int i=1; i<m; i++) {
		
		resBlock = A.at(i,0) * C.rows( 0, q-1 );
		for(int j=1; j<n; j++)
			resBlock += A.at(i,j) * C.rows( j*q, (j+1)*q-1 );
		
		res.rows( i*p, (i+1)*p-1 ) += B * resBlock;
	}
	
	return res;
}

mat mcstat2::dgeikmm(int N, mat A, mat B) {
	int m = A.n_rows;
	int n = A.n_cols;
	int p = B.n_cols;
	
	mat res;
	res = mat(N*m, p, fill::zeros);
	
	for(int i=0; i<N; i++)
		res.rows(i*m, (i+1)*m-1) = A * B.rows(i*n, (i+1)*n-1);
	
	return res;
}


//
// Rcpp Exports
//


// [[Rcpp::export]]
arma::mat r_dgeikmm(int N, arma::mat A, arma::mat B) {
	return mcstat2::dgeikmm(N, A, B);
}
