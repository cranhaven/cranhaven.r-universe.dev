
#include "array.h"
#include "MatTransMix.h"

void run_Mat_Trans_Full(double *y, int *misc_int, double *misc_double, double *tau, double *la1, double *nu1, double *Mu1, double *invS1, double *invPsi1, double *detS, double *detPsi, double *gamma1, int *id, double *ll, int *conv, double *scale){
		
	double ***Y, **la, **nu, ***Mu, ***invS, ***invPsi, **gamma;

	int p, n, T, K, max_iter, Mu_type, Sigma_type, Psi_type, la_type, trans_type;


	p = misc_int[0];
	T = misc_int[1];
	n = misc_int[2];
	K = misc_int[3];	
	max_iter = misc_int[4];
	Mu_type = misc_int[5];		
	Sigma_type = misc_int[6];	
	Psi_type = misc_int[7];	
	la_type = misc_int[8];	
	trans_type = misc_int[9];	

	MAKE_3ARRAY(Y, p, T, n);
	MAKE_3ARRAY(Mu, p, T, K);
	MAKE_3ARRAY(invS, p, p, K);
	MAKE_3ARRAY(invPsi, T, T, K);
	MAKE_MATRIX(gamma, n, K);
	MAKE_MATRIX(la, K, p);
	MAKE_MATRIX(nu, K, T);
	
	array1to3(p, T, n, y, Y);
	array1to3(p, T, K, Mu1, Mu);
	array1to3(p, p, K, invS1, invS);
	array1to3(T, T, K, invPsi1, invPsi);
	array1to2(n, K, gamma1, gamma);
	array1to2(K, p, la1, la);
	array1to2(K, T, nu1, nu);


	EM_Trans_Full(p, T, n, K, Y, la, nu, max_iter, misc_double, tau, Mu, invS, invPsi, detS, detPsi, gamma, id, ll, conv, Mu_type, Sigma_type, Psi_type, la_type, scale, trans_type);


	array3to1(p, T, n, y, Y);
	array3to1(p, T, K, Mu1, Mu);
	array3to1(p, p, K, invS1, invS);
	array3to1(T, T, K, invPsi1, invPsi);
	array2to1(n, K, gamma1, gamma);
	array2to1(K, p, la1, la);
	array2to1(K, T, nu1, nu);

		
	FREE_3ARRAY(Y);
	FREE_3ARRAY(Mu);
	FREE_3ARRAY(invS);
	FREE_3ARRAY(invPsi);
	FREE_MATRIX(gamma);
	FREE_MATRIX(la);
	FREE_MATRIX(nu);

}

