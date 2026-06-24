

#include "array.h"
#include "math.h"
#include "MatTransMix.h"
 
#define Inf 1e+140
#define pi 3.14159265359


#include <R.h>
#include <Rmath.h>




void cpyk(double ***a, int nrows, int ncols, int k, double **b)
{
  int i,j;
  for(i=0;i<nrows;i++) {
    for (j=0;j<ncols;j++) {
      b[i][j]=a[i][j][k];
    }
  }

}




void cpy(double **a, int nrows, int ncols, double **b)
{
  int i,j;
  for(i=0;i<nrows;i++) {
    for (j=0;j<ncols;j++) {
      b[i][j]=a[i][j];
    }
  }

}



void matxvec(double **a, int arows, int acols,
		double *x, int xrows, double *y)
{
  int i, k;
  
  for (i=0; i<arows; i++){
    y[i] = 0;
    for (k=0; k<acols; k++){
      y[i] += a[i][k] * x[k];
    }
  }

}



void cpyk2(double **a, int nrows, int ncols, double ***b, int k)
{
  int i,j;
  for(i=0;i<nrows;i++) {
    for (j=0;j<ncols;j++) {
      b[i][j][k]=a[i][j];
    }
  }

}




void Trans_trans(int p, int T, double *la, double *nu, double **Y, double **MY, int trans_type){

	int j, t;

	if(trans_type == 1){
  		for (j=0; j<p; j++){
    			for(t=0; t<T; t++) {
    				if(Y[j][t] >= 0){
    		 			if (fabs(la[j] + nu[t])<1e-12){
						MY[j][t] = log(Y[j][t] + 1.0);
    					}
					else{
			
						MY[j][t] = (pow((Y[j][t] + 1.0), la[j]+ nu[t]) - 1.0) / (la[j] + nu[t]);
					}
    				}
    				else{
    					if (fabs(la[j] + nu[t] - 2.0)<1e-12){
		 				MY[j][t] = -log(-Y[j][t] + 1.0);
			
    					}
					else{
						MY[j][t] = -(pow((-Y[j][t] + 1.0), 2.0- la[j]- nu[t]) - 1.0) / (2.0- la[j] - nu[t]);
					}
		
    				}
	     
    			}
      
  		}
 	}

	else if(trans_type == 2){
 
		for (j=0; j<p; j++){
			for(t=0; t<T; t++) {
				if (fabs(la[j] + nu[t])<1e-12){
      				
					MY[j][t] = Y[j][t];
      				}
   				else{
					MY[j][t] = (exp(Y[j][t] * (la[j]  + nu[t])) - 1) / (la[j] + nu[t]);
      				}
    			}
      
  		}

	}

	else if(trans_type == 0){
 
		for (j=0; j<p; j++){
			for(t=0; t<T; t++) {
				MY[j][t] = Y[j][t];
      			}
   		}
	}

}

void Trans_trans_whole(int n, int p, int T, double *la, double *nu, double ***Y, double ***MY, int trans_type){

	int i, j, t;
	if(trans_type == 1){
 		for(i=0; i<n; i++){
 			for (j=0; j<p; j++){
    				for(t=0; t<T; t++) {

    					if(Y[j][t][i] >= 0){
    		 				if (fabs(la[j] + nu[t])<1e-12){
							MY[j][t][i] = log(Y[j][t][i] + 1.0);
    						}
						else{
			
							MY[j][t][i] = (pow((Y[j][t][i] + 1.0), la[j]+ nu[t]) - 1.0) / (la[j] + nu[t]);
						}
    					}
    					else{
    						if (fabs(la[j] + nu[t] - 2.0)<1e-12){
							MY[j][t][i] = -log(-Y[j][t][i] + 1.0);
						}
						else{
							MY[j][t][i] = -(pow((-Y[j][t][i] + 1.0), 2.0- la[j]- nu[t]) - 1.0) / (2.0- la[j] - nu[t]);

						}
		
    					}
       
    				}    
  			}
 		}
 	}
 	else if(trans_type == 2){
 		for(i=0; i<n; i++){

  			for (j=0; j<p; j++){

      				for(t=0; t<T; t++) {

    					if (fabs(la[j] + nu[t])<1e-12){
						MY[j][t][i] = Y[j][t][i];
      					}
    					else{
						MY[j][t][i] = (exp(Y[j][t][i] * (la[j] + nu[t])) - 1) / (la[j] + nu[t]);
      					}
    				}
      
  			}

 		}

 	}
 	else if(trans_type == 0){
 		for(i=0; i<n; i++){

  			for (j=0; j<p; j++){
    				
      				for(t=0; t<T; t++) {
					MY[j][t][i] = Y[j][t][i];
      				}
    			}
 		}

 	}

  
}

double mGpdf_Trans_Full(int p, int T, double *la, double *nu, double **Y, double **Mu, double **invS, double **invPsi, double detS, double detPsi, int trans_type){

	int j, t;
	double trace = 0.0, dens, jac = 0.0;
	double **MY, **tMY, **temp1, **temp2, **maha;

	MAKE_MATRIX(maha, p, p);
	MAKE_MATRIX(temp1, p, T);
	MAKE_MATRIX(temp2, p, T);
	MAKE_MATRIX(tMY, T, p);	
	MAKE_MATRIX(MY, p, T);


	
  	Trans_trans(p, T, la, nu, Y, MY, trans_type);
	mat_(p, T, MY, Mu);

	tA(MY, T, p, tMY);

	multiply(invS, p, p, MY, p, T, temp1);
	
	multiply(temp1, p, T, invPsi, T, T, temp2);	

	multiply(temp2, p, T, tMY, T, p, maha);	


	for(j=0; j<p; j++){	 

		trace += maha[j][j];
	}
		
	dens = 1.0 / pow((2*pi), p*T/2.0) / pow(detS, T/2.0) / pow(detPsi, p/2.0) * exp(-1.0 / 2.0 * trace);

 	if(trans_type == 1){
		jac = 1.0;
		for(j=0; j<p; j++){

			for(t=0; t<T; t++){

				if(Y[j][t] >= 0){
					jac = jac * pow(Y[j][t] + 1.0, la[j] + nu[t] - 1.0);
				}
				else{
					jac = jac * pow(-Y[j][t] + 1.0, 1.0 - la[j] - nu[t]);

				}
	
			}	
		}
	}
	else if(trans_type == 2){
		jac = 0.0;

		for(j=0; j<p; j++){

			for(t=0; t<T; t++){

				jac = jac + (la[j] + nu[t]) * Y[j][t];
	
			}	
		}
		jac = exp(jac); 

	}

	else if(trans_type == 0){
		jac = 1.0;
	}


	dens = dens * jac;

	FREE_MATRIX(maha);
	FREE_MATRIX(temp1);
	FREE_MATRIX(temp2);
	FREE_MATRIX(tMY);
	FREE_MATRIX(MY);
	return dens;
}




double mGloglik_Trans_Full(int p, int T, int n, int K, double ***Y, double **la, double **nu, double *tau, double ***Mu, double ***invS, double ***invPsi, double *detS, double *detPsi, double *scale, int trans_type){

	int i,k,j1,j2,j,t;
	double loglik = 0.0;
	double ll, dens = 0.0;
	double **Yi, **Yi_new, **Muk, **invSk, **invPsik, **Muk_new, **invSk_new, *la_new, *nu_new;

	MAKE_MATRIX(Yi_new, p, T);	
	MAKE_MATRIX(Yi, p, T);
	MAKE_MATRIX(Muk, p, T);
	MAKE_MATRIX(invSk, p, p);
	MAKE_MATRIX(invPsik, T, T);
	MAKE_MATRIX(Muk_new, p, T);
	MAKE_MATRIX(invSk_new, p, p);
	MAKE_VECTOR(la_new, p);
	MAKE_VECTOR(nu_new, T);

	for(i=0; i<n; i++){	 

		ll = 0;

		for(k=0; k<K; k++){

			cpyk(Y, p, T, i, Yi);
			cpyk(Mu, p, T, k, Muk);
			cpyk(invS, p, p, k, invSk);
			cpyk(invPsi, T, T, k, invPsik);

			for(j=0; j<p; j++){
				for(t=0; t<T; t++){
					Yi_new[j][t] = Yi[j][t] * scale[0];
 			
				}
			}

			
			for(j=0; j<p; j++){
				for(t=0; t<T; t++){
					Muk_new[j][t] = Muk[j][t] * scale[0];
 			
				}
			}
			
			for(j1=0; j1<p; j1++){
				for(j2=0; j2<p; j2++){
					invSk_new[j1][j2] = invSk[j1][j2] / pow(scale[0], 2.0);
 			
				}
			}

			for(j=0; j<p; j++){
				la_new[j] = la[k][j];

			}
			for(t=0; t<T; t++){
				nu_new[t] = nu[k][t];

			}
			dens = mGpdf_Trans_Full(p, T, la_new, nu_new, Yi_new, Muk_new, invSk_new, invPsik, detS[k]*pow(scale[0], 2.0*p), detPsi[k], trans_type);

			
			ll += tau[k] * dens;


		}

		loglik += log(ll);
	}
	//printf(" scale %lf  %lf \n", scale[0], loglik);

	FREE_VECTOR(la_new);
	FREE_VECTOR(nu_new);
	FREE_MATRIX(Yi_new);
	FREE_MATRIX(Yi);
	FREE_MATRIX(Muk);
	FREE_MATRIX(invSk);
	FREE_MATRIX(invPsik);
	FREE_MATRIX(Muk_new);
	FREE_MATRIX(invSk_new);

	return loglik;
}





void Estep_Trans_Full(int p, int T, int n, int K, double ***Y, double **la, double **nu, double *tau, double ***Mu, double ***invS, double ***invPsi, double *detS, double *detPsi, double **gamma, int trans_type){

	int i,k;		
	double dens =0.0, *sum_gamma, **Yi, **Muk, **invSk, **invPsik;

	MAKE_MATRIX(Yi, p, T);
	MAKE_MATRIX(Muk, p, T);
	MAKE_MATRIX(invSk, p, p);
	MAKE_MATRIX(invPsik, T, T);

	MAKE_VECTOR(sum_gamma, n);

	anull(sum_gamma, n);

	if(K == 1){

		for(i=0; i<n; i++){	 
			gamma[i][0] = 1.0;

		}
	}
	else{

		for(i=0; i<n; i++){	 



			for(k=0; k<K; k++){
				cpyk(Y, p, T, i, Yi);
				cpyk(Mu, p, T, k, Muk);
				cpyk(invS, p, p, k, invSk);
				cpyk(invPsi, T, T, k, invPsik);

				dens = mGpdf_Trans_Full(p, T, la[k], nu[k], Yi, Muk, invSk, invPsik, detS[k], detPsi[k], trans_type);

				gamma[i][k] = tau[k] * dens;

				sum_gamma[i] += gamma[i][k];


			}

	
			for(k=0; k<K; k++){

				gamma[i][k] = gamma[i][k] / sum_gamma[i];


			}


		}

	}
	FREE_MATRIX(Yi);
	FREE_MATRIX(Muk);
	FREE_MATRIX(invSk);
	FREE_MATRIX(invPsik);

	FREE_VECTOR(sum_gamma);

}






double Q1(int n, int p, int T, double *la_nonzero, double *nu, double ***Y, double *gamma_k, double **invSk, double **invPsik, int Mu_type, int trans_type, int la_type){
  
	int i, j, t;
	double res, jac, sum_gamma, det;
	double *la;
	double **Sk, **temp1, **temp2, **temp3;
	double ***MY, **Muk, *Eig1, *Eig2, **MYi, **tMYi, **invPsik0, *Muk_vec, *MY_vec, *Eig, **L, **matconst, **invconst;


	MAKE_VECTOR(Muk_vec, p+T-1);
	MAKE_VECTOR(MY_vec, p+T-1);
	MAKE_MATRIX(matconst, p+T-1, p+T-1);
	MAKE_MATRIX(invconst, p+T-1, p+T-1);
	MAKE_MATRIX(L, p+T-1, p+T-1);
	MAKE_VECTOR(Eig, p+T-1);


		
	MAKE_VECTOR(la, p);
	MAKE_VECTOR(Eig1, T);
	MAKE_VECTOR(Eig2, p);
	MAKE_3ARRAY(MY, p,T,n);
	MAKE_MATRIX(MYi, p, T);
	MAKE_MATRIX(tMYi, T, p);
	MAKE_MATRIX(Sk, p, p);
	MAKE_MATRIX(invPsik0, T, T);
	MAKE_MATRIX(temp1, p, T);
	MAKE_MATRIX(temp2, p, T);
	MAKE_MATRIX(temp3, p, p);
	MAKE_MATRIX(Muk, p, T);

	Anull(matconst, p+T-1, p+T-1);
	for(i=0; i<p; i++){	
		for(j=0; j<p; j++){

			if(i == j){
				matconst[i][j] = T*1.0;
			}

		}
	}

	for(i=p; i<p+T-1; i++){	
		for(j=p; j<p+T-1; j++){

			if(i == j){
				matconst[i][j] = p*1.0;
			}

		}
	}

	for(i=0; i<p; i++){	
		for(j=p; j<p+T-1; j++){
			matconst[i][j] = 1.0;

		}
	}
	for(i=p; i<p+T-1; i++){	
		for(j=0; j<p; j++){
			matconst[i][j] = 1.0;

		}
	}
	anull(Eig, p+T-1);


	EigValDec(p+T-1, Eig, matconst, &det);

	Anull(L, p+T-1, p+T-1);

	for (t=0; t<p+T-1; t++){
		L[t][t] = 1.0 / Eig[t];
		
	}
	
	XAXt(matconst, p+T-1, L, invconst);



	cpy(invPsik, T, T, invPsik0);



	for(j=0; j<p; j++){

		la[j] = la_nonzero[j];
	}	


	sum_gamma = 0;
			
	for(i=0; i<n; i++){


		sum_gamma += gamma_k[i];
	}


	Trans_trans_whole(n, p, T, la, nu, Y, MY, trans_type);
	res = 0;
	Anull(Muk, p, T);
	if(Mu_type == 2){

		for(i=0; i<n; i++){
			for(j=0; j<p; j++){

				for(t=0; t<T; t++){
			
					Muk[j][t] += gamma_k[i] * MY[j][t][i] / sum_gamma;


				}
			}
		}


	}
	else if(Mu_type == 1){

		anull(MY_vec, p+T-1);
	

		for(j=0; j<p; j++){
			for(i=0; i<n; i++){
				for(t=0; t<T; t++){		
					MY_vec[j] += gamma_k[i] * MY[j][t][i] / sum_gamma;



				}
			}
		}


		for(t=p; t<p+T-1; t++){
			for(i=0; i<n; i++){
				for(j=0; j<p; j++){		
					MY_vec[t] += gamma_k[i] * MY[j][t-p][i] / sum_gamma;


				}
			}
		}


		matxvec(invconst, p+T-1, p+T-1, MY_vec, p+T-1, Muk_vec);



		for(j=0; j<p; j++){

			for(t=0; t<T; t++){
			
				if(t<T-1){
					Muk[j][t] = Muk_vec[j] +  Muk_vec[p+t];

				}
				else{
					Muk[j][t] = Muk_vec[j];
				}
			}
		}


	}

	for(i=0; i<n; i++){

		cpyk(MY, p, T, i, MYi);
		mat_(p, T, MYi, Muk);
	
		tA(MYi, T, p, tMYi);

		multiply(invSk, p, p, MYi, p, T, temp1);

		multiply(temp1, p, T, invPsik, T, T, temp2);

		multiply(temp2, p, T, tMYi, T, p, temp3);


		for(j=0; j<p; j++){
			res += -1.0/2.0 * gamma_k[i] * temp3[j][j];

		}

	}
	if(trans_type == 1){

		for(i=0; i<n; i++){

			jac = 0;

			for(j=0; j<p; j++){

				for(t=0; t<T; t++){

					if(Y[j][t][i] >= 0){
						jac = jac + (la[j] + nu[t] - 1) * log(Y[j][t][i] + 1);
					}
					else{
						jac = jac - (la[j] + nu[t] - 1) *  log(-Y[j][t][i] + 1);

					}
	
				}	
			}

			res = res + gamma_k[i] * jac;
		}
	}
	else if(trans_type == 2){
		for(i=0; i<n; i++){

			jac = 0;

			for(j=0; j<p; j++){

				for(t=0; t<T; t++){

					
					jac = jac + Y[j][t][i] * (la[j] + nu[t]);
	
				}	
			}

			res = res + gamma_k[i] * jac;
		}

	}



	FREE_VECTOR(la);
	FREE_MATRIX(MYi);
	FREE_3ARRAY(MY);
	FREE_MATRIX(tMYi);
	FREE_VECTOR(Eig1);
	FREE_VECTOR(Eig2);

	FREE_MATRIX(Sk);
	FREE_MATRIX(invPsik0);
	FREE_MATRIX(temp1);
	FREE_MATRIX(temp2);
	FREE_MATRIX(temp3);
	FREE_MATRIX(Muk);
	FREE_MATRIX(matconst);
	FREE_MATRIX(invconst);
	FREE_MATRIX(L);
	FREE_VECTOR(Eig);
	FREE_VECTOR(Muk_vec);
	FREE_VECTOR(MY_vec);
	return(-res);

}



double Q1_same(int n, int p, int T, double *la_nonzero, double *nu, double ***Y, double *gamma_k, double **invSk, double **invPsik, int Mu_type, int trans_type, int la_type){
  
	int i, j, t;
	double res, jac, sum_gamma, det;
	double *la;
	double **Sk, **temp1, **temp2, **temp3;
	double ***MY, **Muk, *Eig1, *Eig2, **MYi, **tMYi, **invPsik0, *Muk_vec, *MY_vec, *Eig, **L, **matconst, **invconst;


	MAKE_VECTOR(Muk_vec, p+T-1);
	MAKE_VECTOR(MY_vec, p+T-1);
	MAKE_MATRIX(matconst, p+T-1, p+T-1);
	MAKE_MATRIX(invconst, p+T-1, p+T-1);
	MAKE_MATRIX(L, p+T-1, p+T-1);
	MAKE_VECTOR(Eig, p+T-1);


		
	MAKE_VECTOR(la, p);
	MAKE_VECTOR(Eig1, T);
	MAKE_VECTOR(Eig2, p);
	MAKE_3ARRAY(MY, p,T,n);
	MAKE_MATRIX(MYi, p, T);
	MAKE_MATRIX(tMYi, T, p);
	MAKE_MATRIX(Sk, p, p);
	MAKE_MATRIX(invPsik0, T, T);
	MAKE_MATRIX(temp1, p, T);
	MAKE_MATRIX(temp2, p, T);
	MAKE_MATRIX(temp3, p, p);
	MAKE_MATRIX(Muk, p, T);

	Anull(matconst, p+T-1, p+T-1);
	for(i=0; i<p; i++){	
		for(j=0; j<p; j++){

			if(i == j){
				matconst[i][j] = T*1.0;
			}

		}
	}

	for(i=p; i<p+T-1; i++){	
		for(j=p; j<p+T-1; j++){

			if(i == j){
				matconst[i][j] = p*1.0;
			}

		}
	}

	for(i=0; i<p; i++){	
		for(j=p; j<p+T-1; j++){
			matconst[i][j] = 1.0;

		}
	}
	for(i=p; i<p+T-1; i++){	
		for(j=0; j<p; j++){
			matconst[i][j] = 1.0;

		}
	}
	anull(Eig, p+T-1);


	EigValDec(p+T-1, Eig, matconst, &det);

	Anull(L, p+T-1, p+T-1);

	for (t=0; t<p+T-1; t++){
		L[t][t] = 1.0 / Eig[t];
		
	}
	
	XAXt(matconst, p+T-1, L, invconst);



	cpy(invPsik, T, T, invPsik0);

	for(j=0; j<p; j++){

		la[j] = la_nonzero[0];
	}	


	sum_gamma = 0;
			
	for(i=0; i<n; i++){


		sum_gamma += gamma_k[i];
	}


	Trans_trans_whole(n, p, T, la, nu, Y, MY, trans_type);
	res = 0;
	Anull(Muk, p, T);
	if(Mu_type == 2){

		for(i=0; i<n; i++){
			for(j=0; j<p; j++){

				for(t=0; t<T; t++){
			
					Muk[j][t] += gamma_k[i] * MY[j][t][i] / sum_gamma;


				}
			}
		}


	}
	else if(Mu_type == 1){

		anull(MY_vec, p+T-1);
	

		for(j=0; j<p; j++){
			for(i=0; i<n; i++){
				for(t=0; t<T; t++){		
					MY_vec[j] += gamma_k[i] * MY[j][t][i] / sum_gamma;



				}
			}
		}


		for(t=p; t<p+T-1; t++){
			for(i=0; i<n; i++){
				for(j=0; j<p; j++){		
					MY_vec[t] += gamma_k[i] * MY[j][t-p][i] / sum_gamma;


				}
			}
		}


		matxvec(invconst, p+T-1, p+T-1, MY_vec, p+T-1, Muk_vec);



		for(j=0; j<p; j++){

			for(t=0; t<T; t++){
			
				if(t<T-1){
					Muk[j][t] = Muk_vec[j] +  Muk_vec[p+t];

				}
				else{
					Muk[j][t] = Muk_vec[j];
				}
			}
		}


	}

	for(i=0; i<n; i++){

		cpyk(MY, p, T, i, MYi);
		mat_(p, T, MYi, Muk);
	
		tA(MYi, T, p, tMYi);

		multiply(invSk, p, p, MYi, p, T, temp1);

		multiply(temp1, p, T, invPsik, T, T, temp2);

		multiply(temp2, p, T, tMYi, T, p, temp3);


		for(j=0; j<p; j++){
			res += -1.0/2.0 * gamma_k[i] * temp3[j][j];

		}

	}

	if(trans_type == 1){
		for(i=0; i<n; i++){

			jac = 0;

			for(j=0; j<p; j++){

				for(t=0; t<T; t++){

					if(Y[j][t][i] >= 0){
						jac = jac + (la[j] + nu[t] - 1) * log(Y[j][t][i] + 1);
					}
					else{
						jac = jac - (la[j] + nu[t] - 1) *  log(-Y[j][t][i] + 1);
	
					}
	
				}	
			}

			res = res + gamma_k[i] * jac;
		}
	}
	else if(trans_type == 2){
		for(i=0; i<n; i++){

			jac = 0;

			for(j=0; j<p; j++){

				for(t=0; t<T; t++){

					
					jac = jac + Y[j][t][i] * (la[j] + nu[t]);
	
				}	
			}

			res = res + gamma_k[i] * jac;
		}

	}

	for(j=0; j<p; j++){

		la_nonzero[0] = la[0];
	}	




	FREE_VECTOR(la);
	FREE_MATRIX(MYi);
	FREE_3ARRAY(MY);
	FREE_MATRIX(tMYi);
	FREE_VECTOR(Eig1);
	FREE_VECTOR(Eig2);

	FREE_MATRIX(Sk);
	FREE_MATRIX(invPsik0);
	FREE_MATRIX(temp1);
	FREE_MATRIX(temp2);
	FREE_MATRIX(temp3);
	FREE_MATRIX(Muk);
	FREE_MATRIX(matconst);
	FREE_MATRIX(invconst);
	FREE_MATRIX(L);
	FREE_VECTOR(Eig);
	FREE_VECTOR(Muk_vec);
	FREE_VECTOR(MY_vec);
	return(-res);

}



double Q2(int n, int p, int T, double *nu_nonzero, double *la, double ***Y, double *gamma_k, double **invSk, double **invPsik, int Mu_type, int trans_type){
  
	int i, j, t;
	double res, jac, sum_gamma, det;
	double *nu;
	double **Sk, **temp1, **temp2, **temp3;
	double ***MY, **Muk, *Eig1, *Eig2, **MYi, **tMYi, **invPsik0, *Muk_vec, *MY_vec, *Eig, **L, **matconst, **invconst;

	MAKE_VECTOR(Muk_vec, p+T-1);
	MAKE_VECTOR(MY_vec, p+T-1);
	MAKE_MATRIX(matconst, p+T-1, p+T-1);
	MAKE_MATRIX(invconst, p+T-1, p+T-1);
	MAKE_MATRIX(L, p+T-1, p+T-1);
	MAKE_VECTOR(Eig, p+T-1);


		
	MAKE_VECTOR(nu, T);
	MAKE_VECTOR(Eig1, T);
	MAKE_VECTOR(Eig2, p);
	MAKE_3ARRAY(MY, p,T,n);
	MAKE_MATRIX(MYi, p, T);
	MAKE_MATRIX(tMYi, T, p);
	MAKE_MATRIX(Sk, p, p);
	MAKE_MATRIX(invPsik0, T, T);
	MAKE_MATRIX(temp1, p, T);
	MAKE_MATRIX(temp2, p, T);
	MAKE_MATRIX(temp3, p, p);

	MAKE_MATRIX(Muk, p, T);


	Anull(matconst, p+T-1, p+T-1);
	for(i=0; i<p; i++){	
		for(j=0; j<p; j++){

			if(i == j){
				matconst[i][j] = T*1.0;
			}

		}
	}

	for(i=p; i<p+T-1; i++){	
		for(j=p; j<p+T-1; j++){

			if(i == j){
				matconst[i][j] = p*1.0;
			}

		}
	}

	for(i=0; i<p; i++){	
		for(j=p; j<p+T-1; j++){
			matconst[i][j] = 1.0;

		}
	}
	for(i=p; i<p+T-1; i++){	
		for(j=0; j<p; j++){
			matconst[i][j] = 1.0;

		}
	}
	anull(Eig, p+T-1);


	EigValDec(p+T-1, Eig, matconst, &det);

	Anull(L, p+T-1, p+T-1);

	for (t=0; t<p+T-1; t++){
		L[t][t] = 1.0 / Eig[t];
		
	}
	
	XAXt(matconst, p+T-1, L, invconst);


	cpy(invPsik, T, T, invPsik0);

	for(t=0; t<T; t++){

		nu[t] = nu_nonzero[t];
	}	


	sum_gamma = 0;
			
	for(i=0; i<n; i++){


		sum_gamma += gamma_k[i];
	}


	Trans_trans_whole(n, p, T, la, nu, Y, MY, trans_type);
	res = 0;
	Anull(Muk, p, T);

	if(Mu_type == 2){

		for(i=0; i<n; i++){
			for(j=0; j<p; j++){

				for(t=0; t<T; t++){
			
					Muk[j][t] += gamma_k[i] * MY[j][t][i] / sum_gamma;


				}
			}
		}


	}
	else if(Mu_type == 1){

		anull(MY_vec, p+T-1);
	

		for(j=0; j<p; j++){
			for(i=0; i<n; i++){
				for(t=0; t<T; t++){		
					MY_vec[j] += gamma_k[i] * MY[j][t][i] / sum_gamma;



				}
			}
		}


		for(t=p; t<p+T-1; t++){
			for(i=0; i<n; i++){
				for(j=0; j<p; j++){		
					MY_vec[t] += gamma_k[i] * MY[j][t-p][i] / sum_gamma;


				}
			}
		}


		matxvec(invconst, p+T-1, p+T-1, MY_vec, p+T-1, Muk_vec);



		for(j=0; j<p; j++){

			for(t=0; t<T; t++){
			
				if(t<T-1){
					Muk[j][t] = Muk_vec[j] +  Muk_vec[p+t];

				}
				else{
					Muk[j][t] = Muk_vec[j];
				}
			}
		}


	}





	for(i=0; i<n; i++){

		cpyk(MY, p, T, i, MYi);
		mat_(p, T, MYi, Muk);
	
		tA(MYi, T, p, tMYi);

		multiply(invSk, p, p, MYi, p, T, temp1);

		multiply(temp1, p, T, invPsik, T, T, temp2);

		multiply(temp2, p, T, tMYi, T, p, temp3);


		for(j=0; j<p; j++){
			res += -1.0/2.0 * gamma_k[i] * temp3[j][j];

		}

	}


	if(trans_type == 1){
		for(i=0; i<n; i++){

			jac = 0;

			for(j=0; j<p; j++){

				for(t=0; t<T; t++){

					if(Y[j][t][i] >= 0){
						jac = jac + (la[j] + nu[t] - 1) * log(Y[j][t][i] + 1);
					}	
					else{
						jac = jac - (la[j] + nu[t] - 1) * log(-Y[j][t][i] + 1);

					}
	
				}	
			}

			res = res + gamma_k[i] * jac;
		}
	}
	else if(trans_type == 2){
		for(i=0; i<n; i++){

			jac = 0;

			for(j=0; j<p; j++){

				for(t=0; t<T; t++){

					
					jac = jac + Y[j][t][i] * (la[j] + nu[t]);
	
				}	
			}

			res = res + gamma_k[i] * jac;
		}

	}

	FREE_VECTOR(nu);
	FREE_MATRIX(MYi);
	FREE_3ARRAY(MY);
	FREE_MATRIX(tMYi);
	FREE_VECTOR(Eig1);
	FREE_VECTOR(Eig2);
	FREE_MATRIX(Sk);
	FREE_MATRIX(invPsik0);
	FREE_MATRIX(temp1);
	FREE_MATRIX(temp2);
	FREE_MATRIX(temp3);
	FREE_MATRIX(matconst);
	FREE_MATRIX(invconst);
	FREE_MATRIX(L);
	FREE_VECTOR(Eig);
	FREE_VECTOR(Muk_vec);
	FREE_VECTOR(MY_vec);
	FREE_MATRIX(Muk);


	return(-res);


}





double Mstep_Trans_Full(int p, int T, int n, int K, double *misc_double, double ***Y, double **la, double **nu, double **gamma, double ***invS, double ***Mu, double ***invPsi, double *detS, double *detPsi, double *tau, int Mu_type, int Sigma_type, int Psi_type, int la_type, int trans_type){

	int i,j,k,t;
	double *Q_value, Q_value0, min_value, eps, det, *sum_gamma, *gamma_k, **Psi, **S, **temp1, **temp2, **temp3, **temp4, **invPsik, *Eig, **L;
	double **Muk, **invSk, **matconst, **invconst, **MYi, **tMYi, ***MY;
	double *Eig1, *Eig2;
	double **L1, **L2, *Muk_vec, *MY_vec;

	MAKE_VECTOR(Muk_vec, p+T-1);
	MAKE_VECTOR(MY_vec, p+T-1);
	MAKE_MATRIX(matconst, p+T-1, p+T-1);
	MAKE_MATRIX(invconst, p+T-1, p+T-1);
	MAKE_MATRIX(L, p+T-1, p+T-1);
	MAKE_VECTOR(Eig, p+T-1);


	MAKE_3ARRAY(MY, p,T,n);
	MAKE_MATRIX(MYi, p, T);
	MAKE_VECTOR(sum_gamma, K);
	MAKE_VECTOR(gamma_k, n);
	MAKE_VECTOR(Eig1, T);
	MAKE_VECTOR(Eig2, p);
	MAKE_MATRIX(Psi, T, T);
	MAKE_MATRIX(S, p, p);
	MAKE_MATRIX(tMYi, T, p);
	MAKE_MATRIX(temp1, p, T);
	MAKE_MATRIX(temp2, p, p);
	MAKE_MATRIX(temp3, T, p);
	MAKE_MATRIX(temp4, T, T);
	MAKE_MATRIX(L1, T, T);
	MAKE_MATRIX(L2, p, p);
	MAKE_MATRIX(invPsik, T, T);
	MAKE_MATRIX(invSk, p, p);
	MAKE_MATRIX(Muk, p, T);
	MAKE_VECTOR(Q_value, K);






	Anull(matconst, p+T-1, p+T-1);
	for(i=0; i<p; i++){	
		for(j=0; j<p; j++){

			if(i == j){
				matconst[i][j] = T*1.0;
			}

		}
	}

	for(i=p; i<p+T-1; i++){	
		for(j=p; j<p+T-1; j++){

			if(i == j){
				matconst[i][j] = p*1.0;
			}

		}
	}

	for(i=0; i<p; i++){	
		for(j=p; j<p+T-1; j++){
			matconst[i][j] = 1.0;

		}
	}
	for(i=p; i<p+T-1; i++){	
		for(j=0; j<p; j++){
			matconst[i][j] = 1.0;

		}
	}
	anull(Eig, p+T-1);
	EigValDec(p+T-1, Eig, matconst, &det);

	Anull(L, p+T-1, p+T-1);

	for (t=0; t<p+T-1; t++){
		L[t][t] = 1.0 / Eig[t];
		
	}
	
	XAXt(matconst, p+T-1, L, invconst);



	eps = misc_double[0];

	anull(sum_gamma, K);
	Anull3(Mu, p, T, K);



	for(k=0; k<K; k++){

		for(i=0; i<n; i++){	
	
			sum_gamma[k] += gamma[i][k];
	
		}

		tau[k] = sum_gamma[k] / n; 

	}

	for(k=0; k<K; k++){

		Trans_trans_whole(n, p, T, la[k], nu[k], Y, MY, trans_type);


		if(Mu_type == 2){
			for(i=0; i<n; i++){

				cpyk(MY, p, T, i, MYi);
	
				for(j=0; j<p; j++){

					for(t=0; t<T; t++){
			
						Mu[j][t][k] += gamma[i][k] * MYi[j][t] / sum_gamma[k];


					}
				}

			}
		}
		else if(Mu_type == 1){

			anull(MY_vec, p+T-1);


	
			for(j=0; j<p; j++){
				for(i=0; i<n; i++){
					for(t=0; t<T; t++){		
						MY_vec[j] += gamma[i][k] * MY[j][t][i] / sum_gamma[k];
					}
				}
			}


			for(t=p; t<p+T-1; t++){
				for(i=0; i<n; i++){
					for(j=0; j<p; j++){		
						MY_vec[t] += gamma[i][k] * MY[j][t-p][i] / sum_gamma[k];
					}
				}
			}


			matxvec(invconst, p+T-1, p+T-1, MY_vec, p+T-1, Muk_vec);


			for(j=0; j<p; j++){
		
				for(t=0; t<T; t++){
			
					if(t<T-1){
						Mu[j][t][k] = Muk_vec[j] +  Muk_vec[p+t];

					}
					else{
						Mu[j][t][k] = Muk_vec[j];
					}
				}


			}
	

		}

	}




	if(Sigma_type == 1){
		modelB1(p, T, n, K, Y, la, nu, tau, Mu, gamma, invS, invPsi, detS, trans_type);	
	}
	else if(Sigma_type == 2){
		modelB2(p, T, n, K, Y, la, nu, tau, Mu, gamma, invS, invPsi, detS, trans_type);
	}
	else if(Sigma_type == 3){
		modelB3(p, T, n, K, Y, la, nu, tau, Mu, gamma, invS, invPsi, detS, trans_type);
	}
	else if(Sigma_type == 4){
		modelB4(p, T, n, K, Y, la, nu, tau, Mu, gamma, invS, invPsi, detS, trans_type);
	}
	else if(Sigma_type == 5){
		modelB5(p, T, n, K, Y, la, nu, tau, Mu, gamma, invS, invPsi, detS, trans_type);
	}
	else if(Sigma_type == 6){
		modelB6(p, T, n, K, Y, la, nu, tau, Mu, gamma, invS, invPsi, detS, trans_type);
	}
	else if(Sigma_type == 7){
		modelB7(p, T, n, K, Y, la, nu, tau, Mu, gamma, invS, invPsi, detS, trans_type);
	}
	else if(Sigma_type == 8){
		modelB8(p, T, n, K, Y, la, nu, tau, Mu, gamma, invS, invPsi, detS, trans_type);
	}
	else if(Sigma_type == 9){
		modelB9(p, T, n, K, Y, la, nu, tau, Mu, gamma, invS, invPsi, detS, trans_type);
	}
	else if(Sigma_type == 10){
		modelB10(p, T, n, K, Y, la, nu, tau, Mu, gamma, invS, invPsi, detS, trans_type);
	}
	else if(Sigma_type == 11){
		modelB11(p, T, n, K, Y, la, nu, tau, Mu, gamma, invS, invPsi, detS, trans_type);
	}
	else if(Sigma_type == 12){
		modelB12(p, T, n, K, Y, la, nu, tau, Mu, gamma, invS, invPsi, detS, trans_type);
	}
	else if(Sigma_type == 13){
		modelB13(p, T, n, K, Y, la, nu, tau, Mu, gamma, invS, invPsi, detS, trans_type);
	}
	else if(Sigma_type == 14){
		modelB14(p, T, n, K, Y, la, nu, tau, Mu, gamma, invS, invPsi, detS, trans_type);
	}


	if(Psi_type == 1){
		
		Anull3(invPsi, T, T, K);
		for(k=0; k<K; k++){
			for(t=0; t<T; t++){			
	
				invPsi[t][t][k] = 1.0;

			}
			detPsi[k] = 1.0;
		}
		
	}
	else if(Psi_type == 2){
		modelA1(p, T, n, K, Y, la, nu, tau, Mu, gamma, invS, invPsi, detPsi, trans_type);		
	}
	else if(Psi_type == 3){
		modelA2(p, T, n, K, Y, la, nu, tau, Mu, gamma, invS, invPsi, detPsi, trans_type);		
	}
	else if(Psi_type == 4){
		modelA3(p, T, n, K, Y, la, nu, tau, Mu, gamma, invS, invPsi, detPsi, trans_type);		
	}
	else if(Psi_type == 5){
		modelA4(p, T, n, K, Y, la, nu, tau, Mu, gamma, invS, invPsi, detPsi, trans_type);		
	}
	else if(Psi_type == 6){
		modelA5(p, T, n, K, Y, la, nu, tau, Mu, gamma, invS, invPsi, detPsi, trans_type);		
	}
	else if(Psi_type == 7){
		modelA6(p, T, n, K, Y, la, nu, tau, Mu, gamma, invS, invPsi, detPsi, trans_type);
	}
	else if(Psi_type == 8){
		modelA7(p, T, n, K, Y, la, nu, tau, Mu, gamma, invS, invPsi, detPsi, trans_type);
	}


	if(trans_type == 0){
		Q_value0 = 0;
	
		for(k=0; k<K; k++){
			cpyv(gamma, k, n, gamma_k);		
			cpyk(invS, p, p, k, invSk);		
			cpyk(invPsi, T, T, k, invPsik);		

			Q_value[k] = Q1(n, p, T, la[k], nu[k], Y, gamma_k, invSk, invPsik, Mu_type, trans_type, la_type);
			Q_value0 += Q_value[k];
		}
	}
	else if((trans_type == 1) && (la[0][0] == 1)){
		Q_value0 = 0;
	
		for(k=0; k<K; k++){
			cpyv(gamma, k, n, gamma_k);		
			cpyk(invS, p, p, k, invSk);		
			cpyk(invPsi, T, T, k, invPsik);		

			Q_value[k] = Q1(n, p, T, la[k], nu[k], Y, gamma_k, invSk, invPsik, Mu_type, trans_type, la_type);
			Q_value0 += Q_value[k];
		}
	}
	else if((trans_type == 2) && (la[0][0] == 0)){
		Q_value0 = 0;
	
		for(k=0; k<K; k++){
			cpyv(gamma, k, n, gamma_k);		
			cpyk(invS, p, p, k, invSk);		
			cpyk(invPsi, T, T, k, invPsik);		

			Q_value[k] = Q1(n, p, T, la[k], nu[k], Y, gamma_k, invSk, invPsik, Mu_type, trans_type, la_type);
			Q_value0 += Q_value[k];
		}
	}
	else if(la_type == 0){
		Q_value0 = 0;
	
		for(k=0; k<K; k++){

			cpyv(gamma, k, n, gamma_k);		
			cpyk(invS, p, p, k, invSk);		
			cpyk(invPsi, T, T, k, invPsik);				

			min_value = simplex1(Q1, n, p, T, nu[k], Y, gamma_k, invSk, invPsik, la[k], eps, 0.1, Mu_type, trans_type, la_type);

			Q_value[k] = min_value;
			Q_value0 += Q_value[k];
		}

	}

	else if(la_type == 1){
		Q_value0 = 0;

		for(k=0; k<K; k++){
				

			cpyv(gamma, k, n, gamma_k);		
			cpyk(invS, p, p, k, invSk);		
			cpyk(invPsi, T, T, k, invPsik);		

			double *la_nonzero;
			MAKE_VECTOR(la_nonzero, 1);

			la_nonzero[0] = la[k][0];

			min_value = simplex1(Q1_same, n, p, T, nu[k], Y, gamma_k, invSk, invPsik, la_nonzero, eps, 0.1, Mu_type, trans_type, la_type);
				
			for(j=0; j<p; j++){

				la[k][j] = la_nonzero[0];
						
			}		
	
			FREE_VECTOR(la_nonzero);

			Q_value[k] = min_value;
			Q_value0 += Q_value[k];
		}

	}



	


	if(trans_type == 0){
		Q_value0 = 0;
	
		for(k=0; k<K; k++){
			cpyv(gamma, k, n, gamma_k);		
			cpyk(invS, p, p, k, invSk);		
			cpyk(invPsi, T, T, k, invPsik);		

			Q_value[k] = Q2(n, p, T, nu[k], la[k], Y, gamma_k, invSk, invPsik, Mu_type, trans_type);
			Q_value0 += Q_value[k];
		}
	}
	else if((trans_type == 1) && (nu[0][0] == 1)){
		Q_value0 = 0;
	
		for(k=0; k<K; k++){
			cpyv(gamma, k, n, gamma_k);		
			cpyk(invS, p, p, k, invSk);		
			cpyk(invPsi, T, T, k, invPsik);		

			Q_value[k] = Q2(n, p, T, nu[k], la[k], Y, gamma_k, invSk, invPsik, Mu_type, trans_type);
			Q_value0 += Q_value[k];
		}


	}
	else if((trans_type == 2) && (nu[0][0] == 0)){
		Q_value0 = 0;
	
		for(k=0; k<K; k++){
			cpyv(gamma, k, n, gamma_k);		
			cpyk(invS, p, p, k, invSk);		
			cpyk(invPsi, T, T, k, invPsik);		

			Q_value[k] = Q2(n, p, T, nu[k], la[k], Y, gamma_k, invSk, invPsik, Mu_type, trans_type);
			Q_value0 += Q_value[k];
		}


	}
	else{
		Q_value0 = 0;
	
		for(k=0; k<K; k++){
			cpyv(gamma, k, n, gamma_k);		
			cpyk(invS, p, p, k, invSk);		
			cpyk(invPsi, T, T, k, invPsik);		

			min_value = simplex2(Q2, n, p, T, la[k], Y, gamma_k, invSk, invPsik, nu[k], eps, 0.1, Mu_type, trans_type);

			Q_value[k] = min_value;
			Q_value0 += Q_value[k];
		}


	}



	FREE_MATRIX(matconst);
	FREE_MATRIX(invconst);
	FREE_MATRIX(L);
	FREE_VECTOR(Eig);
	FREE_VECTOR(Muk_vec);
	FREE_VECTOR(MY_vec);

	FREE_3ARRAY(MY);
	FREE_VECTOR(Q_value);
	FREE_MATRIX(MYi);
	FREE_MATRIX(Muk);
	FREE_VECTOR(sum_gamma);
	FREE_VECTOR(gamma_k);
	FREE_VECTOR(Eig1);
	FREE_VECTOR(Eig2);
	FREE_MATRIX(Psi);	
	FREE_MATRIX(S);
	FREE_MATRIX(tMYi);
	FREE_MATRIX(temp1);
	FREE_MATRIX(temp2);
	FREE_MATRIX(temp3);
	FREE_MATRIX(temp4);
	FREE_MATRIX(L1);
	FREE_MATRIX(L2);
	FREE_MATRIX(invPsik);
	FREE_MATRIX(invSk);
	return Q_value0;
}





void EM_Trans_Full(int p, int T, int n, int K, double ***Y, double **la, double **nu, int max_iter, double *misc_double, double *tau, double ***Mu, double ***invS, double ***invPsi, double *detS, double *detPsi, double **gamma, int *id, double *ll, int *conv, int Mu_type, int Sigma_type, int Psi_type, int la_type, double *scale, int trans_type){
	int i,k,iter,M = 0;
	double eps,loglik_old,loglik = 0.0,max;

 	eps = misc_double[0];
	loglik_old = -INFINITY;
	iter = 0;
//printf(" scale %lf \n", scale[0]);


	do{
		loglik = loglik_old; 
		
		iter += 1;

			
		Mstep_Trans_Full(p, T, n, K, misc_double, Y, la, nu, gamma, invS, Mu, invPsi, detS, detPsi, tau, Mu_type, Sigma_type, Psi_type, la_type, trans_type);


 
		Estep_Trans_Full(p, T, n, K, Y, la, nu, tau, Mu, invS, invPsi, detS, detPsi, gamma, trans_type);


		
 		loglik_old = mGloglik_Trans_Full(p, T, n, K, Y, la, nu, tau, Mu, invS, invPsi, detS, detPsi, scale, trans_type);
		
					
	}

	while ((iter < max_iter) && (fabs(loglik - loglik_old) / fabs(loglik_old) > eps));



	ll[0] = mGloglik_Trans_Full(p, T, n, K, Y, la, nu, tau, Mu, invS, invPsi, detS, detPsi, scale, trans_type);

	M += K-1;

	if(Mu_type == 1){M += (p+T-1)*K;}
	else{M += p*T*K;}

	if(Sigma_type == 1){M += 1;}
	else if(Sigma_type == 2){M += K;}
	else if(Sigma_type == 3){M += p;}
	else if(Sigma_type == 4){M += K+p-1;}
	else if(Sigma_type == 5){M += 1+K*(p-1);}
	else if(Sigma_type == 6){M += K*p;}
	else if(Sigma_type == 7){M += p*(p+1)/2;}
	else if(Sigma_type == 8){M += K+p*(p+1)/2-1;}
	else if(Sigma_type == 9){M += 1+K*(p-1)+p*(p-1)/2;}
	else if(Sigma_type == 10){M += K*p+p*(p-1)/K;}
	else if(Sigma_type == 11){M += p+K*p*(p-1)/2;}
	else if(Sigma_type == 12){M += K+p-1+K*p*(p-1)/2;}
	else if(Sigma_type == 13){M += 1+K*(p*(p+1)/2-1);}
	else if(Sigma_type == 14){M += K*p*(p+1)/2;}

	if(Psi_type == 2){M += T-1;}
	else if(Psi_type == 3){M += K*(T-1);}
	else if(Psi_type == 4){M += T*(T+1)/2-1;}
	else if(Psi_type == 5){M += (T-1)*(K+T/2);}
	else if(Psi_type == 6){M += (T-1)*(1+K*T/2);}
	else if(Psi_type == 7){M += K*T*(T+1)/2-K;}
	else if(Psi_type == 8){M += K;}
	

	//printf(" pars  %d %d %d %d %d %d %d\n", p, K, T, Mu_type, Sigma_type, Psi_type, M);

	if(trans_type != 0){
		if(la_type == 1){M += K+K*(T-1);}
		else if(la_type == 0){M += K*p+K*(T-1);}

	}
	
	ll[1] = log(n)*M -2.0*ll[0];



	
	conv[0] = iter;
	if(fabs(loglik - loglik_old) / fabs(loglik_old) <= eps){
		conv[1] = 0;
	} else{
		conv[1] = 1;
	}
	
	conv[2] = M;
	anulli(id, n);
	
	for(i=0; i<n; i++){
		max = -INFINITY;
		for(k=0; k<K; k++){

			if(gamma[i][k] > max){
				id[i] = k+1;
			
				max = gamma[i][k];
			}

		}
		
	}




}
