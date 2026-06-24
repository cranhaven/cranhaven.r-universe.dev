

#include "array.h"
#include "math.h"
#include "MatTransMix.h"
 
#define Inf 1e+140



#include <R.h>
#include <Rmath.h>




void modelB1(int p, int T, int n, int K, double ***Y, double **la, double **nu, double *tau, double ***Mu, double **gamma, double ***invS, double ***invPsi, double *detS, int trans_type){

	int i,k,j1,j2;
	double psi = 0.0; 
	double **WR, ***MY, **Muk, **invPsik, **MYi, **tMYi, **temp1, **temp2;
	MAKE_3ARRAY(MY, p,T,n);
	MAKE_MATRIX(WR, p, p);
	MAKE_MATRIX(Muk, p, T);
	MAKE_MATRIX(invPsik, T, T);
	MAKE_MATRIX(MYi, p, T);
	MAKE_MATRIX(tMYi, T, p);
	MAKE_MATRIX(temp1, p, T);
	MAKE_MATRIX(temp2, p, p);



	Anull(WR, p, p);
	


	for(k=0; k<K; k++){

		cpyk(invPsi, T, T, k, invPsik);
		cpyk(Mu, p, T, k, Muk);
		Trans_trans_whole(n, p, T, la[k], nu[k], Y, MY, trans_type);


		for(i=0; i<n; i++){


			cpyk(MY, p, T, i, MYi);

			mat_(p, T, MYi, Muk);
	
			tA(MYi, T, p, tMYi);

			multiply(MYi, p, T, invPsik, T, T, temp1);

			multiply(temp1, p, T, tMYi, T, p, temp2);


			for(j1=0; j1<p; j1++){
				for(j2=0; j2<p; j2++){
					WR[j1][j2] += gamma[i][k] * temp2[j1][j2];
				}

			}

		}
	}

	for(j1=0; j1<p; j1++){
	
		psi += WR[j1][j1] /n/T/p;
			
	}
	

	Anull3(invS, p, p, K);

	for(k=0; k<K; k++){

		for(j1=0; j1<p; j1++){
	
			invS[j1][j1][k] = 1.0 / psi;
			
		}
		detS[k] = pow(psi, p);

	}
	FREE_3ARRAY(MY);
	FREE_MATRIX(WR);
	FREE_MATRIX(Muk);
	FREE_MATRIX(invPsik);
	FREE_MATRIX(MYi);
	FREE_MATRIX(tMYi);
	FREE_MATRIX(temp1);
	FREE_MATRIX(temp2);



}




void modelB2(int p, int T, int n, int K, double ***Y, double **la, double **nu, double *tau, double ***Mu, double **gamma, double ***invS, double ***invPsi, double *detS, int trans_type){

	int i,k,j1,j2;
	double *psi; 
	double ***WR, ***MY, **Muk, **invPsik, **MYi, **tMYi, **temp1, **temp2;
	MAKE_3ARRAY(MY, p,T,n);
	MAKE_3ARRAY(WR, p, p, K);
	MAKE_MATRIX(Muk, p, T);
	MAKE_MATRIX(invPsik, T, T);
	MAKE_MATRIX(MYi, p, T);
	MAKE_MATRIX(tMYi, T, p);
	MAKE_MATRIX(temp1, p, T);
	MAKE_MATRIX(temp2, p, p);
	MAKE_VECTOR(psi, K);


	Anull3(WR, p, p, K);
	

	anull(psi, K);
	for(k=0; k<K; k++){

		cpyk(invPsi, T, T, k, invPsik);
		cpyk(Mu, p, T, k, Muk);
		Trans_trans_whole(n, p, T, la[k], nu[k], Y, MY, trans_type);


		for(i=0; i<n; i++){


			cpyk(MY, p, T, i, MYi);

			mat_(p, T, MYi, Muk);
	
			tA(MYi, T, p, tMYi);

			multiply(MYi, p, T, invPsik, T, T, temp1);

			multiply(temp1, p, T, tMYi, T, p, temp2);


			for(j1=0; j1<p; j1++){
				for(j2=0; j2<p; j2++){
					WR[j1][j2][k] += gamma[i][k] * temp2[j1][j2];
				}

			}

		}


		for(j1=0; j1<p; j1++){
	
			psi[k] += WR[j1][j1][k] /n/T/p/tau[k];
			
		}
	
	}


	Anull3(invS, p, p, K);

	for(k=0; k<K; k++){

		for(j1=0; j1<p; j1++){
	
			invS[j1][j1][k] = 1.0 / psi[k];
			
		}
		detS[k] = pow(psi[k], p);

	}
	FREE_3ARRAY(MY);
	FREE_3ARRAY(WR);
	FREE_MATRIX(Muk);
	FREE_MATRIX(invPsik);
	FREE_MATRIX(MYi);
	FREE_MATRIX(tMYi);
	FREE_MATRIX(temp1);
	FREE_MATRIX(temp2);
	FREE_VECTOR(psi);


}




void modelB3(int p, int T, int n, int K, double ***Y, double **la, double **nu, double *tau, double ***Mu, double **gamma, double ***invS, double ***invPsi, double *detS, int trans_type){

	int i,k,j1,j2;
	double **WR, ***MY, **Muk, **invPsik, **MYi, **tMYi, **temp1, **temp2;
	MAKE_3ARRAY(MY, p,T,n);
	MAKE_MATRIX(WR, p, p);
	MAKE_MATRIX(Muk, p, T);
	MAKE_MATRIX(invPsik, T, T);
	MAKE_MATRIX(MYi, p, T);
	MAKE_MATRIX(tMYi, T, p);
	MAKE_MATRIX(temp1, p, T);
	MAKE_MATRIX(temp2, p, p);



	Anull(WR, p, p);

	for(k=0; k<K; k++){

		cpyk(invPsi, T, T, k, invPsik);
		cpyk(Mu, p, T, k, Muk);
		Trans_trans_whole(n, p, T, la[k], nu[k], Y, MY, trans_type);


		for(i=0; i<n; i++){


			cpyk(MY, p, T, i, MYi);

			mat_(p, T, MYi, Muk);
	
			tA(MYi, T, p, tMYi);

			multiply(MYi, p, T, invPsik, T, T, temp1);

			multiply(temp1, p, T, tMYi, T, p, temp2);


			for(j1=0; j1<p; j1++){
				for(j2=0; j2<p; j2++){
					WR[j1][j2] += gamma[i][k] * temp2[j1][j2];
				}

			}

		}
	}

	Anull3(invS, p, p, K);

	for(k=0; k<K; k++){
		detS[k] = 1.0;
		for(j1=0; j1<p; j1++){
	
			invS[j1][j1][k] = 1.0*n*T / WR[j1][j1];
			
		}

		for(j1=0; j1<p; j1++){
	
			detS[k] *= WR[j1][j1]/n/T;
		}


	}



	FREE_3ARRAY(MY);
	FREE_MATRIX(WR);
	FREE_MATRIX(Muk);
	FREE_MATRIX(invPsik);
	FREE_MATRIX(MYi);
	FREE_MATRIX(tMYi);
	FREE_MATRIX(temp1);
	FREE_MATRIX(temp2);



}







void modelB4(int p, int T, int n, int K, double ***Y, double **la, double **nu, double *tau, double ***Mu, double **gamma, double ***invS, double ***invPsi, double *detS, int trans_type){

	int i,k,j1,j2;
	double *psi; 
	double ***WR, ***MY, **Muk, **invPsik, **MYi, **tMYi, **temp1, **temp2, **Delta, det1p, **WRk;
	MAKE_3ARRAY(MY, p,T,n);
	MAKE_3ARRAY(WR, p, p, K);
	MAKE_MATRIX(Delta, p, p);
	MAKE_MATRIX(WRk, p, p);
	MAKE_MATRIX(Muk, p, T);
	MAKE_MATRIX(invPsik, T, T);
	MAKE_MATRIX(MYi, p, T);
	MAKE_MATRIX(tMYi, T, p);
	MAKE_MATRIX(temp1, p, T);
	MAKE_MATRIX(temp2, p, p);

	MAKE_VECTOR(psi, K);

	Anull(Delta, p, p);	
	Anull3(WR, p, p, K);	

	det1p = 1.0;
	for(k=0; k<K; k++){

		cpyk(invPsi, T, T, k, invPsik);
		cpyk(Mu, p, T, k, Muk);
		Trans_trans_whole(n, p, T, la[k], nu[k], Y, MY, trans_type);


		for(i=0; i<n; i++){


			cpyk(MY, p, T, i, MYi);

			mat_(p, T, MYi, Muk);
	
			tA(MYi, T, p, tMYi);

			multiply(MYi, p, T, invPsik, T, T, temp1);

			multiply(temp1, p, T, tMYi, T, p, temp2);


			for(j1=0; j1<p; j1++){
				for(j2=0; j2<p; j2++){
					WR[j1][j2][k] += gamma[i][k] * temp2[j1][j2];
				}

			}

		}

		for(j1=0; j1<p; j1++){

			Delta[j1][j1] += WR[j1][j1][k] / pow(detS[k], 1.0/p);
		
		}


	}

	for(j1=0; j1<p; j1++){
		det1p *= Delta[j1][j1];
	}
	det1p = pow(det1p, 1.0/p);
	for(j1=0; j1<p; j1++){

		Delta[j1][j1] = det1p / Delta[j1][j1];
		
	}


	anull(psi, K);
	Anull3(invS, p,p,K);

	for(k=0; k<K; k++){

		cpyk(WR, p, p, k, WRk);	
		multiply(Delta, p, p, WRk, p, p, temp2);
		for(j1=0; j1<p; j1++){
			psi[k] += temp2[j1][j1]/T/p/tau[k]/n;
		}
		detS[k] = pow(psi[k],p);
		for(j1=0; j1<p; j1++){

			invS[j1][j1][k] = 1.0/psi[k]*Delta[j1][j1];
		}
	}




	FREE_3ARRAY(MY);
	FREE_3ARRAY(WR);
	FREE_MATRIX(WRk);
	FREE_MATRIX(Muk);
	FREE_MATRIX(invPsik);
	FREE_MATRIX(MYi);
	FREE_MATRIX(tMYi);
	FREE_MATRIX(temp1);
	FREE_MATRIX(temp2);
	FREE_MATRIX(Delta);

	FREE_VECTOR(psi);

}








void modelB5(int p, int T, int n, int K, double ***Y, double **la, double **nu, double *tau, double ***Mu, double **gamma, double ***invS, double ***invPsi, double *detS, int trans_type){

	int i,k,j1,j2;
	double psi = 0.0; 
	double ***WR, ***MY, **Muk, **invPsik, **MYi, **tMYi, **temp1, **temp2, ***Delta, *det1p;
	MAKE_3ARRAY(MY, p,T,n);
	MAKE_3ARRAY(WR, p, p, K);
	MAKE_3ARRAY(Delta, p, p, K);
	MAKE_MATRIX(Muk, p, T);
	MAKE_MATRIX(invPsik, T, T);
	MAKE_MATRIX(MYi, p, T);
	MAKE_MATRIX(tMYi, T, p);
	MAKE_MATRIX(temp1, p, T);
	MAKE_MATRIX(temp2, p, p);
	MAKE_VECTOR(det1p, K);


	Anull3(WR, p, p, K);
	Anull3(Delta, p, p, K);
	

	for(k=0; k<K; k++){

		cpyk(invPsi, T, T, k, invPsik);
		cpyk(Mu, p, T, k, Muk);
		Trans_trans_whole(n, p, T, la[k], nu[k], Y, MY, trans_type);


		for(i=0; i<n; i++){


			cpyk(MY, p, T, i, MYi);

			mat_(p, T, MYi, Muk);
	
			tA(MYi, T, p, tMYi);

			multiply(MYi, p, T, invPsik, T, T, temp1);

			multiply(temp1, p, T, tMYi, T, p, temp2);


			for(j1=0; j1<p; j1++){
				for(j2=0; j2<p; j2++){
					WR[j1][j2][k] += gamma[i][k] * temp2[j1][j2];
				}

			}

		}
		det1p[k] = 1.0;
		
		for(j1=0; j1<p; j1++){

			det1p[k] *= WR[j1][j1][k];		
		}
		det1p[k] = pow(det1p[k], 1.0/p);
		for(j1=0; j1<p; j1++){
			Delta[j1][j1][k] = WR[j1][j1][k] / det1p[k];
		}
	
		psi +=  det1p[k]/n/T;
	}



	Anull3(invS, p, p, K);

	for(k=0; k<K; k++){

		for(j1=0; j1<p; j1++){
			invS[j1][j1][k] = 1.0/psi/Delta[j1][j1][k];
		}	
		detS[k] = pow(psi, p);

	}

	FREE_3ARRAY(MY);
	FREE_3ARRAY(WR);
	FREE_MATRIX(Muk);
	FREE_MATRIX(invPsik);
	FREE_MATRIX(MYi);
	FREE_MATRIX(tMYi);
	FREE_MATRIX(temp1);
	FREE_MATRIX(temp2);
	FREE_3ARRAY(Delta);
	FREE_VECTOR(det1p);

}



void modelB6(int p, int T, int n, int K, double ***Y, double **la, double **nu, double *tau, double ***Mu, double **gamma, double ***invS, double ***invPsi, double *detS, int trans_type){

	int i,k,j1,j2;
	double *psi; 
	double ***WR, ***MY, **Muk, **invPsik, **MYi, **tMYi, **temp1, **temp2;
	MAKE_3ARRAY(MY, p,T,n);
	MAKE_3ARRAY(WR, p, p, K);
	MAKE_MATRIX(Muk, p, T);
	MAKE_MATRIX(invPsik, T, T);
	MAKE_MATRIX(MYi, p, T);
	MAKE_MATRIX(tMYi, T, p);
	MAKE_MATRIX(temp1, p, T);
	MAKE_MATRIX(temp2, p, p);
	MAKE_VECTOR(psi, K);


	Anull3(WR, p, p, K);
	Anull3(invS, p, p, K);

	anull(psi, K);
	for(k=0; k<K; k++){

		cpyk(invPsi, T, T, k, invPsik);
		cpyk(Mu, p, T, k, Muk);
		Trans_trans_whole(n, p, T, la[k], nu[k], Y, MY, trans_type);


		for(i=0; i<n; i++){


			cpyk(MY, p, T, i, MYi);

			mat_(p, T, MYi, Muk);
	
			tA(MYi, T, p, tMYi);

			multiply(MYi, p, T, invPsik, T, T, temp1);

			multiply(temp1, p, T, tMYi, T, p, temp2);


			for(j1=0; j1<p; j1++){
				for(j2=0; j2<p; j2++){
					WR[j1][j2][k] += gamma[i][k] * temp2[j1][j2];
				}

			}

		}
		detS[k] = 1.0;

		for(j1=0; j1<p; j1++){
	
			invS[j1][j1][k] = T*tau[k]*n / WR[j1][j1][k];
			detS[k] *= WR[j1][j1][k]/T/n/tau[k];

			
		}

	
	}

	

	FREE_3ARRAY(MY);
	FREE_3ARRAY(WR);
	FREE_MATRIX(Muk);
	FREE_MATRIX(invPsik);
	FREE_MATRIX(MYi);
	FREE_MATRIX(tMYi);
	FREE_MATRIX(temp1);
	FREE_MATRIX(temp2);
	FREE_VECTOR(psi);


}



void modelB7(int p, int T, int n, int K, double ***Y, double **la, double **nu, double *tau, double ***Mu, double **gamma, double ***invS, double ***invPsi, double *detS, int trans_type){

	int i,k,j1,j2,j;
	double **WR, ***MY, **Muk, **invPsik, **MYi, **tMYi, **temp1, **temp2;
	double det, *Eig2, **L2, **invSk, min;
	
	MAKE_3ARRAY(MY, p,T,n);
	MAKE_MATRIX(WR, p, p);
	MAKE_MATRIX(Muk, p, T);
	MAKE_MATRIX(invPsik, T, T);
	MAKE_MATRIX(MYi, p, T);
	MAKE_MATRIX(tMYi, T, p);
	MAKE_MATRIX(temp1, p, T);
	MAKE_MATRIX(temp2, p, p);

	MAKE_VECTOR(Eig2, p);
	MAKE_MATRIX(L2, p, p);
	MAKE_MATRIX(invSk, p, p);

	Anull(WR, p, p);
	


	for(k=0; k<K; k++){

		cpyk(invPsi, T, T, k, invPsik);
		cpyk(Mu, p, T, k, Muk);
		Trans_trans_whole(n, p, T, la[k], nu[k], Y, MY, trans_type);


		for(i=0; i<n; i++){


			cpyk(MY, p, T, i, MYi);

			mat_(p, T, MYi, Muk);
	
			tA(MYi, T, p, tMYi);

			multiply(MYi, p, T, invPsik, T, T, temp1);

			multiply(temp1, p, T, tMYi, T, p, temp2);


			for(j1=0; j1<p; j1++){
				for(j2=0; j2<p; j2++){
					WR[j1][j2] += gamma[i][k] * temp2[j1][j2]/n/T;
				}

			}

		}
	}


	Anull3(invS, p, p, K);


	anull(Eig2, p);

	EigValDec(p, Eig2, WR, &det);

	min = INFINITY;
	for(j=0; j<p; j++){
		if(Eig2[j] < min){
			min = Eig2[j];
		}

	}


	if(min < pow(10, -300)){

	}else{
		Anull(L2, p, p);

		for (j=0; j<p; j++){
			L2[j][j] = 1.0 / Eig2[j];
		
		}

		XAXt(WR, p, L2, invSk);

	}

	for(k=0; k<K; k++){
		detS[k] = det;
		cpyk2(invSk, p, p, invS, k);
	}

	FREE_3ARRAY(MY);
	FREE_MATRIX(WR);
	FREE_MATRIX(Muk);
	FREE_MATRIX(invPsik);
	FREE_MATRIX(MYi);
	FREE_MATRIX(tMYi);
	FREE_MATRIX(temp1);
	FREE_MATRIX(temp2);
	FREE_VECTOR(Eig2);
	FREE_MATRIX(L2);
	FREE_MATRIX(invSk);



}






