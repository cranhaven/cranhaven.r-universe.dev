

#include "array.h"
#include "math.h"
#include "MatTransMix.h"
 
#define Inf 1e+140



#include <R.h>
#include <Rmath.h>





void modelA7(int p, int T, int n, int K, double ***Y, double **la, double **nu, double *tau, double ***Mu, double **gamma, double ***invS, double ***invPsi, double *detPsi, int trans_type){
  
	int k, t1, t2, t;
	double **temp1, **temp2;
	double **Muk, ***MY, **MYi, **tMYi, **invPsik0, **invSk, *par, Psi2, phi, **A1, **A2, **I, *gamma_k;
	double **WC;

	MAKE_3ARRAY(MY, p,T,n);
	MAKE_MATRIX(MYi, p, T);
	MAKE_MATRIX(tMYi, T, p);
	MAKE_MATRIX(invSk, p, p);
	MAKE_MATRIX(invPsik0, T, T);
	MAKE_MATRIX(temp1, T, p);
	MAKE_MATRIX(temp2, T, T);
	MAKE_MATRIX(Muk, p, T);
	MAKE_MATRIX(WC, T, T);
	MAKE_VECTOR(gamma_k, n);

	MAKE_VECTOR(par,2);
	MAKE_MATRIX(A1, T, T);
	MAKE_MATRIX(A2, T, T);
	MAKE_MATRIX(I, T, T);

	Anull(WC, T, T);
	anull(par, 2);	


	for(k=0; k<K; k++){

		Trans_trans_whole(n, p, T, la[k], nu[k], Y, MY, trans_type);

		cpyk(Mu, p, T, k, Muk);

		cpyv(gamma, k, n, gamma_k);	
		cpyk(invS, p, p, k, invSk);

		//printf(" psi2 phi %lf %lf %lf %lf\n", Muk[0][0], MY[0][0][0], gamma_k[0], invSk[0][0]);

		findPsi2phi(n, p, T, par, MY, Muk, gamma_k, invSk, 0.000001);

		Psi2 = par[0];
		phi = par[1];

		//printf(" psi2 phi %lf %lf \n", Psi2, phi);

		Anull(A1, T, T);
		Anull(A2, T, T);
		Anull(I, T, T);
		Anull(invPsik0, T, T);


		for(t1=0; t1<T; t1++){

			for(t2=0; t2<T; t2++){

				if(abs(t1-t2) == 1){

					A1[t1][t2] = - phi / Psi2;
 

				}

			}

		}

		for(t=1; t<T-1; t++){


			A2[t][t] = pow(phi, 2) / Psi2;
 

		}


		for(t=0; t<T; t++){


			I[t][t] = 1.0 / Psi2;
 

		}

		for(t1=0; t1<T; t1++){

			for(t2=0; t2<T; t2++){

				invPsik0[t1][t2] = A1[t1][t2]+A2[t1][t2]+I[t1][t2];


			}

		}

		//printf(" invPsi %lf \n", invPsik0[0][0]);

		detPsi[k] = pow(Psi2, T) /  (1.0 - pow(phi, 2));
		cpyk2(invPsik0, T, T, invPsi, k);

		//printf(" det %lf \n", detPsi[k]);



	}

	for(k=0; k<K; k++){

		for(t1=0; t1<T; t1++){
			for(t2=0; t2<T; t2++){

				invPsi[t1][t2][k] = pow(detPsi[k], 1.0/T) * invPsi[t1][t2][k];

			}

		}

		//printf(" invPsi updated %lf \n", invPsi[0][0][0]);

		detPsi[k] = 1.0;

	}

	////




	FREE_3ARRAY(MY);
	FREE_MATRIX(MYi);
	FREE_MATRIX(tMYi);
	FREE_MATRIX(invSk);
	FREE_MATRIX(invPsik0);
	FREE_MATRIX(temp1);
	FREE_MATRIX(temp2);
	FREE_MATRIX(Muk);
	FREE_MATRIX(WC);
	


}



void modelA4(int p, int T, int n, int K, double ***Y, double **la, double **nu, double *tau, double ***Mu, double **gamma, double ***invS, double ***invPsi, double *detPsi, int trans_type){
  

	int k,i,t1,t2,t,iter=0;
	double ***WC, **WCk, ***MY, **invSk, **Muk, **MYi, **tMYi, **temp1, **temp2;

	double ***invDelta, **invPsik, *Eig1, det1, **L, *Eig, det, det2, **Psik, *Eig2, *Eig3, det1p;

	double *wk;

	double **pooled, **Gamma_max;

	double fun, fun_new, **Ft, **invDeltak, **tGamma_max, **temp3, **temp4;

	double *w, **tPt, **Pt, **Rt, **tRt, **temp5, **temp6, **temp7, **final, **temp8;

 
	MAKE_3ARRAY(MY, p,T,n);
	MAKE_MATRIX(MYi, p, T);
	MAKE_MATRIX(tMYi, T, p);
	MAKE_MATRIX(invSk, p, p);
	MAKE_MATRIX(temp1, T, p);
	MAKE_MATRIX(temp2, T, T);
	MAKE_MATRIX(Muk, p, T);
	MAKE_3ARRAY(WC, T, T, K);


	MAKE_3ARRAY(invDelta, T, T, K);
	MAKE_MATRIX(invPsik, T, T);
	MAKE_VECTOR(Eig1, T);
	MAKE_MATRIX(L, T, T);
	MAKE_MATRIX(Psik, T, T);


	MAKE_MATRIX(WCk, T, T);
	MAKE_VECTOR(Eig, T);
	MAKE_VECTOR(Eig2, T);
	MAKE_VECTOR(Eig3, T);

	MAKE_VECTOR(wk, K);
	MAKE_MATRIX(Gamma_max, T, T);	
	MAKE_MATRIX(pooled, T, T);	

	MAKE_MATRIX(invDeltak, T, T);	
	MAKE_MATRIX(Ft, T, T);	
	MAKE_MATRIX(tGamma_max, T, T);
	MAKE_MATRIX(temp3, T, T);
	MAKE_MATRIX(temp4, T, T);
	MAKE_MATRIX(tRt, T, T);
	MAKE_MATRIX(tPt, T, T);
	MAKE_MATRIX(Pt, T, T);
	MAKE_VECTOR(w, T);
	MAKE_MATRIX(Rt, T, T);

	MAKE_MATRIX(temp5, 2*T, T);
	MAKE_MATRIX(temp6, T, T);
	MAKE_MATRIX(temp7, T, T);
	MAKE_MATRIX(final, T, T);

	MAKE_MATRIX(temp8, T, T);



	Anull3(WC, T, T, K);
	for(k=0; k<K; k++){

		Trans_trans_whole(n, p, T, la[k], nu[k], Y, MY, trans_type);

		cpyk(invS, p, p, k, invSk);
		cpyk(Mu, p, T, k, Muk);
	
		for(i=0; i<n; i++){


			cpyk(MY, p, T, i, MYi);

			mat_(p, T, MYi, Muk);
	
			tA(MYi, T, p, tMYi);

			multiply(tMYi, T, p, invSk, p, p, temp1);

			multiply(temp1, T, p, MYi, p, T, temp2);


			for(t1=0; t1<T; t1++){
				for(t2=0; t2<T; t2++){
					WC[t1][t2][k] += gamma[i][k] * temp2[t1][t2];
				}

			}

		}

	}
	
	Anull3(invDelta, T, T, K);

	Anull(pooled, T, T);


	for(k=0; k<K; k++){

		cpyk(invPsi, T, T, k, invPsik);



		EigValDec(T, Eig1, invPsik, &det1);
		Anull(L, T, T);

		for (t=0; t<T; t++){
			L[t][t] = 1.0 / Eig1[t];
		
		}
	
		XAXt(invPsik, T, L, Psik);




		for(t1=0; t1<T; t1++){
			invDelta[t1][t1][k] = Eig1[t1];
		
		}


		for(t1=0; t1<T; t1++){
			for(t2=0; t2<T; t2++){
				pooled[t1][t2] += tau[k]* Psik[t1][t2];
			}
		} 



	}


	EigValDec(T, Eig2, pooled, &det2);

	cpy(pooled, T, T, Gamma_max);

	for(k=0; k<K; k++){

		cpyk(WC, T, T, k, WCk);


		EigValDec(T, Eig, WCk, &det);

		wk[k] = Eig[T-1];	

	}


	for(k=0; k<K; k++){

		cpyk(WC, T, T, k, WCk);
		tA(Gamma_max, T, T, tGamma_max);
		multiply(tGamma_max, T, T, WCk, T, T, temp6);
		
		multiply(temp6, T, T, Gamma_max, T, T, temp7);

		Anull(invDeltak, T, T);
		
		det1p = 1.0;
		for(t1=0; t1<T; t1++){
			invDeltak[t1][t1] = temp7[t1][t1];
			det1p *= temp7[t1][t1];
			
		}
		det1p = pow(det1p, 1.0/T);

		for(t1=0; t1<T; t1++){
			invDeltak[t1][t1] = det1p / invDeltak[t1][t1];
			
		}

		cpyk2(invDeltak, T, T, invDelta, k);

	}


	fun_new = 0;



	do{
		iter += 1;
		fun = fun_new;


		Anull(Ft, T, T);
		for(k=0; k<K; k++){	
	
			cpyk(invDelta, T, T, k, invDeltak);
			tA(Gamma_max, T, T, tGamma_max);
			cpyk(WC, T, T, k, WCk);

		
			multiply(invDeltak, T, T, tGamma_max, T, T, temp3);

			multiply(temp3, T, T, WCk, T, T, temp4);


			for(t1=0; t1<T; t1++){
				for(t2=0; t2<T; t2++){

					Ft[t1][t2] += temp4[t1][t2] - wk[k]*temp3[t1][t2];
				}

			}	

		}
		Anull(temp5, 2*T, T);


		for(t1=0; t1<T; t1++){
			for(t2=0; t2<T; t2++){

				temp5[t1][t2] = Ft[t1][t2];
			}

		}	

		svd(temp5, w, T);

		for(t1=0; t1<T; t1++){
			for(t2=0; t2<T; t2++){

				Pt[t1][t2] = temp5[t1][t2];
			}

		}	

		for(t1=0; t1<T; t1++){
			for(t2=0; t2<T; t2++){

				Pt[t1][t2] = Pt[t1][t2] / pow(w[t2], 1.0/2.0);
			}

		}	

		for(t1=T; t1<2*T; t1++){
			for(t2=0; t2<T; t2++){

				Rt[t1-T][t2] = temp5[t1][t2];
			}

		}			

		tA(Pt, T, T, tPt);


		multiply(Rt, T, T, tPt, T, T, Gamma_max);

		fun_new = 0.0;

		for(k=0; k<K; k++){	

			cpyk(WC, T, T, k, WCk);
			cpyk(invDelta, T, T, k, invDeltak);
			tA(Gamma_max, T, T, tGamma_max);

			multiply(WCk, T, T, Gamma_max, T, T, temp6);
			multiply(temp6, T, T, invDeltak, T, T, temp7);
			multiply(temp7, T, T, tGamma_max, T, T, final);
			for(t1=0; t1<T; t1++){
				fun_new += final[t1][t1];
			} 


		}

	}
	while((iter < 10000) && (fabs(fun - fun_new) / fabs(fun) > pow(10, -7)));


	for(k=0; k<K; k++){

		cpyk(WC, T, T, k, WCk);

		multiply(tGamma_max, T, T, WCk, T, T, temp6);
		
		multiply(temp6, T, T, Gamma_max, T, T, temp7);

		Anull(invDeltak, T, T);
		
		det1p = 1.0;
		for(t1=0; t1<T; t1++){
			invDeltak[t1][t1] = temp7[t1][t1];
			det1p *= temp7[t1][t1];
			
		}
		det1p = pow(det1p, 1.0/T);

		for(t1=0; t1<T; t1++){
			invDeltak[t1][t1] = det1p / invDeltak[t1][t1];
			
		}

		detPsi[k] = 1.0;
		multiply(Gamma_max, T, T, invDeltak, T, T, temp8);

		multiply(temp8, T, T, tGamma_max, T, T, invPsik);

		cpyk2(invPsik, T, T, invPsi, k);


	}

	FREE_3ARRAY(invDelta);
	FREE_MATRIX(invPsik);
	FREE_VECTOR(Eig1);

	FREE_3ARRAY(MY);
	FREE_MATRIX(MYi);
	FREE_MATRIX(tMYi);
	FREE_MATRIX(invSk);
	FREE_MATRIX(temp1);
	FREE_MATRIX(temp2);
	FREE_MATRIX(Muk);
	FREE_3ARRAY(WC);

	FREE_MATRIX(WCk);
	FREE_VECTOR(Eig);
	FREE_MATRIX(L);
	FREE_MATRIX(Psik);

	FREE_VECTOR(wk);
	FREE_MATRIX(Gamma_max);	
	FREE_MATRIX(pooled);

	FREE_MATRIX(invDeltak);	
	FREE_MATRIX(Ft);	
	FREE_MATRIX(tGamma_max);	
	FREE_MATRIX(temp3);
	FREE_MATRIX(temp4);
	FREE_MATRIX(temp5);
	FREE_MATRIX(tRt);
	FREE_MATRIX(tPt);
	FREE_MATRIX(Pt);
	FREE_VECTOR(w);
	FREE_MATRIX(Rt);
	FREE_VECTOR(Eig2);
	FREE_VECTOR(Eig3);

	FREE_MATRIX(temp6);
	FREE_MATRIX(temp7);
	FREE_MATRIX(final);


	FREE_MATRIX(temp8);
}




void modelA1(int p, int T, int n, int K, double ***Y, double **la, double **nu, double *tau, double ***Mu, double **gamma, double ***invS, double ***invPsi, double *detPsi, int trans_type){
  
	int i, k, t1, t2;
	double det = 1.0;
	double **temp1, **temp2;
	double **Muk, ***MY, **MYi, **tMYi, **invPsik0, **invSk;
	double **WC;

	MAKE_3ARRAY(MY, p,T,n);
	MAKE_MATRIX(MYi, p, T);
	MAKE_MATRIX(tMYi, T, p);
	MAKE_MATRIX(invSk, p, p);
	MAKE_MATRIX(invPsik0, T, T);
	MAKE_MATRIX(temp1, T, p);
	MAKE_MATRIX(temp2, T, T);
	MAKE_MATRIX(Muk, p, T);
	MAKE_MATRIX(WC, T, T);


	Anull(WC, T, T);
	


	for(k=0; k<K; k++){

		Trans_trans_whole(n, p, T, la[k], nu[k], Y, MY, trans_type);

		for(i=0; i<n; i++){

			cpyk(MY, p, T, i, MYi);

			cpyk(invS, p, p, k, invSk);
			cpyk(Mu, p, T, k, Muk);


			mat_(p, T, MYi, Muk);
	
			tA(MYi, T, p, tMYi);

			multiply(tMYi, T, p, invSk, p, p, temp1);

			multiply(temp1, T, p, MYi, p, T, temp2);


			for(t1=0; t1<T; t1++){
				for(t2=0; t2<T; t2++){
					WC[t1][t2] += gamma[i][k] * temp2[t1][t2];
				}

			}

		}
	}

	for(t1=0; t1<T; t1++){
	
		det *= WC[t1][t1];
			
	}
	det = pow(det, 1.0/T);

	Anull3(invPsi, T, T, K);

	for(k=0; k<K; k++){

		for(t1=0; t1<T; t1++){
	
			invPsi[t1][t1][k] = det / WC[t1][t1];
			
		}
		detPsi[k] = 1.0;

	}

	FREE_3ARRAY(MY);
	FREE_MATRIX(MYi);
	FREE_MATRIX(tMYi);
	FREE_MATRIX(invSk);
	FREE_MATRIX(invPsik0);
	FREE_MATRIX(temp1);
	FREE_MATRIX(temp2);
	FREE_MATRIX(Muk);
	FREE_MATRIX(WC);
	


}


void modelA2(int p, int T, int n, int K, double ***Y, double **la, double **nu, double *tau, double ***Mu, double **gamma, double ***invS, double ***invPsi, double *detPsi, int trans_type){
  
	int i, k, t1, t2;
	double det;
	double **temp1, **temp2;
	double ***MY, **Muk, **MYi, **tMYi, **invPsik0, **invSk;
	double **WC;

	MAKE_3ARRAY(MY, p,T,n);
	MAKE_MATRIX(MYi, p, T);
	MAKE_MATRIX(tMYi, T, p);
	MAKE_MATRIX(invSk, p, p);
	MAKE_MATRIX(invPsik0, T, T);
	MAKE_MATRIX(temp1, T, p);
	MAKE_MATRIX(temp2, T, T);
	MAKE_MATRIX(Muk, p, T);
	MAKE_MATRIX(WC, T, T);


	Anull3(invPsi, T, T, K);
	
	for(k=0; k<K; k++){

		Anull(WC, T, T);
		cpyk(invS, p, p, k, invSk);
		cpyk(Mu, p, T, k, Muk);
		Trans_trans_whole(n, p, T, la[k], nu[k], Y, MY, trans_type);


		for(i=0; i<n; i++){

			cpyk(MY, p, T, i, MYi);

			mat_(p, T, MYi, Muk);
	
			tA(MYi, T, p, tMYi);

			multiply(tMYi, T, p, invSk, p, p, temp1);

			multiply(temp1, T, p, MYi, p, T, temp2);


			for(t1=0; t1<T; t1++){
				for(t2=0; t2<T; t2++){
					WC[t1][t2] += gamma[i][k] * temp2[t1][t2];
				}

			}

		}

		det = 1.0;
		for(t1=0; t1<T; t1++){
	
			det *= WC[t1][t1];
			
		}
		det = pow(det, 1.0/T);

		for(t1=0; t1<T; t1++){

			invPsi[t1][t1][k] = det / WC[t1][t1];

		}
		detPsi[k] = 1.0;


	}


	FREE_MATRIX(MYi);
	FREE_MATRIX(tMYi);
	FREE_MATRIX(invSk);
	FREE_MATRIX(invPsik0);
	FREE_MATRIX(temp1);
	FREE_MATRIX(temp2);
	FREE_MATRIX(Muk);
	FREE_MATRIX(WC);
	FREE_3ARRAY(MY);	


}




void modelA3(int p, int T, int n, int K, double ***Y, double **la, double **nu, double *tau, double ***Mu, double **gamma, double ***invS, double ***invPsi, double *detPsi, int trans_type){
  
	int i, k, t1, t2;
	double det, min;
	double **temp1, **temp2;
	double ***MY, **Muk, **MYi, **tMYi, **invPsik, *Eig1, **invSk, **L1;
	double **WC;

	MAKE_3ARRAY(MY, p,T,n);
	MAKE_MATRIX(MYi, p, T);
	MAKE_MATRIX(tMYi, T, p);
	MAKE_MATRIX(invSk, p, p);
	MAKE_MATRIX(temp1, T, p);
	MAKE_MATRIX(temp2, T, T);
	MAKE_MATRIX(Muk, p, T);
	MAKE_MATRIX(WC, T, T);
	MAKE_VECTOR(Eig1, T);
	MAKE_MATRIX(L1, T, T);
	MAKE_MATRIX(invPsik, T, T);

	Anull(WC, T, T);
	


	for(k=0; k<K; k++){

		Trans_trans_whole(n, p, T, la[k], nu[k], Y, MY, trans_type);
		cpyk(Mu, p, T, k, Muk);
		cpyk(invS, p, p, k, invSk);

		for(i=0; i<n; i++){
			cpyk(MY, p, T, i, MYi);

			mat_(p, T, MYi, Muk);
	
			tA(MYi, T, p, tMYi);

			multiply(tMYi, T, p, invSk, p, p, temp1);

			multiply(temp1, T, p, MYi, p, T, temp2);


			for(t1=0; t1<T; t1++){
				for(t2=0; t2<T; t2++){
					WC[t1][t2] += gamma[i][k] * temp2[t1][t2];
				}

			}


		}
	}


	Anull(invPsik, T, T);

	anull(Eig1, T);


	EigValDec(T, Eig1, WC, &det);

	min = INFINITY;
	for(t1=0; t1<T; t1++){
		if(Eig1[t1] < min){
			min = Eig1[t1];
		}

	}

	if(min > pow(10, -300)){
		
		Anull(L1, T, T);
		for (t1=0; t1<T; t1++){
			L1[t1][t1] = 1.0 / Eig1[t1];
		
		}
	
		XAXt(WC, T, L1, invPsik);


	}
	for(k=0; k<K; k++){

		detPsi[k] = det;

		cpyk2(invPsik, T, T, invPsi, k);

	}


	for(k=0; k<K; k++){

		for(t1=0; t1<T; t1++){
			for(t2=0; t2<T; t2++){

				invPsi[t1][t2][k] = pow(detPsi[k], 1.0/T) * invPsi[t1][t2][k];

			}

		}

		detPsi[k] = 1.0;

	}



	FREE_VECTOR(Eig1);
	FREE_MATRIX(MYi);
	FREE_MATRIX(tMYi);
	FREE_MATRIX(invSk);
	FREE_MATRIX(invPsik);
	FREE_MATRIX(temp1);
	FREE_MATRIX(temp2);
	FREE_MATRIX(Muk);
	FREE_MATRIX(WC);
	FREE_MATRIX(L1);
	
	FREE_3ARRAY(MY);

}




void modelA5(int p, int T, int n, int K, double ***Y, double **la, double **nu, double *tau, double ***Mu, double **gamma, double ***invS, double ***invPsi, double *detPsi, int trans_type){
  
	int i, k, t1, t2;
	double det = 1.0, det2;
	double **temp1, **temp2;
	double ***MY, **Muk, **MYi, **tMYi, **invPsik0, **invSk;
	double **WC, ***L, **Omega, **invOmega, *Eig1, **Lk;

	MAKE_3ARRAY(MY, p,T,n);
	MAKE_MATRIX(MYi, p, T);
	MAKE_MATRIX(tMYi, T, p);
	MAKE_MATRIX(invSk, p, p);
	MAKE_MATRIX(invPsik0, T, T);
	MAKE_MATRIX(temp1, T, p);
	MAKE_MATRIX(temp2, T, T);
	MAKE_MATRIX(Muk, p, T);
	MAKE_MATRIX(WC, T, T);
	MAKE_3ARRAY(L, T, T, K);
	MAKE_MATRIX(Omega, T, T);
	MAKE_MATRIX(invOmega, T, T);
	MAKE_MATRIX(Lk, T, T);
	MAKE_VECTOR(Eig1, T);


	Anull3(L, T, T, K);
	Anull3(invPsi, T, T, K);
	Anull(Omega, T, T);
	Anull(invOmega, T, T);	

	for(k=0; k<K; k++){

		Anull(WC, T, T);
		cpyk(invS, p, p, k, invSk);
		cpyk(Mu, p, T, k, Muk);
		Trans_trans_whole(n, p, T, la[k], nu[k], Y, MY, trans_type);	

		for(i=0; i<n; i++){

			cpyk(MY, p, T, i, MYi);

			mat_(p, T, MYi, Muk);
	
			tA(MYi, T, p, tMYi);

			multiply(tMYi, T, p, invSk, p, p, temp1);

			multiply(temp1, T, p, MYi, p, T, temp2);


			for(t1=0; t1<T; t1++){
				for(t2=0; t2<T; t2++){
					WC[t1][t2] += gamma[i][k] * temp2[t1][t2];
				}

			}

		}

		
		anull(Eig1, T);


		EigValDec(T, Eig1, WC, &det);

		for(t1=0; t1<T; t1++){

			Omega[t1][t1] += Eig1[t1];

		}	

		cpyk2(WC, T, T, L, k);
		


	}
	det2 = 1.0;

	for(t1=0; t1<T; t1++){

		det2 *= Omega[t1][t1];

	}	


	for(k=0; k<K; k++){
		detPsi[k] = 1.0;

	}

	for(t1=0; t1<T; t1++){

		invOmega[t1][t1] = pow(det2, 1.0/T) / Omega[t1][t1];



	}	



	for(k=0; k<K; k++){

		cpyk(L, T, T, k, Lk);
	
		XAXt(Lk, T, invOmega, invPsik0);
	
		cpyk2(invPsik0, T, T, invPsi, k);



	}




	FREE_3ARRAY(MY);
	FREE_VECTOR(Eig1);
	FREE_MATRIX(Lk);
	FREE_MATRIX(invOmega);
	FREE_MATRIX(Omega);
	FREE_3ARRAY(L);
	FREE_MATRIX(MYi);
	FREE_MATRIX(tMYi);
	FREE_MATRIX(invSk);
	FREE_MATRIX(invPsik0);
	FREE_MATRIX(temp1);
	FREE_MATRIX(temp2);
	FREE_MATRIX(Muk);
	FREE_MATRIX(WC);
	


}



void modelA6(int p, int T, int n, int K, double ***Y, double **la, double **nu, double *tau, double ***Mu, double **gamma, double ***invS, double ***invPsi, double *detPsi, int trans_type){
  
  
	int k, i, t1, t2, t;

	double min, det, ***MY, **MYi, **tMYi, **Psi, **temp3, **temp4, *sum_gamma, **invSk, *Eig1, **L1, **Muk, **invPsik;

	MAKE_3ARRAY(MY, p,T,n);
	MAKE_MATRIX(MYi, p, T);
	MAKE_MATRIX(tMYi, T, p);
	MAKE_MATRIX(Psi, T, T);
	MAKE_MATRIX(temp3, T, p);
	MAKE_MATRIX(temp4, T, T);
	MAKE_VECTOR(sum_gamma, K);
	MAKE_MATRIX(invSk, p, p);
	MAKE_VECTOR(Eig1, T);
	MAKE_MATRIX(L1, T, T);
	MAKE_MATRIX(Muk, p, T);
	MAKE_MATRIX(invPsik, T, T);



	for(k=0; k<K; k++){

		Anull(Psi, T, T);
		cpyk(invS, p, p, k, invSk);

	
		cpyk(Mu, p, T, k, Muk);
		Trans_trans_whole(n, p, T, la[k], nu[k], Y, MY, trans_type);	


		for(i=0; i<n; i++){

			cpyk(MY, p, T, i, MYi);

			mat_(p, T, MYi, Muk);

			tA(MYi, T, p, tMYi);

			multiply(tMYi, T, p, invSk, p, p, temp3);

			multiply(temp3, T, p, MYi, p, T, temp4);

			for(t1=0; t1<T; t1++){			

				for(t2=0; t2<T; t2++){
	
					Psi[t1][t2] += gamma[i][k] * temp4[t1][t2];
					
				}
			}


		}



		anull(Eig1, T);


		EigValDec(T, Eig1, Psi, &det);

	
		min = INFINITY;
		for(t=0; t<T; t++){
			if(Eig1[t] < min){
				min = Eig1[t];
			}

		}

	
		if(min < pow(10, -300)){
			break;
		}else{
		
			Anull(L1, T, T);

			for (t=0; t<T; t++){
				L1[t][t] = 1.0 / Eig1[t];
		
			}
	
			XAXt(Psi, T, L1, invPsik);

			detPsi[k] = det;



			cpyk2(invPsik, T, T, invPsi, k);

		}


	}

	for(k=0; k<K; k++){

		for(t1=0; t1<T; t1++){
			for(t2=0; t2<T; t2++){

				invPsi[t1][t2][k] = pow(detPsi[k], 1.0/T) * invPsi[t1][t2][k];

			}

		}

		detPsi[k] = 1.0;

	}
	FREE_3ARRAY(MY);
	FREE_MATRIX(invPsik);
	FREE_MATRIX(Muk);
	FREE_MATRIX(MYi);
	FREE_MATRIX(tMYi);
	FREE_MATRIX(Psi);
	FREE_MATRIX(temp3);
	FREE_MATRIX(temp4);
	FREE_VECTOR(sum_gamma);
	FREE_MATRIX(invSk);
	FREE_VECTOR(Eig1);
	FREE_MATRIX(L1);


}



void findPsi2phi(int n, int p, int T, double *par, double ***MY, double **Muk, double *gamma_k, double **invSk, double eps){

	int t1, t2, i, t, j;

	double phi, Psi2, traceA1 = 0.0, traceA2 = 0.0, traceI = 0.0, sum_gamma = 0.0, trA1, trA2, trI, *coeff;
	
	double **A1, **A2, **I, **MYi, **tMYi, **temp1, **temp2, **temp3, **maha1, **maha2, **maha3, *try, *start;



	MAKE_MATRIX(A1, T, T);
	MAKE_MATRIX(A2, T, T);
	MAKE_MATRIX(I, T, T);
	MAKE_MATRIX(MYi, p, T);
	MAKE_MATRIX(tMYi, T, p);
	MAKE_MATRIX(temp1, p, T);
	MAKE_MATRIX(temp2, p, T);
	MAKE_MATRIX(temp3, p, T);
	MAKE_MATRIX(maha1, p, p);
	MAKE_MATRIX(maha2, p, p);
	MAKE_MATRIX(maha3, p, p);

	MAKE_VECTOR(try, 3);
	MAKE_VECTOR(coeff, 3);
	MAKE_VECTOR(start, 3);

	
 

	Psi2 = par[0];
	phi = par[1];
	
	for(i=0; i<n; i++){	
	
		sum_gamma += gamma_k[i];
	
	}
	
	Anull(A1, T, T);
	Anull(A2, T, T);
	Anull(I, T, T);

	//printf("Q psi2 phi %lf %lf \n", Psi2, phi);
	for(t1=0; t1<T; t1++){

		for(t2=0; t2<T; t2++){

			if(abs(t1-t2) == 1){

				A1[t1][t2] = 1;
 

			}

		}

	}

	for(t=1; t<T-1; t++){


		A2[t][t] = 1;
 

	}


	for(t=0; t<T; t++){


		I[t][t] = 1;
 

	}



	for(i=0; i<n; i++){


		cpyk(MY, p, T, i, MYi);

		mat_(p, T, MYi, Muk);

		tA(MYi, T, p, tMYi);

		multiply(invSk, p, p, MYi, p, T, temp1);

		multiply(temp1, p, T, A1, T, T, temp2);

		multiply(temp2, p, T, tMYi, T, p, maha1);

		multiply(temp1, p, T, A2, T, T, temp3);

		multiply(temp3, p, T, tMYi, T, p, maha2);

		multiply(temp1, p, T, tMYi, T, p, maha3);
		
		trA1 = 0;
		trA2 = 0;
		trI = 0;

		for(j=0; j<p; j++){			
	
			trA1 += maha1[j][j];
			trA2 += maha2[j][j];
			trI += maha3[j][j];
		}
		traceA1 += gamma_k[i] * trA1;
		traceA2 += gamma_k[i] * trA2;
		traceI += gamma_k[i] * trI;

	}

	coeff[0] = (1.0 - 2.0 / T) * traceA1 / (2.0 / T - 2) / traceA2;
	coeff[1] = (2 * traceA2 + 2.0 / T * traceI) / (2.0 / T - 2) / traceA2;
	coeff[2] = -traceA1 / (2.0 / T - 2) / traceA2;

	start[0] = 0.5;
	start[1] = 0;
	start[2] = -0.5;

	rootfinding(eq3, start, coeff, eps);


	
	try[0] = (-start[0] * traceA1 + pow(start[0], 2) * traceA2 + traceI) / p / T / sum_gamma;
	try[1] = (-start[1] * traceA1 + pow(start[1], 2) * traceA2 + traceI) / p / T / sum_gamma;
	try[2] = (-start[2] * traceA1 + pow(start[2], 2) * traceA2 + traceI) / p / T / sum_gamma;
	
	if(fabs(start[0])<1 && try[0]>0) {
		phi = start[0];
		Psi2 = try[0];

	}
	else if(fabs(start[1])<1 && try[1]>0) {
		phi = start[1];
		Psi2 = try[1];

	}
	else if(fabs(start[2])<1 && try[2]>0) {
		phi = start[2];
		Psi2 = try[2];

	}
	par[0] = Psi2;
	par[1] = phi;
	
	//printf("out psi2 phi %lf %lf \n", Psi2, phi);

	FREE_VECTOR(coeff);

	FREE_MATRIX(A1);
	FREE_MATRIX(A2);
	FREE_MATRIX(I);
	FREE_MATRIX(MYi);
	FREE_MATRIX(tMYi);
	FREE_MATRIX(temp1);
	FREE_MATRIX(temp2);
	FREE_MATRIX(temp3);
	FREE_MATRIX(maha1);
	FREE_MATRIX(maha2);
	FREE_VECTOR(start);
	FREE_MATRIX(maha3);

	FREE_VECTOR(try);


}

double eq3(double x, double *coeff)
{
    // the function we are interested in

    return pow(x,3) + coeff[0] * pow(x,2) + coeff[1] * x + coeff[2];
}




void rootfinding(double (*func)(double, double *), double *start, double *coeff, double eps){

	int i=0;
	int max_iterations = 1000;
	int done = 0;

	double p0,q0,r0,p,q,r;

	p = start[0];
	q = start[1];
	r = start[2];

	while (i<max_iterations && done == 0){   
		p0 = p;
		q0 = q;
		r0 = r;


		p = p0 - func(p0, coeff)/((p0-q0)*(p0-r0));
		q = q0 - func(q0, coeff)/((q0-p)*(q0-r0));
		r = r0 - func(r0, coeff)/((r0-p)*(r0-q));


    		if (fabs(p-p0)<eps && fabs(q-q0)<eps && fabs(r-r0)<eps){
        		done = 1;
			start[0] = p;
			start[1] = q;
			start[2] = r;

		}
		i++;
	}

}






