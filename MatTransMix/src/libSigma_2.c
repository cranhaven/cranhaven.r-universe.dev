

#include "array.h"
#include "math.h"
#include "MatTransMix.h"
 
#define Inf 1e+140



#include <R.h>
#include <Rmath.h>



void modelB8(int p, int T, int n, int K, double ***Y, double **la, double **nu, double *tau, double ***Mu, double **gamma, double ***invS, double ***invPsi, double *detS, int trans_type){

	int i,k,j1,j2,j;
	double *psi; 
	double ***WR, ***MY, **Muk, **invPsik, **MYi, **tMYi, **temp1, **temp2, **Delta, **invDelta, *Eig2, det, **L2, **WRk;
	double det1p, min;

	MAKE_3ARRAY(MY, p,T,n);
	MAKE_MATRIX(WRk, p, p);
	MAKE_3ARRAY(WR, p, p, K);
	MAKE_MATRIX(Muk, p, T);
	MAKE_MATRIX(invPsik, T, T);
	MAKE_MATRIX(MYi, p, T);
	MAKE_MATRIX(tMYi, T, p);
	MAKE_MATRIX(temp1, p, T);
	MAKE_MATRIX(temp2, p, p);
	MAKE_VECTOR(psi, K);
	MAKE_VECTOR(Eig2, p);
	MAKE_MATRIX(Delta, p, p);
	MAKE_MATRIX(invDelta, p, p);
	MAKE_MATRIX(L2, p, p);

	Anull3(WR, p, p, K);
	Anull3(invS, p, p, K);

	anull(psi, K);
	Anull(Delta, p, p);

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
			for(j2=0; j2<p; j2++){
				Delta[j1][j2] += WR[j1][j2][k] / pow(detS[k], 1.0/p);
			}

		}

	
	}


	anull(Eig2, p);

	EigValDec(p, Eig2, Delta, &det);


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
	
		XAXt(Delta, p, L2, invDelta);

	}
	det1p = pow(det, 1.0/p);
	for(j1=0; j1<p; j1++){
		for(j2=0; j2<p; j2++){
			invDelta[j1][j2] = invDelta[j1][j2] * det1p;
		}

	}


	for(k=0; k<K; k++){

		cpyk(WR, p, p, k, WRk);
		multiply(invDelta, p, p, WRk, p, p, temp2);
		for(j1=0; j1<p; j1++){
			psi[k] += temp2[j1][j1]/T/p/tau[k]/n;
		}

		for(j1=0; j1<p; j1++){
			for(j2=0; j2<p; j2++){
				invS[j1][j2][k] = invDelta[j1][j2] / psi[k];
			}

		}
		detS[k] = pow(psi[k], p);
	}

	FREE_MATRIX(invDelta);	
	FREE_MATRIX(Delta);
	FREE_MATRIX(L2);
	FREE_3ARRAY(MY);
	FREE_3ARRAY(WR);
	FREE_MATRIX(WRk);
	FREE_MATRIX(Muk);
	FREE_MATRIX(invPsik);
	FREE_MATRIX(MYi);
	FREE_MATRIX(tMYi);
	FREE_MATRIX(temp1);
	FREE_MATRIX(temp2);
	FREE_VECTOR(psi);
	FREE_VECTOR(Eig2);


}


void modelB9(int p, int T, int n, int K, double ***Y, double **la, double **nu, double *tau, double ***Mu, double **gamma, double ***invS, double ***invPsi, double *detS, int trans_type){

	int i,k,j1,j2,j, iter = 0;
	double psi; 
	double ***WR, ***MY, **Muk, **invPsik, **MYi, **tMYi, **temp1, **temp2, **Delta, ***invDelta, *Eig2, det, **L2, **WRk;
	double det1p, **pooled, **invSk, *Eig1, det1, **L, **Sk, det2, *Eig;


	double **Gamma_max;

	double fun, fun_new, **Ft, **invDeltak, **tGamma_max, **temp3, **temp4;

	double *wk, *w, **tPt, **Pt, **Rt, **tRt, **temp5, **temp6, **temp7, **final, **temp8;



	MAKE_3ARRAY(MY, p,T,n);
	MAKE_MATRIX(WRk, p, p);
	MAKE_3ARRAY(WR, p, p, K);
	MAKE_MATRIX(Muk, p, T);
	MAKE_MATRIX(invPsik, T, T);
	MAKE_MATRIX(MYi, p, T);
	MAKE_MATRIX(tMYi, T, p);
	MAKE_MATRIX(temp1, p, T);
	MAKE_MATRIX(temp2, p, p);

	MAKE_VECTOR(Eig2, p);
	MAKE_VECTOR(Eig, p);
	MAKE_MATRIX(Delta, p, p);
	MAKE_3ARRAY(invDelta, p, p, K);
	MAKE_MATRIX(L2, p, p);
	MAKE_MATRIX(pooled, p, p);




	MAKE_MATRIX(invSk, p, p);
	MAKE_VECTOR(Eig1, p);
	MAKE_MATRIX(Sk, p, p);


	MAKE_MATRIX(L, p, p);
	MAKE_VECTOR(wk, K);
	MAKE_MATRIX(Gamma_max, p, p);	

	MAKE_MATRIX(invDeltak, p, p);	
	MAKE_MATRIX(Ft, p, p);	
	MAKE_MATRIX(tGamma_max, p, p);
	MAKE_MATRIX(temp3, p, p);
	MAKE_MATRIX(temp4, p, p);
	MAKE_MATRIX(tRt, p, p);
	MAKE_MATRIX(tPt, p, p);
	MAKE_MATRIX(Pt, p, p);
	MAKE_VECTOR(w, p);
	MAKE_MATRIX(Rt, p, p);

	MAKE_MATRIX(temp5, 2*p, p);
	MAKE_MATRIX(temp6, p, p);
	MAKE_MATRIX(temp7, p, p);
	MAKE_MATRIX(final, p, p);

	MAKE_MATRIX(temp8, p, p);




	Anull3(WR, p, p, K);

	Anull(Delta, p, p);

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
	
	}
	Anull3(invDelta, p, p, K);
	
	Anull(pooled, p, p);


	for(k=0; k<K; k++){

		cpyk(invS, p, p, k, invSk);



		EigValDec(p, Eig1, invSk, &det1);

		Anull(L, p, p);

		for (j=0; j<p; j++){
			L[j][j] = 1.0 / Eig1[j];
		
		}
	
		XAXt(invSk, p, L, Sk);




		for(j1=0; j1<p; j1++){
			invDelta[j1][j1][k] = Eig1[j1];
		
		}


		for(j1=0; j1<p; j1++){
			for(j2=0; j2<p; j2++){
				pooled[j1][j2] += tau[k]* Sk[j1][j2];
			}
		} 



	}



	EigValDec(p, Eig2, pooled, &det2);

	cpy(pooled, p, p, Gamma_max);

	for(k=0; k<K; k++){

		cpyk(WR, p, p, k, WRk);


		EigValDec(p, Eig, WRk, &det);
		wk[k] = Eig[p-1];	

	}



	for(k=0; k<K; k++){

		cpyk(WR, p, p, k, WRk);
		tA(Gamma_max, p, p, tGamma_max);
		multiply(tGamma_max, p, p, WRk, p, p, temp6);
		
		multiply(temp6, p, p, Gamma_max, p, p, temp7);

		Anull(invDeltak, p, p);
		

		det1p = 1.0;
		for(j1=0; j1<p; j1++){
			invDeltak[j1][j1] = temp7[j1][j1];
			det1p *= temp7[j1][j1];			
		}
		det1p = pow(det1p, 1.0/p);

		for(j1=0; j1<p; j1++){
			invDeltak[j1][j1] = det1p / invDeltak[j1][j1];
			
		}

		cpyk2(invDeltak, p, p, invDelta, k);

	}


	fun_new = 0;



	do{
		iter += 1;
		fun = fun_new;


		Anull(Ft, p, p);
		for(k=0; k<K; k++){	
	
			cpyk(invDelta, p, p, k, invDeltak);
			tA(Gamma_max, p, p, tGamma_max);
			cpyk(WR, p, p, k, WRk);

		
			multiply(invDeltak, p, p, tGamma_max, p, p, temp3);

			multiply(temp3, p, p, WRk, p, p, temp4);


			for(j1=0; j1<p; j1++){
				for(j2=0; j2<p; j2++){

					Ft[j1][j2] += temp4[j1][j2] - wk[k]*temp3[j1][j2];
				}

			}	

		}
		Anull(temp5, 2*p, p);


		for(j1=0; j1<p; j1++){
			for(j2=0; j2<p; j2++){

				temp5[j1][j2] = Ft[j1][j2];
			}

		}	

		svd(temp5, w, p);

		for(j1=0; j1<p; j1++){
			for(j2=0; j2<p; j2++){

				Pt[j1][j2] = temp5[j1][j2];
			}

		}	

		for(j1=0; j1<p; j1++){
			for(j2=0; j2<p; j2++){

				Pt[j1][j2] = Pt[j1][j2] / pow(w[j2], 1.0/2.0);
			}

		}	

		for(j1=p; j1<2*p; j1++){
			for(j2=0; j2<p; j2++){

				Rt[j1-p][j2] = temp5[j1][j2];
			}

		}			

		tA(Pt, p, p, tPt);

		multiply(Rt, p, p, tPt, p, p, Gamma_max);

		fun_new = 0.0;

		for(k=0; k<K; k++){	

			cpyk(WR, p, p, k, WRk);
			cpyk(invDelta, p, p, k, invDeltak);
			tA(Gamma_max, p, p, tGamma_max);

			multiply(WRk, p, p, Gamma_max, p, p, temp6);
			multiply(temp6, p, p, invDeltak, p, p, temp7);
			multiply(temp7, p, p, tGamma_max, p, p, final);
			for(j1=0; j1<p; j1++){
				fun_new += final[j1][j1];
			} 


		}


	}
	while((iter < 10000) && (fabs(fun - fun_new) / fabs(fun) > pow(10, -7)));


	for(k=0; k<K; k++){

		cpyk(WR, p, p, k, WRk);

		multiply(tGamma_max, p, p, WRk, p, p, temp6);
		
		multiply(temp6, p, p, Gamma_max, p, p, temp7);

		Anull(invDeltak, p, p);
		
		det1p = 1.0;
		for(j1=0; j1<p; j1++){
			invDeltak[j1][j1] = temp7[j1][j1];
			det1p *= temp7[j1][j1];
			
		}
		det1p = pow(det1p, 1.0/p);

		for(j1=0; j1<p; j1++){
			invDeltak[j1][j1] = det1p / invDeltak[j1][j1];
			
		}

		cpyk2(invDeltak, p, p, invDelta, k);

	}

	psi = 0.0;
	for(k=0; k<K; k++){

		cpyk(WR, p, p, k, WRk);

		cpyk(invDelta, p, p, k, invDeltak);

		multiply(Gamma_max, p, p, invDeltak, p, p, temp8);

		multiply(temp8, p, p, tGamma_max, p, p, temp6);

		multiply(temp6, p, p, WRk, p, p, temp7);

		
		for(j1=0; j1<p; j1++){
			psi += temp7[j1][j1] /n/p/T;
			
		}
	}


	for(k=0; k<K; k++){

		detS[k] = pow(psi, p);

		cpyk(invDelta, p, p, k, invDeltak);

		multiply(Gamma_max, p, p, invDeltak, p, p, temp8);

		multiply(temp8, p, p, tGamma_max, p, p, temp6);

		for(j1=0; j1<p; j1++){
			for(j2=0; j2<p; j2++){

				invSk[j1][j2] = temp6[j1][j2]/psi;
			}

		}	

		cpyk2(invSk, p, p, invS, k);	

	}

	FREE_MATRIX(pooled);
	FREE_3ARRAY(invDelta);	
	FREE_MATRIX(Delta);
	FREE_MATRIX(L2);
	FREE_3ARRAY(MY);
	FREE_3ARRAY(WR);
	FREE_MATRIX(WRk);
	FREE_MATRIX(Muk);
	FREE_MATRIX(invPsik);
	FREE_MATRIX(MYi);
	FREE_MATRIX(tMYi);
	FREE_MATRIX(temp1);
	FREE_MATRIX(temp2);
	FREE_VECTOR(Eig);
	FREE_VECTOR(Eig2);


	FREE_MATRIX(invSk);
	FREE_VECTOR(Eig1);
	FREE_MATRIX(L);
	FREE_MATRIX(Sk);


	FREE_VECTOR(wk);
	FREE_MATRIX(Gamma_max);	

	FREE_MATRIX(invDeltak);	
	FREE_MATRIX(Ft);	
	FREE_MATRIX(tGamma_max);
	FREE_MATRIX(temp3);
	FREE_MATRIX(temp4);
	FREE_MATRIX(tRt);
	FREE_MATRIX(tPt);
	FREE_MATRIX(Pt);
	FREE_VECTOR(w);
	FREE_MATRIX(Rt);

	FREE_MATRIX(temp5);
	FREE_MATRIX(temp6);
	FREE_MATRIX(temp7);
	FREE_MATRIX(final);

	FREE_MATRIX(temp8);



}



void modelB10(int p, int T, int n, int K, double ***Y, double **la, double **nu, double *tau, double ***Mu, double **gamma, double ***invS, double ***invPsi, double *detS, int trans_type){

	int i,k,j1,j2,j, iter = 0;
	double *psi; 
	double ***WR, ***MY, **Muk, **invPsik, **MYi, **tMYi, **temp1, **temp2, **Delta, ***invDelta, *Eig2, det, **L2, **WRk;
	double det1p, **pooled, **invSk, *Eig1, det1, **L, **Sk, det2, *Eig;


	double **Gamma_max;

	double fun, fun_new, **Ft, **invDeltak, **tGamma_max, **temp3, **temp4;

	double *wk, *w, **tPt, **Pt, **Rt, **tRt, **temp5, **temp6, **temp7, **final, **temp8;



	MAKE_3ARRAY(MY, p,T,n);
	MAKE_MATRIX(WRk, p, p);
	MAKE_3ARRAY(WR, p, p, K);
	MAKE_MATRIX(Muk, p, T);
	MAKE_MATRIX(invPsik, T, T);
	MAKE_MATRIX(MYi, p, T);
	MAKE_MATRIX(tMYi, T, p);
	MAKE_MATRIX(temp1, p, T);
	MAKE_MATRIX(temp2, p, p);

	MAKE_VECTOR(psi, K);
	MAKE_VECTOR(Eig2, p);
	MAKE_VECTOR(Eig, p);
	MAKE_MATRIX(Delta, p, p);
	MAKE_3ARRAY(invDelta, p, p, K);
	MAKE_MATRIX(L2, p, p);
	MAKE_MATRIX(pooled, p, p);




	MAKE_MATRIX(invSk, p, p);
	MAKE_VECTOR(Eig1, p);
	MAKE_MATRIX(Sk, p, p);


	MAKE_MATRIX(L, p, p);
	MAKE_VECTOR(wk, K);
	MAKE_MATRIX(Gamma_max, p, p);	

	MAKE_MATRIX(invDeltak, p, p);	
	MAKE_MATRIX(Ft, p, p);	
	MAKE_MATRIX(tGamma_max, p, p);
	MAKE_MATRIX(temp3, p, p);
	MAKE_MATRIX(temp4, p, p);
	MAKE_MATRIX(tRt, p, p);
	MAKE_MATRIX(tPt, p, p);
	MAKE_MATRIX(Pt, p, p);
	MAKE_VECTOR(w, p);
	MAKE_MATRIX(Rt, p, p);

	MAKE_MATRIX(temp5, 2*p, p);
	MAKE_MATRIX(temp6, p, p);
	MAKE_MATRIX(temp7, p, p);
	MAKE_MATRIX(final, p, p);

	MAKE_MATRIX(temp8, p, p);




	Anull3(WR, p, p, K);

	Anull(Delta, p, p);

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
	
	}
	Anull3(invDelta, p, p, K);
	
	Anull(pooled, p, p);


	for(k=0; k<K; k++){

		cpyk(invS, p, p, k, invSk);


		EigValDec(p, Eig1, invSk, &det1);

		Anull(L, p, p);

		for (j=0; j<p; j++){
			L[j][j] = 1.0 / Eig1[j];
		
		}
	
		XAXt(invSk, p, L, Sk);




		for(j1=0; j1<p; j1++){
			invDelta[j1][j1][k] = Eig1[j1];
		
		}


		for(j1=0; j1<p; j1++){
			for(j2=0; j2<p; j2++){
				pooled[j1][j2] += tau[k]* Sk[j1][j2];
			}
		} 



	}



	EigValDec(p, Eig2, pooled, &det2);


	cpy(pooled, p, p, Gamma_max);

	for(k=0; k<K; k++){

		cpyk(WR, p, p, k, WRk);


		EigValDec(p, Eig, WRk, &det);

		wk[k] = Eig[p-1];	

	}



	for(k=0; k<K; k++){

		cpyk(WR, p, p, k, WRk);
		tA(Gamma_max, p, p, tGamma_max);
		multiply(tGamma_max, p, p, WRk, p, p, temp6);
		
		multiply(temp6, p, p, Gamma_max, p, p, temp7);

		Anull(invDeltak, p, p);
		

		det1p = 1.0;
		for(j1=0; j1<p; j1++){
			invDeltak[j1][j1] = temp7[j1][j1];
			det1p *= temp7[j1][j1];			
		}
		det1p = pow(det1p, 1.0/p);

		for(j1=0; j1<p; j1++){
			invDeltak[j1][j1] = det1p / invDeltak[j1][j1];
			
		}

		cpyk2(invDeltak, p, p, invDelta, k);

	}


	fun_new = 0;



	do{
		iter += 1;
		fun = fun_new;

		Anull(Ft, p, p);
		for(k=0; k<K; k++){	
	
			cpyk(invDelta, p, p, k, invDeltak);
			tA(Gamma_max, p, p, tGamma_max);
			cpyk(WR, p, p, k, WRk);

		
			multiply(invDeltak, p, p, tGamma_max, p, p, temp3);

			multiply(temp3, p, p, WRk, p, p, temp4);


			for(j1=0; j1<p; j1++){
				for(j2=0; j2<p; j2++){

					Ft[j1][j2] += temp4[j1][j2] - wk[k]*temp3[j1][j2];
				}

			}	

		}
		Anull(temp5, 2*p, p);


		for(j1=0; j1<p; j1++){
			for(j2=0; j2<p; j2++){

				temp5[j1][j2] = Ft[j1][j2];
			}

		}	

		svd(temp5, w, p);

		for(j1=0; j1<p; j1++){
			for(j2=0; j2<p; j2++){

				Pt[j1][j2] = temp5[j1][j2];
			}

		}	

		for(j1=0; j1<p; j1++){
			for(j2=0; j2<p; j2++){

				Pt[j1][j2] = Pt[j1][j2] / pow(w[j2], 1.0/2.0);
			}

		}	

		for(j1=p; j1<2*p; j1++){
			for(j2=0; j2<p; j2++){

				Rt[j1-p][j2] = temp5[j1][j2];
			}

		}			

		tA(Pt, p, p, tPt);

		multiply(Rt, p, p, tPt, p, p, Gamma_max);

		fun_new = 0.0;

		for(k=0; k<K; k++){	

			cpyk(WR, p, p, k, WRk);
			cpyk(invDelta, p, p, k, invDeltak);
			tA(Gamma_max, p, p, tGamma_max);

			multiply(WRk, p, p, Gamma_max, p, p, temp6);
			multiply(temp6, p, p, invDeltak, p, p, temp7);
			multiply(temp7, p, p, tGamma_max, p, p, final);
			for(j1=0; j1<p; j1++){
				fun_new += final[j1][j1];
			} 


		}


	}
	while((iter < 10000) && (fabs(fun - fun_new) / fabs(fun) > pow(10, -7)));


	for(k=0; k<K; k++){

		cpyk(WR, p, p, k, WRk);

		multiply(tGamma_max, p, p, WRk, p, p, temp6);
		
		multiply(temp6, p, p, Gamma_max, p, p, temp7);

		Anull(invDeltak, p, p);
		
		det1p = 1.0;
		for(j1=0; j1<p; j1++){
			invDeltak[j1][j1] = temp7[j1][j1];
			det1p *= temp7[j1][j1];
			
		}
		det1p = pow(det1p, 1.0/p);

		for(j1=0; j1<p; j1++){
			invDeltak[j1][j1] = det1p / invDeltak[j1][j1];
			
		}

		cpyk2(invDeltak, p, p, invDelta, k);

	}

	anull(psi, K);
	for(k=0; k<K; k++){

		cpyk(WR, p, p, k, WRk);

		cpyk(invDelta, p, p, k, invDeltak);

		multiply(Gamma_max, p, p, invDeltak, p, p, temp8);

		multiply(temp8, p, p, tGamma_max, p, p, temp6);

		multiply(temp6, p, p, WRk, p, p, temp7);

		
		for(j1=0; j1<p; j1++){
			psi[k] += temp7[j1][j1] /n/p/T/tau[k];
			
		}
	}


	for(k=0; k<K; k++){

		detS[k] = pow(psi[k], p);

		cpyk(invDelta, p, p, k, invDeltak);

		multiply(Gamma_max, p, p, invDeltak, p, p, temp8);

		multiply(temp8, p, p, tGamma_max, p, p, temp6);

		for(j1=0; j1<p; j1++){
			for(j2=0; j2<p; j2++){

				invSk[j1][j2] = temp6[j1][j2]/psi[k];
			}

		}	

		cpyk2(invSk, p, p, invS, k);	

	}

	FREE_MATRIX(pooled);
	FREE_3ARRAY(invDelta);	
	FREE_MATRIX(Delta);
	FREE_MATRIX(L2);
	FREE_3ARRAY(MY);
	FREE_3ARRAY(WR);
	FREE_MATRIX(WRk);
	FREE_MATRIX(Muk);
	FREE_MATRIX(invPsik);
	FREE_MATRIX(MYi);
	FREE_MATRIX(tMYi);
	FREE_MATRIX(temp1);
	FREE_MATRIX(temp2);
	FREE_VECTOR(Eig);
	FREE_VECTOR(Eig2);


	FREE_MATRIX(invSk);
	FREE_VECTOR(Eig1);
	FREE_MATRIX(L);
	FREE_MATRIX(Sk);

	FREE_VECTOR(psi);

	FREE_VECTOR(wk);
	FREE_MATRIX(Gamma_max);	

	FREE_MATRIX(invDeltak);	
	FREE_MATRIX(Ft);	
	FREE_MATRIX(tGamma_max);
	FREE_MATRIX(temp3);
	FREE_MATRIX(temp4);
	FREE_MATRIX(tRt);
	FREE_MATRIX(tPt);
	FREE_MATRIX(Pt);
	FREE_VECTOR(w);
	FREE_MATRIX(Rt);

	FREE_MATRIX(temp5);
	FREE_MATRIX(temp6);
	FREE_MATRIX(temp7);
	FREE_MATRIX(final);

	FREE_MATRIX(temp8);



}




void modelB11(int p, int T, int n, int K, double ***Y, double **la, double **nu, double *tau, double ***Mu, double **gamma, double ***invS, double ***invPsi, double *detS, int trans_type){

	int i,k,j1,j2,j;
	double *psi; 
	double ***WR, ***MY, **Muk, **invPsik, **MYi, **tMYi, **temp1, **temp2, **temp3, **Omega, ***L, **invDelta, *Eig2, det, **L2, **WRk, **Lk, **tLk;
	
	MAKE_3ARRAY(MY, p,T,n);
	MAKE_MATRIX(WRk, p, p);
	MAKE_3ARRAY(WR, p, p, K);
	MAKE_MATRIX(Muk, p, T);
	MAKE_MATRIX(invPsik, T, T);
	MAKE_MATRIX(MYi, p, T);
	MAKE_MATRIX(tMYi, T, p);
	MAKE_MATRIX(temp1, p, T);
	MAKE_MATRIX(temp2, p, p);
	MAKE_MATRIX(temp3, p, p);
	MAKE_VECTOR(psi, K);
	MAKE_VECTOR(Eig2, p);
	MAKE_MATRIX(Omega, p,p);	
	MAKE_MATRIX(Lk, p,p);
	MAKE_MATRIX(tLk, p,p);
	MAKE_3ARRAY(L, p,p,K);

	MAKE_MATRIX(invDelta, p, p);
	MAKE_MATRIX(L2, p, p);

	Anull3(WR, p, p, K);
	Anull3(invS, p, p, K);

	Anull(Omega, p, p);
	Anull3(L, p, p, K);
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

		cpyk(WR, p, p, k, WRk);

		anull(Eig2, p);

		EigValDec(p, Eig2, WRk, &det);
	
		for(j=0; j<p; j++){
			Omega[j][j] += Eig2[j];
		}
		

		cpyk2(WRk, p, p, L, k);

	
	}
	
	for(k=0; k<K; k++){
		detS[k] = 1.0;
		for(j=0; j<p; j++){
			detS[k] *= Omega[j][j]/n/T;
		}
	}

	for(j=0; j<p; j++){
		Omega[j][j] = 1.0 /Omega[j][j];
	}
		
	for(k=0; k<K; k++){

		cpyk(L, p, p, k, Lk);
		tA(Lk, p, p, tLk);

		multiply(Lk, p, p, Omega, p, p, temp2);
		multiply(temp2, p, p, tLk, p, p, temp3);

		for(j1=0; j1<p; j1++){
			for(j2=0; j2<p; j2++){
				invS[j1][j2][k] = temp3[j1][j2]*n*T;
			}

		}
		
	}
	FREE_MATRIX(Lk);
	FREE_MATRIX(tLk);
	FREE_MATRIX(invDelta);	
	FREE_MATRIX(Omega);
	FREE_3ARRAY(L);
	FREE_MATRIX(L2);
	FREE_3ARRAY(MY);
	FREE_3ARRAY(WR);
	FREE_MATRIX(WRk);
	FREE_MATRIX(Muk);
	FREE_MATRIX(invPsik);
	FREE_MATRIX(MYi);
	FREE_MATRIX(tMYi);
	FREE_MATRIX(temp1);
	FREE_MATRIX(temp2);
	FREE_MATRIX(temp3);
	FREE_VECTOR(psi);
	FREE_VECTOR(Eig2);


}




void modelB12(int p, int T, int n, int K, double ***Y, double **la, double **nu, double *tau, double ***Mu, double **gamma, double ***invS, double ***invPsi, double *detS, int trans_type){

	int i,k,j1,j2,j;
	double *psi, det1p; 
	double ***WR, ***MY, **Muk, **invPsik, **MYi, **tMYi, **temp1, **temp2, **temp3, ***Omega, **Omegak, ***L, **Delta, **invDelta, *Eig2, det, **L2, **WRk, **Lk, **tLk;
	
	MAKE_3ARRAY(MY, p,T,n);
	MAKE_MATRIX(WRk, p, p);
	MAKE_3ARRAY(WR, p, p, K);
	MAKE_MATRIX(Muk, p, T);
	MAKE_MATRIX(invPsik, T, T);
	MAKE_MATRIX(MYi, p, T);
	MAKE_MATRIX(tMYi, T, p);
	MAKE_MATRIX(temp1, p, T);
	MAKE_MATRIX(temp2, p, p);
	MAKE_MATRIX(temp3, p, p);
	MAKE_VECTOR(psi, K);
	MAKE_VECTOR(Eig2, p);
	MAKE_3ARRAY(Omega, p,p, K);	
	MAKE_MATRIX(Omegak, p, p);
	MAKE_MATRIX(Lk, p,p);
	MAKE_MATRIX(tLk, p,p);
	MAKE_3ARRAY(L, p,p,K);
	MAKE_MATRIX(Delta, p, p);
	MAKE_MATRIX(invDelta, p, p);
	MAKE_MATRIX(L2, p, p);

	Anull3(WR, p, p, K);
	Anull3(invS, p, p, K);

	Anull3(Omega, p, p, K);
	Anull3(L, p, p, K);
	anull(psi, K);
	Anull(Delta, p, p);


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

		cpyk(WR, p, p, k, WRk);

		anull(Eig2, p);

		EigValDec(p, Eig2, WRk, &det);
	
		for(j=0; j<p; j++){
			Omega[j][j][k] = Eig2[j];
		}
		

		cpyk2(WRk, p, p, L, k);

	
	}
	

	det1p = 1.0;

	for(k=0; k<K; k++){

		for(j=0; j<p; j++){
			Delta[j][j] += Omega[j][j][k]/pow(detS[k], 1.0/p);
		
		}
		

	}
	for(j=0; j<p; j++){
			
		det1p *= Delta[j][j];
	}

	det1p = pow(det1p, 1.0/p);


	for(j=0; j<p; j++){
		Delta[j][j] = det1p / Delta[j][j];
		
	}
		


	for(k=0; k<K; k++){

		cpyk(Omega, p, p, k, Omegak);
		multiply(Omegak, p, p, Delta, p, p, temp2);

		for(j=0; j<p; j++){
			psi[k] += temp2[j][j] /T/p/tau[k]/n;
		
		}	
		
		cpyk(L, p, p, k, Lk);
		tA(Lk, p, p, tLk);

		multiply(Lk, p, p, Delta, p, p, temp2);
		multiply(temp2, p, p, tLk, p, p, temp3);

		for(j1=0; j1<p; j1++){
			for(j2=0; j2<p; j2++){
				invS[j1][j2][k] = temp3[j1][j2] / psi[k];
			}

		}
		detS[k] = pow(psi[k], p);
		
	}
	FREE_MATRIX(Lk);
	FREE_MATRIX(tLk);
	FREE_MATRIX(invDelta);	
	FREE_MATRIX(Delta);	
	FREE_3ARRAY(Omega);
	FREE_3ARRAY(L);
	FREE_MATRIX(L2);
	FREE_3ARRAY(MY);
	FREE_3ARRAY(WR);
	FREE_MATRIX(WRk);
	FREE_MATRIX(Muk);
	FREE_MATRIX(invPsik);
	FREE_MATRIX(MYi);
	FREE_MATRIX(tMYi);
	FREE_MATRIX(temp1);
	FREE_MATRIX(temp2);
	FREE_MATRIX(temp3);
	FREE_MATRIX(Omegak);
	FREE_VECTOR(psi);
	FREE_VECTOR(Eig2);


}





void modelB13(int p, int T, int n, int K, double ***Y, double **la, double **nu, double *tau, double ***Mu, double **gamma, double ***invS, double ***invPsi, double *detS, int trans_type){

	int i,k,j1,j2,j;
	double psi =0.0, *det1p; 
	double ***WR, ***invWR, ***MY, **Muk, **invPsik, **MYi, **tMYi, **temp1, **temp2, **temp3, ***Omega, **Omegak, ***L, **Delta, **invDelta, *Eig2, det, **L2, **WRk, **invWRk, **Lk, **tLk, min;
	
	MAKE_3ARRAY(MY, p,T,n);
	MAKE_MATRIX(WRk, p, p);
	MAKE_3ARRAY(WR, p, p, K);
	MAKE_MATRIX(invWRk, p, p);
	MAKE_3ARRAY(invWR, p, p, K);
	MAKE_MATRIX(Muk, p, T);
	MAKE_MATRIX(invPsik, T, T);
	MAKE_MATRIX(MYi, p, T);
	MAKE_MATRIX(tMYi, T, p);
	MAKE_MATRIX(temp1, p, T);
	MAKE_MATRIX(temp2, p, p);
	MAKE_MATRIX(temp3, p, p);
	MAKE_VECTOR(det1p, K);
	MAKE_VECTOR(Eig2, p);
	MAKE_3ARRAY(Omega, p,p, K);	
	MAKE_MATRIX(Omegak, p, p);
	MAKE_MATRIX(Lk, p,p);
	MAKE_MATRIX(tLk, p,p);
	MAKE_3ARRAY(L, p,p,K);
	MAKE_MATRIX(Delta, p, p);
	MAKE_MATRIX(invDelta, p, p);
	MAKE_MATRIX(L2, p, p);

	Anull3(WR, p, p, K);
	Anull3(invS, p, p, K);

	Anull3(Omega, p, p, K);
	Anull3(L, p, p, K);
	Anull(Delta, p, p);


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

		cpyk(WR, p, p, k, WRk);

		anull(Eig2, p);

		EigValDec(p, Eig2, WRk, &det);



		min = INFINITY;
		for(j=0; j<p; j++){
			if(Eig2[j] < min){
				min = Eig2[j];
			}

		}


		if(min < pow(10, -300)){
			break;
		}else{
			Anull(L2, p, p);


			for (j=0; j<p; j++){
				L2[j][j] = 1.0 / Eig2[j];
			
			}
	
			XAXt(WRk, p, L2, invWRk);

			cpyk2(invWRk, p, p, invWR, k);

		}

		det1p[k] = pow(det, 1.0/p);
	
	
	}

	for(k=0; k<K; k++){	
		psi += det1p[k]/n/T;

	}
	for(k=0; k<K; k++){
		detS[k] = pow(psi, p);
	}
	for(k=0; k<K; k++){

		for(j1=0; j1<p; j1++){
			for(j2=0; j2<p; j2++){
				invS[j1][j2][k] = invWR[j1][j2][k] / psi * det1p[k];
			}

		}		
	}


	FREE_MATRIX(Lk);
	FREE_MATRIX(tLk);
	FREE_MATRIX(invDelta);	
	FREE_MATRIX(Delta);	
	FREE_3ARRAY(Omega);
	FREE_3ARRAY(L);
	FREE_MATRIX(L2);
	FREE_3ARRAY(MY);
	FREE_3ARRAY(WR);
	FREE_MATRIX(WRk);
	FREE_3ARRAY(invWR);
	FREE_MATRIX(invWRk);
	FREE_MATRIX(Muk);
	FREE_MATRIX(invPsik);
	FREE_MATRIX(MYi);
	FREE_MATRIX(tMYi);
	FREE_MATRIX(temp1);
	FREE_MATRIX(temp2);
	FREE_MATRIX(temp3);
	FREE_MATRIX(Omegak);
	FREE_VECTOR(Eig2);
	FREE_VECTOR(det1p);

}


void modelB14(int p, int T, int n, int K, double ***Y, double **la, double **nu, double *tau, double ***Mu, double **gamma, double ***invS, double ***invPsi, double *detS, int trans_type){

	int i,k,j1,j2,j;
	double **Muk, **invPsik, **S, ***MY, **MYi, **tMYi, **temp1, **temp2, *Eig2, **L2, **invSk, det = 0.0, min;

	MAKE_3ARRAY(MY, p,T,n);
	MAKE_MATRIX(Muk, p, T);
	MAKE_MATRIX(invPsik, T, T);
	MAKE_MATRIX(S, p, p);
	MAKE_MATRIX(MYi, p, T);
	MAKE_MATRIX(tMYi, T, p);
	MAKE_MATRIX(temp1, p, T);
	MAKE_MATRIX(temp2, p, p);
	MAKE_VECTOR(Eig2, p);
	MAKE_MATRIX(L2, p, p);
	MAKE_MATRIX(invSk, p, p);

	for(k=0; k<K; k++){
		Trans_trans_whole(n, p, T, la[k], nu[k], Y, MY, trans_type);
		cpyk(Mu, p, T, k, Muk);

		cpyk(invPsi, T, T, k, invPsik);

		Anull(S, p, p);


		for(i=0; i<n; i++){

			cpyk(MY, p, T, i, MYi);

			mat_(p, T, MYi, Muk);
	
			tA(MYi, T, p, tMYi);


			multiply(MYi, p, T, invPsik, T, T, temp1);

			multiply(temp1, p, T, tMYi, T, p, temp2);

	
			for(j1=0; j1<p; j1++){			

				for(j2=0; j2<p; j2++){
	
					S[j1][j2] += gamma[i][k] * temp2[j1][j2] / T /tau[k]/n;
				}
			}


		}

		anull(Eig2, p);


		EigValDec(p, Eig2, S, &det);


		min = INFINITY;
		for(j=0; j<p; j++){
			if(Eig2[j] < min){
				min = Eig2[j];
			}

		}


		if(min < pow(10, -300)){
			break;
		}else{
			Anull(L2, p, p);


			for (j=0; j<p; j++){
				L2[j][j] = 1.0 / Eig2[j];
			
			}
	
			XAXt(S, p, L2, invSk);

			detS[k] = det;

			cpyk2(invSk, p, p, invS, k);

		}


	}
	FREE_3ARRAY(MY);
	FREE_MATRIX(Muk);
	FREE_MATRIX(invPsik);
	FREE_MATRIX(S);
	FREE_MATRIX(MYi);
	FREE_MATRIX(tMYi);
	FREE_MATRIX(temp1);
	FREE_MATRIX(temp2);
	FREE_VECTOR(Eig2);
	FREE_MATRIX(L2);
	FREE_MATRIX(invSk);


}

