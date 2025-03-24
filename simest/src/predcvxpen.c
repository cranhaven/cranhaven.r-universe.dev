#include <R_ext/Arith.h>
#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>
#include <float.h>
#include "simest.h"

void predcvxpen(int dim[], double x[], double t[], double zhat[], 
			double deriv[], double L[], double U[], double fun[], 
			double P[], double Q[], double R[]){
	int k = dim[0], n = dim[1];
	double tmp;
	for(int j = (k-1); j >= 0; j--){
		if(x[j] < t[0]){
			R[j] = 0;
			// ## R[j] done.
			P[j] = deriv[0];
			// ## P[j] done.
			Q[j] = zhat[0] - deriv[0]*(t[0] - x[j]);
			// ## Q[j] done.
		}
		if(x[j] > t[n-1]){
			R[j] = 0;
			// ## R[j] done.
			P[j] = deriv[n-1];
			// ## P[j] done.
			Q[j] = zhat[n-1] + deriv[n-1]*(x[j] - t[n-1]);
			// ## Q[j] done.
		}
		for(int i = 0; i <= (n-2); i++){
			if(x[j] >= t[i] && x[j] <= L[i]){
				R[j] = 0;
				// ## R[j] done.
				P[j] = deriv[i];
				// ## P[j] done.
				Q[j] = zhat[i] + deriv[i]*(x[j] - t[i]);
				// ## Q[j] done.
				// # break
			}
			if(x[j] > L[i] && x[j] <= U[i]){
				R[j] = fun[i] + (x[j] - t[i])*(fun[i+1] - fun[i])/(t[i+1] - t[i]);
				// ## R[j] done.
				P[j] = fun[i+1]*((x[j] - t[i])*(x[j] - t[i]) - (L[i] - t[i])*(L[i] - t[i]));
				P[j] = P[j] - fun[i]*((t[i+1] - x[j])*(t[i+1] - x[j]) - (t[i+1] - L[i])*(t[i+1] - L[i]));
				P[j] = deriv[i] + P[j]/(2*(t[i+1] - t[i]));
				// ## P[j] done.
				Q[j] = zhat[i] + deriv[i]*(x[j] - t[i]);
				tmp = fun[i+1]/(2*(t[i+1] - t[i]));
				Q[j] = Q[j] + tmp*(x[j] - t[i])*(x[j] - t[i])*(x[j] - t[i])/3;
				Q[j] = Q[j] - tmp*(L[i] - t[i])*(L[i] - t[i])*(L[i] - t[i])/3;
				Q[j] = Q[j] - tmp*(L[i] - t[i])*(L[i] - t[i])*(x[j] - t[i]);
				tmp = fun[i]/(2*(t[i+1] - t[i]));
				Q[j] = Q[j] + tmp*(t[i+1] - L[i])*(t[i+1] - L[i])*(x[j] - t[i]);
				Q[j] = Q[j] + tmp*(t[i+1] - x[j])*(t[i+1] - x[j])*(t[i+1] - x[j])/3;
				Q[j] = Q[j] - tmp*(t[i+1] - L[i])*(t[i+1] - L[i])*(t[i+1] - L[i])/3;
				// ## Q[j] done.
				// # break
			}
			if(x[j] > U[i] && x[j] <= t[i+1]){
				R[j] = 0;
				// ## R[j] done.
				P[j] = fun[i+1]*((U[i] - t[i])*(U[i] - t[i]) - (L[i] - t[i])*(L[i] - t[i]));
				P[j] = P[j] - fun[i]*((t[i+1] - U[i])*(t[i+1] - U[i]) - (t[i+1] - L[i])*(t[i+1] - L[i]));
				P[j] = deriv[i] + P[j]/(2*(t[i+1] - t[i]));
				// ## P[j] done.
				Q[j] = zhat[i] + deriv[i]*(x[j] - t[i]);
				tmp = fun[i+1]/(2*(t[i+1] - t[i]));
				Q[j] = tmp*((U[i] - t[i])*(U[i] - t[i]) - (L[i] - t[i])*(L[i] - t[i]))*(x[j] - U[i]);
				Q[j] = Q[j] + tmp*(U[i] - t[i])*(U[i] - t[i])*(U[i] - t[i])/3;
				Q[j] = Q[j] - tmp*(L[i] - t[i])*(L[i] - t[i])*(L[i] - t[i])/3;
				Q[j] = Q[j] - tmp*(L[i] - t[i])*(L[i] - t[i])*(U[i] - L[i]);
				tmp = fun[i]/(2*(t[i+1] - t[i]));
				Q[j] = Q[j] - tmp*((t[i+1] - U[i])*(t[i+1] - U[i]) - (t[i+1] - L[i])*(t[i+1] - L[i]))*(x[j] - U[i]);
				Q[j] = Q[j] + tmp*(t[i+1] - U[i])*(t[i+1] - U[i])*(t[i+1] - U[i])/3;
				Q[j] = Q[j] - tmp*(t[i+1] - L[i])*(t[i+1] - L[i])*(t[i+1] - L[i])/3;
				Q[j] = Q[j] + tmp*(t[i+1] - L[i])*(t[i+1] - L[i])*(U[i] - L[i]);
				// ## Q[j] done.
				// # break
			}
		}
	}
}
