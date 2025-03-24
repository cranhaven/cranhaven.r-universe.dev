#include <R_ext/Arith.h>
#include <R.h>
#include <math.h>
#include <Rmath.h>
#include <Rinternals.h>
#include <float.h>
#include "simest.h"

void derivcvxpec(int dim[], double t[], double zhat[], double D[], double kk[]){
	int n = dim[0], r = dim[1], f = dim[2], flag = 0;
	for(int j = 0; j < r; j++){
		flag = 0;
		if(kk[j] <= t[0])
		{	//
			kk[j] = f*D[0] + (1 - f)*zhat[0] + (1 - f)*D[0]*(kk[j] - t[0]);
			flag = 1;
		}	//
		if(flag != 1 && kk[j] >= t[n-1])
		{	//
			kk[j] = f*D[n-1] + (1 - f)*zhat[n-1] + (1 - f)*D[n-1]*(kk[j] - t[n-1]);
			flag = 1;
		}	//
		for(int i = 1; i < n; i++){
			if(flag !=1 && kk[j] >= t[i-1] && kk[j] < t[i]){
				kk[j] = f*D[i-1] + (1 - f)*zhat[i-1] + (1 - f)*D[i-1]*(kk[j] - t[i-1]);
				flag = 1;
				break;
			}
		}
	}
}	
