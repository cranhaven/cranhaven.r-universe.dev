#include <R_ext/Arith.h>
#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>
#include <float.h>
#include "simest.h"

void spen_egcv(int dim[], double x[], double y[], double w[], double h[],
		double QtyPerm[], double lambda[], double m[],
		int nforApp[], int EGCVflag[], double agcv[]){
	long double a = lambda[0], r, s, t, xmult;
	int n = dim[0];
	int num = nforApp[0];
	int EGCV = EGCVflag[0];
	// printf("EGCV = %d\n", EGCVflag[0]);
	long double etotsum = 0.0;
	long double *OD =(long double *) R_allocLD(n-2);
	long double *OUD1 =(long double *) R_allocLD(n-2);
	long double *OUD2 =(long double *) R_allocLD(n-2);
	long double *D =(long double *) R_allocLD(n-2);
	long double *UD1 =(long double *) R_allocLD(n-2);
	long double *UD2 =(long double *) R_allocLD(n-2);
	long double *QTY =(long double *) R_allocLD(n-2);
	long double *C = (long double *) R_allocLD(n-2);
	long double *e =(long double *) R_allocLD(n);
	long double *Ce =(long double *) R_allocLD(n-2);
	long double *QTe =(long double *) R_allocLD(n-2);
	long double *ehat =(long double *) R_allocLD(n);
	// Calculation of the Diagonal and 2 upper diagonals of
	// R + lambda* Q^{T}W^{-1}Q. See sspline.pdf
	for(int i = 0; i < n-4; i++){
		D[i] = 2/(w[i+1]*h[i]*h[i+1]) + 1/(w[i]*h[i]*h[i]) + 1/(w[i+1]*h[i]*h[i]) +
		  1/(w[i+1]*h[i+1]*h[i+1]) + 1/(w[i+2]*h[i+1]*h[i+1]);
		D[i] = a*D[i] + (h[i] + h[i+1])/3;
		UD1[i] = -1/(w[i+1]*h[i]*h[i+1]) - 1/(w[i+1]*h[i+1]*h[i+1]) -
		  1/(w[i+2]*h[i+1]*h[i+1]) - 1/(w[i+2]*h[i+1]*h[i+2]);
		UD1[i] = a*UD1[i] + h[i+1]/6;
		UD2[i] = a/(w[i+2]*h[i+1]*h[i+2]);
	}
	D[n-4] = 2/(w[n-3]*h[n-4]*h[n-3]) + 1/(w[n-4]*h[n-4]*h[n-4]) + 1/(w[n-3]*h[n-4]*h[n-4]) + 1/(w[n-3]*h[n-3]*h[n-3]) + 1/(w[n-2]*h[n-3]*h[n-3]);
	D[n-4] = a*D[n-4] + (h[n-4] + h[n-3])/3;
	D[n-3] = 2/(w[n-2]*h[n-3]*h[n-2]) + 1/(w[n-3]*h[n-3]*h[n-3]) + 1/(w[n-2]*h[n-3]*h[n-3]) +
		1/(w[n-2]*h[n-2]*h[n-2]) + 1/(w[n-1]*h[n-2]*h[n-2]);
	D[n-3] = a*D[n-3] + (h[n-3] + h[n-2])/3;
	UD1[n-4] = -1/(w[n-3]*h[n-4]*h[n-3]) - 1/(w[n-3]*h[n-3]*h[n-3]) -
		1/(w[n-2]*h[n-3]*h[n-3]) - 1/(w[n-2]*h[n-3]*h[n-2]);
	UD1[n-4] = a*UD1[n-4] + h[n-3]/6;
	// End of calculation of the diagonal entries.
	for (int i = 0; i < n-2; i++){
		OD[i]=D[i];
		OUD1[i]=UD1[i];
		OUD2[i]=UD2[i];
		QTY[i] = QtyPerm[i];
	}
	// D = D, A = C = UD1, E = F = UD2
	r = UD1[0];
	s = UD1[1];
	t = UD2[0];
	for(int i = 1; i < n-3; i++){
		xmult = r/D[i-1];
		D[i] = D[i] - xmult*UD1[i-1];
		UD1[i] = UD1[i] - xmult*UD2[i-1];
		QTY[i] = QTY[i] - xmult*QTY[i-1];
		xmult = t/D[i-1];
		r = s - xmult*UD1[i-1];
		D[i+1] = D[i+1] - xmult*UD2[i-1];
		QTY[i+1] = QTY[i+1] - xmult*QTY[i-1];
		s = UD1[i+1];
		t = UD2[i];
	}
	xmult = r/D[n-4];
	D[n-3] = D[n-3] - xmult*UD1[n-4];
	C[n-3] = (QTY[n-3] - xmult*QTY[n-4])/D[n-3];
	lambda[1] = a*C[n-3]*QtyPerm[n-3];
	C[n-4] = (QTY[n-4] - UD1[n-4]*C[n-3])/D[n-4];
	lambda[1] = lambda[1] + a*C[n-4]*QtyPerm[n-4];
	for(int i = n-5; i >= 0; i--){
		C[i] = (QTY[i] - UD2[i]*C[i+2] - UD1[i]*C[i+1])/D[i];
		lambda[1] = lambda[1] + a*C[i]*QtyPerm[i];
	}
	m[0] = C[0]/(w[0]*h[0]);
	m[0] = y[0] - a*m[0];
	m[1] = (C[1] - C[0])/(w[1]*h[1]) - C[0]/(w[1]*h[0]);
	m[1] = y[1] - a*m[1];
	for(int i = 2; i < n-2; i++){
		m[i] = (C[i] - C[i-1])/(w[i]*h[i]) - (C[i-1] - C[i-2])/(w[i]*h[i-1]);
		m[i] = y[i] - a*m[i];
	}
	m[n-2] = -C[n-3]/(w[n-2]*h[n-2]) - (C[n-3] - C[n-4])/(w[n-2]*h[n-3]);
	m[n-2] = y[n-2] - a*m[n-2];
	m[n-1] = C[n-3]/(w[n-1]*h[n-2]);
	m[n-1] = y[n-1] - a*m[n-1];

	//  EGCV
	// printf("EGCV = %d\n", EGCV);
	if(EGCV == 1) {
		for(int j = 1; j <= num; j++){
			r = OUD1[0];
			s = OUD1[1];
			t = OUD2[0];
			for (int i = 0; i < n-2; i++){
				D[i]=OD[i];
				UD1[i]=OUD1[i];
				UD2[i]=OUD2[i];
			}
			GetRNGstate();
			e[0] = 2*rbinom(1,0.5) - 1;
			e[1] = 2*rbinom(1,0.5) - 1;
			for (int i = 2; i < n; i++){
				e[i] = 2*rbinom(1,0.5) - 1;
				// printf("e[%d] = %Lf\n", i, e[i]);
				QTe[i-2] = ( e[i]/sqrt(w[i]) -e[i-1]/sqrt(w[i-1]) )/ h[i-1];
				QTe[i-2]= QTe[i-2]-( e[i-1]/sqrt(w[i-1]) -e[i-2]/sqrt(w[i-2]) )/ h[i-2];
			}
			PutRNGstate();
			for(int i = 1; i < n-3; i++){
				xmult = r/D[i-1];
				D[i] = D[i] - xmult*UD1[i-1];
				UD1[i] = UD1[i] - xmult*UD2[i-1];
				QTe[i] = QTe[i] - xmult*QTe[i-1];
				xmult = t/D[i-1];
				r = s - xmult*UD1[i-1];
				D[i+1] = D[i+1] - xmult*UD2[i-1];
				QTe[i+1] = QTe[i+1] - xmult*QTe[i-1];
				s = UD1[i+1];
				t = UD2[i];
			}
			xmult = r/D[n-4];
			D[n-3] = D[n-3] - xmult*UD1[n-4];
			Ce[n-3] = (QTe[n-3] - xmult*QTe[n-4])/D[n-3];
			Ce[n-4] = (QTe[n-4] - UD1[n-4]*Ce[n-3])/D[n-4];
			for(int i = n-5; i >= 0; i--){
				Ce[i] = (QTe[i] - UD2[i]*Ce[i+2] - UD1[i]*Ce[i+1])/D[i];
			}
			ehat[0] = Ce[0]/(w[0]*h[0]);
			etotsum = etotsum + e[0]*sqrt(w[0])*a*ehat[0]/n;
			ehat[1] = (Ce[1] - Ce[0])/(w[1]*h[1]) - Ce[0]/(w[1]*h[0]);
			etotsum = etotsum + e[1]*sqrt(w[1])*a*ehat[1]/n;
			for(int i = 2; i < n-2; i++){
				ehat[i] = (Ce[i] - Ce[i-1])/(w[i]*h[i]) - (Ce[i-1] - Ce[i-2])/(w[i]*h[i-1]);
				etotsum = etotsum + e[i]*sqrt(w[i])*a*ehat[i]/n;
			}
			ehat[n-2] = -Ce[n-3]/(w[n-2]*h[n-2]) - (Ce[n-3] - Ce[n-4])/(w[n-2]*h[n-3]);
			etotsum = etotsum + e[n-2]*sqrt(w[n-2])*a*ehat[n-2]/n;
			ehat[n-1] = Ce[n-3]/(w[n-1]*h[n-2]);
			etotsum = etotsum + e[n-1]*sqrt(w[n-1])*a*ehat[n-1]/n;
		}
		etotsum = etotsum/num;

		for(int i = n-1; i >=0 ; i--){
			agcv[0] = agcv[0] +  w[i]*(y[i]-m[i])*(y[i]-m[i])/n;
		}
		agcv[0] = agcv[0]/(etotsum*etotsum);
	}

}
