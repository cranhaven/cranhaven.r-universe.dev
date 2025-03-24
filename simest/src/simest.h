#include <R.h>
#include <Rinternals.h>

void cpen(int dim[], double t_input[], double z_input[], double w_input[], 
	double a0_input[], double lambda_input[], double Ky_input[], 
	double L_input[], double U_input[], double fun_input[],
	double res_input[], int flag[], double tol_input[], 
	double zhat_input[], int iter[], double Deriv_input[]);

void derivcvxpec(int dim[], double t[], double zhat[], double D[], double kk[]);

void penta(int dim[], double E[], double A[], double D[], 
            double C[], double F[], double B[], double X[]);

void predcvxpen(int dim[], double x[], double t[], double zhat[], 
			double deriv[], double L[], double U[], double fun[], 
			double P[], double Q[], double R[]);

void spen_egcv(int dim[], double x[], double y[], double w[], double h[],
		double QtyPerm[], double lambda[], double m[],
		int nforApp[], int EGCVflag[], double agcv[]);
