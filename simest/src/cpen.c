#include <R_ext/Arith.h>
#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>
#include <float.h>
#include "simest.h"

void cpen(int dim[], double t_input[], double z_input[], double w_input[], 
	double a0_input[], double lambda_input[], double Ky_input[], 
	double L_input[], double U_input[], double fun_input[],
	double res_input[], int flag[], double tol_input[], 
	double zhat_input[], int iter[], double Deriv_input[])
{
	int n = dim[0], maxit = dim[1], k = 0;
	long double p = lambda_input[0], r=0.0, s=0.0, tp=0.0, tol=tol_input[0];
	long double xmult=0.0, tmp=0.0, tmp1=0.0, tmp2=0.0, ek=0.0, obj=0.0;
	// Initialization to convert inputs of type double to long double.
	long double *t 		= (long double *) R_allocLD(n);
	long double *z 		= (long double *) R_allocLD(n);
	long double *w 		= (long double *) R_allocLD(n);
	long double *fun 	= (long double *) R_allocLD(n);
	long double *res 	= (long double *) R_allocLD(n);
	long double *zhat 	= (long double *) R_allocLD(n);
	long double *Deriv 	= (long double *) R_allocLD(n);
	for(int II = n-1; II >= 0; II--){
		t[II] = t_input[II];
		z[II] = z_input[II];
		w[II] = w_input[II];
		fun[II] = fun_input[II]; 	
		res[II] = res_input[II]; 	
		zhat[II] = zhat_input[II]; 
		Deriv[II] = Deriv_input[II];
	}
	long double *L = (long double *) R_allocLD(n-1);
	long double *U = (long double *) R_allocLD(n-1);
	for(int II = n-2; II >= 0; II--){
		L[II] = L_input[II];
		U[II] = U_input[II];
	}
	long double *a0 = (long double *) R_allocLD(n-2);
	long double *Ky = (long double *) R_allocLD(n-2);
	for(int II = n-3; II >= 0; II--){
		a0[II] = a0_input[II];
		Ky[II] = Ky_input[II];
	}
	// Conversion of double inputs to long double done.
	long double *D0 = (long double *) R_allocLD(n-2);
	long double *UD01 = (long double *) R_allocLD(n-2);
	long double *UD02 = (long double *) R_allocLD(n-2);
	long double *MD = (long double *) R_allocLD(n-2);
	long double *MD1 = (long double *) R_allocLD(n-2);
	long double *MD2 = (long double *) R_allocLD(n-2);
	long double *a1 = (long double *) R_allocLD(n-2);
	long double *deriv = (long double *) R_allocLD(n-2);
	long double *vi2 = (long double *) R_allocLD(maxit);
	// # The following loops compute the 3 upper bands of the matrix
	// # KQ^{-1}K^{T} which is a pentadiagonal matrix.
	// # D0 represents the diagonal vector.
	// # the first n-3 elements of UD01 stores the upper diagonal.
	// # the first n-4 elements of UD02 stores the super upper diagonal.
	for(int i = n-5; i >= 0; i--){
		D0[i] = 1/(w[i]*(t[i+1] - t[i])*(t[i+1] - t[i])*(t[i+2] - t[i])*(t[i+2] - t[i]));
		D0[i] = D0[i] + 1/(w[i+1]*(t[i+2] - t[i+1])*(t[i+2] - t[i+1])*(t[i+1] - t[i])*(t[i+1] - t[i]));
		D0[i] = D0[i] + 1/(w[i+2]*(t[i+2] - t[i])*(t[i+2] - t[i])*(t[i+2] - t[i+1])*(t[i+2] - t[i+1]));
		// ## D0[i] done.
		UD01[i] = 1/(w[i+1]*(t[i+1] - t[i])*(t[i+3] - t[i+1])) + 1/(w[i+2]*(t[i+2] - t[i])*(t[i+3] - t[i+2]));
		UD01[i] = -UD01[i]/((t[i+2] - t[i+1])*(t[i+2] - t[i+1]));
		// ## UD01[i] done.
		UD02[i] = 1/(w[i+2]*(t[i+2] - t[i])*(t[i+2] - t[i+1])*(t[i+3] - t[i+2])*(t[i+4] - t[i+2]));
		// ## UD02[i] done.
	}
	// ## UD01[n-4] done.
	UD02[n-4] = 0.0;
	// UD02[n-4] done.
	UD01[n-1-3] = 1/(w[n-1-2]*(t[n-1-2] - t[n-1-3])*(t[n-1] - t[n-1-2])) + 1/(w[n-1-1]*(t[n-1-1] - t[n-1-3])*(t[n-1] - t[n-1-1]));
	UD01[n-1-3] = -UD01[n-1-3]/((t[n-1-1] - t[n-1-2])*(t[n-1-1] - t[n-1-2]));
	// ## UD01[n-1-3] don-1e.;
	D0[n-1-3] = 1/(w[n-1-3]*(t[n-1-2] - t[n-1-3])*(t[n-1-2] - t[n-1-3])*(t[n-1-1] - t[n-1-3])*(t[n-1-1] - t[n-1-3]));
	D0[n-1-3] = D0[n-1-3] + 1/(w[n-1-2]*(t[n-1-1] - t[n-1-2])*(t[n-1-1] - t[n-1-2])*(t[n-1-2] - t[n-1-3])*(t[n-1-2] - t[n-1-3]));
	D0[n-1-3] = D0[n-1-3] + 1/(w[n-1-1]*(t[n-1-1] - t[n-1-3])*(t[n-1-1] - t[n-1-3])*(t[n-1-1] - t[n-1-2])*(t[n-1-1] - t[n-1-2]));
	// ## D0[n-1-3] don-1e.;
	D0[n-1-2] = 1/(w[n-1-2]*(t[n-1-1] - t[n-1-2])*(t[n-1-1] - t[n-1-2])*(t[n-1] - t[n-1-2])*(t[n-1] - t[n-1-2]));
	D0[n-1-2] = D0[n-1-2] + 1/(w[n-1-1]*(t[n-1] - t[n-1-1])*(t[n-1] - t[n-1-1])*(t[n-1-1] - t[n-1-2])*(t[n-1-1] - t[n-1-2]));
	D0[n-1-2] = D0[n-1-2] + 1/(w[n-1]*(t[n-1] - t[n-1-2])*(t[n-1] - t[n-1-2])*(t[n-1] - t[n-1-1])*(t[n-1] - t[n-1-1]));
	// ## D0[n-4] done.
	UD01[n-3] = 0.0;
	UD02[n-3] = 0.0;
	// ## D0[n-3] done.

	// Iterations for Regularized Newton-Raphson starts.
	while(k < maxit && flag[0] == 1){
		// Computation of intergral of M(t)M^T(t) on the set where alpha^TM(t) > 0
		// starts. This matrix is denoted by M. The vector fun stores alpha^TM(t_i) for 1 <= i <= n so that the
		// first and last elements of this vector are zero.
		fun[0] = 0;
		fun[n-1] = 0;
		for(int i = n-2; i >= 1; i--){
			fun[i] = a0[i-1]/(t[i+1] - t[i-1]);
		}

		// The elements L(i) and U(i) stores the lower and upper endpoints of the
		// intersection of sets {alpha^TM(t) > 0} and [t_i, t_{i+1}] for 1 <= i <= n.
		for(int i = n-2; i >= 0; i--){
			if(fun[i] >= 0){
				L[i] = t[i];
				if(fun[i+1] >= 0){
					U[i] = t[i+1];
				} else{
					U[i] = (fun[i+1]*t[i] - fun[i]*t[i+1])/(fun[i+1] - fun[i]);
				}
			} else{
				U[i] = t[i+1];
				if(fun[i+1] >= 0){
					L[i] = (fun[i+1]*t[i] - fun[i]*t[i+1])/(fun[i+1] - fun[i]);
				} else{
					L[i] = t[i+1];
				}
			}
		}
		// Calculation of L and U ends.

		// MD and MD1 stores the diagonal and upper diagonal elements of the matrix
		// integral of M(t)M^T(t) on {alpha^TM(t) > 0}.
		for(int i = n-4; i >= 0; i--){
			tmp1 = U[i] - t[i]; tmp2 = L[i] - t[i];
			MD[i] = ((tmp1*tmp1*tmp1) - (tmp2*tmp2*tmp2))/(3*(t[i+1] - t[i])*(t[i+1] - t[i]));
			tmp1 = t[i+2] - L[i+1]; tmp2 = t[i+2] - U[i+1];
			MD[i] = MD[i] + ((tmp1*tmp1*tmp1) - (tmp2*tmp2*tmp2))/(3*(t[i+2] - t[i+1])*(t[i+2] - t[i+1]));
			MD[i] = MD[i]/((t[i+2] - t[i])*(t[i+2] - t[i]));
			// ## MD[i] done.
			MD1[i] = 0.5*(t[i+1] + t[i+2])*(U[i+1]*U[i+1] - L[i+1]*L[i+1]);
			MD1[i] = MD1[i] - t[i+1]*t[i+2]*(U[i+1] - L[i+1]);
			MD1[i] = MD1[i] - (U[i+1]*U[i+1]*U[i+1] - L[i+1]*L[i+1]*L[i+1])/3;
			MD1[i] = MD1[i]/((t[i+2] - t[i])*(t[i+3] - t[i+1])*(t[i+2] - t[i+1])*(t[i+2] - t[i+1]));
			// ## MD1[i] done.
			MD2[i] = 0.0;
		}
		tmp1 = U[n-3] - t[n-3]; tmp2 = L[n-3] - t[n-3];
		MD[n-3] = ((tmp1*tmp1*tmp1) - (tmp2*tmp2*tmp2))/(3*(t[n-2] - t[n-3])*(t[n-2] - t[n-3]));
		tmp1 = t[n-1] - L[n-2]; tmp2 = t[n-1] - U[n-2];
		MD[n-3] = MD[n-3] + ((tmp1*tmp1*tmp1) - (tmp2*tmp2*tmp2))/(3*(t[n-1] - t[n-2])*(t[n-1] - t[n-2]));
		MD[n-3] = MD[n-3]/((t[n-1] - t[n-3])*(t[n-1] - t[n-3]));
		// ## MD[n-3] done.
		MD1[n-3] = 0.0;
		MD2[n-3] = 0.0;
		// MD1[n-3] done.

		// ## We now form the matrix M + lambda*KQ^{-1}K^T and store it in MD, MD1, MD2.
		// MD = MD + lambda*D0
		// MD1 = MD1 + lambda*UD01
		// MD2 = MD2 + lambda*UD02
		for(int i = n-3; i >= 0; i--){
			MD[i] = MD[i] + p*D0[i];
			MD1[i] = MD1[i] + p*UD01[i];
			MD2[i] = p*UD02[i];
		}
		// ## Computation of
		// deriv = Ky - (M + lambda*KQ^{-1}K^T)alpha;
		// ek = |(M + lambda*KQ^{-1}K^T)alpha - Ky|;
		// obj = alpha^TKy - alpha^T(M + lambda*KQ^{-1}K^T)alpha/2
		// follows. Note that we want to maximize obj.
		deriv[0] = -MD[0]*a0[0] - MD1[0]*a0[1] - MD2[0]*a0[2] + Ky[0];
		ek = deriv[0]*deriv[0];
		obj = a0[0]*(deriv[0] - Ky[0])/2 + a0[0]*Ky[0];
		deriv[1] = -MD1[0]*a0[0] - MD[1]*a0[1] - MD1[1]*a0[2] - MD2[1]*a0[3] + Ky[1];
		ek = ek + deriv[1]*deriv[1];
		obj = obj + a0[1]*(deriv[1] - Ky[1])/2 + a0[1]*Ky[1];
		for(int i = 2; i <= (n-3); i++){
			deriv[i] = -MD2[i-2]*a0[i-2] - MD1[i-1]*a0[i-1] - MD[i]*a0[i] + Ky[i];
			if(i <= n-5){
				deriv[i] = deriv[i] - MD1[i]*a0[i+1] - MD2[i]*a0[i+2];
			} else if(i==n-4){
				deriv[i] = deriv[i]-MD1[i]*a0[i+1];
			}
			ek = ek + deriv[i]*deriv[i];
			obj = obj + a0[i]*(deriv[i] - Ky[i])/2 + a0[i]*Ky[i];
		}
		// Regularizing the Hessian by adding a multiple of an identity function
		for(int i = n-3; i >= 0; i--){
			MD[i] = MD[i] + sqrtl(ek)/100;
		}
		// Here too MD, MD1, MD2 matches with CvxPenDeriv.R code.
		// This regularization slows down convergence in the beginning since we
		// are intentionally taking smaller step sizes.

		// The following loop solves the equation
		// 	M*a1 = deriv
		// where M is the pentadiagonal matrices with diagonal MD, upper diagonal MD1,
		// and super upper diagonal MD2. Since this is a pentadiagonal matrix, we can use
		// an O(n) algorithm to compute the solution instead of using any standard solver.
		r = MD1[0];
		s = MD1[1];
		tp = MD2[0];
		for(int i = 1; i <= n-4; i++){
			xmult = r/MD[i-1];
			MD[i] = MD[i] - xmult*MD1[i-1];
			MD1[i] = MD1[i] - xmult*MD2[i-1];
			deriv[i] = deriv[i] - xmult*deriv[i-1];
			xmult = tp/MD[i-1];
			r = s - xmult*MD1[i-1];
			MD[i+1] = MD[i+1] - xmult*MD2[i-1];
			deriv[i+1] = deriv[i+1] - xmult*deriv[i-1];
			s = MD1[i+1];
			tp = MD2[i];
		}
		xmult = r/MD[n-4];
		MD[n-3] = MD[n-3] - xmult*MD1[n-4];
		// The following loop computes the solution.
		a1[n-3] =  (deriv[n-3] - xmult*deriv[n-4])/MD[n-3];
		a1[n-4] = (deriv[n-1-3] - MD1[n-1-3]*a1[n-1-2])/MD[n-1-3];
		for(int i = n-5; i >= 0; i--){
			a1[i] = (deriv[i] - MD2[i]*a1[i+2] - MD1[i]*a1[i+1])/MD[i];
		}
		for(int i = n-3; i >= 0; i--){
			a1[i] = a0[i]+a1[i];
		}
		// Solution a1 computed.
		vi2[k] = obj; // The objective function.
		// We see for k > 1, if the absolte change in the objective value is less than tol
		// and also that the norm of the derivative is less than 1e-02.
		// If k reaches the maximum iteration number, then we compute the values for the last a0.
		if((k > 1 && fabsl(vi2[k] - vi2[k-1]) <= tol && sqrtl(ek) <= 1e-02) || k == maxit - 1){
			flag[0] = 0;
			if(k == maxit - 1){
				flag[0] = 1; // If k = maxit - 1, we set flag = 1 to see non-convergence.
				k = k + 1;
			}
			iter[0] = k; // Setting the total number of iterations taken to be k.

			// Calculation of the residual and fit vector starts.
			// res = lambda*Q^{-1}K^Talpha.
			// fit = z - res.
			res[0] = a0[0]/((t[1] - t[0])*(t[2] - t[0]));
			res[0] = p*res[0]/w[0];
			zhat[0] = z[0] - res[0];

			res[1] = a0[1]/((t[2] - t[1])*(t[3] - t[1]));
			res[1] = res[1] - a0[0]/((t[2] - t[1])*(t[1] - t[0]));
			res[1] = p*res[1]/w[1];
			zhat[1] = z[1] - res[1];

			res[n-1] = a0[n-3]/((t[n-1] - t[n-3])*(t[n-1] - t[n-2]));
			res[n-1] = p*res[n-1]/w[n-1];
			zhat[n-1] = z[n-1] - res[n-1];

			res[n-2] = a0[n-4]/((t[n-2] - t[n-4])*(t[n-2] - t[n-3]));
			res[n-2] = res[n-2] - a0[n-3]/((t[n-1] - t[n-2])*(t[n-2] - t[n-3]));
			res[n-2] = p*res[n-2]/w[n-2];
			zhat[n-2] = z[n-2] - res[n-2];

			for(int i = 2; i <= n-3; i++){
				res[i] = a0[i]/((t[i+1] - t[i])*(t[i+2] - t[i]));
				res[i] = res[i] + a0[i-2]/((t[i] - t[i-2])*(t[i] - t[i-1]));
				res[i] = res[i] - a0[i-1]/((t[i+1] - t[i])*(t[i] - t[i-1]));
				res[i] = p*res[i]/w[i];
				zhat[i] = z[i] - res[i];
			}
			//## Calculation of the derivative at each of the x values begins.
			Deriv[0] = (zhat[1] - zhat[0])/(t[1] - t[0]);
			tmp = (t[1] - U[0])*((U[0] - t[0])*(U[0] - t[0]) - (L[0] - t[0])*(L[0] - t[0]));
			tmp = tmp + (U[0] - t[0])*(U[0] - t[0])*(U[0] - t[0])/3;
			tmp = tmp - (L[0] - t[0])*(L[0] - t[0])*(L[0] - t[0])/3;
			tmp = tmp - (L[0] - t[0])*(L[0] - t[0])*(U[0] - L[0]);
			Deriv[0] = Deriv[0] - fun[1]*tmp/(2*(t[1] - t[0])*(t[1] - t[0]));
			for(int i = 0; i <= (n-2); i++){
				Deriv[i+1] = fun[i+1]*((U[i] - t[i])*(U[i] - t[i]) - (L[i] - t[i])*(L[i] - t[i]));
				Deriv[i+1] = Deriv[i+1] - fun[i]*((t[i+1] - U[i])*(t[i+1] - U[i]) - (t[i+1] - L[i])*(t[i+1] - L[i]));
				Deriv[i+1] = Deriv[i] + Deriv[i+1]/(2*(t[i+1] - t[i]));
			}
			//## Calculation of the derivatives end.
		} else{
			// If stop criterion not satisfied then we set a0 = a1.
			for(int i = n-3; i >= 0; i--){
				a0[i] = a1[i];
			}
			k = k+1; // and of course increase the iteration number.
		}
	}
	for(int II = n-1; II >= 0; II--){
		t_input[II] = (double) t[II];
		z_input[II] = (double) z[II];
		w_input[II] = (double) w[II];
		fun_input[II] = (double) fun[II];	
		res_input[II] = (double) res[II];
		zhat_input[II] = (double) zhat[II]; 
		Deriv_input[II] = (double) Deriv[II];
	}
	for(int II = n-2; II >= 0; II--){
		L_input[II] = (double) L[II];
		U_input[II] = (double) U[II];
	}
	for(int II = n-3; II >= 0; II--){
		a0_input[II] = (double) a0[II];
		Ky_input[II] = (double) Ky[II];
	}	
}
