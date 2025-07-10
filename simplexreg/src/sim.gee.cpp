#include<math.h>
#include<R.h>
#include<Rinternals.h>
#include<R_ext/Rdynload.h>
#include<gsl/gsl_vector.h>
#include<gsl/gsl_matrix.h>
#include<gsl/gsl_blas.h>
#include<gsl/gsl_linalg.h>
#include<gsl/gsl_rng.h>
#include<gsl/gsl_cdf.h>
#include<gsl/gsl_randist.h>
#include <gsl/gsl_errno.h>

extern "C"{
double gf1(double x){
	return log(x) - log(1-x);
}

double gf2(double x){
	return gsl_cdf_ugaussian_Pinv(x);
}

double gf3(double x){
	return log(-log(1-x));
}

double gf4(double x){
	return -log(-log(x));
}

double gfd1(double x){
	return 1.0 / (x * (1 - x));
}

double gfd2(double x){
	return 1.0 / gsl_ran_gaussian_pdf(gsl_cdf_ugaussian_Pinv(x), 1.0);
}

double gfd3(double x){
	return -1.0 / ((1 - x) * log(1 - x));
}

double gfd4(double x){
	return -1.0 / (x * log(x));
}

double hf1(double x){
	return exp(x) / (1.0 + exp(x));
}

double hf2(double x){
	return gsl_cdf_ugaussian_P(x);
}

double hf3(double x){
	return 1.0 - exp(-exp(x));
}

double hf4(double x){
	return exp(-exp(-x));
}

double gf(double x, int link){
	if (link == 1)
		return gf1(x);
	else if (link == 2)
		return gf2(x);
	else if (link == 3)
		return gf3(x);
	else
		return gf4(x);
}

double gfd(double x, int link){
	if (link == 1)
		return gfd1(x);
	else if (link == 2)
		return gfd2(x);
	else if (link == 3)
		return gfd3(x);
	else
		return gfd4(x);
}

double hf(double x, int link){
	if (link == 1)
		return hf1(x);
	else if (link == 2)
		return hf2(x);
	else if (link == 3)
		return hf3(x);
	else 
		return hf4(x);
}

double dd(double y, double mu){
	return pow(y - mu, 2) / (y * (1 - y) * pow(mu * (1 - mu), 2));
}

double Vfun(double mu){
	return pow(mu * (1 - mu), 3);
}

double uu(double y, double mu){
	return (y - mu) * (dd(y, mu) / (mu * (1 - mu)) + 1 / Vfun(mu));
}

double var_uu(double mu, double sigma){
	return 3 * pow(sigma, 2) / (mu * (1 - mu)) + sigma / Vfun(mu);
}

double ABS(double x){
	if (x >= 0)
		return x;
	else
		return (-x);
}

// Calculate Sensitivity Matrix
void CalSenMat(gsl_matrix *SenMat, gsl_vector *Psi, double *X, double *Z, double *mu, double *sigma, double *T, double alpha, int a, int b, int p, int link, int type)
{
	// calculate w
	gsl_vector *gsl_w = gsl_vector_alloc(b-a+1);
	double *gsl_w_ptr = gsl_vector_ptr(gsl_w, 0);
	for (int i = 0;i < (b-a+1);i++)
		gsl_w_ptr[i] = 1.0 / gfd(mu[i+a-1], link);

	// calculate matrix D
	gsl_matrix *gsl_D = gsl_matrix_alloc(b-a+1, p);
	double *gsl_D_ptr = gsl_matrix_ptr(gsl_D, 0, 0);
	for (int i = 0;i < (b-a+1);i++)
		for (int j = 0;j < p;j++)
			gsl_D_ptr[i*p+j] = X[(i+a-1)*p+j] * gsl_w_ptr[i];

	// calculate ww
	gsl_vector *gsl_ww = gsl_vector_alloc(b-a+1);
	double *gsl_ww_ptr = gsl_vector_ptr(gsl_ww, 0);
	for (int i = 0;i < (b-a+1);i++)
		gsl_ww_ptr[i] = 3 * sigma[i+a-1] * pow(mu[i+a-1] * (1 - mu[i+a-1]), 2) + 1.0;

	// calculate www
	gsl_vector *gsl_www = gsl_vector_alloc(b-a+1);
	double *gsl_www_ptr = gsl_vector_ptr(gsl_www, 0);
	for (int i = 0;i < (b-a+1);i++)
		gsl_www_ptr[i] = sqrt(sigma[i+a-1] * Vfun(mu[i+a-1]) * gsl_ww_ptr[i]);

	// calculate matrix R
	gsl_matrix *gsl_R = gsl_matrix_alloc(b-a+1, b-a+1);
	gsl_matrix_set_identity(gsl_R);
	double *gsl_R_ptr = gsl_matrix_ptr(gsl_R, 0, 0);
	if (type == 0){
		for (int i = 0;i < (b-a+1);i++)
			for (int j = (i+1);j < (b-a+1);j++)
				gsl_R_ptr[i*(b-a+1)+j] = exp(alpha * ABS(T[a+j-1] - T[a+i-1]));
	}
	else{
		for (int i = 0;i < (b-a+1);i++)
			for (int j = (i+1);j < (b-a+1);j++)
				gsl_R_ptr[i*(b-a+1)+j] = exp(alpha);
	}
	gsl_matrix *gsl_tempM = gsl_matrix_alloc(b-a+1, b-a+1);
	gsl_matrix_transpose_memcpy(gsl_tempM, gsl_R);
	gsl_matrix_add(gsl_R, gsl_tempM);
	for (int i = 0;i < (b-a+1);i++)
		gsl_R_ptr[i*(b-a+1)+i] = 1.0;
	gsl_vector_view gsl_R_row = gsl_matrix_row(gsl_R, 0);
	for (int i = 0;i < (b-a+1);i++){
		gsl_R_row = gsl_matrix_row(gsl_R, i);
		gsl_vector_scale(&gsl_R_row.vector, gsl_www_ptr[i]);
	}
	for (int i = 0;i < (b-a+1);i++){
		gsl_R_row = gsl_matrix_column(gsl_R, i);
		gsl_vector_scale(&gsl_R_row.vector, gsl_www_ptr[i]);
	}

	// S -= t(D) * A * (VV)^-1 * A * D, Psi += t(D) * A * (VV)^-1 * z[a:b]
	// tempM1 = (VV)^-1 * A * D
	gsl_vector_view gsl_D_row = gsl_matrix_row(gsl_D, 0);
	for (int i = 0;i < (b-a+1);i++){
		gsl_D_row = gsl_matrix_row(gsl_D, i);
		gsl_vector_scale(&gsl_D_row.vector, gsl_ww_ptr[i]);
	}
	gsl_matrix *tempM1 = gsl_matrix_alloc(b-a+1, p);
	gsl_set_error_handler_off();
	int status = gsl_linalg_cholesky_decomp(gsl_R);
	if (status)
		::Rf_error( "Correlation matrix is not positive-definite");
	gsl_matrix_memcpy(tempM1, gsl_D);
	gsl_blas_dtrsm(CblasLeft, CblasLower, CblasNoTrans, CblasNonUnit, 1.0, gsl_R, tempM1);
	gsl_blas_dtrsm(CblasLeft, CblasUpper, CblasNoTrans, CblasNonUnit, 1.0, gsl_R, tempM1);
	gsl_matrix *tempM2 = gsl_matrix_alloc(p, p);
	gsl_blas_dgemm(CblasTrans, CblasNoTrans, 1.0, gsl_D, tempM1, 0.0, tempM2);
	gsl_matrix_sub(SenMat, tempM2);
	gsl_vector *temp3 = gsl_vector_alloc(b-a+1);
	double *temp3_ptr = gsl_vector_ptr(temp3, 0);
	for (int i = 0;i < (b-a+1);i++)
		temp3_ptr[i] = Z[i+a-1];
	gsl_vector *temp4 = gsl_vector_alloc(p);
	gsl_blas_dgemv(CblasTrans, 1.0, tempM1, temp3, 0.0, temp4);
	gsl_vector_add(Psi, temp4);

	// free the allocation
	gsl_vector_free(gsl_w);
	gsl_vector_free(gsl_ww);
	gsl_vector_free(gsl_www);
	gsl_matrix_free(gsl_D);
	gsl_matrix_free(gsl_R);
	gsl_matrix_free(gsl_tempM);
	gsl_matrix_free(tempM1);
	gsl_matrix_free(tempM2);
	gsl_vector_free(temp3);
	gsl_vector_free(temp4);
}

// Calculate Sensitivity Matrix
void CalSenMat1(double *S3, double *Phi, double *y, double *mu, double *sigma, double *r, double *T, double alpha, int a, int b)
{
	int n = b - a + 1;
	gsl_vector *gsl_R = gsl_vector_alloc(n*(n-1)/2);
	double *gsl_R_ptr = gsl_vector_ptr(gsl_R, 0);
	gsl_vector *gsl_dR = gsl_vector_alloc(n*(n-1)/2);
	double *gsl_dR_ptr = gsl_vector_ptr(gsl_dR, 0);
	gsl_vector *gsl_Eta = gsl_vector_alloc(n*(n-1)/2);
	double *gsl_Eta_ptr = gsl_vector_ptr(gsl_Eta, 0);
	int s = 0;
	for (int i = 0;i < n-1;i++){
		for (int j = i+1; j < n;j++){
			gsl_R_ptr[s+j-i-1] = r[a+i-1] * r[a+j-1]; //* sqrt(var_uu(mu[a+i-1], sigma[a+i-1])) * sqrt(var_uu(mu[a+j-1], sigma[a+j-1]));
			gsl_Eta_ptr[s+j-i-1] = exp(alpha * ABS(T[a+i-1] - T[a+j-1])); //* sqrt(var_uu(mu[a+i-1], sigma[a+i-1])) * sqrt(var_uu(mu[a+j-1], sigma[a+j-1]));
			gsl_dR_ptr[s+j-i-1] = ABS(T[a+i-1] - T[a+j-1]) * gsl_Eta_ptr[s+j-i-1];
		}
		s += n-i-1;
	}
	*S3 -= pow(gsl_blas_dnrm2(gsl_dR), 2);
	double result[1];
	gsl_vector_sub(gsl_R, gsl_Eta);
	gsl_blas_ddot(gsl_R, gsl_dR, result);
	*Phi += *result;

	gsl_vector_free(gsl_R);
	gsl_vector_free(gsl_dR);
	gsl_vector_free(gsl_Eta);
}

// Calculate Sensitivity Matrix
void CalSenMat2(double *S3, double *Phi, double *y, double *mu, double *sigma, double *r, double alpha, int a, int b)
{
	int n = b - a + 1;
//	gsl_vector *gsl_R = gsl_vector_alloc(n*(n-1)/2);
//	double *gsl_R_ptr = gsl_vector_ptr(gsl_R, 0);
//	gsl_vector *gsl_dR = gsl_vector_alloc(n*(n-1)/2);
//	double *gsl_dR_ptr = gsl_vector_ptr(gsl_dR, 0);
//	gsl_vector *gsl_Eta = gsl_vector_alloc(n*(n-1)/2);
//	double *gsl_Eta_ptr = gsl_vector_ptr(gsl_Eta, 0);
//	int s = 0;
	for (int i = 0;i < n-1;i++){
		for (int j = i+1; j < n;j++){
			*S3 += r[a+i-1] * r[a+j-1]; 
			*Phi += 1.0;
//* sqrt(var_uu(mu[a+i-1], sigma[a+i-1])) * sqrt(var_uu(mu[a+j-1], sigma[a+j-1]));
//			gsl_Eta_ptr[s+j-i-1] = exp(alpha); //* sqrt(var_uu(mu[a+i-1], sigma[a+i-1])) * sqrt(var_uu(mu[a+j-1], sigma[a+j-1]));
//			gsl_dR_ptr[s+j-i-1] = exp(alpha);
		}
//		s += n-i-1;
	}
//	*S3 -= pow(gsl_blas_dnrm2(gsl_dR), 2);
//	double result[1];
//	gsl_vector_sub(gsl_R, gsl_Eta);
//	gsl_blas_ddot(gsl_R, gsl_dR, result);
//	*Phi += *result;

//	gsl_vector_free(gsl_R);
//	gsl_vector_free(gsl_dR);
//	gsl_vector_free(gsl_Eta);
}

// one time update of the parameter
void Update(double *beta, double *Sigma, double *alpha, double *mu, double *sigma, double *y, double *X, double *Z, 
			double *T, int *ID, int *N, int *m, int *p, int *q, int *link, int *type)
{
	try {
		int a, b;
		gsl_vector *gsl_uu = gsl_vector_alloc(*N);
		double *gsl_uu_ptr = gsl_vector_ptr(gsl_uu, 0);

		// update beta
		gsl_matrix_view gsl_X = gsl_matrix_view_array(X, *N, *p);
		gsl_vector_view gsl_beta = gsl_vector_view_array(beta, *p);

		for (int i = 0;i < *N;i++)
			gsl_uu_ptr[i] = uu(y[i], mu[i]) * Vfun(mu[i]);

		gsl_matrix *SenMat = gsl_matrix_calloc(*p, *p);
		gsl_vector *Psi = gsl_vector_calloc(*p);
		for (int i = 0;i < *m;i++){
			if (i == 0)
				a = 1;
			else
				a = ID[i-1]+1;
			b = ID[i];
			CalSenMat(SenMat, Psi, X, gsl_uu_ptr, mu, sigma, T, *alpha, a, b, *p, *link, *type);
		}

		gsl_matrix_scale(SenMat, -1.0);
		int status = gsl_linalg_cholesky_decomp(SenMat);
		gsl_set_error_handler_off();
		if (status)
			::Rf_error( "Sensitivity matrix is not positive-definete" );
		gsl_linalg_cholesky_svx(SenMat, Psi);
		gsl_vector_add(&gsl_beta.vector, Psi);

		// update Sigma
		gsl_matrix_view gsl_Z = gsl_matrix_view_array(Z, *N, *q);
		gsl_matrix *Chole = gsl_matrix_alloc(*q, *q);
		gsl_vector *temp = gsl_vector_alloc(*N);
		double *temp_ptr = gsl_vector_ptr(temp, 0);
		gsl_vector_view gsl_sigma = gsl_vector_view_array(sigma, *N);
		gsl_vector_view gsl_Sigma = gsl_vector_view_array(Sigma, *q);
		for (int i = 0;i < *N;i++)
			temp_ptr[i] = dd(y[i], mu[i]);
		gsl_vector_sub(temp, &gsl_sigma.vector);
		gsl_vector *temp2 = gsl_vector_alloc(*q);
		gsl_blas_dsyrk(CblasUpper, CblasTrans, 1.0, &gsl_Z.matrix, 0.0, Chole);
		double *Chole_ptr = gsl_matrix_ptr(Chole, 0, 0);
		for (int i = 0;i < *q;i++)
			for (int j = (i+1);j < *q;j++)
				Chole_ptr[j*(*q)+i] = Chole_ptr[i*(*q)+j];
		for (int i = 0;i < *N;i++)
			temp_ptr[i] /= sigma[i];
		gsl_blas_dgemv(CblasTrans, 1.0, &gsl_Z.matrix, temp, 0.0, temp2);
	//	}
		gsl_linalg_cholesky_decomp(Chole);
		gsl_linalg_cholesky_svx(Chole, temp2);
		gsl_vector_add(&gsl_Sigma.vector, temp2);

		// update alpha
		for (int i = 0;i < *N;i++)
			gsl_uu_ptr[i] = uu(y[i], mu[i]) / sqrt(var_uu(mu[i], sigma[i]));
		double S3[1] = {0}, Phi[1] = {0};
		for (int i = 0;i < *m;i++){
			if (i == 0)
				a = 1;
			else
				a = ID[i-1]+1;
			b = ID[i];
			if (*type == 0)
				CalSenMat1(S3, Phi, y, mu, sigma, gsl_uu_ptr, T, *alpha, a, b);
			else
				CalSenMat2(S3, Phi, y, mu, sigma, gsl_uu_ptr, *alpha, a, b);
		}
		*alpha = *alpha - *Phi / *S3;
		if (*alpha < -1000)
			*alpha = -1000;
		if (*alpha >= 0)
			*alpha = -1000;

		// update mu
		gsl_vector_view gsl_mu = gsl_vector_view_array(mu, *N);
		gsl_blas_dgemv(CblasNoTrans, 1.0, &gsl_X.matrix, &gsl_beta.vector, 0.0, &gsl_mu.vector);
		for (int i = 0;i < *N;i++)
			mu[i] = hf(mu[i], *link);

		// update sigma
		gsl_blas_dgemv(CblasNoTrans, 1.0, &gsl_Z.matrix, &gsl_Sigma.vector, 0.0, &gsl_sigma.vector);
		for (int i = 0;i < *N;i++)
			sigma[i] = exp(sigma[i]);

		// free the allocation
		gsl_matrix_free(SenMat);
		gsl_vector_free(Psi);
		gsl_vector_free(gsl_uu);
		gsl_vector_free(temp);
		gsl_vector_free(temp2);
		gsl_matrix_free(Chole);
	}
	catch (...) {
		::Rf_error( "c++ exception (unknown reason)" );
	}
}

// one time update of the parameter
void Update_homo(double *beta, double *alpha, double *mu, double *sigma, double *y, double *X, 
			double *T, int *ID, int *N, int *m, int *p, int *link, int *type)
{
	try	{
		int a, b, status;
		gsl_vector *gsl_uu = gsl_vector_alloc(*N);
		double *gsl_uu_ptr = gsl_vector_ptr(gsl_uu, 0);

		// update beta
		gsl_matrix_view gsl_X = gsl_matrix_view_array(X, *N, *p);
		gsl_vector_view gsl_beta = gsl_vector_view_array(beta, *p);

		for (int i = 0;i < *N;i++)
			gsl_uu_ptr[i] = uu(y[i], mu[i]) * Vfun(mu[i]);

		gsl_matrix *SenMat = gsl_matrix_calloc(*p, *p);
		gsl_vector *Psi = gsl_vector_calloc(*p);
		for (int i = 0;i < *m;i++){
			if (i == 0)
				a = 1;
			else
				a = ID[i-1]+1;
			b = ID[i];
			CalSenMat(SenMat, Psi, X, gsl_uu_ptr, mu, sigma, T, *alpha, a, b, *p, *link, *type);
		}

		gsl_matrix_scale(SenMat, -1.0);
		status = gsl_linalg_cholesky_decomp(SenMat);
		gsl_set_error_handler_off();
		if (status)
			::Rf_error( "Sensitivity matrix is not positive-definite" );
		gsl_linalg_cholesky_svx(SenMat, Psi);
		gsl_vector_add(&gsl_beta.vector, Psi);

		// update alpha
		for (int i = 0;i < *N;i++)
			gsl_uu_ptr[i] = uu(y[i], mu[i]) / sqrt(var_uu(mu[i], sigma[i]));
		double S3[1] = {0}, Phi[1] = {0};
		for (int i = 0;i < *m;i++){
			if (i == 0)
				a = 1;
			else
				a = ID[i-1]+1;
			b = ID[i];
			if (*type == 0)
				CalSenMat1(S3, Phi, y, mu, sigma, gsl_uu_ptr, T, *alpha, a, b);
			else
				CalSenMat2(S3, Phi, y, mu, sigma, gsl_uu_ptr, *alpha, a, b);
		}
		if (*type == 0){
			*alpha = *alpha - *Phi / *S3;
			if (*alpha < -1000)
				*alpha = -1000;
			if (*alpha >= 0)
				*alpha = -1000;
		}
		else{
			if (*S3 / *Phi > 0)
				*alpha = log(*S3 / *Phi);
			else
				*alpha = -1000;
			if (*alpha < -1000)
				*alpha = -1000;
			if (*alpha > 0)
				*alpha = -1000;
		}
		
		// update mu
		gsl_vector_view gsl_mu = gsl_vector_view_array(mu, *N);
		gsl_blas_dgemv(CblasNoTrans, 1.0, &gsl_X.matrix, &gsl_beta.vector, 0.0, &gsl_mu.vector);
		for (int i = 0;i < *N;i++)
			mu[i] = hf(mu[i], *link);

		// update sigma
		double temp[1] = {0};
		for (int i = 0;i < *N;i++)
			*temp += dd(y[i], mu[i]);
		*temp /= (*N - *p);

		for (int i = 0;i < *N;i++)
			sigma[i] = *temp;

		// free the allocation
		gsl_matrix_free(SenMat);
		gsl_vector_free(Psi);
		gsl_vector_free(gsl_uu);
	}
	catch (...) {
		::Rf_error( "c++ exception (unknown reason)" );
	}
}

}
