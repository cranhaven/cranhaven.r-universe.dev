#include <Rinternals.h>
#include <R.h>
#include <Rmath.h>
#include <math.h>
#include "consts.h"
#include "utils.h"



void stable_pdf_fourier_integral( double* pdf, double* x, int x_size, double a, double b ){
    int num_quad;
    double* gx;
    double* gw;

    if(a >= 1.1){
        num_quad = 86;
        gx = &GX86[0];
        gw = &GW86[0];
    }
    else if(a > 0.9){
        Rf_error("Inadmissible a for Fourier Integral");
    }
    else if(a >= 0.5){
        num_quad = 94;
        gx = &GX94[0];
        gw = &GW94[0];
    }
    else{
        Rf_error("%s%f", "Inadmissible a for Fourier Integral: ", a);
    }

    for(int i = 0; i != x_size; ++i)
        pdf[i] = 0;

    double rank_scaling =  pow(-log(EPS), 1.0/a);
    double z = -b * tan(a * PI/2);
    double h;

    for(int i = 0; i != num_quad; ++i){

        double scgx = rank_scaling * gx[i];
        double scgx_to_a = pow(scgx, a);
        double scgw = (rank_scaling / PI) * gw[i];

        for(int k = 0; k != x_size; ++k){
            h = (x[k] - z) * scgx + z * scgx_to_a;
            pdf[k] += scgw * cos(h) * exp(-scgx_to_a);
        }

    }
}



SEXP R_stable_pdf_fourier_integral(SEXP x, SEXP a, SEXP b){
    SEXP pdf = PROTECT(allocVector(REALSXP, LENGTH(x)));
    stable_pdf_fourier_integral( REAL(pdf), REAL(x), LENGTH(x), REAL(a)[0], REAL(b)[0]);
    UNPROTECT(1);
    return pdf;
}




void stable_sym_pdf_fourier_integral( double* pdf, double* x, int x_size, double a){
    int num_quad;
    double* gx;
    double* gw;

    if(a >= 0.5){
        num_quad = 46;
        gx = &GX_SYM_46[0];
        gw = &GW_SYM_46[0];
    }
    else
        Rf_error("Inadmissible a for Fourier Integral");

    set_to_zero(pdf, x_size);
    double rank_scaling =  pow(-log(EPS), 1.0/a);
    double scpi = rank_scaling / PI;

    for(int i = 0; i != num_quad; ++i){
        for(int k = 0; k != x_size; ++k){
            pdf[k] +=
                scpi * gw[i] *
                cos(x[k] * rank_scaling * gx[i]) *
                exp(-pow(rank_scaling * gx[i], a));
        }
    }
}


SEXP R_stable_sym_pdf_fourier_integral(SEXP x, SEXP a){
    SEXP pdf = PROTECT(allocVector(REALSXP, LENGTH(x)));
    stable_sym_pdf_fourier_integral( REAL(pdf), REAL(x), LENGTH(x), REAL(a)[0]);
    UNPROTECT(1);
    return pdf;
}







void stable_pdf_series_infinity(double* pdf, double* x, int x_size,
                                double alpha, double beta,
                                int max_coef)
{

    double zeta = -beta * tan( (PI * alpha) / 2.0 );
    double sqrt_1_plus_zeta = sqrt(1 + zeta*zeta);

    for(int i = 0; i != x_size; ++i)
        pdf[i] = 0;

    //------- initiate gamma part -------
    double gamma_part[max_coef + 1];
    for(int i = 0; i != max_coef + 1; ++i){
        gamma_part[i] = gammafn(alpha * (i + 1)) / factorial(i);
    }
    //------- initiate sin part -------
    double sin_part[max_coef + 1];
    for(int i = 0; i != max_coef + 1; ++i)
        sin_part[i] = sin( ((PI/2.0) * alpha - atan(zeta)) * (i + 1) );

    //------- initiate x part -------
    double x_part[x_size];
    for(int i = 0; i != x_size; ++i){
        x_part[i] = 1.0 / (x[i] - zeta);
    }


    int sign = -1;
    double geometric_part = 1.0;

    for(int i = 0; i != max_coef + 1; ++i){
        sign = -sign;
        geometric_part *= sqrt_1_plus_zeta;

        for(int k = 0; k != x_size; ++k){
            x_part[k] *= pow(x[k]-zeta, -alpha);
            pdf[k] += sign * gamma_part[i] * sin_part[i] * x_part[k] * geometric_part;
        }
    }

    for(int k = 0; k != x_size; ++k)
        pdf[k] *= (alpha/PI);
}


SEXP R_stable_pdf_series_infinity(SEXP x, SEXP a, SEXP b, SEXP max_coef){
    SEXP pdf = PROTECT(allocVector(REALSXP, LENGTH(x)));
    stable_pdf_series_infinity(REAL(pdf), REAL(x), LENGTH(x),
                               REAL(a)[0], REAL(b)[0], INTEGER(max_coef)[0]);
    UNPROTECT(1);
    return pdf;
}



void set_dnorm(double* pdf, double* x, int x_size){
    for(int i = 0; i != x_size; ++i)
        pdf[i] = dnorm(x[i], 0, sqrt(2.0), 0);
}


void set_dcauchy(double* pdf, double* x, int x_size){
    for(int i = 0; i != x_size; ++i)
        pdf[i] = dcauchy(x[i], 0, 1, 0);
}


int count_inf_cond(double* x, int x_size, double cond_val){
    int n = 0;
    for(int i = 0; i != x_size; ++i){
        if(x[i] < -cond_val || x[i] > cond_val)
            n++;
    }
    return n;
}



void set_subset_series_infty(double* y, double* x, int* y_idx, double* y_pdf,
                             double* pdf, int x_size, double cond_val, double a)
{

    // Fill up storage (y) with the subset of relevant x-values
    int n_filled = 0;

    for(int i = 0; i != x_size; ++i){
        if(x[i] < -cond_val){
            y[n_filled] = -x[i];   //flip sign of x since series expansion allows pos only
            y_idx[n_filled++] = i;
        }
        else if(x[i] > cond_val){
            y[n_filled] = x[i];
            y_idx[n_filled++] = i;
        }
    }

    stable_pdf_series_infinity(y_pdf, y, n_filled, a, 0, 42);

    for(int i = 0; i != n_filled; ++i)
        pdf[y_idx[i]] = y_pdf[i];
}



void set_subset_fourier(double* y, double* x, int* y_idx, double* y_pdf,
                        double* pdf, int x_size, double cond_val, double a)
{
    int n_filled = 0;
    for(int i = 0; i != x_size; ++i){
        if(-cond_val <= x[i] && x[i] <= cond_val){
            y[n_filled] = x[i];
            y_idx[n_filled++] = i;
        }
    }
    stable_sym_pdf_fourier_integral(y_pdf, y, n_filled, a);
    for(int i = 0; i != n_filled; ++i)
        pdf[y_idx[i]] = y_pdf[i];
}




void stable_sym_pdf(double* pdf, double* x, int x_size, double a){

    if(d_abs_diff(a, 2.0) < EPS)   //Gaussian
        set_dnorm(pdf, x, x_size);
    else if(d_abs_diff(a, 1.0) < EPS) // Cauchy
        set_dcauchy(pdf, x, x_size);
    else if(0.5 <= a && a <= 2.0){
        int n_inf = 42;

        double min_inf_x = pow(a/(PI*EPS) * gammafn(a*n_inf) / gammafn(n_inf), 1/(a*n_inf - 1));
        set_to_zero(pdf, x_size);

        int n_infcond = count_inf_cond(x, x_size, min_inf_x);

        // If some elements satsify the infcond, have to do 2 runs on separate
        // x-arrays: one which satsifies infcond, one which satosfies fouriercond.
        // Values need to be copied from x to these respective arrays.
        // Otherwise run fourier integral on x as-is.
        if(n_infcond > 0){
            int n_fouriercond = x_size - n_infcond;

            // allocate a vector of full x-length for storage of x-values for the various
            // conditions. Re-use this for fourier condition.
            // Additionally need a vector for indicies in x corresponding to the values.
            double y[x_size];
            int y_idx[x_size];
            double y_pdf[x_size];

            set_subset_series_infty(y, x, y_idx, y_pdf, pdf, x_size, min_inf_x, a);

            if(x_size > n_infcond){
                set_subset_fourier(y, x, y_idx, y_pdf, pdf, x_size, min_inf_x, a);
            }
        }
        else{
            stable_sym_pdf_fourier_integral(pdf, x, x_size, a);
        }
    }
    else
        Rf_error("Input alpha has to be between 0.5 and 2");
}


SEXP R_stable_sym_pdf(SEXP x, SEXP a){
    SEXP pdf = PROTECT(allocVector(REALSXP, LENGTH(x)));
    stable_sym_pdf(REAL(pdf), REAL(x), LENGTH(x), REAL(a)[0]);
    UNPROTECT(1);
    return pdf;
}



/*
void set_subset_xlz(double* y, double* x, int* y_idx, double* y_pdf,
                        double* pdf, int x_size, double cond_val, double a, double b)
{
    int n_filled = 0;
    for(int i = 0; i != x_size; ++i){
        if(x[i] < cond_val){
            y[n_filled] = -x[i]; // flip sign
            y_idx[n_filled++] = i;
        }
    }

    if(n_filled){
        stable_pdf(y_pdf, y, n_filled, a, -b);

        for(int i = 0; i != n_filled; ++i)
            pdf[y_idx[i]] = y_pdf[i];
    }
}
*/



/*
void stable_pdf(double* pdf, double* x, int x_size, double a, double b){

    if(a < 0.5 || (d_abs_diff(b, 0) > EPS && a > 0.9 && a < 1.1 ))
        Rf_error("Parameter configuration not supported");


    if(d_abs_diff(b, 0) < EPS){  // symmetric case
        stable_sym_pdf(pdf, x, x_size, a);
    }
    else{


        // // storage for various subset cases of x
        //double y[x_size];
        //int y_idx[x_size];
        //double y_pdf[x_size];


        //double z = -b * tan(0.5 * PI / a);
        //
        // // Go through each condition in sequence, computing the pdf for each segment and
        // // copying into pdf


    }

}
*/



double stable_pdf_singleobs(double x, double a, double b)
{
    if(a < 0.5 || (b != 0 && a > 0.9 && a < 1.1 ) || a > 2 || b < -1 || b > 1)
        Rf_error("Parameter configuration not supported: a = %f, b = %f", a, b);

    double pdf;

    if(b == 0)
        stable_sym_pdf(&pdf, &x, 1, a);
    else{
        double z = -b * tan((PI*a)/2.0);

        if(x < z){
            pdf = stable_pdf_singleobs(-x, a, -b);
        }
        else{
            int n_inf = 1.1 <= a ? 80 : 90;

            double base = pow(1+z*z, n_inf/2) * a /(PI * EPS) * gammafn(a*n_inf)/gammafn(n_inf);
            double min_inf_x = pow(base , 1/(a*n_inf-1));

            min_inf_x += z;

            if(min_inf_x < x)
                stable_pdf_series_infinity(&pdf, &x, 1, a, b, n_inf);
            else
                stable_pdf_fourier_integral(&pdf, &x, 1, a, b);
        }
    }

    return pdf;
}


SEXP R_stable_pdf_singleobs(SEXP x, SEXP a, SEXP b){
    if(LENGTH(x) != 1 || LENGTH(a) != 1 || LENGTH(b) != 1)
        Rf_error("Require unit-length inputs");
    return ScalarReal(stable_pdf_singleobs(REAL(x)[0], REAL(a)[0], REAL(b)[0]));
}


// computes pdf for each x value in the case whether a or b or both are vectors
// with length equal to x
SEXP R_stable_pdf_iter_singleobs(SEXP x, SEXP a, SEXP b){
    int n = LENGTH(x);
    int a_iter = 0;
    int b_iter = 0;

    if(LENGTH(a) > 1){
        if(LENGTH(a) != n)
            Rf_error("Length of alpha vec must be either 1 or equal to x length");
        a_iter = 1;
    }
    if(LENGTH(b) > 1){
        if(LENGTH(b) != n)
            Rf_error("Length of beta vec must be either 1 or equal to x length");
        b_iter = 1;
    }

    if(a_iter == 0 && b_iter == 0) {
        Rf_error("Need either a or b or both to be vectors of length equal to x");
    }


    SEXP pdf = PROTECT(allocVector(REALSXP, LENGTH(x)));
    double* pdfptr = REAL(pdf);
    double* xptr = REAL(x);

    if(a_iter && !b_iter){
        double b_val = REAL(b)[0];
        double* a_ptr = REAL(a);
        for(int i = 0; i != n; ++i)
            pdfptr[i] = stable_pdf_singleobs(xptr[i], a_ptr[i], b_val);
    }
    else if(!a_iter && b_iter){
        double* b_ptr = REAL(b);
        double a_val = REAL(a)[0];
        for(int i = 0; i != n; ++i)
            pdfptr[i] = stable_pdf_singleobs(xptr[i], a_val, b_ptr[i]);
    }
    else{ // iterate thoguth both a and b
        double* b_ptr = REAL(b);
        double* a_ptr = REAL(a);
        for(int i = 0; i != n; ++i){
            pdfptr[i] = stable_pdf_singleobs(xptr[i], a_ptr[i], b_ptr[i]);
        }
    }

    UNPROTECT(1);
    return pdf;
}


