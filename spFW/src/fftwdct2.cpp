
#ifndef FFTWDCT2_CPP_
#define  FFTWDCT2_CPP_

#include <Rcpp.h>
#include <fftw3.h>

using namespace Rcpp;


#ifndef M_SQRT1_2l
# define M_SQRT1_2l	0.707106781186547524400844362104849039L /* 1/sqrt(2) */
#endif

// [[Rcpp::export]]
NumericVector dct2mod(NumericVector inR, size_t m, size_t n)
{

    size_t N = m*n;
    if(inR.size() != N)
        stop("DCT2MOD: The input vector's length does not match with the dimensions provided");

    double *in = inR.begin();


    NumericVector outR(N);
    double *out = outR.begin();

    fftw_plan p = fftw_plan_r2r_2d(n,m,in,out, FFTW_REDFT10, FFTW_REDFT10,
                                   FFTW_ESTIMATE);

    fftw_execute(p);
    fftw_destroy_plan(p);

    /* scale the matrix out */
    double scale1 = 0.25/sqrt(N);
    out[0] *= scale1;

    scale1 *= 2.0; /* 1/(2*sqrt(m*n)) */
    double scale2 = scale1 * M_SQRT1_2l;

    for(int i=1;i<n;++i)
    {
        out[i*m] *= scale2;
        for(int j=1;j<m;++j)
            out[i*m + j] *= scale1;
    }



    for(int i=1;i<m;++i)
        out[i] *= scale2; /* first row */

    NumericVector outR2(N-1);
    std::copy(outR.begin() + 1, outR.end(), outR2.begin());
    return outR2;
}




// [[Rcpp::export]]
NumericVector idct2mod(NumericVector inR, size_t m, size_t n)
{
    size_t N = m*n;

    if(inR.size() != N-1)
        stop("iDCT2MOD: The input vector's length must be m*n-1");

    NumericVector outR(N);
    double *in = inR.begin();
    double *out = outR.begin();

    double scale = 1.0/sqrt(N);
    out[0] = 0;

    scale *= 0.5;
    for(int j=1;j<n;++j)
	for(int i=1;i<m;++i)
            out[i + m*j] = scale*in[i + m*j - 1];


    scale *= 1.41421356237309504880L;

    for(int i=1;i<m;++i)
        out[i] = scale*in[i-1]; /* first column */
    for(int i=1;i<n;++i)
        out[i*m] = scale*in[i*m-1]; /* first row */


    fftw_plan p = fftw_plan_r2r_2d(n, m,out,out, FFTW_REDFT01, FFTW_REDFT01,
            FFTW_ESTIMATE);
    fftw_execute(p);
    fftw_destroy_plan(p);

    return outR;
}

#endif

