#ifndef RAVEUTILS_FFTS_H
#define RAVEUTILS_FFTS_H

#include <fftw3.h>

/* real to complex forward */
void cfft_r2c(int* n, double* data,
              fftw_complex* res, int* retHermConj,
              int* fftwplanopt);

/* complex to real backward */
void cfft_c2r(int* nres, int* ndata, fftw_complex* data,
              double* res, int* fftwplanopt);

/* complex to complex --eitherway */
void cfft_c2c(int* n, fftw_complex* data,
              fftw_complex* res, int* inverse, int* fftwplanopt);

void cmvfft_r2c(int *n, int *m, double* data,
                fftw_complex* res,
                int* fftwplanopt);

void cmvfft_c2r(int *n, int *m, fftw_complex* data,
                double* res, int* fftwplanopt);

void cmvfft_c2c(int *n, int *m, fftw_complex* data,
                fftw_complex* res, int* inverse, int* fftwplanopt);

void cfft_r2c_2d(int* nx, int* ny, double* data, fftw_complex* res, int* fftwplanopt);

void cfft_c2c_2d(int* nx, int* ny, fftw_complex* data,
                 fftw_complex* res, int* inverse, int* fftwplanopt);

void cfft_r2c_3d(int* nx, int* ny, int *nz, double* data, fftw_complex* res,
                 int* fftwplanopt);

void cfft_c2c_3d(int* nx, int* ny, int *nz, fftw_complex* data,
                 fftw_complex* res, int* inverse, int* fftwplanopt);

void cfft_c2c_xd(int* r, int* n, fftw_complex* data,
                 fftw_complex* res, int* inverse);


#endif // RAVEUTILS_FFTS_H
