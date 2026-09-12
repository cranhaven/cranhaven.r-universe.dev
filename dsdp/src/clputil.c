#include <math.h>
#include "clp.h"
#include "clpmisc.h"
#include "clputil.h"
#include <Rinternals.h>
#include <R_ext/BLAS.h>


/*
    Moment of Exponential Distribution
INPUT:
    n: nth moment
    lmd: parameter with lmd*exp(-lmd*x)
RETURN:
    nth moment of exponential distribution with lmd
*/
double momentExpDist(const CLP_INT n, const double lmd)
{
    double z;
    z = tgamma((double)(n + 1)) / pow(lmd, (double)n);
    return z;
}

/*
    Moment of Gaussian Distribution
INPUT:
    n: nth moment
    mu: mean of Gaussian distribution
    sig: standard deviation of Gaussian distribution
RETURN:
    nth moment of Gaussian distribution with mu and sig
*/
double momentGaussDist(const CLP_INT n, const double mu, const double sig)
{
    double dd = 0.0;

    if (n == 0)
    {
        dd = 1;
    }
    else if (n == 1)
    {
        dd = mu;
    }
    else if (n == 2)
    {
        dd = pow(mu, 2) + pow(sig, 2);
    }
    else if (n == 3)
    {
        dd = pow(mu, 3) + 3 * mu * pow(sig, 2);
    }
    else if (n == 4)
    {
        dd = pow(mu, 4) + 6 * pow(mu, 2) * pow(sig, 2) + 3 * pow(sig, 4);
    }
    else if (n == 5)
    {
        dd = pow(mu, 5) + 10 * pow(mu, 3) * pow(sig, 2) + 15 * mu * pow(sig, 4);
    }
    else if (n == 6)
    {
        dd = pow(mu, 6) + 15 * pow(mu, 4) * pow(sig, 2) + 45 * pow(mu, 2) * pow(sig, 4) + 15 * pow(sig, 6);
    }
    else if (n == 7)
    {
        dd = pow(mu, 7) + 21 * pow(mu, 5) * pow(sig, 2) + 105 * pow(mu, 3) * pow(sig, 4) + 105 * mu * pow(sig, 6);
    }
    else if (n == 8)
    {
        dd = pow(mu, 8) + 28 * pow(mu, 6) * pow(sig, 2) + 210 * pow(mu, 4) * pow(sig, 4) + 420 * pow(mu, 2) * pow(sig, 6) + 105 * pow(sig, 8);
    }
    else if (n == 9)
    {
        dd = mu * (pow(mu, 8) + 36 * pow(mu, 6) * pow(sig, 2) + 378 * pow(mu, 4) * pow(sig, 4) + 1260 * pow(mu, 2) * pow(sig, 6) + 945 * pow(sig, 8));
    }
    else if (n == 10)
    {
        dd = pow(mu, 10) + 45 * pow(mu, 8) * pow(sig, 2) + 630 * pow(mu, 6) * pow(sig, 4) + 3150 * pow(mu, 4) * pow(sig, 6) + 4725 * pow(mu, 2) * pow(sig, 8) + 945 * pow(sig, 10);
    }
    else if (n == 11)
    {
        dd = mu * (pow(mu, 10) + 55 * pow(mu, 8) * pow(sig, 2) + 990 * pow(mu, 6) * pow(sig, 4) + 6930 * pow(mu, 4) * pow(sig, 6) + 17325 * pow(mu, 2) * pow(sig, 8) + 10395 * pow(sig, 10));
    }
    else if (n == 12)
    {
        dd = pow(mu, 12) + 66 * pow(mu, 10) * pow(sig, 2) + 1485 * pow(mu, 8) * pow(sig, 4) + 13860 * pow(mu, 6) * pow(sig, 6) + 51975 * pow(mu, 4) * pow(sig, 8) +
             62370 * pow(mu, 2) * pow(sig, 10) + 10395 * pow(sig, 12);
    }
    else if (n == 13)
    {
        dd = mu * (pow(mu, 12) + 78 * pow(mu, 10) * pow(sig, 2) + 2145 * pow(mu, 8) * pow(sig, 4) + 25740 * pow(mu, 6) * pow(sig, 6) + 135135 * pow(mu, 4) * pow(sig, 8) +
                   270270 * pow(mu, 2) * pow(sig, 10) + 135135 * pow(sig, 12));
    }
    else if (n == 14)
    {
        dd = pow(mu, 14) + 91 * pow(mu, 12) * pow(sig, 2) + 3003 * pow(mu, 10) * pow(sig, 4) + 45045 * pow(mu, 8) * pow(sig, 6) + 315315 * pow(mu, 6) * pow(sig, 8) + 945945 * pow(mu, 4) * pow(sig, 10) + 945945 * pow(mu, 2) * pow(sig, 12) + 135135 * pow(sig, 14);
    }
    else if (n == 15)
    {
        dd = pow(mu, 15) + 105 * pow(mu, 13) * pow(sig, 2) + 4095 * pow(mu, 11) * pow(sig, 4) + 75075 * pow(mu, 9) * pow(sig, 6) + 675675 * pow(mu, 7) * pow(sig, 8) + 2837835 * pow(mu, 5) * pow(sig, 10) + 4729725 * pow(mu, 3) * pow(sig, 12) + 2027025 * mu * pow(sig, 14);
    }
    else if (n == 16)
    {
        dd = pow(mu, 16) + 120 * pow(mu, 14) * pow(sig, 2) + 5460 * pow(mu, 12) * pow(sig, 4) + 120120 * pow(mu, 10) * pow(sig, 6) + 1351350 * pow(mu, 8) * pow(sig, 8) + 7567560 * pow(mu, 6) * pow(sig, 10) + 18918900 * pow(mu, 4) * pow(sig, 12) + 16216200 * pow(mu, 2) * pow(sig, 14) + 2027025 * pow(sig, 16);
    }
    else if (n == 17)
    {
        dd = pow(mu, 17) + 136 * pow(mu, 15) * pow(sig, 2) + 7140 * pow(mu, 13) * pow(sig, 4) + 185640 * pow(mu, 11) * pow(sig, 6) + 2552550 * pow(mu, 9) * pow(sig, 8) + 18378360 * pow(mu, 7) * pow(sig, 10) + 64324260 * pow(mu, 5) * pow(sig, 12) + 91891800 * pow(mu, 3) * pow(sig, 14) + 34459425 * mu * pow(sig, 16);
    }
    else if (n == 18)
    {
        dd = pow(mu, 18) + 153 * pow(mu, 16) * pow(sig, 2) + 9180 * pow(mu, 14) * pow(sig, 4) + 278460 * pow(mu, 12) * pow(sig, 6) + 4594590 * pow(mu, 10) * pow(sig, 8) + 41351310 * pow(mu, 8) * pow(sig, 10) + 192972780 * pow(mu, 6) * pow(sig, 12) + 413513100 * pow(mu, 4) * pow(sig, 14) + 310134825 * pow(mu, 2) * pow(sig, 16) + 34459425 * pow(sig, 18);
    }
    else if (n == 19)
    {
        dd = pow(mu, 19) + 171 * pow(mu, 17) * pow(sig, 2) + 11628 * pow(mu, 15) * pow(sig, 4) + 406980 * pow(mu, 13) * pow(sig, 6) + 7936110 * pow(mu, 11) * pow(sig, 8) + 87297210 * pow(mu, 9) * pow(sig, 10) + 523783260 * pow(mu, 7) * pow(sig, 12) + 1571349780 * pow(mu, 5) * pow(sig, 14) + 1964187225 * pow(mu, 3) * pow(sig, 16) + 654729075 * mu * pow(sig, 18);
    }
    else if (n == 20)
    {
        dd = pow(mu, 20) + 190 * pow(mu, 18) * pow(sig, 2) + 14535 * pow(mu, 16) * pow(sig, 4) + 581400 * pow(mu, 14) * pow(sig, 6) + 13226850 * pow(mu, 12) * pow(sig, 8) + 174594420 * pow(mu, 10) * pow(sig, 10) + 1309458150 * pow(mu, 8) * pow(sig, 12) + 5237832600 * pow(mu, 6) * pow(sig, 14) + 9820936125 * pow(mu, 4) * pow(sig, 16) + 6547290750 * pow(mu, 2) * pow(sig, 18) + 654729075 * pow(sig, 20);
    }
    else if (n == 21)
    {
        dd = pow(mu, 21) + 210 * pow(mu, 19) * pow(sig, 2) + 17955 * pow(mu, 17) * pow(sig, 4) + 813960 * pow(mu, 15) * pow(sig, 6) + 21366450 * pow(mu, 13) * pow(sig, 8) + 333316620 * pow(mu, 11) * pow(sig, 10) + 3055402350 * pow(mu, 9) * pow(sig, 12) + 15713497800 * pow(mu, 7) * pow(sig, 14) + 41247931725 * pow(mu, 5) * pow(sig, 16) + 45831035250 * pow(mu, 3) * pow(sig, 18) + 13749310575 * mu * pow(sig, 20);
    }
    else if (n == 22)
    {
        dd = pow(mu, 22) + 231 * pow(mu, 20) * pow(sig, 2) + 21945 * pow(mu, 18) * pow(sig, 4) + 1119195 * pow(mu, 16) * pow(sig, 6) + 33575850 * pow(mu, 14) * pow(sig, 8) + 611080470 * pow(mu, 12) * pow(sig, 10) + 6721885170 * pow(mu, 10) * pow(sig, 12) + 43212118950 * pow(mu, 8) * pow(sig, 14) + 151242416325 * pow(mu, 6) * pow(sig, 16) + 252070693875 * pow(mu, 4) * pow(sig, 18) + 151242416325 * pow(mu, 2) * pow(sig, 20) + 13749310575 * pow(sig, 22);
    }
    else if (n == 23)
    {
        dd = pow(mu, 23) + 253 * pow(mu, 21) * pow(sig, 2) + 26565 * pow(mu, 19) * pow(sig, 4) + 1514205 * pow(mu, 17) * pow(sig, 6) + 51482970 * pow(mu, 15) * pow(sig, 8) + 1081142370 * pow(mu, 13) * pow(sig, 10) + 14054850810 * pow(mu, 11) * pow(sig, 12) + 110430970650 * pow(mu, 9) * pow(sig, 14) + 496939367925 * pow(mu, 7) * pow(sig, 16) + 1159525191825 * pow(mu, 5) * pow(sig, 18) + 1159525191825 * pow(mu, 3) * pow(sig, 20) + 316234143225 * mu * pow(sig, 22);
    }
    else if (n == 24)
    {
        dd = pow(mu, 24) + 276 * pow(mu, 22) * pow(sig, 2) + 31878 * pow(mu, 20) * pow(sig, 4) + 2018940 * pow(mu, 18) * pow(sig, 6) + 77224455 * pow(mu, 16) * pow(sig, 8) + 1853386920 * pow(mu, 14) * pow(sig, 10) + 28109701620 * pow(mu, 12) * pow(sig, 12) + 265034329560 * pow(mu, 10) * pow(sig, 14) + 1490818103775 * pow(mu, 8) * pow(sig, 16) + 4638100767300 * pow(mu, 6) * pow(sig, 18) + 6957151150950 * pow(mu, 4) * pow(sig, 20) + 3794809718700 * pow(mu, 2) * pow(sig, 22) + 316234143225 * pow(sig, 24);
    }
    else if (n == 25)
    {
        dd = pow(mu, 25) + 300 * pow(mu, 23) * pow(sig, 2) + 37950 * pow(mu, 21) * pow(sig, 4) + 2656500 * pow(mu, 19) * pow(sig, 6) + 113565375 * pow(mu, 17) * pow(sig, 8) + 3088978200 * pow(mu, 15) * pow(sig, 10) + 54057118500 * pow(mu, 13) * pow(sig, 12) + 602350749000 * pow(mu, 11) * pow(sig, 14) + 4141161399375 * pow(mu, 9) * pow(sig, 16) + 16564645597500 * pow(mu, 7) * pow(sig, 18) + 34785755754750 * pow(mu, 5) * pow(sig, 20) + 31623414322500 * pow(mu, 3) * pow(sig, 22) + 7905853580625 * mu * pow(sig, 24);
    }
    else if (n == 26)
    {
        dd = pow(mu, 26) + 325 * pow(mu, 24) * pow(sig, 2) + 44850 * pow(mu, 22) * pow(sig, 4) + 3453450 * pow(mu, 20) * pow(sig, 6) + 164038875 * pow(mu, 18) * pow(sig, 8) + 5019589575 * pow(mu, 16) * pow(sig, 10) + 100391791500 * pow(mu, 14) * pow(sig, 12) + 1305093289500 * pow(mu, 12) * pow(sig, 14) + 10767019638375 * pow(mu, 10) * pow(sig, 16) + 53835098191875 * pow(mu, 8) * pow(sig, 18) + 150738274937250 * pow(mu, 6) * pow(sig, 20) + 205552193096250 * pow(mu, 4) * pow(sig, 22) + 102776096548125 * pow(mu, 2) * pow(sig, 24) + 7905853580625 * pow(sig, 26);
    }
    else if (n == 27)
    {
        dd = pow(mu, 27) + 351 * pow(mu, 25) * pow(sig, 2) + 52650 * pow(mu, 23) * pow(sig, 4) + 4440150 * pow(mu, 21) * pow(sig, 6) + 233107875 * pow(mu, 19) * pow(sig, 8) + 7972289325 * pow(mu, 17) * pow(sig, 10) + 180705224700 * pow(mu, 15) * pow(sig, 12) + 2710578370500 * pow(mu, 13) * pow(sig, 14) + 26428139112375 * pow(mu, 11) * pow(sig, 16) + 161505294575625 * pow(mu, 9) * pow(sig, 18) + 581419060472250 * pow(mu, 7) * pow(sig, 20) + 1109981842719750 * pow(mu, 5) * pow(sig, 22) + 924984868933125 * pow(mu, 3) * pow(sig, 24) + 213458046676875 * mu * pow(sig, 26);
    }
    else if (n == 28)
    {
        dd = pow(mu, 28) + 378 * pow(mu, 26) * pow(sig, 2) + 61425 * pow(mu, 24) * pow(sig, 4) + 5651100 * pow(mu, 22) * pow(sig, 6) + 326351025 * pow(mu, 20) * pow(sig, 8) + 12401338950 * pow(mu, 18) * pow(sig, 10) + 316234143225 * pow(mu, 16) * pow(sig, 12) + 5421156741000 * pow(mu, 14) * pow(sig, 14) + 61665657928875 * pow(mu, 12) * pow(sig, 16) + 452214824811750 * pow(mu, 10) * pow(sig, 18) + 2034966711652875 * pow(mu, 8) * pow(sig, 20) + 5179915266025500 * pow(mu, 6) * pow(sig, 22) + 6474894082531875 * pow(mu, 4) * pow(sig, 24) + 2988412653476250 * pow(mu, 2) * pow(sig, 26) + 213458046676875 * pow(sig, 28);
    }
    // else if (n == 29)
    // {
    //     dd = pow(mu, 29) + 406 * pow(mu, 27) * pow(sig, 2) + 71253 * pow(mu, 25) * pow(sig, 4) + 7125300 * pow(mu, 23) * pow(sig, 6) + 450675225 * pow(mu, 21) * pow(sig, 8) + 18928359450 * pow(mu, 19) * pow(sig, 10) + 539458244325 * pow(mu, 17) * pow(sig, 12) + 10480903032600 * pow(mu, 15) * pow(sig, 14) + 137561852302875 * pow(mu, 13) * pow(sig, 16) + 1192202719958250 * pow(mu, 11) * pow(sig, 18) + 6557114959770375 * pow(mu, 9) * pow(sig, 20) + 21459648959248500 * pow(mu, 7) * pow(sig, 22) + 37554385678684875 * pow(mu, 5) * pow(sig, 24) + 28887988983603750 * pow(mu, 3) * pow(sig, 26) + 6190283353629375 * mu * pow(sig, 28);
    // }
    // else if (n == 30)
    // {
    //     dd = pow(mu, 30) + 435 * pow(mu, 28) * pow(sig, 2) + 82215 * pow(mu, 26) * pow(sig, 4) + 8906625 * pow(mu, 24) * pow(sig, 6) + 614557125 * pow(mu, 22) * pow(sig, 8) + 28392539175 * pow(mu, 20) * pow(sig, 10) + 899097073875 * pow(mu, 18) * pow(sig, 12) + 19651693186125 * pow(mu, 16) * pow(sig, 14) + 294775397791875 * pow(mu, 14) * pow(sig, 16) + 2980506799895625 * pow(mu, 12) * pow(sig, 18) + 19671344879311125 * pow(mu, 10) * pow(sig, 20) + 80473683597181875 * pow(mu, 8) * pow(sig, 22) + 187771928393424375 * pow(mu, 6) * pow(sig, 24) + 216659917377028125 * pow(mu, 4) * pow(sig, 26) + 92854250304440625 * pow(mu, 2) * pow(sig, 28) + 6190283353629375 * pow(sig, 30);
    // }
    return dd;
}

/*
    Compute Moment matrices for Gauss Model
INPUT:
    d: degree of polynomial
    mu: mean of Gauss Model
    sig; standard deviation of Gauss model
OUTPUT:
    M1: Moment matrix
*/
void compute_GaussDistMomentMatrix(const CLP_INT d, const double mu, 
    const double sig, double *M1)
{
    CLP_INT n1;
    n1 = d/2 + 1;

    for (size_t i=0; i<n1; ++i)
    {
        for (size_t j=0; j<n1; ++j)
        {
            M1[i * n1 + j] = momentGaussDist(i + j, mu, sig);
        }
    }
}

/*
    Compute Moment matrices for Exponential Model
INPUT:
    d: degree of polynomial
    lmd: lmd*exp(-lmd*x)
OUTPUT:
    M1: Moment matrix
    M2: Moment matrix
*/
void compute_ExpDistMomentMatrix(const CLP_INT d, const double lmd,
                                 double *M1, double *M2)
{
    CLP_INT n1, n2;
    
    if (ODDP(d))
    {
        n1 = (d-1)/2 + 1;
        n2 = n1;
    }
    else
    {
        n1 = d/2+1;
        n2 = d/2;
    }

    for (size_t i=0; i<n1; ++i)
    {
        for (size_t j=0; j<n1; ++j)
        {
            M1[i*n1 + j] = momentExpDist(i + j, lmd);
        }
    }

    for (size_t i=0; i<n2; ++i)
    {
        for (size_t j=0; j<n2; ++j)
        {
            M2[i*n2 + j] = momentExpDist(i + j + 1, lmd);
        }
    }
}

/*
    Compute the data Matrix for Gauss Model
INPUT:
    d: degree of polynomial
    nlen: length of data
    xin: data vector
OUTPUT:
    arrayX: data array
*/
CLP_INT compute_GaussDistDataMatrix(const CLP_INT d, const  CLP_INT nlen,
    const double *xin, double *arrayX)
{
    INFO_CLP info=SUCCESS;
    CLP_INT n1, n1len;
    double x;
    double *tz=NULL;

    n1 = d/2 + 1;
    n1len = n1 * n1;
    tz = (double *) CLP_MALLOC(sizeof(double) * n1);
    CHECKNULL2(tz);

    double alpha = 1.0;
    CLP_INT incx = 1;

    for (size_t i=0; i<nlen; ++i)
    {
        x = xin[i];
        tz[0] = 1.0;
        for (size_t j=1; j<n1; ++j)
        {
            tz[j] = x * tz[j-1];
        }
        F77_NAME(dger)(&n1, &n1, &alpha, tz, &incx, tz, &incx, &(arrayX[i*n1len]), &n1);
    }

EXCEPTION:
    CLP_FREE(tz);
    return info;
}

/*
    Compute the data Matrix for Exponential Model
INPUT:
    d: degree of polynomial
    nlen: length of data
    xin: data vector
OUTPUT:
    arrayX1: data array1
    arrayX2: data array2
*/
CLP_INT compute_ExpDistDataMatrix(const CLP_INT d, const CLP_INT nlen,
    const double *xin, double *arrayX1, double *arrayX2)
{
    INFO_CLP info = SUCCESS;
    CLP_INT n1, n2, n1len, n2len;
    CLP_INT incx = 1;
    double x, alpha=1.0;
    double *tz=NULL;

    if (ODDP(d))
    {
        n1 = (d-1)/2 + 1;
        n2 = n1;
    }
    else
    {
        n1 = d/2+1;
        n2 = d/2;
    }
    n1len = n1 * n1;
    n2len = n2 * n2;
    tz = (double *) CLP_MALLOC(sizeof(double) * n1);
    CHECKNULL2(tz);

    for (size_t i=0; i<nlen; ++i)
    {
        x = xin[i];
        tz[0] = 1.0;
        for (size_t j=1; j<n1; ++j)
        {
            tz[j] = x * tz[j-1];
        }

        F77_NAME(dger)(&n1, &n1, &alpha, tz, &incx, tz, &incx, &(arrayX1[i*n1len]), &n1);
        F77_NAME(dger)(&n2, &n2, &x, tz, &incx, tz, &incx, &(arrayX2[i*n2len]), &n2);
    }

EXCEPTION:
    CLP_FREE(tz);
    return info;
}

/*
    Compute the coefficient of polynomial from Matrices M1
    obtained from Gauss model
INPUT:
    d: degree of polynomial
    M1: coefficient Matrix
OUTPUT:
    av: coefficient vector with length (d+1)
*/
void compute_coeff1(const CLP_INT d, const double *M1, double *av)
{
    CLP_INT n;
    n = d / 2 + 1;

    zerofill_vec(d+1, av);

    for (size_t i=0; i<=d; ++i)
    {
        av[i] = 0.0;
    }

    for (size_t j=0; j<n; ++j)
    {
        for (size_t i=0; i<n; ++i)
        {
            av[i+j] += M1[i+j*n];
        }
    }
}

/*
    Compute the coefficient of polynomial from Matrices M1 and M2,
    obtained from Exponential model
INPUT:
    d: degree of polynomial
    M1: coefficient Matrix
    M2: coefficient Matrix
OUTPUT:
    av: coefficient vector with length (d+1)
*/
void compute_coeff2(const CLP_INT d, const double *M1, const double *M2,
    double *av)
{
    CLP_INT n1, n2;

    if (ODDP(d))
    {
        n1 = (d-1)/2 + 1;
        n2 = n1;
    }
    else
    {
        n1 = d/2+1;
        n2 = d/2;
    }

    zerofill_vec(d+1, av);

    for (size_t j=0; j<n1; ++j)
    {
        for (size_t i=0; i<n1; ++i)
        {
            av[i+j] += M1[i+j*n1];
        }
    }
    for (size_t j=0; j<n2; ++j)
    {
        for (size_t i=0; i<n2; ++i)
        {
            av[i+j+1] += M2[i+j*n2];
        }
    }
}
/*
    Evaluate the polynomial
    d: degree of polynomial
    coeffv: coefficient vector of polynomial of degree n.
    xv: input real vector
    yv: output real vector
    nlen: length of vectors
*/
void eval_poly(const CLP_INT d, const CLP_INT nlen,const double *coeffv, 
    const double *xv, double *yv)
{
    for (size_t j=0; j<nlen; ++j)
    {
        yv[j] = coeffv[d];
    }

    // for (CLP_INT i=d-1; i>=0; ++i)
    for (size_t i=d; i--;)
    {
        for (size_t j=0; j<nlen; ++j)
        {
            yv[j] = yv[j] * xv[j] + coeffv[i];
        }
    }
}

/*
    Compute the coefficient substituted with alpha*x+beta
INPUT:
    n: degree of polynomial
    av: input coefficient vector
    alpha, beta: coefficient of alpha*x+beta
OUTPUT:
    a1v: substitution of coefficient vector
*/
void polyaxb(const CLP_INT n, const double *av, const double c,
    const double alpha, const double beta, double *a1v)
{
    for (size_t j=0; j<=n; ++j)
    {
        a1v[j] = av[j];
    }

    for (size_t j=1; j<=n; ++j)
    {
        a1v[n-j] = a1v[n-j] + beta * a1v[n-j+1];
        for (size_t i=n-j+1; i<=n-1; ++i)
        {
            a1v[i] = alpha * a1v[i] + beta * a1v[i+1];
        }
        a1v[n] = alpha * a1v[n];
    }

    if (c != 1.0)
    {
        for (size_t j=0; j<=n; ++j)
        {
            a1v[j] = c*a1v[j];
        }
    }
}

/*
    Compute AIC for Gaussian Model
INPUT:
    d: degree of polynomial
    n: length of data
    y: vector of likelihood estimation
    c: # of occurrences for each data value
    x: data vector
    mu: mean of Gaussian model
    sig: standard deviation of Gaussian model
RETURN:
    AIC
*/
double aic_GaussDist(const CLP_INT d, const CLP_INT n, const double *y,
    const double *c, const double *x, const double mu, const double sig)
{
    double aic = 0.0;
    double tc = 0.0;
    if (NOTNULLP(c))
    {
        for (size_t i=0; i<n; ++i)
        {
            aic -= c[i] * log(y[i]);
        }
        for (size_t i=0; i<n; ++i)
        {
            tc += c[i];
        }
        tc = tc * (log(0.5*M_PI) + log(sig));
        for (size_t i=0; i<n; ++i)
        {
            tc += 0.5*c[i] * pow(x[i]-mu, 2.0) / (sig*sig);
        }
    }
    else
    {
        for (size_t i=0; i<n; ++i)
        {
            aic -= log(y[i]);
        }

        tc += (double)n;

        tc = tc * (log(0.5*M_PI) + log(sig));
        for (size_t i=0; i<n; ++i)
        {
            tc += 0.5 * pow(x[i]-mu, 2.0) / (sig*sig);
        }
    }


    return aic + tc + (double)d + 2.0;
}

/*
    Compute AIC for Exponential Model
INPUT:
    d: degree of polynomial
    n: length of data
    y: vector of likelihood estimation
    c: # of occurrences for each data value
    x: data vector
    lmd: lmd*exp(-lmd*x)
RETURN:
    AIC
*/
double aic_ExpDist(const CLP_INT d, const CLP_INT n, const double *y,
    const double *c, const double *x, const double lmd)
{
    double aic = 0.0;
    double tc = 0.0;
    double tc2 = 0.0;
    if (NOTNULLP(c))
    {
        for (size_t i=0; i<n; ++i)
        {
            aic -= c[i] * log(y[i]);
        }

        for (size_t i=0; i<n; ++i)
        {
            tc += c[i];
        }
        tc = -tc * (log(lmd));

        for (size_t i=0; i<n; ++i)
        {
            tc2 += c[i] * x[i];
        }
        tc2 = tc2 * lmd;
    }
    else
    {
        for (size_t i=0; i<n; ++i)
        {
            aic -= log(y[i]);
        }

        tc = -(double)n * (log(lmd));

        for (size_t i=0; i<n; ++i)
        {
            tc2 += x[i];
        }
        tc2 = tc2 * lmd;
    }


    return aic + tc + tc2 + (double)d + 1.0;
}

double histmean(const CLP_INT n, const double *data, const double *freq)
{
    double zsum=0.0;
    double nsum=0.0;

    if (NULLP(freq))
    {
        for (CLP_INT i=0; i<n; ++i)
        {
            zsum += data[i];
        }
        return zsum/(double)n;
    }
    else
    {
        for (CLP_INT i=0; i<n; ++i)
        {
            if (freq[i] > 0){
                zsum += data[i]*freq[i];
                nsum += freq[i];
            }
        }
        return zsum/nsum;
    }
}

double histstd(const CLP_INT n, const double mean, const double *data,
    const double *freq)
{
    double zsum=0.0;
    double nsum=0.0;
    double tz;

    if (NULLP(freq))
    {
        for (CLP_INT i=0; i<n; ++i)
        {
            tz = data[i] - mean;
            zsum += tz*tz;
        }
        return sqrt(zsum/(double)(n-1));
    }
    else
    {
        for (CLP_INT i=0; i<n; ++i)
        {
            tz = data[i] - mean;
            zsum += tz*tz*freq[i];
            nsum += freq[i];
        }
        return sqrt(zsum/(nsum-1.0));
    }
}

double ggamma(const double alpha, const double lmd, const double p,
    const double t)
{
    double z;
    z = p*lmd * pow(lmd*t, alpha-1.0) * exp(-pow(lmd*t, p)) / tgamma(alpha/p);
    return z;
}

double momentGGammaDist(const CLP_INT n, const double alpha, const double lmd,
    const double p)
{
    double z;
    z = tgamma(((double)n*alpha)/p) / (pow(lmd, (double)n)*tgamma(alpha/p));
    return z;
}

/*
    Compute Moment matrices for Generalized Gamma Model
INPUT:
    d: degree of polynomial
    alpha, lmd, p
OUTPUT:
    M1: Moment matrix
    M2: Moment matrix
*/
void compute_GGammaDistMomentMatrix(const CLP_INT d, const double alpha,
    const double lmd, const double p, double *M1, double *M2)
{
    CLP_INT n1, n2;
    
    if (ODDP(d))
    {
        n1 = (d-1)/2 + 1;
        n2 = n1;
    }
    else
    {
        n1 = d/2+1;
        n2 = d/2;
    }

    for (size_t i=0; i<n1; ++i)
    {
        for (size_t j=0; j<n1; ++j)
        {
            M1[i*n1 + j] = momentGGammaDist(i + j, alpha, lmd, p);
        }
    }

    for (size_t i=0; i<n2; ++i)
    {
        for (size_t j=0; j<n2; ++j)
        {
            M2[i*n2 + j] = momentGGammaDist(i + j + 1, alpha, lmd, p);
        }
    }
}

CLP_INT cdf_polygauss(const CLP_INT d, const CLP_INT nlen, const double *coeffv,
    const double mu, const double sig, const double *xv, double *yv)
{
    CLP_INT info = SUCCESS;
    double t, a, x; // pans, qans;
    double alpha, beta, *a1v=NULL;
    // long ierr;
    
    a1v = (double*) CLP_MALLOC(sizeof(double) * (d+1));
    CHECKNULL2(a1v);

    alpha = sqrt(2.0) * sig;
    beta = mu;
    polyaxb(d, coeffv, 1.0, alpha, beta, a1v);

    for (CLP_INT i=0; i<nlen; ++i)
    {
        t = sqrt05 * (xv[i] - mu) / sig;
        x = pow(t, 2.0);
        double z=0.0;
        if (t >= 0)
        {
            for (CLP_INT k=0; k<=d; ++k)
            {
                a = 0.5 * (1.0 + (double)k);
                z += a1v[k] * (pow(-1.0, (double)k)  * tgamma(a) + igamma(a, x));
                // dgami(a, x, &pans, &qans, &ierr);
                // if (ierr != 0)
                // {
                //     info = ERROR_IGAMMA;
                //     goto EXCEPTION;
                // }
                // z += a1v[k] * (pow(-1.0, (double)k) + pans) * tgamma(a);
            }
        }
        else
        {
            for (CLP_INT k=0; k<=d; ++k)
            {
                a = 0.5 * (1.0 + (double)k);
                z += a1v[k] * pow(-1.0, (double)k) * icgamma(a, x);
                // dgami(a, x, &pans, &qans, &ierr);
                // if (ierr != 0)
                // {
                //     info = ERROR_IGAMMA;
                //     goto EXCEPTION;
                // }
                // z += a1v[k] * pow(-1.0, (double)k) * qans * tgamma(a);
            }
        }

        yv[i] = 0.5 * z / sqrt(M_PI);
    }
    
EXCEPTION:
    CLP_FREE(a1v);
    return info;
}

CLP_INT cdf_polyggamma(const CLP_INT d, const CLP_INT nlen, const double *coeffv,
    const double alpha, const double lmd, const double p,
    const double *xv, double *yv)
{
    double t, a, x; // pans, qans;
    // long ierr;
    for (CLP_INT i=0; i<nlen; ++i)
    {
        t = xv[i];
        double z=0.0;
        if (t >= 0) {
            for (CLP_INT k=0; k<=d; ++k)
            {
                a = (alpha + (double)k) / p;
                x = pow(lmd*t, p);
                z += coeffv[k] * igamma(a, x) / (tgamma(alpha/p) * pow(lmd, (double)k));
                // dgami(a, x, &pans, &qans, &ierr);
                // if (ierr != 0)
                // {
                //     return ERROR_IGAMMA;
                // }
                // z += coeffv[k] * pans * tgamma(a) / (tgamma(alpha/p) * pow(lmd, (double)k));
            }
        }
        yv[i] = z;
    }
    return SUCCESS;
    
}

/*
    Incomplete gamma function
*/
double F77_NAME(dgami)(const double *, const double *);
double igamma(const double a, const double x)
{
    double z;
    z = F77_NAME(dgami)(&a, &x);
    return z;
}
// {
//     double pans, qans;
//     long ierr;
//     dgami(a, x, &pans, &qans, &ierr);
//     if (ierr != 0)
//     {
//         return -1;
//     }
//     return pans * tgamma(a);
// }

/*
    Complementary incomplete gamma function
*/
double F77_NAME(dgamic)(const double *, const double *);
double icgamma(const double a, const double x)
{
    double z;
    z = F77_NAME(dgamic)(&a, &x);
    return z;
}
// {
//     double pans, qans;
//     long ierr;
//     dgami(a, x, &pans, &qans, &ierr);
//     if (ierr != 0)
//     {
//         return -1;
//     }
//     return qans * tgamma(a);
// }



/*
    Compute integral_{-inf}^{A} x^k gaussDist(x,..) dx
*/
double cdfmomentgauss(const CLP_INT k, const double A, const double mu,
    const double sig)
{
    double z, Ascaled;
    Ascaled = sqrt05 * (A - mu) / sig;
    if (Ascaled >= 0.0)
    {
        z = -0.5*tgamma(0.5*((double)k+1.0)) + 0.5*igamma(0.5*((double)k+1.0), pow(Ascaled, 2.0));
    }
    else
    {
        z = -0.5*icgamma(0.5*((double)k+1.0), pow(Ascaled, 2.0));
    }
    return z;
}

/*
    Compute integral_{-inf}^{A} x^k ggamma(x,..) dx
*/
double cdfmomentggamma(const CLP_INT k, const double A, const double alpha,
    const double lmd, const double p)
{
    double z;
    z = igamma((alpha+(double)k)/p, pow(lmd*A, p)) / (pow(lmd, (double)k) *tgamma(alpha/p));
    return z;
}
