#include <R.h>
#include <Rinternals.h>
#include <R_ext/BLAS.h>
#include "clpmodel.h"
#include "clputil.h"


SEXP rsolve_GaussModel(SEXP d_, SEXP mu_, SEXP sig_, SEXP data_, SEXP chat_,
    SEXP verbose_, SEXP stepvec_)
{
    int d, n1, datalen, info, NSTEP=0, pcnt=0;
    double mu, sig, aic, accuracy;
    double *M1=NULL, *data=NULL, *chat=NULL, *coeff=NULL, *stepvec=NULL;
    bool verbose=false;
    SEXP coeff_, M1_, aic_, accuracy_, ans_;

    if (!isNull(chat_))
    {
        chat_ = PROTECT(coerceVector(chat_, REALSXP));
        pcnt++;
        chat = REAL(chat_);
    }
    if (isLogical(verbose_))
    {
        if (LOGICAL(verbose_)[0] != 0)
        {
            verbose=true;
        }
    }
    if (!isNull(stepvec_))
    {
        stepvec_ = PROTECT(coerceVector(stepvec_, REALSXP));
        pcnt++;
        NSTEP = length(stepvec_);
        stepvec = REAL(stepvec_);
    }

    d_ = PROTECT(coerceVector(d_, INTSXP));
    pcnt++;
    d = INTEGER(d_)[0];
    n1 = d/2+1;

    data_ = PROTECT(coerceVector(data_, REALSXP));
    pcnt++;
    datalen = length(data_);
    data = REAL(data_);

    mu_ = PROTECT(coerceVector(mu_, REALSXP));
    pcnt++;
    sig_ = PROTECT(coerceVector(sig_, REALSXP));
    pcnt++;
    mu = REAL(mu_)[0];
    sig = REAL(sig_)[0];

    M1_ = PROTECT(allocMatrix(REALSXP, n1, n1));
    pcnt++;
    M1 = REAL(M1_);

    info = solve_GaussModel(d, mu, sig, datalen, data, chat, verbose, M1,
                &aic, &accuracy, NSTEP, stepvec);
    // if (info != 0)
    // {
    //     UNPROTECT(pcnt);
    //     return R_NilValue;
    // }

    if (info == 0)
    {
        aic_ = PROTECT(allocVector(REALSXP, 1));
        pcnt++;
        accuracy_ = PROTECT(allocVector(REALSXP, 1));
        pcnt++;
        REAL(aic_)[0] = aic;
        REAL(accuracy_)[0] = accuracy;

        coeff_ = PROTECT(allocVector(REALSXP, d+1));
        pcnt++;
        coeff = REAL(coeff_);
        compute_coeff1(d, M1, coeff);
    }
    else
    {
        aic_ = PROTECT(allocVector(REALSXP, 1));
        pcnt++;
        accuracy_ = PROTECT(allocVector(REALSXP, 1));
        pcnt++;
        REAL(aic_)[0] = NA_REAL;
        REAL(accuracy_)[0] = NA_REAL;

        coeff_ = PROTECT(allocVector(REALSXP, 1));
        pcnt++;
        REAL(coeff_)[0] = NA_REAL;
    }

    ans_ = PROTECT(allocVector(VECSXP, 7));
    pcnt++;
    SET_VECTOR_ELT(ans_, 0, d_);
    SET_VECTOR_ELT(ans_, 1, mu_);
    SET_VECTOR_ELT(ans_, 2, sig_);
    SET_VECTOR_ELT(ans_, 3, aic_);
    SET_VECTOR_ELT(ans_, 4, accuracy_);
    SET_VECTOR_ELT(ans_, 5, coeff_);
    SET_VECTOR_ELT(ans_, 6 , M1_);
    
    UNPROTECT(pcnt);
    return ans_;
    
}

SEXP rsolve_ExpModel(SEXP d_, SEXP lmd_, SEXP data_, SEXP chat_, SEXP verbose_,
    SEXP stepvec_)
{
    int d, n1, n2, datalen, info, NSTEP=0, pcnt=0;
    double lmd, aic, accuracy;
    double *M1=NULL, *M2=NULL, *data=NULL, *chat=NULL, *coeff=NULL, *stepvec=NULL;
    bool verbose=false;
    SEXP M1_, M2_, aic_, accuracy_, ans_, coeff_;

    if (!isNull(chat_))
    {
        chat_ = PROTECT(coerceVector(chat_, REALSXP));
        pcnt++;
        chat = REAL(chat_);
    }
    if (isLogical(verbose_))
    {
        if (LOGICAL(verbose_)[0] != 0)
        {
            verbose=true;
        }
    }
    if (!isNull(stepvec_))
    {
        stepvec_ = PROTECT(coerceVector(stepvec_, REALSXP));
        pcnt++;
        NSTEP = length(stepvec_);
        stepvec = REAL(stepvec_);
    }

    d_ = PROTECT(coerceVector(d_, INTSXP));
    pcnt++;
    d = INTEGER(d_)[0];
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

    data_ = PROTECT(coerceVector(data_, REALSXP));
    pcnt++;
    datalen = length(data_);
    data = REAL(data_);

    lmd_ = PROTECT(coerceVector(lmd_, REALSXP));
    pcnt++;
    lmd = REAL(lmd_)[0];

    M1_ = PROTECT(allocMatrix(REALSXP, n1, n1));
    pcnt++;
    M1 = REAL(M1_);
    M2_ = PROTECT(allocMatrix(REALSXP, n2, n2));
    pcnt++;
    M2 = REAL(M2_);

    info = solve_ExpModel(d, lmd, datalen, data, chat, verbose, M1, M2,
                &aic, &accuracy, NSTEP, stepvec);
    // if (info != 0)
    // {
    //     UNPROTECT(pcnt);
    //     return R_NilValue;
    // }

    if (info == 0)
    {
        aic_ = PROTECT(allocVector(REALSXP, 1));
        pcnt++;
        accuracy_ = PROTECT(allocVector(REALSXP, 1));
        pcnt++;
        REAL(aic_)[0] = aic;
        REAL(accuracy_)[0] = accuracy;

        coeff_ = PROTECT(allocVector(REALSXP, d+1));
        pcnt++;
        coeff = REAL(coeff_);
        compute_coeff2(d, M1, M2, coeff);
    }
    else
    {
        aic_ = PROTECT(allocVector(REALSXP, 1));
        pcnt++;
        accuracy_ = PROTECT(allocVector(REALSXP, 1));
        pcnt++;
        REAL(aic_)[0] = NA_REAL;
        REAL(accuracy_)[0] = NA_REAL;

        coeff_ = PROTECT(allocVector(REALSXP, 1));
        pcnt++;
        REAL(coeff_)[0] = NA_REAL;
    }

    ans_ = PROTECT(allocVector(VECSXP, 7));
    pcnt++;
    SET_VECTOR_ELT(ans_, 0, d_);
    SET_VECTOR_ELT(ans_, 1, lmd_);
    SET_VECTOR_ELT(ans_, 2, aic_);
    SET_VECTOR_ELT(ans_, 3, accuracy_);
    SET_VECTOR_ELT(ans_, 4, coeff_);
    SET_VECTOR_ELT(ans_, 5, M1_);
    SET_VECTOR_ELT(ans_, 6, M2_);

    UNPROTECT(pcnt);
    return ans_;
}


SEXP rcmp_coeff1(SEXP d_, SEXP M1_)
{
    int d, pcnt=0;
    double *M1=NULL, *coeff=NULL;
    SEXP coeff_;
    
    d_ = PROTECT(coerceVector(d_, INTSXP));
    pcnt++;
    d = INTEGER(d_)[0];

    M1_ = PROTECT(coerceVector(M1_, REALSXP));
    pcnt++;
    M1 = REAL(M1_);

    coeff_ = PROTECT(allocVector(REALSXP, d+1));
    pcnt++;
    coeff = REAL(coeff_);

    compute_coeff1(d, M1, coeff);

    UNPROTECT(pcnt);
    return coeff_;
}

SEXP rcmp_coeff2(SEXP d_, SEXP M1_, SEXP M2_)
{
    int d, pcnt=0;
    double *M1=NULL, *M2=NULL,*coeff=NULL;
    SEXP coeff_;
    
    d_ = PROTECT(coerceVector(d_, INTSXP));
    pcnt++;
    d = INTEGER(d_)[0];

    M1_ = PROTECT(coerceVector(M1_, REALSXP));
    pcnt++;
    M1 = REAL(M1_);

    M2_ = PROTECT(coerceVector(M2_, REALSXP));
    pcnt++;
    M2 = REAL(M2_);

    coeff_ = PROTECT(allocVector(REALSXP, d+1));
    pcnt++;
    coeff = REAL(coeff_);

    compute_coeff2(d, M1, M2, coeff);

    UNPROTECT(pcnt);
    return coeff_;
}

SEXP reval_GaussModel(SEXP coeff_, SEXP mu_, SEXP sig_, SEXP data_)
{
    int d, datalen, pcnt=0;
    double *data=NULL, *coeff=NULL, *ydata=NULL;
    double mu, sig;
    SEXP ydata_;

    data_ = PROTECT(coerceVector(data_, REALSXP));
    pcnt++;
    datalen = length(data_);
    data = REAL(data_);

    coeff_ = PROTECT(coerceVector(coeff_, REALSXP));
    pcnt++;
    d = length(coeff_) - 1;
    coeff = REAL(coeff_);

    mu_ = PROTECT(coerceVector(mu_, REALSXP));
    pcnt++;
    mu = REAL(mu_)[0];

    sig_ = PROTECT(coerceVector(sig_, REALSXP));
    pcnt++;
    sig = REAL(sig_)[0];

    ydata_ = PROTECT(allocVector(REALSXP, datalen));
    pcnt++;
    ydata = REAL(ydata_);

    eval_poly(d, datalen, coeff, data, ydata);

    double a1 = 1.0/(sqrt(2.0*M_PI)*sig);
    for (int i=0; i<datalen; ++i)
    {
        ydata[i] = ydata[i] * a1 * exp(-0.5*pow((data[i]-mu)/sig, 2.0));
    }

    UNPROTECT(pcnt);
    return ydata_;
}

SEXP reval_ExpModel(SEXP coeff_, SEXP lmd_, SEXP data_)
{
    int d, datalen, pcnt=0;
    double *data=NULL, *coeff=NULL, *ydata=NULL;
    double lmd;
    SEXP ydata_;

    data_ = PROTECT(coerceVector(data_, REALSXP));
    pcnt++;
    datalen = length(data_);
    data = REAL(data_);

    coeff_ = PROTECT(coerceVector(coeff_, REALSXP));
    pcnt++;
    d = length(coeff_) - 1;
    coeff = REAL(coeff_);

    lmd_ = PROTECT(coerceVector(lmd_, REALSXP));
    pcnt++;
    lmd = REAL(lmd_)[0];

    ydata_ = PROTECT(allocVector(REALSXP, datalen));
    pcnt++;
    ydata = REAL(ydata_);

    eval_poly(d, datalen, coeff, data, ydata);

    for (int i=0; i<datalen; ++i)
    {
        if (data[i] >= 0.0)
        {
            ydata[i] = ydata[i] * lmd * exp(-lmd*data[i]);
        }
        else
        {
            ydata[i] = 0.0;
        }
        
    }

    UNPROTECT(pcnt);
    return ydata_;
}

SEXP reval_poly(SEXP coeff_, SEXP data_)
{
    int d, datalen, pcnt=0;
    double *data=NULL, *coeff=NULL, *ydata=NULL;
    SEXP ydata_;

    data_ = PROTECT(coerceVector(data_, REALSXP));
    pcnt++;
    datalen = length(data_);
    data = REAL(data_);

    coeff_ = PROTECT(coerceVector(coeff_, REALSXP));
    pcnt++;
    d = length(coeff_) - 1;
    coeff = REAL(coeff_);

    ydata_ = PROTECT(allocVector(REALSXP, datalen));
    pcnt++;
    ydata = REAL(ydata_);

    eval_poly(d, datalen, coeff, data, ydata);

    UNPROTECT(pcnt);
    return ydata_;
}

SEXP rpolyaxb(SEXP coeff_, SEXP c_, SEXP alpha_, SEXP beta_)
{
    int d, pcnt=0;
    double c, alpha, beta, *coeff=NULL, *coeff1=NULL;
    SEXP coeff1_;

    coeff_ = PROTECT(coerceVector(coeff_, REALSXP));
    pcnt++;
    d = length(coeff_) - 1;
    coeff = REAL(coeff_);

    c_ = PROTECT(coerceVector(c_, REALSXP));
    pcnt++;
    c = REAL(c_)[0];

    alpha_ = PROTECT(coerceVector(alpha_, REALSXP));
    pcnt++;
    alpha = REAL(alpha_)[0];

    beta_ = PROTECT(coerceVector(beta_, REALSXP));
    pcnt++;
    beta = REAL(beta_)[0];

    coeff1_ = PROTECT(allocVector(REALSXP, d+1));
    pcnt++;
    coeff1 = REAL(coeff1_);

    polyaxb(d, coeff, c, alpha, beta, coeff1);

    UNPROTECT(pcnt);
    return coeff1_;
}

SEXP rhistmean(SEXP data_, SEXP freq_)
{
    CLP_INT n, pcnt=0;
    double *data=NULL, *freq=NULL, z;
    SEXP ans_;

    if (!isNull(freq_))
    {
        freq_ = PROTECT(coerceVector(freq_, REALSXP));
        pcnt++;
        freq = REAL(freq_);
    }
    data_ = PROTECT(coerceVector(data_, REALSXP));
    pcnt++;
    n = length(data_);
    data = REAL(data_);

    z = histmean(n, data, freq);

    ans_ = PROTECT(allocVector(REALSXP, 1));
    pcnt++;
    REAL(ans_)[0] = z;

    UNPROTECT(pcnt);
    return ans_;
}

SEXP rdatastats(SEXP data_, SEXP freq_)
{
    CLP_INT n, pcnt=0;
    double *data=NULL, *freq=NULL, mu, sig;
    SEXP ans_, mu_, sig_;

    if (!isNull(freq_))
    {
        freq_ = PROTECT(coerceVector(freq_, REALSXP));
        pcnt++;
        freq = REAL(freq_);
    }
    data_ = PROTECT(coerceVector(data_, REALSXP));
    pcnt++;
    n = length(data_);
    data = REAL(data_);

    mu = histmean(n, data, freq);
    sig = histstd(n, mu, data, freq);

    mu_ = PROTECT(allocVector(REALSXP, 1));
    pcnt++;
    sig_ = PROTECT(allocVector(REALSXP, 1));
    pcnt++;
    REAL(mu_)[0] = mu;
    REAL(sig_)[0] = sig;
    ans_ = PROTECT(allocVector(VECSXP, 2));
    pcnt++;
    SET_VECTOR_ELT(ans_, 0, mu_);
    SET_VECTOR_ELT(ans_, 1, sig_);

    UNPROTECT(pcnt);
    return ans_;
}

SEXP rcdf_polygauss(SEXP coeff_, SEXP mu_, SEXP sig_, SEXP xv_)
{
    CLP_INT info=0;
    CLP_INT d, nlen, pcnt=0;
    double *coeff=NULL, *xv=NULL, *yv=NULL;
    double mu, sig;
    SEXP yv_;

    coeff_ = PROTECT(coerceVector(coeff_, REALSXP));
    pcnt++;
    d = length(coeff_) - 1;
    coeff = REAL(coeff_);

    xv_ = PROTECT(coerceVector(xv_, REALSXP));
    pcnt++;
    nlen = length(xv_);
    xv = REAL(xv_);

    mu_ = PROTECT(coerceVector(mu_, REALSXP));
    pcnt++;
    mu = REAL(mu_)[0];

    sig_ = PROTECT(coerceVector(sig_, REALSXP));
    pcnt++;
    sig = REAL(sig_)[0];

    yv_ = PROTECT(allocVector(REALSXP, nlen));
    pcnt++;
    yv = REAL(yv_);

    info = cdf_polygauss(d, nlen, coeff, mu, sig, xv, yv);
    if (info != 0)
    {
        UNPROTECT(pcnt);
        return R_NilValue;
    }

    UNPROTECT(pcnt);
    return yv_;
}

SEXP rcdf_polyggamma(SEXP coeff_, SEXP alpha_, SEXP lmd_, SEXP p_, SEXP xv_)
{
    CLP_INT info=0;
    CLP_INT d, nlen, pcnt=0;
    double *coeff=NULL, *xv=NULL, *yv=NULL;
    double alpha, lmd, p;
    SEXP yv_;

    coeff_ = PROTECT(coerceVector(coeff_, REALSXP));
    pcnt++;
    d = length(coeff_) - 1;
    coeff = REAL(coeff_);

    xv_ = PROTECT(coerceVector(xv_, REALSXP));
    pcnt++;
    nlen = length(xv_);
    xv = REAL(xv_);

    alpha_ = PROTECT(coerceVector(alpha_, REALSXP));
    pcnt++;
    alpha = REAL(alpha_)[0];

    lmd_ = PROTECT(coerceVector(lmd_, REALSXP));
    pcnt++;
    lmd = REAL(lmd_)[0];

    p_ = PROTECT(coerceVector(p_, REALSXP));
    pcnt++;
    p= REAL(p_)[0];

    yv_ = PROTECT(allocVector(REALSXP, nlen));
    pcnt++;
    yv = REAL(yv_);

    info = cdf_polyggamma(d, nlen, coeff, alpha, lmd, p, xv, yv);
    if (info != 0)
    {
        UNPROTECT(pcnt);
        return R_NilValue;
    }

    UNPROTECT(pcnt);
    return yv_;
}


SEXP rigamma(SEXP a_, SEXP x_)
{
    double *a, *x, *z;
    CLP_INT pcnt=0, na, nx;
    SEXP z_;

    a_ = PROTECT(coerceVector(a_, REALSXP));
    pcnt++;
    a = REAL(a_);
    na = length(a_);

    x_ = PROTECT(coerceVector(x_, REALSXP));
    pcnt++;
    x = REAL(x_);
    nx = length(x_);

    if (na != nx)
    {
        UNPROTECT(pcnt);
        return R_NilValue;
    }

    z_ = PROTECT(allocVector(REALSXP, na));
    pcnt++;
    z = REAL(z_);

    for (CLP_INT i=0; i<na; ++i)
    {
        if (a[i] <= 0.0)
        {
            z[i] = NAN;
        }
        else if ( x[i] < 0.0) 
        {
            z[i] = NAN;
        }
        else
        {
            z[i] = igamma(a[i], x[i]);
        }
    }

    UNPROTECT(pcnt);
    return z_;
}

SEXP ricgamma(SEXP a_, SEXP x_)
{
    double *a, *x, *z;
    CLP_INT pcnt=0, na, nx;
    SEXP z_;

    a_ = PROTECT(coerceVector(a_, REALSXP));
    pcnt++;
    a = REAL(a_);
    na = length(a_);

    x_ = PROTECT(coerceVector(x_, REALSXP));
    pcnt++;
    x = REAL(x_);
    nx = length(x_);

    if (na != nx)
    {
        UNPROTECT(pcnt);
        return R_NilValue;
    }

    z_ = PROTECT(allocVector(REALSXP, na));
    pcnt++;
    z = REAL(z_);

    for (CLP_INT i=0; i<na; ++i)
    {
        if (x[i] < 0.0)
        {
            z[i] = NAN;
        }
        else if ( x[i] == 0.0 && a[i] < 0.0) 
        {
            z[i] = NAN;
        }
        else
        {
            z[i] = icgamma(a[i], x[i]);
        }
    }

    UNPROTECT(pcnt);
    return z_;

}
