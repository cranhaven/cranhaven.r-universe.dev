#include "SFSI.h"
//#include "utils.c"


//====================================================================
// Recursive quantities for the GEMMA algorithm:
// a'Pib, a'PiPib, and a'PiPiPib, for any a and b
//====================================================================
double atPb(int i, int n, double *a, double *b, double *w, double *dbar){
  if(i == 0){  // atP0b - atP0w*btP0w/w1tP0w
    return(ddot3(n,a,dbar,b) - ddot3(n,a,dbar,w + n*i)*ddot3(n,b,dbar,w + n*i)/ddot3(n,w + n*i,dbar,w + n*i));

  }else{
    return(atPb(i-1,n,a,b,w,dbar) -
           atPb(i-1,n,a,w+n*i,w,dbar)*atPb(i-1,n,b,w+n*i,w,dbar)/atPb(i-1,n,w+n*i,w+n*i,w,dbar));
  }
}

//==========================================

double atPPb(int i, int n, double *a, double *b, double *w, double *dbar){
  double aPw, bPw, wPw;
  if(i == 0){  // atP0P0b + (atP0w)(btP0w)/(wtP0P0w)^2 + ...
    aPw = ddot3(n, a, dbar, w + n*i);
    bPw = ddot3(n, b, dbar, w + n*i);
    wPw = ddot3(n, w + n*i, dbar, w + n*i);
    return(ddot4(n,a,dbar,dbar,b) + aPw*bPw*ddot4(n,w + n*i,dbar,dbar,w + n*i)/pow(wPw,2) -
           aPw*ddot4(n,b,dbar,dbar,w + n*i)/wPw - bPw*ddot4(n,a,dbar,dbar,w + n*i)/wPw);

  }else{
    aPw = atPb(i-1,n,a,w+n*i,w,dbar);
    bPw = atPb(i-1,n,b,w+n*i,w,dbar);
    wPw = atPb(i-1,n,w+n*i,w+n*i,w,dbar);
    return(atPPb(i-1,n,a,b,w,dbar) + aPw*bPw*atPPb(i-1,n,w+n*i,w+n*i,w,dbar)/pow(wPw,2) -
           aPw*atPPb(i-1,n,b,w+n*i,w,dbar)/wPw - bPw*atPPb(i-1,n,a,w+n*i,w,dbar)/wPw);
  }
}

//==========================================

double atPPPb(int i, int n, double *a, double *b, double *w, double *dbar){
  double aPw, bPw, wPw, aPPw, bPPw, wPPw;
  if(i == 0){
    aPw = ddot3(n, a, dbar, w + n*i);
    bPw = ddot3(n, b, dbar, w + n*i);
    wPw = ddot3(n, w + n*i, dbar, w + n*i);
    aPPw = ddot4(n, a, dbar, dbar, w + n*i);
    bPPw = ddot4(n, b, dbar, dbar, w + n*i);
    wPPw = ddot4(n, w + n*i, dbar, dbar, w + n*i);
    return(ddot5(n,a,dbar,dbar,dbar,b) - aPw*bPw*pow(wPPw,2)/pow(wPw,3) -
           aPw*ddot5(n,b,dbar,dbar,dbar,w + n*i)/wPw - bPw*ddot5(n,a,dbar,dbar,dbar,w + n*i)/wPw -
           aPPw*bPPw/wPw + aPw*bPPw*wPPw/pow(wPw,2) + bPw*aPPw*wPPw/pow(wPw,2) +
           aPw*bPw*ddot5(n,w + n*i,dbar,dbar,dbar,w + n*i)/pow(wPw,2)
          );

  }else{
    aPw = atPb(i-1,n,a,w+n*i,w,dbar);
    bPw = atPb(i-1,n,b,w+n*i,w,dbar);
    wPw = atPb(i-1,n,w+n*i,w+n*i,w,dbar);
    aPPw = atPPb(i-1,n,a,w+n*i,w,dbar);
    bPPw = atPPb(i-1,n,b,w+n*i,w,dbar);
    wPPw = atPPb(i-1,n,w+n*i,w+n*i,w,dbar);
    return(atPPPb(i-1,n,a,b,w,dbar) - aPw*bPw*pow(wPPw,2)/pow(wPw,3) -
           aPw*atPPPb(i-1,n,b,w+n*i,w,dbar)/wPw - bPw*atPPPb(i-1,n,a,w+n*i,w,dbar)/wPw -
           aPPw*bPPw/wPw + aPw*bPPw*wPPw/pow(wPw,2) + bPw*aPPw*wPPw/pow(wPw,2) +
           aPw*bPw*atPPPb(i-1,n,w+n*i,w+n*i,w,dbar)/pow(wPw,2)
          );
  }
}

//==========================================

double tr_P(int i, int n, double *w, double *dbar){
  if(i == 0){  //  Tr_P0 - wtP0P0w/wP0w
    return(dsum(n,dbar) - ddot4(n,w + n*i,dbar,dbar,w + n*i)/ddot3(n,w + n*i,dbar,w + n*i));

  }else{
    return(tr_P(i-1,n,w,dbar) - atPPb(i-1,n,w+n*i,w+n*i,w,dbar)/atPb(i-1,n,w+n*i,w+n*i,w,dbar));
  }
}

//==========================================

double tr_PP(int i, int n, double *w, double *dbar){
  double wPw;
  if(i == 0){
    int inc1 = 1;
    wPw = ddot3(n, w + n*i, dbar, w + n*i);
    return(F77_NAME(ddot)(&n, dbar, &inc1, dbar, &inc1) +
           pow(ddot4(n,w + n*i,dbar,dbar,w + n*i),2)/pow(wPw,2) -
           2*ddot5(n,w + n*i,dbar,dbar,dbar,w + n*i)/wPw);

  }else{
    wPw = atPb(i-1,n,w+n*i,w+n*i,w,dbar);
    return(tr_PP(i-1,n,w,dbar) +
           pow(atPPb(i-1,n,w+n*i,w+n*i,w,dbar),2)/pow(wPw,2) -
           2*atPPPb(i-1,n,w+n*i,w+n*i,w,dbar)/wPw
          );
  }
}

//==========================================

double det_WtHinvW(int i, int n, double *w, double *dbar){
  // Obtained from Leibniz formula
  if(i == 0){  // det(W0'H^{-1}W0)*w1'P0w1 = w1'H^{-1}w1 = wPw
    return(ddot3(n, w + n*i, dbar, w + n*i)); // wPw

  }else{
    return(det_WtHinvW(i-1,n,w,dbar)*atPb(i-1,n,w+n*i,w+n*i,w,dbar));
  }
}

//====================================================================
// Log Likelihood (ML) and Restricted Log Likelihood (REML)
//====================================================================
double logLik(double ratio, int n, int p, int nPC, double *Uty, double *UtX,
              double *d, double pi, double *dbar)
{
  int k;
  double logdetH = 0;   // log(det(H)) = log(prod(ratio*d+1))

  for(k=0; k<nPC; k++){
    dbar[k] = 1/(ratio*d[k] + 1);    // dbar = 1 for small d
    logdetH += log(ratio*d[k] + 1);  // log(.) = 0 for small d
  }

  // Remove the factor 0.5: 0.5*n*log(0.5*n/pi) - 0.5*n - 0.5*logdetH - 0.5*n*log(ytPy)
  return(n*log(0.5*n/pi) - n - logdetH - n*log(atPb(p-1,n,Uty,Uty,UtX,dbar)));
}

//====================================================================
// Zhou & Stephen (2012) use degrees of freedom in REML equal to
// df = n-p-1 because they adjust single marker regression as well
// thus substracting 1 df. Here we do not adjust SMR so df = n-p

double logResLik(double ratio, int n, int p, int nPC, double *Uty, double *UtX,
                 double *d, double pi, double *dbar)
{
  int k;
  double logdetH = 0;   // log(det(H)) = log(prod(ratio*d+1))

  for(k=0; k<nPC; k++){
    dbar[k] = 1/(ratio*d[k] + 1);    // dbar = 1 for small d
    logdetH += log(ratio*d[k] + 1);  // log(.) = 0 for small d
  }

  // Remove the factor 0.5 and the term +log(detXtX)
  return((n-p)*log(0.5*(n-p)/pi) - (n-p) -
        logdetH - log(det_WtHinvW(p-1,n,UtX,dbar)) -
        (n-p)*log(atPb(p-1,n,Uty,Uty,UtX,dbar)));
}

//====================================================================
// First derivative of the Log Likelihood (ML)
//====================================================================
double dlogLik(double ratio, int n, int p, int nPC, double *Uty, double *UtX,
               double *d, double pi, double *dbar)
{
  int k;
  for(k=0; k<nPC; k++){
    dbar[k] = 1/(ratio*d[k] + 1);  // dbar = 1 for small d
  }

  double Tr_HinvG = (n - dsum(n,dbar))/ratio;  // sum(diag(Hinv%*%G)) = (n-sumd)/ratio
  double ytPy = atPb(p-1, n, Uty, Uty, UtX, dbar);
  double ytPGPy = (ytPy - atPPb(p-1,n,Uty,Uty,UtX,dbar))/ratio;   // y'Px G Px y = (ytPy-ytPPy)/ratio

  // Remove the factor 0.5: -0.5*Tr_HinvG+0.5*n*ytPGPy/ytPy
  return(-Tr_HinvG + n*ytPGPy/ytPy);
}

//====================================================================
// First derivative of the Log-restricted Likelihood (REML)
//====================================================================
double dlogResLik(double ratio, int n, int p, int nPC, double *Uty, double *UtX,
                  double *d, double pi, double *dbar)
{
  int k;

  for(k=0; k<nPC; k++){
    dbar[k] = 1/(ratio*d[k] + 1);  // dbar = 1 for small d
  }

  double Tr_PG = (n-p-tr_P(p-1,n,UtX,dbar))/ratio;  // sum(diag(Px%*%G))
  double ytPy = atPb(p-1, n, Uty, Uty, UtX, dbar);  // t(y)%*%Px%*%y
  double ytPGPy = (ytPy - atPPb(p-1,n,Uty,Uty,UtX,dbar))/ratio;  // y' Px G Px y = (ytPy-ytPPy)/ratio

  // Remove the factor 0.5: -0.5*Tr_PG+0.5*(n-p)*ytPGPy/ytPy
  return(-Tr_PG + (n-p)*ytPGPy/ytPy);
}

//====================================================================
// TRUE if x1*x2 negative
int RootBracketed(double x1,double x2) {
  //int result;
  if((sign(x1)*sign(x2)) <= 0){
    return(1);
  }else{
    return(0);
  }
}

// Define fun and dfun that will be later either REML or ML
double (*fun)(double, int, int, int, double*, double*, double*, double, double*) = NULL;
double (*dfun)(double, int, int, int, double*, double*,double*, double, double*) = NULL;

/*******************************************************
*              Brent Method Function                   *
* ---------------------------------------------------- *
* Reference:  BORLAND MATHEMATICAL LIBRARY             *
*                                                      *
*                C++ version by J-P Moreau, Paris.     *
*                       (www.jpmoreau.fr)              *
* ---------------------------------------------------- *
* The purpose is to find a real root of a real         *
* function f(x) using Brent's method.                  *
*                                                      *
* INPUTS:  x1,x2     : interval of root                *
*          Tolerance : desired accuracy for root       *
*          maxIter   : maximum number of iterations    *
*                                                      *
* OUTPUTS: The function returns the root value         *
*          ValueAtRoot : value of f(root)              *
*          niter    : number of done iterations        *
*          info     : =0, all OK                       *
*                   : =1, no root found in interval    *
*                   : =2, no more iterations !         *
*******************************************************/
double BrentRoots(double x1, double x2,
                  double Tolerance, int maxIterations,
                  double *valueAtRoot, int *niter, int *info,
                  int n, int p, int nPC, double *Uty, double *UtX,
                  double *d, double pi, double *dbar){

  double FPP = DBL_EPSILON; // 1e-11;
  double nearzero = DBL_EPSILON/10; //1e-20;
  double result, AA, BB, CC, DD, EE, FA, FB, FC, Tol1, PP, QQ, RR, SS, xm;
  int i, done;

  i = 0;
  done = 0;
  info[0] = 0;
  result = NA_REAL;

  AA = x1;
  BB = x2;
  FA = dfun(AA, n, p, nPC, Uty, UtX, d, pi, dbar);
  FB = dfun(BB, n, p, nPC, Uty, UtX, d, pi, dbar);

  // Initialize these as lines below might not be
  CC = AA; FC = FA; DD = BB - AA; EE = DD;

  if(RootBracketed(FA,FB)){
    FC = FB;
    do{
      if(!(RootBracketed(FC,FB))){
        CC = AA; FC = FA; DD = BB - AA; EE = DD;
      }
      if(fabs(FC) < fabs(FB)) {
        AA = BB; BB = CC; CC = AA;
        FA = FB; FB = FC; FC = FA;
      }
      Tol1 = 2.0 * FPP * fabs(BB) + 0.5 * Tolerance;
      xm = 0.5 * (CC-BB);
      if((fabs(xm) <= Tol1) || (fabs(FA) < nearzero)){
        result = BB;
        done = 1;
        valueAtRoot[0] = dfun(result, n, p, nPC, Uty, UtX, d, pi, dbar);
        //Rprintf("A root has been found\n");
      }else{
        if((fabs(EE) >= Tol1) && (fabs(FA) > fabs(FB))){
          SS = FB/ FA;
          if(fabs(AA - CC) < nearzero){
            PP = 2.0 * xm * SS;
            QQ = 1.0 - SS;
          }else{
            QQ = FA/FC;
            RR = FB /FC;
            PP = SS * (2.0 * xm * QQ * (QQ - RR) - (BB-AA) * (RR - 1.0));
            QQ = (QQ - 1.0) * (RR - 1.0) * (SS - 1.0);
          }
          if(PP > nearzero) QQ = -QQ;
          PP = fabs(PP);
          if((2.0 * PP) < fmin2(3.0*xm *QQ-fabs(Tol1 * QQ), fabs(EE * QQ))){
            EE = DD;  DD = PP/QQ;
          }else{
            DD = xm;   EE = DD;
          }
        }else{
          DD = xm;
          EE = DD;
        }
        AA = BB;
        FA = FB;
        if(fabs(DD) > Tol1){
          BB = BB + DD;
        }else{
          if(xm > 0){
             BB = BB + fabs(Tol1);
          }else{
             BB = BB - fabs(Tol1);
          }
        }
        FB = dfun(BB, n, p, nPC, Uty, UtX, d, pi, dbar);
        i++;
      }
	  }
    while((!done) && (i < maxIterations));
    if(i >= maxIterations) info[0] = 2;
  }else{
    //Rprintf("Values f(x) at bounds (%f,%f) are NOT of opposite signs\n",x1,x2);
    info[0] = 1;
  }
  niter[0] = i;
  return result;
}

SEXP R_solve_mixed(SEXP N_, SEXP n_, SEXP nPC_, SEXP ratio_,
                   SEXP Uty_, SEXP UtX_,
                   SEXP d_, SEXP bounds_, SEXP tol_,
                   SEXP maxiter_, SEXP isREML_, SEXP BLUE_)
{
    int j, intvalue;
    double one = 1;
    int nprotect = 4;

    double tol = NUMERIC_VALUE(tol_);
    int maxiter = INTEGER_VALUE(maxiter_);
    int N = INTEGER_VALUE(N_);
    int n = INTEGER_VALUE(n_);
    int nPC = INTEGER_VALUE(nPC_);
    int p = Rf_ncols(UtX_);
    int isREML = asLogical(isREML_);
    int BLUE = asLogical(BLUE_);
    int nintervals = Rf_length(bounds_)-1;

    PROTECT(Uty_ = AS_NUMERIC(Uty_));
    double *Uty = NUMERIC_POINTER(Uty_);

    PROTECT(UtX_ = AS_NUMERIC(UtX_));
    double *UtX = NUMERIC_POINTER(UtX_);

    PROTECT(d_ = AS_NUMERIC(d_));
    double *d = NUMERIC_POINTER(d_);

    PROTECT(bounds_ = AS_NUMERIC(bounds_));
    double *bounds = NUMERIC_POINTER(bounds_);

    int convergence = NA_INTEGER;
    int status = NA_INTEGER;
    double pi = M_PI, eps = DBL_EPSILON;

    intvalue = N>(p*p) ? N : p*p;
    double *dbar = (double *) R_alloc(n, sizeof(double));
    double *work = (double *) R_alloc(intvalue, sizeof(double));
    double *tmp1 = (double *) R_alloc(intvalue, sizeof(double));
    double *tmp2 = (double *) R_alloc(intvalue, sizeof(double));
    double *tmp3 = (double *) R_alloc(intvalue, sizeof(double));

    for(j=0; j<n; j++){
      dbar[j] = 1;
    }

    double ratio = NA_REAL;
    if(Rf_isNull(ratio_)){ // Perform likelihood maximization
      // Define function for REML or ML
      if(isREML){
        fun = &logResLik;
        dfun = &dlogResLik;
      }else{
        fun = &logLik;
        dfun = &dlogLik;
      }

      double x1, x2, fx1, fx2, slope, dfx1, dfx2, value;
      int niter;
      convergence = 0;

      // Evaluate the likelihood at the bounds and get slope
      x1 = bounds[0];
      x2 = bounds[nintervals];
      fx1 = fun(x1, n, p, nPC, Uty, UtX, d, pi, dbar);
      fx2 = fun(x2, n, p, nPC, Uty, UtX, d, pi, dbar);
      slope = (fx2-fx1)/(x2-x1);

      //Rprintf(" Starting searching of optimal value ...\n");
      if(fabs(slope) < eps){
        ratio = x1;
        status = 1;
      }else{
        //Rprintf("Starting Brent's algorithm (within interval)...\n");
        status = 0;
        intvalue = 1;
        for(j=0; j<nintervals; j++){
          dfx1 = dfun(bounds[j], n, p, nPC, Uty, UtX, d, pi, dbar);
          dfx2 = dfun(bounds[j+1], n, p, nPC, Uty, UtX, d, pi, dbar);

          if(sign(dfx1)*sign(dfx2) <= 0){
            ratio = BrentRoots(bounds[j], bounds[j+1], tol, maxiter,
                               &value, &niter, &intvalue,
                               n, p, nPC, Uty, UtX, d, pi, dbar);
            if(intvalue != 1){ // a root is found if intvalue = 0 or 2
              break;
            }
          }
        }

        if(intvalue == 1){
          //Rprintf("No solution was found in the interval\n");
          //Rprintf("The coordinate bound yielding the larger likelihood is returned\n");
          if(fx1 > fx2){
            ratio = x1;
            status = 3;
          }else{
            ratio = x2;
            status = 4;
          }
          convergence = 0;
        }else{
          value = fun(ratio, n, p, nPC, Uty, UtX, d, pi, dbar); // likelihood at root
          if(value < fmax2(fx1, fx2)){  // Degenerated solution
            //Rprintf("A degenerated solution was found: x=%f and f(x)=%f\n",ratio,value);
            if(fx1 > fx2){
              ratio = x1;
              status = 3;
            }else{
              ratio = x2;
              status = 4;
            }
            convergence = 0;
          }else{
            //Rprintf("A good solution was found: x=%f and f(x)=%f\n",ratio,value);
            convergence = (intvalue ==  0 ? 1 : 0);
            if(!convergence){
              status = 2;
            }
          }
        }
      }
    }else{
      //Rprintf("No likelihood optimization was performed\n");
      ratio = NUMERIC_VALUE(ratio_);
    }

    // Variance components and fixed effects
    for(j=0; j<nPC; j++){
      dbar[j] = 1/(ratio*d[j] + 1);
    }

    SEXP bHat_;
    if(BLUE){
      //Rprintf(" Calculating fixed effects b ...\n");
      bHat_ = PROTECT(Rf_allocVector(REALSXP, p));
      double *bHat = NUMERIC_POINTER(bHat_);
      nprotect++;

      // b = solve(UtX' dbar UtX) t(Uty*dbar) UtX
      for(j=0; j<n; j++){
        tmp1[j] = Uty[j]*dbar[j];
      }
      crossproduct(n,1,p,tmp1,UtX,tmp2);   // tmp2 = t(tmp1)%*%UtX
      crossproduct_scale(n,p,p,UtX,dbar,UtX,tmp1,work); // tmp1 = UtX' dbar UtX

      intvalue = 0;
      invert_matrix(p,tmp1,tmp3,&eps,work,&intvalue);     // tmp3 = solve(tmp1)
      if(intvalue != 0){
        UNPROTECT(nprotect);
        return(ScalarInteger(5));  // Status 5: Matrix X is not full rank
      }

      matrix_vector_product(p,p,&one,tmp3,tmp2,1,bHat,0); // b = tmp3*tmp2
    }else{
      bHat_ = R_NilValue;
    }

    double varE = atPb(p-1,n,Uty,Uty,UtX,dbar)/(isREML ? n-p : n);
    double varU = ratio*varE;
    double h2 = varU/(varU + varE);

    SEXP list_ = PROTECT(Rf_allocVector(VECSXP, 7));
    SET_VECTOR_ELT(list_, 0, ScalarReal(ratio));
    SET_VECTOR_ELT(list_, 1, ScalarInteger(convergence));
    SET_VECTOR_ELT(list_, 2, bHat_);
    SET_VECTOR_ELT(list_, 3, ScalarReal(varU));
    SET_VECTOR_ELT(list_, 4, ScalarReal(varE));
    SET_VECTOR_ELT(list_, 5, ScalarReal(h2));
    SET_VECTOR_ELT(list_, 6, ScalarInteger(status));

    // Set dimnames for outputs
    SEXP names_ = PROTECT(Rf_allocVector(VECSXP, 7));
    SET_VECTOR_ELT(names_, 0, mkChar("ratio"));
    SET_VECTOR_ELT(names_, 1, mkChar("convergence"));
    SET_VECTOR_ELT(names_, 2, mkChar("b"));
    SET_VECTOR_ELT(names_, 3, mkChar("varU"));
    SET_VECTOR_ELT(names_, 4, mkChar("varE"));
    SET_VECTOR_ELT(names_, 5, mkChar("h2"));
    SET_VECTOR_ELT(names_, 6, mkChar("status"));
    Rf_setAttrib(list_, R_NamesSymbol, names_);
    nprotect += 2;

    UNPROTECT(nprotect);

    return(list_);
}
