#include <R.h>
#include <vector>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <Rmath.h>
#include <limits>



static void chkIntFn(void *dummy) {
  R_CheckUserInterrupt();
}

// this will call the above in a top-level context so it won't longjmp-out of your context
bool checkInterrupt() {
  return (R_ToplevelExec(chkIntFn, NULL) == FALSE);
}

double stdnormal_inv(double p)
{
    const double a[6] = {
        -3.969683028665376e+01,  2.209460984245205e+02,
        -2.759285104469687e+02,  1.383577518672690e+02,
        -3.066479806614716e+01,  2.506628277459239e+00
    };
    const double b[5] = {
        -5.447609879822406e+01,  1.615858368580409e+02,
        -1.556989798598866e+02,  6.680131188771972e+01,
        -1.328068155288572e+01
    };
    const double c[6] = {
        -7.784894002430293e-03, -3.223964580411365e-01,
        -2.400758277161838e+00, -2.549732539343734e+00,
        4.374664141464968e+00,  2.938163982698783e+00
    };
    const double d[4] = {
        7.784695709041462e-03,  3.224671290700398e-01,
        2.445134137142996e+00,  3.754408661907416e+00
    };

    double q = 0;
    double t = 0;
    double u = 0;

    q = std::min(p,1-p);
    if(q == 0){
        u = -std::numeric_limits<double>::infinity();
    } else {
    if (q > 0.02425) {
        /* Rational approximation for central region. */
        u = q-0.5;
        t = u*u;
        u = u*(((((a[0]*t+a[1])*t+a[2])*t+a[3])*t+a[4])*t+a[5])
        /(((((b[0]*t+b[1])*t+b[2])*t+b[3])*t+b[4])*t+1);
    } else {
        /* Rational approximation for tail region. */
        t = sqrt(-2*log(q));
        u = (((((c[0]*t+c[1])*t+c[2])*t+c[3])*t+c[4])*t+c[5])
        /((((d[0]*t+d[1])*t+d[2])*t+d[3])*t+1);
    }
    /* The relative error of the approximation has absolute value less
     than 1.15e-9.  One iteration of Halley rational method (third
     order) gives full machine precision... */
    //t = 0.5 + 0.5 * erf_R(u / sqrt(2.0)) - q;    /* error */
    t = pnorm(u, 0, 1, 1, 0) - q;    /* error */
    t = t * sqrt(2 * M_PI) * exp(u*u/2);   /* f(u)/df(u) */
    u = u-t/(1+u*t/2);     /* Halleys method */
    }

    return (p > 0.5 ? -u : u);
};

void pointEstimate(int j,
                   int *d,
                   const double *generatingVector,
                   const double *randomShift,
                   const double *b,
                   const double *L,
                   double *est)
{   double value = 0;
    double* x;
    x = (double*) Calloc(*d, double);

    for (int k = 0; k < *d; k++) {
        x[k] = std::abs(2 * ((generatingVector[k] * j +  randomShift[k])  - floor(generatingVector[k] * j +  randomShift[k])) - 1);

    }

    double* e;
    e = (double*) Calloc(*d, double);
    double* y;
    y = (double*) Calloc(*d, double);

    //e[0] = 0.5 + 0.5 * erf_R((b)[0] / ((L)[0]*sqrt(2.0)));
    e[0] = pnorm(((b)[0] / (L)[0]), 0, 1, 1, 0);
    value = e[0] ;

    for (int k = 1; k < *d; k++) {

        y[k-1] = stdnormal_inv(e[k - 1] * x[k - 1]);

        //DEAL WITH INFINTE BOUNDS
        if(!(R_FINITE(y[k-1]))) {
            if(y[k-1] > 0){
                value = 1;
            } else {
                value = 0;
            }
            break;
        }

        double tmp = 0;
        for (int l = 0; l < k; l++){
            tmp += (L)[k * (*d) + l] * y[l];
        }

        //e[k] = 0.5 + 0.5 * erf_R(((b)[k] - tmp) / ((L)[k * (*d) + k] * sqrt(2.0)));
        e[k] = pnorm(((b)[k] - tmp) / ((L)[k * (*d) + k]), 0, 1, 1, 0);
        value = value * e[k];

        }
    *est += value;

    Free(x);
    Free(e);
    Free(y);
}


void pointEstimateAT(int j,
                     int *d,
                     const double *generatingVector,
                     const double *randomShift,
                     const double *b,
                     const double *L,
                     double *est)
{   
  double value1 = 0;
  double value2 = 0;
  double* x1;
  x1 = (double*) Calloc(*d, double);
  double* x2;
  x2 = (double*) Calloc(*d, double);
  
  for (int k = 0; k < *d; k++) {
    x1[k] = std::abs(2 * ((generatingVector[k] * j +  randomShift[k])  - floor(generatingVector[k] * j +  randomShift[k])) - 1);
    x2[k] = 1 - x1[k];
  }
  
  double* e1;
  e1 = (double*) Calloc(*d, double);
  double* y1;
  y1 = (double*) Calloc(*d, double);
  
  double* e2;
  e2 = (double*) Calloc(*d, double);
  double* y2;
  y2 = (double*) Calloc(*d, double);
  
  //e[0] = 0.5 + 0.5 * erf_R((b)[0] / ((L)[0]*sqrt(2.0)));
  e1[0] = pnorm(((b)[0] / (L)[0]), 0, 1, 1, 0);
  value1 = e1[0] ;
  e2[0] =  e1[0] ;
  value2 = e2[0];
  
  for (int k = 1; k < *d; k++) {
    
    y1[k-1] = stdnormal_inv(e1[k - 1] * x1[k - 1]);
    y2[k-1] = stdnormal_inv(e2[k - 1] * x2[k - 1]);
    //DEAL WITH INFINTE BOUNDS
    if(!(R_FINITE(y1[k-1]))) {
      if(y1[k-1] > 0){
        value1 = 1;
      } else {
        value1 = 0;
      }
      break;
    }
    if(!(R_FINITE(y2[k-1]))) {
      if(y2[k-1] > 0){
        value2 = 1;
      } else {
        value2 = 0;
      }
      break;
    }
    double tmp1 = 0;
    double tmp2 = 0;
    for (int l = 0; l < k; l++){
      tmp1 += (L)[k * (*d) + l] * y1[l];
      tmp2 += (L)[k * (*d) + l] * y2[l];
    }
    
    //e[k] = 0.5 + 0.5 * erf_R(((b)[k] - tmp) / ((L)[k * (*d) + k] * sqrt(2.0)));
    e1[k] = pnorm(((b)[k] - tmp1) / ((L)[k * (*d) + k]), 0, 1, 1, 0);
    e2[k] = pnorm(((b)[k] - tmp2) / ((L)[k * (*d) + k]), 0, 1, 1, 0);
    value1 = value1 * e1[k];
    value2 = value2 * e2[k];
    
  }
  *est += value1;
  *est += value2;
  
  Free(x1);
  Free(e1);
  Free(y1);
  Free(x2);
  Free(e2);
  Free(y2);
}

extern "C" void mvtNormCpp(int *tmp_xn, int *tmp_d, double *tmp_mat, double *tmp_b, double *tmp_generatingVector, 
                          int *tmp_nrep, int *antithetic, double *est, double *err) {

    //double diff = 0, p = 0, error = 0;
    //double pi = 3.141592653589793;



    std::vector< double > b(*tmp_d);
    std::vector< double > generatingVector(*tmp_d);
    std::vector< std::vector<double> > mat(*tmp_d, std::vector<double>(*tmp_d));
    for(int i = 0; i < *tmp_d; i++){
        b[i] = tmp_b[i];
        generatingVector[i] = tmp_generatingVector[i];
        for(int j = 0; j < *tmp_d; j++){
            mat[i][j] = tmp_mat[i * *tmp_d + j];
        }
    }


    //DATA PREPARATION
    for(int i = 0; i < *tmp_d; i++){
        double t = std::sqrt(mat[i][i]);
        b[i] = b[i] / t;
        for( int j = 0; j < *tmp_d; j++){
            mat[i][j] = mat[i][j] / t;
            mat[j][i] = mat[j][i] / t;
        }
    }

    //PARAMETERS FOR VARIABLE REORDERING
    std::vector< double > y(*tmp_d);
    std::vector< std::vector<double> > L(*tmp_d, std::vector<double>(*tmp_d));
    int pos = 0;
    double min = LONG_MAX;

    //PARAMETERS FOR INTEGRATION
    // int nRep = 10;
    double diff = 0, p = 0, error = 0;


    //--------------------------------------------------- VARIABLE REORDERING --------------------------------------------
    //FIND THE FIRST POSITION
    for (int itPos = 0; itPos < *tmp_d; ++itPos){
        double b_phi = exp(-pow(b[itPos],2)/2)/ sqrt(2 * M_PI);
        //double b_Phi = 0.5 + 0.5 * erf_R(b[itPos] / sqrt(2.0));
        double b_Phi = pnorm(b[itPos], 0, 1, 1, 0);
        double v = 1 - b[itPos] * b_phi / b_Phi - pow(b_phi/b_Phi,2);
        if(v < min) {
            min = v;
            pos = itPos;
            y[0] = - b_phi / b_Phi;
        }
    }

    //SWITCH POSITIONS 0 and in upper bound
    std::swap(b[0], b[pos]);

    //SWITCH ROWS AND COLUMNS
    std::swap(mat[0], mat[pos]);
    for(int i = 0; i < *tmp_d; ++i){
        std::swap(mat[i][0], mat[i][pos]);
    }

    //COMPUTE CHOLESKY
    L[0][0] = std::sqrt(mat[0][0]);
    for(int i = 1 ; i < *tmp_d ; i++) L[i][0] = mat[i][0]/L[0][0];



    //ITERATE THE PROCESS
    for (int rec = 1; rec < *tmp_d; rec ++){
        min = LONG_MAX;
        pos = rec;
        //------------------COMPUTE CANDIDATE FOR NEW UPPER BOUND
        std::vector< double > b_New_Temp(*tmp_d - rec);
        int count = 0;
        for (int itPos = rec; itPos < *tmp_d; ++itPos){
            double conditionnalSum = 0;
            double squaredSum = 0;
            for(int i = 0; i < rec; i++) {
                conditionnalSum += L[itPos][i] * y[i];
                squaredSum += L[itPos][i] * L[itPos][i];
            }
            b_New_Temp[count] = (b[itPos] - conditionnalSum) / sqrt(mat[itPos][itPos] - squaredSum);
            count++;
        }

        //FIND THE MINIMUM
        for (int itPos = 0; itPos < *tmp_d - rec; ++itPos){
            double b_phi = exp(-pow(b_New_Temp[itPos],2)/2)/ sqrt(2 * M_PI);
            //double b_Phi = 0.5 + 0.5 * erf_R(b_New_Temp[itPos] / sqrt(2.0));
            double b_Phi = pnorm(b_New_Temp[itPos], 0, 1, 1, 0);
            double v = 1 - b_New_Temp[itPos] * b_phi / b_Phi - pow(b_phi/b_Phi,2);
            if(v < min) {
                min = v;
                pos = itPos + rec;
                y[rec] = - b_phi / b_Phi;
            }
        }

        //------------------SWITCH POSITIONS
        //SWITCH POSITIONS 0 and in upper bound
        std::swap(b[rec], b[pos]);

        //SWITCH ROWS AND COLUMNS
        std::swap(mat[rec], mat[pos]);
        for(int i = 0; i < *tmp_d; ++i){
            std::swap(mat[i][rec], mat[i][pos]);
        }
        std::swap(L[rec], L[pos]);

        //----------------COMPUTE CHOLESKY TERMS
        double rowSum = 0;
        for(int j = 0; j < rec; j++) rowSum += L[rec][j] * L[rec][j];
        L[rec][rec] = std::sqrt(mat[rec][rec] - rowSum);
        for(int i = rec + 1 ; i < *tmp_d ; i++) {
            double lProd = 0;
            for(int k = 0; k < rec; k++) lProd += L[rec][k] * L[i][k];
            L[i][rec] = (mat[i][rec] - lProd) / L[rec][rec];
        }
    }

    //--------------------------------------------------- INTEGRATION ROUTINE --------------------------------------------
    std::vector< double > vecL;
    for (int i = 0;i < *tmp_d; i++) {
        vecL.insert(vecL.end(), L[i].begin(), L[i].end());
    }

    int *h_d = tmp_d;
    double *h_generatingVector = tmp_generatingVector, *h_randomShift = (double*) R_alloc(*tmp_d, sizeof(double)), *h_b = &b[0], *h_L =&vecL[0], *h_est = est;
    
    GetRNGstate();

    for (int i = 0; i < *tmp_nrep; i++) {


          for (int k = 0; k < *tmp_d; k++){
              //h_randomShift[k] = ((double) rand() / (RAND_MAX));
              h_randomShift[k] = unif_rand();
          }
          *h_est = 0;
          if(*antithetic == 1){
           for (int j = 0; j < *tmp_xn; j++) {
              pointEstimateAT(j, h_d, h_generatingVector, h_randomShift, h_b, h_L, est);
            }
           diff = ((*h_est / (2* *tmp_xn)) - p) / (i + 1);
          } else{
            for (int j = 0; j < *tmp_xn; j++) {
              pointEstimate(j, h_d, h_generatingVector, h_randomShift, h_b, h_L, h_est);
            }
            diff = ((*h_est / *tmp_xn) - p) / (i + 1);
          }
            p += diff;
            error = ( i - 1 ) * error / (i + 1) + pow(diff,2);
            if (checkInterrupt())  break;
        }

    PutRNGstate();

    error = 3 * sqrt(error);


    *est = p;
    *err = error;
}

void pointEstimateTProb(int j,
                        int *d,
                        const double *generatingVector,
                        const double *randomShift,
                        const double *b_org,
                        const double *L,
                        double *est,
                        double *nu)
{   double value = 0;
    double* x;
    x = (double*) Calloc(*d, double);
    
    for (int k = 0; k < *d; k++) {
        x[k] = std::abs(2 * ((generatingVector[k] * j +  randomShift[k])  - floor(generatingVector[k] * j +  randomShift[k])) - 1);
        
    }
    
    double* e;
    e = (double*) Calloc(*d, double);
    double* y;
    y = (double*) Calloc(*d, double);
    double* b;
    b = (double*) Calloc(*d, double);
    
    double scaling = 0;
    scaling = sqrt(2 * qgamma(x[*d - 1], *nu / 2, 1, 1, 0));
    
    b[0] = scaling * b_org[0];
    //b[0] = b_org[0];
    
    //e[0] = 0.5 + 0.5 * erf_R((b)[0] / ((L)[0]*sqrt(2.0)));
    e[0] = pnorm(((b)[0] / (L)[0]), 0, 1, 1, 0);
    value = e[0] ;
    
    for (int k = 1; k < *d; k++) {
        
        b[k] = scaling * b_org[k];
        //b[k] = b_org[k];
        
        y[k-1] = stdnormal_inv(e[k - 1] * x[k - 1]);
        
        //DEAL WITH INFINITE BOUNDS
        if(!(R_FINITE(y[k-1]))) {
            if(y[k-1] > 0){
                value = 1;
            } else {
                value = 0;
            }
            break;
        }
        
        double tmp = 0;
        for (int l = 0; l < k; l++){
            tmp += (L)[k * (*d) + l] * y[l];
        }
        
        //e[k] = 0.5 + 0.5 * erf_R(((b)[k] - tmp) / ((L)[k * (*d) + k] * sqrt(2.0)));
        e[k] = pnorm(((b)[k] - tmp) / ((L)[k * (*d) + k]), 0, 1, 1, 0);
        value = value * e[k];
        
    }
    *est += value;
    
    Free(x);
    Free(e);
    Free(y);
    Free(b);
}

void pointEstimateTProbAT(int j,
                        int *d,
                        const double *generatingVector,
                        const double *randomShift,
                        const double *b_org,
                        const double *L,
                        double *est,
                        double *nu)
{   
  double value1 = 0;
  double value2 = 0;
  double* x1;
  double* x2;
  x1 = (double*) Calloc(*d, double);
  x2 = (double*) Calloc(*d, double);
  
  for (int k = 0; k < *d; k++) {
    x1[k] = std::abs(2 * ((generatingVector[k] * j +  randomShift[k])  - floor(generatingVector[k] * j +  randomShift[k])) - 1);
    x2[k] = 1 - x1[k];
  }
  
  double* e1;
  e1 = (double*) Calloc(*d, double);
  double* y1;
  y1 = (double*) Calloc(*d, double);
  double* b1;
  b1 = (double*) Calloc(*d, double);
  double* e2;
  e2 = (double*) Calloc(*d, double);
  double* y2;
  y2 = (double*) Calloc(*d, double);
  double* b2;
  b2 = (double*) Calloc(*d, double);
  double scaling1 = 0;
  double scaling2 = 0;
  
  scaling1 = sqrt(2 * qgamma(x1[*d - 1], *nu / 2, 1, 1, 0));
  scaling2 = sqrt(2 * qgamma(x2[*d - 1], *nu / 2, 1, 1, 0));
  
  b1[0] = scaling1 * b_org[0];
  b2[0] = scaling2 * b_org[0];
  //b[0] = b_org[0];
  
  //e[0] = 0.5 + 0.5 * erf_R((b)[0] / ((L)[0]*sqrt(2.0)));
  e1[0] = pnorm(((b1)[0] / (L)[0]), 0, 1, 1, 0);
  e2[0] = pnorm(((b2)[0] / (L)[0]), 0, 1, 1, 0);
  value1 = e1[0];
  value2 = e2[0];
  
  for (int k = 1; k < *d; k++) {
    
    b1[k] = scaling1 * b_org[k];
    b2[k] = scaling2 * b_org[k];
    //b[k] = b_org[k];
    
    y1[k-1] = stdnormal_inv(e1[k - 1] * x1[k - 1]);
    y2[k-1] = stdnormal_inv(e2[k - 1] * x2[k - 1]);
    
    //DEAL WITH INFINITE BOUNDS
    if(!(R_FINITE(y1[k-1]))) {
      if(y1[k-1] > 0){
        value1 = 1;
      } else {
        value1 = 0;
      }
      break;
    }
    if(!(R_FINITE(y2[k-1]))) {
      if(y2[k-1] > 0){
        value2 = 1;
      } else {
        value2 = 0;
      }
      break;
    }
    double tmp1 = 0;
    double tmp2 = 0;
    for (int l = 0; l < k; l++){
      tmp1 += (L)[k * (*d) + l] * y1[l];
      tmp2 += (L)[k * (*d) + l] * y2[l];
    }
    
    //e[k] = 0.5 + 0.5 * erf_R(((b)[k] - tmp) / ((L)[k * (*d) + k] * sqrt(2.0)));
    e1[k] = pnorm(((b1)[k] - tmp1) / ((L)[k * (*d) + k]), 0, 1, 1, 0);
    e2[k] = pnorm(((b2)[k] - tmp2) / ((L)[k * (*d) + k]), 0, 1, 1, 0);
    value1 = value1 * e1[k];
    value2 = value2 * e2[k];
    
  }
  *est += value1;
  *est += value2;
  
  Free(x1);
  Free(e1);
  Free(y1);
  Free(b1);
  Free(x2);
  Free(e2);
  Free(y2);
  Free(b2);
}

extern "C" void mvTProbCpp(int *tmp_xn, int *tmp_d,  double *tmp_mat, double *tmp_b, double *nu, 
                          double *tmp_generatingVector, int *tmp_nrep, int *antithetic, double *est, double *err) {

    //double diff = 0, p = 0, error = 0;
    //double pi = 3.141592653589793;



    std::vector< double > b(*tmp_d);
    std::vector< double > generatingVector(*tmp_d);
    std::vector< std::vector<double> > mat(*tmp_d, std::vector<double>(*tmp_d));
    for(int i = 0; i < *tmp_d; i++){
        b[i] = tmp_b[i] / sqrt(*nu);
        generatingVector[i] = tmp_generatingVector[i];
        for(int j = 0; j < *tmp_d; j++){
            mat[i][j] = tmp_mat[i * *tmp_d + j];
        }
    }


    //DATA PREPARATION
    for(int i = 0; i < *tmp_d; i++){
        double t = std::sqrt(mat[i][i]);
        b[i] = b[i] / t;
        for( int j = 0; j < *tmp_d; j++){
            mat[i][j] = mat[i][j] / t;
            mat[j][i] = mat[j][i] / t;
        }
    }

    //PARAMETERS FOR VARIABLE REORDERING
    std::vector< double > y(*tmp_d);
    std::vector< std::vector<double> > L(*tmp_d, std::vector<double>(*tmp_d));
    int pos = 0;
    double min = LONG_MAX;

    //PARAMETERS FOR INTEGRATION
    double diff = 0, p = 0, error = 0;


    //--------------------------------------------------- VARIABLE REORDERING --------------------------------------------
    //FIND THE FIRST POSITION
    for (int itPos = 0; itPos < *tmp_d; ++itPos){
        double b_phi = exp(-pow(b[itPos],2)/2)/ sqrt(2 * M_PI);
        //double b_Phi = 0.5 + 0.5 * erf_R(b[itPos] / sqrt(2.0));
        double b_Phi = pnorm(b[itPos], 0, 1, 1, 0);
        double v = 1 - b[itPos] * b_phi / b_Phi - pow(b_phi/b_Phi,2);
        if(v < min) {
            min = v;
            pos = itPos;
            y[0] = - b_phi / b_Phi;
        }
    }

    //SWITCH POSITIONS 0 and in upper bound
    std::swap(b[0], b[pos]);

    //SWITCH ROWS AND COLUMNS
    std::swap(mat[0], mat[pos]);
    for(int i = 0; i < *tmp_d; ++i){
        std::swap(mat[i][0], mat[i][pos]);
    }

    //COMPUTE CHOLESKY
    L[0][0] = std::sqrt(mat[0][0]);
    for(int i = 1 ; i < *tmp_d ; i++) L[i][0] = mat[i][0]/L[0][0];



    //ITERATE THE PROCESS
    for (int rec = 1; rec < *tmp_d; rec ++){
        min = LONG_MAX;
        pos = rec;
        //------------------COMPUTE CANDIDATE FOR NEW UPPER BOUND
        std::vector< double > b_New_Temp(*tmp_d - rec);
        int count = 0;
        for (int itPos = rec; itPos < *tmp_d; ++itPos){
            double conditionnalSum = 0;
            double squaredSum = 0;
            for(int i = 0; i < rec; i++) {
                conditionnalSum += L[itPos][i] * y[i];
                squaredSum += L[itPos][i] * L[itPos][i];
            }
            b_New_Temp[count] = (b[itPos] - conditionnalSum) / sqrt(mat[itPos][itPos] - squaredSum);
            count++;
        }

        //FIND THE MINIMUM
        for (int itPos = 0; itPos < *tmp_d - rec; ++itPos){
            double b_phi = exp(-pow(b_New_Temp[itPos],2)/2)/ sqrt(2 * M_PI);
            //double b_Phi = 0.5 + 0.5 * erf_R(b_New_Temp[itPos] / sqrt(2.0));
            double b_Phi = pnorm(b_New_Temp[itPos], 0, 1, 1, 0);
            double v = 1 - b_New_Temp[itPos] * b_phi / b_Phi - pow(b_phi/b_Phi,2);
            if(v < min) {
                min = v;
                pos = itPos + rec;
                y[rec] = - b_phi / b_Phi;
            }
        }

        //------------------SWITCH POSITIONS
        //SWITCH POSITIONS 0 and in upper bound
        std::swap(b[rec], b[pos]);

        //SWITCH ROWS AND COLUMNS
        std::swap(mat[rec], mat[pos]);
        for(int i = 0; i < *tmp_d; ++i){
            std::swap(mat[i][rec], mat[i][pos]);
        }
        std::swap(L[rec], L[pos]);

        //----------------COMPUTE CHOLESKY TERMS
        double rowSum = 0;
        for(int j = 0; j < rec; j++) rowSum += L[rec][j] * L[rec][j];
        L[rec][rec] = std::sqrt(mat[rec][rec] - rowSum);
        for(int i = rec + 1 ; i < *tmp_d ; i++) {
            double lProd = 0;
            for(int k = 0; k < rec; k++) lProd += L[rec][k] * L[i][k];
            L[i][rec] = (mat[i][rec] - lProd) / L[rec][rec];
        }
    }

    //--------------------------------------------------- INTEGRATION ROUTINE --------------------------------------------
    std::vector< double > vecL;
    for (int i = 0;i < *tmp_d; i++) {
        vecL.insert(vecL.end(), L[i].begin(), L[i].end());
    }

    int *h_d = tmp_d;
    double *h_generatingVector = tmp_generatingVector, *h_randomShift = (double*) R_alloc(*tmp_d, sizeof(double)), *h_b = &b[0], *h_L =&vecL[0], *h_est = est, *h_nu = nu;

    GetRNGstate();

    for (int i = 0; i < *tmp_nrep; i++) {

        for (int k = 0; k < *tmp_d; k++){
            //h_randomShift[k] = ((double) rand() / (RAND_MAX));
            h_randomShift[k] = unif_rand();
        }
        *h_est = 0;
        if(*antithetic == 1){
          for (int j = 0; j < *tmp_xn; j++) {
            pointEstimateTProbAT(j, h_d, h_generatingVector, h_randomShift, h_b, h_L, h_est, h_nu);
          }
          diff = ((*h_est / (2* *tmp_xn)) - p) / (i + 1);
        } else{
        for (int j = 0; j < *tmp_xn; j++) {
            pointEstimateTProb(j, h_d, h_generatingVector, h_randomShift, h_b, h_L, h_est, h_nu);
        }
        diff = ((*h_est / *tmp_xn) - p) / (i + 1);
        }
        p += diff;
        error = ( i - 1 ) * error / (i + 1) + pow(diff,2);
        if (checkInterrupt())  break;
    }

    PutRNGstate();

    error = 3 * sqrt(error);


    *est = p;
    *err = error;
}


static const R_CMethodDef cMethods[] = {
    {"mvtNormCpp", (DL_FUNC) &mvtNormCpp, 9},
    {"mvTProbCpp", (DL_FUNC) &mvTProbCpp, 10},
    {NULL, NULL, 0}
};

void R_init_mvPot(DllInfo *info)
{
    R_registerRoutines(info, cMethods, NULL, NULL, NULL);
    R_useDynamicSymbols(info, TRUE);
}

