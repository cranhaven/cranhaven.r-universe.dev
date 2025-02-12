#include "SFSI.h"
//#include "utils.c"

//====================================================================
// Calculates the mean and standard deviation of xj (j=1,...p)
//====================================================================
void get_mean(int n, int p, double *X,
                double *mx  // output: sum of xj
             )
{
  long long i;
  int inc0 = 0, inc1 = 1;
  double *one=(double *) R_alloc(1, sizeof(double));
  one[0] = 1;

  for(i=0; i<p; i++){
    mx[i] = F77_NAME(ddot)(&n, X + n*i, &inc1, one, &inc0)/(double)n;
  }
}

//====================================================================

void get_sd(int n, int p, double *X,
                double factor, // either 1/n or 1/(n-1)
                double *mx,    // input: mean of xj
                double *sdx    // output: standard deviation of xj
           )
{
  long long i;
  int inc1 = 1;

  if(mx == NULL){
    //Rprintf(" No mean is provided, it will be assumed to be zero\n");
    // Euclidean norm DNRM2 := sqrt( x'*x )
    for(i=0; i<p; i++){
      sdx[i] = sqrt(factor)*F77_NAME(dnrm2)(&n, X + n*i, &inc1);
    }

  }else{
    for(i=0; i<p; i++){
      sdx[i] = sqrt(factor)*sqrt(F77_NAME(ddot)(&n,
                                   X + n*i, &inc1,
                                   X + n*i, &inc1)-(double)n*pow(mx[i],2));
    }
  }
}

//====================================================================
//====================================================================

void get_connection(int n, int p, double *X, bool *A,
                    double thr, int useD, double *pos,
                    double dmax, double factor,
                    int centered, int scaled)
{
  long long i, j;
  int flag;
  double one = 1;
  double *xty = (double *) R_alloc(p, sizeof(double));  // crossprod

  if(centered && scaled){
    for(j=0; j<p-1; j++){
      matrix_vector_product(n, p-j-1, &one,
                            X + n*(j+1),
                            X + n*j, 1,
                            xty, 1); // X[,j+1]'X[,j]

      for(i=j+1; i<p; i++){
        flag = pow(factor*xty[i-j-1],2) > thr;  // R2>thr
        if(useD){
          flag = flag*(fabs(pos[i]-pos[j]) <= dmax);
        }
        A[p*j + i] = flag;
        A[p*i + j] = A[p*j + i];
      }
    }

  }else{
    double *mx = (double *) R_alloc(p, sizeof(double));
    double *sdx = (double *) R_alloc(p, sizeof(double));  // standard deviation

    if(centered){
      memset(mx, 0, p*sizeof(double));
      get_sd(n, p, X, factor, NULL, sdx);
    }else{
      get_mean(n, p, X, mx);
      get_sd(n, p, X, factor, mx, sdx);
    }

    for(j=0; j<p-1; j++){
      matrix_vector_product(n, p-j-1, &one,
                            X + n*(j+1),
                            X + n*j, 1,
                            xty, 1);

      for(i=j+1; i<p; i++){
        flag = pow(factor*(xty[i-j-1]-n*mx[i]*mx[j])/(sdx[i]*sdx[j]),2) > thr;
        if(useD){
          flag = flag*(fabs(pos[i]-pos[j]) <= dmax);
        }
        A[p*j + i] = flag;
        A[p*i + j] = A[p*j + i];
      }
    }
  }
}

//====================================================================
//====================================================================

SEXP R_prune(SEXP X_,
             SEXP thr_,
             SEXP pos_,  // Position in bp
             SEXP dmax_, // Maximum distance (bp)
             SEXP centered_,
             SEXP scaled_,
             SEXP verbose_
           )
{
    long long i, j;
    int flag, keep, selected, ndrop;

    int n = Rf_nrows(X_);
    int p = Rf_ncols(X_);
    int useD = Rf_isNull(pos_) ? 0 : 1;
    double thr = NUMERIC_VALUE(thr_);
    double dmax = NUMERIC_VALUE(dmax_);
    int centered = asLogical(centered_);
    int scaled = asLogical(scaled_);
    int verbose = asLogical(verbose_);

    PROTECT(X_ = AS_NUMERIC(X_));
    double *X = NUMERIC_POINTER(X_);

    PROTECT(pos_ = AS_NUMERIC(pos_));
    double *pos = NUMERIC_POINTER(pos_);

    //int *A=(int *) R_alloc(p*p, sizeof(int));
    bool *A = (bool *) R_alloc(p*p, sizeof(bool));
    int *pruneIn = (int *) R_alloc(p, sizeof(int));
    int *pruneOut = (int *) R_alloc(p, sizeof(int));
    int *remain = (int *) R_alloc(p, sizeof(int));
    int *remain0 = (int *) R_alloc(p, sizeof(int));
    int *connection = (int *) R_alloc(p, sizeof(int));
    int *drop = (int *) R_alloc(p, sizeof(int));

    // Get connection using the R2
    double factor = 1/((double)n -1);
    if(verbose){
      Rprintf(" Pruning %d subjects",p);
      if(useD){
        Rprintf(" within a distance of %.2f bp ...\n",dmax);
      }else{
        Rprintf(" ...\n");
      }
      Rprintf(" Obtaining all %d pairwise R2 ...\n",p*(p-1)/2);
    }
    get_connection(n, p, X, A, thr, useD, pos, dmax, factor, centered, scaled);

    // Set some initial values
    int nrem = p;
    int step = 0;
    int nin = 0;
    int nout = 0;
    for(j=0; j<p; j++){
      remain[j] = j;
      connection[j] = 0;
      A[p*j + j] = true; // Connection with itself
    }

    // Get connections summing by column across rows
    for(j=0; j<p; j++){
      for(i=0; i<p; i++){
        if(A[p*j + i]){ connection[j]++; }
      }

      if(connection[j]==1){  // Subjects with no initial connections
        pruneIn[nin] = j+1;
        drop[nin] = j;
        nin++;
      }
    }
    if(nin > 0){
      step++;
      if(verbose){
        Rprintf("--------------------------------------------------------\n");
        Rprintf(" Step:%4d. IN: n=%4d. nConn=%3d. nIN=%4d. nLeft=%4d\n",
                step,nin,0,nin,nrem-nin);
      }
      reduce_vector_integer(nrem, remain, nin, drop);
      reduce_vector_integer(nrem, connection, nin, drop);
      nrem-=nin;
    }

    memcpy(remain0, remain, nrem*sizeof(int));
    flag = nrem > 0;
    while(flag){
        step++;
        keep = imax_integer(nrem, connection); // Index with maximum connection
        if(connection[keep] > 1){
          selected = remain[keep]+1;
          append_to_sorted_vector_integer(nin++, pruneIn, 1, &selected);
          //pruneIn[nin++] = remain[keep]+1;

          // Check the connections with the selected one to drop
          ndrop = 0;
          for(j=0; j<nrem; j++){
            if(A[p*(long long)remain[keep] + (long long)remain[j]]){
               drop[ndrop++] = j;
               if(j != keep){
                 selected = remain[j]+1;
                 append_to_sorted_vector_integer(nout++, pruneOut, 1, &selected);
                 //pruneOut[nout++] = remain[j]+1;
               }
            }
          }

          if(verbose){
            Rprintf("--------------------------------------------------------\n");
            Rprintf(" Step:%4d. IN: i=%4d. nConn=%3d. nIN=%4d. nLeft=%4d\n",
                     step,remain[keep]+1,connection[keep]-1,nin,p-nin-nout);
          }

          // Update current conections by sustracting connection with the ones to be dropped
          reduce_vector_integer(nrem, remain0, ndrop, drop);
          reduce_vector_integer(nrem, connection, ndrop, drop);
          nrem -= ndrop;
          for(i=0; i<ndrop; i++){
            for(j=0; j<nrem; j++){
              if(A[p*(long long)remain[drop[i]]+(long long)remain0[j]]){
                connection[j]--;
              }
            }
          }
          memcpy(remain, remain0, nrem*sizeof(int));
          flag = nrem > 0;

        }else{  // Pass all the remaining entries to pruneIn
          for(i=0; i<nrem; i++){
            selected = remain[i]+1;
            append_to_sorted_vector_integer(nin++, pruneIn, 1, &selected);
            //pruneIn[nin++] = remain[i]+1;
          }
          if(verbose){
            Rprintf("--------------------------------------------------------\n");
            Rprintf(" Step:%4d. IN: n=%4d. nConn=%3d. nIN=%4d. nLeft=%4d\n",
                     step,nrem,connection[keep]-1,nin,p-nin-nout);
          }
          flag = 0;
        }
    }
    if(verbose){
      Rprintf("--------------------------------------------------------\n");
      Rprintf(" Total subjects=%6d\n",p);
      Rprintf(" N pruned-in=%6d\n",nin);
      Rprintf(" N pruned-out=%6d\n",nout);
    }

    SEXP pruneIn_ = PROTECT(Rf_allocVector(INTSXP, nin));
    SEXP pruneOut_ = PROTECT(Rf_allocVector(INTSXP, nout));
    memcpy(INTEGER_POINTER(pruneIn_), pruneIn, nin*sizeof(int));
    memcpy(INTEGER_POINTER(pruneOut_), pruneOut, nout*sizeof(int));

    SEXP list_ = PROTECT(Rf_allocVector(VECSXP, 2));
    SET_VECTOR_ELT(list_, 0, pruneIn_);
    SET_VECTOR_ELT(list_, 1, pruneOut_);

    UNPROTECT(5);

    return(list_);
}
