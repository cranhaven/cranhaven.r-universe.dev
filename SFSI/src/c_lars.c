#include "SFSI.h"
//#include "utils.c"

//====================================================================
//    Append to a vector v, k elements provided in another vector
//    IN: Vector values (length at least k) to append
//    OUT: Vector v will contain the n+k elements
//====================================================================
double* append_to_vector_double(int n, double *v, int k, double *values)
{
  int i;
  v = R_Realloc(v, n+k, double);

  for(i=0; i<k; i++){
    v[n+i] = values[i];
  }

  return(v);
}

int* append_to_vector_integer(int n, int *v, int k, int *values)
{
  int i;
  v = R_Realloc(v, n+k, int);

  for(i=0; i<k; i++){
    v[n+i] = values[i];
  }

  return(v);
}

//====================================================================
//    Subset a vector v1 by selecting k elements
//    provided in the INDEX vector: any of 0,1,2,...,n-1
//    OUT: Vector v2 contain the k subset elements
//====================================================================
void subset_vector_double(double *v1, double *v2, int k, int *index)
{
  int i;
  for(i=0; i<k; i++){
    v2[i] = v1[index[i]];
  }
}

//====================================================================
//    Delete a row or a column in a n x p matrix M
//    irow: any of 0,1,2, ..., n-1, if irow < 0 no row is deleted
//    icol: any of 0,1,2, ..., p-1, if icol < 0 no column is deleted
//====================================================================
void reduce_matrix(int nrow, int ncol, int irow, int icol, double *M)
{
  long long j;
  long long offset;

  long long nnew = nrow;
  if(irow > -1){
    nnew = nrow-1;
    offset = nrow-irow-1;
    for(j=1; j<ncol; j++){
      memmove(M + nnew*j - offset, M + nrow*j - offset, nnew*sizeof(double));
    }
    if(offset>0){
      memmove(M + nnew*ncol - offset,
              M + (long long)nrow*(long long)ncol - offset,
              offset*sizeof(double));
    }
  }

  if(icol > -1){
    offset = ncol-icol-1;
    if(offset>0){
      memmove(M + nnew*(long long)icol,
              M + nnew*((long long)icol+1),
              nnew*offset*sizeof(double));
    }
  }
}

//====================================================================
//    Delete k elements from a vector of length n
//    k: number of elements to delete
//    INDEX: vector with any of 0,1,2,...,n-1. Must to be sorted ascending
//====================================================================
void reduce_vector_double(int n, double *v, int k, int *index)
{
  int chunk;

  int pos = index[0];
  int j = 0;
  int flag = 1;
  while(flag){
    while(((j+1)<k) && ((index[j+1]-index[j])==1)){
      j++;
    }
    if((index[j]+1) < n){
      if((j+1) < k){
        chunk=index[j+1]-index[j]-1;
      }else{
        chunk=n-index[j]-1;
        flag = 0;
      }
      memmove(v + pos, v + index[j]+1, chunk*sizeof(double));
      pos += chunk;
      j++;
    }else{
      flag = 0;
    }
  }
}

//====================================================================
// Update coefficients in the p x p-1 matrix R
// that has been formed from a p x p upper-triangular
// matrix whose column k (0, 1,...,p-1) was deleted
//====================================================================
void update_deleted_cols(int p, int k, double *R, int nz, double *z)
{
  double a, b, c, s, tau;
  long long i, j;
  long long pos1, pos2;

  double zero = DBL_EPSILON;

  for(i=k; i<p-1; i++)
  {
    pos1 = p*i + i;
    a = R[pos1];
    b = R[pos1 + 1];
    if(fabs(b) > zero) //if(b!=0.0f)
    {
      // Compute the rotation
       if(fabs(b)>fabs(a)){
         tau = -a/b;
         s = 1/sqrt(1 + tau*tau);
         c = s * tau;
       }else{
         tau = -b/a;
         c = 1/sqrt(1 + tau*tau);
         s = c * tau;
       }

       // update r and z
       R[pos1] = c*a - s*b;
       R[pos1 + 1] = s*a + c*b;

       for(j=i+1; j<p-1; j++)
       {
         pos2 = p*j + i;
         a = R[pos2];
         b = R[pos2 + 1];
         R[pos2] = c*a - s*b;
         R[pos2 + 1] = s*a + c*b;
       }
       for(j=0; j<nz; j++)
       {
         pos2 = p*j + i;
         a = z[pos2];
         b = z[pos2 + 1];
         z[pos2] = c*a - s*b;
         z[pos2 + 1] = s*a + c*b;
       }
    }
  }
}

//====================================================================
// Drop row and columm k from a p x p upper-triangular matrix R
// and update resulting (p-1)x(p-1) matrix
// k: any of 0,1,...,p-1
//====================================================================
void downdate_chol(int p, int k, double *R, int nz, double *z)
{
  reduce_matrix(p, p, -1, k, R);
  update_deleted_cols(p, k, R, nz, z);
  reduce_matrix(p, p-1, p-1, -1, R);
}

//====================================================================

SEXP R_lars(SEXP XtX_, SEXP Xty_,
            SEXP eps_, SEXP dfmax_, SEXP maxsteps_,
            SEXP scale_, SEXP sd_, SEXP isLASSO_,
            SEXP filename_,
            SEXP doubleprecision_, SEXP verbose_)
{
    double A, gamma, gamma0, zmin, Cmax, value;
    int i, j, k;
    int info, inew, nnew, varsize, vartype;
    int inc1 = 1;
    float valuefloat;
    int nprotect = 7;
    FILE *f = NULL;

    int p = Rf_length(Xty_);
    int dfmax = INTEGER_VALUE(dfmax_);
    int maxsteps = Rf_isNull(maxsteps_) ? INT_MAX : INTEGER_VALUE(maxsteps_);
    int verbose = INTEGER_VALUE(verbose_);
    int scale = asLogical(scale_);
    int isLASSO = asLogical(isLASSO_);
    double eps = NUMERIC_VALUE(eps_);
    int doubleprecision = asLogical(doubleprecision_);
    int save = !Rf_isNull(filename_);

    PROTECT(XtX_ = AS_NUMERIC(XtX_));
    double *XtX = NUMERIC_POINTER(XtX_);

    PROTECT(Xty_ = AS_NUMERIC(Xty_));
    double *Xty = NUMERIC_POINTER(Xty_);

    PROTECT(sd_ = AS_NUMERIC(sd_));
    double *sd = NUMERIC_POINTER(sd_);

    int nsteps = 1.25*dfmax;

    double *lambda = (double *) R_Calloc(nsteps, double);
    int *df = (int *) R_Calloc(nsteps, int);

    //Rprintf("Allocating memory for B ...\n");
    // Allocated memory is set to zero (as in calloc)
    double *B = (double *) R_Calloc(0, double);
    if(save){
      varsize = doubleprecision ? sizeof(double) : sizeof(float);
      vartype = 3;
      f = fopen(CHAR(STRING_ELT(filename_,0)),"wb");
      fwrite(&p, sizeof(int), 1, f);
      fwrite(&nsteps, sizeof(int), 1, f);
      fwrite(&vartype, sizeof(int), 1, f);
      fwrite(&varsize, sizeof(int), 1, f);
    }else{
      B = R_Realloc(B, p*nsteps, double);
    }

    int *im = (int *) R_alloc(p, sizeof(int));
    double *Sign = (double *) R_alloc(p, sizeof(double));
    int *inactive = (int *) R_alloc(p, sizeof(int));
    int *active = (int *) R_alloc(p, sizeof(int));
    int *activeignores = (int *) R_alloc(p, sizeof(int));
    double *z = (double *) R_alloc(p, sizeof(double));
    double *b = (double *) R_alloc(p, sizeof(double));
    double *bout = (double *) R_alloc(p, sizeof(double));  // Output
    double *R = (double *) R_alloc((long long)p*p, sizeof(double));
    int *itmp = (int *) R_Calloc(0, int);
    double *tmp = (double *) R_alloc(p, sizeof(double));
    double *covar = (double *) R_alloc(p, sizeof(double));
    double *a = (double *) R_alloc(p, sizeof(double));
    double *rhs = (double *) R_alloc(p, sizeof(double));
    double *w = (double *) R_alloc(p, sizeof(double));

    memset(b, 0, p*sizeof(double));        // Initialize all coefficients to zero
    memset(df, 0, nsteps*sizeof(int));

    //Rprintf("Initializing first column of B to zero ...\n");
    if(save){
      if(doubleprecision){
        fwrite(b, varsize, p, f);
      }else{  // Cast to float one by one
        valuefloat = 0;
        for(j=0; j<p; j++){
          fwrite(&valuefloat, varsize, 1, f);
        }
      }
    }else{
      memset(B, 0, p*sizeof(double));
    }

    for(j=0; j<p; j++){
      inactive[j] = j;
      im[j] = j;
      z[j] = 1;
    }

    F77_NAME(dcopy)(&p, Xty, &inc1, rhs, &inc1); //  rhs <- Xty
    int nactive = 0;
    int ninactive = p;
    int nignores = 0;
    int ndrops = 0;
    int extrasteps = (0.01*p)<1 ? 1 : 0.01*p;  // Enlarge by 0.01xp the output's memory
    double zero = DBL_EPSILON;

    //Rprintf("Starting LARS ...\n");
    k = 0;
    while((k<maxsteps) && (nactive<dfmax) && nactive<(p-nignores))
    {
      subset_vector_double(rhs, covar, ninactive, inactive);

      Cmax = fabs(covar[F77_NAME(idamax)(&ninactive, covar, &inc1)-1]);
      if(Cmax < eps*100){
        if(verbose){
          Rprintf(" Max absolute correlation is zero. Exiting...\n");
        }
        break;
      }
      lambda[k++] = Cmax;

      if(ndrops == 0){
        nnew = 0;
        for(i=0; i<ninactive; i++){
          //if(fabs(covar[i]) >= Cmax-eps){
          if(fabs(fabs(covar[i])-Cmax) <= eps){
            itmp = append_to_vector_integer(nnew++, itmp, 1, &i);
          }
        }
        reduce_vector_double(ninactive, covar, nnew, itmp);

        if(verbose){
          Rprintf("--------------------------------------------------------------------\n");
        }
        for(i=0; i<nnew; i++){
          inew = inactive[itmp[i]];
          append_to_sorted_vector_integer(nactive+nignores, activeignores, 1, &inew);
          update_chol(p, XtX, nactive, R, inew, active, &eps, tmp, &info);

          if(info == 0){
            active[nactive] = inew;
            Sign[nactive] = sign(rhs[inew]);
            nactive++;
            if(verbose){
              Rprintf(" Step=%5d. lambda=%1.8f. Feature %5d IN\n",k,Cmax,inew+1);
            }
          }else{
            reduce_matrix(nactive+1, nactive+1, nactive, nactive, R); // R <- R[1:nactive,1:nactive]
            nignores++;
            if(verbose){
              Rprintf(" Step=%5d. lambda=%1.8f. Feature %5d DROPPED by collinearity\n",k,Cmax,inew+1);
            }
          }
        }
      }
      memcpy(w, Sign, nactive*sizeof(double));
      backsolvet(nactive, R, w);
      backsolve(nactive, R, w);  // Gi1 <- backsolve(R, backsolvet(R, Sign))

      A = 1/sqrt(F77_NAME(ddot)(&nactive, w, &inc1, Sign, &inc1)); // A <- 1/sqrt(sum(Gi1*Sign))
      F77_NAME(dscal)(&nactive, &A, w, &inc1);    // w <- A*Gi1

      // Get inactive subjects
      memcpy(inactive, im, p*sizeof(int));
      reduce_vector_integer(p, inactive, nactive+nignores, activeignores);
      ninactive = p-nactive-nignores;

      // a <- XtX[,active] %*% w = X'X[,active]w
      matrix_vector_product_subset(p,p,XtX,w,a,0,NULL,nactive,active,0,tmp);

      if(nactive >= (p-nignores)){  // No more inactive predictors
        gamma = Cmax/A;
      }else{
        // a <- XtX[-c(active,ignores),active] %*% w
        //matrix_vector_product_subset(p,p,XtX,w,a,ninactive,inactive,
        //                             nactive,active,0,tmp);
        gamma0 = INFINITY;
        for(i=0; i<ninactive; i++){
          //value = (Cmax-covar[i])/(A-a[i]);
          value = (Cmax-covar[i])/(A-a[inactive[i]]);
          if(value>eps && value<gamma0){   // this is new
            gamma0 = value;
          }

          //value = (Cmax+covar[i])/(A+a[i]);
          value = (Cmax+covar[i])/(A+a[inactive[i]]);
          if(value>eps && value<gamma0){
            gamma0 = value;
          }
        }
        gamma = gamma0 < Cmax/A ? gamma0 : Cmax/A;
      }
      if(ndrops == 0){
        if(verbose>1){
          Rprintf(" nInactive=%5d. nDropped=%5d. nActive=%5d. A=%1.8f. gamma=%1.8f. C/A=%1.8f\n",
                  ninactive,nignores,nactive,A,gamma,Cmax/A);
        }
      }

      ndrops = 0;
      if(isLASSO){
        zmin = gamma;
        for(i=0; i<nactive; i++){ // z <- -B[,j]/w
          tmp[i] = -1*b[active[i]]/w[i];
          if(tmp[i]>eps && tmp[i]<zmin){
            zmin = tmp[i];
          }
        }
        if(zmin < gamma){
          gamma = zmin;
          for(i=0; i<nactive; i++){
            if(fabs(tmp[i]-zmin) < zero){ // tmp[i]==zmin
              itmp = append_to_vector_integer(ndrops++, itmp, 1, &i);
            }
          }
        }
      }

      for(i=0; i<nactive; i++){
        b[active[i]] += gamma*w[i];
      }

      // Update covariances: rhs <- rhs - gamma*XtX[,active]%*%w
      // matrix_vector_product_subset(p,p,XtX,w,a,0,NULL,nactive,active,0,tmp);
      value = -1*gamma;
      F77_NAME(daxpy)(&p, &value, a, &inc1, rhs, &inc1);

      if(isLASSO && ndrops>0){
        if(verbose){
          Rprintf("--------------------------------------------------------------------\n");
        }
        for(i=0; i<ndrops; i++){
          if(verbose){
            Rprintf(" Step=%5d. lambda=%1.8f. Feature %5d OUT\n",k+1,Cmax,active[itmp[ndrops-(i+1)]]+1);
          }
          // For nR = nactive, nactive-1,...,nactive-(ndrops-1)
          downdate_chol(nactive-i, itmp[ndrops-(i+1)], R, 1, z);

          b[active[itmp[i]]] = 0;
          for(j=0; j<nactive+nignores; j++){
            if(activeignores[j]==active[itmp[i]]){
              reduce_vector_integer(nactive+nignores, activeignores, 1, &j);
              break;
            }
          }

          // Return dropped active to inactive
          append_to_sorted_vector_integer(ninactive, inactive, 1, active+itmp[i]);
          ninactive++;
        }
        reduce_vector_integer(nactive, active, ndrops, itmp);
        reduce_vector_double(nactive, Sign, ndrops, itmp);
        nactive-=ndrops;

        if(verbose>1){
          Rprintf(" nInactive=%5d. nDropped=%5d. nActive=%5d. A=%1.8f. gamma=%1.8f. C/A=%1.8f\n",
                  ninactive,nignores,nactive,A,gamma,Cmax/A);
        }
      }

      F77_NAME(dcopy)(&p, b, &inc1, bout, &inc1);
      df[k] = nactive;

      if(scale){
        for(j=0; j<p; j++){
          bout[j] = bout[j]/sd[j];
        }
      }

      if(save){
        if(doubleprecision){
          fwrite(bout, varsize, p, f);
        }else{  // Cast to float one by one
          for(j=0; j<p; j++){
            valuefloat = bout[j];
            fwrite(&valuefloat, varsize, 1, f);
          }
        }
      }else{
        F77_NAME(dcopy)(&p, bout, &inc1, B+(long long)p*(long long)k, &inc1);
      }

      if(k == nsteps-1){  // Enlarge memory for outputs if needed
        nsteps += extrasteps;
        lambda = R_Realloc(lambda, nsteps, double);
        df = R_Realloc(df, nsteps, int);
        if(!save){
          B = R_Realloc(B, p*nsteps, double);
        }
      }
    }

    // Get the next max correlation
    if(nactive <= (p-nignores)){
      subset_vector_double(rhs, covar, ninactive, inactive);
      lambda[k] = fabs(covar[F77_NAME(idamax)(&ninactive, covar, &inc1)-1]);
    }else{
      lambda[k] = fabs(rhs[F77_NAME(idamax)(&p, rhs, &inc1)-1]);
    }
    k++;

    SEXP lambda_ = PROTECT(Rf_allocVector(REALSXP, k));
    memcpy(NUMERIC_POINTER(lambda_), lambda, k*sizeof(double));

    SEXP df_ = PROTECT(Rf_allocVector(INTSXP, k));
    memcpy(INTEGER_POINTER(df_), df, k*sizeof(int));

    SEXP B_ = NULL;
    if(save){
      fseek(f, 4, SEEK_SET); // Save the final number of solutions
      fwrite(&k, 4, 1, f);
      fclose(f);
      B_ = R_NilValue;
    }else{
      B_ = PROTECT(Rf_allocMatrix(REALSXP, p, k));
      memcpy(NUMERIC_POINTER(B_), B, p*k*sizeof(double));
      R_Free(B);
      nprotect++;
    }

    R_Free(lambda);
    R_Free(df);
    R_Free(itmp);

    // Creating a list with 3 vector elements:
    SEXP list_ = PROTECT(Rf_allocVector(VECSXP, 3));
    SET_VECTOR_ELT(list_, 0, B_);
    SET_VECTOR_ELT(list_, 1, lambda_);
    SET_VECTOR_ELT(list_, 2, df_);

    // Set dimnames for outputs
    SEXP names_ = PROTECT(Rf_allocVector(VECSXP, 3));
    SET_VECTOR_ELT(names_, 0, mkChar("beta"));
    SET_VECTOR_ELT(names_, 1, mkChar("lambda"));
    SET_VECTOR_ELT(names_, 2, mkChar("nsup"));
    setAttrib(list_, R_NamesSymbol, names_);

    UNPROTECT(nprotect);

    return(list_);
}
