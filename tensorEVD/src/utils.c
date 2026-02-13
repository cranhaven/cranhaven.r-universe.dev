#include "tensorEVD.h"

//==============================================================
// Return sorted (descendant order) indices for a vector of
// numeric values. The ordered position of the element k,
// values[k], is found by comparing with the (ordered) values
// of the previous k-1 values. This is, by finding the last
// index order[j] (j=0,1,...,k) such that
//                values[order[j]] > values[k]
// where 'order' contains the ordered indices for the first
//        k elements of 'values': values[0],...,values[k-1]
// The new vector with sorted indices will contain the k+1
// elements:
//    order[0],...,order[j-1], k, order[j],...,order[k-1]
//==============================================================
void append_to_sorted_vector(int k, double *values, int *order)
{
  int j;

  if(k==0){
    order[0] = k;
  }else{
    j=0;
    while(values[order[j]]>values[k]){
      j++;
      if(j == k){
        break;
      }
    }
    if(j < k){
      memmove(order + j+1, order + j, (k-j)*sizeof(int));
    }
    order[j]=k;
  }
}

//==============================================================
//==============================================================
void get_dimnames(int nrow, int ncol,
                  int *irow1, int *irow2, int *irow,
                  int *icol1, int *icol2, int *icol,
                  SEXP dimnames1_,  // optional dimnames from inputs
                  SEXP dimnames2_,
                  SEXP dimnames_    // pointer to output
                  )
{
  SEXP rownames_ = PROTECT(Rf_allocVector(STRSXP, nrow));
  SEXP colnames_ = PROTECT(Rf_allocVector(STRSXP, ncol));

  int i, j;
  char *name1 = (char*)malloc(100*sizeof(char));
  char *name2 = (char*)malloc(100*sizeof(char));

  int flag1 = !Rf_isNull(dimnames1_) && (Rf_length(dimnames1_)==2);
  int flag2 = !Rf_isNull(dimnames2_) && (Rf_length(dimnames2_)==2);

  //Rprintf(" Making rownames ...\n");
  for(i=0; i<nrow; i++){
    j = (irow == NULL) ? i : irow[i];
    if(flag1 && (Rf_length(VECTOR_ELT(dimnames1_,0))>0)){
      strcpy(name1, CHAR(STRING_ELT(VECTOR_ELT(dimnames1_,0),irow1[j])));
    }else{
      snprintf(name1, 100, "%d", irow1[j]+1);
    }
    if(flag2 && (Rf_length(VECTOR_ELT(dimnames2_,0))>0)){
      strcpy(name2, CHAR(STRING_ELT(VECTOR_ELT(dimnames2_,0),irow2[j])));
    }else{
      snprintf(name2, 100, "%d", irow2[j]+1);
    }
    SET_STRING_ELT(rownames_,i,mkChar(strcat(strcat(name1, ":"), name2)));
  }

  //Rprintf(" Making colnames ...\n");
  for(i=0; i<ncol; i++){
    j = (icol == NULL) ? i : icol[i];
    if(flag1 && (Rf_length(VECTOR_ELT(dimnames1_,1))>0)){
      strcpy(name1, CHAR(STRING_ELT(VECTOR_ELT(dimnames1_,1),icol1[j])));
    }else{
      snprintf(name1, 100, "%d", icol1[j]+1);
    }
    if(flag2 && (Rf_length(VECTOR_ELT(dimnames2_,1))>0)){
      strcpy(name2, CHAR(STRING_ELT(VECTOR_ELT(dimnames2_,1),icol2[j])));
    }else{
      snprintf(name2, 100, "%d", icol2[j]+1);
    }
    SET_STRING_ELT(colnames_,i,mkChar(strcat(strcat(name1, ":"), name2)));
  }

  SET_VECTOR_ELT(dimnames_, 0, rownames_);
  SET_VECTOR_ELT(dimnames_, 1, colnames_);

  UNPROTECT(2);
}

//==============================================================
// Obtain the indices for the kronecker product between matrices
// A (nrowA x ncolA) and B (nrowB x ncolB).
// Let n1 and n2 to be either nrow or ncol from A and B,
// the kronecker is obtained by multiplying elements
//     [1,1,...,1,2,2,...,2,...,n1,nA...,nA]  from A
// and elements
//     [1,2,...,nB,1,2,...,nB,...,1,2,...,nB] from B
//
//==============================================================
void get_pos(int nA, int nB, int k, int *i, int *j, int start)
{
  i[0] = (int) floor(k/nB) + start;
  j[0] = (k % nB) + start;
}

//==============================================================

void get_kronecker_index(int nA, int nB, int *iA, int *iB, int ni, int *index, int start)
{
  int i, j;

  if(ni == 0){
    int tmp = 0;
    for(i=0; i<nA; i++){
      for(j=0; j<nB; j++){
        iA[tmp] = i + start;
        iB[tmp] = j + start;
        tmp++;
      }
    }
  }else{
    for(i=0; i<ni; i++){
      get_pos(nA, nB, index[i], iA+i, iB+i, start);
    }
  }
}

SEXP R_kronecker_index(SEXP nrowA_, SEXP ncolA_,
                       SEXP nrowB_, SEXP ncolB_,
                       SEXP rows_, SEXP cols_,
                       SEXP swap_, SEXP zerobased_)
{
  int nprotect = 6;

  int nrowA = INTEGER_VALUE(nrowA_);
  int ncolA = INTEGER_VALUE(ncolA_);
  int nrowB = INTEGER_VALUE(nrowB_);
  int ncolB = INTEGER_VALUE(ncolB_);
  int swap = asLogical(swap_);

  int start = asLogical(zerobased_) ? 0 : 1;
  int nirow = Rf_length(rows_);
  int nicol = Rf_length(cols_);

  PROTECT(rows_ = AS_INTEGER(rows_));
  int *irow = INTEGER_POINTER(rows_);

  PROTECT(cols_ = AS_INTEGER(cols_));
  int *icol = INTEGER_POINTER(cols_);

  int nrow = (nirow == 0) ? nrowA*nrowB : nirow;
  int ncol = (nicol == 0) ? ncolA*ncolB : nicol;

  SEXP irowA_, irowB_, icolA_, icolB_;
  int *irowA, *irowB, *icolA, *icolB;

  // Get positions for rows: irowA and irowB
  irowA_ = PROTECT(Rf_allocVector(INTSXP, nrow));
  irowA = INTEGER_POINTER(irowA_);

  irowB_ = PROTECT(Rf_allocVector(INTSXP, nrow));
  irowB = INTEGER_POINTER(irowB_);

  // Rprintf(" Creating indices for rows: irowA and irowB ...\n");
  if(swap){   // kronecker(B,A)
    get_kronecker_index(nrowB, nrowA, irowB, irowA, nirow, irow, start);
  }else{      // kronecker(A,B)
    get_kronecker_index(nrowA, nrowB, irowA, irowB, nirow, irow, start);
  }

  // Get positions for cols: icolA and icolB
  if((nrowA==ncolA) && (nrowB==ncolB) && ((nirow+nicol)==0)){
    // Rprintf(" Recycling indices for cols because both are squared matrices ...\n");
    icolA_ = irowA_;
    icolB_ = irowB_;
  }else{
    // Rprintf(" Creating indices for cols: icolA and icolB ...\n");
    icolA_ = PROTECT(Rf_allocVector(INTSXP, ncol));
    icolA = INTEGER_POINTER(icolA_);

    icolB_ = PROTECT(Rf_allocVector(INTSXP, ncol));
    icolB = INTEGER_POINTER(icolB_);
    nprotect+=2;

    if(swap){   // kronecker(B,A)
      get_kronecker_index(ncolB, ncolA, icolB, icolA, nicol, icol, start);
    }else{      // kronecker(A,B)
      get_kronecker_index(ncolA, ncolB, icolA, icolB, nicol, icol, start);
    }
  }

  // Rprintf(" Outputting irowA, icolA, irowB, and icolB ...\n");
  SEXP list_ = PROTECT(Rf_allocVector(VECSXP, 4));
  SET_VECTOR_ELT(list_, 0, irowA_);
  SET_VECTOR_ELT(list_, 1, icolA_);
  SET_VECTOR_ELT(list_, 2, irowB_);
  SET_VECTOR_ELT(list_, 3, icolB_);

  SEXP names_ = PROTECT(Rf_allocVector(VECSXP, 4));
  SET_VECTOR_ELT(names_, 0, mkChar("irowA"));
  SET_VECTOR_ELT(names_, 1, mkChar("icolA"));
  SET_VECTOR_ELT(names_, 2, mkChar("irowB"));
  SET_VECTOR_ELT(names_, 3, mkChar("icolB"));
  Rf_setAttrib(list_, R_NamesSymbol, names_);

  UNPROTECT(nprotect);

  return(list_);
}

//====================================================================
// Weighted sum between two vectors:
//       dz[j] <- a * dx[ix[j]] + b * dy[iy[j]],    j = 1,2,...,n
//
//   [in]  a: (double) A factor to multiply the first array by
//   [in]  n: (int) Number of elements in input vector(s) ix and iy
//   [in]  dx: double precision array of dimension <= max(ix)+1
//   [in]  ix: integer array (zero-based) of dimension n
//   [in]  b: (double) A factor to multiply the second array by
//   [in]  dy: double precision array of dimension <= max(iy)+1
//   [in]  ix: integer array (zero-based) of dimension n
//   [out] dz: double precision array of dimension at least n
//====================================================================
void sum_set(int n, double *a, double *dx, int *ix, double *b, double *dy, int *iy, double *dz)
{
    int m, i;

    /* Clean-up loop so remaining vector length is a multiple of 5.  */
    m = n % 5;
    if(m != 0){
       for(i=0; i<m; i++){
          dz[i] = a[0] * dx[ix[i]] + b[0] * dy[iy[i]];
       }
       if(n < 5){
          return;
       }
    }
    for(i=m; i<n; i+=5)
    {
       dz[i] = a[0] * dx[ix[i]] + b[0] * dy[iy[i]];
       dz[i+1] = a[0] * dx[ix[i+1]] + b[0] * dy[iy[i+1]];
       dz[i+2] = a[0] * dx[ix[i+2]] + b[0] * dy[iy[i+2]];
       dz[i+3] = a[0] * dx[ix[i+3]] + b[0] * dy[iy[i+3]];
       dz[i+4] = a[0] * dx[ix[i+4]] + b[0] * dy[iy[i+4]];
    }
}

//====================================================================
// Hadamard product between two vectors:
//       dz[j] <- a * dx[ix[j]] * dy[iy[j]],    j = 1,2,...,n
//
//   [in]  a: (double) A factor to multiply the hadamard by
//   [in]  n: (int) Number of elements in input vector(s) ix and iy
//   [in]  dx: double precision array of dimension <= max(ix)+1
//   [in]  ix: integer array (zero-based) of dimension n
//   [in]  dy: double precision array of dimension <= max(iy)+1
//   [in]  ix: integer array (zero-based) of dimension n
//   [out] dz: double precision array of dimension at least n
//====================================================================
void hadam_set(int n, double *a, double *dx, int *ix, double *dy, int *iy, double *dz)
{
    int m, i;

    /* Clean-up loop so remaining vector length is a multiple of 5.  */
    m = n % 5;
    if(m != 0){
       for(i=0; i<m; i++){
          dz[i] = a[0] * dx[ix[i]] * dy[iy[i]];
       }
       if(n < 5){
          return;
       }
    }
    for(i=m; i<n; i+=5)
    {
       dz[i] = a[0] * dx[ix[i]] * dy[iy[i]];
       dz[i+1] = a[0] * dx[ix[i+1]] * dy[iy[i+1]];
       dz[i+2] = a[0] * dx[ix[i+2]] * dy[iy[i+2]];
       dz[i+3] = a[0] * dx[ix[i+3]] * dy[iy[i+3]];
       dz[i+4] = a[0] * dx[ix[i+4]] * dy[iy[i+4]];
    }
}

//====================================================================
// Euclidean norm of a vector dz that is formed as a Hadamard product
// between two subset vectors:
//       dz[j] <- dx[ix[j]] * dy[iy[j]],    j = 1,2,...,n
// Then the norm is:
//       sqrt(dz[1]^2 + ... + dz[n]^2)
//====================================================================
double dnorm_hadam_set(int n, double *dx, int *ix, double *dy, int *iy)
{
    int m, i;
    double out = 0.0;

    /* Clean-up loop so remaining vector length is a multiple of 5.  */
    m = n % 5;
    if(m != 0){
       for(i=0; i<m; i++){
          out += pow(dx[ix[i]] * dy[iy[i]], 2);
       }
       if(n < 5){
          return(sqrt(out));
       }
    }
    for(i=m; i<n; i+=5)
    {
       out += pow(dx[ix[i]] * dy[iy[i]], 2);
       out += pow(dx[ix[i+1]] * dy[iy[i+1]], 2);
       out += pow(dx[ix[i+2]] * dy[iy[i+2]], 2);
       out += pow(dx[ix[i+3]] * dy[iy[i+3]], 2);
       out += pow(dx[ix[i+4]] * dy[iy[i+4]], 2);
    }

    return(sqrt(out));
}
