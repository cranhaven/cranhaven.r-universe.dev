#include "tensorEVD.h"

//==============================================================
// Calculate the entry-wise sum
//     a*A[irowA,icolA] + b*B[irowB,icolB]
// A is of dimensions: nrowA,ncolA
// B is of dimensions: nrowB,ncolB
//==============================================================
SEXP R_sumvec(SEXP a_, SEXP nrowA_, SEXP ncolA_, SEXP A_,
              SEXP b_, SEXP nrowB_, SEXP ncolB_, SEXP B_,
              SEXP irowA_, SEXP icolA_,
              SEXP irowB_, SEXP icolB_,
              SEXP out_, SEXP drop_,
              SEXP makedimnames_, SEXP inplace_)
{
    int nprotect = 4;

    int nrowA = INTEGER_VALUE(nrowA_);
    //int ncolA = INTEGER_VALUE(ncolA_);
    int nrowB = INTEGER_VALUE(nrowB_);
    //int ncolB = INTEGER_VALUE(ncolB_);
    double a = NUMERIC_VALUE(a_);
    double b = NUMERIC_VALUE(b_);
    int drop = asLogical(drop_);
    int makedimnames = asLogical(makedimnames_);
    int inplace = INTEGER_VALUE(inplace_);

    PROTECT(A_ = AS_NUMERIC(A_));
    double *A = NUMERIC_POINTER(A_);

    PROTECT(B_ = AS_NUMERIC(B_));
    double *B = NUMERIC_POINTER(B_);

    int nrow = Rf_length(irowA_);

    PROTECT(irowA_ = AS_INTEGER(irowA_));
    int *irowA = INTEGER_POINTER(irowA_);

    PROTECT(irowB_=AS_INTEGER(irowB_));
    int *irowB = INTEGER_POINTER(irowB_);

    int ncol;
    int *icolA, *icolB;
    if(Rf_length(icolA_) == 0){
      icolA = irowA;
      ncol = nrow;
    }else{
      ncol = Rf_length(icolA_);

      PROTECT(icolA_ = AS_INTEGER(icolA_));
      icolA = INTEGER_POINTER(icolA_);
      nprotect++;
    }

    if(Rf_length(icolB_) == 0){
      icolB = irowB;
    }else{
      PROTECT(icolB_ = AS_INTEGER(icolB_));
      icolB = INTEGER_POINTER(icolB_);
      nprotect++;
    }

    // Rprintf(" Allocating memory for Hadamard product ...\n");
    SEXP out2_;
    int ismatrix = 1;
    double *out2;
    if(inplace == 0){
      // Rprintf(" New memory for a %d x %d matrix\n",nrow,ncol);
      if((nrow==1) || (ncol==1))
      {
        if(drop){
          out2_ = PROTECT(Rf_allocVector(REALSXP, (long long)nrow*ncol));
          ismatrix = 0;
        }else{
          out2_ = PROTECT(Rf_allocMatrix(REALSXP, nrow, ncol));
        }
      }else{
        out2_ = PROTECT(Rf_allocMatrix(REALSXP, nrow, ncol));
      }
      out2 = NUMERIC_POINTER(out2_);
      nprotect++;

    }else{
      //out2_ = R_NilValue;
      // Rprintf(" Memory from input %d\n",inplace);
      if(inplace == 1){
        out2 = A;
        out2_ = A_;
      }else{ // inplace==2
        out2 = B;
        out2_ = B_;
      }
    }

    // Rprintf(" Making the sum ...\n");
    size_t j;
    for(j=0; j<ncol; j++){
      sum_set(nrow, &a, A + (long long)nrowA*icolA[j], irowA, &b, B + (long long)nrowB*icolB[j], irowB, out2 + nrow*j);
    }

    if(ismatrix && makedimnames && (inplace==0)){
      // Rprintf(" Making dimnames ...\n");
      SEXP dimnames_ = PROTECT(Rf_allocVector(VECSXP, 2));
      SEXP dimnamesA_ = PROTECT(Rf_getAttrib(A_, R_DimNamesSymbol));
      SEXP dimnamesB_ = PROTECT(Rf_getAttrib(B_, R_DimNamesSymbol));
      get_dimnames(nrow, ncol, irowA, irowB, NULL, icolA, icolB, NULL,
                   dimnamesA_, dimnamesB_, dimnames_);
      Rf_setAttrib(out2_, R_DimNamesSymbol, dimnames_);
      nprotect += 3;
    }

    UNPROTECT(nprotect);
    return(out2_);
}
