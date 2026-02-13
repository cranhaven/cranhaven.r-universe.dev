#include "tensorEVD.h"

//==============================================================
//  d1, V1:       Eigenvectors (n1 x nPC1) and eigenvalues of K1
//  d2, V2:       Eigenvectors (n2 x nPC2) and eigenvalues of K2
//  minvalue:  Minimum acepted value for the kronecker eigenvalues
//             (i.e., valuesK1[i]*valuesK2[j])
//  index1:   Which elements from K1 are included in the tensor, zero based
//  index2:   which elements from K2 are included in the tensor, zero based
//  alpha:     Maximum percentage of variance explained
//==============================================================
SEXP R_tensor_evd(SEXP n_, SEXP n1_, SEXP nPC1_, SEXP n2_, SEXP nPC2_,
                  SEXP d1_, SEXP V1_,
                  SEXP d2_, SEXP V2_,
                  SEXP minvalue_, SEXP index1_, SEXP index2_,
                  SEXP alpha_,
                  SEXP makedimnames_, SEXP verbose_)
{
    int nprotect = 10;

    int n = INTEGER_VALUE(n_);
    int n1 = INTEGER_VALUE(n1_);
    int nPC1 = INTEGER_VALUE(nPC1_);
    int n2 = INTEGER_VALUE(n2_);
    int nPC2 = INTEGER_VALUE(nPC2_);
    double minvalue = NUMERIC_VALUE(minvalue_);
    double alpha = NUMERIC_VALUE(alpha_);
    int makedimnames = asLogical(makedimnames_);
    int verbose = asLogical(verbose_);
    double eps = DBL_EPSILON*100;

    PROTECT(V1_ = AS_NUMERIC(V1_));
    double *V1 = NUMERIC_POINTER(V1_);

    PROTECT(V2_ = AS_NUMERIC(V2_));
    double *V2 = NUMERIC_POINTER(V2_);

    PROTECT(d1_ = AS_NUMERIC(d1_));
    double *d1 = NUMERIC_POINTER(d1_);

    PROTECT(d2_ = AS_NUMERIC(d2_));
    double *d2 = NUMERIC_POINTER(d2_);

    PROTECT(index1_ = AS_INTEGER(index1_));
    int *index1 = INTEGER_POINTER(index1_);

    PROTECT(index2_ = AS_INTEGER(index2_));
    int *index2 = INTEGER_POINTER(index2_);

    int nmap = nPC1*nPC2;
    double *d = (double *) R_alloc(nmap, sizeof(double));
    double *w0 = (double *) R_alloc(nmap, sizeof(double));
    double *cumvar = (double *) R_alloc(nmap, sizeof(double));
    int *order = (int *) R_alloc(nmap, sizeof(int));
    int *K1i = (int *) R_alloc(nmap, sizeof(int));
    int *K2i = (int *) R_alloc(nmap, sizeof(int));

    // Get the PC's variances and total variance from the full Kronecker
    // The full design Kronecker has nK1*nK2 rows, positions will be saved in K1i and K2i
    // The variance of the tensor PCs are obtained as the product valuesK1[i]*valuesK2[j]
    // They will be descendantly sorted to return the one with highest variance first
    // until the one that jointly explains certain proportion of total variance
    if(verbose){
      Rprintf(" EVD of K1: n1=%d loadings and nPC1=%d eigenvectors\n",n1,nPC1);
      Rprintf(" EVD of K2: n2=%d loadings and nPC2=%d eigenvectors\n",n2,nPC2);
      Rprintf(" Calculating N=%d (nPC1 x nPC2) tensor variances ...\n",nmap);
    }
    double totalvar = 0;
    int cont = 0;
    long long i, j;
    for(i=0; i<nPC1; i++)  // loop over the rows of MAP
    {
      for(j=0; j<nPC2; j++)
      {
        K1i[cont] = (int) i;  // Storage the Kronecker positions for K1
        K2i[cont] = (int) j;  // Storage the Kronecker positions for K2

        // Get the norms of the Hadamard eigenvectors
        w0[cont] = dnorm_hadam_set(n, V1 + n1*i, index1, V2 + n2*j, index2);

        // Derive and scale kronecker eigenvalues by multiplying by the squared norm
        d[cont] = d1[i]*d2[j]*pow(w0[cont],2);
    	  totalvar += d[cont];

        // Keep track of the ordered variances
        append_to_sorted_vector(cont, d, order);
        cont++;
      }
    }

    // Rprintf(" Get the number of PC that accumulated certain variance ...\n");
    double cumvar0 = 0; //d[order[0]]/totalvar;
    double mindif = fabs(cumvar0-alpha);
    int nd = nmap;  // N positive tensor eigenvalues
    for(i=0; i<nmap; i++){  // loop over the positive ones to get the minimum
      if(d[order[i]] < minvalue){
        nd = (int) i;
        if(verbose){
          Rprintf(" Dropped bottom %d of %d eigenvectors with eigenvalue smaller than %.5e\n",nmap-nd,nmap,minvalue);
        }
        break;
      }else{
        cumvar[i] = cumvar0 + d[order[i]]/totalvar;
        if(fabs(cumvar[i]-alpha) < mindif){
          mindif = fabs(cumvar[i]-alpha);
        }
        cumvar0 = cumvar[i];
      }
    }
    // Rprintf(" Minimum diff fabs(cumvar[i]-alpha)=%g\n",mindif);

    int nPC = 0;
    for(i=0; i<nd; i++){
      if(fabs(fabs(cumvar[i]-alpha)-mindif) <= eps){
        nPC = (int) i + 1;
        break;
      }
    }

    if(verbose){
      Rprintf(" Top %d of %d eigenvectors explain %.1f %% of the variance=%f\n",nPC,nmap,100*cumvar[nPC-1],totalvar);
    }

    if(verbose){
      Rprintf(" Obtaining tensor eigenvectors ...\n");
    }
    // The ith Tensor Eigenvector is formed by the product of the corresponding
    // Eigenvectors of K1 and K2 (positions in K1i and K2i) that were previously
    // sorted by the Tensor variance.
    // Only those entries of the selected Eigenvectors of K1 and K2 that are in
    // the Tensor (provided by indexK1 and indexK2) will enter in the dot product
    // Output objects
    SEXP vectors_ = PROTECT(Rf_allocMatrix(REALSXP, n, nPC));
    double *vectors = NUMERIC_POINTER(vectors_);

    SEXP values_ = PROTECT(Rf_allocVector(REALSXP, nPC));
    double *values = NUMERIC_POINTER(values_);

    double w;
    for(i=0; i<nPC; i++)  // loop over the rows of the (ordered) MAP
    {
      w = 1/w0[order[i]];
      values[i] = d[order[i]];
      // Derive the ith column Hadamard using selected kronecker eigenvectors and scale it by dividing by w
      hadam_set(n, &w, V1 + (long long)n1*K1i[order[i]], index1, V2 + (long long)n2*K2i[order[i]], index2, vectors + n*i);
    }

    if(verbose){
      Rprintf(" Done!\n");
    }

    // Set dimnames for vectors
    if(makedimnames){
      SEXP dimnames_ = PROTECT(Rf_allocVector(VECSXP, 2));
      SEXP dimnamesV1_ = PROTECT(Rf_getAttrib(V1_, R_DimNamesSymbol));
      SEXP dimnamesV2_ = PROTECT(Rf_getAttrib(V2_, R_DimNamesSymbol));
      get_dimnames(n, nPC, index1, index2, NULL, K1i, K2i, order,
                   dimnamesV1_, dimnamesV2_, dimnames_);
      Rf_setAttrib(vectors_, R_DimNamesSymbol, dimnames_);
      nprotect += 3;
    }

    SEXP list_ = PROTECT(Rf_allocVector(VECSXP, 3));
    SET_VECTOR_ELT(list_, 0, values_);
    SET_VECTOR_ELT(list_, 1, vectors_);
    SET_VECTOR_ELT(list_, 2, ScalarReal(totalvar));

    // Set dimnames for outputs
    SEXP names_ = PROTECT(Rf_allocVector(VECSXP, 3));
    SET_VECTOR_ELT(names_, 0, mkChar("values"));
    SET_VECTOR_ELT(names_, 1, mkChar("vectors"));
    SET_VECTOR_ELT(names_, 2, mkChar("totalVar"));
    Rf_setAttrib(list_, R_NamesSymbol, names_);

    UNPROTECT(nprotect);

    return(list_);
}
