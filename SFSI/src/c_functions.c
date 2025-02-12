#include "SFSI.h"
//#include "utils.c"

char* format_file_size(long long file_size)
{
  char *ns[4] = {"Gb", "Mb", "Kb", "bytes"};
  int k = 0;
  for(int i=0; i<4; i++){
    if(((double)file_size/pow(1000,3.0-i))>1){
      k = i;
      break;
    }
  }
  double fs = (double)file_size/pow(1000,3.0-k);
  char *out = (char*)malloc(100*sizeof(char));;
  snprintf(out, 100, "%.2f", fs);
  strcat(out, " ");
  strcat(out, ns[k]);

  return(out);
}

//====================================================================
// vartype: 1:integer, 2: logical, 3: double
//====================================================================
SEXP R_writeBinFile(SEXP filename_,
                    SEXP nrow_, SEXP ncol_, SEXP X_,
                    SEXP doubleprecision_,
                    SEXP verbose_)
{
    FILE *f = NULL;
    long long i, j, tmp;
    int varsize = 0, vartype = 0;
    float valuefloat;
    size_t cont;
    char *type = NULL;

    int nrow = INTEGER_VALUE(nrow_);
    int ncol = INTEGER_VALUE(ncol_);
    int doubleprecision = asLogical(doubleprecision_);
    int verbose = asLogical(verbose_);

    f = fopen(CHAR(STRING_ELT(filename_,0)),"wb");
    fwrite(&nrow, sizeof(int), 1, f);
    fwrite(&ncol, sizeof(int), 1, f);

    int ismatrix = ncol > 0 ? 1 : 0;
    ncol = (ncol == 0) ? 1 : ncol;

    cont = 0;
    if(TYPEOF(X_) == INTSXP || TYPEOF(X_) == LGLSXP)
    {
      vartype = TYPEOF(X_) ==  INTSXP ? 1 : 2;
      type = TYPEOF(X_) ==  INTSXP ? "integer" : "logical";
      //Rprintf(" Writting variable of '%s' type ...\n",type);

      PROTECT(X_ = AS_INTEGER(X_));
      int *X = INTEGER_POINTER(X_);   // An integer pointer is also used if is FLOAT
      varsize = sizeof(X[0]);
      fwrite(&vartype, sizeof(int), 1, f);
      fwrite(&varsize, sizeof(int), 1, f);

      //Rprintf(" Writting lines ...\n");
      for(j=0; j<ncol; j++){
        cont += fwrite(X + (long long)nrow*j, varsize, nrow, f);
      }

    }else{
      if(TYPEOF(X_) == REALSXP)
      {
        vartype = 3;
        type = "numeric";
        //Rprintf(" Writting variable of '%s' type ...\n",type);
        PROTECT(X_ = AS_NUMERIC(X_));
        double *X = NUMERIC_POINTER(X_);

        if(doubleprecision)
        {
          varsize = sizeof(X[0]);
          fwrite(&vartype, sizeof(int), 1, f);
          fwrite(&varsize, sizeof(int), 1, f);

          //Rprintf(" Writting lines ...\n");
          for(j=0; j<ncol; j++){
            cont += fwrite(X + (long long)nrow*j, varsize, nrow, f);
          }
        }else{
          varsize = sizeof(float);
          fwrite(&vartype, sizeof(int), 1, f);
          fwrite(&varsize, sizeof(int), 1, f);

          for(j=0; j<ncol; j++){
            for(i=0; i<nrow; i++){
              valuefloat = X[(long long)nrow*j + i];
              cont += fwrite(&valuefloat, varsize, 1, f);
            }
          }
        }

      }else{
        Rprintf(" File can not be saved with the current type format\n");
      }
    }
    fclose(f);
    //Rprintf(" Done with saving file ...\n",type);

    tmp = (long long)nrow*(long long)ncol;
    if(cont == tmp){
      if(verbose){
        tmp = tmp*varsize + 4*sizeof(int);  // File size

        Rprintf(" Saved file: '%s'\n",CHAR(STRING_ELT(filename_,0)));
        if(ismatrix){
          Rprintf(" Dimension: %d x %d\n",nrow, ncol);
        }else{
          Rprintf(" Dimension: %d\n",nrow);
        }
        Rprintf(" Data type: %s\n",type);
        Rprintf(" Data size: %d bytes\n",varsize);
        Rprintf(" File size: %s\n",format_file_size(tmp));
      }

    }else{
      Rprintf("  Error: The function failed to write data to file\n");
    }

    UNPROTECT(1);

    return(R_NilValue);
}

//====================================================================
// Functions used by the 'readBinary' routine
//====================================================================
void read_integer(FILE *f, long long start, long long ncol, long long nrow,
                  long long n, long long p,
                  int *X, int varsize, int nirow, int *irow,
                  int nicol, int *icol, int *status){
  long long i, j;
  int *line;
  size_t cont = 0;

  if(nirow>0){
   line = (int *) R_alloc(nrow, sizeof(int));
  }

  for(j=0; j<p; j++){
    if(nicol > 0){
      fseeko(f, start + nrow*(long long)varsize*(long long)icol[j], SEEK_SET);
    }

    if(nirow>0){
       cont += fread(line, varsize, nrow, f);
       for(i=0; i<n; i++){
         X[n*j + i] = line[irow[i]];
       }
    }else{
       cont += fread(X + n*j, varsize, nrow, f);
    }
  }
  status[0] = (cont == (nrow*p)) ? 0 : 1;
}

//====================================================================

void read_double(FILE *f, long long start, long long ncol, long long nrow,
                 long long n, long long p,
                 double *X, int varsize, int nirow, int *irow,
                 int nicol, int *icol, int *status){
  long long i, j;
  size_t cont = 0;
  double *linedouble=NULL;
  float *linefloat=NULL;

  if(varsize == sizeof(double)){
    if(nirow>0){
      linedouble = (double *) R_alloc(nrow, sizeof(double));
    }
  }else{
    linefloat = (float *) R_alloc(nrow, sizeof(float));
  }

  if(varsize == sizeof(double)){
    for(j=0; j<p; j++){
      if(nicol > 0){
        fseeko(f, start + nrow*(long long)varsize*(long long)icol[j], SEEK_SET);
      }

      if(nirow>0){
         cont += fread(linedouble, varsize, nrow, f);
         for(i=0; i<n; i++){
           X[n*j + i] = linedouble[irow[i]];
         }
      }else{
         cont += fread(X + n*j, varsize, nrow, f);
      }
    }
  }else{
    for(j=0; j<p; j++){
      if(nicol > 0){
        fseeko(f, start + nrow*(long long)varsize*(long long)icol[j], SEEK_SET);
      }

      cont += fread(linefloat, varsize, nrow, f);
      if(nirow>0){
        for(i=0; i<n; i++){
          X[n*j + i] = (double)linefloat[irow[i]];
        }
      }else{
        for(i=0; i<n; i++){
          X[n*j + i] = (double)linefloat[i];
        }
      }
    }
  }
  status[0] = (cont == (nrow*p)) ? 0 : 1;
}

//====================================================================
//====================================================================

SEXP R_readBinFile(SEXP filename_, SEXP irow_, SEXP icol_, SEXP drop_,
                   SEXP verbose_)
{
    FILE *f=NULL;
    int varsize, vartype;
    long long tmp;
    int nrow, ncol, n, p;
    size_t cont;
    char *type;
    int nprotect = 2;

    int drop = asLogical(drop_);
    int verbose = asLogical(verbose_);
    int nirow = Rf_isNull(irow_) ? 0 : XLENGTH(irow_);
    int nicol = Rf_isNull(icol_) ? 0 : XLENGTH(icol_);
    SEXP X_ = NULL;

    PROTECT(irow_ = AS_INTEGER(irow_));
    int *irow = INTEGER_POINTER(irow_);

    PROTECT(icol_ = AS_INTEGER(icol_));
    int *icol = INTEGER_POINTER(icol_);

    f = fopen(CHAR(STRING_ELT(filename_,0)),"rb");

    cont = fread(&nrow, sizeof(int), 1, f);
    cont += fread(&ncol, sizeof(int), 1, f);
    cont += fread(&vartype, sizeof(int), 1, f);
    cont += fread(&varsize, sizeof(int), 1, f);

    int ismatrix = ncol > 0 ? 1 : 0;
    ncol = (ncol == 0) ? 1 : ncol;

    if(cont != 4){
      Rprintf(" Error: The function failed to read data information\n");
      UNPROTECT(nprotect);
      return(R_NilValue);
    }

    int start = 4*sizeof(int);
    fseeko(f, 0, SEEK_END);
    if(((long long)nrow*(long long)ncol*(long long)varsize + start) != ftello(f)){
      Rprintf(" Error: file does not have %d x %d (nrows x ncols) elements\n",nrow,ncol);
      UNPROTECT(nprotect);
      return(R_NilValue);
    }
    fseeko(f, start, SEEK_SET);

    // Check if any index is larger than n or p
    if(nirow > 0){
       tmp =  irow[imax_integer(nirow, irow)]+1;
       //Rprintf("  Max index row=%d\n",itmp);
       if(tmp > nrow){
         Rprintf(" Error: row %lld can not be read, file contains only %d rows\n",tmp,nrow);
         UNPROTECT(nprotect);
         return(R_NilValue);
       }
    }
    if(nicol > 0){
       tmp = icol[imax_integer(nicol, icol)]+1;
       //Rprintf("  Max index column=%d\n",itmp);
       if(tmp > ncol){
         Rprintf(" Error: column %lld can not be read, file contains only %d column\n",tmp,ncol);
         UNPROTECT(nprotect);
         return(R_NilValue);
       }
    }

    n = nirow == 0 ? nrow : nirow;
    p = nicol == 0 ? ncol : nicol;
    //Rprintf("  To read: n=%d, p=%d, vartype=%d, varsize=%d\n",n,p,vartype,varsize);

    int status = 1;
    // vartype: 1:integer, 2: logical, 3: double
    if(vartype == 1 || vartype == 2)  // INTEGER|LOGICAL
    {
      if(vartype == 1)
      {
        if(ismatrix)
        {
          if((n==1) || (p==1))
          {
            if(drop){
              X_ = PROTECT(Rf_allocVector(INTSXP, (long long)n*(long long)p));
            }else{
              X_ = PROTECT(Rf_allocMatrix(INTSXP, n, p));
            }
          }else{
            X_ = PROTECT(Rf_allocMatrix(INTSXP, n, p));
          }
        }else{
          X_ = PROTECT(Rf_allocVector(INTSXP, n));
        }
        type = "integer";
      }else{
        if(ismatrix)
        {
          if((n==1) || (p==1))
          {
            if(drop){
              X_ = PROTECT(Rf_allocVector(LGLSXP, (long long)n*(long long)p));
            }else{
              X_ = PROTECT(Rf_allocMatrix(LGLSXP, n, p));
            }
          }else{
            X_ = PROTECT(Rf_allocMatrix(LGLSXP, n, p));
          }
        }else{
          X_=PROTECT(Rf_allocVector(LGLSXP, n));
        }
        type = "logical";
      }
      nprotect++;

      int *X = INTEGER_POINTER(X_);
      read_integer(f, start, ncol, nrow, n, p, X,
                   varsize, nirow, irow, nicol, icol, &status);

    }else{
      if(vartype == 3)  // DOUBLE
      {
        type = "numeric";
        if(ismatrix)
        {
          if((n==1) || (p==1))
          {
            if(drop){
              X_ = PROTECT(Rf_allocVector(REALSXP, (long long)n*(long long)p));
            }else{
              X_ = PROTECT(Rf_allocMatrix(REALSXP, n, p));
            }
          }else{
            X_ = PROTECT(Rf_allocMatrix(REALSXP, n, p));
          }
        }else{
          X_ = PROTECT(Rf_allocVector(REALSXP, n));
        }
        nprotect++;

        double *X = NUMERIC_POINTER(X_);
        read_double(f, start, ncol, nrow, n, p, X,
                    varsize, nirow, irow, nicol, icol, &status);

      }else{
        Rprintf(" Error: File can not be read with the current type format\n");
      }
    }
    fclose(f);

    if(status == 0){
      if(verbose){
        tmp = (long long)nrow*(long long)ncol*(long long)varsize + start;
        Rprintf(" Loaded file: '%s'\n",CHAR(STRING_ELT(filename_,0)));
        if(ismatrix){
          Rprintf(" Dimension: %d x %d\n",n, p);
        }else{
          Rprintf(" Dimension: %d\n",n);
        }
        Rprintf(" Data type: %s\n",type);
        Rprintf(" Data size: %d bytes\n",varsize);
        Rprintf(" File size: %s\n",format_file_size(tmp));
      }
      UNPROTECT(nprotect);
      return(X_);

    }else{
      UNPROTECT(nprotect);
      return(R_NilValue);
    }
}
