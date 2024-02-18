#include "utils.h"
#include "TinyParallel.h"

using namespace Rcpp;


template <typename T>
struct ArrayShifter : public TinyParallel::Worker
{
  const SEXP& x;
  const SEXP& re;
  const R_xlen_t& alongIdx;
  const R_xlen_t& unitIdx;
  const R_xlen_t& leap;
  const T& na;
  const R_xlen_t& nThreads;
  const SEXP& idxBuf;

  const T* x_ptr;
  const T* re_ptr;
  const int* idx_ptr0;
  const int* xDim_ptr;
  const int* shiftAmount_ptr;
  R_xlen_t nDims;
  R_xlen_t xLen;
  R_xlen_t partitionLen;

  ArrayShifter(
    const SEXP& x,
    const SEXP& re,
    const SEXP& xDim,
    const R_xlen_t& alongIdx,
    const R_xlen_t& unitIdx,
    const R_xlen_t& leap,
    const T& na,
    const SEXP& shiftAmount_,
    const SEXP& idxBuf,
    const R_xlen_t& nThreads
  ): x(x), re(re), alongIdx(alongIdx), unitIdx(unitIdx), leap(leap),
  na(na), nThreads(nThreads), idxBuf(idxBuf) {
    this->x_ptr = get_sexp_pointer<T>(x);
    this->re_ptr = get_sexp_pointer<T>(re);
    this->idx_ptr0 = INTEGER(idxBuf);
    this->xDim_ptr = INTEGER(xDim);
    this->nDims = Rf_xlength(xDim);
    this->xLen = Rf_xlength(x);
    this->partitionLen = this->xLen / nThreads;
    if( this->partitionLen * nThreads < this->xLen ){
      this->partitionLen++;
    }
    this->shiftAmount_ptr = INTEGER(shiftAmount_);
  }

  void shift(R_xlen_t thread){
    // calculate start and end
    R_xlen_t begin = this->partitionLen * thread;
    R_xlen_t end = begin + this->partitionLen;
    if( end  > xLen ){ end = xLen; }
    if( end <= begin ){ return; }

    // Rcout << "Begin: " << begin << "; End: " << end << "\n";

    // ------------------------ Body part ------------------------
    // used to calculate locations
    // SEXP idx = PROTECT(Rf_allocVector(INTSXP, nDims));
    int* idx_ptr = (int*)this->idx_ptr0 + thread * nDims;
    int* xDim_ptr2 = (int*)this->xDim_ptr;
    R_xlen_t jj, trial, new_t;

    R_xlen_t ii = begin;
    R_xlen_t rem = ii;
    R_xlen_t rem2;
    for( jj = 0; jj < nDims; jj++, xDim_ptr2++ ){
      rem2 = rem / (R_xlen_t)(*xDim_ptr2);
      *(idx_ptr + jj) = rem - rem2 * (*xDim_ptr2);
      rem = rem2;
    }
    // xDim_ptr2 = (int*)xDim_ptr;
    // idx_ptr = (int*)this->idx_ptr0 + thread * nDims;
    *idx_ptr -= 1;

    int* shiftAmount_ptr2 = (int*)this->shiftAmount_ptr;

    const int* trial_ptr = idx_ptr + (unitIdx - 1);
    T* re_ptr2 = (T*)this->re_ptr + begin;

    for( R_xlen_t ii = begin; ii < end; ii++, re_ptr2++ ){

      // get_index(idx.begin(), begin, dims);
      // Calculate current index
      xDim_ptr2 = (int*)this->xDim_ptr;
      idx_ptr = (int*)this->idx_ptr0 + thread * nDims;
      *(idx_ptr) += 1;
      for( jj = 0; jj < nDims - 1 ; jj++, idx_ptr++, xDim_ptr2++ ){
        if( *idx_ptr == *xDim_ptr2 ){
          *idx_ptr = 0;
          *(idx_ptr + 1) += 1;
        }
      }
      idx_ptr = (int*)this->idx_ptr0 + (thread * nDims + alongIdx - 1);
      xDim_ptr2 = (int*)(xDim_ptr + (alongIdx - 1));

      // get trial index (starts from 0)
      trial = *trial_ptr;

      // shift time index
      shiftAmount_ptr2 = (int*)this->shiftAmount_ptr + trial;


      if(*shiftAmount_ptr2 == NA_INTEGER){
        *re_ptr2 = na;
      } else {
        new_t = *idx_ptr + *shiftAmount_ptr2;

        // check if idx[tidx] is too large or too small
        if( new_t >= 0 && new_t < *xDim_ptr2 ){
          *re_ptr2 = *(this->x_ptr + (ii + leap * *shiftAmount_ptr2));
        }else{
          *re_ptr2 = na;
        }

        // print(idxBuf);
        // Rcout << "  Trial: " << trial <<
        //   ", orig_idx: " << *idx_ptr <<
        //   ", shift: " << *shiftAmount_ptr2 <<
        //   ", new_t: " << new_t << "; value_idx: " << (ii + leap * *shiftAmount_ptr2)  <<
        //     "; value: " << *(this->x_ptr + (ii + leap * *shiftAmount_ptr2)) <<
        //       "; limit: " << *xDim_ptr2 << "\n";
      }

    }
  }

  void operator()(std::size_t begin, std::size_t end) {
    for(std::size_t thread = begin; thread < end; thread++){
      this->shift(thread);
    }
  }
};


template <typename T>
SEXP shiftArrayTemplate(const SEXP& x,
                const R_xlen_t& alongIdx,
                const R_xlen_t& unitIdx,
                const SEXP& shiftAmount,
                const T& na) {
  // along_idx along time
  // unit_idx trial index

  SEXP re = R_NilValue;
  SEXPTYPE xType = TYPEOF(x);

  R_xlen_t xLen = Rf_xlength(x);
  SEXP xDim = PROTECT(Rf_getAttrib(x, R_DimSymbol)); // TODO: check dim type in R
  R_xlen_t nDims = Rf_xlength(xDim);

  // check alongIdx and unitIdx
  if( alongIdx < 1 || alongIdx > nDims ){
    re = PROTECT(make_error("`alongIdx` must be an integer from 1 to nDims."));
    UNPROTECT(2); // re, xDim
    return(re);
  }

  if( unitIdx < 1 || unitIdx > nDims ){
    re = PROTECT(make_error("`unitIdx` must be an integer from 1 to nDims."));
    UNPROTECT(2); // re, xDim
    return(re);
  }

  if( unitIdx == alongIdx ){
    re = PROTECT(make_error("`unitIdx` cannot be the same as `alongIdx`"));
    UNPROTECT(2); // re, xDim
    return(re);
  }

  // check shiftAmount length
  R_xlen_t shiftAmountLen = Rf_xlength(shiftAmount);
  if( shiftAmountLen != *(INTEGER(xDim) + (unitIdx - 1)) ){
    re = PROTECT(make_error("`shiftAmount` must match with the dimension length along the unit margin."));
    UNPROTECT(2); // re, xDim
    return(re);
  }

  SEXP shiftAmount_;
  if(TYPEOF(shiftAmount) != INTSXP){
    shiftAmount_ = PROTECT(Rf_coerceVector(shiftAmount, INTSXP));
  } else {
    shiftAmount_ = PROTECT(shiftAmount);
  }

  SEXP xDimNames = PROTECT(Rf_getAttrib(x, R_DimNamesSymbol));

  PROTECT(re = Rf_allocVector(xType, xLen));
  Rf_setAttrib(re, R_DimSymbol, xDim);
  Rf_setAttrib(re, R_DimNamesSymbol, xDimNames);

  // Increment steps, depending on the time margin
  R_xlen_t leap = 1;
  for( R_xlen_t jj = 0; jj < alongIdx - 1; jj++ ){
    leap *= *(INTEGER(xDim) + jj);
  }


  // ------------------------ Body part ------------------------
  // used to calculate locations
  R_xlen_t nThreads = 8;
  SEXP idxBuf = PROTECT(Rf_allocVector(INTSXP, nDims * nThreads));


  ArrayShifter<T> shifter(x, re, xDim, alongIdx, unitIdx, leap,
                           na, shiftAmount_, idxBuf, nThreads);

  parallelFor(0, nThreads, shifter);

  UNPROTECT(5); // xDim, xDimNames, shiftAmount_, re, idx

  return(re);


}

// [[Rcpp::export]]
SEXP shiftArray(const SEXP& x,
                const R_xlen_t& alongIdx,
                const R_xlen_t& unitIdx,
                const SEXP& shiftAmount) {
  SEXP re;

  switch(TYPEOF(x)) {
  case INTSXP:
    PROTECT(re = shiftArrayTemplate<int>(x, alongIdx, unitIdx, shiftAmount, NA_INTEGER));
    break;
  case REALSXP:
    PROTECT(re = shiftArrayTemplate<double>(x, alongIdx, unitIdx, shiftAmount, NA_REAL));
    break;
  case RAWSXP:
    PROTECT(re = shiftArrayTemplate<Rbyte>(x, alongIdx, unitIdx, shiftAmount, 0));
    break;
  case LGLSXP:
    PROTECT(re = shiftArrayTemplate<int>(x, alongIdx, unitIdx, shiftAmount, NA_LOGICAL));
    break;
  case CPLXSXP:
    Rcomplex na;
    na.r = NA_REAL;
    na.i = NA_REAL;
    PROTECT(re = shiftArrayTemplate<Rcomplex>(x, alongIdx, unitIdx, shiftAmount, na));
    break;

  default:
    PROTECT(re = make_error("C++ `shiftArray`: Unsupported data type"));
  }

  UNPROTECT(1);
  return re;
}



/*** R
RcppParallel::ravetools_threads(1)
x <- matrix(as.double(1:10), nrow = 2, byrow = TRUE)
shift_array(x, 2, 1, c(1,2))

# shiftArray(array(as.double(1:12), c(4,3)), 2, 1, 0:3)
dim = c(100,10,300,20)
# dim <- c(4,3)
x = array(rnorm(prod(dim)), dim)
# time is dim 2
# trial is dim 1
tidx = 3
sidx = 1
shifts = sample(dim[3], dim[1])
shifts[1] = NA
f1 = function(){
  shiftArray(x, tidx, sidx, shifts)
}
f2 = function(){
  tm = seq_len(dim[3])
  re = sapply(seq_len(dim[1]), function(ii){
    shift = shifts[ii]
    new_idx = tm + shift
    new_idx[new_idx > dim[3]] = NA
    new_idx[new_idx <= 0] = NA
    x[ii,,new_idx,]
  })
  dim(re) = c(dim(x)[-1], dim[1])
  re = aperm(re, c(4,1,2,3))
  re
}

range(f2()-f1(), na.rm = TRUE)
# microbenchmark::microbenchmark({f1()}, {f2()}, times = 10)
*/
