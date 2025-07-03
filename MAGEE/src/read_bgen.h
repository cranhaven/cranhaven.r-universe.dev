#ifndef READBGEN_H
#define READBGEN_H


void Bgen13GetTwoVals(const unsigned char* prob_start, uint32_t bit_precision, uintptr_t offset, uintptr_t* first_val_ptr, uintptr_t* second_val_ptr);

extern "C" 
{
  SEXP bgenHeader(SEXP bgenfile_in);
  SEXP getVariantPos(SEXP bgenfile_in, SEXP offset_in, SEXP mbgen_in, SEXP nbgen_in, SEXP compression_in, SEXP layout_in, SEXP cores_in);
  SEXP bgenVariantInfo(SEXP bgenfile_in, SEXP offset_in, SEXP mbgen_in, SEXP nbgen_in, SEXP layout_in, SEXP compression_in);
}


#endif



