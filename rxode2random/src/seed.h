#ifndef __SEED_H__
#define __SEED_H__
#include "../inst/include/rxode2random.h"

#if defined(__cplusplus)
extern "C" {
#endif
  uint32_t getRxSeed1(int ncores);
  void setSeedEng1(uint32_t seed);
  SEXP _rxSetSeed(SEXP intIn);
  void setRxSeedFinal(uint32_t seed);
  void seedEng(int ncores);
#if defined(__cplusplus)
}
#endif

#endif // __SEED_H__
