#include <Rcpp.h>
#include "pow4inthead.h"
using namespace Rcpp;

int pow4int(int K) {
  int x = 0;
  switch(K) {
  case 0 : x = 1;
    break;
  case 1 : x = 4;
    break;
  case 2 : x = 16;
    break;
  case 3 : x = 64;
    break;
  case 4 : x = 256;
    break;
  case 5 : x = 1024;
    break;
  case 6 : x = 4096;
    break;
  case 7 : x = 16384;
    break;
  case 8 : x = 65536;
    break;
  case 9 : x = 262144;
    break;
  case 10 : x = 1048576;
    break;
  case 11 : x = 4194304;
    break;
  case 12 : x = 16777216;
    break;
  case 13 : x = 67108864;
    break;
  case 14 : x = 268435456;
    break;
  case 15 : x = 1073741824;
    break;
  }
  return x;
}
