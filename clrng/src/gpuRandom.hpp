#include <Rcpp.h>
#include <string>

//#include <clRNG/clRNG.h>
//#include <clRNG/mrg31k3p.h>
//#include <dynMatrix/dynVCLMatGeostatsgpu.hpp>
//#include <dynMatrix/dynVCLVecGeostatsgpu.hpp>
//#include <gpuR/dynVCLMat.hpp>
//#include <gpuR/dynVCLVec.hpp>

#include <gpuR/utils.hpp>
#include <gpuR/getVCLptr.hpp>
#include "viennacl/linalg/sum.hpp"
#include "viennacl/ocl/backend.hpp"
#ifdef __APPLE__
#include <TargetConditionals.h>
#endif
//from typeDef.cpp
template <typename T> std::string openclTypeString();



std::string mrg31k3pString();


double logfactsum(
    viennacl::matrix<int>  &x, 
    Rcpp::IntegerVector numWorkItems,
    int ctx_id);

























