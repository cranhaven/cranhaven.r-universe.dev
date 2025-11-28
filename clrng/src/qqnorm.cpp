#include "gpuRandom.hpp"
#include <CL/kernelqqnorm.hpp>


using namespace Rcpp;
using namespace viennacl;


//#define DEBUGKERNEL
// kernel adapted from      https://github.com/SurajGupta/r-source/blob/master/src/nmath/qnorm.c
//using namespace viennacl::linalg;

template<typename T> 
void gpu_qqnorm_0( 
    viennacl::vector_base<T> &out, // viennacl::vector_base<double> &p, 
    double mu,
    double sigma,
    int lowertail,
    IntegerVector numWorkItems,
    IntegerVector numLocalItems,
    const int ctx_id){
#ifndef __APPLE__    
  std::string kernel_string;
    
  kernel_string= qqnormkernelString<T>();
  
  viennacl::ocl::context ctx(viennacl::ocl::get_context(ctx_id));

  viennacl::ocl::program &my_prog = ctx.add_program(kernel_string, "my_kernel");
  // get compiled kernel function
#ifdef DEBUGKERNEL
  Rcpp::Rcout << kernel_string << "\n\n";
#endif  
  
  
  viennacl::ocl::kernel &qqnorm = my_prog.get_kernel("qnorm");

  qqnorm.global_work_size(0, numWorkItems[0]);
  qqnorm.global_work_size(1, numWorkItems[1]);
  
  qqnorm.local_work_size(0, numLocalItems[0]);
  qqnorm.local_work_size(1, numLocalItems[1]);
  const int outsize = out.size();
  //viennacl::vector_base<double> out = viennacl::vector_base<double>(outsize, ctx); 
  viennacl::ocl::enqueue(qqnorm( out, mu, sigma, outsize, lowertail) );
#endif
//#endif  
  
}




template<typename T> 
SEXP cpp_gpu_qqnorm_Templated(
    Rcpp::S4  outR,
    double mu,
    double sigma,
    int lowertail,
    Rcpp::IntegerVector max_global_size,
    Rcpp::IntegerVector max_local_size){
  
   const int ctx_id = INTEGER(outR.slot(".context_index"))[0]-1;
   std::shared_ptr<viennacl::vector_base<T> > out = getVCLVecptr<T>(outR.slot("address"), 1, ctx_id);
   gpu_qqnorm_0<T>(*out, mu, sigma, lowertail, max_global_size, max_local_size, ctx_id);
  
  return outR;
  
}





// [[Rcpp::export]]
SEXP cpp_gpu_qqnorm(
    Rcpp::S4  outR,
    double mu,
    double sigma,
    int lowertail,
    Rcpp::IntegerVector max_global_size,
    Rcpp::IntegerVector max_local_size){
  SEXP result;
  
  Rcpp::traits::input_parameter< std::string >::type classInput(RCPP_GET_CLASS(outR));
  std::string classInputString = (std::string) classInput;
  
  if(classInputString == "dvclVector") {
    result = cpp_gpu_qqnorm_Templated<double>(outR, mu, sigma, lowertail, max_global_size, max_local_size);
  }else if(classInputString == "fvclVector"){
    result = cpp_gpu_qqnorm_Templated<float>(outR, mu, sigma, lowertail, max_global_size, max_local_size);
  }else{
    result = Rcpp::wrap(1L);
  }
  return(result);
  
}












