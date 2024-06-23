#include "gpuRandom.hpp"
#include <CL/kernelqqnorm.hpp>


using namespace Rcpp;
using namespace viennacl;


//#define DEBUGKERNEL
// kernel adapted from      https://github.com/SurajGupta/r-source/blob/master/src/nmath/qnorm.c
//using namespace viennacl::linalg;

void gpu_qqnorm_0( 
    viennacl::vector_base<double> &out, // viennacl::vector_base<double> &p, 
    double mu,
    double sigma,
    int lowertail,
    IntegerVector numWorkItems,
    IntegerVector numLocalItems,
    const int ctx_id){
//#ifdef UNDEF  
  std::string kernel_string;
    
  kernel_string= qqnormkernelString;
  
  viennacl::ocl::context ctx(viennacl::ocl::get_context(ctx_id));

  viennacl::ocl::program &my_prog = ctx.add_program(kernel_string, "my_kernel");
  // get compiled kernel function
#ifdef DEBUGKERNEL
  Rcpp::Rcout << kernel_string << "\n\n";
#endif  
  
  
  viennacl::ocl::kernel &qqnorm = my_prog.get_kernel("qnorm");

//#ifdef UNDEF  
//  cl_device_type type_check = ctx.current_device().type();
  
  qqnorm.global_work_size(0, numWorkItems[0]);
  qqnorm.global_work_size(1, numWorkItems[1]);
  
  qqnorm.local_work_size(0, numLocalItems[0]);
  qqnorm.local_work_size(1, numLocalItems[1]);
  
  const int outsize = out.size();
  //viennacl::vector_base<double> out = viennacl::vector_base<double>(outsize, ctx); 

  viennacl::ocl::enqueue(qqnorm( out, mu, sigma, outsize, lowertail) );
//#endif  
  
}




// [[Rcpp::export]]
SEXP cpp_gpu_qqnorm(
    Rcpp::S4  outR,
    double mu,
    double sigma,
    int lowertail,
    Rcpp::IntegerVector max_global_size,
    Rcpp::IntegerVector max_local_size){
  
  const int ctx_id = INTEGER(outR.slot(".context_index"))[0]-1;
  

  std::shared_ptr<viennacl::vector_base<double> > out = getVCLVecptr<double>(outR.slot("address"), 1, ctx_id);
  
   gpu_qqnorm_0(*out, mu, sigma, lowertail, max_global_size, max_local_size, ctx_id);
  
  return outR;
  
}


















