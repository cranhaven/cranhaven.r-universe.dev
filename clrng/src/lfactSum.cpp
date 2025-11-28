#include "gpuRandom.hpp"
// Important! this must be a matrix of integers! otherwise, wrong results!

//#define DEBUGKERNEL
template <typename T> 
std::string sum_of_LfactorialString(const int Nrow, const int Ncol, const int NpadCol) {  //internal column size
  
  std::string typeString = "int";
  std::string typeStringSum = openclTypeString<T>();  // type of the sum of log factorial

  std::string result = "";
  
  if(typeStringSum == "double") {
    result += "\n#pragma OPENCL EXTENSION cl_khr_fp64 : enable\n";
  }


  result +=
    "\n#define Nrow " + std::to_string(Nrow) + "\n"    
    "#define Ncol " + std::to_string(Ncol) + "\n"
    "#define NpadCol " + std::to_string(NpadCol) + "\n";    

  
  result += 
    "\n\n__kernel void sumLfactorial(\n"
    "  __global " + typeString + "* x,\n"  
    "  __global " + typeStringSum + "* result"  
    "){\n\n";  

// TO DO: use groups and local memory

  result += "int Drow, Dcol, Dindex;\n";
  result += typeStringSum + " Dresult=0;\n";
  result += typeStringSum + " insidevalue;\n";

  result += 
  "  for(Drow = get_global_id(0);   Drow < Nrow;    Drow+=get_global_size(0)){\n"
  "    for(Dcol = get_global_id(1),   Dindex = Drow*NpadCol+Dcol;\n" 
  "        Dcol < Ncol; Dcol+=get_global_size(1), Dindex+=get_global_size(1)){\n"
  "        insidevalue = 1 + x[Dindex];\n"
  "       Dresult += lgamma(insidevalue);\n"
  "    } // end loop through columns\n"
  "  } // end loop through rows\n";

  result += 
  "result[get_global_id(1) + get_global_id(0)*get_global_size(1)] = Dresult;\n";

  result += 
    "}//sumLfactorial kernel\n";

  return(result);
}










template<typename T> 
double logfactsum(
    viennacl::matrix<int> &x,// viennacl::vector_base<int>  rowSum, viennacl::vector_base<int>  colSum,  
    Rcpp::IntegerVector numWorkItems,
    int ctx_id) {

  double result;
  viennacl::vector_base<T> logFactorials(numWorkItems[0] * numWorkItems[1]);
  
#ifndef __APPLE__   
  std::string sumKernelString = sum_of_LfactorialString<T>(
    x.size1(), 
    x.size2(),
    x.internal_size2() 
  );
  
  // the context
  viennacl::ocl::switch_context(ctx_id);
  viennacl::ocl::program & my_prog = viennacl::ocl::current_context().add_program(sumKernelString, "my_kernel");
  
#ifdef DEBUGKERNEL
  Rcpp::Rcout << sumKernelString << "\n\n";
#endif  
 
  viennacl::ocl::kernel &sumLfactorialKernel = my_prog.get_kernel("sumLfactorial");
  sumLfactorialKernel.global_work_size(0, numWorkItems[0]);
  sumLfactorialKernel.global_work_size(1, numWorkItems[1]);
  
  sumLfactorialKernel.local_work_size(0, 1L);
  sumLfactorialKernel.local_work_size(1, 1L);

  viennacl::ocl::enqueue(sumLfactorialKernel(x, logFactorials) );
#endif   
 
  result = viennacl::linalg::sum(logFactorials);
  return result;
  
}



template<typename T> 
SEXP logfactsumTemplated(
    Rcpp::S4 xR,
    Rcpp::IntegerVector numWorkItems) {
  
  double result;

  const bool BisVCL=1;
  const int ctx_id = INTEGER(xR.slot(".context_index"))[0]-1;
  std::shared_ptr<viennacl::matrix<int> > x = getVCLptr<int>(xR.slot("address"), BisVCL, ctx_id);
  
  result = logfactsum<T>(*x, numWorkItems, ctx_id);
  
  return Rcpp::wrap(result);

}





//[[Rcpp::export]]
SEXP logfactsumBackend(
    Rcpp::S4 xR,
    Rcpp::IntegerVector numWorkItems) {
  
  SEXP result;

  Rcpp::traits::input_parameter< std::string >::type classVarR(RCPP_GET_CLASS(xR));
  std::string precision_type = (std::string) classVarR;
  
  if (precision_type == "ivclMatrix") {
#ifdef __APPLE__
    result = logfactsumTemplated<float>(xR, numWorkItems);
#else
    result = logfactsumTemplated<double>(xR, numWorkItems);
#endif
  } else {
    Rcpp::warning("class of param must be ivclMatrix\n\n");
    result = Rcpp::wrap(1L);
  }
  return result;
 
  }


















/*
//' @export
// [[Rcpp::export]]
SEXP colsumRowsumBackend(
    Rcpp::S4  x,
    Rcpp::S4  rowSum,
    Rcpp::S4  colSum,   
    Rcpp::IntegerVector numWorkItems) {
  
  double result;
   
  const bool BisVCL=1;
  const int ctx_id = INTEGER(x.slot(".context_index"))[0]-1;
  
  std::shared_ptr<viennacl::matrix<int> > xVcl = getVCLptr<int>(x.slot("address"), BisVCL, ctx_id);
  std::shared_ptr<viennacl::vector_base<int> > rowSumVcl = getVCLVecptr<int>(rowSum.slot("address"), BisVCL, ctx_id);
  std::shared_ptr<viennacl::vector_base<int> > colSumVcl = getVCLVecptr<int>(colSum.slot("address"), BisVCL, ctx_id);

  std::string sumKernelString = sum_of_LfactorialString(
    (*xVcl).size1(), 
    (*xVcl).size2(),
    (*xVcl).internal_size2() 
    );

    // the context
  viennacl::ocl::switch_context(ctx_id);
  viennacl::ocl::program & my_prog = viennacl::ocl::current_context().add_program(sumKernelString, "my_kernel");
  
  
  viennacl::ocl::kernel &sumKernel = my_prog.get_kernel("colsumRowsum");


  sumKernel.global_work_size(0, numWorkItems[0]);
  sumKernel.global_work_size(1, numWorkItems[1]);
  
  sumKernel.local_work_size(0, 1L);
  sumKernel.local_work_size(1, 1L);

  viennacl::ocl::enqueue(sumKernel(*xVcl, *rowSumVcl, *colSumVcl) );

  viennacl::ocl::kernel &sumLfactorialKernel = my_prog.get_kernel("sumLfactorial");
  sumLfactorialKernel.global_work_size(0, numWorkItems[0]);
  sumLfactorialKernel.global_work_size(1, numWorkItems[1]);
  
  sumLfactorialKernel.local_work_size(0, 1L);
  sumLfactorialKernel.local_work_size(1, 1L);

  viennacl::vector_base<double> logFactorial(numWorkItems[0] * numWorkItems[1]);

  viennacl::ocl::enqueue(sumLfactorialKernel(*xVcl, logFactorial) );


  result = viennacl::linalg::sum(logFactorial);

  return(Rcpp::wrap(result));

}

*/


