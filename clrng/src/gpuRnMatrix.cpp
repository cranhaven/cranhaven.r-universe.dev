#include "gpuRandom.hpp"

//#define DEBUG
//#define DEBUGKERNEL



using namespace Rcpp;
using namespace viennacl;	
using namespace viennacl::linalg;




template <typename T> 
std::string mrg31k3pMatrixString(
    const int Nrow, 
    const int Ncol,
    const int NpadCol,
    const int NpadStreams,
    const std::string random_type) { 
  
  std::string typeString = openclTypeString<T>();
  std::string result = "";
  
  if(typeString == "double") {
    
    result += "\n#pragma OPENCL EXTENSION cl_khr_fp64 : enable\n"
    "#define PI_2 M_PI_2\n"
    "#define TWOPI 6.283185307179586231996\n" 
    "#define mrg31k3p_NORM_cl 4.656612873077392578125e-10\n"
    "//TWOPI * mrg31k3p_NORM_cl\n"
    "#define TWOPI_mrg31k3p_NORM_cl 2.925836158534319248049e-09\n\n";
    
  } else if(typeString == "float") {
    result += "\n#define TWOPI 6.28318530717\n" 
    "#define PI_2 M_PI_2_F\n"
    "\n#define mrg31k3p_NORM_cl 4.6566126e-10\n\n"
    "//TWOPI * mrg31k3p_NORM_cl\n"
    "#define TWOPI_mrg31k3p_NORM_cl 2.9258361e-09\n\n";
  } else if (typeString == "int"){
    result += 
      "\n#define mrg31k3p_NORM_cl 1L\n";    
  } 
  // else if (random_type == "exponential"){
  //   result +=
  //  "#define mrg31k3p_NORM_cl 4.656612875245796923096e-10\n";
  // }
  

  result +=
    "\n#define Nrow " + std::to_string(Nrow) + "\n"    
    "#define Ncol " + std::to_string(Ncol) + "\n"
    "#define NpadStreams " + std::to_string(NpadStreams) + "\n"
    "#define NpadCol " + std::to_string(NpadCol) + "\n";    
  
  result += mrg31k3pString();
  
  result += 
    "\n\n__kernel void mrg31k3pMatrix(\n"
    "  __global int* streams,\n" 
    "  __global " + typeString + "* out){\n\n";  
  
  
  result += 
    "const int index = get_global_id(0)*get_global_size(1) + get_global_id(1);\n";
  
  result += "int Drow, Dcol, DrowStart, Dentry, DrowBlock, DcolBlock, DrowInBounds;\n";
  
  result += "const int DrowStartInc = get_global_size(0) * NpadCol;\n";
  
  result += "uint g1[3], g2[3];\n"
  "const int startvalue=index * NpadStreams;\n";

  result += typeString + " temp;\n";

  if(random_type == "normal"){
    result += "const " + typeString + " fact[2] = { mrg31k3p_NORM_cl, TWOPI * mrg31k3p_NORM_cl };\n";
    result += "const " + typeString + " addForSine[2] = { 0.0, - PI_2 };\n";
    result += 
      "local " + typeString + " part[2];\n";// local size must be X,2
  } else if (typeString == "int"){
    result += "const int fact = 1;\n";
  } else {
    result += "const " + typeString + " fact = mrg31k3p_NORM_cl;\n";
  }

  result += "streamsToPrivate(streams,g1,g2,startvalue);\n";
  
  result += 
    "for(DrowBlock = 0, Drow=get_global_id(0), DrowStart = Drow * NpadCol;\n" 
    "    DrowBlock < Nrow;\n"
    "    Drow += get_global_size(0), DrowBlock +=get_global_size(0), DrowStart += DrowStartInc) {\n";
  
  result+= 
    "    DrowInBounds = Drow < Nrow;\n";
  
  result += 
    "    for(DcolBlock = 0, Dcol=get_global_id(1), Dentry = DrowStart + Dcol;\n" 
    "        DcolBlock < Ncol;\n"
    "        DcolBlock += get_global_size(1), Dentry += get_global_size(1) ) {\n";

  if(random_type == "normal"){  
    result += "      part[get_local_id(1)] = fact[get_local_id(1)] * clrngMrg31k3pNextState(g1, g2);\n";
    result += 
      "      barrier(CLK_LOCAL_MEM_FENCE);\n"
      "      temp = sqrt( -2.0*log(part[0]) ) * cos(part[1] + addForSine[get_local_id(1)] );// is cos for local0, sine for local1\n";
  } else if(random_type == "exponential"){
    result += "      temp = - log(fact * clrngMrg31k3pNextState(g1, g2));\n";
  } else {
    result += "      temp = fact * clrngMrg31k3pNextState(g1, g2);\n";
  }
  
  
  result += 
    "        if(DrowInBounds) out[Dentry] = temp;\n";
  
  
  if (random_type == "normal"){ 
    result+=
      "      barrier(CLK_LOCAL_MEM_FENCE);\n";
  }
  

  
  
  result += 
    "  }//Dcol\n";
  result += 
    "}//Drow\n";
  
  result += "streamsFromPrivate(streams,g1,g2,startvalue);\n";
  

  result += 
  "}//kernel\n";
  
  return(result);
}


template<typename T>
int gpuMatrixRn(
    viennacl::matrix<T> &x,
    viennacl::matrix<int> &streams,
    const IntegerVector numWorkItems,
    const int ctx_id,
    const std::string random_type, 
    Rcpp::IntegerVector verbose){
  
  
  std::string mrg31k3pkernelString = mrg31k3pMatrixString<T>(
    x.size1(),
    x.size2(),
    x.internal_size2(),
    streams.internal_size2(),
    random_type);
  
  // if(numWorkItems[1] < 2) {
  //   Rcpp::warning(
  //     "number of work items needs to be an even number for second dimension\n");
  // }  
  // if(numWorkItems[1] * (x.internal_size2()/numWorkItems[1]) != x.internal_size2()) {
  //   Rcpp::warning(
  //     "number of work items in dimension 2 must be a divisor of internal size 2 of x\n");
  // }  
  
if(verbose[0]>1) {
  Rcpp::Rcout << mrg31k3pkernelString << "\n\n";
}
    
  // the context
  viennacl::ocl::switch_context(ctx_id);
  viennacl::ocl::program & my_prog = viennacl::ocl::current_context().add_program(mrg31k3pkernelString, "my_kernel");
  
  viennacl::ocl::kernel &random_number = my_prog.get_kernel("mrg31k3pMatrix");
  
  random_number.global_work_size(0, numWorkItems[0]);
  random_number.global_work_size(1, numWorkItems[1]);
  
  random_number.local_work_size(0, 1L);
  random_number.local_work_size(1, 2L);
  
  viennacl::ocl::command_queue theQueue = random_number.context().get_queue();
  
#ifndef __APPLE__  
  viennacl::ocl::enqueue(random_number(streams, x), theQueue);
  clFinish(theQueue.handle().get());
#endif  
  
  return(0L);
}



template<typename T> 
SEXP gpuRnMatrixTyped(
    Rcpp::S4  xR,
    Rcpp::S4  streamsR,
    Rcpp::IntegerVector max_global_size,
    std::string  random_type, 
    Rcpp::IntegerVector verbose) 
{
  
  const bool BisVCL=1;
  const int ctx_id = INTEGER(xR.slot(".context_index"))[0]-1;
  
  std::shared_ptr<viennacl::matrix<T> > x = getVCLptr<T>(xR.slot("address"), BisVCL, ctx_id);
  std::shared_ptr<viennacl::matrix<int> > streams = getVCLptr<int>(streamsR.slot("address"), BisVCL, ctx_id);
  
  
  return(Rcpp::wrap(
      gpuMatrixRn<T>(*x, *streams, max_global_size, 
                     ctx_id, random_type, verbose)
           ));	
  
}





// [[Rcpp::export]]
SEXP gpuRnBackend(
    Rcpp::S4  x,
    Rcpp::S4  streams,
    IntegerVector max_global_size,
    std::string  random_type,
    IntegerVector verbose) {
  
  SEXP result;
  

  
  Rcpp::traits::input_parameter< std::string >::type classInput(RCPP_GET_CLASS(x));
  std::string classInputString = (std::string) classInput;
  
  
  
  if(classInputString == "fvclMatrix") {
    result = gpuRnMatrixTyped<float>(x, streams, max_global_size, random_type, verbose);
  } else if (classInputString == "dvclMatrix") {
    result = gpuRnMatrixTyped<double>(x, streams, max_global_size, random_type, verbose);
  } else if (classInputString == "ivclMatrix") {
    result = gpuRnMatrixTyped<int>(x, streams, max_global_size, random_type, verbose);
  } else {
    result = Rcpp::wrap(1L);
  }
  return(result);
}






