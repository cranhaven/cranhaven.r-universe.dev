/* Algorithm AS 159 Applied Statistics (1981), vol. 30, no. 1
 original (C) Royal Statistical Society 1981
 
 Generate random two-way table with given marginal totals.
 
 Heavily pretty edited by Martin Maechler, Dec 2003
 use double precision for integer multiplication (against overflow);
 */

/*
 * the original files have been largely adapted
 */


#include "gpuRandom.hpp"
#include <R.h>
//#define DEBUGKERNEL

using namespace Rcpp;
using namespace viennacl; 
using namespace viennacl::linalg;




#define DOUBLE_EPS 2.220446049250313080847e-16

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





template <typename T> 
std::string logfactString() {  
  
  std::string typeString = openclTypeString<T>();  // type of the log factorial
  std::string result = "";
  
  if(typeString == "double") {
    result += "\n#pragma OPENCL EXTENSION cl_khr_fp64 : enable\n";
  }
  
  result += 
    "\n\n__kernel void logfactorial(\n"  // "  __global int *vector,\n"
    "  __global " + typeString + "* out,\n"  
    "  const int numElements\n"
    "){\n";  
  
  
  result += "const int size = get_global_size(1)*get_global_size(0);\n"
  "int index = get_global_id(1)*get_global_size(0) + get_global_id(0);\n"
  "int D;\n";
  result += typeString + " DD;\n\n";   
  
  result += "for(D=index; D< numElements; D+=size){\n"
  " DD = D+1;\n"
  "out[D]=lgamma(DD);\n"
  "}\n"
  "}\n";
  
  return(result);
}




template <typename T> 
std::string FisherSimkernelString(const int NR, const int NC, const int NpadStreams) { 
  
  std::string typeString = openclTypeString<T>();
  std::string result = "";
  
  if(typeString == "double") {
    
    result += "\n#pragma OPENCL EXTENSION cl_khr_fp64 : enable\n"
    "#define mrg31k3p_NORM_cl 4.656612873077392578125e-10\n";
    
  } else if(typeString == "float") {
    result +=  "\n#define mrg31k3p_NORM_cl 4.6566126e-10\n\n";
    
  } else {
    result += 
      "\n#define mrg31k3p_NORM_cl 1L\n\n";
  }
  
  
  result +=
    "#define NpadStreams " + std::to_string(NpadStreams) + "\n";    
  
  
  result += "\n"
  "\n#define MAXITER 2500\n"
  "#define nrow " + std::to_string(NR) + "\n"
  "#define ncol " + std::to_string(NC) + "\n"
  "#define nr_1  " + std::to_string(NR - 1) + "\n"
  "#define nc_1 " + std::to_string(NC - 1) + "\n"
  "#define nc_1nrow " + std::to_string((NC - 1) * NR) + "\n"
  "#define nrowncol " + std::to_string(NR*NC) + "\n";
  
  result += mrg31k3pString();
  
  result += "\n"
  "\n__kernel void fisher_sim_gpu(\n"
  "   __global int *nrowt, \n"
  "   __global int *ncolt, \n"
  "   const int n, \n" 
  "   const int vsize,\n"   //number of simulations
  " __global  " + typeString + " *count, \n" // number of work items
  + typeString + " threshold,\n"
  " __global  double  *fact,\n"
  " __global  " + typeString + "  *results,\n" // fisher log p
  " __global int *streams" 
  ") { \n\n";
  //#ifdef UNDEF  
  result +=
    "   local int jwork[nc_1];\n"  
    "   local int matrix[nrowncol];\n"
    "   int u, D; \n"  //original ii changed to u, added a D here
    "   " + typeString + "  ans;\n"
    "   const int globalSize = get_global_size(1)*get_global_size(0);\n"
    "   const int index=get_global_id(1)*get_global_size(0) + get_global_id(0);\n"
    "   local int jjj, l, m, ia, ib, ic, jc, id, ie, ii, nll, nlm;\n" //original j changed to jjj 
    "   local " + typeString + " x, y, dummy, sumprb;\n"
    "   bool lsm, lsp;\n\n"
    "   int Diter1, Diter2, Diter3, goTo160;\n"
    "   int countD=0;\n";
  // "   int newsize=round((vsize-index)/globalSize);\n";
  
  
  
  result += "int Drow, Dcol, DrowStart, Dentry;\n";
  
  result += "uint y1, y2, temp;\n";
  result += "uint g1[3], g2[3];\n" 
  "const int startvalue=index * NpadStreams;\n";
  
  
  result += "streamsToPrivate(streams,g1,g2,startvalue);\n";
  
  
  result += 
    
    "   for(D = 0; D < vsize; D ++) {\n"
    "      ib = 0;\n"  
    
    /* Construct random matrix */  
    "      for (jjj = 0; jjj < nc_1; ++jjj){\n"
    "        jwork[jjj] = ncolt[jjj];\n"
    "      }\n"
    
    "async_work_group_copy(jwork, ncolt, nc_1, 0);\n"
    
    "      jc = n;\n"
    
    //L LOOP
    "      for (l = 0; l < nr_1; ++l) {\n"
    "          ia = nrowt[l];\n"
    "          ic = jc;\n"
    "          jc -= ia;\n"
    //M LOOP
    "          for (m = 0; m < nc_1; ++m) {\n"
    "            id = jwork[m];\n"
    "            ie = ic;\n"
    "            ic -= id;\n"
    "            ib = ie - ia;\n"
    "            ii = ib - id;\n"
    
    "            if (ie == 0) {\n" /* Row [l,] is full, fill rest with zero entries */
    "              for (jjj = m; jjj < nc_1; ++jjj)\n"
    "                matrix[l + jjj * nrow] = 0;\n"
    "              ia = 0;\n"
    "              break;\n"  // induce break of m loop
    " }else{\n" // ie not zero
    
    "temp = clrngMrg31k3pNextState(g1, g2);\n"
    "dummy = mrg31k3p_NORM_cl * temp;\n" 
    "goTo160 = 0;\n"
    "Diter1 = 0;\n"
    
    
    // LOOP 1
    "do { \n"
    "  Diter1 ++;\n"
    "     nlm = (int)(ia * (id / (" + typeString + ") ie) + 0.5);\n"
    "     x = exp(fact[ia] + fact[ib] + fact[ic] + fact[id]- fact[ie] - fact[nlm]\n"
    "         - fact[id - nlm] - fact[ia - nlm] - fact[ii + nlm]);\n"
    "     if (x >= dummy)\n"
    "        break;\n"// break loop 1
    
    "     sumprb = x;\n"
    "     y = x;\n"
    "     nll = nlm;\n"  
    "     Diter2 = 0;\n"
    
    // LOOP 2
    " do {\n"
    " Diter2 ++;\n"
    " jjj=(int)((id - nlm) * (" + typeString + ")(ia - nlm));\n"
    " lsp = (jjj == 0);\n"
    " if (!lsp) {\n"
    "   ++nlm;\n"
    "   x = x * jjj / ((" + typeString + ") nlm * (ii + nlm));\n"
    "   sumprb += x;\n"
    "   if (sumprb >= dummy) {\n"
    "     goTo160 = 1;\n"
    " }\n"
    " }\n" // if !lsp
    " if(goTo160) \n"
    "    break;\n" // break loop 2
    
    
    
    " Diter3 = 0;\n"
    // LOOP 3
    " do {\n"  
    " Diter3 ++;\n"
    " jjj = (int)(nll * (" + typeString + ")(ii + nll));\n"  
    " lsm = (jjj == 0);\n"
    " if (!lsm) {\n"
    " --nll;\n"
    " y = y * jjj / ((" + typeString + ") (id - nll) * (ia - nll));\n"
    " sumprb += y;\n"
    "  if (sumprb >= dummy) {\n"
    "  nlm = nll;\n"
    "  goTo160 = 1;\n"
    "  }\n" // if sumprb
    "  if (!lsp)\n" 
    "    break;\n" // to while (!lsp) loop 3
    " }\n" // if !lsm
    " } while (!lsm & (Diter3 < MAXITER) & (!goTo160) );\n" 
    // END LOOP 3
    " } while (!lsp & (Diter2 < MAXITER) & (!goTo160));\n" 
    // END LOOP 2
    
    "temp = clrngMrg31k3pNextState(g1, g2);\n"
    "dummy = sumprb * mrg31k3p_NORM_cl * temp;\n"
    " } while ((Diter1 < MAXITER) & !goTo160 );\n"  
    // END LOOP 1
    
    // still in M loop
    "        matrix[l + m * nrow] = nlm;\n"
    "        ia -= nlm;\n"
    "        jwork[m] -= nlm;\n"
    "        }\n"//end LOOP ie not zero
    "        }" //end M LOOP
    
    /* last column in row l , l is from 0*/
    "  matrix[l + nc_1nrow] = ia;\n"
    "  }\n" //end L LOOP
    
    /* Compute entries in last row of MATRIX */
    "  for (m = 0; m < nc_1; ++m) {\n"
    "    matrix[nr_1 + m * nrow] = jwork[m];\n"
    "  }\n"
    "  matrix[nr_1 + nc_1nrow] = ib - matrix[nr_1 + (nc_1-1) * nrow ];\n"
    // end of rcont2
    
    
    // now on fisher_sim
    "  ans = 0.;\n"
    "  for (m = 0; m < ncol; ++m) {\n"
    "    for (l = 0,u=m*nrow; l < nrow;  l++, u++) {\n"
    "      ans -= fact[matrix[u]];\n"
    "    }\n"  // for l
    "  }\n";  // for m
  
  
  
  result += "\n" 
  "#ifdef returnResults\n"
  "results[index + D*globalSize] = ans;\n"   
  "#endif\n";
  
  
  
  result += " if (ans<=threshold) {\n"
  " countD=countD+1;\n"
  "}\n"
  "}\n" //end D loop
  
  // save countD
  " count[index] = countD;\n";
  
  result += "streamsFromPrivate(streams,g1,g2,startvalue);\n";
  
  
  
  result +=  "}\n" ;
  return(result);
}






template<typename T> 
Rcpp::NumericVector gpuFisher_test(
    viennacl::matrix<int> &x, //  viennacl::vector_base<int> &sr,  //  viennacl::vector_base<int> &sc,
    viennacl::vector_base<T> &results,  
    int B, //number of simualtion each work item, 
    viennacl::matrix<int> &streams,
    Rcpp::IntegerVector numWorkItems,
    Rcpp::IntegerVector numLocalItems,
    int ctx_id){
  
  
  std::vector<T> twostatistics(2);
  //T threshold;
  double statistics;
  const int nr = x.size1(), nc = x.size2(), resultSize = results.size();
  //int countss=0;
  // int userrequired=(B-1)*numWorkItems[0]*numWorkItems[1]+remainder;
  // row and column sums
  viennacl::vector_base<int> sr(nr);
  viennacl::vector_base<int> sc(nc);
  
#ifdef DEBUG
  Rcpp::Rcout << "x0 " << x(0,0) << " row0 " << sr(0)<< " col0 " << sc(0) << "\n";
#endif  
  
  
  viennacl::vector<T> count(numWorkItems[0]*numWorkItems[1]); 

  ///////
  std::string sumlfKernelString = sum_of_LfactorialString<double>(
    x.size1(), 
    x.size2(),
    x.internal_size2() 
  );
  
  //////
  std::string lfactorialKernelString = logfactString<double>();
#ifdef DEBUGKERNEL
  Rcpp::Rcout << lfactorialKernelString << "\n\n";
#endif  
  
  
  ///////
  std::string Fisher_kernel_string = FisherSimkernelString<T>(nr, nc, streams.internal_size2());
  if(resultSize >= B) {
    Fisher_kernel_string = "\n#define returnResults\n" + Fisher_kernel_string;
  }
#ifdef DEBUGKERNEL
  Rcpp::Rcout << Fisher_kernel_string << "\n\n";
#endif  
  
  // the context//////////////////////////////////////////////////////
  viennacl::ocl::context ctx(viennacl::ocl::get_context(ctx_id));  
  
  viennacl::ocl::program & my_prog_sumlf = viennacl::ocl::current_context().add_program(sumlfKernelString, "sumlf_kernel"); 
  viennacl::ocl::program & my_prog_lf = viennacl::ocl::current_context().add_program(lfactorialKernelString, "lfactorialKernel");
  viennacl::ocl::program &my_prog_Fisher = ctx.add_program(Fisher_kernel_string, "Fisher_kernel");
  
  viennacl::ocl::kernel &sumLfactorialKernel = my_prog_sumlf.get_kernel("sumLfactorial");
  viennacl::ocl::kernel &lfactorialKernel = my_prog_lf.get_kernel("logfactorial");
  viennacl::ocl::kernel &fisher_sim = my_prog_Fisher.get_kernel("fisher_sim_gpu"); 
  
  sumLfactorialKernel.global_work_size(0, numWorkItems[0]);
  sumLfactorialKernel.global_work_size(1, numWorkItems[1]);
  sumLfactorialKernel.local_work_size(0, 1L);
  sumLfactorialKernel.local_work_size(1, 1L);
  
  lfactorialKernel.global_work_size(0, numWorkItems[0]);
  lfactorialKernel.global_work_size(1, numWorkItems[1]);
  lfactorialKernel.local_work_size(0, 1L);
  lfactorialKernel.local_work_size(1, 1L); 
  
  fisher_sim.global_work_size(0, numWorkItems[0]);
  fisher_sim.global_work_size(1, numWorkItems[1]);
  fisher_sim.local_work_size(0, 1L);
  fisher_sim.local_work_size(1, 1L); 
  
  
  
  
  viennacl::vector_base<double> logFactorials(numWorkItems[0] * numWorkItems[1]);
  
  viennacl::ocl::command_queue theQueue = sumLfactorialKernel.context().get_queue();
  viennacl::ocl::enqueue(sumLfactorialKernel(x, logFactorials), theQueue);

  statistics = viennacl::linalg::sum(logFactorials);
  twostatistics[0] = (T) (-statistics)/(1+64 * DOUBLE_EPS);   // threshold
  row_sum_impl(x, sr);
  column_sum_impl(x, sc);
  int n = viennacl::linalg::sum(sr); //sum of row/column totals
  viennacl::vector<double> factTrue(n+1); 
  
  
  /*
   viennacl::vector<T> fact(n+1); 
   T factTemp;
   
   // Calculate log-factorials.  fact[i] = lgamma(i+1)/
   fact(0) = 0.;
   fact(1) = 0.;
   factTemp = 0.;
   int i;
   for(i = 2; i <= n; i++) {
   factTemp = factTemp + log(i);
   fact(i) = factTemp;    //    fact(i) = fact(i - 1) + log(i);
   }
   */
  viennacl::ocl::enqueue(lfactorialKernel(factTrue, (n+1)),theQueue);
  viennacl::ocl::enqueue(fisher_sim(sr, sc, n, B, count, twostatistics[0], factTrue, results, streams),theQueue); 
  
  clFinish(theQueue.handle().get());
  
  /* if(B < resultSize) {
   results[B] = results[0];
  }
   results[0] = statistics;*/
  
  twostatistics[1] = viennacl::linalg::sum(count); //countss
  
#ifdef DEBUGKERNEL
 // Rcpp::Rcout << "threshold " << threshold << " countss " << countss << " count0 " << count(0) << " size " << B <<  "\n";
#endif  
  
  return (wrap(twostatistics));
}





template<typename T> 
SEXP gpuFisher_test_Templated(
    Rcpp::S4  xR, 
    Rcpp::S4  resultsR,
    int B,// int remainder,
    Rcpp::S4 streamsR,   
    Rcpp::IntegerVector max_global_size,
    Rcpp::IntegerVector max_local_size){
  
  const bool BisVCL=1;
  const int ctx_id = INTEGER(resultsR.slot(".context_index"))[0]-1;
  //int countss=0;
  
  Rcpp::NumericVector twostatistics;
  std::shared_ptr<viennacl::matrix<int> > x =getVCLptr<int>(xR.slot("address"), BisVCL, ctx_id);
  std::shared_ptr<viennacl::vector_base<T> > results = getVCLVecptr<T>(resultsR.slot("address"), BisVCL, ctx_id);
  std::shared_ptr<viennacl::matrix<int> > streams = getVCLptr<int>(streamsR.slot("address"), BisVCL, ctx_id);
  
  twostatistics=gpuFisher_test<T>(*x, *results, B, *streams, max_global_size, max_local_size, ctx_id);
  
  return twostatistics;
}




// [[Rcpp::export]]
SEXP cpp_gpuFisher_test(
    Rcpp::S4  xR, 
    Rcpp::S4  resultsR,
    int B,// int remainder,
    Rcpp::S4 streamsR,  
    Rcpp::IntegerVector max_global_size,
    Rcpp::IntegerVector max_local_size){
  
  Rcpp::traits::input_parameter< std::string >::type classVarR(RCPP_GET_CLASS(resultsR));
  std::string precision_type = (std::string) classVarR;
  
  //#ifdef UNDEF    
  if(precision_type == "fvclVector") {
    return (gpuFisher_test_Templated<float>(xR,resultsR, B, streamsR, max_global_size, max_local_size));
  } else if (precision_type == "dvclVector") {
    return (gpuFisher_test_Templated<double>(xR,resultsR, B, streamsR, max_global_size, max_local_size));
  } else {
    Rcpp::warning("class of var must be fvclVector or dvclVector");
  }
  return(Rcpp::wrap(-1));
  //#endif  
}








