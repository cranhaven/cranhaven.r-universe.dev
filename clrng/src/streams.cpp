
#include "gpuRandom.hpp"
//#define DEBUGKERNEL
// #define mrg31k3p_M1 2147483647             /* 2^31 - 1 */
// #define mrg31k3p_M2 2147462579             /* 2^31 - 21069 */

std::string streamsString(int NpadStreams, 
                          const int keepInitial) {  
  
  
  std::string result = "";
  
  
  result += 
    "#define NpadStreams " + std::to_string(NpadStreams) + "\n"
    "#define mrg31k3p_M1 2147483647 \n"
    "#define mrg31k3p_M2 2147462579 \n\n\n";
  
  
  result += "__constant ulong jmatrix[18]= {1702500920, 1849582496, 1656874625,\n"
  " 828554832, 1702500920, 1512419905,\n"
  " 1143731069,  828554832,  102237247,\n"
  " 796789021, 1464208080,  607337906, \n"
  " 1241679051, 1431130166, 1464208080, \n"
  " 1401213391, 1178684362, 1431130166};\n\n\n";
  
  
  /* 
   result += "__constant cl_uint jmatrix[18]= {1, 2, 3,\n"
   " 4, 5, 6,\n"
   " 7,  8, 9,\n"
   "1, 0, 0,\n"
   " 0, 1, 0,\n"
   " 0,  0,  1 };\n\n\n";
   */  
  
  
  result += 
  "\n__kernel void createStreams(\n"    
  "__global uint *creatorInitialGlobal, \n"
  "__global uint *streams,\n"
  "int Nstreams){\n";
  
  
  result +=
    "uint creatorNextState[6], creatorCurrentState[6];\n"  
    "int i, row, col, Dstream;\n"
    "ulong acc; \n\n\n";
  
  
  
  
  if (keepInitial >= 1){
    result +=  
      " for (i=0; i<6; i++) {\n"
      "creatorCurrentState[i] = creatorInitialGlobal[i];\n"
      "}\n";
    
  }else if (keepInitial == 0) {
    
    result +=     
      // modMatVec(creator->nuA1, creator->nextState.g1, creator->nextState.g1, mrg31k3p_M1);
      "for (row=0; row<3; row++){\n"
      "acc = 0;\n"
      "for (col=0; col<3; col++){\n"
      "acc += (jmatrix[3 * row + col] * ( (ulong) creatorInitialGlobal[col]) ) % mrg31k3p_M1;\n"
      " }\n"
      //"creatorCurrentState[row] = acc; \n"
      "creatorCurrentState[row] = (uint) (acc % mrg31k3p_M1);\n"
      
      "}\n"
      
      
      // modMatVec(creator->nuA2, creator->nextState.g2, creator->nextState.g2, mrg31k3p_M2);
      "for (row=3; row<6; row++){\n"
      " acc = 0;\n"
      "for (col=0; col<3; col++){\n"
      "acc += (jmatrix[3 * row + col] * ((ulong) creatorInitialGlobal[col+3])) % mrg31k3p_M2;\n"
      "}\n"
      //"creatorCurrentState[row] = acc; \n"
      "creatorCurrentState[row] = (uint) (acc % mrg31k3p_M2);\n"
      
      "}\n\n";
    
  }
  
  
  result +=  
    
    "for(Dstream = 0;   Dstream < Nstreams;    Dstream++){\n\n"
    
    // upate creatorNext from creatorCurrentState,
    "for (i=0; i<6; i++) {\n"
    " streams[Dstream * NpadStreams +  i] = \n"//initial
    " streams[Dstream * NpadStreams + 6 + i] = \n"//current
    //"streams[Dstream * NpadStreams + 12 + i] = "// substream
    " creatorNextState[i] = creatorCurrentState[i];\n"
    "}\n"
    
    
    
    // Matrix-vector modular multiplication
    // modMatVec(creator->nuA1, creator->nextState.g1, creator->nextState.g1, mrg31k3p_M1);
    "for (row=0; row<3; row++){\n"
    "acc = 0;\n"
    "for (col=0; col<3; col++){\n"
    "acc += (jmatrix[3 * row + col] * ( (ulong) creatorNextState[col]) ) % mrg31k3p_M1;\n"
    " }\n"
    //"creatorCurrentState[row] = acc; \n"
    "creatorCurrentState[row] = (uint) (acc % mrg31k3p_M1);\n"
    // "creatorCurrentState[row] = fmod((float)acc, (float)mrg31k3p_M1);\n"
    "}\n"
    
    
    
    // modMatVec(creator->nuA2, creator->nextState.g2, creator->nextState.g2, mrg31k3p_M2);
    "for (row=3; row<6; row++){\n"
    " acc = 0;\n"
    "for (col=0; col<3; col++){\n"
    "acc += (jmatrix[3 * row + col] * ((ulong)creatorNextState[col+3])) % mrg31k3p_M2;\n"
    "}\n"
    // "creatorCurrentState[row] = acc; \n"
    "creatorCurrentState[row] = (uint) (acc % mrg31k3p_M2);\n"
    // "creatorCurrentState[row] = fmod((float)acc, (float)mrg31k3p_M2);\n"
    "}\n"
    
    "}\n"; // loop through streams
  
  
  result +=  
    " for (i=0; i<6; i++) {\n"
    "creatorInitialGlobal[i] = creatorCurrentState[i];\n"
    "}\n"
    
    
    
    "}\n"; 
  
  return(result);
}





#define mrg31k3p_M1 2147483647            
#define mrg31k3p_M2 2147462579   


Rcpp::IntegerVector CreateStreamsGpu(
    Rcpp::IntegerVector creatorInitialGlobalR,
    viennacl::matrix_base<int> &streams, 
    Rcpp::IntegerMatrix streamsMat,
    const int onGpu,
    const int keepInitial,
    int ctx_id) {
  
  static std::vector<int>  creatorInitial_cpu = Rcpp::as<std::vector<int> >(creatorInitialGlobalR);
  const int Nstreams = streams.size1(); //# of rows
  
  if(onGpu==1){
    viennacl::vector_base<int> creatorInitial_gpu(6); 
    copy(creatorInitial_cpu, creatorInitial_gpu);
    /* fill a vector on CPU
     for (size_t i=0; i<6; ++i)
     cpu_vector[i] = 12345; */
    
    // fill a vector on GPU with data from CPU - faster versions:
    //copy(cpu_vector, creatorInitialGlobal);  //option 1 // copy(cpu_vector.begin(), cpu_vector.end(), vcl_vector.begin()); //option 2
    
    
    
    std::string streamsKernelString = streamsString(
      streams.internal_size2(), 
      keepInitial
    );
    
    
    
    // the context
    viennacl::ocl::switch_context(ctx_id);
    viennacl::ocl::program & my_prog = viennacl::ocl::current_context().add_program(streamsKernelString, "my_kernel");
    
    //Rcpp::Rcout << "after adding kernelstring" << "\n\n";
#ifdef DEBUGKERNEL
    Rcpp::Rcout << streamsKernelString << "\n\n";
#endif  
    
    
    viennacl::ocl::kernel &streamsKernel = my_prog.get_kernel("createStreams");
    streamsKernel.global_work_size(0, 1L);
    streamsKernel.global_work_size(1, 1L);
    
    streamsKernel.local_work_size(0, 1L);
    streamsKernel.local_work_size(1, 1L);
    //Rcpp::Rcout << "before enqueue kernel" << "\n\n";
    
    viennacl::ocl::enqueue(streamsKernel(creatorInitial_gpu, streams, Nstreams) );
    clFinish(streamsKernel.context().get_queue().handle().get());
    //Rcpp::Rcout << "after enqueue kernel\n\n" << "\n\n";
    
    copy(creatorInitial_gpu,creatorInitial_cpu);
    
    
  } else {
    int i, row, col, Dstream;
    unsigned long acc; 
    
    const unsigned long JUMP_MATRIX[18]= {
      1702500920, 1849582496, 1656874625,
      828554832, 1702500920, 1512419905,
      1143731069,  828554832,  102237247,
      796789021, 1464208080,  607337906, 
      1241679051, 1431130166, 1464208080, 
      1401213391, 1178684362, 1431130166};
    
    
    //static std::vector<cl_uint>  creatorInitial_cpu = Rcpp::as<std::vector<cl_uint> >(initial);
    uint creatorNextState[6];
    
    
    for(Dstream = 0;   Dstream < Nstreams;    Dstream++){
      
      for (i=0; i<6; i++) {
        streamsMat(Dstream, i) = 
          streamsMat(Dstream, 6 + i) = 
          creatorNextState[i] = creatorInitial_cpu[i];
      }
      
      
      for (row=0; row<3; row++){
        acc = 0;
        for (col=0; col<3; col++){
          acc += (JUMP_MATRIX[3 * row + col] * ( (unsigned long) creatorNextState[col] ) ) % mrg31k3p_M1;
        }
        creatorInitial_cpu[row] = (uint) (acc % mrg31k3p_M1);
      }
      
      
      for (row=3; row<6; row++){
        acc = 0;
        for (col=0; col<3; col++){
          acc += (JUMP_MATRIX[3 * row + col] * ( (unsigned long) creatorNextState[col+3]) ) % mrg31k3p_M2;
        }
        creatorInitial_cpu[row] = (uint) (acc % mrg31k3p_M2);
      }
      
    }

  }
  
  Rcpp::IntegerVector currentSeed( creatorInitial_cpu.begin(), creatorInitial_cpu.end() );
  
  return currentSeed;
  /* 
   Rcpp::Rcout << streams(0,0) << "\n" << streams(0,1) << "\n"<< streams(0,2) << "\n\n";
   Rcpp::Rcout << streams(1,0) << "\n" << streams(1,1) << "\n"<< streams(1,2) << "\n\n";
   Rcpp::Rcout << streams(2,0) << "\n" << streams(2,1) << "\n"<< streams(2,2) << "\n\n";
   */
}









SEXP CreateStreamsTemplated(
    Rcpp::IntegerVector creatorInitialGlobalR,
    Rcpp::S4 streamsR,
    Rcpp::IntegerMatrix streamsMat,
    const int onGpu,
    const int keepInitial){
  
  const bool BisVCL=1;
  const int ctx_id = INTEGER(streamsR.slot(".context_index"))[0]-1;

  std::shared_ptr<viennacl::matrix_base<int> > streams = getVCLptr<int>(streamsR.slot("address"), BisVCL, ctx_id);
  

  return Rcpp::wrap(CreateStreamsGpu(creatorInitialGlobalR, *streams, streamsMat, onGpu, keepInitial, ctx_id));
  
}




//[[Rcpp::export]]
SEXP CreateStreamsBackend(
    Rcpp::IntegerVector creatorInitialGlobalR,    
    Rcpp::S4 streamsR,
    Rcpp::IntegerMatrix streamsMat,
    const int onGpu,
    const int keepInitial){
  
  colnames(streamsMat) = Rcpp::CharacterVector::create(
    "current.g1.1", "current.g1.2", "current.g1.3", "current.g2.1", "current.g2.2", "current.g2.3",
    "initial.g1.1", "initial.g1.2", "initial.g1.3", "initial.g2.1", "initial.g2.2", "initial.g2.3");
  
  return CreateStreamsTemplated(creatorInitialGlobalR, streamsR, streamsMat, onGpu, keepInitial);
  
  
}
