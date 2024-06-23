
 /* Use 0.5 - p + 0.5 to perhaps gain 1 bit of accuracy */

std::string qqnormkernelString = 

"\n#pragma OPENCL EXTENSION cl_khr_fp64 : enable\n\n"

"\n#define R_D_Lval(p)	(lower_tail ? (p) : (0.5 - (p) + 0.5) )\n"
"#define R_D_Cval(p)	(lower_tail ? (0.5 - (p) + 0.5) : (p) )\n"

"\n\n__kernel void qnorm(__global double  *out,\n"
                      "const double mu,\n "
                      "const double sigma,\n"
                      "const int out_size,\n"
                      "const int lower_tail){\n"
                    
         "const int size = get_global_size(1)*get_global_size(0);\n"
         "const int index = get_global_id(1)*get_global_size(0) + get_global_id(0);\n"
         "int D;\n"  
//#ifdef UNDEF                    
         "double p_, q, r, val;\n"    
         "double pD;"
         "double multFrac;\n"
         "if(out_size > 10) {multFrac = 0.375;} else {multFrac=0.5;}"
          // "if(sigma == 0)  return mu;\n"
        
      
          " for (D=index; D < out_size ; D += size) {\n"
          "pD = ( D+1-multFrac)/(out_size+1-2*multFrac);"
          
          /* real lower_tail prob. p */
          " p_ = R_D_Lval(pD);\n "   //a change here              
          " q = p_ - 0.5;\n"
          
          "if (fabs(q) <= .425) {\n"/* 0.075 <= p <= 0.925 */
          " r = .180625 - q * q;\n"
          " val =q * (((((((r * 2509.0809287301226727 + 33430.575583588128105) * r \n"
          "  + 67265.770927008700853) * r +45921.953931549871457) * r + \n"
          " 13731.693765509461125) * r +1971.5909503065514427) * r + \n"
          " 133.14166789178437745) * r +3.387132872796366608)/ (((((((r * \n"
          " 5226.495278852854561 + 28729.085735721942674) * r + \n"
          " 39307.89580009271061) * r + 21213.794301586595867) * r +\n "
          "  5394.1960214247511077) * r + 687.1870074920579083) * r +\n "
          " 42.313330701600911252) * r + 1.);"
          " }\n"
          
          "else{\n "          /* closer than 0.075 from {0,1} boundary */ /* r = min(p, 1-p) < 0.075 */
          
          " if (q > 0) {\n"
          " r = R_D_Cval(pD);  } \n "   /* 1-p */   // a change here
          " else{\n"
          " r = p_; }\n"
          " r = sqrt(- log(r));\n"
          
          " if (r <= 5.) {\n"         /* <==> min(p,1-p) >= exp(-25) ~= 1.3888e-11 */
          " r += -1.6;\n"
          "val = (((((((r * 7.7454501427834140764e-4 +.0227238449892691845833) * r + .24178072517745061177) *\n"
          " r + 1.27045825245236838258) * r +3.64784832476320460504) * r + 5.7694972214606914055) *\n"
          " r + 4.6303378461565452959) * r +1.42343711074968357734)/ (((((((r *\n"
          " 1.05075007164441684324e-9 + 5.475938084995344946e-4) *\n"
          " r + .0151986665636164571966) * r +.14810397642748007459) * r + .68976733498510000455) *\n"
          " r + 1.6763848301838038494) * r +2.05319162663775882187) * r + 1.);\n"
          "}\n"
          
          "else{\n" /* very close to  0 or 1 */
          " r += -5.;\n"
          " val = (((((((r * 2.01033439929228813265e-7 +2.71155556874348757815e-5) * r +\n"
          " .0012426609473880784386) * r + .026532189526576123093) *r + .29656057182850489123) * r + \n"
          " 1.7848265399172913358) * r + 5.4637849111641143699) *r + 6.6579046435011037772)/ (((((((r \n"
          " *2.04426310338993978564e-15 + 1.4215117583164458887e-7)*r + 1.8463183175100546818e-5) * r +\n "
          "  7.868691311456132591e-4) * r + .0148753612908506148525)* r + .13692988092273580531) * r +\n"
          " .59983220655588793769) * r + 1.);\n"
          " }\n"
          
          "  if(q < 0.0){\n"
          " val = -val;}\n"
          "}\n  "
          
          "out[D]= mu + sigma * val;}\n"
//#endif          
  "}\n";
    
    
    
    
    
    
    
    
    
