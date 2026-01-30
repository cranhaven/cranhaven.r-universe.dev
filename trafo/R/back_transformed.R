back_transformed <- function(predictor, trafo, lambda, shift) {

    
 predictor <- if (trafo == "boxcoxshift") {
    box_cox_shift_back(y = predictor, lambda = lambda, shift = shift)
  } else if (trafo == "boxcox") {
    box_cox_back(y = predictor, lambda = lambda)
  } else if (trafo == "modulus") {
    modul_back(y = predictor, lambda = lambda)
 } else if (trafo == "bickeldoksum") {
     Bick_dok_back(y = predictor, lambda = lambda)
   } else if (trafo == "manly") {
     Manly_back(y = predictor, lambda = lambda)
   } else if (trafo == "dual") {
     Dual_back(y = predictor, lambda = lambda)
   } else if (trafo == "yeojohnson") {
     Yeo_john_back(y = predictor, lambda = lambda)
   } else if (trafo == "logshiftopt") {
     log_shift_opt_back(y = predictor, lambda = lambda)
   } else if (trafo == "sqrtshift") {
     sqrt_shift_back(y = predictor, lambda = lambda)
   } else if (trafo == "log") {
     Log_back(y = predictor)
   } else if (trafo == "logshift") {
     Log_shift_back(y = predictor)
   } else if (trafo == "reciprocal") {
     Reciprocal_back(y = predictor)
   } else if (trafo == "neglog") {
     neg_log_back(y = predictor)
   }
 
 return(predictor = predictor)
}