
## rcalibration Class


setClass("rcalibration", 		
         representation( 
           p_x = "integer",          ## dimension of the inputs
           p_theta="integer",        ## dimention of the calibration parameters
           num_obs = "integer",          ## experimental observations number
           ## data
           input = "matrix",           ## the design of experiments, size n x p_x
           output = "vector",           ## the experimental observations, size n x 1
           X="matrix",                  ## mean basis for experiment, size n x q
           have_trend="logical",         ## have mean or no
           q="integer",                 ## number of mean basis for experiment
           R0="list",                    ##abs difference of each type of input
           kernel_type="character",       #####type of kernel to specify
           alpha="vector",              ####roughness parameter in the kernel, only useful for pow_exp
           theta_range= "matrix",        ## the range of calibration parameters
           lambda_z="vector",        ## parameter in the S-GaSP
           S="integer",                   ## number of MCMC
           S_0="integer",                 ## number of burn-in
           thinning="integer",                 ## number of thinning
           prior_par="vector",            ## prior parameters for jointly robust prior
           output_weights="vector",       ##whether the output contains weights
           sd_proposal="vector",            ##standard deviation of the MCMC, if we have a discrepancy, 
           ##the size is p_theta+p_x+1; if we don't, the size is p_theta
           discrepancy_type="character",    ## no-discrepancy,  GaSP and  S-GaSP
           simul_type="integer",          ## 0 means use RobustGaSP pacakge to fit the computer model, 
           ## 1 means the simulator is defined by the user  
           ## 2 means the simulator for Kilauea Volcano by the ascending-mode image 
           ##  3 means the simulator for Kilauea Volcano by the decending-mode image
           post_sample="matrix",    ## posterior samples after burn-in
           post_value="vector",             ## posterior value after burn-in after burn-in
           accept_S="vector",                ## number of proposed samples of the calibation parameters are accepted
           count_boundary="numeric",          ## number of proposed samples is outside the boundary of calibration parameters
           emulator_rgasp="rgasp",             ## an S4 class called rgasp from RobustGaSP
           emulator_ppgasp="ppgasp",           ## an S4 class called ppgasp from RobustGaSP
           ##replicate
           have_replicates="logical",           ## logical value about whether we have replicates of observations or not.
           S_2_f="numeric",                ## if there is no replicate, this is zero. This is sum of squares of observations if we have replicate
           num_replicates="vector",        ##a vector about replicate for each input
           num_obs_all="integer",     ##the total number of obs
           ##mle
           method="character",        ##posterior sampling or MLE
           initial_values="matrix",  ## a matrix of initial start if optimization is used
           param_est="vector",  ## a vector of initial start if optimization is used 
           opt_value="numeric",  ## optimized value of log profile likelihood up to a constant 
           ##emulator
           emulator_type='character', ##rgasp or ppgasp emulator 
           loc_index_emulator="vector" ##a vector of location index of field observations as a subset of the input_simul

         ), 
)





setClass("predictobj.rcalibration", representation(
  mean = "vector",
  math_model_mean = "vector",
  math_model_mean_no_trend = "vector",
  interval = "matrix",
  delta="vector"
),
)

# 
if(!isGeneric("show")) {
  setGeneric(name = "show",
             def = function(object) standardGeneric("show")
  )
}

setMethod("show", "rcalibration",
          function(object){
            show.rcalibration(object)
          }
)


if(!isGeneric("predict")) {
  setGeneric(name = "predict",
             def = function(object, ...) standardGeneric("predict")
  )
}

setMethod("predict", "rcalibration",
          definition=function(object, testing_input,X_testing=NULL,
                              n_thinning=10, testing_output_weights=NULL,
                              interval_est=NULL,interval_data=F,math_model=NULL,test_loc_index_emulator=NULL,...){
            predict.rcalibration(object = object, testing_input = testing_input, X_testing=X_testing,
                                 n_thinning=n_thinning, testing_output_weights=testing_output_weights,
                                 interval_est=interval_est,interval_data=interval_data,
                                 math_model=math_model,test_loc_index_emulator=test_loc_index_emulator,...)
          }
)




setClass("rcalibration_MS", 		
         representation( 
           num_sources="integer",    ## number of sources 
           p_x = "vector",          ## dimension of the inputs
           p_theta="integer",        ## dimention of the calibration parameters
           num_obs = "vector",          ## experimental observations number
           index_theta="list",         ##index of theta shared at each source. 
           ##the default is that theta is shared  for each sources
           ## data
           input = "list",           ## the design of experiments, size n x p_x
           output = "list",           ## the experimental observations, size n x 1
           X="list",                  ## mean basis for experiment, size n x q
           have_trend="vector",         ## have mean or no
           q="vector",                 ## number of mean basis for experiment
           R0="list",                    ##abs difference of each type of input
           kernel_type="vector",       #####type of kernel to specify
           alpha="list",              ####roughness parameter in the kernel, only useful for pow_exp
           theta_range= "matrix",        ## the range of calibration parameters
           lambda_z="list",        ## parameter in the S-GaSP
           S="integer",                   ## number of MCMC
           S_0="integer",                 ## number of burn-in
           thinning="integer",                 ## number of thinning
           prior_par="list",            ## prior parameters for jointly robust prior
           output_weights="list",       ##whether the output contains weights
           sd_proposal_theta="vector",            ##standard deviation of theta in MCMC
           sd_proposal_cov_par="list",            ##standard deviation of the covariance par in MCMC
           discrepancy_type="vector",    ## no-discrepancy,  GaSP and  S-GaSP
           simul_type="vector",          ## 0 means use RobustGaSP pacakge to fit the computer model, 
           emulator_rgasp="list",             ## a list of rgasp emulators from RobustGaSP
           emulator_ppgasp="list",             ## a list of S4 class of ppgasp emulators from RobustGaSP
           post_theta="matrix",    ## posterior samples of calibration after burn-in
           post_individual_par="list",   ## posterior sample of the covariance parameters, noise, and trend
           post_value="matrix",             ## posterior value after burn-in after burn-in
           accept_S_theta="numeric",                ## number of proposed samples of the calibation parameters are accepted
           accept_S_beta="vector",                ## number of proposed samples of the calibation parameters are accepted
           count_boundary="numeric",          ## number of proposed samples is outside the boundary of calibration parameters
           measurement_bias="logical",        ##whether this is measurement bias
           post_delta="matrix",       ##posterior of the sample of the model bias
           post_measurement_bias="list",     ##the mean of measurement bias
           have_measurement_bias_recorded="logical",  ##whether we record the mean of measurement bias
           ##no replicate is allowed for calibration of multiple sources
           #emulator
           emulator_type='vector', ##a vector of the type of the emulator
           loc_index_emulator="list" ##a list of location index of each source of field observations as a subset of the input_simul
           ), 
)



# setClass("rcalibration_MS_opt", 		
#          representation(    
#            num_sources="integer",    ## number of sources 
#            p_x = "vector",          ## dimension of the inputs
#            p_theta="integer",        ## dimention of the calibration parameters
#            num_obs = "vector",          ## experimental observations number
#            index_theta="list",         ##index of theta shared at each source. 
#            ##the default is that theta is shared  for each sources
#            ## data
#            input = "list",           ## the design of experiments, size n x p_x
#            output = "list",           ## the experimental observations, size n x 1
#            X="list",                  ## mean basis for experiment, size n x q
#            have_trend="vector",         ## have mean or no
#            q="vector",                 ## number of mean basis for experiment
#            R0="list",                    ##abs difference of each type of input
#            kernel_type="vector",       #####type of kernel to specify
#            alpha="list",              ####roughness parameter in the kernel, only useful for pow_exp
#            theta_range= "matrix",        ## the range of calibration parameters
#            lambda_z="vector",        ## parameter in the S-GaSP
#            output_weights="list",       ##whether the output contains weights
#            discrepancy_type="vector",    ## no-discrepancy,  GaSP and  S-GaSP
#            simul_type="vector",          ## 0 means use RobustGaSP pacakge to fit the computer model, 
#            emulator="list",             ## emulators from RobustGaSP
#            ##
#            initial_starts="matrix",       ##initial starts of the optimization
#            theta_est="vector",            ##estimated calibration and model parameter
#            individual_param_est="list",    ##parameter for each source
#            ##the first p_theta are the calibration parameters, the p_x+1 are the range and nugget parameters
#            ##then last q+1 parameters are the mean and variance
#            opt_value="numeric"           ##optimization value
#          ), 
# )




if(!isGeneric("predict_MS")) {
  setGeneric(name = "predict_MS",
             def = function(object, ...) standardGeneric("predict_MS")
  )
}

setMethod("predict_MS", "rcalibration_MS",
          definition=function(object, testing_input, X_testing=as.list(rep(0,object@num_sources)),
                              testing_output_weights=NULL, n_thinning=10,
                              interval_est=NULL,interval_data=rep(F,length(testing_input)),
                              math_model=NULL,...){
            predict_MS.rcalibration_MS(object=object, testing_input=testing_input, X_testing=X_testing,
                                       testing_output_weights=testing_output_weights, n_thinning=n_thinning, 
                                       interval_est=interval_est,interval_data=interval_data,
                                       math_model=math_model,...)
          }
)

setClass("predictobj.rcalibration_MS", representation(
  mean = "list",
  math_model_mean = "list",
  math_model_mean_no_trend = "list",
  delta_mean="vector",
  measurement_bias_mean="list",
  interval = "list"  ##this is interval for the data
  
),
)



