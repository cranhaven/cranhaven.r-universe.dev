
##########################################################################
## rcalibration function for multiple sources data
##########################################################################
##for the design and observations are both list

rcalibration_MS <- function(design, observations, p_theta=NULL, index_theta=NULL,
                            X=as.list(rep(0,length(design))), have_trend=rep(FALSE,length(design)),
                            simul_type=rep(1, length(design)), input_simul=NULL, output_simul=NULL,
                            simul_nug=rep(FALSE,length(design)),loc_index_emulator=NULL,math_model=NULL,
                            theta_range=NULL, sd_proposal_theta=NULL,
                            sd_proposal_cov_par=NULL,
                            S=10000,S_0=2000, thinning=1,
                            measurement_bias=FALSE, shared_design=NULL,have_measurement_bias_recorded=F,
                            shared_X=0,have_shared_trend=FALSE,
                            discrepancy_type=rep('S-GaSP',length(design)+measurement_bias),
                            kernel_type=rep('matern_5_2',length(design)+measurement_bias),
                            lambda_z=as.list(rep(NA,length(design)+measurement_bias)),
                            #lambda_z=list(NA), 
                            a=NULL,b=NULL,alpha=NULL,
                            output_weights=NULL,...){
  
  
  if( !is.list(design) | !is.list(observations) ){
    stop("The design and observations should both be a list for data from multiple sources \n")
    
  }
  
  if(is.null(p_theta)){
    stop("Please specify `p_theta', the number of parameters to be calibrated \n")
  }
  
  if(is.null(sd_proposal_theta)){
    sd_proposal_theta=0.05*(as.matrix(theta_range)[,2]-as.matrix(theta_range)[,1])
  }

  num_sources=length(design)   
  
  
  
  model <- new("rcalibration_MS")
  
  model@num_sources=num_sources
  ###add the measurement_bias and share design
  model@measurement_bias=measurement_bias
  
  ##default choice is that the all calibration parameters are shared across all sources
  ##
  if(is.null(index_theta)[1]){
    model@index_theta=list()
    for(i in 1:num_sources){
      model@index_theta[[i]]=1:p_theta
    }
  }
  
  if(is.null(theta_range) ){
    stop("Please specify theta_range \n")
  }
  
  
  if(model@measurement_bias==T){
    if(is.null(shared_design)){
      stop("Please specify shared_design \n")
    }
   for(i_source in 1:num_sources){
     if(discrepancy_type[i_source]!='GaSP' & discrepancy_type[i_source]!='S-GaSP'){
       stop("the measurement bias should either be GaSP or S-GaSP \n")
     }
   }
  }

  model@p_x=rep(0,num_sources+measurement_bias)
  model@q=rep(0, num_sources)
  
  ##make it a matrix 
  for(i_source in 1:num_sources){
    design[[i_source]]=as.matrix(design[[i_source]])
    ##make it a numeric matrix
    design[[i_source]]=matrix(as.numeric(design[[i_source]]), ncol = ncol(design[[i_source]]))
  
  }
  

  model@input=design
  
  ###I add 
  ##add to the input if there is a shared one
  if(measurement_bias){
    shared_design=as.matrix(shared_design)
    shared_design=matrix(as.numeric(shared_design), ncol = ncol(shared_design))
    
    model@input[[num_sources+1]]=shared_design
  }
  model@output=observations
  
  ##
  model@num_obs=rep(0,num_sources+measurement_bias)
  
  ##no replicate for this multiple sources scenarios 
  #model@S_2_f=0
  #model@have_replicates=F
  #model@num_obs_all=0
  
  
  model@output_weights=as.list(rep(0,num_sources))
  model@X=as.list(1:(num_sources))
  
  model@have_trend=have_trend
  

  for(i in 1: num_sources){
    model@input[[i]]=as.matrix(model@input[[i]])
    
    model@p_x[i]=dim(model@input[[i]])[2]
    model@num_obs[i]=dim(model@input[[i]])[1]
    
    
    model@output_weights[[i]]=rep(1,  model@num_obs[i] )
    model@X[[i]]=as.matrix(X[[i]])
    
    if(model@have_trend[i]==TRUE){
      model@q[i]=dim(model@X[[i]])[2]
    }
    
    
    if(simul_type[i]==1){
      if(is.null(math_model)){
        stop("Please specify the math_model  \n")
      }
    }
    
    if ( (discrepancy_type[i]!='no-discrepancy')&&(discrepancy_type[i]!='GaSP')&& (discrepancy_type[i]!='S-GaSP') ){
      stop("simul_type should be chosen from no-discrepancy, GaSP or S-GaSP \n")
    }
    if(simul_type[i]==0){
      if(is.null(input_simul[i]) | is.null(output_simul[i])  ){
        stop("input_simul and output_simul are needed to be specified for constructing the emulator \n")
        
      }
    }
    if( (!(kernel_type[i]=='matern_5_2'))&(!(kernel_type[i]=='matern_3_2'))&(!(kernel_type[i]=='pow_exp'))){
      stop("only 'matern_5_2', 'matern_3_2' and 'pow_exp' are implemented. See kernel_type for more information.")
    }
    
  }
  
  ##add some shared trend
  if(have_shared_trend){
    model@shared_X=as.matrix(shared_X)
    model@have_shared_trend=have_shared_trend
  }
  
  ##the one for the measurement bias
  if(measurement_bias){
    model@p_x[num_sources+1]=dim(model@input[[num_sources+1]])[2]
    model@num_obs[num_sources+1]=dim(model@input[[num_sources+1]])[1]
    if( (!(kernel_type[num_sources+1]=='matern_5_2'))&(!(kernel_type[num_sources+1]=='matern_3_2'))&(!(kernel_type[num_sources+1]=='pow_exp'))){
      stop("only 'matern_5_2', 'matern_3_2' and 'pow_exp' are implemented. See kernel_type  for more information.")
    }
    
  }
  
  if(!is.null(output_weights)){
    model@output_weights=output_weights
  }
  
  if(is.null(b)){
    b=rep(1, num_sources)
    if(measurement_bias){
      b=c(b,1)  ##I give the last one as the one for shared cov
    }
  }
  
  
  model@theta_range=theta_range
  model@p_theta=as.integer(p_theta)
  model@have_trend=have_trend
  model@kernel_type=kernel_type
  model@alpha=as.list(rep(0,num_sources+measurement_bias))
  model@discrepancy_type=discrepancy_type
  model@post_measurement_bias=list()
  model@have_measurement_bias_recorded=have_measurement_bias_recorded
  
  
  model@R0 =list()
  

  
  model@prior_par=as.list(rep(0,num_sources))
  
  
  #model@prior_par=c(CL,a,b)
  
  
  if(model@discrepancy_type[i]!='no-discrepancy'){
     for(i in 1:(num_sources+measurement_bias)){
  
        ##work on the prior parameter
        CL = rep(0,model@p_x[i])    ###CL is also used in the prior so I make it a model parameter
        for(i_cl in 1:model@p_x[i]){
          CL[i_cl] = (max(model@input[[i]][,i_cl])-min(model@input[[i]][,i_cl]))/model@num_obs[i]^{1/model@p_x[i]}
        }
        model@prior_par[[i]]=CL
        
        if(is.null(a)){
          model@prior_par[[i]]=c( model@prior_par[[i]],1/2-model@p_x[i])
        }else{
          model@prior_par[[i]]=c( model@prior_par[[i]],a[i])
        }
        model@prior_par[[i]]=c( model@prior_par[[i]],b[i])
        
        
        model@R0[[i]]=as.list(1:model@p_x[i])
        
        for(i_p_x in 1:model@p_x[i]){
          model@R0[[i]][[i_p_x]] = as.matrix(abs(outer(model@input[[i]][,i_p_x], model@input[[i]][,i_p_x], "-")))
        }
        
        if(kernel_type[i]=='pow_exp'){
          model@alpha[[i]]=alpha[[i]]
        }
      
    }
    
  }
  
  
  model@sd_proposal_theta=sd_proposal_theta
  
  model@sd_proposal_cov_par=as.list(rep(0,num_sources+ measurement_bias))
  
  
  if(is.null(sd_proposal_cov_par)){
    for(i in 1:(num_sources)){
      model@sd_proposal_cov_par[[i]]=rep(0.25,dim(model@input[[i]])[2]+1)
    }
    ##add this for measurement_bias
    if(measurement_bias){
      ##it looks the one for the model discrepancy should be smaller
      # model@sd_proposal_cov_par[[num_sources+1]]=c(rep(0.25,dim(model@input[[num_sources+1]])[2])) 
       model@sd_proposal_cov_par[[num_sources+1]]=c(rep(0.15,dim(model@input[[num_sources+1]])[2])) 
       
    }
  }else{
    model@sd_proposal_cov_par=sd_proposal_cov_par
  }
  
  
  model@lambda_z=as.list(1:(num_sources+measurement_bias))
  
  for(i in 1:num_sources){
    if(is.na(lambda_z[[i]][1])){ ##this is comment case
      if(discrepancy_type[i]=='S-GaSP'){
        model@lambda_z[[i]]=rep(NA,S) ##this is to sample lambda_z
      }
    }else{
      if(length(lambda_z[[i]])==1){  ## a list of constant
          model@lambda_z[[i]]=rep(lambda_z[[i]],S) ##user may only give a value
      }else{
          model@lambda_z[[i]]=as.vector(lambda_z[[i]])
      }
    }
  }
  if(measurement_bias){
    if(discrepancy_type[num_sources+1]=='S-GaSP'){ ##use a fixed lambda_z first?
      if(is.na(lambda_z[[num_sources+1]][1])){ ##this is comment case
          model@lambda_z[[num_sources+1]]=rep(100*sqrt(length(model@output[[1]]) ),S) ##fix to be constant for multicalibration with measurement bias
      }else{
        if(length(lambda_z[[num_sources+1]])==1){  ## a list of constant
          model@lambda_z[[num_sources+1]]=rep(lambda_z[[num_sources+1]],S) ##user may only give a value
        }else{
          model@lambda_z[[num_sources+1]]=as.vector(lambda_z[[num_sources+1]])
        }
      }
    }
  }
  # if(measurement_bias){
  #     if(discrepancy_type[num_sources+1]=='S-GaSP'){
  #       model@lambda_z[[num_sources+1]]=rep(NA,S) ##
  #     }
  #   }
  # }else{
  #   if(length(lambda_z[[1]])==1){  ## a list of constant
  #     for(i in 1:num_sources){
  #       model@lambda_z[[i]]=rep(lambda_z[[i]],S) ##user may only give a value
  #     }
  #     if(measurement_bias){
  #       model@lambda_z[[num_sources+1]]=rep(lambda_z[[num_sources+1]],S) ##
  #     }
  #   }else{
  #     for(i in 1:num_sources){
  #       model@lambda_z[[i]]=as.vector(lambda_z[[i]])
  #     }
  #     if(measurement_bias){
  #       model@lambda_z[[num_sources+1]]=as.vector(lambda_z[[num_sources+1]])
  #     }
  #   }
  # }
  #model@lambda_z=lambda_z
  

  model@S=as.integer(S)
  model@S_0=as.integer(S_0)
  model@thinning=as.integer(thinning)
  
  
  #prior parameter for jointly robust prior
  
  
  
  model@simul_type=as.integer(simul_type)
  
  par_cur_theta=rowMeans(theta_range)  ###current par
  
  par_cur_individual=as.list(rep(0,num_sources+measurement_bias))
  math_model_MS=list()
  
  model@emulator_type=rep(NA,num_sources)
  emulator=as.list(rep(NA,num_sources))
  model@emulator_rgasp=as.list(rep(NA,num_sources))
  model@emulator_ppgasp=as.list(rep(NA,num_sources))

  for(i in 1:num_sources){
    if(model@simul_type[i]==1){
      math_model_MS[[i]]=math_model[[i]]
    }
    #model@simul_type[i]=as.integer(simul_type[i])
    
    ## enmulator for each computer model for each source of data
    if(model@simul_type[i]==0){
      cat('Starting emulation \n')
      output_simul[[i]]=as.matrix(output_simul[[i]])
      input_simul[[i]]=as.matrix(input_simul[[i]])
      if(dim(input_simul[[i]])[1]!=dim(output_simul[[i]])[1]){
        stop("The number of rows of the input_simul and the output_simul is not the same \n")
      }
      
      
      if(dim(output_simul[[i]])[2]==1){ ##vector output
        emulator[[i]]=rgasp(design=input_simul[[i]], response=output_simul[[i]],nugget.est=simul_nug[i])
        model@emulator_type[i]='rgasp'
        model@emulator_rgasp[[i]]=emulator[[i]]
          
        cat('end emulation for source ', i,'\n')
        cat('The inverse range  parameters from the emulation are ', model@emulator_rgasp[[i]]@beta_hat, '\n')
        cat('The nugget  parameter from the emulation is ', model@emulator_rgasp[[i]]@nugget, '\n')
        
      }else{### ppgasp 
        
        emulator[[i]]=ppgasp(design=input_simul[[i]], response=output_simul[[i]],nugget.est=simul_nug[i])
        model@emulator_type[i]='ppgasp'
        model@emulator_ppgasp[[i]]=emulator[[i]]
          
        if(!is.null(loc_index_emulator[[i]])){
          model@loc_index_emulator[[i]]=loc_index_emulator[[i]] ##only useful for ppgasp emulator
        }else{
          model@loc_index_emulator[[i]]=1:dim(output_simul[[i]])[2]
        }
        
      }

    }
    
    
    
    if(discrepancy_type[i]=='no-discrepancy'){
      par_cur_individual[[i]]= 1 #var par
    }else{
      par_cur_individual[[i]]=log(10/colMeans(abs(model@input[[i]])) )
      par_cur_individual[[i]]= c(par_cur_individual[[i]],0,1) #nugget and var par
    }
    
    if(model@have_trend[[i]]){
      par_cur_individual[[i]]=c( par_cur_individual[[i]],rep(0,dim(model@X[[i]])[2]))
    }
    
  }
  
  
  
  
  
  if(measurement_bias){
    par_cur_individual[[num_sources+1]]=c(log(10/colMeans(abs(model@input[[num_sources+1]])) ),1)
  }
  
  
  #cat('success \n')
  #cat(p_theta,' \n')
  
  ###initialize MCMC
  if(measurement_bias==F){
     sample_list=post_sample_MS(model,par_cur_theta, par_cur_individual, emulator,math_model_MS)
  }else{
    sample_list=post_sample_MS_with_measurement_bias(model,par_cur_theta, par_cur_individual, emulator,math_model_MS)
  }
  
  
  #start_S=floor((S_0+1)/thinning)+1
  start_S=floor((S_0)/thinning)+1
  
  end_S=floor(S/thinning)
    #dim(sample_list[[1]])[1]
  
  
  model@post_theta=as.matrix(sample_list$record_theta[start_S:end_S,]) 
  #model@post_individual_par=as.list(rep(0,model@num_sources))
  model@post_individual_par=list()
  
  index_emulator=0
  
  for(i in 1:model@num_sources){
    model@post_individual_par[[i]]=as.matrix(sample_list$individual_par[[i]][start_S:end_S,]) 
    if(model@simul_type[i]==0){
      index_emulator=1
    }
  }

  model@post_value=sample_list$record_post[start_S:end_S,] 
  
  
  if(measurement_bias==T){
    model@post_individual_par[[model@num_sources+1]]=as.matrix(sample_list$individual_par[[model@num_sources+1]][start_S:end_S,]) 
    if(model@discrepancy_type[num_sources+1]!='no-discrepancy'){
      model@post_delta=sample_list$record_model_bias[start_S:end_S,] 
    }else{
      model@post_delta=sample_list$record_model_bias
    }
     if(have_measurement_bias_recorded==T){
       for(i in 1:num_sources){
         model@post_measurement_bias[[i]]=sample_list$record_measurement_bias[[i]][start_S:end_S,] 
       }
     }
  }
  
  ##model@accept_S=c(sample_list$accept_S_theta,sample_list$accept_S_beta)
  
  model@accept_S_theta=sample_list$accept_S_theta
  model@accept_S_beta=sample_list$accept_S_beta
  
  model@count_boundary=sample_list$accept_S_dec
  
  for(i in 1:model@num_sources){
    if(discrepancy_type[i]=='S-GaSP' ){
      model@lambda_z[[i]]=sample_list$lambda_z[[i]]
    }
  }
  if(measurement_bias){
    model@lambda_z[[num_sources+1]]=sample_list$lambda_z[[num_sources+1]]
    
  }
  
  #sample_list
  return(model)
  
  # if(index_emulator==1){## if there is an emulator, we also return it
  #   return_list=list()
  #   return_list$model=model
  #   return(return_list)
  # }else{
  #   return(model)
  # }
}




# ##comment for now, the opt code for  rcalibration_MS is not ready
# rcalibration_MS_opt<-function(design, observations, p_theta=NULL,index_theta=NULL,X=as.list(rep(0,length(design))),
#                             have_trend=rep(FALSE,length(design)),
#                             simul_type=rep(0, length(design)), input_simul=NULL, output_simul=NULL,
#                             simul_nug=rep(FALSE,length(design)),math_model=NULL,
#                             theta_range=NULL,
#                             lambda_z=rep(100*sqrt(length(observations[[1]]))/length(observations[[1]]), length(design)),
#                             discrepancy_type=rep('S-GaSP',length(design)),
#                             kernel_type=rep('matern_5_2',length(design)),opt_type='MLE',
#                             alpha=NULL,num_initial_starts=1, initial_starts=NULL,
#                             output_weights=NULL,max_eval=50,xtol_rel=1e-5){
# 
#   if( !is.list(design) | !is.list(observations) ){
#     stop("The design and observations should both be a list for data from multiple sources \n")
# 
#   }
# 
#   if(is.null(p_theta)){
#     stop("Please specify `p_theta', the number of parameters to be calibrated \n")
#   }
# 
#   num_sources=length(design)
# 
# 
# 
#   model <- new("rcalibration_MS_opt")
# 
#   model@num_sources=num_sources
# 
#   ##default choice is that the all calibration parameters are shared across all sources
#   ##
#   if(is.null(index_theta)[1]){
#     model@index_theta=list()
#     for(i in 1:num_sources){
#       model@index_theta[[i]]=1:p_theta
#     }
#   }
# 
# 
#   if(is.null(theta_range) ){
#     stop("Please specify theta_range \n")
#   }
# 
# 
# 
#   model@p_x=rep(0,num_sources)
#   model@q=rep(0, num_sources)
# 
#   model@input=design
# 
#   ##add to the input if there is a shared one
#   model@output=observations
# 
#   model@output_weights=as.list(rep(0,num_sources))
#   model@X=as.list(1:(num_sources))
# 
#   model@have_trend=have_trend
# 
#   model@num_obs=rep(0,num_sources)
# 
#   for(i in 1: num_sources){
#     model@input[[i]]=as.matrix(model@input[[i]])
# 
#     model@p_x[i]=dim(model@input[[i]])[2]
#     model@num_obs[i]=dim(model@input[[i]])[1]
# 
# 
#     model@output_weights[[i]]=rep(1,  model@num_obs[i] )
#     model@X[[i]]=as.matrix(trend[[i]])
# 
#     if(model@have_trend[i]==TRUE){
#       model@q[i]=dim(model@X[[i]])[2]
#     }
# 
# 
#     if(simul_type[i]==1){
#       if(is.null(math_model)){
#         stop("Please specify the math_model  \n")
#       }
#     }
# 
#     if ( (discrepancy_type[i]!='no-discrepancy')&&(discrepancy_type[i]!='GaSP')&& (discrepancy_type[i]!='S-GaSP') ){
#       stop("simul_type should be chosen from no-discrepancy, GaSP or S-GaSP \n")
#     }
#     if(simul_type[i]==0){
#       if(is.null(input_simul[i]) | is.null(output_simul[i])  ){
#         stop("input_simul and output_simul are needed to be specified for constructing the emulator \n")
# 
#       }
#     }
#   }
# 
# 
#   ###
#   if(!is.null(output_weights)){
#     model@output_weights=output_weights
#   }
# 
# 
#   model@theta_range=theta_range
#   model@p_theta=as.integer(p_theta)
#   model@have_trend=have_trend
#   model@kernel_type=kernel_type
#   model@alpha=as.list(rep(0,num_sources))
#   model@discrepancy_type=discrepancy_type
# 
#   model@R0 =list()
# 
#   for(i in 1:(num_sources)){
# 
#     model@R0[[i]]=as.list(1:model@p_x[i])
# 
#     for(i_p_x in 1:model@p_x[i]){
#       model@R0[[i]][[i_p_x]] = as.matrix(abs(outer(model@input[[i]][,i_p_x], model@input[[i]][,i_p_x], "-")))
#     }
# 
#     if(kernel_type[i]=='pow_exp'){
#       model@alpha[[i]]=alpha[[i]]
#     }
# 
#   }
#   ###
#   model@lambda_z=lambda_z
# 
#   model@simul_type=as.integer(simul_type)
# 
#   model@emulator=emulator=as.list(rep(0,num_sources))
#   math_model_MS=list()
# 
#   for(i in 1:num_sources){
#     if(model@simul_type[i]==1){
#       math_model_MS[[i]]=math_model[[i]]
#     }
#     #model@simul_type[i]=as.integer(simul_type[i])
# 
#     ##
#     if(model@simul_type[i]==0){
#       cat('Starting emulation \n')
#       emulator[[i]]=rgasp(design=input_simul[[i]], response=output_simul[[i]],nugget.est=simul_nug[i])
# 
#       cat('end emulation \n')
#       cat('The inverse range  parameters from the emulation are ', emulator[[i]]@beta_hat, '\n')
#       cat('The nugget  parameter from the emulation is ', emulator[[i]]@nugget, '\n')
#       model@emulator[[i]]=emulator[[i]]
#     }
#   }
# 
# 
#   ##
#   ##this is related to the range of x
#   CL = list();
#   for(i in 1:num_sources){
#     CL[[i]]=rep(0,model@p_x[i])    ###CL is also used in the prior so I make it a model parameter
#     for(i_cl in 1:model@p_x[i]){
#       CL[[i]][i_cl] = (max(model@input[[i]][,i_cl])-min(model@input[[i]][,i_cl]))/model@num_obs[i]^{1/model@p_x[i]}
#     }
#   }
# 
# 
#   #model@initial_starts=list()
#   
#     initial_starts_matrix=matrix(0,num_initial_starts,model@p_theta+sum(model@p_x)+num_sources)
#     
#       
#     if(!is.null(initial_starts)){
#       initial_starts_matrix=initial_starts
#     }else{
#       index_start=model@p_theta+1
#       for(i_source in 1:num_sources){
#         initial_starts_matrix[1,1:model@p_theta]=rowMeans(theta_range)
#         xi_initial=log((1+model@p_x[i_source])/(model@p_x[i_source]*CL[[i_source]])/2)
#         log_eta_initial=log(0.01)
#         initial_starts_matrix[1,(index_start):(index_start+model@p_x[i_source])]=c(xi_initial,log_eta_initial)
#         index_start=index_start+model@p_x[i_source]+1
#       }
#       if(num_initial_starts>1){
#         for(i_starts in 2:num_initial_starts){
#           index_start=model@p_theta+1
#           for(i_source in 1:num_sources){
#             initial_starts_matrix[i_starts,1:model@p_theta]=rowMeans(theta_range)
#             xi_initial=log((1+model@p_x[i_source])/(model@p_x[i_source]*CL[[i_source]])/2)
#             log_eta_initial=log(0.01)
#             initial_starts_matrix[i_starts,(index_start):(index_start+model@p_x[i_source])]=c(xi_initial,log_eta_initial)+2*(runif(model@p_x[i_source]+1)-0.5)
#             index_start=index_start+model@p_x[i_source]+1
#           }
#         }  
#       }
# 
#    }
# 
#   model@initial_starts=as.matrix(initial_starts_matrix);
#   
#   neg_log_profile_lik_MS(param=initial_starts_matrix[1,],input=model@input, output=model@output, 
#                          R0_list=model@R0, kernel_type=model@kernel_type, p_theta=model@p_theta,
#                          p_x=model@p_x, output_weights=model@output_weights,
#            lambda_z=model@lambda_z,theta_range=model@theta_range, X=model@X, have_trend=model@have_trend, 
#            alpha=model@alpha,discrepancy_type=model@discrepancy_type,
#            simul_type=model@simul_type,emulator=model@emulator,math_model=math_model,only_value=F)
# 
#   
#   lower=c(theta_range[,1],rep(-Inf,dim(model@initial_starts)[2]-model@p_theta) )
#   upper=c(theta_range[,2],rep(Inf,dim(model@initial_starts)[2]-model@p_theta) )
#   
#   if(opt_type=='MLE'){
#     tt_all <- try(nloptr::lbfgs(model@initial_starts[1,], neg_log_profile_lik_MS, 
#                                 input=model@input, output=model@output, 
#                                 R0_list=model@R0, kernel_type=model@kernel_type, p_theta=model@p_theta,
#                                 p_x=model@p_x, output_weights=model@output_weights,
#                                 lambda_z=model@lambda_z,theta_range=model@theta_range, X=model@X, have_trend=model@have_trend, 
#                                 alpha=model@alpha,discrepancy_type=model@discrepancy_type,
#                                 simul_type=model@simul_type,emulator=model@emulator,math_model=math_model_MS,
#                                 lower=lower,upper=upper, nl.info = FALSE, control = list(maxeval=max_eval, xtol_rel=xtol_rel)),TRUE)
#   
#   
#   cat(paste('Done optimization for the initial start ',1,'. \n',sep=''))
#   cat(paste('The optimized calibration parameters are: ', tt_all$par[1:model@p_theta],'. \n',sep=''))
#   if(num_initial_starts>1){
#     for(i_start in 2:num_initial_starts ){
#       tt_all_try <- try(nloptr::lbfgs(model@initial_starts[i_start,], neg_log_profile_lik_MS, 
#                                       input=model@input, output=model@output, 
#                                       R0_list=model@R0, kernel_type=model@kernel_type, p_theta=model@p_theta,
#                                       p_x=model@p_x, output_weights=model@output_weights,
#                                       lambda_z=model@lambda_z,theta_range=model@theta_range, X=model@X, have_trend=model@have_trend, 
#                                       alpha=model@alpha,discrepancy_type=model@discrepancy_type,
#                                       simul_type=model@simul_type,emulator=model@emulator,math_model=math_model_MS,
#                                       lower=lower,upper=upper,nl.info = FALSE, control = list(maxeval=max_eval, xtol_rel=xtol_rel)),TRUE)
#       
#       if(class(tt_all)=="try-error"){
#         tt_all=tt_all_try
#       }else if(!class(tt_all_try)=="try-error"){
#         if(tt_all$value>tt_all_try$value){
#           tt_all=tt_all_try
#         }
#       }
#       cat(paste('Done optimization for the initial start ',i_start,'. \n',sep=''))
#       if(!class(tt_all_try)=="try-error"){
#         cat(paste('The optimized calibration parameters are: ', tt_all_try$par[1:model@p_theta],'. \n',sep=''))
#       }else{
#         cat("The optimization with the initial values leads to an error. \n")
#       }
#     }
#   }
#   
#   
#   
#   if(class(tt_all)=="try-error"){
#     #sink()
#     stop(tt_all)
#   }
#   
#   neg_log_lik_all=neg_log_profile_lik_MS(tt_all$par,input=model@input, output=model@output, 
#                                          R0_list=model@R0, kernel_type=model@kernel_type, p_theta=model@p_theta,
#                                          p_x=model@p_x, output_weights=model@output_weights,
#                                          lambda_z=model@lambda_z,theta_range=model@theta_range, X=model@X, 
#                                          have_trend=model@have_trend, 
#                                          alpha=model@alpha,discrepancy_type=model@discrepancy_type,
#                                          simul_type=model@simul_type,emulator=model@emulator,math_model=math_model_MS,
#                                          only_value=F)
#   
#   model@theta_est=tt_all$par[1:model@p_theta]
#   index_start=model@p_theta+1
#   for(i_source in 1:num_sources){
#     model@individual_param_est[[i_source]]=c(1/exp(tt_all$par[index_start:(index_start+model@p_x[i_source]-1)]),
#                     exp(tt_all$par[index_start+model@p_x[i_source]]),neg_log_lik_all[[i_source]][[2]])
#     if(model@have_trend[i_source]){
#       model@individual_param_est[[i_source]]=c(model@individual_param_est[[i_source]],neg_log_lik_all[[i_source]][[3]])
#     }
#     index_start=index_start+model@p_x[i_source]
#   }
#   model@opt_value=-neg_log_lik_all[[num_sources+1]]    
#   }else{
#     stop('We only support the MLE for optimization in this version.')
#   }
#   
#   return(model)
# }
