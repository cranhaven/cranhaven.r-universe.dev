##########################################################################
## rcalibration fit function
##########################################################################


rcalibration <- function(design, observations, p_theta=NULL,X=matrix(0,dim(as.matrix(design))[1],1), have_trend=FALSE,
                         simul_type=1, input_simul=NULL, output_simul=NULL,simul_nug=FALSE,loc_index_emulator=NULL,
                         math_model=NULL,
                         theta_range=NULL, sd_proposal=NULL,
                         S=10000,S_0=2000,thinning=1, discrepancy_type='S-GaSP', kernel_type='matern_5_2',
                         lambda_z=NA, a=1/2-dim(as.matrix(design))[2],b=1,
                         alpha=rep(1.9,dim(as.matrix(design))[2]), output_weights=rep(1,dim(as.matrix(design))[1]),
                         method='post_sample',initial_values=NULL,num_initial_values=3, ...){
  
  if(is.null(theta_range)){
    stop("Please specify the theta_range for the range of calibration parameters \n")
    
  }
  
  if(!is.null(p_theta)){
    p_theta=as.integer(p_theta)
  }else{
    p_theta=dim(as.matrix(theta_range) )[1]
  }
  
  if(simul_type==1){
    if(is.null(math_model)){
      stop("Please specify the math_model  \n")
    }
  }
  if(!is.null(math_model) ){ ##in this case the math model is given
    simul_type=1;
  }
  if(is.null(p_theta)){
    stop("Please specify `p_theta', the number of parameters to be calibrated \n")
  }
  
  if (simul_type>3 | simul_type<0){
    stop("simul_type should be chosen from 0, 1, 2 or 3\n")
  }
  if ( (discrepancy_type!='no-discrepancy')&&(discrepancy_type!='GaSP')&& (discrepancy_type!='S-GaSP') ){
    stop("simul_type should be chosen from no-discrepancy, GaSP or S-GaSP \n")
  }
  
  if(simul_type==0){
    if(is.null(input_simul) | is.null(output_simul)  ){
      stop("input_simul and output_simul are needed to be specified for constructing the emulator \n")
      
    }
  }
  
  ##perform kernel check 
  if( (!(kernel_type=='matern_5_2'))&(!(kernel_type=='matern_3_2'))&(!(kernel_type=='pow_exp'))){
    stop("only 'matern_5_2', 'matern_3_2' and 'pow_exp' are implemented. See kernel_type for more information.")
  }
  
  ##set sd_proposal is not given
  if(is.null(sd_proposal)){
    sd_proposal=c(0.05*(as.matrix(theta_range)[,2]-as.matrix(theta_range)[,1]),
      rep(0.25,dim(as.matrix(design))[2]),0.25)
  }
  model <- new("rcalibration")
  design=as.matrix(design)
  ##make it a numeric matrix
  design=matrix(as.numeric(design), ncol = ncol(design))
  
  
  model@p_x=dim(design)[2]
  #model@p_theta=p_theta
  model@num_obs=dim(design)[1]
  
  X=as.matrix(X)
  if (model@num_obs != dim(X)[1]){
    stop("The dimensions of the design matrix and trend do not match. \n")
  }
  
  
  if(simul_type==0){
    model@p_theta=p_theta;
    #model@p_theta=dim(input_simul)[2]-model@p_x
    #if(model@p_theta!=p_theta){
    #  stop("the dimension of the input_simul is wrong \n");
    #}
    ##theta_range=matrix(0,model@p_theta,2);
    # if(is.null(theta_range)){
    #   theta_range[,1]=apply(input_simul[(model@p_x+1):(model@p_x+model@p_theta),],2,min)
    #   theta_range[,1]=theta_range[,1]-0.001*abs(theta_range[,1]) ##do not let the design point be the boundary
    #   theta_range[,2]=apply(input_simul[(model@p_x+1):(model@p_x+model@p_theta),],2,max)
    #   theta_range[,2]=theta_range[,2]+0.001*abs(theta_range[,2])
    # }
    
  }else if (simul_type==1){
    model@p_theta=p_theta;
  }else if(simul_type==2 | simul_type==3){ ##Kilauea volcano
    model@p_theta=as.integer(5);
    if(is.null(theta_range)){
      theta_range=matrix(0,model@p_theta,2)
      theta_range[1,]=c(-2000, 3000)  
      theta_range[2,]=c(-2000, 5000)  
      theta_range[3,]=c(500, 6000)  
      theta_range[4,]=c(0, .3)   
      theta_range[5,]=c(.25, .33)  
    }
  }
  
  
  model@input=design
  
  ##deal with replicate
  model@S_2_f=0
  model@have_replicates=F
  
  if(is.numeric(observations)){
    if(length(observations)==model@num_obs){##no replicate
      model@output=(observations)
      model@num_obs_all=model@num_obs
    }else{ ##this is replicate, same for each one
      if(dim(observations)[1]!=model@num_obs){
        stop("The number of row in the observation matrix
             should be the same of the number of rows in the design matrix. \n")
      }
      
      model@have_replicates=T
      model@num_replicates=rep(dim(observations)[2],model@num_obs)  ##
      
      model@num_obs_all=sum(model@num_replicates)  ##total number of invididual obs
      
      row_mean_obs=rowMeans(observations)  ##give output value
      #model@S_2_f=sum( (observations-row_mean_obs)^2)
      
      model@S_2_f=sum(output_weights*rowSums( (observations-row_mean_obs)^2))
      
      output_weights=output_weights*(model@num_replicates)
      model@output=as.matrix(row_mean_obs)
    }
  }else if(is.list(observations)){
    model@have_replicates=T
    if(length(observations)!=model@num_obs){
      stop("The number of row in the observation matrix
             should be the same of the number of rows in the design matrix. \n")
    }
    model@have_replicates=T
    model@num_replicates=sapply(observations,length)
    model@num_obs_all=sum(model@num_replicates)  ##total number of indididual obs
    
    row_mean_obs=sapply(observations,mean)
    model@S_2_f=0
    for(i in 1:model@num_obs){
      model@S_2_f=model@S_2_f+output_weights[i]*sum(observations[[i]]-row_mean_obs[i])^2
    }
    output_weights=output_weights*(model@num_replicates)
    
    model@output=as.matrix(row_mean_obs)
  }
  
  
  #model@output=observations
  
  model@X=X
  model@q=dim(model@X)[2]
  
  model@R0 = as.list(1:model@p_x)
  for(i in 1:model@p_x){
    model@R0[[i]] = as.matrix(abs(outer(model@input[,i], model@input[,i], "-")))
  }
  model@kernel_type=kernel_type
  model@alpha=alpha
  
  model@S=as.integer(S)
  model@S_0=as.integer(S_0)
  model@thinning=as.integer(thinning)
  #prior parameter for jointly robust prior
  CL = rep(0,model@p_x)    ###CL is also used in the prior so I make it a model parameter
  
  for(i_cl in 1:model@p_x){
    CL[i_cl] = (max(model@input[,i_cl])-min(model@input[,i_cl]))/model@num_obs^{1/model@p_x}
  }
  model@prior_par=c(CL,a,b)
  
  model@output_weights=output_weights
  
  
  
  model@have_trend=have_trend
  
  if(have_trend==F){
    model@X=matrix(rep(0,model@num_obs),model@num_obs,1)
    model@q=as.integer(0);
  }
  
  
  model@theta_range=theta_range
  
  model@sd_proposal=sd_proposal
  
  if(discrepancy_type=='no-discrepancy'){
    model@sd_proposal=sd_proposal[1:p_theta]
  }
  model@discrepancy_type=discrepancy_type
  
  model@simul_type=as.integer(simul_type)
  
  ##
  if(model@simul_type==0){
    cat('Starting emulation \n')
    output_simul=as.matrix(output_simul)
    input_simul=as.matrix(input_simul)
    if(dim(input_simul)[1]!=dim(output_simul)[1]){
      stop("The number of rows of the input_simul and the output_simul is not the same \n")
    }
    if(dim(output_simul)[2]==1){ ##vector output
      emulator=rgasp(design=input_simul, response=output_simul,nugget.est=simul_nug)
      model@emulator_type='rgasp'
      model@emulator_rgasp=emulator
      
    }else{ ##matrix output, ppgasp
      emulator=ppgasp(design=input_simul, response=output_simul,nugget.est=simul_nug)
      model@emulator_type='ppgasp'
      model@emulator_ppgasp=emulator
      
      if(!is.null(loc_index_emulator)){
        model@loc_index_emulator=loc_index_emulator ##only useful for ppgasp emulator
      }else{
        model@loc_index_emulator=1:dim(output_simul)[2]
      }
    }
    
    #param_emulator=c(emulator@beta_hat)
    #if(simul_nug==T){
    #  param_emulator=c(param_emulator,emulator@nugget)
    #}
    cat('end emulation \n')
    cat('The inverse range  parameters from the emulation are ', emulator@beta_hat, '\n')
    cat('The nugget  parameter from the emulation is ', emulator@nugget, '\n')
    
  }else{
    emulator=NA
    #model@loc_index_emulator=NULL
    model@emulator_type='NA'
  }
  
  model@method=method
  
  if(model@method=='post_sample'){
    if(is.na(lambda_z[1])){
      model@lambda_z=rep(NA,S)
    }else{
      if(length(lambda_z)==1){
        model@lambda_z=rep(lambda_z,S) ##user may only give a value
      }else{
        model@lambda_z=as.vector(lambda_z)
      }
    }
    #model@lambda_z=lambda_z
    
    # ###initial parameter
    # if(simul_type==0){
    #   if(model@p_theta>1){
    #     #par_cur=c(colMeans(input_simul[(model@p_x+1):(model@p_x+model@p_theta),]))
    #     par_cur=c(colMeans(input_simul[,(model@p_x+1):(model@p_x+model@p_theta)]))
    #     
    #   }else{
    #     #par_cur=c(mean(input_simul[(model@p_x+1):(model@p_x+model@p_theta),]))
    #     par_cur=c(mean(input_simul[,(model@p_x+1):(model@p_x+model@p_theta)]))
    #     
    #   }
    # }else if(simul_type==1){
    #   par_cur=rowMeans(theta_range)  ###current par
    # }else if(simul_type==2 | simul_type==3){
    #   par_cur=rowMeans(theta_range)  ###current par
    # }
    ##Sep 2022, allow the initial values of the theta in posterior sample
    if(is.null(initial_values)){
       par_cur=rowMeans(theta_range)  ###current par
    }else{
      par_cur=initial_values
    }
    
    if(discrepancy_type=='no-discrepancy'){
      par_cur= c(par_cur,1) #var par
    }else{
      for(i_x in 1: model@p_x){
        par_cur= c(par_cur,log(10/sd(model@input[,i_x])))
        #   Input_list[[8]]= c(Input_list[[8]], -5)
      }
      par_cur= c(par_cur,0,1) #nugget and var par
    }
    if(model@have_trend){
      par_cur=c(par_cur,rep(0,dim(model@X)[2]))
    }
    #cat('success \n')
    #cat(p_theta,' \n')
    
    
    ##print(model@lambda_z[1:10])
    ###initialize MCMC
    sample_list=post_sample(model@input, model@output, model@R0, model@kernel_type, model@p_theta, model@output_weights,
                            par_cur, model@lambda_z, model@prior_par,model@theta_range,model@S,model@thinning, model@X, model@have_trend, model@alpha,model@sd_proposal,
                            model@discrepancy_type, model@simul_type,emulator,model@emulator_type,loc_index_emulator,
                            math_model,model@S_2_f,model@num_obs_all);
    
    start_S=floor((S_0+1)/thinning)
    end_S=dim(sample_list[[1]])[1]
    
    model@post_sample=sample_list[[1]][(start_S:end_S),]
    model@post_value=sample_list[[2]][start_S:end_S]
    model@accept_S=sample_list[[3]][1:(length(sample_list[[3]])-1)]
    model@count_boundary=sample_list[[3]][length(sample_list[[3]])]
    if(discrepancy_type=='S-GaSP' ){
      model@lambda_z=sample_list[[4]][start_S:end_S]
    }
  }else if(model@method=='mle'){
    ## have some initial starts
    
    if(model@discrepancy_type=='no-discrepancy'){
      
      initial_values_matrix=matrix(0,num_initial_values,model@p_theta)
      
      if(!is.null(initial_values)){
        initial_values_matrix=initial_values
      }else{
        initial_values_matrix[1,]=rowMeans(theta_range)
        if(num_initial_values>1){
          for(i_start in 2:num_initial_values){
            initial_values_matrix[i_start,]=quantile(theta_range,(i_start-1)/num_initial_values)+(runif(model@p_theta)-0.5)*rowMeans(theta_range)*0.5
            
            #initial_values_matrix[i_start,]=rowMeans(theta_range)+(runif(model@p_theta)-0.5)*rowMeans(theta_range)*0.5
          }
        }
      }
      
      lower=c(theta_range[,1])
      upper=c(theta_range[,2])
      
      #cm_obs=mathematical_model_eval(model@input,initial_values_matrix[1,],model@simul_type,model@emulator,math_model);
      model@initial_values=initial_values_matrix;
      
      tt_all <- try(nloptr::lbfgs(model@initial_values[1,], loss_function_no_discrepancy,
                                  input=model@input, output=model@output, p_theta=model@p_theta,
                                  output_weights=model@output_weights, 
                                  X=model@X, have_trend=model@have_trend, simul_type=model@simul_type,emulator=emulator,
                                  emulator_type=model@emulator_type,loc_index_emulator=model@loc_index_emulator,
                                  only_value=T,math_model=math_model,num_obs_all=model@num_obs_all, S_2_f=model@S_2_f,
                                  lower=lower,upper=upper,  nl.info = FALSE),TRUE)
      
      
      
      # loss_function_no_discrepancy(param=model@initial_values[1,],input=model@input, output=model@output, p_theta, output_weights,
      #                                        X, have_trend,simul_type,emulator,emulator_type=model@emulator_type,loc_index_emulator=model@loc_index_emulator,math_model,num_obs_all=model@num_obs_all,
      #                                        S_2_f=model@S_2_f,only_value=T)
        

      cat('Done optimization for the initial start ',1,'\n')
      
      #if(!class(tt_all)=="try-error"){
      if(class(tt_all)[1]!="try-error"){
        cat('The optimized calibration parameters are: ', tt_all$par[1:model@p_theta],'\n')
      }
      if(num_initial_values>1){
        for(i_start in 2:num_initial_values ){
          tt_all_try <- try(nloptr::lbfgs(model@initial_values[i_start,], loss_function_no_discrepancy,
                                          input=model@input, output=model@output, p_theta=model@p_theta,
                                          output_weights=model@output_weights, 
                                          X=model@X, have_trend=model@have_trend, simul_type=model@simul_type,emulator=emulator,
                                          emulator_type=model@emulator_type,loc_index_emulator=model@loc_index_emulator,
                                          only_value=T,math_model=math_model,num_obs_all=model@num_obs_all, S_2_f=model@S_2_f,
                                          lower=lower,upper=upper, nl.info = FALSE),TRUE)
          
          

          
          if(class(tt_all)[1]=="try-error"){
            tt_all=tt_all_try
          }else if(class(tt_all_try)[1]!="try-error"){
            
            if(tt_all$value>tt_all_try$value){
              tt_all=tt_all_try
            }
          }
          cat('Done optimization for the initial start ',i_start,'\n')
          if(class(tt_all)[1]!="try-error"){
            cat('The optimized calibration parameters are: ', tt_all$par[1:model@p_theta],'\n')
          }else{
            cat("The optimization with the initial values leads to an error \n")
          }
        }
      }
      
      if(class(tt_all)[1]=="try-error"){
        #sink()
        stop(tt_all)
      }
      
      
      loss_all=loss_function_no_discrepancy(tt_all$par,input=model@input, output=model@output, p_theta=model@p_theta,
                                            output_weights=model@output_weights, 
                                            X=model@X, have_trend=model@have_trend, simul_type=model@simul_type,emulator=emulator,
                                            emulator_type=model@emulator_type,loc_index_emulator=model@loc_index_emulator,
                                            math_model=math_model,num_obs_all=model@num_obs_all, S_2_f=model@S_2_f,only_value=F)
      
      model@param_est=c(tt_all$par,loss_all[[2]])
      if(model@have_trend){
        model@param_est=c(model@param_est,loss_all[[3]])
      }
      model@opt_value=loss_all[[1]]
      
    }else{
      
      if(is.na(lambda_z[1])){
        model@lambda_z=NA ###there is a default way?
      }else{
        model@lambda_z=lambda_z ##user may only give a value
      }
      
      initial_values_matrix=matrix(0,num_initial_values,model@p_theta+model@p_x+1)
      
      if(!is.null(initial_values)){
        initial_values_matrix=initial_values
      }else{
        initial_values_matrix[1,1:model@p_theta]=rowMeans(theta_range)
        xi_initial=log((1+model@p_x)/(model@p_x*CL)/2)
        log_eta_initial=log(0.01)
        initial_values_matrix[1,(model@p_theta+1):(model@p_theta+model@p_x+1)]=c(xi_initial,log_eta_initial)
        
        if(num_initial_values>1){
          for(i_start in 2:num_initial_values){
            initial_values_matrix[i_start,1:model@p_theta]=quantile(theta_range,(i_start-1)/num_initial_values)+(runif(model@p_theta)-0.5)*rowMeans(theta_range)*0.5
            initial_values_matrix[i_start,(model@p_theta+1):(model@p_theta+model@p_x+1)]=c(xi_initial,log_eta_initial)+2*(runif(model@p_x+1)-0.5)
          }
        }
      }
      
      model@initial_values=initial_values_matrix;
      
      ##length of input
      length_input=rep(0,model@p_x)
      
      for(i_cl in 1:model@p_x){
        length_input[i_cl] = (max(model@input[,i_cl])-min(model@input[,i_cl]))
      }
      
      
      lower=c(theta_range[,1],rep(-Inf,model@p_x+1))
      upper=c(theta_range[,2],rep(Inf,model@p_x+1))
      
      #model@output_weights##rep(.1,10)
      tt_all <- try(nloptr::lbfgs(model@initial_values[1,], neg_log_profile_lik,
                                  input=model@input, output=model@output, R0_list=model@R0, kernel_type=model@kernel_type, p_theta=model@p_theta,
                                  p_x=model@p_x,output_weights=model@output_weights, lambda_z=model@lambda_z,theta_range=model@theta_range,
                                  X=model@X, have_trend=model@have_trend, alpha=model@alpha,
                                  discrepancy_type=model@discrepancy_type, simul_type=model@simul_type,
                                  emulator=emulator,emulator_type=model@emulator_type,loc_index_emulator=model@loc_index_emulator,
                                  only_value=T,math_model=math_model,
                                  length_input=length_input,num_obs_all=model@num_obs_all, S_2_f=model@S_2_f,
                                  lower=lower,upper=upper, nl.info = FALSE),TRUE)
      

      

      
      cat('Done optimization for the initial start ',1,'. \n')
      
      if(class(tt_all)[1]!="try-error"){
        cat('The optimized calibration parameters are: ', tt_all$par[1:model@p_theta],'\n')
        cat('The optimized range and nugget parameters are: ',c(1/exp(tt_all$par[(model@p_theta+1):(model@p_theta+model@p_x)]),
                                                                exp(tt_all$par[model@p_theta+model@p_x+1])),'\n')
      }
      if(num_initial_values>1){
        for(i_start in 2:num_initial_values ){
          tt_all_try <- try(nloptr::lbfgs(model@initial_values[i_start,], neg_log_profile_lik,
                                          input=model@input, output=model@output, R0_list=model@R0, kernel_type=model@kernel_type, p_theta=model@p_theta,
                                          p_x=model@p_x,output_weights=model@output_weights, lambda_z=model@lambda_z,theta_range=model@theta_range,
                                          X=model@X, have_trend=model@have_trend, alpha=model@alpha,
                                          discrepancy_type=model@discrepancy_type, simul_type=model@simul_type,emulator=emulator,
                                          emulator_type=model@emulator_type,loc_index_emulator=model@loc_index_emulator,math_model=math_model,
                                          length_input=length_input,num_obs_all=model@num_obs_all,S_2_f=model@S_2_f,
                                          lower=lower,upper=upper,only_value=T,nl.info = FALSE),TRUE)
          
          
          if(class(tt_all)[1]=="try-error"){
            tt_all=tt_all_try
          }else if(class(tt_all_try)[1]!="try-error"){
            if(tt_all$value>tt_all_try$value){
              tt_all=tt_all_try
            }
          }
          paste('Done optimization for the initial start ',i_start,'\n')
          if(class(tt_all)[1]!="try-error"){
            cat('The optimized calibration parameters are: ', tt_all$par[1:model@p_theta],'\n')
            cat('The optimized range and nugget parameters are: ',c(1/exp(tt_all$par[(model@p_theta+1):(model@p_theta+model@p_x)]),
                                                                    exp(tt_all$par[model@p_theta+model@p_x+1])),'\n')
          }else{
            cat("The optimization with the initial values leads to an error\n")
          }
        }
      }
      
      
      if(class(tt_all)[1]=="try-error"){
        #sink()
        stop(tt_all)
      }
      
      neg_log_lik_all=neg_log_profile_lik(tt_all$par,input=model@input, output=model@output, R0_list=model@R0, kernel_type=model@kernel_type, p_theta=model@p_theta,
                                          p_x=model@p_x,output_weights=model@output_weights, lambda_z=model@lambda_z,theta_range=model@theta_range,
                                          X=model@X, have_trend=model@have_trend, alpha=model@alpha,
                                          discrepancy_type=model@discrepancy_type, simul_type=model@simul_type,emulator=emulator,
                                          emulator_type=model@emulator_type,loc_index_emulator=model@loc_index_emulator,math_model=math_model,
                                          length_input=length_input,num_obs_all=model@num_obs_all,S_2_f=model@S_2_f,
                                          only_value=F)
      
      
      model@param_est=c(tt_all$par[1:model@p_theta],1/exp(tt_all$par[(model@p_theta+1):(model@p_theta+model@p_x)]),
                        exp(tt_all$par[model@p_theta+model@p_x+1]),neg_log_lik_all[[2]])
      if(model@have_trend){
        model@param_est=c(model@param_est,neg_log_lik_all[[3]])
      }
      model@opt_value=-neg_log_lik_all[[1]]
      
      
      if(is.na(model@lambda_z)&(model@discrepancy_type=='S-GaSP')){
        model@lambda_z=neg_log_lik_all[[4]] 
      }
      
    }
  }else{
    
    stop('We only support the posterior sampling or MLE in this version. Please adjust the method argument.')
    
  }
  return(model)
  
}


show.rcalibration <- function(object) {	
  
  
  #cat('The dimension  of the design is: ',dim(object@input),'\n')
  #cat('The number of the output is: ',dim(object@output),'\n')
  cat('type of discrepancy function: ',object@discrepancy_type,'\n')
  if(object@method=='post_sample'){
    cat('number of after burn-in posterior: ',object@S-object@S_0,'\n')
    cat(object@accept_S[1]/object@S, 'of proposed calibration parameters are accepted \n')
    #cat(object@accept_S[2]/object@S, 'of proposed range and nugget parameters are accepted \n')
    for( i in 1: object@p_theta){
      cat('median and 95% posterior credible interval of calibration parameter ',i, 'are', quantile(object@post_sample[,i], c(0.5,0.025,0.975) ),'\n')
    }
  }else{
    
    cat('calibration parameters: ', object@param_est[1:object@p_theta],'\n')
    if(!object@discrepancy_type=='no-discrepancy'){
      cat('range parameters: ', object@param_est[(object@p_theta+1):(object@p_theta+object@p_x)],'\n')
      cat('nugget parameter: ', object@param_est[object@p_theta+object@p_x+1],'\n')
    }
  }
  
}


