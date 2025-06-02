
##multiple sources
predict_MS.rcalibration_MS<-function(object, testing_input, X_testing=as.list(rep(0,object@num_sources)),
                                 testing_output_weights=NULL, 
                                 n_thinning=10,
                                  interval_est=NULL,interval_data=rep(F,length(testing_input)),math_model=NULL,...){
  

  if(object@measurement_bias==T){
    if(length(testing_input)!=(object@num_sources+1) ){
      stop("please specified the testing input for the model discrepancy. \n")
    }
  }else{
    if(length(testing_input)!=object@num_sources){
      stop("The number of sources in the testing input should match the number of sources in the object. \n")
    }
  }
  
  for(i_source in 1:object@num_sources){
    testing_input[[i_source]]=as.matrix(testing_input[[i_source]])
    ##make it a numeric matrix
    testing_input[[i_source]]=matrix(as.numeric(testing_input[[i_source]]), ncol = ncol(testing_input[[i_source]]))
    
  }
  if(object@measurement_bias==T){
    testing_input[[i_source+1]]=as.matrix( testing_input[[i_source+1]])
    ##make it a numeric matrix
    testing_input[[i_source+1]]=matrix(as.numeric(testing_input[[i_source+1]]), ncol = ncol(testing_input[[i_source+1]]))
    
  }
  
  if(is.null(testing_output_weights)){
    testing_output_weights=list()
    for(i_source in 1: object@num_sources){
      testing_output_weights[[i_source]]=rep(1,dim(testing_input[[i_source]])[1])
    }
  }
  
  
  if(object@measurement_bias==F){
    predict_obj=predict_MS_no_measurement_bias(object,testing_input,X_testing,testing_output_weights,interval_est,interval_data,math_model,n_thinning)
    
  }else{
    predict_obj=predict_MS_with_measurement_bias(object,testing_input,X_testing,testing_output_weights,interval_est,interval_data,math_model,n_thinning)
    
  }
  return(predict_obj)
  
  
}

predict_MS_no_measurement_bias<-function(object, testing_input, X_testing=as.list(rep(0,object@num_sources)),
                                        testing_output_weights=NULL, 
                                         interval_est=NULL,interval_data=rep(F,length(testing_input)),math_model=NULL,n_thinning=10){
  predictobj <- new("predictobj.rcalibration_MS")
  
  emulator=as.list(rep(NA,object@num_sources))
  
  for(i_source in 1:object@num_sources){
    if(object@simul_type[[i_source]]==0){
      if(length(object@emulator_ppgasp[[i_source]]@p>0)){ ##ppgasp
        emulator[[i_source]]=object@emulator_ppgasp[[i_source]] ##add sources
      }else{
        emulator[[i_source]]=object@emulator_rgasp[[i_source]]
        
      }
      
     }
   }
    
  
  for(i_source in 1:object@num_sources){
    ##sep 2022
    if(!is.null(interval_est)){
      record_interval=matrix(0,dim(testing_input[[i_source]])[1],length(interval_est[[i_source]]));
    }
    SS=floor(dim(object@post_theta)[1]/n_thinning)
    
    
    if(object@discrepancy_type[i_source]=='no-discrepancy'){
      record_cm_pred=0
      record_cm_pred_no_mean=0
      c_prop=1/4
      if(!is.null(interval_est)){
        if(interval_data[[i_source]]!=T){ ##interval for mean 
          record_sample_interval_mean=matrix(NA,dim(testing_input[[i_source]])[1],SS) ###mean for this source
        }
      }
      #Sep 2022
      count_i_S=0
      for(i_S in (1: SS)*n_thinning ){
        #print(i_S)
        #Sep 2022
        count_i_S=count_i_S+1
        
        if(i_S==floor(SS*n_thinning*c_prop)){
         cat(c_prop*100, 'percent is completed \n')
         c_prop=c_prop+1/4
        }
        
        
        theta=object@post_theta[i_S,] ##shared parameter
        sigma_2_0=object@post_individual_par[[i_source]][i_S,1] #the first one is the sigma_2
        
        mean_cm_test=mathematical_model_eval(testing_input[[i_source]],theta,object@simul_type[i_source],emulator[[i_source]],
                                             object@emulator_type[i_source],object@loc_index_emulator[[i_source]], math_model[[i_source]]);
        mean_cm_test_no_mean=mean_cm_test
        if(object@have_trend[i_source]){
          theta_m=as.matrix(object@post_individual_par[[i_source]][i_S,2:(1+object@q[i_source]) ])
          
          #object@post_sample[i_S,(object@p_theta+2):(object@p_theta+1+object@q)]
          mean_cm_test=mean_cm_test+X_testing[[i_source]]%*%theta_m
        }
        
        
        record_cm_pred=record_cm_pred+mean_cm_test
        record_cm_pred_no_mean=record_cm_pred_no_mean+mean_cm_test_no_mean
        
        if(!is.null(interval_est)){
          if(interval_data[[i_source]]==T){
            qnorm_all=qnorm(interval_est[[i_source]]);
            for(i_int in 1:length(interval_est[[i_source]]) ){
              record_interval[,i_int]=record_interval[,i_int]+mean_cm_test+qnorm_all[i_int]*sqrt(sigma_2_0/testing_output_weights[[i_source]])
            }
          }else{
            record_sample_interval_mean[,count_i_S]=mean_cm_test
            
          }
        }
        
      }
      
      record_cm_pred=record_cm_pred/floor(SS)
      record_cm_pred_no_mean=record_cm_pred_no_mean/floor(SS)
      #quilt.plot(x=input_ascending[,1], y=input_ascending[,2], z=record_cm_pred,nrow = 64, ncol = 64,main='real')
      
      #output.list[[i_source]]=list()
      
      #output.list[[i_source]]$math_model_mean=record_cm_pred
      
      predictobj@math_model_mean[[i_source]]=record_cm_pred
      predictobj@math_model_mean_no_trend[[i_source]]=record_cm_pred_no_mean
      if(!is.null(interval_est)){
        if(interval_data[[i_source]]==T){
        record_interval=record_interval/floor(SS)
        #output.list[[i_source]]$interval=record_interval
        predictobj@interval[[i_source]]=record_interval
        #ans.list[[2]]=record_interval
        }else{
          for(i_test in 1:dim(testing_input[[i_source]])[1]){
            record_interval[i_test,]=quantile(record_sample_interval_mean[i_test,],interval_est[[i_source]],na.rm = T)
            predictobj@interval[[i_source]]=record_interval
            
          }
          
        }
      }
      cat('Source',i_source, 'is completed \n')
      
      # return(output.list)
      
    }else{
      
      if(!is.null(interval_est)){
        c_star_record=rep(0,dim(testing_input[[i_source]])[1])
      }
      
      N_testing=dim(testing_input[[i_source]])[1]
      r0=as.list(1:object@p_x[i_source])
      for(i in 1:object@p_x[i_source]){
        r0[[i]]=abs(outer(object@input[[i_source]][,i],testing_input[[i_source]][,i],'-'))
      }
      
      record_cm_pred=0
      record_cm_pred_no_mean=0
      record_pred_mean=0
      

      #c_star=rep(0, n_testing)
      c_prop=1/4
      
      
      
      for(i_S in (1: SS)*n_thinning ){
        #print(i_S)
        
        if(i_S==floor(SS*n_thinning*c_prop)){
          cat(c_prop*100, 'percent is completed \n')
          c_prop=c_prop+1/4
        }
        
        theta=object@post_theta[i_S,]
        beta_delta=exp(object@post_individual_par[[i_source]][i_S,1:object@p_x[i_source]])
        eta_delta=exp(object@post_individual_par[[i_source]][i_S,object@p_x[i_source]+1])
        ##Sep 2022
        sigma_2_0=object@post_individual_par[[i_source]][i_S,object@p_x[i_source]+2]
        sigma_2_delta=sigma_2_0/eta_delta
        
        if(object@have_trend[i_source]){
          theta_m=as.matrix(object@post_individual_par[[i_source]][i_S,(object@p_x[i_source]+3):(object@p_x[i_source]+2+object@q[i_source])])
        }
        
        mean_cm=mathematical_model_eval(object@input[[i_source]],theta,object@simul_type[[i_source]],emulator[[i_source]],
                                        object@emulator_type[i_source],object@loc_index_emulator[[i_source]],math_model[[i_source]]);
        
        output_minus_cm=object@output[[i_source]]- mean_cm
        
        if(object@have_trend[i_source]){
          output_minus_cm=output_minus_cm-object@X[[i_source]]%*%theta_m
        }
        ##remember 
        if(object@discrepancy_type[i_source]=="GaSP"){
          L=Get_R_new( beta_delta,   eta_delta,  
                       object@R0[[i_source]], object@kernel_type[[i_source]],object@alpha[[i_source]],
                       1/object@output_weights[[i_source]])
        }else if(object@discrepancy_type[i_source]=="S-GaSP"){
          L=Get_R_z_new( beta_delta,   eta_delta,  object@lambda_z[[i_source]][i_S], ##allow sampling of lambda_z
                         object@R0[[i_source]], object@kernel_type[[i_source]],object@alpha[[i_source]],
                         1/object@output_weights[[i_source]])
        }
        
        ##Sep 2022, I change this back as I sample sigma_2_0 
        L=sqrt(eta_delta)*L
        
        R_inv_y=backsolve(t(L),forwardsolve(L,output_minus_cm ))
        
        if(object@discrepancy_type[i_source]=="S-GaSP"){
          R_inv_y=Update_R_inv_y(R_inv_y,object@R0[[i_source]],beta_delta,object@kernel_type[i_source],object@alpha[[i_source]],
                                 object@lambda_z[[i_source]][i_S],object@num_obs[i_source])
        }
        
        r=separable_kernel(r0,beta_delta,kernel_type=object@kernel_type[i_source],object@alpha[[i_source]])
        
        rt_R_inv_y=t(r)%*%R_inv_y
        
        mean_cm_test=mathematical_model_eval(testing_input[[i_source]],theta,object@simul_type[i_source],emulator[[i_source]],
                                             object@emulator_type[i_source],object@loc_index_emulator[[i_source]],math_model[[i_source]]);
        mean_cm_test_no_mean=mean_cm_test
        #f_M_testing=mean_cm_test
        
        if(object@have_trend[i_source]){
          mean_cm_test=mean_cm_test+X_testing[[i_source]]%*%theta_m
        }
        
        
        pred_mean=mean_cm_test+rt_R_inv_y
        
        record_cm_pred=record_cm_pred+mean_cm_test
        record_cm_pred_no_mean=record_cm_pred_no_mean+mean_cm_test_no_mean
        
        record_pred_mean=record_pred_mean+pred_mean
        
        
        
        
        if(!is.null(interval_est)){
          if(object@discrepancy_type[i_source]=="GaSP"){
            R_tilde_inv_r=(backsolve(t(L),forwardsolve(L, r )))
            
            for(i_testing in 1:N_testing){
              c_star_record[i_testing]=1-r[,i_testing]%*%R_tilde_inv_r[,i_testing]
            }
            
            ##Sep 2022
            c_star_record[which(c_star_record<0)]=0
            
            var_gasp_f=sigma_2_delta*c_star_record
            if(interval_data[i_source]==T){
              var_gasp_f=var_gasp_f+sigma_2_delta*eta_delta/testing_output_weights[[i_source]]
            }
            
            qnorm_all=qnorm(interval_est[[i_source]]);
            for(i_int in 1:length(interval_est[[i_source]]) ){
              record_interval[,i_int]=record_interval[,i_int]+pred_mean+qnorm_all[i_int]*sqrt(var_gasp_f)
              
            }
            
          }else if(object@discrepancy_type[i_source]=="S-GaSP"){
            R=separable_kernel(object@R0[[i_source]], beta_delta, object@kernel_type[i_source],
                               object@alpha[[i_source]])
            
            #R_tilde=R+1/object@lambda_z[[i_source]][i_S]*diag(object@num_obs[i_source]);  
            ##Sep 2022
            R_tilde=R+object@num_obs/object@lambda_z[[i_source]][i_S]*diag(object@num_obs[i_source]);
            
            #system.time(Chol_Eigen(R_tilde))
            
            L_R_tilde=Chol_Eigen(R_tilde)
            
            I_minus_R_R_tilde=diag(object@num_obs[i_source])-t(backsolve(t(L_R_tilde),forwardsolve(L_R_tilde,R )))
            
            
            R_tilde_inv_r=(backsolve(t(L_R_tilde),forwardsolve(L_R_tilde, r )))
            
            r_z=I_minus_R_R_tilde%*%r
            
            R_z_tilde_inv_r_z=(backsolve(t(L),forwardsolve(L, r_z )))
            
            for(i_testing in 1:N_testing){
              c_star_record[i_testing]=1-t(r[,i_testing])%*%R_tilde_inv_r[,i_testing]-r_z[,i_testing]%*%R_z_tilde_inv_r_z[,i_testing]
            }
            ##Sep 2022
            c_star_record[which(c_star_record<0)]=0
            
            #t(r[,i_testing])%*%solve(R_tilde)%*%r[,i_testing] 
            
            var_gasp_f=sigma_2_delta*c_star_record
            
            if(interval_data[i_source]==T){
              var_gasp_f=var_gasp_f+sigma_2_delta*eta_delta/testing_output_weights[[i_source]]
            }
            
            qnorm_all=qnorm(interval_est[[i_source]]);
            for(i_int in 1:length(interval_est[[i_source]]) ){
              record_interval[,i_int]=record_interval[,i_int]+pred_mean+qnorm_all[i_int]*sqrt(var_gasp_f)
              
            }
            
          }
        }  
      }
      
      
      
      #quilt.plot(x=input_ascending[,1], y=input_ascending[,2], z=record_cm_pred,nrow = 64, ncol = 64,main='real')
      
      #quilt.plot(x=input_ascending[,1], y=input_ascending[,2], z=record_pred_mean,nrow = 64, ncol = 64,main='real')
      
      record_cm_pred=record_cm_pred/floor(SS)
      record_cm_pred_no_mean=record_cm_pred_no_mean/floor(SS)
      record_pred_mean=record_pred_mean/floor(SS)
      
      predictobj@math_model_mean[[i_source]]=record_cm_pred
      predictobj@math_model_mean_no_trend[[i_source]]=record_cm_pred_no_mean
      predictobj@mean[[i_source]]=record_pred_mean
      
      #output.list[[i_source]]=list()
      
      #output.list[[i_source]]$math_model_mean=record_cm_pred
      #output.list[[i_source]]$mean=record_pred_mean
      
      if(!is.null(interval_est)){
        record_interval=record_interval/floor(SS)
        predictobj@interval[[i_source]]=record_interval
        
        #output.list[[i_source]]$interval=record_interval
      }
      
    }
    
  }
  
  return(predictobj)
  
  
}





predict_MS_with_measurement_bias<-function(object, testing_input, X_testing=as.list(rep(0,object@num_sources)),
                                       testing_output_weights=NULL, 
                                         interval_est=NULL,interval_data=rep(F,length(testing_input)),
                                       math_model=NULL, n_thinning=10){
  
    emulator=as.list(rep(NA,object@num_sources))
    
    for(i_source in 1:object@num_sources){
      if(object@simul_type[[i_source]]==0){
        if(length(object@emulator_ppgasp[[i_source]]@p>0)){ ##ppgasp
          emulator[[i_source]]=object@emulator_ppgasp[[i_source]]
        }else{
          emulator[[i_source]]=object@emulator_rgasp[[i_source]]
          
        }
        
      }
    }
  
    predictobj <- new("predictobj.rcalibration_MS")
  
    ##I add real interval for model and delta 
    SS=dim(object@post_individual_par[[1]])[1]/n_thinning
    
    if(!is.null(interval_est)){
      #record_interval_delta=matrix(0,dim(testing_input[[object@num_sources+1]]),length(interval_est[[object@num_sources+1]]));
      c_star_record_delta=rep(0,dim(testing_input[[object@num_sources+1]])[1])
      ##Sep 2022
      sample_record=as.list(1:2)
      for(i_source in 1:object@num_sources){
         sample_record[[i_source]]=matrix(NA,floor(SS),dim(testing_input[[i_source]])[1])
      }
      predictobj@interval=as.list(1:object@num_sources)
      
    }
    

    
    num_sources_1=object@num_sources+1
    num_obs=object@num_obs[1]  ##shared sample size
    
    
    ###still need to rewrite things into a big piece? 
    
    
    ##first get the model output ready
    for(i_source in 1:object@num_sources){
      record_cm_pred=0
      record_cm_pred_no_mean=0
      
      N_testing=dim(testing_input[[i_source]])[1]
      
      count_here=0
      for(i_S in (1: SS)*n_thinning ){
        count_here=count_here+1
        theta=object@post_theta[i_S,]
        mean_cm_test=mathematical_model_eval(testing_input[[i_source]],theta,object@simul_type[i_source],emulator[[i_source]],
                                             object@emulator_type[i_source],object@loc_index_emulator[[i_source]],math_model[[i_source]]);
        mean_cm_test_no_mean=mean_cm_test
        #f_M_testing=mean_cm_test
        
        if(object@have_trend[i_source]){
          theta_m=as.matrix(object@post_individual_par[[i_source]][i_S,(object@p_x[i_source]+3):(object@p_x[i_source]+2+object@q[i_source])])
          mean_cm_test=mean_cm_test+X_testing[[i_source]]%*%theta_m
        }
        record_cm_pred=record_cm_pred+mean_cm_test
        record_cm_pred_no_mean=record_cm_pred_no_mean+mean_cm_test_no_mean
        
        sample_record[[i_source]][count_here,]=mean_cm_test
      }
      
      record_cm_pred=record_cm_pred/floor(SS)
      record_cm_pred_no_mean=record_cm_pred_no_mean/floor(SS)
      
      predictobj@math_model_mean[[i_source]]=record_cm_pred
      predictobj@math_model_mean_no_trend[[i_source]]=record_cm_pred_no_mean
      
    }
    
     #quantile(sample_record[[i_source]][,10],c(0.025,0.975))
     #quilt.plot(x=(testing_input[[6]][,1]), y=(testing_input[[6]][,2]),
     #          z=predictobj@math_model_mean_no_trend[[3]],nrow = 50, ncol = 50,zlim=c(-0.03,0.05))
    
    cat('Complete the mathematical model prediction \n')
    
    ##model bias

    r0=list()
    for(i in 1:object@p_x[num_sources_1]){
      r0[[i]]=abs(outer(object@input[[num_sources_1]][,i],testing_input[[num_sources_1]][,i],'-'))
    }
    
    #var_delta=0;
    ##Sep 2022
    #par_cur_individual=as.list(1:num_sources_1)
    #is_SGaSP=rep(NA,num_sources_1)
    #lambda_z_cur=rep(NA,num_sources_1)
    #for(j_source in 1:num_sources_1){
    #  is_SGaSP[j_source]=sum(object@discrepancy_type[j_source]=='S-GaSP')
    #}
    delta_mean=rep(0,dim(testing_input[[1]])[1])
    
    if(object@discrepancy_type[num_sources_1]!='no-discrepancy'){
      count_here=0
      for(i_S in  (1: SS)*n_thinning ){
        count_here=count_here+1
        #print(i_S)
        ##Sep 2022
        #for(j_source in 1:num_sources_1){
        #  par_cur_individual[[j_source]]=object@post_individual_par[[j_source]][i_S,]
        #  lambda_z_cur[j_source]=object@lambda_z[[j_source]][i_S]
        #}
        
        par_cur=object@post_individual_par[[num_sources_1]][i_S,]
        #object@post_individual_par[[num_sources_1]][,object@p_x[num_sources_1]+1]
        ##this should be fine for delta when measurement bias is presentd
        sigma_2_delta=par_cur[object@p_x[num_sources_1]+1]
        ##even if it is S-GaSP, when there is no noise, it is the same as GaSP  in prediction
        
        L_delta=Get_R_new(exp(par_cur[1:object@p_x[num_sources_1]]),0,
                         object@R0[[num_sources_1]],object@kernel_type[num_sources_1],object@alpha[[num_sources_1]], 
                          rep(1,num_obs));
        Sigma_inv_delta=(backsolve(t(L_delta),forwardsolve(L_delta, object@post_delta[i_S,] )))
        
        ##Sep 2022 
        #cov_inv_all=Get_inv_all(par_cur_individual, lambda_z_cur, is_SGaSP,object@R0,  object@kernel_type, object@alpha, object@p_x, object@num_sources)
        
        
        r=separable_kernel(r0,exp(par_cur[1:object@p_x[num_sources_1]]),kernel_type=object@kernel_type[num_sources_1],object@alpha[[num_sources_1]])
        
        pred_mean= t(r)%*%Sigma_inv_delta
        
        delta_mean=delta_mean+ pred_mean
        
        if(!is.null(interval_est)){
            #R_tilde_inv_r=(backsolve(t(L_delta),forwardsolve(L_delta, r )))
            
            #for(i_testing in 1:N_testing){
            #  c_star_record_delta[i_testing]=1-r[,i_testing]%*%R_tilde_inv_r[,i_testing]
            #}
           for(i_source in 1:object@num_sources){
             sample_record[[i_source]][count_here,]=sample_record[[i_source]][count_here,]+pred_mean
           }
            #var_delta=var_delta+sigma_2_delta*c_star_record_delta
  
        }
      }

    #quantile(sample_record[[1]][,20],c(0.025,0.975))

    
      delta_mean=delta_mean/floor(SS)
  
      #var_delta=var_delta/floor(SS)
  

      #quilt.plot(x=(testing_input[[num_sources_1]][,1]), y=(testing_input[[num_sources_1]][,2]),
      #           z=delta_mean,nrow = 50, ncol = 50,zlim=c(-0.03,0.05))
      
        
      cat('Complete the model discrepancy \n')
    }
    predictobj@delta_mean=delta_mean
    
    ##measurement bias 

    
    for(i_source in 1: object@num_sources){
      #measurement_bias_with_mean=0
      measurement_bias_no_mean=0
      
      for(i in 1:object@p_x[i_source]){
        r0[[i]]=abs(outer(object@input[[i_source]][,i],testing_input[[i_source]][,i],'-'))
      }
      
      N_testing=dim(testing_input[[i_source]])[1]
      
      #if(!is.null(interval_est)){
      #  record_interval=matrix(0,dim(testing_input[[i_source]])[1],length(interval_est[[i_source]]));
      #  c_star_record=rep(0,dim(testing_input[[i_source]])[1])
        
      #}
      
      #var_measurement_bias=0;
      count_here=0
      abs_sd_data=0;
      for(i_S in  (1: SS)*n_thinning ){
        count_here=count_here+1
        #print(i_S)
        theta=object@post_theta[i_S,]
        beta_delta=exp(object@post_individual_par[[i_source]][i_S,1:object@p_x[i_source]])
        eta_delta=exp(object@post_individual_par[[i_source]][i_S,object@p_x[i_source]+1])
        
        ##Sep 2022
        sigma_2_0=object@post_individual_par[[i_source]][i_S,object@p_x[i_source]+2]
        sigma_2_delta=sigma_2_0/eta_delta
        
        #theta=object@post_theta[i_S,]
        #beta_delta=exp(object@post_individual_par[[i_source]][i_S,1:object@p_x])
        #eta_delta=exp(object@post_individual_par[[i_source]][i_S,object@p_x+1])
        

        mean_cm=mathematical_model_eval(object@input[[i_source]],theta,object@simul_type[[i_source]],emulator[[i_source]],
                                        object@emulator_type[i_source],object@loc_index_emulator[[i_source]],math_model[[i_source]]);

        output_minus_cm=object@output[[i_source]]- mean_cm
        
        if(object@have_trend[i_source]){
          theta_m=as.matrix(object@post_individual_par[[i_source]][i_S,(object@p_x[i_source]+3):(object@p_x[i_source]+2+object@q[i_source])])
          output_minus_cm=output_minus_cm-object@X[[i_source]]%*%theta_m
        }
        
        output_minus_cm_minus_delta=output_minus_cm
        
        if(object@discrepancy_type[num_sources_1]!='no-discrepancy'){
          output_minus_cm_minus_delta=output_minus_cm_minus_delta-object@post_delta[i_S,]
        }

        if(object@discrepancy_type[i_source]=="GaSP"){
          L=Get_R_new( beta_delta,   eta_delta,  
                       object@R0[[i_source]], object@kernel_type[[i_source]],object@alpha[[i_source]],
                       1/object@output_weights[[i_source]])
        }else if(object@discrepancy_type[i_source]=="S-GaSP"){
          L=Get_R_z_new( beta_delta,   eta_delta,  object@lambda_z[[i_source]][i_S],
                         object@R0[[i_source]], object@kernel_type[[i_source]],object@alpha[[i_source]],
                         1/object@output_weights[[i_source]])
        }
        ##Sep 2022, I change this back as I sample sigma_2_0 
        L=sqrt(eta_delta)*L
        
        R_inv_y=backsolve(t(L),forwardsolve(L,output_minus_cm_minus_delta ))
        
        if(object@discrepancy_type[i_source]=="S-GaSP"){
          ##need to check 
          R_inv_y=Update_R_inv_y(R_inv_y,object@R0[[i_source]],beta_delta,object@kernel_type[i_source],object@alpha[[i_source]],
                                 object@lambda_z[[i_source]][i_S],object@num_obs[i_source])
        }
        
        r=separable_kernel(r0,beta_delta,kernel_type=object@kernel_type[i_source],object@alpha[[i_source]])
        
        rt_R_inv_y=t(r)%*%R_inv_y
        
        
        if(!is.null(interval_est)){
            sample_record[[i_source]][count_here,]=sample_record[[i_source]][count_here,]+rt_R_inv_y
            if(interval_data[i_source]==T){
             #  sample_record[[i_source]][count_here,]=sample_record[[i_source]][count_here,]+sqrt(sigma_2_delta*eta_delta/testing_output_weights[[i_source]])*rnorm(N_testing)
              abs_sd_data=abs_sd_data+sqrt(sigma_2_delta*eta_delta/testing_output_weights[[i_source]])
            }
        }
        # quilt.plot(x=(testing_input[[num_sources_1]][,1]), y=(testing_input[[num_sources_1]][,2]),
        #            z=rt_R_inv_y,nrow = 100, ncol = 100,zlim=c(-0.03,0.05))
        
        measurement_bias_no_mean=measurement_bias_no_mean+rt_R_inv_y
        #if(object@have_trend[i_source]){
        #   measurement_bias_with_mean=measurement_bias_with_mean+rt_R_inv_y+X_testing[[i_source]]%*%theta_m
        #}
        
        
        
      }
      measurement_bias_no_mean=measurement_bias_no_mean/floor(SS)
      #measurement_bias_with_mean=measurement_bias_with_mean/floor(SS)
      #var_measurement_bias=var_measurement_bias/floor(SS)
      predictobj@measurement_bias_mean[[i_source]]=measurement_bias_no_mean
      
      # if(!is.null(interval_est)){
      #   qnorm_all=qnorm(interval_est[[i_source]]);
      #   for(i_int in 1:length(interval_est[[i_source]]) ){
      #     record_interval[,i_int]=record_interval[,i_int]+measurement_bias_no_mean+qnorm_all[i_int]*sqrt(var_measurement_bias+var_delta)
      #   }
      #   
      #   predictobj@interval[[i_source]]=record_interval
      # }
      predictobj@mean[[i_source]]=predictobj@delta_mean+predictobj@measurement_bias_mean[[i_source]]+predictobj@math_model_mean[[i_source]]
      
      ##Sep 2022
      if(!is.null(interval_est)){
        # qnorm_all=qnorm(interval_est[[i_source]]);
        # for(i_int in 1:length(interval_est[[i_source]]) ){
        #   record_interval[,i_int]=record_interval[,i_int]+predictobj@mean[[i_source]]+qnorm_all[i_int]*(sqrt(abs(var_measurement_bias) )+sqrt(abs(var_delta) )) ###this is still to estimate
        # }
        #record_interval=record_interval/floor(SS)

        predictobj@interval[[i_source]]=matrix(NA,N_testing,length(interval_est[[i_source]]))
        for(i_testing in 1:N_testing){
          predictobj@interval[[i_source]][i_testing,]=quantile(sample_record[[i_source]][,i_testing],interval_est[[i_source]])
        }
        for(i_int in 1:length(interval_est[[i_source]]) ){
          predictobj@interval[[i_source]][,i_int]=    predictobj@interval[[i_source]][,i_int]+abs_sd_data/floor(SS)*qnorm(interval_est[[i_source]][i_int])
        }
      }
      
      #quilt.plot(x=(testing_input[[3]][,1]), y=(testing_input[[3]][,2]),
      #          z= predictobj@mean[[3]],nrow = 50, ncol = 50)
      
    }
    # quantile(sample_record[[i_source]][,10],c(0.025,0.975))
    #  plot(predictobj@interval[[1]][,1],type='l')
    #  lines(predictobj@interval[[1]][,2],type='l')
      #lines(truth)
      
     return(predictobj)
        
}




# 2D lattice code, comment it for now 
predict_separable_2dim_MS<-function(object, testing_input_separable,
                                             X_testing=NULL,math_model=NULL,...){


  if(object@measurement_bias==T){
    if(length(testing_input_separable)!=(object@num_sources+1) ){
      stop("please specified the testing input for the model discrepancy. \n")
    }
  }else{
    if(length(testing_input_separable)!=object@num_sources){
      stop("The number of sources in the testing input should match the number of sources in the object. \n")
    }
  }

  if(object@measurement_bias==F){
    predict_obj=predict_separable_2dim_MS_no_measurement_bias(object,testing_input_separable,X_testing,math_model)

  }else{
    predict_obj=predict_separable_2dim_MS_with_measurement_bias(object,testing_input_separable,X_testing,math_model)

  }
  return(predict_obj)

}
# 
# with discrepancy and measurement bias
predict_separable_2dim_MS_no_measurement_bias<-function(object,testing_input_separable,X_testing,math_model){
  predictobj <- new("predictobj.rcalibration_MS")

  emulator=as.list(rep(NA,object@num_sources))
  
  for(i_source in 1:object@num_sources){
    if(object@simul_type[[i_source]]==0){
      if(length(object@emulator_ppgasp[[i_source]]@p>0)){ ##ppgasp
        emulator[[i_source]]=object@emulator_ppgasp[[i_source]] ##add sources
      }else{
        emulator[[i_source]]=object@emulator_rgasp[[i_source]]
        
      }
      
    }
  }
  
  r_separable=list()
  r0_separable=list()
  for(i_source in 1:object@num_sources){
    SS=floor(dim(object@post_theta)[1])

      #N_testing=dim(testing_input[[i_source]])[1]
      #r0=as.list(1:object@p_x[i_source])
      for(i in 1:object@p_x[i_source]){
        r0_separable[[i]]=abs(outer(object@input[[i_source]][,i],testing_input_separable[[i_source]][[i]],'-'))
      }

      record_cm_pred=0
      record_cm_pred_no_mean=0
      record_pred_mean=0

      SS=floor(dim(object@post_theta)[1])

      #c_star=rep(0, n_testing)
      c_prop=1/4

      testing_input[[i_source]]=expand.grid(testing_input_separable[[i_source]][[1]],testing_input_separable[[i_source]][[2]])
      testing_input[[i_source]]=as.matrix(testing_input[[i_source]])
      

      for(i_S in (1: SS) ){
        #print(i_S)

        if(i_S==floor(SS*c_prop)){
          cat(c_prop*100, 'percent is completed \n')
          c_prop=c_prop+1/4
        }

        theta=object@post_theta[i_S,]
        beta_delta=exp(object@post_individual_par[[i_source]][i_S,1:object@p_x[i_source]])
        eta_delta=exp(object@post_individual_par[[i_source]][i_S,object@p_x[i_source]+1])
        
        ##Sep 2022
        sigma_2_0=object@post_individual_par[[i_source]][i_S,object@p_x[i_source]+2]
        sigma_2_delta=sigma_2_0/eta_delta
        #sigma_2_delta=object@post_individual_par[[i_source]][i_S,object@p_x[i_source]+2]
        if(object@have_trend[i_source]){
          theta_m=as.matrix(object@post_individual_par[[i_source]][i_S,(object@p_x[i_source]+3):(object@p_x[i_source]+2+object@q[i_source])])
        }

        #mean_cm=mathematical_model_eval(object@input[[i_source]],theta,object@simul_type[[i_source]],emulator[[i_source]],math_model[[i_source]]);
        #Sep 2022
        mean_cm=mathematical_model_eval(object@input[[i_source]],theta,object@simul_type[i_source],emulator[[i_source]],
                                             object@emulator_type[i_source],object@loc_index_emulator[[i_source]], math_model[[i_source]]);
        
        output_minus_cm=object@output[[i_source]]- mean_cm

        if(object@have_trend[i_source]){
          output_minus_cm=output_minus_cm-object@X[[i_source]]%*%theta_m
        }
        ##remember
        if(object@discrepancy_type[i_source]=="GaSP"){
          L=Get_R_new( beta_delta,   eta_delta,
                       object@R0[[i_source]], object@kernel_type[[i_source]],object@alpha[[i_source]],
                       1/object@output_weights[[i_source]])
        }else if(object@discrepancy_type[i_source]=="S-GaSP"){
          L=Get_R_z_new( beta_delta,   eta_delta,  object@lambda_z[[i_source]][i_S],
                         object@R0[[i_source]], object@kernel_type[[i_source]],object@alpha[[i_source]],
                         1/object@output_weights[[i_source]])
        }
        ##Sep 2022
        L=sqrt(eta_delta)*L
        
        R_inv_y=backsolve(t(L),forwardsolve(L,output_minus_cm ))

        if(object@discrepancy_type[i_source]=="S-GaSP"){
          R_inv_y=Update_R_inv_y(R_inv_y,object@R0[[i_source]],beta_delta,object@kernel_type[i_source],object@alpha[[i_source]],
                                 object@lambda_z[[i_source]][i_S],object@num_obs[i_source])
        }


        for(i_x in 1:2){
          if(object@kernel_type[i_source]=="matern_5_2"){
            r_separable[[i_x]]=matern_5_2_funct(r0_separable[[i_x]],beta_delta[i_x])
          }else if(object@kernel_type[i_source]=="matern_3_2"){
            r_separable[[i_x]]=matern_3_2_funct(r0_separable[[i_x]],beta_delta[i_x])
          }else if(object@kernel_type[i_source]=="pow_exp"){
            r_separable[[i_x]]=pow_exp_funct(r0_separable[[i_x]],beta_delta[i_x],object@alpha[[i_source]][i_x])
          }
        }


        r2_dot_R_inv_y=r_separable[[2]]*as.vector(R_inv_y)

        rt_R_inv_y=as.vector(t(r_separable[[1]])%*%r2_dot_R_inv_y)

        #r=separable_kernel(r0,beta_delta,kernel_type=object@kernel_type[i_source],object@alpha[[i_source]])

        #rt_R_inv_y=t(r)%*%R_inv_y

        #mean_cm_test=mathematical_model_eval(testing_input[[i_source]],theta,object@simul_type[i_source],emulator[[i_source]],math_model[[i_source]]);
        #Sep 2022
        mean_cm_test=mathematical_model_eval(testing_input[[i_source]],theta,object@simul_type[i_source],emulator[[i_source]],
                                             object@emulator_type[i_source],object@loc_index_emulator[[i_source]], math_model[[i_source]]);
        
        mean_cm_test_no_mean=mean_cm_test
        #f_M_testing=mean_cm_test

        if(object@have_trend[i_source]){
          mean_cm_test=mean_cm_test+X_testing[[i_source]]%*%theta_m
        }


        pred_mean=mean_cm_test+rt_R_inv_y

        record_cm_pred=record_cm_pred+mean_cm_test
        record_cm_pred_no_mean=record_cm_pred_no_mean+mean_cm_test_no_mean

        record_pred_mean=record_pred_mean+pred_mean



      }



      #quilt.plot(x=input_ascending[,1], y=input_ascending[,2], z=record_cm_pred,nrow = 64, ncol = 64,main='real')

      #quilt.plot(x=input_ascending[,1], y=input_ascending[,2], z=record_pred_mean,nrow = 64, ncol = 64,main='real')

      record_cm_pred=record_cm_pred/floor(SS)
      record_cm_pred_no_mean=record_cm_pred_no_mean/floor(SS)
      record_pred_mean=record_pred_mean/floor(SS)

      predictobj@math_model_mean[[i_source]]=record_cm_pred
      predictobj@math_model_mean_no_trend[[i_source]]=record_cm_pred_no_mean
      predictobj@mean[[i_source]]=record_pred_mean

      #output.list[[i_source]]=list()

      #output.list[[i_source]]$math_model_mean=record_cm_pred
      #output.list[[i_source]]$mean=record_pred_mean



  }



  return(predictobj)


}
# 
# 
predict_separable_2dim_MS_with_measurement_bias<-function(object,testing_input_separable,X_testing,math_model){

  predictobj <- new("predictobj.rcalibration_MS")

  emulator=as.list(rep(NA,object@num_sources))
  
  for(i_source in 1:object@num_sources){
    if(object@simul_type[[i_source]]==0){
      if(length(object@emulator_ppgasp[[i_source]]@p>0)){ ##ppgasp
        emulator[[i_source]]=object@emulator_ppgasp[[i_source]] ##add sources
      }else{
        emulator[[i_source]]=object@emulator_rgasp[[i_source]]
        
      }
      
    }
  }
  
  SS=dim(object@post_individual_par[[1]])[1]

  num_sources_1=object@num_sources+1
  num_obs=object@num_obs[1]  ##shared sample size
  testing_input=list()
  ##first get the model output ready
  for(i_source in 1:object@num_sources){
    #print(i_source)
    record_cm_pred=0
    record_cm_pred_no_mean=0


    testing_input[[i_source]]=expand.grid(testing_input_separable[[i_source]][[1]],testing_input_separable[[i_source]][[2]])
    testing_input[[i_source]]=as.matrix(testing_input[[i_source]])

    #N_testing=dim(testing_input[[i_source]][[1]])[1]

    for(i_S in (1: SS) ){
      theta=object@post_theta[i_S,]
      #mean_cm_test=mathematical_model_eval(testing_input[[i_source]],theta,object@simul_type[i_source],emulator[[i_source]],math_model[[i_source]]);
      ##Sep 2022
      mean_cm_test=mathematical_model_eval(testing_input[[i_source]],theta,object@simul_type[i_source],emulator[[i_source]],
                                           object@emulator_type[i_source],object@loc_index_emulator[[i_source]], math_model[[i_source]]);
      
      mean_cm_test_no_mean=mean_cm_test
      #f_M_testing=mean_cm_test

      if(object@have_trend[i_source]){
        theta_m=as.matrix(object@post_individual_par[[i_source]][i_S,(object@p_x[i_source]+3):(object@p_x[i_source]+2+object@q[i_source])])
        mean_cm_test=mean_cm_test+X_testing[[i_source]]%*%theta_m
      }
      record_cm_pred=record_cm_pred+mean_cm_test
      record_cm_pred_no_mean=record_cm_pred_no_mean+mean_cm_test_no_mean

    }

    record_cm_pred=record_cm_pred/floor(SS)
    record_cm_pred_no_mean=record_cm_pred_no_mean/floor(SS)

    predictobj@math_model_mean[[i_source]]=record_cm_pred
    predictobj@math_model_mean_no_trend[[i_source]]=record_cm_pred_no_mean

  }

  #quilt.plot(x=(testing_input_separable[[6]][[1]]), y=(testing_input_separable[[6]][[2]]),
  #           z=predictobj@math_model_mean_no_trend[[3]],nrow = 50, ncol = 50,zlim=c(-0.03,0.05))

  #image2D(t(matrix(predictobj@math_model_mean_no_trend[[3]],541,469)))
  cat('Complete the mathematical model prediction \n')


  ##model bias

  r0_separable=list()
  for(i_x in 1:2){
    r0_separable[[i_x]]=as.matrix(abs(outer(object@input[[num_sources_1]][,i_x],testing_input_separable[[num_sources_1]][[i_x]],'-')))
  }

  delta_mean=0
  #var_delta=0;
  r_separable=list()

  for(i_S in (1: SS) ){
    #print(i_S)
    par_cur=object@post_individual_par[[num_sources_1]][i_S,]
    beta_delta=exp(par_cur[1:object@p_x[num_sources_1]])
    sigma_2_delta=par_cur[object@p_x[num_sources_1]+1]

    ##even if it is S-GaSP, when there is no noise, it is the same as GaSP  in prediction
    L_delta=Get_R_new(beta_delta,0,
                      object@R0[[num_sources_1]],object@kernel_type[num_sources_1],object@alpha[[num_sources_1]],
                      rep(1,num_obs));
    Sigma_inv_delta=(backsolve(t(L_delta),forwardsolve(L_delta, object@post_delta[i_S,] )))

    ##only work for two dimensions
    for(i_x in 1:2){
      if(object@kernel_type[num_sources_1]=="matern_5_2"){
        r_separable[[i_x]]=matern_5_2_funct(r0_separable[[i_x]],beta_delta[i_x])
      }else if(object@kernel_type[num_sources_1]=="matern_3_2"){
        r_separable[[i_x]]=matern_3_2_funct(r0_separable[[i_x]],beta_delta[i_x])
      }else if(object@kernel_type[num_sources_1]=="pow_exp"){
          r_separable[[i_x]]=pow_exp_funct(r0_separable[[i_x]],beta_delta[i_x],object@alpha[[num_sources_1]][i_x])
      }
    }


    r2_dot_R_inv_y=r_separable[[2]]*as.vector(Sigma_inv_delta)


    pred_mean=as.vector(t(r_separable[[1]])%*%r2_dot_R_inv_y)


    #pred_mean= t(r)%*%Sigma_inv_delta

    delta_mean=delta_mean+ pred_mean

  }



  delta_mean=delta_mean/floor(SS)

  predictobj@delta_mean=delta_mean

  #image2D(t(matrix(delta_mean,541,469)))

  #quilt.plot(x=(testing_input[[num_sources_1]][,1]), y=(testing_input[[num_sources_1]][,2]),
  #           z=delta_mean,nrow = 50, ncol = 50,zlim=c(-0.03,0.05))


  cat('Complete the model discrepancy \n')


  ##measurement bias

  for(i_source in 1: object@num_sources){
    #print(i_source)
    #measurement_bias_with_mean=0
    measurement_bias_no_mean=0

    for(i in 1:object@p_x[i_source]){
      r0_separable[[i]]=abs(outer(object@input[[i_source]][,i],testing_input_separable[[i_source]][[i]],'-'))
    }

    #N_testing=dim(testing_input[[i_source]])[1]

    for(i_S in 1: SS ){
      #print(i_S)
      theta=object@post_theta[i_S,]
      beta_delta=exp(object@post_individual_par[[i_source]][i_S,1:object@p_x[i_source]])
      eta_delta=exp(object@post_individual_par[[i_source]][i_S,object@p_x[i_source]+1])
      #sigma_2_delta=object@post_individual_par[[i_source]][i_S,object@p_x[i_source]+2]
      ##Sep 2022
      #sigma_2_0=object@post_individual_par[[i_source]][i_S,object@p_x[i_source]+2]
      #sigma_2_delta=sigma_2_0/eta_delta
      
      #mean_cm=mathematical_model_eval(object@input[[i_source]],theta,object@simul_type[[i_source]],emulator[[i_source]],math_model[[i_source]]);
      #Sep 2022
      mean_cm=mathematical_model_eval(object@input[[i_source]],theta,object@simul_type[i_source],emulator[[i_source]],
                                      object@emulator_type[i_source],object@loc_index_emulator[[i_source]], math_model[[i_source]]);
      
      output_minus_cm=object@output[[i_source]]- mean_cm

      if(object@have_trend[i_source]){
        theta_m=as.matrix(object@post_individual_par[[i_source]][i_S,(object@p_x[i_source]+3):(object@p_x[i_source]+2+object@q[i_source])])
        output_minus_cm=output_minus_cm-object@X[[i_source]]%*%theta_m
      }

      output_minus_cm_minus_delta=output_minus_cm-object@post_delta[i_S,]


      if(object@discrepancy_type[i_source]=="GaSP"){
        L=Get_R_new( beta_delta,   eta_delta,
                     object@R0[[i_source]], object@kernel_type[[i_source]],object@alpha[[i_source]],
                     1/object@output_weights[[i_source]])
      }else if(object@discrepancy_type[i_source]=="S-GaSP"){
        L=Get_R_z_new( beta_delta,   eta_delta,  object@lambda_z[[i_source]][i_S],
                       object@R0[[i_source]], object@kernel_type[[i_source]],object@alpha[[i_source]],
                       1/object@output_weights[[i_source]])
      }
      
      ##Sep 2022
      L=sqrt(eta_delta)*L
      

      R_inv_y=backsolve(t(L),forwardsolve(L,output_minus_cm_minus_delta ))

      if(object@discrepancy_type[i_source]=="S-GaSP"){
        R_inv_y=Update_R_inv_y(R_inv_y,object@R0[[i_source]],beta_delta,object@kernel_type[i_source],object@alpha[[i_source]],
                               object@lambda_z[[i_source]][i_S],object@num_obs[i_source])
      }

      for(i_x in 1:2){
        if(object@kernel_type[i_source]=="matern_5_2"){
          r_separable[[i_x]]=matern_5_2_funct(r0_separable[[i_x]],beta_delta[i_x])
        }else if(object@kernel_type[i_source]=="matern_3_2"){
          r_separable[[i_x]]=matern_3_2_funct(r0_separable[[i_x]],beta_delta[i_x])
        }else if(object@kernel_type[i_source]=="pow_exp"){
          r_separable[[i_x]]=pow_exp_funct(r0_separable[[i_x]],beta_delta[i_x],object@alpha[[i_source]][i_x])
        }
      }


      r2_dot_R_inv_y=r_separable[[2]]*as.vector(R_inv_y)

      pred_mean=as.vector(t(r_separable[[1]])%*%r2_dot_R_inv_y)

      #r=separable_kernel(r0,beta_delta,kernel_type=object@kernel_type[i_source],object@alpha[[i_source]])

      #rt_R_inv_y=t(r)%*%R_inv_y

      # quilt.plot(x=(testing_input[[num_sources_1]][,1]), y=(testing_input[[num_sources_1]][,2]),
      #            z=rt_R_inv_y,nrow = 100, ncol = 100,zlim=c(-0.03,0.05))

      #image2D(t(matrix(pred_mean,541,469)))

      measurement_bias_no_mean=measurement_bias_no_mean+pred_mean

    }

    measurement_bias_no_mean=measurement_bias_no_mean/floor(SS)
    #measurement_bias_with_mean=measurement_bias_with_mean/floor(SS)
    #var_measurement_bias=var_measurement_bias/floor(SS)
    predictobj@measurement_bias_mean[[i_source]]=measurement_bias_no_mean

    predictobj@mean[[i_source]]=predictobj@delta_mean+predictobj@measurement_bias_mean[[i_source]]+predictobj@math_model_mean[[i_source]]
  }

 # image2D((matrix(predictobj@mean[[1]],541,469))



  return(predictobj)

}



