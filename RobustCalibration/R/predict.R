


predict.rcalibration<-function(object, testing_input, X_testing=NULL,
                               n_thinning=10, testing_output_weights=NULL, 
                               interval_est=NULL,interval_data=F,math_model=NULL,test_loc_index_emulator=NULL,...){
  predictobj<- new("predictobj.rcalibration")
  
  testing_input=as.matrix(testing_input)
  ##make it a numeric matrix
  testing_input=matrix(as.numeric(testing_input), ncol = ncol(testing_input))
  
  if(is.null(X_testing)){
    X_testing=matrix(0,dim(testing_input)[1],1)
  }
  if(is.null(testing_output_weights)){
    testing_output_weights=rep(1,dim(testing_input)[1])
  }
  if(object@simul_type==0){
    if(is.null(test_loc_index_emulator)){
      if(object@emulator_type=='ppgasp'){
        test_loc_index_emulator=1:dim(object@emulator_ppgasp@output)[2]
      }else{
        test_loc_index_emulator=c(1)
      }
    }
    if(length(object@emulator_ppgasp@p>0)){ ##ppgasp
      emulator=object@emulator_ppgasp
    }else{
      emulator=object@emulator_rgasp
      
    }
       
  }

  
  if(object@method=="post_sample"){
    
    record_cm_pred=0
    record_cm_pred_no_trend=0
    
    
    if(object@discrepancy_type=='no-discrepancy'){
      SS=floor(dim(object@post_sample)[1]/n_thinning)
      c_prop=1/4
      
      if(!is.null(interval_est)){
        record_interval=matrix(0,dim(testing_input)[1],length(interval_est));
        if(interval_data!=T){ ##interval for mean 
          record_sample_interval_mean=matrix(NA,dim(testing_input)[1],SS)
        }
      }
      count_i_S=0;
      for(i_S in (1: SS)*n_thinning ){
        count_i_S=count_i_S+1
        #print(i_S)
        
        if(i_S==floor(SS*n_thinning*c_prop)){
          cat(c_prop*100, 'percent is completed \n')
          c_prop=c_prop+1/4
        }
        
        # print(i_S)
        
        theta=object@post_sample[i_S,1:object@p_theta]
        sigma_2_0=object@post_sample[i_S,object@p_theta+1]
        
        mean_cm_test=mathematical_model_eval(testing_input,theta,object@simul_type,emulator,object@emulator_type,
                                             loc_index_emulator=test_loc_index_emulator,math_model);
        
        if(object@emulator_type=='ppgasp'){
          
        #if(dim(mean_cm_test)[1]==1){
          mean_cm_test=as.vector(mean_cm_test)
        #}
        }
        
        mean_cm_test_no_trend=mean_cm_test
        if(object@have_trend){
          theta_m=object@post_sample[i_S,(object@p_theta+2):(object@p_theta+1+object@q)]
          mean_cm_test=mean_cm_test+X_testing%*%theta_m
        }
        
        
        record_cm_pred=record_cm_pred+mean_cm_test
        
        record_cm_pred_no_trend=record_cm_pred_no_trend+mean_cm_test_no_trend
        if(!is.null(interval_est)){
          if(interval_data==T){
            qnorm_all=qnorm(interval_est);
            for(i_int in 1:length(interval_est) ){
              record_interval[,i_int]=record_interval[,i_int]+mean_cm_test+qnorm_all[i_int]*sqrt(sigma_2_0/testing_output_weights)
              
            }
            #if(interval_data==T){
            # for(i_int in 1:length(interval_est) ){
            #   record_interval[,i_int]=record_interval[,i_int]+qnorm_all[i_int]*sqrt(sigma_2_0/testing_output_weights)
            # }
            #}
          }else{##for mean 
            record_sample_interval_mean[,count_i_S]=mean_cm_test
          }
          
        }
      }
      record_cm_pred=record_cm_pred/floor(SS)
      record_cm_pred_no_trend=record_cm_pred_no_trend/floor(SS)
      #output.list <- list()
      
      #output.list$math_model_mean=record_cm_pred
      
      predictobj@math_model_mean=record_cm_pred
      predictobj@math_model_mean_no_trend=record_cm_pred_no_trend
      predictobj@mean=predictobj@math_model_mean
      
      if(!is.null(interval_est)){
        if(interval_data==T){
          record_interval=record_interval/floor(SS)
          #output.list$interval=record_interval
          predictobj@interval=record_interval
          #ans.list[[2]]=record_interval
        }else{ ###interval for mean
          for(i_test in 1:dim(testing_input)[1]){
            record_interval[i_test,]=quantile(record_sample_interval_mean[i_test,],interval_est,na.rm = T)
            predictobj@interval=record_interval
            
          }
        }
      }
      
    }else{  ###GaSP and S-GaSP
      if(!is.null(interval_est)){
        c_star_record=rep(0,dim(testing_input)[1])
        record_interval=matrix(0,dim(testing_input)[1],length(interval_est));
      }
      
      N_testing=dim(testing_input)[1]
      r0=as.list(1:object@p_x)
      for(i in 1:object@p_x){
        r0[[i]]=abs(outer(object@input[,i],testing_input[,i],'-'))
      }
      
      record_pred_mean=0
      ##Sep 2023, include delta
      record_delta=0
      
      
      SS=floor(dim(object@post_sample)[1]/n_thinning)
      
      #c_star=rep(0, n_testing)
      c_prop=1/4
      
      for(i_S in (1: SS)*n_thinning ){
        if(i_S==floor(SS*n_thinning*c_prop)){
          cat(c_prop*100, 'percent is completed \n')
          c_prop=c_prop+1/4
        }
        
        theta=object@post_sample[i_S,1:object@p_theta]
        beta_delta=exp(object@post_sample[i_S,(object@p_theta+1):(object@p_theta+object@p_x)])
        eta_delta=exp(object@post_sample[i_S,object@p_theta+object@p_x+1])
        sigma_2_0=object@post_sample[i_S,object@p_theta+object@p_x+2]
        
        sigma_2_delta=sigma_2_0/eta_delta
        if(object@have_trend){
          theta_m=object@post_sample[i_S,(object@p_theta+object@p_x+3):(object@p_theta+object@p_x+2+object@q)]
        }
        
        mean_cm=mathematical_model_eval(object@input,theta,object@simul_type,emulator,object@emulator_type,
                                        loc_index_emulator=object@loc_index_emulator, math_model); ##this is cm for field obs
        if(object@emulator_type=='ppgasp'){
        #  if(dim(mean_cm)[1]==1){
            mean_cm=as.vector(mean_cm)
        #  }
        }
        output_minus_cm=object@output- mean_cm
        
        if(object@have_trend){
          output_minus_cm=output_minus_cm-object@X%*%theta_m
        }
        ##remember 
        if(object@discrepancy_type=="GaSP"){
          L=Get_R_new( (beta_delta),   (eta_delta),  
                       object@R0, object@kernel_type,object@alpha,1/object@output_weights)
        }else if(object@discrepancy_type=="S-GaSP"){
          L=Get_R_z_new( (beta_delta),   (eta_delta),  object@lambda_z[i_S],
                         object@R0, object@kernel_type,object@alpha,1/object@output_weights)
        }
        
        ##July 11, 2020, I change this back as I sample sigma_2_0 
        L=sqrt(eta_delta)*L
        ###
        
        R_inv_y=backsolve(t(L),forwardsolve(L,output_minus_cm ))
        
        if(object@discrepancy_type=="S-GaSP"){
          R_inv_y=Update_R_inv_y(R_inv_y,object@R0,beta_delta,object@kernel_type,object@alpha,object@lambda_z[i_S],object@num_obs)
        }
        
        r=separable_kernel(r0,beta_delta,kernel_type=object@kernel_type,object@alpha)
        
        rt_R_inv_y=t(r)%*%R_inv_y
        
        mean_cm_test=mathematical_model_eval(testing_input,theta,object@simul_type,emulator,object@emulator_type,
                                             loc_index_emulator=test_loc_index_emulator,math_model);
        if(object@emulator_type=='ppgasp'){
          
        #if(dim(mean_cm_test)[1]==1){
            mean_cm_test=as.vector(mean_cm_test)
        #}
        }
        
        mean_cm_test_no_trend=mean_cm_test
        #f_M_testing=mean_cm_test
        
        if(object@have_trend){
          mean_cm_test=mean_cm_test+X_testing%*%theta_m
        }
        
        
        pred_mean=mean_cm_test+rt_R_inv_y
        
        record_cm_pred=record_cm_pred+mean_cm_test
        record_cm_pred_no_trend=record_cm_pred_no_trend+mean_cm_test_no_trend
        
        record_pred_mean=record_pred_mean+pred_mean
        ##Sep 2023, include delta
        record_delta=record_delta+(pred_mean-mean_cm_test)
          
        if(!is.null(interval_est)){
          if(object@discrepancy_type=="GaSP"){
            R_tilde_inv_r=(backsolve(t(L),forwardsolve(L, r )))
            
            for(i_testing in 1:N_testing){
              c_star_record[i_testing]=1-r[,i_testing]%*%R_tilde_inv_r[,i_testing]
            }
            var_gasp_f=sigma_2_delta*c_star_record
            if(interval_data==T){
              var_gasp_f=var_gasp_f+sigma_2_delta*eta_delta/testing_output_weights
            }
            
            qnorm_all=qnorm(interval_est);
            for(i_int in 1:length(interval_est) ){
              record_interval[,i_int]=record_interval[,i_int]+pred_mean+qnorm_all[i_int]*sqrt(var_gasp_f)
              
            }
            
          }else if(object@discrepancy_type=="S-GaSP"){
            R=separable_kernel(object@R0, beta_delta, object@kernel_type,object@alpha)
            R_tilde=R+object@num_obs/object@lambda_z[i_S]*diag(object@num_obs);
            
            
            #system.time(Chol_Eigen(R_tilde))
            
            L_R_tilde=Chol_Eigen(R_tilde)
            
            I_minus_R_R_tilde=diag(object@num_obs)-t(backsolve(t(L_R_tilde),forwardsolve(L_R_tilde,R )))
            
            
            R_tilde_inv_r=(backsolve(t(L_R_tilde),forwardsolve(L_R_tilde, r )))
            
            r_z=I_minus_R_R_tilde%*%r
            
            R_z_tilde_inv_r_z=(backsolve(t(L),forwardsolve(L, r_z )))
            
            for(i_testing in 1:N_testing){
              c_star_record[i_testing]=1-r[,i_testing]%*%R_tilde_inv_r[,i_testing]-r_z[,i_testing]%*%R_z_tilde_inv_r_z[,i_testing]
            }
            var_gasp_f=sigma_2_delta*c_star_record
            
            if(interval_data==T){
              var_gasp_f=var_gasp_f+sigma_2_delta*eta_delta/testing_output_weights
            }
            
            qnorm_all=qnorm(interval_est);
            for(i_int in 1:length(interval_est) ){
              record_interval[,i_int]=record_interval[,i_int]+pred_mean+qnorm_all[i_int]*sqrt(var_gasp_f)
              
            }
            
          }
          
        }
        
        
        
      }
      
      record_cm_pred=record_cm_pred/floor(SS)
      record_pred_mean=record_pred_mean/floor(SS)
      record_cm_pred_no_trend=record_cm_pred_no_trend/floor(SS)
      ##Sep 2023, include delta
      record_delta=record_delta/floor(SS)
        
      predictobj@math_model_mean=record_cm_pred
      predictobj@math_model_mean_no_trend=record_cm_pred_no_trend
      predictobj@mean=record_pred_mean
      ##Sep 2023, include delta
      predictobj@delta=record_delta
      
      
      if(!is.null(interval_est)){
        record_interval=record_interval/floor(SS)
        predictobj@interval=record_interval
      }
      
      
    }
  }else if(object@method=='mle'){
    
    if(object@discrepancy_type=='no-discrepancy'){
      if(!is.null(interval_est)){
        record_interval=matrix(0,dim(testing_input)[1],length(interval_est));
      }
      
      theta=object@param_est[1:object@p_theta]
      sigma_2_0=object@param_est[object@p_theta+1]
      
      mean_cm_test=mathematical_model_eval(testing_input,theta,object@simul_type,emulator,object@emulator_type,
                                           loc_index_emulator=test_loc_index_emulator,math_model);
      mean_cm_test_no_trend=mean_cm_test
      
      
      if(object@have_trend){
        theta_m=object@param_est[(object@p_theta+2):(object@p_theta+1+object@q)]
        mean_cm_test=mean_cm_test+X_testing%*%theta_m
      }
      
      
      #record_cm_pred=mean_cm_test
      #record_cm_pred_no_trend=mean_cm_test_no_trend
      
      
      if(!is.null(interval_est)&(interval_data==T)){
        qnorm_all=qnorm(interval_est);
        for(i_int in 1:length(interval_est) ){
          record_interval[,i_int]=mean_cm_test+qnorm_all[i_int]*sqrt(sigma_2_0/testing_output_weights)
        }
      }
      
      predictobj@math_model_mean=mean_cm_test
      predictobj@math_model_mean_no_trend=mean_cm_test_no_trend
      predictobj@mean=predictobj@math_model_mean ##mean as math model mean if no discrepancy 
      
      if(!is.null(interval_est)&(interval_data==T)){
        #record_interval=record_interval/floor(SS)
        predictobj@interval=record_interval
      }
      
      
      
      
    }else{
      ##GaSP or S-GaSP
      if(!is.null(interval_est)){
        c_star_record=rep(0,dim(testing_input)[1])
        record_interval=matrix(0,dim(testing_input)[1],length(interval_est));
      }
      
      N_testing=dim(testing_input)[1]
      r0=as.list(1:object@p_x)
      for(i in 1:object@p_x){
        r0[[i]]=abs(outer(object@input[,i],testing_input[,i],'-'))
      }
      
      
      ###starts
      theta=object@param_est[1:object@p_theta]
      beta_delta=1/(object@param_est[(object@p_theta+1):(object@p_theta+object@p_x)])
      eta_delta=(object@param_est[object@p_theta+object@p_x+1])
      sigma_2_0=object@param_est[object@p_theta+object@p_x+2]
      
      sigma_2_delta=sigma_2_0/eta_delta
      if(object@have_trend){
        theta_m=object@param_est[(object@p_theta+object@p_x+3):(object@p_theta+object@p_x+2+object@q)]
      }
      
      mean_cm=mathematical_model_eval(object@input,theta,object@simul_type,emulator,object@emulator_type,
                                      loc_index_emulator=object@loc_index_emulator,math_model);
      if(object@emulator_type=='ppgasp'){
        
      #if(dim(mean_cm)[1]==1){
        mean_cm=as.vector(mean_cm)
      #}
      }
      
      output_minus_cm=object@output- mean_cm
      
      if(object@have_trend){
        output_minus_cm=output_minus_cm-object@X%*%theta_m
      }
      ##remember 
      if(object@discrepancy_type=="GaSP"){
        L=Get_R_new( (beta_delta),   (eta_delta),  
                     object@R0, object@kernel_type,object@alpha,1/object@output_weights)
      }else if(object@discrepancy_type=="S-GaSP"){
        L=Get_R_z_new( (beta_delta),   (eta_delta),  object@lambda_z,
                       object@R0, object@kernel_type,object@alpha,1/object@output_weights)
      }
      
      ##July 11, 2020, I change this back as I sample sigma_2_0 
      L=sqrt(eta_delta)*L
      ###
      
      R_inv_y=backsolve(t(L),forwardsolve(L,output_minus_cm ))
      
      if(object@discrepancy_type=="S-GaSP"){
        R_inv_y=Update_R_inv_y(R_inv_y,object@R0,beta_delta,object@kernel_type,object@alpha,object@lambda_z,object@num_obs)
      }
      
      r=separable_kernel(r0,beta_delta,kernel_type=object@kernel_type,object@alpha)
      
      rt_R_inv_y=t(r)%*%R_inv_y
      
      mean_cm_test=mathematical_model_eval(testing_input,theta,object@simul_type,emulator,object@emulator_type,
                                           loc_index_emulator=test_loc_index_emulator,math_model);
      
      if(object@emulator_type=='ppgasp'){
        
      #if(dim(mean_cm_test)[1]==1){
        mean_cm_test=as.vector(mean_cm_test)
      #}
      }
      
      mean_cm_test_no_trend=mean_cm_test
      #f_M_testing=mean_cm_test
      
      if(object@have_trend){
        mean_cm_test=mean_cm_test+X_testing%*%theta_m
      }
      
      
      # pred_mean=mean_cm_test+rt_R_inv_y
      # record_cm_pred=record_cm_pred+mean_cm_test
      # record_cm_pred_no_trend=record_cm_pred_no_trend+mean_cm_test_no_trend
      # record_pred_mean=record_pred_mean+pred_mean
      
      predictobj@math_model_mean=mean_cm_test
      predictobj@math_model_mean_no_trend=mean_cm_test_no_trend
      predictobj@mean=mean_cm_test+rt_R_inv_y
      
      
      
      if(!is.null(interval_est)){
        if(object@discrepancy_type=="GaSP"){
          R_tilde_inv_r=(backsolve(t(L),forwardsolve(L, r )))
          
          for(i_testing in 1:N_testing){
            c_star_record[i_testing]=1-r[,i_testing]%*%R_tilde_inv_r[,i_testing]
          }
          var_gasp_f=sigma_2_delta*c_star_record
          if(interval_data==T){
            var_gasp_f=var_gasp_f+sigma_2_delta*eta_delta/testing_output_weights
          }
          
          qnorm_all=qnorm(interval_est);
          for(i_int in 1:length(interval_est) ){
            record_interval[,i_int]=predictobj@mean+qnorm_all[i_int]*sqrt(var_gasp_f)
            
          }
          
        }else if(object@discrepancy_type=="S-GaSP"){
          R=separable_kernel(object@R0, beta_delta, object@kernel_type,object@alpha)
          R_tilde=R+object@num_obs/object@lambda_z*diag(object@num_obs);
          
          
          #system.time(Chol_Eigen(R_tilde))
          
          L_R_tilde=Chol_Eigen(R_tilde)
          
          I_minus_R_R_tilde=diag(object@num_obs)-t(backsolve(t(L_R_tilde),forwardsolve(L_R_tilde,R )))
          
          
          R_tilde_inv_r=(backsolve(t(L_R_tilde),forwardsolve(L_R_tilde, r )))
          
          r_z=I_minus_R_R_tilde%*%r
          
          R_z_tilde_inv_r_z=(backsolve(t(L),forwardsolve(L, r_z )))
          
          for(i_testing in 1:N_testing){
            c_star_record[i_testing]=1-r[,i_testing]%*%R_tilde_inv_r[,i_testing]-r_z[,i_testing]%*%R_z_tilde_inv_r_z[,i_testing]
          }
          var_sgasp_f=sigma_2_delta*c_star_record
          
          if(interval_data==T){
            var_sgasp_f=var_sgasp_f+sigma_2_delta*eta_delta/testing_output_weights
          }
          
          qnorm_all=qnorm(interval_est);
          for(i_int in 1:length(interval_est) ){
            record_interval[,i_int]=predictobj@mean+qnorm_all[i_int]*sqrt(var_sgasp_f)
            
          }
          
        }
        predictobj@interval=record_interval
        
      }
      
    }
    
  }
  
  
  return(predictobj)
  
}



##Need to revise later to make sure this is correct, Dec 2021
predict_separable_2dim<-function(object, testing_input_separable,
                                 X_testing=matrix(0,length(testing_input_separable[[1]])*length(testing_input_separable[[2]]),1 ),
                                 n_thinning=10,interval_est=NULL,math_model=NULL,test_loc_index_emulator=NULL,...){

  predictobj<- new("predictobj.rcalibration")

  r0_separable=as.list(1:object@p_x)

  for(i_x in 1:object@p_x){
    r0_separable[[i_x]]=abs(outer(object@input[,i_x],testing_input_separable[[i_x]],'-'))
  }

  testing_input=expand.grid(testing_input_separable[[1]],testing_input_separable[[2]])
  testing_input=as.matrix(testing_input)

  
  
  if(is.null(testing_output_weights)){
    testing_output_weights=rep(1,dim(testing_input)[1])
  }
  
  
  if(object@simul_type==0){
    if(is.null(test_loc_index_emulator)){
      if(object@emulator_type=='ppgasp'){
        test_loc_index_emulator=1:dim(object@emulator_ppgasp@output)[2]
      }else{
        test_loc_index_emulator=c(1)
      }
    }
    if(length(object@emulator_ppgasp@p>0)){ ##ppgasp
      emulator=object@emulator_ppgasp
    }else{
      emulator=object@emulator_rgasp
      
    }
    
  }
  
  
  ##need to implement MLE
  if(object@method=="post_sample"){
    
    record_cm_pred=0
    record_cm_pred_no_trend=0
    record_pred_mean=0
  
    SS=floor(dim(object@post_sample)[1]/n_thinning)
  
    r_separable=as.list(1:object@p_x)
  
  
    c_prop=1/4
  
    for(i_S in (1: SS)*n_thinning ){
  
      if(i_S==floor(SS*n_thinning*c_prop)){
        cat(c_prop*100, 'percent is completed \n')
        c_prop=1/4
  
      }
  
  
      #print(i_S)
  
      theta=object@post_sample[i_S,1:object@p_theta]
      beta_delta=exp(object@post_sample[i_S,(object@p_theta+1):(object@p_theta+object@p_x)])
      eta_delta=exp(object@post_sample[i_S,object@p_theta+object@p_x+1])
      sigma_2_0=object@post_sample[i_S,object@p_theta+object@p_x+2]
  
      mean_cm=mathematical_model_eval(object@input,theta,object@simul_type,emulator,object@emulator_type,
                                      loc_index_emulator=object@loc_index_emulator,math_model);
  
      # if(object@simul_type==2 | object@simul_type==3){
      #    mean_cm=Mogihammer(theta,input,object@simul_type)
      # }
  
      if(object@emulator_type=='ppgasp'){
  
      #if(dim(mean_cm)[1]==1){
        mean_cm=as.vector(mean_cm)
      #}
      }
      
      
      
  
      
      
      output_minus_cm=object@output- mean_cm
      if(object@have_trend){
        theta_m=object@post_sample[i_S,(object@p_theta+object@p_x+3):(object@p_theta+object@p_x+2+object@q)]
        output_minus_cm=output_minus_cm-object@X%*%theta_m
      }
      ##remember
      if(object@discrepancy_type=="GaSP"){
        L=Get_R_new( (beta_delta),   (eta_delta),
                     object@R0, object@kernel_type,object@alpha,1/object@output_weights)
      }else if(object@discrepancy_type=="S-GaSP"){
        L=Get_R_z_new( (beta_delta),   (eta_delta),  object@lambda_z[i_S],
                       object@R0, object@kernel_type,object@alpha,1/object@output_weights)
      }
  
  
  
  
      R_inv_y=backsolve(t(L),forwardsolve(L,output_minus_cm ))
  
  
  
      if(object@discrepancy_type=="S-GaSP"){
        R_inv_y=Update_R_inv_y(R_inv_y,object@R0,beta_delta,object@kernel_type,object@alpha,object@lambda_z[i_S],object@num_obs)
      }
  
  
      if(object@kernel_type=="matern_5_2"){
        for(i_x in 1:object@p_x){
          r_separable[[i_x]]=matern_5_2_funct(r0_separable[[i_x]],beta_delta[i_x])
        }
      }else if(object@kernel_type=="matern_3_2"){
        for(i_x in 1:object@p_x){
          r_separable[[i_x]]=matern_3_2_funct(r0_separable[[i_x]],beta_delta[i_x])
        }
      }else if(object@kernel_type=="pow_exp"){
        for(i_x in 1:object@p_x){
          r_separable[[i_x]]=pow_exp_funct(r0_separable[[i_x]],beta_delta[i_x],object@alpha[i_x])
        }
      }
  
      ###so this is only for for image or p_x=2
  
      r2_dot_R_inv_y=r_separable[[2]]*as.vector(R_inv_y)
  
      rt_R_inv_y=as.vector(t(r_separable[[1]])%*%r2_dot_R_inv_y)
  
      mean_cm_test=mathematical_model_eval(testing_input,theta,object@simul_type,emulator,object@emulator_type,
                                           loc_index_emulator=test_loc_index_emulator,math_model);
  
      if(object@emulator_type=='ppgasp'){
  
      #if(dim(mean_cm_test)[1]==1){
        mean_cm_test=as.vector(mean_cm_test)
      #}
      }
  
      mean_cm_test_no_trend=mean_cm_test
  
      #if(object@simul_type==2 | object@simul_type==3){
      #   mean_cm_test=Mogihammer(theta,testing_input,object@simul_type)
      #}
      #f_M_testing=mean_cm_test
  
      if(object@have_trend){
        mean_cm_test=mean_cm_test+X_testing%*%theta_m
      }
  
  
      pred_mean=mean_cm_test+rt_R_inv_y
  
      record_cm_pred=record_cm_pred+mean_cm_test
      record_cm_pred_no_trend=record_cm_pred_no_trend+mean_cm_test_no_trend
  
      record_pred_mean=record_pred_mean+pred_mean
  
  
  
    }
  
  
  
  
    record_cm_pred=record_cm_pred/floor(SS)
    record_cm_pred_no_trend=record_cm_pred_no_trend/floor(SS)
    record_pred_mean=record_pred_mean/floor(SS)
  
    predictobj@math_model_mean=record_cm_pred
    predictobj@math_model_mean_no_trend=record_cm_pred_no_trend
    predictobj@mean=record_pred_mean
    #ans.list=as.list(1:3)
  
    #ans.list[[1]]=record_cm_pred
    #ans.list[[2]]=record_pred_mean
    #ans.list[[3]]=NULL
    if(!is.null(interval_est)){
      record_interval=record_interval/floor(SS)
      predictobj@interval=record_interval
    }
  }else{
    
    stop("MLE has no built for this scenario. Please select methd='post_sample' \n")
    
  }
  
  return(predictobj)
}


