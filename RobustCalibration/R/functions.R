

mathematical_model_eval<-function(input,theta,simul_type, emulator,emulator_type,loc_index_emulator,math_model){
  #VectorXd cm_obs_cur;
  p_theta=length(theta);
  p_x=dim(input)[2];
  n_testing=dim(input)[1];
  
  if(simul_type==0){ ###emulator

    #cm_obs_cur=Sample(emulator,testing_input);
    ##use the mean to approximate
    if(emulator_type=='rgasp'){
      testing_input=cbind(input, t(matrix(theta,p_theta,n_testing)));
      cm_obs_cur=predict.rgasp(emulator,testing_input)$mean;
    }else if(emulator_type=='ppgasp'){ ##then ppgasp
      if(is.vector(theta)){
        testing_input=matrix(theta,1,length(theta));
      }else{##matrix
        testing_input=(as.matrix(theta));
        
      }
      #t(matrix(theta,p_theta,n_testing));
      
      cm_obs_cur=predict.ppgasp(emulator,testing_input,loc_index=loc_index_emulator)$mean;
      
    }
    
  }else if(simul_type==1){
    #testing_input=cbind(input, t(matrix(theta,p_theta,n_testing)));
    cm_obs_cur=math_model(input, theta);
  }
  #else if( (simul_type==2) | (simul_type==3) ){
  #  cm_obs_cur= Mogihammer(input,theta,simul_type);
  #}
  return(cm_obs_cur);
}


##tell whether it has discrepancy function or no
post_sample <- function(input, output, R0_list, kernel_type, p_theta, output_weights,
                        par_cur, lambda_z,prior_par,theta_range,S,thinning, X, have_trend, alpha,sd_proposal,
                        discrepancy_type, simul_type,emulator,
                        emulator_type,loc_index_emulator,
                        math_model,S_2_f,num_obs_all){ 
  #method_type=Input_list[[17]];
  if(discrepancy_type=='no-discrepancy'){
    ans=post_sample_no_discrepancy(input, output, R0_list,  p_theta, output_weights,
                                   par_cur, theta_range,S,thinning, X, have_trend, alpha,sd_proposal,
                                   discrepancy_type, simul_type,emulator,emulator_type,loc_index_emulator,math_model,S_2_f,num_obs_all);
  }else{
    ans=post_sample_with_discrepancy(input, output, R0_list, kernel_type, p_theta, output_weights,
                                     par_cur, lambda_z,prior_par,theta_range,S,thinning, X, have_trend, alpha,sd_proposal,
                                     discrepancy_type, simul_type,emulator,emulator_type,loc_index_emulator,math_model,S_2_f,num_obs_all);
  }
  return(ans);
  
}

##sample the posterior with discrepancy
post_sample_with_discrepancy<-function(input, output, R0_list, kernel_type, p_theta, output_weights,
                                       par_cur, lambda_z,prior_par,theta_range,S,thinning, X, have_trend, alpha,sd_proposal,
                                       discrepancy_type,simul_type,emulator,emulator_type,loc_index_emulator,math_model,S_2_f,num_obs_all){
  
  
  mylist=as.list(1:4);
  
  #Initialization
  
  
  num_obs=dim(input)[1];
  p_x=dim(input)[2];
  
  length_input=rep(0,p_x)
  
  for(i_cl in 1:p_x){
    length_input[i_cl] = (max(input[,i_cl])-min(input[,i_cl]))
  }
  
  
  
  CL_a_b=prior_par;
  
  CL=CL_a_b[1:p_x];
  a=CL_a_b[p_x+1];
  b=CL_a_b[p_x+2];
  
  p_theta_m=0;
  
  if(have_trend){
    p_theta_m=dim(X)[2];
    #.cols();
  }
  
  sd_theta=sd_proposal[1:p_theta];
  sd_log_beta=sd_proposal[(p_theta+1):(p_theta+p_x)];
  sd_log_eta=sd_proposal[p_theta+p_x+1];
  
  
  inv_output_weights=1/output_weights;
  
  record_S=length(1:(S/thinning))
  
  record_par=matrix(0,record_S,p_theta+p_x+2+p_theta_m);
  record_post=rep(0,record_S)
  lambda_z_record=rep(NA,record_S)
  
  param=par_cur;
  
  
  #MatrixXd L;
  if(discrepancy_type=='GaSP'){
    L=Get_R_new(exp(param[(p_theta+1):(p_theta+p_x)]),exp(param[p_theta+p_x+1]),R0_list,kernel_type,alpha, inv_output_weights);
  }else{
    xi_initial=param[(p_theta+1):(p_theta+p_x)]
    log_eta_intial=param[p_theta+p_x+1]
    
    if(is.na(lambda_z[1])){
      #lambda_z_cur=sqrt( sqrt(sum((exp(xi_initial)*length_input)^2)) /(exp(log_eta_intial)/num_obs_all))
      
      #lambda_z_cur=sqrt(10*num_obs_all*sqrt(sum(1/(length_input*exp(xi_initial))^2)) /(exp(log_eta_intial)))
      lambda_z_cur=sqrt(num_obs_all*sqrt(sum((length_input*exp(xi_initial))^2)) /(exp(log_eta_intial))) ##inverse proportional to gamma
      

    }else{
      lambda_z_cur=lambda_z[1]
    }
    L=Get_R_z_new(exp(xi_initial),exp(log_eta_intial),lambda_z_cur,R0_list,kernel_type,alpha, inv_output_weights );
  }
  
  #MatrixXd L_propose;
  
  accept_S_theta=0;
  accept_S_beta=0;
  accept_S_dec=0; # this is to count how many sample points are outside the boundary and get rejected
  
  post_cur=0;
  
  theta_cur=rep(0,p_theta);
  theta_sample=rep(0,p_theta);
  
  
  #bool decision_0;
  #bool decision;
  param_propose=par_cur;
  xi_sample=rep(0,p_x);
  
  log_eta_sample=0;
  #post_propose;
  r_ratio=0;
  
  
  cm_obs_cur=mathematical_model_eval(input,param[1:p_theta],simul_type,emulator,emulator_type,loc_index_emulator,math_model);
  
  
  c_prop=1/4
  
  #start of the sampling
  
  for (i_S in 1:S){
    if(i_S==floor(S*c_prop)){
      #cat(post_cur,'\n')
      cat(c_prop*S, ' of ', S, ' posterior samples are drawn \n')
      c_prop=c_prop+1/4
    }
    #cat(post_cur,'\n')
    #cat(par_cur,'\n')
    #cat(accept_S_theta,'\n')
    
    par_cur[(p_theta+p_x+2):(p_theta+p_x+2+p_theta_m)]=Sample_sigma_2_theta_m(par_cur,L,output,p_theta,p_x,X,have_trend,cm_obs_cur,S_2_f,num_obs_all);
    
    # par_cur.segment(p_theta+p_x+1,1+p_theta_m)=Sample_sigma_2_theta_m(par_cur,L,output,p_theta,p_x,X,have_trend,cm_obs_cur);
    
    post_cur=Log_marginal_post(par_cur,L,output,p_theta,p_x,X,have_trend,CL,a,b,cm_obs_cur,S_2_f,num_obs_all);
    
    #//sample theta
    theta_cur=par_cur[1:p_theta];
    decision_0=F;
    
    
    # for(int i_theta=0; i_theta<p_theta; ++i_theta){
    #   theta_sample(i_theta)=theta_cur(i_theta)+sd_theta(i_theta)*(theta_range(i_theta,1)-theta_range(i_theta,0))*distribution_stan_norm(generator);
    #   if((theta_sample(i_theta)>theta_range(i_theta,1))|| (theta_sample(i_theta)<theta_range(i_theta,0)) ){
    #     decision_0=true;
    #     count_dec_record(i_S)=1;
    #     break;
    #   }
    #   
    # }
    
    
    for(i_theta in 1:p_theta){
      theta_sample[i_theta]=rnorm(1,mean=theta_cur[i_theta],sd= (sd_theta[i_theta]*(theta_range[i_theta,2]- theta_range[i_theta,1]) ) )  ##here maybe truncated
      #cat(theta_sample[i_theta],'\n')
      #cat(theta_range[i_theta,2],'\n')
      #cat(decision_0,'\n')
      if((theta_sample[i_theta]>theta_range[i_theta,2])| (theta_sample[i_theta]<theta_range[i_theta,1]) ){
        decision_0=T  ##reject directly
        accept_S_dec=accept_S_dec+1
        break;
      }
    }
    
    
    #//if decision_0==true, then stay at original place
    #//ow see whether we accept the sample
    if(decision_0==F){
      param_propose=par_cur;
      param_propose[1:p_theta]=theta_sample;
      
      
      cm_obs_propose=mathematical_model_eval(input,theta_sample,simul_type,emulator,emulator_type,loc_index_emulator,math_model);
      
      post_propose=Log_marginal_post(param_propose,L,output,p_theta,p_x,X,have_trend,CL,a,b, cm_obs_propose,S_2_f,num_obs_all);
      r_ratio=exp(post_propose-post_cur);
      decision=Accept_proposal(r_ratio);
      if(decision){
        
        par_cur=param_propose;
        post_cur=post_propose;
        cm_obs_cur=cm_obs_propose;
        accept_S_theta=accept_S_theta+1;
        
        #cat(par_cur,'\n')
        
      }
      
    }
    
    #//sample xi and log_eta 
    for(i_x in 1: p_x){
      xi_sample[i_x]=par_cur[p_theta+i_x]+sd_log_beta[i_x]*rnorm(1);
      #distribution_stan_norm(generator);
    }
    log_eta_sample=par_cur[p_theta+p_x+1]+sd_log_eta*rnorm(1); 
    #distribution_stan_norm(generator);
    
    param_propose=par_cur;
    
    param_propose[(p_theta+1):(p_theta+p_x)]=xi_sample;
    param_propose[p_theta+p_x+1]=log_eta_sample;
    
    param=param_propose;
    
    
    
    if(discrepancy_type=='GaSP'){
      L_propose=Get_R_new(exp(param[(p_theta+1):(p_theta+p_x)]),exp(param[p_theta+p_x+1]),R0_list,kernel_type,alpha, inv_output_weights);
    }else{
      if(is.na(lambda_z[i_S])){
        #lambda_z_propose=sqrt(sqrt(sum((exp(xi_sample)*length_input)^2)) /(exp(log_eta_sample)/num_obs_all))
        #lambda_z_propose=sqrt(10*num_obs_all*sqrt(sum(1/(length_input*exp(xi_sample))^2)) /(exp(log_eta_sample)))
        lambda_z_propose=sqrt(num_obs_all*sqrt(sum((length_input*exp(xi_sample))^2)) /(exp(log_eta_sample)))  ##inverse proportional to gamma
        
      }else{
        lambda_z_propose=lambda_z[i_S]
      }
      L_propose=Get_R_z_new(exp(param[(p_theta+1):(p_theta+p_x)]),exp(param[p_theta+p_x+1]),lambda_z_propose,R0_list,kernel_type,alpha, inv_output_weights );
    }
    
    post_propose=Log_marginal_post(param,L_propose,output,p_theta,p_x,X,have_trend,CL,a,b,cm_obs_cur,S_2_f,num_obs_all);
    
    r_ratio=exp(post_propose-post_cur);
    
    decision=Accept_proposal(r_ratio);
    
    
    
    if(decision){
      if(discrepancy_type=='S-GaSP'){
        if(is.na(lambda_z[i_S])){
          lambda_z_cur=lambda_z_propose
        }else{
          lambda_z_cur=lambda_z[i_S]
        }
      }
      
      par_cur=param_propose;
      post_cur=post_propose;
      L=L_propose;
      accept_S_beta=accept_S_beta+1;
    }
    
    if(i_S%%thinning==0){
      
      record_par[floor(i_S/thinning),]=par_cur;
      if(discrepancy_type=='S-GaSP'){
        lambda_z_record[floor(i_S/thinning)]=lambda_z_cur;
      }
      
      record_post[floor(i_S/thinning)]=post_cur;
    }
    
    
    #record_par.block(i_S,0,1,p_theta+p_x+2+p_theta_m)=par_cur.transpose();
    #record_post(i_S)=post_cur;
  }
  
  
  mylist[[1]]=record_par;
  mylist[[2]]=record_post;
  
  mylist[[3]]=c(accept_S_theta,accept_S_beta,accept_S_dec);
  mylist[[4]]=lambda_z_record;
  
  ##mylist[[4]]=accept_S_dec;
  
  cat('Done with posterior sampling \n')
  
  cat(accept_S_theta, ' of ', S, ' proposed posterior samples of calibration parameters are accepted \n')
  cat(accept_S_beta, ' of ', S, ' proposed posterior samples of range and nugget parameters are accepted \n')
  
  return(mylist);
  
  
}






post_sample_no_discrepancy<-function(input, output, R0_list,  p_theta, output_weights,
                                     par_cur, theta_range,S,thinning, X, have_trend, alpha,sd_proposal,
                                     discrepancy_type, simul_type,emulator,emulator_type,loc_index_emulator,math_model,S_2_f,num_obs_all){
  
  
  mylist=list();
  
  #Initialization
  num_obs=dim(input)[1];
  p_x=dim(input)[2];
  
  p_theta_m=0;
  
  if(have_trend){
    p_theta_m=dim(X)[2];
  }
  
  #List mylist;
  sd_theta=sd_proposal[1:p_theta];
  
  # simul_type=  Input_list[17];
  
  
  
  inv_output_weights=1/output_weights;
  
  record_S=length(1:(S/thinning))
  
  record_par=matrix(0,S,p_theta+1+p_theta_m);
  record_post=rep(0,S)
  
  accept_S_theta=0;
  accept_S_dec=0; # this is to count how many sample points are outside the boundary and get rejected
  
  
  #MatrixXd record_par=MatrixXd::Zero(S,p_theta+1+p_theta_m);
  #VectorXd record_post=VectorXd::Zero(S);
  
  param=par_cur;
  
  
  post_cur=0;
  
  
  theta_cur=rep(0,p_theta);
  theta_sample=rep(0,p_theta);
  
  #bool decision_0;
  #bool decision;
  param_propose=par_cur;
  r_ratio=0;
  
  
  cm_obs_cur=mathematical_model_eval(input,param[1:p_theta],simul_type,emulator,emulator_type,loc_index_emulator,math_model);
  
  
  #cm_obs_cur=mathematical_model_eval(param.head(p_theta),input,simul_type,emulator_par,simul_input,simul_output);
  
  c_prop=1/4
  for (i_S in 1:S){
    if(i_S==floor(S*c_prop)){
      #cat(post_cur,'\n')
      cat(c_prop*S, ' of ', S, ' posterior samples are drawn \n')
      c_prop=c_prop+1/4
    }
    #cat(post_cur,'\n')
    
    #cat(par_cur,'\n')
    par_cur[(p_theta+1):(p_theta+1+p_theta_m)]=Sample_sigma_2_theta_m_no_discrepancy(par_cur,output,p_theta,X,have_trend, inv_output_weights,cm_obs_cur,S_2_f,num_obs_all);
    
    # par_cur.segment(p_theta+p_x+1,1+p_theta_m)=Sample_sigma_2_theta_m(par_cur,L,output,p_theta,p_x,X,have_trend,cm_obs_cur);
    
    post_cur=Log_marginal_post_no_discrepancy(par_cur,output,p_theta,X,have_trend, inv_output_weights,cm_obs_cur,S_2_f,num_obs_all);
    
    
    #par_cur.segment(p_theta,1+p_theta_m)=Sample_sigma_2_theta_m_no_discrepancy(par_cur,output,p_theta,X,have_mean, inv_output_weights,cm_obs_cur);
    
    #post_cur=Log_marginal_post_no_discrepancy(par_cur,output,p_theta,X,have_mean, inv_output_weights,cm_obs_cur);
    
    #sample theta
    theta_cur=par_cur[1:p_theta];
    decision_0=F;
    
    for(i_theta in 1:p_theta){
      theta_sample[i_theta]=rnorm(1,mean=theta_cur[i_theta],sd=sd_theta[i_theta]*(theta_range[i_theta,2]- theta_range[i_theta,1]) )  ##here maybe truncated
      if((theta_sample[i_theta]>theta_range[i_theta,2])| (theta_sample[i_theta]<theta_range[i_theta,1]) ){
        decision_0=T  ##reject directly
        accept_S_dec=accept_S_dec+1
        break;
      }
    }
    
    #//if decision_0==true, then stay at original place
    #//ow see whether we accept the sample
    
    if(decision_0==F){
      param_propose=par_cur;
      param_propose[1:p_theta]=theta_sample;
      cm_obs_propose=mathematical_model_eval(input,theta_sample,simul_type,emulator,emulator_type,loc_index_emulator,math_model);
      
      post_propose=Log_marginal_post_no_discrepancy(param_propose,output,p_theta,X,have_trend, inv_output_weights,cm_obs_propose,S_2_f,num_obs_all);
      
      #  Log_marginal_post(param_propose,L,output,p_theta,p_x,X,have_trend,CL,a,b, cm_obs_propose);
      r_ratio=exp(post_propose-post_cur);
      decision=Accept_proposal(r_ratio);
      if(decision){
        par_cur=param_propose;
        post_cur=post_propose;
        cm_obs_cur=cm_obs_propose;
        accept_S_theta=accept_S_theta+1;
      }
      
    }
    
    
    if(i_S%%thinning==0){
      
      record_par[floor(i_S/thinning),]=par_cur;
      record_post[floor(i_S/thinning)]=post_cur;
    }
    
    
    #record_par.block(i_S,0,1,p_theta+p_x+2+p_theta_m)=par_cur.transpose();
    #record_post(i_S)=post_cur;
  }
  
  cat('Done with posterior sampling \n')
  mylist[[1]]=record_par;
  mylist[[2]]=record_post;
  
  mylist[[3]]=c(accept_S_theta,accept_S_dec);
  
  #mylist[[4]]=count_dec_record;
  
  cat(accept_S_theta, ' of ', S, ' proposed posterior samples samples of calibration parameters are accepted \n')
  
  
  return(mylist);
  
  
}



# multisources, comment out for now 
post_sample_MS <- function(model,par_cur_theta, par_cur_individual, emulator,math_model_MS){

  mylist=list();


  record_S=length(1:(model@S/model@thinning))


  record_par_individual=as.list(rep(0,model@num_sources))
  for(i in 1: model@num_sources){
    if(model@discrepancy_type[i]=='GaSP' | model@discrepancy_type[i]=='S-GaSP'){
      record_par_individual[[i]]=  matrix(0,record_S,model@p_x[i]+2+model@q[i]);
    }else{
      record_par_individual[[i]]=  matrix(0,record_S,1+model@q[i]);
    }
  }

  record_post=matrix(0,record_S,model@num_sources)
  record_theta=matrix(0,record_S,model@p_theta)


  length_input=as.list(1:model@num_sources)
    for(i in 1:model@num_sources){
      length_input[[i]]=rep(0,model@p_x[i])
      for(i_cl in 1:model@p_x[i]){
        length_input[[i]][i_cl] = (max(model@input[[i]][,i_cl])-min(model@input[[i]][,i_cl]))
      }
    }


  lambda_z_record=as.list(1:model@num_sources)
  for(i in 1:model@num_sources){
    lambda_z_record[[i]]=rep(NA,record_S)

  }


  #MatrixXd L;
  L=as.list(rep(0,model@num_sources))
  L_propose=L

  ###well, then I need to update this
  ##I will add R here, only support a GP here

  ####I guess it is better to take the variance into account
  lambda_z_cur=rep(NA,model@num_sources)
  lambda_z_propose=rep(NA,model@num_sources)
  for(i in 1: model@num_sources){
    if(model@discrepancy_type[i]=='GaSP'){
      L[[i]]=Get_R_new(exp(par_cur_individual[[i]][1:model@p_x[i]]),exp(par_cur_individual[[i]][model@p_x[i]+1]),
                       model@R0[[i]],model@kernel_type[i],model@alpha[[i]], 1/model@output_weights[[i]]);
    }else if(model@discrepancy_type[i]=='S-GaSP'){
      ##define param
      xi_initial=par_cur_individual[[i]][(model@p_theta+1):(model@p_theta+model@p_x[i])]
      log_eta_intial=par_cur_individual[[i]][model@p_theta+model@p_x[i]+1]

      if(is.na(model@lambda_z[[i]][1])){

        #lambda_z_cur[i]=sqrt(sqrt(sum((exp(xi_initial)*length_input[[i]])^2)) /(exp(log_eta_intial)/model@num_obs[i]))
        
        #lambda_z_cur[i]=sqrt(10*model@num_obs[i]*sqrt(sum(1/(length_input[[i]]*exp(xi_initial))^2)) /(exp(log_eta_intial)))
        lambda_z_cur[i]=sqrt(model@num_obs[i]*sqrt(sum((length_input[[i]]*exp(xi_initial))^2)) /(exp(log_eta_intial))) ##inverse proportional to gamma
        
      }else{
        lambda_z_cur[i]=model@lambda_z[[i]][1]
      }


      L[[i]]=Get_R_z_new(exp(par_cur_individual[[i]][1:model@p_x[i]]),exp(par_cur_individual[[i]][model@p_x[i]+1]),
                         lambda_z_cur[i],model@R0[[i]],model@kernel_type[i],
                         model@alpha[[i]], 1/model@output_weights[[i]] );
    }
  }


  #L_propose=L
  #MatrixXd L_propose;

  accept_S_theta=0;
  accept_N_cov_par=rep(0,model@num_sources);
  accept_S_beta=rep(0,model@num_sources);

  accept_S_dec=0; # this is to count how many sample points are outside the boundary and get rejected for calibration

  #post_cur=0;

  theta_cur=par_cur_theta;
  theta_sample=rep(0,model@p_theta);

  individual_par_cur=par_cur_individual

  ###########################
  #param_propose=par_cur;
  #xi_sample=rep(0,p_x);
  #log_eta_sample=0;
  #post_propose;
  r_ratio=0;

  cm_obs_cur=as.list(rep(0,model@num_sources))
  cm_obs_propose=cm_obs_cur

  for(i in 1:model@num_sources){
    cm_obs_cur[[i]]=mathematical_model_eval(model@input[[i]],theta_cur[model@index_theta[[i]]],model@simul_type[i],emulator[[i]],
                                            model@emulator_type[i],model@loc_index_emulator[[i]], math_model_MS[[i]]);
  }

  c_prop=1/4


  post_cur=rep(0,model@num_sources)
  post_propose=rep(0,model@num_sources)

  for (i_S in 1:model@S){
    if(i_S==floor(model@S*c_prop)){
      #cat(post_cur,'\n')
      cat(c_prop*model@S, ' of ', model@S, ' posterior samples are drawn \n')
      c_prop=c_prop+1/4
    }

    #print(i_S)
    #print(accept_S_theta)
    #print(individual_par_cur[[1]])
    #print(theta_cur)
    #print(post_cur)
    #print(individual_par_cur[[2]])

    #print(post_cur)


    for(i in 1:model@num_sources){
      if(model@discrepancy_type[i]=='no-discrepancy'){
        sigma_2_theta_m_sample=Sample_sigma_2_theta_m_no_discrepancy(c(theta_cur[model@index_theta[[i]]],
                                                                       individual_par_cur[[i]]),
                                                                     model@output[[i]],length(model@index_theta[[i]]),
                                                                     model@X[[i]],
                                                                     model@have_trend[i],1/model@output_weights[[i]],cm_obs_cur[[i]],0,model@num_obs[i] );
        #Sample_sigma_2_theta_m_no_discrepancy(par_cur,output,p_theta,X,have_trend, inv_output_weights,cm_obs_cur,S_2_f,num_obs_all);
        
        individual_par_cur[[i]]=sigma_2_theta_m_sample

        post_cur[i]=Log_marginal_post_no_discrepancy(c(theta_cur[model@index_theta[[i]]],
                                                       individual_par_cur[[i]]),model@output[[i]],length(model@index_theta[[i]]),
                                                     model@X[[i]],
                                                     model@have_trend[i],1/model@output_weights[[i]],cm_obs_cur[[i]],0,model@num_obs[i] );


        if(i_S%%model@thinning==0){
          record_par_individual[[i]][floor(i_S/model@thinning),]=as.matrix(sigma_2_theta_m_sample)  ##first par is the sigma_2
        }

      }else{

        sigma_2_theta_m_sample=Sample_sigma_2_theta_m(c(theta_cur[model@index_theta[[i]]],
                                                        individual_par_cur[[i]]),L[[i]],
                                                      model@output[[i]],
                                                      length(model@index_theta[[i]]),
                                                      model@p_x[i],model@X[[i]],
                                                      model@have_trend[i],cm_obs_cur[[i]],0,model@num_obs[i]);


        individual_par_cur[[i]][ (model@p_x[i]+2):(model@p_x[i]+2+model@q[i]) ]=sigma_2_theta_m_sample

        post_cur[i]=Log_marginal_post(c(theta_cur[model@index_theta[[i]]],
                                        individual_par_cur[[i]]),L[[i]],
                                      model@output[[i]],
                                      length(model@index_theta[[i]]),
                                      model@p_x[i],model@X[[i]],
                                      model@have_trend[i],
                                      model@prior_par[[i]][1:model@p_x[i]],
                                      model@prior_par[[i]][model@p_x[i]+1],
                                      model@prior_par[[i]][model@p_x[i]+2],
                                      cm_obs_cur[[i]],0,model@num_obs[i] );




        #//sample xi and log_eta
        xi_sample=rep(0,model@p_x[i])

        for(i_x in 1: model@p_x[i]){
          xi_sample[i_x]=individual_par_cur[[i]][i_x]+model@sd_proposal_cov_par[[i]][i_x]*rnorm(1);
          #distribution_stan_norm(generator);
        }
        log_eta_sample=individual_par_cur[[i]][model@p_x[i]+1]+
          model@sd_proposal_cov_par[[i]][model@p_x[i]+1]*rnorm(1);

        individual_par_propose=  individual_par_cur[[i]]
        individual_par_propose[1:model@p_x[i]]=xi_sample
        individual_par_propose[model@p_x[i]+1]=log_eta_sample



        if(model@discrepancy_type[i]=='GaSP'){
          L_propose[[i]]=Get_R_new(exp(individual_par_propose[1:model@p_x[i]]),
                                   exp(individual_par_propose[model@p_x[i]+1]),
                                   model@R0[[i]],model@kernel_type[i],model@alpha[[i]], 1/model@output_weights[[i]]);
        }else if(model@discrepancy_type[i]=='S-GaSP'){
          if(is.na(model@lambda_z[[i]][i_S])){
            #lambda_z_propose[i]=sqrt(sqrt(sum((exp(xi_sample)*length_input[[i]])^2)) /(exp(log_eta_sample)/model@num_obs[i]))
            
            #lambda_z_propose[i]=sqrt(10*model@num_obs[i]*sqrt(sum(1/(length_input[[i]]*exp(xi_sample))^2)) /(exp(log_eta_sample)))
            lambda_z_propose[i]=sqrt(model@num_obs[i]*sqrt(sum((length_input[[i]]*exp(xi_sample))^2)) /(exp(log_eta_sample))) ###inverse pro
            
          }else{
            lambda_z_propose[i]=model@lambda_z[[i]][i_S]
          }



          L_propose[[i]]=Get_R_z_new(exp(individual_par_propose[1:model@p_x[i]]),
                                     exp(individual_par_propose[model@p_x[i]+1]),
                                     lambda_z_propose[i],model@R0[[i]],model@kernel_type[i],
                                     model@alpha[[i]], 1/model@output_weights[[i]] );
        }


        post_propose[i]=Log_marginal_post(c(theta_cur[model@index_theta[[i]]],
                                            individual_par_propose),L_propose[[i]],
                                          model@output[[i]],
                                          length(model@index_theta[[i]]),
                                          model@p_x[i],model@X[[i]],
                                          model@have_trend[i],
                                          model@prior_par[[i]][1:model@p_x[i]],
                                          model@prior_par[[i]][model@p_x[i]+1],
                                          model@prior_par[[i]][model@p_x[i]+2],
                                          cm_obs_cur[[i]],0,model@num_obs[i] );


        r_ratio=exp( post_propose[i]-post_cur[i]);

        decision=Accept_proposal(r_ratio);

        if(decision){
          if(model@discrepancy_type[i]=='S-GaSP'){
            if(is.na(model@lambda_z[[i]][i_S])){
              lambda_z_cur[i]=lambda_z_propose[i]
            }else{
              lambda_z_cur[i]=model@lambda_z[[i]][i_S]
            }
          }


          individual_par_cur[[i]]=individual_par_propose;
          post_cur[i]=post_propose[i];
          L[[i]]=L_propose[[i]];
          accept_S_beta[i]=accept_S_beta[i]+1;
        }


        if(i_S%%model@thinning==0){
          record_par_individual[[i]][floor(i_S/model@thinning),]=individual_par_cur[[i]]

          if(model@discrepancy_type[i]=='S-GaSP'){
            lambda_z_record[[i]][floor(i_S/model@thinning)]=lambda_z_cur[i];
          }

        }
      }

    }

    ############################################################
    #//sample theta
    #theta_cur=par_cur[1:p_theta];
    decision_0=F;




    for(i_theta in 1:model@p_theta){
      theta_sample[i_theta]=rnorm(1,mean=theta_cur[i_theta],sd= (model@sd_proposal_theta[i_theta]*(model@theta_range[i_theta,2]- model@theta_range[i_theta,1]) ) )  ##here maybe truncated
      if((theta_sample[i_theta]>model@theta_range[i_theta,2])| (theta_sample[i_theta]<model@theta_range[i_theta,1]) ){
        decision_0=T  ##reject directly
        accept_S_dec=accept_S_dec+1
        break;
      }
    }


    #//if decision_0==true, then stay at original place
    #//ow see whether we accept the sample
    if(decision_0==F){
      #theta_propose=theta_sample;

      for(i in 1:model@num_sources){
        cm_obs_propose[[i]]=mathematical_model_eval(model@input[[i]],theta_sample[model@index_theta[[i]]],model@simul_type[i],emulator[[i]],
                                                    model@emulator_type[i],model@loc_index_emulator[[i]],math_model_MS[[i]]);

        if(model@discrepancy_type[i]=='no-discrepancy'){
          post_propose[i]=Log_marginal_post_no_discrepancy(c(theta_sample[model@index_theta[[i]]],
                                                             individual_par_cur[[i]]),model@output[[i]],length(model@index_theta[[i]]),
                                                           model@X[[i]],
                                                           model@have_trend[i],1/model@output_weights[[i]],cm_obs_propose[[i]],0,model@num_obs[i] );
        }else{
          post_propose[i]=Log_marginal_post(c(theta_sample[model@index_theta[[i]]],
                                              individual_par_cur[[i]]),L[[i]],
                                            model@output[[i]],
                                            length(model@index_theta[[i]]),
                                            model@p_x[i],model@X[[i]],
                                            model@have_trend[i],
                                            model@prior_par[[i]][1:model@p_x[i]],
                                            model@prior_par[[i]][model@p_x[i]+1],
                                            model@prior_par[[i]][model@p_x[i]+2],
                                            cm_obs_propose[[i]],0,model@num_obs[i] );

        }


      }


      r_ratio=exp(sum(post_propose)-sum(post_cur));
      decision=Accept_proposal(r_ratio);
      if(decision){

        theta_cur=theta_sample;
        post_cur=post_propose;
        cm_obs_cur=cm_obs_propose;
        accept_S_theta=accept_S_theta+1;

        #cat(par_cur,'\n')

      }

    }


    #if(i_S%%model@thinning==0){
    #  record_par_individual[[i]][floor(i_S/model@thinning),]=individual_par_cur[[i]]
    #}

    if(i_S%%model@thinning==0){
      record_post[floor(i_S/model@thinning),]=post_cur
      record_theta[floor(i_S/model@thinning),]=theta_cur
    }



  }

  mylist$record_post=record_post

  mylist$record_theta=record_theta



  #mylist$individual_par=record_par_individual
  ####I need to have a matrix
  mylist$individual_par=record_par_individual

  mylist$accept_S_theta=accept_S_theta
  mylist$accept_S_beta=accept_S_beta
  mylist$accept_S_dec=accept_S_dec

  mylist$lambda_z=lambda_z_record;


  cat('Done with posterior sampling \n')
  cat(accept_S_theta, ' of ', model@S, ' proposed posterior samples of calibration parameters are accepted \n')

  for(i in 1:model@num_sources){
    if(model@discrepancy_type[i]!='no-discrepancy'){
      cat('For source', i,':', accept_S_beta[i], ' of ', model@S,
          ' proposed posterior samples of range and nugget parameters are accepted \n')
    }
  }
  return(mylist)

}

# comment out multi-sources for now
post_sample_MS_with_measurement_bias <- function(model,par_cur_theta, par_cur_individual, emulator,math_model_MS){

  mylist=list();

  ##the sample size should be the same here
  num_obs=model@num_obs[[model@num_sources+1]];

  record_S=length(1:(model@S/model@thinning))

  record_par_individual=as.list(rep(0,model@num_sources))

  is_SGaSP=rep(0, model@num_sources+1)

  for(i in 1: model@num_sources){
      record_par_individual[[i]]=  matrix(0,record_S,model@p_x[i]+2+model@q[i]);
      if(model@discrepancy_type[i]=='S-GaSP'){
        is_SGaSP[i]=as.integer(1)
      }
  }

  if(model@discrepancy_type[model@num_sources+1]=='S-GaSP'){
    is_SGaSP[model@num_sources+1]=as.integer(1)
  }

  ###
  length_input=as.list(1:(model@num_sources+1))
  for(i in 1:model@num_sources){
    length_input[[i]]=rep(0,model@p_x[i])
    for(i_cl in 1:model@p_x[i]){
      length_input[[i]][i_cl] = (max(model@input[[i]][,i_cl])-min(model@input[[i]][,i_cl]))
    }
  }
  ##measurement bias with additional one as discrepancy
  length_input[[model@num_sources+1]]=rep(0,model@p_x[model@num_sources+1])
  for(i_cl in 1:model@p_x[model@num_sources+1]){
    length_input[[model@num_sources+1]][i_cl] = (max(model@input[[model@num_sources+1]][,i_cl])-min(model@input[[model@num_sources+1]][,i_cl]))
  }

  ##
  lambda_z_record=as.list(1:(model@num_sources+1))
  for(i in 1:(model@num_sources+1)){
    lambda_z_record[[i]]=rep(NA,record_S)
  }
  ###


  if(model@have_measurement_bias_recorded==T){
     record_measurement_bias=as.list(1:model@num_sources)
     for(i in 1: model@num_sources){
       record_measurement_bias[[i]]=matrix(0,record_S,num_obs);
     }
  }

  ## range parameter and the variance parameter of the shared bias
  record_par_individual[[model@num_sources+1]]=matrix(0,record_S,model@p_x[model@num_sources+1]+1)

  record_post=matrix(0,record_S,model@num_sources)
  record_theta=matrix(0,record_S,model@p_theta)

  ##prepare the sample the delta and the cholesky decomposition
  ##Compute the cholesky
  num_sources_1=model@num_sources+1
  all_one_vec=rep(1,model@num_obs[num_sources_1])


  ##I still need the cholesky as I need to sample those

  #MatrixXd L;
  L=as.list(rep(0,model@num_sources))
  L_propose=L
  
  cm_obs_cur=as.list(rep(0,model@num_sources))
  tilde_output_cur=as.list(rep(0,model@num_sources))
  
  cm_obs_propose=cm_obs_cur
  
  
  for(i in 1:model@num_sources){
    cm_obs_cur[[i]]=mathematical_model_eval(model@input[[i]],par_cur_theta[model@index_theta[[i]]],model@simul_type[i],emulator[[i]], 
                                            model@emulator_type[i],model@loc_index_emulator[[i]],math_model_MS[[i]]);
    tilde_output_cur[[i]]=model@output[[i]]- cm_obs_cur[[i]]  ##here assume zero mean parameter
  }
  
  ###well, then I need to update this
  ##I will add R here, only support a GP here

  ####I guess it is better to take the variance into account
  lambda_z_cur=rep(NA,model@num_sources)
  lambda_z_propose=rep(NA,model@num_sources)
  for(i in 1: model@num_sources){
    if(model@discrepancy_type[i]=='GaSP'){
      L[[i]]=Get_R_new(exp(par_cur_individual[[i]][1:model@p_x[i]]),exp(par_cur_individual[[i]][model@p_x[i]+1]),
                       model@R0[[i]],model@kernel_type[i],model@alpha[[i]], 1/model@output_weights[[i]]);
    }else if(model@discrepancy_type[i]=='S-GaSP'){
      ##define param
      xi_initial=par_cur_individual[[i]][(model@p_theta+1):(model@p_theta+model@p_x[i])]
      log_eta_intial=par_cur_individual[[i]][model@p_theta+model@p_x[i]+1]

      if(is.na(model@lambda_z[[i]][1])){
        #lambda_z_cur=sqrt( min(exp(xi_initial)*length_input)/(exp(log_eta_intial)/num_obs_all))

        #lambda_z_cur[i]=sqrt(10*sqrt(sum(1/(length_input[[i]]*exp(xi_initial))^2)) /(exp(log_eta_intial)/model@num_obs[i]))
        lambda_z_cur[i]=sqrt(sqrt(sum((length_input[[i]]*exp(xi_initial))^2)) /(exp(log_eta_intial)/model@num_obs[i]))
        
      }else{
        lambda_z_cur[i]=model@lambda_z[[i]][1]
      }

      L[[i]]=Get_R_z_new(exp(par_cur_individual[[i]][1:model@p_x[i]]),exp(par_cur_individual[[i]][model@p_x[i]+1]),
                         lambda_z_cur[i],model@R0[[i]],model@kernel_type[i],
                         model@alpha[[i]], 1/model@output_weights[[i]] );
    }
  }

  delta_sample=0
  
  if(model@discrepancy_type[num_sources_1]!='no-discrepancy'){
    record_model_bias=matrix(0,record_S,num_obs);
    
    ##initial value of lambda_z_cur for discrepancy
    #lambda_z_cur[num_sources_1]=sqrt(10*sqrt(sum(1/(exp(xi_initial)*length_input[[num_sources_1]])^2)) /(exp(log_eta_intial)/model@num_obs[num_sources_1]))
    #lambda_z_cur[num_sources_1]=sqrt(sqrt(sum((exp(xi_initial)*length_input[[num_sources_1]])^2)) /(exp(log_eta_intial)/model@num_obs[num_sources_1]))
    lambda_z_cur[num_sources_1]=model@lambda_z[[num_sources_1]][1]  ##assumed it is given
  
    ##need to have two, one for GP, one for SGP
    cov_inv_all=Get_inv_all(par_cur_individual, lambda_z_cur, is_SGaSP,model@R0,  model@kernel_type, model@alpha, model@p_x, model@num_sources)
  
    ###check this is correct
    #R_5=separable_kernel(model@R0[[5]], exp(par_cur_individual[[5]][1:model@p_x[5]]), model@kernel_type[5],model@alpha[[5]])
    #diag(cov_inv_all[[5]]%*%(R_5+exp(par_cur_individual[[5]][1+model@p_x[5]])*diag(model@num_obs[5])))
  
    
    delta_sample=Sample_delta(cov_inv_all, tilde_output_cur,    par_cur_individual,model@p_x,
                              model@num_sources,
                              num_obs,  rnorm(num_obs))
  }
  
  accept_S_theta=0;
  accept_S_delta=0;

  #accept_N_cov_par=rep(0,model@num_sources);
  accept_S_beta=rep(0,model@num_sources);

  accept_S_dec=0; #this is to count how many sample points are outside the boundary and get rejected for calibration

  #post_cur=0;
  theta_sample=rep(0,model@p_theta);

  r_ratio=0;

  c_prop=1/4


  post_cur=rep(0,model@num_sources)
  post_propose=rep(0,model@num_sources)



  individual_par_cur=par_cur_individual;
  theta_cur=par_cur_theta;

  ##begin MCMC
  for (i_S in 1:model@S){
    #print(i_S)
    if(i_S==floor(model@S*c_prop)){
      #cat(post_cur,'\n')
      cat(c_prop*model@S, ' of ', model@S, ' posterior samples are drawn \n')
      c_prop=c_prop+1/4
    }


    ##1. sample individual parameter for
    for(i in 1:model@num_sources){

        sigma_2_theta_m_sample=Sample_sigma_2_theta_m(c(theta_cur[model@index_theta[[i]]],
                                                        individual_par_cur[[i]]),L[[i]],
                                                      model@output[[i]]-delta_sample,
                                                      length(model@index_theta[[i]]),
                                                      model@p_x[i],model@X[[i]],
                                                      model@have_trend[i],cm_obs_cur[[i]],0,model@num_obs[i] );


        individual_par_cur[[i]][ (model@p_x[i]+2):(model@p_x[i]+2+model@q[i]) ]=sigma_2_theta_m_sample

        post_cur[i]=Log_marginal_post(c(theta_cur[model@index_theta[[i]]],
                                        individual_par_cur[[i]]),L[[i]],
                                      model@output[[i]]-delta_sample,
                                      length(model@index_theta[[i]]),
                                      model@p_x[i],model@X[[i]],
                                      model@have_trend[i],
                                      model@prior_par[[i]][1:model@p_x[i]],
                                      model@prior_par[[i]][model@p_x[i]+1],
                                      model@prior_par[[i]][model@p_x[i]+2],
                                      cm_obs_cur[[i]],0,model@num_obs[i] );
        #//sample xi and log_eta
        xi_sample=rep(0,model@p_x[i])

        for(i_x in 1: model@p_x[i]){
          xi_sample[i_x]=individual_par_cur[[i]][i_x]+model@sd_proposal_cov_par[[i]][i_x]*rnorm(1);
          #distribution_stan_norm(generator);
        }
        log_eta_sample=individual_par_cur[[i]][model@p_x[i]+1]+
          model@sd_proposal_cov_par[[i]][model@p_x[i]+1]*rnorm(1);

        individual_par_propose=  individual_par_cur[[i]]
        individual_par_propose[1:model@p_x[i]]=xi_sample
        individual_par_propose[model@p_x[i]+1]=log_eta_sample



        if(model@discrepancy_type[i]=='GaSP'){
          L_propose[[i]]=Get_R_new(exp(individual_par_propose[1:model@p_x[i]]),
                                   exp(individual_par_propose[model@p_x[i]+1]),
                                   model@R0[[i]],model@kernel_type[i],model@alpha[[i]], 1/model@output_weights[[i]]);
        }else if(model@discrepancy_type[i]=='S-GaSP'){
          if(is.na(model@lambda_z[[i]][i_S])){
            #lambda_z_propose=sqrt( min(exp(xi_sample)*length_input)/(exp(log_eta_sample)/num_obs_all))
            #lambda_z_propose[i]=sqrt(10*sqrt(sum(1/(exp(xi_sample)*length_input[[i]])^2)) /(exp(log_eta_sample)/model@num_obs[i]))
            lambda_z_propose[i]=sqrt(sqrt(sum((exp(xi_sample)*length_input[[i]])^2)) /(exp(log_eta_sample)/model@num_obs[i]))
            
          }else{
            lambda_z_propose[i]=model@lambda_z[[i]][i_S]
          }



          L_propose[[i]]=Get_R_z_new(exp(individual_par_propose[1:model@p_x[i]]),
                                     exp(individual_par_propose[model@p_x[i]+1]),
                                     lambda_z_propose[i],model@R0[[i]],model@kernel_type[i],
                                     model@alpha[[i]], 1/model@output_weights[[i]] );
        }


        post_propose[i]=Log_marginal_post(c(theta_cur[model@index_theta[[i]]],
                                            individual_par_propose),L_propose[[i]],
                                          model@output[[i]]-delta_sample,
                                          length(model@index_theta[[i]]),
                                          model@p_x[i],model@X[[i]],
                                          model@have_trend[i],
                                          model@prior_par[[i]][1:model@p_x[i]],
                                          model@prior_par[[i]][model@p_x[i]+1],
                                          model@prior_par[[i]][model@p_x[i]+2],
                                          cm_obs_cur[[i]],0,model@num_obs[i] );


        r_ratio=exp( post_propose[i]-post_cur[i]);

        decision=Accept_proposal(r_ratio);

        if(decision){
          if(model@discrepancy_type[i]=='S-GaSP'){
            if(is.na(model@lambda_z[[i]][i_S])){
              lambda_z_cur[i]=lambda_z_propose[i]
            }else{
              lambda_z_cur[i]=model@lambda_z[[i]][i_S]
            }
          }

          individual_par_cur[[i]]=individual_par_propose;
          post_cur[i]=post_propose[i];
          L[[i]]=L_propose[[i]];
          accept_S_beta[i]=accept_S_beta[i]+1;
        }


        if(i_S%%model@thinning==0){
          record_par_individual[[i]][floor(i_S/model@thinning),]=individual_par_cur[[i]]

          if(model@discrepancy_type[i]=='S-GaSP'){
            lambda_z_record[[i]][floor(i_S/model@thinning)]=lambda_z_cur[i];
          }

        }


    }


    ############################################################
    #//sample theta
    #theta_cur=par_cur[1:p_theta];
    decision_0=F;

    for(i_theta in 1:model@p_theta){
      theta_sample[i_theta]=rnorm(1,mean=theta_cur[i_theta],sd= (model@sd_proposal_theta[i_theta]*(model@theta_range[i_theta,2]- model@theta_range[i_theta,1]) ) )  ##here maybe truncated
      #cat(theta_sample[i_theta],'\n')
      #cat(theta_range[i_theta,2],'\n')
      #cat(decision_0,'\n')
      if((theta_sample[i_theta]>model@theta_range[i_theta,2])| (theta_sample[i_theta]<model@theta_range[i_theta,1]) ){
        decision_0=T  ##reject directly
        accept_S_dec=accept_S_dec+1
        break;
      }
    }


    #//if decision_0==true, then stay at original place
    #//ow see whether we accept the sample
    if(decision_0==F){
      #theta_propose=theta_sample;

      for(i in 1:model@num_sources){
        cm_obs_propose[[i]]=mathematical_model_eval(model@input[[i]],theta_sample[model@index_theta[[i]]],model@simul_type[i],emulator[[i]],
                                                    model@emulator_type[i],model@loc_index_emulator[[i]],math_model_MS[[i]]);

        #if(model@discrepancy_type[i]=='no-discrepancy'){
        #  post_propose[i]=Log_marginal_post_no_discrepancy(c(theta_sample[model@index_theta[[i]]],
        #                                                     individual_par_cur[[i]]),model@output[[i]]-delta_sample,length(model@index_theta[[i]]),
        #                                                   model@X[[i]],
        #                                                   model@have_trend[i],1/model@output_weights[[i]],cm_obs_propose[[i]],0,model@num_obs[i] );
        #}else{
          
          post_propose[i]=Log_marginal_post(c(theta_sample[model@index_theta[[i]]],
                                              individual_par_cur[[i]]),L[[i]],
                                            model@output[[i]]-delta_sample,
                                            length(model@index_theta[[i]]),
                                            model@p_x[i],model@X[[i]],
                                            model@have_trend[i],
                                            model@prior_par[[i]][1:model@p_x[i]],
                                            model@prior_par[[i]][model@p_x[i]+1],
                                            model@prior_par[[i]][model@p_x[i]+2],
                                            cm_obs_propose[[i]],0,model@num_obs[i] );

        #}


      }


      r_ratio=exp(sum(post_propose)-sum(post_cur));
      decision=Accept_proposal(r_ratio);
      if(decision){

        theta_cur=theta_sample;
        post_cur=post_propose;
        cm_obs_cur=cm_obs_propose;
        accept_S_theta=accept_S_theta+1;

        #cat(par_cur,'\n')

      }

    }


    if(i_S%%model@thinning==0){
      record_post[floor(i_S/model@thinning),]=post_cur

      record_theta[floor(i_S/model@thinning),]=theta_cur

    }


    ##done sample theta

    if(model@discrepancy_type[num_sources_1]!='no-discrepancy'){
      ##3 sample the parameter for delta, only variance
      S_delta=t(delta_sample)%*%cov_inv_all[[num_sources_1]]%*%delta_sample
  
  
      inv_var_delta=rgamma(1,shape=num_obs/2, scale=2/S_delta)
  
  
      individual_par_cur[[num_sources_1]][ (model@p_x[num_sources_1]+1)]=1/inv_var_delta
  
  
      ##sample range par
      if(is_SGaSP[num_sources_1]==0){
        L_delta=Get_R_new(exp(individual_par_cur[[num_sources_1]][1:model@p_x[num_sources_1]]),0,
                          model@R0[[num_sources_1]],model@kernel_type[num_sources_1],model@alpha[[num_sources_1]],
                          rep(1,num_obs));
      }else{
  
        L_delta=Get_R_z_new(exp(individual_par_cur[[num_sources_1]][1:model@p_x[num_sources_1]]),0,lambda_z_cur[num_sources_1],
                          model@R0[[num_sources_1]],model@kernel_type[num_sources_1],model@alpha[[num_sources_1]],
                          rep(1,num_obs));
        
        #L_propose[[i]]=Get_R_z_new(exp(individual_par_propose[1:model@p_x[i]]),
         #                          exp(individual_par_propose[model@p_x[i]+1]),
        #                           lambda_z_propose[i],model@R0[[i]],model@kernel_type[i],
         #                          model@alpha[[i]], 1/model@output_weights[[i]] );
        
  
      }
  
     log_post_delta_cur= Log_marginal_post_delta(individual_par_cur[[num_sources_1]],L_delta,
                        delta_sample,
                        model@p_x[num_sources_1],
                        model@prior_par[[num_sources_1]][1:model@p_x[num_sources_1]],
                        model@prior_par[[num_sources_1]][model@p_x[num_sources_1]+1],
                        model@prior_par[[num_sources_1]][model@p_x[num_sources_1]+2]);
  
  
      #//sample xi
      xi_sample=rep(0,model@p_x[num_sources_1])
  
      for(i_x in 1: model@p_x[num_sources_1]){
        xi_sample[i_x]=individual_par_cur[[num_sources_1]][i_x]+model@sd_proposal_cov_par[[num_sources_1]][i_x]*rnorm(1);
        #distribution_stan_norm(generator);
      }
  
      individual_par_propose=individual_par_cur[[num_sources_1]]
      individual_par_propose[1:model@p_x[num_sources_1]]=(xi_sample)
  
      ##here I add something
      R_propose_delta=separable_kernel(model@R0[[num_sources_1]], exp(individual_par_propose[1:model@p_x[num_sources_1]]),
                                       model@kernel_type[num_sources_1],model@alpha[[num_sources_1]])
      ##too large then I wouldn't update
      if(kappa(R_propose_delta)<10^15){
  
      #if(is_SGaSP[num_sources_1]==1){
      #  R_propose_delta=solve(solve(R_propose_delta)+lambda_z[num_sources_1]*diag(num_obs))
      #}
  
  
        if(is_SGaSP[num_sources_1]==0){
          L_delta_propose=Get_R_new(exp(individual_par_propose[1:model@p_x[num_sources_1]]),0,
                            model@R0[[num_sources_1]],model@kernel_type[num_sources_1],model@alpha[[num_sources_1]],
                            rep(1,num_obs));
        }else{
  
            if(is.na(model@lambda_z[[num_sources_1]][i_S])){
              #lambda_z_propose=sqrt( min(exp(xi_sample)*length_input)/(exp(log_eta_sample)/num_obs_all))
              #lambda_z_propose[num_sources_1]=sqrt(10*sqrt(sum(1/(exp(xi_sample)*length_input[[num_sources_1]])^2)) /(exp(log_eta_sample)/model@num_obs[num_sources_1]))
              lambda_z_propose[num_sources_1]=sqrt(sqrt(sum((exp(xi_sample)*length_input[[num_sources_1]])^2)) /(exp(log_eta_sample)/model@num_obs[num_sources_1]))
              
            }else{
              lambda_z_propose[num_sources_1]=model@lambda_z[[num_sources_1]][i_S]
            }
  
          L_delta_propose=Get_R_z_new(exp(individual_par_propose[1:model@p_x[num_sources_1]]),0,lambda_z_propose[num_sources_1],
                                    model@R0[[num_sources_1]],model@kernel_type[num_sources_1],model@alpha[[num_sources_1]],
                                    rep(1,num_obs));
  
        }
  
        log_post_delta_propose= Log_marginal_post_delta(individual_par_propose,L_delta_propose,
                                                   delta_sample,
                                                   model@p_x[num_sources_1],
                                                   model@prior_par[[num_sources_1]][1:model@p_x[num_sources_1]],
                                                   model@prior_par[[num_sources_1]][model@p_x[num_sources_1]+1],
                                                   model@prior_par[[num_sources_1]][model@p_x[num_sources_1]+2]);
        r_ratio=exp(log_post_delta_propose-log_post_delta_cur);
        decision=Accept_proposal(r_ratio);
  
  
        if(decision){
          if(model@discrepancy_type[num_sources_1]=='S-GaSP'){
            if(is.na(model@lambda_z[[num_sources_1]][i_S])){
              lambda_z_cur[num_sources_1]=lambda_z_propose[num_sources_1]
            }else{
              lambda_z_cur[num_sources_1]=model@lambda_z[[num_sources_1]][i_S]
            }
          }
  
          individual_par_cur[[num_sources_1]]=individual_par_propose;
         # post_cur=post_propose;
         # cm_obs_cur=cm_obs_propose;
          accept_S_delta=accept_S_delta+1;
  
          #cat(par_cur,'\n')
  
        }
  
  
  
        if(i_S%%model@thinning==0){
  
           record_par_individual[[num_sources_1]][floor(i_S/model@thinning),]=individual_par_cur[[num_sources_1]]
  
           if(model@discrepancy_type[num_sources_1]=='S-GaSP'){
             lambda_z_record[[num_sources_1]][floor(i_S/model@thinning)]=lambda_z_cur[num_sources_1];
           }
  
        }
      }
      ##need to redo sample delta and compute all inv
      ##need to have two, one for GP, one for SGP
      cov_inv_all=Get_inv_all(individual_par_cur, lambda_z_cur, is_SGaSP, model@R0,  model@kernel_type, model@alpha, model@p_x, model@num_sources)
  
      for(i in 1:model@num_sources){
        cm_obs_cur[[i]]=mathematical_model_eval(model@input[[i]],theta_cur[model@index_theta[[i]]],model@simul_type[i],emulator[[i]],
                                                model@emulator_type[i],model@loc_index_emulator[[i]],math_model_MS[[i]]);
        tilde_output_cur[[i]]=model@output[[i]]- cm_obs_cur[[i]]  ##here assume zero mean parameter
      }
  
      delta_sample=Sample_delta(cov_inv_all, tilde_output_cur,    individual_par_cur,model@p_x,
                                model@num_sources,
                                num_obs,  rnorm(num_obs))
  
  
      #if(i_S%%100==1){
      #  quilt.plot(x=(model@input[[num_sources_1]][,1]), y=(model@input[[num_sources_1]][,2]),
      #             z=delta_sample,nrow = 80, ncol = 80)
      #}
  
      if(i_S%%model@thinning==0){
        record_model_bias[floor(i_S/model@thinning),]=delta_sample
      }
    }

    ###record the measurement_bias, need to sample that
    if(model@have_measurement_bias_recorded==T){
      for(i in 1: model@num_sources){
        record_measurement_bias[[i]][floor(i_S/model@thinning), ]=tilde_output_cur[[i]]-
          delta_sample+exp(individual_par_cur[[i]][model@p_x[i]+1])*individual_par_cur[[i]][model@p_x[i]+2]*rnorm(num_obs)
      }

    }

    #print(theta_cur[3])
    #print(individual_par_cur)
    #cat(theta_cur,'\n')
  #  print(individual_par_cur[[num_sources_1]])

  }


  mylist$record_post=record_post

  mylist$record_theta=record_theta

  #mylist$individual_par=record_par_individual
  ####I need to have a matrix
  mylist$individual_par=record_par_individual
  if(model@discrepancy_type[num_sources_1]!='no-discrepancy'){
      mylist$record_model_bias=record_model_bias
  }else{
     mylist$record_model_bias=matrix(0,1,num_obs);
  }
  
  mylist$record_measurement_bias=record_measurement_bias


  mylist$accept_S_theta=accept_S_theta
  mylist$accept_S_beta=accept_S_beta
  mylist$accept_S_delta=accept_S_delta

  mylist$accept_S_dec=accept_S_dec

  mylist$lambda_z=lambda_z_record;

  cat('Done with posterior sampling \n')
  cat(accept_S_theta, ' of ', model@S, ' proposed posterior samples of calibration parameters are accepted \n')
  cat( 'For model discrepancy:', accept_S_delta, ' of ', model@S,
       ' proposed posterior samples of range and nugget parameters are accepted \n')

  for(i in 1:model@num_sources){
    if(model@discrepancy_type[i]!='no-discrepancy'){
      cat('For source', i,':', accept_S_beta[i], ' of ', model@S,
          ' proposed posterior samples of range and nugget parameters are accepted \n')
    }
  }

  return(mylist)

}
#   


# ## this is for optimization, need to adjust it for derivatives
neg_log_profile_lik<-function(param,input, output, R0_list, kernel_type, p_theta,p_x, output_weights,
                              lambda_z,theta_range, X, have_trend, alpha,
                              discrepancy_type, simul_type,emulator,emulator_type,loc_index_emulator,math_model,only_value=T,length_input,num_obs_all,S_2_f){
  
  cm_obs=mathematical_model_eval(input,param[1:p_theta],simul_type,emulator,emulator_type,loc_index_emulator,math_model);
  #num_obs_all=length(output); ##may need to adjust when there are multiple
  if(is.na(lambda_z)){
    
    #lambda_z=sqrt( 10*sqrt(sum(1/(exp(param[(p_theta+1):(p_theta+p_x)])*length_input)^2)) /(exp(param[p_theta+p_x+1])/num_obs_all))
    lambda_z=sqrt( sqrt(sum((exp(param[(p_theta+1):(p_theta+p_x)])*length_input)^2)) /(exp(param[p_theta+p_x+1])/num_obs_all))
    
    
    
  }
  
  log_profile_all=Log_profile_lik(param=param[(p_theta+1):(p_theta+1+p_x)], discrepancy_type=discrepancy_type,  output=output,
                                  p_theta=p_theta, p_x=p_x,  X=X, have_mean=have_trend,  cm_obs=cm_obs,
                                  lambda_z=lambda_z,  R0=R0_list, kernel_type= kernel_type,  alpha=alpha,
                                  inv_output_weights=1.0/output_weights, num_obs_all=num_obs_all,  S_2_f=S_2_f)
  
  
  
  
  if(only_value){
    -log_profile_all[[1]]
  }else{
    ans=log_profile_all
    ans[[1]]=-ans[[1]]
    if(discrepancy_type=='S-GaSP'){
      ans[[4]]=lambda_z
    }
    ans
  }
}


loss_function_no_discrepancy<-function(param,input, output, p_theta, output_weights,
                                       X, have_trend,simul_type,emulator,emulator_type,loc_index_emulator,math_model,num_obs_all,S_2_f,
                                       only_value=T){
  
  cm_obs=mathematical_model_eval(input,param,simul_type,emulator,emulator_type,loc_index_emulator,math_model);
  
  loss=Loss_function_no_discrepancy(output=output,  p_theta=p_theta, X=X, have_mean=have_trend,  cm_obs=cm_obs,
                                    inv_output_weights=1/output_weights,num_obs_all,S_2_f)
  
  if(only_value){
    loss[[1]]
  }else{
    loss
  }
}




#  
# neg_log_profile_lik_MS<-function(param,input, output, R0_list, kernel_type, p_theta,p_x, output_weights,
#                               lambda_z,theta_range, X, have_trend, alpha,
#                               discrepancy_type, simul_type,emulator,math_model,only_value=T){
#   num_sources=length(R0_list);
#   return_list=list();
#   profile_lik_sum=0
#   for(i_source in 1:num_sources){
#      cm_obs=mathematical_model_eval(input[[i_source]],param[1:p_theta],simul_type[i_source],emulator[[i_source]],math_model[[i]]);
# 
#      index_start=p_theta+1
#      log_profile_all=Log_profile_lik_all(param[(index_start):(index_start+p_x[i_source])], discrepancy_type[[i_source]],  output[[i_source]],
#                                       p_theta, p_x[i_source],  X[[i_source]], have_trend[[i_source]],  cm_obs,
#                                       lambda_z[i_source],  R0_list[[i_source]],  kernel_type[i_source],  alpha[[i_source]],1/output_weights[[i_source]])
#      return_list[[i_source]]=log_profile_all;
#      profile_lik_sum=profile_lik_sum+log_profile_all[[1]];
#   }
#   if(only_value){
#     -profile_lik_sum
#   }else{
#     return_list[[num_sources+1]]=-profile_lik_sum
#     return_list
#   }
# }

