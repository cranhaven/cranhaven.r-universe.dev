#========================== Aalen's method ===============================

# Find the tail of the InputData
# The aalen's method is invalid when the covariates matrix of certain time point is not of full rank
# This function aims to find the last time point at which the matrix is of full rank (the tail)

Calc_Tail <- function(InputData){
  
  
  # The InputData is of p +3 columns 
  # with the 1st column the time point
  # the following p+1 columns the covariates
  # the last column the censoring indication
  
  CP_nrow = nrow(InputData)
  p = ncol(InputData) -1 -1 -1
  
  #Initialize
  ind = CP_nrow # ind points to the time point reversely
  flag = TRUE   
  
  #loop reversely to find the tail
  while(flag){
    ind = ind -1
    matrix_temp = as.matrix(InputData[(ind:CP_nrow),(2:(p+2))])
    if ((p+1) == rankMatrix(matrix_temp) ){ # the covariate matrix is of full rank
      flag = FALSE
    }
  }
  return(ind)
}


# Aalen OLS estimation, algorithm part

aalen_base <- function(InputData){ 
  
  
  # The InputData is of p +3 columns 
  # with the 1st column the time point
  # the following p+1 columns the covariates
  # the last column the censoring indication
  
  p = ncol(InputData)-1-1-1
  CP_nrow = nrow(InputData)
  CP_tail = Calc_Tail(InputData)
  # prepare matrix X
  
  X  = as.matrix(InputData[,(2:(p+2))])
  
  # prepare to restore beta
  
  
  beta = matrix(0, nrow = CP_nrow + 1, ncol = p+2)
  beta = as.data.frame(beta)
  beta[1,] = 0
  beta[2:(CP_nrow+1),1] = InputData[,1]
  
  # prepare local variables
  
  beta_temp = matrix(NA,p+1,1)
  X_temp = matrix(NA,CP_nrow,p+1)
  
  # Use OLS formula to compute beta
  
  for (i in (1:CP_nrow)){
    if(InputData[i,p+1+1+1] == 1){
      Y_temp = matrix(0,CP_nrow,1)
      Y_temp[i] = 1
      X_temp = as.matrix(X)
      if (i>1){
        X_temp[1:(i-1),] = 0
      }
      if (i <= CP_tail){
        beta_temp = (solve(t(X_temp)%*%X_temp))%*%(t(X_temp)%*%Y_temp)
        for (j in (1:(p+1))){
          beta[i+1,j+1] = beta_temp[j]
        }
      }
    }
    
    if(InputData[i,p+1+1+1] == 0){
      for (j in (1:(p+1))){
        beta[i+1,j+1] = 0
      }
    }
    
  }
  
  Cbeta = Calc_Cbeta(beta)
  
  # prepare to store the records of edges
  
  corner = matrix(NA, nrow = CP_nrow + 1, ncol = p + 1)
  corner = as.data.frame(corner)
  corner[1,] = 0
  corner[(2:(CP_nrow+1)),1] = InputData[,1]
  
  result= list(beta = beta, cumbeta = Cbeta, corner = corner)
  return(result)
}



#========================== Maximum likelihood method ====================

mle_addhazardbase<-function(InputData, matrix_domain, progbar, method, startedge){
  
  # This function is to find the beta which gives the maximum likelihood
  # with restriction to certain domain
  
  
  # We assume there are p covariates in the additive hazard, so there are p+1 beta's to determine( with the constant)
  # The InputData is of p +3 columns 
  # with the 1st column the time point
  # the following p+1 columns the covariates
  # the last column the censoring indication
  
  
  # each rows of the matrix matrix_domain is the coefficients of the linear inequality
  # the domain is defined by matrix_domain \cdot beta >= 0
  
  time_size = nrow(InputData)
  p = ncol(InputData)-1-1-1
  CP_nrow = nrow(InputData)
  InputData_R = InputData[,(1:(p+2))]
  
  # prepare to store beta
  
  beta = matrix(0, nrow = CP_nrow + 1, ncol = p + 2)
  beta = as.data.frame(beta)
  beta[1,] = 0
  beta[(2:(CP_nrow+1)),1] = InputData[,1]
  
  # prepare to store cumulative beta
  
  Cbeta = matrix(0, nrow = CP_nrow + 1, ncol = p + 2)
  Cbeta = as.data.frame(Cbeta)
  Cbeta[1,] = 0
  Cbeta[(2:(CP_nrow+1)),1] = InputData[,1]
  
  # prepare to store the records of edges
  
  corner = matrix(NA, nrow = CP_nrow + 1, ncol = p + 1)
  corner = as.data.frame(corner)
  corner[1,] = 0
  corner[(2:(CP_nrow+1)),1] = InputData[,1]
  
  # prepare result
  
  result= list(beta = beta, cumbeta = Cbeta, corner = corner)
  
  # calculate 
  
  if(progbar){
    cat("\nCalculating beta\n")
    pb <- txtProgressBar(min = 0, max = time_size, style = 3)}
  
  for (time_loop in (2:(CP_nrow+1))){
    
    if(progbar){setTxtProgressBar(pb, time_loop-1)}
    
    # not censored
    if(InputData[time_loop-1,p+1+1+1] == 1){
      if(method == 3){
        combi_indicator = mle_simpl_FStep_Cpp(time_loop,InputData_R,matrix_domain)
        #print(combi_indicator)       #track code
        temp_result = mle_simpl_SStep_R(time_loop, combi_indicator,InputData_R, matrix_domain)
        result$beta[time_loop,2:(p+2)] = temp_result$beta
        result$corner[time_loop,2:(p+1)] = temp_result$corner
        
        #print(beta[time_loop,2:(p+2)])       #track code
      }
      if(method == 2){
        temp_result = mle_loop_Cpp_sub(time_loop, InputData_R, matrix_domain)
        result$beta[time_loop,2:(p+2)] = temp_result[1:(p+1)]
        result$corner[time_loop,2:(p+1)] = temp_result[(p+2):(p+1+p)]
      }
      if(method == 1){
        temp_result = mle_opt_sub(time_loop, InputData_R)
        result$beta[time_loop,2:(p+2)] = temp_result[1:(p+1)]
        result$corner[time_loop,2:(p+1)] = NA
      }
      if(method == 4){
        temp_result = mle_asc_R(time_loop, InputData_R, matrix_domain, startedge)
        result$beta[time_loop,2:(p+2)] = temp_result$beta
        result$corner[time_loop,2:(p+1)] = temp_result$corner
        
      }
    }
    
    # censored
    if(InputData[time_loop-1,p+1+1+1] == 0){
      for (j in (1:(p+1))){
        beta[time_loop,j+1] = 0
      }    
    }
    
  }
  if(progbar){
    close(pb)
    cat("Calculation finished\n")}
  
  return (result)
  
}

#Descending method

mle_simpl_SStep_R <- function(time_indicator,combi_indicator,InputData,matrix_domain){
  
  # This function is the second step to find the beta which gives the likelihood method
  # The time_indicator and the combi_indicator is the beta which gives some local maximum point (only satisfy some of the conditions of the domain)
  
  # We assume there are p covariates in the additive hazard, so there are p+1 beta's to determine( with the constant)
  # The InputData is of p +3 columns 
  # with the 1st column the time point
  # the following p+1 columns the covariates
  # the last column the censoring indication
  
  epsilon = 0.0000001    
  p = ncol(InputData) - 2
  
  history_com = matrix(0,nrow = 1, ncol = p)
  history_com[] = combi_indicator
  
  coefficient_matrix = matrix(0, nrow = p+1, ncol = p+1)
  
  coefficient_matrix[,1:p] = t(matrix_domain[combi_indicator,])
  coefficient_matrix[,p+1] = t(as.matrix(InputData[time_indicator-1,(2:(p+2))]))
  
  N_vector = matrix(0,nrow = p+1, ncol = 1)
  
  for (i in (1:(p+1))){
    N_vector[i] = -sum(InputData[(time_indicator-1):nrow(InputData),i+1])
  }
  
  solution =  matrix(0, nrow = p+1, ncol = 1)
  
  
  
  #print(coefficient_matrix)
  #print(N_vector)
  #print(solution)
  
  solution = solve(coefficient_matrix)%*%N_vector
  
  coefficient_matrix_beta = matrix(0, nrow = p+1, ncol = p+1)
  constant_vector_beta = matrix(0,nrow = p+1, ncol = 1)
  solution_beta = matrix(0,nrow = p+1, ncol = 1)
  
  
  coefficient_matrix_beta = t(coefficient_matrix)
  
  constant_vector_beta[p+1] = -1/solution[p+1]
  min_u = constant_vector_beta[p+1]
  #print(min_u)
  
  solution_beta = solve(coefficient_matrix_beta)%*%constant_vector_beta
  
  domain_check = rep(0,nrow(matrix_domain))
  domain_check = matrix_domain %*% solution_beta
  
  combi_indicator_temp = matrix(0,nrow= 1, ncol = p)
  
  while(!all(domain_check> -1*epsilon)){
    #print("truesolution:")
    #print(solution)
    insert_position = which(domain_check< -1*epsilon)[1]
    
    #print(insert_position)
    
    for(find_loop in 1:p){
      for.next = FALSE
      combi_indicator_temp[] = combi_indicator[]
      combi_indicator_temp[find_loop] = insert_position[1]
      #print(combi_indicator_temp)
      
      for(i in 1:nrow(history_com))
        if(all(combi_indicator_temp == history_com[i,])){
          for.next = TRUE
          break
        }
      if(for.next) next
      
      coefficient_matrix[,1:p] = t(matrix_domain[combi_indicator_temp,])
      coefficient_matrix[,p+1] = t(as.matrix(InputData[time_indicator-1,(2:(p+2))]))
      
      if(abs(det(coefficient_matrix))< epsilon) {next }
      
      solution = solve(coefficient_matrix)%*%N_vector
      
      #print(coefficient_matrix)
      #print(solution)
      
      if(!(all(solution< epsilon) & solution[p+1] < (-1)*epsilon)) {next }
      
      coefficient_matrix_beta = t(coefficient_matrix)
      
      constant_vector_beta[1:p] = 0
      constant_vector_beta[p+1] = -1/solution[p+1]
      if(constant_vector_beta[p+1]>= min_u+epsilon) {next }
      
      min_u= constant_vector_beta[p+1]
      #print(min_u)
      
      solution_beta = solve(coefficient_matrix_beta)%*%constant_vector_beta
      
      combi_indicator[] = sort(combi_indicator_temp[])
      history_com = rbind(history_com, combi_indicator)
      
      domain_check = matrix_domain %*% solution_beta
      
      break
    }
    
  }
  #print(domain_check)
  result = list(beta = solution_beta, corner = combi_indicator)
  return(result)
}

#Optimal method

mle_opt_sub <- function(time_indicator,InputData){
  
  p = ncol(InputData)-1-1
  #matrix_domain  = Calc_BinaryDomain(p) 
  
  J = matrix(ncol = p+1, nrow =  2*p, 0)
  for(i in 1:p){
    J[i,i+1] = 1
  }
  for(i in 1:p){
    J[i+p,1] = 1
    J[i+p,i+1] = -1
  }
  
  X = rep(0,p+1)
  X = InputData[(time_indicator-1),2:(p+2)]
  
  S = rep(0,p+1)
  for (i in (1:(p+1))){
    S[i] = sum(InputData[(time_indicator-1):nrow(InputData),i+1])
  }
  
  value = rep(0, 2*p)
  for (i in 1:(2*p)){
    value[i] = sum(X*J[i,])/ sum(S*J[i,])
  }
  
  beta.choose = which.max(value)
  beta = matrix(nrow = length(beta.choose), ncol = p+1, 0)
  for(i in 1:row(beta)){
    beta[i,] = J[beta.choose[i],]*(1/sum(J[beta.choose[i],]*S))
  }
  
  beta.result = rep(NA, p+1)
  for (i in 1:(p+1)){
    beta.result[i] = sum(beta[,i])/nrow(beta)
  }
  
  return(beta.result)
  
  
}

#Asceding method

mle_asc_R<- function(time_indicator, InputData, matrix_domain, startedge){
  
  #%cat("time_indicator: ", time_indicator)
  #cat("\n")
  p = ncol(InputData) - 2
  epsilon = 0.0000001   
  coefficient_matrix = matrix(0, nrow = p+1, ncol = p+1)
  
  
  combi_indicator = rep(0, p)
  combi_indicator[]  = startedge[]
  
  
  history_com = matrix(0,nrow = 1, ncol = p)
  history_com[] = combi_indicator
  
  direction_auxiliary= matrix(0, nrow =p, ncol = p+1)
  
  N_vector = matrix(0,nrow = p+1, ncol = 1)
  solution =  matrix(0, nrow = p+1, ncol = 1)
  
  
  coefficient_matrix_beta = matrix(0, nrow = p+1, ncol = p+1)
  constant_vector_beta = matrix(0,nrow = p+1, ncol = 1)
  solution_beta = matrix(0,nrow = p+1, ncol = 1)
  
  combi_indicator_temp = matrix(0,nrow= 1, ncol = p)
  gradient = rep(0, p+1)
  direction = rep(0, p+1)
  orthogonal = rep(0,p+1)
  
  insert_checka = rep(0, nrow(matrix_domain))
  insert_checkd = rep(0, nrow(matrix_domain))
  insert_check = rep(0, nrow(matrix_domain))
  
  Zoek = TRUE
  
  
  while(Zoek){
    
    #cat(combi_indicator)
    #cat("\n")
    #if(all(combi_indicator == c(1,6,7,8))) break
    Is.Find = FALSE
    
    coefficient_matrix[,1:p] = t(matrix_domain[combi_indicator,])
    coefficient_matrix[,p+1] = t(as.matrix(InputData[time_indicator-1,(2:(p+2))]))
    
    
    for (i in (1:(p+1))){
      N_vector[i] = -sum(InputData[(time_indicator-1):nrow(InputData),i+1])
    }
    
    #print(coefficient_matrix)
    #cat(N_vector)
    #cat("\n")
    #print(solution)
    
    solution = solve(coefficient_matrix)%*%N_vector
    #cat(solution)
    #cat("\n")
    Veri_solution = all(solution < 1*epsilon) &&(solution[p+1] < -1*epsilon)
    
    
    coefficient_matrix_beta = t(coefficient_matrix)
    
    constant_vector_beta[p+1] = -1/solution[p+1]
    max_u = constant_vector_beta[p+1]
    #cat("max_u: ", max_u)
    #cat("\n")
    
    solution_beta = solve(coefficient_matrix_beta)%*%constant_vector_beta
    #cat("combination: ", combi_indicator)
    #cat("\n")
    #cat("beta solution: \n", solution_beta)
    #cat("\n")
    
    domain_check = rep(0,nrow(matrix_domain))
    domain_check = matrix_domain %*% solution_beta
    #cat("domian_check:\n", domain_check)
    #cat("\n")
    
    Veri_domain = all(domain_check> -1*epsilon)
    
    if(Veri_domain && Veri_solution){
      cat("beta solution: \n", solution_beta)
      cat("\n")
      return(solution_beta)
    }else{
      remove_combiposition = which(solution > epsilon)
      
      #gradient =  -1* solution[p+1] *(as.matrix(InputData[time_indicator-1,(2:(p+2))])) + N_vector
      
      
      for(remove_indicator in remove_combiposition){
        
        direction[] = matrix_domain[combi_indicator[remove_indicator],]
        #direction[] = -1* solution[p+1] *(as.matrix(InputData[time_indicator-1,(2:(p+2))])) + N_vector
        
        
        
        j=1
        for(i in 1:p){
          if(i != remove_indicator){
            direction_auxiliary[j,] = matrix_domain[combi_indicator[i],]
            j= j+1
          }
        }
        direction_auxiliary[p,] = direction[]
        
        for(i in 1:(p-1)){
          orthogonal =  direction_auxiliary[i,]/sqrt(sum(direction_auxiliary[i,] * direction_auxiliary[i,]))
          for(j in (i+1):p){
            direction_auxiliary[j,] = direction_auxiliary[j,] - sum(direction_auxiliary[j,] *orthogonal)*orthogonal
          }
        }
        direction[] = - direction_auxiliary[p,]
        #cat(direction)
        #cat("\n")
        
        #insert_checka = matrix_domain %*% solution_beta
        insert_checka = domain_check[]
        insert_checkd = matrix_domain %*% direction
        #cat(insert_checka)
        #cat("\n")
        #cat(insert_checkd)
        #cat("\n")
        
        for.next = FALSE
        for(i in (1:nrow(matrix_domain))){
          #if(abs (insert_checka[i])<epsilon){
          if(abs(insert_checka[i]) <epsilon){
            if(insert_checkd[i]>epsilon){
              for.next = TRUE
            }
          }        
        }
        if(for.next) next
        
        
        for(i in (1:nrow(matrix_domain))){
          if( insert_checkd[i] == 0){
            insert_check[i] = -1
          }else if(abs(insert_checka[i])< epsilon){
            insert_check[i] =0
          }
          else{
            insert_check[i] = insert_checka[i]/insert_checkd[i]
          }
        }
        
        #cat(insert_check)
        #cat("\n")
        
        insert_check[insert_check<epsilon] = NA
        #cat(insert_check)
        #cat("\n")
        
        insert_indicator = which.min(insert_check)
        #cat(insert_indicator)
        #cat("\n")
        if(0 == length(insert_indicator)) next
        
        
        combi_indicator_temp[] = combi_indicator[]
        combi_indicator_temp[remove_indicator] = insert_indicator
        combi_indicator_temp = sort(combi_indicator_temp)
        
        
        for.next = FALSE
        for(i in 1:nrow(history_com))
          if(all(combi_indicator_temp == history_com[i,])){
            for.next = TRUE
            break
          }
        if(for.next) next
        
        combi_indicator[] = combi_indicator_temp[]
        history_com = rbind(history_com, combi_indicator)
        Is.Find = TRUE
        break
        
      }
      
    }
    
    if(!Is.Find){
      facet.choose = combn(which (abs(domain_check)<epsilon),p)
      facet.choose.ncol  = ncol(facet.choose)
      
      for(i in 1:facet.choose.ncol){
        
        coefficient_matrix[,1:p] = t(matrix_domain[facet.choose[,i],])
        coefficient_matrix[,p+1] = t(as.matrix(InputData[time_indicator-1,(2:(p+2))]))
        if(abs(det(coefficient_matrix))<epsilon) {next}
        else {
          for.next = FALSE
          for(j in 1:nrow(history_com))
            if(all(facet.choose[,i] == history_com[j,])){
              for.next = TRUE
              break
            }
          if(for.next) next
          combi_indicator = t(facet.choose[,i])
          history_com = rbind(history_com, combi_indicator)
          break
        }
        
      }
      
    }
    
    
  }  
  
}

#========================= Support Functions =============================

Calc_BinaryDomain <- function(p){
  
  # Calculate the default matrix domain
  
  matrix_domain = matrix(0, ncol = p+1, nrow = 2^p)
  matrix_domain[,1] = 1
  
  row_indicator = 1
  #matrix_domain[row_indicator,2:(p+1)] = 0
  row_indicator = row_indicator + 1
  
  for (number_picked in 1:p){
    
    index_picked  = combn(p,number_picked)
    size_index_picked = ncol(index_picked)
    
    for(newrow in 1:size_index_picked){
      
      matrix_domain[row_indicator,(index_picked[,newrow]+1)] =1
      row_indicator = row_indicator + 1      
      
    }
  }
  
  return (matrix_domain)
  
}

Scale_data_pre <- function(matrix_design){
  
  p = ncol(matrix_design) -1 -1 -1
  C = rep(0,p) 
  E = rep(0,p)
  for (i in 1:p){
    if(min(matrix_design[,i+2]) == max(matrix_design[,i+2])) {C[i] = 0}
    else{C[i] = min(matrix_design[,i+2])}
    E[i] = max(matrix_design[,i+2] - C[i])
  }
  
  for (i in 1:p){
    matrix_design[,i+2] = (matrix_design[,i+2] - C[i])/E[i]
  }
  
  return(matrix_design)
  
}

Scale_data_after <- function(beta, matrix_design){
  
  p = ncol(matrix_design) -1 -1 -1
  C = rep(0,p) 
  E = rep(0,p)
  for (i in 1:p){
    if(min(matrix_design[,i+2]) == max(matrix_design[,i+2])) {C[i] = 0}
    else{C[i] = min(matrix_design[,i+2])}
    E[i] = max(matrix_design[,i+2] - C[i])
  }
  # calculate beta_0
  #print(C)
  #print(E)
  
  for (i in 1:p){
    beta[,2] = beta[,2]- beta[,i+2]*C[i]/E[i]
  }
  # calculate beta_1 to beta_p
  for (i in 1:p){
    beta[,i+2] = beta[,i+2]/E[i]
  }
  return(beta)
  
  
}
