input_order = function(mat){
  mat[,1] = factor(mat[,1])
  mat = mat[order(mat[,1]),]
  if (ncol(mat) > 2){
    mat = mat[order(mat[,2]),]
  }
  return(mat)
}

btri_mat = function(mat_list){
  t = length(mat_list)
  tmp_long_mat = do.call(rbind, mat_list)
  n = length(tmp_long_mat[1,])
  output = tmp_long_mat
  for (i in 2:t){
    output = cbind(output, rbind(matrix(0, nrow = (i - 1)*n, ncol = n), tmp_long_mat[1:(n*(t - (i - 1))),]))
  }
  return(output)
}

c_mat_pows = function(p, t, iirws, dthetas, lws){
  c_mat = iirws %*% (dthetas + lws)
  output = list()
  temp_mat = diag(p)
  for (i in 0:t){
    output[[as.character(i)]] = temp_mat
    temp_mat = temp_mat %*% c_mat 
  }
  return(output)
}

psi_plus_all = function(p, t, psi){
  output = matrix(0, nrow = p*t, ncol = p)
  for (i in 1:t) {
    output = output + psi[, ((i - 1)*p + 1):(i*p)]
  }
  return(output)
}

make_A_df = function(p, t, dthetas, lws, iirws){
  c_mat = iirws %*% (dthetas + lws)
  m_list = list()
  m_list[[1]] = diag(p)
  for (i in 2:(t+1)){
    if (i == 2){
      m_list[[i]] = c_mat - 2*diag(p)
    }else if(i == 3){
      tmp_mat = diag(p) - c_mat
      tmp_mat = tmp_mat %*% tmp_mat
      m_list[[i]] = tmp_mat
    } else{
      tmp_mat = c_mat %*% tmp_mat
      m_list[[i]] = tmp_mat
    }
  }
  mat_proto = btri_mat(m_list)
  mat_A = mat_proto[-c(1:p), -c((t*p+1):((t+1)*p))]
  mat_A1 = mat_proto[-c((t*p+1):((t+1)*p)), -c((t*p+1):((t+1)*p))]
  return(list("A" = mat_A, 
              "A_1" = mat_A1
  )
  )
}

make_A_deriv_df = function(p, t, w, dthetas, lws, iirws, mode){
  c_mat = iirws %*% (dthetas + lws)
  switch(mode,
         "rho" = {
           deri_c_mat = iirws %*% w %*% iirws %*% (dthetas + lws)
         },
         "theta" = {
           deri_c_mat = iirws
         },
         "lambda" = {
           deri_c_mat = iirws %*% w
         },
         stop("please choose from rho, theta and lambda"))
  sq_mat = (diag(p) - c_mat)%*%(diag(p) - c_mat)
  deri_sq_mat = -((diag(p) - c_mat)%*%deri_c_mat + deri_c_mat %*%(diag(p) - c_mat))
  m_list = list()
  m_list[[1]] = matrix(0, p, p)
  for (i in 2:(t+1)){
    if (i == 2){
      m_list[[i]] = deri_c_mat
    }else if(i == 3){
      m_list[[i]] = deri_sq_mat
    } else if(i == 4){
      tmp_deri_mat = deri_c_mat
      tmp_pow_mat = c_mat
      m_list[[i]] = tmp_pow_mat%*%deri_sq_mat + tmp_deri_mat%*%sq_mat
    } else{
      tmp_deri_mat = c_mat%*%tmp_deri_mat + deri_c_mat%*%tmp_pow_mat
      tmp_pow_mat = tmp_pow_mat%*%c_mat
      m_list[[i]] = tmp_pow_mat%*%deri_sq_mat + tmp_deri_mat%*%sq_mat
    }
  }
  mat_deriv_proto = btri_mat(m_list)
  mat_A_deriv = mat_deriv_proto[-c(1:p), -c((t*p+1):((t+1)*p))]
  mat_A1_deriv = mat_deriv_proto[-c((t*p+1):((t+1)*p)), -c((t*p+1):((t+1)*p))]
  return(list("A_deriv" = mat_A_deriv, 
              "A_deriv_1" = mat_A1_deriv
  )
  )
}

make_R_mats = function(list_pow_c_mat){
  mat_R = as.matrix(bdiag(list_pow_c_mat[-1]))
  mat_R_1 = as.matrix(bdiag(list_pow_c_mat[-length(list_pow_c_mat)]))
  return(list("R" = mat_R,
              "R_1" = mat_R_1))
}

make_G_mats = function(list_pow_c_mat){
  t = length(list_pow_c_mat) - 1
  p = dim(list_pow_c_mat[["0"]])[1]
  list_pow_c_mat[[t + 1]] = NULL
  list_pow_c_mat = c(list(matrix(0, p, p)), list_pow_c_mat)
  G_proto = btri_mat(list_pow_c_mat)
  mat_G = G_proto[-c(1:p), -c((t*p+1):((t+1)*p))]
  mat_G_1 = G_proto[-c((t*p+1):((t+1)*p)), -c((t*p+1):((t+1)*p))]
  return(list("G" = mat_G,
              "G_1" = mat_G_1))
}

#slice horizotally to get g1i
make_g1i_all = function(p, t, pi, v_vec){
  output = matrix(0, nrow = p, ncol = dim(pi)[2])
  temp = pi*v_vec
  for (j in 1:t) {
    output = output + temp[((j-1)*p+1):(j*p),]
  }
  return(output)
}

#contain p elements, i = 1, 2,..., p
make_g2i_all = function(p, t, phi, v_vec, sigs, inv_c, t3 = T){
  # l_phi = u_phi = phi
  # l_phi[upper.tri(l_phi, diag = T)] = 0
  # u_phi[lower.tri(u_phi, diag = T)] = 0
  # slu_phi = as.matrix(forceSymmetric(l_phi + t(u_phi), uplo = "L"))
  
  slu_phi = phi + t(phi)
  #first term
  temp1 = numeric(p*t)
  for (i in 1:t){
    temp1_mat_1 = matrix(0, nrow = p, ncol = t*p)
    for (j in 1:t){
      # temp1_mat_2_l = phi[((i-1)*p + 1):(i*p),((j-1)*p + 1):(j*p)]
      # temp1_mat_2_l[upper.tri(temp1_mat_2_l, diag = T)] = 0
      # temp1_mat_2_u = phi[((j-1)*p + 1):(j*p),((i-1)*p + 1):(i*p)]
      # temp1_mat_2_u[lower.tri(temp1_mat_2_u, diag = T)] = 0
      # temp1_mat_1[, ((j-1)*p + 1):(j*p)] = temp1_mat_2_l + t(temp1_mat_2_u)
      
      temp1_mat_2 = slu_phi[((i-1)*p + 1):(i*p),((j-1)*p + 1):(j*p)]
      l_temp1_mat_2 = temp1_mat_2
      l_temp1_mat_2[upper.tri(l_temp1_mat_2, diag = T)] = 0
      temp1_mat_1[, ((j-1)*p + 1):(j*p)] = l_temp1_mat_2
      
      # temp1_mat_2 = phi[((i-1)*p + 1):(i*p),((j-1)*p + 1):(j*p)]
      # diag(temp1_mat_2) = 0
      # temp1_mat_1[, ((j-1)*p + 1):(j*p)] = temp1_mat_2
    }
    # temp1[((i-1)*p + 1):(i*p)] = v_vec[((i-1)*p + 1):(i*p)]*(temp1_mat_1%*%v_vec)
    temp1[((i-1)*p + 1):(i*p)] = temp1_mat_1%*%v_vec
  }
  #second term
  temp2 = numeric(p*t)
  for (i in 1:t){
    temp2_mat_1 = matrix(0, nrow = p, ncol = t*p)
    for (j in 1:t){
      temp2_mat_2 = phi[((i-1)*p + 1):(i*p),((j-1)*p + 1):(j*p)]
      temp2_mat_1[, ((j-1)*p + 1):(j*p)] = diag(diag(temp2_mat_2))
    }
    # temp2[((i-1)*p + 1):(i*p)] = v_vec[((i-1)*p + 1):(i*p)]*(temp2_mat_1%*%v_vec)
    temp2[((i-1)*p + 1):(i*p)] = temp2_mat_1%*%v_vec
  }
  if (t3){
    #third term
    temp3 = -sigs*diag(kronecker(solve(inv_c), diag(p))%*%phi)
    return(rowSums(matrix(v_vec*(temp1 + temp2) + temp3, ncol = t)))
  } else {
    return(rowSums(matrix(v_vec*(temp1 + temp2), ncol = t)))
  }
}

#contain n elements, i = 1, 2,..., n 
make_g3i_all = function(p, t, y0, y0_ast, psi, v_vec, iirws, iiaws, sigs){
  all_psi = as.matrix(psi_plus_all(p, t, psi))
  list_all_psi = list()
  for (i in 1:t){
    list_all_psi[[i]] = all_psi[((i-1)*p + 1):(i*p),]
  }
  Theta = list_all_psi[[1]]%*%iirws%*%iiaws
  u_Theta = l_Theta = Theta
  u_Theta[lower.tri(u_Theta, diag = T)] = 0
  l_Theta[upper.tri(l_Theta, diag = T)] = 0
  # d0_theta = Theta
  # diag(d0_theta) = 0
  y0_psi = as.vector(bdiag(list_all_psi)%*%y0)
  temp1 = v_vec[1:p]*((l_Theta + t(u_Theta))%*%y0_ast) + diag(Theta)*(v_vec[1:p]*y0_ast + sigs)
  # temp1 = v_vec[1:p]*(d0_theta%*%y0_ast) + diag(Theta)*(v_vec[1:p]*y0_ast + sigs)
  temp2 = rowSums(matrix((v_vec*y0_psi)[-c(1:p)], nrow = p))
  return(temp1 + temp2)
}


df_data = function(dat){
  index = dat[,c(1,2)]
  n = length(unique(index[,1]))
  t = length(unique(index[,2]))
  output = dat[(n+1):(n*t), -c(1,2), drop = F] - dat[1:(n*(t-1)), -c(1,2), drop = F]
  return(cbind(index[-c(1:n),], output))
}

merge_th_diag = function(v1, v2, th){
  p = length(th)
  v_th = rep(v2, p)
  v_th[th] = v1
  return(diag(v_th))
}

make_A_deriv_df_th = function(p, t, th, w, dthetas, lws, iirws, mode){
  c_mat = iirws %*% (dthetas + lws)
  switch(mode,
         "rho1" = {
           temp_w = w
           temp_w[!th, ] = 0 
           deri_c_mat = iirws %*% temp_w %*% iirws %*% (dthetas + lws)
         },
         "theta1" = {
           temp_d = diag(p)
           temp_d[!th, ] = 0 
           deri_c_mat = iirws %*% temp_d
         },
         "lambda1" = {
           temp_w = w
           temp_w[!th, ] = 0 
           deri_c_mat = iirws %*% temp_w
         },
         "rho2" = {
           temp_w = w
           temp_w[th, ] = 0 
           deri_c_mat = iirws %*% temp_w %*% iirws %*% (dthetas + lws)
         },
         "theta2" = {
           temp_d = diag(p)
           temp_d[th, ] = 0 
           deri_c_mat = iirws %*% temp_d
         },
         "lambda2" = {
           temp_w = w
           temp_w[th, ] = 0 
           deri_c_mat = iirws %*% temp_w
         },
         stop("please choose from rho, theta and lambda"))
  sq_mat = (diag(p) - c_mat)%*%(diag(p) - c_mat)
  deri_sq_mat = -((diag(p) - c_mat)%*%deri_c_mat + deri_c_mat %*%(diag(p) - c_mat))
  m_list = list()
  m_list[[1]] = matrix(0, p, p)
  for (i in 2:(t+1)){
    if (i == 2){
      m_list[[i]] = deri_c_mat
    }else if(i == 3){
      m_list[[i]] = deri_sq_mat
    } else if(i == 4){
      tmp_deri_mat = deri_c_mat
      tmp_pow_mat = c_mat
      m_list[[i]] = tmp_pow_mat%*%deri_sq_mat + tmp_deri_mat%*%sq_mat
    } else{
      tmp_deri_mat = c_mat%*%tmp_deri_mat + deri_c_mat%*%tmp_pow_mat
      tmp_pow_mat = tmp_pow_mat%*%c_mat
      m_list[[i]] = tmp_pow_mat%*%deri_sq_mat + tmp_deri_mat%*%sq_mat
    }
  }
  mat_deriv_proto = btri_mat(m_list)
  mat_A_deriv = mat_deriv_proto[-c(1:p), -c((t*p+1):((t+1)*p))]
  mat_A1_deriv = mat_deriv_proto[-c((t*p+1):((t+1)*p)), -c((t*p+1):((t+1)*p))]
  return(list("A_deriv" = mat_A_deriv, 
              "A_deriv_1" = mat_A1_deriv
  )
  )
}