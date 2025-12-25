th_full_aqs = function(para, x_, y, y1, w, th, correction, mode = "normal", all_er = F, w_er, w_lam, inv_c, th_type = "row"){
  x = x_
  xt = t(x)
  tp = length(y)
  p = dim(w)[1]
  t = tp/p
  th_e = rep(th, t)
  bdw = kronecker(diag(t), w)
  bdw_er = kronecker(diag(t), w_er)
  bdw_lam = kronecker(diag(t), w_lam)
  rho1 = para[1]
  alp1 = para[2]
  theta1 = para[3]
  lam1 = para[4]
  rho2 = para[5]
  alp2 = para[6]
  theta2 = para[7]
  lam2 = para[8]
  eq = numeric(8)
  # mat_c = diag(2, t, t)
  # mat_c[(row(mat_c)==col(mat_c)+1)|(row(mat_c)==col(mat_c)-1)] = -1
  # inv_c = solve(mat_c)
  bdinv_c = kronecker(inv_c, diag(p))
  y_q1 = y_q2 = y
  y1_q1 = y1_q2 = y1
  bdw_q1 = bdw_q2 = bdw
  bdw_er_q1 = bdw_er_q2 = bdw_er
  bdw_lam_q1 = bdw_lam_q2 = bdw_lam
  bdw0_q1 = bdw0_q2 = diag(tp)
  w_q1 = w_q2 = w
  w_er_q1 = w_er_q2 = w_er
  w_lam_q1 = w_lam_q2 = w_lam
  y_q1[!th_e] = 0
  y_q2[th_e] = 0
  y1_q1[!th_e] = 0
  y1_q2[th_e] = 0
  switch(th_type,
         "row" = {
           bdw0_q1[!th_e,] = 0
           bdw0_q2[th_e,] = 0
           bdw_q1[!th_e,] = 0
           bdw_q2[th_e,] = 0
           bdw_er_q1[!th_e,] = 0
           bdw_er_q2[th_e,] = 0
           bdw_lam_q1[!th_e,] = 0
           bdw_lam_q2[th_e,] = 0
           w_q1[!th,] = 0
           w_q2[th,] = 0
           w_er_q1[!th,] = 0
           w_er_q2[th,] = 0
           w_lam_q1[!th,] = 0
           w_lam_q2[th,] = 0
           iaws = diag(p) - merge_th_diag(alp1, alp2, th) %*% w_er
           irws = diag(p) - merge_th_diag(rho1, rho2, th) %*% w
           lws = merge_th_diag(lam1, lam2, th) %*% w_lam
         },
         "col" = {
           bdw0_q1[,!th_e] = 0
           bdw0_q2[,th_e] = 0
           bdw_q1[,!th_e] = 0
           bdw_q2[,th_e] = 0
           bdw_er_q1[,!th_e] = 0
           bdw_er_q2[,th_e] = 0
           bdw_lam_q1[,!th_e] = 0
           bdw_lam_q2[,th_e] = 0
           w_q1[,!th] = 0
           w_q2[,th] = 0
           w_er_q1[,!th] = 0
           w_er_q2[,th] = 0
           w_lam_q1[,!th] = 0
           w_lam_q2[,th] = 0
           iaws = diag(p) - w_er %*% merge_th_diag(alp1, alp2, th)
           irws = diag(p) - w %*% merge_th_diag(rho1, rho2, th)
           lws = w_lam %*% merge_th_diag(lam1, lam2, th)
         },
         stop("invalid th_type"))
  iiaws = solve(iaws)
  iaw = kronecker(diag(t), iaws)
  iiaw = kronecker(diag(t), iiaws)
  iaaw = kronecker(inv_c, t(iaws) %*% iaws)
  iirws = solve(irws)
  irw = kronecker(diag(t), irws)
  iirw = kronecker(diag(t), iirws)
  lw =  kronecker(diag(t), lws)
  dthetas = merge_th_diag(theta1, theta2, th)
  dtheta = kronecker(diag(t), dthetas)
  beta = solve(xt %*% iaaw %*% x) %*% xt %*% iaaw %*% (irw %*% y - (dtheta + lw) %*% y1)
  k_ast = irw %*% y - x %*% beta - (dtheta + lw) %*% y1
  sigs = as.numeric(t(k_ast) %*% iaaw %*% k_ast/tp)
  k = length(beta)
  switch(mode,
         "normal" = {
           tmp_mat_1 = t(k_ast) %*% iaaw 
           if (correction){
             A_mats = make_A_df(p, t, dthetas, lws, iirws)
             # vec_bias_theta = diag(bdinv_c %*%A_mats$A_1 %*% iirw)
             # vec_bias_rho = diag(bdinv_c %*%A_mats$A %*% iirw %*% bdw)
             # vec_bias_lam = diag(bdinv_c %*%A_mats$A_1 %*% iirw %*% bdw_lam)
             # eq[1] = 1/sigs * tmp_mat_1%*% y1_q1 + sum(vec_bias_theta[th_e])
             # eq[2] = 1/sigs * tmp_mat_1 %*% bdw_q1 %*% y + sum(vec_bias_rho[th_e])
             # eq[3] = 1/sigs * tmp_mat_1 %*% bdw_lam_q1 %*% y1 + sum(vec_bias_lam[th_e])
             eq[1] = 1/sigs * tmp_mat_1%*% y1_q1 + sum(diag(bdinv_c %*%A_mats$A_1 %*% iirw%*%bdw0_q1))
             eq[2] = 1/sigs * tmp_mat_1 %*% bdw_q1 %*% y + sum(diag(bdinv_c %*%A_mats$A %*% iirw %*% bdw_q1))
             eq[3] = 1/sigs * tmp_mat_1 %*% bdw_lam_q1 %*% y1 + sum(diag(bdinv_c %*%A_mats$A_1 %*% iirw %*% bdw_lam_q1))
             eq[4] = 0.5/sigs * t(k_ast) %*% kronecker(inv_c, t(w_er_q1) %*% iaws + t(iaws) %*% w_er_q1) %*% k_ast - t * sum(diag(iiaws %*% w_er_q1))
             # eq[5] = 1/sigs * tmp_mat_1 %*% y1_q2 + sum(vec_bias_theta[!th_e])
             # eq[6] = 1/sigs * tmp_mat_1 %*% bdw_q2 %*% y + sum(vec_bias_rho[!th_e])
             # eq[7] = 1/sigs * tmp_mat_1 %*% bdw_lam_q2 %*% y1 + sum(vec_bias_lam[!th_e])
             eq[5] = 1/sigs * tmp_mat_1 %*% y1_q2 +  sum(diag(bdinv_c %*%A_mats$A_1 %*% iirw%*%bdw0_q2))
             eq[6] = 1/sigs * tmp_mat_1 %*% bdw_q2 %*% y + sum(diag(bdinv_c %*%A_mats$A %*% iirw %*% bdw_q2))
             eq[7] = 1/sigs * tmp_mat_1 %*% bdw_lam_q2 %*% y1 + sum(diag(bdinv_c %*%A_mats$A_1 %*% iirw %*% bdw_lam_q2))
             eq[8] = 0.5/sigs * t(k_ast) %*% kronecker(inv_c, t(w_er_q2) %*% iaws + t(iaws) %*% w_er_q2) %*% k_ast - t * sum(diag(iiaws %*% w_er_q2))
           } else {
             eq[1] = 1/sigs * tmp_mat_1 %*% y1_q1
             eq[2] = 1/sigs * tmp_mat_1 %*% bdw_q1 %*% y - t * sum(diag(iirws %*% w_q1))
             eq[3] = 1/sigs * tmp_mat_1 %*% bdw_lam_q1 %*% y1
             eq[4] = 0.5/sigs * t(k_ast) %*% kronecker(inv_c, t(w_er_q1) %*% iaws + t(iaws) %*% w_er_q1) %*% k_ast - t * sum(diag(iiaws %*% w_er_q1))
             eq[5] = 1/sigs * tmp_mat_1 %*% y1_q2
             eq[6] = 1/sigs * tmp_mat_1 %*% bdw_q2 %*% y - t * sum(diag(iirws %*% w_q2))
             eq[7] = 1/sigs * tmp_mat_1 %*% bdw_lam_q2 %*% y1
             eq[8] = 0.5/sigs * t(k_ast) %*% kronecker(inv_c, t(w_er_q2) %*% iaws + t(iaws) %*% w_er_q2) %*% k_ast - t * sum(diag(iiaws %*% w_er_q2))
           }
           return(sum(eq^2))
         },
         "beta_sigs" = {
           return(list(beta = beta,
                       sigma2 = sigs))
         },
         "opmd" = {
           #calculate second derivative
           sec_deri = matrix(, nrow = k + 9, ncol = k + 9)
           #calculate derivative of A_mat
           A_mats = make_A_df(p, t, dthetas, lws, iirws)
           A_mats_rho1 = make_A_deriv_df_th(p, t, th, w, dthetas, lws, iirws, mode = "rho1")
           A_mats_theta1 = make_A_deriv_df_th(p, t, th, w, dthetas, lws, iirws, mode = "theta1")
           A_mats_lam1 = make_A_deriv_df_th(p, t, th, w_lam, dthetas, lws, iirws, mode = "lambda1")
           A_mats_rho2 = make_A_deriv_df_th(p, t, th, w, dthetas, lws, iirws, mode = "rho2")
           A_mats_theta2 = make_A_deriv_df_th(p, t, th, w, dthetas, lws, iirws, mode = "theta2")
           A_mats_lam2 = make_A_deriv_df_th(p, t, th, w_lam, dthetas, lws, iirws, mode = "lambda2")
           
           ##order: beta, sigma2, theta1, rho1, lambda1, alpha1, theta2, rho2, lambda2, alpha2
           #lines beta
           sec_deri[1:k, 1:k] = -1/sigs*t(x)%*%iaaw%*%x
           sec_deri[1:k, k+1] = -1/sigs^2*t(x)%*%iaaw%*%k_ast
           sec_deri[1:k, k+2] = -1/sigs*t(x)%*%iaaw%*%y1_q1
           sec_deri[1:k, k+3] = -1/sigs*t(x)%*%iaaw%*%bdw_q1%*%y
           sec_deri[1:k, k+4] = -1/sigs*t(x)%*%iaaw%*%bdw_lam_q1%*%y1
           sec_deri[1:k, k+5] = 1/sigs*t(x)%*%(-kronecker(inv_c, t(w_er_q1) %*% iaws + t(iaws) %*% w_er_q1))%*%k_ast
           sec_deri[1:k, k+6] = -1/sigs*t(x)%*%iaaw%*%y1_q2
           sec_deri[1:k, k+7] = -1/sigs*t(x)%*%iaaw%*%bdw_q2%*%y
           sec_deri[1:k, k+8] = -1/sigs*t(x)%*%iaaw%*%bdw_lam_q2%*%y1
           sec_deri[1:k, k+9] = 1/sigs*t(x)%*%(-kronecker(inv_c, t(w_er_q2) %*% iaws + t(iaws) %*% w_er_q2))%*%k_ast
           
           #line sigma2
           sec_deri[k+1, 1:k] = t(sec_deri[1:k, k+1])
           sec_deri[k+1, k+1] = -1/sigs^3*t(k_ast)%*%iaaw%*%k_ast + tp/(2*sigs^2)
           sec_deri[k+1, k+2] = -1/sigs^2*t(y1_q1)%*%iaaw%*%k_ast
           sec_deri[k+1, k+3] = -1/sigs^2*t(y)%*%t(bdw_q1)%*%iaaw%*%k_ast
           sec_deri[k+1, k+4] = -1/sigs^2*t(y1)%*%t(bdw_lam_q1)%*%iaaw%*%k_ast
           sec_deri[k+1, k+5] = 1/(2*sigs^2)*t(k_ast)%*%(-kronecker(inv_c, t(w_er_q1) %*% iaws + t(iaws) %*% w_er_q1))%*%k_ast
           sec_deri[k+1, k+6] = -1/sigs^2*t(y1_q2)%*%iaaw%*%k_ast
           sec_deri[k+1, k+7] = -1/sigs^2*t(y)%*%t(bdw_q2)%*%iaaw%*%k_ast
           sec_deri[k+1, k+8] = -1/sigs^2*t(y1)%*%t(bdw_lam_q2)%*%iaaw%*%k_ast
           sec_deri[k+1, k+9] = 1/(2*sigs^2)*t(k_ast)%*%(-kronecker(inv_c, t(w_er_q2) %*% iaws + t(iaws) %*% w_er_q2))%*%k_ast
           
           #line theta1
           sec_deri[k+2, 1:k] = t(sec_deri[1:k, k+2])
           sec_deri[k+2, k+1] = sec_deri[k+1, k+2]
           sec_deri[k+2, k+2] = -1/sigs*t(y1_q1)%*%iaaw%*%y1_q1 + sum(diag(bdinv_c%*%A_mats_theta1$A_deriv_1%*%iirw%*%bdw0_q1))
           sec_deri[k+2, k+3] = -1/sigs*t(y1_q1)%*%iaaw%*%bdw_q1%*%y + sum(diag(bdinv_c%*%(A_mats_rho1$A_deriv_1%*%iirw + A_mats$A_1%*%iirw%*%bdw_q1%*%iirw)%*%bdw0_q1))
           sec_deri[k+2, k+4] = -1/sigs*t(y1_q1)%*%iaaw%*%bdw_lam_q1%*%y1 + sum(diag(bdinv_c%*%A_mats_lam1$A_deriv_1%*%iirw%*%bdw0_q1))
           sec_deri[k+2, k+5] = 1/sigs*t(y1_q1)%*%(-kronecker(inv_c, t(w_er_q1) %*% iaws + t(iaws) %*% w_er_q1))%*%k_ast
           sec_deri[k+2, k+6] = -1/sigs*t(y1_q1)%*%iaaw%*%y1_q2 + sum(diag(bdinv_c%*%A_mats_theta2$A_deriv_1%*%iirw%*%bdw0_q1))
           sec_deri[k+2, k+7] = -1/sigs*t(y1_q1)%*%iaaw%*%bdw_q2%*%y + sum(diag(bdinv_c%*%(A_mats_rho2$A_deriv_1%*%iirw + A_mats$A_1%*%iirw%*%bdw_q2%*%iirw)%*%bdw0_q1))
           sec_deri[k+2, k+8] = -1/sigs*t(y1_q1)%*%iaaw%*%bdw_lam_q2%*%y1 + sum(diag(bdinv_c%*%A_mats_lam2$A_deriv_1%*%iirw%*%bdw0_q1))
           sec_deri[k+2, k+9] = 1/sigs*t(y1_q1)%*%(-kronecker(inv_c, t(w_er_q2) %*% iaws + t(iaws) %*% w_er_q2))%*%k_ast
           
           #line rho1
           sec_deri[k+3, 1:k] = t(sec_deri[1:k, k+3])
           sec_deri[k+3, k+1] = sec_deri[k+1, k+3]
           sec_deri[k+3, k+2] = -1/sigs*t(y)%*%t(bdw_q1)%*%iaaw%*%y1_q1 + sum(diag(bdinv_c%*%A_mats_theta1$A_deriv%*%iirw%*%bdw_q1))
           sec_deri[k+3, k+3] = -1/sigs*t(y)%*%t(bdw_q1)%*%iaaw%*%bdw_q1%*%y + sum(diag(bdinv_c%*%(A_mats_rho1$A_deriv%*%iirw + A_mats$A%*%iirw%*%bdw_q1%*%iirw)%*%bdw_q1))
           sec_deri[k+3, k+4] = -1/sigs*t(y)%*%t(bdw_q1)%*%iaaw%*%bdw_lam_q1%*%y1 + sum(diag(bdinv_c%*%A_mats_lam1$A_deriv%*%iirw%*%bdw_q1))
           sec_deri[k+3, k+5] = 1/sigs*t(y)%*%t(bdw_q1)%*%(-kronecker(inv_c, t(w_er_q1) %*% iaws + t(iaws) %*% w_er_q1))%*%k_ast
           sec_deri[k+3, k+6] = -1/sigs*t(y)%*%t(bdw_q1)%*%iaaw%*%y1_q2 + sum(diag(bdinv_c%*%A_mats_theta2$A_deriv%*%iirw%*%bdw_q1))
           sec_deri[k+3, k+7] = -1/sigs*t(y)%*%t(bdw_q1)%*%iaaw%*%bdw_q2%*%y + sum(diag(bdinv_c%*%(A_mats_rho2$A_deriv%*%iirw + A_mats$A%*%iirw%*%bdw_q2%*%iirw)%*%bdw_q1))
           sec_deri[k+3, k+8] = -1/sigs*t(y)%*%t(bdw_q1)%*%iaaw%*%bdw_lam_q2%*%y1 + sum(diag(bdinv_c%*%A_mats_lam2$A_deriv%*%iirw%*%bdw_q1))
           sec_deri[k+3, k+9] = 1/sigs*t(y)%*%t(bdw_q1)%*%(-kronecker(inv_c, t(w_er_q2) %*% iaws + t(iaws) %*% w_er_q2))%*%k_ast
           
           #line lambda1
           sec_deri[k+4, 1:k] = t(sec_deri[1:k, k+4])
           sec_deri[k+4, k+1] = sec_deri[k+1, k+4]
           sec_deri[k+4, k+2] = -1/sigs*t(y1)%*%t(bdw_lam_q1)%*%iaaw%*%y1_q1 + sum(diag(bdinv_c%*%A_mats_theta1$A_deriv_1%*%iirw%*%bdw_lam_q1))
           sec_deri[k+4, k+3] = -1/sigs*t(y1)%*%t(bdw_lam_q1)%*%iaaw%*%bdw_q1%*%y + sum(diag(bdinv_c%*%(A_mats_rho1$A_deriv_1%*%iirw + A_mats$A_1%*%iirw%*%bdw_q1%*%iirw)%*%bdw_lam_q1))
           sec_deri[k+4, k+4] = -1/sigs*t(y1)%*%t(bdw_lam_q1)%*%iaaw%*%bdw_lam_q1%*%y1 + sum(diag(bdinv_c%*%A_mats_lam1$A_deriv_1%*%iirw%*%bdw_lam_q1))
           sec_deri[k+4, k+5] = 1/sigs*t(y1)%*%t(bdw_lam_q1)%*%(-kronecker(inv_c, t(w_er_q1) %*% iaws + t(iaws) %*% w_er_q1))%*%k_ast
           sec_deri[k+4, k+6] = -1/sigs*t(y1)%*%t(bdw_lam_q1)%*%iaaw%*%y1_q2 + sum(diag(bdinv_c%*%A_mats_theta2$A_deriv_1%*%iirw%*%bdw_lam_q1))
           sec_deri[k+4, k+7] = -1/sigs*t(y1)%*%t(bdw_lam_q1)%*%iaaw%*%bdw_q2%*%y + sum(diag(bdinv_c%*%(A_mats_rho2$A_deriv_1%*%iirw + A_mats$A_1%*%iirw%*%bdw_q2%*%iirw)%*%bdw_lam_q1))
           sec_deri[k+4, k+8] = -1/sigs*t(y1)%*%t(bdw_lam_q1)%*%iaaw%*%bdw_lam_q2%*%y1 + sum(diag(bdinv_c%*%A_mats_lam2$A_deriv_1%*%iirw%*%bdw_lam_q1))
           sec_deri[k+4, k+9] = 1/sigs*t(y1)%*%t(bdw_lam_q1)%*%(-kronecker(inv_c, t(w_er_q2) %*% iaws + t(iaws) %*% w_er_q2))%*%k_ast
           
           #line alpha1
           sec_deri[k+5, 1:k] = t(sec_deri[1:k, k+5])
           sec_deri[k+5, k+1] = sec_deri[k+1, k+5]
           sec_deri[k+5, k+2] = sec_deri[k+2, k+5]
           sec_deri[k+5, k+3] = sec_deri[k+3, k+5]
           sec_deri[k+5, k+4] = sec_deri[k+4, k+5]
           sec_deri[k+5, k+5] = -0.5/sigs*t(k_ast)%*%(kronecker(inv_c, t(w_er_q1) %*% w_er_q1 + t(w_er_q1) %*% w_er_q1))%*%k_ast - sum(diag(bdw_er_q1%*%iiaw%*%bdw_er_q1%*%iiaw))
           sec_deri[k+5, k+6] = 1/sigs*t(y1_q2)%*%(-kronecker(inv_c, t(w_er_q1) %*% iaws + t(iaws) %*% w_er_q1))%*%k_ast
           sec_deri[k+5, k+7] = 1/sigs*t(y)%*%t(bdw_er_q2)%*%(-kronecker(inv_c, t(w_er_q1) %*% iaws + t(iaws) %*% w_er_q1))%*%k_ast
           sec_deri[k+5, k+8] = 1/sigs*t(y1)%*%t(bdw_er_q2)%*%(-kronecker(inv_c, t(w_er_q1) %*% iaws + t(iaws) %*% w_er_q1))%*%k_ast
           sec_deri[k+5, k+9] = -0.5/sigs*t(k_ast)%*%(kronecker(inv_c, t(w_er_q1) %*% w_er_q2 + t(w_er_q2) %*% w_er_q1))%*%k_ast - sum(diag(bdw_er_q1%*%iiaw%*%bdw_er_q2%*%iiaw))
           
           #line theta2
           sec_deri[k+6, 1:k] = t(sec_deri[1:k, k+6])
           sec_deri[k+6, k+1] = sec_deri[k+1, k+6]
           sec_deri[k+6, k+2] = -1/sigs*t(y1_q2)%*%iaaw%*%y1_q1 + sum(diag(bdinv_c%*%A_mats_theta1$A_deriv_1%*%iirw%*%bdw0_q2))
           sec_deri[k+6, k+3] = -1/sigs*t(y1_q2)%*%iaaw%*%bdw_q1%*%y + sum(diag(bdinv_c%*%(A_mats_rho1$A_deriv_1%*%iirw + A_mats$A_1%*%iirw%*%bdw_q1%*%iirw)%*%bdw0_q2))
           sec_deri[k+6, k+4] = -1/sigs*t(y1_q2)%*%iaaw%*%bdw_lam_q1%*%y1 + sum(diag(bdinv_c%*%A_mats_lam1$A_deriv_1%*%iirw%*%bdw0_q2))
           sec_deri[k+6, k+5] = 1/sigs*t(y1_q2)%*%(-kronecker(inv_c, t(w_er_q1) %*% iaws + t(iaws) %*% w_er_q1))%*%k_ast
           sec_deri[k+6, k+6] = -1/sigs*t(y1_q2)%*%iaaw%*%y1_q2 + sum(diag(bdinv_c%*%A_mats_theta2$A_deriv_1%*%iirw%*%bdw0_q2))
           sec_deri[k+6, k+7] = -1/sigs*t(y1_q2)%*%iaaw%*%bdw_q2%*%y + sum(diag(bdinv_c%*%(A_mats_rho2$A_deriv_1%*%iirw + A_mats$A_1%*%iirw%*%bdw_q2%*%iirw)%*%bdw0_q2))
           sec_deri[k+6, k+8] = -1/sigs*t(y1_q2)%*%iaaw%*%bdw_lam_q2%*%y1 + sum(diag(bdinv_c%*%A_mats_lam2$A_deriv_1%*%iirw%*%bdw0_q2))
           sec_deri[k+6, k+9] = 1/sigs*t(y1_q2)%*%(-kronecker(inv_c, t(w_er_q2) %*% iaws + t(iaws) %*% w_er_q2))%*%k_ast
           
           #line rho2
           sec_deri[k+7, 1:k] = t(sec_deri[1:k, k+7])
           sec_deri[k+7, k+1] = sec_deri[k+1, k+7]
           sec_deri[k+7, k+2] = -1/sigs*t(y)%*%t(bdw_q2)%*%iaaw%*%y1_q1 + sum(diag(bdinv_c%*%A_mats_theta1$A_deriv%*%iirw%*%bdw_q2))
           sec_deri[k+7, k+3] = -1/sigs*t(y)%*%t(bdw_q2)%*%iaaw%*%bdw_q1%*%y + sum(diag(bdinv_c%*%(A_mats_rho1$A_deriv%*%iirw + A_mats$A%*%iirw%*%bdw_q1%*%iirw)%*%bdw_q2))
           sec_deri[k+7, k+4] = -1/sigs*t(y)%*%t(bdw_q2)%*%iaaw%*%bdw_lam_q1%*%y1 + sum(diag(bdinv_c%*%A_mats_lam1$A_deriv%*%iirw%*%bdw_q2))
           sec_deri[k+7, k+5] = 1/sigs*t(y)%*%t(bdw_q2)%*%(-kronecker(inv_c, t(w_er_q1) %*% iaws + t(iaws) %*% w_er_q1))%*%k_ast
           sec_deri[k+7, k+6] = -1/sigs*t(y)%*%t(bdw_q2)%*%iaaw%*%y1_q2 + sum(diag(bdinv_c%*%A_mats_theta2$A_deriv%*%iirw%*%bdw_q2))
           sec_deri[k+7, k+7] = -1/sigs*t(y)%*%t(bdw_q2)%*%iaaw%*%bdw_q2%*%y + sum(diag(bdinv_c%*%(A_mats_rho2$A_deriv%*%iirw + A_mats$A%*%iirw%*%bdw_q2%*%iirw)%*%bdw_q2))
           sec_deri[k+7, k+8] = -1/sigs*t(y)%*%t(bdw_q2)%*%iaaw%*%bdw_lam_q2%*%y1 + sum(diag(bdinv_c%*%A_mats_lam2$A_deriv%*%iirw%*%bdw_q2))
           sec_deri[k+7, k+9] = 1/sigs*t(y)%*%t(bdw_q2)%*%(-kronecker(inv_c, t(w_er_q2) %*% iaws + t(iaws) %*% w_er_q2))%*%k_ast
           
           #line lambda2
           sec_deri[k+8, 1:k] = t(sec_deri[1:k, k+8])
           sec_deri[k+8, k+1] = sec_deri[k+1, k+8]
           sec_deri[k+8, k+2] = -1/sigs*t(y1)%*%t(bdw_lam_q2)%*%iaaw%*%y1_q1 + sum(diag(bdinv_c%*%A_mats_theta1$A_deriv_1%*%iirw%*%bdw_lam_q2))
           sec_deri[k+8, k+3] = -1/sigs*t(y1)%*%t(bdw_lam_q2)%*%iaaw%*%bdw_q1%*%y + sum(diag(bdinv_c%*%(A_mats_rho1$A_deriv_1%*%iirw + A_mats$A_1%*%iirw%*%bdw_q1%*%iirw)%*%bdw_lam_q2))
           sec_deri[k+8, k+4] = -1/sigs*t(y1)%*%t(bdw_lam_q2)%*%iaaw%*%bdw_lam_q1%*%y1 + sum(diag(bdinv_c%*%A_mats_lam1$A_deriv_1%*%iirw%*%bdw_lam_q2))
           sec_deri[k+8, k+5] = 1/sigs*t(y1)%*%t(bdw_lam_q2)%*%(-kronecker(inv_c, t(w_er_q1) %*% iaws + t(iaws) %*% w_er_q1))%*%k_ast
           sec_deri[k+8, k+6] = -1/sigs*t(y1)%*%t(bdw_lam_q2)%*%iaaw%*%y1_q2 + sum(diag(bdinv_c%*%A_mats_theta2$A_deriv_1%*%iirw%*%bdw_lam_q2))
           sec_deri[k+8, k+7] = -1/sigs*t(y1)%*%t(bdw_lam_q2)%*%iaaw%*%bdw_q2%*%y + sum(diag(bdinv_c%*%(A_mats_rho2$A_deriv_1%*%iirw + A_mats$A_1%*%iirw%*%bdw_q2%*%iirw)%*%bdw_lam_q2))
           sec_deri[k+8, k+8] = -1/sigs*t(y1)%*%t(bdw_lam_q2)%*%iaaw%*%bdw_lam_q2%*%y1 + sum(diag(bdinv_c%*%A_mats_lam2$A_deriv_1%*%iirw%*%bdw_lam_q2))
           sec_deri[k+8, k+9] = 1/sigs*t(y1)%*%t(bdw_lam_q2)%*%(-kronecker(inv_c, t(w_er_q2) %*% iaws + t(iaws) %*% w_er_q2))%*%k_ast
           
           #line alpha2
           sec_deri[k+9, 1:k] = t(sec_deri[1:k, k+9])
           sec_deri[k+9, k+1] = sec_deri[k+1, k+9]
           sec_deri[k+9, k+2] = sec_deri[k+2, k+9]
           sec_deri[k+9, k+3] = sec_deri[k+3, k+9]
           sec_deri[k+9, k+4] = sec_deri[k+4, k+9]
           sec_deri[k+9, k+5] = -0.5/sigs*t(k_ast)%*%(kronecker(inv_c, t(w_er_q2) %*% w_er_q1 + t(w_er_q1) %*% w_er_q2))%*%k_ast - sum(diag(bdw_er_q2%*%iiaw%*%bdw_er_q1%*%iiaw))
           sec_deri[k+9, k+6] = sec_deri[k+6, k+9]
           sec_deri[k+9, k+7] = sec_deri[k+7, k+9]
           sec_deri[k+9, k+8] = sec_deri[k+8, k+9]
           sec_deri[k+9, k+9] = -0.5/sigs*t(k_ast)%*%(kronecker(inv_c, t(w_er_q2) %*% w_er_q2 + t(w_er_q2) %*% w_er_q2))%*%k_ast - sum(diag(bdw_er_q2%*%iiaw%*%bdw_er_q2%*%iiaw))
           
           #make list of powers of c_mat
           list_pow_c_mat = c_mat_pows(p,t, iirws, dthetas, lws)
           #make R and G mats
           R_mats = make_R_mats(list_pow_c_mat)
           G_mats = make_G_mats(list_pow_c_mat)
           #make eta, S, pi, phi, psi and v_vec
           iciaw = kronecker(inv_c, iaws)
           eta = G_mats$G %*% iirw %*% x %*% beta
           eta_1 = G_mats$G_1 %*% iirw %*% x %*% beta
           S = G_mats$G %*% iirw %*% iiaw
           S_1 = G_mats$G_1 %*% iirw %*% iiaw
           eta_1_1 = eta_1_2 = eta_1
           eta_1_1[!th_e] = 0
           eta_1_2[th_e] = 0
           S_1_1 = S_1_2 = S_1
           S_1_1[!th_e, ] = 0
           S_1_2[th_e, ] = 0
           R_1_1 = R_1_2 = R_mats$R_1
           R_1_1[!th_e, ] = 0
           R_1_2[th_e, ] = 0
           
           #for beta
           pi_1 = 1/sigs*t(iciaw)%*%x
           #for q1
           pi_2 = 1/sigs*t(iciaw)%*%eta_1_1
           pi_3 = 1/sigs*t(iciaw)%*%bdw_q1%*%eta
           pi_4 = 1/sigs*t(iciaw)%*%bdw_lam_q1%*%eta_1
           #for q2
           pi_5 = 1/sigs*t(iciaw)%*%eta_1_2
           pi_6 = 1/sigs*t(iciaw)%*%bdw_q2%*%eta
           pi_7 = 1/sigs*t(iciaw)%*%bdw_lam_q2%*%eta_1
           
           #for sigma2
           phi_1 = 0.5/(sigs^2)*kronecker(inv_c, diag(p))
           #for q1
           phi_2 = 1/sigs*iciaw%*%S_1_1
           phi_3 = 1/sigs*iciaw%*%bdw_q1%*%S
           phi_4 = 1/sigs*iciaw%*%bdw_lam_q1%*%S_1
           phi_5 = 0.5/sigs*kronecker(inv_c, t(w_er_q1%*%iiaws) + w_er_q1%*%iiaws)
           #for q2
           phi_6 = 1/sigs*iciaw%*%S_1_2
           phi_7 = 1/sigs*iciaw%*%bdw_q2%*%S
           phi_8 = 1/sigs*iciaw%*%bdw_lam_q2%*%S_1
           phi_9 = 0.5/sigs*kronecker(inv_c, t(w_er_q2%*%iiaws) + w_er_q2%*%iiaws)
           
           #for q1
           psi_1 = 1/sigs*iciaw%*%R_1_1
           psi_2 = 1/sigs*iciaw%*%bdw_q1%*%R_mats$R
           psi_3 = 1/sigs*iciaw%*%bdw_lam_q1%*%R_mats$R_1
           #for q2
           psi_4 = 1/sigs*iciaw%*%R_1_2
           psi_5 = 1/sigs*iciaw%*%bdw_q2%*%R_mats$R
           psi_6 = 1/sigs*iciaw%*%bdw_lam_q2%*%R_mats$R_1
           
           v_vec = as.vector(iaw%*%k_ast)
           #make y0, y0_ast
           y0 = rep(y1[1:p], t)
           y0_ast = iaws %*% irws %*% y0[1:p]
           
           #make all gs
           g11 = make_g1i_all(p, t, pi_1, v_vec)
           g12 = make_g1i_all(p, t, pi_2, v_vec)
           g13 = make_g1i_all(p, t, pi_3, v_vec)
           g14 = make_g1i_all(p, t, pi_4, v_vec)
           g15 = make_g1i_all(p, t, pi_5, v_vec)
           g16 = make_g1i_all(p, t, pi_6, v_vec)
           g17 = make_g1i_all(p, t, pi_7, v_vec)
           g21 = make_g2i_all(p, t, phi_1, v_vec, sigs, inv_c)
           g22 = make_g2i_all(p, t, phi_2, v_vec, sigs, inv_c)
           g23 = make_g2i_all(p, t, phi_3, v_vec, sigs, inv_c)
           g24 = make_g2i_all(p, t, phi_4, v_vec, sigs, inv_c)
           g25 = make_g2i_all(p, t, phi_5, v_vec, sigs, inv_c)
           g26 = make_g2i_all(p, t, phi_6, v_vec, sigs, inv_c)
           g27 = make_g2i_all(p, t, phi_7, v_vec, sigs, inv_c)
           g28 = make_g2i_all(p, t, phi_8, v_vec, sigs, inv_c)
           g29 = make_g2i_all(p, t, phi_9, v_vec, sigs, inv_c)
           g31 = make_g3i_all(p, t, y0, y0_ast, psi_1, v_vec, iirws, iiaws, sigs)
           g32 = make_g3i_all(p, t, y0, y0_ast, psi_2, v_vec, iirws, iiaws, sigs)
           g33 = make_g3i_all(p, t, y0, y0_ast, psi_3, v_vec, iirws, iiaws, sigs)
           g34 = make_g3i_all(p, t, y0, y0_ast, psi_4, v_vec, iirws, iiaws, sigs)
           g35 = make_g3i_all(p, t, y0, y0_ast, psi_5, v_vec, iirws, iiaws, sigs)
           g36 = make_g3i_all(p, t, y0, y0_ast, psi_6, v_vec, iirws, iiaws, sigs)
           
           #make all g mat
           all_g = matrix(0, nrow = k + 9, ncol = p)
           all_g[1:k, ] = t(g11)
           all_g[k + 1, ] = g21
           all_g[k + 2, ] = g31 + g12 + g22
           all_g[k + 3, ] = g32 + g13 + g23
           all_g[k + 4, ] = g33 + g14 + g24
           all_g[k + 5, ] = g25
           all_g[k + 6, ] = g34 + g15 + g26
           all_g[k + 7, ] = g35 + g16 + g27
           all_g[k + 8, ] = g36 + g17 + g28
           all_g[k + 9, ] = g29
           
           inv_sig_mat = solve(-sec_deri)
           gamma_mat = all_g%*%t(all_g)
           
           if (all_er){
             return_list = list(vc_mat = inv_sig_mat%*%gamma_mat%*%inv_sig_mat,
                                hes_mat = inv_sig_mat,
                                gamma_mat = gamma_mat)
             return(return_list)
           } else {
             vc_mat = inv_sig_mat%*%gamma_mat%*%inv_sig_mat
             return(list(vc_mat = vc_mat))
           }
         },
         "residual" = {
           return(as.vector(iaw%*%k_ast))
         },
         stop("undefined mode"))
}

th_slm_aqs = function(para, x_, y, y1, w, th, correction, mode = "normal", all_er = F, inv_c, th_type = "row"){
  x = x_
  tp = length(y)
  p = dim(w)[1]
  t = tp/p
  th_e = rep(th, t)
  bdw = kronecker(diag(t), w)
  rho1 = para[1]
  theta1 = para[2]
  rho2 = para[3]
  theta2 = para[4]
  eq = numeric(4)
  # mat_c = diag(2, t, t)
  # mat_c[(row(mat_c)==col(mat_c)+1)|(row(mat_c)==col(mat_c)-1)] = -1
  # inv_c = solve(mat_c)
  bdinv_c = kronecker(inv_c, diag(p))
  y_q1 = y_q2 = y
  y1_q1 = y1_q2 = y1
  bdw_q1 = bdw_q2 = bdw
  bdw0_q1 = bdw0_q2 = diag(tp)
  w_q1 = w_q2 = w
  y_q1[!th_e] = 0
  y_q2[th_e] = 0
  y1_q1[!th_e] = 0
  y1_q2[th_e] = 0
  switch(th_type,
         "row" = {
           bdw0_q1[!th_e,] = 0
           bdw0_q2[th_e,] = 0
           bdw_q1[!th_e,] = 0
           bdw_q2[th_e,] = 0
           w_q1[!th,] = 0
           w_q2[th,] = 0
           irws = diag(p) - merge_th_diag(rho1, rho2, th) %*% w
         },
         "col" = {
           bdw0_q1[,!th_e] = 0
           bdw0_q2[,th_e] = 0
           bdw_q1[,!th_e] = 0
           bdw_q2[,th_e] = 0
           w_q1[,!th] = 0
           w_q2[,th] = 0
           irws = diag(p) - w %*% merge_th_diag(rho1, rho2, th)
         },
         stop("invalid th_type"))
  xt = t(x)
  iaws = diag(p)
  iiaws = diag(p)
  iaw = diag(p*t)
  iiaw = diag(p*t)
  iaaw = bdinv_c
  iirws = solve(irws)
  irw = kronecker(diag(t), irws)
  iirw = kronecker(diag(t), iirws)
  lws = 0
  lw =  0
  dthetas = merge_th_diag(theta1, theta2, th)
  dtheta = kronecker(diag(t), dthetas)
  beta = solve(xt %*% iaaw %*% x) %*% xt %*% iaaw %*% (irw %*% y - (dtheta + lw) %*% y1)
  k_ast = irw %*% y - x %*% beta - (dtheta + lw) %*% y1
  sigs = as.numeric(t(k_ast) %*% iaaw %*% k_ast/tp)
  k = length(beta)
  switch(mode,
         "normal" = {
           tmp_mat_1 = t(k_ast) %*% iaaw 
           if (correction){
             A_mats = make_A_df(p, t, dthetas, lws, iirws)
             # vec_bias_theta = diag(bdinv_c %*%A_mats$A_1 %*% iirw)
             # vec_bias_rho = diag(bdinv_c %*%A_mats$A %*% iirw %*% bdw)
             # eq[1] = 1/sigs * tmp_mat_1%*% y1_q1 + sum(vec_bias_theta[th_e])
             # eq[2] = 1/sigs * tmp_mat_1 %*% bdw_q1 %*% y + sum(vec_bias_rho[th_e])
             # eq[3] = 1/sigs * tmp_mat_1 %*% y1_q2 + sum(vec_bias_theta[!th_e])
             # eq[4] = 1/sigs * tmp_mat_1 %*% bdw_q2 %*% y + sum(vec_bias_rho[!th_e])
             eq[1] = 1/sigs * tmp_mat_1%*% y1_q1 + sum(diag(bdinv_c %*%A_mats$A_1 %*% iirw %*% bdw0_q1))
             eq[2] = 1/sigs * tmp_mat_1 %*% bdw_q1 %*% y + sum(diag(bdinv_c %*%A_mats$A %*% iirw %*% bdw_q1))
             eq[3] = 1/sigs * tmp_mat_1 %*% y1_q2 + sum(diag(bdinv_c %*%A_mats$A_1 %*% iirw %*% bdw0_q2))
             eq[4] = 1/sigs * tmp_mat_1 %*% bdw_q2 %*% y + sum(diag(bdinv_c %*%A_mats$A %*% iirw %*% bdw_q2))
           } else {
             eq[1] = 1/sigs * tmp_mat_1 %*% y1_q1
             eq[2] = 1/sigs * tmp_mat_1 %*% bdw_q1 %*% y - t * sum(diag(iirws %*% w_q1))
             eq[3] = 1/sigs * tmp_mat_1 %*% y1_q2
             eq[4] = 1/sigs * tmp_mat_1 %*% bdw_q2 %*% y - t * sum(diag(iirws %*% w_q2))
           }
           return(sum(eq^2))
         },
         "beta_sigs" = {
           return(list(beta = beta,
                       sigma2 = sigs))
         },
         "opmd" = {
           #calculate second derivative
           sec_deri = matrix(, nrow = k + 5, ncol = k + 5)
           #calculate derivative of A_mat
           A_mats = make_A_df(p, t, dthetas, lws, iirws)
           A_mats_rho1 = make_A_deriv_df_th(p, t, th, w, dthetas, lws, iirws, mode = "rho1")
           A_mats_theta1 = make_A_deriv_df_th(p, t, th, w, dthetas, lws, iirws, mode = "theta1")
           A_mats_rho2 = make_A_deriv_df_th(p, t, th, w, dthetas, lws, iirws, mode = "rho2")
           A_mats_theta2 = make_A_deriv_df_th(p, t, th, w, dthetas, lws, iirws, mode = "theta2")
           
           ##order: beta, sigma2, theta1, rho1, lambda1, alpha1, theta2, rho2, lambda2, alpha2
           #lines beta
           sec_deri[1:k, 1:k] = -1/sigs*t(x)%*%iaaw%*%x
           sec_deri[1:k, k+1] = -1/sigs^2*t(x)%*%iaaw%*%k_ast
           sec_deri[1:k, k+2] = -1/sigs*t(x)%*%iaaw%*%y1_q1
           sec_deri[1:k, k+3] = -1/sigs*t(x)%*%iaaw%*%bdw_q1%*%y
           sec_deri[1:k, k+4] = -1/sigs*t(x)%*%iaaw%*%y1_q2
           sec_deri[1:k, k+5] = -1/sigs*t(x)%*%iaaw%*%bdw_q2%*%y
           
           #line sigma2
           sec_deri[k+1, 1:k] = t(sec_deri[1:k, k+1])
           sec_deri[k+1, k+1] = -1/sigs^3*t(k_ast)%*%iaaw%*%k_ast + tp/(2*sigs^2)
           sec_deri[k+1, k+2] = -1/sigs^2*t(y1_q1)%*%iaaw%*%k_ast
           sec_deri[k+1, k+3] = -1/sigs^2*t(y)%*%t(bdw_q1)%*%iaaw%*%k_ast
           sec_deri[k+1, k+4] = -1/sigs^2*t(y1_q2)%*%iaaw%*%k_ast
           sec_deri[k+1, k+5 ] = -1/sigs^2*t(y)%*%t(bdw_q2)%*%iaaw%*%k_ast
           
           #line theta1
           sec_deri[k+2, 1:k] = t(sec_deri[1:k, k+2])
           sec_deri[k+2, k+1] = sec_deri[k+1, k+2]
           sec_deri[k+2, k+2] = -1/sigs*t(y1_q1)%*%iaaw%*%y1_q1 + sum(diag(bdinv_c%*%A_mats_theta1$A_deriv_1%*%iirw%*%bdw0_q1))
           sec_deri[k+2, k+3] = -1/sigs*t(y1_q1)%*%iaaw%*%bdw_q1%*%y + sum(diag(bdinv_c%*%(A_mats_rho1$A_deriv_1%*%iirw + A_mats$A_1%*%iirw%*%bdw_q1%*%iirw)%*%bdw0_q1))
           sec_deri[k+2, k+4] = -1/sigs*t(y1_q1)%*%iaaw%*%y1_q2 + sum(diag(bdinv_c%*%A_mats_theta2$A_deriv_1%*%iirw%*%bdw0_q1))
           sec_deri[k+2, k+5] = -1/sigs*t(y1_q1)%*%iaaw%*%bdw_q2%*%y + sum(diag(bdinv_c%*%(A_mats_rho2$A_deriv_1%*%iirw + A_mats$A_1%*%iirw%*%bdw_q2%*%iirw)%*%bdw0_q1))
           
           #line rho1
           sec_deri[k+3, 1:k] = t(sec_deri[1:k, k+3])
           sec_deri[k+3, k+1] = sec_deri[k+1, k+3]
           sec_deri[k+3, k+2] = -1/sigs*t(y)%*%t(bdw_q1)%*%iaaw%*%y1_q1 + sum(diag(bdinv_c%*%A_mats_theta1$A_deriv%*%iirw%*%bdw_q1))
           sec_deri[k+3, k+3] = -1/sigs*t(y)%*%t(bdw_q1)%*%iaaw%*%bdw_q1%*%y + sum(diag(bdinv_c%*%(A_mats_rho1$A_deriv%*%iirw + A_mats$A%*%iirw%*%bdw_q1%*%iirw)%*%bdw_q1))
           sec_deri[k+3, k+4] = -1/sigs*t(y)%*%t(bdw_q1)%*%iaaw%*%y1_q2 + sum(diag(bdinv_c%*%A_mats_theta2$A_deriv%*%iirw%*%bdw_q1))
           sec_deri[k+3, k+5] = -1/sigs*t(y)%*%t(bdw_q1)%*%iaaw%*%bdw_q2%*%y + sum(diag(bdinv_c%*%(A_mats_rho2$A_deriv%*%iirw + A_mats$A%*%iirw%*%bdw_q2%*%iirw)%*%bdw_q1))
           
           #line theta2
           sec_deri[k+4, 1:k] = t(sec_deri[1:k, k+4])
           sec_deri[k+4, k+1] = sec_deri[k+1, k+4]
           sec_deri[k+4, k+2] = -1/sigs*t(y1_q2)%*%iaaw%*%y1_q1 + sum(diag(bdinv_c%*%A_mats_theta1$A_deriv_1%*%iirw%*%bdw0_q2))
           sec_deri[k+4, k+3] = -1/sigs*t(y1_q2)%*%iaaw%*%bdw_q1%*%y + sum(diag(bdinv_c%*%(A_mats_rho1$A_deriv_1%*%iirw + A_mats$A_1%*%iirw%*%bdw_q1%*%iirw)%*%bdw0_q2))
           sec_deri[k+4, k+4] = -1/sigs*t(y1_q2)%*%iaaw%*%y1_q2 + sum(diag(bdinv_c%*%A_mats_theta2$A_deriv_1%*%iirw%*%bdw0_q2))
           sec_deri[k+4, k+5] = -1/sigs*t(y1_q2)%*%iaaw%*%bdw_q2%*%y + sum(diag(bdinv_c%*%(A_mats_rho2$A_deriv_1%*%iirw + A_mats$A_1%*%iirw%*%bdw_q2%*%iirw)%*%bdw0_q2))
           
           
           #line rho2
           sec_deri[k+5, 1:k] = t(sec_deri[1:k, k+4])
           sec_deri[k+5, k+1] = sec_deri[k+1, k+4]
           sec_deri[k+5, k+2] = -1/sigs*t(y)%*%t(bdw_q2)%*%iaaw%*%y1_q1 + sum(diag(bdinv_c%*%A_mats_theta1$A_deriv%*%iirw%*%bdw_q2))
           sec_deri[k+5, k+3] = -1/sigs*t(y)%*%t(bdw_q2)%*%iaaw%*%bdw_q1%*%y + sum(diag(bdinv_c%*%(A_mats_rho1$A_deriv%*%iirw + A_mats$A%*%iirw%*%bdw_q1%*%iirw)%*%bdw_q2))
           sec_deri[k+5, k+4] = -1/sigs*t(y)%*%t(bdw_q2)%*%iaaw%*%y1_q2 + sum(diag(bdinv_c%*%A_mats_theta2$A_deriv%*%iirw%*%bdw_q2))
           sec_deri[k+5, k+5] = -1/sigs*t(y)%*%t(bdw_q2)%*%iaaw%*%bdw_q2%*%y + sum(diag(bdinv_c%*%(A_mats_rho2$A_deriv%*%iirw + A_mats$A%*%iirw%*%bdw_q2%*%iirw)%*%bdw_q2))
           
           
           
           #make list of powers of c_mat
           list_pow_c_mat = c_mat_pows(p,t, iirws, dthetas, lws)
           #make R and G mats
           R_mats = make_R_mats(list_pow_c_mat)
           G_mats = make_G_mats(list_pow_c_mat)
           #make eta, S, pi, phi, psi and v_vec
           iciaw = kronecker(inv_c, iaws)
           eta = G_mats$G %*% iirw %*% x %*% beta
           eta_1 = G_mats$G_1 %*% iirw %*% x %*% beta
           S = G_mats$G %*% iirw %*% iiaw
           S_1 = G_mats$G_1 %*% iirw %*% iiaw
           eta_1_1 = eta_1_2 = eta_1
           eta_1_1[!th_e] = 0
           eta_1_2[th_e] = 0
           S_1_1 = S_1_2 = S_1
           S_1_1[!th_e, ] = 0
           S_1_2[th_e, ] = 0
           R_1_1 = R_1_2 = R_mats$R_1
           R_1_1[!th_e, ] = 0
           R_1_2[th_e, ] = 0
           
           #for beta
           pi_1 = 1/sigs*t(iciaw)%*%x
           #for q1
           pi_2 = 1/sigs*t(iciaw)%*%eta_1_1
           pi_3 = 1/sigs*t(iciaw)%*%bdw_q1%*%eta
           #for q2
           pi_5 = 1/sigs*t(iciaw)%*%eta_1_2
           pi_6 = 1/sigs*t(iciaw)%*%bdw_q2%*%eta
           
           #for sigma2
           phi_1 = 0.5/(sigs^2)*kronecker(inv_c, diag(p))
           #for q1
           phi_2 = 1/sigs*iciaw%*%S_1_1
           phi_3 = 1/sigs*iciaw%*%bdw_q1%*%S
           
           #for q2
           phi_6 = 1/sigs*iciaw%*%S_1_2
           phi_7 = 1/sigs*iciaw%*%bdw_q2%*%S
           
           
           #for q1
           psi_1 = 1/sigs*iciaw%*%R_1_1
           psi_2 = 1/sigs*iciaw%*%bdw_q1%*%R_mats$R
           
           #for q2
           psi_4 = 1/sigs*iciaw%*%R_1_2
           psi_5 = 1/sigs*iciaw%*%bdw_q2%*%R_mats$R
           
           
           v_vec = as.vector(iaw%*%k_ast)
           #make y0, y0_ast
           y0 = rep(y1[1:p], t)
           y0_ast = iaws %*% irws %*% y0[1:p]
           
           #make all gs
           g11 = make_g1i_all(p, t, pi_1, v_vec)
           g12 = make_g1i_all(p, t, pi_2, v_vec)
           g13 = make_g1i_all(p, t, pi_3, v_vec)
           g15 = make_g1i_all(p, t, pi_5, v_vec)
           g16 = make_g1i_all(p, t, pi_6, v_vec)
           g21 = make_g2i_all(p, t, phi_1, v_vec, sigs, inv_c)
           g22 = make_g2i_all(p, t, phi_2, v_vec, sigs, inv_c)
           g23 = make_g2i_all(p, t, phi_3, v_vec, sigs, inv_c)
           g26 = make_g2i_all(p, t, phi_6, v_vec, sigs, inv_c)
           g27 = make_g2i_all(p, t, phi_7, v_vec, sigs, inv_c)
           g31 = make_g3i_all(p, t, y0, y0_ast, psi_1, v_vec, iirws, iiaws, sigs)
           g32 = make_g3i_all(p, t, y0, y0_ast, psi_2, v_vec, iirws, iiaws, sigs)
           g34 = make_g3i_all(p, t, y0, y0_ast, psi_4, v_vec, iirws, iiaws, sigs)
           g35 = make_g3i_all(p, t, y0, y0_ast, psi_5, v_vec, iirws, iiaws, sigs)
           
           #make all g mat
           all_g = matrix(0, nrow = k + 5, ncol = p)
           all_g[1:k, ] = t(g11)
           all_g[k + 1, ] = g21
           all_g[k + 2, ] = g31 + g12 + g22
           all_g[k + 3, ] = g32 + g13 + g23
           all_g[k + 4, ] = g34 + g15 + g26
           all_g[k + 5, ] = g35 + g16 + g27
           
           
           #calculate VC matrix
           # inv_sig_mat = solve(-1/tp*sec_deri)
           # gamma_mat = 1/tp*all_g%*%t(all_g)
           
           inv_sig_mat = solve(-sec_deri)
           gamma_mat = all_g%*%t(all_g)
           
           if (all_er){
             return_list = list(vc_mat = inv_sig_mat%*%gamma_mat%*%inv_sig_mat,
                                hes_mat = inv_sig_mat,
                                gamma_mat = gamma_mat)
             return(return_list)
           } else {
             vc_mat = inv_sig_mat%*%gamma_mat%*%inv_sig_mat
             return(list(vc_mat = vc_mat))
           }
           
         },
         "residual" = {
           return(k_ast)
         },
         stop("undefined mode"))
}

th_sem_aqs = function(para, x_, y, y1, w_er, th, correction, mode = "normal", all_er = F, inv_c, th_type = "row"){
  x = x_
  tp = length(y)
  p = dim(w_er)[1]
  t = tp/p
  th_e = rep(th, t)
  w = matrix(0,p,p)
  bdw_er = kronecker(diag(t), w_er)
  alp1 = para[1]
  theta1 = para[2]
  alp2 = para[3]
  theta2 = para[4]
  eq = numeric(4)
  # mat_c = diag(2, t, t)
  # mat_c[(row(mat_c)==col(mat_c)+1)|(row(mat_c)==col(mat_c)-1)] = -1
  # inv_c = solve(mat_c)
  bdinv_c = kronecker(inv_c, diag(p))
  y_q1 = y_q2 = y
  y1_q1 = y1_q2 = y1
  bdw0_q1 = bdw0_q2 = diag(tp)
  bdw_er_q1 = bdw_er_q2 = bdw_er
  w_er_q1 = w_er_q2 = w_er
  y_q1[!th_e] = 0
  y_q2[th_e] = 0
  y1_q1[!th_e] = 0
  y1_q2[th_e] = 0
  switch(th_type,
         "row" = {
           bdw0_q1[!th_e,] = 0
           bdw0_q2[th_e,] = 0
           bdw_er_q1[!th_e,] = 0
           bdw_er_q2[th_e,] = 0
           w_er_q1[!th,] = 0
           w_er_q2[th,] = 0
           iaws = diag(p) - merge_th_diag(alp1, alp2, th) %*% w_er
         },
         "col" = {
           bdw0_q1[,!th_e] = 0
           bdw0_q2[,th_e] = 0
           bdw_er_q1[,!th_e] = 0
           bdw_er_q2[,th_e] = 0
           w_er_q1[,!th] = 0
           w_er_q2[,th] = 0
           iaws = diag(p) - w_er %*% merge_th_diag(alp1, alp2, th)
         },
         stop("invalid th_type"))
  w_er_q2[th,] = 0
  xt = t(x)
  iiaws = solve(iaws)
  iaw = kronecker(diag(t), iaws)
  iiaw = kronecker(diag(t), iiaws)
  iaaw = kronecker(inv_c, t(iaws) %*% iaws)
  irws = diag(p)
  iirws =  diag(p)
  irw = diag(tp)
  iirw = diag(tp)
  lws = 0
  lw =  0
  dthetas = merge_th_diag(theta1, theta2, th)
  dtheta = kronecker(diag(t), dthetas)
  beta = solve(xt %*% iaaw %*% x) %*% xt %*% iaaw %*% (irw %*% y - (dtheta + lw) %*% y1)
  k_ast = irw %*% y - x %*% beta - (dtheta + lw) %*% y1
  sigs = as.numeric(t(k_ast) %*% iaaw %*% k_ast/tp)
  k = length(beta)
  switch(mode,
         "normal" = {
           tmp_mat_1 = t(k_ast) %*% iaaw 
           if (correction){
             A_mats = make_A_df(p, t, dthetas, lws, iirws)
             # vec_bias_theta = diag(bdinv_c %*%A_mats$A_1 %*% iirw)
             # eq[1] = 1/sigs * tmp_mat_1%*% y1_q1 + sum(vec_bias_theta[th_e])
             eq[1] = 1/sigs * tmp_mat_1%*% y1_q1 + sum(diag(bdinv_c %*%A_mats$A_1 %*% iirw %*% bdw0_q1))
             eq[2] = 0.5/sigs * t(k_ast) %*% kronecker(inv_c, t(w_er_q1) %*% iaws + t(iaws) %*% w_er_q1) %*% k_ast - t * sum(diag(iiaws %*% w_er_q1))
             # eq[3] = 1/sigs * tmp_mat_1 %*% y1_q2 + sum(vec_bias_theta[!th_e])
             eq[3] = 1/sigs * tmp_mat_1 %*% y1_q2 + sum(diag(bdinv_c %*%A_mats$A_1 %*% iirw %*% bdw0_q2))
             eq[4] = 0.5/sigs * t(k_ast) %*% kronecker(inv_c, t(w_er_q2) %*% iaws + t(iaws) %*% w_er_q2) %*% k_ast - t * sum(diag(iiaws %*% w_er_q2))
           } else {
             eq[1] = 1/sigs * tmp_mat_1 %*% y1_q1
             eq[2] = 0.5/sigs * t(k_ast) %*% kronecker(inv_c, t(w_er_q1) %*% iaws + t(iaws) %*% w_er_q1) %*% k_ast - t * sum(diag(iiaws %*% w_er_q1))
             eq[3] = 1/sigs * tmp_mat_1 %*% y1_q2
             eq[4] = 0.5/sigs * t(k_ast) %*% kronecker(inv_c, t(w_er_q2) %*% iaws + t(iaws) %*% w_er_q2) %*% k_ast - t * sum(diag(iiaws %*% w_er_q2))
           }
           return(sum(eq^2))
         },
         "beta_sigs" = {
           return(list(beta = beta,
                       sigma2 = sigs))
         },
         "opmd" = {
           #calculate second derivative
           sec_deri = matrix(, nrow = k + 5, ncol = k + 5)
           #calculate derivative of A_mat
           A_mats = make_A_df(p, t, dthetas, lws, iirws)
           A_mats_theta1 = make_A_deriv_df_th(p, t, th, w, dthetas, lws, iirws, mode = "theta1")
           A_mats_theta2 = make_A_deriv_df_th(p, t, th, w, dthetas, lws, iirws, mode = "theta2")
           
           ##order: beta, sigma2, theta1, rho1, lambda1, alpha1, theta2, rho2, lambda2, alpha2
           #lines beta
           sec_deri[1:k, 1:k] = -1/sigs*t(x)%*%iaaw%*%x
           sec_deri[1:k, k+1] = -1/sigs^2*t(x)%*%iaaw%*%k_ast
           sec_deri[1:k, k+2] = -1/sigs*t(x)%*%iaaw%*%y1_q1
           sec_deri[1:k, k+3] = 1/sigs*t(x)%*%(-kronecker(inv_c, t(w_er_q1) %*% iaws + t(iaws) %*% w_er_q1))%*%k_ast
           sec_deri[1:k, k+4] = -1/sigs*t(x)%*%iaaw%*%y1_q2
           sec_deri[1:k, k+5] = 1/sigs*t(x)%*%(-kronecker(inv_c, t(w_er_q2) %*% iaws + t(iaws) %*% w_er_q2))%*%k_ast
           
           #line sigma2
           sec_deri[k+1, 1:k] = t(sec_deri[1:k, k+1])
           sec_deri[k+1, k+1] = -1/sigs^3*t(k_ast)%*%iaaw%*%k_ast + tp/(2*sigs^2)
           sec_deri[k+1, k+2] = -1/sigs^2*t(y1_q1)%*%iaaw%*%k_ast
           sec_deri[k+1, k+3] = 1/(2*sigs^2)*t(k_ast)%*%(-kronecker(inv_c, t(w_er_q1) %*% iaws + t(iaws) %*% w_er_q1))%*%k_ast
           sec_deri[k+1, k+4] = -1/sigs^2*t(y1_q2)%*%iaaw%*%k_ast
           sec_deri[k+1, k+5] = 1/(2*sigs^2)*t(k_ast)%*%(-kronecker(inv_c, t(w_er_q2) %*% iaws + t(iaws) %*% w_er_q2))%*%k_ast
           
           #line theta1
           sec_deri[k+2, 1:k] = t(sec_deri[1:k, k+2])
           sec_deri[k+2, k+1] = sec_deri[k+1, k+2]
           sec_deri[k+2, k+2] = -1/sigs*t(y1_q1)%*%iaaw%*%y1_q1 + sum(diag(bdinv_c%*%A_mats_theta1$A_deriv_1%*%iirw%*%bdw0_q1))
           sec_deri[k+2, k+3] = 1/sigs*t(y1_q1)%*%(-kronecker(inv_c, t(w_er_q1) %*% iaws + t(iaws) %*% w_er_q1))%*%k_ast
           sec_deri[k+2, k+4] = -1/sigs*t(y1_q1)%*%iaaw%*%y1_q2 + sum(diag(bdinv_c%*%A_mats_theta2$A_deriv_1%*%iirw%*%bdw0_q1))
           sec_deri[k+2, k+5] = 1/sigs*t(y1_q1)%*%(-kronecker(inv_c, t(w_er_q2) %*% iaws + t(iaws) %*% w_er_q2))%*%k_ast
           
           #line alpha1
           sec_deri[k+3, 1:k] = t(sec_deri[1:k, k+3])
           sec_deri[k+3, k+1] = sec_deri[k+1, k+3]
           sec_deri[k+3, k+2] = sec_deri[k+2, k+3]
           sec_deri[k+3, k+3] = -0.5/sigs*t(k_ast)%*%(kronecker(inv_c, t(w_er_q1) %*% w_er_q1 + t(w_er_q1) %*% w_er_q1))%*%k_ast - sum(diag(bdw_er_q1%*%iiaw%*%bdw_er_q1%*%iiaw))
           sec_deri[k+3, k+4] = 1/sigs*t(y1_q2)%*%(-kronecker(inv_c, t(w_er_q1) %*% iaws + t(iaws) %*% w_er_q1))%*%k_ast
           sec_deri[k+3, k+5] = -0.5/sigs*t(k_ast)%*%(kronecker(inv_c, t(w_er_q1) %*% w_er_q2 + t(w_er_q2) %*% w_er_q1))%*%k_ast - sum(diag(bdw_er_q1%*%iiaw%*%bdw_er_q2%*%iiaw))
           
           #line theta2
           sec_deri[k+4, 1:k] = t(sec_deri[1:k, k+4])
           sec_deri[k+4, k+1] = sec_deri[k+1, k+4]
           sec_deri[k+4, k+2] = -1/sigs*t(y1_q2)%*%iaaw%*%y1_q1 + sum(diag(bdinv_c%*%A_mats_theta1$A_deriv_1%*%iirw%*%bdw0_q2))
           sec_deri[k+4, k+3] = 1/sigs*t(y1_q2)%*%(-kronecker(inv_c, t(w_er_q1) %*% iaws + t(iaws) %*% w_er_q1))%*%k_ast
           sec_deri[k+4, k+4] = -1/sigs*t(y1_q2)%*%iaaw%*%y1_q2 + sum(diag(bdinv_c%*%A_mats_theta2$A_deriv_1%*%iirw%*%bdw0_q2))
           sec_deri[k+4, k+5] = 1/sigs*t(y1_q2)%*%(-kronecker(inv_c, t(w_er_q2) %*% iaws + t(iaws) %*% w_er_q2))%*%k_ast
           
           
           #line alpha2
           sec_deri[k+5, 1:k] = t(sec_deri[1:k, k+5])
           sec_deri[k+5, k+1] = sec_deri[k+1, k+5]
           sec_deri[k+5, k+2] = sec_deri[k+2, k+5]
           sec_deri[k+5, k+3] = -0.5/sigs*t(k_ast)%*%(kronecker(inv_c, t(w_er_q2) %*% w_er_q1 + t(w_er_q1) %*% w_er_q2))%*%k_ast - sum(diag(bdw_er_q2%*%iiaw%*%bdw_er_q1%*%iiaw))
           sec_deri[k+5, k+4] = sec_deri[k+4, k+5]
           sec_deri[k+5, k+5] = -0.5/sigs*t(k_ast)%*%(kronecker(inv_c, t(w_er_q2) %*% w_er_q2 + t(w_er_q2) %*% w_er_q2))%*%k_ast - sum(diag(bdw_er_q2%*%iiaw%*%bdw_er_q2%*%iiaw))
           
           #make list of powers of c_mat
           list_pow_c_mat = c_mat_pows(p,t, iirws, dthetas, lws)
           #make R and G mats
           R_mats = make_R_mats(list_pow_c_mat)
           G_mats = make_G_mats(list_pow_c_mat)
           #make eta, S, pi, phi, psi and v_vec
           iciaw = kronecker(inv_c, iaws)
           eta = G_mats$G %*% iirw %*% x %*% beta
           eta_1 = G_mats$G_1 %*% iirw %*% x %*% beta
           S = G_mats$G %*% iirw %*% iiaw
           S_1 = G_mats$G_1 %*% iirw %*% iiaw
           eta_1_1 = eta_1_2 = eta_1
           eta_1_1[!th_e] = 0
           eta_1_2[th_e] = 0
           S_1_1 = S_1_2 = S_1
           S_1_1[!th_e, ] = 0
           S_1_2[th_e, ] = 0
           R_1_1 = R_1_2 = R_mats$R_1
           R_1_1[!th_e, ] = 0
           R_1_2[th_e, ] = 0
           
           #for beta
           pi_1 = 1/sigs*t(iciaw)%*%x
           #for q1
           pi_2 = 1/sigs*t(iciaw)%*%eta_1_1
           pi_3 = 1/sigs*t(iciaw)%*%bdw_er_q1%*%eta
           pi_4 = 1/sigs*t(iciaw)%*%bdw_er_q1%*%eta_1
           #for q2
           pi_5 = 1/sigs*t(iciaw)%*%eta_1_2
           pi_6 = 1/sigs*t(iciaw)%*%bdw_er_q2%*%eta
           pi_7 = 1/sigs*t(iciaw)%*%bdw_er_q2%*%eta_1
           
           #for sigma2
           phi_1 = 0.5/(sigs^2)*kronecker(inv_c, diag(p))
           #for q1
           phi_2 = 1/sigs*iciaw%*%S_1_1
           phi_5 = 0.5/sigs*kronecker(inv_c, t(w_er_q1%*%iiaws) + w_er_q1%*%iiaws)
           #for q2
           phi_6 = 1/sigs*iciaw%*%S_1_2
           phi_9 = 0.5/sigs*kronecker(inv_c, t(w_er_q2%*%iiaws) + w_er_q2%*%iiaws)
           
           #for q1
           psi_1 = 1/sigs*iciaw%*%R_1_1
           #for q2
           psi_4 = 1/sigs*iciaw%*%R_1_2
           
           v_vec = as.vector(iaw%*%k_ast)
           #make y0, y0_ast
           y0 = rep(y1[1:p], t)
           y0_ast = iaws %*% irws %*% y0[1:p]
           
           #make all gs
           g11 = make_g1i_all(p, t, pi_1, v_vec)
           g12 = make_g1i_all(p, t, pi_2, v_vec)
           g15 = make_g1i_all(p, t, pi_5, v_vec)
           g21 = make_g2i_all(p, t, phi_1, v_vec, sigs, inv_c)
           g22 = make_g2i_all(p, t, phi_2, v_vec, sigs, inv_c)
           g25 = make_g2i_all(p, t, phi_5, v_vec, sigs, inv_c)
           g26 = make_g2i_all(p, t, phi_6, v_vec, sigs, inv_c)
           g29 = make_g2i_all(p, t, phi_9, v_vec, sigs, inv_c)
           g31 = make_g3i_all(p, t, y0, y0_ast, psi_1, v_vec, iirws, iiaws, sigs)
           g34 = make_g3i_all(p, t, y0, y0_ast, psi_4, v_vec, iirws, iiaws, sigs)
           
           #make all g mat
           all_g = matrix(0, nrow = k + 5, ncol = p)
           all_g[1:k, ] = t(g11)
           all_g[k + 1, ] = g21
           all_g[k + 2, ] = g31 + g12 + g22
           all_g[k + 3, ] = g25
           all_g[k + 4, ] = g34 + g15 + g26
           all_g[k + 5, ] = g29
           
           
           inv_sig_mat = solve(-sec_deri)
           gamma_mat = all_g%*%t(all_g)
           
           if (all_er){
             return_list = list(vc_mat = inv_sig_mat%*%gamma_mat%*%inv_sig_mat,
                                hes_mat = inv_sig_mat,
                                gamma_mat = gamma_mat)
             return(return_list)
           } else {
             vc_mat = inv_sig_mat%*%gamma_mat%*%inv_sig_mat
             return(list(vc_mat = vc_mat))
           }
           
         },
         "residual" = {
           return(as.vector(iaw%*%k_ast))
         },
         stop("undefined mode"))
}

th_sltl_aqs = function(para, x_, y, y1, w, w_lam, th, correction, mode = "normal", all_er = F, inv_c, th_type = "row"){
  x = x_
  tp = length(y)
  p = dim(w)[1]
  t = tp/p
  th_e = rep(th, t)
  bdw = kronecker(diag(t), w)
  bdw_lam = kronecker(diag(t), w_lam)
  rho1 = para[1]
  theta1 = para[2]
  lam1 = para[3]
  rho2 = para[4]
  theta2 = para[5]
  lam2 = para[6]
  eq = numeric(6)
  # mat_c = diag(2, t, t)
  # mat_c[(row(mat_c)==col(mat_c)+1)|(row(mat_c)==col(mat_c)-1)] = -1
  # inv_c = solve(mat_c)
  bdinv_c = kronecker(inv_c, diag(p))
  y_q1 = y_q2 = y
  y1_q1 = y1_q2 = y1
  bdw0_q1 = bdw0_q2 = diag(tp)
  bdw_q1 = bdw_q2 = bdw
  bdw_lam_q1 = bdw_lam_q2 = bdw_lam
  w_q1 = w_q2 = w
  w_lam_q1 = w_lam_q2 = w_lam
  y_q1[!th_e] = 0
  y_q2[th_e] = 0
  y1_q1[!th_e] = 0
  y1_q2[th_e] = 0
  switch(th_type,
         "row" = {
           bdw0_q1[!th_e,] = 0
           bdw0_q2[th_e,] = 0
           bdw_q1[!th_e,] = 0
           bdw_q2[th_e,] = 0
           bdw_lam_q1[!th_e,] = 0
           bdw_lam_q2[th_e,] = 0
           w_q1[!th,] = 0
           w_q2[th,] = 0
           w_lam_q1[!th,] = 0
           w_lam_q2[th,] = 0
           irws = diag(p) - merge_th_diag(rho1, rho2, th) %*% w
           lws = merge_th_diag(lam1, lam2, th) %*% w_lam
         },
         "col" = {
           bdw0_q1[,!th_e] = 0
           bdw0_q2[,th_e] = 0
           bdw_q1[,!th_e] = 0
           bdw_q2[,th_e] = 0
           bdw_lam_q1[,!th_e] = 0
           bdw_lam_q2[,th_e] = 0
           w_q1[,!th] = 0
           w_q2[,th] = 0
           w_lam_q1[,!th] = 0
           w_lam_q2[,th] = 0
           irws = diag(p) - w %*% merge_th_diag(rho1, rho2, th)
           lws = w_lam %*% merge_th_diag(lam1, lam2, th)
         },
         stop("invalid th_type"))
  xt = t(x)
  iaws = diag(p)
  iiaws = diag(p)
  iaw = diag(tp)
  iiaw = diag(tp)
  iaaw = bdinv_c
  irws = diag(p) - merge_th_diag(rho1, rho2, th) %*% w
  iirws = solve(irws)
  irw = kronecker(diag(t), irws)
  iirw = kronecker(diag(t), iirws)
  lws = merge_th_diag(lam1, lam2, th) %*% w_lam
  lw =  kronecker(diag(t), lws)
  dthetas = merge_th_diag(theta1, theta2, th)
  dtheta = kronecker(diag(t), dthetas)
  beta = solve(xt %*% iaaw %*% x) %*% xt %*% iaaw %*% (irw %*% y - (dtheta + lw) %*% y1)
  k_ast = irw %*% y - x %*% beta - (dtheta + lw) %*% y1
  sigs = as.numeric(t(k_ast) %*% iaaw %*% k_ast/tp)
  k = length(beta)
  switch(mode,
         "normal" = {
           tmp_mat_1 = t(k_ast) %*% iaaw 
           if (correction){
             A_mats = make_A_df(p, t, dthetas, lws, iirws)
             # vec_bias_theta = diag(bdinv_c %*%A_mats$A_1 %*% iirw)
             # vec_bias_rho = diag(bdinv_c %*%A_mats$A %*% iirw %*% bdw)
             # vec_bias_lam = diag(bdinv_c %*%A_mats$A_1 %*% iirw %*% bdw_lam)
             # eq[1] = 1/sigs * tmp_mat_1%*% y1_q1 + sum(vec_bias_theta[th_e])
             # eq[2] = 1/sigs * tmp_mat_1 %*% bdw_q1 %*% y + sum(vec_bias_rho[th_e])
             # eq[3] = 1/sigs * tmp_mat_1 %*% bdw_lam_q1 %*% y1 + sum(vec_bias_lam[th_e])
             # eq[4] = 1/sigs * tmp_mat_1 %*% y1_q2 + sum(vec_bias_theta[!th_e])
             # eq[5] = 1/sigs * tmp_mat_1 %*% bdw_q2 %*% y + sum(vec_bias_rho[!th_e])
             # eq[6] = 1/sigs * tmp_mat_1 %*% bdw_lam_q2 %*% y1 + sum(vec_bias_lam[!th_e])
             eq[1] = 1/sigs * tmp_mat_1%*% y1_q1 + sum(diag(bdinv_c %*%A_mats$A_1 %*% iirw %*% bdw0_q1))
             eq[2] = 1/sigs * tmp_mat_1 %*% bdw_q1 %*% y + sum(diag(bdinv_c %*%A_mats$A %*% iirw %*% bdw_q1))
             eq[3] = 1/sigs * tmp_mat_1 %*% bdw_lam_q1 %*% y1 + sum(diag(bdinv_c %*%A_mats$A_1 %*% iirw %*% bdw_lam_q1))
             eq[4] = 1/sigs * tmp_mat_1 %*% y1_q2 + sum(diag(bdinv_c %*%A_mats$A_1 %*% iirw %*% bdw0_q2))
             eq[5] = 1/sigs * tmp_mat_1 %*% bdw_q2 %*% y + sum(diag(bdinv_c %*%A_mats$A %*% iirw %*% bdw_q2))
             eq[6] = 1/sigs * tmp_mat_1 %*% bdw_lam_q2 %*% y1 + sum(diag(bdinv_c %*%A_mats$A_1 %*% iirw %*% bdw_lam_q2))
           } else {
             eq[1] = 1/sigs * tmp_mat_1 %*% y1_q1
             eq[2] = 1/sigs * tmp_mat_1 %*% bdw_q1 %*% y - t * sum(diag(iirws %*% w_q1))
             eq[3] = 1/sigs * tmp_mat_1 %*% bdw_lam_q1 %*% y1
             eq[4] = 1/sigs * tmp_mat_1 %*% y1_q2
             eq[5] = 1/sigs * tmp_mat_1 %*% bdw_q2 %*% y - t * sum(diag(iirws %*% w_q2))
             eq[6] = 1/sigs * tmp_mat_1 %*% bdw_lam_q2 %*% y1
           }
           return(sum(eq^2))
         },
         "beta_sigs" = {
           return(list(beta = beta,
                       sigma2 = sigs))
         },
         "opmd" = {
           #calculate second derivative
           sec_deri = matrix(, nrow = k + 7, ncol = k + 7)
           #calculate derivative of A_mat
           A_mats = make_A_df(p, t, dthetas, lws, iirws)
           A_mats_rho1 = make_A_deriv_df_th(p, t, th, w, dthetas, lws, iirws, mode = "rho1")
           A_mats_theta1 = make_A_deriv_df_th(p, t, th, w, dthetas, lws, iirws, mode = "theta1")
           A_mats_lam1 = make_A_deriv_df_th(p, t, th, w, dthetas, lws, iirws, mode = "lambda1")
           A_mats_rho2 = make_A_deriv_df_th(p, t, th, w, dthetas, lws, iirws, mode = "rho2")
           A_mats_theta2 = make_A_deriv_df_th(p, t, th, w, dthetas, lws, iirws, mode = "theta2")
           A_mats_lam2 = make_A_deriv_df_th(p, t, th, w, dthetas, lws, iirws, mode = "lambda2")
           
           ##order: beta, sigma2, theta1, rho1, lambda1, alpha1, theta2, rho2, lambda2, alpha2
           #lines beta
           sec_deri[1:k, 1:k] = -1/sigs*t(x)%*%iaaw%*%x
           sec_deri[1:k, k+1] = -1/sigs^2*t(x)%*%iaaw%*%k_ast
           sec_deri[1:k, k+2] = -1/sigs*t(x)%*%iaaw%*%y1_q1
           sec_deri[1:k, k+3] = -1/sigs*t(x)%*%iaaw%*%bdw_q1%*%y
           sec_deri[1:k, k+4] = -1/sigs*t(x)%*%iaaw%*%bdw_lam_q1%*%y1
           sec_deri[1:k, k+5] = -1/sigs*t(x)%*%iaaw%*%y1_q2
           sec_deri[1:k, k+6] = -1/sigs*t(x)%*%iaaw%*%bdw_q2%*%y
           sec_deri[1:k, k+7] = -1/sigs*t(x)%*%iaaw%*%bdw_lam_q2%*%y1
           
           #line sigma2
           sec_deri[k+1, 1:k] = t(sec_deri[1:k, k+1])
           sec_deri[k+1, k+1] = -1/sigs^3*t(k_ast)%*%iaaw%*%k_ast + tp/(2*sigs^2)
           sec_deri[k+1, k+2] = -1/sigs^2*t(y1_q1)%*%iaaw%*%k_ast
           sec_deri[k+1, k+3] = -1/sigs^2*t(y)%*%t(bdw_q1)%*%iaaw%*%k_ast
           sec_deri[k+1, k+4] = -1/sigs^2*t(y1)%*%t(bdw_lam_q1)%*%iaaw%*%k_ast
           sec_deri[k+1, k+5] = -1/sigs^2*t(y1_q2)%*%iaaw%*%k_ast
           sec_deri[k+1, k+6] = -1/sigs^2*t(y)%*%t(bdw_q2)%*%iaaw%*%k_ast
           sec_deri[k+1, k+7] = -1/sigs^2*t(y1)%*%t(bdw_lam_q2)%*%iaaw%*%k_ast
           
           #line theta1
           sec_deri[k+2, 1:k] = t(sec_deri[1:k, k+2])
           sec_deri[k+2, k+1] = sec_deri[k+1, k+2]
           sec_deri[k+2, k+2] = -1/sigs*t(y1_q1)%*%iaaw%*%y1_q1 + sum(diag(bdinv_c%*%A_mats_theta1$A_deriv_1%*%iirw%*%bdw0_q1))
           sec_deri[k+2, k+3] = -1/sigs*t(y1_q1)%*%iaaw%*%bdw_q1%*%y + sum(diag(bdinv_c%*%(A_mats_rho1$A_deriv_1%*%iirw + A_mats$A_1%*%iirw%*%bdw_q1%*%iirw)%*%bdw0_q1))
           sec_deri[k+2, k+4] = -1/sigs*t(y1_q1)%*%iaaw%*%bdw_lam_q1%*%y1 + sum(diag(bdinv_c%*%A_mats_lam1$A_deriv_1%*%iirw%*%bdw0_q1))
           sec_deri[k+2, k+5] = -1/sigs*t(y1_q1)%*%iaaw%*%y1_q2 + sum(diag(bdinv_c%*%A_mats_theta2$A_deriv_1%*%iirw%*%bdw0_q1))
           sec_deri[k+2, k+6] = -1/sigs*t(y1_q1)%*%iaaw%*%bdw_q2%*%y + sum(diag(bdinv_c%*%(A_mats_rho2$A_deriv_1%*%iirw + A_mats$A_1%*%iirw%*%bdw_q2%*%iirw)%*%bdw0_q1))
           sec_deri[k+2, k+7] = -1/sigs*t(y1_q1)%*%iaaw%*%bdw_lam_q2%*%y1 + sum(diag(bdinv_c%*%A_mats_lam2$A_deriv_1%*%iirw%*%bdw0_q1))
           
           
           #line rho1
           sec_deri[k+3, 1:k] = t(sec_deri[1:k, k+3])
           sec_deri[k+3, k+1] = sec_deri[k+1, k+3]
           sec_deri[k+3, k+2] = -1/sigs*t(y)%*%t(bdw_q1)%*%iaaw%*%y1_q1 + sum(diag(bdinv_c%*%A_mats_theta1$A_deriv%*%iirw%*%bdw_q1))
           sec_deri[k+3, k+3] = -1/sigs*t(y)%*%t(bdw_q1)%*%iaaw%*%bdw_q1%*%y + sum(diag(bdinv_c%*%(A_mats_rho1$A_deriv%*%iirw + A_mats$A%*%iirw%*%bdw_q1%*%iirw)%*%bdw_q1))
           sec_deri[k+3, k+4] = -1/sigs*t(y)%*%t(bdw_q1)%*%iaaw%*%bdw_lam_q1%*%y1 + sum(diag(bdinv_c%*%A_mats_lam1$A_deriv%*%iirw%*%bdw_q1))
           sec_deri[k+3, k+5] = -1/sigs*t(y)%*%t(bdw_q1)%*%iaaw%*%y1_q2 + sum(diag(bdinv_c%*%A_mats_theta2$A_deriv%*%iirw%*%bdw_q1))
           sec_deri[k+3, k+6] = -1/sigs*t(y)%*%t(bdw_q1)%*%iaaw%*%bdw_q2%*%y + sum(diag(bdinv_c%*%(A_mats_rho2$A_deriv%*%iirw + A_mats$A%*%iirw%*%bdw_q2%*%iirw)%*%bdw_q1))
           sec_deri[k+3, k+7] = -1/sigs*t(y)%*%t(bdw_q1)%*%iaaw%*%bdw_lam_q2%*%y1 + sum(diag(bdinv_c%*%A_mats_lam2$A_deriv%*%iirw%*%bdw_q1))
           
           #line lambda1
           sec_deri[k+4, 1:k] = t(sec_deri[1:k, k+4])
           sec_deri[k+4, k+1] = sec_deri[k+1, k+4]
           sec_deri[k+4, k+2] = -1/sigs*t(y1)%*%t(bdw_lam_q1)%*%iaaw%*%y1_q1 + sum(diag(bdinv_c%*%A_mats_theta1$A_deriv_1%*%iirw%*%bdw_lam_q1))
           sec_deri[k+4, k+3] = -1/sigs*t(y1)%*%t(bdw_lam_q1)%*%iaaw%*%bdw_q1%*%y + sum(diag(bdinv_c%*%(A_mats_rho1$A_deriv_1%*%iirw + A_mats$A_1%*%iirw%*%bdw_q1%*%iirw)%*%bdw_lam_q1))
           sec_deri[k+4, k+4] = -1/sigs*t(y1)%*%t(bdw_lam_q1)%*%iaaw%*%bdw_lam_q1%*%y1 + sum(diag(bdinv_c%*%A_mats_lam1$A_deriv_1%*%iirw%*%bdw_lam_q1))
           sec_deri[k+4, k+5] = -1/sigs*t(y1)%*%t(bdw_lam_q1)%*%iaaw%*%y1_q2 + sum(diag(bdinv_c%*%A_mats_theta2$A_deriv_1%*%iirw%*%bdw_lam_q1))
           sec_deri[k+4, k+6] = -1/sigs*t(y1)%*%t(bdw_lam_q1)%*%iaaw%*%bdw_q2%*%y + sum(diag(bdinv_c%*%(A_mats_rho2$A_deriv_1%*%iirw + A_mats$A_1%*%iirw%*%bdw_q2%*%iirw)%*%bdw_lam_q1))
           sec_deri[k+4, k+7] = -1/sigs*t(y1)%*%t(bdw_lam_q1)%*%iaaw%*%bdw_lam_q2%*%y1 + sum(diag(bdinv_c%*%A_mats_lam2$A_deriv_1%*%iirw%*%bdw_lam_q1))
           
           #line theta2
           sec_deri[k+5, 1:k] = t(sec_deri[1:k, k+5])
           sec_deri[k+5, k+1] = sec_deri[k+1, k+5]
           sec_deri[k+5, k+2] = -1/sigs*t(y1_q2)%*%iaaw%*%y1_q1 + sum(diag(bdinv_c%*%A_mats_theta1$A_deriv_1%*%iirw%*%bdw0_q2))
           sec_deri[k+5, k+3] = -1/sigs*t(y1_q2)%*%iaaw%*%bdw_q1%*%y + sum(diag(bdinv_c%*%(A_mats_rho1$A_deriv_1%*%iirw + A_mats$A_1%*%iirw%*%bdw_q1%*%iirw)%*%bdw0_q2))
           sec_deri[k+5, k+4] = -1/sigs*t(y1_q2)%*%iaaw%*%bdw_lam_q1%*%y1 + sum(diag(bdinv_c%*%A_mats_lam1$A_deriv_1%*%iirw%*%bdw0_q2))
           sec_deri[k+5, k+5] = -1/sigs*t(y1_q2)%*%iaaw%*%y1_q2 + sum(diag(bdinv_c%*%A_mats_theta2$A_deriv_1%*%iirw%*%bdw0_q2))
           sec_deri[k+5, k+6] = -1/sigs*t(y1_q2)%*%iaaw%*%bdw_q2%*%y + sum(diag(bdinv_c%*%(A_mats_rho2$A_deriv_1%*%iirw + A_mats$A_1%*%iirw%*%bdw_q2%*%iirw)%*%bdw0_q2))
           sec_deri[k+5, k+7] = -1/sigs*t(y1_q2)%*%iaaw%*%bdw_lam_q2%*%y1 + sum(diag(bdinv_c%*%A_mats_lam2$A_deriv_1%*%iirw%*%bdw0_q2))
           
           #line rho2
           sec_deri[k+6, 1:k] = t(sec_deri[1:k, k+6])
           sec_deri[k+6, k+1] = sec_deri[k+1, k+6]
           sec_deri[k+6, k+2] = -1/sigs*t(y)%*%t(bdw_q2)%*%iaaw%*%y1_q1 + sum(diag(bdinv_c%*%A_mats_theta1$A_deriv%*%iirw%*%bdw_q2))
           sec_deri[k+6, k+3] = -1/sigs*t(y)%*%t(bdw_q2)%*%iaaw%*%bdw_q1%*%y + sum(diag(bdinv_c%*%(A_mats_rho1$A_deriv%*%iirw + A_mats$A%*%iirw%*%bdw_q1%*%iirw)%*%bdw_q2))
           sec_deri[k+6, k+4] = -1/sigs*t(y)%*%t(bdw_q2)%*%iaaw%*%bdw_lam_q1%*%y1 + sum(diag(bdinv_c%*%A_mats_lam1$A_deriv%*%iirw%*%bdw_q2))
           sec_deri[k+6, k+5] = -1/sigs*t(y)%*%t(bdw_q2)%*%iaaw%*%y1_q2 + sum(diag(bdinv_c%*%A_mats_theta2$A_deriv%*%iirw%*%bdw_q2))
           sec_deri[k+6, k+6] = -1/sigs*t(y)%*%t(bdw_q2)%*%iaaw%*%bdw_q2%*%y + sum(diag(bdinv_c%*%(A_mats_rho2$A_deriv%*%iirw + A_mats$A%*%iirw%*%bdw_q2%*%iirw)%*%bdw_q2))
           sec_deri[k+6, k+7] = -1/sigs*t(y)%*%t(bdw_q2)%*%iaaw%*%bdw_lam_q2%*%y1 + sum(diag(bdinv_c%*%A_mats_lam2$A_deriv%*%iirw%*%bdw_q2))
           
           #line lambda2
           sec_deri[k+7, 1:k] = t(sec_deri[1:k, k+7])
           sec_deri[k+7, k+1] = sec_deri[k+1, k+7]
           sec_deri[k+7, k+2] = -1/sigs*t(y1)%*%t(bdw_lam_q2)%*%iaaw%*%y1_q1 + sum(diag(bdinv_c%*%A_mats_theta1$A_deriv_1%*%iirw%*%bdw_lam_q2))
           sec_deri[k+7, k+3] = -1/sigs*t(y1)%*%t(bdw_lam_q2)%*%iaaw%*%bdw_q1%*%y + sum(diag(bdinv_c%*%(A_mats_rho1$A_deriv_1%*%iirw + A_mats$A_1%*%iirw%*%bdw_q1%*%iirw)%*%bdw_lam_q2))
           sec_deri[k+7, k+4] = -1/sigs*t(y1)%*%t(bdw_lam_q2)%*%iaaw%*%bdw_lam_q1%*%y1 + sum(diag(bdinv_c%*%A_mats_lam1$A_deriv_1%*%iirw%*%bdw_lam_q2))
           sec_deri[k+7, k+5] = -1/sigs*t(y1)%*%t(bdw_lam_q2)%*%iaaw%*%y1_q2 + sum(diag(bdinv_c%*%A_mats_theta2$A_deriv_1%*%iirw%*%bdw_lam_q2))
           sec_deri[k+7, k+6] = -1/sigs*t(y1)%*%t(bdw_lam_q2)%*%iaaw%*%bdw_q2%*%y + sum(diag(bdinv_c%*%(A_mats_rho2$A_deriv_1%*%iirw + A_mats$A_1%*%iirw%*%bdw_q2%*%iirw)%*%bdw_lam_q2))
           sec_deri[k+7, k+7] = -1/sigs*t(y1)%*%t(bdw_lam_q2)%*%iaaw%*%bdw_lam_q2%*%y1 + sum(diag(bdinv_c%*%A_mats_lam2$A_deriv_1%*%iirw%*%bdw_lam_q2))
           
           #make list of powers of c_mat
           list_pow_c_mat = c_mat_pows(p,t, iirws, dthetas, lws)
           #make R and G mats
           R_mats = make_R_mats(list_pow_c_mat)
           G_mats = make_G_mats(list_pow_c_mat)
           #make eta, S, pi, phi, psi and v_vec
           iciaw = kronecker(inv_c, iaws)
           eta = G_mats$G %*% iirw %*% x %*% beta
           eta_1 = G_mats$G_1 %*% iirw %*% x %*% beta
           S = G_mats$G %*% iirw %*% iiaw
           S_1 = G_mats$G_1 %*% iirw %*% iiaw
           eta_1_1 = eta_1_2 = eta_1
           eta_1_1[!th_e] = 0
           eta_1_2[th_e] = 0
           S_1_1 = S_1_2 = S_1
           S_1_1[!th_e, ] = 0
           S_1_2[th_e, ] = 0
           R_1_1 = R_1_2 = R_mats$R_1
           R_1_1[!th_e, ] = 0
           R_1_2[th_e, ] = 0
           
           #for beta
           pi_1 = 1/sigs*t(iciaw)%*%x
           #for q1
           pi_2 = 1/sigs*t(iciaw)%*%eta_1_1
           pi_3 = 1/sigs*t(iciaw)%*%bdw_q1%*%eta
           pi_4 = 1/sigs*t(iciaw)%*%bdw_lam_q1%*%eta_1
           #for q2
           pi_5 = 1/sigs*t(iciaw)%*%eta_1_2
           pi_6 = 1/sigs*t(iciaw)%*%bdw_q2%*%eta
           pi_7 = 1/sigs*t(iciaw)%*%bdw_lam_q2%*%eta_1
           
           #for sigma2
           phi_1 = 0.5/(sigs^2)*kronecker(inv_c, diag(p))
           #for q1
           phi_2 = 1/sigs*iciaw%*%S_1_1
           phi_3 = 1/sigs*iciaw%*%bdw_q1%*%S
           phi_4 = 1/sigs*iciaw%*%bdw_lam_q1%*%S_1
           #for q2
           phi_6 = 1/sigs*iciaw%*%S_1_2
           phi_7 = 1/sigs*iciaw%*%bdw_q2%*%S
           phi_8 = 1/sigs*iciaw%*%bdw_lam_q2%*%S_1
           
           #for q1
           psi_1 = 1/sigs*iciaw%*%R_1_1
           psi_2 = 1/sigs*iciaw%*%bdw_q1%*%R_mats$R
           psi_3 = 1/sigs*iciaw%*%bdw_lam_q1%*%R_mats$R_1
           #for q2
           psi_4 = 1/sigs*iciaw%*%R_1_2
           psi_5 = 1/sigs*iciaw%*%bdw_q2%*%R_mats$R
           psi_6 = 1/sigs*iciaw%*%bdw_lam_q2%*%R_mats$R_1
           
           v_vec = as.vector(iaw%*%k_ast)
           #make y0, y0_ast
           y0 = rep(y1[1:p], t)
           y0_ast = iaws %*% irws %*% y0[1:p]
           
           #make all gs
           g11 = make_g1i_all(p, t, pi_1, v_vec)
           g12 = make_g1i_all(p, t, pi_2, v_vec)
           g13 = make_g1i_all(p, t, pi_3, v_vec)
           g14 = make_g1i_all(p, t, pi_4, v_vec)
           g15 = make_g1i_all(p, t, pi_5, v_vec)
           g16 = make_g1i_all(p, t, pi_6, v_vec)
           g17 = make_g1i_all(p, t, pi_7, v_vec)
           g21 = make_g2i_all(p, t, phi_1, v_vec, sigs, inv_c)
           g22 = make_g2i_all(p, t, phi_2, v_vec, sigs, inv_c)
           g23 = make_g2i_all(p, t, phi_3, v_vec, sigs, inv_c)
           g24 = make_g2i_all(p, t, phi_4, v_vec, sigs, inv_c)
           g26 = make_g2i_all(p, t, phi_6, v_vec, sigs, inv_c)
           g27 = make_g2i_all(p, t, phi_7, v_vec, sigs, inv_c)
           g28 = make_g2i_all(p, t, phi_8, v_vec, sigs, inv_c)
           g31 = make_g3i_all(p, t, y0, y0_ast, psi_1, v_vec, iirws, iiaws, sigs)
           g32 = make_g3i_all(p, t, y0, y0_ast, psi_2, v_vec, iirws, iiaws, sigs)
           g33 = make_g3i_all(p, t, y0, y0_ast, psi_3, v_vec, iirws, iiaws, sigs)
           g34 = make_g3i_all(p, t, y0, y0_ast, psi_4, v_vec, iirws, iiaws, sigs)
           g35 = make_g3i_all(p, t, y0, y0_ast, psi_5, v_vec, iirws, iiaws, sigs)
           g36 = make_g3i_all(p, t, y0, y0_ast, psi_6, v_vec, iirws, iiaws, sigs)
           
           #make all g mat
           all_g = matrix(0, nrow = k + 7, ncol = p)
           all_g[1:k, ] = t(g11)
           all_g[k + 1, ] = g21
           all_g[k + 2, ] = g31 + g12 + g22
           all_g[k + 3, ] = g32 + g13 + g23
           all_g[k + 4, ] = g33 + g14 + g24
           all_g[k + 5, ] = g34 + g15 + g26
           all_g[k + 6, ] = g35 + g16 + g27
           all_g[k + 7, ] = g36 + g17 + g28
           
           inv_sig_mat = solve(-sec_deri)
           gamma_mat = all_g%*%t(all_g)
           
           if (all_er){
             return_list = list(vc_mat = inv_sig_mat%*%gamma_mat%*%inv_sig_mat,
                                hes_mat = inv_sig_mat,
                                gamma_mat = gamma_mat)
             return(return_list)
           } else {
             vc_mat = inv_sig_mat%*%gamma_mat%*%inv_sig_mat
             return(list(vc_mat = vc_mat))
           }
         },
         "residual" = {
           return(as.vector(iaw%*%k_ast))
         },
         stop("undefined mode"))
}