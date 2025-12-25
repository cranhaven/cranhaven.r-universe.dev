full_aqs = function(para, x_, y, y1, w, w_er, w_lam, inv_c, correction, mode = "normal", hessian_er = F){
  x = x_
  k = dim(x)[2]
  tp = length(y)
  p = dim(w)[1]
  t = tp/p
  rho = para[1]
  alp = para[2]
  theta = para[3]
  lam = para[4]
  eq = numeric(4)
  bdinv_c = kronecker(inv_c, diag(p))
  xt = t(x)
  iaws = diag(p) - alp * w_er
  iaw = kronecker(diag(t), iaws)
  iiaws = solve(iaws)
  iaaw = kronecker(inv_c, t(iaws)%*%iaws)
  irws = diag(p) - rho * w
  iirws = solve(irws)
  irw = kronecker(diag(t), irws)
  lws = lam * w_lam
  lw = kronecker(diag(t), lws)
  beta = solve(xt%*%iaaw%*%x)%*%xt%*%iaaw%*%(irw%*%y - theta*y1 - lw%*%y1)
  k_ast = irw %*% y - x%*%beta - theta*y1 - lw%*%y1
  sigs = as.numeric(t(k_ast) %*% iaaw %*% k_ast/tp)
  iirw = kronecker(diag(t), iirws)
  iiaw = kronecker(diag(t), iiaws)
  bdw = kronecker(diag(t), w)
  bdw_lam = kronecker(diag(t), w_lam)
  bdw_er = kronecker(diag(t), w_er)
  switch(mode,
         "normal" = {
           tmp_mat_1 = t(k_ast) %*% iaaw
           if (correction){
             #aqs
             A_mats = make_A_df(p, t, diag(theta, p, p), lws, iirws)
             vec_bias_theta = diag(bdinv_c %*% A_mats$A_1 %*% iirw)
             vec_bias_rho = diag(bdinv_c %*% A_mats$A %*% iirw %*% bdw)
             vec_bias_lam = diag(bdinv_c %*% A_mats$A_1 %*% iirw %*% bdw_lam)
             eq[1] = 1/sigs * tmp_mat_1 %*% y1 + sum(vec_bias_theta)
             eq[2] = 1/sigs * tmp_mat_1 %*% bdw %*% y + sum(vec_bias_rho)
             eq[3] = 1/sigs * tmp_mat_1 %*% bdw_lam %*% y1 + sum(vec_bias_lam)
             eq[4] = 0.5/sigs * t(k_ast) %*% kronecker(inv_c, t(w_er) %*% iaws + t(iaws) %*% w_er) %*% k_ast - t * sum(diag(w_er %*% iiaws))
           } else {
             #qs
             eq[1] = 1/sigs * tmp_mat_1 %*% y1
             eq[2] = 1/sigs * tmp_mat_1 %*% bdw %*% y - t * sum(diag(iirws %*% w))
             eq[3] = 1/sigs * tmp_mat_1 %*% bdw_lam %*% y1
             eq[4] = 0.5/sigs * t(k_ast) %*% kronecker(inv_c, t(w_er) %*% iaws + t(iaws) %*% w_er) %*% k_ast - t * sum(diag(w_er %*% iiaws))
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
           dthetas = diag(theta, p, p)
           A_mats = make_A_df(p, t, dthetas, lws, iirws)
           A_mats_rho = make_A_deriv_df(p, t, w, dthetas, lws, iirws, mode = "rho")
           A_mats_theta = make_A_deriv_df(p, t, w, dthetas, lws, iirws, mode = "theta")
           A_mats_lam = make_A_deriv_df(p, t, w_lam, dthetas, lws, iirws, mode = "lambda")
           #lines beta
           sec_deri[1:k, 1:k] = -1/sigs*t(x)%*%iaaw%*%x
           sec_deri[1:k, k+1] = -1/sigs^2*t(x)%*%iaaw%*%k_ast
           sec_deri[1:k, k+2] = -1/sigs*t(x)%*%iaaw%*%y1
           sec_deri[1:k, k+3] = -1/sigs*t(x)%*%iaaw%*%bdw%*%y
           sec_deri[1:k, k+4] = -1/sigs*t(x)%*%iaaw%*%bdw_lam%*%y1
           sec_deri[1:k, k+5] = 1/sigs*t(x)%*%(-kronecker(inv_c, t(w_er) %*% iaws + t(iaws) %*% w_er))%*%k_ast
           #line sigma2
           sec_deri[k+1, 1:k] = t(sec_deri[1:k, k+1])
           sec_deri[k+1, k+1] = -1/sigs^3*t(k_ast)%*%iaaw%*%k_ast + tp/(2*sigs^2)
           sec_deri[k+1, k+2] = -1/sigs^2*t(y1)%*%iaaw%*%k_ast
           sec_deri[k+1, k+3] = -1/sigs^2*t(y)%*%t(bdw)%*%iaaw%*%k_ast
           sec_deri[k+1, k+4] = -1/sigs^2*t(y1)%*%t(bdw_lam)%*%iaaw%*%k_ast
           sec_deri[k+1, k+5] = 1/(2*sigs^2)*t(k_ast)%*%(-kronecker(inv_c, t(w_er) %*% iaws + t(iaws) %*% w_er))%*%k_ast
           #line theta
           sec_deri[k+2, 1:k] = t(sec_deri[1:k, k+2])
           sec_deri[k+2, k+1] = sec_deri[k+1, k+2]
           sec_deri[k+2, k+2] = -1/sigs*t(y1)%*%iaaw%*%y1 + sum(diag(bdinv_c%*%A_mats_theta$A_deriv_1%*%iirw))
           sec_deri[k+2, k+3] = -1/sigs*t(y1)%*%iaaw%*%bdw%*%y + sum(diag(bdinv_c%*%(A_mats_rho$A_deriv_1%*%iirw + A_mats$A_1%*%iirw%*%bdw%*%iirw)))
           sec_deri[k+2, k+4] = -1/sigs*t(y1)%*%iaaw%*%bdw_lam%*%y1 + sum(diag(bdinv_c%*%A_mats_lam$A_deriv_1%*%iirw))
           sec_deri[k+2, k+5] = 1/sigs*t(y1)%*%(-kronecker(inv_c, t(w_er) %*% iaws + t(iaws) %*% w_er))%*%k_ast
           #line rho
           sec_deri[k+3, 1:k] = t(sec_deri[1:k, k+3])
           sec_deri[k+3, k+1] = sec_deri[k+1, k+3]
           sec_deri[k+3, k+2] = -1/sigs*t(y)%*%t(bdw)%*%iaaw%*%y1 + sum(diag(bdinv_c%*%A_mats_theta$A_deriv%*%iirw%*%bdw))
           sec_deri[k+3, k+3] = -1/sigs*t(y)%*%t(bdw)%*%iaaw%*%bdw%*%y + sum(diag(bdinv_c%*%(A_mats_rho$A_deriv%*%iirw + A_mats$A%*%iirw%*%bdw%*%iirw)%*%bdw))
           sec_deri[k+3, k+4] = -1/sigs*t(y)%*%t(bdw)%*%iaaw%*%bdw_lam%*%y1 + sum(diag(bdinv_c%*%A_mats_lam$A_deriv%*%iirw%*%bdw))
           sec_deri[k+3, k+5] = 1/sigs*t(y)%*%t(bdw)%*%(-kronecker(inv_c, t(w_er) %*% iaws + t(iaws) %*% w_er))%*%k_ast
           #line lambda
           sec_deri[k+4, 1:k] = t(sec_deri[1:k, k+4])
           sec_deri[k+4, k+1] = sec_deri[k+1, k+4]
           sec_deri[k+4, k+2] = -1/sigs*t(y1)%*%t(bdw_lam)%*%iaaw%*%y1 + sum(diag(bdinv_c%*%A_mats_theta$A_deriv_1%*%iirw%*%bdw_lam))
           sec_deri[k+4, k+3] = -1/sigs*t(y1)%*%t(bdw_lam)%*%iaaw%*%bdw%*%y + sum(diag(bdinv_c%*%(A_mats_rho$A_deriv_1%*%iirw + A_mats$A_1%*%iirw%*%bdw%*%iirw)%*%bdw_lam))
           sec_deri[k+4, k+4] = -1/sigs*t(y1)%*%t(bdw_lam)%*%iaaw%*%bdw_lam%*%y1 + sum(diag(bdinv_c%*%A_mats_lam$A_deriv_1%*%iirw%*%bdw_lam))
           sec_deri[k+4, k+5] = 1/sigs*t(y1)%*%t(bdw_lam)%*%(-kronecker(inv_c, t(w_er) %*% iaws + t(iaws) %*% w_er))%*%k_ast
           #line alpha
           sec_deri[k+5, 1:k] = t(sec_deri[1:k, k+5])
           sec_deri[k+5, k+1] = sec_deri[k+1, k+5]
           sec_deri[k+5, k+2] = sec_deri[k+2, k+5]
           sec_deri[k+5, k+3] = sec_deri[k+3, k+5]
           sec_deri[k+5, k+4] = sec_deri[k+4, k+5]
           sec_deri[k+5, k+5] = -1/sigs*t(k_ast)%*%(kronecker(inv_c, t(w_er) %*% w_er))%*%k_ast - sum(diag((bdw_er%*%iiaw)%*%(bdw_er%*%iiaw)))
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
           pi_1 = 1/sigs*t(iciaw)%*%x
           pi_2 = 1/sigs*t(iciaw)%*%eta_1
           pi_3 = 1/sigs*t(iciaw)%*%bdw%*%eta
           pi_4 = 1/sigs*t(iciaw)%*%bdw_lam%*%eta_1
           phi_1 = 0.5/(sigs^2)*kronecker(inv_c, diag(p))
           phi_2 = 1/sigs*iciaw%*%S_1
           phi_3 = 1/sigs*iciaw%*%bdw%*%S
           phi_4 = 1/sigs*iciaw%*%bdw_lam%*%S_1
           phi_5 = 0.5/(sigs)*kronecker(inv_c, t(w_er%*%iiaws) + w_er%*%iiaws)
           psi_1 = 1/sigs*iciaw%*%R_mats$R_1
           psi_2 = 1/sigs*iciaw%*%bdw%*%R_mats$R
           psi_3 = 1/sigs*iciaw%*%bdw_lam%*%R_mats$R_1
           v_vec = as.vector(iaw%*%k_ast)
           #make y0, y0_ast
           y0 = rep(y1[1:p], t)
           y0_ast = iaws %*% irws %*% y0[1:p]
           #make all gs
           g11 = make_g1i_all(p, t, pi_1, v_vec)
           g12 = make_g1i_all(p, t, pi_2, v_vec)
           g13 = make_g1i_all(p, t, pi_3, v_vec)
           g14 = make_g1i_all(p, t, pi_4, v_vec)
           g21 = make_g2i_all(p, t, phi_1, v_vec, sigs, inv_c)
           # g21 = make_g2i_all(p, t, phi_1, v_vec, sigs, inv_c, t3 = F) - t/(2*sigs)
           g22 = make_g2i_all(p, t, phi_2, v_vec, sigs, inv_c)
           g23 = make_g2i_all(p, t, phi_3, v_vec, sigs, inv_c)
           g24 = make_g2i_all(p, t, phi_4, v_vec, sigs, inv_c)
           g25 = make_g2i_all(p, t, phi_5, v_vec, sigs, inv_c)
           # g25 = make_g2i_all(p, t, phi_5, v_vec, sigs, inv_c, t3 = F) - t*diag(w%*%iiaws)
           g31 = make_g3i_all(p, t, y0, y0_ast, psi_1, v_vec, iirws, iiaws, sigs)
           g32 = make_g3i_all(p, t, y0, y0_ast, psi_2, v_vec, iirws, iiaws, sigs)
           g33 = make_g3i_all(p, t, y0, y0_ast, psi_3, v_vec, iirws, iiaws, sigs)
           #make all g mat
           all_g = matrix(0, nrow = k + 5, ncol = p)
           all_g[1:k, ] = t(g11)
           all_g[k + 1, ] = g21
           all_g[k + 2, ] = g31 + g12 + g22 
           all_g[k + 3, ] = g32 + g13 + g23 
           all_g[k + 4, ] = g33 + g14 + g24 
           all_g[k + 5, ] = g25
           inv_sig_mat = solve(-sec_deri)
           gamma_mat = all_g%*%t(all_g)
           if (hessian_er){
             return_list = list(vc_mat = inv_sig_mat%*%gamma_mat%*%inv_sig_mat,
                                hes_mat = inv_sig_mat)
             return(return_list)
           } else {
             vc_mat = inv_sig_mat%*%gamma_mat%*%inv_sig_mat
             return(list(vc_mat = vc_mat))
           }
         },
         stop("undefined mode"))
}

slm_aqs = function(para, x_, y, y1, w, inv_c, correction, mode = "normal", hessian_er = F){
  x = x_
  k = dim(x)[2]
  tp = length(y)
  p = dim(w)[1]
  t = tp/p
  rho = para[1]
  theta = para[2]
  eq = numeric(2)
  bdinv_c = kronecker(inv_c, diag(p))
  xt = t(x)
  iaaw = bdinv_c
  irws = diag(p) - rho * w
  iirws = solve(irws)
  irw = kronecker(diag(t), irws)
  beta = solve(xt%*%iaaw%*%x)%*%xt%*%iaaw%*%(irw%*%y - theta*y1)
  k_ast = irw %*% y - x%*%beta - theta*y1 
  sigs = as.numeric(t(k_ast) %*% iaaw %*% k_ast/tp)
  iirw = kronecker(diag(t), iirws)
  bdw = kronecker(diag(t), w)
  switch(mode,
         "normal" = {
           tmp_mat_1 = t(k_ast) %*% iaaw
           if (correction){
             #aqs
             A_mats = make_A_df(p, t, diag(theta, p, p), 0, iirws)
             vec_bias_theta = diag(bdinv_c %*% A_mats$A_1 %*% iirw)
             vec_bias_rho = diag(bdinv_c %*% A_mats$A %*% iirw %*% bdw)
             eq[1] = 1/sigs * tmp_mat_1 %*% y1 + sum(vec_bias_theta)
             eq[2] = 1/sigs * tmp_mat_1 %*% bdw %*% y + sum(vec_bias_rho)
           } else {
             #qs
             eq[1] = 1/sigs * tmp_mat_1 %*% y1
             eq[2] = 1/sigs * tmp_mat_1 %*% bdw %*% y - t * sum(diag(iirws %*% w))
           }
           return(sum(eq^2))
         },
         "beta_sigs" = {
           return(list(beta = beta,
                       sigma2 = sigs))
         },
         "opmd" = {
           #calculate second derivative
           sec_deri = matrix(, nrow = k + 3, ncol = k + 3)
           #calculate derivative of A_mat
           dthetas = diag(theta, p, p)
           A_mats = make_A_df(p, t, dthetas, 0, iirws)
           A_mats_rho = make_A_deriv_df(p, t, w, dthetas, 0, iirws, mode = "rho")
           A_mats_theta = make_A_deriv_df(p, t, matrix(0,p,p), dthetas, 0, iirws, mode = "theta")
           #lines beta
           sec_deri[1:k, 1:k] = -1/sigs*t(x)%*%iaaw%*%x
           sec_deri[1:k, k+1] = -1/sigs^2*t(x)%*%iaaw%*%k_ast
           sec_deri[1:k, k+2] = -1/sigs*t(x)%*%iaaw%*%y1
           sec_deri[1:k, k+3] = -1/sigs*t(x)%*%iaaw%*%bdw%*%y
           #line sigma2
           sec_deri[k+1, k+1] = -1/sigs^3*t(k_ast)%*%iaaw%*%k_ast + tp/(2*sigs^2)
           sec_deri[k+1, k+2] = -1/sigs^2*t(y1)%*%iaaw%*%k_ast
           sec_deri[k+1, k+3] = -1/sigs^2*t(y)%*%t(bdw)%*%iaaw%*%k_ast
           #line theta
           sec_deri[k+2, k+2] = -1/sigs*t(y1)%*%iaaw%*%y1 + sum(diag(bdinv_c%*%A_mats_theta$A_deriv_1%*%iirw))
           sec_deri[k+2, k+3] = -1/sigs*t(y1)%*%iaaw%*%bdw%*%y + sum(diag(bdinv_c%*%(A_mats_rho$A_deriv_1%*%iirw + A_mats$A_1%*%iirw%*%bdw%*%iirw)))
           #line rho
           sec_deri[k+3, k+3] = -1/sigs*t(y)%*%t(bdw)%*%iaaw%*%bdw%*%y + sum(diag(bdinv_c%*%(A_mats_rho$A_deriv%*%iirw + A_mats$A%*%iirw%*%bdw%*%iirw)%*%bdw))
           #make it symmetric
           sec_deri = as.matrix(forceSymmetric(sec_deri, uplo = "U"))
           #make list of powers of c_mat
           list_pow_c_mat = c_mat_pows(p,t, iirws, dthetas, 0)
           #make R and G mats
           R_mats = make_R_mats(list_pow_c_mat)
           G_mats = make_G_mats(list_pow_c_mat)
           #make eta, S, pi, phi, psi and v_vec
           iciaw = bdinv_c
           eta = G_mats$G %*% iirw %*% x %*% beta
           eta_1 = G_mats$G_1 %*% iirw %*% x %*% beta
           S = G_mats$G %*% iirw
           S_1 = G_mats$G_1 %*% iirw
           pi_1 = 1/sigs*t(iciaw)%*%x
           pi_2 = 1/sigs*t(iciaw)%*%eta_1
           pi_3 = 1/sigs*t(iciaw)%*%bdw%*%eta
           phi_1 = 0.5/(sigs^2)*kronecker(inv_c, diag(p))
           phi_2 = 1/sigs*iciaw%*%S_1
           phi_3 = 1/sigs*iciaw%*%bdw%*%S
           psi_1 = 1/sigs*iciaw%*%R_mats$R_1
           psi_2 = 1/sigs*iciaw%*%bdw%*%R_mats$R
           v_vec = as.vector(k_ast)
           #make y0, y0_ast
           y0 = rep(y1[1:p], t)
           y0_ast = irws %*% y0[1:p]
           #make all gs
           g11 = make_g1i_all(p, t, pi_1, v_vec)
           g12 = make_g1i_all(p, t, pi_2, v_vec)
           g13 = make_g1i_all(p, t, pi_3, v_vec)
           g21 = make_g2i_all(p, t, phi_1, v_vec, sigs, inv_c)
           g22 = make_g2i_all(p, t, phi_2, v_vec, sigs, inv_c)
           g23 = make_g2i_all(p, t, phi_3, v_vec, sigs, inv_c)
           g31 = make_g3i_all(p, t, y0, y0_ast, psi_1, v_vec, iirws, diag(p), sigs)
           g32 = make_g3i_all(p, t, y0, y0_ast, psi_2, v_vec, iirws, diag(p), sigs)
           #make all g mat
           all_g = matrix(0, nrow = k + 3, ncol = p)
           all_g[1:k, ] = t(g11)
           all_g[k + 1, ] = g21
           all_g[k + 2, ] = g31 + g12 + g22 
           all_g[k + 3, ] = g32 + g13 + g23 
           inv_sig_mat = solve(-sec_deri)
           gamma_mat = all_g%*%t(all_g)
           if (hessian_er){
             return_list = list(vc_mat = inv_sig_mat%*%gamma_mat%*%inv_sig_mat,
                                hes_mat = inv_sig_mat)
             return(return_list)
           } else {
             vc_mat = inv_sig_mat%*%gamma_mat%*%inv_sig_mat
             return(list(vc_mat = vc_mat))
           }
         },
         stop("undefined mode"))
}

sem_aqs = function(para, x_, y, y1, w_er, inv_c, correction, mode = "normal", hessian_er = F){
  x = x_
  k = dim(x)[2]
  tp = length(y)
  p = dim(w_er)[1]
  t = tp/p
  alp = para[1]
  theta = para[2]
  eq = numeric(2)
  bdinv_c = kronecker(inv_c, diag(p))
  xt = t(x)
  iaws = diag(p) - alp * w_er
  iaw = kronecker(diag(t), iaws)
  iiaws = solve(iaws)
  iaaw = kronecker(inv_c, t(iaws)%*%iaws)
  beta = solve(xt%*%iaaw%*%x)%*%xt%*%iaaw%*%(y - theta*y1)
  k_ast = y - x%*%beta - theta*y1
  sigs = as.numeric(t(k_ast) %*% iaaw %*% k_ast/tp)
  iiaw = kronecker(diag(t), iiaws)
  bdw_er = kronecker(diag(t), w_er)
  switch(mode,
         "normal" = {
           tmp_mat_1 = t(k_ast) %*% iaaw
           if (correction){
             #aqs
             A_mats = make_A_df(p, t, diag(theta, p, p), 0, diag(p))
             vec_bias_theta = diag(bdinv_c %*% A_mats$A_1)
             eq[1] = 1/sigs * tmp_mat_1 %*% y1 + sum(vec_bias_theta)
             eq[2] = 0.5/sigs * t(k_ast) %*% kronecker(inv_c, t(w_er) %*% iaws + t(iaws) %*% w_er) %*% k_ast - t * sum(diag(w_er %*% iiaws))
           } else {
             #qs
             eq[1] = 1/sigs * tmp_mat_1 %*% y1
             eq[2] = 0.5/sigs * t(k_ast) %*% kronecker(inv_c, t(w_er) %*% iaws + t(iaws) %*% w_er) %*% k_ast - t * sum(diag(w_er %*% iiaws))
           }
           return(sum(eq^2))
         },
         "beta_sigs" = {
           return(list(beta = beta,
                       sigma2 = sigs))
         },
         "opmd" = {
           #calculate second derivative
           sec_deri = matrix(, nrow = k + 3, ncol = k + 3)
           #calculate derivative of A_mat
           dthetas = diag(theta, p, p)
           A_mats = make_A_df(p, t, dthetas,  0, diag(p))
           A_mats_theta = make_A_deriv_df(p, t, matrix(0,p,p), dthetas, 0, diag(p), mode = "theta")
           #lines beta
           sec_deri[1:k, 1:k] = -1/sigs*t(x)%*%iaaw%*%x
           sec_deri[1:k, k+1] = -1/sigs^2*t(x)%*%iaaw%*%k_ast
           sec_deri[1:k, k+2] = -1/sigs*t(x)%*%iaaw%*%y1
           sec_deri[1:k, k+3] = 1/sigs*t(x)%*%(-kronecker(inv_c, t(w_er) %*% iaws + t(iaws) %*% w_er))%*%k_ast
           #line sigma2
           sec_deri[k+1, k+1] = -1/sigs^3*t(k_ast)%*%iaaw%*%k_ast + tp/(2*sigs^2)
           sec_deri[k+1, k+2] = -1/sigs^2*t(y1)%*%iaaw%*%k_ast
           sec_deri[k+1, k+3] = 1/(2*sigs^2)*t(k_ast)%*%(-kronecker(inv_c, t(w_er) %*% iaws + t(iaws) %*% w_er))%*%k_ast
           #line theta
           sec_deri[k+2, k+2] = -1/sigs*t(y1)%*%iaaw%*%y1 + sum(diag(bdinv_c%*%A_mats_theta$A_deriv_1%*%diag(t*p)))
           sec_deri[k+2, k+3] = 1/sigs*t(y1)%*%(-kronecker(inv_c, t(w_er) %*% iaws + t(iaws) %*% w_er))%*%k_ast
           #line alpha
           sec_deri[k+3, k+3] = -1/sigs*t(k_ast)%*%(kronecker(inv_c, t(w_er) %*% w_er))%*%k_ast - sum(diag((bdw_er%*%iiaw)%*%(bdw_er%*%iiaw)))
           #make it symmetric
           sec_deri = as.matrix(forceSymmetric(sec_deri, uplo = "U"))
           
           #make list of powers of c_mat
           list_pow_c_mat = c_mat_pows(p, t, diag(p), dthetas, 0)
           #make R and G mats
           R_mats = make_R_mats(list_pow_c_mat)
           G_mats = make_G_mats(list_pow_c_mat)
           #make eta, S, pi, phi, psi and v_vec
           iciaw = kronecker(inv_c, iaws)
           eta = G_mats$G %*% x %*% beta
           eta_1 = G_mats$G_1 %*%  x %*% beta
           S = G_mats$G %*%iiaw
           S_1 = G_mats$G_1 %*% iiaw
           pi_1 = 1/sigs*t(iciaw)%*%x
           pi_2 = 1/sigs*t(iciaw)%*%eta_1
           phi_1 = 0.5/(sigs^2)*kronecker(inv_c, diag(p))
           phi_2 = 1/sigs*iciaw%*%S_1
           phi_5 = 0.5/(sigs)*kronecker(inv_c, t(w_er%*%iiaws) + w_er%*%iiaws)
           psi_1 = 1/sigs*iciaw%*%R_mats$R_1
           v_vec = as.vector(iaw%*%k_ast)
           #make y0, y0_ast
           y0 = rep(y1[1:p], t)
           y0_ast = iaws %*% y0[1:p]
           #make all gs
           g11 = make_g1i_all(p, t, pi_1, v_vec)
           g12 = make_g1i_all(p, t, pi_2, v_vec)
           g21 = make_g2i_all(p, t, phi_1, v_vec, sigs, inv_c)
           g22 = make_g2i_all(p, t, phi_2, v_vec, sigs, inv_c)
           g25 = make_g2i_all(p, t, phi_5, v_vec, sigs, inv_c)
           g31 = make_g3i_all(p, t, y0, y0_ast, psi_1, v_vec, diag(p), iiaws, sigs)
           #make all g mat
           all_g = matrix(0, nrow = k + 3, ncol = p)
           all_g[1:k, ] = t(g11)
           all_g[k + 1, ] = g21
           all_g[k + 2, ] = g31 + g12 + g22 
           all_g[k + 3, ] = g25
           inv_sig_mat = solve(-sec_deri)
           gamma_mat = all_g%*%t(all_g)
           if (hessian_er){
             return_list = list(vc_mat = inv_sig_mat%*%gamma_mat%*%inv_sig_mat,
                                hes_mat = inv_sig_mat)
             return(return_list)
           } else {
             vc_mat = inv_sig_mat%*%gamma_mat%*%inv_sig_mat
             return(list(vc_mat = vc_mat))
           }
         },
         stop("undefined mode"))
}

sltl_aqs = function(para, x_, y, y1, w, w_lam, inv_c, correction, mode = "normal", hessian_er = F){
  x = x_
  k = dim(x)[2]
  tp = length(y)
  p = dim(w)[1]
  t = tp/p
  rho = para[1]
  theta = para[2]
  lam = para[3]
  eq = numeric(3)
  bdinv_c = kronecker(inv_c, diag(p))
  xt = t(x)
  iaaw = bdinv_c
  irws = diag(p) - rho * w
  iirws = solve(irws)
  irw = kronecker(diag(t), irws)
  lws = lam * w_lam
  lw = kronecker(diag(t), lws)
  beta = solve(xt%*%iaaw%*%x)%*%xt%*%iaaw%*%(irw%*%y - theta*y1 - lw%*%y1)
  k_ast = irw %*% y - x%*%beta - theta*y1 - lw%*%y1
  sigs = as.numeric(t(k_ast) %*% iaaw %*% k_ast/tp)
  iirw = kronecker(diag(t), iirws)
  bdw = kronecker(diag(t), w)
  bdw_lam = kronecker(diag(t), w_lam)
  switch(mode,
         "normal" = {
           tmp_mat_1 = t(k_ast) %*% iaaw
           if (correction){
             #aqs
             A_mats = make_A_df(p, t, diag(theta, p, p), lws, iirws)
             vec_bias_theta = diag(bdinv_c %*% A_mats$A_1 %*% iirw)
             vec_bias_rho = diag(bdinv_c %*% A_mats$A %*% iirw %*% bdw)
             vec_bias_lam = diag(bdinv_c %*% A_mats$A_1 %*% iirw %*% bdw_lam)
             eq[1] = 1/sigs * tmp_mat_1 %*% y1 + sum(vec_bias_theta)
             eq[2] = 1/sigs * tmp_mat_1 %*% bdw %*% y + sum(vec_bias_rho)
             eq[3] = 1/sigs * tmp_mat_1 %*% bdw_lam %*% y1 + sum(vec_bias_lam)
           } else {
             #qs
             eq[1] = 1/sigs * tmp_mat_1 %*% y1
             eq[2] = 1/sigs * tmp_mat_1 %*% bdw %*% y - t * sum(diag(iirws %*% w))
             eq[3] = 1/sigs * tmp_mat_1 %*% bdw_lam %*% y1
           }
           return(sum(eq^2))
         },
         "beta_sigs" = {
           return(list(beta = beta,
                       sigma2 = sigs))
         },
         "opmd" = {
           #calculate second derivative
           sec_deri = matrix(, nrow = k + 4, ncol = k + 4)
           #calculate derivative of A_mat
           dthetas = diag(theta, p, p)
           A_mats = make_A_df(p, t, dthetas, lws, iirws)
           A_mats_rho = make_A_deriv_df(p, t, w, dthetas, lws, iirws, mode = "rho")
           A_mats_theta = make_A_deriv_df(p, t, matrix(0,p,p), dthetas, lws, iirws, mode = "theta")
           A_mats_lam = make_A_deriv_df(p, t, w_lam, dthetas, lws, iirws, mode = "lambda")
           #lines beta
           sec_deri[1:k, 1:k] = -1/sigs*t(x)%*%iaaw%*%x
           sec_deri[1:k, k+1] = -1/sigs^2*t(x)%*%iaaw%*%k_ast
           sec_deri[1:k, k+2] = -1/sigs*t(x)%*%iaaw%*%y1
           sec_deri[1:k, k+3] = -1/sigs*t(x)%*%iaaw%*%bdw%*%y
           sec_deri[1:k, k+4] = -1/sigs*t(x)%*%iaaw%*%bdw_lam%*%y1
           #line sigma2
           sec_deri[k+1, k+1] = -1/sigs^3*t(k_ast)%*%iaaw%*%k_ast + tp/(2*sigs^2)
           sec_deri[k+1, k+2] = -1/sigs^2*t(y1)%*%iaaw%*%k_ast
           sec_deri[k+1, k+3] = -1/sigs^2*t(y)%*%t(bdw)%*%iaaw%*%k_ast
           sec_deri[k+1, k+4] = -1/sigs^2*t(y1)%*%t(bdw_lam)%*%iaaw%*%k_ast
           #line theta
           sec_deri[k+2, k+2] = -1/sigs*t(y1)%*%iaaw%*%y1 + sum(diag(bdinv_c%*%A_mats_theta$A_deriv_1%*%iirw))
           sec_deri[k+2, k+3] = -1/sigs*t(y1)%*%iaaw%*%bdw%*%y + sum(diag(bdinv_c%*%(A_mats_rho$A_deriv_1%*%iirw + A_mats$A_1%*%iirw%*%bdw%*%iirw)))
           sec_deri[k+2, k+4] = -1/sigs*t(y1)%*%iaaw%*%bdw_lam%*%y1 + sum(diag(bdinv_c%*%A_mats_lam$A_deriv_1%*%iirw))
           #line rho
           sec_deri[k+3, k+3] = -1/sigs*t(y)%*%t(bdw)%*%iaaw%*%bdw%*%y + sum(diag(bdinv_c%*%(A_mats_rho$A_deriv%*%iirw + A_mats$A%*%iirw%*%bdw%*%iirw)%*%bdw))
           sec_deri[k+3, k+4] = -1/sigs*t(y)%*%t(bdw)%*%iaaw%*%bdw_lam%*%y1 + sum(diag(bdinv_c%*%A_mats_lam$A_deriv%*%iirw%*%bdw))
           #line lambda
           sec_deri[k+4, k+4] = -1/sigs*t(y1)%*%t(bdw_lam)%*%iaaw%*%bdw_lam%*%y1 + sum(diag(bdinv_c%*%A_mats_lam$A_deriv_1%*%iirw%*%bdw_lam))
           #make it symmetric
           sec_deri = as.matrix(forceSymmetric(sec_deri, uplo = "U"))
           #make list of powers of c_mat
           list_pow_c_mat = c_mat_pows(p,t, iirws, dthetas, lws)
           #make R and G mats
           R_mats = make_R_mats(list_pow_c_mat)
           G_mats = make_G_mats(list_pow_c_mat)
           #make eta, S, pi, phi, psi and v_vec
           iciaw = bdinv_c
           eta = G_mats$G %*% iirw %*% x %*% beta
           eta_1 = G_mats$G_1 %*% iirw %*% x %*% beta
           S = G_mats$G %*% iirw
           S_1 = G_mats$G_1 %*% iirw
           pi_1 = 1/sigs*t(iciaw)%*%x
           pi_2 = 1/sigs*t(iciaw)%*%eta_1
           pi_3 = 1/sigs*t(iciaw)%*%bdw%*%eta
           pi_4 = 1/sigs*t(iciaw)%*%bdw_lam%*%eta_1
           phi_1 = 0.5/(sigs^2)*kronecker(inv_c, diag(p))
           phi_2 = 1/sigs*iciaw%*%S_1
           phi_3 = 1/sigs*iciaw%*%bdw%*%S
           phi_4 = 1/sigs*iciaw%*%bdw_lam%*%S_1
           psi_1 = 1/sigs*iciaw%*%R_mats$R_1
           psi_2 = 1/sigs*iciaw%*%bdw%*%R_mats$R
           psi_3 = 1/sigs*iciaw%*%bdw_lam%*%R_mats$R_1
           v_vec = as.vector(k_ast)
           #make y0, y0_ast
           y0 = rep(y1[1:p], t)
           y0_ast = irws %*% y0[1:p]
           #make all gs
           g11 = make_g1i_all(p, t, pi_1, v_vec)
           g12 = make_g1i_all(p, t, pi_2, v_vec)
           g13 = make_g1i_all(p, t, pi_3, v_vec)
           g14 = make_g1i_all(p, t, pi_4, v_vec)
           g21 = make_g2i_all(p, t, phi_1, v_vec, sigs, inv_c)
           # g21 = make_g2i_all(p, t, phi_1, v_vec, sigs, inv_c, t3 = F) - t/(2*sigs)
           g22 = make_g2i_all(p, t, phi_2, v_vec, sigs, inv_c)
           g23 = make_g2i_all(p, t, phi_3, v_vec, sigs, inv_c)
           g24 = make_g2i_all(p, t, phi_4, v_vec, sigs, inv_c)
           # g25 = make_g2i_all(p, t, phi_5, v_vec, sigs, inv_c, t3 = F) - t*diag(w%*%iiaws)
           g31 = make_g3i_all(p, t, y0, y0_ast, psi_1, v_vec, iirws, diag(p), sigs)
           g32 = make_g3i_all(p, t, y0, y0_ast, psi_2, v_vec, iirws, diag(p), sigs)
           g33 = make_g3i_all(p, t, y0, y0_ast, psi_3, v_vec, iirws, diag(p), sigs)
           #make all g mat
           all_g = matrix(0, nrow = k + 4, ncol = p)
           all_g[1:k, ] = t(g11)
           all_g[k + 1, ] = g21
           all_g[k + 2, ] = g31 + g12 + g22 
           all_g[k + 3, ] = g32 + g13 + g23 
           all_g[k + 4, ] = g33 + g14 + g24 
           inv_sig_mat = solve(-sec_deri)
           gamma_mat = all_g%*%t(all_g)
           if (hessian_er){
             return_list = list(vc_mat = inv_sig_mat%*%gamma_mat%*%inv_sig_mat,
                                hes_mat = inv_sig_mat)
             return(return_list)
           } else {
             vc_mat = inv_sig_mat%*%gamma_mat%*%inv_sig_mat
             return(list(vc_mat = vc_mat))
           }
         },
         stop("undefined mode"))
}