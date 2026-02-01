library(Rcpp)
library(RcppArmadillo)
#' @import Rcpp
#' @import RcppArmadillo

sourceCpp("./src/I_kl.cpp")
sourceCpp("./src/Delta_Orange_5.cpp")
#sourceCpp("./src/Delta.cpp")
sourceCpp("./src/Delta_SBM_2.cpp")
sourceCpp("./src/convertNodePair.cpp")


#-----------------Termes kl-----------------------------------
#Compute the usefull terms 
terme_kl <- function(ind.all, dataVec, Avec, A_test, Z, Z_matrix, iStar, g, h, k, l, rho, n0, eta0, zeta0, fast){
  lenZ=length(Z)
  if(fast){
    #Using Rcpp
    # ---------------- I_kl & n_kl
    I<- I_fast(ind.all, Avec, Z_matrix, k, l)
    I_kl=I$I_kl
    n_kl=I$n_kl


    # ---------------- delta_kl
    ind.iStar= convertNodePair_fast(iStar,(1:lenZ)[-iStar],lenZ,directed=FALSE)[,1]  #index of (iStar,j) for j!=iStar
    # D<-Delta_fast(Avec, A_test, Z_matrix, iStar, g, h, k, l, ind.iStar, n_kl)
    #   k_add=D$k_add[1,]
    #   l_remove=D$l_remove[1,]
    #   delta_kl = D$delta_kl
    #   n_kl_test =D$n_kl_test
    #
    D<-Delta(Avec, A_test, Z_matrix, iStar, g, h, k, l, ind.iStar, n_kl)
    k_add=D$k_add
    l_remove=D$l_remove
    delta_kl = D$delta_kl
    n_kl_test =D$n_kl_test


    # ---------------- Delta Orange : terme 5

    DO5<-Delta_orange_5_fast(dataVec, Z_matrix, iStar, g, h, k, l, I_kl, n_kl, n_kl_test, ind.iStar, k_add, l_remove)
    E_X=DO5[1]
    E_X2=DO5[2]
    E_X_test=DO5[3]
    E_X2_test=DO5[4]
    sum_E_X=DO5[5]
    sum_E_X2=DO5[6]
    sum_psi_X=DO5[7]
    sum_psi_X2=DO5[8]

    # ---------------- Delta SBM : terme 2

    DS2<-Delta_SBM_2_fast(Z, Z_matrix, iStar, g, h, k, l, n0, eta0, zeta0, n_kl, n_kl_test)
    eta_kl=DS2[1,1]
    zeta_kl= DS2[2,1]
    eta_kl_test=DS2[3,1]
    zeta_kl_test=DS2[4,1]
  }else{
    #Not using Rcpp
    # ---------------- I_kl & n_kl
    I_kl <- I(ind.all, Avec, Z_matrix, k, l)
    n_kl = sum(I_kl)

    # ---------------- delta_kl
    ind.iStar= convertNodePair(iStar,(1:lenZ)[-iStar],lenZ,directed=FALSE)  #index of (iStar,j) for j!=iStar
    D<-Delta(Avec, A_test, Z_matrix, iStar, g, h, k, l, ind.iStar, n_kl)
    k_add=D$k_add
    l_remove=D$l_remove
    delta_kl = D$delta_kl
    n_kl_test =D$n_kl_test


    # ---------------- Delta Orange : terme 5
    DO5<-Delta_orange_5(dataVec, Z_matrix, iStar, g, h, k, l, I_kl, n_kl, n_kl_test, ind.iStar, k_add, l_remove)
    E_X=DO5$E_X
    E_X2=DO5$E_X2
    E_X_test=DO5$E_X_test
    E_X2_test=DO5$E_X2_test
    sum_E_X=DO5$sum_E_X
    sum_E_X2=DO5$sum_E_X2
    sum_psi_X=DO5$sum_psi_X
    sum_psi_X2=DO5$sum_psi_X2

    # ---------------- Delta SBM : terme 2
    DS2<-Delta_SBM_2(Z, Z_matrix, iStar, g, h, k, l, n0, eta0, zeta0, n_kl, n_kl_test)
    eta_kl=DS2$eta_kl
    zeta_kl= DS2$zeta_kl
    eta_kl_test=DS2$eta_kl_test
    zeta_kl_test=DS2$zeta_kl_test
  }


  # ---------------- Delta Orange : sum terme 4
  DO4<-Delta_orange_4(Z_matrix, rho, n_kl, delta_kl, sum_E_X, sum_E_X2, sum_psi_X, sum_psi_X2)
  sum_Ikl_4=DO4$sum_Ikl_4
  sum_psi_4 =DO4$sum_psi_4


  #----------------- Return
  return(list(n_kl = n_kl, delta_kl=delta_kl, n_kl_test=n_kl_test, sum_Ikl_4=sum_Ikl_4, sum_psi_4=sum_psi_4, E_X=E_X, E_X2=E_X2, E_X_test=E_X_test, E_X2_test=E_X2_test, eta_kl=eta_kl, zeta_kl=zeta_kl, eta_kl_test = eta_kl_test, zeta_kl_test=zeta_kl_test))

}


terme_NIG_kl <- function(ind.all, dataVec, Avec, A_test, Z, Z_matrix, iStar, g, h, k, l, n0, eta0, zeta0, fast){     #pas besoin de rho
  lenZ=length(Z)
  if(fast){
    #Using Rcpp
    # ---------------- I_kl & n_kl
    I<- I_fast(ind.all, Avec, Z_matrix, k, l)
    I_kl=I$I_kl
    n_kl=I$n_kl

    # ---------------- delta_kl
    ind.iStar= convertNodePair_fast(iStar,(1:lenZ)[-iStar],lenZ,directed=FALSE)[,1]  #index of (iStar,j) for j!=iStar
    # D<-Delta_fast(Avec, A_test, Z_matrix, iStar, g, h, k, l, ind.iStar, n_kl)
    #   k_add=D$k_add[1,]
    #   l_remove=D$l_remove[1,]
    #   delta_kl = D$delta_kl
    #   n_kl_test =D$n_kl_test
    #
    D<-Delta(Avec, A_test, Z_matrix, iStar, g, h, k, l, ind.iStar, n_kl)
    k_add=D$k_add
    l_remove=D$l_remove
    delta_kl = D$delta_kl
    n_kl_test =D$n_kl_test


    # ---------------- Delta Orange : terme 5

    DO5<-Delta_orange_5_fast(dataVec, Z_matrix, iStar, g, h, k, l, I_kl, n_kl, n_kl_test, ind.iStar, k_add, l_remove)
    E_X=DO5[1]
    E_X2=DO5[2]
    E_X_test=DO5[3]
    E_X2_test=DO5[4]
    sum_E_X=DO5[5]
    sum_E_X2=DO5[6]
    sum_psi_X=DO5[7]
    sum_psi_X2=DO5[8]

    # ---------------- Delta SBM : terme 2

    DS2<-Delta_SBM_2_fast(Z, Z_matrix, iStar, g, h, k, l, n0, eta0, zeta0, n_kl, n_kl_test)
    eta_kl=DS2[1,1]
    zeta_kl= DS2[2,1]
    eta_kl_test=DS2[3,1]
    zeta_kl_test=DS2[4,1]
  }else{
    #Not using Rcpp
    # ---------------- I_kl & n_kl
    I_kl <- I(ind.all, Avec, Z_matrix, k, l)
    n_kl = sum(I_kl)

    # ---------------- delta_kl
    ind.iStar= convertNodePair(iStar,(1:lenZ)[-iStar],lenZ,directed=FALSE)  #index of (iStar,j) for j!=iStar
    D<-Delta(Avec, A_test, Z_matrix, iStar, g, h, k, l, ind.iStar, n_kl)
    k_add=D$k_add
    l_remove=D$l_remove
    delta_kl = D$delta_kl
    n_kl_test =D$n_kl_test

    # ---------------- Delta Orange : terme 5
    DO5<-Delta_orange_5(dataVec, Z_matrix, iStar, g, h, k, l, I_kl, n_kl, n_kl_test, ind.iStar, k_add, l_remove)
    E_X=DO5$E_X
    E_X2=DO5$E_X2
    E_X_test=DO5$E_X_test
    E_X2_test=DO5$E_X2_test

    # ---------------- Delta SBM : terme 2
    DS2<-Delta_SBM_2(Z, Z_matrix, iStar, g, h, k, l, n0, eta0, zeta0, n_kl, n_kl_test)
    eta_kl=DS2$eta_kl
    zeta_kl= DS2$zeta_kl
    eta_kl_test=DS2$eta_kl_test
    zeta_kl_test=DS2$zeta_kl_test
  }


  #----------------- Return
  return(list(n_kl = n_kl, delta_kl=delta_kl, n_kl_test=n_kl_test, E_X=E_X, E_X2=E_X2, E_X_test=E_X_test, E_X2_test=E_X2_test, eta_kl=eta_kl, zeta_kl=zeta_kl, eta_kl_test = eta_kl_test, zeta_kl_test=zeta_kl_test))

}




#-----Auxiliary functions for term_kl --------------


I<- function(ind.all, Avec, Z_matrix, k, l){
  mask <- if (k==l) (Z_matrix[k, ind.all[,1]]*Z_matrix[l, ind.all[,2]]) else
    (Z_matrix[k, ind.all[,1]]*Z_matrix[l, ind.all[,2]] + Z_matrix[k, ind.all[,2]]*Z_matrix[l, ind.all[,1]] )
  I <- mask * Avec
  return(I)
}

Delta<-function(Avec, A_test, Z_matrix, iStar, g, h, k, l, ind.iStar, n_kl){
  l_remove = Avec[ind.iStar]* Z_matrix[l, -iStar]  # 1 1 for pairs (iStar,j) with edge, and with j in l
  delta_l_remove = sum(l_remove)
  k_add = A_test[-iStar]*Z_matrix[k, -iStar]
  delta_k_add = sum(k_add)
  delta_kl = -1*(k==g)*delta_l_remove + 1*(l==h)*delta_k_add
  #we'll have k=g; l  in seqQ[-h]   :-sum(l_remove)
  #we'll have l=h; k  in seqQ[-g]   :+sum(l_add)
  #we'll have k=g; l =h             :-sum(l_remove)+sum(l_add)
  n_kl_test = n_kl + delta_kl
  return(list(delta_kl=delta_kl,n_kl_test=n_kl_test,k_add=k_add,l_remove=l_remove))
}

Delta_orange_5<-function(dataVec, Z_matrix, iStar, g, h, k, l, I_kl, n_kl, n_kl_test, ind.iStar, k_add, l_remove){
  # -- E_Ikl
  sum_E_X = sum(I_kl *  dataVec)
  sum_E_X2 = sum(I_kl *  dataVec**2)

  if (n_kl == 0){ E_X = 0 ; E_X2 = 0 }
  else{  E_X = sum_E_X / n_kl ;  E_X2 = sum_E_X2 / n_kl }

  # -- E_Ikl_test
  sum_psi_X =-1*(k==g)* sum(l_remove * dataVec[ind.iStar]) + 1*(l==h)*sum(k_add * dataVec[ind.iStar])
  sum_psi_X2 = -1*(k==g)* sum(l_remove * (dataVec[ind.iStar])**2) + 1*(l==h)*sum(k_add * (dataVec[ind.iStar])**2)

  if (n_kl_test == 0){ E_X2_test = 0 ;   E_X_test = 0 }
  else{ E_X2_test = (n_kl/n_kl_test)*E_X2+ (1/n_kl_test)*sum_psi_X2
  E_X_test = (n_kl/n_kl_test)*E_X+ (1/n_kl_test)*sum_psi_X    }

  return(list(E_X=E_X, E_X2=E_X2, E_X_test=E_X_test, E_X2_test=E_X2_test, sum_E_X=sum_E_X, sum_E_X2=sum_E_X2, sum_psi_X=sum_psi_X,  sum_psi_X2= sum_psi_X2 ))
}

Delta_orange_4<-function(Z_matrix, rho, n_kl, delta_kl, sum_E_X, sum_E_X2, sum_psi_X, sum_psi_X2 ){
  sum_Ikl_4 = sum_E_X2 -2*rho*sum_E_X +(rho^2)*n_kl
  sum_psi_4 = sum_psi_X2 -2*rho*sum_psi_X + (rho^2)* delta_kl
  return(list(sum_Ikl_4=sum_Ikl_4, sum_psi_4 =sum_psi_4 ))
}

Delta_SBM_2<-function(Z, Z_matrix, iStar, g, h, k, l, n0, eta0, zeta0, n_kl, n_kl_test){
  n_k =  n0 + sum(Z==k)
  n_l =  n0 + sum(Z==l)
  m_kl <- if (k==l) 1/2*(n_k-n0)*(n_k-n0 - 1)  else (n_k-n0)*(n_l-n0)
  m_kl_test <- m_kl - 1*(k==g)*(n_l - n0 - (Z[iStar]==l)) + 1*(l==h)*(n_k - n0 - (Z[iStar]==k))

  eta_kl = eta0 + n_kl
  zeta_kl = zeta0 + m_kl - n_kl
  eta_kl_test = eta0 + n_kl_test
  zeta_kl_test = zeta0 + m_kl_test - n_kl_test
  return(list(eta_kl=eta_kl, zeta_kl= zeta_kl,eta_kl_test=eta_kl_test,zeta_kl_test=zeta_kl_test))
}

#======================= Delta Nsbm When the variance is known =========================
## ----------------------------Not Empty Loop -----------------------
# function used in loop when g not empty after removing

delta_NSBM_not_empty_loop <- function(ind.all, dataVec, A, A_test, Z, Z_matrix, iStar, g, h, k, l, rho, tau, n0, eta0, zeta0, sigma0, sigma1, fast){
  # ---------------- termes kl
  termes = terme_kl(ind.all, dataVec, A, A_test, Z, Z_matrix, iStar, g, h, k, l, rho, n0, eta0, zeta0,fast)
  n_kl = termes$n_kl
  delta_kl = termes$delta_kl
  n_kl_test = termes$n_kl_test
  sum_Ikl_4 = termes$sum_Ikl_4
  sum_psi_4= termes$sum_psi_4
  E_X = termes$E_X
  E_X2 = termes$E_X2
  E_X_test = termes$E_X_test
  E_X2_test = termes$E_X2_test
  eta_kl = termes$eta_kl
  zeta_kl = termes$zeta_kl
  eta_kl_test = termes$eta_kl_test
  zeta_kl_test = termes$zeta_kl_test

  # ---------------- termes Orange 2-5
  terme3_db =  log((sigma1**2+tau**2*n_kl_test)/(sigma1**2+tau**2*n_kl))
  terme4_db =  ((1/(sigma1**2+tau**2*(n_kl_test)))-(1/(sigma1**2+tau**2*n_kl)))*sum_Ikl_4 + 1/(sigma1**2+tau**2 *(n_kl_test)) * sum_psi_4
  terme5_db = (((n_kl_test**2)*(E_X2_test-E_X_test**2))/((sigma1**4)/(tau**2)+(sigma1**2)*(n_kl_test))) - ((n_kl**2)*(E_X2 - E_X**2))/((sigma1**4)/(tau**2)+(sigma1**2)*(n_kl))
  terme6_db = log(sigma1)*(n_kl_test-n_kl)

  # ---------------- termes SBM
  terme2_SBM =  lbeta(eta_kl_test, zeta_kl_test)-lbeta(eta_kl, zeta_kl)
  return (list(t3=terme3_db, t4=terme4_db, t5=terme5_db,t6=terme6_db, terme2_SBM=terme2_SBM))
}

# ---------------------------- Empty Loop----------------------------------------------------------------------------------

# function used in loop when g empty after removing
delta_NSBM_empty_loop <- function(ind.all, dataVec, A, A_test, Z, Z_matrix, iStar, g, h, k, l, rho, tau, n0, eta0, zeta0, sigma0, sigma1, fast){

  # ---------------- termes kl
  termes = terme_kl(ind.all, dataVec, A, A_test, Z, Z_matrix, iStar, g, h, k, l, rho, n0, eta0, zeta0,fast)
  n_kl = termes$n_kl
  sum_Ikl_4 = termes$sum_Ikl_4
  E_X = termes$E_X
  E_X2 = termes$E_X2
  eta_kl = termes$eta_kl
  zeta_kl = termes$zeta_kl

  # ---------------- termes Orange 3-6

  terme3_db= log(sigma1**2/((sigma1**2)+tau**2*n_kl))
  terme4_db= - (1/((sigma1**2)+tau**2*n_kl))*sum_Ikl_4
  terme5_db= - ((n_kl**2)*(E_X2 - E_X**2))/((sigma1**4)/(tau**2)+(sigma1**2)*(n_kl))
  terme6_db= - log(sigma1)*n_kl

  # ---------------- termes SBM
  terme2_SBM = lbeta(eta0, zeta0)-lbeta(eta_kl, zeta_kl)

  return (list( t3=terme3_db, t4=terme4_db, t5=terme5_db,t6=terme6_db, terme2_SBM=terme2_SBM))
}



# ------------------------------- delta NSBM ---------------------------------------------------------------------------------------------
delta_NSBM <- function(ind.all, dataVec, A, A_test, Z, Z_matrix, iStar, g, h,  rho, tau, n0, eta0, zeta0, sigma0, sigma1, fast){
  Q=nrow(Z_matrix)
  seqQ = 1:Q
  lenZ = length(Z)

  # ---------------- Orange : terme 2
  ind = convertNodePair(iStar,(1:lenZ)[-iStar],lenZ,directed=FALSE)
  terme2 =-sum((log(sigma0)+0.5*(dataVec[ind]/sigma0)**2)*(A[ind ] -A_test[-iStar]))

  # ---------------- Autres termes
  terme3_db_g = 0
  terme3_db_h = 0
  terme4_db_g = 0
  terme4_db_h = 0
  terme5_db_g = 0
  terme5_db_h = 0
  terme6_db_g = 0
  terme6_db_h = 0
  terme2_h_SBM = 0
  terme2_g_SBM = 0

  l = h
  for (k in seqQ[-g]){
    delta = delta_NSBM_not_empty_loop(ind.all, dataVec, A, A_test, Z, Z_matrix, iStar, g, h, k, l, rho, tau, n0, eta0, zeta0, sigma0, sigma1, fast)
    terme3_db_h = terme3_db_h + delta$t3
    terme4_db_h = terme4_db_h + delta$t4
    terme5_db_h = terme5_db_h + delta$t5
    terme6_db_h = terme6_db_h + delta$t6
    terme2_h_SBM = terme2_h_SBM + delta$terme2_SBM
  }

  #if not empty :
  if ((length(which(Z[-iStar]==g))) != 0){
    n_h=n0 + sum(Z==h)
    n_g=n0 + sum(Z==g)
    terme1_SBM = log(n_h / (n_g - 1))

    k = g
    for (l in seqQ){  #included cas k=g et l=h
      delta = delta_NSBM_not_empty_loop(ind.all, dataVec, A, A_test, Z, Z_matrix, iStar, g, h, k, l, rho, tau, n0, eta0, zeta0, sigma0, sigma1, fast)
      terme3_db_g = terme3_db_g + delta$t3
      terme4_db_g = terme4_db_g + delta$t4
      terme5_db_g = terme5_db_g + delta$t5
      terme6_db_g = terme6_db_g + delta$t6
      terme2_g_SBM = terme2_g_SBM + delta$terme2_SBM
    }
  }

  # if empty :
  else {
    n_h=n0 + sum(Z==h)
    terme1_SBM = log((n_h / n0)*((gamma((Q-1)*n0)*gamma(Q*n0+lenZ))/(gamma(Q*n0)*gamma((Q-1)*n0+lenZ))))

    k = g
    for (l in seqQ){
      delta = delta_NSBM_empty_loop(ind.all, dataVec, A, A_test, Z, Z_matrix, iStar, g, h, k, l, rho, tau, n0, eta0, zeta0, sigma0, sigma1, fast)
      terme3_db_g = terme3_db_g + delta$t3
      terme4_db_g = terme4_db_g + delta$t4
      terme5_db_g = terme5_db_g + delta$t5
      terme6_db_g = terme6_db_g + delta$t6
      terme2_g_SBM = terme2_g_SBM + delta$terme2_SBM
    }
  }

  terme3 = -0.5* (terme3_db_g + terme3_db_h)
  terme4 = -0.5* (terme4_db_g + terme4_db_h)
  terme5 = -0.5* (terme5_db_g + terme5_db_h)
  terme6 = -(terme6_db_g + terme6_db_h)

  delta_orange = terme2+terme3+terme4+terme5+terme6
  delta_SBM= terme1_SBM +terme2_h_SBM + terme2_g_SBM

  delta=delta_orange+delta_SBM
  return(list(delta=delta, delta_orange=delta_orange, delta_SBM=delta_SBM))
}

#============== Delta Nsbm When the variance is unknown (NIG case) ====================
##-----------------------------Auxiliary Functions ------------------
d_post_kl<-function(a,b,c,d,E_X,E_X2,n_kl){
  if(n_kl==0){
    toReturn=d
  }else{
    toReturn=d+0.5*n_kl*(E_X2-(E_X)**2)+n_kl*b*(E_X-a)**2/(2*(n_kl+b))
  }
  return(toReturn)
}

terme_log<-function(b_x,c_x,d_x){
  toReturn=lgamma(c_x)-c_x*log(d_x)-0.5*log(b_x)
  return(toReturn)
}

## ----------------------------Not Empty Loop NIG-----------------------

delta_NSBM_not_empty_loop_NIG <- function(ind.all, dataVec, A, A_test, Z, Z_matrix, iStar, g, h, k, l, a, b, c, d, n0, eta0, zeta0, sigma0, fast){
  # ---------------- termes kl
  termes = terme_NIG_kl(ind.all, dataVec, A, A_test, Z, Z_matrix, iStar, g, h, k, l, n0, eta0, zeta0, fast)
  n_kl = termes$n_kl
  delta_kl = termes$delta_kl
  n_kl_test = termes$n_kl_test
  E_X = termes$E_X
  E_X2 = termes$E_X2
  E_X_test = termes$E_X_test
  E_X2_test = termes$E_X2_test
  eta_kl = termes$eta_kl
  zeta_kl = termes$zeta_kl
  eta_kl_test = termes$eta_kl_test
  zeta_kl_test = termes$zeta_kl_test

  # ---------------- termes NSBM
  b_x_test=b+n_kl_test
  c_x_test=c+0.5*n_kl_test
  d_x_test=d_post_kl(a,b,c,d,E_X_test,E_X2_test, n_kl_test)

  b_x=b+n_kl
  c_x=c+0.5*n_kl
  d_x=d_post_kl(a,b,c,d,E_X,E_X2,n_kl)

  terme_NSBM=terme_log(b_x_test,c_x_test,d_x_test)-terme_log(b_x,c_x,d_x)

  # ---------------- termes SBM
  terme2_SBM =  lbeta(eta_kl_test, zeta_kl_test)-lbeta(eta_kl, zeta_kl)


  return (list(terme_NSBM=terme_NSBM, terme2_SBM=terme2_SBM))
}

# ---------------------------- Empty Loop NIG----------------------------------------------------------------------------------
# function used in loop when g empty after removing
delta_NSBM_empty_loop_NIG <- function(ind.all, dataVec, A, A_test, Z, Z_matrix, iStar, g, h, k, l, a,b,c,d , n0, eta0, zeta0, sigma0, fast){

  # ---------------- termes kl
  termes = terme_NIG_kl(ind.all, dataVec, A, A_test, Z, Z_matrix, iStar, g, h, k, l, n0, eta0, zeta0,fast)
  n_kl = termes$n_kl
  E_X = termes$E_X
  E_X2 = termes$E_X2
  eta_kl = termes$eta_kl
  zeta_kl = termes$zeta_kl

  # ---------------- termes NSBM
  b_x=b+n_kl
  c_x=c+0.5*n_kl
  d_x=d_post_kl(a,b,c,d,E_X,E_X2, n_kl)

  terme_NSBM=terme_log(b,c,d)-terme_log(b_x,c_x,d_x)

  # ---------------- termes SBM
  terme2_SBM = lbeta(eta0, zeta0)-lbeta(eta_kl, zeta_kl)

  return (list(terme_NSBM=terme_NSBM, terme2_SBM=terme2_SBM))
}

# ------------------------------- delta NSBM_NIG ---------------------------------------------------------------------------------------------
delta_NSBM_NIG <- function(ind.all, dataVec, A, A_test, Z, Z_matrix, iStar, g, h, a, b, c, d, n0, eta0, zeta0, sigma0, fast){
  Q=nrow(Z_matrix)
  seqQ = 1:Q
  lenZ = length(Z)

  # ---------------- Orange : terme 2
  ind = convertNodePair(iStar,(1:lenZ)[-iStar],lenZ,directed=FALSE)
  terme2 =-sum((log(sigma0)+0.5*(dataVec[ind]/sigma0)**2)*(A[ind ] -A_test[-iStar]))

  # ---------------- Autres termes
  terme_nsbm_db_g = 0
  terme_nsbm_db_h = 0

  terme2_h_SBM = 0
  terme2_g_SBM = 0

  l = h
  for (k in seqQ[-g]){
    delta = delta_NSBM_not_empty_loop_NIG(ind.all, dataVec, A, A_test, Z, Z_matrix, iStar, g, h, k, l, a, b, c, d, n0, eta0, zeta0, sigma0, fast)
    terme_nsbm_db_h =  terme_nsbm_db_h + delta$terme_NSBM
    terme2_h_SBM = terme2_h_SBM + delta$terme2_SBM
  }

  #if not empty :
  if ((length(which(Z[-iStar]==g))) != 0){
    n_h=n0 + sum(Z==h)
    n_g=n0 + sum(Z==g)
    terme1_SBM = log(n_h / (n_g - 1))

    k = g
    for (l in seqQ){  #included cas k=g et l=h
      delta = delta_NSBM_not_empty_loop_NIG(ind.all, dataVec, A, A_test, Z, Z_matrix, iStar, g, h, k, l, a, b, c, d, n0, eta0, zeta0, sigma0, fast)
      terme_nsbm_db_g =  terme_nsbm_db_g + delta$terme_NSBM
      terme2_g_SBM = terme2_g_SBM + delta$terme2_SBM
    }
  }

  # if empty :
  else {
    n_h=n0 + sum(Z==h)
    terme1_SBM = log((n_h / n0)*((gamma((Q-1)*n0)*gamma(Q*n0+lenZ))/(gamma(Q*n0)*gamma((Q-1)*n0+lenZ))))

    k = g
    for (l in seqQ){
      delta = delta_NSBM_empty_loop_NIG(ind.all, dataVec, A, A_test, Z, Z_matrix, iStar, g, h, k, l, a, b, c, d, n0, eta0, zeta0, sigma0, fast)
      terme_nsbm_db_g =  terme_nsbm_db_g + delta$terme_NSBM
      terme2_g_SBM = terme2_g_SBM + delta$terme2_SBM
    }
  }


  delta_NSBM = terme2 + terme_nsbm_db_g+terme_nsbm_db_h
  delta_SBM= terme1_SBM +terme2_h_SBM + terme2_g_SBM

  delta=delta_NSBM+delta_SBM
  return(list(delta=delta, delta_NSBM=delta_NSBM, delta_SBM=delta_SBM))
}
