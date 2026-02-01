cut_diff_ama <- function(V_kk,K_c,K,cutoff=0.01){

  ## -----------------------------------------------------------------------------------------------------------------
  ## The name of the function: cut_diff_ama
  ## -----------------------------------------------------------------------------------------------------------------
  ## Description:
  ##            Truncation of small differences in parameter estimates between subgroups.
  ## -----------------------------------------------------------------------------------------------------------------
  ## Required preceding functions or packages: No
  ## -----------------------------------------------------------------------------------------------------------------
  ## Input:
  ## @ V_kk: L2-norm differences in the precision matrices between different subgroups.
  ## @ K_c: All combinations of natural numbers within K.
  ## @ K: int, a selected upper bound of K_0.
  ## @ cutoff: a float value, a given cut-off value.
  ## -----------------------------------------------------------------------------------------------------------------
  ## Output:
  ## @ K_group_final: the partition of the original K subgroups.
  ## -----------------------------------------------------------------------------------------------------------------

  V_kk_num = which(V_kk < cutoff)
  K_group_final = list()
  if(length(V_kk_num) > 0){
    K_group = list()
    for (j in 1:length(V_kk_num)) {
      K_group[[j]] = K_c[,V_kk_num[j]]
    }
    outnum = setdiff(1:K,Reduce(union,K_group))
    if(length(outnum) > 0){
      for (j in 1:length(outnum)) {
        K_group[[length(V_kk_num)+j]] = outnum[j]
      }
      K_group[[length(V_kk_num)+j+1]] = K
    } else{
      K_group[[length(V_kk_num)+1]] = K
    }

    kk = 1
    repeat{
      repeat{
        K_group_old=K_group
        k_del = NULL
        for (kkk in setdiff(1:length(K_group),1) ) {
          if(length(Reduce(intersect,list(K_group[[1]],K_group_old[[kkk]]))) > 0){
            K_group[[1]] = sort(unique(c(K_group[[1]],K_group_old[[kkk]])))
            k_del = c(k_del,kkk)
          }
        }
        if(length(k_del) > 0){
          for (j in sort(k_del,decreasing = T)) {
            K_group[[j]] = NULL
          }
        }
        if(length(K_group_old) == length(K_group)){break}
      }
      K_group_final[[kk]] = K_group[[1]]
      if(kk==1 && length(K_group) == 1){
        # print("Warning: Only one cluster!")
        break}
      if(length(K_group) == 2){K_group_final[[kk+1]] = K_group[[2]];break}
      if(kk>1 && length(K_group) == 1){break}
      K_group[[1]] = NULL
      kk = kk+1
    }
  }else {
    for (k in 1:K) {
      K_group_final[[k]] = k
    }
  }

  return(K_group_final)
}
