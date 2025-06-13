#==================================================================================================#
# Date:
# Description:
#==================================================================================================#
auto_mtar = function(Yt, Zt = NULL, Xt = NULL, l0_min = 2, l0_max = 3,
                     maxorders = list(pj = 2,qj = 0,dj = 0),
                     niter = 3000, chain = FALSE, method = 'KUO', parallel = FALSE) {
  if (!is.logical(chain)) {stop('chain must be a logical object')}
  if (!is.list(maxorders) | length(maxorders) != 3) {
    stop('maxorders must be a list of length 3 list(pj, qj, dj)')
  }else if (!{all(names(maxorders) %in% c('pj','qj','dj'))}) {
    stop('maxorders must be a list of length 3 list(pj, qj, dj)')
  }
  data = tsregime(Yt,Zt,Xt)
  k = data$k
  nu = data$nu
  results = vector('list')
  if (is.null(nu)) {nu = 0}
  pjmax = maxorders$pj
  qjmax = maxorders$qj
  djmax = maxorders$dj
  if (any(is.na(data$Yt)) | any(is.na(data$Xt)) | any(is.na(data$Zt))) {
    data_temp = data
    meanY = apply(data_temp$Yt,2,mean,na.rm = T)
    nasy = apply(data_temp$Yt,2,is.na)
    meanZ = mean(data_temp$Zt,na.rm = T)
    if (nu > 1) {
      meanX = apply(data_temp$Yt,2,mean,na.rm = T)
      nasx = apply(data_temp$Xt,2,is.na)
    }else{
      meanX = mean(data_temp$Xt,na.rm = T)
    }
    for (i in 1:k) {
      data_temp$Yt[nasy[,i],i] = meanY[i]
    }
    data_temp$Zt[is.na(data_temp$Zt)] = meanZ
    if (nu > 1) {
      for (i in 1:nu) {
        data_temp$Xt[nasx[,i],i] = meanX[i]
      }
    }else{
      data_temp$Xt[is.na(data_temp$Xt)] = meanX
    }
    data_temp = tsregime(data_temp$Yt,data_temp$Zt,data_temp$Xt)
    initial = mtarinipars(tsregime_obj = data_temp,
                          list_model = list(l0_max = l0_max,l0_min = l0_min),method = method)
    numregest_1 = mtarnumreg(ini_obj = initial,niter_m = niter,NAIC = TRUE,
                             ordersprev = list(maxpj = pjmax,maxqj = qjmax,maxdj = djmax),parallel = parallel)
    l_1 = numregest_1$NAIC_final_m
    estrucopt = numregest_1$list_m[[paste0('m',l_1)]]$par
    initial = mtarinipars(tsregime_obj = data_temp,method = method,
                          list_model = list(pars = list(l = l_1),
                                            orders = list(pj = estrucopt$orders$pj,qj = estrucopt$orders$qj,dj = estrucopt$orders$dj)))
    est_1 = mtarstr(ini_obj = initial,niter = niter,chain = chain, parallel = parallel)
    initial = mtarinipars(tsregime_obj = data_temp,
                          list_model = list(pars = list(l = l_1,r = est_1$r,
                                                        orders = list(pj = est_1$orders$pj, qj = est_1$orders$qj,dj = est_1$orders$dj))))
    missingest = mtarmissing(ini_obj = initial,niter = niter)
    results$missing = missingest
    data_complete = missingest$tsregim
  }else{
    data_complete = data
  }
  initial = mtarinipars(tsregime_obj = data_complete,
                        list_model = list(l0_max = l0_max,l0_min = l0_min),method = method)
  numregest_final = mtarnumreg(ini_obj = initial,niter_m = niter,chain_m = chain,list_m = TRUE,
                               ordersprev = list(maxpj = pjmax, maxqj = qjmax, maxdj = djmax), parallel = parallel)
  lf = numregest_final$final_m
  estrucopt = numregest_final$list_m[[paste0('m',lf)]]$par
  initial = mtarinipars(tsregime_obj = data_complete,method = method,
                        list_model = list(pars = list(l = lf),
                                          orders = list(pj = estrucopt$orders$pj,
                                                        qj = estrucopt$orders$qj,dj = estrucopt$orders$dj)))
  est_final = mtarstr(ini_obj = initial,niter = niter,chain = chain,parallel = parallel)
  results$tsregim = data_complete
  results$numreg = numregest_final
  results$pars = est_final
  return(results)
}
