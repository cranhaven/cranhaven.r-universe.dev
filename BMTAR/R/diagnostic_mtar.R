#==================================================================================================#
# Date: 14/04/2020
# Description: Display some graphics for residuals analysis
# Function:
#==================================================================================================#
diagnostic_mtar = function(regime_model, lagmax = NULL, alpha = '0.05'){
  if (!requireNamespace('ggplot2', quietly = TRUE)) {
    stop('ggplot2 is needed for this function to work')
  }else {
    if (!inherits(regime_model, 'regime_model')) {
      stop('diagnostic.mtar requires a regime_model object')
    }}
  if (is.numeric(alpha)) {alpha = as.character(alpha)}
  if (!{alpha %in% c('0.10','0.05','0.025','0.01','0.005')}) {
      stop('alpha should take values in c(0.10,0.05,0.025,0.01,0.005)')
  }
  tablasq = data.frame('0.10' = c( 1.0729830,-0.6698868,-0.5816458),
             '0.05' = c(1.2238734,-0.6700069,-0.7351697),
             '0.025' = c(1.3581015,-0.6701218,-0.8858694),
             '0.01' = c(1.5174271,-0.6702672,-1.0847745),
             '0.005' = c(1.6276236,-0.6703724,-1.2365861))
  tablac = data.frame('0.10' = 0.850,'0.05' = 0.948,'0.025' = 1.036,'0.01' = 1.143,'0.005' = 1.217)
  e_k = tsregime(as.matrix(regime_model$residuals))
  p1 = autoplot.tsregime(e_k) + ggplot2::geom_hline(yintercept = 0,color = "red") +
    ggplot2::ggtitle('Residual serie plot')
  e_data = as.data.frame(e_k$Yt)
  time = seq(1,nrow(e_data))
  dat = data.frame(label = 'Series.1',time = time,value = e_data[,1],
                   cusum = cumsum(e_data[,1])/stats::sd(e_data[,1]),
                   cumsq = c(cumsum(e_data[,1]^2)/sum(e_data[,1]^2)))
  if (ncol(e_data) > 1) {
    for (i in 2:ncol(e_data)) {
      dat = rbind.data.frame(dat,data.frame(label = paste0('Series.',i),time = time,value = e_data[,i],
                                 cusum = cumsum(e_data[,i])/stats::sd(e_data[,i]),
                                 cumsq = c(cumsum(e_data[,i]^2)/sum(e_data[,i]^2))))
    }
  }
  p2 = ggplot2::ggplot(ggplot2::aes_(x = ~value, color = ~label),data = dat) +
    ggplot2::geom_density() + ggplot2::theme_bw()
  p2 = p2 + ggplot2::stat_function(fun = stats::dnorm,color = "black")
  p2 = p2 + ggplot2::ggtitle("Residual density plot")

  Af = c(tablac[,paste0('X',alpha)])
  LS = Af*sqrt(e_k$N) + 2*Af*c(1:e_k$N)/sqrt(e_k$N)
  LI = -LS
  p3 = ggplot2::ggplot(ggplot2::aes_(x = ~time, y = ~cusum,color = ~label),data = dat)
  p3 = p3 + ggplot2::geom_ribbon(ggplot2::aes(ymin = rep(LS,e_k$k), ymax = rep(LI,e_k$k)),
                                 fill = "gray",color = NA,alpha = 0.5)
  p3 = p3 + ggplot2::geom_line() + ggplot2::theme_bw() + ggplot2::ggtitle('CUSUM statistic for residuals')
# Tabla CusumSQ
  if (is.null(regime_model$data$nu)) {nu = 0}else{
    nu = regime_model$data$nu
  }
  k = regime_model$data$k
  if (is.null(regime_model$data$Zt)) {
    Ind = rep(1,regime_model$data$N)
  }else{
    Ind = lists_ind(regime_model$r[1],regime_model$data$Zt,length(regime_model$r[1]) + 1)$Ind
  }
  etaj = 1 + regime_model$orders$pj*k + regime_model$orders$qj*nu + regime_model$orders$dj
  ff = 1/2*(regime_model$data$N - etaj[Ind]) - 1
  co = 1/ff^(1/2)*tablasq[1,paste0('X',alpha)] + 1/ff*tablasq[2,paste0('X',alpha)] + 1/ff^(3/2)*tablasq[3,paste0('X',alpha)]
  LQS = co + (1:e_k$N)/e_k$N
  LQI = -co + (1:e_k$N)/e_k$N
  p4 = ggplot2::ggplot(ggplot2::aes_(x = ~time, y = ~cumsq,color = ~label),data = dat)
  p4 = p4 + ggplot2::geom_ribbon(ggplot2::aes(ymin = rep(LQS,e_k$k), ymax = rep(LQI,e_k$k)),
                                   fill = "gray",color = NA,alpha = 0.5)
  p4 = p4 + ggplot2::geom_line() + ggplot2::theme_bw() + ggplot2::ggtitle('CUSUMSQ statistic for residuals')

  acf_i = stats::acf(regime_model$residuals[,1],lag.max = lagmax,plot = FALSE,type = 'correlation')
  acf_Yt = data.frame(Lag = acf_i$lag, value = acf_i$acf,names = 'Serie.1',type = 'ACF')
  pacf_i = stats::acf(regime_model$residuals[,1],lag.max = lagmax,plot = FALSE,type = 'partial')
  pacf_Yt = data.frame(Lag = pacf_i$lag, value = pacf_i$acf,names = 'Serie.1',type = 'PACF')
  if (ncol(regime_model$residuals) > 1) {
    for (i in 2:ncol(regime_model$residuals)) {
      acf_i = stats::acf(regime_model$residuals[,i],lag.max = lagmax,plot = FALSE,type = 'correlation')
      acf_Yt = rbind(acf_Yt,data.frame(Lag = acf_i$lag + 0.1*i, value = acf_i$acf,names = paste0('Series.',i),type = 'ACF'))
      pacf_i = stats::acf(regime_model$residuals[,i],lag.max = lagmax,plot = FALSE,type = 'partial')
      pacf_Yt = rbind(pacf_Yt,data.frame(Lag = pacf_i$lag + 0.1*i, value = pacf_i$acf,names = paste0('Series.',i),type = 'PACF'))
    }
  }
  dat_cor = rbind.data.frame(acf_Yt,pacf_Yt)
  p5 = ggplot2::ggplot(ggplot2::aes_(x = ~Lag, y = ~value),data = dat_cor[floor(dat_cor$Lag) != 0,])
  p5 = p5 + ggplot2::geom_hline(yintercept = 0) + ggplot2::facet_grid(type~names)
  p5 = p5 + ggplot2::geom_segment(ggplot2::aes(xend = dat_cor[floor(dat_cor$Lag) != 0,]$Lag,yend = 0)) + ggplot2::geom_point(color = "blue",size = 0.4)
  ci = stats::qnorm((as.numeric(alpha))/2)/sqrt(nrow(regime_model$residuals))
  p5 = p5 + ggplot2::geom_ribbon(ggplot2::aes(ymax = ci ,ymin = -ci),color = NA,fill = "blue",alpha = 0.2)
  p5 = p5 + ggplot2::ggtitle('ACF and PACF plots for residuals series') + ggplot2::theme_bw()
  return(list(p1,p2,p3,p4,p5))
}
