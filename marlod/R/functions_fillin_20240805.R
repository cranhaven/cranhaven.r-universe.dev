#' @return A numeric vector
#' @export
#'
#' @examples
#' Fillin()

Fillin=function(y, lod, substitue)
{
  #Substitution method using LOD
  if(substitue=="None") cen_y <- y

  #Substitution method using LOD
  if(substitue=="LOD") cen_y <- ifelse(y>=lod,y,lod)

  #Substitution method using LOD/2
  if(substitue=="LOD2") cen_y <- ifelse(y>=lod,y,lod/2)

  #Substitution method using LOD/square root of 2
  if(substitue=="LODS2") cen_y <- ifelse(y>=lod,y,lod/sqrt(2))

  #Beta-substitution method using mean
  if(substitue=="BetaMean"){
    y1 <- y[which(y>=lod)]
    lny1 <- log(y1)
    y_bar <- mean(lny1)
    z <- qnorm((length(y)-length(y1))/length(y))
    f_z <- dnorm(qnorm((length(y)-length(y1))/length(y)))/(1-pnorm(qnorm((length(y)-length(y1))/length(y))))
    sy <- sqrt((y_bar-log((length(y)-length(y1))))^2/(dnorm(qnorm((length(y)-length(y1))/length(y)))/(1-pnorm(qnorm((length(y)-length(y1))/length(y))))-qnorm((length(y)-length(y1))/length(y)))^2)
    f_sy_z <- (1-pnorm(qnorm((length(y)-length(y1))/length(y))-sy/length(y)))/(1-pnorm(qnorm((length(y)-length(y1))/length(y))))
    beta_mean <- length(y)/(length(y)-length(y1))*pnorm(z-sy)*exp(-sy*z+(sy)^2/2)
    cen_y <- ifelse(y>=lod, y, lod*beta_mean)
  }
  #Beta-substitution method using geometric mean
  if(substitue=="BetaGM"){
    y1 <- y[which(y>=lod)]
    lny1 <- log(y1)
    y_bar <- mean(lny1)
    z <- qnorm((length(y)-length(y1))/length(y))
    f_z <- dnorm(qnorm((length(y)-length(y1))/length(y)))/(1-pnorm(qnorm((length(y)-length(y1))/length(y))))
    sy <- sqrt((y_bar-log((length(y)-length(y1))))^2/(dnorm(qnorm((length(y)-length(y1))/length(y)))/(1-pnorm(qnorm((length(y)-length(y1))/length(y))))-qnorm((length(y)-length(y1))/length(y)))^2)
    f_sy_z <- (1-pnorm(qnorm((length(y)-length(y1))/length(y))-sy/length(y)))/(1-pnorm(qnorm((length(y)-length(y1))/length(y))))
    beta_GM <- exp((-(length(y)-(length(y)-length(y1)))*length(y))/(length(y)-length(y1))*log(f_sy_z)-sy*z-(length(y)-(length(y)-length(y1)))/(2*(length(y)-length(y1))*length(y))*(sy)^2)
    cen_y <- ifelse(y>=lod, y, lod*beta_GM)
  }
  #Multiple imputation method using QQ-plot approach
  if(substitue=="QQplot"){
    y1 <- y[which(y>=lod)]
    lny1 <- log(y1)

    obs <- rank(y)
    rank <- (obs-0.5)/length(y)
    zscore0 <- qnorm(rank)*ifelse(y>=lod,1,0)
    zscore1 <- zscore0[which(zscore0!=0)]

    data_frame0=data.frame(cbind(lny1,zscore1))
    beta_est=as.matrix(glm(lny1~zscore1, data=data_frame0, family=gaussian)$coefficients)
    lny_zscore <- beta_est[1,1] + beta_est[2,1]*qnorm(rank)
    y_zscore <- exp(lny_zscore)
    cen_y <-ifelse(y>=lod,y,y_zscore)
  }
  cen_y
}
