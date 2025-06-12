##################################################################################################
envelop <- function(y,x1, model,replicas=100){
  n        <- length(y)
  e        <- matrix(0,n,replicas)
  param    <- model
  G        <- length(param$result$pii)

  if(G==1){
    medij1   <- x1%*%param$result$betas + param$result$medj[1]
  }

  if(G==2){
    medij1   <- x1%*%param$result$betas + param$result$medj[1]
    medij2   <- x1%*%param$result$betas + param$result$medj[2]
  }

  if(G==3){
    medij1   <- x1%*%param$result$betas + param$result$medj[1]
    medij2   <- x1%*%param$result$betas + param$result$medj[2]
    medij3   <- x1%*%param$result$betas + param$result$medj[3]
  }

  if(G==4){
    medij1   <- x1%*%param$result$betas + param$result$medj[1]
    medij2   <- x1%*%param$result$betas + param$result$medj[2]
    medij3   <- x1%*%param$result$betas + param$result$medj[3]
    medij4   <- x1%*%param$result$betas + param$result$medj[4]
  }

  td       <- (y-x1%*%param$result$betas)  ### residuos do modelo

  nresp    <- c()
  i        <- 0

  #start.timeI      <- proc.time();
  while(i <= replicas)
  {

    if(G==1) for(j in 1:n) nresp[j] <- rmix.logbs(1,param$result$alpha,c(medij1[j]),param$result$pii)$y
    if(G==2) for(j in 1:n) nresp[j] <- rmix.logbs(1,param$result$alpha,c(medij1[j],medij2[j]),param$result$pii)$y
    if(G==3) for(j in 1:n) nresp[j] <- rmix.logbs(1,param$result$alpha,c(medij1[j],medij2[j],medij3[j]),param$result$pii)$y
    if(G==4) for(j in 1:n) nresp[j] <- rmix.logbs(1,param$result$alpha,c(medij1[j],medij2[j],medij3[j],medij4[j]),param$result$pii)$y


    valores_iniciales <- initial.values(nresp,x1,g=length(param$result$pii),algorithm="k-means")

    fit        <- try(EMmixlogbs(nresp, x1, valores_iniciales$alpha, valores_iniciales$Abetas, valores_iniciales$medj, valores_iniciales$pii, nu=3 ,g=length(param$result$pii), mfamily="Normal", accuracy = 10^-6, iter.max = 100),silent = TRUE)

    if(class(fit)!="try-error" && fit$result$convergence==TRUE)
    {
      i         <- i + 1
      if(i <= replicas)
      {
        e[,i]     <- sort((nresp-x1%*%fit$result$betas))
        print(i)
      }
    }
  }

  #end.timeF           <- proc.time() - start.timeI #Reset time
  #text                <- c("Total time",round(end.timeF[3]/60, digits=5),"minutes","e",round(end.timeF[3]/3600, digits=5),"hours")

  e1 <- c()
  e2 <- c()

  for(i in 1:n)
  {
    eo    <- sort(e[i,])
    e1[i] <- (eo[2]+eo[3])/2
    e2[i] <- (eo[97]+eo[98])/2
  }

  med    <- apply(e,1,mean)
  faixa  <- range(td,e1,e2)
  #postscript("envelopeFM_SHN_g2.eps", width=5.75, height=5.75, horizontal=FALSE, onefile=TRUE)
  par(pty="s")
  qqnorm(td,xlab="Standard Normal Quantiles", ylab="Residuals", ylim=faixa,main="FM-SHN-LR",pch=16)
  par(new=T)
  qqnorm(e1,axes=F,xlab="",ylab="",type="l",ylim=faixa,main="",lty=1)
  par(new=T)
  qqnorm(e2,axes=F,xlab="",ylab="", type="l",ylim=faixa,main="",lty=1)
  par(new=T)
  qqnorm(med,axes=F,xlab="", ylab="", type="l",ylim=faixa,main="",lty=2)
  #dev.off() #Fechando o dispositivo potscript
}

##################################################################################################
initial.values<- function(y,x1,g,algorithm="k-means")
{
  if (algorithm == "k-means")
  {
    if(length(g) == 0) stop("g is not specified correctly.\n")

    k.iter.max   <- 500
    n.start      <- 50
    algorithm    <- "Hartigan-Wong"

    if(g > 1){
      Abetas     <- solve(t(x1)%*%x1)%*%t(x1)%*%y;#print(Abetas)
      yr         <- y-x1%*%Abetas
      init       <- kmeans(yr,g,k.iter.max,n.start,algorithm)
      pii        <- init$size/length(yr)
      medj       <- as.vector(init$centers)
      alpha      <- rep(0,g)

      for(j in 1:g)
      {
        alpha[j] <- mmmethSN(y[init$cluster == j],medj[j])
      }
    } else{
      medj       <- mean(y)
      Abetas     <- solve(t(x1)%*%x1)%*%t(x1)%*%y#;print(Abetas)
      pii        <- 1
      alpha      <- mmmethSN(y,medj)
    }
  }

  ###################################################################################
  if (algorithm == "k-medoids")
  {
    if(length(g) == 0) {stop("g is not specified correctly.\n")}

    if(g > 1){
      Abetas       <- solve(t(x1)%*%x1)%*%t(x1)%*%y;print(Abetas)
      yr           <- y-x1%*%Abetas
      init         <- Cluster_Medoids(as.matrix(yr),clusters=g, distance_metric="euclidean", swap_phase = TRUE, fuzzy = TRUE)
      pii          <- init$medoid_indices/length(yr)
      alpha        <- rep(0,g)
      medj         <- as.vector(init$medoids)

      for(j in 1:g)
      {
        alpha[j]   <- mmmethSN(y[init$clusters == j],medj[j])
      }
    } else{
      medj       <- mean(y)
      Abetas     <- solve(t(x1)%*%x1)%*%t(x1)%*%y#;print(Abetas)
      pii        <- 1
      alpha      <- mmmethSN(y,medj)
    }
  }

  ###################################################################################

  object <- list(Abetas=Abetas,alpha=alpha,medj=medj,pii=pii)
}


#valores_iniciales = initial.values(y,x1,g,algorithm="k-means")


##################################################################################################

mmmethSN <- function(y, mu)
{
  n      <- length(y)
  Delta  <- (y - mu)/2
  alpha  <- sqrt((4/n)*((sum((sinh(Delta))^2))))
  return(alpha)
}

##################################################################################################

logbs.fdp <- function(y, alpha, mu)
{
  xi2 <- (2/alpha)*sinh((y-mu)/2)
  xi1 <- (2/alpha)*cosh((y-mu)/2)
  fdp <- 0.5*dnorm(xi2)*xi1
  return(fdp)
}

logbs.smn.fdp <- function(y, alpha, mu,nu,mfamily="Normal")
{
  if(mfamily=="Normal")
  {
    xi2 <- (2/alpha)*sinh((y-mu)/2)
    xi1 <- (2/alpha)*cosh((y-mu)/2)
    fdp <- 0.5*dnorm(xi2)*xi1
  }

  if(mfamily=="T")
  {
    xi2 <- (2/alpha)*sinh((y-mu)/2)
    xi1 <- (2/alpha)*cosh((y-mu)/2)
    knu <- gamma((nu+1)/2)/(sqrt(pi)*gamma(nu/2))
    fdp <- 0.5*knu*nu^(nu/2)*(nu + xi2^2)^(-(nu + 1)/2)*xi1
  }
  return(fdp)
}

logbs.t.fdp <- function(y, alpha, mu,nu)
{
  xi2 <- (2/alpha)*sinh((y-mu)/2)
  xi1 <- (2/alpha)*cosh((y-mu)/2)
  knu <- gamma((nu+1)/2)/(sqrt(pi)*gamma(nu/2))
  fdp <- 0.5*knu*nu^(nu/2)*(nu + xi2^2)^(-(nu + 1)/2)*xi1
  return(fdp)
}

logbs.fdp2 <-
  function(
    y, # evaluation points
    m, # mode parameter
    h
    )
{
  alpha <- 2*sinh((m-h)/2)
  xi2   <- (2/alpha)*sinh((y-h)/2)
  xi1   <- (2/alpha)*cosh((y-h)/2)
  fdp   <- 0.5*dnorm(xi2)*xi1
  return(fdp)
}

d.mixed.logbs <- function(y, pii, alpha, mu)
{
  # x: vetor de dados
  # outros parametros devem ser do tipo vetor c() de dimensao g (qtd de misturas)
  g      <- length(pii)
  dens   <- 0
  for (j in 1:g) dens <- dens + pii[j]*logbs.fdp(y, alpha[j], mu[,j])
  return(dens)
}

d.mixed.logbs.smn <- function(y, pii, alpha, mu,nu,mfamily="Normal")
{
  # x: vetor de dados
  # outros parametros devem ser do tipo vetor c() de dimensao g (qtd de misturas)
  g      <- length(pii)
  dens   <- 0
  for (j in 1:g) dens <- dens + pii[j]*logbs.smn.fdp(y, alpha[j], mu[,j],nu,mfamily)
  return(dens)
}

d.mixed.logbs.t <- function(y, pii, alpha, mu,nu,mfamily="Normal")
{
  # x: vetor de dados
  # outros parametros devem ser do tipo vetor c() de dimensao g (qtd de misturas)
  g      <- length(pii)
  dens   <- 0
  for (j in 1:g) dens <- dens + pii[j]*logbs.t.fdp(y, alpha[j], mu[,j],nu)
  return(dens)
}


##################################################################################################

gen.erro.logbs<-function(n, alpha, mu)
{
  z <- rnorm(n)
  y <- mu+2*asinh(alpha*z*0.5)
  return(y)
}

gen.erro.logbs.smn <- function(n, alpha, mu,nu,mfamily="Normal")
{
  if(mfamily=="Normal")
  {
    z <- rnorm(n)
    y <- mu+2*asinh(alpha*z*0.5)
  }

  if(mfamily=="T")
  {
    z <- rnorm(n)
    u <- rgamma(n,nu/2,nu/2)
    y <- mu+2*asinh(alpha*z*u*0.5)
  }
  return(y)
}

#gen.erro.logbs.smn(n, alpha, nu,mfamily="T")

rmix.logbs = function(n,alpha,mu,pii)
{
  as.numeric(Sys.time())-> t; set.seed((t - floor(t)) * 1e8 -> seed)
  y <- vector()
  G <- length(alpha)

  z <- sample(G,size=n,replace=TRUE,prob=pii)
  for(i in 1:n)
  {
    y[i] = gen.erro.logbs(1, alpha[z[i]], mu[z[i]])
  }
  return(list(z=z,y=y))
}

rmix.logbs.smn = function(n,alpha,mu,pii,nu,mfamily="Normal")
{
  as.numeric(Sys.time())-> t; set.seed((t - floor(t)) * 1e8 -> seed)
  y <- vector()
  G <- length(alpha)

  z <- sample(G,size=n,replace=TRUE,prob=pii)
  for(i in 1:n)
  {
    y[i] = gen.erro.logbs.smn(1, alpha[z[i]], mu[z[i]],nu,mfamily)
  }
  return(list(z=z,y=y))
}

##################################################################################################
maxbeta.logbs <- function(y, pii, alpha, betas, varphi, x, zij)
{
  n      <- length(y)
  g      <- length(pii)

  if(g==1)
  {
    xi2 <- (2/alpha[1])*sinh((y-x%*%betas-varphi[1])/2)
    xi1 <- (2/alpha[1])*cosh((y-x%*%betas-varphi[1])/2)
    Q      <- sum(zij[,1]*log(xi1)) - 0.5*sum(zij[,1]*xi2^2)
  }

  if(g==2)
  {
    xi2_1 <- (2/alpha[1])*sinh((y-x%*%betas-varphi[1])/2)
    xi1_1 <- (2/alpha[1])*cosh((y-x%*%betas-varphi[1])/2)
    xi2_2 <- (2/alpha[2])*sinh((y-x%*%betas-varphi[2])/2)
    xi1_2 <- (2/alpha[2])*cosh((y-x%*%betas-varphi[2])/2)
    Q      <-  sum(zij[,1]*log(pii[1])) + sum(zij[,1]*log(xi1_1)) - 0.5*sum(zij[,1]*xi2_1^2)+
               sum(zij[,2]*log(pii[2])) + sum(zij[,2]*log(xi1_2)) - 0.5*sum(zij[,2]*xi2_2^2)
  }

  if(g==3)
  {
    xi2_1 <- (2/alpha[1])*sinh((y-x%*%betas-varphi[1])/2)
    xi1_1 <- (2/alpha[1])*cosh((y-x%*%betas-varphi[1])/2)
    xi2_2 <- (2/alpha[2])*sinh((y-x%*%betas-varphi[2])/2)
    xi1_2 <- (2/alpha[2])*cosh((y-x%*%betas-varphi[2])/2)
    xi2_3 <- (2/alpha[3])*sinh((y-x%*%betas-varphi[3])/2)
    xi1_3 <- (2/alpha[3])*cosh((y-x%*%betas-varphi[3])/2)
    Q     <-  sum(zij[,1]*log(pii[1])) + sum(zij[,1]*log(xi1_1)) - 0.5*sum(zij[,1]*xi2_1^2)+
              sum(zij[,2]*log(pii[2])) + sum(zij[,2]*log(xi1_2)) - 0.5*sum(zij[,2]*xi2_2^2)+
              sum(zij[,3]*log(pii[3])) + sum(zij[,3]*log(xi1_3)) - 0.5*sum(zij[,3]*xi2_3^2)
  }

  if(g==4)
  {
    xi2_1 <- (2/alpha[1])*sinh((y-x%*%betas-varphi[1])/2)
    xi1_1 <- (2/alpha[1])*cosh((y-x%*%betas-varphi[1])/2)
    xi2_2 <- (2/alpha[2])*sinh((y-x%*%betas-varphi[2])/2)
    xi1_2 <- (2/alpha[2])*cosh((y-x%*%betas-varphi[2])/2)
    xi2_3 <- (2/alpha[3])*sinh((y-x%*%betas-varphi[3])/2)
    xi1_3 <- (2/alpha[3])*cosh((y-x%*%betas-varphi[3])/2)
    xi2_4 <- (2/alpha[4])*sinh((y-x%*%betas-varphi[4])/2)
    xi1_4 <- (2/alpha[4])*cosh((y-x%*%betas-varphi[4])/2)
    Q     <-  sum(zij[,1]*log(pii[1])) + sum(zij[,1]*log(xi1_1)) - 0.5*sum(zij[,1]*xi2_1^2)+
              sum(zij[,2]*log(pii[2])) + sum(zij[,2]*log(xi1_2)) - 0.5*sum(zij[,2]*xi2_2^2)+
              sum(zij[,3]*log(pii[3])) + sum(zij[,3]*log(xi1_3)) - 0.5*sum(zij[,3]*xi2_3^2)+
              sum(zij[,4]*log(pii[4])) + sum(zij[,4]*log(xi1_4)) - 0.5*sum(zij[,4]*xi2_4^2)
  }

  if(g==5)
  {
    xi2_1 <- (2/alpha[1])*sinh((y-x%*%betas-varphi[1])/2)
    xi1_1 <- (2/alpha[1])*cosh((y-x%*%betas-varphi[1])/2)
    xi2_2 <- (2/alpha[2])*sinh((y-x%*%betas-varphi[2])/2)
    xi1_2 <- (2/alpha[2])*cosh((y-x%*%betas-varphi[2])/2)
    xi2_3 <- (2/alpha[3])*sinh((y-x%*%betas-varphi[3])/2)
    xi1_3 <- (2/alpha[3])*cosh((y-x%*%betas-varphi[3])/2)
    xi2_4 <- (2/alpha[4])*sinh((y-x%*%betas-varphi[4])/2)
    xi1_4 <- (2/alpha[4])*cosh((y-x%*%betas-varphi[4])/2)
    xi2_5 <- (2/alpha[5])*sinh((y-x%*%betas-varphi[5])/2)
    xi1_5 <- (2/alpha[5])*cosh((y-x%*%betas-varphi[5])/2)
    Q     <-  sum(zij[,1]*log(pii[1])) + sum(zij[,1]*log(xi1_1)) - 0.5*sum(zij[,1]*xi2_1^2)+
              sum(zij[,2]*log(pii[2])) + sum(zij[,2]*log(xi1_2)) - 0.5*sum(zij[,2]*xi2_2^2)+
              sum(zij[,3]*log(pii[3])) + sum(zij[,3]*log(xi1_3)) - 0.5*sum(zij[,3]*xi2_3^2)+
              sum(zij[,4]*log(pii[4])) + sum(zij[,4]*log(xi1_4)) - 0.5*sum(zij[,4]*xi2_4^2)+
              sum(zij[,5]*log(pii[5])) + sum(zij[,5]*log(xi1_5)) - 0.5*sum(zij[,5]*xi2_5^2)
  }
  return(-Q)
}


##################################################################################################
im.FMlogbs     <- function(y, x1, alpha, betas, medj,  pii)
{
  #alpha =  fit.mix.logbs$result$alpha
  #betas = fit.mix.logbs$result$betas
  #medj = fit.mix.logbs$result$medj
  #pii = fit.mix.logbs$result$pii

  #Definiendo os parametros
  g              <- length(pii)
  n              <- length(y) #Tamanho de amostra
  y              <- as.numeric(y)
  Abetas         <- betas
  #########################################
  #Parametros estimador pelo Algoritmo EM
  beta0          <- Abetas[1]
  betas          <- as.matrix(Abetas[2:(length(Abetas))])   # parameters of regression dimension "p"
  x              <- as.matrix(x1[,2:(length(Abetas))])

  ##########################################Definiendo os parametros
  n              <- length(y) #Tamanho de amostra

  mu=mu1         <- matrix(0,n,g)
  varphi         <- rep(0,g) # beta0 + mu_j #Parametrizacao
  for (k in 1:g)
  {
    varphi[k]     <- beta0+medj[k]
    mu1[,k]      <- x%*%betas
    mu[,k]       <- mu1[,k]+varphi[k]
  }

  #Verosimilhanca da mixtura da Skew.T
  lk             <- d.mixed.logbs(y, pii, alpha, mu)

  ################################
  #Definiendo os objetos Si
  Sibetas       <- matrix(0,nrow=length(betas),ncol=n)
  Sivarphi      <- matrix(0,g,n)
  Sialpha       <- matrix(0,g,n)
  Sipi          <- matrix(0,g-1,n)
  xi2           <- matrix(0,g,n)
  xi1           <- matrix(0,g,n)
  ################################

  #Nesta parte estou obtenido Sibetas onde considero "beta1", "beta2", pois na derivada respecto de "varnu" ja esta incluso o "beta0"
  #pelo que a derivada respecto a "beta" seria sem considera "beta0"
  #Lembrar que para cada "i" teremos a suma das "g" componentes
  for(i in 1:n)
  {
    #Para betas
    for(j in 1:g)
    {
      xi2[j,i]      <- (2/alpha[j])*sinh((y[i]-x[i,]%*%betas-varphi[j])/2)
      xi1[j,i]      <- (2/alpha[j])*cosh((y[i]-x[i,]%*%betas-varphi[j])/2)
      Sibetas[,i]   <- Sibetas[,i]   + (0.25*dnorm(xi2[j,i])*xi2[j,i]*(xi1[j,i]^2-1)*x[i,])*(pii[j]/lk[i])
      Sivarphi[j,i] <- Sivarphi[j,i] + (0.25*dnorm(xi2[j,i])*xi2[j,i]*(xi1[j,i]^2-1))*(pii[j]/lk[i])
      Sialpha[j,i]  <- Sialpha[j,i]  + ((0.5/alpha[j])*dnorm(xi2[j,i])*xi1[j,i]*(xi2[j,i]^2-1))*(pii[j]/lk[i])
    }
    for(j in 1:(g-1)) Sipi[j,i]  <- (logbs.fdp(y[i], alpha[j], mu[i,j]) - logbs.fdp(y[i], alpha[g], mu[i,g]))/lk[i]
  }

  #Nesta parte vamos obter os demais Si, onde tambem es considerado o Sibetas (de tres componentes, pois temos 3 variaveis que incluye o intercepto)
  soma           <- 0
  for(i in 1:n)
  {
    S            <- c(Sialpha[,i],Sipi[,i],Sibetas[,i],Sivarphi[,i])
    soma         <- soma + S%*%t(S)
  }

  EP1               <-  sqrt(diag(solve(soma)))
  IM1               <- soma
  #Jacobiano
  p      <- length(betas)

  j11    <- diag(rep(1,g))
  j12    <- matrix(0, nrow=g, ncol=g-1)
  j13    <- matrix(0, nrow=g, ncol=p)
  j14    <- matrix(0, nrow=g, ncol=g)
  J1     <- cbind(j11,j12,j13,j14)

  j21    <- matrix(0, nrow=g-1, ncol=g)
  j22    <- diag(rep(1,g-1))
  j22[lower.tri(j22)] <- -1
  j22[upper.tri(j22)] <- -1
  j23    <- matrix(0, nrow=g-1, ncol=p)
  j24    <- matrix(0, nrow=g-1, ncol=g)
  J2     <- cbind(j21,j22,j23,j24)

  j31    <- matrix(0, nrow=1, ncol=g)
  j32    <- matrix(0, nrow=1, ncol=g-1)
  j33    <- matrix(0, nrow=1, ncol=p)
  j34    <- matrix(1, nrow=1, ncol=g)
  J3     <- cbind(j31,j32,j33,j34)

  j41    <- matrix(0, nrow=p, ncol=g)
  j42    <- matrix(0, nrow=p, ncol=g-1)
  j43    <- diag(rep(1,p))
  j44    <- matrix(0, nrow=p, ncol=g)
  J4     <- cbind(j41,j42,j43,j44)

  j51    <- matrix(0, nrow=g, ncol=g)
  j52    <- -as.matrix((medj[1:(g-1)] - medj[g])^(-1))%*%t(as.matrix(pii))
  j53    <- matrix(0, nrow=g, ncol=p)
  j54    <- diag(rep(1,g))
  J5     <- cbind(j51,j52,j53,j54)

  Jacobian       <- rbind(J1,J2,J3,J4,J5)
  IM             <- Jacobian%*%solve(soma)%*%t(Jacobian)
  EP             <- as.matrix(sqrt(diag(IM)))

  #namesrowBetas  <- c(); for(i in 1:length(betas)){namesrowBetas[i]  <- paste("beta",i,sep="")}
  #namesrowMedj   <- c(); for(i in 1:g)            {namesrowMedj[i]   <- paste("mu",i,sep="")}
  #namesrowSigmas <- c(); for(i in 1:g)            {namesrowSigmas[i] <- paste("sigma",i,sep="")}
  #namesrowShape  <- c(); for(i in 1:g)            {namesrowShape[i]  <- paste("shape",i,sep="")}
  #namesrowPii    <- c(); for(i in 1:(g-1))        {namesrowPii[i]    <- paste("pii",i,sep="")}
  #rownames(EP)   <- c(namesrowBetas,namesrowPii,"beta0",namesrowMedj,namesrowSigmas,namesrowShape)
  #colnames(EP)   <- c("SE")

  #colnames(IM)   <- c(namesrowBetas,namesrowPii,"beta0",namesrowMedj,namesrowSigmas,namesrowShape)
  #rownames(IM)   <- c(namesrowBetas,namesrowPii,"beta0",namesrowMedj,namesrowSigmas,namesrowShape)

  return(list(IM=IM,EP=EP,IM1=IM,EP1=EP1))
}

#im.FMlogbs2(y, x, alpha, betas, medj,  pii)




