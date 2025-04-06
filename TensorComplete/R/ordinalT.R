#### cost function based on unfolded matrix
h1 = function(A_1,W1,ttnsr,omega,type="ordinal"){
  k = length(omega)
  theta_output =W1%*%c(A_1)
  if(type=="Gaussian"){
    l=sqrt(sum((theta_output[is.na(ttnsr)==F]-ttnsr[is.na(ttnsr)==F])^2))
    return(l)
  }
  p = theta_to_p(theta_output,omega)
  l = lapply(1:(k+1),function(i) -log(p[which(c(ttnsr)==i),i]))
  return(sum(unlist(l)))
}

#### cost function based on Tucker representation
hc = function(A_1,A_2,A_3,C,ttnsr,omega,type="ordinal"){
  k = length(omega)
  theta_output = c(ttl(C,list(A_1,A_2,A_3),ms=1:3)@data)
  if(type=="Gaussian"){
    l=sqrt(sum((theta_output[is.na(ttnsr)==F]-ttnsr[is.na(ttnsr)==F])^2))
    return(l)
  }
  p = theta_to_p(theta_output,omega)
  l = lapply(1:(k+1),function(i) -log(p[which(c(ttnsr)==i),i]))
  return(sum(unlist(l)))
}

#### gradient function based on unfolded matrix
g1 = function(A_1,W,ttnsr,omega,type="ordinal"){
  k = length(omega)
  theta_output =W%*%c(A_1)
  if(type=="Gaussian"){
    pretheta=theta_output-as.matrix(ttnsr)
    pretheta[is.na(pretheta)==T]=0
    l=2*t(pretheta)%*%W
    return(l)
  }
  p = matrix(nrow = length(theta_output),ncol = k)
  for (i in 1:k) {
    p[,i] = as.numeric(logistic(theta_output + omega[i]))
  }
  q = matrix(nrow = length(theta_output),ncol = k+1)
  q[,1] <- p[,1]-1
  if(k>=2){
    for (i in 2:k) {
      q[,i] <- p[,i]+p[,i-1]-1
    }
  }
  q[,k+1] <- p[,k]
  l <- Reduce("+",lapply(1:(k+1),function(i) apply(rbind(W[which(c(ttnsr)==i),,drop=F])*q[which(c(ttnsr)==i),i],2,sum)))
  return(l)
}

#### gradient function with respect to theta
gradient_tensor=function(A_1,A_2,A_3,C,ttnsr,omega,type="ordinal"){
  k = length(omega)
  theta_output = c(ttl(C,list(A_1,A_2,A_3),ms=1:3)@data)
  if(type=="Gaussian"){
    pretheta=theta_output-ttnsr
    pretheta[is.na(pretheta)==T]=0
    l=2*pretheta
    return(l)
  }
  p = matrix(nrow = length(theta_output),ncol = k)
  for (i in 1:k) {
    p[,i] = as.numeric(logistic(theta_output + omega[i]))
  }
  q = matrix(nrow = length(theta_output),ncol = k+1)
  q[,1] <- p[,1]-1
  if(k>=2){
    for (i in 2:k) {
      q[,i] <- p[,i]+p[,i-1]-1
    }
  }
  q[,k+1] <- p[,k]
  output=ttnsr
  for(i in 1:(k+1)){
    output[which(c(ttnsr)==i)]=q[which(c(ttnsr)==i),i]
  }
  return(output)
}

#### gradient function with respect to the core tensor (gradient not using kronecker product at all)
gc = function(A_1,A_2,A_3,C,ttnsr,omega,type="ordinal"){
  g = gradient_tensor(A_1,A_2,A_3,C,ttnsr,omega,type) ## first, take entrywise gradient w.r.t. theta

  g = ttl(as.tensor(g),list(t(A_1),t(A_2),t(A_3)),ms = 1:3)@data ## then, take entrywise gradient w.r.t. core tensor

  return(g)
}

#### update one factor matrix at a time while holding others fixed
comb = function(A,W,ttnsr,k,omega,alpha=TRUE,type="ordinal"){
  nA = A
  tnsr1 <- k_unfold(as.tensor(ttnsr),k)@data
  if (alpha==TRUE) {
    l <- lapply(1:nrow(A),function(i){optim(A[i,],
                                            function(x) h1(x,W,tnsr1[i,],omega,type),
                                            function(x) g1(x,W,tnsr1[i,],omega,type),method = "BFGS")$par})
    nA <- matrix(unlist(l),nrow = nrow(A),byrow = T)
  }else{
    l <- lapply(1:nrow(A),function(i){constrOptim(A[i,],
                                                  function(x) h1(x,W,tnsr1[i,],omega,type),
                                                  function(x) g1(x,W,tnsr1[i,],omega,type),
                                                  ui = as.matrix(rbind(W,-W)),ci = rep(-alpha,2*nrow(W)),method = "BFGS")$par})
    nA <- matrix(unlist(l),nrow = nrow(A),byrow = T)
  }
  return(nA)
}

#### update core tensor holding other factors fixed
corecomb = function(A_1,A_2,A_3,C,ttnsr,omega,alpha=TRUE,type="ordinal"){
  h <- function(x) hc(A_1,A_2,A_3,new("Tensor",C@num_modes,C@modes,data = x),ttnsr,omega,type)
  g <- function(x) c(gc(A_1,A_2,A_3,new("Tensor",C@num_modes,C@modes,data = x),ttnsr,omega,type))
  d <- optim(c(C@data),h,g,method="BFGS")
  C <- new("Tensor",C@num_modes,C@modes,data =d$par)

  return(C)
}


#' Main function for parametric tensor estimation and completion based on ordinal observations.
#'
#' Estimate a signal tensor from a noisy and incomplete ordinal-valued tensor using the cumulative logistic model.
#' @param ttnsr A given (possibly noisy and incomplete) data tensor. The function allows binary- and ordinal-valued tensors. Missing value should be encoded as \code{NA}.
#' @param r A rank to be fitted (Tucker rank).
#' @param omega The cut-off points if known,
#'
#' \code{omega = TRUE} if unknown.
#' @param alpha A signal level
#'
#' \code{alpha = TRUE} if the signal level is unknown.
#' @return A list containing the following:
#' @return \code{C} - An estimated core tensor.
#' @return \code{A} - Estimated factor matrices.
#' @return \code{theta} - An estimated latent parameter tensor.
#' @return \code{iteration} - The number of iterations.
#' @return \code{cost} - Log-likelihood value at each iteration.
#' @return \code{omega} - Estimated cut-off points.
#' @usage fit_ordinal(ttnsr,r,omega=TRUE,alpha = TRUE)
#' @references C. Lee and M. Wang. Tensor denoising and completion based on ordinal observations. \emph{International Conference on Machine Learning (ICML)}, 2020.
#' @examples
#' # Latent parameters
#' library(tensorregress)
#' alpha = 10
#' A_1 = matrix(runif(10*2,min=-1,max=1),nrow = 10)
#' A_2 = matrix(runif(10*2,min=-1,max=1),nrow = 10)
#' A_3 = matrix(runif(10*2,min=-1,max=1),nrow = 10)
#' C = as.tensor(array(runif(2^3,min=-1,max=1),dim = c(2,2,2)))
#' theta = ttm(ttm(ttm(C,A_1,1),A_2,2),A_3,3)@data
#' theta = alpha*theta/max(abs(theta))
#' adj = mean(theta)
#' theta = theta-adj
#' omega = c(-0.2,0.2)+adj
#'
#' # Observed tensor
#' ttnsr <- realization(theta,omega)@data
#'
#' # Estimation of parameters
#' ordinal_est = fit_ordinal(ttnsr,c(2,2,2),omega = TRUE,alpha = 10)
#'
#' @export
#' @import tensorregress
#' @import MASS
#' @importFrom methods "new"
#' @importFrom stats "constrOptim" "optim"

fit_ordinal = function(ttnsr,r,omega=TRUE,alpha = TRUE){
  if(length(r) != length(dim(ttnsr))) stop("the rank is not valid")
  d = dim(ttnsr)
  r = r-1
  A_1 = as.matrix(randortho(d[1])[,1:r[1]])
  A_2 = as.matrix(randortho(d[2])[,1:r[2]])
  A_3 = as.matrix(randortho(d[3])[,1:r[3]])
  C = rand_tensor(modes = r)
  C = C*ifelse(is.logical(alpha),1,
               1/max(abs(ttl(C,list(A_1,A_2,A_3),ms=1:3)@data))*alpha/10 )
  ## initial theta is in the interior

  if(is.logical(alpha)) alpha_minus=alpha_minus2=TRUE
  else{
    alpha_minus=alpha-epsilon
    alpha_minus2=alpha-2*epsilon
    prevtheta <- ttl(C,list(A_1,A_2,A_3),ms=1:3)@data
    if(max(abs(prevtheta))>alpha){
      warning("the input tensor exceeds the magnitude bound. Perform rescaling on the core tensor...")
      C=C/max(abs(prevtheta))*alpha/10
    }
  }

  result = list()
  error<- 3
  iter = 0
  cost=NULL
  omg = omega
  k=length(unique(as.factor(c(ttnsr))))-is.element(NA,ttnsr) ## for NA not being included in length
  d=dim(ttnsr)
  ttnsr=array(as.numeric(as.factor(ttnsr)),dim=d) ## code labels from 1 to K

  while ((error > 10^-4)&(iter<10) ) {
    iter = iter +1
    #update omega
    prevtheta <- ttl(C,list(A_1,A_2,A_3),ms=1:3)@data

    currentomega=omega
    if(is.logical(omg)) {
      omega=tryCatch(polr(as.factor(c(ttnsr))~offset(-c(prevtheta)))$zeta,error=function(c)"omega cannot be reliably estimated",warning=function(c)"omega cannot be reliably estimated")
      if(inherits(omega,"numeric")==FALSE){
        warning("omega cannot be estimated! Omega from previous step is used");
        if(iter==1) omega=pracma::logit(1:(k-1)/k)
        else omega=currentomega
      }
      currentomega=omega
    }


    prev <- likelihood(ttnsr,prevtheta,omega,type = "ordinal")


    #update A_1
    W <-kronecker(A_3,A_2)%*%t(k_unfold(C,1)@data)
    A_1<- comb(A_1,W,ttnsr,1,omega,alpha_minus2)
    #orthognalize A_1
    qr_res=qr(A_1)
    A_1=qr.Q(qr_res)
    C=ttm(C,qr.R(qr_res),1)

    # update A_2
    W <- kronecker(A_3,A_1)%*%t(k_unfold(C,2)@data)
    A_2 <- comb(A_2,W,ttnsr,2,omega,alpha_minus)
    #orthognalize A_2
    qr_res=qr(A_2)
    A_2=qr.Q(qr_res)
    C=ttm(C,qr.R(qr_res),2)

    # update A_3
    W <- kronecker(A_2,A_1)%*%t(k_unfold(C,3)@data)
    A_3 <- comb(A_3,W,ttnsr,3,omega,alpha)
    #orthognalize A_3
    qr_res=qr(A_3)
    A_3=qr.Q(qr_res)
    C=ttm(C,qr.R(qr_res),3)

    # update C
    C_update <- corecomb(A_1,A_2,A_3,C,ttnsr,omega)

    if(!is.logical(alpha) & max(abs(ttl(C_update,list(A_1,A_2,A_3),ms=1:3)@data))>=alpha_minus2){
      theta <- ttl(C,list(A_1,A_2,A_3),ms=1:3)@data
      new <- likelihood(ttnsr,theta,omega,type ="ordinal")
      cost = c(cost,new); break
    }else{
      C=C_update
      theta <- ttl(C,list(A_1,A_2,A_3),ms=1:3)@data
      new <- likelihood(ttnsr,theta,omega,type ="ordinal")
      cost = c(cost,new)
      (error <- abs((new-prev)/prev))
    }
    message(paste(iter,"-th  iteration -- cost value is",new," -----------------"))

  }
  # identifiability adjustment
  madj <- mean(theta)
  theta <- theta -madj
  final <- hosvd(as.tensor(theta),r+1)

  result$C <- final$Z; result$A <- final$U
  result$theta= theta
  result$iteration <- iter
  result$omega=omega+madj
  result$cost = cost
  return(result)
}


#' Signal tensor estimation from a noisy and incomplete data tensor based on the Tucker model.
#'
#' Estimate a signal tensor from a noisy and incomplete data tensor using the Tucker model.
#' @param ttnsr A given (possibly noisy and incomplete) data tensor.
#' @param r A rank to be fitted (Tucker rank).
#' @param alpha A signal level
#'
#' \code{alpha = TRUE} If the signal level is unknown.
#' @return A list containing the following:
#' @return \code{C} - An estimated core tensor.
#' @return \code{A} - Estimated factor matrices.
#' @return \code{iteration} - The number of iterations.
#' @return \code{cost} - Log-likelihood value at each iteration.
#' @usage fit_continuous_tucker(ttnsr,r,alpha = TRUE)
#' @examples
#' # Latent parameters
#' library(tensorregress)
#' alpha = 10
#' A_1 = matrix(runif(10*2,min=-1,max=1),nrow = 10)
#' A_2 = matrix(runif(10*2,min=-1,max=1),nrow = 10)
#' A_3 = matrix(runif(10*2,min=-1,max=1),nrow = 10)
#' C = as.tensor(array(runif(2^3,min=-1,max=1),dim = c(2,2,2)))
#' theta = ttm(ttm(ttm(C,A_1,1),A_2,2),A_3,3)@data
#' theta = alpha*theta/max(abs(theta))
#' adj = mean(theta)
#' theta = theta-adj
#' omega = c(-0.2,0.2)+adj
#'
#' # Observed tensor
#' ttnsr <- realization(theta,omega)@data
#'
#' # Estimation of parameters
#' continuous_est = fit_continuous_tucker(ttnsr,c(2,2,2),alpha = 10)
#'
#' @export
#' @import tensorregress
#' @import MASS
#' @importFrom pracma "randortho"
#' @importFrom methods "new"
#' @importFrom stats "constrOptim" "optim"
fit_continuous_tucker=function(ttnsr,r,alpha = TRUE){
  if(length(r) != length(dim(ttnsr))) stop("the rank is not valid")
  d = dim(ttnsr)
  A_1 = as.matrix(randortho(d[1])[,1:r[1]])
  A_2 = as.matrix(randortho(d[2])[,1:r[2]])
  A_3 = as.matrix(randortho(d[3])[,1:r[3]])
  C = rand_tensor(modes = r)
  C = C*ifelse(is.logical(alpha),1,
               1/max(abs(ttl(C,list(A_1,A_2,A_3),ms=1:3)@data))*alpha/10 )
  ## initial theta is in the interior
  if(is.logical(alpha)) alpha_minus=alpha_minus2=TRUE
  else{
    alpha_minus=alpha-epsilon
    alpha_minus2=alpha-2*epsilon
    prevtheta <- ttl(C,list(A_1,A_2,A_3),ms=1:3)@data
    if(max(abs(prevtheta))>alpha){
      warning("the input tensor exceeds the magnitude bound. Perform rescaling on the core tensor...")
      C=C/max(abs(prevtheta))*alpha/10
    }
  }

  result = list()
  error<- 3
  iter = 0
  cost=NULL
  omega=0

  while ((error > 10^-4)&(iter<10) ) {
    iter = iter +1

    prevtheta <- ttl(C,list(A_1,A_2,A_3),ms=1:3)@data
    prev <- likelihood(ttnsr,prevtheta,omega,type="Gaussian")



    W <-kronecker(A_3,A_2)%*%t(k_unfold(C,1)@data)
    A_1 <- comb(A_1,W,ttnsr,1,omega,alpha= alpha_minus2,type="Gaussian")
    #orthognalize A_1
    qr_res=qr(A_1)
    A_1=qr.Q(qr_res)
    C=ttm(C,qr.R(qr_res),1)

    # update A_2
    W <- kronecker(A_3,A_1)%*%t(k_unfold(C,2)@data)
    A_2<- comb(A_2,W,ttnsr,2,omega,alpha= alpha_minus,type="Gaussian")
    #orthognalize A_2
    qr_res=qr(A_2)
    A_2=qr.Q(qr_res)
    C=ttm(C,qr.R(qr_res),2)

    # update A_3
    W <- kronecker(A_2,A_1)%*%t(k_unfold(C,3)@data)
    A_3 <- comb(A_3,W,ttnsr,3,omega,alpha= alpha,type="Gaussian")
    #orthognalize A_3
    qr_res=qr(A_3)
    A_3=qr.Q(qr_res)
    C=ttm(C,qr.R(qr_res),3)

    C_update <- corecomb(A_1,A_2,A_3,C,ttnsr,omega,type="Gaussian")

    if(!is.logical(alpha) & max(abs(ttl(C_update,list(A_1,A_2,A_3),ms=1:3)@data))>=alpha_minus2){
      theta <- ttl(C,list(A_1,A_2,A_3),ms=1:3)@data
      new <- likelihood(ttnsr,theta,omega,type="Gaussian")
      cost = c(cost,new); break
    }else{
      C=C_update
      theta <- ttl(C,list(A_1,A_2,A_3),ms=1:3)@data
      new <- likelihood(ttnsr,theta,omega,type="Gaussian")
      cost = c(cost,new)
      (error <- abs((new-prev)/prev))
    }
    message(paste(iter,"-th  iteration -- cost value is",new," -----------------"))

  }


  result$C <- C; result$A <- list(A_1,A_2,A_3)
  result$theta = theta
  result$iteration <- iter
  result$cost = cost
  return(result)
}

