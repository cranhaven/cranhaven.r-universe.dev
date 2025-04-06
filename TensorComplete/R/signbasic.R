
fnorm=function(tnsr){
  arr<-tnsr@data
  sqrt(sum(arr*arr))
}


rs_unfold=function(tnsr,m=NULL){
  if(is.null(m)) stop("mode m must be specified")
  num_modes <- tnsr@num_modes
  rs <- m
  cs <- (1:num_modes)[-m]
  unfold(tnsr,row_idx=rs,col_idx=cs)
}

hadamard_list <- function(L){
  isvecORmat <- function(x){is.matrix(x) || is.vector(x)}
  stopifnot(all(unlist(lapply(L,isvecORmat))))
  retmat <- L[[1]]
  for (i in 2:length(L)){
    retmat <- retmat*L[[i]]
  }
  retmat
}



khatri_rao_list <- function(L,reverse=FALSE){
  stopifnot(all(unlist(lapply(L,is.matrix))))
  ncols <- unlist(lapply(L,ncol))
  stopifnot(length(unique(ncols))==1)
  ncols <- ncols[1]
  nrows <- unlist(lapply(L,nrow))
  retmat <- matrix(0,nrow=prod(nrows),ncol=ncols)
  if (reverse) L <- rev(L)
  for(j in 1:ncols){
    Lj <- lapply(L,function(x) x[,j])
    retmat[,j] <- kronecker_list(Lj)
  }
  retmat
}




#Wrapper to Inverse FFT
.ifft <- function(x){suppressWarnings(as.numeric(fft(x,inverse=TRUE))/length(x))}
#Creates a superdiagonal tensor
.superdiagonal_tensor <- function(num_modes,len,elements=1L){
  modes <- rep(len,num_modes)
  arr <- array(0, dim = modes)
  if(length(elements)==1) elements <- rep(elements,len)
  for (i in 1:len){
    txt <- paste("arr[",paste(rep("i", num_modes),collapse=","),"] <- ", elements[i],sep="")
    eval(parse(text=txt))
  }
  as.tensor(arr)
}



cp <- function(tnsr, num_components=NULL,max_iter=25, tol=1e-5){
  if(is.null(num_components)) stop("num_components must be specified")
  stopifnot(is(tnsr,"Tensor"))

  #initialization via truncated hosvd
  num_modes <- tnsr@num_modes
  modes <- tnsr@modes
  U_list <- vector("list",num_modes)
  unfolded_mat <- vector("list",num_modes)
  tnsr_norm <- fnorm(tnsr)
  for(m in 1:num_modes){
    unfolded_mat[[m]] <- rs_unfold(tnsr,m=m)@data
    U_list[[m]] <- matrix(rnorm(modes[m]*num_components), nrow=modes[m], ncol=num_components)
  }
  est <- tnsr
  curr_iter <- 1
  converged <- FALSE
  #set up convergence check
  fnorm_resid <- rep(0, max_iter)
  CHECK_CONV <- function(est){
    curr_resid <- fnorm(est - tnsr)
    fnorm_resid[curr_iter] <<- curr_resid
    if (curr_iter==1) return(FALSE)
    if (abs(curr_resid-fnorm_resid[curr_iter-1])/tnsr_norm < tol) return(TRUE)
    else{ return(FALSE)}
  }
  #progress bar
  pb <- txtProgressBar(min=0,max=max_iter,style=3)
  #main loop (until convergence or max_iter)
  norm_vec <- function(vec){
    norm(as.matrix(vec))
  }
  while((curr_iter < max_iter) && (!converged)){
    setTxtProgressBar(pb,curr_iter)
    for(m in 1:num_modes){
      V <- hadamard_list(lapply(U_list[-m],function(x) {t(x)%*%x}))
      V_inv <- solve(V)
      tmp <- unfolded_mat[[m]]%*%khatri_rao_list(U_list[-m],reverse=TRUE)%*%V_inv
      lambdas <- apply(tmp,2,norm_vec)
      U_list[[m]] <- sweep(tmp,2,lambdas,"/")
      Z <- .superdiagonal_tensor(num_modes=num_modes,len=num_components,elements=lambdas)
      est <- ttl(Z,U_list,ms=1:num_modes)
    }
    #checks convergence
    if(CHECK_CONV(est)){
      converged <- TRUE
      setTxtProgressBar(pb,max_iter)
    }else{
      curr_iter <- curr_iter + 1
    }
  }
  if(!converged){setTxtProgressBar(pb,max_iter)}
  close(pb)
  #end of main loop
  #put together return list, and returns
  fnorm_resid <- fnorm_resid[fnorm_resid!=0]
  norm_percent<- (1-(tail(fnorm_resid,1)/tnsr_norm))*100
  invisible(list(lambdas=lambdas, U=U_list, conv=converged, est=est, norm_percent=norm_percent, fnorm_resid = tail(fnorm_resid,1),all_resids=fnorm_resid))
}
