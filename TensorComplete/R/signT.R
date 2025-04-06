
min.thresh=10^(-3)
max.thresh=10^10



# gradient function of weighted classification with large margin loss
gradient=function(A1,A2,A3,mode,Ybar,W,type=c("logistic","hinge")){
    d=dim(Ybar)
    margin=Ybar*tensorize(A1,A2,A3)
    R=dim(A3)[2]

    if(type=="logistic"){
        tem=-W*Ybar*exp(-margin)/(1+exp(-margin))
    }else if(type=="hinge"){
        tem=-W*Ybar*(margin<1)
    }
    scale = length(tem)-sum(is.na(tem))
    tem[is.na(tem)]=0

    if(mode==3){
        Grad=matrix(0,nrow=dim(A3)[1],ncol=R)
        for(r in 1:R){
            Grad[,r]=ttl(as.tensor(tem),list(as.matrix(t(A1[,r])),as.matrix(t(A2[,r]))),ms=c(1,2))@data/scale
        }}else if(mode==2){
            Grad=matrix(0,nrow=dim(A2)[1],ncol=R)
            for(r in 1:R){
                Grad[,r]=ttl(as.tensor(tem),list(as.matrix(t(A1[,r])),as.matrix(t(A3[,r]))),ms=c(1,3))@data/scale
            }}else if(mode==1){
                Grad=matrix(0,nrow=dim(A1)[1],ncol=R)
                for(r in 1:R){
                    Grad[,r]=ttl(as.tensor(tem),list(as.matrix(t(A2[,r])),as.matrix(t(A3[,r]))),ms=c(2,3))@data/scale
                }}
    return(Grad)
}


# weighted classification with large margin loss function
cost=function(A1,A2,A3,Ybar,W,type=c("logistic","hinge")){
    return(mean(W*loss(tensorize(A1,A2,A3)*Ybar,type),na.rm=TRUE))
}



# gradient function of weighted classification with large margin loss in matrix case
gradientm=function(A1,A2,mode,Ybar,W,type=c("logistic","hinge")){
    margin = Ybar*(A1%*%t(A2))
    d = dim(Ybar)
    if(type=="logistic"){
        tem=-W*Ybar*exp(-margin)/(1+exp(-margin))
    }else if(type=="hinge"){
        tem=-W*Ybar*(margin<1)
    }
    scale = length(tem)-sum(is.na(tem))
    tem[is.na(tem)]=0

    if(mode==1){
        Grad = tem%*%A2/scale
    }else if(mode==2){
        Grad = t(tem)%*%A1/scale
    }
    return(Grad)
}



# weighted classification with large margin loss function in matrix case
costm = function(A1,A2,Ybar,W,type = c("logistic","hinge")){
    return(mean(W*loss((A1%*%t(A2))*Ybar,type),na.rm=TRUE))
}




# large margin loss functions
loss=function(y,type=c("logistic","hinge")){
    if(type=="hinge") return(ifelse(1-y>0,1-y,0))
    if(type=="logistic") return(log(1+exp(-y)))
}


# weighted classification loss with binary loss
binaryloss=function(Ybar,W,Yfit){
    return(mean(W*abs(Ybar-sign(Yfit)),na.rm=TRUE))
}



# Squared error loss
SEL = function(data,theta){
    index=which(is.na(data)==F & is.na(theta)==F)
    return(sqrt(sum((data[index]-theta[index])^2)))
}

# Tensorrization
tensorize=function(X,Y,Z){
    r=dim(X)[2]
    tensor=0
    if(is.matrix(X)==0){
        tensor=X%o%Y%o%Z
        return(tensor)
    }

    for(i in 1:r){
        tensor=tensor+X[,i]%o%Y[,i]%o%Z[,i]
    }
    return(tensor)
}

#################### main function for nonparametric tensor completion  ####################



#' Main function for nonparametric tensor estimation and completion based on low sign rank model.
#'
#' Estimate a signal tensor from a noisy and incomplete data tensor using nonparametric tensor method via sign series.
#' @param Y A given (possibly noisy and incomplete) data tensor. The function allows both continuous- and binary-valued tensors. Missing value should be encoded as \code{NA}.
#' @param truer Sign rank of the signal tensor.
#' @param H Resolution parameter.
#' @param Lmin Minimum value of the signal tensor (or minimum value of the tensor Y).
#' @param Lmax Maximum value of the signal tensor (or maximum value of the tensor Y).
#' @param option A large margin loss to be used. Use logistic loss if \code{option} = 1, hinge loss if \code{option} = 2. Hinge loss is default.
#' @return The returned object is a list of components.
#' @return \code{fitted} - A series of optimizers that minimize the weighted classification loss at each level.
#' @return \code{est} - An estimated signal tensor based on nonparametic tensor method via sign series.
#' @usage fit_nonparaT(Y,truer,H,Lmin,Lmax,option = 2)
#' @references C. Lee and M. Wang. Beyond the Signs: Nonparametric Tensor Completion via Sign Series. \emph{Neural Information Processing Systems 34 (NeurIPS)}, 2021.
#' @examples
#' library(tensorregress)
#' indices = c(2,2,2)
#' noise = rand_tensor(indices)@data
#' Theta = array(runif(prod(indices),min=-1,max = 1),indices)
#'
#' # The signal plus noise model
#' Y = Theta + noise
#'
#' # Estimate Theta from nonparametic completion method via sign series
#' hatTheta = fit_nonparaT(Y,truer = 1,H = 1,Lmin = -1,Lmax = 1, option =2)
#' print(hatTheta$est)
#'
#' @export
#' @import tensorregress
#' @import utils
#' @importFrom stats "optim" "runif" "rnorm" "fft"
#' @importFrom methods "is"


fit_nonparaT = function(Y,truer,H=5,Lmin,Lmax,option=2){
    if(length(dim(Y))==2){
        result = SignM(Y,truer,H,Lmin,Lmax,option)
    }else{
        result = SignT(Y,truer,H,Lmin,Lmax,option)
    }
    return(result)
}


SignT=function(Y,truer,H=5,Lmin,Lmax,option=2){
    B_fitted=result=list()
    pi_seq=seq(from=Lmin,to=Lmax,length=2*H+1)

    for(h in 2:(2*H)){
        pi=pi_seq[h]
        if(option==1){
            res=Alt(sign(Y-pi),abs(Y-pi),r=truer,type="logistic",start="linear")## recommend
        }else if(option==2){
            res=Alt(sign(Y-pi),abs(Y-pi),r=truer,type="hinge",start="linear")## recommend
        }
        B_fitted[[h]]=res$fitted
    }
    B_fitted=array(unlist(B_fitted),dim=c(dim(Y),2*H-1));
    res=list();
    res$fitted=B_fitted
    res$est=1/2*(apply(sign(B_fitted),1:3,sum)/(2*H)+1)*(Lmax-Lmin)+Lmin
    return(res)
}


SignM=function(Y,truer,H=5,Lmin,Lmax,option=2){
    B_fitted=result=list()
    pi_seq=seq(from=Lmin,to=Lmax,length=2*H+1)

    for(h in 2:(2*H)){
        pi=pi_seq[h]
        if(option==1){
            res=Altm(sign(Y-pi),abs(Y-pi),r=truer,type="logistic",start="linear")## recommend
        }else if(option==2){
            res=Altm(sign(Y-pi),abs(Y-pi),r=truer,type="hinge",start="linear")## recommend
        }
        B_fitted[[h]]=res$fitted
    }
    B_fitted=array(unlist(B_fitted),dim=c(dim(Y),2*H-1));
    res=list();
    res$fitted=B_fitted
    res$est=1/2*(apply(sign(B_fitted),1:2,sum)/(2*H)+1)*(Lmax-Lmin)+Lmin
    return(res)
}



#' Alternating optimization of the weighted classification loss
#'
#' Optimize the weighted classification loss given a weight tensor, an observed data tensor, and a large margin loss. This function is used as a subroutine in the main function \code{fit_nonparaT}.
#' @param Ybar A given (possibly noisy and incomplete) data tensor.
#' @param W A weight tensor used in the weighted classification loss.
#' @param r A rank to be fitted (CP rank).
#' @param type A large margin loss to be used. Logistic or hinge loss is available.
#' @param start Choice of initialization method. Use random initialization if \code{start} = "random"; Use the initialization based on low rank approximation if \code{start} = "linear". Linear initialization is default.
#' @return The returned object is a list of components.
#' @return \code{binary_obj} - Trajectory of binary loss values over iterations.
#' @return \code{obj} - Trajectory of weighted classification loss values over iterations.
#' @return \code{iter} - The number of iterations.
#' @return \code{error} - Trajectory of errors over iterations.
#' @return \code{fitted} - A tensor that optimizes the weighted classification loss.
#' @usage Altopt(Ybar,W,r,type = c("logistic","hinge"),start = "linear")
#' @references C. Lee and M. Wang. Beyond the Signs: Nonparametric Tensor Completion via Sign Series. \emph{Neural Information Processing Systems 34 (NeurIPS)}, 2021.
#' @examples
#' library(tensorregress)
#' indices = c(2,2,2)
#' noise = rand_tensor(indices)@data
#' Theta = array(runif(prod(indices),min=-3,max = 3),indices)
#'
#' # The signal plus noise model
#' Y = Theta + noise
#'
#' # Optimize the weighted classification for given a sign tensor sign(Y) and a weight tensor abs(Y)
#' result = Altopt(sign(Y),abs(Y),r = 3,type = "hinge",start = "linear")
#' signTheta = sign(result$fitted)
#'
#' @export
#' @import utils
#' @importFrom stats "optim" "runif" "rnorm" "fft"
#' @importFrom methods "is"

Altopt = function(Ybar,W,r,type=c("logistic","hinge"),start="linear"){
    if(length(dim(Ybar))==2){
        result = Altm(Ybar,W,r,type,start)
    }else{
        result = Alt(Ybar,W,r,type,start)
    }
    return(result)
}

Alt=function(Ybar,W,r,type=c("logistic","hinge"),start="linear"){
    result=list()
    d=dim(Ybar)
    if(start=="linear"){
    ini=fit_continuous_cp(Ybar,r)
    A1 = ini$U[[1]];
    A2 = ini$U[[2]];
    scale=matrix(0,nrow=r,ncol=r)
    diag(scale)=ini$lambda
    A3 = ini$U[[3]]%*%scale;
    }else{
    A1 = matrix(runif(d[1]*r,-1,1),nrow = d[1],ncol = r);
    A2 = matrix(runif(d[2]*r,-1,1),nrow = d[2],ncol = r);
    A3 = matrix(runif(d[3]*r,-1,1),nrow = d[3],ncol = r);
    }
    obj=cost(A1,A2,A3,Ybar,W,type);
    binary_obj=binaryloss(Ybar,W,tensorize(A1,A2,A3))

    error=1;iter=1;

 while((error>0.01)&(iter<20)){


 optimization=optim(c(A3),function(x)cost(A1,A2,matrix(x,ncol=r),Ybar,W,type),function(x)gradient(A1,A2,matrix(x,ncol=r),3,Ybar,W,type),method="BFGS")
 A3=matrix(optimization$par,ncol=r)
 optimization=optim(c(A2),function(x)cost(A1,matrix(x,ncol=r),A3,Ybar,W,type),function(x)gradient(A1,matrix(x,ncol=r),A3,2,Ybar,W,type),method="BFGS")
 A2=matrix(optimization$par,ncol=r)
 optimization=optim(c(A1),function(x)cost(matrix(x,ncol=r),A2,A3,Ybar,W,type),function(x)gradient(matrix(x,ncol=r),A2,A3,1,Ybar,W,type),method="BFGS")
 A1=matrix(optimization$par,ncol=r)

        obj=c(obj,cost(A1,A2,A3,Ybar,W,type))
        binary_obj=c(binary_obj,binaryloss(Ybar,W,tensorize(A1,A2,A3)))
        iter=iter+1
error=(obj[iter-1]-obj[iter])

 }
 result$binary_obj=binary_obj;
 result$obj=obj;
 result$iter=iter;
 result$error=error;
 result$fitted=tensorize(A1,A2,A3); ## exact low-rank
 return(result)
}


Altm=function(Ybar,W,r,type=c("logistic","hinge"),start="linear"){
    result=list()
    d=dim(Ybar)
    if(start=="linear"){
        ini=fit_continuous_cp(Ybar,r)
        A1 = ini$U[[1]];
        scale=matrix(0,nrow=r,ncol=r)
        diag(scale)=ini$lambda
        A2 = ini$U[[2]]%*%scale;
    }else{
        A1 = matrix(runif(d[1]*r,-1,1),nrow = d[1],ncol = r);
        A2 = matrix(runif(d[2]*r,-1,1),nrow = d[2],ncol = r);
    }
    obj=costm(A1,A2,Ybar,W,type)
    binary_obj=binaryloss(Ybar,W,A1%*%t(A2))

    error=1;iter=1;

    while((error>0.01)&(iter<20)){


        optimization=optim(c(A2),function(x) costm(A1,matrix(x,ncol=r),Ybar,W,type),function(x) c(gradientm(A1,matrix(x,ncol=r),2,Ybar,W,type)),method="BFGS")
        A2=matrix(optimization$par,ncol=r)

        optimization=optim(c(A1),function(x)costm(matrix(x,ncol=r),A2,Ybar,W,type),function(x)gradientm(matrix(x,ncol=r),A2,1,Ybar,W,type),method="BFGS")
        A1=matrix(optimization$par,ncol=r)

        obj=c(obj,costm(A1,A2,Ybar,W,type))
        binary_obj=c(binary_obj,binaryloss(Ybar,W,A1%*%t(A2)))
        iter=iter+1
        error=(obj[iter-1]-obj[iter])

    }
    result$binary_obj=binary_obj;
    result$obj=obj;
    result$iter=iter;
    result$error=error;
    result$fitted=A1%*%t(A2); ## exact low-rank
    return(result)
}


#' Signal tensor estimation from a noisy and incomplete data tensor based on CP low rank tensor method.
#'
#' Estimate a signal tensor from a noisy and incomplete data tensor using CP low rank tensor method.
#' @param data A given (possibly noisy and incomplete) data tensor.
#' @param r A rank to be fitted (CP rank).
#' @return The returned object is a list of components.
#' @return \code{est} - An estimated signal tensor based on CP low rank tensor method.
#' @return \code{U} - A list of factor matrices.
#' @return \code{lambda} - A vector of tensor singular values.
#' @usage fit_continuous_cp(data,r)
#' @examples
#' library(tensorregress)
#' indices = c(2,3,4)
#' noise = rand_tensor(indices)@data
#' Theta = array(runif(prod(indices),min=-3,max = 3),indices)
#'
#' # The signal plus noise model
#' Y = Theta + noise
#'
#' # Estimate Theta from CP low rank tensor method
#' hatTheta = fit_continuous_cp(Y,3)
#' print(hatTheta$est)
#'
#' @export
#' @import tensorregress
#' @importFrom stats "optim" "runif"


fit_continuous_cp=function(data,r){
    index=which(is.na(data)==TRUE)
    data[index]=mean(data,na.rm=TRUE)
   original_data=data

    if(length(dim(data))>=3){
        sink(tempfile())
decomp=tryCatch(cp(as.tensor(original_data),r),error=function(c)"degeneracy")
sink()
if(inherits(decomp,"character")==TRUE){
    U=list();
    U[[1]]=matrix(0,ncol=r,nrow=dim(data)[1])
    U[[2]]=matrix(0,ncol=r,nrow=dim(data)[2])
    U[[3]]=matrix(0,ncol=r,nrow=dim(data)[3])
return(list("est"=array(NA,dim=dim(data)),"U"=U,"lambda"=rep(0,r),"info"="degeneracy"))
}
    res0=1
    res=0
    thresh=10^(-3)
    error=NULL

    while((res0-res)>thresh){
    res0=SEL(original_data,decomp$est@data)
    sink(tempfile())
    decomp=cp(as.tensor(data),r)
    sink()
    res=SEL(original_data,decomp$est@data)
    data[index]=decomp$est@data[index]
    error=c(error,res)
    }

    return(list("est"=decomp$est@data,"U"=decomp$U,"lambda"=decomp$lambda))
    }else if(length(dim(data))==2){
        PQ=svd(data)
        if(r==1){
            est=cbind(PQ$u[,1:r])%*%diag(as.matrix(PQ$d[1:r]))%*%t(cbind(PQ$v[,1:r]))
            U = list(PQ$u[,1:r],PQ$v[,1:r])
            lambda = PQ$d[1:r]
        }else{
            est=PQ$u[,1:r]%*%diag(PQ$d[1:r])%*%t(PQ$v[,1:r])
            U = list(PQ$u[,1:r],PQ$v[,1:r])
            lambda = PQ$d[1:r]
        }
        res0=1
        res=0
        thresh=10^(-3)
        error=NULL

        while((res0-res)>thresh){
            res0=SEL(original_data,est)
            if(r==1){
                est=cbind(PQ$u[,1:r])%*%diag(as.matrix(PQ$d[1:r]))%*%t(cbind(PQ$v[,1:r]))
                U = list(PQ$u[,1:r],PQ$v[,1:r])
                lambda = PQ$d[1:r]
            }else{
                est=PQ$u[,1:r]%*%diag(PQ$d[1:r])%*%t(PQ$v[,1:r])
                U = list(PQ$u[,1:r],PQ$v[,1:r])
                lambda = PQ$d[1:r]
            }
            res=SEL(original_data,est)
            data[index]=est[index]
            error=c(error,res)
        }
        return(list("est" = est,"U" = U,"lambda"= lambda))
}
}

