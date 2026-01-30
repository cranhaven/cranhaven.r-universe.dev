#' Variable selection method with multiple block-wise imputation (MBI)
#'
#' Fit a variable selection method with multiple block-wise imputation (MBI).
#'
#' The function uses the penalized generalized method of moments with multiple block-wise imputation to handle block-wise missing data, commonly found in multi-source datasets.
#'
#' @import MASS
#' @import glmnet
#' @import pryr
#' @import doParallel
#' @import foreach
#' @import glmnetcr
#' @import Matrix
#' @importFrom stats sd var lm coef rnorm
#' @param X Design matrix for block-wise missing covariates.
#' @param y Response vector.
#' @param cov_index Starting indexes of covariates in data sources.
#' @param sub_index Starting indexes of subjects in missing groups.
#' @param miss_source Indexes of missing data sources in missing groups, respectively ('NULL' represents no missing).
#' @param complete Logical indicator of whether there is a group of complete cases. If there is a group of complete cases,
#' it should be the first group. 'TRUE' represents that there is a group of complete cases.
#' @param lambda A user supplied sequence of tuning parameter in penalty. If NULL, a sequence is automatically generated.
#' @param eps1 Convergence threshold at certain stage of the algorithm. Default is 1e-3.
#' @param eps2 Convergence threshold at certain stage of the algorithm. Default is 1e-7.
#' @param eps3 Convergence threshold at certain stage of the algorithm. Default is 1e-8.
#' @param max.iter The maximum number of iterations allowed. Default is 1000.
#' @param lambda.min Smallest value for \code{lambda}, as a fraction of the maximum value in \code{lambda}. Default depends on the size of input.
#' @param nlam The number of \code{lambda} values. Default is 100.
#' @param beta0 Initial value for regression coefficients. If NULL, they are initialized automatically.
#' @param a Tuning parameter in the SCAD penalty. Default is 3.7.
#' @param gamma.ebic Parameter in the EBIC criterion. Default is 0.5.
#' @param alpha1 A sequence of candidate values for the step size in the conjugate gradient algorithm. Default is 0.5^(0:12).
#' @param h1 A sequence of candidate values for the parameter in the numerical calculation of the first derivative of the objective function. Default is 2^(-(8:30)).
#' @param ratio Parameter in the numerical calculation of the first derivative. Default is 1.
#'
#' @return \item{beta}{Estimated coefficients matrix with \code{length(lambda)} rows and \code{dim(X)[2]} columns.}
#' \item{lambda}{The actual sequence of \code{lambda} values used.}
#' \item{bic1}{BIC criterion values. '0' should be ignored.}
#' \item{notcon}{Value indicating whether the algorithm is converged or not. '0' represents convergence; otherwise non-convergence.}
#' \item{intercept}{Intercept sequence of length \code{length(lambda)}.}
#' \item{beta0}{Estimated coefficients matrix for standardized \code{X}}
#' @author Fei Xue and Annie Qu
#' @references Xue, F., and Qu, A. (2021)
#' \emph{Integrating Multisource Block-Wise Missing Data in Model Selection (2021), Journal of the American Statistical Association, Vol. 116(536), 1914-1927}.
#' @examples
#'
#' library(MASS)
#'
#' # Number of subjects
#' n <- 30
#'
#' # Number of total covariates
#' p <- 4
#'
#' # Number of missing groups of subjects
#' ngroup <- 2
#'
#' # Number of data sources
#' nsource <- 2
#'
#' # Starting indexes of covariates in data sources
#' cov_index=c(1, 3)
#'
#' # Starting indexes of subjects in missing groups
#' sub_index=c(1, 16)
#'
#' # Indexes of missing data sources in missing groups, respectively ('NULL' represents no missing)
#' miss_source=list(NULL, 1)
#'
#' # Indicator of whether there is a group of complete cases. If there is a group of complete cases,
#' # it should be the first group.
#' complete=TRUE
#'
#' # Create a block-wise missing design matrix X and response vector y
#' set.seed(1)
#' sigma=diag(1-0.4,p,p)+matrix(0.4,p,p)
#' X <- mvrnorm(n,rep(0,p),sigma)
#' beta_true <- c(2.5, 0, 3, 0)
#' y <- rnorm(n) + X%*%beta_true
#'
#' for (i in 1:ngroup) {
#'   if (!is.null(miss_source[[i]])) {
#'     if (i==ngroup) {
#'       if (miss_source[[i]]==nsource) {
#'         X[sub_index[i]:n, cov_index[miss_source[[i]]]:p] = NA
#'       } else {
#'         X[sub_index[i]:n, cov_index[miss_source[[i]]]:(cov_index[miss_source[[i]]+1]-1)] = NA
#'       }
#'     } else {
#'       if (miss_source[[i]]==nsource) {
#'         X[sub_index[i]:(sub_index[i+1]-1), cov_index[miss_source[[i]]]:p] = NA
#'       } else {
#'         X[sub_index[i]:(sub_index[i+1]-1), cov_index[miss_source[[i]]]:
#'         (cov_index[miss_source[[i]]+1]-1)] = NA
#'       }
#'     }
#'   }
#' }
#'
#' # Now we can use the function with this simulated data
#' #start.time = proc.time()
#' result <- MBI(X=X, y=y, cov_index=cov_index, sub_index=sub_index, miss_source=miss_source,
#' complete=complete, nlam = 15, eps2 = 1e-3, h1=2^(-(8:20)))
#' #time = proc.time() - start.time
#'
#' theta=result$beta
#' bic1=result$bic1
#' best=which.min(bic1[bic1!=0])
#' beta_est=theta[best,]
#'
#'
#' @export

MBI <- function (X, y, cov_index, sub_index, miss_source, complete,
                 lambda=NULL, eps1 = 1e-3, eps2 = 1e-7, eps3=1e-8, max.iter = 1000, lambda.min=ifelse(n>p,.001,.05), nlam=100,
                 beta0=NULL, a=3.7, gamma.ebic=0.5, alpha1=0.5^(0:12), h1=2^(-(8:30)), ratio=1) {

  dims=dim(X)
  n=dims[1]
  p=dims[2]
  nsource=length(cov_index)

  C0=NULL

  #m0=length(index0)     #Number of original patterns
  #nm=dim(missid)[1]     #Number of missing sources
  n_pat=length(sub_index)   #Number of missing groups
    #length(unique(pat))     #Number of patterns

  # Indexes of covariates in each source
  cov_source=vector("list", nsource)
  for (i in 1:nsource) {
    if (i==nsource) {
      cov_source[[i]]=cov_index[i]:p
    } else {
      cov_source[[i]]=cov_index[i]:(cov_index[i+1]-1)
    }
  }

  # Indexes of missing covariates in each missing group
  miss_cov=vector("list", n_pat)

  # Indexes of observed covariates in each missing group
  obs_cov=vector("list", n_pat)

  for (i in 1:n_pat) {
    if (!is.null(miss_source[[i]])) {
      for (l in 1:length(miss_source[[i]])) {
        miss_cov[[i]] = c(miss_cov[[i]], cov_source[[miss_source[[i]][l]]])
      }
      obs_cov[[i]]=(1:p)[!(1:p) %in% miss_cov[[i]]]
    } else {
      obs_cov[[i]]=1:p
    }
  }

  if (complete==TRUE & !is.null(miss_cov[[1]])) {
    stop("Complete case group should be the first group.")
  }

  # Useful groups for imputation of each missing group
  useful_group=vector("list", n_pat)

  # Number of total imputations
  m=0
  for (i in 1:n_pat) {
    if (!is.null(miss_source[[i]])) {
      for (j in 1:n_pat) {
        miss_obs=intersect(miss_cov[[i]], obs_cov[[j]])
        obs_obs=intersect(obs_cov[[i]], obs_cov[[j]])
        if (j!=i & length(miss_obs)==length(miss_cov[[i]]) & length(obs_obs)>0) {
          useful_group[[i]]=c(useful_group[[i]], j)
          m=m+1
        }
      }
    } else {
      m=m+1
      useful_group[[i]]=i
    }
  }


  ##################################### Standardization #################################
  X2=X
  centerx=rep(0,p)
  scalex=rep(1,p)
  centery=mean(y)
  y2=y-centery
  for (j in 1:p) {
    centerx[j]=mean(X[,j], na.rm=TRUE)
    scalex[j]=sd(X[,j], na.rm = TRUE)
    X2[,j]=(X[,j]-centerx[j])/scalex[j]
  }

  X3=array(0,dim=c(n,p,m))
  XX=array(0,dim=c(n,p,m))
  y3=matrix(0,n,m)
  index1=matrix(FALSE,n,m)            # use which sample
  index2=matrix(TRUE,p,m)             # take which derivative in estimating equations
  pat=rep(0,m)
  PCA_m=rep(2,m)

  index0=NULL
  miss2=is.na(X2)

  pat_count=1
  index2_count=1
  for (i in 1:n_pat) {
    pat[pat_count:(pat_count+length(useful_group[[i]])-1)]=i
    y3[,pat_count:(pat_count+length(useful_group[[i]])-1)]=y2
    if (i<n_pat) {
      index1[sub_index[i]:(sub_index[i+1]-1), pat_count:(pat_count+length(useful_group[[i]])-1)]=TRUE
      X3[sub_index[i]:(sub_index[i+1]-1),,pat_count:(pat_count+length(useful_group[[i]])-1)]=X2[sub_index[i]:(sub_index[i+1]-1),]
      XX[sub_index[i]:(sub_index[i+1]-1),,pat_count:(pat_count+length(useful_group[[i]])-1)]=X2[sub_index[i]:(sub_index[i+1]-1),]
    } else {
      index1[sub_index[i]:n, pat_count:(pat_count+length(useful_group[[i]])-1)]=TRUE
      X3[sub_index[i]:n,,pat_count:(pat_count+length(useful_group[[i]])-1)]=X2[sub_index[i]:n,]
      XX[sub_index[i]:n,,pat_count:(pat_count+length(useful_group[[i]])-1)]=X2[sub_index[i]:n,]
    }

    for (j in 1:length(useful_group[[i]])) {
      index2[union(miss_cov[[i]], miss_cov[[useful_group[[i]][j]]]), index2_count]=FALSE

      if (j==1) {
        PCA_m[index2_count]=1
      }

      if (useful_group[[i]][j]==1) {
        index0=c(index0, index2_count)
      }

      miss_tmp=is.na(X3[which.max(index1[,index2_count]),,index2_count])
      if (sum(miss_tmp)>0) {
        ind_y=(1:p)[miss_tmp]
        ind_tmp=intersect(obs_cov[[i]], obs_cov[[useful_group[[i]][j]]])
        XX[index1[,index2_count]==TRUE,ind_y,index2_count]=imputeglm.predict(X=X2, ind_y=ind_y, ind_x = ind_tmp, miss=miss2, newdata = X3[index1[,index2_count]==TRUE,ind_tmp,index2_count])$PRED
      }

      index2_count=index2_count+1
    }

    pat_count=pat_count+length(useful_group[[i]])
  }

  if (!complete) {
    index0=0
  }

  yy=y3

  ##### Next: Define initial values


  vary=rep(1,m)
  for (i in 1:m) {
    vary[i]=var(yy[index1[,i],i])
  }


  # if (is.null(beta0)) {
  #   if (!is.null(lambda)) {
  #     if (lambda==0) {
  #       if (sum(index1[,1])>p) {
  #         beta0=lm(yy[index1[,1],1]~XX[index1[,1],,1])$coef[-1]
  #       } else {
  #         cvfit <- cv.glmnet(XX[index1[,1],,1], yy[index1[,1],1], nfolds=3)
  #         fit <- cvfit$glmnet.fit
  #         beta0=t(as.numeric(coef(fit, s=cvfit$lambda.min))[-1])
  #       }
  #     } else {
  #       model_0=MBI(X=X, y=y, cov_index=cov_index, sub_index=sub_index, miss_source=miss_source, complete=complete, lambda=0, eps1 = eps1, eps2 = eps2, eps3=eps3, max.iter = max.iter, lambda.min=lambda.min, nlam=nlam, beta0=beta0, C0=C0, a=a, gamma.ebic=gamma.ebic, alpha1=alpha1, h1=h1, ratio = ratio)
  #       beta0=model_0$beta0
  #     }
  #   } else {
  #     model_0=MBI(X=X, y=y, cov_index=cov_index, sub_index=sub_index, miss_source=miss_source, complete=complete, lambda=0, eps1 = eps1, eps2 = eps2, eps3=eps3, max.iter = max.iter, lambda.min=lambda.min, nlam=nlam, beta0=beta0, C0=C0, a=a, gamma.ebic=gamma.ebic, alpha1=alpha1, h1=h1, ratio = ratio)
  #     beta0=model_0$beta0
  #   }
  # }

  if (is.null(beta0)) {
    if (!is.null(lambda)) {
      if (lambda==0) {
        if (sum(index1[,1])>p & sum(index0==0)==0) {        ##### Should we change more?
          beta0=lm(yy[index1[,1],1]~XX[index1[,1],,1])$coef[-1]
        } else if (sum(index1[,1])<=p & sum(index0==0)==0) {
          cvfit <- cv.glmnet(XX[index1[,1],,1], yy[index1[,1],1], nfolds=3)
          fit <- cvfit$glmnet.fit
          beta0=t(as.numeric(coef(fit, s=cvfit$lambda.min))[-1])
        } else if (sum(index0==0)==1) {
          data_tmp=matrix(0,n,p)
          for (i in 1:m) {
            data_tmp=data_tmp+XX[,,i]
          }
          for (i in 1:n_pat) {
            index_tmp=index1[,min(which(pat==i))]
            data_tmp[index_tmp,]=data_tmp[index_tmp,]/sum(pat==i)
          }
          cvfit <- cv.glmnet(data_tmp, yy[,1], nfolds=3)
          fit <- cvfit$glmnet.fit
          beta0=t(as.numeric(coef(fit, s=cvfit$lambda.min))[-1])
          # }
        }
      } else {
        model_0=MBI(X=X, y=y, cov_index=cov_index, sub_index=sub_index, miss_source=miss_source, complete=complete, lambda=0, eps1 = eps1, eps2 = eps2, eps3=eps3, max.iter = max.iter, lambda.min=lambda.min, nlam=nlam, beta0=beta0, a=a, gamma.ebic=gamma.ebic, alpha1=alpha1, h1=h1, ratio = ratio)
        beta0=model_0$beta0
      }
    } else {
      model_0=MBI(X=X, y=y, cov_index=cov_index, sub_index=sub_index, miss_source=miss_source, complete=complete, lambda=0, eps1 = eps1, eps2 = eps2, eps3=eps3, max.iter = max.iter, lambda.min=lambda.min, nlam=nlam, beta0=beta0, a=a, gamma.ebic=gamma.ebic, alpha1=alpha1, h1=h1, ratio = ratio)
      beta0=model_0$beta0
    }
  }

  N=sum(index2)
  N0=sum(index2[,index0])
  numequ=apply(index2,2,sum)
  nn=apply(index1,2,sum)
  nnn=sqrt(apply(index1,2,sum))
  g=rep(0,N)
  # fg=matrix(0,N,p)
  fq=rep(0,p)
  # sq=matrix(0,p,p)
  # fC=array(0,dim=c(N,N,p))
  PCA_ind=array(NA,dim=c(n_pat,2,N))
  PCA_ind0=matrix(0,n_pat,2)
  ind_C=rep(1,n_pat+1)
  ind_pat=rep(1,n_pat+1)              #### similar to pat_start, but has one more element
  U_ind=array(NA,dim=c(n_pat,2,N))
  #U_ind0=matrix(0,n_pat,2)
  # T1=diag(1,N,N)
  # T2=matrix(0,N,N)


  # Z1=array(0,dim = c(n,N,p))
  Z2=matrix(0,n,N)
  tempequ1=rep(0,m+1)
  tempequ1[1]=1
  for (s in 1:m) {
    tempequ1[s+1]=tempequ1[s]+numequ[s]
    # fg[tempequ1[s]:(tempequ1[s+1]-1),]=t(XX[index1[,s],index2[,s],s])%*%(-XX[index1[,s],,s])/(vary[s]*nn[s])
    # for (j in 1:p) {
    #   Z1[,tempequ1[s]:(tempequ1[s+1]-1),j]=apply(XX[,index2[,s],s], 2, function(x) x*(-XX[,j,s]))/(vary[s]*nnn[s])
    # }
    if(PCA_m[s]==1) {
      PCA_ind[pat[s],1,(PCA_ind0[pat[s],1]+1):(PCA_ind0[pat[s],1]+numequ[s])]=tempequ1[s]:(tempequ1[s+1]-1)
      PCA_ind0[pat[s],1]=PCA_ind0[pat[s],1]+numequ[s]
    } else if (PCA_m[s]==2) {
      PCA_ind[pat[s],2,(PCA_ind0[pat[s],2]+1):(PCA_ind0[pat[s],2]+numequ[s])]=tempequ1[s]:(tempequ1[s+1]-1)
      PCA_ind0[pat[s],2]=PCA_ind0[pat[s],2]+numequ[s]
    }
  }

  size1=matrix(1,N,N)
  #size2=array(1,dim=c(N,N,p))
  for (s1 in 1:m) {
    for (s2 in 1:m) {
      n_tmp=sum(index1[,s1]*index1[,s2])
      if (n_tmp*sum(abs(index1[,s1]-index1[,s2]))!=0) {
        size1[tempequ1[s1]:(tempequ1[s1+1]-1),tempequ1[s2]:(tempequ1[s2+1]-1)]=nnn[s1]*nnn[s2]/n_tmp
        #size2[tempequ1[s1]:(tempequ1[s1+1]-1),tempequ1[s2]:(tempequ1[s2+1]-1),]=nnn[s1]*nnn[s2]/n_tmp
      }
    }
  }

  res=matrix(0,n,m)
  resvar=rep(1,m)

  for (i in 1:n_pat) {
    ind_C[i+1]=ind_C[i]+sum(PCA_ind0[i,])
    ind_pat[i+1]=ind_pat[i]+sum(index1[,min(which(pat==i))])
  }

  if (is.null(lambda)) {
    #fg_temp=fg
    #Z1_temp=Z1
    #size2_temp=size2
    for (s in 1:m) {
      res[index1[,s],s]=yy[index1[,s],s]-XX[index1[,s],,s]%*%(rep(0.01,p))
      # resvar[s]=var(res[index1[,s],s])                                   #### not divided by variance of residuals
      g[tempequ1[s]:(tempequ1[s+1]-1)]=t(XX[index1[,s],index2[,s],s])%*%(res[index1[,s],s])/(resvar[s]*nn[s])
      Z2[,tempequ1[s]:(tempequ1[s+1]-1)]=apply(XX[,index2[,s],s], 2, function(x) x*res[,s])/(resvar[s]*nnn[s])

      #fg_temp[tempequ1[s]:(tempequ1[s+1]-1),]=fg_temp[tempequ1[s]:(tempequ1[s+1]-1),]*vary[s]/resvar[s]
      #Z1_temp[,tempequ1[s]:(tempequ1[s+1]-1),]=Z1_temp[,tempequ1[s]:(tempequ1[s+1]-1),]*vary[s]/resvar[s]
    }


    if (is.null(C0)) {
      #start.time = proc.time()
      C=matrix(0,N,N)
      for (ii in 1:n_pat) {
        C[ind_C[ii]:(ind_C[ii+1]-1),ind_C[ii]:(ind_C[ii+1]-1)]=t(Z2[ind_pat[ii]:(ind_pat[ii+1]-1),ind_C[ii]:(ind_C[ii+1]-1), drop=FALSE])%*%Z2[ind_pat[ii]:(ind_pat[ii+1]-1),ind_C[ii]:(ind_C[ii+1]-1), drop=FALSE]
      }
      #time = proc.time() - start.time
      #C=t(Z2)%*%Z2
      C=C*size1

      # Q0=Qfun(C=C, g=g, PCA_ind=PCA_ind, PCA_ind0=PCA_ind0, N=N, n_pat=n_pat, eps3 = eps3, index1=index1, pat=pat, res=res, numequ=numequ, tempequ1=tempequ1)
      # q_star=Q0$q_star
      #start.time = proc.time()
      fq=Qfirstdev1(h1=h1, beta_pre=rep(0.01,p), XX=XX, yy=yy, index1=index1, index2=index2, numequ=numequ, tempequ1=tempequ1, PCA_ind=PCA_ind, PCA_ind0=PCA_ind0, n_pat=n_pat, eps3=eps3, pat=pat, ind_C=ind_C, ind_pat=ind_pat, ratio = ratio)$fq_star
      #print("Get first derivative 0")
      time=proc.time()
      #print(time)
      #time = proc.time() - start.time
      lambda.max=max(abs(fq))+0.5
      lambda=exp(seq(log(lambda.min*lambda.max),log(lambda.max),len=nlam))
    }
    # else {
    #   fq=2*t(fg_temp)%*%solve(C0,g)
    #   sq=2*t(fg_temp)%*%solve(C0,fg_temp)
    #
    #   lambda.max=5*max(abs(fq)+abs(sq%*%rep(10,p)))
    #   lambda=exp(seq(log(lambda.min*lambda.max),log(lambda.max),len=nlam))
    # }
  }

  L=length(lambda)
  beta=matrix(0,L,p)
  beta_pre=beta0
  res=matrix(0,n,m)

  qq=rep(0,L)
  biq=rep(0,L)
  aiq=rep(0,L)
  bic=rep(0,L)
  ebic1=rep(0,L)
  ebic2=rep(0,L)
  ebic11=rep(0,L)
  ebic21=rep(0,L)
  ebicM=rep(0,L)
  ebicM1=rep(0,L)
  rss=rep(0,L)
  bic1=rep(0,L)
  rss1=rep(0,L)
  beta_out=matrix(0,L,p)
  intercept=rep(0,L)
  objective=matrix(0,L,2)
  objective1=matrix(0,L,2)

  notcon=0

  ind=rep(TRUE, p)
  p1=p
  # fC0=array(0,dim=c(N,N,p))

  end=FALSE

  beta_pre2=beta_pre+0.05
  # if ((sum(index1[,1])+sum(index1[,2]))>p) {
  #   beta_pre2=lm(c(yy[index1[,1],1],yy[index1[,2],2])~rbind(XX[index1[,1],,1],XX[index1[,2],,2]))$coef[-1]
  # } else {
  #   cvfit <- cv.glmnet(rbind(XX[index1[,1],,1],XX[index1[,2],,2]), c(yy[index1[,1],1],yy[index1[,2],2]), nfolds=3)
  #   fit <- cvfit$glmnet.fit
  #   beta_pre2=t(as.numeric(coef(fit, s=cvfit$lambda.min))[-1])
  # }

  # fg_temp=fg
  # Z1_temp=Z1
  # for (s in 1:m) {
  #   res[index1[,s],s]=yy[index1[,s],s]-XX[index1[,s],,s]%*%(as.numeric(beta_pre2))
  #   resvar[s]=var(res[index1[,s],s])
  #   g[tempequ1[s]:(tempequ1[s+1]-1)]=t(XX[index1[,s],index2[,s],s])%*%(res[index1[,s],s])/(resvar[s]*nn[s])
  #   Z2[,tempequ1[s]:(tempequ1[s+1]-1)]=apply(XX[,index2[,s],s], 2, function(x) x*res[,s])/(resvar[s]*nnn[s])
  #
  #   fg_temp[tempequ1[s]:(tempequ1[s+1]-1),]=fg_temp[tempequ1[s]:(tempequ1[s+1]-1),]*vary[s]/resvar[s]
  #   Z1_temp[,tempequ1[s]:(tempequ1[s+1]-1),]=Z1_temp[,tempequ1[s]:(tempequ1[s+1]-1),]*vary[s]/resvar[s]
  #
  #   # g[tempequ1[s]:(tempequ1[s+1]-1)]=t(XX[index1[,s],index2[,s],s])%*%(yy[index1[,s],s])/(vary[s]*nn[s])
  #   # Z2[,tempequ1[s]:(tempequ1[s+1]-1)]=apply(XX[,index2[,s],s], 2, function(x) x*yy[,s])/(vary[s]*nnn[s])
  # }
  # C=t(Z2)%*%Z2
  # C=C*size1
  # Q0=Qfun(C=C, g=g, PCA_ind=PCA_ind, PCA_ind0=PCA_ind0, N=N, n_pat=n_pat, eps3 = eps3)
  # q_star=Q0$q_star

  # print('Start')            #### Add print
  time=proc.time()
  # print(time)

  for (l in 1:L){
    reset=FALSE
    conjugate=FALSE
    alpha=alpha1
    lalpha=length(alpha)
    ff=rep(0,lalpha)
    # if (l==9) {
    #   print(l)
    # }

    if (l!=1) {
      beta_pre=beta[l-1,]
    }
    fq2=Qfirstdev1(h1=h1, beta_pre=beta_pre2, XX=XX, yy=yy, index1=index1, index2=index2, numequ=numequ, tempequ1=tempequ1, PCA_ind=PCA_ind, PCA_ind0=PCA_ind0, n_pat=n_pat, eps3=eps3, pat=pat, ind_C=ind_C, ind_pat=ind_pat, ratio = ratio)$fq_star
    # print("Get first derivative 1")
    time=proc.time()
    # print(time)
    pe2=rep(0,p)
    if (lambda[l]!=0) {
      for (j in 1:p) {
        u=abs(beta_pre2[j])
        # if (l==9) {
        #   print(u)
        # }
        if (u<=lambda[l]+1e-15) {
          if (u==0) {
            if (fq2[j]>lambda[l]) {
              pe2[j]=-lambda[l]
            } else if (fq2[j]<-lambda[l]) {
              pe2[j]=lambda[l]
            } else {
              pe2[j]=-fq2[j]
            }
          } else {
            pe2[j]=lambda[l]
          }
        } else if (u<=a*lambda[l]) {
          pe2[j]=(a*lambda[l]-u)/(a-1)
        } else {
          pe2[j]=0
        }
      }
    }
    firstdev2=fq2+pe2
    for (i in 1:max.iter) {
      if (lambda[l]!=0) {
        p1=sum(beta_pre!=0)
        ind=(beta_pre!=0)
        # fC=fC0[,,1:p1, drop=FALSE]
      } else {
        # fC=fC0
      }

      #if (sum(apply(missid,1,function(x) sum(ind[x], na.rm = T))!=0)<nm) {
      if (sum(ind)==0) {
        end=TRUE
        break
        # print(beta[l,])
      }

      #fg_temp=fg[,ind, drop=FALSE]
      #Z1_temp=Z1[,,ind, drop=FALSE]
      #size2_temp=size2[,,ind, drop=FALSE]

      for (s in 1:m) {
        res[index1[,s],s]=yy[index1[,s],s]-XX[index1[,s],,s]%*%(as.numeric(beta_pre))
        # resvar[s]=var(res[index1[,s],s])
        g[tempequ1[s]:(tempequ1[s+1]-1)]=t(XX[index1[,s],index2[,s],s])%*%(res[index1[,s],s])/(resvar[s]*nn[s])
        Z2[,tempequ1[s]:(tempequ1[s+1]-1)]=apply(XX[,index2[,s],s], 2, function(x) x*res[,s])/(resvar[s]*nnn[s])

        #fg_temp[tempequ1[s]:(tempequ1[s+1]-1),]=fg_temp[tempequ1[s]:(tempequ1[s+1]-1),]*vary[s]/resvar[s]
        #Z1_temp[,tempequ1[s]:(tempequ1[s+1]-1),]=Z1_temp[,tempequ1[s]:(tempequ1[s+1]-1),]*vary[s]/resvar[s]

      }

      if (is.null(C0)) {
        C=matrix(0,N,N)
        for (ii in 1:n_pat) {
          C[ind_C[ii]:(ind_C[ii+1]-1),ind_C[ii]:(ind_C[ii+1]-1)]=t(Z2[ind_pat[ii]:(ind_pat[ii+1]-1),ind_C[ii]:(ind_C[ii+1]-1), drop=FALSE])%*%Z2[ind_pat[ii]:(ind_pat[ii+1]-1),ind_C[ii]:(ind_C[ii+1]-1), drop=FALSE]
        }
        #C=t(Z2)%*%Z2
        C=C*size1

        # Q0=Qfun1(C=C, g=g, PCA_ind=PCA_ind, PCA_ind0=PCA_ind0, N=N, n_pat=n_pat, eps3 = eps3, index1=index1, pat=pat, res=res, numequ=numequ, tempequ1=tempequ1, ind_C=ind_C)
        # print("Get Q function value 1")
        # time=proc.time()
        # print(time)
        # q_star=Q0$q_star

        fq=Qfirstdev1(h1=h1, beta_pre=beta_pre, XX=XX, yy=yy, index1=index1, index2=index2, numequ=numequ, tempequ1=tempequ1, PCA_ind=PCA_ind, PCA_ind0=PCA_ind0, n_pat=n_pat, eps3=eps3, pat=pat, ind_C=ind_C, ind_pat=ind_pat, ratio = ratio)$fq_star
        # print("Get first derivative 2")
        time=proc.time()
        # print(time)

      }
      # else {
      #   fq=2*t(fg_temp)%*%solve(C0,g)
      #   sq=2*t(fg_temp)%*%solve(C0,fg_temp)
      # }

      pe=rep(0,p)
      if (lambda[l]!=0) {
        for (j in 1:p) {
          u=abs(beta_pre[j])
          if (u<=lambda[l]+1e-15) {
            if (u==0) {
              if (fq[j]>lambda[l]) {
                pe[j]=-lambda[l]
              } else if (fq[j]<-lambda[l]) {
                pe[j]=lambda[l]
              } else {
                pe[j]=-fq[j]
              }
            } else {
              pe[j]=lambda[l]
            }
          } else if (u<=a*lambda[l]) {
            pe[j]=(a*lambda[l]-u)/(a-1)
          } else {
            pe[j]=0
          }
          pe[j]=pe[j]*sign(beta_pre[j])
        }
        #pe=diag(lambda[l]/abs(beta_pre[ind]),p1,p1)     #p1: number of nonzero covariates
      }

      firstdev=fq+pe
      if (conjugate) {
        if (firstdev[ind]%*%conj2[ind]>=0) {
          reset=TRUE
        }
      }
      if (i==1 | reset) {
        reset=FALSE

        # print('before')
        ff = foreach (j = 1:lalpha, .combine = rbind, .packages = c("MASS", "Matrix")) %do% {
          beta_t0=beta_pre[ind]-alpha[j]*firstdev[ind]
          beta_t=beta_pre
          beta_t[ind]=beta_t0

          for (s in 1:m) {
            res[index1[,s],s]=yy[index1[,s],s]-XX[index1[,s],,s]%*%(as.numeric(beta_t))
            # resvar[s]=var(res[index1[,s],s])
            g[tempequ1[s]:(tempequ1[s+1]-1)]=t(XX[index1[,s],index2[,s],s])%*%(res[index1[,s],s])/(resvar[s]*nn[s])
            Z2[,tempequ1[s]:(tempequ1[s+1]-1)]=apply(XX[,index2[,s],s], 2, function(x) x*res[,s])/(resvar[s]*nnn[s])

            #fg_temp[tempequ1[s]:(tempequ1[s+1]-1),]=fg_temp[tempequ1[s]:(tempequ1[s+1]-1),]*vary[s]/resvar[s]
            #Z1_temp[,tempequ1[s]:(tempequ1[s+1]-1),]=Z1_temp[,tempequ1[s]:(tempequ1[s+1]-1),]*vary[s]/resvar[s]
          }
          C=matrix(0,N,N)
          for (ii in 1:n_pat) {
            C[ind_C[ii]:(ind_C[ii+1]-1),ind_C[ii]:(ind_C[ii+1]-1)]=t(Z2[ind_pat[ii]:(ind_pat[ii+1]-1),ind_C[ii]:(ind_C[ii+1]-1), drop=FALSE])%*%Z2[ind_pat[ii]:(ind_pat[ii+1]-1),ind_C[ii]:(ind_C[ii+1]-1), drop=FALSE]
          }
          #C=t(Z2)%*%Z2
          C=C*size1
          pe_t=0
          for (jj in 1:p) {
            u=abs(beta_t[jj])
            if (u<=lambda[l]) {
              pe_t=pe_t+lambda[l]*u
            } else if (u<=a*lambda[l]) {
              pe_t=pe_t-(u^2-2*a*lambda[l]*u+lambda[l]^2)/(2*(a-1))
            } else {
              pe_t=pe_t+(a+1)*lambda[l]^2/2
            }
          }
          ff_temp=Qfun1(C=C, g=g, PCA_ind=PCA_ind, PCA_ind0=PCA_ind0, N=N, n_pat=n_pat, eps3 = eps3, index1=index1, pat=pat, res=res, numequ=numequ, tempequ1=tempequ1, ind_C=ind_C)$q_star+pe_t
          return(ff_temp)
        }
        # print('after')

        beta_tmp=beta_pre[ind]-alpha[which.min(ff)]*firstdev[ind]
        conj2=-firstdev
      } else {
        conjugate=TRUE
        threshold=5*sqrt(sum(beta_pre[ind]^2))/(max(alpha)*sqrt(sum(conj2[ind]^2)))
        if ((conj2[ind]%*%(firstdev2[ind]-firstdev[ind]))==0) {
          gamma=threshold
        } else {
          gamma=max(0,-(firstdev[ind]%*%(firstdev[ind]))/(conj2[ind]%*%(firstdev2[ind]-firstdev[ind])))
          if (gamma>threshold) {
            gamma=threshold
          }
        }
        #gamma=max(0,(firstdev[ind]%*%(firstdev[ind]-firstdev2[ind]))/(firstdev2[ind]%*%firstdev2[ind]))
        #gamma=(firstdev%*%(firstdev))/(conj2%*%(firstdev-firstdev2))
        # if (abs(gamma)>100) {
        #   print(gamma)
        # }
        conj=-firstdev+gamma*conj2        ##### conjugate direction
        #gamma=as.numeric(((beta_pre-beta_pre2)%*%(firstdev-firstdev2))/(sum((firstdev-firstdev2)^2)))

        # print('before')
        ff = foreach (j = 1:lalpha, .combine = rbind, .packages = c("MASS", "Matrix")) %do% {
          beta_t0=beta_pre[ind]+alpha[j]*conj[ind]
          beta_t=beta_pre
          beta_t[ind]=beta_t0
          #fg_temp=fg
          #Z1_temp=Z1
          for (s in 1:m) {
            res[index1[,s],s]=yy[index1[,s],s]-XX[index1[,s],,s]%*%(as.numeric(beta_t))
            # resvar[s]=var(res[index1[,s],s])
            g[tempequ1[s]:(tempequ1[s+1]-1)]=t(XX[index1[,s],index2[,s],s])%*%(res[index1[,s],s])/(resvar[s]*nn[s])
            Z2[,tempequ1[s]:(tempequ1[s+1]-1)]=apply(XX[,index2[,s],s], 2, function(x) x*res[,s])/(resvar[s]*nnn[s])

            #fg_temp[tempequ1[s]:(tempequ1[s+1]-1),]=fg_temp[tempequ1[s]:(tempequ1[s+1]-1),]*vary[s]/resvar[s]
            #Z1_temp[,tempequ1[s]:(tempequ1[s+1]-1),]=Z1_temp[,tempequ1[s]:(tempequ1[s+1]-1),]*vary[s]/resvar[s]
          }
          C=matrix(0,N,N)
          for (ii in 1:n_pat) {
            C[ind_C[ii]:(ind_C[ii+1]-1),ind_C[ii]:(ind_C[ii+1]-1)]=t(Z2[ind_pat[ii]:(ind_pat[ii+1]-1),ind_C[ii]:(ind_C[ii+1]-1), drop=FALSE])%*%Z2[ind_pat[ii]:(ind_pat[ii+1]-1),ind_C[ii]:(ind_C[ii+1]-1), drop=FALSE]
          }
          #C=t(Z2)%*%Z2
          C=C*size1
          pe_t=0
          for (jj in 1:p) {
            u=abs(beta_t[jj])
            if (u<=lambda[l]) {
              pe_t=pe_t+lambda[l]*u
            } else if (u<=a*lambda[l]) {
              pe_t=pe_t-(u^2-2*a*lambda[l]*u+lambda[l]^2)/(2*(a-1))
            } else {
              pe_t=pe_t+(a+1)*lambda[l]^2/2
            }
          }
          ff_temp=Qfun1(C=C, g=g, PCA_ind=PCA_ind, PCA_ind0=PCA_ind0, N=N, n_pat=n_pat, eps3 = eps3, index1=index1, pat=pat, res=res, numequ=numequ, tempequ1=tempequ1, ind_C=ind_C)$q_star+pe_t
          return(ff_temp)
        }
        # print('after')

        # if (lambda[l]!=0) {
        #   print(lambda[l])
        # }
        # if (max(abs(alpha[which.min(ff)]*conj[ind]))>10) {
        #   print(abs(alpha[which.min(ff)]*conj[ind]))
        # }
        # if (l==31 & i>50) {
        #   print(alpha[which.min(ff)])
        # }
        # print(ff[which.min(ff)])
        beta_tmp=beta_pre[ind]+alpha[which.min(ff)]*conj[ind]
        conj2=conj
      }
      # print(ff[which.min(ff)])

      # if (abs(beta_tmp[1])<eps1) {
      #   print(i)
      # }
      if (lambda[l]!=0) {
        beta_tmp[abs(beta_tmp)<eps1]=0
        if (sum(abs(beta_tmp)<eps1)>0) {
          reset=TRUE
        }
      }
      beta[l,ind]=beta_tmp
      firstdev2=firstdev
      #print(min(ff))
      # print(i)
      # if (beta[l,1]<2.601 & beta[l,1]>2.6) {
      # if (l==31) {
      #   print(beta[l,])
      # }
      # print(q_star+lambda[l]*sum(abs(beta_pre)!=0))
      # }
      # print(g)
      # print(resvar)
      # print(q_star)
      # print(count)
      # if (t(g)%*%invCg+lambda[l]*sum(abs(beta[l,]))<1.1) {
      #   print(i)
      # }
      diff=max(abs(beta[l,]-beta_pre))
      # diff0=beta[l,]-beta_pre
      # print(t(g)%*%invCg+t(fq)%*%diff0[ind]+0.5*diff0[ind]%*%sq%*%diff0[ind]+0.5*beta[l,ind]%*%pe%*%beta[l,ind])
      #print(diff)
      if (i>11) {
        diff1=max(abs(beta[l,]-beta_pre2), abs(beta_pre-beta_pre3))
      }
      if (i>10) {
        beta_pre3=beta_pre2
      }
      beta_pre2=beta_pre
      beta_pre=beta[l,]
      if (diff<eps2) {
        break
      } else {
        if (i>11) {
          if (diff1<eps2) {
            alpha=alpha1/max(abs(conj2))
          }
        }
      }
      # print(paste("lambda=", lambda[l], ", iteration=", i, ", diff=", diff))            #### Add print
      time=proc.time()
      # print(time)
      # print(mem_used())
    }
    if (i==max.iter) {
      converge=FALSE
      notcon=notcon+1
      # print(diff)
      # print(l)
      #break     #test
    } else {
      converge=TRUE
    }
    if (end) {
      break
    }
    # print(l)
    for (s in 1:m) {
      res[index1[,s],s]=yy[index1[,s],s]-XX[index1[,s],,s]%*%(as.numeric(beta[l,]))
      # resvar[s]=var(res[index1[,s],s])
      g[tempequ1[s]:(tempequ1[s+1]-1)]=t(XX[index1[,s],index2[,s],s])%*%(res[index1[,s],s])/(resvar[s]*nn[s])
      Z2[,tempequ1[s]:(tempequ1[s+1]-1)]=apply(XX[,index2[,s],s], 2, function(x) x*res[,s])/(resvar[s]*nnn[s])
    }

    if (is.null(C0)) {
      C=matrix(0,N,N)
      for (ii in 1:n_pat) {
        C[ind_C[ii]:(ind_C[ii+1]-1),ind_C[ii]:(ind_C[ii+1]-1)]=t(Z2[ind_pat[ii]:(ind_pat[ii+1]-1),ind_C[ii]:(ind_C[ii+1]-1), drop=FALSE])%*%Z2[ind_pat[ii]:(ind_pat[ii+1]-1),ind_C[ii]:(ind_C[ii+1]-1), drop=FALSE]
      }
      #C=t(Z2)%*%Z2
      C=C*size1

      Q0=Qfun1(C=C, g=g, PCA_ind=PCA_ind, PCA_ind0=PCA_ind0, N=N, n_pat=n_pat, eps3 = eps3, index1=index1, pat=pat, res=res, numequ=numequ, tempequ1=tempequ1, ind_C=ind_C)
      # print("Get Q function value 4")
      time=proc.time()
      # print(time)

      TT=Q0$T2[1:(Q0$count-1),, drop=FALSE]%*%Q0$T1
      g_star=TT%*%g
      C_star=TT%*%C%*%t(TT)
      # if(!class(try(solve(C_star),silent=T))=='matrix') {
      #   print(eigen(C_star)$values)
      #   print(C)
      #   print(g)
      #   print(l)
      #   print(resvar)
      # }
      invCg_star=solve(C_star,g_star)
    } else {
      invCg=solve(C0,g)
    }

    qq[l]=t(g_star)%*%invCg_star
    rss[l]=sum(res[,index0]^2)
    for (k1 in 1:n_pat) {
      rss1[l]=rss1[l]+sum(res[,pat==k1]^2)/sum(pat==k1)
    }
    biq[l]=qq[l]+sum(beta[l,]!=0)*log(n)
    aiq[l]=qq[l]+sum(beta[l,]!=0)*2
    bic[l]=n*log(rss[l])+sum(beta[l,]!=0)*log(n)
    bic1[l]=n*log(rss1[l])+sum(beta[l,]!=0)*log(n)
    ebic1[l]=n*log(rss[l])+(log(n)+2*gamma.ebic*log(p))*sum(beta[l,]!=0)
    ebic2[l]=n*log(rss[l])+log(n)*sum(beta[l,]!=0)+2*gamma.ebic*log(choose(p,sum(beta[l,]!=0)))
    ebic11[l]=n*log(rss1[l])+(log(n)+2*gamma.ebic*log(p))*sum(beta[l,]!=0)
    ebic21[l]=n*log(rss1[l])+log(n)*sum(beta[l,]!=0)+2*gamma.ebic*log(choose(p,sum(beta[l,]!=0)))
    ebicM[l]=n*log(rss[l])+(log(n)+2*log(p))*sum(beta[l,]!=0)
    ebicM1[l]=n*log(rss1[l])+(log(n)+2*log(p))*sum(beta[l,]!=0)
    objective[l,1]=n*log(rss[l])
    objective[l,2]=log(n)*sum(beta[l,]!=0)
    objective1[l,1]=n*log(rss1[l])              ### newly added
    objective1[l,2]=log(n)*sum(beta[l,]!=0)
    beta_out[l,]=beta[l,]/scalex
    intercept[l] = centery-crossprod(beta_out[l,],as.numeric(centerx))
  }
  returnlist=list("beta"=beta_out, 'bic1'=bic1, "lambda"=lambda, "notcon"=notcon, "intercept"=intercept, "beta0"=beta)
  return(returnlist)
}


Qfirstdev1 <- function (h1=2^(-(8:30)), beta_pre, XX, yy, index1, index2, numequ, tempequ1, PCA_ind, PCA_ind0, n_pat, eps3, pat, ind_C, ind_pat, ratio) {
  dims=dim(XX)
  n=dims[1]
  p=dims[2]
  m=dims[3]
  res=matrix(0,n,m)
  res1=matrix(0,n,m)
  res2=matrix(0,n,m)
  resvar=rep(1,m)
  resvar1=rep(1,m)
  resvar2=rep(1,m)
  N=sum(index2)
  Z2=matrix(0,n,N)
  Z21=matrix(0,n,N)
  Z22=matrix(0,n,N)
  g=rep(0,N)
  g1=rep(0,N)
  g2=rep(0,N)
  nn=apply(index1,2,sum)
  nnn=sqrt(apply(index1,2,sum))


  for (s in 1:m) {
    res[index1[,s],s]=yy[index1[,s],s]-XX[index1[,s],,s]%*%(as.numeric(beta_pre))
    # resvar1[s]=var(res1[index1[,s],s])
    g[tempequ1[s]:(tempequ1[s+1]-1)]=t(XX[index1[,s],index2[,s],s])%*%(res[index1[,s],s])/(resvar[s]*nn[s])
    Z2[,tempequ1[s]:(tempequ1[s+1]-1)]=apply(XX[,index2[,s],s], 2, function(x) x*res[,s])/(resvar[s]*nnn[s])
  }
  C=matrix(0,N,N)
  for (ii in 1:n_pat) {
    C[ind_C[ii]:(ind_C[ii+1]-1),ind_C[ii]:(ind_C[ii+1]-1)]=t(Z2[ind_pat[ii]:(ind_pat[ii+1]-1),ind_C[ii]:(ind_C[ii+1]-1), drop=FALSE])%*%Z2[ind_pat[ii]:(ind_pat[ii+1]-1),ind_C[ii]:(ind_C[ii+1]-1), drop=FALSE]
  }
  #C=t(Z2)%*%Z2
  maxeigen=base::norm((C+t(C))/2, type = '2')
  #print(maxeigen)

  qfun=Qfun1(C=C, g=g, PCA_ind=PCA_ind, PCA_ind0=PCA_ind0, N=N, n_pat=n_pat, eps3 = eps3, index1=index1, pat=pat, res=res, numequ=numequ, tempequ1=tempequ1, maxeigen=maxeigen, ind_C=ind_C)
  tempT=qfun$tempT
  C_star=qfun$C_star
  S_C_star=solve(C_star)


  #fq_star=rep(0,p)
  # start.time = proc.time()
  j1=NA
  result = foreach (j1 = 1:p, .combine = rbind, .packages = c("MASS", "Matrix")) %do% {
    h=h1
    if (abs(beta_pre[j1])<1e-2 & abs(beta_pre[j1])>0) {
      h=h1*max(1e-2, abs(beta_pre[j1]))
    }
    temp_fq=0
    temp_sq=0
    temp_diff1=Inf
    temp_diff2=Inf
    for (hh in 1:length(h)) {
      # start.time = proc.time()
      beta_temp1=beta_pre
      beta_temp2=beta_pre
      beta_temp1[j1]=beta_pre[j1]+h[hh]
      beta_temp2[j1]=beta_pre[j1]-h[hh]

      for (s in 1:m) {
        res1[index1[,s],s]=yy[index1[,s],s]-XX[index1[,s],,s]%*%(as.numeric(beta_temp1))
        # resvar1[s]=var(res1[index1[,s],s])
        g1[tempequ1[s]:(tempequ1[s+1]-1)]=t(XX[index1[,s],index2[,s],s])%*%(res1[index1[,s],s])/(resvar1[s]*nn[s])
        # Z21[,tempequ1[s]:(tempequ1[s+1]-1)]=apply(XX[,index2[,s],s], 2, function(x) x*res1[,s])/(resvar1[s]*nnn[s])

        res2[index1[,s],s]=yy[index1[,s],s]-XX[index1[,s],,s]%*%(as.numeric(beta_temp2))
        # resvar2[s]=var(res2[index1[,s],s])
        g2[tempequ1[s]:(tempequ1[s+1]-1)]=t(XX[index1[,s],index2[,s],s])%*%(res2[index1[,s],s])/(resvar2[s]*nn[s])
        # Z22[,tempequ1[s]:(tempequ1[s+1]-1)]=apply(XX[,index2[,s],s], 2, function(x) x*res2[,s])/(resvar2[s]*nnn[s])
      }
      # C1=matrix(0,N,N)
      # C2=matrix(0,N,N)
      # for (i in 1:n_pat) {
      #   C1[ind_C[i]:(ind_C[i+1]-1),ind_C[i]:(ind_C[i+1]-1)]=t(Z21[ind_pat[i]:(ind_pat[i+1]-1),ind_C[i]:(ind_C[i+1]-1), drop=FALSE])%*%Z21[ind_pat[i]:(ind_pat[i+1]-1),ind_C[i]:(ind_C[i+1]-1), drop=FALSE]
      #   C2[ind_C[i]:(ind_C[i+1]-1),ind_C[i]:(ind_C[i+1]-1)]=t(Z22[ind_pat[i]:(ind_pat[i+1]-1),ind_C[i]:(ind_C[i+1]-1), drop=FALSE])%*%Z22[ind_pat[i]:(ind_pat[i+1]-1),ind_C[i]:(ind_C[i+1]-1), drop=FALSE]
      # }
      #C1=t(Z21)%*%Z21
      #C2=t(Z22)%*%Z22

      # time1 = proc.time() - start.time
      # start.time = proc.time()

      # q_star1=Qfun1(C=C1, g=g1, PCA_ind=PCA_ind, PCA_ind0=PCA_ind0, N=N, n_pat=n_pat, eps3 = eps3, index1=index1, pat=pat, res=res1, numequ=numequ, tempequ1=tempequ1, maxeigen=maxeigen, ind_C=ind_C)$q_star
      # q_star2=Qfun1(C=C2, g=g2, PCA_ind=PCA_ind, PCA_ind0=PCA_ind0, N=N, n_pat=n_pat, eps3 = eps3, index1=index1, pat=pat, res=res2, numequ=numequ, tempequ1=tempequ1, maxeigen=maxeigen, ind_C=ind_C)$q_star


      g_star1=tempT%*%g1
      g_star2=tempT%*%g2
      q_star1=t(g_star1)%*%S_C_star%*%g_star1
      q_star2=t(g_star2)%*%S_C_star%*%g_star2


      # time2 = proc.time() - start.time
      # start.time = proc.time()

      fq_star_par=(q_star1-q_star2)/(2*h[hh])
      #print(sq_star[j1,j1])
      if (hh==1) {
        fq_pre=fq_star_par
      } else {
        # if (fq_pre!=0) {
        fq_diff=max(abs((fq_star_par-fq_pre)))
        # } else {
        #   fq_diff==max(abs((fq_star[j1]-fq_pre)))
        # }
        # if (is.na(fq_diff) | is.nan(fq_diff) | is.infinite(fq_diff)) {
        #   print(fq_diff)
        # }
        if (fq_diff<temp_diff1) {
          temp_diff1=fq_diff
          temp_fq=fq_star_par
        }
        if (fq_diff<1e-3) {
          break
        } else {
          fq_pre=fq_star_par
        }
      }
      # time3 = proc.time() - start.time
    }
    if (fq_diff>1e-3) {
      fq_star_par=temp_fq
      # print(paste(j1,l,i))
    }
    # print(hh)
    return(c(fq_star_par,q_star1,q_star2))
  }
  # time = proc.time() - start.time
  returnlist=list('fq_star'=result[,1]+rnorm(p,0,0.1), 'q_star1'=result[,2], 'q_star2'=result[,3])
  return(returnlist)
}


Qfun1 <- function (C, g, PCA_ind, PCA_ind0, N, n_pat, eps3=1e-8, index1, pat, res, numequ, tempequ1, maxeigen=NULL, ind_C) {
  # start.time = proc.time()
  U_ind=array(NA,dim=c(n_pat,2,N))
  U_ind0=matrix(0,n_pat,2)
  T1=diag(1,N,N)
  T2=matrix(0,N,N)

  # time1 = proc.time() - start.time
  # start.time = proc.time()

  #maxeigen=max(eigen((C+t(C))/2)$values)
  if (is.null(maxeigen)){
    maxeigen=base::norm((C+t(C))/2, type = '2')
  }
  # print(maxeigen)

  # time2 = proc.time() - start.time
  # start.time = proc.time()

  count=1
  for (k1 in 1:n_pat) {
    id_im=which(pat==k1)
    n_im=length(id_im)
    if (n_im>1) {
      PCA_ind[k1,2,]=NA
      PCA_ind0[k1,2]=0
      for (i in 2:n_im) {
        if (sum(abs(res[,id_im[1]]-res[,id_im[i]]))==0) {     ##### If residuals of other layers are the same as the main layer in the same group?
          id_im[i]=0
        } else {
          PCA_ind[k1,2,(PCA_ind0[k1,2]+1):(PCA_ind0[k1,2]+numequ[id_im[i]])]=tempequ1[id_im[i]]:(tempequ1[id_im[i]+1]-1)
          PCA_ind0[k1,2]=PCA_ind0[k1,2]+numequ[id_im[i]]
        }
      }
    }
    for (k2 in 1:2) {
      if (PCA_ind0[k1,k2]!=0) {
        if (k2==1 | U_ind0[k1,1]==0) {
          cov_temp=C[PCA_ind[k1,k2,1:PCA_ind0[k1,k2]], PCA_ind[k1,k2,1:PCA_ind0[k1,k2]], drop=FALSE]
        } else {
          temp1=T2[U_ind[k1,1,1:U_ind0[k1,1]], PCA_ind[k1,1,1:PCA_ind0[k1,1]], drop=FALSE]
          temp2=solve(temp1%*%C[PCA_ind[k1,1,1:PCA_ind0[k1,1]], PCA_ind[k1,1,1:PCA_ind0[k1,1]], drop=FALSE]%*%t(temp1))
          temp3=C[PCA_ind[k1,k2,1:PCA_ind0[k1,k2]], PCA_ind[k1,1,1:PCA_ind0[k1,1]], drop=FALSE]%*%t(temp1)
          T1[PCA_ind[k1,k2,1:PCA_ind0[k1,k2]], PCA_ind[k1,1,1:PCA_ind0[k1,1]]]=-temp3%*%temp2%*%temp1
          temp_ind=c(PCA_ind[k1,1,1:PCA_ind0[k1,1]],PCA_ind[k1,k2,1:PCA_ind0[k1,k2]])
          temp4=T1[temp_ind, temp_ind, drop=FALSE]%*%C[temp_ind,temp_ind]%*%t(T1[temp_ind,temp_ind])
          cov_temp=temp4[(PCA_ind0[k1,1]+1):(PCA_ind0[k1,1]+PCA_ind0[k1,2]),(PCA_ind0[k1,1]+1):(PCA_ind0[k1,1]+PCA_ind0[k1,2])]
          #cov_temp=C[PCA_ind[k1,k2,1:PCA_ind0[k1,k2]], PCA_ind[k1,k2,1:PCA_ind0[k1,k2]], drop=FALSE]-temp3%*%temp2%*%t(temp3)
        }
        eigens=eigen((cov_temp+t(cov_temp))/2)
        values=eigens$values
        # if (is.complex(values)) {
        #   print(values)
        # }
        if (max(values)>maxeigen) {
          maxeigen=max(values)
        }
        # eigenthreshold=max(eps3, maxeigen*1e-10)
        n_tmp0=sum(index1[,min(which(pat==k1)), drop=FALSE])   ### Same as pQIFmp_scad20.R, except using the BIC in Cho's paper and ask number of selected PCA less than n
        nr=n_tmp0*PCA_ind0[k1,k2]
        if (k2==1) {                                           ### Ask number of selected PCA less than n
          tm=n_tmp0-1
        } else {
          tm=n_tmp0-1-U_ind0[k1,1]
        }
        eigenthreshold=max(eps3, maxeigen*1e-10, sum(values)*log(nr)/(nr))
        rank=rankMatrix(cov_temp)
        if (min(values)>eigenthreshold & rank==min(dim(cov_temp)) & min(dim(cov_temp))<=tm) {
          U_ind0[k1,k2]=PCA_ind0[k1,k2]
          U_ind[k1,k2,1:U_ind0[k1,k2]]=count:(count+U_ind0[k1,k2]-1)
          #nu_num[count+1]=nu_num[count]+PCA_ind0[k1,k2]
          T2[U_ind[k1,k2,1:U_ind0[k1,k2]], PCA_ind[k1,k2,1:PCA_ind0[k1,k2]]]=diag(1,PCA_ind0[k1,k2],PCA_ind0[k1,k2])
        } else if (max(values)>eigenthreshold & rank>0 & tm>0) {
          #nu_num[count+1]=nu_num[count]+sum(values>0)
          # if (k1>1) {
          #   print(k1)
          # }
          U_ind0[k1,k2]=max(1,min(apply(index1[,which(pat==k1), drop=FALSE],2,sum), sum(values>eigenthreshold), rank, tm))
          U_ind[k1,k2,1:U_ind0[k1,k2]]=count:(count+U_ind0[k1,k2]-1)
          U_temp=as.matrix(eigens$vectors[,1:U_ind0[k1,k2]])
          T2[U_ind[k1,k2,1:U_ind0[k1,k2]], PCA_ind[k1,k2,1:PCA_ind0[k1,k2]]]=t(U_temp%*%diag(sign(U_temp[dim(U_temp)[1],]), length(sign(U_temp[dim(U_temp)[1],])), length(sign(U_temp[dim(U_temp)[1],]))))
        }
        count=count+U_ind0[k1,k2]
      }
    }
  }
  # print(count)

  # time3 = proc.time() - start.time
  # start.time = proc.time()

  ################################ Block diagonal matrix multiplication ####################
  ind_tT=rep(1,n_pat+1)
  #ind_Cstar=rep(1,n_pat+1)
  tempT=matrix(0,(count-1),N)
  C_star=matrix(0,(count-1),(count-1))
  for (ii in 1:n_pat) {
    ind_tT[ii+1]=ind_tT[ii]+sum(U_ind0[ii,])
    #ind_Cstar[ii+1]=ind_Cstar[ii]+sum(PCA_ind0[ii,])
    tempT[ind_tT[ii]:(ind_tT[ii+1]-1),ind_C[ii]:(ind_C[ii+1]-1)]=T2[ind_tT[ii]:(ind_tT[ii+1]-1),ind_C[ii]:(ind_C[ii+1]-1), drop=FALSE]%*%T1[ind_C[ii]:(ind_C[ii+1]-1),ind_C[ii]:(ind_C[ii+1]-1), drop=FALSE]
    C_star[ind_tT[ii]:(ind_tT[ii+1]-1),ind_tT[ii]:(ind_tT[ii+1]-1)]=tempT[ind_tT[ii]:(ind_tT[ii+1]-1),ind_C[ii]:(ind_C[ii+1]-1), drop=FALSE]%*%C[ind_C[ii]:(ind_C[ii+1]-1),ind_C[ii]:(ind_C[ii+1]-1), drop=FALSE]%*%t(tempT[ind_tT[ii]:(ind_tT[ii+1]-1),ind_C[ii]:(ind_C[ii+1]-1), drop=FALSE])
  }
  #time6 = proc.time() - start.time
  #tempT=T2[1:(count-1),, drop=FALSE]%*%T1
  #C_star=tempT%*%C%*%t(tempT)

  g_star=tempT%*%g
  C_star=(C_star+t(C_star))/2
  # if (min(eigen(C_star)$values)<1e-10) {
  #   print(min(eigen(C_star)$values))
  # }

  # time4 = proc.time() - start.time
  # start.time = proc.time()

  # if(!class(try(solve(C_star),silent=T))=='matrix') {
  #   print(eigen(C_star)$values)
  #   print(C)
  #   print(g)
  # }
  q_star=t(g_star)%*%solve(C_star,g_star)

  # time5 = proc.time() - start.time
  returnlist=list('q_star'=q_star, 'C_star'=C_star, 'g_star'=g_star, 'T1'=T1, 'T2'=T2, 'count'=count, 'tempT'=tempT)
  return(returnlist)
}



Qfun1 <- function (C, g, PCA_ind, PCA_ind0, N, n_pat, eps3=1e-8, index1, pat, res, numequ, tempequ1, maxeigen=NULL, ind_C) {
  # start.time = proc.time()
  U_ind=array(NA,dim=c(n_pat,2,N))
  U_ind0=matrix(0,n_pat,2)
  T1=diag(1,N,N)
  T2=matrix(0,N,N)

  # time1 = proc.time() - start.time
  # start.time = proc.time()

  #maxeigen=max(eigen((C+t(C))/2)$values)
  if (is.null(maxeigen)){
    maxeigen=base::norm((C+t(C))/2, type = '2')
  }
  # print(maxeigen)

  # time2 = proc.time() - start.time
  # start.time = proc.time()

  count=1
  for (k1 in 1:n_pat) {
    id_im=which(pat==k1)
    n_im=length(id_im)
    if (n_im>1) {
      PCA_ind[k1,2,]=NA
      PCA_ind0[k1,2]=0
      for (i in 2:n_im) {
        if (sum(abs(res[,id_im[1]]-res[,id_im[i]]))==0) {     ##### If residuals of other layers are the same as the main layer in the same group?
          id_im[i]=0
        } else {
          PCA_ind[k1,2,(PCA_ind0[k1,2]+1):(PCA_ind0[k1,2]+numequ[id_im[i]])]=tempequ1[id_im[i]]:(tempequ1[id_im[i]+1]-1)
          PCA_ind0[k1,2]=PCA_ind0[k1,2]+numequ[id_im[i]]
        }
      }
    }
    for (k2 in 1:2) {
      if (PCA_ind0[k1,k2]!=0) {
        if (k2==1 | U_ind0[k1,1]==0) {
          cov_temp=C[PCA_ind[k1,k2,1:PCA_ind0[k1,k2]], PCA_ind[k1,k2,1:PCA_ind0[k1,k2]], drop=FALSE]
        } else {
          temp1=T2[U_ind[k1,1,1:U_ind0[k1,1]], PCA_ind[k1,1,1:PCA_ind0[k1,1]], drop=FALSE]
          temp2=solve(temp1%*%C[PCA_ind[k1,1,1:PCA_ind0[k1,1]], PCA_ind[k1,1,1:PCA_ind0[k1,1]], drop=FALSE]%*%t(temp1))
          temp3=C[PCA_ind[k1,k2,1:PCA_ind0[k1,k2]], PCA_ind[k1,1,1:PCA_ind0[k1,1]], drop=FALSE]%*%t(temp1)
          T1[PCA_ind[k1,k2,1:PCA_ind0[k1,k2]], PCA_ind[k1,1,1:PCA_ind0[k1,1]]]=-temp3%*%temp2%*%temp1
          temp_ind=c(PCA_ind[k1,1,1:PCA_ind0[k1,1]],PCA_ind[k1,k2,1:PCA_ind0[k1,k2]])
          temp4=T1[temp_ind, temp_ind, drop=FALSE]%*%C[temp_ind,temp_ind]%*%t(T1[temp_ind,temp_ind])
          cov_temp=temp4[(PCA_ind0[k1,1]+1):(PCA_ind0[k1,1]+PCA_ind0[k1,2]),(PCA_ind0[k1,1]+1):(PCA_ind0[k1,1]+PCA_ind0[k1,2])]
          #cov_temp=C[PCA_ind[k1,k2,1:PCA_ind0[k1,k2]], PCA_ind[k1,k2,1:PCA_ind0[k1,k2]], drop=FALSE]-temp3%*%temp2%*%t(temp3)
        }
        eigens=eigen((cov_temp+t(cov_temp))/2)
        values=eigens$values
        # if (is.complex(values)) {
        #   print(values)
        # }
        if (max(values)>maxeigen) {
          maxeigen=max(values)
        }
        # eigenthreshold=max(eps3, maxeigen*1e-10)
        n_tmp0=sum(index1[,min(which(pat==k1)), drop=FALSE])   ### Same as pQIFmp_scad20.R, except using the BIC in Cho's paper and ask number of selected PCA less than n
        nr=n_tmp0*PCA_ind0[k1,k2]
        if (k2==1) {                                           ### Ask number of selected PCA less than n
          tm=n_tmp0-1
        } else {
          tm=n_tmp0-1-U_ind0[k1,1]
        }
        eigenthreshold=max(eps3, maxeigen*1e-10, sum(values)*log(nr)/(nr))
        rank=rankMatrix(cov_temp)
        if (min(values)>eigenthreshold & rank==min(dim(cov_temp)) & min(dim(cov_temp))<=tm) {
          U_ind0[k1,k2]=PCA_ind0[k1,k2]
          U_ind[k1,k2,1:U_ind0[k1,k2]]=count:(count+U_ind0[k1,k2]-1)
          #nu_num[count+1]=nu_num[count]+PCA_ind0[k1,k2]
          T2[U_ind[k1,k2,1:U_ind0[k1,k2]], PCA_ind[k1,k2,1:PCA_ind0[k1,k2]]]=diag(1,PCA_ind0[k1,k2],PCA_ind0[k1,k2])
        } else if (max(values)>eigenthreshold & rank>0 & tm>0) {
          #nu_num[count+1]=nu_num[count]+sum(values>0)
          # if (k1>1) {
          #   print(k1)
          # }
          U_ind0[k1,k2]=max(1,min(apply(index1[,which(pat==k1), drop=FALSE],2,sum), sum(values>eigenthreshold), rank, tm))
          U_ind[k1,k2,1:U_ind0[k1,k2]]=count:(count+U_ind0[k1,k2]-1)
          U_temp=as.matrix(eigens$vectors[,1:U_ind0[k1,k2]])
          T2[U_ind[k1,k2,1:U_ind0[k1,k2]], PCA_ind[k1,k2,1:PCA_ind0[k1,k2]]]=t(U_temp%*%diag(sign(U_temp[dim(U_temp)[1],]), length(sign(U_temp[dim(U_temp)[1],])), length(sign(U_temp[dim(U_temp)[1],]))))
        }
        count=count+U_ind0[k1,k2]
      }
    }
  }
  # print(count)

  # time3 = proc.time() - start.time
  # start.time = proc.time()

  ################################ Block diagonal matrix multiplication ####################
  ind_tT=rep(1,n_pat+1)
  #ind_Cstar=rep(1,n_pat+1)
  tempT=matrix(0,(count-1),N)
  C_star=matrix(0,(count-1),(count-1))
  for (ii in 1:n_pat) {
    ind_tT[ii+1]=ind_tT[ii]+sum(U_ind0[ii,])
    #ind_Cstar[ii+1]=ind_Cstar[ii]+sum(PCA_ind0[ii,])
    tempT[ind_tT[ii]:(ind_tT[ii+1]-1),ind_C[ii]:(ind_C[ii+1]-1)]=T2[ind_tT[ii]:(ind_tT[ii+1]-1),ind_C[ii]:(ind_C[ii+1]-1), drop=FALSE]%*%T1[ind_C[ii]:(ind_C[ii+1]-1),ind_C[ii]:(ind_C[ii+1]-1), drop=FALSE]
    C_star[ind_tT[ii]:(ind_tT[ii+1]-1),ind_tT[ii]:(ind_tT[ii+1]-1)]=tempT[ind_tT[ii]:(ind_tT[ii+1]-1),ind_C[ii]:(ind_C[ii+1]-1), drop=FALSE]%*%C[ind_C[ii]:(ind_C[ii+1]-1),ind_C[ii]:(ind_C[ii+1]-1), drop=FALSE]%*%t(tempT[ind_tT[ii]:(ind_tT[ii+1]-1),ind_C[ii]:(ind_C[ii+1]-1), drop=FALSE])
  }
  #time6 = proc.time() - start.time
  #tempT=T2[1:(count-1),, drop=FALSE]%*%T1
  #C_star=tempT%*%C%*%t(tempT)

  g_star=tempT%*%g
  C_star=(C_star+t(C_star))/2
  # if (min(eigen(C_star)$values)<1e-10) {
  #   print(min(eigen(C_star)$values))
  # }

  # time4 = proc.time() - start.time
  # start.time = proc.time()

  # if(!class(try(solve(C_star),silent=T))=='matrix') {
  #   print(eigen(C_star)$values)
  #   print(C)
  #   print(g)
  # }
  q_star=t(g_star)%*%solve(C_star,g_star)

  # time5 = proc.time() - start.time
  returnlist=list('q_star'=q_star, 'C_star'=C_star, 'g_star'=g_star, 'T1'=T1, 'T2'=T2, 'count'=count, 'tempT'=tempT)
  return(returnlist)
}


Qfirstdev2 <- function (h1=2^(-(8:30)), beta_pre, XX, yy, index1, index2, numequ, tempequ1, PCA_ind, PCA_ind0, n_pat, eps3, pat, ind_C, ind_pat, ratio) {
  dims=dim(XX)
  n=dims[1]
  p=dims[2]
  m=dims[3]
  res=matrix(0,n,m)
  res1=matrix(0,n,m)
  res2=matrix(0,n,m)
  resvar=rep(1,m)
  resvar1=rep(1,m)
  resvar2=rep(1,m)
  N=sum(index2)
  Z2=matrix(0,n,N)
  Z21=matrix(0,n,N)
  Z22=matrix(0,n,N)
  g1=rep(0,N)
  g2=rep(0,N)
  nn=apply(index1,2,sum)
  nnn=sqrt(apply(index1,2,sum))

  for (s in 1:m) {
    res[index1[,s],s]=yy[index1[,s],s]-XX[index1[,s],,s]%*%(as.numeric(beta_pre))
    # resvar1[s]=var(res1[index1[,s],s])
    Z2[,tempequ1[s]:(tempequ1[s+1]-1)]=apply(XX[,index2[,s],s], 2, function(x) x*res[,s])/(resvar[s]*nnn[s])
  }
  C=matrix(0,N,N)
  for (ii in 1:n_pat) {
    C[ind_C[ii]:(ind_C[ii+1]-1),ind_C[ii]:(ind_C[ii+1]-1)]=t(Z2[ind_pat[ii]:(ind_pat[ii+1]-1),ind_C[ii]:(ind_C[ii+1]-1), drop=FALSE])%*%Z2[ind_pat[ii]:(ind_pat[ii+1]-1),ind_C[ii]:(ind_C[ii+1]-1), drop=FALSE]
  }
  #C=t(Z2)%*%Z2
  maxeigen=base::norm((C+t(C))/2, type = '2')
  #print(maxeigen)

  #fq_star=rep(0,p)
  # start.time = proc.time()
  p1=round(p*ratio)
  index_random=sort(sample.int(p, p1))
  result=matrix(0, p, 3)
  j1=NA
  result[index_random,] = foreach (j1 = index_random, .combine = rbind, .packages = c("MASS", "Matrix")) %do% {
    h=h1
    if (abs(beta_pre[j1])<1e-2 & abs(beta_pre[j1])>0) {
      h=h1*max(1e-2, abs(beta_pre[j1]))
    }
    temp_fq=0
    temp_sq=0
    temp_diff1=Inf
    temp_diff2=Inf
    for (hh in 1:length(h)) {
      # start.time = proc.time()
      beta_temp1=beta_pre
      beta_temp2=beta_pre
      beta_temp1[j1]=beta_pre[j1]+h[hh]
      beta_temp2[j1]=beta_pre[j1]-h[hh]

      for (s in 1:m) {
        res1[index1[,s],s]=yy[index1[,s],s]-XX[index1[,s],,s]%*%(as.numeric(beta_temp1))
        # resvar1[s]=var(res1[index1[,s],s])
        g1[tempequ1[s]:(tempequ1[s+1]-1)]=t(XX[index1[,s],index2[,s],s])%*%(res1[index1[,s],s])/(resvar1[s]*nn[s])
        Z21[,tempequ1[s]:(tempequ1[s+1]-1)]=apply(XX[,index2[,s],s], 2, function(x) x*res1[,s])/(resvar1[s]*nnn[s])

        res2[index1[,s],s]=yy[index1[,s],s]-XX[index1[,s],,s]%*%(as.numeric(beta_temp2))
        # resvar2[s]=var(res2[index1[,s],s])
        g2[tempequ1[s]:(tempequ1[s+1]-1)]=t(XX[index1[,s],index2[,s],s])%*%(res2[index1[,s],s])/(resvar2[s]*nn[s])
        Z22[,tempequ1[s]:(tempequ1[s+1]-1)]=apply(XX[,index2[,s],s], 2, function(x) x*res2[,s])/(resvar2[s]*nnn[s])
      }
      C1=matrix(0,N,N)
      C2=matrix(0,N,N)
      for (i in 1:n_pat) {
        C1[ind_C[i]:(ind_C[i+1]-1),ind_C[i]:(ind_C[i+1]-1)]=t(Z21[ind_pat[i]:(ind_pat[i+1]-1),ind_C[i]:(ind_C[i+1]-1), drop=FALSE])%*%Z21[ind_pat[i]:(ind_pat[i+1]-1),ind_C[i]:(ind_C[i+1]-1), drop=FALSE]
        C2[ind_C[i]:(ind_C[i+1]-1),ind_C[i]:(ind_C[i+1]-1)]=t(Z22[ind_pat[i]:(ind_pat[i+1]-1),ind_C[i]:(ind_C[i+1]-1), drop=FALSE])%*%Z22[ind_pat[i]:(ind_pat[i+1]-1),ind_C[i]:(ind_C[i+1]-1), drop=FALSE]
      }
      #C1=t(Z21)%*%Z21
      #C2=t(Z22)%*%Z22

      # time1 = proc.time() - start.time
      # start.time = proc.time()

      q_star1=Qfun1(C=C1, g=g1, PCA_ind=PCA_ind, PCA_ind0=PCA_ind0, N=N, n_pat=n_pat, eps3 = eps3, index1=index1, pat=pat, res=res1, numequ=numequ, tempequ1=tempequ1, maxeigen=maxeigen, ind_C=ind_C)$q_star
      q_star2=Qfun1(C=C2, g=g2, PCA_ind=PCA_ind, PCA_ind0=PCA_ind0, N=N, n_pat=n_pat, eps3 = eps3, index1=index1, pat=pat, res=res2, numequ=numequ, tempequ1=tempequ1, maxeigen=maxeigen, ind_C=ind_C)$q_star

      # time2 = proc.time() - start.time
      # start.time = proc.time()

      fq_star_par=(q_star1-q_star2)/(2*h[hh])
      #print(sq_star[j1,j1])
      if (hh==1) {
        fq_pre=fq_star_par
      } else {
        # if (fq_pre!=0) {
        fq_diff=max(abs((fq_star_par-fq_pre)))
        # } else {
        #   fq_diff==max(abs((fq_star[j1]-fq_pre)))
        # }
        # if (is.na(fq_diff) | is.nan(fq_diff) | is.infinite(fq_diff)) {
        #   print(fq_diff)
        # }
        if (fq_diff<temp_diff1) {
          temp_diff1=fq_diff
          temp_fq=fq_star_par
        }
        if (fq_diff<1e-3) {
          break
        } else {
          fq_pre=fq_star_par
        }
      }
      # time3 = proc.time() - start.time
    }
    if (fq_diff>1e-3) {
      fq_star_par=temp_fq
      # print(paste(j1,l,i))
    }
    # print(hh)
    return(c(fq_star_par,q_star1,q_star2))
  }
  # time = proc.time() - start.time
  returnlist=list('fq_star'=result[,1], 'q_star1'=result[,2], 'q_star2'=result[,3])
  return(returnlist)
}



