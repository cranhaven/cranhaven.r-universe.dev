#' @title Simulates longitudinal data based on the iCARH model.
#'
#' @description Simulates longitudinal data based on the iCARH model.
#' Returns two types of datasets with relevant parameters (see below).
#'
#' @param Tp number of time points
#' @param N number of samples (by default first N/2 controls and last N/2 cases)
#' @param J number of metabolites
#' @param P number of pathways (will probably change)
#' @param K number of bacteria profiles (Y variables)
#' @param path.names pathways to sample from as specified in KEGG. If not specified, path.probs will be considered.
#' @param path.probs if TRUE, KEGG like density of pathways per metabolite is used to sample from.
#' If scalar, path.probs is the expected ratio of metabolites in each pathway. Needs to be specified if
#' path.names is not.
#' @param pathway.perturb.ratio expected ratio of perturbed pathways
#' @param Ygroupeff vector of 2xK variables (treatment effect on Y variables)
#' @param Zgroupeff vector of 2 variables for treatment effect
#' @param fe fixed effect
#' @param num.corr.y number of correlated Y variables. The last num.corr.y will be highly
#' correlated to the first num.corr.y variables
#' @param beta.val beta values (regression coefficients) to sample from. Values will be randomly sampled if not specified.
#' @param sigma2 individual variance of metabolites
#' @param arz autoregressive coefficient for treatment simulation
#' @param sdx noise for autoregressive process, recommended value is 0.01
#'
#' @return list with the following objects :
#' \item{XX}{metabolomics data, X data}
#' \item{Y}{additional omic data, Y data}
#' \item{Z}{treatment}
#' \item{beta}{effects of Y variables on X variables, column K+1 represents effect of treatment on X variables}
#' \item{pathways}{pathway adjacency matrices}
#' \item{path.perturb}{which pathways are perturbed?}
#' \item{phi}{"spatial" dependence parameter, indicative of pathway perturbation}
#' \item{arx}{autoregressive coefficients for X data}
#' \item{ary}{autoregressive coefficients for Y data}
#'
#' @examples  data.sim = iCARH.simulate(4, 8, 10, 2, 2, path.probs=0.3, Zgroupeff=c(0,4),
#' beta.val=c(1,-1,0.5, -0.5))
#'
#' @export iCARH.simulate
#'
#' @import stats
#' @importFrom graphics abline
#' @importFrom MASS mvrnorm

iCARH.simulate = function(Tp, #number of time points
                          N, #number of samples (half controls and half cases)
                          J, #number of metabolites
                          P, #number of pathways (will probably change)
                          K, #number of bacteria profiles (Y variables)
                          path.names=NULL,
                          path.probs=FALSE,
                          pathway.perturb.ratio=0.5, #expected ratio of perturbed pathways
                          Ygroupeff=NULL, #vector of 2xK variables (treatment effect on Y variables)
                          Zgroupeff=NULL, #vector of 2 variables for treatment effect
                          fe=0, # fixed effect
                          num.corr.y=0, #number of correlated Y variables
                          #(the last num.corr.y will be highly correlated to the first num.corr.y variables)
                          beta.val=NULL, #beta values (regression coefficients) to sample from
                          sigma2=1, #individual variance of metabolites
                          arz=0.7, #autoregressive coefficient for treatment simulation
                          sdx=0.01 #noise for autoregressive process, recommended value
                          ){
  # path.names needs to be in the form path:map[number]"
  # Use KEGG like density of pathways per metabolite to simulate A matrix
  # (might take a while) or
  # use specified probability of membership (only first element is considered)

  if(P>J) stop("Problem: Ill-conditioned matrix P>J ")

  A=NULL
  P.sim=P

  # Simulating adjacency matrices
  if(length(path.names)){
    if(sum(grepl("path:map",path.names)) != P) stop("Pathways uncorrectly specified.")
    compounds = abind::abind(lapply(path.names, function(x){
      convertTable(RCurl::getURL(paste0("http://rest.kegg.jp/link/cpd/", x)))}),along=1)
    keggid = as.list(c(gsub("cpd:","",sample(compounds[,2], J-2, replace = T)), paste0("Unk", 1:2)))
    AA = iCARH.getPathwaysMat(keggid, "rno")
    P.sim=length(AA)
    AA = lapply(AA, function(x) 1/(x+1))
  } else if(path.probs){
    if(is.logical(path.probs)){
      # Using density from KEGG to simulate matrix
      all.compounds = unique(convertTable(RCurl::getURL("http://rest.kegg.jp/list/compound"))[,1])
      pathway.nb = sapply(all.compounds,function(x){
        pp = convertTable(RCurl::getURL(paste0("http://rest.kegg.jp/link/pathway/", x)));
        ifelse(length(pp)>0, length(unique(pp[,2])), 0)})
      probs = table(pathway.nb)/length(pathway.nb)
      pathway.sim = rmultinom(1, J, probs)
      pathway.sim = as.numeric(rep(rownames(pathway.sim), times=pathway.sim))
      A = matrix(0, nrow=J, ncol=P)
      lapply(seq_along(pathway.sim), function(x) {if(pathway.sim[x]!=0)
        A[x, sample(1:ncol(A), pathway.sim[x])] <<- 1})
    } else if(is.numeric(path.probs)){
      A = matrix(rbinom(J*P, 1, path.probs[1]), nrow=J, ncol=P)
    }
      A = A[,!duplicated(A, MARGIN = 2) & colSums(A)>1, drop=F]
      P.sim = ncol(A)
      if(P.sim != Matrix::rankMatrix(A)) warning("Model might not be identifiable.
                                         Check adjacency matrix.")
      AA = lapply(data.frame(A), function(x) tcrossprod(x))
  }
  # check obtained on number of pathways
  if(P.sim != P) warning(paste("Number of pathways reduced to ", P.sim,
                               "due to random selection of metabolites
                                 in the intially specified pathways."))
  P=P.sim

  AA = lapply(AA, function(x) {diag(x)=0; 1/max(rowSums(x>0))*x})

  # simulating spatial parameter (pathway perturbation parameter)
  lambdas = lapply(AA,function(x) sort(eigen(x)$values)[c(1,nrow(x))])
  sigp = rbinom(P,1,pathway.perturb.ratio)
  noise.phi = 0.2
  phi = mapply(function(x,y)
  {if(y==1) c(mc2d::rtrunc(distr = rnorm, 1, linf=0, lsup=1/(x[2])-0.005,mean=1/(x[2])-0.005, sd=noise.phi),
              mc2d::rtrunc(distr = rnorm, 1, linf=1/(x[1])+0.005, lsup=0,mean=1/(x[1])+0.005, sd=noise.phi))
    else rep(mc2d::rtrunc(distr = rnorm, 1, linf = 1/(x[1])+0.005, lsup = 1/(x[2])-0.005,
                    mean=sample(c(1/(x[1])+0.005, 1/(x[2])-0.005),1), sd=noise.phi),2)},lambdas, sigp)

  Xsim = array(dim = c(Tp,N,J))

  #temporal variations simulation
  ar = runif(J, min = -1, max = 1)
  for(j in 1:J){
    for(i in 1:N){
      Xsim[,i,j] = arima.sim(list(order = c(1,0,0), ar = ar[j], sd=sdx), n = Tp)
      Xsim[,i,j] = ( (Xsim[,i,j] - mean(Xsim[,i,j])) / sd(Xsim[,i,j]) ) * sdx + 0 #correct for true mean and variance
    }
  }

  #exogeneous variables and treatment variable
  if(is.null(Ygroupeff)) Ygroupeff = rbind(rep(0,K),sample(c(10,1,-1,2),K,replace=T))
  if(is.null(Zgroupeff)) Zgroupeff = c(0,rnorm(1,4,0.5))
  Ysd = rep(1,K)
  Zsd = 1
  Y = array(dim = c(Tp,N,K))
  Z = matrix(nrow=Tp, ncol=N)

  if(is.null(beta.val)) beta = matrix(rnorm((K+1)*J, mean=1, sd=10),nrow=J,ncol=K+1)
  else beta = matrix(sample(beta.val, (K+1)*J, replace = T), nrow=J,ncol=K+1)

  ary = runif(min = -0.9, max=0.9, n=K)
  for(i in 1:N){
    for(k in 1:K){
      tmp = arima.sim(list(order = c(1,0,0), ar = ary[k], sd=Ysd[k]), n = Tp)
      Y[,i,k] = ( (tmp - mean(tmp)) / sd(tmp) ) * Ysd[k] + 0
    }
    for(k in 1:(K/2)) Y[,i,k] = Ygroupeff[1+(i>(N/2)),k] + Y[,i,k]
    # Simulate correlated variables
    if(num.corr.y) for(j in 0:num.corr.y) Y[,i,K-j] = 0.1*Y[,i,K-j] + Y[,i,j+1]
    tmp = arima.sim(list(order = c(1,0,0), ar = arz, sd=Zsd), n = Tp)
    Z[,i] = Zgroupeff[1+(i>(N/2))] + ( (tmp - mean(tmp)) / sd(tmp) ) * Zsd + 0
  }

  #covariate function
  for(t in 1:Tp){
    for(j in 1:J){
      Xsim[t,,j] = Xsim[t,,j] + Y[t,,1:K]%*%beta[j,1:K] + beta[j,K+1] * Z[t,]
    }
  }

  Xsim = Xsim + fe

  #covariance matrix of CAR component
  SS = list()
  SS[[1]] = matrix(0,J,J)
  SS[[2]] = matrix(0,J,J)
  I = diag(1,nrow=J)
  for(p in 1:P){
    SS[[1]] = SS[[1]] + phi[1,p]/P*AA[[p]]
    SS[[2]] = SS[[2]] + phi[2,p]/P*AA[[p]]
  }

  SS[[1]] = solve(I-SS[[1]])*sigma2
  SS[[2]] = solve(I-SS[[2]])*sigma2

  XX = Xsim
  for(t in 1:Tp){
    tmp = mvrnorm(n=N/2, mu=rep(0,J), diag(1,J))
    XX[t,1:(N/2),] = Xsim[t,1:(N/2),] + scale(tmp%*%chol(SS[[1]]),scale=F, center=rep(0,J))
    tmp = mvrnorm(n=N-N/2, mu=rep(0,J), diag(1,J))
    XX[t,(N/2+1):N,] = Xsim[t,(N/2+1):N,] + scale(tmp%*%chol(SS[[2]]),scale=F, center=rep(0,J))
  }

  return(list(XX=XX, Y=Y, Z=Z, beta=beta, pathways=AA, path.perturb=sigp, phi=phi, arx=ar, ary=ary))
}
