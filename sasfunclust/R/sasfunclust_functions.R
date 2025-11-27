#' @title Sparse and Smooth Functional Data Clustering
#' @description Sparse and smooth functional clustering (SaS-Funclust) allows to cluster a sample of curves
#'  into homogeneous groups while jointly detecting the most informative portion of domain. (Centofanti et al., 2021).
#' @param X For functional data observed over a regular grid: a matrix of where  the rows must correspond to argument values and columns to replications.
#' For functional data observed over an irregular grid:  a vector of length \eqn{\sum_{i=1}^{N}n_i}, with \eqn{N}  the number of curves,
#'  where the entries from  \eqn{\sum_{i=1}^{k-1}(n_i+1)} to \eqn{\sum_{i=1}^{k}n_i} are elements representing the observations for curve \eqn{k}.
#' @param timeindex A vector of length \eqn{\sum_{i=1}^{N}n_i}. The entries from  \eqn{\sum_{i=1}^{k-1}(n_i+1)} to \eqn{\sum_{i=1}^{k}n_i} provide the locations on \code{grid} of curve  \eqn{k}.
#'  So for example, if the \eqn{k}th curve is observed at time points \eqn{t_l,t_m} of the \code{grid} then the the entries from  \eqn{\sum_{i=1}^{k-1}(n_i+1)} to \eqn{\sum_{i=1}^{k}n_i} would be \eqn{l,m}, being \eqn{n_k=2}.
#'  If X is a matrix, timeindex is ignored.
#' @param curve  A vector of length \eqn{\sum_{i=1}^{N}n_i}. The entries from  \eqn{\sum_{i=1}^{k-1}(n_i+1)} to \eqn{\sum_{i=1}^{k}n_i} are equal to  \eqn{k}.
#' If X is a matrix, curve is ignored.
#' @param grid The vector of time points where the curves are sampled.
#'  For Functional data observed over an irregular grid, \code{timeindex} and \code{grid} provide the time points for each curve.
#' @param q The dimension of the set of B-spline functions.
#' @param lambda_l Tuning parameter of the functional adaptive pairwise fusion penalty (FAPFP).
#' @param lambda_s Tuning parameter of the smoothness penalty.
#' @param G The number of clusters.
#' @param tol The tolerance for the stopping condition of the expectation conditional maximization (ECM) algorithms.
#' The algorithm stops when the log-likelihood difference between two consecutive iterations is less or equal than \code{tol}.
#' @param maxit The maximum number of iterations allowed in the ECM algorithm.
#' @param par_LQA A list of parameters for the local quadratic approximation (LQA) in the ECM algorithm.
#'  \code{eps_diff} is the lower bound for the coefficient mean differences, values below \code{eps_diff} are set to zero.
#'  \code{MAX_iter_LQA} is the maximum number of iterations allowed in the LQA.
#'  \code{eps_LQA} is the tolerance for the stopping condition of LQA.
#' @param plot If TRUE, the estimated cluster means are plotted at each iteration of the ECM algorithm. Default is FALSE.
#' @param trace If TRUE, information are shown at each iteration of the ECM algorithm. Default is FALSE.
#' @param init It is the way to initialize the ECM algorithm. There are three ways of initialization: "kmeans", "model-based", and "hierarchical", that
#'  provide initialization through the k-means algorithm,  model-based clustering based on parameterized finite Gaussian mixture model, and  hierarchical clustering, respectively.
#' Default is "kmeans".
#' @param varcon A vector of character strings indicating the type of coefficient covariance matrix. Three values are allowed: "full", "diagonal", and "equal".
#' "full" means unrestricted cluster coefficient covariance matrices allowed to be different among clusters.
#' "diagonal" means diagonal cluster coefficient covariance matrices that are equal among clusters.
#' "equal" means diagonal cluster coefficient covariance matrices, with equal diagonal entries, that are equal among clusters.
#'  Default is "diagonal".
#' @param lambda_s_ini The tuning parameter used to obtain the functional data through smoothing B-splines before applying the initialization algorithm.
#' If NULL a Generalized cross validation procedure is used as described in Ramsay (2005). Default is NULL.
#' @return   A list containing the following arguments:
#' \code{mod} that is a list composed by
#' \itemize{
#' \item \code{data}: A list containing the vectorized form of \code{X}, \code{timeindex}, and \code{curve}. For functional data observed over a regular grid \code{timeindex} and \code{curve} are trivially obtained.
#'
#' \item \code{parameters}: A list containing all the estimated parameters.
#'
#' \item \code{vars}: A list containing results from the Expectation step of the ECM algorithm.
#'
#' \item \code{FullS}: The matrix of B-spline computed over \code{grid}.
#'
#' \item \code{grid}: The vector of time points where the curves are sampled.
#'
#' \item \code{W}: The basis roughness penalty matrix containing the inner products of pairs of basis function second derivatives.
#'
#' \item \code{AW_vec}: Vectorized version of the diagonal matrix used in the approximation of FAPFP.
#'
#' \item \code{P_tot}: Sparse Matrix used to compute all the pairwise comparisons in the FAPFP.
#'
#' \item \code{lambda_s}: Tuning parameter of the smoothness penalty.
#'
#' \item \code{lambda_l}: Tuning parameter of the FAPFP.
#'}
#'
#'A list, named \code{clus}, containing the following arguments:
#'\itemize{
#' \item \code{classes}: The vector of cluster membership.
#'
#' \item \code{po_pr}: Posterior probabilities of cluster membership.
#'}
#'
#'\code{mean_fd} The estimated cluster mean functions.
#'
#'\code{class} A label for the output type.
#'@seealso \code{\link{sasfclust_cv}}
#'
#' @export
#' @references
#' Centofanti, F., Lepore, A., & Palumbo, B. (2021).
#' Sparse and Smooth Functional Data Clustering.
#' \emph{arXiv preprint arXiv:2103.15224}.
#'
#' Ramsay, J., Ramsay, J., & Silverman, B. W. (2005). Functional Data Analysis. Springer Science & Business Media.
#' @examples
#' library(sasfunclust)
#' train<-simulate_data("Scenario I",n_i=20,var_e = 1,var_b = 0.5^2)
#' mod<-sasfclust(X=train$X,grid=train$grid,lambda_s = 10^-6,lambda_l =10,G = 2,maxit = 5,q=10)
#' plot(mod)
#' @importFrom matrixcalc vec
#' @importFrom fda create.bspline.basis fd plot.fd
sasfclust <-
  function(X=NULL, timeindex=NULL,curve=NULL,grid = NULL, q = 30, lambda_l = 1e1, lambda_s = 1e1,G = 2,
           tol = 10^-7, maxit = 50,par_LQA=list(eps_diff = 1e-06,MAX_iter_LQA=200,eps_LQA = 1e-05),
            plot= F,trace=F,init="kmeans",varcon="diagonal",lambda_s_ini=NULL)
  {


    der=2
    gamma_ada=1
    CK=0
    hard = FALSE
    perc_rankpapp=pert=NULL
    if(G==1)lambda_l=0

    if(length(dim(X))==2){
      n_obs<-dim(X)[2]
      n_t<-dim(X)[1]
      if(is.null(grid)) grid<-seq(0, 1, length.out = n_t)
      if(length(grid)!=n_t)  stop("Length grid is wrong!")
      vec<-list(x=matrixcalc::vec(X),timeindex=rep(1:length(grid),n_obs),curve=rep(1:n_obs,each=length(grid)))
    }
    else if(is.null(dim(X))){

      if(is.null(grid)) stop("For irregularly sampled functional data grid must be provided")
      if(is.null(timeindex)) stop("For irregularly sampled functional timeindex grid must be provided")
      if(is.null(curve)) stop("For irregularly sampled functional timeindex curve must be provided")
      vec<-list(x=as.matrix(X),timeindex=timeindex,curve=curve)
    }
    else{
      stop("No data provided")
    }

    data=vec
    # Initialize the parameters

    initfit <- sasfclustinit(data = data, pert = pert, grid = grid,  q = q, G = G,der=der,gamma_ada=gamma_ada,lambda_s_ini=lambda_s_ini,init=init,varcon=varcon)
    parameters <- initfit$parameters
    vars <- initfit$vars
    S <- initfit$S
    FullS <- initfit$FullS
    W <- initfit$W
    AW_vec<-initfit$AW_vec
    P_tot<- initfit$P_tot
    P <-initfit$P
    basis<- initfit$basis
    sigma.old <- 0
    sigma.new <- parameters$sigma
    ind <- 1
    if(plot){
      basis<-fda::create.bspline.basis(c(grid[1],grid[length(grid)]),nbasis = q)
      mean_fd<-fda::fd(t(parameters$mu),basis)
      fda::plot.fd(mean_fd,type='n',ylab="Cluster Mean")
    }
    # Main loop. Iterates between M and E steps and stops when the stopping condition is met.
    lk_old=0

    lk_new=loglik (parameters=parameters, data = data, vars=vars, FullS = FullS,W = W,AW_vec = AW_vec,P_tot = P_tot,lambda_s = lambda_s,lambda_l = lambda_l)
    if (trace)    print(paste("Iteration", 0,": Sigma = ",sigma.new," loglk = ",lk_new[1]," ploglk = ",lk_new[2]))
    lk_new=lk_new[2]

    while(abs(lk_old - lk_new) > tol & (ind <= maxit)) {
      parameters <- sasfclustMstep(parameters, data, vars, S, tol, hard,lambda_s,lambda_l,W,AW_vec,P_tot,par_LQA,CK,perc_rankpapp,varcon=varcon)
      vars <- sasfclustEstep(parameters, data, vars, S, hard)
      lk_old<-lk_new
      lk_i<-loglik (parameters=parameters, data = data, vars=vars, FullS = FullS,W = W,AW_vec = AW_vec,P_tot = P_tot,lambda_s = lambda_s,lambda_l = lambda_l)
      lk_new<--lk_i[2]
      sigma.old <- sigma.new
      sigma.new <- parameters$sigma[1]
      if (trace)
        print(paste("Iteration", ind,": Sigma = ",sigma.new," loglk = ",lk_i[1]," ploglk = ",lk_i[2]))
      #Plot cluster mean curves.
      if(plot){
        basis<-fda::create.bspline.basis(c(grid[1],grid[length(grid)]),nbasis = q)
        mean_fd<-fda::fd(t(parameters$mu),basis)
        fda::plot.fd(mean_fd,type='n',ylab="Cluster Mean")
      }
      ind <- ind + 1

    }
    mod=list(data=data,parameters = parameters, vars = vars, FullS = FullS,grid=grid,
         W=W,AW_vec=AW_vec,P_tot=P_tot,lambda_s=lambda_s,lambda_l=lambda_l)
    mean_fd<-fda::fd(t(parameters$mu),basis)
    clus<-classify(mod)

    out<-list(mod=mod,
              mean_fd=mean_fd,
              clus=clus)
    class(out)<-"sasfclust"

    return(out)
    }

sasfclustinit <-
  function(data, pert = 0, grid = seq(0.01, 1, length = 100),  q = 5,G = G,der=der,gamma_ada=gamma_ada,lambda_s_ini=lambda_s_ini,init=init,varcon=varcon){
    S <- FullS <- NULL
    # This function initializes all the parameters.
    # Produce spline basis matrix
    basis<-fda::create.bspline.basis(c(grid[1],grid[length(grid)]),nbasis = q)
    FullS<-fda::eval.basis(grid,basis)
    W<-fda::eval.penalty(basis,2)
    S <- FullS[data$timeindex,  ]
    order<-q-length(basis$params)
    breaks<-basis$params

    ext_break<-c(rep(grid[1],order),breaks,rep(grid[length(grid)],order))
    weights_vec<-rep(diff(ext_break,lag=order)/order,each=G)

    # Get pairwise matrix
    if(G!=1)
    {
      P<-matrix(0,((G-1)^2+(G-1))/2,G)
      ind<-c(1,G-1)
      for (ii in 1:(G-1)) {

        P[ind[1]:ind[2],ii]<-rep(1,length(ind[1]:ind[2]))
        if(length(ind[1]:ind[2])==1)
          aa<--1
        else
          aa<-diag(rep(-1,length(ind[1]:ind[2])))

        P[ind[1]:ind[2],(ii+1):G]<-aa
        ind<-ind+c((G-1)-(ii-1),(G-1)-(ii))
        ind<-c(min(((G-1)^2+(G-1))/2,ind[1]),min(((G-1)^2+(G-1))/2,ind[2]))
      }

      P_tot<-matrix(0,q*((G-1)^2+(G-1))/2,G*q)
      for (ii in 1:G) {
        P_tot[, ((ii - 1) * q + 1):((ii) * q)]<-kronecker(diag(q), P[, ii])
      }
      P_tot<-Matrix::Matrix(P_tot,sparse = TRUE)
    }
    else{
      P<-P_tot<-diag(q)
    }

    # Weight approximation L1 penalty
    order<-q-length(basis$params)
    breaks<-basis$params

    ext_break<-c(rep(grid[1],order),breaks,rep(grid[length(grid)],order))
    weights_vec<-rep(diff(ext_break,lag=order)/order,each=((G-1)^2+(G-1))/2)
    N <- length(unique(data$curve))

    # Compute initial estimate of basis coefficients.
    if(!is.null(pert)){
      points <- matrix(0,N,sum(q))
      for (i in 1:N){
        Si <- S[data$curve==i,]
        xi <- data$x[data$curve==i]
        points[i,] <- solve(t(Si) %*% Si + pert * diag(q)) %*% t(Si) %*%xi
      }
    }
    else{
      d<-sapply(1:N,function(i)length(which(data$curve==unique(data$curve)[i])))
      e<-lapply(1:N, function(i)data$timeindex[data$curve==unique(data$curve)[i]])

      if(length(unique(d))==1&length(unique(e))==1){##Regular grid
        basis_start<-basis
        grid_i <- grid[data$timeindex[data$curve==1]]
        X<-sapply(1:N,function(i) data$x[data$curve==i])
        loglam         = seq(-10, 6, 0.25)
        Gcvsave        = numeric()
        for(i in 1:length(loglam)){
          fdPari  = fda::fdPar(basis_start, Lfdobj=2, 10^loglam[i])
          Sm.i    = fda::smooth.basis(grid_i, X, fdPari)
          Gcvsave[i] = sum(Sm.i$gcv)

        }
        lambda_s_sm=if(is.null(lambda_s_ini))10^loglam[which.min(Gcvsave)]else lambda_s_ini
        fdPari  = fda::fdPar(basis_start, Lfdobj=2,lambda_s_sm)
        points<-t(fda::smooth.basis(grid_i, X, fdPari)$fd$coefs)
      }
      else{## Irregular grid
        basis_start<-basis
        loglam         = seq(-3, 1, 0.25)
        Gcvsave        = matrix(0,N,length(loglam))
        points <- matrix(0,N,sum(q))
        lambda_s_i_vec<-numeric()
        for (i in 1:length(unique(data$curve))){

          xi <- data$x[data$curve==unique(data$curve)[i]]
          grid_i <- grid[data$timeindex[data$curve==unique(data$curve)[i]]]

          for(l in 1:length(loglam)){
            fdPari  = fda::fdPar(basis_start, Lfdobj=2, 10^loglam[l])
            Sm.i    = fda::smooth.basis(grid_i, xi, fdPari)
            Gcvsave[i,l] = sum(Sm.i$gcv)
          }
          lambda_s_i_vec[i]=if(is.null(lambda_s_ini)) 10^loglam[which.min(Gcvsave[i,])] else lambda_s_ini
          fdPari  = fda::fdPar(basis_start, Lfdobj=2,lambda_s_i_vec[i])
          points[i,]<-t(fda::smooth.basis(grid_i, xi, fdPari)$fd$coefs)

        }
      }
    }

    # Initialization cluster memberships from points.
    if(G > 1){
      if(init=="kmeans")class <-  stats::kmeans(points, G, 1000,nstart = 10000)$cluster
      else if(init=="model-based") class <- mclust::Mclust(points,G,verbose=FALSE,modelNames = "EII")$classification
      else if(init=="hierarchical")class <-  stats::cutree(stats::hclust(stats::dist(points), method = "ward.D2"),k=G)
      else{
        stop("Wrong initialization!")
      }
    }
    else {
      class <- rep(1, N)
    }
    piigivej <- matrix(0, N, G)
    piigivej[col(piigivej) == class] <- 1
    pi=apply(piigivej,2,mean)

    # # Calculate coefficeints for cluster means.
    classmean <- matrix(0,G,q)
    for (k in 1:G)
      classmean[k,] <- apply(as.matrix(points[class==k,]),2,mean)

    # Initialize mu, gamma, Gamma
    mu<-mu_start<-classmean
    gamma<-array(0, c(N, G, sum(q)))
    gprod <- NULL
    if(G==1){
      for(i in 1:N){
        gamma[i,,]<-t(points[i,] - t(mu))
        gprod <- cbind(gprod, (gamma[i,  ,]) %*% t(gamma[i, , ]))
      }
    }
    else{
      for(i in 1:N){
        gamma[i,,]<-t(points[i,] - t(mu))
        gprod <- cbind(gprod, t(gamma[i,  ,]) %*% (gamma[i, , ]))
      }
    }
    N <- dim(gamma)[1]
    ind <- matrix(rep(c(rep(c(1, rep(0, q - 1)), N), 0), q)[1:(N*q^2)], N * q, q)
    Gamma <-gprod %*% ind/N
    if(varcon=="diagonal")Gamma <-diag(diag( Gamma))
    if(varcon=="equal")Gamma=diag(q)*sum(diag( Gamma))/(q)

    gcov <- matrix(0, sum(q), N * sum(q))

    # Get weight matrix
    if(G!=1){
      AW<-matrix(0,((G-1)^2+(G-1))/2,q)
      for (ii in 1:q) {
        AW[,ii]<-1/(abs(P%*%mu_start[,ii])^gamma_ada)
      }
      AW_vec<-matrixcalc::vec(AW)*(weights_vec)
    }
    else{
      AW_vec<-rep(1,q)
    }
    n <- length(data$curve)
    n_i<-sapply(1:N,function(ii)length(which(data$curve==unique(data$curve)[ii])))
    sigma=as.numeric(get_sigma( data$x, data$curve, data$time,  as.matrix(S),  piigivej,  gcov,n_i,gamma,mu))/n
    list(S = S, W = W, AW_vec=AW_vec,P_tot=P_tot,P=P, FullS = FullS, parameters = list(mu=mu_start,sigma=sigma,pi=pi,Gamma=Gamma), vars = list(gamma = gamma,piigivej = piigivej,
                                                                                                                                               gprod = gprod, gcov = gcov ),basis=basis)
  }

sasfclustMstep <-
  function(parameters, data, vars, S, tol,  hard,lambda_s,lambda_l,W,AW_vec,P_tot,par_LQA,CK,perc_rankpapp,varcon=varcon)
  {
    # This function implements the M step of the EM algorithm.
    G <- dim(parameters$mu)[1]
    mu<-parameters$mu
    gamma <- vars$gamma
    gcov <- vars$gcov
    curve <- data$curve
    piigivej <- vars$piigivej
    N <- dim(gamma)[1]
    G <- dim(mu)[1]
    n <- length(curve)
    q <- dim(S)[2]
    # Compute pi.
    if(hard)
      parameters$pi <- rep(1/G, G)
    else parameters$pi <- (apply(vars$piigivej, 2, mean)*N+CK)/(N+G*CK)
    ind <- matrix(rep(c(rep(c(1, rep(0, q - 1)), N), 0), q)[1:(N*q^2)], N * q, q)
    if(!is.null(perc_rankpapp)){
      gsvd <- svd(vars$gprod %*% ind/N)
      p<-which(cumsum( gsvd$d)/sum( gsvd$d)>=perc_rankpapp)[1]
      gsvd$d[ - (1:p)] <- 0
      parameters$Gamma <- gsvd$u %*% diag(gsvd$d) %*% t(gsvd$u)
    }
    else{
      parameters$Gamma <-vars$gprod %*% ind/N
    }
    if(varcon=="diagonal")parameters$Gamma <-diag(diag( parameters$Gamma))
    if(varcon=="equal")parameters$Gamma=diag(q)*sum(diag(parameters$Gamma))/(q)

    # Local quadratic approximation to get mu
    W_star<-matrix(0,q*G,q*G)
    for (i in 1:G) {
      W_star[((i-1)*q+1):((i)*q),((i-1)*q+1):((i)*q)]<-W
    }
    W_star<-Matrix::Matrix(W_star, sparse = TRUE)
    x <- data$x
    n_i<-sapply(1:N,function(ii)length(which(data$curve==unique(data$curve)[ii])))
    numden<-get_numden( data$x, data$curve, data$time,  as.matrix(S),  piigivej,  gcov,n_i,gamma)
    VY<-Matrix::Matrix(numden[[1]],sparse = TRUE)
    S.den<-Matrix::Matrix(numden[[2]],sparse=TRUE)

    z_int=1
    diff_inter <- 100
    mu_old=mu

    while(diff_inter>par_LQA$eps_LQA && z_int<=par_LQA$MAX_iter_LQA) {

      mu_vec<-matrixcalc::vec(t(mu))
      diff_mat<-abs(P_tot%*%mu_vec)
      diff_mat[diff_mat < par_LQA$eps_diff]<-par_LQA$eps_diff
      V_l<-Matrix::Matrix(diag(as.numeric(AW_vec/(2*diff_mat))),sparse = TRUE)
      mu_vec<-solve(S.den*(1/parameters$sigma)+lambda_s*2*W_star+2*lambda_l*Matrix::t(P_tot)%*%V_l%*%P_tot)%*%VY*(1/parameters$sigma)
      mu<-matrix(mu_vec,G,q,byrow = TRUE)
      diff_inter<-sum(abs(mu-mu_old))/(sum(abs(mu_old)))
      mu_old<-mu
      z_int=z_int+1
    }

    # Get sigma
    sigma<-get_sigma( data$x, data$curve, data$time,  as.matrix(S),  piigivej,  gcov,n_i,gamma,mu)
    sigma=as.numeric(sigma)/n
    parameters$mu <- mu
    parameters$sigma <- sigma

    parameters
  }

sasfclustEstep <-
  function(parameters, data, vars, S, hard)
  {
    # This function performs the E step of the EM algorithm
    N <- dim(vars$gamma)[1]
    n_i<-sapply(1:N,function(ii)length(which(data$curve==unique(data$curve)[ii])))
    parameters$sigma=as.matrix(parameters$sigma)
    parameters$pi=as.matrix(parameters$pi)
    aa<-get_Estep(parameters, data, vars, S, hard,n_i)
    vars$gamma=aa[[1]]
    vars$piigivej=aa[[2]]
    vars$gprod=aa[[3]]
    vars$gcov=aa[[4]]
    vars
  }





#' @title Cross-validation for sasfclust
#' @description K-fold cross-validation procedure to choose the number of clusters and the tuning parameters for the sparse and smooth functional clustering (SaS-Funclust) method (Centofanti et al., 2021).
#' @param X For functional data observed over a regular grid: a matrix of where  the rows must correspond to argument values and columns to replications.
#' For functional data observed over an irregular grid:  a vector of length \eqn{\sum_{i=1}^{N}n_i}, with \eqn{N}  the number of curves,
#'  where the entries from  \eqn{\sum_{i=1}^{k-1}(n_i+1)} to \eqn{\sum_{i=1}^{k}n_i} are elements representing the observations for curve \eqn{k}.
#' @param timeindex A vector of length \eqn{\sum_{i=1}^{N}n_i}. The entries from  \eqn{\sum_{i=1}^{k-1}(n_i+1)} to \eqn{\sum_{i=1}^{k}n_i} provide the locations on \code{grid} of curve  \eqn{k}.
#'  So for example, if the \eqn{k}th curve is observed at time points \eqn{t_l,t_m} of the \code{grid} then the the entries from  \eqn{\sum_{i=1}^{k-1}(n_i+1)} to \eqn{\sum_{i=1}^{k}n_i} would be \eqn{l,m}, being \eqn{n_k=2}.
#'  If X is a matrix, timeindex is ignored.
#' @param curve  A vector of length \eqn{\sum_{i=1}^{N}n_i}. The entries from  \eqn{\sum_{i=1}^{k-1}(n_i+1)} to \eqn{\sum_{i=1}^{k}n_i} are equal to  \eqn{k}.
#' If X is a matrix, curve is ignored.
#' @param grid The vector of time points where the curves are sampled.
#'  For Functional data observed over an irregular grid, \code{timeindex} and \code{grid} provide the time points for each curve.
#' @param q The dimension of the set of B-spline functions.
#' @param lambda_l_seq Sequence of tuning parameter of the functional adaptive pairwise fusion penalty (FAPFP).
#' @param lambda_s_seq Sequence of tuning parameter of the smoothness penalty.
#' @param G_seq Sequence of number of clusters.
#' @param tol The tolerance for the stopping condition of the expectation conditional maximization (ECM) algorithms.
#' The algorithm stops when the log-likelihood difference between two consecutive iterations is less or equal than \code{tol}.
#' @param maxit The maximum number of iterations allowed in the ECM algorithm.
#' @param par_LQA A list of parameters for the local quadratic approximation (LQA) in the ECM algorithm.
#'  \code{eps_diff} is the lower bound for the coefficient mean differences, values below \code{eps_diff} are set to zero.
#'  \code{MAX_iter_LQA} is the maximum number of iterations allowed in the LQA.
#'  \code{eps_LQA} is the tolerance for the stopping condition of LQA.
#' @param plot If TRUE, the estimated cluster means are plotted at each iteration of the ECM algorithm. Default is FALSE.
#' @param trace If TRUE, information are shown at each iteration of the ECM algorithm. Default is FALSE.
#' @param init It is the way to initialize the ECM algorithm. There are three ways of initialization: "kmeans", "model-based", and "hierarchical", that
#'  provide initialization through the k-means algorithm,  model-based clustering based on parameterized finite Gaussian mixture model, and  hierarchical clustering, respectively.
#' Default is "kmeans".
#' @param varcon A vector of character strings indicating the type of coefficient covariance matrix. Three values are allowed: "full", "diagonal", and "equal".
#' "full" means unrestricted cluster coefficient covariance matrices allowed to be different among clusters.
#' "diagonal" means diagonal cluster coefficient covariance matrices that are equal among clusters.
#' "equal" means diagonal cluster coefficient covariance matrices, with equal diagonal entries, that are equal among clusters.
#'  Default is "diagonal".
#' @param lambda_s_ini The tuning parameter used to obtain the functional data through smoothing B-splines before applying the initialization algorithm.
#' If NULL a Generalized cross validation procedure is used as described in Ramsay (2005). Default is NULL.
#' @param K_fold Number of folds. Default is 5.
#' @param X_test Only for functional data observed over a regular grid, a matrix  where  the rows must correspond to argument values and columns to replications of the test set. Default in NULL.
#' @param grid_test The vector of time points where the test set curves are sampled. Default is NULL.
#' @param m1 The m-standard deviation rule parameter to choose \code{G} for each \code{lambda_s} and \code{lambda_l}.
#' @param m2 The m-standard deviation rule parameter to choose \code{lambda_s} fixed \code{G} for each \code{lambda_l}.
#' @param m3 The m-standard deviation rule parameter to choose \code{lambda_l} fixed \code{G} and \code{lambda_s}.
#' @param ncores If \code{ncores}>1, then parallel computing is used, with \code{ncores} cores. Default is 1.

#' @return   A list containing the following arguments:
#'
#'  \code{G_opt}: The optimal  number of clusters.
#'
#'  \code{lambda_l_opt}: The optimal tuning parameter of the FAPFP.
#'
#'  \code{lambda_s_opt}: The optimal tuning parameter of the smoothness penalty.
#'
#'  \code{comb_list}: The combinations of \code{G},\code{lambda_s} and \code{lambda_l} explored.
#'
#'  \code{CV}: The cross-validation values obtained for each combination of \code{G},\code{lambda_s} and \code{lambda_l}.
#'
#'  \code{CV_sd}: The standard deviations of the cross-validation values.
#'
#'  \code{zeros}: Fraction of domain over which the estimated cluster means are fused.
#'
#'  \code{ms}: The m-standard deviation rule parameters.
#'
#'  \code{class}: A label for the output type.

#' @export
#' @references
#' Centofanti, F., Lepore, A., & Palumbo, B. (2021).
#' Sparse and Smooth Functional Data Clustering.
#' \emph{arXiv preprint arXiv:2103.15224}.
#'
#' Ramsay, J., Ramsay, J., & Silverman, B. W. (2005). Functional Data Analysis. Springer Science & Business Media.
#' @seealso\code{\link{sasfclust}}
#' @examples
#' \donttest{
#' library(sasfunclust)
#' train<-simulate_data("Scenario I",n_i=20,var_e = 1,var_b = 0.5^2)
#' lambda_s_seq=10^seq(-4,-3)
#' lambda_l_seq=10^seq(-1,0)
#' G_seq=2
#' mod_cv<-sasfclust_cv(X=train$X,grid=train$grid,G_seq=G_seq,
#' lambda_l_seq = lambda_l_seq,lambda_s_seq =lambda_s_seq,maxit = 20,K_fold = 2,q=10)
#' plot(mod_cv)
#' }
#' @importFrom matrixcalc vec
#' @importFrom fda create.bspline.basis fd plot.fd
sasfclust_cv<-function(X=NULL, timeindex=NULL,curve=NULL,grid = NULL, q = 30,lambda_l_seq=10^seq(-1,2),lambda_s_seq=10^seq(-5,-3),G_seq=2,
                       tol = 10^-7, maxit = 50,par_LQA=list(eps_diff = 1e-06,MAX_iter_LQA=200,eps_LQA = 1e-05),
                       plot= FALSE,trace=FALSE,init="kmeans",varcon="diagonal",lambda_s_ini=NULL,
                       K_fold=5,X_test=NULL,grid_test=NULL,m1=1,m2=0,m3=1,ncores=1){

  if(length(dim(X))==2){
    N<-dim(X)[2]
  }
  else if(is.null(dim(X))){
    if(is.null(grid)) stop("For irregularly sampled functional data grid must be provided")
    if(is.null(timeindex)) stop("For irregularly sampled functional timeindex  must be provided")
    if(is.null(curve)) stop("For irregularly sampled functional timeindex curve must be provided")
    if(max(timeindex)>length(grid))  stop("Length grid is wrong!")
    N<-length(unique(curve))
  }
  else{
    stop("No data provided")
  }
  comb_list<-expand.grid(G_seq,lambda_s_seq,lambda_l_seq)

  if(is.null(X_test)){#If test set is not provided
    parr_fun<-function(ii){

      parameters<-as.numeric(comb_list[ii,])
      G_i<-parameters[1]
      lambda_s_i<-parameters[2]
      lambda_l_i<-parameters[3]

      ran_seq<-sample(seq(1, N), N, replace=FALSE)
      split_vec<-split(ran_seq,cut(seq(1,N),breaks=K_fold,labels=FALSE))
      l_i<-zeros_vec<-numeric()
      data_fold<-data_i<-list()
      for(lll in 1:K_fold){
        ind_fold<-as.numeric(unlist(split_vec[-lll]))
        ind_i<-split_vec[[lll]]

        grid_fold<-grid
        if(length(dim(X))==2){
          X_fold<-X[,ind_fold]#as.numeric(unlist(lapply(1:length(ind_fold),function(pp)X[which(curve==ind_fold[pp])])))
          timeindex_fold=curve_fold=NULL
        }
        else if(is.null(dim(X))){
          X_fold<-as.numeric(unlist(lapply(1:length(ind_fold),function(pp)X[which(curve==ind_fold[pp])])))
          timeindex_fold=as.numeric(unlist(lapply(1:length(ind_fold),function(pp)timeindex[which(curve==ind_fold[pp])])))
          curve_fold= as.numeric(unlist(lapply(1:length(ind_fold),function(pp)rep(pp,length(which(curve==ind_fold[pp]))))))
          # curve_fold=as.numeric(unlist(lapply(1:length(ind_fold),function(pp)curve[which(curve==ind_fold[pp])])))
        }

        grid_i<-grid
        if(length(dim(X))==2){
          X_i<- X[,ind_i]#as.numeric(unlist(lapply(1:length(ind_i),function(pp)X[which(curve==ind_i[pp])])))
          timeindex_i=curve_i=NULL
        }
        else if(is.null(dim(X))){
          X_i<- as.numeric(unlist(lapply(1:length(ind_i),function(pp)X[which(curve==ind_i[pp])])))
          timeindex_i=as.numeric(unlist(lapply(1:length(ind_i),function(pp)timeindex[which(curve==ind_i[pp])])))
          curve_i=as.numeric(unlist(lapply(1:length(ind_i),function(pp)rep(pp,length(which(curve==ind_i[pp]))))))
          # curve_i=as.numeric(unlist(lapply(1:length(ind_i),function(pp)curve[which(curve==ind_i[pp])])))
        }

        mod<-sasfclust(X=X_fold,timeindex=timeindex_fold,curve=curve_fold,grid = grid_fold, lambda_l = lambda_l_i,lambda_s =lambda_s_i,G=G_i,maxit=maxit,q=q,init=init,varcon=varcon,tol = tol,par_LQA=par_LQA,plot=plot,trace=trace)
        l_i[lll]<-loglik(parameters = mod[[1]]$parameters,X=X_i,timeindex=timeindex_i,curve=curve_i,grid=grid_i,vars = mod[[1]]$vars, FullS = mod[[1]]$FullS,W = mod[[1]]$W,AW_vec = mod[[1]]$AW_vec,P_tot = mod[[1]]$P_tot)[1]
        zeros_vec[lll]<-get_zero(mod[[1]])
        rm(mod)
      }
      mean<-mean(l_i)
      sd<-sd(l_i)/sqrt(K_fold)
      zeros<-mean(zeros_vec)

      out<-list(mean=mean,
                sd=sd,
                zeros=zeros)
      return(out)


    }
  }
  else{#When the test set is provided
    if(!all(grid==grid_test))
      stop("Not equal grids between training and test set \n")
    parr_fun<-function(ii){
      parameters<-as.numeric(comb_list[ii,])
      G_i<-parameters[1]
      lambda_s_i<-parameters[2]
      lambda_l_i<-parameters[3]
      mod<-sasfclust(X=X,timeindex=timeindex,curve=curve,grid = grid, lambda_l = lambda_l_i,lambda_s =lambda_s_i,G=G_i,maxit=maxit,q=q,init=init,lambda_s_ini=lambda_s_ini,varcon=varcon,tol = tol,par_LQA=par_LQA,plot=plot,trace=trace)
      l_i<-loglik(parameters = mod[[1]]$parameters,X = X_test,grid = grid_test,vars = mod[[1]]$vars, FullS = mod[[1]]$FullS,W = mod[[1]]$W,AW_vec = mod[[1]]$AW_vec,P_tot = mod[[1]]$P_tot,lambda_s = mod[[1]]$lambda_s,lambda_l = mod[[1]]$lambda_l)[1]
      zeros<-get_zero(mod[[1]])
      mean<-l_i
      sd<-0
      out<-list(mean=mean,
                sd=sd,
                zeros=zeros)
      return(out)
    }
  }


  if(!is.null(X_test))ncores<-1
  if(ncores>1){
    if(.Platform$OS.type=="unix"){
      vec_par<-parallel::mclapply(seq(1,length(comb_list[,1])),parr_fun,mc.cores = ncores)
    }
    else if(.Platform$OS.type=="windows"){
      cl<-parallel::makeCluster(ncores)
      parallel::clusterEvalQ(cl,library(sasfunclust))
      parallel::clusterExport(cl, c("comb_list","N","X","timeindex","curve","grid","q","maxit","K_fold","init","varcon","tol" ,"par_LQA","plot","trace"),envir = environment())
      vec_par<- parallel::parLapply(cl, seq(1,length(comb_list[,1])),parr_fun)
      parallel::stopCluster(cl)
      }
  }
  else{
    vec_par<-lapply(seq(1,length(comb_list[,1])),parr_fun)
  }
  par<-sapply(vec_par,"[[",1)
  sds<-sapply(vec_par,"[[",2)
  zeros<-sapply(vec_par,"[[",3)
  l_opt<-as.numeric(comb_list[max(which(par==max(par))),])

  ksdrule<-get_msdrule(par,sds,comb_list,m1,m2,m3)

  G_opt<-ksdrule[1]
  lambda_s_opt<-ksdrule[2]
  lambda_l_opt<-ksdrule[3]

  out<-list(G_opt=G_opt,
            lambda_l_opt=lambda_l_opt,
            lambda_s_opt=lambda_s_opt,
            comb_list=comb_list,
            CV=par,
            CV_sd=sds,
            zeros=zeros,
            ms=c(m1,m2,m3))
  class(out)<-"sasfclust_cv"

  return(out)
}



#' @title Simulate data for functional clustering
#' @description Generate synthetic data as in the simulation study of Centofanti et al., 2021.
#' @param scenario A  character strings indicating the scenario considered. It could be "Scenario I", "Scenario II", and "Scenario III".
#' @param n_i Number of curves in each cluster.
#' @param nbasis  The dimension of the set of B-spline functions.
#' @param length_tot Number of evaluation points.
#' @param var_e Variance of the measurement error.
#' @param var_b Diagonal entries of the coefficient variance matrix, which is assumed to be diagonal, with equal diagonal entries, and the same among clusters.


#' @return   A list containing the following arguments:
#'
#'  \code{X}: Observation matrix, where  the rows  correspond to argument values and columns to replications.
#'
#'  \code{X_fd}: Functional observations without measurement error.
#'
#'  \code{mu_fd}: True cluster mean function.
#'
#'  \code{grid}: The vector of time points where the curves are sampled.
#'
#'  \code{clus}: True cluster membership vector.
#'
#' @export
#' @examples
#' library(sasfunclust)
#' train<-simulate_data("Scenario I",n_i=20,var_e = 1,var_b = 0.5^2)
simulate_data<-function(scenario,n_i=50,nbasis=30,length_tot=50,var_e=1,var_b=1) {


  grid<-seq(0,1,length.out = length_tot)
  domain<-c(0,1)


  X_basis<-fda::create.bspline.basis(domain,norder = 4,nbasis = nbasis)
  mean_list<-list()
  if(scenario=="Scenario I"){
    mean_list[[1]]<-c(rep(1.5,nbasis/6),rep(0,nbasis*5/6))
    mean_list[[2]]<-c(rep(-1.5,nbasis/6),rep(0,nbasis*5/6))
    clus_true<-rep(1:2,each=n_i)

  }
  if(scenario=="Scenario II"){
    mean_list[[1]]<-c(rep(3,nbasis/6),rep(1.5,nbasis/6),rep(0,nbasis/6),rep(0,nbasis/2))
    mean_list[[2]]<-c(rep(0,nbasis/6),rep(1.5,nbasis/6),rep(0,nbasis/6),rep(0,nbasis/2))
    mean_list[[3]]<-c(rep(0,nbasis/6),rep(-1.5,nbasis/6),rep(0,nbasis/6),rep(0,nbasis/2))
    clus_true<-rep(1:3,each=n_i)

  }
  if(scenario=="Scenario III"){
    mean_list[[1]]<-c(rep(1.5,nbasis/6),rep(3,nbasis/6),rep(1.5,nbasis/6),rep(0,nbasis/2))
    mean_list[[2]]<-c(rep(1.5,nbasis/6),rep(0,nbasis/6),rep(1.5,nbasis/6),rep(0,nbasis/2))
    mean_list[[3]]<-c(rep(-1.5,nbasis/6),rep(0,nbasis/6),rep(-1.5,nbasis/6),rep(0,nbasis/2))
    mean_list[[4]]<-c(rep(-1.5,nbasis/6),rep(-3,nbasis/6),rep(-1.5,nbasis/6),rep(0,nbasis/2))
    clus_true<-rep(1:4,each=n_i)

  }
  mu_fd<-fda::fd(t(do.call("rbind",mean_list)),X_basis)
  if(length(mean_list)==1)X_coef<-t(MASS::mvrnorm(n_i, mean_list[[1]],diag(nbasis)*var_b))
  else{
    X_coef<-t(MASS::mvrnorm(n_i, mean_list[[1]],diag(nbasis)*var_b))
    for (ii in 2:length(mean_list)) {
      X_coef<-cbind(X_coef,t(MASS::mvrnorm(n_i, mean_list[[ii]],diag(nbasis)*var_b)))

    }
  }

  X_fd<-fda::fd(X_coef,X_basis)
  X<-fda::eval.fd(grid,X_fd)
  X<-X+matrix(stats::rnorm(dim(X)[1]*dim(X)[2],0,sqrt(var_e)),dim(X)[1],dim(X)[2])

  out<-list(X=X,
            X_fd=X_fd,
            mu_fd=mu_fd,
            grid=grid,
            clus=clus_true)

  return(out)
}


