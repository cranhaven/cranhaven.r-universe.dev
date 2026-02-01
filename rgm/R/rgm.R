#' Random Graph Model (RGM) Simulation and Analysis
#'
#' This function performs simulations and analysis based on a random graph model.
#' It processes the provided data and optional covariates, performing iterative
#' computations and returning a comprehensive set of results, including graph samples,
#' precision matrices, and other statistical measures.
#'
#' @param data A list of matrices, each representing a condition's data.
#' @param X Optional matrix of covariates; if not provided, the function will handle data without covariates (default: NULL).
#' @param iter Integer, the number of iterations for the MCMC simulation (default: 1000).
#' @param burnin Integer, the number of burn-in iterations for MCMC (default: 0).
#' @param initial.graphs Optional array specifying the initial graphs for each condition (default: NULL).
#' @param D Integer, the dimension for certain calculations (default: 2).
#' @param initial.loc Optional matrix specifying the initial locations (default: NULL).
#' @param initial.alpha Optional vector for initial alpha values (default: NULL).
#' @param initial.theta Optional vector for initial theta values (default: NULL).
#' @param bd.iter Integer, the number of iterations for BDgraph (default: 20).
#' @param bd.jump Integer, the jump parameter for BDgraph (default: 10).
#' @param method Character vector specifying the method to use, options are "ggm" (Gaussian Graphical Model) and "gcgm" (Gaussian Copula Graphical Model) (default: c("ggm", "gcgm")).
#' @param gcgm.dwpar Optional list of parameters for the Gaussian Copula Graphical Model (default: NULL).
#'
#' @return A list containing various elements including sample alpha, sample locations, sample precision matrices (K), sample graphs, and other relevant results. The structure of the list varies based on whether covariates (X) are provided or not.
#' @export
#'
rgm<-function(data,X=NULL,iter=1000,burnin=0,initial.graphs=NULL, D=2, initial.loc=NULL, initial.alpha=NULL, initial.theta=NULL, bd.iter=20,bd.jump=10, method=c("ggm","gcgm"), gcgm.dwpar=NULL)
{
  p<-ncol(data[[1]]) #number of nodes
  n.edge<-p*(p-1)/2 #number of edges
  B<-length(data) #number of conditions

  sample.graphs<-array(dim=c(n.edge,B,iter))
  sample.K<-array(dim=c(n.edge+p,B,iter))
  sample.pi<-array(dim=c(n.edge,B,iter))
  pi.probit = array(dim=c(n.edge,B,iter))

  # lower triangle of pxp matrix
  m<-matrix(1:p,ncol=p,nrow = p)
  lt<-lower.tri(m)
  ltd<-lower.tri(m,diag=TRUE)

  if(is.null(initial.graphs))
  {
    for(i in 1:B)
    {
      g<-huge::huge.select(huge::huge(as.matrix(data[[i]]),method="glasso",verbose = FALSE),criterion="stars",verbose = FALSE)$refit
      sample.graphs[,i,1]<-g[lt]
    }
  }
  else
    sample.graphs[,,1]<-initial.graphs

  sample.cloc<-array(dim = c(B,D,iter))
  sample.alpha<-matrix(0,B,iter)

  if(is.null(initial.loc))
    sample.cloc[,,1]<-matrix(stats::rnorm(B*D),ncol=D)
  else
    sample.cloc[,,1]<-initial.loc

  Z <- X
  if(!is.null(X))
  {
    Z<-as.matrix(X)
    sample.beta<-matrix(0,ncol(Z),iter)
    if(is.null(initial.theta))
    {
      y<-as.vector(sample.graphs[,,1])
      X<-apply(Z,2,rep,B)
      sample.beta[,1]<-stats::coef(stats::glm(y~X, family=stats::binomial(link = "probit")))[-1]
    }
    if(!is.null(initial.theta))
      sample.beta[,1]<-initial.theta
  }

  if(is.null(burnin))
    burnin<-floor(0.75*iter)

  # edge indicators
  e1<-t(m)[lt]
  e2<-m[lt]



  # Initialize K (precision matrix)
  K=vector(mode="list", length=B)
  for (i in 1:B){
    K[[i]]<-diag(p)
    sample.K[,i,1]<-K[[i]][ltd]
  }


  if (method[1]=="gcgm"){
    #calculate truncated points
    tpoints<-vector("list",B)
    for(i in 1:B)
    {
      tpoints[[i]]<-vector("list",2)
      beta.dw<-gcgm.dwpar[[i]]$beta
      q<-gcgm.dwpar[[i]]$q
      pii<-matrix(rep(gcgm.dwpar[[i]]$pii,each=nrow(q)),nrow(q),ncol(q))
      pdw_lb = BDgraph::pdweibull( data[[i]] - 1, q = q, beta = beta.dw)
      pdw_ub = BDgraph::pdweibull( data [[i]], q = q, beta = beta.dw)
      tpoints[[i]][[1]]<-stats::qnorm( ( 1 - pii)*( data[[i]] != 0 ) + pii*pdw_lb)
      tpoints[[i]][[2]] <- stats::qnorm( (1 - pii) + pii*pdw_ub)
    }
  }

  pb <- utils::txtProgressBar(min = 0, max = (iter-1), style = 3)
  for (k in 1: (iter-1))
  {
    utils::setTxtProgressBar(pb = pb, value = k,title = "Performing MCMC iterations")

    # update data if the Gaussian Copula GM (gcgm) is selected
    if (method[1]=="gcgm"){
      data<-sample.data(data, K, tpoints)
    }


    # update latent node and condition locations
    G<-sample.graphs[,,k]
    if(is.null(Z))
      G.loc<-Gmcmc(G,alpha=sample.alpha[,k],loc=sample.cloc[,,k],iter=1,burnin = 0)
    else
      G.loc<-Gmcmc(G,X=Z,alpha=sample.alpha[,k],theta=sample.beta[,k],loc=sample.cloc[,,k],iter=1,burnin = 0)

    cloc<-G.loc$loc[,,1]
    alpha<-G.loc$alpha
    beta<- G.loc$theta

    dist.cond<-matrix(ncol=B,nrow=n.edge)
    for (b in 1:B){
      #updating condition-specific intercept
      dist.cond[,b]<-apply(G,1,function(g,cloc,b){crossprod(colSums(cloc * g)-cloc[b,]*g[b],cloc[b,])},cloc=cloc,b=b)
    }
    Pi = matrix(ncol=B,nrow=n.edge)
    for (b in 1:B){
      for (i in 2:p){
        for (j in 1:(i-1)){
          ind<-e1==j & e2==i
          if(is.null(Z))
            {
            Pi[ind,b]<-stats::pnorm(alpha[b]+dist.cond[ind,b])
            pi.probit[ind,b,k+1]<-stats::pnorm(alpha[b]+dist.cond[ind,b])
            }
          else
            {
            Pi[ind,b]<-stats::pnorm(alpha[b]+dist.cond[ind,b]+Z[ind,]%*%beta)
            pi.probit[ind,b,k+1]<-stats::pnorm(alpha[b]+dist.cond[ind,b]+Z[ind,]%*%beta)
            }
        }
      }
    }

    for (j in 1:B)
    {
      pi.post<-Pi[,j]
      g.prior<-matrix(0,nrow=p,ncol=p)
      g.prior[lt] <- pi.post
      g.prior<-g.prior+t(g.prior)

      g.start<-matrix(0,nrow=p,ncol=p)
      g.start[lt] <- sample.graphs[,j,k]
      g.start<-g.start+t(g.start)

      # update K
      res.bd<-suppressWarnings(BDgraph::bdgraph(data[[j]], iter = bd.iter, jump=bd.jump, g.start=g.start,  g.prior=g.prior, save=FALSE, burnin=0,verbose = FALSE))

      g<-res.bd$last_graph
      K[[j]]<-res.bd$last_K
      sample.graphs[,j,k+1]<-g[lt]
      sample.K[,j,k+1] <- K[[j]][ltd]
      sample.pi[,j,k+1] <- t(res.bd$p_links)[lt]
    }

    sample.cloc[,,k+1]<-cloc
    sample.alpha[,k+1]<-alpha
    if(!is.null(Z))
      sample.beta[,k+1]<-beta
  }

  sample.cloc<-sample.cloc[,,-(1:burnin)]
  sample.alpha<-sample.alpha[,-(1:burnin)]
  sample.graphs<-sample.graphs[,,-(1:burnin)]
  sample.K<-sample.K[,,-(1:burnin)]
  sample.pi<-sample.pi[,,-(1:burnin)]
  pi.probit<-pi.probit[,,-(1:burnin)]

  if(!is.null(Z))
    sample.beta<-sample.beta[,-(1:burnin),drop=FALSE]

  if(is.null(Z))
    return(list(sample.alpha=sample.alpha,sample.loc=sample.cloc,sample.K=sample.K,sample.graphs=sample.graphs,sample.pi=sample.pi,pi.probit=pi.probit))
  else
    return(list(sample.alpha=sample.alpha,sample.theta=sample.beta,sample.K=sample.K,sample.loc=sample.cloc,sample.graphs=sample.graphs,sample.pi=sample.pi,pi.probit=pi.probit))
}


