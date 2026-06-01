informedsen <-
function(gamma,sc,z,mset,alpha=0.05){
  chvec<-requireNamespace("gurobi", quietly = TRUE)
  eps=0.005
  # Check input
  if (is.data.frame(sc)) sc <- as.matrix(sc)
  stopifnot(is.matrix(sc)) # sc must be a matrix or a dataframe
  stopifnot((dim(sc)[2])>=2)
  stopifnot(is.vector(gamma)&(length(gamma)==1)&(gamma>=1))
  stopifnot(all(!is.na(as.vector(sc)))) # At present, sc has no NAs
  stopifnot(all((z==1)|(z==0)))
  stopifnot(length(z)==length(mset))
  stopifnot(length(z)==(dim(sc)[1]))
  stopifnot(sum(z)==length(unique(mset)))
  stopifnot(is.vector(alpha))
  stopifnot(all(alpha>0)&(all(alpha<1)))
  if (length(alpha)==1) alpha<-rep(alpha,dim(sc)[2])
  stopifnot(length(alpha)==(dim(sc)[2]))

  # Organize input into a uniform form


  crit<-qchisq(1-alpha,1) # Critical values for tests
  nsets<-length(unique(mset)) # Number of matched sets
  ss<-length(mset)/nsets # set-size = Size of each matched set
  nobs<-length(z)  # Number of observations = nsets*ss
  nvars<-dim(sc)[2]  # number of variables

  o<-order(mset,1-z)
  id<-1:nobs
  id<-id[o]
  z<-z[o]
  mset<-mset[o]
  sc<-sc[o,]
  org<-function(v){
    as.vector(t(matrix(v,ss,nsets)))
  }
  z<-org(z)
  mset<-org(mset)
  for (j in 1:nvars){
    sc[,j]<-org(sc[,j])
  }

  # In general, a vector ending in m has one entry per column of sc
  Tm<-apply(sc[z==1,],2,sum) # Vector of test statistics

  # The am are the linear coefficients in the quadratic functions
  am<-matrix(NA,ss*nsets,nvars)
  for (j in 1:nvars){
    am[,j]<-(2*Tm[j]*sc[,j])+(crit[j]*sc[,j]*sc[,j])
  }

  # The bm are the quadratic coefficients in the quadratic functions
  bm<-array(NA,c(ss*nsets,ss*nsets,nvars))
  delta<-1*outer(rep(1:nsets,ss),rep(1:nsets,ss),"==")
  for (j in 1:nvars){
    bm[,,j]<-outer(sc[,j],sc[,j],"*")*(1+crit[j]*delta)
  }
  rm(delta)

  # Set up the quadratic model for gurobi

  model<-list()
  model$lb<-rep(1/(1+(ss-1)*gamma),nobs) #lower bounds
  model$ub<-rep(gamma/((ss-1)+gamma),nobs) #upper bounds

  # Quadratic objective function
  model$modelsense<-"min" # this is the default anyway
  model$objcon<-(Tm[1]*Tm[1])
  model$obj<-(-am[,1])
  model$Q<-bm[,,1]

  # Quadratic constraints
  model$quadcon<-NULL
  for (j in 2:nvars){
    model$quadcon[[j-1]]<-list(Qc=bm[,,j],q=(-am[,j]),rhs=-(Tm[j]*Tm[j]),
                               beta=-(Tm[j]*Tm[j]),sense="<")
  }

  # Make linear constraint matrix
  A<-1*outer(1:nsets,rep(1:nsets,ss),"==")
  con<-NULL
  for (i in 1:ss){
    for (j in 1:ss){
      if (i != j){
        conij<-rep(0,ss)
        conij[i]<-(-gamma)
        conij[j]<-1
        con<-rbind(con,conij)
      }
    }
  }
  A<-rbind(A,con%x%diag(rep(1,nsets)))
  rhs<-c(rep(1,nsets),rep(0,nsets*(dim(con)[1])))

  model$A<-A
  model$rhs<-rhs
  model$sense<-c(rep("=",nsets),rep("<",nsets*(dim(con)[1])))


  result<-gurobi::gurobi(model)


  eval<-function(thetas=result$x){
    o<-matrix(NA,5,nvars)
    thetam<-matrix(thetas,nsets,ss)
    colnames(o)<-c(0:(nvars-1))
    rownames(o)<-c("num","denom","dev","dif","altdif")
    for (j in 1:nvars){
      scj<-matrix(sc[,j],nsets,ss)
      num<-sum(scj[,1])-sum(scj*thetam)
      o[1,j]<-num
      denom<-sum(apply(scj*scj*thetam,1,sum)-(apply(scj*thetam,1,sum)^2))
      o[2,j]<-denom
      dev<-num/sqrt(denom)
      o[3,j]<-dev
      dif<-(num*num)-(crit[j]*denom)
      o[4,j]<-dif
      altdif<-(Tm[j]^2)+(thetas%*%bm[,,j]%*%thetas)-sum(am[,j]*thetas)
      o[5,j]<-altdif
    }

    totals<-summary(apply(thetam,1,sum))
    ratios<-summary(apply(thetam,1,max)/apply(thetam,1,min))
    list(obj=result$objval,o=o,totals=totals,ratios=ratios)
  }

  if (!is.null(result$x)) {
    ev<-eval(thetas=result$x)
    theta<-result$x
    frm<-data.frame(mset,z,theta,sc)
    o<-order(mset,1-z)
    frm<-frm[o,]
    rownames(frm)<-1:(dim(frm)[1])
    biastests<-as.vector(ev$o[3,2:nvars]^2)
    bt<-all(biastests<=(eps+crit[2:nvars]))
  }
  else {
    frm=NULL
    bt<-FALSE
  }

  if ((!is.null(result$x)) & bt) {
    text1<-paste("Some biases of magnitude Gamma = ",gamma,
    " are not rejected.")
    text2<-"The optimization problem is feasible."
    if (ev$obj>=0){
      if (nvars==2) text3<-paste("The test on the primary outcome at level ",alpha[1],
                                 "rejects the hypothesis of no effect in the presence of a bias of at most Gamma = ",gamma,
                                 " inside the confidence set formed by the test for bias.")
      else text3<-paste("The test on the primary outcome at level ",alpha[1],
       " rejects the hypothesis of no effect in the presence of a bias of at most Gamma = ",gamma,
       " inside the confidence set formed by the tests for bias.")
    }
    else {
      if (nvars==2) text3<-paste("The test on the primary outcome at level ",alpha[1],
                                 "fails to reject the hypothesis of no effect in the presence of a bias of at most Gamma = ",
                                 gamma," inside the confidence set formed by the test for bias.")
      else text3<-paste("The test on the primary outcome at level ",alpha[1],
      "fails to reject the hypothesis of no effect in the presence of a bias of at most Gamma = ",gamma,
                        " inside formed by the tests for bias.")
    }
  }
  else{
    text1<-paste("All biases of magnitude Gamma = ",gamma,
    " are rejected as too small.")
    text2<-"The optimization problem is infeasible."
    text3<-"To continue the analysis, try a larger Gamma."
  }
  if (is.null(colnames(sc))) nms<-(1:nvars)
  else nms<-colnames(sc)

  if (!is.null(result$x)) {
    deviates<-as.vector(ev$o[3,])

  }
  else deviates<-rep(NA,nvars)
  names(deviates)<-nms
  alpha<-c(alpha,sum(alpha))
  names(alpha)<-c(nms,"total")
  list(result=text1,optimization.problem=text2,
       conclusion=text3,deviates=deviates,alphas=alpha)
}
