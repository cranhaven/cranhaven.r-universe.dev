#####################################################################
# Computation of Volatility impulse response function of time series#
# Arguments: data - log return multivariate time series data        # 
#             shock - point of shock in the time series data        #
# Values: List containing variance and covariance values            #
#####################################################################

VIRF<-function(data,shock)
{

  data1<-ts(data)
  datar<-nrow(data) #number of row in time-series data
  datac<-ncol(data) #number of column in time-series data

   ######################
   # Fitting BEKK Model #
   ######################                           

  fit <-BEKK(data1, order = c(1, 1), params = NULL, fixed = NULL, method = "BFGS", verbose = F)

  params<-fit$est.params
  amat<-params[[1]]
  bmat<-params[[2]]
  cmat<-params[[3]]

  q<-ks::vech(t(amat)%*%amat)
  d<-duplication.matrix(datac)
  l<-elimination.matrix(datac)
  kr<-kronecker(bmat,bmat)
  kp<-kronecker(cmat,cmat)
  r<-l%*%kr%*%d
  p<-l%*%kp%*%d

  ho<-fit$uncond.cov.matrix
  ht<-ks::vech(ho)
  ress<-fit$residuals
  resi<-matrix(unlist(ress),ncol=datac)

  compute<-function(timev)

  {
     ###################################################
     # Recursive function for computation of VECH-BEKK #
     ###################################################

    vech_bekk<-function(shock1)
    {
      if(shock1==shock)
      {
        return(ht)
      }
      else
      {

        return(q+r%*%(ks::vech(as.matrix(resi[shock1-1,])%*%as.matrix(t(resi[shock1-1,]))))+p%*%vech_bekk(shock1-1))
      }

    }

    res<-vech_bekk(timev)
    hht<-invvech(res)

    hrt=sqrtm(hht)
    kk=kronecker(hrt,hrt)

    D <- duplication.matrix(datac)
    DI<-MPinv(D)

    er<-resi[shock,]

    zz<-sqrtm(inv(hht))%*%(er)

    zzt<-t(zz)
    I=diag(datac)
     
    #############################################################################
    # Recursive function for computation of Volatility impulse response function#
    #############################################################################  

    virf_vech<-function(shock2)
    {
      if(shock2==shock)
      {

        return(r%*%DI%*%(kk)%*%D%*%ks::vech((zz%*%zzt)-I))
      }

      else
      {
        return((p+r)%*%virf_vech(shock2-1))
      }

    }
    vv<-virf_vech(timev)

  }


  var<-{}
  cov<-{}

  for(itr in (shock+1):(shock+20))
  {
    virfres<-compute(itr)
    vx<-invvech(virfres)
    dx<-diag(vx)
    ui<- setdiff(virfres,dx)
    cov<-cbind(cov,ui)
    var<-cbind(var,dx)
  }

  var_virf<-t(var)
  cov_virf<-t(cov)
  
  #####################
  # Plotting of graphs#
  #####################

  for(ki in 1:datac)
  {
    plot(var_virf[,ki],type="o",main=paste("variance plot for Series: ",ki),xlab="Time",ylab="Virf")

  }
  tsp<-{}

  for(il in 1:(datac-1))
  {

    for(jl in (il+1):datac)
    {

      ps<-paste(il,",",jl)
      tsp<-rbind(tsp,ps)

    }

  }

  for(kij in 1:((datac*(datac-1))/2))
  {
    plot(cov_virf[,kij],type="o",main=paste("covariance plot for Series: ",tsp[kij]),xlab="Time",ylab="Virf")

  }
  
   
  virfresult<-list(var_virf=var_virf,cov_virf=cov_virf)
  return(virfresult)
}

