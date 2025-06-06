laws_ipc <-
function(B,DD,yy,pp,lambda,smooth,nb,nbp,nbunp,center,types,LAWSmaxCores=1, KMweights, hat1) #!
    ###### expectile regression according to eilers, schnabel
    # parameters:
    # formula - vector of responses ~ f(vector of independent,type="which base to use") + ...
    # smooth - if smoothing with schall's algorithm, asymmetric cross validation or no smoothing shall be done
    # lambda - smoothing penalty, important if no smoothing is done
    # nb - number of parameters per covariate
    # nbp - number of penalized parameters per covariate
    # nbunp - number of unpenalized parameters per covariate
{
    nterms = length(nb)
    m = length(yy)
    np = length(pp)
    
    
    #  myapply <- lapply
    #  if (.Platform$OS.type == "unix" && require("parallel")) 
    #  {
    #      if (!parallel:::isChild()) 
    #      {
    #          myapply <- mclapply
    #      }
    #  }
    
    if(length(lambda) < nterms)
        lala = rep(lambda[1],nterms)
    else
        lala = lambda
    
    lala_orig <- lala
    
    constmat = matrix(0,nrow=1,ncol=ncol(B))
    
    dummy.reg <- function(pp,lala,smooth,yy,B,DD,nb,nterms,center)
    {
        #cat("Expectile: ",pp,"\n")
        
        if(smooth == "schall")
        {
       sch = schall_ipc(yy,B,pp,DD,nb,lala,constmat,center,types, KMweights = KMweights, hat1 = hat1) #!
       lala = sch[[2]]
       vector.a.ma.schall = sch[[1]]
       diag.hat = sch[[3]]
       hat <- sch$hat #!
       aic <- sch$aic #!
        } else if (smooth == "ocv") {
            
            acv.min = nlminb(start = lala, objective = acv_ipc, yy = yy, 
                             B = B, quantile = pp, DD = DD, nb = nb, constmat = constmat, 
                             KMweights = KMweights, lower = 0, upper = 10000)
            aa <- asyregpen_ipc(yy, B, pp, abs(acv.min$par), DD, nb, constmat, KMweights = KMweights, hat1 = hat1) #!
            vector.a.ma.schall <- aa$a
            lala <- abs(acv.min$par)
            diag.hat = aa$diag.hat.ma
            aic <- log(sum(aa$weight * (yy - B %*% aa$a)^2/length(yy))) * length(yy) + 2 * (sum(aa$diag.hat.ma))
        } else if (smooth == "aic") {
            acv.min = nlminb(start = lala, objective = aicfun_ipc, 
                             yy = yy, B = B, quantile = pp, DD = DD, nb = nb, 
                             constmat = constmat, KMweights = KMweights, lower = 0, upper = 10000)
            aa <- asyregpen_ipc(yy, B, pp, abs(acv.min$par), DD, nb, constmat, KMweights = KMweights, hat1 = hat1) #!
            vector.a.ma.schall <- aa$a
            lala <- abs(acv.min$par)
            diag.hat = aa$diag.hat.ma
            aic <- log(sum(aa$weight * (yy - B %*% aa$a)^2/length(yy))) * length(yy) + 2 * (sum(aa$diag.hat.ma))
        } else if (smooth == "bic") {
            acv.min = nlminb(start = lala, objective = bicfun_ipc, 
                             yy = yy, B = B, quantile = pp, DD = DD, nb = nb, 
                             constmat = constmat, KMweights = KMweights, lower = 0, upper = 10000)
            aa <- asyregpen_ipc(yy, B, pp, abs(acv.min$par), DD, nb, constmat, KMweights = KMweights, hat1 = hat1) #!
            vector.a.ma.schall <- aa$a
            lala <- abs(acv.min$par)
            diag.hat = aa$diag.hat.ma
            aic <- log(sum(aa$weight * (yy - B %*% aa$a)^2/length(yy))) * length(yy) + 2 * (sum(aa$diag.hat.ma))
        } else if (smooth == "cvgrid") {
            lala = cvgrid_ipc(yy, B, pp, DD, nb, constmat, types, KMweights = KMweights)
            aa <- asyregpen_ipc(yy, B, pp, lala, DD, nb, constmat, KMweights = KMweights, hat1 = hat1) #!
            vector.a.ma.schall <- aa$a
            diag.hat = aa$diag.hat.ma
            aic <- log(sum(aa$weight * (yy - B %*% aa$a)^2/length(yy))) * length(yy) + 2 * (sum(aa$diag.hat.ma))
        } else if (smooth == "lcurve") {
            lala = lcurve_ipc(yy, B, pp, DD, nb, constmat, types, KMweights = KMweights)
            aa <- asyregpen_ipc(yy, B, pp, lala, DD, nb, constmat, KMweights = KMweights, hat1 = hat1) #!
            vector.a.ma.schall <- aa$a
            diag.hat = aa$diag.hat.ma
            aic <- log(sum(aa$weight * (yy - B %*% aa$a)^2/length(yy))) * length(yy) + 2 * (sum(aa$diag.hat.ma))
        } else{
            #!
            aa <- asyregpen_ipc(yy, B, pp, lala, DD, nb, constmat, KMweights = KMweights, hat1 = hat1) #!
            vector.a.ma.schall <- aa$a
            diag.hat = aa$diag.hat.ma
            hat <- aa$hat
            aic <- log(sum(aa$weight * (yy - B %*% aa$a)^2/length(yy))) * length(yy) + 2 * (sum(aa$diag.hat.ma))
            #!

        }

        
        list(vector.a.ma.schall,lala,diag.hat, aic, hat)
    }
    
    if (.Platform$OS.type == "unix")
        coef.vector = mclapply(pp,function(pp) dummy.reg(pp,lala,smooth,yy,B,DD,nb,nterms,center),mc.cores = max(1,min(detectCores()-1,LAWSmaxCores)))
    else if (.Platform$OS.type == "windows")
        coef.vector = mclapply(pp,function(pp) dummy.reg(pp,lala,smooth,yy,B,DD,nb,nterms,center),mc.cores = 1)
    
    
    lala <- matrix(lambda, nrow=nterms, ncol=np)
    vector.a.ma.schall <- matrix(NA, nrow=sum(nb)+(1*center),ncol=np)
    diag.hat = matrix(NA,nrow=m,ncol=np)
    
    aic <- rep(NA, np) #!
    
    for(i in 1:np)
    {
        vector.a.ma.schall[,i] = coef.vector[[i]][[1]]
        lala[,i] = coef.vector[[i]][[2]]
        diag.hat[,i] = coef.vector[[i]][[3]]
        aic[i] = coef.vector[[i]][[4]] #!
        hat <- coef.vector[[i]][[5]] #!
    }
    
    # right Riemannian sum
    if(length(pp) > 1){ #!
        aic_area <- sum(aic * (c(pp[-1], pp[length(pp)]) - pp))  #!
    } else { #!
        aic_area <- aic #!
    }#!
    
    return(list(vector.a.ma.schall,lala,diag.hat, aic_area = aic_area, hat = hat)) #!
}
