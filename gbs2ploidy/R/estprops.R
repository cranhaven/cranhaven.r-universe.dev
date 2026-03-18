estprops <-
function(cov1=NA,cov2=NA,props=c(0.25,0.33,0.5,0.66,0.75),mcmc.nchain=2,mcmc.steps=10000,mcmc.burnin=1000,mcmc.thin=2){
    Nind<-dim(cov1)[2]
    ## create a list with the allele counts for JAGS
    rdDepthList<-vector("list",Nind)
    for(j in 1:Nind){
        x<-which(is.na(cov1[,j])==FALSE)
        rdDepthList[[j]]<-as.matrix(cbind(cov1[x,j],cov2[x,j]))
    }

    ## run the MCMC analysis in R using rjags
    alphas<-vector("list",Nind)
    for(i in 1:Nind){

        ## define the model, which is written in JAGS
        model<-textConnection("model{

        ## binomial likelihood
        for(j in 1:N){ ## N is the number of SNPs where an individual was het.
            y[j] ~ dbin(p[j],n[j])
        }
    
        ## categorical prior on p[j], true allele depth at the SNP
        for(j in 1:N){
            x[j] ~ dcat(alpha[])
            p[j]<-adepth[x[j]]
        }
     
        ## dirichlet prior on alpha
        alpha ~ ddirch(a[]) 
        }")

        ## compile the model
        mod<-jags.model(model,data=list(y=rdDepthList[[i]][,1],n=apply(rdDepthList[[i]],1,sum),adepth=props,a=rep(1,length(props)),N=dim(rdDepthList[[i]])[1]),n.chains=mcmc.nchain)

        ## run the model, burnin then samples
        update(mod,n.iter=mcmc.burnin)
        out<-coda.samples(model=mod,variable.names="alpha",n.iter=mcmc.steps,thin=mcmc.thin)
        est<-summary(out)
        alphas[[i]]<-est[[2]]
        rownames(alphas[[i]])<-props
    }
    alphas
}
