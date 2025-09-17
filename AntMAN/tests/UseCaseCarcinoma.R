#######################################################################################
###############
############### AntMAN Package : Tests and Examples
###############
###############
#######################################################################################

quit() ## Skip this test - too long
library("AntMAN")

library('salso')  ### load the salso library


### We load the following libraries that provide nice graphical tools for dendograms representation
library("ggplot2")
library("ggdendro")

suppressPackageStartupMessages(library('dendextend'))

### Load the data
data(carcinoma)



## write the data in a vector y_mvb just to for notational purposes,
## it reminds data for multivariate binomial data
y_mvb <- carcinoma

## Just verify that we have loaded the right data
is(y_mvb)
head(y_mvb)

### Data quantities
n <- dim(y_mvb)[1] ## The number of observation
d <- dim(y_mvb)[2] ## The dimension of the data





######
## Set the Gibbs Parameter
mcmc_params        = AM_mcmc_parameters(niter=5000 + 50 * 5000, burnin=5000, thin=50, verbose=0, output = c("CI","K","M","Tau","H","Q","W"))

## We are going to use a Bayesian Latent Class analysis, i.e. a mixture of Multivariate Bernoulli
## for the first analysis I will define independent beta prior with (1,1) parameters (i.e. uniform)
## for each component
mixture_mvb_params <- AM_mix_hyperparams_multiber(a0=rep(1,d),b0= rep(1,d))
### WE USE THE DEFAULT PRIOR FOR NEG_BINIMIAL
components_prior   <-  AM_mix_components_prior_negbin(R=1, init_P=0.1,a_P=1,b_P =1)
weights_prior      <-  AM_mix_weights_prior_gamma(init=5, a=1, b=1)

### Let's start with all the data in separate cluster (i.e. n groups each of size 1)
set.seed(321)
 fit <- AM_mcmc_fit(
  		   #initial_clustering=init_ci_mvb,
  		   init_K = 20,
     y = y_mvb, 
    mix_kernel_hyperparams = mixture_mvb_params,
    mix_components_prior =components_prior,
    mix_weight_prior = weights_prior,
    mcmc_parameters = mcmc_params)
   
  summary(fit)
# 
# 
### Save the postarior chains 
# save(fit,file="fit_car_long.Rout")
### Load the posterior chains
#load("fit_car_long.Rout")
plot(fit)

#### Obtain original graphical parameters
oldpar <- par(no.readonly = TRUE)

#### Plot the trace plot of the number of cluster as well as the posterior pdf 
par(mfrow=c(1,2))
plot(fit$K)
plot(table(fit$K))
par(mfrow=c(1,1))


## 
G <- length(fit$K) ## length of the posterior chain
## Posterior estimation of the number of clusters
table(fit$K)/G





#### Posterior Clustering

### Since posterior chain are saved as a list from AntMAN we will
### transform this in a matrix. Moreover we have to add one because 
### AntMAN codes the clustering from 0 to K-1.
ci <- t(do.call(cbind,fit$CI))+1
dim(ci)



hatc <- dlso(ci)  ### that provides the fuction dlso to find the clustering
                  ### that minimizes the Binder loss function

hatk <- length(unique(hatc)) ### the estimated number of cluster is the number of 
                             ### clusters in hatc
table(hatc)/n     ## frequencies of the clusters


C <- AM_coclustering(fit)  ### Compute the similarity matrix
### Visualize the similarity matrix
AM_plot_similarity_matrix (C[ order(hatc),order(hatc)]) ### does not work at the moment
## Old code! 
image(1:n,1:n,C[ order(hatc),order(hatc)],xaxt='n',yaxt="n",xlab="ord. labels",ylab="ord. labels",main="Ordered similarity matrix")
axis(side=1, labels= hatc,at=1:n)
axis(side=2, labels= hatc,at=1:n)



############################################################
########### Cluster specific parameters estimate ###########
############################################################



### define the matrix theta that contain the draw at each iteration 
### of the data specific parameter tau_{c_i} for i=1...n
### Then we need an array with the right dimensions= 
###    "number of posterior sample" x "sample size" x "data dimension"
thetapost <- array(0,dim=c(G,n,d))
#dim(thetapost)
for(g in 1:G){
	thetapost[g,,] =(( fit$TAU[[g]]$theta)[ci[g,],])
}


### We can compute an individual specific (slice in this example) estimation of this parameter
thetahat <- apply(thetapost,c(2,3),mean)


### The cluster specific parameter will be saved in a matrix
est <- matrix(ncol=dim(y_mvb)[2],nrow=hatk)


### We set a single loop for bot compute the cluster specific estimate and plot them
par(mfrow=c(1,hatk))
for(j in 1:hatk){ ## for each cluster:
	#### a) compute the cluster specific estimate
	est[j,] <- apply(thetahat[hatc==j,],2,mean)
	###  b) plot the estimate
	plot(1:7,apply(thetahat[hatc==j,],2,mean),type="h",xaxt="n",xlim=c(0.8,7.2),ylim=c(0,1),col=j,lwd=2,xlab="pathologist",ylab=expression(paste(hat(theta)^"*")),main=paste("group ",j))
	#### c) plot the observed frequencies
	lines((1:7)+0.1,apply(y_mvb[hatc==j,],2,mean),type="h",xaxt="n",xlim=c(0.8,7.2),ylim=c(0,1),col=j,lwd=2,ylab="",lty=2)
	### d) add the abscissa axis wit the corresponding labels
	axis(1, at=1:7, labels=colnames(y_mvb))
}






#########################################################################
########### BNP Hierarchical Clustering following Medvedovic  ###########
#########################################################################



### Fix the color forrhe three clusters
color <- c("black","red","green")

### Compute the dendogram using the ggplot2 syntax where %>% is the pipe operator
hclu <-dist(1-C) %>% hclust %>% as.dendrogram 
hclu <- rotate(hclu,hatc)
hclu <- hclu %>%  set("leaves_pch", 19)  %>% set("leaves_col", color[sort(hatc)]) %>% set("branches_k_color",value=color,k=3) %>% set("branches_lwd", 1.2) %>% set("labels_colors",value=color,k=3) %>% set("labels_cex", 0.7)  
plot(hclu,main="Cluster Dendogram")





##################################################################################
##########  Predictive goodnes of clustering index Argiento et al (2014) #########
##################################################################################



### The data matrix as just 20 different row that we store in the vector yub (y unique)
yun <- unique.matrix(y_mvb)

### in the matrix cnew we are going to save the component at which the new identical observations are 
### assigned to 
cnew <- matrix(nrow=G,ncol=dim(yun)[1])
for(g in 1:G){ ## for each MCMC draw
	for(ii in 1:(dim(yun)[1])){## for each of the 20 different value in yun
		ploc <- vector(length=fit$M[g]) 
		for(mm in 1:fit$M[g]) { ### compute the component probability
			ploc[mm] <-  prod(dbinom(yun[ii,],size=rep(1,d),prob= fit$TAU[[g]]$theta[mm,]))*fit$S[[g]][mm]
		}
		cnew[g,ii] <- sample(1:fit$M[g],size=1,prob= ploc) ### assign the new value to a component according to the probabilities just computed
	}
}



pnew <- rep(0,n)  ### vector of the predictive indices one for each observation
pchin <- rep(0,n) ### a vector that just indicate to wich of the 20 unique values the i-th observation is equal to
for(ii in 1:(dim(yun)[1])){ ### for each of the 20 unique values
	## find the index of these observations that are equal to observation ii
	indx <- which(apply(y_mvb, 1, function(x) identical(x[1:d], yun[ii,]))) 
	for(id in indx){ ## and for these value
	pnew[id] <- mean((cnew[,ii]-ci[,id])==0) ## compute the predictive indices
	pchin[id] <- ii
	}
}

### plot the predictive goodness of clustering indices
plot(pnew,col=hatc,pch=pchin,xlab="",ylab=expression(paste(p^new)),main="predictive index")
abline(h=0.7,lty=2,col="red") ## add a threshold 
idx <- which(pnew<0.7)        ## find which observation express and index below the threshold 

y_mvb[idx,]                   ## give a look at these observation for which clustering is more uncertain 






##### PDF plots
pdf(file.path(tempdir(), "trestime.pdf"),paper="special",height=4, width=12)

par(mar=c(3,3.5,1.5,0.5)+0.1)
#'mar' A numerical vector of the form 'c(bottom, left, top, right)'
#          which gives the number of lines of margin to be specified on
#          the four sides of the plot.  The default is 'c(5, 4, 4, 2) +
#          0.1'.
par(mgp=c(2, 1.0, 0))
#'mgp' The margin line (in 'mex' units) for the axis title, axis
#          labels and axis line.  Note that 'mgp[1]' affects 'title'
#          whereas 'mgp[2:3]' affect 'axis'.  The default is 'c(3, 1,
#          0)'.

par(mfrow=c(1,3))
## SIMILARITY
image(1:n,1:n,C[ order(hatc),order(hatc)],xaxt='n',yaxt="n",xlab="ord. labels",ylab="ord. labels",main="Ordered similarity matrix")
axis(side=1, labels= hatc,at=1:n)
axis(side=2, labels= hatc,at=1:n)
### DENDOGRAM
hclu <- hclu %>% set("labels_cex", 0.01)
plot(hclu,main="Cluster Dendogram",xlab="ord labels")
### INDICE

plot(pnew,col=hatc,pch=pchin,xlab="1:n",ylab=expression(paste(p^new)),main="predictive index")
abline(h=0.7,lty=2,col="red")

#### reset the graphical parameters
par(oldpar)


dev.off()






