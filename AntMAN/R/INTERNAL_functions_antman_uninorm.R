#######################################################################################
###############
############### AntMAN Package
###############
###############
#######################################################################################







## This function  produce nice representation of the posterior chain 
## of univariate hyperparameter


univariate_plot <- function(chain,  main_trace="Trace plot", main_auto="Autocorrelation",main_hist="Histogram",col_hist="gray",col_dens="blue",col_quant="red",nclass="fd",lag_acf=30){
### A nice plot to  synthesize our analysis on the parameter theta!
### Please try to understand the code
#x11()

	if(length(unique(chain))==1){
		warning("The chain is constant")
		stop();
	}




## Representation of the posterior chain of  theta
#Divide the plot device in three sub-graph regions
#two square on the upper and a rectangle on the bottom
layout(matrix(c(1,2,3,3),2,2,byrow=TRUE))
#trace-plot of the posterior chain
plot(chain,type="l",main=main_trace)
# autocorrelation plot
acf(chain,main=main_auto,lag.max=lag_acf)


#Histogram
hist(chain,nclass=nclass,freq=FALSE,main=main_hist,col=col_hist) 

## Overlap the kernel-density 
lines(density(chain),col=col_dens,lwd=2)

## Posterior credible interval of beta0
## quantile(chain,prob=c(0.05,0.5,0.95))


## We Display the posterior credible interval on the graph
abline(v=quantile(chain,prob=c(0.05)),col=col_quant,lty=2,lwd=2)
abline(v=quantile(chain,prob=c(0.5)),col=col_quant,lty=1,lwd=2)
abline(v=quantile(chain,prob=c(0.95)),col=col_quant,lty=2,lwd=2)

## Add the legend to the plot
legend("topright",legend=c("posterior median", "95% Credible bounds","kernel density smoother"),lwd=c(2,2,2), col=c(col_quant,col_quant,col_dens),lty=c(1,2,1))


layout(matrix(c(1,1,1,1),1,1,byrow=TRUE))
}


