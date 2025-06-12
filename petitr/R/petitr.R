petitr<-function(tabvie,niter=100,eps=1E-07,m=1,alpha=0.05,s=1)
{
# check the data
if (class(tabvie)!="data.frame") stop("**** Parameter tabvie must be a data frame ****\n")
n=dim(tabvie)[2]-1
if( n %% m != 0) stop ("**** m must divide n ****\n")
#-------------------------------------------------
# computes lx and mx (function xlxmx)
#---------------------------------------------------

tablif=xlxmx(tabvie,s)
#-------------------------------------------------
# computes r (function r)
#---------------------------------------------------
rm=r(tablif,eps)
cat("-----------------------------------------------------------------------\n")
cat("              petitr : intrinsic rate of increase calculus             \n")
cat("-----------------------------------------------------------------------\n")
cat("\nraw estimator : r= \t",rm,"\n\n")
# Jackknife estimator
#
rpart=NULL
range=1:n
for(i in 1:(n %/% m))
	{
	index=range<((i-1)*m+1) | range>(i*m)
	index=c(TRUE,index)
	# print(index)			#
	# print(tabvie[index])		#
	partlife=xlxmx(tabvie[,index],s)	
	# print(partlife)
	rminus=r(partlife,eps)
	# print(rminus)
	rpart=c(rpart,rminus)
	}
psv=(n*rm-(n-m)*rpart)/m
# cat("partial r list \n")
# print(rpart)
# cat("pseudovalues \n")
# print(psv)
rmj=mean(psv)
# rpm=mean(rpart)
# cat("rm partiel moyen : ",rpm,"\n")
se = sqrt(var(psv))
lower = rmj-qnorm(1-alpha)*sqrt(var(psv))
upper = rmj+qnorm(1-alpha)*sqrt(var(psv))
res=data.frame(t(c(rm,rmj,se,lower,upper)))
names(res)=c("rm raw","rm jackknife","se","lower","upper")
print(res)
invisible(psv)
}
