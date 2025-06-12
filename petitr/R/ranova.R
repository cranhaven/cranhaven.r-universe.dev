ranova<-function (listab,levels=NULL) 
{
if(class(listab)!="list")stop("**** Parameter listab must be a list *****\n")
p=length(listab)
#
# Computation of the p relative growth rates
#
psv=NULL
levelstobuild=is.null(levels)
for (i in 1:p)
	{
		if(class(listab[[i]])!="data.frame") stop("**** All elements must be a data.frame ****\n")
		if(levelstobuild) 
			{
			level=paste("l",i,sep="")
			levels=c(levels,level)
			}
			else level=levels[i]
		res=petitr(listab[[i]])
		psv=rbind(psv,cbind(res,rep(level,length(res))))
	}
psv=data.frame(psv)
names(psv)=c("pseudoval","pop")
psv$pseudoval=as.numeric(as.character(psv$pseudoval))
# Analysis of variance with lm
m=lm(pseudoval~pop,data=psv)
print(anova(m))
print(summary(m))
invisible(psv)
}
