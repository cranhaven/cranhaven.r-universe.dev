cap_design.default<-function(area,params,w=NA,plot=TRUE)
{
   if(!inherits(area,"createArea")) {stop('wrong area object specified!')}
   classes<-rep(NA,length(params))
   for(i in 1:length(params)) classes[i]<-class(params[[i]][[1]])
   if(all(is.na(w))) w<-rep(1/sum(classes=="createDemandSite"),sum(classes=="createDemandSite"))
   if(sum(classes=="createDemandSite")!=length(w)) {stop("the length of w must be equal to the number of demand site existing in the params!")}
   if(!inherits(plot,"logical")) {stop("plot arugement accepts logical values only!")}
   resault<-list()
   operation<-cap_design.base(area,params,w,plot)
   resault$operation<-operation
   resault$call<-match.call()
   class(resault)<-'cap_design'
   return(resault)
}