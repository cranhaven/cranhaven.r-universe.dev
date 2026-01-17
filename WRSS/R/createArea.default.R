createArea.default <-
function(name="unknown",location="unknown",simulation=list(start=NULL,end=NULL,interval=NULL))
{
   names(simulation)<-c("start","end","interval")
   if(any(c(is.null(simulation$start),is.null(simulation$end),is.null(simulation$interval))))
   {
      stop("Simulation time settings are not accuaretly set!")
   }
   dates<-seq(from=as.Date(simulation$start),to=as.Date(simulation$end),by=simulation$interval)

   if(length(dates)==0)
   {
      stop("The enterd dates are not accuarte !")
   }
   resault<-list()
   operation<-createArea.base(name,
                              location,
                              simulation)
   resault$operation<-operation
   resault$call<-match.call()
   class(resault)<-'createArea'
   return(resault)
}