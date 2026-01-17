sim.default <-
function(object)
{
   if (missing(object))
   {
      stop("Missing basin object!")
   }
   result<-list()
   operation<-sim.base(object)
   result$operation<-operation
   result$call<-match.call()
   class(result)<-"sim"
   return(result)
}
