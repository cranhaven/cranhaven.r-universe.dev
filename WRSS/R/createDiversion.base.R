createDiversion.base <-
function(name,capacity,divertObject, downstream,priority)
{
   diversion<-list(name=name,
                  label=runif(1),
                  capacity=capacity,
                  divertObject=divertObject,
                  downstream=downstream,
                  priority=priority)
   return(diversion)
}
