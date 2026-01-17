createRiver.base <-
function(name ,downstream,seepageFraction,seepageObject,discharge,priority)
{
   river<-list(name=name,
               label=runif(1),
               downstream=downstream,
               seepageFraction=seepageFraction,
               seepageObject=seepageObject,
               discharge=discharge,
               priority=priority)
   return(river)
}
