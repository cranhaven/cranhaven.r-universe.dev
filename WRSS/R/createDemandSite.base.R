createDemandSite.base <-
function(name,demandTS,demandParams,returnFlowFraction,suppliers,downstream,priority)
{
   demand<-list(name=name,
                label=runif(1),
                demandTS=demandTS,
                demandParams=demandParams,
                returnFlowFraction=returnFlowFraction,
                suppliers=suppliers,
                downstream=downstream,
                priority=priority)
   return(demand)
}