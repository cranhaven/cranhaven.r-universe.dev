createArea.base <-
function(name,location,simulation)
{
   reservoirs        <-list()
   junctions         <-list()
   rivers            <-list()
   diversions        <-list()
   demands           <-list()
   aquifers          <-list()
   if (length(names(simulation))==0) names(simulation)<-c("start","end","interval")
   dates<-seq(as.Date(simulation$start),as.Date(simulation$end),simulation$interval)
   conversionTable<-cbind(c('month','week','day'),c(0.382614,1.653439,11.57407))
   conversionFactor<-as.numeric(conversionTable[which(simulation$interval==conversionTable[,1]),2])
   simulation$dates<-dates
   area<-list(name=name,
              location  =location,
              reservoirs=reservoirs,
              aquifers=aquifers,
              junctions =junctions,
              rivers    =rivers,
              diversions=diversions,
              demands   =demands,
              simulation=simulation,
              conversionFactor=conversionFactor,
              timeOfInstantiation=paste(Sys.time(),Sys.timezone()))
   return(area)
}
