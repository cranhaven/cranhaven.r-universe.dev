GOF.default<-function(basin,object,observed)
{
   if(missing(basin))
   {
      stop('basin object is missing!')
   }
   if(missing(object))
   {
      stop('object is missing!')
   }
   if(missing(basin))
   {
      stop('observed time series is missing!')
   }
   if(!inherits(basin,'sim'))
   {
      stop('wrong basin object specified!')
   }
   if(!inherits(object,c("createAquifer","createRiver","createReservoir","createDiversion","createJunction","createDemandSite")))
   {
      stop('wrong simulated object specified!')
   }
   if(!(length(basin$operation$operation$simulation$dates)==length(observed)))
   {
      stop('the length of observed time series with the length of simulated times in the object does not match!')
   }
   resault<-list()
   operation<-GOF.base (basin,object,observed)
   resault$operation<-operation
   resault$call<-match.call()
   class(resault)<-'GOF'
   return(resault)
}