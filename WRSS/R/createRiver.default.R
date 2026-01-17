createRiver.default <-
function(name            ="river1",
         downstream      =NA      ,
         seepageFraction =NA      ,
         seepageObject   =NA      ,
         discharge       =NA      ,
         priority        =NA)
{
   if(!any(c(inherits(downstream,c("createAquifer","createRiver","createReservoir","createDiversion","createJunction","createDemandSite")),all(is.na(downstream)))))
   {
      stop("river downstream object is wrongly specified!")
   }
   if(all(!is.na(downstream)))
   {
      downstream<-downstream$operation$label
   }
   if(!any(c(inherits(seepageObject,c("createAquifer","createRiver","createReservoir","createDiversion","createJunction","createDemandSite")),all(is.na(seepageObject)))))
   {
      stop("river seepage object is wrongly specified!")
   }
   if(all(!is.na(seepageObject)))
   {
      seepageObject<-seepageObject$operation$label
   }
   if(missing(discharge))
   {
      stop("discharge time series is not specified!")
   }
   if(!is.na(seepageFraction))
   {
      if(seepageFraction>1 | seepageFraction<0)
      {
         stop('seepageFraction must be in [0, 1] interval!')
      }
   }else{
      seepageFraction<-0
   }
   if(is.na(priority)){priority<-Inf}
   resault<-list()
   operation<-createRiver.base(name,
                               downstream,
                               seepageFraction,
                               seepageObject,
                               discharge,
                               priority)
   resault$operation<-operation
   resault$call<-match.call()
   class(resault)<-'createRiver'
   return(resault)
}
