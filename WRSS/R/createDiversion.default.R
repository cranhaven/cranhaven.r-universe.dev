createDiversion.default <-
function(name         ="Div1",
         capacity            ,
         divertObject =NA    ,
         downstream   =NA    ,
         priority     =NA)
{
   if(!any(c(inherits(downstream,c("createAquifer","createRiver","createReservoir","createDiversion","createJunction","createDemandSite")),all(is.na(downstream)))))
   {
      stop("diversion downstream is wrongly specified!")
   }
   if(all(!is.na(downstream)))
   {
      downstream<-downstream$operation$label
   }
   
   if(!any(c(inherits(divertObject,c("createAquifer","createRiver","createReservoir","createDiversion","createJunction","createDemandSite")),all(is.na(divertObject)))))
   {
      stop("diversion target is wrongly specified!")
   }
   if(!all(is.na(divertObject)))
   {
      divertObject<-divertObject$operation$label
   }
   if(missing(capacity))
   {
      stop("capacity is not specified!")
   }
   if(is.na(priority))
   {
      priority<-Inf
   }
   resault<-list()
   operation<-createDiversion.base(name,
                                   capacity,
                                   divertObject,
                                   downstream,
                                   priority)
   resault$operation<-operation
   resault$call<-match.call()
   class(resault)<-'createDiversion'
   return(resault)
}
