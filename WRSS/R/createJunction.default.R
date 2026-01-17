createJunction.default <-
function(name         ="junc1"  ,
         downstream   =NA)
{
   
   if(!any(c(inherits(downstream,c("createAquifer","createRiver","createReservoir","createDiversion","createJunction","createDemandSite")),all(is.na(downstream)))))
   {
      stop("junction downstream is wrongly specified!")
   }
   if(all(!is.na(downstream)))
   {
      downstream<-downstream$operation$label
   }
   resault<-list()
   operation<-createJunction.base(name,
                                  downstream)
   resault$operation<-operation
   resault$call<-match.call()
   class(resault)<-'createJunction'
   return(resault)
}
