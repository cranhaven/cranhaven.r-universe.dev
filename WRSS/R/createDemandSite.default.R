createDemandSite.default <-
function(name              ="Unknown"          ,
         demandTS          =NA                 ,
         demandParams=list(waterUseRate=NULL   ,
                           waterVariation=NULL ,
                           cropArea=NULL)      ,
         returnFlowFraction =0.0               ,
         suppliers          =NA                ,
         downstream        =NA                 ,
         priority          =NA)
{
   
   if(!any(c(inherits(downstream,c("createAquifer","createRiver","createReservoir","createDiversion","createJunction","createDemandSite")),all(is.na(downstream)))))
   {
      stop("demand site downstream object is wrongly specified!")
   }
   if(all(!is.na(downstream)))
   {
      downstream<-downstream$operation$label
   }
   if(any(c(is.null(demandParams$waterUseRate  ),
            is.null(demandParams$waterVariation),
            is.null(demandParams$cropArea       ))) && all(is.na(demandTS)))  
   {
      stop("missing demand parameter(s) !")
   }
   if(all(is.na(demandTS)))
   {
      if(sum(demandParams$waterVariation)!=100)
      {
         warning('waterVariation is adjusted! the sum of it must be 100 %')
         demandParams$waterVariation<-demandParams$waterVariation+
                                      demandParams$waterVariation/sum(demandParams$waterVariation)*(100-sum(demandParams$waterVariation))
      }
   }
   if(!all(is.na(suppliers)))
   {
      if(all(is.na(match(unlist(lapply(suppliers,class)),c("createAquifer","createRiver","createReservoir","createDiversion")))))
      {
         stop("demand site supplier(s) is/are wrongly specified!")
      }
   }
   suppliersCode<-c()
   if(!all(is.na(suppliers)))
   {
      for(i in 1:length(suppliers)) suppliersCode<-c(suppliersCode,suppliers[[i]]$operation$label)
   }
   suppliers<-suppliersCode
   if(is.na(priority))
   {
      priority<-Inf
   }
   
   resault<-list()
   operation<-createDemandSite.base(name              =name              ,
                                    demandTS          =demandTS          ,
                                    demandParams      =demandParams      ,
                                    returnFlowFraction=returnFlowFraction,
                                    suppliers         =suppliers         ,
                                    downstream        =downstream        ,
                                    priority          =priority)
   resault$operation<-operation
   resault$call<-match.call()
   class(resault)<-'createDemandSite'
   return(resault)
}