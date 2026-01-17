set.as<-function(base,target,type='downstream')
{
   if(missing(base)){stop('base object is missing, with no default value!')}
   if(missing(target)){stop('target object is missing, with no default value!')}
   if(!inherits(base,c("createAquifer","createRiver","createReservoir","createDiversion","createJunction","createDemandSite")))
   {
      stop("base object is wrongly specified!")
   }
   if(!inherits(target,c("createAquifer","createRiver","createReservoir","createDiversion","createJunction","createDemandSite")))
   {
      stop("base object is wrongly specified!")
   }
   if(type=='supplier')
   {
      
      if(!inherits(base,c("createAquifer","createRiver","createReservoir","createDiversion")))
      {
         stop("base object must be from a water resources class type!")
      }
	  
      if(!inherits(target,"createDemandSite"))
      {
         stop("base object must be from a demand site class type!")
      }
   }
   if(!any(type==c('downstream','supplier','leakageObject','divertObject')))
   {
      stop('type is wrongly specified!')
   }
   if(type=='downstream')   {target$operation$downstream   <-base$operation$label}
   if(type=='supplier')     {target$operation$suppliers    <-c(target$operation$suppliers,base$operation$label)}
   if(type=='leakageObject'){target$operation$leakageObject<-base$operation$label}
   if(type=='divertObject') {target$operation$divertObject <-base$operation$label}
   return(target)
}