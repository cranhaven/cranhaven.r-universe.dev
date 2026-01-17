createReservoir.default <-
function(type='storage',name='unknown',
         priority=NA,downstream=NA,netEvaporation=NA,
         seepageFraction=NA,seepageObject=NA,
         geometry=list(storageAreaTable=NULL,
                       storageElevationTable=NULL,
                       dischargeElevationTable=NULL,
                       deadStorage=NULL,capacity=NULL),
         plant=list(installedCapacity=NULL,
                    efficiency=NULL,
                    designHead=NULL,
                    designFlow=NULL,
                    turbineAxisElevation=NULL,
                    submerged=FALSE,loss=0),
         penstock=list(diameter=NULL,length=NULL,roughness=110),
         initialStorage=NA)
{
   
   if(!any(c(inherits(downstream,c("createAquifer","createRiver","createReservoir","createDiversion","createJunction","createDemandSite")),all(is.na(downstream)))))
   {
      stop("reservoir downstream is wrongly specified!")
   }
   if(all(!is.na(downstream)))
   {
      downstream<-downstream$operation$label
   }
   if(!any(c(inherits(seepageObject,c("createAquifer","createRiver","createReservoir","createDiversion","createJunction","createDemandSite")),all(is.na(seepageObject)))))
   {
      stop("reservoir seepage object is wrongly specified!")
   }
   if(all(!is.na(seepageObject)))
   {
      seepageObject<-seepageObject$operation$label
   }

   geometryBase<-list(storageAreaTable=NULL,
                      storageElevationTable=NULL,
                      dischargeElevationTable=NULL,
                      deadStorage=NULL,
                      capacity=NULL)
   if(any(names(geometry)=='storageElevationTable'))   geometryBase$storageElevationTable<-geometry$storageElevationTable
   if(any(names(geometry)=='storageAreaTable'))        geometryBase$storageAreaTable<-geometry$storageAreaTable
   if(any(names(geometry)=='dischargeElevationTable')) geometryBase$dischargeElevationTable<-geometry$dischargeElevationTable
   if(any(names(geometry)=='deadStorage'))             geometryBase$deadStorage<-geometry$deadStorage
   if(any(names(geometry)=='capacity'))                geometryBase$capacity<-geometry$capacity
   geometry<-geometryBase

   if(type == 'storage')
   {
      if(any(c(is.null(geometry$storageAreaTable),is.null(geometry$capacity))))
      {
         stop('reservoir geometric specifications are not specified!')
      }
   }
   if(type == 'hydropower')
   {
      plantBase<-list(installedCapacity=NULL,
                      efficiency=NULL,
                      designHead=NULL,
                      designFlow=NULL,
                      turbineAxisElevation=NULL,
                      submerged=FALSE,
                      loss=0)
      penstockBase<-list(diameter=NULL,
                         length=NULL,
                         roughness=110)
      if(any(names(plant)   =='installedCapacity'))       plantBase$installedCapacity<-plant$installedCapacity
      if(any(names(plant)   =='efficiency'))              plantBase$efficiency<-plant$efficiency
      if(any(names(plant)   =='designHead'))              plantBase$designHead<-plant$designHead
      if(any(names(plant)   =='designFlow'))              plantBase$designFlow<-plant$designFlow
      if(any(names(plant)   =='turbineAxisElevation'))    plantBase$turbineAxisElevation<-plant$turbineAxisElevation
      if(any(names(plant)   =='submerged'))               plantBase$submerged<-plant$submerged
      if(any(names(plant)   =='loss'))                    plantBase$loss<-plant$loss
      if(any(names(penstock)=='diameter'))                penstockBase$diameter<-penstock$diameter
      if(any(names(penstock)=='length'))                  penstockBase$length<-penstock$length
      if(any(names(penstock)=='roughness'))               penstockBase$roughness<-penstock$roughness
      plant<-plantBase
      penstock<-penstockBase
      if(any(c(is.null(geometry$storageElevationTable),
               is.null(geometry$capacity),
               ifelse(plant$submerged,is.null(geometry$dischargeElevationTable),FALSE),
               is.null(plant$installedCapacity),
               is.null(plant$efficiency),
               is.null(plant$designHead),
               is.null(plant$designFlow),
               is.null(plant$turbineAxisElevation),
               ifelse(penstock$length>0,is.null(penstock$diameter),FALSE))))
      {
          stop('plant parameter(s) or reservoir geometric specifications required for power simulation are missing!')
      }
   }
   if(is.na(priority)){priority<-Inf}
   if(is.null(geometry$deadStorage)){geometry$deadStorage<-0}
   if(is.null(geometry$capacity)){stop("Maximum storage is missing!")}
   if(geometry$deadStorage>geometry$capacity){stop("Minimum storage cannot be greater than capacity!")}
   if(!is.na(initialStorage)){if(initialStorage>geometry$capacity | initialStorage<0){stop('bad initial storage!')}}

   if(!is.na(seepageFraction))
   {
      if(seepageFraction>1 | seepageFraction<0)
      {
         stop('seepageFraction must be in [0, 1] interval!')
      }
   }else{
      seepageFraction<-0
   }

   resault<-list()
   operation<-createReservoir.base (type,name,priority,downstream,netEvaporation,seepageFraction,seepageObject,geometry,plant,penstock,initialStorage)
   resault$operation<-operation
   resault$call<-match.call()
   class(resault)<-'createReservoir'
   return(resault)
}