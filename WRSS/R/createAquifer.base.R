createAquifer.base <-
function(name,area,volume,rechargeTS,Sy,leakageFraction,initialStorage,leakageObject,priority)
{
   aquifer<-list(name           =name           ,
                 area           =area           ,
                 label          =runif(1)       ,
                 volume         =volume         ,
                 rechargeTS     =rechargeTS     ,
                 Sy             =Sy             ,
                 leakageFraction=leakageFraction,
                 initialStorage =initialStorage ,
                 leakageObject  =leakageObject  ,
                 priority       =priority)
   return(aquifer)
}
