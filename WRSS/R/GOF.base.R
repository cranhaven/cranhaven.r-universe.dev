GOF.base<-function(basin,object,observed)
{
   label<-object$operation$label
   NOobjects<-as.numeric(summary(basin$operation$operation)[3:8,1])
   for (i in 1:length(NOobjects))
   {
      if(NOobjects[i]>0)
      {
         for (j in 1:NOobjects[i])
         {
            if((basin$operation$operation[[2+i]])[[j]]$operation$label==object$operation$label)
            {
               return(chisq.test(x=observed,y=apply((basin$operation$operation[[2+i]])[[j]]$operation$outflow,1,sum),simulate.p.value = TRUE))
            }
         }
      }
   }
}