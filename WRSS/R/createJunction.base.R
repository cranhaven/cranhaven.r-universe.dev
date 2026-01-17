createJunction.base <-
function(name,
         downstream)
{
   junction<-list(name=name,
                  label=runif(1),
                  downstream=downstream)

   return(junction)
}
