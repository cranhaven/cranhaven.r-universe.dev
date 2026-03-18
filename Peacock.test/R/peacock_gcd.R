#####################################
#
#  Greatest common divisor
#
#####################################
gcd <- function(x, y)
{
   a <- as.integer(x)
   b <- as.integer(y)

   if( (a == 0) && (b ==0) )
     return (1)

   if(a == 0) return (b)
   if(b == 0) return (a)

   while(1) {
 
      a <-  a %% b
      if(a == 0) return (b)
      
      b <- b %% a
      if(b == 0) return (a)
   }
}
