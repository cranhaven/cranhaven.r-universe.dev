###############################################
#
#  Peacock test statistic: two-dimensional case
#
###############################################

peacock2 <- function(x, y)
{
   xx <- as.matrix(x)
   yy <- as.matrix(y)
  
   n1 <- nrow(xx)
   n2 <- nrow(yy)
   n <- n1 + n2

   ##########################
   #  geatest common divisor: dd
   #  least common multiple: L
   ##########################
   
   dd <- gcd(n1, n2)

   L <- n1/dd*n2   
   d1 <- L/n1
   d2 <- L/n2
   
   ##########################
 
   dim1 <- ncol(xx)
   dim2 <- ncol(yy)

   dmin <- min(dim1, dim2)

   if( dmin < 2 ) stop('The dimensions of both samples should be at least two')

   ######################################
   # just deal with the first 2 columns 
   ######################################

   ######################################
   # sort the matrix rows by each column
   ######################################

   xy1 <- c(xx[,1], yy[,1])
   xy2 <- c(xx[,2], yy[,2])

   I1 <- order( xy1 )
   I2 <- order( xy2 )
    
   max_hnn <- 0 
   max_hpn <- 0
   max_hnp <- 0
   max_hpp <- 0 
   for(zu in xy1[I1]){
            
        hnn <- 0
        hpn <- 0

        t <- 1
        while( t <= n ){

           v <- I2[t]
           if(xy1[v] <= zu){

               if(v <= n1) hnn <- hnn + d1
               else        hnn <- hnn - d2
              
               max_hnn <- max(max_hnn, abs(hnn))
           }else{

               if(v <= n1) hpn <- hpn + d1
               else        hpn <- hpn - d2

               max_hpn <- max(max_hpn, abs(hpn))
           } 

           t <- t + 1
        }

        hnp <- 0
        hpp <- 0

        t <- n
        while(t > 1){

           v <- I2[t]
           if(xy1[v] <= zu){

               if(v <= n1) hnp <- hnp + d1
               else        hnp <- hnp - d2

               max_hnp <- max(max_hnp, abs(hnp))

           }else{

               if(v <= n1) hpp <- hpp + d1
               else        hpp <- hpp - d2

               max_hpp <- max(max_hpp, abs(hpp))
           } 

          t <- t - 1
       }
   }    
   return (max(max_hnn, max_hpn, max_hnp, max_hpp)/L)
}
