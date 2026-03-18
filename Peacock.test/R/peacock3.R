###############################################
#
#  Peacock test statistic: three-dimensional case
#
###############################################

peacock3 <- function(x, y)
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

   if( dmin < 3 ) stop('The dimensions of both samples should be at least three')

   ######################################
   # just deal with the first 2 columns 
   ######################################

   ######################################
   # sort the matrix rows by each column
   ######################################

   xy1 <- c(xx[,1], yy[,1])
   xy2 <- c(xx[,2], yy[,2])
   xy3 <- c(xx[,3], yy[,3])

   I1 <- order( xy1 )
   I2 <- order( xy2 )
   I3 <- order( xy3 )
    
   max_hnnn <- 0 
   max_hnpn <- 0
   max_hpnn <- 0
   max_hppn <- 0 

   max_hnnp <- 0 
   max_hnpp <- 0
   max_hpnp <- 0
   max_hppp <- 0 

   for(zu in xy1[I1]){ #1
     for(zv in xy2[I2]){ #2

        hnnn <- 0
        hnpn <- 0
        hpnn <- 0
        hppn <- 0

        t <- 1
        while( t <= n ){

           w <- I3[t]
           if(xy1[w] <= zu){
             if(xy2[w] <= zv){

                 if(w <= n1)
                    hnnn <- hnnn + d1
                 else
                    hnnn <- hnnn - d2

                max_hnnn <- max(max_hnnn, abs(hnnn))

             }else{

                 if(w <= n1)
                    hnpn <- hnpn + d1
                 else
                    hnpn <- hnpn - d2

                max_hnpn <- max(max_hnpn, abs(hnpn))
             }

           }else{
             if(xy2[w] <= zv){

                 if(w <= n1)
                    hpnn <- hpnn + d1
                 else
                    hpnn <- hpnn - d2

                max_hpnn <- max(max_hpnn, abs(hpnn))

             }else{

                 if(w <= n1)
                    hppn <- hppn + d1
                 else
                    hppn <- hppn - d2

                max_hppn <- max(max_hppn, abs(hppn))
             }
           } 

           t <- t + 1
        }

        ####################################

        hnnp <- 0
        hnpp <- 0
        hpnp <- 0
        hppp <- 0

        t <- n
        while( t > 1 ){

           w <- I3[t]
           if(xy1[w] <= zu){
             if(xy2[w] <= zv){

                 if(w <= n1)
                    hnnp <- hnnp + d1
                 else
                    hnnp <- hnnp - d2

                max_hnnp <- max(max_hnnp, abs(hnnp))

             }else{

                 if(w <= n1)
                    hnpp <- hnpp + d1
                 else
                    hnpp <- hnpp - d2

                max_hnpp <- max(max_hnpp, abs(hnpp))
             }

           }else{
             if(xy2[w] <= zv){

                 if(w <= n1)
                    hpnp <- hpnp + d1
                 else
                    hpnp <- hpnp - d2

                max_hpnp <- max(max_hpnp, abs(hpnp))
             }else{

                 if(w <= n1)
                    hppp <- hppp + d1
                 else
                    hppp <- hppp - d2

                max_hppp <- max(max_hppp, abs(hppp))
             }
           } 

           t <- t - 1
        }

     } #2
   } #3
            
   return (max(max_hnnn, max_hnpn, max_hpnn, max_hppn,  max_hnnp, max_hnpp, max_hpnp, max_hppp)/L);        
}
