
########### Third integral of an I-spline ########

ispline3 <- function(x, knots1, m) {    # ispline1 is a function that evaluates integral of an I-spline created by inegrating
  
  # x is a vector of values to compute the I-spline at
  # i ith I-spline, k= knots vector, m= order
  # m <- order 4
  # k <-knots (the knots used in computing M-splines of order 5 should be used
  # in computing the I-splines)
  dk <- knots1[2]-knots1[1]
  k  <- c(knots1[1]-dk*(4:1), knots1, knots1[length(knots1)]+dk*(1:4))
  k6  <- c(knots1[1]-dk*(5:1), knots1, knots1[length(knots1)]+dk*(1:5))
  k7  <- c(knots1[1]-dk*(6:1), knots1, knots1[length(knots1)]+dk*(1:6))
  k8  <- c(knots1[1]-dk*(7:1), knots1, knots1[length(knots1)]+dk*(1:7))
  
  msplinedesign8 <- dmsplinedesign(x, knots1, 8, deriv=0) # A matrix of order 8 to be used in computing the I-splines (integral of the M-splines of order 7),
  
  # k8 = knots8 is the set of knots used to compute M-splines of order 8
  
  d <- length(k)-m-1 # number of columuns for the I-spline matrices
  resu <- matrix(0, length(x), length(k)-m-1)
  for (j in 1:length(x))
    for (i in 1:(length(k)-m-1))
      if (x[j]>k[i+m+1]){
        # resu[j,i] <- 1
        resu[j,i] <- ((((x[j])^3)/(2*3)) - ((k[i+m+1]*x[j]^2))/2 + ((x[j]*k[i+m+1]^2)/2) - ((k[i+m+1]^3)/6))
        
        + ((k[i+m+1+1]-k[i+1])/(m+1))*(    ((k6[i+m+2+1]-k6[i+1])/(m+2))*( (  ((k7[i+m+3+1]-k7[i+1])/(m+3))*((((k8[i+m+4+1]-k8[i+1])/(m+4))*((dmsplinedesign(k[i+m+1], knots1, 8, deriv=0))[1,i+1+3])) + (((k8[i+1+m+4+1]-k8[i+1+1])/(m+4))*((dmsplinedesign(k[i+m+1], knots1, 8, deriv=0))[1,i+1+1+3])) + (((k8[i+2+m+4+1]-k8[i+2+1])/(m+4))*((dmsplinedesign(k[i+m+1], knots1, 8, deriv=0))[1,i+2+1+3])) + (((k8[i+3+m+4+1]-k8[i+3+1])/(m+4))*((dmsplinedesign(k[i+m+1], knots1, 8, deriv=0))[1,i+3+1+3])))
                                                                              
                                                                              
                                                                              + ((k7[i+1+m+3+1]-k7[i+1+1])/(m+3))*(((((k8[i+1+m+4+1]-k8[i+1+1])/(m+4))*((dmsplinedesign(k[i+m+1], knots1, 8, deriv=0))[1,i+1+1+3])) + (((k8[i+2+m+4+1]-k8[i+2+1])/(m+4))*((dmsplinedesign(k[i+m+1], knots1, 8, deriv=0))[1,i+2+1+3])) + (((k8[i+3+m+4+1]-k8[i+3+1])/(m+4))*((dmsplinedesign(k[i+m+1], knots1, 8, deriv=0))[1,i+3+1+3])))
                                                                                                                   
                                                                              )
                                                                              
                                                                              
                                                                              + ((k7[i+2+m+3+1]-k7[i+2+1])/(m+3))*(((((k8[i+2+m+4+1]-k8[i+2+1])/(m+4))*((dmsplinedesign(k[i+m+1], knots1, 8, deriv=0))[1,i+2+1+3])) + (((k8[i+3+m+4+1]-k8[i+3+1])/(m+4))*((dmsplinedesign(k[i+m+1], knots1, 8, deriv=0))[1,i+3+1+3])))
                                                                                                                   
                                                                              )
                                                                              
                                                                              
                                                                              + ((k7[i+3+m+3+1]-k7[i+3+1])/(m+3))*(((((k8[i+3+m+4+1]-k8[i+3+1])/(m+4))*((dmsplinedesign(k[i+m+1], knots1, 8, deriv=0))[1,i+3+1+3])))
                                                                                                                   
                                                                              )      ))
                                           
                                           
                                           + ((k6[i+1+m+2+1]-k6[i+1+1])/(m+2))*( ( ((k7[i+1+m+3+1]-k7[i+1+1])/(m+3))*(((((k8[i+1+m+4+1]-k8[i+1+1])/(m+4))*((dmsplinedesign(k[i+m+1], knots1, 8, deriv=0))[1,i+1+1+3])) + (((k8[i+2+m+4+1]-k8[i+2+1])/(m+4))*((dmsplinedesign(k[i+m+1], knots1, 8, deriv=0))[1,i+2+1+3])) + (((k8[i+3+m+4+1]-k8[i+3+1])/(m+4))*((dmsplinedesign(k[i+m+1], knots1, 8, deriv=0))[1,i+3+1+3])))
                                                                                                                      
                                           )
                                           
                                           
                                           + ((k7[i+2+m+3+1]-k7[i+2+1])/(m+3))*(((((k8[i+2+m+4+1]-k8[i+2+1])/(m+4))*((dmsplinedesign(k[i+m+1], knots1, 8, deriv=0))[1,i+2+1+3])) + (((k8[i+3+m+4+1]-k8[i+3+1])/(m+4))*((dmsplinedesign(k[i+m+1], knots1, 8, deriv=0))[1,i+3+1+3])))
                                                                                
                                           )
                                           
                                           
                                           + ((k7[i+3+m+3+1]-k7[i+3+1])/(m+3))*(((((k8[i+3+m+4+1]-k8[i+3+1])/(m+4))*((dmsplinedesign(k[i+m+1], knots1, 8, deriv=0))[1,i+3+1+3])))
                                                                                
                                           )      ))
                                           
                                           
                                           
                                           + ((k6[i+2+m+2+1]-k6[i+2+1])/(m+2))*( ( ((k7[i+2+m+3+1]-k7[i+2+1])/(m+3))*(((((k8[i+2+m+4+1]-k8[i+2+1])/(m+4))*((dmsplinedesign(k[i+m+1], knots1, 8, deriv=0))[1,i+2+1+3])) + (((k8[i+3+m+4+1]-k8[i+3+1])/(m+4))*((dmsplinedesign(k[i+m+1], knots1, 8, deriv=0))[1,i+3+1+3])))
                                                                                                                      
                                           )
                                           
                                           
                                           + ((k7[i+3+m+3+1]-k7[i+3+1])/(m+3))*(((((k8[i+3+m+4+1]-k8[i+3+1])/(m+4))*((dmsplinedesign(k[i+m+1], knots1, 8, deriv=0))[1,i+3+1+3])))
                                                                                
                                           )      ))
                                           
                                           
                                           
                                           + ((k6[i+3+m+2+1]-k6[i+3+1])/(m+2))*( (((k7[i+3+m+3+1]-k7[i+3+1])/(m+3))*(((((k8[i+3+m+4+1]-k8[i+3+1])/(m+4))*((dmsplinedesign(k[i+m+1], knots1, 8, deriv=0))[1,i+3+1+3]))))))
                                           
        )
        
        
        + ((k[i+1+m+1+1]-k[i+1+1])/(m+1))*(    ((k6[i+1+m+2+1]-k6[i+1+1])/(m+2))*( ( ((k7[i+1+m+3+1]-k7[i+1+1])/(m+3))*(((((k8[i+1+m+4+1]-k8[i+1+1])/(m+4))*((dmsplinedesign(k[i+m+1], knots1, 8, deriv=0))[1,i+1+1+3])) + (((k8[i+2+m+4+1]-k8[i+2+1])/(m+4))*((dmsplinedesign(k[i+m+1], knots1, 8, deriv=0))[1,i+2+1+3])) + (((k8[i+3+m+4+1]-k8[i+3+1])/(m+4))*((dmsplinedesign(k[i+m+1], knots1, 8, deriv=0))[1,i+3+1+3])))
                                                                                                                        
        )
        
        
        + ((k7[i+2+m+3+1]-k7[i+2+1])/(m+3))*(((((k8[i+2+m+4+1]-k8[i+2+1])/(m+4))*((dmsplinedesign(k[i+m+1], knots1, 8, deriv=0))[1,i+2+1+3])) + (((k8[i+3+m+4+1]-k8[i+3+1])/(m+4))*((dmsplinedesign(k[i+m+1], knots1, 8, deriv=0))[1,i+3+1+3])))
                                             
        )
        
        
        + ((k7[i+3+m+3+1]-k7[i+3+1])/(m+3))*(((((k8[i+3+m+4+1]-k8[i+3+1])/(m+4))*((dmsplinedesign(k[i+m+1], knots1, 8, deriv=0))[1,i+3+1+3])))
                                             
        )      ))
        
        
        
        + ((k6[i+2+m+2+1]-k6[i+2+1])/(m+2))*( ( ((k7[i+2+m+3+1]-k7[i+2+1])/(m+3))*(((((k8[i+2+m+4+1]-k8[i+2+1])/(m+4))*((dmsplinedesign(k[i+m+1], knots1, 8, deriv=0))[1,i+2+1+3])) + (((k8[i+3+m+4+1]-k8[i+3+1])/(m+4))*((dmsplinedesign(k[i+m+1], knots1, 8, deriv=0))[1,i+3+1+3])))
                                                                                   
        )
        
        
        + ((k7[i+3+m+3+1]-k7[i+3+1])/(m+3))*(((((k8[i+3+m+4+1]-k8[i+3+1])/(m+4))*((dmsplinedesign(k[i+m+1], knots1, 8, deriv=0))[1,i+3+1+3])))
                                             
        )      ))
        
        
        
        + ((k6[i+3+m+2+1]-k6[i+3+1])/(m+2))*( (((k7[i+3+m+3+1]-k7[i+3+1])/(m+3))*(((((k8[i+3+m+4+1]-k8[i+3+1])/(m+4))*((dmsplinedesign(k[i+m+1], knots1, 8, deriv=0))[1,i+3+1+3]))))))
        
        )
        
        
        + ((k[i+2+m+1+1]-k[i+2+1])/(m+1))*(((k6[i+2+m+2+1]-k6[i+2+1])/(m+2))*( ( ((k7[i+2+m+3+1]-k7[i+2+1])/(m+3))*(((((k8[i+2+m+4+1]-k8[i+2+1])/(m+4))*((dmsplinedesign(k[i+m+1], knots1, 8, deriv=0))[1,i+2+1+3])) + (((k8[i+3+m+4+1]-k8[i+3+1])/(m+4))*((dmsplinedesign(k[i+m+1], knots1, 8, deriv=0))[1,i+3+1+3])))
                                                                                                                    
        )
        + ((k7[i+3+m+3+1]-k7[i+3+1])/(m+3))*(((((k8[i+3+m+4+1]-k8[i+3+1])/(m+4))*((dmsplinedesign(k[i+m+1], knots1, 8, deriv=0))[1,i+3+1+3])))
                                             
        )      ))
        
        
        
        + ((k6[i+3+m+2+1]-k6[i+3+1])/(m+2))*( (((k7[i+3+m+3+1]-k7[i+3+1])/(m+3))*(((((k8[i+3+m+4+1]-k8[i+3+1])/(m+4))*((dmsplinedesign(k[i+m+1], knots1, 8, deriv=0))[1,i+3+1+3]))))))
        
        )
        
        
        + ((k[i+3+m+1+1]-k[i+3+1])/(m+1))*(((k6[i+3+m+2+1]-k6[i+3+1])/(m+2))*( (((k7[i+3+m+3+1]-k7[i+3+1])/(m+3))*(((((k8[i+3+m+4+1]-k8[i+3+1])/(m+4))*((dmsplinedesign(k[i+m+1], knots1, 8, deriv=0))[1,i+3+1+3]))))))
        )
        
      } else if (x[j] < k[i+1]) {
        # resu[j,i]<- 0
        resu[j,i]<- 0
      } else if (x[j]> k[i+1] && x[j]<=k[i+1+1]) {
        # resu[j,i] <- (k[i+m+1]-k[i])*(msplinedesign5[j,i])/(m+1)
        resu[j,i] <- ((k[i+m+1+1]-k[i+1])/(m+1))*((k6[i+m+2+1]-k6[i+1])/(m+2))*((k7[i+m+3+1]-k7[i+1])/(m+3))*((k8[i+m+4+1]-k8[i+1])/(m+4))*(msplinedesign8[j,i+1+3])
        
      } else if (x[j]>k[i+1+1] && x[j]<=k[i+2+1]) {
        # resu[j,i] <- (k[i+m+1]-k[i])*(msplinedesign5[j,i])/(m+1) + (k[i+1+m+1]-k[i+1])*(msplinedesign5[j,i+1])/(m+1)
        # resu[j,i] <- ((k[i+m+1]-k[i])/(m+1))*((k6[i+m+2]-k6[i])/(m+2))*((k7[i+m+3]-k7[i])/(m+3))*((k8[i+m+4]-k8[i])/(m+4))*(msplinedesign8[j,i]) + ((k[i+1+m+1]-k[i+1])/(m+1))*((k6[i+1+m+2]-k6[i+1])/(m+2))*((k7[i+1+m+3]-k7[i+1])/(m+3))*((k8[i+1+m+4]-k8[i+1])/(m+4))*(msplinedesign8[j,i+1])
        
        resu[j,i] <- ((k[i+m+1+1]-k[i+1])/(m+1))*((((k6[i+m+2+1]-k6[i+1])/(m+2))*((((k7[i+m+3+1]-k7[i+1])/(m+3))*((((k8[i+m+4+1]-k8[i+1])/(m+4))*(msplinedesign8[j,i+1+3])) + (((k8[i+1+m+4+1]-k8[i+1+1])/(m+4))*(msplinedesign8[j,i+1+1+3]))) + ((k7[i+1+m+3+1]-k7[i+1+1])/(m+3))* (((k8[i+1+m+4+1]-k8[i+1+1])/(m+4))*(msplinedesign8[j,i+1+1+3])))) + ((k6[i+1+m+2+1]-k6[i+1+1])/(m+2))*((((k7[i+1+m+3+1]-k7[i+1+1])/(m+3))* (((k8[i+1+m+4+1]-k8[i+1+1])/(m+4))*(msplinedesign8[j,i+1+1+3]))))))
        
        + ((k[i+1+m+1+1]-k[i+1+1])/(m+1))*((((k6[i+1+m+2+1]-k6[i+1+1])/(m+2))*((((k7[i+1+m+3+1]-k7[i+1+1])/(m+3))* (((k8[i+1+m+4+1]-k8[i+1+1])/(m+4))*(msplinedesign8[j,i+1+1+3]))))))
        
      } else if (x[j]>k[i+2+1] && x[j] <= k[i+3+1]){
        # resu[j,i] <- (k[i+m+1]-k[i])*(msplinedesign5[j,i])/(m+1) + (k[i+1+m+1]-k[i+1])*(msplinedesign5[j,i+1])/(m+1) + (k[i+2+m+1]-k[i+2])*(msplinedesign5[j,i+2])/(m+1)
        # resu[j,i] <- ((k[i+m+1]-k[i])/(m+1))*((k6[i+m+2]-k6[i])/(m+2))*((k7[i+m+3]-k7[i])/(m+3))*((k8[i+m+4]-k8[i])/(m+4))*(msplinedesign8[j,i]) + ((k[i+1+m+1]-k[i+1])/(m+1))*((k6[i+1+m+2]-k6[i+1])/(m+2))*((k7[i+1+m+3]-k7[i+1])/(m+3))*((k8[i+1+m+4]-k8[i+1])/(m+4))*(msplinedesign8[j,i+1])+ ((k[i+2+m+1]-k[i+2])/(m+1))*((k6[i+2+m+2]-k6[i+2])/(m+2))*((k7[i+2+m+3]-k7[i+2])/(m+3))*((k8[i+2+m+4]-k8[i+2])/(m+4))*(msplinedesign8[j,i+2])
        
        resu[j,i] <- ((k[i+m+1+1]-k[i+1])/(m+1))*(( ((k6[i+m+2+1]-k6[i+1])/(m+2))*( (   ((k7[i+m+3+1]-k7[i+1])/(m+3))*((((k8[i+m+4+1]-k8[i+1])/(m+4))*(msplinedesign8[j,i+1+3])) + (((k8[i+1+m+4+1]-k8[i+1+1])/(m+4))*(msplinedesign8[j,i+1+1+3])) + (((k8[i+2+m+4+1]-k8[i+2+1])/(m+4))*(msplinedesign8[j,i+2+1+3])) )
                                                                                        
                                                                                        
                                                                                        +((k7[i+1+m+3+1]-k7[i+1+1])/(m+3))*( (((k8[i+1+m+4+1]-k8[i+1+1])/(m+4))*(msplinedesign8[j,i+1+1])) + (((k8[i+2+m+4+1]-k8[i+2+1])/(m+4))*(msplinedesign8[j,i+2+1+3])) )
                                                                                        
                                                                                        
                                                                                        + ((k7[i+2+m+3+1]-k7[i+2+1])/(m+3))*((((k8[i+2+m+4+1]-k8[i+2+1])/(m+4))*(msplinedesign8[j,i+2+1+3])) )  ))
                                                    
                                                    
                                                    
                                                    + ((k6[i+1+m+2+1]-k6[i+1+1])/(m+2))*( ( ((k7[i+1+m+3+1]-k7[i+1+1])/(m+3))*( (((k8[i+1+m+4+1]-k8[i+1+1])/(m+4))*(msplinedesign8[j,i+1+1+3])) + (((k8[i+2+m+4+1]-k8[i+2+1])/(m+4))*(msplinedesign8[j,i+2+1+3])) )
                                                                                            
                                                                                            
                                                                                            + ((k7[i+2+m+3+1]-k7[i+2+1])/(m+3))*((((k8[i+2+m+4+1]-k8[i+2+1])/(m+4))*(msplinedesign8[j,i+2+1+3])) )  ))
                                                    
                                                    
                                                    
                                                    + ((k6[i+2+m+2+1]-k6[i+2+1])/(m+2)))*((((k7[i+2+m+3+1]-k7[i+2+1])/(m+3))*((((k8[i+2+m+4+1]-k8[i+2+1])/(m+4))*(msplinedesign8[j,i+2+1+3])) )  )))
        
        + ((k[i+1+m+1+1]-k[i+1+1])/(m+1))*((((k6[i+1+m+2+1]-k6[i+1+1])/(m+2))*( ( ((k7[i+1+m+3+1]-k7[i+1+1])/(m+3))*( (((k8[i+1+m+4+1]-k8[i+1+1])/(m+4))*(msplinedesign8[j,i+1+1+3])) + (((k8[i+2+m+4+1]-k8[i+2+1])/(m+4))*(msplinedesign8[j,i+2+1+3])) )
                                                                                  
                                                                                  
                                                                                  + ((k7[i+2+m+3+1]-k7[i+2+1])/(m+3))*((((k8[i+2+m+4+1]-k8[i+2+1])/(m+4))*(msplinedesign8[j,i+2+1+3])) )  ))
                                            
                                            
                                            
                                            + ((k6[i+2+m+2+1]-k6[i+2+1])/(m+2)))*((((k7[i+2+m+3+1]-k7[i+2+1])/(m+3))*((((k8[i+2+m+4+1]-k8[i+2+1])/(m+4))*(msplinedesign8[j,i+2+1+3])) )  )))
        
        
        
        
        + ((k[i+2+m+1+1]-k[i+2+1])/(m+1))*((((k6[i+2+m+2+1]-k6[i+2+1])/(m+2)))*((((k7[i+2+m+3+1]-k7[i+2+1])/(m+3))*((((k8[i+2+m+4+1]-k8[i+2+1])/(m+4))*(msplinedesign8[j,i+2+1+3])) )  )))
        
      } else
        #if (x>=k[i+3] && x < k[i+4])
      {
        # resu[j,i] <- (k[i+m+1]-k[i])*(msplinedesign5[j,i])/(m+1) + (k[i+1+m+1]-k[i+1])*(msplinedesign5[j,i+1])/(m+1) + (k[i+2+m+1]-k[i+2])*(msplinedesign5[j,i+2])/(m+1) + (k[i+3+m+1]-k[i+3])*(msplinedesign5[j,i+3])/(m+1)
        #resu[j,i] <- ((k[i+m+1]-k[i])/(m+1))*((k6[i+m+2]-k6[i])/(m+2))*((k7[i+m+3]-k7[i])/(m+3))*((k8[i+m+4]-k8[i])/(m+4))*(msplinedesign8[j,i]) + ((k[i+1+m+1]-k[i+1])/(m+1))*((k6[i+1+m+2]-k6[i+1])/(m+2))*((k7[i+1+m+3]-k7[i+1])/(m+3))*((k8[i+1+m+4]-k8[i+1])/(m+4))*(msplinedesign8[j,i+1])+ ((k[i+2+m+1]-k[i+2])/(m+1))*((k6[i+2+m+2]-k6[i+2])/(m+2))*((k7[i+2+m+3]-k7[i+2])/(m+3))*((k8[i+2+m+4]-k8[i+2])/(m+4))*(msplinedesign8[j,i+2]) + ((k[i+3+m+1]-k[i+3])/(m+1))*((k6[i+3+m+2]-k6[i+3])/(m+2))*((k7[i+3+m+3]-k7[i+3])/(m+3))*((k8[i+3+m+4]-k8[i+3])/(m+4))*(msplinedesign8[j,i+3])
        
        resu[j,i] <-   ((k[i+m+1+1]-k[i+1])/(m+1))*( (  ((k6[i+m+2+1]-k6[i+1])/(m+2))*( (  ((k7[i+m+3+1]-k7[i+1])/(m+3))*( (((k8[i+m+4+1]-k8[i+1])/(m+4))*(msplinedesign8[j,i+1+3])) + (((k8[i+1+m+4+1]-k8[i+1+1])/(m+4))*(msplinedesign8[j,i+1+1+3])) + (((k8[i+2+m+4+1]-k8[i+2+1])/(m+4))*(msplinedesign8[j,i+2+1+3])) + (((k8[i+3+m+4+1]-k8[i+3+1])/(m+4))*(msplinedesign8[j,i+3+1+3])))
                                                                                           
                                                                                           + ((k7[i+1+m+3+1]-k7[i+1+1])/(m+3))*((((k8[i+1+m+4+1]-k8[i+1+1])/(m+4))*(msplinedesign8[j,i+1+1+3])) + (((k8[i+2+m+4+1]-k8[i+2+1])/(m+4))*(msplinedesign8[j,i+2+1+3])) + (((k8[i+3+m+4+1]-k8[i+3+1])/(m+4))*(msplinedesign8[j,i+3+1+3])))
                                                                                           
                                                                                           + ((k7[i+2+m+3+1]-k7[i+2+1])/(m+3))*((((k8[i+2+m+4+1]-k8[i+2+1])/(m+4))*(msplinedesign8[j,i+2+1+3])) + (((k8[i+3+m+4+1]-k8[i+3+1])/(m+4))*(msplinedesign8[j,i+3+1+3])))
                                                                                           
                                                                                           + ((k7[i+3+m+3+1]-k7[i+3+1])/(m+3))*((((k8[i+3+m+4+1]-k8[i+3+1])/(m+4))*(msplinedesign8[j,i+3+1+3])))    )
        )
        
        + ((k6[i+1+m+2+1]-k6[i+1+1])/(m+2))*( ( ((k7[i+1+m+3+1]-k7[i+1+1])/(m+3))*((((k8[i+1+m+4+1]-k8[i+1+1])/(m+4))*(msplinedesign8[j,i+1+1+3])) + (((k8[i+2+m+4+1]-k8[i+2+1])/(m+4))*(msplinedesign8[j,i+2+1+3])) + (((k8[i+3+m+4+1]-k8[i+3+1])/(m+4))*(msplinedesign8[j,i+3+1+3])))
                                                
                                                + ((k7[i+2+m+3+1]-k7[i+2+1])/(m+3))*((((k8[i+2+m+4+1]-k8[i+2+1])/(m+4))*(msplinedesign8[j,i+2+1+3])) + (((k8[i+3+m+4+1]-k8[i+3+1])/(m+4))*(msplinedesign8[j,i+3+1+3])))
                                                
                                                + ((k7[i+3+m+3+1]-k7[i+3+1])/(m+3))*((((k8[i+3+m+4+1]-k8[i+3+1])/(m+4))*(msplinedesign8[j,i+3+1+3])))    )
        )
        
        
        + ((k6[i+2+m+2+1]-k6[i+2+1])/(m+2))*( ( ((k7[i+2+m+3+1]-k7[i+2+1])/(m+3))*((((k8[i+2+m+4+1]-k8[i+2+1])/(m+4))*(msplinedesign8[j,i+2+1+3])) + (((k8[i+3+m+4+1]-k8[i+3+1])/(m+4))*(msplinedesign8[j,i+3+1+3])))
                                                
                                                + ((k7[i+3+m+3+1]-k7[i+3+1])/(m+3))*((((k8[i+3+m+4+1]-k8[i+3+1])/(m+4))*(msplinedesign8[j,i+3+1+3])))    )
        )
        
        + ((k6[i+3+m+2+1]-k6[i+3+1])/(m+2))*( (((k7[i+3+m+3+1]-k7[i+3+1])/(m+3))*((((k8[i+3+m+4+1]-k8[i+3+1])/(m+4))*(msplinedesign8[j,i+3+1+3]))))))
        )
        
        
        + ((k[i+1+m+1+1]-k[i+1+1])/(m+1))*( (((k6[i+1+m+2+1]-k6[i+1+1])/(m+2))*( ( ((k7[i+1+m+3+1]-k7[i+1+1])/(m+3))*((((k8[i+1+m+4+1]-k8[i+1+1])/(m+4))*(msplinedesign8[j,i+1+1+3])) + (((k8[i+2+m+4+1]-k8[i+2+1])/(m+4))*(msplinedesign8[j,i+2+1+3])) + (((k8[i+3+m+4+1]-k8[i+3+1])/(m+4))*(msplinedesign8[j,i+3+1+3])))
                                                                                   
                                                                                   + ((k7[i+2+m+3+1]-k7[i+2+1])/(m+3))*((((k8[i+2+m+4+1]-k8[i+2+1])/(m+4))*(msplinedesign8[j,i+2+1+3])) + (((k8[i+3+m+4+1]-k8[i+3+1])/(m+4))*(msplinedesign8[j,i+3+1+3])))
                                                                                   
                                                                                   + ((k7[i+3+m+3+1]-k7[i+3+1])/(m+3))*((((k8[i+3+m+4+1]-k8[i+3+1])/(m+4))*(msplinedesign8[j,i+3+1+3])))    )
        )
        
        
        + ((k6[i+2+m+2+1]-k6[i+2+1])/(m+2))*( ( ((k7[i+2+m+3+1]-k7[i+2+1])/(m+3))*((((k8[i+2+m+4+1]-k8[i+2+1])/(m+4))*(msplinedesign8[j,i+2+1+3])) + (((k8[i+3+m+4+1]-k8[i+3+1])/(m+4))*(msplinedesign8[j,i+3+1+3])))
                                                
                                                + ((k7[i+3+m+3+1]-k7[i+3+1])/(m+3))*((((k8[i+3+m+4+1]-k8[i+3+1])/(m+4))*(msplinedesign8[j,i+3+1+3])))    )
        )
        
        
        + ((k6[i+3+m+2+1]-k6[i+3+1])/(m+2))*( (((k7[i+3+m+3+1]-k7[i+3+1])/(m+3))*((((k8[i+3+m+4+1]-k8[i+3+1])/(m+4))*(msplinedesign8[j,i+3+1+3]))))))
        )
        
        
        
        
        + ((k[i+2+m+1+1]-k[i+2+1])/(m+1))*( ( ((k6[i+2+m+2+1]-k6[i+2+1])/(m+2))*( ( ((k7[i+2+m+3+1]-k7[i+2+1])/(m+3))*((((k8[i+2+m+4+1]-k8[i+2+1])/(m+4))*(msplinedesign8[j,i+2+1+3])) + (((k8[i+3+m+4+1]-k8[i+3+1])/(m+4))*(msplinedesign8[j,i+3+1+3])))
                                                                                    
                                                                                    + ((k7[i+3+m+3+1]-k7[i+3+1])/(m+3))*((((k8[i+3+m+4+1]-k8[i+3+1])/(m+4))*(msplinedesign8[j,i+3+1+3])))    )
        )
        
        
        + ((k6[i+3+m+2+1]-k6[i+3+1])/(m+2))*( (((k7[i+3+m+3+1]-k7[i+3+1])/(m+3))*((((k8[i+3+m+4+1]-k8[i+3+1])/(m+4))*(msplinedesign8[j,i+3+1+3]))))))
        )
        
        + ((k[i+3+m+1+1]-k[i+3+1])/(m+1))*( (  ((k6[i+3+m+2+1]-k6[i+3+1])/(m+2))*( (((k7[i+3+m+3+1]-k7[i+3+1])/(m+3))*((((k8[i+3+m+4+1]-k8[i+3+1])/(m+4))*(msplinedesign8[j,i+3+1+3])))))))
        
      }
  return(resu[,1:(d-1)])
}
