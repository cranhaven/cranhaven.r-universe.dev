symarma.sim <-
function(model, n, family="Normal", index1, index2, varphi=1)
{ 
   if (!is.list(model)) 
      model = list()
   if(family=="Normal" || family=="Student" || family=="Gstudent" || family=="ExpPower" ||
      family=="LogisticII" || family=="Cauchy") {}
   else stop(paste("family not implemented"))
   
   if(family=="Normal")  
   {  resp <- rnorm(n,0,1)
      scalevar <- 1   
   }
   if(family=="Student")  
   {  if(is.null(index1)=="TRUE")
             stop(paste("no degrees of freedom argument"))
        else if(index1<0)
             stop(paste("allowed values for degrees of freedom positive"))
        else
        {    resp <- rt(n=n,df=index1)
             scalevar <- index1/(index1-2)
        }        
   }
   if(family=="Gstudent") 
   {  if(is.null(index1)=="TRUE" || is.null(index2)=="TRUE")
          stop(paste("no index1 or index2 argument"))
      else if(index1<=0 || index2<=0)
          stop(paste("index1 and index2 must be positive"))
      else
      {  z <- rnorm(n,0,1)
         T <- 1/rgamma(n, shape = index1/2, rate = index2/2)
         resp <- T^(-0.5)*z
         scalevar <- index2/(index1-2)
      }
   }
   if(family=="ExpPower") 
   {  if(is.null(index1)=="TRUE")
          stop(paste("no index1 argument"))
      else if(index1<=-1 || index1>1)
          stop(paste("index1 must be in (-1,1]"))
      else
      {  v <- runif(n,-1,1)
         w <- rgamma(n,shape=(1+(1+index1)/2))
         resp <- (2*w)^((1+index1)/2)*v                           
         scalevar <- 2^(1+index1)*(gamma(1.5*(index1+1))/(gamma((index1+1)/2)))
      }
   }
   if(family=="LogisticII") 
   {  v <- runif(n,0,1)
      resp <- log(v/(1-v))
      scalevar <- pi^2/3
   }      
   if(family=="Cauchy") 
   {   v <- rnorm(n,0,1)
       w <- rnorm(n,0,1)
       resp <- v/w
       scalevar <- 1 #non exist
   }

   serie <- arima.sim(n=n,model=model, rand.gen = function(n, ...) sqrt(scalevar*varphi)*resp)         
   return(ts(serie))
}
