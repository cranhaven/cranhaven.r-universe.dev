elliptical.ts <-
function(Y, family="Normal", order=c(0,0,0), xreg=NULL, include.mean=TRUE, 
epsilon=0.0001, maxit=100, trace="TRUE", index1=NULL, index2=NULL, fixed=NULL)
{   
   if (NCOL(Y) > 1) 
       stop("only implemented for univariate time series")
   if (!is.numeric(Y)) 
       stop("'Y' must be numeric")
   
   if (order[2] > 0) 
       include.mean=FALSE 
  
   if(family=="Normal" || family=="Student" || family=="Gstudent" || family=="ExpPower" ||
      family=="LogisticI" || family=="LogisticII" || family=="Glogistic" || family=="Cauchy" || 
      family=="Cnormal") {}
   else stop(paste("family not implemented"))
   
   if (!is.numeric(order) || length(order) != 3 || any(order<0)) 
      stop("'order' must be a non-negative numeric vector of length 3")

   np = order[1]
   nq = order[3]
   m = max(np,nq) 
   d = order[2]
   Y = as.ts(Y) 
   n = length(Y)
  
   if(is.null(xreg)==TRUE) ncolxreg <- 0L
   if(is.null(xreg)==FALSE)
   {   namexreg <- deparse(substitute(xreg))
       if(NROW(xreg) != n) 
           stop("lengths of 'Y' and 'xreg' do not match")
       ncolxreg <- NCOL(xreg)
       xreg <- as.matrix(xreg)
       storage.mode(xreg) <- "double"
   }

   class(xreg) <- NULL
   if (ncolxreg > 0L) 
   {   if(ncolxreg == 1L) colnames(xreg) <- namexreg
       else colnames(xreg) <- paste0(namexreg, 1L:ncolxreg)
   }
   if(include.mean==TRUE)
   {   xreg <- cbind(intercept = rep(1,n), xreg = xreg)
       ncolxreg <- ncolxreg + 1L
   }

   if(is.null(fixed)==FALSE)
   {     if(include.mean==TRUE)
         {  if (length(fixed) != np+nq+ncolxreg) stop("wrong length for 'fixed'")
            if(is.na(fixed[np+nq+1])==FALSE) stop("model include a intercept, wrong argument for 'fixed'")
            if(is.null(xreg)==FALSE && sum(is.na(fixed[(np+nq+1):(np+nq+ncolxreg)])==FALSE)>0)
                  stop("model include external regressors, wrong argument for 'fixed'")
         } 
         if(include.mean==FALSE)
         {  if (length(fixed) != np+nq+ncolxreg) stop("wrong length for 'fixed'")
            if(is.null(xreg)==FALSE && sum(is.na(fixed[(np+nq+1):(np+nq+ncolxreg)])==FALSE)>0)
                  stop("model include external regressors, wrong argument for 'fixed'")
         } 
   } 

   if (all(order==0) && ncolxreg==0)
      stop("no specified parameter")

   if (order[2] > 0) 
   {  Y <- diff(Y, 1, order[2])
      if(!is.null(xreg)==TRUE) xreg <- diff(xreg, 1, order[2])
   }

   Y = as.ts(Y) 
   n = length(Y)

   # Initialize values

   beta = NULL
   phi = matrix(0,nrow=np,ncol=1)   
   theta = matrix(0,nrow=nq,ncol=1)
   aux.model = arima(Y,order=c(np,0,nq),xreg=xreg,include.mean=F,fixed=fixed,transform.pars = FALSE,method=c("CSS")) 
   if(ncolxreg!=0) beta <- aux.model$coef[(np+nq+1):(np+nq+ncolxreg)]
   if(np!=0) phi = aux.model$coef[1:np]
   if(nq!=0) theta = aux.model$coef[(1+np):(np+nq)]

   delta <- c(beta,phi,theta)
   dispersao <- aux.model$sigma2
   if(ncolxreg!=0)    XB = xreg%*%as.vector(beta)
   else  XB=seq(0,0,length=n)
   residuos = aux.model$residuals
   z <- residuos[(m+1):n]/sqrt(dispersao)

   # Initialize arguments

   A = matrix(0,nrow=(n-m),ncol=np)
   B = matrix(0,nrow=(n-m),ncol=nq)
   C = matrix(0,nrow=(n-m),ncol=ncolxreg)
   aux = matrix(0,nrow=(n-m),ncol=np)
   x = matrix(0,nrow=(n-m),ncol=ncolxreg)
 
   if(np!=0)  for(i in 1:np)  A[,i] = Y[(m+1-i):(n-i)] - XB[(m+1-i):(n-i)]
   if(nq!=0)  for(i in 1:nq)  B[,i] = residuos[(m+1-i):(n-i)]  
   
   if(ncolxreg!=0)
   {  for(i in 1:ncolxreg)  
      {   if(np!=0)
         {  for(j in 1:np) aux[,j] = xreg[(m+1-j):(n-j),i]
            C[,i] = xreg[(m+1):n,i] - aux%*%phi
         }
         if(np==0) C[,i] = xreg[(m+1):n,i]
      }
   }
  
   if(is.null(fixed)=="FALSE")
   {  if(np!=0 && nq==0)
      {  for(i in 1:np)
         {  if(is.na(fixed[i])=="FALSE") 
              A[,i]=0
         }
      }
      if(np==0 && nq!=0)
      {  for(i in 1:nq)
         {  if(is.na(fixed[i])=="FALSE") 
              B[,i]=0
         }
      }
      if(np!=0 && nq!=0)
      {  for(i in 1:np)
         {  if(is.na(fixed[i])=="FALSE") 
              A[,i]=0
         }
         for(i in (np+1):(np+nq))
         {  if(is.na(fixed[i])=="FALSE") 
              B[,(i-np)]=0
         }
      }
   }

   O = cbind(C,A,B)
   start = delta
   
   if(is.null(fixed)=="FALSE")
   {   O.new = matrix(0,nrow=(n-m),ncol=table(is.na(fixed))["TRUE"])
       j=1
       for(i in 1:ncol(O))
       {  if(sum(O[,i])!=0)
          {  O.new[,j] = O[,i]
             j=j+1
          }
       }
       delta.new = matrix(0,nrow=table(is.na(fixed))["TRUE"],ncol=1)
       j=1
       for(i in 1:length(delta))
       {  if(delta[i]!=0)
          {  delta.new[j] = delta[i]
             j=j+1
          }
       }
   O = O.new
   start = delta.new
   }

   if(ncolxreg!=0)  x = as.matrix(xreg[(m+1):n,])%*%as.vector(beta)
   else x = seq(0,0,length=(n-m))
   MU = x + A%*%phi + B%*%theta
   Y.=Y[(m+1):n]
  
   # Iterative process
 
   iter <- 1
   error1 <- error2 <- 0
   repeat 
   {
      if(family=="Normal")
      { g0 <- log(1/sqrt(2*pi)*exp(-0.5*z^2))
        w.1 <- rep(-0.5,length(MU))
        w.2 <- rep(0,length(z))
        dg <- 1/4
        fg <- 3/4  
        scalevar <- 1
      }
      if(family=="Student")
      { if(is.null(index1)=="TRUE")
             stop(paste("no degrees of freedom argument"))
        else if(index1<0)
             stop(paste("allowed values for degrees of freedom positive"))
        else{
        g0 <- log(index1^(index1/2)*gamma(0.5+index1/2)/(gamma(0.5)*gamma(index1/2))*(index1+z^2)^(-0.5*(index1+1)))
        w.1 <- (index1+1)/(-2*(index1+z^2))
        w.2 <- (index1+1)/(2*(index1+z^2)^2)
        dg <- (index1+1)/(4*(index1+3))
        fg <- (3*(index1+1))/(4*(index1+3))
        scalevar <- index1/(index1-2) }
      }   
      if(family=="Gstudent")
      { if(is.null(index1)=="TRUE" || is.null(index2)=="TRUE")
             stop(paste("no index1 or index2 argument"))
        else if(index1<=0 || index2<=0)
             stop(paste("index1 and index2 must be positive"))
        else{
        g0 <- log((index2^(index1/2)*gamma(0.5+index1/2))/(gamma(0.5)*gamma(index1/2))*(index2+z^2)^(-0.5*(index1+1)))
        w.1 <- (index1+1)/(-2*(index2+z^2))
        w.2 <- (index1+1)/(2*(index2+z^2)^2)
        dg <- (index1*(index1+1))/(4*index2*(index1+3))
        fg <- (3*(index1+1))/(4*(index1+3))
        scalevar <- index2/(index1-2) }
      }
      if(family=="ExpPower")
      { if(is.null(index1)=="TRUE")
             stop(paste("no index1 argument"))
        else if(index1<=-1 || index1>1)
             stop(paste("index1 must be in (-1,1]"))
        else{
        g0 = log(1/(gamma(1+((1+index1)/2))*2^(1+(1+index1)/2))*exp(-0.5*(abs(z)^(2/(1+index1)))))
	  w.1 = 1/(-2*(1+index1)*(z^2)^(index1/(1+index1))) 
        w.2 = index1/(2*(z^2)^((2*index1+1)/(1+index1))*((1+index1)^2))
	  dg = (gamma((3-index1)/2))/(4*(2^(index1-1)*(1+index1)^2*gamma((index1+1)/2)))
	  fg = (index1+3)/(4*(index1+1)) 
        scalevar <-  2^(1+index1)*(gamma(1.5*(index1+1))/(gamma((index1+1)/2))) }
      }
      if(family=="LogisticI")
      { g0 = log(1.4843300029*exp(-z^2)/(1+exp(-z^2))^2)
	  w.1 = -tanh(z^2/2)
        w.2 = -0.5+0.5*tanh(0.5*z^2)^2
	  dg = 1.477240176/4
	  fg = 4.013783934/4
        scalevar <- 0.79569
      }
      if(family=="LogisticII")
      { g0 = log(exp(z)/(1+exp(z))^2)
        w.1 = (exp(z)-1)/(-2*(z*(1+exp(z))))
        w.2 = (2*exp(z)*z-exp(2*z)+1)/(4*z^3*(1+exp(z)^2))
        dg = 1/12
	  fg = 2.42996/4
        scalevar <- pi^2/3
      }
      if(family=="Glogistic")
      { if(is.null(index1)=="TRUE" || is.null(index2)=="TRUE")
             stop(paste("no index1 or index2 argument"))
        else if(index1<=0 || index2<=0)
             stop(paste("index1 and index2=index2(index1) must be positive"))
        else{
        g0 = log((index2*gamma(index1+index1))/(gamma(index1)*gamma(index1))*
	       (exp(index2*z)/(1+exp(index2*z))^2)^index1)
        w.1 = index2*index1*(exp(index2*z)-1)/(-2*(z*(1+exp(index2*z))))
        w.2 = index2*index1*(2*index2*exp(index2*z)*z-exp(2*index2*z)+1)/(4*z^3*(1+exp(index2*z)^2))
	  dg = (index2^2*index1^2)/(4*(2*index1+1))
	  fg = (2*m)*(2+index1^2*trigamma(index1))/(4*(2*index1+1))
        scalevar <- 2*trigamma(index1)}
      }
      if(family=="Cauchy")
      { g0 <- log((1/pi)*(1+z^2)^(-1))
        w.1 <- -1/(1+z^2)
        w.2 <- 1/((1+z^2)^2)
        dg <- 1/8
        fg <- 3/8  
        scalevar <- 1 # non exist
      }
      if(family=="Cnormal")
      { if(is.null(index1)=="TRUE" || is.null(index2)=="TRUE")
             stop(paste("no index1 or index2 argument"))
        else if(index1<0 || index1>1 || index2<=0)
             stop(paste("index1 must be in [0,1] and index2 must be positive"))
        else{
        g0 <- log((1-index1)*1/(sqrt(2*pi))*exp(-0.5*z^2)+
	      index1*1/(sqrt(2*pi)*index2)*exp(-0.5*z^2/index2^2))
	  w.1 <- ((1-index1)*exp(-z^2/2)+(index1*(index2^2)^(-1.5)*exp(-z^2/(2*index2^2))))/
	        ((-2)*((1-index1)*exp(-z^2/2)+(index1*(index2^2)^(-0.5)*exp(-z^2/(2*index2^2)))))
        w.2 <- NULL
	  dg <- NULL
	  fg <- NULL
        scalevar <- 1+index1*(index2^2-1)}
      }
  
      D <- diag(as.vector(-2*w.1))
      z_delta = O%*%start + 1/(4*dg)*D%*%(Y.-MU)       
      new.start <- solve(t(O)%*%O)%*%t(O)%*%z_delta
      
 	error1 <- max(abs((new.start-start)/start))
	start <- new.start

      if(is.null(fixed)=="FALSE")
      {   new.start2 = matrix(0,nrow=(ncolxreg+np+nq),ncol=1)
          fixed.new = NULL
          if(ncolxreg!=0) fixed.new[1:ncolxreg] = fixed[(np+nq+1):(np+nq+ncolxreg)]
          fixed.new[(ncolxreg+1):(np+nq+ncolxreg)] = fixed[1:(np+nq)]        

          j=1
          for(i in 1:(ncolxreg+np+nq))
          {  if(is.na(fixed.new[i])=="TRUE") 
             {  new.start2[i] = new.start[j]
                j=j+1
             }
          }
          new.start = new.start2
      }   

      beta <- new.start[1:ncolxreg]
      if(np!=0) phi <- new.start[(ncolxreg+1):(ncolxreg+np)]
      if(nq!=0) theta <- new.start[(ncolxreg+np+1):(ncolxreg+np+nq)]
      if(ncolxreg!=0)    XB = xreg%*%as.vector(beta)
      else  XB=seq(0,0,length=n)
    
      # New residuals
   
      if(np!=0 && nq==0)
      {  residuos = NULL  # original scale of measurement   
         new.A = matrix(0,nrow=(n-m),ncol=np)
         for(i in 1:np)
         { if(ncolxreg!=0)  new.A[1:(n-np),i] = Y[(np+1-i):(n-m+np-i)] - as.matrix(xreg[(np+1-i):(n-m+np-i),])%*%as.vector(beta)
           if(ncolxreg==0)  new.A[1:(n-np),i] = Y[(np+1-i):(n-m+np-i)]
         }
         if(ncolxreg!=0)  residuos = Y[(m+1):n] - as.matrix(xreg[(m+1):n,])%*%as.vector(beta) - new.A%*%phi
         if(ncolxreg==0)  residuos = Y[(m+1):n] - new.A%*%phi
      }
      if(np==0 && nq!=0)
      {  residuos = NULL  # original scale of measurement 
         new.B = matrix(0,nrow=n,ncol=nq)
         for(i in 1:n)
         {  if(ncolxreg!=0) residuos[i] = Y[i] - xreg[i,]%*%as.vector(beta) - new.B[i,]%*%as.vector(theta)
            if(ncolxreg==0) residuos[i] = Y[i] - new.B[i,]%*%as.vector(theta)
            for(j in 1:nq)
                if(j<=i && i<n)  new.B[(i+1),j] = residuos[i+1-j]
         }     
      }
      if(np!=0 && nq!=0)
      {  residuos = NULL  # original scale of measurement 
         new.A = matrix(0,nrow=n,ncol=np)
         new.B = matrix(0,nrow=n,ncol=nq)
         if(ncolxreg!=0)
         {  for(i in 1:n)
            {  residuos[i] = Y[i] - xreg[i,]%*%as.vector(beta) - new.A[i,]%*%as.vector(phi) - new.B[i,]%*%as.vector(theta)
               for(j in 1:np)
                   if(j<=i && i<n)  new.A[(i+1),j] = Y[i+1-j] - xreg[i+1-j,]%*%as.vector(beta)
               for(j in 1:nq)
                   if(j<=i && i<n)  new.B[(i+1),j] = residuos[i+1-j]
             }
         }
         if(ncolxreg==0)
         {  for(i in 1:n)
            {  residuos[i] = Y[i] - new.A[i,]%*%as.vector(phi) - new.B[i,]%*%as.vector(theta)
               for(j in 1:np)
                   if(j<=i && i<n)  new.A[(i+1),j] = Y[i+1-j]
               for(j in 1:nq)
                   if(j<=i && i<n)  new.B[(i+1),j] = residuos[i+1-j]
             }
         }
      }
      if(np==0 && nq==0)
      {  residuos = NULL  # original scale of measurement 
         residuos = Y[1:n] - as.matrix(xreg[1:n,])%*%as.vector(beta)
      }

      # New arguments
 
      if(np!=0)  for(i in 1:np)  A[,i] = Y[(m+1-i):(n-i)] - XB[(m+1-i):(n-i)]
      if(nq!=0)  for(i in 1:nq)  B[,i] = residuos[(m+1-i):(n-i)]  

      if(ncolxreg!=0)
      {  for(i in 1:ncolxreg)  
         {   if(np!=0)
             {   for(j in 1:np)  aux[,j] = xreg[(m+1-j):(n-j),i]
                 C[,i] = xreg[(m+1):n,i] - aux%*%phi
             }
            if(np==0)  C[,i] = xreg[(m+1):n,i]
         }
      }
   
      if(is.null(fixed)=="FALSE")
      {  if(np!=0 && nq==0)
         {  for(i in 1:np)
            {  if(is.na(fixed[i])=="FALSE") 
                 A[,i]=0
            }
         }
         if(np==0 && nq!=0)
         {  for(i in 1:nq)
            {  if(is.na(fixed[i])=="FALSE") 
                 B[,i]=0
            }
         }
         if(np!=0 && nq!=0)
         {  for(i in 1:np)
            {  if(is.na(fixed[i])=="FALSE") 
                 A[,i]=0
            }
            for(i in (np+1):(np+nq))
            {  if(is.na(fixed[i])=="FALSE") 
                 B[,(i-np)]=0
            }
         }
      }

      O = cbind(C,A,B)
   
      if(is.null(fixed)=="FALSE")
      {   O.new = matrix(0,nrow=(n-m),ncol=table(is.na(fixed))["TRUE"])
          j=1
          for(i in 1:ncol(O))
          {  if(sum(O[,i])!=0)
             {  O.new[,j] = O[,i]
                j=j+1
             }
          }
          O = O.new
      }

      if(ncolxreg!=0) x = as.matrix(xreg[(m+1):n,])%*%as.vector(beta)
      else x = seq(0,0,length=(n-m))
      MU = x + A%*%phi + B%*%theta
      Y.=Y[(m+1):n]
     
      new.dispersao <- 1/(n-m)*t(Y.-MU)%*%D%*%(Y.-MU)

      error2 <- abs((new.dispersao-dispersao)/dispersao)
      dispersao <- c(new.dispersao)
      if(np!=0 && nq==0)  z <- residuos/sqrt(dispersao)
      if(np!=0 && nq!=0)  z <- residuos[(m+1):n]/sqrt(dispersao)
      if(np==0 && nq!=0)  z <- residuos[(m+1):n]/sqrt(dispersao)
      if(np==0 && nq==0)  z <- residuos[(m+1):n]/sqrt(dispersao)
      
      if((iter == maxit) || (max(error1, error2, na.rm = TRUE) < epsilon))
 	     break

      iter <- iter + 1
   }

   if(maxit>1 && iter==maxit)
   {
      warning(paste("\n linear convergence not obtained in", maxit,
                    "iterations"))
   }
     
   desviopadrao <- sqrt(diag(solve((4*dg/c(dispersao))*t(O)%*%O)))
   if(is.null(fixed)=="FALSE")
   {   desvio2 = matrix(0,nrow=(ncolxreg+np+nq),ncol=1)
       fixed.new = NULL
       if(ncolxreg!=0) fixed.new[1:ncolxreg] = fixed[(np+nq+1):(np+nq+ncolxreg)]
       fixed.new[(ncolxreg+1):(np+nq+ncolxreg)] = fixed[1:(np+nq)] 
       j=1
       for(i in 1:(ncolxreg+np+nq))
       {  if(is.na(fixed.new[i])=="TRUE") 
          {  desvio2[i] = desviopadrao[j]
             j=j+1
          }
       }
       desviopadrao = desvio2
   }   

   desviopadrao2 <- sqrt(1/((n-m)*(4*fg-1)/(4*c(dispersao)^2)))
  
   # Log -likelihooh
   if(family=="Normal") g0 <- log(1/sqrt(2*pi)*exp(-0.5*z^2))
   if(family=="Student") g0 <- log(index1^(index1/2)*gamma(0.5+index1/2)/(gamma(0.5)*gamma(index1/2))*(index1+z^2)^(-0.5*(index1+1)))
   if(family=="Gstudent") g0 <- log((index2^(index1/2)*gamma(0.5+index1/2))/(gamma(0.5)*gamma(index1/2))*(index2+z^2)^(-0.5*(index1+1)))
   if(family=="ExpPower")  g0 <- log(1/(gamma(1+((1+index1)/2))*2^(1+(1+index1)/2))*exp(-0.5*(abs(z)^(2/(1+index1)))))
   if(family=="LogisticI")  g0 <- log(1.4843300029*exp(-z^2)/(1+exp(-z^2))^2)
   if(family=="LogisticII")  g0 <- log(exp(z)/(1+exp(z))^2)
   if(family=="Glogistic")   g0 <- log((index2*gamma(index1+index1))/(gamma(index1)*gamma(index1))*
                         	       (exp(index2*z)/(1+exp(index2*z))^2)^index1)
   if(family=="Cauchy") g0 <- log((1/pi)*(1+z^2)^(-1))
   if(family=="Cnormal") g0 <- log((1-index1)*1/(sqrt(2*pi))*exp(-0.5*z^2)+
                       	      index1*1/(sqrt(2*pi)*index2)*exp(-0.5*z^2/index2^2))

   loglik <- sum(-0.5*log(dispersao) + g0)
  
   # --------------

   AIC <- -2*loglik + 2*(np+nq+ncolxreg)
   BIC <- -2*loglik + (np+nq+ncolxreg)*log(length(MU))
   RMSE <- sqrt(sum(((Y. - MU)/Y.)^2)/length(Y.))
   new.start <- round(new.start,digits=4)
   desviopadrao <- round(desviopadrao,digits=4)

   if(trace == "TRUE")
   {  
	print(matrix(nrow=2, ncol=0, byrow=TRUE,dimnames = list(c("Call:", paste(c("symarma(",np,",",order[2],",",nq,") - family: ",family),collapse="")))))    
	print(matrix(nrow=1, ncol=0, byrow=TRUE,dimnames = list(c("Coefficients:"))))
	if(np!=0 && nq!=0)
	   print(noquote(matrix(c(new.start,desviopadrao), nrow=np+nq+ncolxreg, ncol=2, byrow=FALSE, dimnames = list(c(colnames(xreg),paste("ar", 1:np, sep = ""),paste("ma", 1:nq, sep = "")), c("Estimate","Std. Error")))))
	if(np!=0 && nq==0)
	   print(noquote(matrix(c(new.start,desviopadrao), nrow=np+nq+ncolxreg, ncol=2, byrow=FALSE, dimnames = list(c(colnames(xreg),paste("ar", 1:np, sep = "")), c("Estimate","Std. Error")))))
	if(np==0 && nq!=0)
	   print(noquote(matrix(c(new.start,desviopadrao), nrow=np+nq+ncolxreg, ncol=2, byrow=FALSE, dimnames = list(c(colnames(xreg),paste("ma", 1:nq, sep = "")), c("Estimate","Std. Error")))))
	if(np==0 && nq==0)
	   print(noquote(matrix(c(new.start,desviopadrao), nrow=np+nq+ncolxreg, ncol=2, byrow=FALSE, dimnames = list(c(colnames(xreg)), c("Estimate","Std. Error")))))

	print(matrix(nrow=5, ncol=0, byrow=TRUE,dimnames =list(c(
        paste(c("Varphi estimated: ",sprintf("%0.4f", dispersao),"  (Std. Error: ",sprintf("%0.4f",desviopadrao2),")"),collapse=""),
        paste(c("Log-likelihood: ",sprintf("%0.2f", loglik)),collapse=""),
        paste(c("RMSE: ",sprintf("%0.2f", RMSE)),collapse=""),
        paste(c("Number of iterations in Fisher scoring optimization: ",iter),collapse=""),
        ""))))
   }

   fit <- list(coefficients=new.start, dispersion=dispersao,resid.raw=(Y.-MU),resid.stand=(Y.-MU)/sqrt(scalevar*c(dispersao)),
               ut=(Y.-MU)/sqrt(c(dispersao)),fitted.values=MU,loglik=loglik,aic=AIC,bic=BIC,rmse=RMSE,Wg=w.1,Wgder=w.2,iter=iter,fun.g=g0,
               scale=(4*dg),scaledispersion=(4*fg-1),scalevariance=scalevar,A=A,B=B,C=C,O=O,n=length(Y),
               sd.coef=desviopadrao,sd.disp=desviopadrao2,family=family,index1=index1,index2=index2,np=np,nq=nq,d=d,
		   fixed=fixed,xreg=xreg,Y=Y,include.mean=include.mean)
}
