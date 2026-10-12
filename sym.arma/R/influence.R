influence <-
function(model, diag="slope", scheme="additive", iter=2000, alpha=0.95, theta=0.05, plot="TRUE")
{   
   if(diag=="slope" || diag=="cook" || diag=="lv") {}
   else stop(paste("diag function assessed for slope, cook or lv"))
   if(scheme=="additive" || scheme=="dispersion") {}
   else stop(paste("perturbation scheme assessed for additive or dispersion"))
   fit = model
   n = fit$n
   X = fit$xreg
   if(is.null(fit$xreg)==TRUE) p <- 0L
   if(is.null(fit$xreg)==FALSE) p <- NCOL(X)

   g0.model = fit$fun.g
   Wg.model = fit$Wg
   Wgder.model = fit$Wgder
   var.model = as.vector(fit$dispersion)
   scalevar.model = fit$scalevar
   hat = fit$coefficients
   v.model <- -2*Wg.model
   erro.model = fit$resid.raw
   uwt.model = fit$ut^2
   A.model <- fit$A
   B.model <- fit$B
   C.model <- fit$C
   dist<- fit$family
   np <- fit$np
   nq <- fit$nq
   d <- fit$d
   m <- max(np,nq)
   index1 <- fit$index1
   index2 <- fit$index2
   fixed <- fit$fixed
   C.model <- fit$C
   O <- cbind(C.model,A.model,B.model)

   ar = ma = NULL
   if(length(hat)==(np+nq))
   {  if(np!=0) ar[1:np] = hat[1:np]
      if(np!=0 && nq!=0) ma[1:nq] = hat[(np+1):(np+nq)]
      if(np==0 && nq!=0) ma[1:nq] = hat[1:nq]
   }
   else
   {  if(np!=0) ar[1:np] = hat[(NCOL(X)+1):(NCOL(X)+np)]
      if(np!=0 && nq!=0) ma[1:nq] = hat[(NCOL(X)+np+1):(NCOL(X)+np+nq)]
      if(np==0 && nq!=0) ma[1:nq] = hat[(NCOL(X)+1):(NCOL(X)+nq)]
   }

#################################################################
# Slope
#################################################################
if(diag=="slope")
{     if(scheme=="additive")
      {  S.exe = -(1/var.model)*v.model*erro.model
	   O.exe = sqrt(sum(S.exe^2))
	   D.exe = 2*S.exe
      }
      if(scheme=="dispersion")
      {  S.exe = -0.5 + 0.5*v.model*uwt.model
	   O.exe = sqrt(sum(S.exe^2))
	   D.exe = 2*S.exe
      }

     ########################
     ## Benchmarks
     ########################

      serie = matrix(0,nrow = n, ncol = iter)
      S = matrix(0,nrow = (n-m), ncol = iter)
 	loop=1 
      repeat
      {  for(j in loop:iter)
         {  if(dist=="LogisticI")
               stop(paste("Function not implemented for Generalized Logistic distribution"))
            if(dist=="Glogistic")
               stop(paste("Function not implemented for Generalized Logistic distribution"))
            if(dist=="Cauchy") 
               stop(paste("Function not implemented for Cauchy distribution"))
            if(dist=="Cnormal")
               stop(paste("Function not implemented for Contamined Normal distribution"))

            serie[,j] <- symarma.sim(model=list(ar=ar,ma=ma),n=n,family=dist,index1,index2,varphi=var.model)           
            fit <- elliptical.ts(serie[,j],order=c(np,d,nq),xreg=X,include.mean=FALSE,index1=index1,index2=index2,family=dist,trace=FALSE,fixed=fixed)

            Wg = fit$Wg
            erro = fit$resid.raw
            varphi.hat = as.vector(fit$dispersion)
            g0 = fit$fun.g
            v.loop = -2*Wg
            uwt = fit$ut^2
            A.loop = fit$A
            B.loop = fit$B

            if(scheme=="additive") S[,j] = -(1/varphi.hat)*v.loop*erro
            if(scheme=="dispersion") S[,j] = -0.5 + 0.5*v.loop*uwt  

            if(j==1) print("Benchmarks - 00% ...")   
            if(j==floor(1*iter/4)) print("Benchmarks - 25% ...")
            if(j==floor(2*iter/4)) print("Benchmarks - 50% ...")
            if(j==floor(2*iter/4)) print("Benchmarks - 75% ...")
            if(j==floor(4*iter/4)) print("Benchmarks - 100%")
        }
        loop = j
        if(loop==iter)  break
      }
	
      # "BS0" statistic
      O = NULL
      for(i in 1:iter)    O[i] = sqrt(sum(S[,i]^2))
      BS0 = quantile(O,  probs = alpha)

      # "BS1" statistic
      D = matrix(nrow = (n-m), ncol = iter)
      for(i in 1:iter)    D[,i] = 2*S[,i]
      qnt = NULL
      for(i in 1:iter) qnt[i] = max(abs(D[,i]))
      BS1 = quantile(qnt,  probs = alpha)

      # "BS2" statistic
      bs2 = NULL
      j=1
      for(i in 1:iter)
      {  if(O[i] > BS0)
         {  bs2[j] = i
            j = j+1
         }
      }
      help = NULL
      for(i in 1:length(bs2))  
      {   k = bs2[i]
          help[i] = max(abs(D[,k]))
      }
      BS2 = quantile(help,  probs = theta)
 
      ## Plot
      ############################################
       if(plot=="TRUE")
      {  dat = abs(D.exe)[,1]
         if(dist=="Normal")  barplot(dat,space=1,xlab="Index",main=expression("SYMARMA" ~~ {Normal}),ylab=substitute(paste("Billor and Loynes")),ylim=c(0,max(BS1+(BS1-BS2),max(dat))))       
         if(dist=="Student")  barplot(dat,space=1,xlab="Index",main=expression("SYMARMA" ~~ {Student-t}),ylab=substitute(paste("Billor and Loynes")),ylim=c(0,max(BS1+(BS1-BS2),max(dat))))
         if(dist=="ExpPower")   barplot(dat,space=1,xlab="Index",main=expression("SYMARMA" ~~ {ExpPower}),ylab=substitute(paste("Billor and Loynes")),ylim=c(0,max(BS1+(BS1-BS2),max(dat))))
         if(dist=="LogisticII")  barplot(dat,space=1,xlab="Index",main=expression("SYMARMA" ~~ {LogisticII}),ylab=substitute(paste("Billor and Loynes")),ylim=c(0,max(BS1+(BS1-BS2),max(dat))))
         if(dist=="Gstudent")   barplot(dat,space=1,xlab="Index",main=expression("SYMARMA" ~~ {Generalized-Student-t}),ylab=substitute(paste("Billor and Loynes")),ylim=c(0,max(BS1+(BS1-BS2),max(dat))))
         abline(h=BS1,lty=1)
         abline(h=BS2,lty=2)
      }

      ## Print
      ############################################
   	 print(matrix(nrow=2, ncol=0, byrow=TRUE,dimnames = list(c("Call:", paste(c("symarma(",np,",",nq,") - family: ",dist),collapse="")))))    
       print(matrix(nrow=1, ncol=0, byrow=TRUE,dimnames = list(c("Billor & Loynes"))))
       print(matrix(nrow=1, ncol=0, byrow=TRUE,dimnames = list(c("Measures of local influence:"))))
       print(matrix(c(O.exe),nrow=1, ncol=1,byrow=TRUE,dimnames = list(c("Slope"))))
       print(matrix(nrow=1, ncol=0, byrow=TRUE,dimnames = list(c("Benchmarks:"))))
       print(matrix(c(BS0,BS1,BS2),nrow=3, ncol=1,byrow=FALSE,dimnames = list(c("BS0","BS1","BS2"))))
     
       Indiv1 = BS1
       Indiv2 = BS2
       VecInd = abs(D.exe)
}

#################################################################
# Curvature
#################################################################
if(diag=="cook" || diag=="lv")
{
      f1 <- 0.5 + 2*Wg.model*uwt.model + Wgder.model*uwt.model^2
      A <- as.vector(-2*(Wg.model + 2*Wgder.model*uwt.model))
      B <- as.vector((Wg.model + Wgder.model*uwt.model)*erro.model)
      C <- as.vector(Wg.model + Wgder.model*uwt.model)

      # Observed Fisher information matrix

      I11 <- -(1/var.model)*t(C.model)%*%diag(A)%*%C.model  # beta x beta
      I12 <-  (2/var.model^2)*t(C.model)%*%B                # beta x varphi 
      I13 <- -(1/var.model)*t(C.model)%*%diag(A)%*%A.model  # beta x phi
      I14 <- -(1/var.model)*t(C.model)%*%diag(A)%*%B.model  # beta x theta
      I22 <-  (1/var.model^2)*sum(f1)                       # varphi
      I23 <-  (2/var.model^2)*t(A.model)%*%B                # varphi x phi
      I24 <-  (2/var.model^2)*t(B.model)%*%B                # varphi x theta
      I33 <- -(1/var.model)*t(A.model)%*%diag(A)%*%A.model  # phi x phi 
      I34 <- -(1/var.model)*t(A.model)%*%diag(A)%*%B.model  # phi x theta
      I44 <- -(1/var.model)*t(B.model)%*%diag(A)%*%B.model  # theta x theta

      I <- matrix(nrow=(np+nq+p+1), ncol=(np+nq+p+1))
      if(p!=0)
      {  I[1:p,1:p] = I11
         I[p+1,p+1] = I22
         I[(1:p),(p+1)] = I12
         I[(p+1),(1:p)] = t(I12)
         if(np!=0 && nq==0)
         {   I[(1:p),(p+2):(p+1+np)] = I13
             I[(p+2):(p+1+np),(1:p)] = t(I13)
             I[(p+1),(p+2):(p+1+np)] = t(I23)
             I[(p+2):(p+1+np),(p+1)] = I23
             I[(p+2):(p+1+np),(p+2):(p+1+np)] = I33
         }    
         if(np==0 && nq!=0)
         {   I[(1:p),(p+2):(p+1+nq)] = I14
             I[(p+2):(p+1+nq),(1:p)] = t(I14)
             I[(p+1),(p+2):(p+1+nq)] = t(I24)
             I[(p+2):(p+1+nq),(p+1)] = I24
             I[(p+2):(p+1+nq),(p+2):(p+1+nq)] = I44
         }
         if(np!=0 && nq!=0)
         {   I[(1:p),(p+2):(p+1+np)] = I13
             I[(p+2):(p+1+np),(1:p)] = t(I13)
             I[(p+1),(p+2):(p+1+np)] = t(I23)
             I[(p+2):(p+1+np),(p+1)] = I23
             I[(p+2):(p+1+np),(p+2):(p+1+np)] = I33
          
             I[(1:p),(p+2+np):(p+1+np+nq)] = I14
             I[(p+2+np):(p+1+np+nq),(1:p)] = t(I14)
             I[(p+1),(p+2+np):(p+1+np+nq)] = t(I24)
             I[(p+2+np):(p+1+np+nq),(p+1)] = I24
             I[(p+2+np):(p+1+np+nq),(p+2+np):(p+1+np+nq)] = I44
         }    
      }
      if(p==0)
      {  I[1:1] = I22
         if(np!=0 && nq==0)
         {   I[1,2:(1+np)] = t(I23)
             I[2:(1+np),1] = I23
             I[2:(1+np),2:(1+np)] = I33
         }    
         if(np==0 && nq!=0)
         {   I[1,2:(1+nq)] = t(I24)
             I[2:(1+nq),1] = I24
             I[2:(1+nq),2:(1+nq)] = I44
         }
         if(np!=0 && nq!=0)
         {   I[1,2:(1+np)] = t(I23)
             I[2:(1+np),1] = I23
             I[2:(1+np),2:(1+np)] = I33
             I[1,(2+np):(1+np+nq)] = t(I24)
             I[(2+np):(1+np+nq),1] = I24
             I[(2+np):(1+np+nq),(2+np):(1+np+nq)] = I44
         }    
      }

      D.aux1=D.aux2=NULL
      if(scheme=="additive") 
      {   D.aux1 <- -(2/var.model^2)*B         # varphi
          D.aux2 <- (1/var.model)*diag(A)%*%O  # delta
          Delta <- matrix(c(D.aux1,D.aux2),ncol=(n-m),byrow=TRUE)
      }
      if(scheme=="dispersion")
      {   D.aux1 <- (1/var.model)*diag(C)%*%uwt.model  # varphi
          D.aux2 <- (2/var.model)*diag(B)%*%O          # delta
          Delta <- matrix(c(D.aux1,D.aux2),ncol=(n-m),byrow=TRUE)
      }

      if(is.null(fixed)=="FALSE")
      {   Delta.new = matrix(0,ncol=(n-m),nrow=(1+table(is.na(fixed))["TRUE"]))
          j=1
          for(i in 1:nrow(Delta))
          {  if(sum(Delta[i,])!=0)
             {  Delta.new[j,] = Delta[i,]
                j=j+1
             }
          }
          I.new = matrix(0,nrow=(1+table(is.na(fixed))["TRUE"]),ncol=(np+nq+1+p))
          j=1
          for(i in 1:nrow(I))
          {  if(sum(I[i,])!=0)
             {  I.new[j,] = I[i,]
                j=j+1
             }
          }
          I.new.new = matrix(0,nrow=(1+table(is.na(fixed))["TRUE"]),ncol=(1+table(is.na(fixed))["TRUE"]))
          j=1
          for(i in 1:ncol(I.new))
          {  if(sum(I.new[,i])!=0)
             {  I.new.new[,j] = I.new[,i]
                j=j+1
             }
          }
          Delta = Delta.new
          I = I.new.new
      }

     ########################
     ## Benchmarks
     ########################

      serie = matrix(0,nrow = n, ncol = iter)     
      Oc.sim = NULL
      C.sim = matrix(nrow = (n-m), ncol = iter)
      loop=1 
      repeat
      {
         for(j in loop:iter)
         {  if(dist=="LogisticI")
               stop(paste("Function not implemented for Logistic I distribution"))
            if(dist=="Glogistic")
               stop(paste("Function not implemented for Generalized Logistic distribution"))
            if(dist=="Cauchy") 
               stop(paste("Function not implemented for Cauchy distribution"))
            if(dist=="Cnormal")
               stop(paste("Function not implemented for Contamined Normal distribution"))

            serie[,j] <- symarma.sim(model=list(ar=ar,ma=ma),n=n,family=dist,index1,index2,varphi=var.model) 
            fit <- elliptical.ts(serie[,j],order=c(np,d,nq),xreg=X,include.mean=FALSE,index1=index1,index2=index2,family=dist,trace=FALSE,fixed=fixed)
            
            varphi.hat = as.vector(fit$dispersion)
            Wg = fit$Wg
            Wgder = fit$Wgder
            uwt = fit$ut^2
            erro = fit$resid.raw
            A.fit <- fit$A
            B.fit <- fit$B 
            C.fit <- fit$C
            f1.sim <- 0.5 + 2*Wg*uwt + Wgder*uwt^2
            A.sim <- as.vector(-2*(Wg + 2*Wgder*uwt))
            B.sim <- as.vector((Wg + Wgder*uwt)*erro)
            C.sim.sim <- as.vector(Wg + Wgder*uwt)
            g0 = fit$fun.g
            v = -2*Wg
            O.sim <- cbind(C.fit,A.fit,B.fit)
            
            I11.sim <- -(1/varphi.hat)*t(C.fit)%*%diag(A.sim)%*%C.fit  # beta x beta
            I12.sim <-  (2/varphi.hat^2)*t(C.fit)%*%B.sim              # beta x varphi 
            I13.sim <- -(1/varphi.hat)*t(C.fit)%*%diag(A.sim)%*%A.fit  # beta x phi
            I14.sim <- -(1/varphi.hat)*t(C.fit)%*%diag(A.sim)%*%B.fit  # beta x theta
            I22.sim <-  (1/varphi.hat^2)*sum(f1.sim)                   # varphi
            I23.sim <-  (2/varphi.hat^2)*t(A.fit)%*%B.sim              # varphi x phi
            I24.sim <-  (2/varphi.hat^2)*t(B.fit)%*%B.sim              # varphi x theta
            I33.sim <- -(1/varphi.hat)*t(A.fit)%*%diag(A.sim)%*%A.fit  # phi x phi 
            I34.sim <- -(1/varphi.hat)*t(A.fit)%*%diag(A.sim)%*%B.fit  # phi x theta
            I44.sim <- -(1/varphi.hat)*t(B.fit)%*%diag(A.sim)%*%B.fit  # theta x theta
    
            I.sim <- matrix(nrow=(np+nq+p+1), ncol=(np+nq+p+1))
            if(p!=0)
            {  I.sim[1:p,1:p] = I11.sim
               I.sim[p+1,p+1] = I22.sim
               I.sim[(1:p),(p+1)] = I12.sim
               I.sim[(p+1),(1:p)] = t(I12.sim)
               if(np!=0 && nq==0)
               {   I.sim[(1:p),(p+2):(p+1+np)] = I13.sim
                   I.sim[(p+2):(p+1+np),(1:p)] = t(I13.sim)
                   I.sim[(p+1),(p+2):(p+1+np)] = t(I23.sim)
                   I.sim[(p+2):(p+1+np),(p+1)] = I23.sim
                   I.sim[(p+2):(p+1+np),(p+2):(p+1+np)] = I33.sim
               }    
               if(np==0 && nq!=0)
               {   I.sim[(1:p),(p+2):(p+1+nq)] = I14.sim
                   I.sim[(p+2):(p+1+nq),(1:p)] = t(I14.sim)
                   I.sim[(p+1),(p+2):(p+1+nq)] = t(I24.sim)
                   I.sim[(p+2):(p+1+nq),(p+1)] = I24.sim
                   I.sim[(p+2):(p+1+nq),(p+2):(p+1+nq)] = I44.sim
               }
               if(np!=0 && nq!=0)
               {   I.sim[(1:p),(p+2):(p+1+np)] = I13.sim
                   I.sim[(p+2):(p+1+np),(1:p)] = t(I13.sim)
                   I.sim[(p+1),(p+2):(p+1+np)] = t(I23.sim)
                   I.sim[(p+2):(p+1+np),(p+1)] = I23.sim
                   I.sim[(p+2):(p+1+np),(p+2):(p+1+np)] = I33.sim
          
                   I.sim[(1:p),(p+2+np):(p+1+np+nq)] = I14.sim
                   I.sim[(p+2+np):(p+1+np+nq),(1:p)] = t(I14.sim)
                   I.sim[(p+1),(p+2+np):(p+1+np+nq)] = t(I24.sim)
                   I.sim[(p+2+np):(p+1+np+nq),(p+1)] = I24.sim
                   I.sim[(p+2+np):(p+1+np+nq),(p+2+np):(p+1+np+nq)] = I44.sim
               }    
             }
             if(p==0)
             {  I.sim[1:1] = I22.sim
                if(np!=0 && nq==0)
                {   I.sim[1,2:(1+np)] = t(I23.sim)
                    I.sim[2:(1+np),1] = I23.sim
                    I.sim[2:(1+np),2:(1+np)] = I33.sim
                }    
                if(np==0 && nq!=0)
                {  I.sim[1,2:(1+nq)] = t(I24.sim)
                   I.sim[2:(1+nq),1] = I24.sim
                   I.sim[2:(1+nq),2:(1+nq)] = I44.sim
                }
                if(np!=0 && nq!=0)
                {  I.sim[1,2:(1+np)] = t(I23.sim)
                   I.sim[2:(1+np),1] = I23.sim
                   I.sim[2:(1+np),2:(1+np)] = I33.sim
                   I.sim[1,(2+np):(1+np+nq)] = t(I24.sim)
                   I.sim[(2+np):(1+np+nq),1] = I24.sim
                   I.sim[(2+np):(1+np+nq),(2+np):(1+np+nq)] = I44.sim
                }    
             }

            D.sim.aux1=D.sim.aux2=NULL
            if(scheme=="additive") 
            {   D.sim.aux1 <- -(2/varphi.hat^2)*B.sim             # varphi
                D.sim.aux2 <- (1/varphi.hat)*diag(A.sim)%*%O.sim  # delta
                Delta.sim <- matrix(c(D.sim.aux1,D.sim.aux2),ncol=(n-m),byrow=TRUE)
            }
            if(scheme=="dispersion")
            {   D.sim.aux1 <- (1/varphi.hat)*diag(C.sim.sim)%*%uwt       # varphi
                D.sim.aux2 <- (2/varphi.hat)*diag(B.sim)%*%O.sim          # delta
                Delta.sim <- matrix(c(D.sim.aux1,D.sim.aux2),ncol=(n-m),byrow=TRUE)
            }

            if(is.null(fixed)=="FALSE")
            {   Delta.sim.new = matrix(0,ncol=(n-m),nrow=(1+table(is.na(fixed))["TRUE"]))
                l=1
                for(i in 1:nrow(Delta.sim))
                {  if(sum(Delta.sim[i,])!=0)
                   {  Delta.sim.new[l,] = Delta.sim[i,]
                      l=l+1
                   }
                }
                I.sim.new = matrix(0,nrow=(1+table(is.na(fixed))["TRUE"]),ncol=(np+nq+p+1))
                l=1
                for(i in 1:nrow(I.sim))
                {  if(sum(I.sim[i,])!=0)
                   {  I.sim.new[l,] = I.sim[i,]
                      l=l+1
                   }
                }
                I.sim.new.new = matrix(0,nrow=(1+table(is.na(fixed))["TRUE"]),ncol=(1+table(is.na(fixed))["TRUE"]))
                l=1
                for(i in 1:ncol(I.sim.new))
                {  if(sum(I.sim.new[,i])!=0)
                   {  I.sim.new.new[,l] = I.sim.new[,i]
                      l=l+1
                   }
                }
                Delta.sim = Delta.sim.new
                I.sim = I.sim.new.new
            }

            Curvatura.sim <- -2*t(Delta.sim)%*%solve(I.sim)%*%Delta.sim
            Oc.sim[j] <- max(abs(eigen(Curvatura.sim)$values))
            if(diag=="cook")  C.sim[,j] <- eigen(Curvatura.sim)$vectors[,abs(eigen(Curvatura.sim)$values)==max(abs(eigen(Curvatura.sim)$values))]
            if(diag=="lv") C.sim[,j] <- diag(Curvatura.sim)

            if(j==1) print("Benchmarks - 00% ...")   
            if(j==floor(1*iter/4)) print("Benchmarks - 25% ...")
            if(j==floor(2*iter/4)) print("Benchmarks - 50% ...")
            if(j==floor(2*iter/4)) print("Benchmarks - 75% ...")
            if(j==floor(4*iter/4)) print("Benchmarks - 100%")
        }         
        loop = j
        if(loop==iter)  break
      }

      # "BC0" statistic
      BC0 = quantile(Oc.sim,  probs = alpha)

      # "BC1" statistic
      K = matrix(nrow = (n-m), ncol = iter)
      for(i in 1:iter)    K[,i] = C.sim[,i]
      qnt.sim = NULL
      for(i in 1:iter)   qnt.sim[i] = max(abs(K[,i]))
      BC1 = quantile(qnt.sim, probs = alpha)
 
      # "BC2" statistic
      bs2 = NULL
      j=1
      for(i in 1:iter)
      {   if(Oc.sim[i] > BC0)
          {  bs2[j] = i
             j = j+1
          }
      }
      help = NULL
      for(i in 1:length(bs2))  
      {   kk = bs2[i]
          help[i] = max(abs(K[,kk]))
      }
      BC2 = quantile(help,  probs = theta)


      Curvatura <- -2*t(Delta)%*%solve(I)%*%Delta
      OC.exe <- max(abs(eigen(Curvatura)$values))
      if(diag=="cook")  C.exe <- eigen(Curvatura)$vectors[,abs(eigen(Curvatura)$values)==max(abs(eigen(Curvatura)$values))]
      if(diag=="lv")   C.exe <- diag(Curvatura)

      ## Plot
      ############################################
      if(plot=="TRUE")
      {  dat = abs(C.exe)
         if(diag=="cook")
         { if(dist=="Normal")  barplot(dat,space=1,xlab="Index",main=expression("SYMARMA" ~~ {Normal}),ylab=substitute(paste("Cook")),ylim=c(0,max(BC1+(BC1-BC2),max(dat))))
           if(dist=="Student")  barplot(dat,space=1,xlab="Index",main=expression("SYMARMA" ~~ {Student-t}),ylab=substitute(paste("Cook")),ylim=c(0,max(BC1+(BC1-BC2),max(dat))))
           if(dist=="ExpPower")   barplot(dat,space=1,xlab="Index",main=expression("SYMARMA" ~~ {ExpPower}),ylab=substitute(paste("Cook")),ylim=c(0,max(BC1+(BC1-BC2),max(dat))))
           if(dist=="LogisticII")  barplot(dat,space=1,xlab="Index",main=expression("SYMARMA" ~~ {LogisticII}),ylab=substitute(paste("Cook")),ylim=c(0,max(BC1+(BC1-BC2),max(dat))))
           if(dist=="Gstudent")   barplot(dat,space=1,xlab="Index",main=expression("SYMARMA" ~~ {Generalized-Student-t}),ylab=substitute(paste("Cook")),ylim=c(0,max(BC1+(BC1-BC2),max(dat))))
         }
         if(diag=="lv")
         { if(dist=="Normal")  barplot(dat,space=1,xlab="Index",main=expression("SYMARMA" ~~ {Normal}),ylab=substitute(paste("Lesaffre & Verbeke")),ylim=c(0,max(BC1+(BC1-BC2),max(dat))))
           if(dist=="Student")  barplot(dat,space=1,xlab="Index",main=expression("SYMARMA" ~~ {Student-t}),ylab=substitute(paste("Lesaffre & Verbeke")),ylim=c(0,max(BC1+(BC1-BC2),max(dat))))
           if(dist=="ExpPower")   barplot(dat,space=1,xlab="Index",main=expression("SYMARMA" ~~ {ExpPower}),ylab=substitute(paste("Lesaffre & Verbeke")),ylim=c(0,max(BC1+(BC1-BC2),max(dat))))
           if(dist=="LogisticII")  barplot(dat,space=1,xlab="Index",main=expression("SYMARMA" ~~ {LogisticII}),ylab=substitute(paste("Lesaffre & Verbeke")),ylim=c(0,max(BC1+(BC1-BC2),max(dat))))
           if(dist=="Gstudent")   barplot(dat,space=1,xlab="Index",main=expression("SYMARMA" ~~ {Generalized-Student-t}),ylab=substitute(paste("Lesaffre & Verbeke")),ylim=c(0,max(BC1+(BC1-BC2),max(dat))))
         }
         abline(h=BC1,lty=1)
         abline(h=BC2,lty=2)
      }


      ## Print
      ############################################
 	 print(matrix(nrow=2, ncol=0, byrow=TRUE,dimnames = list(c("Call:", paste(c("symarma(",np,",",d,",",nq,") - family: ",dist),collapse="")))))    
       if(diag=="cook") print(matrix(nrow=1, ncol=0, byrow=TRUE,dimnames = list(c("Cook"))))
       if(diag=="lv") print(matrix(nrow=1, ncol=0, byrow=TRUE,dimnames = list(c("Lesaffre & Verbeke"))))
       print(matrix(nrow=1, ncol=0, byrow=TRUE,dimnames = list(c("Measures of local influence:"))))
       print(matrix(c(OC.exe),nrow=1, ncol=1,byrow=TRUE,dimnames = list(c("Curvature"))))
       print(matrix(nrow=1, ncol=0, byrow=TRUE,dimnames = list(c("Benchmarks:"))))
       print(matrix(c(BC0,BC1,BC2),nrow=3, ncol=1,byrow=FALSE,dimnames = list(c("BC0","BC1","BC2"))))
      
       Indiv1 = BC1
       Indiv2 = BC2
       VecInd = abs(C.exe)
}
fit <- list(Indiv1=Indiv1,Indiv2=Indiv2,VectorInd=VecInd)
}
