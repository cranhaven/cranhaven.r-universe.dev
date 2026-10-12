nbfactors <-
function(dta, maxnbfactors = 15, diagnostic.plot = FALSE, min.err = 0.001, verbose = FALSE,svd.method=c("fast.svd","irlba")) {

   bivprob = function(rho, lower, upper = -lower, mean = 0) {
      nu = 0
      low = rep(as.double((lower - mean)), 2)
      upp = rep(as.double((upper - mean)), 2)
      if (any(lower == upper)) 
         return(0)
      infin = c(2, 2)
      infin = as.integer(infin)
      low = replace(low, low == -Inf, 0)
      upp = replace(upp, upp == Inf, 0)
      rho = as.double(rho)
      prob = as.double(0)
      a = lapply(rho, function(r, low, upp) biv.nt.prob(df = Inf, 
                                                    lower = low, upper = upp, mean = rep(0, 2), S = matrix(c(1,r, r, 1), 2, 2)), low = low, upp = upp)
      return(unlist(a))
   }

   Dt = function(rho) {
      threshold = 0.05
      ut = qnorm(1 - threshold/2)
      delta = unlist(lapply(rho, bivprob, lower = -ut)) - (1 - threshold)^2
      dt = delta/(threshold * (1 - threshold))
      return(dt)
   }

   VarInflation = function(dta, falist, maxnbfactors, dig, verbose) {
      m = ncol(dta)
      n = nrow(dta)
      vecrho = round(seq(10^(-dig), 1, 10^(-dig)), digits = dig)
      vecdt = unlist(lapply(vecrho, Dt))
      sampled = sample(1:m, min(1000, m))
      sampsize = length(sampled)
      cordata = t(dta[, sampled]) %*% dta[, sampled]/(n - 1)
      sdt = rep(0, maxnbfactors + 1)
      names(sdt) = paste(0:maxnbfactors, "factors")
      for (i in 1:(maxnbfactors + 1)) {
         if (verbose) 
            print(paste("Calculating Variance Inflation criterion for the model with",i - 1, "factors"))
         B = matrix(falist[[i]]$B[sampled, ], nrow = sampsize)
         sdb = sqrt(1 - apply(B^2, 1, sum)) # sqrt(falist[[i]]$Psi) 
         matrho = cordata - B %*% t(B)
         matrho = sweep(matrho, 2, FUN = "/", STATS = sdb)
         matrho = sweep(matrho, 1, FUN = "/", STATS = sdb)
         rho = matrho[col(matrho) > row(matrho)]
         rho[abs(rho) >= 1] = 1
         veccor = sort(round(abs(rho), digits = dig))
         duplic = duplicated(veccor)
         vduplic = sort(unique(veccor[duplic]))
         vunic = setdiff(unique(veccor), vduplic)
         dtunic = vecdt[is.element(vecrho, vunic)]
         dtduplic = vecdt[is.element(vecrho, vduplic)]
         vmatch = match(vecrho, veccor, 0)
         nboccur = diff(c(vmatch[vmatch > 0], length(veccor) + 1))
         nboccur = nboccur[nboccur > 1]
         sdt[i] = 2 * (m - 1) * (sum(dtunic) + crossprod(nboccur,dtduplic))/(sampsize * (sampsize - 1))
      }
      return(sdt)
   }

   svd.method = match.arg(svd.method,c("fast.svd","irlba"))
   dig = 2
   n = nrow(dta)
   m = ncol(dta)
   mdta = (t(rep(1,n))%*%dta)[1,]/n
   cdta = dta-rep(1,n)%*%t(mdta)
   sddta = sqrt((t(rep(1,n))%*%cdta^2)[1,]/(n-1))
   falist = vector(length = maxnbfactors + 1, "list")
   falist[[1]] = list(Psi=rep(1,m),B = matrix(0, ncol = 1, nrow = m))
   falist[-1] = lapply(1:maxnbfactors, emfa, dta = dta,min.err = min.err, verbose = verbose,svd.method=svd.method)
   sdt = VarInflation(dta, falist, maxnbfactors, dig, verbose)
   if (diagnostic.plot) {
      plot(0:maxnbfactors, sdt, ylab = "Variance Inflation Criterion", 
         xlab = "Number of factors", bty = "l", lwd = 1.25, 
         type = "b", pch = 16, cex.lab = 1.25, cex = 1.25, 
         cex.axis = 1.25)
   }
   if (which.min(sdt) == 1) {
      nbf = 0
      opt1 = 0
      opt2 = 0
   }   
   if (which.min(sdt) > 1) {
      jumps = -diff(sdt)/(diff(range(sdt)))
      opt1 = (0:maxnbfactors)[which.min(sdt)]
      opt2 = max((1:maxnbfactors)[jumps > 0.05])
      nbf = ifelse(opt2<=opt1,opt2,opt1)
      if (opt2>=opt1) opt2=opt1
   }
   if (diagnostic.plot) {
      abline(v=c(opt1,opt2),lwd=2,lty=3)
      text(opt1,0.8*max(sdt),paste(opt1," factors ",sep=""),
         adj=1,cex=1.25)
      text(opt2,0.9*max(sdt),paste(" ",opt2," factors",sep=""),
         adj=0,cex=1.25)
   }
   list(criterion = sdt, optimalnbfactors = nbf)
}
