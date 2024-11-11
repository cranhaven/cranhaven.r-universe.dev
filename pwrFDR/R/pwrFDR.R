"pwrFDR" <-
function(effect.size, n.sample, r.1, alpha, delta=NULL, groups=2, N.tests,
         average.power, tp.power, lambda, type=c("paired","balanced","unbalanced"),
         grpj.per.grp1=NULL, FDP.control.method=c("BHFDR","BHCLT","Romano","Auto","both"),
         method=c("FixedPoint", "simulation"), n.sim=1000, temp.file,
         control=list(tol=1e-8, max.iter=c(1000,20), distopt=1, CS=list(NULL), sim.level=2,
             low.power.stop=TRUE, FDP.meth.thresh=FDP.cntl.mth.thrsh.def, verb=FALSE,
                      ast.le.a=TRUE))
{
    .call. <- m <- match.call()
    pfx <- as.character(m[[1]])
    
    frmls <- names(formals(pwrFDR))
    sppld <- names(m)[-1]

    m <- evald.call <- args.def.err.chk(m, sppld, frmls)
    err.msg <- attr(m, "err.msg")
    
    if(!is.null(err.msg)) stop(err.msg)

    m[[1]] <- as.name(pfx %,% "." %,% pwrfdrmthd.sfx[m$method])

    m$method <- m$temp.file <- NULL 

    args.full <- evald.call
    args.full[[1]] <- as.name("list")
    args.full <- eval(args.full)
    args.full$call <- evald.call

    out <- eval(m)
    class(out) <- "pwr"
    attr(out, "arg.vals") <- args.full
    out$call <- .call.
    out
}

"pwrFDR.grid" <-
function(effect.size, n.sample, r.1, alpha, delta, groups, N.tests, average.power,
             tp.power, lambda, type, grpj.per.grp1, FDP.control.method, control)
{
    .call. <- m <- fd <- match.call()
    pfx <- as.character(m[[1]])
    frmls <- names(formals(pwrFDR.grid))
    sppld <- names(m)[-1]
    n.sppld <- length(sppld)
    is.msng <- !(frmls %in% sppld)
    names(is.msng) <- frmls
    msng.nms <- names(is.msng)[is.msng]
    eval.env <- sys.parent
    for (k in 1:n.sppld)
    {
      m[[sppld[k]]] <- eval(m[[sppld[k]]], eval.env())
      fd[[sppld[k]]] <- length(m[[sppld[k]]])
    }
    is.pwr <- (!is.msng["average.power"]) || (!is.msng["tp.power"])
    nera <- c("n.sample", "effect.size", "r.1", "alpha")
   
    # check that either power is unspecified and nera are all specified
    # or that power is unspecified and exactly one nera is missing.

    # are we doing the power specified or power unspecified case.
    pwr.spcfd <- !is.msng["average.power"]||!is.msng["tp.power"]

    # which power type?
    use.L.pwr <- !is.msng["tp.power"]
    pwr.nm <- c("average.power", "tp.power")[1+use.L.pwr]
    msng.nera <- nera[which(is.msng[nera])]

    fd[[1]] <- as.name("factorial.design")

    fd <- as.data.frame(eval(fd))
    n.conds <- nrow(fd)
 
    pwrFDR.call <- as.call(expression(pwrFDR))

    names(fd) <- sppld

    rslts <- list()
    cnds <- NULL
    for(j in 1:n.conds)
    {
      cnds.j <- NULL
      for(k in 1:n.sppld)
      {
          cnds.j <- c(cnds.j, m[[sppld[k]]][fd[j,k]])
          pwrFDR.call[[sppld[k]]] <- m[[sppld[k]]][fd[j,k]]
      }
      cnds <- rbind(cnds, cnds.j)
      rslts[[j]] <- try(eval(pwrFDR.call, eval.env()), silent=TRUE)
    }
    dimnames(cnds)[[1]] <- NULL
    cnds <- as.data.frame(cnds)
    names(cnds) <- sppld
    
    list(conditions=cnds, results=rslts)
}

"pwrFDR.FP" <-
function(effect.size, n.sample, r.1, alpha, delta, groups, N.tests,
         average.power, tp.power, lambda, type, grpj.per.grp1,
         FDP.control.method, method, n.sim, temp.file, control)
{
    ## pwr, n.sample, effect.size, r.1, alpha
    .call. <- m.sv <- m <- match.call()
    ee <- function(x)exp(exp(x))-1
    ll <- function(x)log(log(1 + x))
    ijumps <- FALSE
    Auto <- ""
   
    is.msng <- control$is.msng

    pfx <- as.character(m[[1]])
    
    is.N <- !is.msng["N.tests"]
    pwr.spcfd <- !is.msng["average.power"]||!is.msng["tp.power"]

    if(!pwr.spcfd)
    {
      m[[1]] <- as.name(as.character(m[[1]]) %,% ".1X")
      m$average.power <- m$tp.power <- NULL
      out <- eval(m)
    }
    if(pwr.spcfd)
    {
      use.L.pwr <- !is.msng["tp.power"]
      pwr.nm <- c("average.power", "tp.power")[1+use.L.pwr]
      msng.nera <- nera[which(is.msng[nera])]
      psi.inv <- list(n.sample=ee, effect.size=ee, r.1=logit.inv.r, alpha=logit.inv.r)[[msng.nera]]
      psi <- list(n.sample=ll, effect.size=ll, r.1=logit, alpha=logit)[[msng.nera]]
      OBJ <-
      function(x, m, misc, p=c(2, 1.25))
      {
        rslt.call <- m
        rslt.call[[1]] <- as.name(pfx %,% ".1X")
        rslt.call[[misc$msng.nera]] <- misc$psi.inv(x)

        rslt.call$average.power <- rslt.call$tp.power <- NULL
        rslt <- suppressWarnings(eval(rslt.call))

        if(!use.L.pwr)
        {
          pwr <- rslt$pwr <- rslt$average.power
          pwr. <- max(min(pwr, 1-1e-16),1e-16)
          out <- out.n <- ((logit(pwr.) - logit(average.power))^p[1])^(1/p[2])
        }
        if(use.L.pwr)
        {
          pwr <- rslt$pwr <- rslt$tp.power
          pwr. <- min(max(pwr, 1e-16), 1-1e-16)
          out <- out.n <- ((logit(pwr.) - logit(tp.power))^p[1])^(1/p[2])
        }        
        attr(out, "detail") <- rslt
        if(control$verb>0)
          cat(sprintf("(obj, %s, %s)=(%g, %g, %g)\n", misc$msng.nera, misc$pwr.nm, out, misc$psi.inv(x), pwr.))
        if(control$verb>0 && is.na(out)) browser()            
        out
      }
      idistopt <- control$distopt
      is.pos <- (dists$minv[[1+idistopt]]==0)

      if(use.L.pwr) pwr <- tp.power
      if(!use.L.pwr) pwr <- average.power

      l.nera <- c(n.sample=5,        effect.size=0.01, r.1=0.00001, alpha=0.00001)
      u.nera <- c(n.sample=Inf, effect.size=2,    r.1=0.99999, alpha=0.99999)
      l.x <- psi(l.nera[msng.nera])
      u.x <- psi(u.nera[msng.nera])
      if(msng.nera=="n.sample")
      {
          grps <- groups
          r.0 <- 1 - r.1
          alpha.0 <- r.0*alpha
          if(use.L.pwr) pwr <- tp.power
          if(!use.L.pwr) pwr <- average.power
          gma <- r.1*pwr/(1-alpha.0)
          ftest.ss <- f.power(power=pwr, groups=grps, effect.size=effect.size, e.I=gma*alpha)
          u.x <- psi(10*ftest.ss$n.sample)
          done <- FALSE
          while(!done)
          {
              obj.ux <-OBJ(u.x,m=m.sv, misc=list(msng.nera=msng.nera, pwr.nm=pwr.nm, psi.inv=psi.inv), p=c(1,1))
              done <- !is.na(obj.ux)
              if(!done) u.x <- psi(psi.inv(u.x) -1)
          }
          lu.k <- c(l.x, u.x)
          err <- 1
          x <- u.x
          iter <- 0
          while(err > 1e-3)
          {
            while(err > 1e-3 && iter < 20)
            {
              obj <- OBJ(x, m=m.sv, misc=list(msng.nera=msng.nera, pwr.nm=pwr.nm, psi.inv=psi.inv), p=c(1,1))
              pos <- obj > 0
              err <- abs(obj)
              if(control$verb > 0) cat(sprintf("n: %d, x: %g, lu.k[1]: %g, lu.k[2]: %g, obj: %g, err: %g\n",
                                               ceiling(psi.inv(x)), x, lu.k[1], lu.k[2], obj, err))
              lu.k <- pos*c(lu.k[1], x) + (1-pos)*c(x, lu.k[2])
              x <- mean(lu.k)
              iter <- iter + 1
            }
            if(err > 1e-3)
            {
              lu.k <- lu.k <- c(l.x, u.x)
              if(m.sv$FDP.control.method %in% c("BHCLT","Auto")) m.sv$FDP.control.method <- Auto <- "Romano"
              ijumps <- TRUE
              iter <- 0
            }
          }
          ans <- list(objective=obj)
          y <- ceiling(psi.inv(x))
      }
      if(msng.nera!="n.sample")
      {
          ans <- optimize(f=OBJ, lower=l.x, upper=u.x, m=m.sv, tol=.Machine$double.eps^0.35,
                          misc=list(msng.nera=msng.nera, pwr.nm=pwr.nm, psi.inv=psi.inv))
          obj <- ans$objective
          if(abs(obj)>1e-3) stop("No Solution, " %,% msng.nera %,% " too close to " %,%
                                 l.nera[msng.nera] %,% " or " %,% u.nera[msng.nera] %,% " and " %,%
                                 pwr.nm %,% " is " %,% signif(attr(obj,"detail")$pwr,3))
          y <- psi.inv(ans$minimum)
      }
      
      assign(msng.nera, y)

      nii.sample <- n.sample
      if(groups>=2 && type==3) nii.sample <- n.sample*grpj.per.grp1

      .DF. <- eval(DF[type])
      .NCP. <- eval(NCP[type])
      
      pars0 <- eval(dists$pars0[[1+idistopt]])
      pars1 <- eval(dists$pars1[[1+idistopt]])

      r.0 <- 1 - r.1
      alpha.0 <- r.0*alpha
      gma <- r.1*ans$average.power/(1-alpha.0)
      c.g <- dists$qdist[[1+idistopt]](1 - gma*alpha/2^(!is.pos), pars0)
      eIII <- dists$pdist[[1+idistopt]](-c.g, pars1)
      dtl <- attr(ans$objective, "detail")
      dtl$pwr <- NULL
      out <- numeric(0)
      out[msng.nera] <- get(msng.nera)
      if(ijumps) dtl$Auto <- Auto
      out <- c(out, dtl)
    }
    out
}

"pwrFDR.FP.1X" <-
function(effect.size, n.sample, r.1, alpha, delta, groups, N.tests,
         average.power, tp.power, lambda, type, grpj.per.grp1,
         FDP.control.method, method, n.sim, temp.file, control)
{
    .call. <- m <- m.sv <- match.call()
    is.msng <- control$is.msng
    ast.le.a <- control$ast.le.a
    if(is.msng["alpha"]) .call.$delta <- m$delta <- m.sv$delta <- m$alpha

    is.N <- !missing(N.tests)
    idistopt <- control$distopt
    
    nii.sample <- n.sample
    if(groups>=2 && type==3) nii.sample <- n.sample*grpj.per.grp1

    ## if(groups==1) type is 1 (paired)
    ## if(groups==2) type is 1 (paired), 2 (balanced) or 3 (unbalanced)
    ## if(groups>=3) type is 2 (balanced) or 3 (unbalanced)
    .DF. <- eval(DF[type])
    .NCP. <- eval(NCP[type])
    
    pars0 <- eval(dists$pars0[[1+idistopt]])
    pars1 <- eval(dists$pars1[[1+idistopt]])
    
    is.pos <- (dists$minv[[1+idistopt]]==0)
    
    Auto <- se.by.a <- gma.Ntsts <- NULL
    
    n <- n.sample
    r.0 <- 1 - r.1
    alpha.0 <- r.0*alpha
    alpha.st <- NULL
    do.tp.power <- !missing(lambda)
    if(FDP.control.method == "Auto")
    {        
      ## determine whether to use BHFDR, BHCLT or Romano
      ## compute se.VoR under BHFDR
      m.fxd.pt <- m  
      m.fxd.pt[[1]] <- as.name("CDF.Pval.ua.eq.u")
      m.fxd.pt$N.tests <- m.fxd.pt$lambda <- m.fxd.pt$FDP.control.method <- m.fxd.pt$delta <- NULL
      gma <- eval(m.fxd.pt, sys.parent())$gamma
      se.VoR <- (r.0*alpha*(1-r.0*alpha*gma)/(N.tests*gma))^0.5

      # based upon se[V/R] / alpha   and N.tests
      FDP.meth.thresh <- control$FDP.meth.thresh
      se.by.a <- se.VoR/alpha
      
      do.bhfdr <- (se.by.a < FDP.meth.thresh[1])
      do.bhclt <- ((se.by.a >= FDP.meth.thresh[1]) && (N.tests >= FDP.meth.thresh[2]))
      do.romano <- !do.bhfdr && !do.bhclt
        
      mthd.chc <- 1*do.romano + 2*do.bhclt + 3*do.bhfdr
      FDP.control.method <- c("Romano", "BHCLT", "BHFDR")[mthd.chc]
      Auto <- FDP.control.method  
    }
    
    if(FDP.control.method == "BHFDR")
    {
      m.fxd.pt <- m  
      m.fxd.pt[[1]] <- as.name("CDF.Pval.ua.eq.u")
      m.fxd.pt$N.tests <- m.fxd.pt$lambda <- m.fxd.pt$FDP.control.method <- m.fxd.pt$delta <- NULL
      gma <- gma.st <- eval(m.fxd.pt, sys.parent())$gamma
      c.g <- qdist(1 - gma*alpha/2^(!is.pos), pars0)
      G1.pr <- r.1*(ddist(c.g, pars1) +
               (!is.pos)*ddist(-c.g, pars1))/(ddist(c.g, pars0) + (!is.pos)*ddist(-c.g, pars0))
      G.pr <- r.0 + G1.pr
      average.power <- pi.1 <- 1 - pdist(c.g, pars1) + (!is.pos) * pdist(-c.g, pars1)
      ## se.VoR computed above correctly in this case
      ## se.VoR <- r.0*alpha*(1-r.0*alpha*gma)/(N.tests*gma)^0.5
      sigma.rtm.VoR <- (r.0*alpha*(1-r.0*alpha*gma)/gma)^0.5
      
      tau2 <- gma*(1-gma)/(1-alpha*G.pr)^2

      v.W1 <- r.1*pi.1 - r.1^2*pi.1^2
      c.W0.W1 <- - r.0*alpha*gma*r.1*pi.1
    
      v.top <- v.W1 + alpha^2*G1.pr^2*tau2 + 2*alpha*G1.pr*(v.W1 + c.W0.W1)/
                            (1-alpha*G.pr)
      v.bot <- r.0*r.1
      c.top.bot <- r.0*r.1*(pi.1 + alpha*G1.pr/(1-alpha*G.pr)*(pi.1 - alpha*gma))
    # T/B - t/b = 1/b (T-t) - t/b^2 (B - b)
    # var(T/B) = 1/b^2 var(T) - 2*t/b^3 cov(T, B) + t^2/b^4 var(B)
    #          = 1/b^2 * (var(T) - t/b * cov(T, B) + (t/b)^2*var(B)

      sigma.rtm.ToM <- (v.top - 2*pi.1*c.top.bot + pi.1^2*v.bot)^0.5/r.1
    }
    if(FDP.control.method == "BHCLT")
    {
      m.cntlfdp <- m  
      m.cntlfdp[[1]] <- as.name("controlFDP")
      m.cntlfdp$lambda <- m.cntlfdp$FDP.control.method <- NULL
      alpha.st <- eval(m.cntlfdp, sys.parent())$alpha.star
      if(ast.le.a) alpha.st <- min(alpha.st, alpha)
      if(is.na(alpha.st)) FDP.control.method <- Auto <- "Romano"
      if(!is.na(alpha.st))
      {
        m.fxd.pt <- m  
        m.fxd.pt[[1]] <- as.name("CDF.Pval.ua.eq.u")
        m.fxd.pt$N.tests <- m.fxd.pt$lambda <- m.fxd.pt$FDP.control.method <- m.fxd.pt$delta <- NULL
        m.fxd.pt$alpha <- alpha.st
        gma <- gma.st <- eval(m.fxd.pt, sys.parent())$gamma
        c.g <- qdist(1 - gma.st*alpha.st/2^(!is.pos), pars0)
        G1.pr <- r.1*(ddist(c.g, pars1) +
                 (!is.pos)*ddist(-c.g, pars1))/(ddist(c.g, pars0) + (!is.pos)*ddist(-c.g, pars0))
        G.pr <- r.0 + G1.pr
        average.power <- pi.1 <- 1 - pdist(c.g, pars1) + (!is.pos) * pdist(-c.g, pars1)
        sigma.rtm.VoR <- (r.0*alpha.st*(1-r.0*gma.st*alpha.st)/gma.st)^0.5
        tau2 <- gma.st*(1-gma.st)/(1-alpha.st*G.pr)^2

        v.W1 <- r.1*pi.1 - r.1^2*pi.1^2
        c.W0.W1 <- - r.0*alpha.st*gma.st*r.1*pi.1
    
        v.top <- v.W1 + alpha.st^2*G1.pr^2*tau2 + 2*alpha.st*G1.pr*(v.W1 + c.W0.W1)/
                            (1-alpha.st*G.pr)
        v.bot <- r.0*r.1
        c.top.bot <- r.0*r.1*(pi.1 + alpha.st*G1.pr/(1-alpha.st*G.pr)*(pi.1 - alpha.st*gma.st))
    # T/B - t/b = 1/b (T-t) - t/b^2 (B - b)
    # var(T/B) = 1/b^2 var(T) - 2*t/b^3 cov(T, B) + t^2/b^4 var(B)
    #          = 1/b^2 * (var(T) - t/b * cov(T, B) + (t/b)^2*var(B)

        sigma.rtm.ToM <- (v.top - 2*pi.1*c.top.bot + pi.1^2*v.bot)^0.5/r.1
      }
    }
    if(FDP.control.method == "Romano")
    {
      m.fxd.pt <- m
      m.fxd.pt[[1]] <- as.name("CDF.Pval.ar.eq.u")
      m.fxd.pt$N.tests <- m.fxd.pt$lambda <- m.fxd.pt$FDP.control.method <- NULL
      gma <- gma.R <- eval(m.fxd.pt, sys.parent())$gamma
    
      psi. <- gma.R*delta/(1-(1-delta)*gma.R)
      psi.pr <- delta/(1-(1-delta)*gma.R)^2
      u <- psi.*alpha
      c.g <- qdist(1 - u/2^(!is.pos), pars0)
      G1.pr <- r.1*(ddist(c.g, pars1) +
               (!is.pos)*ddist(-c.g, pars1))/(ddist(c.g, pars0) + (!is.pos)*ddist(-c.g, pars0))
      G.pr <- r.0 + G1.pr
      average.power <- pi.1 <- 1 - pdist(c.g, pars1) + (!is.pos)*pdist(-c.g, pars1)

      tau2 <- gma.R*(1-gma.R)/(1-alpha*psi.pr*G.pr)^2

      sigma.rtm.VoR <-
          ((1+r.0*alpha*(psi.pr-psi./gma.R)/(1-alpha*psi.pr*G.pr))^2*r.0*alpha*psi.*(1-r.0*alpha*psi.)
           +(r.0*alpha*(psi.pr-psi./gma.R)/(1-alpha*psi.pr*G.pr))^2*r.1*pi.1*(1-r.1*pi.1)
            -2*(1+r.0*alpha*(psi.pr-psi./gma.R)/(1-alpha*psi.pr*G.pr))*
            (r.0*alpha*(psi.pr-psi./gma.R)/(1-alpha*psi.pr*G.pr))*r.0*alpha*psi.*r.1*pi.1)^0.5/gma.R
      
      v.W1 <- r.1*pi.1 - r.1^2*pi.1^2
      c.W0.W1 <- - r.0*alpha*psi.*r.1*pi.1
    
      v.top <- v.W1 + alpha^2*psi.pr^2*G1.pr^2*tau2 + 2*alpha*psi.pr*G1.pr*(v.W1 + c.W0.W1)/
                            (1-alpha*psi.pr*G.pr)
      v.bot <- r.0*r.1
      c.top.bot <- r.0*r.1*(pi.1 + alpha*psi.pr*G1.pr/(1-alpha*psi.pr*G.pr)*(pi.1 - alpha*psi.))
    # T/B - t/b = 1/b (T-t) - t/b^2 (B - b)
    # var(T/B) = 1/b^2 var(T) - 2*t/b^3 cov(T, B) + t^2/b^4 var(B)
    #          = 1/b^2 * (var(T) - t/b * cov(T, B) + (t/b)^2*var(B)

      sigma.rtm.ToM <- (v.top - 2*pi.1*c.top.bot + pi.1^2*v.bot)^0.5/r.1
    }

    eIII <- dists$pdist[[1+idistopt]](-c.g, pars1)
    out <- list(average.power = average.power, c.g = c.g, gamma = gma, err.III=eIII)
    if(!is.N)
    {
      out$sigma.rtm.Rom <- max(tau2^0.5, 1e-10)
      out$sigma.rtm.VoR <- max(sigma.rtm.VoR, 1e-10)
      out$sigma.rtm.ToM <- max(sigma.rtm.ToM, 1e-10)
    }
    if(is.N)
    {
      out$se.Rom <- max((tau2/N.tests)^0.5, 1e-10)
      out$se.VoR <- max(sigma.rtm.VoR/N.tests^0.5, 1e-10)
      out$se.ToM <- max(sigma.rtm.ToM/N.tests^0.5, 1e-10)
    }
    
    if(!is.null(Auto)) out$Auto <- Auto
    if(!is.null(alpha.st)&&!is.na(alpha.st)) out$alpha.star <- alpha.st
    if(do.tp.power)
    {
      se.ToM <- sigma.rtm.ToM/N.tests^0.5
      tp.power <- pnorm((average.power - lambda)/se.ToM)
      L.eq <- average.power - se.ToM * qnorm(average.power)
      out$tp.power <- tp.power
      out$L.eq <- L.eq
    }
    attr(out, "pars") <- rbind(pars0=pars0,pars1=pars1)
    
    out
}

"pwrFDR.sim" <-
function(groups, effect.size, n.sample, r.1, alpha, N.tests, control, lambda,
         FDP.control.method, type, grpj.per.grp1, n.sim=1000, delta)
{
    .call. <- m <- match.call()
    m.FP.1X <- m
    m.FP.1X$n.sim <- NULL
    m.FP.1X$do.Romano <- NULL
    m.FP.1X[[1]] <- as.name("pwrFDR.FP.1X")
    m.FP.1X$FDP.control.method <- "BHFDR"
    rslt.FP.1X <- eval(m.FP.1X, sys.parent())
    low.power.stop <- control$low.power.stop
    if(is.null(control$low.power.stop)) low.power.stop <- TRUE
    if(rslt.FP.1X$average.power < 0.50 && low.power.stop)
        stop("You don't want to run a simulation on a set of inputs with average power < 0.50")
    verb <- control$verb
    idistopt <- control$distopt
    ast.le.a <- control$ast.le.a
    
    nii.sample <- n.sample
    if(groups>=2 && type==3) nii.sample <- n.sample*grpj.per.grp1
    
    ## if(groups==1) type is 1 (paired)
    ## if(groups==2) type is 1 (paired), 2 (balanced) or 3 (unbalanced)
    ## if(groups>=3) type is 2 (balanced) or 3 (unbalanced)
    .DF. <- eval(DF[type])
    .NCP. <- eval(NCP[type])

    pars0 <- eval(dists$pars0[[1+idistopt]])
    pars1 <- eval(dists$pars1[[1+idistopt]])
    is.pos <- (dists$minv[[1+idistopt]]==0)

    nsim <- n.sim
    do.tp.power <- !missing(lambda)
    alpha.st <- alpha
    BHCLT.lvl <- control$sim.level*(FDP.control.method=="BHCLT" || FDP.control.method=="both")
    Rmno <- (FDP.control.method=="Romano" || FDP.control.method=="both")
    if(BHCLT.lvl>=1)
    {
        m.cntlFDP <- m
        m.cntlFDP$n.sim <- m.cntlFDP$lambda <- m.cntlFDP$FDP.control.method <-
            m.cntlFDP$do.Romano <- NULL
        m.cntlFDP[[1]] <- as.name("controlFDP")
        alpha.st <- eval(m.cntlFDP, sys.parent())$alpha.star
        alpha.st <- ifelse(!is.na(alpha.st), alpha.st, -1)
        if(ast.le.a) alpha.st <- min(alpha.st, alpha)
        BHCLT.lvl <- ifelse(alpha.st > 0, BHCLT.lvl, 0)
    }
    is.CS <- !is.null(control$CS[[1]])
    if(!is.CS)
    {
      if(verb)
      {
        cat(sprintf("nsim=%d, alpha=%g, N.tests=%d, r.1=%g, n.sample=%d, effect.size=%g\n",
                    nsim, alpha, N.tests, r.1, n.sample, effect.size))
      }
     rslt <- .C("pwrFDRsim",
                 nsim    = as.integer(nsim),
                 alpha   = as.double(alpha),
                 BHCLT   = as.integer(BHCLT.lvl),
                 do.Rmno = as.integer(Rmno), 
                 alpha.star  = as.double(alpha.st),
                 Ng      = as.integer(N.tests),
                 r1      = as.double(r.1),
                 n       = as.integer(n.sample),
                 theta   = as.double(effect.size),
                 distopt = as.integer(control$distopt),
                 groups  = as.double(groups),
                 delta   = as.double(delta),
                 verb    = as.integer(verb),
                 M1      = integer(nsim),
                 R       = integer(nsim),
                 T       = integer(nsim),
                 R.st    = integer(nsim),
                 T.st    = integer(nsim),
                 p0.ht   = double(nsim),
                 R.R     = integer(nsim),
                 T.R     = integer(nsim), 
                 X.i     = double(N.tests),
                 M.i     = integer(1),
                 PACKAGE = "pwrFDR")
    }
    if(is.CS)
    {
      CS <- control$CS    
      bad <- is.null(CS$rho)||is.null(CS$n.WC)
      if(!bad)
      {
        rho <- control$CS$rho
        n.WC <- control$CS$n.WC
        bad <- (rho <= -1 || rho >= 1 || n.WC <= 0)
        if(!bad) bad <- (N.tests %% n.WC) != 0
        if(bad) stop("list argument 'control' must contain a component, 'CS', of type list, " %,%
                     "with components 'rho' and 'n.WC', which must satisfy -1 < rho < 1 " %,%
                     "and 'n.WC' divides 'N.tests' evenly")
        # cat(sprintf("is.CS=%d, rho=%g, n.WC=%d\n",is.CS, rho,n.WC))
      }
      if(verb)
      {
        cat(sprintf("nsim=%d, alpha=%g, N.tests=%d, r.1=%g, n.sample=%d, effect.size=%g, rho=%g, n.WC=%d\n",
                    nsim, alpha, N.tests, r.1, n.sample, effect.size, rho, n.WC))
      }
      rslt <- .C("pwrFDRsimCS",
                 nsim    = as.integer(nsim),
                 alpha   = as.double(alpha),
                 BHCLT   = as.integer(BHCLT.lvl),
                 do.Rmno = as.integer(Rmno), 
                 alpha.star  = as.double(alpha.st),
                 Ng      = as.integer(N.tests),
                 r1      = as.double(r.1),
                 n       = as.integer(n.sample),
                 theta   = as.double(effect.size),
                 rho     = as.double(rho),
                 n.WC    = as.integer(n.WC),
                 delta   = as.double(delta),
                 pverb   = as.integer(verb),
                 M1      = integer(nsim),
                 R       = integer(nsim),
                 T       = integer(nsim),
                 R.st    = integer(nsim),
                 T.st    = integer(nsim),
                 p0.ht   = double(nsim),
                 R.R     = integer(nsim),
                 T.R     = integer(nsim),
                 X.i     = double(N.tests),
                 M.i     = integer(1),
                 PACKAGE = "pwrFDR")
    }

    reps <- data.frame(M1=rslt$M1, R=rslt$R, T=rslt$T)
    average.power <- with(reps, mean(T %over% M1))
    gamma <- with(reps, mean(R)/N.tests)
    se.Rom <- with(reps, var(R/N.tests)^0.5)
    se.VoR <- with(reps, var((R - T) %over% R)^0.5)
    se.ToM <- with(reps, var(T %over% M1)^0.5)
    out <- list(average.power=average.power, gamma=gamma,
                se.Rom=se.Rom, se.VoR=se.VoR, se.ToM=se.ToM)
    if(do.tp.power)
    {
      tp.power <- with(reps, mean(T %over% M1 >= lambda))
      out$tp.power <- tp.power
    }
    
    se.VoR.st <- se.ToM.st <- se.VoR.st.ht <- se.ToM.st.ht <- se.VoR.R <-
        se.ToM.R <- NULL
    if(BHCLT.lvl >= 1)
    {
      reps <- cbind(reps, R.st=rslt$R.st, T.st=rslt$T.st)
      average.power.st <- with(reps, mean(T.st %over% M1))
      P.st <- with(reps, mean((R.st - T.st) %over% R.st > alpha))
      se.Rom.st <-  with(reps, var(R.st/N.tests)^0.5)
      se.VoR.st <- with(reps, var((R.st - T.st) %over% R.st)^0.5)
      se.ToM.st <- with(reps, var(T.st %over% M1)^0.5)
      gamma.st <- with(reps, mean(R.st)/N.tests)
      out <- c(out, average.power.st=average.power.st, gamma.st=gamma.st,
               se.Rom.st=se.Rom.st, se.VoR.st=se.VoR.st, se.ToM.st=se.ToM.st,
               alpha.star=alpha.st)
      if(do.tp.power)
      {
        tp.power.st <- with(reps, mean(T.st %over% M1 >= lambda))
        out$tp.power.st <- tp.power.st
      }
    }
    if(BHCLT.lvl == 2)
    {
      reps <- cbind(reps, p0.ht=pmin(rslt$p0.ht,1))
      out <- c(out, p0.ht.avg=mean(rslt$p0.ht), sd.p0.ht=var(rslt$p0.ht)^0.5)
    }
    if(Rmno)
    {
      reps <- cbind(reps, R.R=rslt$R.R, T.R=rslt$T.R)
      average.power.R <- with(reps, mean(T.R %over% M1))
      P.R <- with(reps, mean((R.R - T.R) %over% R.R > alpha))
      se.Rom.R <-  with(reps, var(R.R/N.tests)^0.5)
      se.VoR.R <- with(reps, var((R.R - T.R) %over% R.R)^0.5)
      se.ToM.R <- with(reps, var(T.R %over% M1)^0.5)
      gamma.R <- with(reps, mean(R.R)/N.tests)
      out <- c(out, average.power.R=average.power.R, gamma.R=gamma.R,
               se.Rom.R=se.Rom.R, se.VoR.R=se.VoR.R, se.ToM.R=se.ToM.R)
      if(do.tp.power)
      {
        tp.power.R <- with(reps, mean(T.R %over% M1 >= lambda))
        out$tp.power.R <- tp.power.R
      }
    }

    dtl <- list(reps=reps)
    X.i <- rslt$X.i
    M.i <- rslt$M.i
    rep.i <- cbind(X=X.i, xi=c(rep(1, M.i), rep(0, N.tests - M.i)))
    rep.i <- as.data.frame(cbind(rep.i, pval=2^(!is.pos)*(1-dists$pdist[[1+idistopt]](abs(X.i), pars0))))
    rep.i <- rep.i[order(rep.i$pval), ]
    rep.i$BHFDRcrit <- alpha*(1:N.tests)/N.tests
    rep.i$Rmnocrit <- (floor(alpha*(1:N.tests)) + 1)*alpha/(N.tests + floor(alpha*(1:N.tests)) + 1 -(1:N.tests))
    dtl$X <- rep.i
    attr(out, "detail") <- dtl
    attr(out, "pars") <- rbind(pars0=pars0,pars1=pars1)
    out$call <- .call.
    class(out) <- "pwr"
    out
}

"criterion" <-
function(alpha, delta, N.tests, FDP.control.method=c("BHFDR","Romano"))
{
    .call. <- m <- match.call()

    frmls <- names(formals(criterion))
    sppld <- names(m)[-1]

    is.msng <- !(frmls %in% sppld)
    names(is.msng) <- frmls

    err.msng <- any(c("alpha", "N.tests", "FDP.control.method") %in% names(is.msng[is.msng]))

    if(err.msng) stop("Arguments 'alpha', 'N.tests' and 'FDP.control.method' are required")
    
    if(is.msng["delta"]) delta <- alpha
    
    errs <- list(alpha=(alpha <= 0 || alpha >= 1),
                 delta=(delta <= 0 || delta >= 1),
                 N.tests=(!is.int(N.tests) || N.tests <= 0),
                 FDP.control.method=!(FDP.control.method %in% c("BHFDR", "Romano")))

    msgs <- list(alpha="Argument, 'alpha' must be between 0 and 1\n",
                 delta="Argument, 'delta' must be between 0 and 1\n",
                 N.tests="Argument, 'N.tests' must be integral and positive\n",
                 FDP.control.method="Argument, 'FDP.control.method' must be either 'BHFDR' or 'Romano'\n")
    
    err <- any(unlist(errs))
    msg <- paste(msgs[names(which(unlist(errs)))], collapse="")
    if(err) stop(msg)

    ii <- 1:m$N.tests
    
    if(FDP.control.method=="BHFDR") crit <- m$alpha*ii/m$N.tests
    if(FDP.control.method=="Romano") crit <- (floor(delta*ii) + 1)*alpha/(N.tests + floor(delta*ii) + 1 - ii)

    crit
}

"es.ROC" <-
function(FPR0, FPR1=NULL, TPR0, TPR1=NULL, b=NULL)
{
    m <- match.call()
    fn.nm <- as.character(m[[1]])
    is.FPR1 <- !missing(FPR1)
    is.TPR1 <- !missing(TPR1)
    if(!is.FPR1&!is.TPR1) stop("You must specify exactly one of the arguments 'FPR1' and 'TPR1'")
    if(is.TPR1) fn.nm <- fn.nm %,% ".fxFPR"
    if(is.FPR1) fn.nm <- fn.nm %,% ".fxTPR"
    m[[1]] <- as.name(fn.nm)
    eval(m)
}

"cc.ROC" <-
function(FPR0, FPR1=NULL, TPR0, TPR1=NULL, b=NULL)
{
    m <- match.call()
    fn.nm <- as.character(m[[1]])
    is.FPR1 <- !missing(FPR1)
    is.TPR1 <- !missing(TPR1)
    if(!is.FPR1&!is.TPR1) stop("You must specify exactly one of the arguments 'FPR1' and 'TPR1'")
    if(is.TPR1) fn.nm <- fn.nm %,% ".fxFPR"
    if(is.FPR1) fn.nm <- fn.nm %,% ".fxTPR"
    m[[1]] <- as.name(fn.nm)
    eval(m)
}

"es.ROC.fxTPR" <-
function(FPR0, FPR1, TPR0, b=NULL)
{
  if(missing(b)) b <- 1
  v.tpr0 <- TPR0*(1-TPR0)
  v.fpr1 <- FPR1*(1-FPR1)
  r <- b*dnorm(qnorm(1-TPR0))/dnorm(qnorm(1-FPR1))
  k <- (v.fpr1/v.tpr0)^0.5/r
#  k <- ceiling(k)
  nu <- (FPR1-FPR0)/(v.fpr1 + r^2/k*v.tpr0)^0.5
  abs(nu)
}

"cc.ROC.fxTPR" <-
function(FPR0, FPR1, TPR0, b=NULL)
{
  if(missing(b)) b <- 1
  v.tpr0 <- TPR0*(1-TPR0)
  v.fpr1 <- FPR1*(1-FPR1)
  r <- b*dnorm(qnorm(1-TPR0))/dnorm(qnorm(1-FPR1))
  k <- (v.fpr1/v.tpr0)^0.5/r
#  k <- ceiling(k)
  k
}

"es.ROC.fxFPR" <-
function(FPR0, TPR0, TPR1, b=NULL)
{
  if(missing(b)) b <- 1
  v.fpr0 <- FPR0*(1-FPR0)
  v.tpr1 <- TPR1*(1-TPR1)
  r <- b*dnorm(qnorm(TPR1))/dnorm(qnorm(FPR0))
  k <- (v.tpr1/v.fpr0)^0.5/r
#  k <- ceiling(k)
  nu <- (TPR1-TPR0)/(v.tpr1 + k*r^2*v.fpr0)^0.5
  abs(nu)
}

"cc.ROC.fxFPR" <-
function(FPR0, TPR0, TPR1, b=NULL)
{
  if(missing(b)) b <- 1
  v.fpr0 <- FPR0*(1-FPR0)
  v.tpr1 <- TPR1*(1-TPR1)
  r <- b*dnorm(qnorm(TPR1))/dnorm(qnorm(FPR0))
  k <- (v.tpr1/v.fpr0)^0.5/r
#  k <- ceiling(k)
  k
}

"CDF.Pval" <-
function(u, effect.size, n.sample, r.1, groups=2, type="balanced", grpj.per.grp1=1, control)
{
  .call. <- m <- match.call()
  pfx <- as.character(m[[1]])
    
  frmls <- names(formals(CDF.Pval))
  sppld <- names(m)[-1]

  m <- evald.call <- args.def.err.chk(m, sppld, frmls, other.rules=FALSE)
  err.msg <- attr(m, "err.msg")
    
  if(!is.null(err.msg)) stop(err.msg)

  u <- m$u
  groups <- m$groups
  r.1 <- m$r.1
  effect.size <- m$effect.size
  n.sample <- m$n.sample
  type <- m$type
  grpj.per.grp1 <- m$grpj.per.grp1
  control <- m$control
  
  nii.sample <- n.sample
  if(groups>=2 && type==3) nii.sample <- n.sample*grpj.per.grp1

  ## if(groups==1) type is 1 (paired)
  ## if(groups==2) type is 1 (paired), 2 (balanced) or 3 (unbalanced)
  ## if(groups>=3) type is 2 (balanced) or 3 (unbalanced)
  idistopt <- control$distopt
  .DF. <- eval(DF[type])
  .NCP. <- eval(NCP[type])
  
  pars0 <- eval(dists$pars0[[1+idistopt]])
  pars1 <- eval(dists$pars1[[1+idistopt]])
  
  u <- abs(u)
  is.pos <- (dists$minv[[1+idistopt]]==0)
  c.g <- dists$qdist[[1+idistopt]](1-u/2^(!is.pos), pars0)
  ans <- (1-r.1)*u + r.1*(1-dists$pdist[[1+idistopt]](c.g, pars1) + (!is.pos)*dists$pdist[[1+idistopt]](-c.g, pars1))
  df <- as.data.frame(list(u=u, CDF.Pval=ans))
  out <- list(CDF.Pval=df, call=.call.)
  attr(out, "pars") <- rbind(pars0=pars0,pars1=pars1)
  
  class(out) <- "cdf"
  out
}

"CDF.Pval.HA" <-
function(u, effect.size, n.sample, r.1, groups=2, type="balanced", grpj.per.grp1=1, control)
{
  .call. <- m <- match.call()
  pfx <- as.character(m[[1]])
    
  frmls <- names(formals(CDF.Pval.HA))
  sppld <- names(m)[-1]

  m <- evald.call <- args.def.err.chk(m, sppld, frmls, other.rules=FALSE)
  err.msg <- attr(m, "err.msg")
    
  if(!is.null(err.msg)) stop(err.msg)

  u <- m$u
  groups <- m$groups
  r.1 <- m$r.1
  effect.size <- m$effect.size
  n.sample <- m$n.sample
  type <- m$type
  grpj.per.grp1 <- m$grpj.per.grp1
  control <- m$control

  nii.sample <- n.sample
  if(groups>=2 && type==3) nii.sample <- n.sample*grpj.per.grp1

  ## if(groups==1) type is 1 (paired)
  ## if(groups==2) type is 1 (paired), 2 (balanced) or 3 (unbalanced)
  ## if(groups>=3) type is 2 (balanced) or 3 (unbalanced)
  idistopt <- control$distopt  
  .DF. <- eval(DF[type])
  .NCP. <- eval(NCP[type])
  
  pars0 <- eval(dists$pars0[[1+idistopt]])
  pars1 <- eval(dists$pars1[[1+idistopt]])

  u <- abs(u)

  is.pos <- (dists$minv[[1+idistopt]]==0)
  c.g <- dists$qdist[[1+idistopt]](1-u/2^(!is.pos), pars0)
  ans <- 1-dists$pdist[[1+idistopt]](c.g, pars1) + (!is.pos)*dists$pdist[[1+idistopt]](-c.g, pars1)
  df <- as.data.frame(list(u=u, CDF.Pval.HA=ans))
  out <- list(CDF.Pval.HA=df, call=.call.)
  attr(out, "pars") <- rbind(pars0=pars0,pars1=pars1)
  
  class(out) <- "cdf"
  out  
}

"CDF.Pval.ua.eq.u" <-
function(effect.size, n.sample, r.1, alpha, groups, type, grpj.per.grp1, control)
{
    calling.env <- sys.parent()

    eval.env <- ifelse(calling.env==0, topenv, sys.parent)    
    m <- .call. <- match.call()

    n.args <- length(m)-1
    arg.nms <- names(m)[-1]

    is.rqd <- all(nera %in% arg.nms)
    if(!is.rqd) stop(list.a(nera, "Arguments ", " are required by CDF.Pval.ua.eq.u"))

    if(missing(groups)) m$groups <- 2
    if(missing(type)) m$type <- 2
    if(missing(grpj.per.grp1)) m$grpj.per.grp1 <- 1
    
    if(missing(control)) m$control <- list(distopt=1, tol=1e-8)
    
    alpha <- eval(m$alpha, eval.env())
    groups <- eval(m$groups, eval.env())
    r.1 <- eval(m$r.1, eval.env())
    effect.size <- eval(m$effect.size, eval.env())
    n.sample <- eval(m$n.sample, eval.env())
    type <- eval(m$type, eval.env())
    grpj.per.grp1 <- eval(m$grpj.per.grp1, eval.env())
    control <- eval(m$control, eval.env())
        
    idistopt <- control$distopt

    nii.sample <- n.sample
    if(groups>=2 && type==3) nii.sample <- n.sample*grpj.per.grp1

    ## if(groups==1) type is 1 (paired)
    ## if(groups==2) type is 1 (paired), 2 (balanced) or 3 (unbalanced)
    ## if(groups>=3) type is 2 (balanced) or 3 (unbalanced)
    .DF. <- eval(DF[type])
    .NCP. <- eval(NCP[type])

    pars0 <- eval(dists$pars0[[1+idistopt]])
    pars1 <- eval(dists$pars1[[1+idistopt]])

    is.pos <- (dists$minv[[1+idistopt]]==0)
    
    conv <- is.na(alpha)
    gma.old <- r.1
    gma.new <- 1
    
    while(!conv)
    {
       u <- gma.old*alpha
       c.g <- dists$qdist[[1+idistopt]](1-u/2^(!is.pos), pars0)
       gma.new <- (1-r.1)*u + r.1*(1-dists$pdist[[1+idistopt]](c.g, pars1) +
                                       (!is.pos)*dists$pdist[[1+idistopt]](-c.g, pars1))
       obj <- abs(gma.new-gma.old)
       conv <- (obj < control$tol)
       gma.old <- gma.new
    }
    gamma <- gma.new
    out <- list(gamma=gamma, call=.call.)
    attr(out, "pars") <- rbind(pars0=pars0,pars1=pars1)
    
    class(out) <- "cdf"
    out
}

"CDF.Pval.ar.eq.u" <-
function(effect.size, n.sample, r.1, alpha, delta, groups, type, grpj.per.grp1, control)
{
    calling.env <- sys.parent()

    eval.env <- ifelse(calling.env==0, topenv, sys.parent)    
    m <- .call. <- match.call()

    n.args <- length(m)-1
    arg.nms <- names(m)[-1]

    is.rqd <- all(nera %in% arg.nms)
    if(!is.rqd) stop(list.a(nera, "Arguments ", " are required by CDF.Pval.ua.eq.u"))
    
    if(missing(groups)) m$groups <- 2
    if(missing(delta)) m$delta <- alpha
    if(missing(type)) m$type <- 2
    if(missing(grpj.per.grp1)) m$grpj.per.grp1 <- 1
    
    if(missing(control)) m$control <- list(distopt=1, tol=1e-8)
    
    alpha <- eval(m$alpha, eval.env())
    delta <- eval(m$delta, eval.env())
    groups <- eval(m$groups, eval.env())
    r.1 <- eval(m$r.1, eval.env())
    effect.size <- eval(m$effect.size, eval.env())
    n.sample <- eval(m$n.sample, eval.env())
    type <- eval(m$type, eval.env())
    grpj.per.grp1 <- eval(m$grpj.per.grp1, eval.env())
    control <- eval(m$control, eval.env())
    
    idistopt <- control$distopt
    
    nii.sample <- n.sample
    if(groups>=2 && type==3) nii.sample <- n.sample*grpj.per.grp1

    ## if(groups==1) type is 1 (paired)
    ## if(groups==2) type is 1 (paired), 2 (balanced) or 3 (unbalanced)
    ## if(groups>=3) type is 2 (balanced) or 3 (unbalanced)
    .DF. <- eval(DF[type])
    .NCP. <- eval(NCP[type])
    
    pars0 <- eval(dists$pars0[[1+idistopt]])
    pars1 <- eval(dists$pars1[[1+idistopt]])
  
    is.pos <- (dists$minv[[1+idistopt]]==0)
    
    conv <- is.na(alpha)
    gma.old <- r.1
    gma.new <- 1
    psi <- function(gma,a)gma*a/(1-(1-a)*gma)
    while(!conv)
    {
        u <- psi(gma.old, delta)*alpha
        c.g <- dists$qdist[[1+idistopt]](1-u/2^(!is.pos), pars0)
        gma.new <- (1-r.1)*u + r.1*(1-dists$pdist[[1+idistopt]](c.g, pars1) +
                                                  (!is.pos)*dists$pdist[[1+idistopt]](-c.g, pars1))
        obj <- abs(gma.new-gma.old)
        conv <- (obj < control$tol)
        gma.old <- gma.new
    }
    gamma <- gma.new
    out <- list(gamma=gamma, call=.call.)
    attr(out, "pars") <- rbind(pars0=pars0,pars1=pars1)
    
    class(out) <- "cdf"
    out
}

"sd.rtm.Rom" <-
function(object) 
{
  fn.nm <- as.character(object$call[[1]])
  frml.vals <- formals(get(fn.nm))
  frmls <- names(frml.vals)
  sppld <- names(object$call)[-1]
  is.msng <- !(frmls %in% sppld)
  names(is.msng) <- frmls
  if(is.msng["method"]) object$call$method <- "FixedPoint"
  method <- match.arg(object$call$method, eval(frml.vals$method))
  sfx <- NULL
  if(method=="simulation")
  {
    fdpsfx <- c(`BHFDR`="",`BHCLT`=".st", `Romano`=".R")
    if(is.msng["FDP.control.method"]) object$call$FDP.control.method <- "BHFDR"
    sfx <- fdpsfx[object$call$FDP.control.method]
  }
  if(!is.msng["N.tests"]) ans <- object[["se.Rom" %,% sfx]]*eval(object$call$N.tests)^0.5
  if(is.msng["N.tests"]) ans <- object[["sigma.rtm.Rom" %,% sfx]]
  ans
}

"sd.rtm.VoR" <-
function(object)
{
  fn.nm <- as.character(object$call[[1]])
  frml.vals <- formals(get(fn.nm))
  frmls <- names(frml.vals)
  sppld <- names(object$call)[-1]
  is.msng <- !(frmls %in% sppld)
  names(is.msng) <- frmls
  if(is.msng["method"]) object$call$method <- "FixedPoint"
  method <- match.arg(object$call$method, eval(frml.vals$method))
  sfx <- NULL
  if(method=="simulation")
  {
    fdpsfx <- c(`BHFDR`="",`BHCLT`=".st", `Romano`=".R")
    if(is.msng["FDP.control.method"]) object$call$FDP.control.method <- "BHFDR"
    sfx <- fdpsfx[object$call$FDP.control.method]
  }
  if(!is.msng["N.tests"]) ans <- object[["se.VoR" %,% sfx]]*eval(object$call$N.tests)^0.5
  if(is.msng["N.tests"]) ans <- object[["sigma.rtm.VoR" %,% sfx]]
  ans
}

"sd.rtm.ToM" <-
function(object)
{
  fn.nm <- as.character(object$call[[1]])
  frml.vals <- formals(get(fn.nm))
  frmls <- names(frml.vals)
  sppld <- names(object$call)[-1]
  is.msng <- !(frmls %in% sppld)
  names(is.msng) <- frmls
  if(is.msng["method"]) object$call$method <- "FixedPoint"
  method <- match.arg(object$call$method, eval(frml.vals$method))
  sfx <- NULL
  if(method=="simulation")
  {
    fdpsfx <- c(`BHFDR`="",`BHCLT`=".st", `Romano`=".R")
    if(is.msng["FDP.control.method"]) object$call$FDP.control.method <- "BHFDR"
    sfx <- fdpsfx[object$call$FDP.control.method]
  }
  if(!is.msng["N.tests"]) ans <- object[["se.ToM" %,% sfx]]*eval(object$call$N.tests)^0.5
  if(is.msng["N.tests"]) ans <- object[["sigma.rtm.ToM" %,% sfx]]
  ans
}

# idea.
#    P( V_m / R_m  > \lambda )
#  = P( m^0.5 ( V_m / R_m - (1-r)alpha )/v^0.5  > m^0.5(\lambda - (1-r)alpha)/(v/m)^0.5
#  = 1 - pnorm(m^0.5(\lambda - (1-r)alpha)/v^0.5)
#
# if you want to control the tail probability instead of the expected value, set the above equal to (1-r) alpha
# and invert for \lambda, obtaining

# (1-r)alpha =  1 - pnorm(m^0.5(\lambda - (1-r)alpha)/v^0.5)
# pnorm(m^0.5(\lambda - (1-r)alpha)/v^0.5) = 1-(1-r)alpha
# m^0.5(\lambda - (1-r)alpha)/v^0.5 = qnorm(1-(1-r)alpha)
# \lambda_{r,alpha} = (1-r)alpha + (v/m)^0.5 qnorm(1-(1-r)alpha)

# so BH-FDR procedure at FDR=alpha bounds the probability that V_m / R_m  is in excess of \lambda_{r, alpha}
# by (1-r) alpha. This is good to have the probability bounded instead of the expected value, but what
# if \lambda_{r,alpha} is unacceptably large. For alpha < 1/2, \lambda_{r,alpha} > alpha 

# how about finding alpha* so that for v=v(alpha*)

# (1-r)alpha = \lambda_{r,alpha*} = (1-r)alpha* + (v(alpha*)/m)^0.5 qnorm(1-(1-r)alpha*)


# algorithm
# 1. consider r, theta=effect.size, n.sample, known
# 2. stipulate alpha
# 3. find alpha* such that:  (1-r)alpha = (1-r)alpha* + (v(alpha*)/m)^0.5 qnorm(1-(1-r)alpha*)


"controlFDP" <-
function(effect.size, n.sample, r.1, alpha, delta, groups=2, N.tests, 
         type, grpj.per.grp1, control, formula, data)
{
    calling.env <- sys.parent()
    eval.env <- ifelse(calling.env==0, topenv, sys.parent)
    m <- .call. <- match.call()

    m.sv <- m
    n.args <- length(m)-1
    arg.nms <- names(m)[-1]

    is.form <- !missing(formula)
    if(is.form)
    {
      m[[1]] <- as.name("controlFDP.formula")  
      dfa <- c("data","formula","alpha")
      is.rqd <- all(dfa %in% arg.nms)
      if(!is.rqd) stop(list.a(dfa, "Aguments ", " are required by the formula " %,%
                                                "'controlFDP' method "))
      if(missing(delta)) m$delta <- alpha
      if(missing(control)) m$control <- list(distopt=1, verb=0, tol=1e-8)
    }
    if(!is.form)
    {
      m[[1]] <- as.name("controlFDP.theoret")
      is.rqd <- all(Nnera %in% arg.nms)
      if(!is.rqd) stop(list.a(Nnera, "Arguments ", " are required by the asymptotic " %,%
                                                   "'controlFDP' method"))
      if(missing(delta)) m$delta <- alpha
      if(missing(groups)) m$groups <- 2
      if(missing(type)) m$type <- 2
      if(missing(grpj.per.grp1)) m$grpj.per.grp1 <- 1
      if(missing(control)) m$control <- list(distopt=1, verb=0, tol=1e-8)
    }
  
    out <- eval(m)
    out$call <- .call.
    class(out) <- "vvv"
    out   
}

"controlFDP.formula" <-
function(effect.size, n.sample, r.1, alpha, delta, groups=2, N.tests, 
         type, grpj.per.grp1, control, formula, data)
{
    .call. <- m <- match.call()

    calling.env <- sys.parent()
    eval.env <- ifelse(calling.env==0, topenv, sys.parent)

    mf <- as.call(expression(model.frame))
    mf$groups <- mf$effect.size <- mf$n.sample <- mf$r.1 <- mf$N.tests <- mf$control <- mf$delta <- NULL
    mf$formula <- formula
    mf$data <- data
    mf <- eval(mf, eval.env())
    pval <- model.extract(mf, "response")
    N.tests <- length(pval)
    pvalgthlf <- 1*(pval > 0.5)
    r.0 <- 2*mean(pvalgthlf)
    r.1 <- 1-r.0

    conv <- neg <- FALSE
    msg <- NULL
    ast.old <- alpha
    a.star <- gma.st <- obj <- sqrt.v <- P.star <- NA

    while(!(conv||neg))
    {
        gma <- max(which(pval <= (1:N.tests)/N.tests*ast.old))/N.tests
        v <- r.0*ast.old*(1-r.0*ast.old*gma)/(N.tests*gma)
        ast.new <- (delta - v^0.5*qnorm(1-alpha))/r.0
        neg <- ast.new < 0 
        if(control$verb>1) cat(sprintf("ast.new=%f\n", ast.new))
        obj <- abs(ast.new-ast.old)
        conv <- (obj < 1e-4*alpha)
        ast.old <- ast.new
    }
    if(neg)
    {
        a.star <- gma.st <- obj <- sqrt.v <- P.star <- NA
        msg <- "Solution not attainable."
    }
    if(!neg)
    {
        a.star <- ast.new
        gma.st <- max(which(pval <= (1:N.tests)/N.tests*ast.old))/N.tests
        v <- r.0*a.star*(1-r.0*a.star*gma.st)/(N.tests*gma.st)
        sqrt.v <- v^0.5
        P.star <- 1-pnorm((delta - r.0*a.star)/sqrt.v)
    }
    out <- list(alpha.star = a.star, gamma=gma.st, obj = obj, se.FDF=sqrt.v, P.star = P.star)
    if(!is.null(msg)) out$note <- msg
    out
}

"controlFDP.theoret" <-
function(effect.size, n.sample, r.1, alpha, delta, groups=2, N.tests, 
         type, grpj.per.grp1, control, formula, data)
{
    .call. <- m <- match.call()

    calling.env <- sys.parent()
    eval.env <- ifelse(calling.env==0, topenv, sys.parent)

    ast.old <- alpha
    conv <- neg <- FALSE
    msg <- NULL
    a.star <- gma.st <- obj <- sqrt.v <- P.star <- NA

    gma.call <- match.call()
    gma.call[[1]] <- as.name("CDF.Pval.ua.eq.u")
    
    gma.call$formula <- gma.call$data <- gma.call$N.tests <- gma.call$delta <- NULL
    gma0 <- eval(gma.call)
    r.0 <- 1-r.1

    while(!(conv || neg))
    {
        gma.st <- update(gma0, alpha=ast.old)$gamma
        v <- r.0*ast.old*(1-r.0*ast.old*gma.st)/(N.tests*gma.st)
        ast.new <- (delta - v^0.5*qnorm(1 - alpha))/r.0
        neg <- ast.new < 0 
        if(control$verb>1) cat(sprintf("ast.new=%f\n", ast.new))
        obj <- abs(ast.new-ast.old)
        conv <- (obj < 1e-4*alpha)
        ast.old <- ast.new
    }
    if(neg)
    {
        a.star <- gma.st <- obj <- sqrt.v <- P.star <- NA
        msg <- "Solution not attainable."
    }
    if(!neg)
    {
        a.star <- ast.new
        gma.st <- update(gma0, alpha=a.star)$gamma
        v <- r.0*a.star*(1-r.0*a.star*gma.st)/(N.tests*gma.st)
        sqrt.v <- v^0.5
        P.star <- 1-pnorm((delta - r.0*a.star)/sqrt.v)
    }
    out <- list(alpha.star = a.star, gamma=gma.st, obj = obj, se.FDF=sqrt.v, P.star = P.star)
    if(!is.null(msg)) out$note <- msg
    out
}

"cCDF.Rom" <-
function(u, effect.size, n.sample, r.1, alpha, delta, groups=2, N.tests,
         type=c("paired","balanced","unbalanced"), grpj.per.grp1=NULL, FDP.control.method="BHFDR", 
         control=list(tol=1e-8, max.iter=c(1000,20), distopt=1, CS=list(NULL), sim.level=2,
                      low.power.stop=TRUE, FDP.meth.thresh=FDP.cntl.mth.thrsh.def, verb=FALSE))
{
  m <- .call. <- match.call()
  m[[1]] <- as.name("pwrFDR")
  m$u <- NULL  
  avgpwr <- eval(m, sys.parent())
  gma <- avgpwr$gamma
  sdrtmRom <- sd.rtm.Rom(avgpwr)
  ans <- 1-pnorm(N.tests^0.5*(u - gma)/sdrtmRom)
  df <- as.data.frame(list(u=u, cCDF.Rom=ans))
  out <- list(cCDF.Rom=df, call=.call.)
  class(out) <- "cdf"
  out
}

"cCDF.ToM" <-
function(u, effect.size, n.sample, r.1, alpha, delta, groups=2, N.tests,
         type=c("paired","balanced","unbalanced"), grpj.per.grp1=NULL, FDP.control.method="BHFDR", 
         control=list(tol=1e-8, max.iter=c(1000,20), distopt=1, CS=list(NULL), sim.level=2,
                      low.power.stop=TRUE, FDP.meth.thresh=FDP.cntl.mth.thrsh.def, verb=FALSE))
{
  m <- .call. <- match.call()
  m[[1]] <- as.name("pwrFDR")
  m$u <- NULL  
  avgpwr <- eval(m, sys.parent())
  pi <- avgpwr$average.power
  sdrtmToM <- sd.rtm.ToM(avgpwr)
  ans <- 1-pnorm(N.tests^0.5*(u - pi)/sdrtmToM)
  df <- as.data.frame(list(lambda=u, cCDF.ToM=ans))
  out <- list(cCDF.ToM=df, call=.call.)
  class(out) <- "cdf"
  out
}

"cCDF.VoR" <-
function(u, effect.size, n.sample, r.1, alpha, delta, groups=2, N.tests, 
         type=c("paired","balanced","unbalanced"), grpj.per.grp1=NULL, FDP.control.method="BHFDR", 
         control=list(tol=1e-8, max.iter=c(1000,20), distopt=1, CS=list(NULL), sim.level=2,
                      low.power.stop=TRUE, FDP.meth.thresh=FDP.cntl.mth.thrsh.def, verb=FALSE))
{
  m <- .call. <- match.call()
  m[[1]] <- as.name("pwrFDR")
  m$u <- NULL  
  avgpwr <- eval(m, sys.parent())
  sdrtmVoR <- sd.rtm.VoR(avgpwr)
  r.0 <- 1-r.1
  do.bhclt <- do.R <- FALSE
  if(FDP.control.method=="BHCLT") do.bhclt <- TRUE
  if(FDP.control.method=="Romano") do.R <- TRUE
  if(FDP.control.method=="Auto")
  {
      if(avgpwr$Auto=="BHCLT") do.bhclt <- TRUE
      if(avgpwr$Auto=="Romano") do.R <- TRUE
  }
  alpha. <- alpha
  psi.o.gma <- 1
  if((do.bhclt || do.R) && missing(delta)) delta <- attr(avgpwr, "arg.vals")$delta
  if(do.bhclt) alpha. <- avgpwr$alpha.star
  if(do.R)
  {
      psi <- function(gma,a)gma*a/(1-(1-a)*gma)
      gma <- avgpwr$gamma
      .psi. <- psi(gma, delta)
      psi.o.gma <- .psi./gma
  }
  ans <- 1-pnorm(N.tests^0.5*(u - r.0*psi.o.gma*alpha.)/sdrtmVoR)
  df <- as.data.frame(list(delta=u, cCDF.VoR=ans))
  out <- list(cCDF.VoR=df, call=.call.)
  class(out) <- "cdf"
  out
}

"detail" <-
function(obj)
{
  attr(obj, "detail")
}

"%,%" <- function(x,y)paste(x,y,sep="")
"DX" <- function(x)c(x[1],diff(x))
  
"ddist" <- 
function(x, pars)
{
  idistopt <- pars[1]
  dists$ddist[[1+idistopt]](x, pars)
}

"pdist" <-
function(x, pars)
{
  idistopt <- pars[1]
  dists$pdist[[1+idistopt]](x, pars)
}

"qdist" <-
function(x, pars)
{
  idistopt <- pars[1]
  dists$qdist[[1+idistopt]](x, pars)
}

"gentempfilenm" <-
function(prfx="temp", sfx=".txt")
{
    alphanum <- c(letters, toupper(letters), 0:9)
    n <- length(alphanum)
    prfx %,% paste(alphanum[sample(n, 5)], collapse="") %,% sfx
}

"print.pwr" <- 
function (x, ...) 
{
    y <- x
    cat("Call:\n")
    print(x$call)
    class(x) <- NULL
    x$call <- NULL
    x <- as.data.frame(x)
    dimnames(x)[[1]] <- " "
    out <- t(format(x))
    print(out, quote=FALSE)
    invisible(x)
}

"print.vvv" <- 
function (x, ...) 
{
    y <- x
    cat("Call:\n")
    print(x$call)
    class(x) <- NULL
    x$call <- NULL
    x <- as.data.frame(x)
    dimnames(x)[[1]] <- " "
    print(x)
    invisible(x)
}

"print.cdf" <-
function(x, ...)
{
    y <- x
    cat("Call:\n")
    print(x$call)
    class(x) <- NULL
    x$call <- NULL
    print(x[[1]])
    invisible(x)    
}

"is.int" <-
function(x)
{
  abs(x - floor(x)) < 1e-10
}

`+.pwr` <-
  function(x,y)
  {
    xpwr <- x
    ypwr <- y
    if(is(x,"pwr")) xpwr <- ifelse(!is.null(x$call$lambda), x$tp.power, x$average.power)
    if(is(y,"pwr")) ypwr <- ifelse(!is.null(y$call$lambda), y$tp.power, y$average.power)
    xpwr + ypwr
  }

`-.pwr` <-
  function(x,y)
  {
    xpwr <- x
    ypwr <- y
    if(is(x,"pwr")) xpwr <- ifelse(!is.null(x$call$lambda), x$tp.power, x$average.power)
    if(is(y,"pwr")) ypwr <- ifelse(!is.null(y$call$lambda), y$tp.power, y$average.power)
    xpwr - ypwr
  }

`*.pwr` <-
  function(x,y)
  {
    xpwr <- x
    ypwr <- y
    if(is(x,"pwr")) xpwr <- ifelse(!is.null(x$call$lambda), x$tp.power, x$average.power)
    if(is(y,"pwr")) ypwr <- ifelse(!is.null(y$call$lambda), y$tp.power, y$average.power)
    xpwr * ypwr
  }

`/.pwr` <-
  function(x,y)
  {
    xpwr <- x
    ypwr <- y
    if(is(x,"pwr")) xpwr <- ifelse(!is.null(x$call$lambda), x$tp.power, x$average.power)
    if(is(y,"pwr")) ypwr <- ifelse(!is.null(y$call$lambda), y$tp.power, y$average.power)
    xpwr / ypwr
  }

`^.pwr` <-
  function(x,y)
  {
    xpwr <- x
    ypwr <- y
    if(is(x,"pwr")) xpwr <- ifelse(!is.null(x$call$lambda), x$tp.power, x$average.power)
    if(is(y,"pwr")) ypwr <- ifelse(!is.null(y$call$lambda), y$tp.power, y$average.power)
    xpwr ^ ypwr
  }

exp.pwr <- function(x) exp(ifelse(!is.null(x$call$lambda), x$tp.power, x$average.power))
log.pwr <- function(x, base)log(ifelse(!is.null(x$call$lambda), x$tp.power, x$average.power), base)
logit.inv.r <- function(x)binomial("logit")$linkinv(max(min(x, 1e8), -1e8))
logitInv.default <- binomial("logit")$linkinv
logit.default <- binomial("logit")$linkfun
logit <- function(mu)UseMethod("logit")
logitInv <- function(eta)UseMethod("logitInv")
logit.pwr <- function(mu)logit.default(ifelse(!is.null(mu$call$lambda), mu$tp.power, mu$average.power))
logitInv.pwr <- function(eta)logitInv.default(ifelse(!is.null(eta$call$lambda), eta$tp.power, eta$average.power))
arg.vals <- function(object)attr(object, "arg.vals")
"%over%" <-
function(x,y)
{
  dx <- dim(x)
  dy <- dim(y)
  bad <- any(dx!=dy) && dx[1]!=length(y)
  if(bad) stop("Incompatable dimensions")
  ans <- 0*x
  bad <- c(which(y==0),which(is.na(y)))
  if(length(bad)>0) ans[-bad] <- x[-bad]/y[-bad]
  if(length(bad)==0) ans <- x/y
  structure(ans, dim=dx)
}

##  Sample size and power for omnibus F-test, 'groups' groups
##
##  H_0  mu_j - mu_avg = 0
##  H_A  mu_j = mu_avg = -theta/2, 0, ..., 0, theta/2
##
##  NOTE: this will be the case (H_A) when
##  mu = (mu_0, mu_0 + theta/2, ..., mu_0 + theta/2, mu_0 + theta)
##  In this case, the ncp = n*(mu - mu_avg)^2/2 = n*theta^2/2
##  which is the same as the square of the two sample t-test.

"f.power" <-
function(n.sample, groups, effect.size, e.I, power)
{
  no.n <- missing(n.sample)
  no.g <- missing(groups)
  no.e <- missing(effect.size)
  no.p <- missing(power)
  if(no.g || no.e) stop("Arguements 'groups' and 'effect.size' are required")
  bad <- (no.n && no.p) || (!no.n && !no.p)
  if(bad) stop("You must specify _either_ 'n.sample' or 'power'")
  do.p <- no.p
  do.ss <- no.n
  if(do.ss)
  {
    pwr <- power    
    OBJ <-
    function(x, groups, effect.size, e.I, power)
    {
      n.sample <- exp(x)
      ncp <- n.sample*effect.size^2/2
      df1 <- groups - 1
      df2 <- groups*(n.sample-1)
      Fcrit <- qf(1-e.I, df1=df1, df2=df2)
      ((pwr - (1-pf(Fcrit, ncp=ncp, df1=df1, df2=df2)))^2)^(1/1.25)
    }
    norm.ss <- 2*(qnorm(power) + qnorm(1-e.I))^2/effect.size^2
    l <- log(1/10*norm.ss)
    u <- log(10*norm.ss)
    opt <- optimize(f=OBJ, lower=l, upper=u, groups=groups, effect.size=effect.size, e.I=e.I, power=power)
    n.sample <- round(exp(opt$minimum))
    obj <- opt$objective
  }
  if(do.p)
  {
    obj <- 0
    ncp <- n.sample*effect.size^2/2
    df1 <- groups - 1
    df2 <- groups*(n.sample-1)
    Fcrit <- qf(1-e.I, df1=df1, df2=df2)
    pwr <- 1-pf(Fcrit, ncp=ncp, df1=df1, df2=df2)
  }
  ncp <- n.sample*effect.size^2/2
  df1 <- groups - 1
  df2 <- groups*(n.sample-1)
  Fcrit <- qf(1-e.I, df1=df1, df2=df2)
  out <- list(power=pwr, n.sample=n.sample, Fcrit=Fcrit, ncp=ncp, df1=df1, df2=df2, objective=obj)
  as.data.frame(out)
}

power.bonferroni <-
function(alpha, effect.size, n.pergroup, x, prior.HA, N.tests, groups=2, method=c("a","s"), n.sim=1000)
{
    bad <- missing(alpha) || missing(effect.size) || missing(n.pergroup) || missing(x) ||
        missing(prior.HA) || missing(N.tests)
    if(bad) stop("Arguments 'alpha', 'effect.size', 'n.pergroup', 'x', 'prior.HA', 'N.tests' are required")
    theta <- effect.size
    if(missing(method)) method <- "a"
    if(missing(groups)) groups <- 2
    if(groups<2) stop("groups must be 2 or larger")
    if(theta<=0) stop("Effect size must be positive")
    if(missing(n.sim) && method=="s") n.sim <- 1000
    m <- N.tests
    n <- n.pergroup
    r <- prior.HA
    d <- groups
    mu <- theta*(n/2)^0.5
    ncp <- mu^2
    if(method=="a")
    {
      if(d==2) p <- 1-pnorm(qnorm(1-alpha/(2*m))-mu)
      if(d>2) p <- f.power(n.sample=n, groups=d, effect.size=theta, e.I=alpha/(2*m))$power
      s <- 0
      for(k in x:m)
        s <- s + choose(m,k)*r^k*(1-r)^(m-k)*(1-pbinom(x-1, prob=p, size=k))
      ans <- s
    }
    if(method=="s")
    {
      s <- 0
      cMs <- list(mean, colMeans)[[1 + 1*(d>2)]]
      for(ii in 1:n.sim)
      {
        rej <- 0
        for(k in 1:m)
        {
          xi <- 1*(runif(1)<=r)
          X <- matrix(rnorm(n*(d-1), mean=xi*theta, sd=1), n, d-1)
          X0 <- rnorm(n, mean=0, sd=1)
          if(d==2) rej <- rej + 1*xi*(n^0.5*(mean(X) - mean(X0))/(var(c(X)) + var(X0))^0.5>qnorm(1-alpha/(2*m)))
          if(d>2)
          {
            SST <- (n*d-1)*var(c(X,X0))
            SSW <- sum((n-1)*apply(cbind(X,X0), 2, var))
            FF <- ((SST-SSW)/(d-1))/(SSW/(d*(n-1)))
            rej <- rej + 1*xi*(FF>qf(1-alpha/(2*m), df1=d-1, df2=d*(n-1)))
          }
        }
        s <- s + 1*(rej>=x)
      }
      ans <- s/n.sim
    }
    ans
}

"view.presentation" <-
function()
{
    n <- grep("pdf", names(options()))
    system(options()[[n]] %,%  " $R_HOME/library/pwrFDR/doc/2021-01-25-NWU-Biostat-Talk.pdf 2> /dev/null &")
}

"factorial.design" <-
function(...)
{
    m <- match.call()
    cc <- m
    cc[[1]] <- as.name("c")
    arg.vals <- eval(cc, sys.parent())
    n.args <- length(arg.vals)
    
    n.l <- 0
    n.r <- n.args-1
    fwd.cpd <- c(1, cumprod(arg.vals))
    rev.cpd <- c(cumprod(arg.vals[n.args:2])[(n.args-1):1],1)
    "%,%" <- paste0
    main.call <- as.call(expression(cbind))
    for(k in 1:n.args)
    {
        arg.k <- eval(m[[1+k]], sys.parent())
        rep.call <- as.call(expression(rep))
        rep.call$x <- 1:arg.k
        if(n.l>0) rep.call$times <- fwd.cpd[k]
        if(n.r>0) rep.call$each <- rev.cpd[k]
        main.call[["x" %,% k]] <- rep.call
        n.l <- n.l + 1
        n.r <- n.r - 1
    }
    eval(main.call)
}

"nna" <-
function(x)
{
  par.env.nms <- ls.str(, envir=parent.frame())
  m <- match.call()
  vnm <- as.character(m[[2]])
  exts <- vnm %in% par.env.nms
  n <- length(get(par.env.nms[1], envir=parent.frame()))
  ans <- rep(NA, n)
  if(exts) ans <- get(vnm, envir=parent.frame())
  ans
}

is.formula <- function(x)class(x)=="formula"

"if.na.x" <- function(x, x0=FALSE)
{
  d.x <- dim(x)
  x. <- c(as.matrix(x))
  y <- rep(x0, length(x.))
  y[!is.na(x.)] <- x.[!is.na(x.)]
  structure(y, dim=d.x)
}

if.y.z <-
function(x, y=0,z=1)
{
  ans <- x
  ans[x==y] <- z
  ans
}

if.0.rm <- function(x)x[x!=0]


#######################################################################
## argument checking/default argument block                          ##
#######################################################################
pwrfdrmthd.sfx <- c(`FixedPoint`="FP", `simulation`="sim")
pwrfdrmthd.vals <- names(pwrfdrmthd.sfx)
FDP.cntl.mth.thrsh.def <- c(0.10, 50)
FDP.mthd.vals <- c("BHFDR","BHCLT","Romano","Auto","both")
type.vals <- c("paired", "balanced", "unbalanced")
"is.int" <- function(x)(floor(x)==x)
"cma.and" <- function(s){n.s <- length(s); c(rep(", ", n.s-2), " and ", "")}
"cma.or" <- function(s){n.s <- length(s); c(rep(", ", n.s-2), " or ", "")}
"list.a" <-
function(lst, pfx=NULL, sfx=NULL)
  pfx %,% paste(c(t(cbind("\'" %,% lst %,% "\'", cma.and(lst)))), collapse="") %,% sfx
"list.o" <-
function(lst, pfx=NULL, sfx=NULL)
  pfx %,% paste(c(t(cbind("\'" %,% lst %,% "\'", cma.or(lst)))), collapse="") %,% sfx

nera <- c("n.sample", "effect.size", "r.1", "alpha")
Nnera <- c(nera, "N.tests")
dNnera <- c("data", "formula", "alpha")
default <- list(u=function(...)seq(from=0,to=1,len=100),
                alpha=function(...)NA,
                groups=function(...)2,
                control=function(groups=groups, alpha=alpha)
                    switch(1*(groups<=2)+2*(groups>2),
                               list(tol=1e-8, max.iter=c(1000,20), distopt=1, CS=list(NULL),
                                    sim.level=2, FDP.meth.thresh=FDP.cntl.mth.thrsh.def,
                                    verb=FALSE, low.power.stop=TRUE, ast.le.a=TRUE),
                               list(tol=1e-8, max.iter=c(1000,20), distopt=2, CS=list(NULL),
                                    sim.level=2, FDP.meth.thresh=FDP.cntl.mth.thrsh.def,
                                    verb=FALSE, low.power.stop=TRUE, ast.le.a=TRUE)),
                type=function(groups=groups,alpha=alpha)
                    c("paired","balanced")[min(max(floor(eval(groups)),1),2)],
                grpj.per.grp1=function(...)1,
                method=function(...)"FixedPoint",
                delta=function(groups=groups, alpha=alpha)alpha)

valid.arg.dict <- list(u=function(x)all(x>=0&x<=1),
                       groups=function(x)is.int(x)&(x>0),
                       effect.size=function(x)(x>0),
                       n.sample=function(x)is.int(x)&(x>0),
                       r.1=function(x)(x>0&x<1),
                       alpha=function(x){if(is.na(x)) x <- -1; (x>0 & x < 1)},
                       N.tests=function(x)is.int(x)&(x>0),
                       average.power=function(x)(x>0.5&x<1),
                       tp.power=function(x)(x>0.5&x<1),
                       lambda=function(x)(x>0&x<1),
                       FDP.control.method=function(x, vals=FDP.mthd.vals)
                       {
                           fdp.mthd <- try(match.arg(x, vals))
                           class(fdp.mthd)!="try-error"
                       },
                       method=function(x, vals=pwrfdrmthd.vals)
                       {
                           mthd <- try(match.arg(x, vals))
                           class(mthd)!="try-error"
                       },
                       type=function(x, vals=type.vals)
                       {
                           ty <- try(match.arg(x, vals))
                           class(ty)!="try-error"
                       },
                       grpj.per.grp1=function(x, vals=1)TRUE, # Checking this argument in 'other rules'
                       n.sim=function(x)is.int(x)&(x>0),
                       delta=function(x)(x>0&x<1),
                       tempfile=function(x)is.character(x)&length(x)==1)

"valid.arg.msg" <-
function(x, vrbl)
{
  msg <- 
    switch(vrbl,
         u=ifelse(!x, "All components of 'u' must be between 0 and 1 inclusive\n", NA),
         groups=ifelse(!x, "Argument 'groups' must be integral and > 0\n", NA),
         effect.size=ifelse(!x, "Argument 'effect.size' must be > 0\n", NA),
         n.sample=ifelse(!x, "Argument 'n.sample' must be integral and > 0\n", NA),
         r.1=ifelse(!x, "Argument 'r.1' must be between 0 and 1\n", NA),
         alpha=ifelse(!x, "Argument 'alpha' must be between 0 and 1\n", NA),
         N.tests=ifelse(!x, "Argument 'N.tests' must be integral and > 0\n", NA),
         average.power=ifelse(!x,"Argument 'average.power' must be between 1/2 and 1\n",NA),
         tp.power=ifelse(!x, "Argument 'tp.power' must be between 1/2 and 1\n", NA),
         lambda=ifelse(!x, "Argument 'lambda' must be between 1/2 and 1\n", NA),
         FDP.control.method=ifelse(!x, list.o(FDP.mthd.vals,
                                              "Argument 'FDP.control.method' must be " %,%
                                              "set to one of the values \n"), NA),
         method=ifelse(!x, list.o(pwrfdrmthd.vals, 
                                  "Argument 'method' must be set to one " %,%
                                  "of the values \n"), NA),
         n.sim=ifelse(!x, "Argument 'n.sim' must be integral and > 0\n", NA),
         type=ifelse(!x, "Argument 'type' must be 'paired', 'balanced' or 'unbalanced'\n", NA),
         grpj.per.grp1=ifelse(!x, "this gets checked in other rules", NA),
         delta=ifelse(!x, "Argument 'delta' must be between 0 and 1\n", NA),
         tempfile=ifelse(!x, "Argument 'tempfile' must be a character string\n", NA))
  if(is.na(msg)) msg <- NULL
  msg
}
                       
other.rules <-
function(sppld, frmls, m)
{
  is.msng <- !(frmls %in% sppld)
  names(is.msng) <- frmls
  no.n.sample <- is.msng["n.sample"]
  is.pwr <- (!is.msng["average.power"]) || (!is.msng["tp.power"])

  pwr.nera.chk <- (!is.pwr && all(!is.msng[nera])) || (is.pwr && (sum(is.msng[nera])==1))
  
  fdp.needs.N <- c("Auto", "BHCLT","Romano")
  
  msg.1 <- msg.2 <- msg.3 <- msg.4 <- msg.5 <- msg.6 <- msg.7 <- msg.8 <- msg.9 <- msg.10 <- msg.11 <- NULL
  
  ##  pwr.nera.chk failed
  if(!pwr.nera.chk)
  {
      msg.1 <- list.o(nera,"When neither 'average.power' or 'tp.power' is specified, " %,%
                           "you must specify each of the arguemtns,", "\n")
      msg.1 <- msg.1 %,% list.o(nera, "If 'average.power' or 'tp.power' is specified, " %,%
                                      "then all but one of arguemtns.", "must be specified")
  }
  ## Specification of 'tp.power' requires specification of 'lambda'
  if(!is.msng["tp.power"] && is.msng["lambda"])
    msg.2 <-"Specification of argument 'tp.power' requires specification of argument 'lambda' "%,%
            "tp.power = P(TPP > lambda)\n"

  ## 'simulation' method not for sample size calculation
  if(is.pwr && m$method=="simulation")
    msg.3 <- "Sample size calculation not implemented in 'simulation' method.\n"
  
  ## 'simulation' method, 'Auto', 'BHCLT', 'Romano' FDP.control.method's need N.tests
  if((m$FDP.control.method %in% fdp.needs.N || m$method=="simulation" || !is.msng["lambda"]
                                            || !is.msng["tp.power"])&&is.msng["N.tests"])
    msg.4 <- "Argument, 'N.tests' is required for the simulation method, for computing " %,%
             "tp.power, and for the " %,%
             list.a(fdp.needs.N, "", " FDP.control.method's\n")
  
  ## 'both' FDP.control.method is for simulation only
  if(m$FDP.control.method=="both" && m$method=="FixedPoint")
    msg.5 <- "FDP.control.method 'both' is for the simulation method only\n"
  
  ## 'Auto' FDP.control.method is for FixedPoint method only
  if(m$FDP.control.method=="Auto" && m$method=="simulation")
      msg.6 <- "FDP.control.method 'Auto' is for the FixedPoint method only\n"

  ## If 'type' is 'balanced' then argument 'grpj.per.grp1' must be unspecified
  if(m$type=="balanced" && !is.msng["grpj.per.grp1"])
      msg.7 <- "If 'type' is 'balanced' then argument 'grpj.per.grp1' must be unspecified"

  ## If 'type' is 'unbalanced' then argument 'grpj.per.grp1' must be supplied and > 0
  if(m$type=="unbalanced" && (is.msng["grpj.per.grp1"] || m$grpj.per.grp1 <= 0))
      msg.8 <- "If 'type' is 'unbalanced' then argument 'grpj.per.grp1' must be supplied and > 0" 

  ## if 'groups'==1 then 'type' must be unspecified
  if(m$groups==1 && !is.msng["type"])
      msg.9 <- "When argument, 'groups' is 1, then argument 'type' must be unspecified\n"

  ## if 'groups'>=2 and 'type'=='unbalanced' then 'grpj.per.grp1' must be of length groups - 1
  if(m$groups>=2 && m$type=="unbalanced" && length(m$grpj.per.grp1)!=(m$groups-1))
      msg.10 <- "If 'groups'>=2 and 'type'=='unbalanced' then 'grpj.per.grp1' must be of length groups - 1"
     
  ## if 'groups>=3' then 'type' must be balanced or unbalanced
  if(m$groups>=3 && !(m$type %in% c("balanced", "unbalanced")))
      msg.11 <- "If 'groups>=3' then 'type' must be balanced or unbalanced"

  msg <- c(msg.1, msg.2, msg.3, msg.4, msg.5, msg.6, msg.7, msg.8, msg.9, msg.10, msg.11)
  msg
}

"args.def.err.chk" <-
function(m, sppld, frmls, other.rules=TRUE, eval.env)
{
    if(missing(eval.env))
    {
      calling.env <- sys.parent()
      eval.env <- ifelse(calling.env==0, topenv, sys.parent)
    }
    
    n.sppld <- length(sppld)

    is.msng <- !(frmls %in% sppld)
    names(is.msng) <- frmls
    msng.nms <- names(is.msng)[is.msng]
   
    fill.in.default.arg.nms <- match.arg(msng.nms, names(default), several.ok=TRUE)
    for(v in fill.in.default.arg.nms) m[[v]] <- default[[v]](m$groups, m$alpha)

    for(k in 1:n.sppld) m[[sppld[k]]] <- eval(m[[sppld[k]]], eval.env())

    if(!("control" %in% msng.nms) && ("control" %in% frmls))
    {
        pfdr.cntl.call <- as.call(expression(pwrFDR.control))
        pfdr.cntl.call$groups <- m$groups
        pfdr.cntl.call$alpha <- m$alpha
        nms.cntl <- names(m$control)
        n.c <- length(m$control)
        for(k in 1:n.c) pfdr.cntl.call[[nms.cntl[k]]] <- m$control[[nms.cntl[k]]]
        m[["control"]] <- eval(pfdr.cntl.call)
    }
    
    err.msg <- NULL
    chk.valid.arg.nms <- match.arg(sppld, names(valid.arg.dict), several.ok=TRUE)
    for(v in chk.valid.arg.nms)
        err.msg <- c(err.msg, valid.arg.msg(valid.arg.dict[[v]](m[[v]]), v))

    m[["method"]] <- match.arg(m$method, pwrfdrmthd.vals)
    m[["FDP.control.method"]] <- match.arg(m$FDP.control.method, FDP.mthd.vals)
    if(m$method!="simulation") m$n.sim <- NULL
    if(other.rules) err.msg <- c(err.msg, other.rules(sppld, frmls, m))
    
    m$type <- list(`paired`=1, `balanced`=2, `unbalanced`=3)[[m$type]]
    m[["control"]]$is.msng <- is.msng

    attr(m, "err.msg") <- err.msg
    m
}

"pwrFDR.control" <-
function(tol, max.iter, distopt, CS, sim.level, FDP.meth.thresh, verb, low.power.stop, ast.le.a, groups, alpha)
{
    calling.env <- sys.parent()
    eval.env <- ifelse(calling.env==0, topenv, sys.parent)
    m <- match.call()
    m$groups <- m$alpha <- NULL
    frmls <- names(formals(pwrFDR.control))
    frmls <- frmls[-which(frmls=="groups")]
    frmls <- frmls[-which(frmls=="alpha")]
    sppld <- names(m)[-1]
    n.sppld <- length(sppld)
    is.msng <- !(frmls %in% sppld)
    names(is.msng) <- frmls
    msng.nms <- names(is.msng)[is.msng]
    default.pwrFDR.cntl <- default$control(groups=groups, alpha=alpha)
    fill.in.default.arg.nms <- match.arg(msng.nms, names(default.pwrFDR.cntl), several.ok=TRUE)
    for(v in fill.in.default.arg.nms) m[[v]] <- default.pwrFDR.cntl[[v]]
    for(k in 1:n.sppld) m[[sppld[k]]] <- eval(m[[sppld[k]]], eval.env())
    m[[1]] <- as.name("list")
    eval(m, eval.env())
}
#######################################################################
## end argument checking/default argument block                      ##
#######################################################################



#######################################################################
## test statistic distribution block                                 ##
#######################################################################
DF <- c(expression(n.sample-1),
        expression(groups*(n.sample-1)),
        expression((1/n.sample + sum(1/nii.sample))^2/(1/(n.sample^2*(n.sample-1)) + sum(1/(nii.sample^2*(nii.sample-1))))))

NCP <- c(expression(n.sample^0.5*effect.size),
         expression((n.sample/groups)^0.5*effect.size),
         expression(((n.sample-1)/(1 + sum((n.sample-1)/(nii.sample-1))))^0.5*effect.size))

"dists" <-
  as.data.frame(rbind(c(### Normal with 2 groups
  pars0=as.call(expression(c, 0, ncp=0,   sd=1)),
  pars1=as.call(expression(c, 0, ncp=.NCP., sd=1)),
  minv=-Inf, 
  ddist=function(x, par) dnorm(x, mean=par[2], sd=par[3]),
  pdist=function(x, par) pnorm(x, mean=par[2], sd=par[3]),
  qdist=function(x, par) qnorm(x, mean=par[2], sd=par[3])), 
c(### t with 2 groups
  pars0=as.call(expression(c,1,ncp=0,     ndf=.DF.)),
  pars1=as.call(expression(c,1,ncp=.NCP., ndf=.DF.)),
  minv=-Inf, 
  ddist=function(x, par) dt(x, ncp=par[2], df=par[3]),
  pdist=function(x, par) pt(x, ncp=par[2], df=par[3]),
  qdist=function(x, par) qt(x, ncp=par[2], df=par[3])), 
c(### F with 'groups' groups, effect.size=theta*c(0, 0.5, 0.5, ..., 0.5, 1)
  pars0=as.call(expression(c,2,ncp=0,     ndf1=groups-1, ndf2=.DF.)),
  pars1=as.call(expression(c,2,ncp=.NCP.^2, ndf1=groups-1, ndf2=.DF.)),
  minv=0,
  ddist=function(x, par) df(x, ncp=par[2], df1=par[3], df2=par[4]),
  pdist=function(x, par) pf(x, ncp=par[2], df1=par[3], df2=par[4]),
  qdist=function(x, par) qf(x, ncp=par[2], df1=par[3], df2=par[4]))))
#######################################################################
## END test statistic distribution block                             ##
#######################################################################


.onAttach <- function(libname, pkgname)
{
    options(stringsAsFactors=FALSE)
    ver <- read.dcf(file=system.file("DESCRIPTION", package=pkgname), fields="Version")
    msg <- paste(pkgname, ver) %,% "\n\n" %,% "Type ?pwrFDR"
    msg <- msg %,% "\n" %,% "  or   view.presentation()"
    packageStartupMessage(msg)
}

