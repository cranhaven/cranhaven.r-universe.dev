# K = i = NULL; prune = FALSE; cor.max = 0.97; lambda.min = 1E-8; nbreaks.x = 6
path.plot <- function(object, K = NULL, i = NULL,
                      prune = FALSE, cor.max = 0.97,
                      lambda.min = .Machine$double.eps^0.5,
                      nbreaks.x = 6, npaths.max = 5000, ...)
{
  # object=fm1; K=G; prune=TRUE; cor.max=0.9
  flagKinship <- FALSE
  eps <- .Machine$double.eps
  args0 <- list(...)

  if(!inherits(object, c("LASSO","SGP"))){
     stop("The input object is not of the class 'LASSO' or 'SGP'")
  }

  if(inherits(object, "SGP") & ("SGP.CV" %in% attr(object,"type"))){
    stop("'path.plot' cannot be applied after cross-validation")
  }

  xlab <- "Number of active predictors"
  ylab <- expression(beta)
  main <- NULL
  lwd <- ifelse("lwd" %in% names(args0), args0$lwd, 0.2)
  if("main" %in% names(args0)) main <- args0$main
  if("xlab" %in% names(args0)) xlab <- args0$xlab
  if("ylab" %in% names(args0)) ylab <- args0$ylab

  theme0 <- mytheme() + ggplot2::theme(legend.key.height = ggplot2::unit(3,"line"),
                                       legend.key.width = ggplot2::unit(0.8, "lines"))

  MAP <- NULL
  isSGP <- FALSE
  if(inherits(object, "SGP"))
  {
    if(!is.null(K)){
      flagKinship <- TRUE
      if((length(dim(K)) != 2L) | (length(K) != object$n^2)){
        stop("Input 'K' must be a ",object$n," x ",object$n," matrix")
      }
      MAP <- map_set(i=object$ID_geno, j=object$ID_trait, n=object$n, x=object$trn, y=object$tst)
    }

    beta <- coef.SGP(object)
    # Coarce nsup and lambda to lists
    object$lambda <- lapply(1:nrow(object$lambda),function(k)object$lambda[k,])
    object$nsup <- lapply(1:nrow(object$nsup),function(k)object$nsup[k,])

    tst <- as.numeric(rownames(object$yHat))
    if(length(tst) == 1L){
      beta <- list(beta)
    }

    if(is.null(i)){
      indexTST <- seq_along(tst)
    }else{
      tmp <- match(tst, object$tst)
      if( any(!i %in% tmp) ){
        stop("All elements in 'i' must be between ",min(tmp)," and ",max(tmp))
      }
      indexTST <- match(i, tmp)
    }
    isSGP <- TRUE

  }else{
    beta <- coef.LASSO(object)
    object$trn <- seq(object$p)
    tst <- seq(object$q)

    if(object$q == 1L){
      beta <- list(beta)
      object$lambda <- list(object$lambda)
      object$nsup <- list(object$nsup)
    }
    if(!is.null(i)){
      if( max(i) > object$q ){
        stop("All elements in 'i' must be between 0 < i <= q, where q=ncol(Gamma)")
      }
      indexTST <- i
    }else{
      indexTST <- seq_along(tst)
    }
  }

  beta <- beta[indexTST]
  object$lambda <- object$lambda[indexTST]
  object$nsup <- object$nsup[indexTST]

  for(k in 1:length(object$lambda)){
    index <- which(object$lambda[[k]] >= lambda.min)
    object$lambda[[k]] <- object$lambda[[k]][index]
    object$nsup[[k]] <- object$nsup[[k]][index]
    beta[[k]] <- beta[[k]][,index]
  }

  nLambda <- unlist(lapply(object$lambda,length))
  if(all(nLambda < 5L)){
    stop("Coefficients path plot can be generated for at least 5 lambda values")
  }

  #if(any(unlist(lapply(object$lambda,min)) < lambda.min)){
  #  min0 <- min(unlist(lapply(object$lambda, function(x)min(x[x>lambda.min+eps]))))
  #  for(k in 1:length(object$lambda)){
  #    tmp <- object$lambda[[k]]
  #    object$lambda[[k]] <- ifelse(tmp < lambda.min, min0, tmp)
  #  }
  #}

  if(length(unique(nLambda)) > 1L){
    INDEX1 <- matrix(seq_along(indexTST),ncol=1)
    if(prune){
      message("'pruning' is applied for each response variable")
    }
  }else{
    if(length(object$trn)*length(beta) > npaths.max){
      nTST0 <- ceiling(npaths.max/length(object$trn))
      message("The number of paths is very large. Only ",nTST0*length(object$trn),
              " paths corresponding to the first ",nTST0,
              ifelse(isSGP," testing elements"," response variables"),
              " are plotted.")
      message("You can select specific paths through 'i' argument")

      beta <- beta[seq(nTST0)]
      object$lambda <- object$lambda[seq(nTST0)]
      object$nsup <- object$nsup[seq(nTST0)]
      indexTST <- indexTST[seq(nTST0)]
    }
    nTSTi <- ceiling(1000/length(object$trn))
    INDEX1 <- matrix(seq(nTSTi*ceiling(length(indexTST)/nTSTi)),ncol=nTSTi, byrow=TRUE)
    if(prune & nrow(INDEX1)>1L){
      message("'pruning' is applied in groups of ",nTSTi*length(object$trn)," paths")
    }
  }

  i0 <- j0 <- path <- Kij <- lambda <- NULL
  dat <- c()
  for(k in 1:nrow(INDEX1))
  {
    tst0 <- INDEX1[k,][INDEX1[k,] <= length(indexTST)]
    b0 <- do.call(rbind, beta[tst0])
    INDEX2 <- cbind(rep(tst0, each=length(object$trn)),
                    rep(seq_along(object$trn),length(tst0)))

    indexOK <- 1:nrow(b0)
    indexdrop <- which(apply(b0, 1L, function(x)all(abs(x) <= eps)))
    if(length(indexdrop) > 1){  # leave one that has all bij==0
      b0 <- b0[-indexdrop[-1], ,drop=FALSE]
      indexOK <- indexOK[-indexdrop[-1]]
    }

    if(prune & (nrow(b0)>1)){
      tmp <- Prune(t(b0), alpha=cor.max^2)$prune.in
      indexOK <- indexOK[tmp]
      b0 <- b0[tmp, ,drop=FALSE]
    }

    dat0 <- do.call(rbind,lapply(1:length(indexOK), function(j){
          tmp <- INDEX2[indexOK[j],]
          i0 <- tst[indexTST[tmp[1]]]
          j0 <- object$trn[tmp[2]]

          if(flagKinship){
            Kij <- K[MAP[i0,"i"],MAP[j0,"i"]]
          }else{
            Kij <- NA
          }
          data.frame(nsup=object$nsup[[tmp[1]]],lambda=object$lambda[[tmp[1]]],
                     beta=b0[j,], Kij=Kij, i=i0, path=paste0(i0,":",j0))
    }))

    dat <- rbind(dat, dat0)
  }
  dat$i <- factor(as.character(dat$i))
  dat$path <- factor(as.character(dat$path))
  alpha0 <- 1 - 1/(1+0.5*exp(500/nlevels(dat$path)))

  # Labels and breaks for the nsup axis
  tmp <- get_breaks(x=-log(unlist(object$lambda)), y=unlist(object$nsup),
                    nbreaks.x=nbreaks.x, ymin=0)
  breaks0 <- tmp$breaks.x
  labels0 <- round(tmp$breaks.y)
  labels2 <- sprintf('%.2f', breaks0)

  if(flagKinship){
    pp <- ggplot2::ggplot(dat, ggplot2::aes(-log(lambda),beta,color=Kij,group=path)) +
          ggplot2::labs(color=expression(k[ij])) +
          viridis::scale_color_viridis(direction = -1) +
          ggplot2::geom_line(linewidth=lwd, alpha=alpha0) +
          ggplot2::geom_hline(yintercept=0, linewidth=0.30, color="gray35") +
          ggplot2::theme_bw() + theme0

  }else{
    pp <- ggplot2::ggplot(dat, ggplot2::aes(-log(lambda),beta,color=i,group=path)) +
          ggplot2::geom_line(linewidth=lwd, alpha=alpha0) +
          ggplot2::geom_hline(yintercept=0, linewidth=0.30, color="gray35") +
          ggplot2::theme_bw() + theme0 + ggplot2::theme(legend.position = "none")
  }

  pp <- pp +
        ggplot2::labs(title=main,y=ylab,x=xlab) +
        ggplot2::scale_x_continuous(breaks=breaks0,labels=labels0,
                                    sec.axis=ggplot2::sec_axis(~.+0,expression(paste("-log(",lambda,")")),
                                    breaks=breaks0, labels=labels2))
  pp

}
