# lambda.min = 1E-8
nsup.plot <- function(object, lambda.min = .Machine$double.eps^0.5, ...)
{
  # object=fm1
  eps <- .Machine$double.eps
  args0 <- list(...)

  lambda <- nsup <- NULL

  if(!inherits(object, c("LASSO","SGP"))){
     stop("The input object is not of the class 'LASSO' or 'SGP'")
  }

  xlab <- expression(paste("-log(",lambda,")"))
  ylab <- "Support set size"
  main <- NULL
  lwd <- ifelse("lwd" %in% names(args0), args0$lwd, 0.25)
  if("main" %in% names(args0)) main <- args0$main
  if("xlab" %in% names(args0)) xlab <- args0$xlab
  if("ylab" %in% names(args0)) ylab <- args0$ylab

  if(inherits(object, "SGP"))
  {
    # Coarce nsup and lambda to lists
    if("SGP.CV" %in% attr(object,"type")){
      object$lambda <- lapply(object$CV,function(x)x$lambda[,"overall"])
      object$nsup <- lapply(object$CV,function(x)x$nsup[,"overall"])
    }else{
      object$lambda <- lapply(1:nrow(object$lambda),function(k)object$lambda[k,])
      object$nsup <- lapply(1:nrow(object$nsup),function(k)object$nsup[k,])
    }

  }else{
    if(object$q == 1L){
      object$lambda <- list(object$lambda)
      object$nsup <- list(object$nsup)
    }
  }

  stopifnot(length(object$lambda) == length(object$nsup))
  nlambda <- unlist(lapply(object$lambda,length))
  if(all(nlambda < 5L)){
    stop("Plot can be generated for at least 5 lambda values")
  }

  if(any(unlist(lapply(object$lambda,min)) < lambda.min)){
    min0 <- min(unlist(lapply(object$lambda, function(x)min(x[x>lambda.min+eps]))))
    for(k in 1:length(object$lambda)){
      tmp <- object$lambda[[k]]
      object$lambda[[k]] <- ifelse(tmp < lambda.min, min0, tmp)
    }
  }

  dat <- c()
  for(i in 1:length(object$lambda))
  {
    dat <- rbind(dat, data.frame(i=i, nsup=object$nsup[[i]],
                                 lambda=object$lambda[[i]]))
  }
  dat$i <- factor(as.character(dat$i))
  alpha0 <- 1 - 1/(1+0.5*exp(200/nlevels(dat$i)))

  theme0 <- mytheme() + ggplot2::theme(legend.position = "none")

  pp <- ggplot2::ggplot(dat, ggplot2::aes(-log(lambda),nsup,color=i,group=i)) +
        ggplot2::geom_line(linewidth=lwd, alpha=alpha0) +
        ggplot2::geom_hline(yintercept=0, linewidth=0.30, color="gray35") +
        ggplot2::theme_bw() + theme0 +
        ggplot2::labs(title=main, y=ylab, x=xlab)
  pp

}
