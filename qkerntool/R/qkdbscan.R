# qkernel Isometric Feature Mapping

## Wraps around dbscan in package fpc, 
## to carry out modifications to the qKernels and cnd kernels

setGeneric("qkdbscan",function(x, ...) standardGeneric("qkdbscan"))

setMethod("qkdbscan",signature(x = "matrix"),
          function(x, kernel = "rbfbase", qpar = list(sigma = 0.1, q = 0.9), eps = 0.25, MinPts = 5, hybrid = TRUE, seeds = TRUE, showplot  = FALSE, countmode = NULL, na.action = na.omit, ...)
{

  x <- na.action(x)
  x <- as.matrix(x)
  ret <- new("qkdbscan")

  if (!is(kernel,"qkernel") && !is(kernel,"cndkernel"))
    {
      if(is(kernel,"function")) kernel <- deparse(substitute(kernel))       # if delete "function"???
      kernel <- do.call(kernel, qpar)

  }
  if(!is(kernel,"qkernel") && !is(kernel,"cndkernel")) stop("kernel must inherit from class 'qkernel' or 'cndkernel'")

  n <- nrow(x)
 # if (scale)
 #   x <- scale(x)
  classn <- cv <- integer(n)

  isseed <- logical(n)
  cn <- integer(1)
  for (i in 1:n){
    if (i %in% countmode)
      cat("Processing point ", i," of ",n, ".\n")
    unclass <- (1:n)[cv<1]

    if (cv[i]==0){
        #tempdist <- Eucdist(x[i,, drop=FALSE],x[unclass,, drop=FALSE], sEuclidean = TRUE)
        # print(tempdist)
      if(is(kernel,"cndkernel"))
        ## Compute conditionally negative definite kernel matrix
        tempdist <- cndkernmatrix(kernel,x[i,, drop=FALSE],x[unclass,, drop=FALSE])
      else if (is(kernel,"qkernel"))
        tempdist <- qkernmatrix(kernel, x[i,, drop=FALSE],x[unclass,, drop=FALSE])
        reachables <- unclass[as.vector(tempdist)<=eps]
      if (length(reachables)+classn[i]<MinPts)
        cv[i] <- (-1)
      else{
        cn <- cn+1
        cv[i] <- cn
        isseed[i] <- TRUE
        reachables <- setdiff(reachables, i)
        unclass <- setdiff(unclass, i)
        classn[reachables] <- classn[reachables]+1
        while (length(reachables)){
          if (showplot)
            plot(x,  col=1+cv, pch=1+isseed)
          cv[reachables] <- cn
          ap <- reachables
          reachables <- integer()
          if (hybrid){
            if(is(kernel,"cndkernel"))
              ## Compute conditionally negative definite kernel matrix
              tempdist <- cndkernmatrix(kernel,x[ap,, drop=FALSE],x[unclass,, drop=FALSE])
            else if (is(kernel,"qkernel"))
              tempdist <- qkernmatrix(kernel, x[ap,, drop=FALSE],x[unclass,, drop=FALSE])
            frozen.unclass <- unclass
          }
          for (i2 in seq(along=ap)){
            j <- ap[i2]
            if (showplot>1)
              plot(x, col=1+cv, pch=1+isseed)
            if (hybrid){
              jreachables <- unclass[tempdist[i2,match(unclass, frozen.unclass)]<=eps]
            }else{
              if(is(kernel,"cndkernel"))
                ## Compute conditionally negative definite kernel matrix
                tempdist <- cndkernmatrix(kernel,x[j,, drop=FALSE],x[unclass,, drop=FALSE])
              else if (is(kernel,"qkernel"))
                tempdist <- qkernmatrix(kernel, x[j,, drop=FALSE],x[unclass,, drop=FALSE])  # method=="raw"
              jreachables <- unclass[as.vector(tempdist)<=eps]
            }
            if (length(jreachables)+classn[j]>=MinPts){
              isseed[j] <- TRUE
              cv[jreachables[cv[jreachables]<0]] <- cn
              reachables <- union(reachables, jreachables[cv[jreachables]==0])  # isseed for these new reachables tested at next while loop
            }
            # must be after querying classn, otherwise we count j itself twice
            classn[jreachables] <- classn[jreachables]+1
            unclass <- setdiff(unclass, j)
          } # for j
        } # while sum reachables>0
      } # else (sum reachables + ... >= MinPts)
    } # if cv==0
    if (!length(unclass))
      break
  } # for i
  rm(classn)
  if (any(cv==(-1))){
    cv[cv==(-1)] <- 0
  }
  if (showplot)
    plot(x,  col=1+cv, pch=1+isseed)

  clust(ret) <- cv
  eps(ret) <- eps
  MinPts(ret) <- MinPts
  kcall(ret) <- match.call()
  xmatrix(ret) <- x
  qpar(ret) <- qpar
  cndkernf(ret) <- kernel
  if (seeds && cn>0){
    isseed(ret) <- isseed
  }

  return(ret)
})


setMethod("qkdbscan",signature(x = "cndkernmatrix"),
          function(x, eps = 0.25, MinPts = 5, seeds = TRUE, showplot  = FALSE, countmode = NULL, ...)      # method = "dist"
{
  ret <- new("qkdbscan")
  n <- dim(x)[1]
  if(n!= dim(x)[2])
    stop("cndkernel matrix has to be symetric, conditionally negative definite and zero diagonal")
  x <- as.matrix(x)
  classn <- cv <- integer(n)

  isseed <- logical(n)
  cn <- integer(1)
  for (i in 1:n){
    if (i %in% countmode)
      cat("Processing point ", i," of ",n, ".\n")
      unclass <- (1:n)[cv<1]

    if (cv[i]==0){

        reachables <- unclass[x[i,unclass]<=eps]

      if (length(reachables)+classn[i]<MinPts)
        cv[i] <- (-1)
      else{
        cn <- cn+1
        cv[i] <- cn
        isseed[i] <- TRUE
        reachables <- setdiff(reachables, i)
        unclass <- setdiff(unclass, i)
        classn[reachables] <- classn[reachables]+1
        while (length(reachables)){
          if (showplot)
            plot(x,  col=1+cv, pch=1+isseed)
          cv[reachables] <- cn
          ap <- reachables
          reachables <- integer()

          for (i2 in seq(along=ap)){
            j <- ap[i2]
            if (showplot>1)
              plot(x, col=1+cv, pch=1+isseed)
              jreachables <- unclass[x[j,unclass]<=eps]

            if (length(jreachables)+classn[j]>=MinPts){
              isseed[j] <- TRUE
              cv[jreachables[cv[jreachables]<0]] <- cn
              reachables <- union(reachables, jreachables[cv[jreachables]==0])  # isseed for these new reachables tested at next while loop
            }
            # must be after querying classn, otherwise we count j itself twice
            classn[jreachables] <- classn[jreachables]+1
            unclass <- setdiff(unclass, j)
          } # for j
        } # while sum reachables>0
      } # else (sum reachables + ... >= MinPts)
    } # if cv==0
    if (!length(unclass))
      break
  } # for i
  rm(classn)
  if (any(cv==(-1))){
    cv[cv==(-1)] <- 0
  }
  if (showplot)
    plot(x,  col=1+cv, pch=1+isseed)

  clust(ret) <- cv
  eps(ret) <- eps
  MinPts(ret) <- MinPts
  kcall(ret) <- match.call()
  xmatrix(ret) <- x
  cndkernf(ret) <- "cndkernel"
  if (seeds && cn>0){
    isseed(ret) <- isseed
  }

  return(ret)
 })


setMethod("qkdbscan",signature(x = "qkernmatrix"),
          function(x, eps = 0.25, MinPts = 5, seeds = TRUE, showplot  = FALSE, countmode = NULL, ...)         # method = "dist"
{
  ret <- new("qkdbscan")
  n <- dim(x)[1]
  if(n!= dim(x)[2])
    stop("qkernel matrix has to be symetric, conditionally negative definite and zero diagonal")
  x <- as.matrix(x)
  classn <- cv <- integer(n)

  isseed <- logical(n)
  cn <- integer(1)
  for (i in 1:n){
    if (i %in% countmode)
      cat("Processing point ", i," of ",n, ".\n")
      unclass <- (1:n)[cv<1]

    if (cv[i]==0){

        reachables <- unclass[x[i,unclass]<=eps]

      if (length(reachables)+classn[i]<MinPts)
        cv[i] <- (-1)
      else{
        cn <- cn+1
        cv[i] <- cn
        isseed[i] <- TRUE
        reachables <- setdiff(reachables, i)
        unclass <- setdiff(unclass, i)
        classn[reachables] <- classn[reachables]+1
        while (length(reachables)){
          if (showplot)
            plot(x,  col=1+cv, pch=1+isseed)
          cv[reachables] <- cn
          ap <- reachables
          reachables <- integer()

          for (i2 in seq(along=ap)){
            j <- ap[i2]
            if (showplot>1)
              plot(x, col=1+cv, pch=1+isseed)
              jreachables <- unclass[x[j,unclass]<=eps]

            if (length(jreachables)+classn[j]>=MinPts){
              isseed[j] <- TRUE
              cv[jreachables[cv[jreachables]<0]] <- cn
              reachables <- union(reachables, jreachables[cv[jreachables]==0])  # isseed for these new reachables tested at next while loop
            }
            # must be after querying classn, otherwise we count j itself twice
            classn[jreachables] <- classn[jreachables]+1
            unclass <- setdiff(unclass, j)
          } # for j
        } # while sum reachables>0
      } # else (sum reachables + ... >= MinPts)
    } # if cv==0
    if (!length(unclass))
      break
  } # for i
  rm(classn)
  if (any(cv==(-1))){
    cv[cv==(-1)] <- 0
  }
  if (showplot)
    plot(x,  col=1+cv, pch=1+isseed)

  clust(ret) <- cv
  eps(ret) <- eps
  MinPts(ret) <- MinPts
  kcall(ret) <- match.call()
  xmatrix(ret) <- x
  cndkernf(ret) <- "qkernel"
  if (seeds && cn>0){
    isseed(ret) <- isseed
  }

  return(ret)
 })





setMethod("print",signature(x="qkdbscan"), function(x,y...){
  cat("qkdbscan Pts=", length(clust(x)), " MinPts=", MinPts(x), " eps=", eps(x), "\n", sep="")
  if (is.null(isseed(x)))
    tab <- table(clust(x))
  else{
    tab <- table(c("seed", "border")[2-isseed(x)], clust=clust(x))
    if (is.null(dim(tab))){
      tab <- cbind(tab)
      colnames(tab) <- unique(clust(x))
    }
    tab <- rbind(tab, total=colSums(tab))
  }
  print(tab)
})

# setGeneric("plot", function(x, y, ...) standardGeneric("plot"), useAsDefault = graphics::plot)
#setMethod("plot", signature(object="matrix"), function(object, data, ...)
#{
#  plot(data, col=1+clust(object), pch=1+isseed(object), ...)
#})

setMethod("plot", signature(x="qkdbscan"), function(x, y, ...)
{
  plot(y, col=1+clust(x), pch=1+isseed(x), ...)
})


setMethod("predict", signature(object="qkdbscan"), function(object, data, newdata = NULL, predict.max = 1000, ...)
{

  if (is.null(newdata)){

    return(clust(object))

  }else{

    if (is.null(isseed(object)))
      stop("no seeds to predict")

    dmax <- eps(object)
    data <- data[isseed(object), , drop=FALSE]
    out <- clust(object)[isseed(object)]

#    if (!require(distpatch))
    distpair <- function(x,data){
      sqrt(rowSums((x-data)^2))
    }

#    require(class)
    batchpredict <- function(newdata){
      w <- as.integer(knn(data, newdata, 1:n.orig))
      newout <- out[w]
      if (!is.null(dmax)){
        d <- distpair(data[w,,drop=FALSE], newdata)
        newout[d>dmax] <- 0
      }
      return(newout)
    }
    n <- nrow(newdata)
    n.orig <- nrow(data)
    if (n>predict.max){
      i <- 1:n
      ret <- do.call("c", lapply(split(i, (i-1)%/%predict.max), function(i)batchpredict(newdata[i, , drop=FALSE])))
    }else{
      ret <- batchpredict(newdata)
    }
    return(ret)
  }
})

