lmestData <- function(data, id = NULL,
                      time = NULL, idAsFactor = TRUE, timeAsFactor = TRUE,
                      responsesFormula = NULL, latentFormula = NULL, na.rm = FALSE, check.names = FALSE)
{

  data <- data.frame(data)
  #bal.transf <- match.arg(bal.transf, choices = eval(formals(lmestData)$bal.transf))

  # if(missing(id))
  # {
  #   stop("id must be provided", call. = FALSE)
  # }
  if(is.null(id) & is.null(time))
  {
    id <- data[,1]
    time <- data[,2]
    #data <- x[,-c(1,2)]
    id.which <- 1
    tv.which <- 2
  }else if(!is.null(id) & !is.null(time)){
    if(is.character(id))
    {
      id.which <- which(colnames(data) == id)
      if(length(id.which) == 0)
      {
        stop("the id column does not exist")
      }

      id <- data[,id.which]
    }else{
      id.which <- id
      id <- data[,id.which]
    }

    if(is.character(time))
    {
      tv.which <- which(colnames(data) == time)

      if(length(tv.which) == 0)
      {
        stop("the time column does not exist")
      }

      time <- data[,tv.which]
    }else{
      tv.which <- time
      time <- data[,tv.which]
    }

    #data <- x[,-c(idCol,timeCol)]

  }else if(is.null(id) & !is.null(time)){

    if(is.character(time))
    {
      tv.which <- which(colnames(data) == time)
      time <- data[,tv.which]
    }else{
      tv.which <- time
      time <- data[,tv.which]
    }

    id <- data[,1]
    id.which <- 1

    #data <- data.frame(data, id = id)

  }else if(!is.null(id) & is.null(time)){

    if(is.character(id))
    {
      id.which <- which(colnames(data) == id)
      if(length(id.which) == 0)
      {
        stop("the id column does not exist")
      }
      id <- data[,id.which]
    }else{
      id.which <- id
      id <- data[,id.which]
    }
    time <- rep(NA, length(id))
    uniid <- unique(id)
    for(i in uniid)
    {
      ii <- id == i
      time.temp <- time[ii]
      time[ii] <- 1:length(time.temp)
    }

    tv.which <- ncol(data) + 1
    data <- data.frame(data, tt = time, check.names = check.names)
    #data <- x[,-timeCol]

  }

  if(na.rm)
  {
    # data.new <- data[,-c(id.which,tv.which), drop = FALSE]
    # nas <- is.na(data[,-c(id.which,tv.which)])
    # nn <- sapply(1:max(id), function(x) colSums(data[id == x,]))
    nna <- which(sapply(1:length(unique(id)), function(x) any(is.na(data[id == x,])) == FALSE))
    data <- data[id%in%nna,]
    id <- data[,id.which]
    time <- data[,tv.which]
  }


  if(is.character(id) | idAsFactor)
  {
    data[,id.which] <- factor(data[,id.which])
    id <- factor(id)
    n <- nlevels(id)
  }

  if(is.character(time) | timeAsFactor)
  {
    data[,tv.which] <- factor(data[,tv.which])
    time <- factor(time)
  }

  for(i in (1:ncol(data))[-c(id.which,tv.which)])
  {
    if(is.character(data[,i]))
    {
      data[,i] <- factor(data[,i])
    }
  }


  # id <- data[,id.which]
  # tv <- data[,tv.which]
  n <- nlevels(id)

  if(is.factor(id))
  {
    n <- nlevels(id)
  }else{
    n <- length(unique(id))
  }

  if(is.factor(time))
  {
    TT <- nlevels(time)
  }else{
    TT <- max(time)
  }

  bal <- all(table(id) == TT)
  if(!bal)
  {
    warning("data are not balanced.",call. = FALSE)
  }



  data.new <- data[,-c(id.which,tv.which), drop = FALSE]
  d <- ncol(data.new)
  ## of frequencies of the available configurations
  if(is.null(responsesFormula))
  {
    Y <- data.new
    Xmanifest <- NULL
    Xinitial <- NULL
    Xtrans <- NULL
  }else{
    temp <- getResponses(data = data.new,formula = responsesFormula)
    Y <- temp$Y
    Xmanifest <- temp$X
    Xinitial <- NULL
    Xtrans <- NULL
  }


  if(!is.null(latentFormula))
  {
    temp <- getLatent(data = data.new,latent = latentFormula, responses = responsesFormula)
    Xinitial <- temp$Xinitial
    Xtrans <- temp$Xtrans
  }
  #data <- cbind(id,time,data)

  attributes(data)$responsesFormula = responsesFormula
  attributes(data)$latentFormula = latentFormula
  attributes(data)$whichid = id.which
  attributes(data)$whichtv = tv.which
  attributes(data)$id = id
  attributes(data)$time = time
  attributes(data)$balanced = bal

  out <- list(data = data, id = as.character(factor(id)), time = as.character(factor(time)),
              n = n, TT = TT, d = d,
              Y = Y, Xmanifest = Xmanifest, Xinitial = Xinitial, Xtrans = Xtrans)

  class(out) <- c("lmestData")
  return(out)
}

summary.lmestData <- function(object, type = c("all","cross", "year"),
                              dataSummary = c("all", "responses", "manifest", "initial", "transition"),
                              varType = rep("c", x$d),
                              digits = getOption("digits"), maxsum = 10, maxobs = 20, ...)
{

  x <- object
  dataSummary <- match.arg(dataSummary, choices = eval(formals(summary.lmestData)$dataSummary))
  type <- match.arg(type, choices = eval(formals(summary.lmestData)$type))



  id.which <- attributes(x$data)$whichid
  tv.which <- attributes(x$data)$whichtv

  if(!attributes(x$data)$bal)
  {
    tab <- table(table(x$id))
    tab <- data.frame(cbind(times = dimnames(tab)[[1]],freq = tab,rel.freq = prop.table(tab)), check.names = "FALSE")
  }
  if(dataSummary == "responses" & !is.null(x$Y))
  {
    dt <- data.frame(time = factor(x$data[,tv.which]), x$Y, check.names = "FALSE")
    if(length(varType) != ncol(dt)-1)
    {
      stop("lenght of varType must be equal to the number of variables")
    }


  }else if(dataSummary == "manifest" & !is.null(x$Xmanifest))
  {
    dt <- data.frame(time = factor(x$data[,tv.which]), x$Xmanifest, check.names = "FALSE")
    if(length(varType) != ncol(dt)-1)
    {
      stop("lenght of varType must be equal to the number of variables")
    }
  }else if(dataSummary == "initial" & !is.null(x$Xinitial))
  {
    dt <- data.frame(time = factor(x$data[,tv.which]), x$Xinitial, check.names = "FALSE")
    if(length(varType) != ncol(dt)-1)
    {
      stop("lenght of varType must be equal to the number of variables")
    }
  }else if(dataSummary == "transition" & !is.null(x$Xtrans))
  {
    dt <- data.frame(time = factor(x$data[,tv.which]), x$Xtrans, check.names = "FALSE")
    if(length(varType) != ncol(dt)-1)
    {
      stop("lenght of varType must be equal to the number of variables")
    }
  }else if(dataSummary == "all")
  {
    if(length(varType) != x$d)
    {
      stop("lenght of varType must be equal to the number of variables")
    }

    dt <- x$data[,-c(id.which, tv.which)]
    dt <- data.frame(time = factor(x$data[,tv.which]), dt, check.names = "FALSE")
  }

  x$d <- ncol(dt)-1
  ## Informazioni principali sui dati
  cat("\nData Info:")
  cat("\n----------","\n\n")
  cat("Observations:       ", x$n, "\n")
  cat("Time occasions:     ", ifelse(is.factor(dt$time),nlevels(dt$time),max(unique(dt$time))), "\n")
  cat("Variables:          ", ncol(dt)-1, "\n\n")

  if(!attributes(x$data)$bal)
  {
    print(tab, row.names = FALSE)
  }

if(type == "all")
{
  typeCross = TRUE
  typeYear = TRUE
}else{
  typeCross <- type ==  "cross"
  typeYear <- type ==  "year"
}

whichC <- varType == "c"
C <- length(unique(whichC))
  if(C > 1)
  {
    cat("\nSummary for continuous variables:")
    cat("\n----------","\n\n")
    print(summary(dt[,-c(1,which(varType == "d"))], maxsum = maxsum, digits = digits))
    if(x$n <= maxobs)
    {
      cat("\nMean for each observation:")
      cat("\n----------","\n\n")
      dt1 <- dt[,-c(1,(which(varType == "d")+1))]
      ii <- x$data[,id.which]
      dtt <- t(sapply(levels(factor(ii)), function(x) colMeans(as.matrix(dt1[ii==x,]))))
      print(data.frame(dtt,row.names = unique(x$data[,c(id.which)])))
    }

    cat("\nProportion for discrete variables:")
    cat("\n----------","\n\n")
    #dt1 <- dt[,-1]
    dt1 <- dt[,-(which(varType == "c")+1)]
    temp1 <- data.frame(lapply(dt1, factor))
    nle <- max(sapply(temp1,nlevels))
    temp1 <- summary(temp1)
    temp <- matrix(NA, nrow = 150, ncol = ncol(dt1))
    #temp[1:x$TT,1] <- paste(1:x$TT,summary(dt[,1]),sep = ":")
    temp[1:x$TT,1] <- temp1[1:x$TT,1]
    attr(temp, "dimnames")[[1]] <- rep("", 150)
    for(i in 2:ncol(dt1))
    {
      tp <- prop.table(table(dt1[,i]))
      temp[1:length(tp),i] <- paste(attr(tp, "dimnames")[[1]],round(tp,digits = digits), sep = ":")

    }

    #dt1 <-
    temp <- as.table(temp)[1:nle,]
    attr(temp, "dimnames")[[2]] <- attr(temp1, "dimnames")[[2]][1:ncol(dt1)]
    print(temp)

    }else if (all(whichC)){
      cat("\nSummary:")
      cat("\n----------","\n\n")
      print(summary(dt[,-1], maxsum = maxsum, digits = digits))
      if(x$n <= maxobs)
      {
        cat("\nMean for each observation:")
        cat("\n----------","\n\n")
        dt1 <- dt[,-1]
        ii <- x$data[,id.which]
        dtt <- t(sapply(levels(factor(ii)), function(x) colMeans(as.matrix(dt1[ii==x,]))))
        print(data.frame(dtt,row.names = unique(x$data[,c(id.which)])))
      }
    }else{

      cat("\nProportion:")
      cat("\n----------","\n\n")
      #dt1 <- dt[,-1]
      temp1 <- data.frame(lapply(dt, factor))
      nle <- max(sapply(temp1,nlevels))
      temp1 <- summary(temp1,maxsum=50)
      temp <- matrix(NA, nrow = 150, ncol = x$d+1)
      #temp[1:x$TT,1] <- paste(1:x$TT,summary(dt[,1]),sep = ":")
      temp[1:x$TT,1] <- temp1[1:x$TT,1]
      attr(temp, "dimnames")[[1]] <- rep("", 150)

      for(i in 2:(x$d +1))
      {
        tp <- prop.table(table(dt[,i]))
        temp[1:length(tp),i] <- paste(unlist(attr(tp, "dimnames")),round(tp,digits = digits), sep = ":")

      }

      temp <- as.table(temp)[1:nle,]
      attr(temp, "dimnames")[[2]] <- attr(temp1, "dimnames")[[2]]
      print(temp)

    }



  if(typeYear)
  {
    if(C > 1)
    {
      cat("\nSummary by year:")
      cat("\n----------","\n\n")

      for(i in unique(x$time))
      {
        cat("\nTime = ", i, "\n\n")
        cat("\nSummary:")
        cat("\n----------","\n\n")
      print(summary(dt[x$time == i,-c(1,which(varType == "d"))], maxsum = maxsum, digits = digits))


      cat("\nProportion for discrete variables:")
      cat("\n----------","\n\n")
      #dt1 <- dt[,-1]

      dt1 <- dt[x$time == i,-(which(varType == "c")+1)]
      temp1 <- data.frame(lapply(dt1, factor))
      nle <- max(sapply(temp1,nlevels))
      temp1 <- summary(temp1)
      temp <- matrix(NA, nrow = 150, ncol = ncol(dt1))
      temp[1,1] <- temp1[1,1]
      attr(temp, "dimnames")[[1]] <- rep("", 150)
      for(j in 2:ncol(dt1))
      {
        if(all(is.na(dt[x$time == i,j])))
        {
          tp <- summary(factor(dt[x$time == i,j]))
          temp[1:length(tp),j] <- paste("NAs",round(tp,digits = digits), sep = ":")


        }else
        {
          tp <- prop.table(table(dt[x$time == i,j]))
          temp[1:length(tp),j] <- paste(attr(tp, "dimnames")[[1]],round(tp,digits = digits), sep = ":")

        }
        temp[1:length(tp),j] <- paste(attr(tp, "dimnames")[[1]],round(tp,digits = digits), sep = ":")

      }
      #dt1 <-
      temp <- as.table(temp)[1:nle,]
      attr(temp, "dimnames")[[2]] <- attr(temp1, "dimnames")[[2]][1:ncol(dt1)]
      print(temp)
      }
    }else if (all(whichC)){
      cat("\nSummary by year:")
      cat("\n----------","\n\n")
      for(i in unique(x$time))
      {
        cat("\nTime = ", i, "\n\n")
      print(summary(dt[x$time == i,-1], maxsum = maxsum, digits = digits))
      }
    }else{

      cat("\nProportion by year:")
      cat("\n----------","\n\n")
      #dt1 <- dt[,-1]
      for(i in unique(x$time))
      {
        cat("\nTime = ", i, "\n\n")
        temp1 <- data.frame(lapply(dt[x$time == i,], factor))
        nle <- max(sapply(temp1,nlevels))
        temp1 <- summary(temp1)
      temp <- matrix(NA, nrow = 150, ncol = x$d+1)
      temp[1,1] <- temp1[1,1]
      attr(temp, "dimnames")[[1]] <- rep("", 150)
      for(j in 2:(x$d+1))
      {
        if(all(is.na(dt[x$time == i,j])))
        {
          tp <- summary(factor(dt[x$time == i,j]))
          temp[1:length(tp),j] <- paste("NAs",round(tp,digits = digits), sep = ":")


        }else
        {
          tp <- prop.table(table(dt[x$time == i,j]))
          temp[1:length(tp),j] <- paste(attr(tp, "dimnames")[[1]],round(tp,digits = digits), sep = ":")

        }

      }

      #dt1 <-
      temp <- as.table(temp)[1:nle,]
      attr(temp, "dimnames")[[2]] <- attr(temp1, "dimnames")[[2]]
      print(temp)

      }
    }


  }


invisible()

}


plot.lmestData <- function(x,
                           typePlot = c("s", "sh"),
                           dataPlots = c("all", "responses", "manifest", "initial", "transition"),
                           ...)
{

  #
  # s = scatterplot matrix
  # sh = scatterplot matrix with histogram in the main diagonal
  #
  dataPlots <- match.arg(dataPlots, choices = eval(formals(plot.lmestData)$dataPlots))
  typePlot <- match.arg(typePlot, choices = eval(formals(plot.lmestData)$typePlot))
  id.which <- attributes(x$data)$whichid
  tv.which <- attributes(x$data)$whichtv

  if(dataPlots == "all")
  {
    dt <- x$data[,-c(id.which, tv.which)]
  }else if(dataPlots == "responses")
  {
    dt <- x$Y

  }else if(dataPlots == "manifest")
  {
    dt <- x$Xmanifest

  }else if(dataPlots == "initial")
  {
    dt <- x$Xinitial

  }else if(dataPlots == "transition")
  {
    dt <- x$Xtrans

  }

  if(typePlot == "s")
  {
    if(ncol(dt) == 1)
    {
      h <- hist(x = dt, xlab = colnames(dt),...)
    }else{
      pairs(x = dt, ...)
    }


  }else if (typePlot == "sh")
  {

    if(ncol(dt) == 1)
    {
      h <- hist(dt, xlab = colnames(dt),...)
    }else{
      panel.hist <- function(x, ...)
      {
        usr <- par("usr"); on.exit(par(usr))
        par(usr = c(usr[1:2], 0, 1.5) )
        h <- hist(x, plot = FALSE)
        breaks <- h$breaks; nB <- length(breaks)
        y <- h$counts; y <- y/max(y)
        rect(breaks[-nB], 0, breaks[-1], y, col = "grey", ...)
      }
      pairs(dt, diag.panel = panel.hist, ... )
    }

  }

}

print.lmestData <- function(x, ...)
{
  cat("\nAvailable objects:\n")
  print(names(x))

  cat("\nData Info:")
  cat("\n----------","\n\n")
  cat("Observations:       ", x$n, "\n")
  cat("Time occasions:     ", x$TT, "\n")
  cat("Variables:          ", x$d, "\n\n")
}
