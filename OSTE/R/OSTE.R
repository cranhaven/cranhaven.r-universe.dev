OSTE <-
function(formula=NULL,data,t.initial=NULL,v.size=NULL,mtry=NULL,M=NULL,
                 minimum.node.size = NULL,
                 always.split.features=NULL,replace=TRUE,splitting.rule = NULL,info=TRUE){
  exact=dts=NULL
  requireNamespace("ranger", quietly = TRUE)
  
  t.initial <- ifelse(is.null(t.initial),500,t.initial)
  n <- nrow(data)
  d <- ncol(data)-2
  mtry <- ifelse(is.null(mtry),round(sqrt(d)),mtry)
  v.size <- ifelse(is.null(v.size),0.3,v.size)
  minimum.node.size <- ifelse(is.null(minimum.node.size),3,minimum.node.size)
  splitting.rule <- ifelse(is.null(splitting.rule),"logrank",splitting.rule)
  #dep.var.name <- ifelse(is.null(formula),dep.var.name,NULL)
  #stat.var.name <- ifelse(is.null(formula),stat.var.name,NULL)
  M <- ifelse(is.null(M),0.2,M)
  frm <- formula
  name.frm <- frm[[2]]
  var.frm <- frm[[3]]
  dependents <- c(as.character(frm[[2]][[2]]),as.character(frm[[2]][[3]]))

if(var.frm=='.')
  var.frm <- names(data)[-which(names(data) %in% dependents)]
  #if(var.frm!='.')
   # var.frm <- var.frm
  frm <- as.formula(paste(paste(paste(frm)[[2]],"~"),paste(var.frm[-1], collapse="+")))
  ################################################################
  ################################################################
  comb.ranger <- function (...)
  {
    
    rangerlist <- list(...) # All arguments passed to function are given to 'rangerlist',
    are_ranger <- sapply(rangerlist, function(x) inherits(x, "ranger"))#apply function on all arguments present in rangerlist.inherits means that x is ranger or not.
    if (any(!are_ranger)) 
      stop("Argument must be a list of ranger objects") # ranger objects are thoes object which comes from ranger code)
    ranger <- rangerlist[[1]] # 1 means the first object
    trees <- sapply(rangerlist, function(x) x$num.trees) #x$num.trees means number of trees in object x
    num.trees <- sum(trees)
    ranger$num.trees <- num.trees
    
    # see whether forest object is present
    haveForest <- sapply(rangerlist, function(x) !is.null(x$forest))
    if (all(haveForest)) {
      ranger$forest$dependent.varID <- rangerlist[[1]]$forest$dependent.varID
      ranger$forest$num.trees  <- sum(do.call("c",lapply(rangerlist, function(x) x$forest$num.trees)))
      ranger$forest$child.nodeIDs  <- do.call("c",lapply(rangerlist, function(x) x$forest$child.nodeIDs))
      ranger$forest$split.varIDs  <- do.call("c",lapply(rangerlist, function(x) x$forest$split.varIDs))
      ranger$forest$split.values  <- do.call("c",lapply(rangerlist, function(x) x$forest$split.values))
      ranger$forest$is.ordered <- rangerlist[[1]]$forest$is.ordered
      ranger$forest$status.varID <- rangerlist[[1]]$forest$status.varID
      ranger$forest$chf  <- do.call("c",lapply(rangerlist, function(x) x$forest$chf))
      ranger$forest$unique.death.times <- rangerlist[[1]]$forest$unique.death.times
      ranger$forest$independent.variable.names <- rangerlist[[1]]$forest$independent.variable.names
      ranger$forest$treetype <- rangerlist[[1]]$forest$treetype
    }
    else {
      ranger$forest <- NULL
    }
    #rest of the values returned
    ranger$num.independent.variables <- rangerlist[[1]]$num.independent.variables
    ranger$unique.death.times <- rangerlist[[1]]$unique.death.times
    ranger$mtry <- rangerlist[[1]]$mtry
    ranger$min.node.size <- rangerlist[[1]]$min.node.size 
    ranger$variable.importance <- rangerlist[[1]]$variable.importance
    ranger$prediction.error <- mean(do.call("c",lapply(rangerlist, function(x) x$prediction.error)))
    
    #Cummulative hazzard function
    chfs <- do.call("list",lapply(rangerlist, function(x) x$chf)) 
    ranger$chf <- Reduce("+", chfs)/length(chfs)
    
    # Survival values
    survivals <- do.call("list",lapply(rangerlist, function(x) x$survival)) 
    ranger$survival <- Reduce("+", survivals)/length(survivals)
    
    #Tree type
    ranger$treetype <- rangerlist[[1]]$treetype 
    ranger$call <- rangerlist[[1]]$call 
    ranger$importance.mode <- rangerlist[[1]]$importance.mode 
    ranger$num.samples <- rangerlist[[1]]$num.samples
    return(ranger)
  }
  ################################################################
  
  
  
  
  ################################################################
  
  predictSurvProb.ranger <- function (object, newdata, times, ...) {
    ptemp <- predict(object, data = newdata, importance = "none")
    ptemp <- ptemp$survival
    pos <- sindex(jump.times = object$unique.death.times,
                           eval.times = times)
    p <- cbind(1, ptemp)[, pos + 1, drop = FALSE]
    if (NROW(p) != NROW(newdata) || NCOL(p) != length(times))
      stop(paste("\nPrediction matrix has wrong dimensions:\nRequested newdata x times: ",
                 NROW(dts[trainind,]), " x ", length(1), "\nProvided prediction matrix: ",
                 NROW(p), " x ", NCOL(p), "\n\n", sep = ""))
    p
  }
  ################################################################
  ################################################################
   trainind <- sample(1:n,n*(1-v.size))
    trainind2 <- (1:n)[-trainind]
    rff <- list()
    #rf.use <- list()
    er <- c()
    if (info == TRUE) 
    cat("Assessing survival trees based on individual performance..............\n")
    for (j in 1:t.initial) {
      rff[[j]] <- ranger(frm, data = data[trainind,],num.trees = 1,num.threads = 1,
                         always.split.variables = always.split.features,mtry=mtry,
                         min.node.size=minimum.node.size, splitrule = splitting.rule,
                         replace=replace)
      er[j] <- rff[[j]]$prediction.error
    #   if(j==1)
    #     frm2 <- as.formula(paste(paste(paste(fm)[[2]],"~"),paste(rff[[j]]$forest$independent.variable.names, collapse="+")))
    }
    order1 <- order(er)
    rff <- rff[order1]
    rf.all <- rff[[1]]
    RF.ALL <- rff[[1]]
     if (info == TRUE) 
    cat("Assessing survival trees based on ensemble performance..............\n")
    
    times <- order(rff[[1]]$unique.death.times)
    v.time <- quantile(times)[2]

    for (k in 1:(M * length(rff) - 1)) {
      # Using pec for IBS estimation
      PredError <- pec(object=rf.all, exact==TRUE,
                       formula = frm, cens.model="marginal",
                       data=data[trainind2,], verbose=F)
      IBS1 <- crps(object = PredError, times =v.time, start = PredError$start)[2,1]
      
      
      rf.all <- comb.ranger(rf.all, rff[[k + 1]])
      #prediction after adding tree
      PredError <- pec(object=rf.all, exact==TRUE,
                       formula = frm, cens.model="marginal",
                       data=data[trainind2,], verbose=F)
      IBS2 <- crps(object = PredError, times =v.time, start = PredError$start)[2,1]
      
      if (IBS1 > IBS2) 
        RF.ALL <- comb.ranger(RF.ALL, rff[[k + 1]])
    }
    
    trees_selected <- RF.ALL$num.trees
    if(info)
    cat("Number of trees selected..............",trees_selected,"\n\n")
    results <- list("trees_selected"=trees_selected,"unique.death.times"=RF.ALL$unique.death.times,
                    "CHF"=RF.ALL$chf,"Survival_Prob"=RF.ALL$survival,"mtry"=RF.ALL$mtry,"forest"=RF.ALL)
    return(results)
 
    }
