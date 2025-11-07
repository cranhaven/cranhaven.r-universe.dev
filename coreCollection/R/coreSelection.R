.computeRandomSelectionCoreCollection <- function(object) {
  #get data
  dm <- as.matrix(object$distanceMatrix)
  preselected <- match(object$preselected, labels(object$distanceMatrix))-1
  n <- object$n
  seed <- object$seed;
  return(computeRandomSelection(dm, n, preselected, seed))
}

.computeAdjustedSelectionCoreCollection <- function(object, groups) {
  #compute adjusted group with correct method
  dm <- as.matrix(object$distanceMatrix)
  if(object$adjustedGroupMethod=="split") {
    preselected <- match(object$preselected, labels(object$distanceMatrix))-1
    return(computeAdjustedSelectionUsingSplitMethod(dm, groups, preselected))
  } else if(object$adjustedGroupMethod=="recompute") {
    adjustedSelected <- rownames(object$randomSelected[which(object$randomSelected[,"preselects"]==0),])
    adjustedSelected <- c(adjustedSelected, object$preselected)
    adjustedSelected <- match(adjustedSelected, labels(object$distanceMatrix))-1
    return(computeAdjustedSelectionUsingRecomputeMethod(dm, adjustedSelected))
  } else {
    stop(paste0("unknown adjustedGroupMethod ",object$adjustedGroupMethod))
  }
}

.computeCoreSelectionCoreCollection <- function(object) {
  #cat(paste0(mem_used(),"\n"))
  #cat("Compute core\n")
  dm <- as.matrix(object$distanceMatrix)
  rawGroups <- object$adjustedBasedGroups
  for(groupName in names(rawGroups)) {
    if(groupName %in% object$preselected) {
      rawGroups[[groupName]] <- match(c(groupName), labels(object$distanceMatrix))-1
    } else {
      rawGroups[[groupName]] <- match(rawGroups[[groupName]], labels(object$distanceMatrix))-1
    }
  }
  names(rawGroups) <- match(names(rawGroups), labels(object$distanceMatrix))-1
  result <- computeCore(object$algorithm,object$coreSelectMethod,dm,rawGroups)
  if(is.null(result)) {
    stop(paste0("no result, possibly unknown coreSelectMethod ",object$coreSelectMethod))
  }
  #cat("Computed core\n")
  #cat(paste0(mem_used(),"\n"))
  return(as.numeric(result))
}

.computeMeasure <- function(object, method) {
  if(!missing(method) && !is.null(method)) {
    dm <- as.matrix(object$distanceMatrix)
    c <- match(rownames(object$core), labels(object$distanceMatrix))-1
    return(computeMeasure(method, dm, c))
  } else {
    return(paste0("couldn't compute measure for method",method))
  }
}

.computeAlternativeCore <- function(object, n) {
  if(!missing(n) && !is.null(n)) {
    alternativeCore <- object$core
    alternativeCore[[paste0("alternative.",n)]] <- NA
    dm <- as.matrix(object$distanceMatrix)
    groups <- object$adjustedBasedGroups
    list <- labels(groups)
    for(i in 1:length(list)) {
      groupEntries <- unlist(groups[list[i]])
      if(length(groupEntries)>n) {
        #get core entry
        coreEntry <- groupEntries[groupEntries %in% rownames(object$core)][[1]]
        #remove main entry
        groupEntries <- groupEntries[!(groupEntries %in% rownames(object$core))]
        #compute distances
        distances <- unlist(lapply(groupEntries, function(x) {return(dm[list[[i]],x])}))
        #select nth from ordered entries
        alternativeCore[[coreEntry,paste0("alternative.",n)]] <- groupEntries[order(distances)][n]
      }
    }
    return(alternativeCore)
  }
}

