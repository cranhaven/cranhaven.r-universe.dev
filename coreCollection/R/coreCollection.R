#' @importFrom methods setOldClass
methods::setOldClass("dist")

.showCoreCollection <- function(object, title) {
  cat("==========\n")
  cat(paste0(title,"\n"))
  cat(paste0("- distanceMatrix for ",attr(object$distanceMatrix,"Size")," items\n"))
  cat(paste0("- required core size is ",object$n))
  lengthPreselected <- length(object$preselected);
  if(lengthPreselected>0) {
    cat(paste0(" with ",lengthPreselected," fixed item",ifelse(lengthPreselected>1,"s","")," to include\n"))
  } else {
    cat("\n");
  }
  cat(paste0("- random selected items: ",nrow(object$randomSelected),"\n"))
  if(lengthPreselected>0) {
    cat(paste0("- adjusted selected items: ",nrow(object$adjustedSelected),"\n"))
  }
  cat(paste0("- core items: ",nrow(object$core),"\n"))
  cat(paste0("- seed: ",object$seed,"\n"))
  if(lengthPreselected>0) {
    cat(paste0("The adjustedGroupMethod is '",object$adjustedGroupMethod,"' and the "))
  } else {
    cat("The ")
  }
  cat(paste0("coreSelectMethod is '",object$coreSelectMethod,"'\n"))
  cat(paste0("Applied algorithm is '",object$algorithm,"'\n"))
  cat("==========\n")
}

#' The CoreCollection Class
#'
#' @docType class
#' @useDynLib coreCollection
#' @importFrom R6 R6Class
#' @importFrom Rcpp evalCpp
#' @keywords internal
#' @format A \code{\link{R6Class}} generator object
.CoreCollectionClass <- R6Class(
  classname="coreCollection",
  private = list(
    initialised = FALSE,
    constantAvailableCoreSelectMethods = c("A-NE", "E-NE", "E-E"),
    constantAvailableAlgorithms = c("randomDescent"),
    constantAvailableAdjustedGroupMethods = c("split", "recompute"),
    variableDistanceMatrix = NULL,
    variableN = NULL,
    variablePreselected = NULL,
    variableRandomSelected = NULL,
    variableRandomBasedGroups = NULL,
    variableAdjustedGroupMethod = NULL,
    variableAdjustedSelected = NULL,
    variableAdjustedBasedGroups = NULL,
    variableCoreSelectMethod = NULL,
    variableCoreSelected = NULL,
    variableAlgorithm = NULL,
    variableSeed = NULL,
    variableRandomSeed = NULL,
    setDistanceMatrix=function(value) {
      if(!missing(value) && !is.null(value)) {
        distanceMatrix <- as.dist(value)
        if(attr(distanceMatrix,"Size")<=1) {
          stop("distanceMatrix size should be > 1")
        }
        # enforce labels if necessary
        if(is.null(labels(distanceMatrix))) {
          distanceMatrix <- as.matrix(distanceMatrix)
          rownames(distanceMatrix) <- seq(1,nrow(distanceMatrix))
          distanceMatrix <- as.dist(distanceMatrix)
        }
        private$variableDistanceMatrix <- distanceMatrix
      }
    },
    setN=function(value) {
      if(!missing(value) && !is.null(value)) {
        n <- as.integer(value);
        if(is.na(n) | n<1) {
          stop(paste0("n (",n,") should be a postive integer"))
        } else {
          sizeDistanceMatrix <- ifelse(is.null(private$variableDistanceMatrix),0,attr(private$variableDistanceMatrix,"Size"));
          if(n>=sizeDistanceMatrix) {
            stop(paste0("n (",n,") should be less than the size of the distanceMatrix (",sizeDistanceMatrix,")"))
          }
        }
        private$variableN <- n;
      }
    },
    setPreselected=function(value) {
      if(!missing(value) && !is.null(value)) {
        preselected <- unique(as.vector(value, mode="character"))
        l <- length(preselected)
        if(l>=private$variableN) {
          stop(paste0("preselected should not contain equal or more items (",l,") than the requested core size (",private$variableN,")"))
        } else if(l>0) {
          labels <- labels(private$variableDistanceMatrix)
          diff <- setdiff(preselected,labels)
          if(length(diff)>0) {
            stop(paste0("preselected should not not contain items not included in the distanceMatrix: ",paste(diff,collapse=", "),"\n"))
          } else {
            private$variablePreselected <- preselected
          }
        } else {
          private$variablePreselected <- c()
        }
      } else {
        private$variablePreselected <- c()
      }
    },
    setCoreSelectMethod=function(value) {
      if(!missing(value) && !is.null(value)) {
        method <- as.character(value)
        if(length(method)==1 && method %in% private$constantAvailableCoreSelectMethods) {
          private$variableCoreSelectMethod <- method
        } else {
          stop(paste0("can't use '",paste(method,collapse=", "),"' as coreSelectMethod, allowed: '",paste(private$constantAvailableCoreSelectMethods, collapse="', '"),"'"))
        }
      } else {
        private$variableCoreSelectMethod <- private$constantAvailableCoreSelectMethods[1];
      }
    },
    setAlgorithm=function(value) {
      if(!missing(value) && !is.null(value)) {
        algorithm <- as.character(value)
        if(length(algorithm)==1 && algorithm %in% private$constantAvailableAlgorithms) {
          private$variableAlgorithm <- algorithm
        } else {
          stop(paste0("can't use '",paste(algorithm,collapse=", "),"' as algorithm, allowed: '",paste(private$constantAvailableAlgorithms, collapse="', '"),"'"))
        }
      } else {
        private$variableAlgorithm <- private$constantAvailableAlgorithms[1];
      }
    },
    setSeed=function(value) {
      if(!missing(value) && !is.null(value)) {
        seed <- as.integer(value)
        if(!is.na(seed)) {
          private$variableSeed <- seed
          set.seed(private$variableSeed)
        } else {
          stop(paste0("can't use '",paste(value,collapse=", "),"' as seed, use integer"))
        }
      } else if(is.null(value)) {
        private$variableRandomSeed=round((as.numeric(Sys.time())*1000)%%.Machine$integer.max)
        set.seed(private$variableRandomSeed)
      } else {
        private$variableSeed <- NULL
      }
    },
    setAdjustedGroupMethod=function(value) {
      if(!missing(value) && !is.null(value)) {
        method <- as.character(value)
        if(length(method)==1 && method %in% private$constantAvailableAdjustedGroupMethods) {
          private$variableAdjustedGroupMethod <- method
        } else {
          stop(paste0("can't use '",paste(method,collapse=", "),"' as adjustedGroupMethod, allowed: '",paste(private$constantAvailableAdjustedGroupMethods, collapse="', '"),"'"))
        }
      } else {
        private$variableAdjustedGroupMethod <- private$constantAvailableCoreSelectMethods[1];
      }
    },
    setRandomSelected=function(value) {
      if(!missing(value) && !is.null(value)) {
        result <- as.vector(value, mode="character")
        labels <- labels(private$variableDistanceMatrix)
        diff <- setdiff(result,labels)
        if(length(diff)>0) {
          stop(paste0("random selected set should not not contain rownames not included in the distanceMatrix: ",paste(diff,collapse=", "),"\n"))
        } else {
          randomSelectedTable <- table(result)
          #set randomSelected set including frequencies
          private$variableRandomSelected <- data.frame(contains=as.numeric(randomSelectedTable))
          rownames(private$variableRandomSelected) <- rownames(randomSelectedTable)
          if(length(private$variablePreselected)>0) {
            preselectedTable <- table(result[match(private$variablePreselected, labels)])
            private$variableRandomSelected[,"preselects"] <- 0
            private$variableRandomSelected[rownames(preselectedTable),"preselects"] <- as.numeric(preselectedTable)
            private$variableRandomSelected[,"preselected"] <- (rownames(private$variableRandomSelected) %in% private$variablePreselected)
          } else {
            private$variableRandomSelected[,"preselects"] <- 0
            private$variableRandomSelected[,"preselected"] <- FALSE
          }
          private$variableRandomSelected[,"random"] <- TRUE
          private$variableRandomBasedGroups <- result
          names(private$variableRandomBasedGroups) <- labels
        }
      } else {
        private$variableRandomSelected <- NULL
        private$variableRandomBasedGroups <- NULL
      }
    },
    setAdjustedSelected=function(value) {
      if(!missing(value) && !is.null(value)) {
        result <- as.vector(value, mode="character")
        labels <- labels(private$variableDistanceMatrix)
        diff <- setdiff(result,labels)
        if(length(diff)>0) {
          stop(paste0("adjusted selected set should not not contain rownames not included in the distanceMatrix: ",paste(diff,collapse=", "),"\n"))
        } else {
          adjustedSelectedTable <- table(result)
          #set adjustedSelected set including frequencies
          private$variableAdjustedSelected <- data.frame(contains=as.numeric(adjustedSelectedTable))
          rownames(private$variableAdjustedSelected) <- rownames(adjustedSelectedTable)
        }
        if(length(private$variablePreselected)>0) {
          preselectedTable <- table(result[match(private$variablePreselected, labels)])
          private$variableAdjustedSelected[,"preselects"] <- 0
          private$variableAdjustedSelected[rownames(preselectedTable),"preselects"] <- as.numeric(preselectedTable)
          private$variableAdjustedSelected[,"preselected"] <- (rownames(private$variableAdjustedSelected) %in% private$variablePreselected)
        } else {
          private$variableAdjustedSelected[,"preselects"] <- 0
          private$variableAdjustedSelected[,"preselected"] <- FALSE
        }
        private$variableAdjustedSelected[,"random"] <- (rownames(private$variableAdjustedSelected) %in% rownames(private$variableRandomSelected))
        private$variableAdjustedBasedGroups <- result
        names(private$variableAdjustedBasedGroups) <- labels
      } else {
        private$variableAdjustedSelected <- NULL
        private$variableAdjustedBasedGroups <- NULL
      }
    },
    setCore=function(value) {
      if(!missing(value) && !is.null(value)) {
        result <- as.vector(value, mode="character")
        labels <- labels(private$variableDistanceMatrix)
        diff <- setdiff(result,labels)
        if(length(diff)>0) {
          stop(paste0("core set should not not contain rownames not included in the distanceMatrix: ",paste(diff,collapse=", "),"\n"))
        } else {
          coreSelectedTable <- table(result)
          #set coreSelected set including frequencies
          private$variableCoreSelected <- data.frame(contains=as.numeric(coreSelectedTable))
          rownames(private$variableCoreSelected) <- rownames(coreSelectedTable)
        }
        if(length(private$variablePreselected)>0) {
          preselectedTable <- table(result[match(private$variablePreselected, labels)])
          private$variableCoreSelected[,"preselected"] <- (rownames(private$variableCoreSelected) %in% private$variablePreselected)
        } else {
          private$variableCoreSelected[,"preselected"] <- FALSE
        }
        private$variableCoreSelected$contains <- NULL
      } else {
        private$variableCoreSelected <- NULL
      }
    },
    setInitialised=function(title) {
      if(!private$initialised) {
        #random result
        rawRandomResult <- .computeRandomSelectionCoreCollection(self)
        randomResult <- labels(private$variableDistanceMatrix)[as.numeric(rawRandomResult)+1]
        names(randomResult) <- labels(private$variableDistanceMatrix)
        private$setRandomSelected(randomResult)
        #adjusted result
        rawAdjustedResult <- .computeAdjustedSelectionCoreCollection(self, rawRandomResult)
        adjustedResult <- labels(private$variableDistanceMatrix)[as.numeric(rawAdjustedResult)+1]
        names(adjustedResult) <- labels(private$variableDistanceMatrix)
        private$setAdjustedSelected(adjustedResult)
        #create core
        rawCoreResult <- .computeCoreSelectionCoreCollection(self)
        coreResult <- labels(private$variableDistanceMatrix)[as.numeric(rawCoreResult)+1]
        private$setCore(coreResult)
        #finished
        private$initialised = TRUE
        .showCoreCollection(self, title)
      } else {
        cat("CoreCollection not initialised!\n")
      }
    }
  ),
  public = list(
    recompute = function() {
      if(private$initialised) {
        private$initialised = FALSE
        private$setSeed(private$variableSeed)
        private$setInitialised("Recompute Core Collection Object")
      }
    },
    alternativeCore = function(n) {
      if(private$initialised) {
        if(!missing(n) && !is.null(n)) {
          return(.computeAlternativeCore(self, n))
        } else {
          stop("n is required")
        }
      }
    },
    initialize = function(distanceMatrix, n, preselected, coreSelectMethod, adjustedGroupMethod, algorithm, seed) {
      if(!private$initialised) {
        if(!missing(distanceMatrix) && !is.null(distanceMatrix)) {
          private$setDistanceMatrix(distanceMatrix)
        } else {
          stop("distanceMatrix is required")
        }
        if(!missing(n) && !is.null(n)) {
          private$setN(n)
        } else {
          stop("n is required")
        }
        if(!missing(preselected)) {
          private$setPreselected(preselected)
        } else {
          private$setPreselected()
        }
        if(!missing(coreSelectMethod)) {
          private$setCoreSelectMethod(coreSelectMethod)
        } else {
          private$setCoreSelectMethod()
        }
        if(!missing(adjustedGroupMethod)) {
          private$setAdjustedGroupMethod(adjustedGroupMethod)
        } else {
          private$setAdjustedGroupMethod()
        }
        if(!missing(algorithm)) {
          private$setAlgorithm(algorithm)
        } else {
          private$setAlgorithm()
        }
        if(!missing(seed)) {
          private$setSeed(seed)
        } else {
          private$setSeed()
        }
        N <- attr(private$variableDistanceMatrix,"Size")
        #initialize
        private$setInitialised("Created new Core Collection Object")
      } else {
        cat("CoreCollection already initialised!\n")
      }
      invisible(self)
    },
    print = function(...) {
      .showCoreCollection(self,"CoreCollection object")
      invisible(self)
    },
    summary = function(...) {
      .showCoreCollection(self,"CoreCollection object")
      invisible(self)
    },
    measure = function(coreSelectMethod) {
      if(missing(coreSelectMethod)) {
        method <- private$variableCoreSelectMethod
      } else {
        method <- as.character(coreSelectMethod)
        if(length(method)!=1 | !(method %in% private$constantAvailableCoreSelectMethods)) {
          stop(paste0("can't use '",paste(method,collapse=", "),"' as distance method, allowed: '",paste(private$constantAvailableCoreSelectMethods, collapse="', '"),"'"))
        }
      }
      return(.computeMeasure(self, method))
    },
    measures = function(...) {
      m <- c()
      for(method in private$constantAvailableCoreSelectMethods) {
        m <- c(m, .computeMeasure(self, method))
      }
      result <- data.frame(measure=m)
      rownames(result) <- private$constantAvailableCoreSelectMethods
      return(result)
    }
  ),
  active = list(
    distanceMatrix = function(value) {
      if(missing(value)) {
        return(private$variableDistanceMatrix)
      } else {
        cat("Changing distanceMatrix not allowed\n")
      }
    },
    n = function(value) {
      if(missing(value)) {
        return(private$variableN)
      } else {
        cat("Changing n not allowed\n")
      }
    },
    preselected = function(value) {
      if(missing(value)) {
        return(private$variablePreselected)
      } else {
        cat("Changing preselected not allowed\n")
      }
    },
    coreSelectMethod = function(value) {
      if(missing(value)) {
        return(private$variableCoreSelectMethod)
      } else {
        cat("Changing coreSelectMethod not allowed\n")
      }
    },
    adjustedGroupMethod = function(value) {
      if(missing(value)) {
        return(private$variableAdjustedGroupMethod)
      } else {
        cat("Changing adjustedGroupMethod not allowed\n")
      }
    },
    algorithm = function(value) {
      if(missing(value)) {
        return(private$variableAlgorithm)
      } else {
        cat("Changing algorithm not allowed\n")
      }
    },
    randomSelected = function(value) {
      if(missing(value)) {
        return(private$variableRandomSelected)
      } else {
        cat("Changing random selected set not allowed\n")
      }
    },
    adjustedSelected = function(value) {
      if(missing(value)) {
        return(private$variableAdjustedSelected)
      } else {
        cat("Changing adjusted selected set not allowed\n")
      }
    },
    core = function(value) {
      if(missing(value)) {
        return(private$variableCoreSelected)
      } else {
        cat("Changing core not allowed\n")
      }
    },
    randomBasedGroups = function(value) {
      if(missing(value)) {
        df <- data.frame(random=private$variableRandomBasedGroups)
        groups <- split(df,df$random)
        for(groupName in names(groups)) {
          groups[[groupName]] <- rownames(groups[[groupName]])
        }
        return(groups)
      } else {
        cat("Changing random selected groups not allowed\n")
      }
    },
    adjustedBasedGroups = function(value) {
      if(missing(value)) {
        df <- data.frame(adjusted=private$variableAdjustedBasedGroups)
        groups <- split(df,df$adjusted)
        for(groupName in names(groups)) {
          groups[[groupName]] <- rownames(groups[[groupName]])
        }
        return(groups)
      } else {
        cat("Changing adjusted selected groups not allowed\n")
      }
    },
    pop = function(value) {
      if(missing(value)) {
        result <- c()
        labels <- labels(private$variableDistanceMatrix)
        coreLabels <- rownames(private$variableCoreSelected)
        preselectedLabels <- private$variablePreselected
        for(item in labels) {
          if(item %in% coreLabels) {
            if(!is.null(preselectedLabels) && (item %in% preselectedLabels)) {
              result <- c(result, "core - preselected")
            } else {
              result <- c(result, paste0("core - ",private$variableCoreSelectMethod))
            }
          } else {
            result <- c(result, "other")
          }
        }
        result <- data.frame(result)
        rownames(result) <- labels(private$variableDistanceMatrix)
        return(result)
      } else {
        cat("Changing pop not allowed\n")
      }
    },
    seed = function(value) {
      if(missing(value)) {
        if(!is.null(private$variableSeed)) {
          return(private$variableSeed)
        } else {
          return(private$variableRandomSeed)
        }
      } else {
        cat("Changing seed not allowed\n")
      }
    }

  )
)

#' The CoreCollection class
#'
#' R6 class for creating a core collection based on the provided \code{distanceMatrix},
#' required size of the core \code{n} and optionally a set of \code{preselected} accessions to be included
#' into the core.
#'
#' Based on a provided \code{distanceMatrix} and required number \code{n} of accessions
#' within the core, a random set of accessions is created, implicitly dividing the full
#' population into initial groups based on the nearest randomly chosen random accession. If a
#' set of \code{preselected} accessions is provided, this initial division is adjusted using the
#' \code{adjustedGroupMethod}. Then, using the \code{coreSelectMethod} in the \code{algorithm}, the
#' core accessions within these groups are calculated, resulting in the final core collection.
#'
#' @param distanceMatrix A distance matrix; can be either a \link[=matrix]{matrix} or a \link[stats:dist]{dist}
#' @param n The number of items in the core
#' @param preselected An optional list of preselected accessions to be included in the core collection;
#' the provided accessions should occur in the labels or rownames of the provided distanceMatrix
#' @param coreSelectMethod The method for \link[coreCollection:computeCore]{computing} core accessions within the groups:
#' \code{A-NE} (accession nearest entry), \code{E-NE} (entry nearest entry) or \code{E-E} (entry entry)
#' @param adjustedGroupMethod The method to handle adjusting groups when multiple preselected accessions occur within a single group:
#' \code{split} to just split the initial groups with multiple accessions or \code{recompute} to recompute the division of
#' accessions over the groups.
#' @param algorithm Algorithm applied to \link[coreCollection:computeCore]{compute} a solution: currently, only \code{randomDescent} is available
#' @param seed The seed used when generating the core collection. If no seed is provided, a random
#' seed is chosen and each time the \code{recompute()} method is called on the object, a new seed will be used.
#' @field adjustedBasedGroups A list describing the initial random division of all accessions into groups, adjusted for the
#' set of \code{preselected} accessions by using the defined \code{adjustedGroupMethod}.
#' @field adjustedGroupMethod The method to handle adjusting groups when multiple preselected accessions occur within a single group.
#' @field adjustedSelected A \link[=data.frame]{data.frame} representing the intial random selection of accesions, adjusted for the
#' set of \code{preselected} accessions by using the defined \code{adjustedGroupMethod}, with the accession names as labels and the following columns: \itemize{
#' \item \code{contains}: the (positive) number of accessions that have this accessions as the closest random selected accession
#' \item \code{preselects}: the number of these closest accessions that were preselected
#' \item \code{preselected}: a boolean indicating if the random selected accession was preselected
#' \item \code{random}: a boolean indiciating if the selected accesion was initially randomly chosen or introduced later by the applied \code{adjustedGroupMethod}.
#' }
#' @field algorithm The applied algorithm to compute the solution.
#' @field core A \link[=data.frame]{data.frame} representing the core collection with the accession names as labels and in the first and only column a boolean value indicating whether or not the accession was preselected.
#' @field coreSelectMethod The applied method to select the core accessions based on the computed \code{adjustedBasedGroups}.
#' @field distanceMatrix The distance matrix; this will allways be a \link[stats:dist]{dist} object.
#' @field n The required core size
#' @field pop A \link[=data.frame]{data.frame} representing the whole collection with the accession names as labels and in the first and only column:\itemize{
#' \item \code{result}: a string describing if the accession is marked as \code{other} or as included in the \code{core}, and if in the \code{core} because it was \code{preselected} or because of the applied \code{coreSelectMethod}.
#' }
#' @field preselected The list of preselected accessions.
#' @field randomBasedGroups A list with the initial division into groups based on the initial random selection of accessions described by \code{randomSelected}. Each item describes all accessions that have the random selected accesion from the label as the nearest neighbour, including the random selected accession.
#' @field randomSelected A \link[=data.frame]{data.frame} representing the intial random selection of accesions with the accession names as labels and the following columns: \itemize{
#' \item \code{contains}: the (positive) number of accessions that have this accessions as the closest random selected accession
#' \item \code{preselects}: the number of these closest accessions that were preselected
#' \item \code{preselected}: a boolean indicating if the random selected accession was preselected
#' \item \code{random}: a boolean indiciating if the random selected accesion was randomly chosen. This will always be TRUE for this field, but including this column makes the output comparable with \code{adjustedSelected}.
#' }
#' @field seed The last applied seed for the randomizer. This will only change when the \code{recompute()} method
#' is called and no initial \code{seed} is defined.
#' @section Methods:
#' \describe{
#'   \item{\code{alternativeCore(n)}}{The \code{n}th alternative core with \code{n} a positive integer. Provides for each accession in the core, if available, the \code{n}th nearest accession from within the same group as an alternative.}
#'   \item{\code{clone(deep = FALSE)}}{The default \link{R6Class} clone method.}
#'   \item{\code{initialize(distanceMatrix, n, preselected, coreSelectMethod, adjustedGroupMethod, algorithm, seed)}}{Initialisation of the object, is called automatically on creation or recomputing.}
#'   \item{\code{measure(coreSelectMethod)}}{The measure for the provided \code{coreSelectMethod}. If no value is provided, the current selected \code{coreSelectMethod} is used. The measure is used by the algorithm to compute the core collection.}
#'   \item{\code{measures()}}{A \link[=data.frame]{data.frame} with the available \code{coreSelectMethods} as labels and in the first and only column the measures for these methods.}
#'   \item{\code{recompute()}}{Recompute the core collection: If on initialisation of the object a seed was provided, this same seed will be applied and therefore the same core collection will be created. Otherwise, a new seed is generated, resulting in a new core.}
#'   \item{\code{print()}}{Create a summary of the core collection object, same as \code{summary()}.}
#'   \item{\code{summary()}}{Create a summary of the core collection object, same as \code{print()}.}
#' }
#' @name CoreCollection
#' @aliases CoreCollection
#' @family core collection
#' @export
#' @examples
#' \donttest{
#' if(requireNamespace("vcfR")) {
#'   # Get a vcf-file
#'   library(vcfR)
#'   vcfFile <- tempfile()
#'   vcfFileSource <- paste0(
#'     "ftp://ftp-trace.ncbi.nih.gov/",
#'     "1000genomes/ftp/pilot_data/release/2010_07",
#'     "/exon/snps/CEU.exon.2010_03.genotypes.vcf.gz")
#'   download.file(vcfFileSource, vcfFile)
#'   vcf <- read.vcfR(vcfFile)
#'   file.remove(vcfFile)
#'
#'   # Create a distance object
#'   gl <- vcfR2genlight(vcf)
#'   dist <- dist(gl)
#'
#'   # Create a Core Collection
#'   n = 10
#'   cc <- CoreCollection(dist, n)
#'   cc$core
#'
#'   # Create a Core Collection with preselected accessions
#'   preselected = sample(labels(dist), 5)
#'   ccp <- CoreCollection(dist, n, preselected)
#'   preselected
#'   ccp$core
#'
#'   if(requireNamespace("ggfortify")) {
#'     # Visualize the Core Collection
#'     library(ggfortify)
#'     data = setNames(data.frame(matrix(ncol=2,nrow=attr(dist,"Size"))), c("core","preselected"))
#'     data$core <- labels(dist) %in% rownames(ccp$core)
#'     data$preselected <- labels(dist) %in% preselected
#'     autoplot(prcomp(dist), data=data, colour="core", shape="preselected")
#'   }
#' }

CoreCollection <- function(distanceMatrix, n, preselected=c(), coreSelectMethod="A-NE", adjustedGroupMethod="split", algorithm="randomDescent", seed=NULL) {
  return(.CoreCollectionClass$new(distanceMatrix, n, preselected, coreSelectMethod, adjustedGroupMethod, algorithm, seed))
}
