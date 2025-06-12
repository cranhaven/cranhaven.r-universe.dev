#' @title Curve of phylogenetic signal at metacommunity level
#' 
#' @description The function estimate the phylogenetic signal at metacommunity level and draws
#' a representation curve.
#' 
#' @details The sequence species show up in the community data matrix must be the 
#' same as they show up in the phylogenetic distance matrix and in traits
#' matrix. The function \code{\link{organize.pcps}} organizes the data, placing the matrices of 
#' community and phylogenetic distance and trait in the same order. The function use of function 
#' organize.pcps is not requered for run the functions, but is recommended. In this way 
#' the arguments comm and phylodist can be specified them as normal arguments or by passing
#' them with the object returned by the function \code{\link{organize.pcps}} using, in this
#' case only the argument comm. Using the object returned by organize.pcps, the comm argument 
#' is used as an alternative way of entering to set all data.frames/matrices, and therefore 
#' the arguments phylodist and trait must not be specified.
#' 
#' The PCPS are used, in a sequential manner, as predictors in a linear regression
#' to model the trait averages across the metacommunity. The curve is drawn as the
#' percentage of cumulative eigenvalues in the abscissa and as the determination 
#' coefficient of regressions in the ordinate.
#' 
#' Two null models are available. The first one (ts), the null curves are generated
#' shuffling terminal tips across the phylogenetic tree, generates a set of random PCPS
#' and recalculates the curves. The second (bm), the null curves are generated with 
#' simulate traits evolving under Brownian motion model. 
#'
#' @encoding UTF-8
#' @include pcps.R
#' @importFrom ape rTraitCont
#' @importFrom vegan vegdist
#' @importFrom stats quantile
#' @importFrom RcppArmadillo fastLm
#' @importFrom graphics plot points segments
#' @importFrom parallel makeCluster clusterExport clusterApply stopCluster
#' @aliases pcps.curve print.pcpscurve summary.pcpscurve plot.pcpscurve
#' @param comm Community data, with species as columns and sampling units as rows. This 
#' matrix can contain either presence/absence or abundance data.
#' Alternatively comm can be an object of class metacommunity.data, an alternative
#' way to set all data.frames/matrices. When you use the class metacommunity.data the arguments
#' trait and phylodist must not be specified. See details.
#' @param phylodist Matrix containing phylogenetic distances between species.
#' @param trait Matrix data of species described by traits, with traits as columns and species as rows.
#' @param checkdata Logical argument (TRUE or FALSE) to check if species
#' sequence in the community data follows the same order as the one in the
#' trait and in the phylodist matrices (Default checkdata = TRUE).
#' @param method Dissimilarity index, as accepted by \code{\link{vegdist}} (Default dist = "bray").
#' @param squareroot Logical argument (TRUE or FALSE) to specify if use square root of dissimilarity
#' index (Default squareroot = TRUE).
#' @param ranks Logical argument (TRUE or FALSE) to specify if ordinal variables are 
#' convert to ranks (Default ranks = TRUE).
#' @param null.model.ts Logical argument (TRUE or FALSE) to specify if use null model that shuffles
#' terminal tips across the phylogenetic tree to generate null curves. See details (Default null.model.ts = FALSE).
#' @param null.model.bm Logical argument (TRUE or FALSE) to specify if use null model that simulate 
#' trait evolving under Brownian motion to generate null curves. See details (Default null.model.bm = FALSE).
#' @param tree Phylogenetic tree, as phylo object.
#' @param runs Number of randomizations.
#' @param progressbar Logical argument (TRUE or FALSE) to specify if display a progress bar 
#' on the R console (Default progressbar = FALSE).
#' @param parallel Number of parallel processes or a predefined socket cluster done with parallel package. Tip: use detectCores() (Default parallel = NULL).
#' @param values The eigenvalues, relative eigenvalues and cumulative relative eigenvalues returned by \code{\link{pcps}}. 
#' @param vectors The principal coordinates of phylogenetic structure returned by \code{\link{pcps}}.
#' @param mt Matrix containing trait average at community level for one trait.
#' @param object An object of class pcpscurve.
#' @param x An object of class pcpscurve.
#' @param probs Numeric vector of probabilities used by \code{\link{quantile}}. (Default probs = c(0.025, 0.975)).
#' @param type Type of the plot to be drawn (Default type = "b").
#' @param draw.model Type of null model to draw; none (none), taxa shuffle (ts), browian motion model (bm).
#' @param col Plot color.
#' @param model.col Color of lines of null models.
#' @param ... Further graphical parameters for points.
#' @return \item{curve.obs}{The cumulative PCPS eigenvalues and the coefficient of determination.}
#' \item{curve.null.ts}{The cumulative PCPS eigenvalues and the coefficient of determination for 
#' each randomization using the taxa shuffle null model.} \item{curve.null.bm}{The cumulative PCPS 
#' eigenvalues and the coefficient of determination for each randomization using the Brownian motion null model.}
#' @note \strong{IMPORTANT}: The sequence of species in the community data matrix
#' MUST be the same as that in the phylogenetic distance matrix and in traits
#' matrix. See details and \code{\link{organize.pcps}}.
#' @author Vanderlei Julio Debastiani <vanderleidebastiani@@yahoo.com.br>
#' @seealso \code{\link{matrix.p}}, \code{\link{pcps}}
#' @references Duarte, L.S. (2011). Phylogenetic habitat filtering influences forest nucleation
#' in grasslands. Oikos, 120, 208:215.
#' @keywords PCPS
#' @examples
#' 
#' \dontrun{
#' data(flona)
#' res<-pcps.curve(flona$community, flona$phylo, flona$trait[,1,drop = FALSE], 
#'        null.model.ts = TRUE, runs = 9)
#' res
#' summary(res)
#' plot(res, draw.model = "ts", type = "b", col = "red")
#' }
#' 
#' @export
pcps.curve <- function(comm, phylodist, trait, checkdata = TRUE, 
                       method = "bray", squareroot = TRUE, ranks = TRUE, 
                       null.model.ts = FALSE, null.model.bm = FALSE, tree, 
                       runs = 99, progressbar = FALSE, parallel = NULL){  
  res <- list(call= match.call())
  if (inherits(comm, "metacommunity.data")) {
    if (!missing(phylodist) | !missing(trait)) {
      stop("\n When you use an object of class metacommunity.data the arguments phylodist and trait must not be specified. \n")
    }
    phylodist <- comm$phylodist
    trait <- comm$traits
    comm <- comm$community
  }
  list.warning <- list()
  if(checkdata){
    organize.temp <- organize.pcps(comm, phylodist = phylodist, trait = trait, check.comm = TRUE)
    if(!is.null(organize.temp$stop)){
      organize.temp$call <- match.call()
      return(organize.temp)
    }
    list.warning <- organize.temp$list.warning
    comm <- organize.temp$community
    phylodist <- organize.temp$phylodist
    trait <- organize.temp$traits
  }
  if(length(list.warning)>0){
    res$list.warning <- list.warning
  }
  if(ncol(trait)!=1){
    stop("\n Only one trait is allowed\n")
  }
  MT <- SYNCSA::matrix.t(comm, trait, scale = FALSE, ranks = ranks, notification = FALSE)$matrix.T
  res.pcps <- pcps(comm, phylodist, method = method, squareroot = squareroot, correlations = FALSE)
  res.values <- res.pcps$values
  res.vectors <- res.pcps$vectors
  curve.obs <- pcpc.curve.calc(res.values, res.vectors, MT)
  rownames(curve.obs) <- rownames(res.values)
  res$curve.obs <- curve.obs
  if(progressbar){
    if(null.model.ts & null.model.bm){
      BarRuns <- runs*2
    }else{
      BarRuns <- runs
    }
  }
  newClusters <- FALSE
  if (is.numeric(parallel)) {
    parallel <- parallel::makeCluster(parallel, type = "PSOCK")
    newClusters <- TRUE
  }
  ptest.ts <- function(samp, comm, phylodist, method, squareroot, mt){
    pcps.null <- PCPS::pcps(comm, phylodist[samp, samp], method = method, squareroot = squareroot, correlations = FALSE)
    res <- PCPS::pcpc.curve.calc(pcps.null$values, pcps.null$vectors, mt)
    return(res)
  }
  ptest.bm <- function(samp, tree, comm, values, vectors, ranks){
    trait.null <- cbind(ape::rTraitCont(phy = tree, model = "BM"))
    match.names <- match(colnames(comm), rownames(trait.null))
    MT.null <- SYNCSA::matrix.t(comm, trait.null[match.names,,drop = FALSE], scale = FALSE, ranks = ranks, notification = FALSE)$matrix.T
    res <- PCPS::pcpc.curve.calc(values, vectors, MT.null)
    return(res)
  }
  if(null.model.ts){
    seqpermutation <- SYNCSA::permut.vector(ncol(phylodist), nset = runs)
    seqpermutation <- lapply(seq_len(nrow(seqpermutation)), function(i) seqpermutation[i,])
    if(!inherits(parallel, "cluster")){
      res.curve.null.ts <- vector("list", runs)
      for (i in 1:runs) {
        res.curve.null.ts[[i]] <- ptest.ts(samp = seqpermutation[[i]], comm = comm, phylodist = phylodist, method = method, squareroot = squareroot, mt = MT)   
        if(progressbar){
          SYNCSA::ProgressBAR(i, BarRuns, style = 3)
        }
      }
    } else {
      res.curve.null.ts <- parallel::clusterApply(parallel, seqpermutation, ptest.ts, comm = comm, phylodist = phylodist, method = method, squareroot = squareroot, mt = MT)		
    }	
    res$curve.null.ts <- res.curve.null.ts
  }
  if(null.model.bm){
    seqpermutation <- vector("list",runs)
    if(!inherits(parallel, "cluster")){
      res.curve.null.bm <- vector("list",runs)
      for (i in 1:runs) {
        res.curve.null.bm[[i]] <- ptest.bm(NULL, tree, comm, res.values, res.vectors, ranks = ranks)
        if(progressbar){
          SYNCSA::ProgressBAR(i+runs, BarRuns, style = 3)
        }
      }
    } else {
      res.curve.null.bm <- parallel::clusterApply(parallel, seqpermutation, ptest.bm, tree = tree, comm = comm, values = res.values, vectors = res.vectors, ranks = ranks)		
    }
    res$curve.null.bm <- res.curve.null.bm
  }
  if (newClusters) {
    parallel::stopCluster(parallel)
  }
  class(res) <- "pcpscurve"
  return(res)
}