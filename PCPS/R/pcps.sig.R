#' @title Association between phylogeny-weighted species composition and environmental predictors
#' 
#' @description Analyses to relate an environmental gradient to the phylogenetic assembly of species 
#' across a metacommunity by means of phylogenetic fuzzy weighting.
#' 
#' @details Each metacommunity is submitted to phylogenetic fuzzy weighting, generating a matrix
#' that describing the phylogeny-weighted species composition of the communities
#' (\code{\link{matrix.p}}). The function matrix.p.sig test directly the association 
#' this matrix with the environmental predictors. The pairwise dissimilarities are 
#' submitted to Mantel test (\code{\link{mantel}}) or ADONIS test (\code{\link{adonis}} or \code{\link{adonis2}})
#' to evaluate the influence of an environmental gradient on species dispersion across 
#' the communities. The function pcps.sig generates principal coordinates of phylogenetic
#' structure (\code{\link{pcps}}) and use a single axis for run a generalized linear 
#' model (GLM, \code{\link{glm}}), linear model using generalized least squares (GLS, \code{\link{gls}}),
#' linear mixed-effects models (LME, \code{\link{lme}}) or use set of axis for run a distance-based redundancy
#' analysis (db-RDA, \code{\link{rda}}).
#' 
#' The sequence species show up in the community data matrix must be the 
#' same as they show up in the phylogenetic distance matrix and, similarly, 
#' the sequence of communities in the community data matrix must be the same as that in 
#' the environmental data. The function \code{\link{organize.pcps}} organizes the data, placing the matrices of 
#' community, phylogenetic distance and environmental data in the same order. The function use of function 
#' organize.pcps is not requered for run the functions, but is recommended. In this way 
#' the arguments comm and phylodist can be specified them as normal arguments or by passing
#' them with the object returned by the function \code{\link{organize.pcps}} using, in this
#' case only the argument comm. Using the object returned by organize.pcps, the comm argument 
#' is used as an alternative way of entering to set all data.frames/matrices, and therefore 
#' the arguments phylodist and envir must not be specified.
#' 
#' The significance is obtained via two null models, one that shuffles sites across the
#' environmental gradient and another that shuffles terminal tips (taxa) across the phylogenetic
#' tree. The first null model (site shuffle) shuffles the site position across the environmental
#' gradient and rerun the same model, generating a null F value (or r value in Mantel test). The
#' second null model (taxa shuffle), shuffles terminal tips across the phylogenetic tree and 
#' generates a null matrix containing phylogeny-weighted species composition and rerun the same
#' model, generating another null F value. In the pcps.sig function are generate set of null PCPS
#' and each null PCPS (or set of PCPS in RDA) is submitted to a procrustean adjustment 
#' (see \code{\link{procrustes}}), and the fitted values between observed PCPS and null PCPS is 
#' obtained. The adjusted null PCPS is used to rerun the model, generating another null F value. 
#' The observed F value (or r value) is compared independently with both null sets of F values 
#' (or r value) to generate a probability value of the original F value being generated merely by
#' chance according to each null model.
#' 
#' \strong{The argument FUN}
#' 
#' The type of analysis performed by this function is specified using the argument \emph{FUN}. The current version 
#' of package includes ten predefined function, however additional small functions can be easy specify. All
#' this function uses the environmental variables to analyze the association between phylogeny-weighted species
#' composition and environmental predictors. For matrix P analysis, in \emph{matrix.p.sig} function, the predefined 
#' functions available are \emph{FUN.MANTEL}, \emph{FUN.ADONIS}, \emph{FUN.ADONIS2.global} and \emph{FUN.ADONIS2.margin}. For PCPS 
#' analysis, in \emph{pcps.sig} function, the predefined functions available are \emph{FUN.GLM}, \emph{FUN.RDA}, \emph{FUN.GLS.marginal}, 
#' \emph{FUN.GLS.sequential}, \emph{FUN.LME.marginal} and \emph{FUN.LME.sequential}. The significance 
#' for each null model is performed as described here, NOT using p value of basic functions.
#' 
#' \strong{FUN.MANTEL}
#' 
#' Mantel test that can be used in matrix P analysis. The arguments \emph{method.p} and \emph{sqrt.p} are specified for determine resemblance 
#' index between communities based on P matrix. The argument \emph{method.envir} is specified to determine resemblance 
#' index between communities based on environmental variables. The significance is assess using r value, see more in \code{\link{mantel}}.
#' 
#' \strong{FUN.ADONIS}
#' 
#' Multivariate analysis of variance that can be used in matrix P analysis. The arguments \emph{method.p} and \emph{sqrt.p} are specified for determine resemblance 
#' index between communities based on P matrix. The argument \emph{formula} is specified, where the left hand side gives 
#' the resemblance data, right hand side gives the variables. The resemblance data is internally named \emph{p.dist}, 
#' thus formula is an expression of the form \emph{p.dist ~ model} (see Examples). The significance is assess using overall F value, 
#' see more in \code{\link{adonis}}.
#' 
#' \strong{FUN.ADONIS2.global and FUN.ADONIS2.margin}
#' 
#' Multivariate analysis of variance that can be used in matrix P analysis. The arguments \emph{method.p} and \emph{sqrt.p} are specified for determine resemblance 
#' index between communities based on P matrix. The argument \emph{formula} is specified, where the left hand side gives 
#' the resemblance data, right hand side gives the variables. The resemblance data is internally named \emph{p.dist}, 
#' thus formula is an expression of the form \emph{p.dist ~ model} (see Examples). The significance is assess using F value 
#' and the difference between function is due to the argument \emph{by} in \code{\link{adonis2}}. The function 
#' \emph{FUN.ADONIS2.global} use as default \emph{by = NULL} to assess the overall significance of all terms together
#' whereas the function \emph{FUN.ADONIS2.margin} use as default \emph{by = margin} to assess the marginal effects of 
#' the terms and return F and p value for each term. See more in \code{\link{adonis2}}.
#' 
#' The function \code{\link{adonis2}} evaluate the formula argument in the global environment, however CRAN 
#' do not allow assignments to the global environment. As a temporary workaround, copy and run the lines below to make 
#' the functions FUN.ADONIS2.global and FUN.ADONIS2.margin available.
#' 
#' \preformatted{
#' 
#' FUN.ADONIS2.global <- function(x, envir, method.p, formula, sqrt.p = TRUE, return.model = FALSE){
#' p.dist <- vegan::vegdist(x, method = method.p)
#' if(sqrt.p){
#'   p.dist <- sqrt(p.dist)
#' }
#' assign("p.dist", p.dist, envir = globalenv())
#' mod.obs <- vegan::adonis2(formula, data = data.frame(envir), permutations = 0, by = NULL, parallel = NULL)
#' rm(p.dist, envir = globalenv())
#' statistic.obs <- mod.obs$F[1]
#' if(return.model){
#'   res <- list()
#'   res$mod.obs <- mod.obs
#'   res$statistic.obs <- statistic.obs
#' } else{
#'   res <- statistic.obs
#' }
#' return(res)
#' }
#' 
#' FUN.ADONIS2.margin <- function(x, envir, method.p, formula, sqrt.p = TRUE, return.model = FALSE){
#' p.dist <- vegan::vegdist(x, method = method.p)
#' if(sqrt.p){
#'   p.dist <- sqrt(p.dist)
#' }
#' assign("p.dist", p.dist, envir = globalenv())
#' mod.obs <- vegan::adonis2(formula, data = data.frame(envir), permutations = 2, by = "margin", parallel = NULL)
#' rm(p.dist, envir = globalenv())
#' nf <- length(mod.obs$F)-2
#' statistic.obs <- mod.obs$F[seq_len(nf)]
#' if(return.model){
#'   res <- list()
#'   res$mod.obs <- mod.obs
#'   res$statistic.obs <- statistic.obs
#' } else{
#'   res <- statistic.obs
#' }
#' return(res)
#' }
#'
#' }
#' 
#' \strong{FUN.GLM}
#' 
#' Generalized linear models that can be used in PCPS analysis. The argument \emph{formula} is specified, where the left hand side gives the PCPS used, 
#' right hand side gives the variables. The PCPS are internally named sequentially \emph{pcps.1}, \emph{pcps.2}, \emph{pcps.3} and so 
#' on. Thus, formula is an expression of the form \emph{pcps.1 ~ model} (see Examples). The type of environmental variables are 
#' extracted directly from \emph{envir} argument, thus variables of class \code{\link{factor}} can be already
#' specified in \emph{envir} \code{\link{data.frame}} or through \emph{formula} argument. The significance is assess using overall 
#' F value, see more in \code{\link{glm}}.
#' 
#' \strong{FUN.RDA}
#' 
#' Redundancy analysis that can be used in PCPS analysis. The RDA analysis is performed using all PCPS specified with choices argument and 
#' all environmental variables specified by envir argument. The significance is assess using overall 
#' F value, see more in \code{\link{rda}}.
#' 
#' \strong{FUN.GLS.marginal and FUN.GLS.sequential}
#' 
#' Linear model using generalized least squares that can be used in PCPS analysis. The argument \emph{formula} is specified, where the left hand side gives the PCPS used, 
#' right hand side gives the variables. The PCPS are internally named sequentially \emph{pcps.1}, \emph{pcps.2}, \emph{pcps.3} and so 
#' on. Thus, formula is an expression of the form \emph{pcps.1 ~ model} (see Examples). The type of environmental variables are 
#' extracted directly from \emph{envir} argument, thus variables of class \code{\link{factor}} can be already
#' specified in \emph{envir} \code{\link{data.frame}} or through \emph{formula} argument. The significance is assess using F value 
#' and the difference between function is due to the argument \emph{type} in \code{\link{anova.gls}}. The function 
#' \emph{FUN.GLS.marginal} use as default \emph{type = marginal} to assess the marginal significance of all terms
#' whereas the function \emph{FUN.GSL.sequential} use as default \emph{type = sequential} to assess the sequential effects of 
#' the terms. Those funcitons return all F values calculed by \code{\link{anova.gls}}, including the intercept if it is in the model. 
#' Additional arguments as \emph{correlation} can be passed by \emph{...} argument. See more in \code{\link{gls}} and \code{\link{anova.gls}}.
#' 
#' \strong{FUN.LME.marginal and FUN.LME.sequential}
#' 
#' Linear mixed-effects models that can be used in PCPS analysis. The argument \emph{formula} is specified, where the left hand side gives the PCPS used, 
#' right hand side gives the variables. The PCPS are internally named sequentially \emph{pcps.1}, \emph{pcps.2}, \emph{pcps.3} and so 
#' on. Thus, formula is an expression of the form \emph{pcps.1 ~ model} (see Examples). The type of environmental variables are 
#' extracted directly from \emph{envir} argument, thus variables of class \code{\link{factor}} can be already
#' specified in \emph{envir} \code{\link{data.frame}} or through \emph{formula} argument. The significance is assess using F value 
#' and the difference between function is due to the argument \emph{type} in \code{\link{anova.lme}}. The function 
#' \emph{FUN.LME.marginal} use as default \emph{type = marginal} to assess the marginal significance of all terms
#' whereas the function \emph{FUN.LME.sequential} use as default \emph{type = sequential} to assess the sequential effects of 
#' the terms. Those funcitons return all F values calculed by \code{\link{anova.lme}}, including the intercept if it is in the model. 
#' Additional arguments as \emph{correlation} and \emph{random} can be passed by \emph{...} argument. See more in \code{\link{lme}} and \code{\link{anova.lme}}.
#' 
#' \strong{Additional function}
#' 
#' The functions \emph{matrix.p.sig} and \emph{pcps.sig} only perform permutation following null models and apply the functions in all 
#' permuted matrices. Additional functions can be easy specify and passed via \emph{FUN} argument. A skeleton of this function is slowed 
#' below. In this function the argument \emph{x} will be always the matrix P or one matrix with PCPS choose, when additional arguments
#' as \emph{envir} will specify statistical analysis performed in matrix P ou PCPS. This function must return the observed statistical in addition the
#' \emph{return.model} argument must not be specified because it specify the return options used for observed and null statistics.
#' 
#' \preformatted{FUN.X <- function(x, envir, ..., return.model = FALSE){
#'   mod.obs <- # Function to perform analysis using x, envir and any additional argument
#'   statistic.obs <- # Extract only the numeric values of observed statistical
#'   # Next lines are mandatory
#'    if(return.model){
#'       res <- list()
#'       res$mod.obs <- mod.obs
#'       res$statistic.obs <- statistic.obs
#'     } else{
#'       res <- statistic.obs
#'     }
#'   return(res) 
#' }}
#' 
#' @encoding UTF-8
#' @include pcps.R
#' @import SYNCSA
#' @importFrom vegan procrustes rda adonis adonis2 mantel vegdist
#' @importFrom parallel makeCluster parLapply stopCluster
#' @importFrom stats glm summary.lm cor anova
#' @importFrom nlme lme gls
#' @aliases pcps.sig matrix.p.sig print.pcpssig FUN.ADONIS FUN.ADONIS2.global FUN.ADONIS2.margin FUN.GLM FUN.MANTEL FUN.RDA FUN.GLS.marginal FUN.GLS.sequential FUN.LME.marginal FUN.LME.sequential
#' @param comm Community data, with species as columns and sampling units as rows. This matrix 
#' can contain either presence/absence or abundance data.
#' Alternatively comm can be an object of class metacommunity.data, an alternative
#' way to set all data.frames/matrices. When you use the class metacommunity.data the arguments
#' phylodist and envir must not be specified. See details.
#' @param phylodist Matrix containing phylogenetic distances between species.
#' @param envir A matrix or data.frame with environmental variables for each community, with variables as columns and 
#' sampling units as rows. See Details and Examples.
#' @param checkdata Logical argument (TRUE or FALSE) to check if species
#' sequence in the community data follows the same order as the one in the phylodist matrix 
#' and if sampling units in the community data follows the same order as the one in the 
#' environmental data (Default checkdata = TRUE).
#' @param method Dissimilarity index, as accepted by \code{\link{vegdist}} (Default dist = "bray").
#' @param squareroot Logical argument (TRUE or FALSE) to specify if use square root of 
#' dissimilarity index (Default squareroot = TRUE).
#' @param FUN An object of class function to perform the analysis. See Details and Examples.
#' @param choices Numeric vector to choose the PCPS used in analysis. See Details and Examples.
#' @param runs Number of permutations for assessing significance.
#' @param parallel Number of parallel processes or a predefined socket cluster done with parallel package. Tip: use detectCores() (Default parallel = NULL).
#' @param ... Other arguments passed to FUN function. See Details and Examples.
#' @param newname New name to be replaced in object returned by \code{\link{matrix.p.null}} (Default newname = "pcps").
#' @param x An object of class pcpssig or other object to apply the function passed by FUN. See Details.
#' @param method.p Resemblance index between communities based on P matrix, as accepted by \code{\link{vegdist}}. 
#' Used in FUN.MANTEL, FUN.ADONIS, FUN.ADONIS2.global and FUN.ADONIS2.margin analysis. See Details and Examples.
#' @param sqrt.p Logical argument (TRUE or FALSE) to specify if use square root of dissimilarity P matrix. Used in
#' FUN.MANTEL, FUN.ADONIS, FUN.ADONIS2.global and FUN.ADONIS2.margin analysis. See Details and Examples (Default sqrt.p = TRUE). 
#' @param method.envir Resemblance index between communities based on environmental variables, as accepted by \code{\link{vegdist}}.
#' Used in FUN.MANTEL analysis. See Details and Examples.
#' @param formula An object of class \code{\link{formula}}. Used in FUN.GLM, FUN.ADONIS, 
#' FUN.ADONIS2.global, FUN.ADONIS2.margin, FUN.GLS.marginal, FUN.GLS.sequential, FUN.LME.marginal and FUN.LME.sequential analysis. See Details and Examples.
#' @param return.model Must not be specified. See Details.
#' @return \item{call}{The arguments used.}
#' \item{P.obs}{Phylogeny-weighted species composition matrix.}
#' \item{PCPS.obs}{The principal coordinates of phylogenetic structure (PCPS)}
#' \item{model}{The observed model returned by FUN, an object of class glm, gls, lme, rda, adonis, adonis2 or mantel to predefined function.}
#' \item{fun}{The funtion used.}
#' \item{statistic.null.site}{A matrix with null statistic for site shuffle null model.}
#' \item{statistic.null.taxa}{A matrix with null statistic for taxa shuffle null model.}
#' \item{obs.statistic}{Observed statistic, F value or r value to predefined function.}
#' \item{p.site.shuffle}{The p value for the site shuffle null model.}
#' \item{p.taxa.shuffle}{The p value for the taxa shuffle null model.}
#' 
#' @note \strong{IMPORTANT}: The sequence of species in the community data matrix
#' MUST be the same as that in the phylogenetic distance matrix and, similarly, 
#' the sequence of communities in the community data matrix MUST be the same as that in 
#' the environmental data. See details and \code{\link{organize.pcps}}.
#' 
#' @author Vanderlei Julio Debastiani <vanderleidebastiani@@yahoo.com.br>
#' @seealso \code{\link{matrix.p}}, \code{\link{pcps}}, \code{\link{procrustes}}, 
#' \code{\link{glm}}, \code{\link{rda}}, \code{\link{adonis}}, \code{\link{adonis2}}, 
#' \code{\link{mantel}}
#' @references Duarte, L.S. (2011). Phylogenetic habitat filtering influences forest 
#' nucleation in grasslands. Oikos, 120, 208:215.
#' 
#' Duarte, L.S. (2016). Dissecting phylogenetic fuzzy weighting: theory and application 
#' in metacommunity phylogenetics. Methods in Ecology and Evolution, 7(8), 937:946.
#' @keywords PCPS
#' @examples
#' 
#' \dontrun{
#' data(flona)
#' 
#' # MANTEL
#' res <- matrix.p.sig(flona$community,flona$phylo, FUN = FUN.MANTEL, method.p = "bray", 
#'              method.envir = "euclidean", envir = flona$environment[, 2, drop = FALSE], runs = 99)
#' res
#' 
#' # ADONIS
#' res <- matrix.p.sig(flona$community,flona$phylo, FUN = FUN.ADONIS, method.p = "bray", 
#'              formula = p.dist~temp, envir = flona$environment[, 2, drop = FALSE], runs = 99)
#' res
#' 
#' # ADONIS2
#' res <- matrix.p.sig(flona$community,flona$phylo, FUN = FUN.ADONIS2.global, 
#'              envir = flona$environment, formula = p.dist~temp+alt, 
#'              method.p = "bray", runs = 99)
#' res            
#' res <- matrix.p.sig(flona$community,flona$phylo, FUN = FUN.ADONIS2.margin, 
#'               envir = flona$environment, formula = p.dist~temp+alt, 
#'               method.p = "bray", runs = 99)
#' res            
#' 
#' # GLM
#' res <- pcps.sig(flona$community, flona$phylo, FUN = FUN.GLM, method = "bray", 
#'          formula = pcps.1~temp, envir = flona$environment, choices = 1, runs = 99)
#' res
#' summary.lm(res$model)
#' 
#' # RDA
#' res <- pcps.sig(flona$community, flona$phylo, FUN = FUN.RDA, envir = flona$environment, 
#'          choices = 1:2, runs = 99)
#' res
#'
#' # GLS
#' res <- pcps.sig(flona$community, flona$phylo, FUN = FUN.GLS.marginal, 
#'          formula = pcps.1~temp, envir = flona$environment, choices = 1, runs = 99)
#' res
#' anova(res$model, type = "marginal")
#' 
#' res <- pcps.sig(flona$community, flona$phylo, FUN = FUN.GLS.marginal, 
#'          formula = pcps.1~temp, envir = flona$environment, 
#'          correlation = nlme::corCAR1(form = ~1:39), choices = 1, runs = 99)
#' res
#' anova(res$model, type = "marginal")
#' 
#' # LME
#' res <- pcps.sig(flona$community, flona$phylo, FUN = FUN.LME.marginal, formula = pcps.1~alt, 
#'          envir = flona$environment, random = ~1|temp, choices = 1, runs = 99)
#' res
#' anova(res$model, type = "marginal")
#' 
#' res <- pcps.sig(flona$community, flona$phylo, FUN = FUN.LME.sequential, formula = pcps.1~alt,
#'          envir = flona$environment, random = ~1|temp, choices = 1, runs = 99)
#' res
#' anova(res$model, type = "sequential")
#' }
#' 
#' @export
pcps.sig <- function (comm, phylodist, envir, checkdata = TRUE, method = "bray", squareroot = TRUE, FUN, choices, runs = 999, parallel = NULL, newname = "pcps", ...) 
{
  res <- list(call = match.call())
  if (inherits(comm, "metacommunity.data")) {
    if (!missing(phylodist)) {
      stop("\n When you use an object of class metacommunity.data the argument phylodist must not be specified. \n")
    }
    phylodist <- comm$phylodist
    envir <- comm$environmental
    comm <- comm$community
  }
  list.warning <- list()
  if(checkdata){
    organize.temp <- organize.pcps(comm, phylodist = phylodist, envir = envir, check.comm = TRUE)
    if(!is.null(organize.temp$stop)){
      organize.temp$call <- match.call()
      return(organize.temp)
    }
    list.warning <- organize.temp$list.warning
    comm <- organize.temp$community
    phylodist <- organize.temp$phylodist
    envir <- organize.temp$environmental
    # str(envir)
    # envir nao conferido
    
  }
  if(length(list.warning)>0){
    res$list.warning <- list.warning
  }
  res.pcps.null <- matrix.p.null(comm, phylodist, runs = runs, calcpcps = TRUE, adjpcps = TRUE, choices = choices, method = method, squareroot = squareroot)
  res.pcps.null <- mutate.names.matrix.p.null(res.pcps.null, "pcps", newname)
  res$PCPS.obs <- res.pcps.null$pcps.obs
  statistic.obs <- sapply(list(res.pcps.null$pcps.obs[, choices, drop = FALSE]), FUN = FUN, simplify = FALSE, envir = envir, return.model = TRUE, ...)
  res$model <- statistic.obs[[1]]$mod.obs
  res$fun <- FUN
  res$obs.statistic <- statistic.obs[[1]]$statistic.obs
  newClusters <- FALSE
  if (is.numeric(parallel)) {
    parallel <- parallel::makeCluster(parallel, type = "PSOCK")
    newClusters <- TRUE
  }
  if (!inherits(parallel, "cluster")) {
    statistic.null.site <- sapply(sapply(res.pcps.null$pcps.null.site, function(x, choices) x[,choices,drop = FALSE], simplify = FALSE, choices = choices), FUN = FUN, simplify = FALSE, envir = envir, ...)
    statistic.null.taxa <- sapply(res.pcps.null$pcps.null.taxa.adj, FUN = FUN, simplify = FALSE, envir = envir, ...)
  }
  else {
    statistic.null.site <- parallel::parLapply(parallel, sapply(res.pcps.null$pcps.null.site, function(x, choices) x[,choices,drop = FALSE], simplify = FALSE, choices = choices), fun = FUN, envir = envir, ...)
    statistic.null.taxa <- parallel::parLapply(parallel, res.pcps.null$pcps.null.taxa.adj, fun = FUN, envir = envir, ...)
  }
  if (newClusters) {
    parallel::stopCluster(parallel)
  }
  res$statistic.null.site <- do.call("rbind", statistic.null.site)
  res$statistic.null.taxa <- do.call("rbind", statistic.null.taxa)
  res$p.site.shuffle <- as.vector(rbind((apply(sweep(res$statistic.null.site, 2, res$obs.statistic, ">="), 2, sum)+1)/(runs + 1)))
  res$p.taxa.shuffle <- as.vector(rbind((apply(sweep(res$statistic.null.taxa, 2, res$obs.statistic, ">="), 2, sum)+1)/(runs + 1)))
  class(res) <- "pcpssig"
  return(res)
}