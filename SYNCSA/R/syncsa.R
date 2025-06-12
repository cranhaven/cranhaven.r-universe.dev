#' @title SYNCSA
#'
#' @description This function integrates several steps for the analysis of phylogenetic
#' assembly patterns and their links to traits and ecological processes in a
#' metacommunity (Pillar et al. 2009, Pillar & Duarte 2010, Debastiani & Pillar 2012). The
#' function implement methods that have been available in the SYNCSA
#' application written in C++ (by Valerio Pillar, available at
#' http://ecoqua.ecologia.ufrgs.br/SYNCSA.html). See details.
#'
#'
#' @details Package \strong{SYNCSA} requires that the species and community sequence in
#' the data.frame or matrix must be the same for all dataframe/matrices.
#' The function \code{\link{organize.syncsa}} organizes the data for the functions
#' of the package, placing the matrices of community, traits, phylogenetic distance,
#' environmental varibles and strata vector in the same order. The function
#' use of function organize.syncsa is not requered for run the functions, but
#' is recommended. It requires data organized into the following matrices: (1) the
#' presences or abundances of species in a set of communities (\strong{W}); (2) the
#' phylogenetic pairwise dissimilarities of these species (\strong{DF}); (3) a set of
#' functional traits describing the species (\strong{B}), which
#' may be a mixture of binary and quantitative traits (continual and ordinal),
#' but not nominal ones (these should be expanded into binary traits); and (4)
#' the ecological gradient of interest, which may be one or more factors to
#' which the communities respond or ecosystem effects of the communities
#' (\strong{E}). In this way the arguments comm, traits, phylodist, envir,
#' as well as the arguments put.together and strata, can be specified them as normal
#' arguments or by passing them with the object returned by the function
#' \code{\link{organize.syncsa}} using, in this case only the argument comm. Using the
#' object returned by organize.syncsa, the comm argument is used as an alternative way
#' of entering to set all data.frames/matrices, and therefore the other arguments
#' (traits, phylodist, envir, put.together and strata) must be null.
#'
#'
#' \strong{Correlations}
#'
#' The function computes several correlations (Mantel or
#' Procrustes) that express trait-convergence assembly patterns (TCAP),
#' trait-divergence assembly patterns (TDAP), and phylogenetic signal in
#' functional traits at the species pool level and at the metacomunity level.
#' This function also generates P-values by permutation testing based on null
#' models (Pillar et al. 2009, Pillar & Duarte 2010).
#'
#' \strong{ro(TE)}
#'
#' This correlation refers to trait-convergence assembly patterns related to
#' the ecological gradient (TCAP, Pillar et al. 2009). For evaluating TCAP, by
#' matrix multiplication we define \strong{T = WB}, which with previous
#' standardization of \strong{W} to unit column totals will contain the trait
#' averages in each community. The elements in \strong{T} are community
#' weighted mean values or community functional parameters (Violle et al.
#' 2007). Standardization of the traits (rows) in \strong{T} is needed if the
#' trait set contains traits measured with different scales. By using matrix
#' correlation, we evaluate how the trait patterns in \strong{T} are associated
#' to ecological gradients in \strong{E}. For relating \strong{T} to
#' \strong{E}, using Mantel correlation we define a distance matrix of the
#' communities (\strong{DT}) using \strong{T}, and another distance matrix of
#' the community sites (\strong{DE}) using \strong{E}. The matrix correlation
#' ro(\strong{TE}) = ro(\strong{DT};\strong{DE}) measures the level of
#' congruence between TCAP and \strong{E}. A strong correlation ro(\strong{TE})
#' indicates the factors directly or indirectly represented in \strong{E} are
#' involved in ecological filtering of species that, at least for the traits
#' considered in the analysis, consistently produce trait-convergence assembly
#' patterns along the gradient comprising the metacommunity.
#'
#' \strong{ro(XE) and ro(XE.T)}
#'
#' These matrix correlations refer to trait-divergence assembly patterns
#' related to the ecological gradient (TDAP, Pillar et al. 2009). For the
#' identification of TDAP, in a first step the species pairwise similarities
#' (in the range 0 to 1) in matrix \strong{SB} based on traits in \strong{B}
#' are used to define matrix \strong{U} with degrees of belonging of species to
#' fuzzy sets. By matrix multiplication \strong{X = WU} will contain the
#' species composition of the communities after fuzzy-weighting by their trait
#' similarities (each row in \strong{X} will refer to a species). Matrix
#' \strong{X} expresses both TCAP and TDAP (Pillar et al. 2009). By using
#' matrix correlation, we evaluate how the trait patterns in \strong{X} (TCAP
#' and TDAP) are associated to ecological gradients in \strong{E}. For relating
#' \strong{X} to \strong{E}, we define a distance matrix of the communities
#' (\strong{DX}) using \strong{X}, and another distance matrix of the community
#' sites (\strong{DE}) using \strong{E}. The matrix correlation ro(\strong{XE})
#' = ro(\strong{DX};\strong{DE}) between \strong{X} and \strong{E} is defined.
#' We then remove the trait-convergence component ro(\strong{TE}) from
#' ro(\strong{XE}) by computing the partial matrix correlation
#' ro(\strong{XE.T}), which measures the level of congruence between TDAP and
#' \strong{E}. Trait-divergence assembly patterns (TDAP, Pillar et al. 2009)
#' may result from community assembly processes related to biotic interactions
#' (Stubbs & Wilson 2004; Wilson 2007).
#'
#' \strong{ro(PE)}
#'
#' This matrix correlation refers to the phylogenetic structure related to the
#' ecological gradient comprising the metacommunity. The phylogenetic pairwise
#' dissimilarities in \strong{DF} are transformed into similarities and used to
#' define degrees of belonging qij to fuzzy sets. This is analogous to the
#' definition of functional fuzzy sets (Pillar & Orloci 1991; Pillar et al.
#' 2009). Based on the phylogenetic similarities, every species i among s
#' species in the metacommunity specifies a fuzzy set to which every species j
#' (j = 1 to s species, including species i) belongs with a certain degree of
#' belonging in the interval [0, 1]. In our definition, each row in matrix
#' \strong{Q} with the degrees of belonging must add to unit, i.e., the degrees
#' of belonging of a given species across the fuzzy sets are standardized to
#' unit total. By matrix multiplication \strong{P = WQ} will contain the
#' composition of the communities after fuzzy-weighting of species presences or
#' abundances by the species` phylogenetic similarities. Each column in matrix
#' \strong{P} holds the phylogenetic structure of a community. The
#' standardization of \strong{Q} is essential for the community totals in each
#' column in \strong{W} remaining the same in \strong{P}. Further, matrix
#' \strong{W} is adjusted to unit column totals prior to multiplication, so
#' that the total richness or abundance within each community in \strong{W}
#' will be standardized. Matrix correlation ro(\strong{PE}) =
#' ro(\strong{DP};\strong{DE}) measures the strength of the association between
#' community distances based on their phylogenetic structure in \strong{DP} and
#' distances based on their ecological conditions (\strong{DE}). Further,
#' \strong{P} can be explored for phylogenetic patterns at the metacommunity
#' level by using, e.g., ordination techniques.
#'
#' \strong{ro(PT) and ro(PX.T)}
#'
#' These matrix correlations measure phylogenetic signal at the metacommunity
#' level related to TCAP and to TDAP. We define phylogenetic signal at the
#' metacommunity level related to TCAP (PSMT) as the correlation between the
#' phylogenetic structure described in matrix \strong{P} and the
#' trait-convergence structure described in matrix \strong{T}. For this, a
#' proper distance matrix (e.g. Euclidean distances) of communities
#' (\strong{DP}) is computed using \strong{P} and another distance matrix of
#' the same communities (\strong{DT}) is computed using \strong{T}. Then matrix
#' correlation ro(\strong{PT}) = ro(\strong{DP};\strong{DT}) will measure the
#' level of congruence between variation in \strong{P} and \strong{T}, which is
#' a measure of PSMT. A strong phylogenetic signal at the metacommunity level
#' is expected when communities that are more similar in terms of phylogenetic
#' structure are also similar regarding their average trait values. We also
#' define phylogenetic signal at the metacommunity level related to TDAP
#' (PSMX.T) as the partial matrix correlation ro(\strong{PX.T}) =
#' ro(\strong{DP};\strong{DX.DT}) between community distances DP computed on
#' phylogenetic structure and community distances \strong{DX} computed on
#' species composition after fuzzy-weighting by the species, or trait
#' similarities, removing the effect of TCAP (\strong{DT}). This is analogous
#' to TDAP.
#'
#' \strong{ro(BF)}
#'
#' This matrix correlation measures phylogenetic signal at the species pool
#' level (PSS, Pillar & Duarte 2010). We define PSS as the matrix correlation
#' ro(\strong{FB}) = ro(\strong{DF};\strong{DB}) between species phylogenetic
#' dissimilarities (already defined as matrix \strong{DF}) and species trait
#' dissimilarities (derived from already defined matrix \strong{SB}) computed
#' on any number of traits from matrix \strong{B}. The species pool refers to
#' the species present in the metacommunity.
#'
#' \strong{Additional matrix correlations}
#'
#' The matrix correlations ro(\strong{TE.P}) and ro(\strong{XE.P}) are also
#' computed, which may be useful for evaluating causal models in path analysis.
#'
#' \strong{Mantel correlations}
#'
#' The Mantel and Partial Mantel statistics are calculated simply as the correlation
#' entries the dissimilarity matrices, using standard Mantel test (see
#' \code{\link{mantel}} and \code{\link{cor.mantel}}). Partial Mantel
#' statistic use paired correlation between the three matrices and obtains the partial
#' correlation using the formula of first-order partial correlation coefficient. The
#' significances are obtained using a different procedure than standard Mantel test,
#' see section Testing against null models below.
#'
#' \strong{Procrustes correlations}
#'
#' The Procrustes correlation uses symmetric Procrustes as a measure of concordance
#' between the data matrices (see \code{\link{procrustes}} and
#' \code{\link{cor.procrustes}}). Procrustes procedure use rotation, translation,
#' and rescaling for minimizing sum of squared differences between two data sets.
#' The correlation of Procrustes is calculated as the statistic derived from the
#' symmetric Procrustes sum of squares, representing the optimal fit between the two
#' data matrices. Partial Procrustes correlation is obtained by Procrustes correlation
#' between residuals matrices. Firstly one Principal Components Analysis (PCA,
#' see \code{\link{prcomp}}) is performed in the matrix Z for dimensionality reduction.
#' The max number of axis kept in the analysis is the number of sampling units divided
#' by 2, this axes of PCA represent the total variation in the Z matrix. After the
#' kept axes are used as predictor in one linear model for each variable of the
#' matrices X and Y. For this a linear model is build using as response one variable
#' of X (same via for Y matrix) and as predictor all remaining axes of PCA, after model
#' fitted and the residual are extracted with the aim of form the residual matrix. The linear
#' model is repeated in the other variables, only with the changed the response variable.
#' The same procedure is performed in the matrix
#' Y. Both residual matrices are submitted to Procrustes analysis and the statistic is
#' returned as a partial correlation, the Partial Procrustes statistic. The significances
#' are obtained using the same procedure than Mantel test, see section Testing against
#' null models below.
#'
#' \strong{Testing against null models}
#'
#' All the matrix correlations are tested against null models. The null model
#' is defined accoding to the correlation being tested. Usually in the SYNCSA package
#' the null models are based in permutation of species rather than permutation
#' of sample units. For ro(\strong{TE}),each permutation generates a random
#' matrix \strong{T} calculated after the
#' permutation among the species vectors in matrix \strong{B}. For
#' ro(\strong{XE}) and ro(\strong{XE.T}), each permutation generates a random
#' matrix \strong{X} after the permutation among species fuzzy sets (rows) in
#' matrix \strong{U}. For ro(\strong{PE}), ro(\strong{PT}), and
#' ro(\strong{PX.T}), each permutation generates a random matrix \strong{P}
#' after the permutation among species fuzzy sets (rows) in matrix \strong{Q}.
#' For ro(\strong{BF}), a conventional Mantel test is performed with
#' dissimilarity matrices \strong{DF} and \strong{DB}. Analogous null models
#' are used for testing the additional matrix correlations; that is, the same
#' null model for ro(\strong{TE}) is used for ro(\strong{TE.P}), the same model
#' for ro(\strong{XE}) is used for ro(\strong{XE.P}). The permutation can be restrict
#' within species groups specifying the strata argument.
#'
#' \strong{Traits types}
#'
#' Traits data can be numeric, factor or ordered factor. For this be considered in the
#' analyses traits data must be of data.frame class and containing each variable type
#' determined. Gower index is used to calculate the similarity between species, using
#' the function gowdis of package FD. For additional details and requirements of function
#' please see \code{\link{gowdis}}.
#'
#' \strong{Missing data}
#'
#' The functions ignore missing data when specified. In the case of direct
#' multiplication of matrices the missing data are replaced by 0, ignoring the cell with missing value. For the
#' matrix \strong{T = WB} an adjustment is done by divide each cell of the product
#' matrix (\strong{T}) by the sum of species proportion with trait data in \strong{B}. Result
#' matrices are shown without missing values. Where the matrices are calculated
#' using a dissimilarity index (matrix \strong{U} and correlations between
#' matrices) the missing data are ignored as in \code{\link{vegdist}} function.
#' In some cases the dissimilarity matrices obtained by the function
#' \code{\link{vegdist}} still contain some missing values. In these cases the
#' rest of the procedure will be affected. In these cases you can find
#' solutions in impute the missing values.
#'
#'
#' \strong{Error messenger and options}
#'
#' The data pass by several ckeck points that can produce error messenger. The
#' matrices or data frames must be contain only numeric, binary or ordinal
#' variables, in the way that nominal variable should be expanded into binary
#' (see \code{\link{var.dummy}}). For enhance the code speed some functions use
#' by default matrix algebra, this option can produce error under certain circumstances. This
#' global option can be changed using options("SYNCSA.speed" = FALSE). If SYNCSA.speed = TRUE
#' for use matrix algebra and if SYNCSA.speed = FALSE use not another function of same procedure.
#'
#' @encoding UTF-8
#' @aliases SYNCSA syncsa print.syncsa
#' @importFrom stats cor as.dist
#' @importFrom vegan wcmdscale protest vegdist
#' @importFrom permute how
#' @importFrom parallel makeCluster stopCluster
#' @param comm Community data, with species as columns and sampling units as
#' rows. This matrix can contain either presence/absence or abundance data.
#' Alternatively comm can be an object of class metacommunity.data, an alternative
#' way to set all data.frames/matrices. When you use the class metacommunity.data the arguments
#' traits, phylodist, envir and put.together must be null. See details.
#' @param traits Data frame or matrix data of species described by traits, with traits as
#' columns and species as rows (Default traits = NULL).
#' @param phylodist Matrix containing phylogenetic distance between species.
#' Must be a complete matrix, not a half diagonal matrix (Default phylodist = NULL).
#' @param envir Environmental variables for each community, with variables as
#' columns and sampling units as rows (Default envir = NULL).
#' @param checkdata Logical argument (TRUE or FALSE) to check if species
#' sequence in the community data follows the same order as the one in the
#' trait and in the phylodist matrices and if sampling units in the community data follows
#' the same order as the one in the environmental data (Default checkdata = TRUE).
#' @param ro.method Method to obtain the correlation, "mantel" or "procrustes"
#' (Default ro.method = "mantel").
#' @param method Mantel correlation method, as accepted by cor: "pearson",
#' "spearman" or "kendall" (Default method = "pearson").
#' @param dist Dissimilarity index used for Mantel correlation, as accepted by
#' vegdist: "manhattan", "euclidean", "canberra", "bray", "kulczynski",
#' "jaccard", "gower", "altGower", "morisita", "horn", "mountford", "raup" ,
#' "binomial" or "chao" (Default dist = "euclidean").
#' @param scale Logical argument (TRUE or FALSE) to specify if the traits are
#' measured on different scales (Default Scale = TRUE). When scale = TRUE traits
#' are measured on different scales the the matrix T is subjected to
#' standardization within each trait. When scale = FALSE traits are measured on
#' the same scale and the matrix T is not subjected to standardization.
#' Furthermore, if scale = TRUE the matrix of traits is subjected to
#' standardization within each trait, and Gower Index is used to calculate the
#' degree of belonging to the species, and if scale = FALSE the matrix of
#' traits is not subjected to standardization, and Euclidean distance is
#' calculated to determine the degree of belonging to the species.
#' @param scale.envir Logical argument (TRUE or FALSE) to specify if the
#' environmental variables are measured on different scales. If the
#' enviromental variables are measured on different scales, the scale.envir
#' = TRUE the matrix with enviromental variables is subjected to centralization
#' and standardization within each variable. (Default scale.envir = TRUE).
#' @param ranks Logical argument (TRUE or FALSE) to specify if ordinal variables are
#' convert to ranks (Default ranks = TRUE).
#' @param ord Method to be used for ordinal variables, see \code{\link{gowdis}}.
#' @param put.together List to specify group of traits. Each group specify receive the
#' same weight that one trait outside any group, in the way each group is considered
#' as unique trait (Default put.together = NULL). This argument must be a list, see
#' examples.
#' @param na.rm Logical argument (TRUE or FALSE) to specify if pairwise
#' distances should be deleted in cases of missing observations (Default na.rm
#' = FALSE).
#' @param strata Strata named vector to specify restricting permutations within species
#' groups (Default strata = NULL).
#' @param permutations Number of permutations in assessing significance.
#' @param parallel Number of parallel processes. Tip: use parallel::detectCores() (Default parallel = NULL).
#' @param notification Logical argument (TRUE or FALSE) to specify if
#' notification of missing observations should to be shown (Default
#' notification = TRUE).
#' @param x An object of class syncsa.
#' @param ... Other parameters for the respective functions.
#' @return \item{call}{The arguments used.} \item{notes}{Some notes about the statistics.}
#' \item{statistics}{Correlations roTE, roXE, roPE, roPT, roPX.T, roXE.T, roTE.P, roXE.P
#' and roBF, and their significance levels based on permutations.} \item{matrices}{The matrices
#' produced for the functions, see details.} \item{FunRao}{Rao
#' quadratic entropy within each community, considering trait distance.} \item{weights}{Weight for each trait.}
#'
#' @note The function calculates the correlations despite the lack of one of
#' the matrices, provided that community data had been entered. Correlations
#' including unspecified matrices will appear with NA.
#'
#' \strong{IMPORTANT}: The sequence of species in the community data matrix
#' MUST be the same as that in the phylogenetic distance matrix and in traits
#' matrix. Similarly, the sequence of communities in the community data matrix
#' MUST be the same as that in the environmental data. See details and
#' \code{\link{organize.syncsa}}.
#'
#' @author Vanderlei Julio Debastiani <vanderleidebastiani@@yahoo.com.br>
#' @seealso \code{\link{organize.syncsa}}, \code{\link{matrix.t}},
#' \code{\link{matrix.x}}, \code{\link{matrix.p}}, \code{\link{optimal}},
#' \code{\link{rao.diversity}}, \code{\link{cor.matrix}}, \code{\link{var.type}},
#' \code{\link{var.dummy}}
#'
#' @references
#'
#' Debastiani, V.J & Pillar, V.D., (2012). SYNCSA-R tool for analysis of metacommunities
#' based on functional traits and phylogeny of the community components. Bioinformatics,
#' 28(15), 2067–2068.
#'
#' Pillar, V.D.; Duarte, L.d.S., (2010). A framework for metacommunity analysis
#' of phylogenetic structure. Ecology Letters, 13, 587:596.
#'
#' Pillar, V.D., Duarte, L.d.S., Sosinski, E.E. & Joner, F. (2009).
#' Discriminating trait-convergence and trait-divergence assembly patterns in
#' ecological community gradients. Journal of Vegetation Science, 20, 334:348.
#'
#' Pillar, V.D. & Orloci, L., (1991). Fuzzy components in community level
#' comparisons. In: Computer Assisted Vegetation Analysis (eds Feoli, E. &
#' Orloci, L.). Kluwer, Dordrecht, 87:93.
#'
#' Stubbs, W.J. & Wilson, J.B., (2004). Evidence for limiting similarity in a
#' sand dune community. Journal of Ecology, 92, 557:567.
#'
#' Violle, C., Navas, M.L., Vile, D., Kazakou, E., Fortunel, C., Hummel, I. &
#' Garnier, E., (2007). Let the concept of trait be functional! Oikos, 116,
#' 882:892.
#'
#' Wilson, J.B., (2007). Trait-divergence assembly rules have been demonstrated:
#' limiting similarity lives! A reply to Grime. Journal of Vegetation Science,
#' 18, 451:452.
#' @keywords SYNCSA
#' @examples
#' data(ADRS)
#' syncsa(ADRS$community, ADRS$traits, ADRS$phylo, ADRS$envir, permutations = 99)
#' data(flona)
#' put.together<-list(c("fol","sem"), c("tam", "red"))
#' put.together
#' res<-syncsa(flona$community, flona$traits, envir = flona$environment,
#'    put.together = put.together, permutations = 99)
#' res$weights
#' @export
syncsa <- function (comm, traits = NULL, phylodist = NULL, envir = NULL, checkdata = TRUE, ro.method = "mantel",
                    method = "pearson", dist = "euclidean", scale = TRUE, scale.envir = TRUE, ranks = TRUE, ord,
                    put.together = NULL, na.rm = FALSE, strata = NULL, permutations = 999,
                    parallel = NULL, notification = TRUE)
{
  res <- list(call = match.call())
  roTE <- NA
  roXE <- NA
  roPE <- NA
  roPT <- NA
  roPX.T <- NA
  roXE.T <- NA
  roTE.P <- NA
  roXE.P <- NA
  roBF <- NA
  roRE <- NA
  note.roTE <- paste("Trait-convergence assembly patterns (TCAP): roTE")
  note.roXE <- paste("Both trait-convergence assembly patterns and trait-divergence assembly patterns: roXE")
  note.roXE.T <- paste("Trait-divergence assembly patterns (TDAP): roXE.T")
  note.roBF <- paste("Phylogenetic signal at species level: roBF")
  note.roPE <- paste("Correlation of phylogenetically structured assembly patterns to ecological variables: roPE")
  note.roPT <- paste("Correlation of phylogenetically structured assembly patterns to trait-convergence assembly patterns: roPT")
  note.roPX.T <- paste("Correlation of phylogenetically structured assembly patterns to trait-divergence assembly patterns: roPX.T")
  note.roTE.P <- paste("Removing phylogeny from trait-convergence assembly patterns: roTE.P")
  note.roXE.P <- paste("Removing phylogeny from both trait-convergence assembly patterns and trait-divergence assembly patterns: roXE.P")
  note.roRE <- paste("Alpha divergence (correlation between environmental variables and Rao entropy): roRE")
  note <- rbind(note.roTE, note.roXE, note.roXE.T, note.roBF, note.roPE, note.roPT, note.roPX.T, note.roTE.P, note.roXE.P, note.roRE)
  colnames(note) <- "Correlation meanings"
  res.matrices <- list()
  res$notes <- note
  N <- permutations
  roMETHOD <- c("mantel", "procrustes")
  romethod <- pmatch(ro.method, roMETHOD)
  if (length(romethod) > 1) {
    stop("\n Only one argument is accepted in ro.method \n")
  }
  if (is.na(romethod)) {
    stop("\n Invalid ro.method \n")
  }
  if (inherits(comm, "metacommunity.data")) {
    if (!is.null(traits) | !is.null(phylodist) | !is.null(envir) | !is.null(put.together) | !is.null(strata)) {
      stop("\n When you use an object of class metacommunity.data the arguments traits, phylodist, envir, put.together and strata must be null. \n")
    }
    traits <- comm$traits
    phylodist <- comm$phylodist
    envir <- comm$environmental
    put.together <- comm$put.together
    strata <- comm$strata
    comm <- comm$community
  }
  list.warning <- list()
  if(checkdata){
    organize.temp <- organize.syncsa(comm, traits = traits, phylodist = phylodist, envir = envir,
                                     strata = strata, check.comm = TRUE)
    if(!is.null(organize.temp$stop)){
      organize.temp$call <- match.call()
      return(organize.temp)
    }
    list.warning <- organize.temp$list.warning
    comm <- organize.temp$community
    traits <- organize.temp$traits
    phylodist <- organize.temp$phylodist
    envir <- organize.temp$environmental
    strata <- organize.temp$strata
  }
  if(notification & !checkdata){
    if (!missing(comm)) {
      if(any(is.na(comm))){
        warning("Warning: NA in community data", call. = FALSE)
      }
    }
    if (!is.null(traits)) {
      if(any(is.na(traits))){
        warning("Warning: NA in traits matrix", call. = FALSE)
      }
    }
    if (!is.null(phylodist)) {
      if(any(is.na(phylodist))){
        warning("Warning: NA in phylodist data", call. = FALSE)
      }
    }
    if (!is.null(envir)) {
      if(any(is.na(envir))){
        warning("Warning: NA in environmental data", call. = FALSE)
      }
    }
  }
  if(!is.null(strata)){
    if(length(strata) != ncol(comm)){
      stop("\n strata must be the same length of number of species \n")
    }
  }
  seqpermutation <- permut.vector(ncol(comm), strata = strata, nset = permutations)
  if(!checkdata){
    commvartype <- var.type(comm)
    if(any(commvartype == "n")){
      stop("\n comm must contain only numeric, binary or ordinal variables \n")
    }
    if (!is.null(traits)) {
      traitsvartype <- var.type(traits)
      if(any(traitsvartype == "n")){
        stop("\n trait must contain only numeric, binary or ordinal variables \n")
      }
    }
    if (!is.null(phylodist)) {
      phylodistvartype <- var.type(phylodist)
      if(any(phylodistvartype == "n")){
        stop("\n phylodist must contain only numeric, binary or ordinal variables \n")
      }
    }
    if (!is.null(envir)) {
      envirvartype <- var.type(envir)
      if(any(envirvartype == "n")){
        stop("\n envir must contain only numeric, binary or ordinal variables \n")
      }
    }
  }
  if (!is.null(envir)) {
    if(romethod == 1 & any(is.na(suppressWarnings(vegan::vegdist(envir, method = dist, na.rm = TRUE))))){
      stop("\n envir with too much NA \n")
    }
    if(romethod == 2 & any(is.na(envir))){
      stop("\n envir with NA \n")
    }
  }
  if(!is.null(parallel)){
    CL <- parallel::makeCluster(parallel, type = "PSOCK")
  } else {
    CL <- NULL
  }
  if (!is.null(traits)) {
    m <- ncol(traits)
    weights <- rep(1, m)
    make.names <- is.null(colnames(traits))
    colnames(traits) <- colnames(traits, do.NULL = FALSE, prefix = "T")
    names(weights) <- colnames(traits)
    if(!is.null(put.together)){
      if(!inherits(put.together, "list")){
        stop("\n put.together must be a object of class list\n")
      }
      if(make.names){
        for(k in 1:length(put.together)){
          put.together[[k]] <- paste("T", put.together[[k]], sep = "")
        }
      }
      if(max(table(unlist(put.together)))>1){
        stop("\n The same trait appears more than once in put.together\n")
      }
      if(length(setdiff(unlist(put.together),colnames(traits)))>0){
        stop("\n Check traits names in put.together\n")
      }
      for(k in 1:length(put.together)){
        weights[put.together[[k]]] <- 1/length(put.together[[k]])
      }
    }
    matrixT <- matrix.t(comm, traits, scale = scale, ranks = ranks, notification = FALSE)
    check.U <- function(traits, scale, ranks, ord, ...){
      vartype <- var.type(traits)
      if(missing(ord)){
        for(i in 1:length(vartype)){
          if(ranks & vartype[i] == "o"){
            traits[, i] <- rank(traits[, i], na.last = "keep")
          }
          traits[, i] <- as.numeric(traits[, i])
        }
        traits <- as.matrix(traits)
      }
      if (scale) {
        dist.traits <- FD::gowdis(traits, asym.bin = NULL, ...)
      }
      else{
        dist.traits <- as.matrix(vegan::vegdist(traits, method = "euclidean", diag = TRUE, upper = TRUE, na.rm = TRUE))
      }
      res <- any(is.na(dist.traits))
      return(res)
    }
    if(notification){
      if(check.U(traits, scale = scale, ranks = ranks, ord, w = weights)){
        warning("Warning: NA in distance matrix between species based in traits", call. = FALSE)
      }
    }
    matrixX <- matrix.x(comm, traits, scale = scale, ranks = ranks, notification = FALSE, ord, w = weights)
    W <- matrixT$matrix.w
    B <- matrixT$matrix.b
    T <- matrixT$matrix.T
    U <- matrixX$matrix.u
    X <- matrixX$matrix.X
    res.matrices$W <- W
    res.matrices$B <- B
    res.matrices$T <- T
    res.matrices$U <- U
    res.matrices$X <- X
    res$weights <- weights
    res$FunRao <- cbind(rao.diversity(comm, traits = B, checkdata = FALSE, put.together = put.together)$FunRao)
    colnames(res$FunRao) <- "FunRao"
    if (!is.null(envir)) {
      E <- envir
      if (scale.envir) {
        E <- cent.norm(envir, na.rm = na.rm)
      }
      res.matrices$E <- E
      if(romethod == 1){
        roTE <- cor.matrix(mx1 = W, mx2 = B, x = T, y = E, method = method, dist = dist, permutations = N, norm = scale, strata = strata, na.rm = na.rm, seqpermutation = seqpermutation, parallel = parallel, newClusters = FALSE, CL = CL)
        roXE <- cor.matrix(mx1 = W, mx2 = U, x = X, y = E, method = method, dist = dist, permutations = N, strata = strata, na.rm = na.rm, seqpermutation = seqpermutation, parallel = parallel, newClusters = FALSE, CL = CL)
        roXE.T <- cor.matrix.partial(mx1 = W, mx2 = U, x = X, y = E, mz1 = W, mz2 = B, z = T, permute.my2 = FALSE, permute.mz2 = TRUE, method = method, dist = dist, permutations = N, strata = strata, na.rm = na.rm, norm.z = scale, seqpermutation = seqpermutation, parallel = parallel, newClusters = FALSE, CL = CL)
        roRE <- cor.matrix2(mx1 = comm, B, res$FunRao, E, method = method, dist = dist, put.together = put.together, permutations = N, strata = strata, na.rm = na.rm, seqpermutation = seqpermutation, parallel = parallel, newClusters = FALSE, CL = CL)
      }
      if(romethod == 2){
        roTE <- pro.matrix(mx1 = W, mx2 = B, x = T, y = E, permutations = N, norm = scale, strata = strata, seqpermutation = seqpermutation, parallel = parallel, newClusters = FALSE, CL = CL)
        roXE <- pro.matrix(mx1 = W, mx2 = U, x = X, y = E, permutations = N, strata = strata, seqpermutation = seqpermutation, parallel = parallel, newClusters = FALSE, CL = CL)
        roXE.T <- pro.matrix.partial(mx1 = W, mx2 = U, x = X, y = E, mz1 = W, mz2 = B, z = T, permute.my2 = FALSE, permute.mz2 = TRUE, permutations = N, strata = strata, norm.z = scale, seqpermutation = seqpermutation, parallel = parallel, newClusters = FALSE, CL = CL)
        roRE <- pro.matrix2(mx1 = comm, B, res$FunRao, E, put.together = put.together, permutations = N, strata = strata, seqpermutation = seqpermutation, parallel = parallel, newClusters = FALSE, CL = CL)
      }
    }
  }
  if (!is.null(phylodist)) {
    matrixP <- matrix.p(comm, phylodist, notification = FALSE)
    W <- matrixP$matrix.w
    Q <- matrixP$matrix.q
    P <- matrixP$matrix.P
    res.matrices$W <- W
    res.matrices$Q <- Q
    res.matrices$P <- P
    if (!is.null(envir)) {
      E <- envir
      if (scale.envir) {
        E <- cent.norm(envir, na.rm = na.rm)
      }
      res.matrices$E <- E
      if(romethod == 1){
        roPE <- cor.matrix(mx1 = W, mx2 = Q, x = P, y = E, method = method, dist = dist, permutations = N, strata = strata, na.rm = na.rm, seqpermutation = seqpermutation, parallel = parallel, newClusters = FALSE, CL = CL)
      }
      if(romethod == 2){
        roPE <- pro.matrix(mx1 = W, mx2 = Q, x = P, y = E, permutations = N, strata = strata, seqpermutation = seqpermutation, parallel = parallel, newClusters = FALSE, CL = CL)
      }
      if (!is.null(traits)) {
        if(romethod == 1){
          roTE.P <- cor.matrix.partial(mx1 = W, mx2 = B, x = T, y = E, mz1 = W, mz2 = Q, z = P, permute.my2 = FALSE, permute.mz2 = TRUE, method = method, dist = dist, permutations = N, norm = scale, strata = strata, na.rm = na.rm, seqpermutation = seqpermutation, parallel = parallel, newClusters = FALSE, CL = CL)
          roXE.P <- cor.matrix.partial(mx1 = W, mx2 = U, x = X, y = E, mz1 = W, mz2 = Q, z = P, permute.my2 = FALSE, permute.mz2 = TRUE, method = method, dist = dist, permutations = N, strata = strata, na.rm = na.rm, seqpermutation = seqpermutation, parallel = parallel, newClusters = FALSE, CL = CL)
        }
        if(romethod == 2){
          roTE.P <- pro.matrix.partial(mx1 = W, mx2 = B, x = T, y = E, mz1 = W, mz2 = Q, z = P, permute.my2 = FALSE, permute.mz2 = TRUE, permutations = N, norm = scale, strata = strata, seqpermutation = seqpermutation, parallel = parallel, newClusters = FALSE, CL = CL)
          roXE.P <- pro.matrix.partial(mx1 = W, mx2 = U, x = X, y = E, mz1 = W, mz2 = Q, z = P, permute.my2 = FALSE, permute.mz2 = TRUE, permutations = N, strata = strata, seqpermutation = seqpermutation, parallel = parallel, newClusters = FALSE, CL = CL)
        }
      }
    }
    if (!is.null(traits)) {
      if(romethod == 1){
        roPT <- cor.matrix(mx1 = W, mx2 = Q, x = P, my1= W, my2 = B, y = T, permute.my2 = TRUE, method = method, dist = dist, permutations = N, norm.y = scale, strata = strata, na.rm = na.rm, seqpermutation = seqpermutation, parallel = parallel, newClusters = FALSE, CL = CL)
        roPX.T <- cor.matrix.partial(mx1 = W, mx2 = Q, x = P, my1 = W, my2 = U, y = X, mz1 = W, mz2 = B, z = T, permute.my2 = TRUE, permute.mz2 = TRUE, method = method, dist = dist, permutations = N, strata = strata, na.rm = na.rm, norm.z = scale, seqpermutation = seqpermutation, parallel = parallel, newClusters = FALSE, CL = CL)
      }
      if(romethod == 2){
        roPT <- pro.matrix(mx1 = W, mx2 = Q, x = P, my1= W, my2 = B, y = T, permute.my2 = TRUE, permutations = N, norm.y = scale, strata = strata, seqpermutation = seqpermutation, parallel = parallel, newClusters = FALSE, CL = CL)
        roPX.T <- pro.matrix.partial(mx1 = W, mx2 = Q, x = P, my1 = W, my2 = U, y = X, mz1 = W, mz2 = B, z = T, permute.my2 = TRUE, permute.mz2 = TRUE, permutations = N, strata = strata, norm.z = scale, seqpermutation = seqpermutation, parallel = parallel, newClusters = FALSE, CL = CL)
      }
      if(romethod == 1){
        if (scale) {
          dist.traits <- vegan::vegdist(traits, method = "gower", diag = TRUE, upper = TRUE, na.rm = na.rm)
        } else{
          dist.traits <- vegan::vegdist(traits, method = "euclidean", diag = TRUE, upper = TRUE, na.rm = na.rm)
        }
        roBF <- cor.mantel(dist.traits, stats::as.dist(phylodist), method = method, permutations = N, strata = strata, na.rm = na.rm, seqpermutation = seqpermutation, parallel = parallel, newClusters = FALSE, CL = CL)
      }
      if(romethod == 2){
        vectors <- vegan::wcmdscale(phylodist/max(phylodist), eig = TRUE)$points
        traits.t <- sweep(B, 2, sqrt(apply(B^2, 2, sum, na.rm = na.rm)), "/")
        roBF <- cor.procrustes(vectors, traits.t, permutations = N, strata = strata, na.rm = na.rm,seqpermutation = seqpermutation, parallel = parallel, newClusters = FALSE, CL = CL)
      }
    }
  }
  if(!is.null(parallel)){
    parallel::stopCluster(CL)
  }
  if(length(list.warning)>0){
    res$list.warning <- list.warning
  }
  res$statistics <- rbind(roTE, roXE, roPE, roPT, roPX.T, roXE.T, roTE.P, roXE.P, roBF, roRE)
  if(is.null(colnames(res$statistics))){
    colnames(res$statistics) <- "Obs"
  }
  res$matrices <- res.matrices
  class(res) <- "syncsa"
  return(res)
}
