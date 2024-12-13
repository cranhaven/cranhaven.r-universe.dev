#' Create a single object containing all necessary objects for multiResGrid functions
#' 
#' @eval MRGparam("ifg")
#' @eval MRGparam("ress")
#' @eval MRGparam("lnames")
#' @eval MRGparam("geovar")
#' @eval MRGparam("vars")
#' @eval MRGparam("weights")
#' @eval MRGparam("mincount")
#' @eval MRGparam("countFeatureOrTotal")
#' @eval MRGparam("nlarge")
#' @eval MRGparam("plim")
#' @eval MRGparam("verbose")
#' @eval MRGparam("nclus")
#' @eval MRGparam("clusType")
#' @eval MRGparam("plim")
#' @eval MRGparam("domEstat")
#' @eval MRGparam("consistencyCheck")
#' @eval MRGparam("outfile")
#' @eval MRGparam("splitlim")
#' @eval MRGparam("checkDominance")
#' @eval MRGparam("checkReliability")
#' @eval MRGparam("userfun")
#' @eval MRGparam("strat")
#' @eval MRGparam("confrules")
#' @eval MRGparam("suppresslim")
#' @eval MRGparam("sumsmall")
#' @eval MRGparam("suppresslimSum")
#' @eval MRGparam("reliabilitySplit")
#' @eval MRGparam("pseudoreg")
#' @eval MRGparam("plotIntermediate")
#' @eval MRGparam("addIntermediate")
#' @eval MRGparam("postProcess")
#' @eval MRGparam("rounding")
#' @eval MRGparam("remCols")
#' @eval MRGparam("locAdj")
#' @eval MRGparam("ellipsis")
#' 
#' 
#' @details The function creates a single object, containing both 
#' the mapped data and the parameters for for further processing.
#' This assures that all processing is done with the same variables.
#' 
#' 
#' @returns A list containing the necessary elements for further processing 
#' with the \code{MRG}-package.
#' 
#' 
#' 
#' 
#' @examples
#' \donttest{
#' library(sf)
#'
#' # These are SYNTHETIC agricultural FSS data 
#' data(ifs_dk) # Census data
#' 
#' # Create spatial data
#' ifg = fssgeo(ifs_dk, locAdj = "LL")
#' 
#' ress = 1000*2^(1:7)
#' MRGobject = createMRGobject(ifg = ifg, ress = ress, var = "UAA")
#' # Run the adaptive grid function only with farm number as con, then plot results
#' himg1 = multiResGrid(MRGobject)
#' 
#' himg1 = multiResGrid(MRGobject)
#' # Parameters can be updated in the object or in the call to multiResGrid
#' MRGobject$suppresslim = 0.02
#' himg2 = multiResGrid(MRGobject)
#' himg3 = multiResGrid(MRGobject, suppresslim = 0.05)
#' 
#'} 
#' 
#' 
#' @rdname createMRGobject
#' @export
createMRGobject = function(ifg, ress = c(1,5,10,20,40)*1000,  
                           geovar = c("GEO_LCT", "geometry"), lnames = NULL, vars = NULL, weights = NULL, 
                           mincount = 10, countFeatureOrTotal = "feature", #minpos = 4, 
                           nlarge = 2,
                           plim = 0.85, verbose = FALSE, nclus = 1, clusType = NULL, 
                           domEstat = TRUE, consistencyCheck = FALSE, outfile = NULL, splitlim = 50000000,
                           checkDominance = TRUE, checkReliability = FALSE, userfun = NULL, strat = NULL,
                           confrules = "individual", suppresslim = 0, sumsmall = FALSE, suppresslimSum = 0,
                           reliabilitySplit = TRUE, pseudoreg = NULL, plotIntermediate = FALSE, addIntermediate = FALSE, 
                           locAdj = "LL", 
                           postProcess = TRUE,
                           rounding = -1, remCols = TRUE, ...) {
  
  if (is.list(ifg) & !inherits(ifg, "data.frame")) {
    if (is.null(lnames)) if (!is.null(names(ifg))) lnames = names(ifg) else lnames = make.names(1:length(ifg))
    for (il in 1:length(ifg)) {
      geoids = which(names(ifg[[il]]) %in% geovar)
      names(ifg[[il]])[-geoids] = paste(names(ifg[[il]])[-geoids], lnames[il], sep = "_")
    }
    #' @importFrom plyr rbind.fill
    ifg = rbind.fill(ifg)
    vars = paste(rep(vars, each = length(lnames)), lnames, sep = "_")
  }
  
  if (!inherits(ifg, "sf")) ifg = fssgeo(ifg)
  
  if (!isFALSE(locAdj)) ifg = locAdjFun(ifg, locAdj, ress[1])
  
  gdl = gridData(ifg, res = ress, vars = vars, weights = weights, 
                                  nclus = nclus, verbose = verbose)  
  MRGobject = list(MRGinp = gdl, ifg = ifg, ress = ress, vars = vars, weights = weights,
                   mincount = mincount, countFeatureOrTotal = countFeatureOrTotal, #minpos = minpos, 
                   nlarge = nlarge, plim = plim,
                                 verbose = verbose, nclus = nclus, clusType = clusType,
                                 domEstat = domEstat, 
                                 consistencyCheck = consistencyCheck, outfile = outfile,
                                 splitlim = splitlim, checkDominance = checkDominance, checkReliability = checkReliability,
                                 userfun = userfun, strat = strat, confrules = confrules, suppresslim = suppresslim,
                                 sumsmall= sumsmall, suppresslimSum = suppresslimSum, 
                                 plotIntermediate = plotIntermediate, addIntermeidate = addIntermediate, 
                                 reliabilitySplit = reliabilitySplit, pseudoreg = pseudoreg,
                                 locAdj = locAdj, postProcess = postProcess, rounding = rounding, 
                                 remCols = remCols)

  dots = list(...)
  MRGobject = modifyList(MRGobject, dots)

  class(MRGobject) = "MRG"
  MRGobject
}


