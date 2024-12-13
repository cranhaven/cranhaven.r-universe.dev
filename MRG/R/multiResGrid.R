#' Create multi-resolution grids based on confidentiality or reliability restrictions
#' 
#' Function that creates a multi-resolution grid with larger grid cells in
#' regions with lower resolution of data, or where data needs to
#' be anonymized for disclosure control reasons. The function can also be used
#' to create a grid of new variables, using an existing multi-resolution grid 
#' as template.
#'
#' The possible restrictions that will lead to aggregation of a grid cell are:
#' \enumerate{
#'  \item{ Frequency rule (Aggregate to reach a minimum number of counts)}
#'  \item{ Dominance rule (Aggregate because of dominance by one or more units)}
#'  \item{ Reliability rule (Aggregate because the uncertainty is too high)}
#'  \item{ User defined rule (Aggregate because a grid cell does not respect a user defined criteria)}
#' }
#'
#'
#' @eval MRGparam("MRGinp")
#' @eval MRGparam("mincount")
#' @eval MRGparam("nlarge")
#' @eval MRGparam("plim")
#' @eval MRGparam("ifg")
#' @eval MRGparam("vars")
#' @eval MRGparam("weights")
#' @eval MRGparam("countFeatureOrTotal")
#' # @eval MRGparam("minpos")
#' @eval MRGparam("verbose")
#' @eval MRGparam("plim")
#' @eval MRGparam("domEstat")
#' @eval MRGparam("outfile")
#' @eval MRGparam("checkDominance")
#' @eval MRGparam("checkReliability")
#' @eval MRGparam("pseudoreg")
#' @eval MRGparam("userfun")
#' @eval MRGparam("strat")
#' @eval MRGparam("confrules")
#' @eval MRGparam("suppresslim")
#' @eval MRGparam("sumsmall")
#' @eval MRGparam("suppresslimSum")
#' @eval MRGparam("reliabilitySplit")
#' @eval MRGparam("plotIntermediate")
#' @eval MRGparam("addIntermediate")
#' @eval MRGparam("postProcess")
#' @eval MRGparam("rounding")
#' @eval MRGparam("remCols")
#' @eval MRGparam("ellipsis")
#'
#' @details This function will find the highest resolution data set that fulfills the 
#'        confidentiality rules and potential reliability rules for variable(s) of interest.
#'        Starting with the second highest resolution (5 km in the default settings),
#'        the function will check if any of the 1 km sub pixels will have values not fulfilling
#'        any of the confidentiality rules (number of farms, values of the 2 largest compared to values of the 
#'        entire grid cell).
#'        If all values are above the confidentiality limits, the grid cells will be kept at a 1 km resolution,
#'        otherwise only the 5 km grid cell will be kept. This will again be tested against the confidentiality
#'        rules in the next iteration, when grid cells will possibly be merged to 10 km grid cells.
#'
#'        The function can also be called if it is necessary to create a grid of a new variable for the same
#'        grid as an already existing variable. The confidentiality rules will then be applied to the new
#'        variables for the existing grid cells, and mask the ones that do not respect the rules.
#'        The function will not do any further merging
#'        of grid cells, for this it is necessary to grid the variables together.
#'        This feature is useful when the new data set has a similar resolution 
#'        as the original data set. It will give a high number of missing values if
#'        the resolution of the new data is more sparse than the original. In the examples below,
#'        this means that it is possible to copy the grid of organic organic 
#'        agricultural area to a grid of all agricultural area, whereas the opposite
#'        will not work well.
#'
#'        The standard threshold rule for spatial data is at least 10 units (mincount). 
#'        The parameters nlarge and plim are used for determining the dominance treatment for the variable of interest,
#'        with default values of \code{nlarge = 2} and \code{plim = 0.85}. 
#'        If more than plim of the values of the grid cell (e.g. UAA, arable land, number of livestock)
#'        is explained by 1-nlarge weighted holdings, the grid cell will not pass the confidentiality rule.
#'        
#'        The concept of reliability is explained in details in section 4.6 in the integrated farm survey handbook for 2023:
#'        https://wikis.ec.europa.eu/display/IFS/Integrated+Farm+Statistics+Manual+%7C+2023+edition
#'        In short, it is an estimate of the coefficient of variation for an estimate (a grid cell in this case),
#'        based on the number in the sample relative to the number in the population, and taking into account
#'        possible stratified sampling approaches. The number is zero if all holdings in the population in 
#'        a grid cell has been sampled, and the default requirement is that the CV is less than 35%.
#'        
#'        The computation can be time and memory intensive, particularly for the first iteration. 
#'        The method involves creation (and inversion) of a matrix of
#'        size \code{nr*ng}, where \code{nr} is the number of records and \code{ng} is the number of grid cells.
#'        it is therefore sometimes necessary to split the data set into smaller parts, to reduce
#'        the computational challenges. The parameter \code{reliabilitySplit} is used for this. 
#'        It will split the area of interest into several subsets. This will have some impact
#'        on the reliability calculations. The \code{reliabilitySplit} value might be set temporarily
#'        higher for the first iterations, as it will also depend on the number of grid cells.
#'        
#'        Reliability cannot be calculated for records belonging to strata with only one record.
#'        The function will therefore attempt to merge these into pseudostrata, if there is more than one 
#'        of these strata. The \code{pseudoreg}-parameter can be used
#'        to define the regions within which the pseudostrata are created (for example NUTS2-region).
#'        If there are still strata with only one record, these will cause a printed warning.
#'        
#'        
#'        There are some cases where aggregation might not be desired. In the situation where a 
#'        relatively large single grid cell does not respect the confidentiality rules, it is fine to 
#'        aggregate it if the neighbouring grid cells are also relatively large. However, it can be seen
#'        as unfortunate if the single cell was aggregated with many smaller grid cells that could otherwise 
#'        be disseminated at a high resolution. The added value of being able to present a value for a 
#'        region with very few farms is perhaps lower than what is lost by having to aggregate to a 
#'        lower resolution. The parameter \code{suppresslim} indicates the minimum value in a grid cell 
#'        relative to the possible lower resolution grid cell 
#'        before it is necessary to aggregate. If the limit is 0.05, a grid cell would only cause an aggregation
#'        to lower resolution if the value in the grid cell is more than 5\% of the value in the lower resolution 
#'        grid cell. Instead, it would be left as it is, and will be suppressed in the post-processing step.
#'
#'        There are cases when the built-in confidentiality checks are not what the user needs. 
#'        That is why it is possible to submit a user defined function. This function needs to follow
#'        certain rules. 
#'        \enumerate{
#'          \item The first argument must be a data.frame with name \code{df}. 
#'                This is a data.frame with the individual records
#'                for a particular grid cell. It has three columns: 
#'              \enumerate{
#'                 \item himgid - the ID of the current grid cell. This is the grouping variable
#'                        and is constant for the data.frame
#'                 \item gridvar - a new common name for the current variable to be gridded
#'                 \item weight - the weight of the variable to be gridded
#'              }  
#'          \item The function can include additional parameters for calculation of confidentiality
#'                 (or reliability, or suitability, if the meaning of the function refers to something else).
#'                 This can be new parameters to this particular function (through the 
#'                 ellipsis argument (...) of \code{multiResGrid}), existing parameters to \code{multiResGrid},
#'                 or potentially internal variables of \code{multiResGrid.})
#'          \item The result of the function must be a logical, either the rule was passed 
#'                for the records of this grid cell, or not (TRUE/FALSE)
#'          \item The function can potentially use internal variables of \code{multiResGrid}, however,
#'                the meaning of these will have to be understood from the code
#'        }
#'        A simple example of a \code{userfun} is given in the example section below (the one producing \code{himg6})
#'        
#'
#'
#' @returns The function will return a multi-resolution grid with observations
#' gridded to different grid cell sizes according to the confidentiality rules
#' to be applied. It can also include some additional columns that indicates 
#' which of the different confidentiality rules that have been applied.
#' 
#' Note that the function might (if \code{postProcess = FALSE})
#' return values also for the confidential grid-cells. 
#'
#' @examples
#' \donttest{
#' library(sf)
#' if (!require(ggplot2)) print("Plotting of results will not work 
#'                      without installation of ggplot2")
#' if (!require(viridis)) print("Some of the plots will not work 
#'                      without installation of viridis package")
#' if (!require(patchwork)) print("Some of the plots will not work 
#'                      without installation of patchwork")
#' 
#' if (require(giscoR)) {
#'   useBorder = TRUE 
#' } else {
#'   useBorder = FALSE
#'   print("You need to install giscoR for plotting borders and clipping the gridded maps")
#' }
#' # These are SYNTHETIC agricultural FSS data 
#' data(ifs_dk) # Census data
#' ifs_weight = ifs_dk %>% dplyr::filter(Sample == 1) # Extract weighted subsample
#' 
#' # Create spatial data
#' ifg = fssgeo(ifs_dk, locAdj = "LL")
#' fsg = fssgeo(ifs_weight, locAdj = "LL")
#' 
#' if (useBorder) {
#' # Read country borders, only used for plotting
#'   borders = gisco_get_nuts(nuts_level = 0)
#'   dkb = borders[borders$CNTR_CODE == "DK",] %>% st_transform(crs = 3035)
#' }
#'
#' ress = c(1,5,10,20,40, 80, 160)*1000
#' # Gridding Utilized agricultural area (UAA)
#' ifl = gridData(ifg, "UAA",res = ress)
#' # Gridding organic utilized agricultural area
#' ifl2 = gridData(ifg, vars = "UAAXK0000_ORG", res = ress)
#' 
#' # Gridding UAA and organic UAA together
#' ifl3 = gridData(ifg, vars = c("UAA", "UAAXK0000_ORG"), res = ress)
#' 
#' # Gridding the UAA from the survey - the survey weights are in the column EXT_MODULE
#' fsl = gridData(fsg,  vars = c("UAA"), weights = "EXT_MODULE",  res = ress)
#' 
#' # Create a multi-resolution grid only with farm number as confidentiality rule, then plot results
#' himg0 = multiResGrid(ifl, checkReliability = FALSE, suppresslim = 0)
#' ggplot(himg0) + geom_sf(aes(fill = count))
#' 
#' # Create a multi-resolution grid of UAA, also based on the dominance rule (default)
#' himg1 = multiResGrid(ifl, vars = "UAA", ifg = ifg)
#'   p1 = ggplot(himg1) + geom_sf(aes(fill = UAA))
#'   p1
#' # Create multi-resolution grid of organic UAA
#' himg2 = multiResGrid(ifl2, vars = "UAAXK0000_ORG", ifg = ifg)
#' himg21 = multiResGrid(ifl2, vars = "UAAXK0000_ORG", ifg = ifg, postProcess = FALSE)
#' 
#' ggplot(himg2) + geom_sf(aes(fill = UAAXK0000_ORG))
#' 
#' # Create joint multi-resolution grid of organic UAA and total UAA
#' himg3 = multiResGrid(ifl3, vars = c("UAA", "UAAXK0000_ORG"), ifg = ifg, 
#'                   checkReliability = FALSE, suppresslim = 0)
#' # Create multi-resolution grid of organic UAA, based on the UAA grid
#' # The large number of missing values indicates that this feature should
#' # mainly be used for data that have similar or higher resolution as the
#' # original data set.
#' himg33 = multiResGrid(himg1, vars = c("UAAXK0000_ORG"), ifg = ifg, 
#'                   checkReliability = FALSE, suppresslim = 0)
#' p31 = ggplot(himg3) + geom_sf(aes(fill = UAA))
#' p32 = ggplot(himg3) + geom_sf(aes(fill = UAAXK0000_ORG))
#' p33 = ggplot(himg33) + geom_sf(aes(fill = UAAXK0000_ORG))
#' p31 + p32 + p33
#' 
#' # Create multi-resolution grid of UAA, based on survey data,
#' # with and without applying reliability check
#' # This is a relatively slow functionality
#' # rounding is set to FALSE, to be better able to visualize the few records
#' # (Not recommended for data to be published)
#' himg4 = multiResGrid(fsl,  vars = c("UAA"), weights = "EXT_MODULE", ifg = fsg, 
#'                       strat = "STRA_ID_CORE", checkReliability = FALSE, rounding = FALSE)
#'# The parameter reliabilitySplit = 15 will divide the data set in 15 groups for the 
#'# reliabilityCheck.
#'# A lower value would be recommended, but a high value speeds up the computation for this example
#' himg5 = multiResGrid(fsl,  vars = c("UAA"), weights = "EXT_MODULE", ifg = fsg, 
#'                       strat = "STRA_ID_CORE", checkReliability = TRUE, 
#'                       reliabilitySplit = TRUE, rounding = FALSE, pseudoreg = "REGIONS")
#'                       
#'# Apply suppreslim to suppress insignificant grid cells
#'# Show intermediate maps of confidential cells (wait 5 seconds)
#' pint = ifelse(interactive(), 5, FALSE)
#' #himg11 = multiResGrid(ifl, vars = "UAA", ifg = ifg, 
#' #                  suppresslim = 0, plotIntermediate = pint)
#' himg11 = himg1
#' himg12 = multiResGrid(ifl, vars = "UAA", ifg = ifg, 
#'                  suppresslim = 0.02, plotIntermediate = pint)
#' himg13 = multiResGrid(ifl, vars = "UAA", ifg = ifg, 
#'                  suppresslim = 0.05, plotIntermediate = pint)
#' himg14 = multiResGrid(ifl, vars = "UAA", ifg = ifg, 
#'                  suppresslim = 0.1, plotIntermediate = pint)
#'  
#'  
#'  # This is an example of a userfun that can be used for alternative restrictions
#'  # for a grid cell. This particular toy example assures that there are at least
#'  # \\code{nabove} records with a value (UAA in this case) above a certain "limit". 
#'  ufun = function(df, nabove, limit) {
#'    sum(df$gridvar > limit) < nabove
#'  }
#'  
#' himg6 = multiResGrid(ifl, vars = "UAA", ifg = ifg, 
#'                  suppresslim = 0.2, plotIntermediate = pint, userfun = ufun, nabove = 5, limit = 10)
#'  
#'  
#'  
#' if (useBorder) himg00 = st_intersection(dkb, himg0) else himg00 = himg0
#' p00 = ggplot() + geom_sf(data = himg00, aes(fill = count, color = count)) +
#'   scale_fill_viridis( name = "number of farms", trans = "log10") +
#'   scale_color_viridis( name = "number of farms", trans = "log10") +
#'   coord_sf(crs = 3035) +#, xlim = c(2377294, 6400000), ylim = c(1313597, 5628510)) +
#'   ggtitle("Number of farms for variable grid cell size, only frequency confidentiality") +
#'   theme_bw()
#' if (useBorder) p00 = p00 + geom_sf(data = dkb, fill = NA, colour='black', lwd = 1) 
#' p00
#'   
#' if (useBorder) himg01 = st_intersection(dkb, himg1) else himg01 = himg1
#' p01 = ggplot() + geom_sf(data = himg01, aes(fill = count, color = count)) +
#'   scale_fill_viridis( name = "number of farms", trans = "log10") +
#'   scale_color_viridis( name = "number of farms", trans = "log10") +
#'   coord_sf(crs = 3035) +#, xlim = c(2377294, 6400000), ylim = c(1313597, 5628510)) +
#'   ggtitle("Number of farms for variable grid cell size, frequency and dominance confidentiality") +
#'   theme_bw()
#' if (useBorder) p01 = p01 + geom_sf(data = dkb, fill = NA, colour='black', lwd = 1) 
#' p01
#'   
#' # Plot the density of organic agriculture, as hectares per square km
#' if (useBorder)himg02 = st_intersection(dkb, himg2) else himg02 = himg2
#' himg02$orgarea = himg02$UAAXK0000_ORG/units::set_units(st_area(himg02), "km^2")
#' units(himg02$orgarea) = NULL
#' p02 = ggplot() + geom_sf(data = himg02, aes(fill = orgarea), lwd = 0) +
#'   scale_fill_viridis( name = "ha / km2") +
#'   coord_sf(crs = 3035) +#, xlim = c(2377294, 6400000), ylim = c(1313597, 5628510)) +
#'   ggtitle("Organic UAA density")  +
#'   theme_bw()
#' if (useBorder) p02 = p02 + geom_sf(data = dkb, fill = NA, colour='black', lwd = 1) 
#' p02
#' 
#' # Plot the relative abundance of organic UAA relative to total UAA
#' if (useBorder) himg03 = st_intersection(dkb, himg3) else himg03 = himg3
#' himg03$ouaashare = himg03$UAAXK0000_ORG/himg03$UAA*100
#' p03 = ggplot() + geom_sf(data = himg03, aes(fill = ouaashare), lwd = 0) +
#'   scale_fill_viridis( name = "% Organic") +
#'   coord_sf(crs = 3035) +#, xlim = c(2377294, 6400000), ylim = c(1313597, 5628510)) +
#'   ggtitle("Organic share")  +
#'   theme_bw()
#' if (useBorder) p03 = p03 + geom_sf(data = dkb, fill = NA, colour='black', lwd = 1) 
#' p03
#'   
#'   
#' # Plot maps from survey data before and after adding the reliability constraint 
#' # The percentage of UAA can be above 100% due to farm area being registered at the location
#' # of the administration building, but the map without reliability check has too high values 
#' # for too many cells
#' 
#' if (useBorder) himg04 = st_intersection(dkb, himg4) else himg04 = himg4
#' himg04$area = st_area(himg04)/1e6
#' units(himg04$area) = NULL
#' himg04$uaashare = himg04$UAA/himg04$area
#' himg04$uaashare[himg04$uaashare > 1000] = 1000
#' p04 = ggplot() + geom_sf(data = himg04, aes(fill = uaashare), lwd = 0) +
#'   scale_fill_viridis( name = "% UAA",  trans = "log10", limits = c(1,1000)) +
#'   geom_sf(data = dkb, fill = NA, colour='black', lwd = 1) +
#'   coord_sf(crs = 3035) +#, xlim = c(2377294, 6400000), ylim = c(1313597, 5628510)) +
#'   ggtitle("UAA share (sample without reliability check)")  +
#'   theme_bw()
#' if (useBorder) p04 = p04 + geom_sf(data = dkb, fill = NA, colour='black', lwd = 1) 
#' p04
#'   
#' if (useBorder) himg05 = st_intersection(dkb, himg5) else himg05 = himg5
#' himg05$area = st_area(himg05)/1e6
#' units(himg05$area) = NULL
#' himg05$uaashare = himg05$UAA/himg05$area
#' himg05$uaashare[himg05$uaashare > 1000] = 1000
#' p05 = ggplot() + geom_sf(data = himg05, aes(fill = uaashare), lwd = 0) +
#'   scale_fill_viridis( name = "% UAA",  trans = "log10", limits = c(1,1000)) +
#'   coord_sf(crs = 3035) +#, xlim = c(2377294, 6400000), ylim = c(1313597, 5628510)) +
#'   ggtitle("UAA share (sample with reliability check)")  +
#'   theme_bw()
#' if (useBorder) p05 = p05 + geom_sf(data = dkb, fill = NA, colour='black', lwd = 1) 
#'   
#' if (require(patchwork)) p04 + p05 + plot_layout(guides = "collect")
#'   
#' if (useBorder) himg06 = st_intersection(dkb, himg6) else himg06 = himg6
#' p06 = ggplot() + geom_sf(data = himg06, aes(fill = UAA), lwd = 0) +
#'   scale_fill_viridis( name = "ha") +
#'   coord_sf(crs = 3035) +#, xlim = c(2377294, 6400000), ylim = c(1313597, 5628510)) +
#'   ggtitle("UAA, with additional user defined function")  +
#'   theme_bw()
#' if (useBorder) p06 = p06 + geom_sf(data = dkb, fill = NA, colour='black', lwd = 1) 
#' p06
#'   
#'      
#' # Plot the different maps from using different suppreslim values
#' himgs = list(himg11, himg12, himg13, himg14)
#' slims = c(0, 0.02, 0.05, 0.1, 0.2)
#' plots = list()
#' uaas = c(himg11$UAA, himg12$UAA, himg13$UAA, himg14$UAA)
#' lims = range(uaas[uaas > 0], na.rm = TRUE)
#' for (ii in 1:4) {
#'   if (useBorder) himg = st_intersection(dkb, himgs[[ii]]) else himg = himgs[[ii]]
#'   plots[[ii]] = 
#'    ggplot() + geom_sf(data = himg, aes(fill = UAA), lwd = 0) +
#'     scale_fill_viridis( name = "UAA (ha)", trans = "log10", limits = lims, na.value="red") +
#'     ggtitle(paste("Suppresslim = ", slims[[ii]])) +
#'     xlab("") + ylab("") +
#'     theme_bw()
#'    if (useBorder) plots[[ii]] = plots[[ii]] + 
#'                      geom_sf(data = dkb, fill = NA, colour='black', lwd = 0.5)
#' }
#' 
#' if (require(patchwork)) plots[[1]]  + plots[[2]] + plots[[3]]  + plots[[4]] + 
#'                               plot_layout(guides = "collect")
#'  
#' 
#' }
#' 
#' #' @rdname multiResGrid
#' @export
multiResGrid  <- function(MRGinp, ...) UseMethod("multiResGrid")
#' 
#' 
#' @rdname multiResGrid
#' @export
multiResGrid.MRG <- function(MRGinp, ...) {
  dots = list(...)
  #' @importFrom utils modifyList
  if (length(dots) > 0) MRGinp = modifyList(MRGinp, dots)
  do.call(multiResGrid, MRGinp)
}
#'
#' @rdname multiResGrid
#' @export
multiResGrid.sf <- function(MRGinp, ..., ifg, vars) {
  if (missing(ifg) | missing(vars)) stop("Both ifg and vars (as named variables) are necessary for the data.frame method of multiResGrid")
  dots = c(list(...), list(ifg = ifg, vars = vars))
  #' @importFrom utils modifyList
  MRGobject = list(list(MRGinp = MRGinp))
  MRGobject = modifyList(MRGobject, dots)
  do.call(multiResGrid, MRGobject)
}
#' 
#' 
#' 
#' @rdname multiResGrid
#' @export
multiResGrid.list <- function(MRGinp, ifg, vars, weights, countFeatureOrTotal = "feature", mincount = 10, #minpos = 4, 
                              nlarge = 2,
                              plim = 0.85, verbose = FALSE, domEstat = TRUE, 
                              outfile = NULL, checkDominance = TRUE,
                              checkReliability = FALSE, userfun, strat = NULL, confrules = "individual", 
                              suppresslim = 0, sumsmall = FALSE, suppresslimSum = NULL,
                              reliabilitySplit = TRUE, pseudoreg = NULL,
                              plotIntermediate = FALSE,  addIntermediate = FALSE,
                              postProcess = TRUE, rounding = -1, remCols = TRUE, ...) {
  #  To avoid R CMD check notes
  hsum = wsum = www = small = weight = data = himgid = dominance = . = NULL
  if (!missing(ifg) && !inherits(ifg, "sf")) stop("ifg is not an sf-object ")
  
  if (length(MRGinp) > 1) {
    if (!inherits(MRGinp[[1]], "sf")) cat("MRGinp is not a list of sf-objects \n")
    if (!"res" %in% names(MRGinp[[1]])) cat("MRGinp does not have a column with resolutions \n")
    ress = unlist(lapply(MRGinp, FUN = function(MRGinpl) MRGinpl$res[1])) 
  } else {
    if (!inherits(MRGinp, "sf")) cat("MRGinp is not an sf-object \n")
    ress = 0
  }
  if (checkReliability) {
    if (missing(strat) | is.null(strat) ) {
      if (!"strat" %in% names(ifg)) ifg$strat = 1  
      #' @importFrom dplyr mutate group_by
    } else ifg = ifg %>% mutate(strat = .data[[strat]])
    if (missing(pseudoreg) | is.null(pseudoreg)) {
      if (!"pseudoreg" %in% names(ifg)) ifg$pseudoreg = 1
    } else ifg = ifg %>% mutate(pseudoreg = .data[[pseudoreg]])
  }
  
  if (!missing(vars)) {
    if (missing(ifg))
      stop(paste("Cannot create values for variable(s) ",
                 vars, " without ifg"))
    
    # Some kind of test needed to check if this step has already been done, and also if it is necessary    
    #    for (iw in 1:length(vars)) ifg[, paste0(vars, iw)] = data.frame(ifg)[, vars[iw]]
    if (!"ID" %in% names(ifg)) ifg$ID = 1:dim(ifg)[1]
    if (!length(grep(paste0("weight_", vars, collapse = "|"), names(ifg))) == length(vars)) ifg = addweights(ifg, vars, weights)
    for (iw in 1:length(vars)) {
      ifg[, paste0("gridvar", iw)] = st_drop_geometry(ifg[, vars[iw]])
      ifg[, paste0("weight", iw)] = st_drop_geometry(ifg[, paste0("weight_", vars[iw])])
    }
    if (checkReliability) {
      ifg = ifg[, c("ID", paste0("gridvar", 1:length(vars)), paste0("weight", 1:length(vars)), "strat", "pseudoreg")]
    } else {
      ifg = ifg[, c("ID", paste0("gridvar", 1:length(vars)), paste0("weight", 1:length(vars)))]
    }
  } 
  if (length(MRGinp) == 1) {
    loh = NULL
    MRGinp = MRGinp[[1]]   
    if (!"ID" %in% names(MRGinp)) MRGinp$ID = 1:dim(MRGinp)[1]
    if (!"ID" %in% names(ifg)) ifg$ID = 1:dim(ifg)[1]
    ifg = ifg %>% mutate(himgid = st_join(., MRGinp, join = st_within)$ID.y) 
    himg = MRGinp[, c("ID", "res")]
    #' @importFrom dplyr summarize count
    himg = himg %>% mutate(count = st_drop_geometry(ifg) %>% group_by(himgid) %>% summarize(count = n()) %>% select(count) %>% pull)
    
    if (missing(weights) || is.null(weights)) weights = 1
    if (weights == 1)  himg$countw = himg$count
    
    if (!is.null(vars)) {
      if (length(vars) > 0 & length(weights) == 1) weights = rep(weights, length(vars))
      for (ivar in 1:length(vars)) {
        ww = paste0("weight", ivar)
        vv = paste0("gridvar", ivar)
        vvv = vars[ivar]
        #' @importFrom rlang :=
        if (!is.null(ww) & ww != 1) ifg$wsum = (st_drop_geometry(ifg[,vv]) %>% pull)*(st_drop_geometry(ifg[,ww]) %>% pull) else ifg$gridvar = st_drop_geometry(ifg[,vv]) %>% pull
        himg = himg %>% mutate(!!vvv := st_drop_geometry(ifg) %>% group_by(himgid)  %>% summarize(vvv = sum(wsum)) %>% select(vvv) %>% pull) %>%
          mutate(!!ww  := st_drop_geometry(ifg) %>% group_by(himgid)  %>% summarize(www = sum(.data[[ww]])) %>% select(www) %>% pull)
      }
    }
  } else {
    himg = MRGinp[[1]]   
    if (missing(vars)) {
      vvars = NULL
      wweights = NULL
    } else {
      vvars = vars
      wweights = paste0("weight_", vars)
    }
    hcols = which(names(himg)  %in% c("ID", "res", "count", "countw", "geometry", vvars, wweights))
    himg = himg[,hcols]
  }
  himg = himg %>% mutate(confidential = FALSE, reliability = FALSE, small = FALSE,
                         freq = FALSE, dom = FALSE, ufun = FALSE)
  himgs = list()
  lohs = list()
  if (!missing(vars)) for (ivar in 1:length(vars)) himg[,paste0("vres", ivar)] = 0
  for (ires in 2:(length(ress) + 1)) {
    if (verbose) cat("Creating multi-resolution grid, iteration ", ires, "\n")
    lres = ress[ires]
    if (ires <= length(ress)) {
      limg = MRGinp[[ires]] 
      lcols = which(names(limg)  %in% c("ID", "res", "count", "countw", "geometry", vvars, wweights))
      limg = limg[,lcols]
      limg = limg %>% mutate(confidential = FALSE, reliability = FALSE, small = FALSE,
                             freq = FALSE, dom = FALSE, ufun = FALSE)
      
      if (!missing(vars)) for (ivar in 1:length(vars)) limg[,paste0("vres", ivar)] = 0
    }    
    ##    #' @importFrom utils txtProgressBar setTxtProgressBar
    #' @importFrom sf st_join st_within st_drop_geometry st_make_grid st_coordinates st_crs st_area
    if (!missing(ifg) && !is.null(ifg) && ires <= length(ress)) {
      ifg$himgid = st_join(ifg, himg, join = st_within)$ID.y
      ifg$limgid = st_join(ifg, limg, join = st_within)$ID.y
      ifgl = data.frame(ifg)[, grep("ID|himgid|gridvar|weight|strat", names(ifg))]
    } else {
      ifgl = NULL
      if (!missing(ifg)) {
        ifg$himgid = st_join(ifg, himg, join = st_within)$ID.y
        ifgl = data.frame(ifg)[, grep("ID|himgid|gridvar|weight|strat", names(ifg))]
      } 
    }
    if (ires <= length(ress)) loh = st_drop_geometry(st_join(himg, limg, join = st_within))
    if (!missing(vars) & ires <= length(ress)) {
      for (ivar in 1:length(vars)){
        if (tolower(countFeatureOrTotal) == "feature") {
          #          sel = (loh[[paste0(vars[ivar], "_w", ivar, ".x")]]  <
          #                   suppresslim*loh[[paste0(vars[ivar], "_w", ivar, ".y")]]) & loh[[paste0("weight",ivar, ".x")]] < mincount
          sel = (loh[[paste0(vars[ivar], ".x")]]  <
                   suppresslim*loh[[paste0(vars[ivar], ".y")]]) & loh[[paste0("weight_",vars[ivar], ".x")]] < mincount
          #        } else sel = (loh[[paste0(vars[ivar], "_w", ivar, ".x")]]  <
          #                        suppresslim*loh[[paste0(vars[ivar], "_w", ivar, ".y")]]) & loh[["countw.x"]] < mincount
        } else sel = (loh[[paste0(vars[ivar], ".x")]]  <
                        suppresslim*loh[[paste0(vars[ivar], ".y")]]) & loh[["countw.x"]] < mincount
        if (sumsmall & sum(sel) == 0) {
          sshare = data.frame(Group.1 = 999999, x = 9999999)
        } else if (sumsmall) {
          #' @importFrom stats aggregate
          #          sshare = aggregate(loh[[paste0(vars[ivar], "_w", ivar, ".x")]][sel], by = list(loh$ID.y[sel]), sum) 
          sshare = aggregate(loh[[paste0(vars[ivar], ".x")]][sel], by = list(loh$ID.y[sel]), sum) 
          loh$sshare = sshare$x[match(loh$ID.y, sshare$Group.1)]
          #          loh$sshare[is.na(loh$sshare)] = max(loh[[paste0(vars[ivar], "_w", ivar, ".y")]])
          loh$sshare[is.na(loh$sshare)] = max(loh[[paste0(vars[ivar], ".y")]])
          #          loh[[paste("hsmall", ivar)]] = loh$sshare <  suppresslimSum*loh[[paste0(vars[ivar], "_w", ivar, ".y")]]
          loh[[paste("hsmall", ivar)]] = loh$sshare <  suppresslimSum*loh[[paste0(vars[ivar], ".y")]]
        } else {
          loh[[paste("hsmall", ivar)]] = sel
        }
      }
      
      #' @importFrom tidyselect contains
      himg$small = loh %>% select(contains("hsmall")) %>% mutate(hsum = rowSums(.)) %>%
        mutate(small = (hsum == length(vars))) %>% select(small) %>% pull
    }
    himgdat = st_drop_geometry(himg)
    ifgdat = st_drop_geometry(ifgl)
    
    if (tolower(countFeatureOrTotal) == "feature" & !missing(vars)) {
      ww = himgdat[,names(himgdat) %in% paste0("weight_", vars), drop = FALSE]
      ww = apply(ww, MARGIN = 1, FUN = function(x) if (sum(x > 0))  min(x[x>0]) else 0)
    } else ww = himgdat[, "countw"]
    wf = which(ww > 0 & ww < mincount)
    if (length(wf) > 0)  himg$freq[which(himg$ID %in% himgdat$ID[wf])] = TRUE
    len = ifelse(missing(vars), 1, length(vars))
    ehimgid = ufres = NULL
    for (ivar in 1:len) {
      ifgdatl = NULL
      if (checkDominance & !missing(vars)) {
        if (verbose) cat("Checking dominance \n")
        ifgdatl <- ifgdat[,c("himgid", paste0("gridvar", ivar), paste0("weight",ivar))] 
        names(ifgdatl) = c("himgid", "gridvar", "weight")
        ifgdatl$ehimgid = ifgdatl$himgid
        ifgdatl = ifgdatl[order(ifgdatl$ehimgid),]
        #' @importFrom tidyr unnest nest 
        #' @importFrom purrr map 
        dom = ifgdatl %>% filter(weight != 0)  %>%
          group_by(ehimgid) %>%  nest() %>%
          mutate(dominance = map(data, ~dominanceRule(., nlarge = nlarge, plim = plim, 
                                                      domEstat = domEstat))) %>%
          unnest(dominance) %>% ungroup %>% select(dominance) %>% pull 
        
        domid = which(dom)
        if (length(domid) > 0) himg$dom[domid] = TRUE
      } 
      if (!missing(userfun) && is.function(userfun)) {
        if (verbose) cat("Checking userfun \n")
        if (is.null(ifgdatl)) ifgdatl <- ifgdat[,c("himgid", paste0("gridvar", ivar), 
                                                   paste0("weight",ivar))] 
        
        ifgdatl$ehimgid = ifgdatl$himgid
        ifgdatl = ifgdatl[order(ifgdatl$ehimgid),]
        dots = list(...)
        # The next 14 lines are based on an answer on StackOverflow:
        # https://stackoverflow.com/questions/78647845/using-purrrmap-with-a-user-defined-function-how-to-pass-arguments
        # by the user Nir Graham (userid: 11726436)
        # licensed by StackOverlow under CC BY-SA 4.0
        fargs = names(formals(userfun))
        if ("hareas" %in% fargs) hareas = st_area(himg)
        passed_in  <- setdiff(intersect(ls(),fargs),"df")
        names(passed_in) <- passed_in
        args_to_pass <- map(passed_in, dynGet)
        ddots = dots[names(dots) %in% fargs]
        args_to_pass <- c(args_to_pass,ddots)
        
        localUserFun = function(subdata, args_to_pass) {
          local_args <- c(list("df" = subdata), args_to_pass)
          do.call(userfun, args=local_args)}
        
        ufRes = ifgdatl %>% 
          group_by(ehimgid) %>%  nest() %>%
          mutate(ufres = map(data, ~localUserFun(., args_to_pass))) %>%
          unnest(ufres) %>% ungroup %>% select(ufres) %>% pull 
        ufid = which(ufRes)
        if (length(ufid) > 0) himg$ufun[ufid] = TRUE
      }
    }
    
    if (checkReliability) {
      if (verbose) cat("Checking reliability \n")
      rsplit = reliabilitySplit
      if (reliabilitySplit & (dim(ifg)[1] > 50000 | dim(himg)[1] > 1000)) {
        if (is.logical(reliabilitySplit)) rsplit = dim(ifg)[1] %/% 30000
        if (dim(himg)[1] > rsplit*1000) rsplit = dim(himg)[1] %/% 1000
      }
      if (!missing(vars) && !is.null(vars)){
        for (ivar in 1:length(vars)){
          nhimg = dim(himg)[1]
          vestres = mrg_varestim(ifg, var = paste0("gridvar", ivar), strat = "strat", PSU = "ID", 
                                 weight = paste0("weight", ivar), split = rsplit, pseudoreg = "pseudoreg", 
                                 verbose = verbose, nhimg = nhimg)
          himg[,paste0("vres",ivar)] = vestres
        }
      }
      nonvalids = suppressWarnings(which(apply(st_drop_geometry(himg[, grep("vres", names(himg))]), 1, max, na.rm = TRUE) > 0.35))
      himg$reliability[nonvalids] = TRUE
    }
    
    himg$confidential = rowSums(st_drop_geometry(himg[,c("freq", "dom", "ufun", "reliability")])) > 0
    if (verbose) cat("Finished checks, updating multi-resoluion grid \n")
    
    if (ires <= length(ress)) {
      loh$confidential = himg$confidential
      loh$small = himg$small
      idRem = which(himg$confidential & !himg$small)
      # Compute how many non-confidential himg-cells there are per limg-cell
      nclimg = aggregate(!loh$confidential, by = list(loh$ID.y), sum)
      loh$nclimg = nclimg$x[loh$ID.y]
      # Add cells to the aggregation list if none of the hing-cells in a limg-cell
      # are non-confidential, even if tthey are are small
      idAdd = NULL
      idRem = unique(c(idRem, which(loh$small & loh$nclimg == 0)))
      if (length(idRem) > 0) {
        idAdd = which(limg$ID %in% unique(loh$ID.y[loh$ID.x %in% idRem]))
        # Check how many himg-cells per limg-cell. Do not aggregate if it is only 1 
        iac = aggregate(rep(1, length(loh$ID.x)), by = list(loh$ID.y), FUN = sum)
        singlimg = iac$Group.1[iac$x == 1]
        if (length(singlimg) > 0) {
          idAdd = idAdd[!(idAdd %in% singlimg)]
          himg$singlimg = FALSE
          himg$singlimg[loh$ID.y %in% singlimg] = TRUE
        }
        if ("singlimg" %in% names(himg)) limg$singlimg = FALSE
        # Find all himg-cells in the limg-cells to be added 
        idRem = which(himg$ID %in% unique(loh$ID.x[loh$ID.y %in% idAdd]))
        if (length(idRem) == 0) break
        remIdRem = loh$ID.x[loh$ID.y %in% singlimg]
        if (length(remIdRem) > 0) {
          if (sum(idRem %in% remIdRem) > 0) {
            stop(paste("There are still single himg-cells to be deleted from limg-cells
                This should not happen", sum(idRem %in% remIdRem)))
          }
          idRem = idRem[!(idRem %in% remIdRem)]
        }
        
        himg = himg[-idRem,]
        if (verbose > 1) {
          cat("names(himg):", names(himg), "\n")
          cat("names(limg):", names(limg), "\n")
          cat("length(idAdd)", length(idAdd), "\n")
          cat("sum(himg$singlimg", sum(himg$singlimg), "\n")
        }
        himg = rbind(himg, limg[idAdd,])
      } 
    } else {idRem = NULL; idAdd = NULL}      
    # Reindex himg, and add himg and loh to the lists
    himg$ID = 1:dim(himg)[1]
    himgs[[ires]] = himg
    lohs[[ires]] = loh
    
    if (ires <= length(ress)) print(paste("ires", ires, ress[ires], "#himg-cells:", dim(himg)[1], "; removed:", 
                                          length(idRem), "; added:", length(idAdd), "; confidential:", sum(himg$confidential) ))
    if (plotIntermediate) {
      plot(himg[, "confidential"], main = "Confidential cells")
      Sys.sleep(ifelse(is.logical(plotIntermediate), 5, plotIntermediate))
    }
  }
  if ("gridvar" %in% names(himg)) himg = himg[, -grep("gridvar", names(himg))]
  if (!missing(vars)) attr(himg, "vars") = vars
  if (postProcess) {
    himg = MRGpostProcess(himg, vars, remCols, rounding)
  } else {
    attr(himg, "remCols" ) = remCols
    attr(himg, "rounding") = rounding
  }
  if (addIntermediate) {
    attr(himg, "himgs") = himgs
    attr(himg, "lohs") = lohs
  }
  himg
}









mrg_varestim <- function(x, var, strat, PSU, weight, split, verbose, pseudoreg, nhimg){
  t0 = proc.time()[3]
  ID = n = hld = w_sum = wdiff = NULL
  nx = dim(x)[1]
  himgids = unique(x$himgid)
  if (inherits(x, "sf")) x = st_drop_geometry(x)
  if (!missing(PSU)) x$ID = x[[PSU]]  
  icor = 0
  if (missing(strat) || is.null(strat)) {
    x$strat = 1
  } else {
    if (!strat %in% names(x)) stop(paste(strat, "is missing from from the data.frame"))
    x$strat = x[[strat]]  
  }
  
  # Check if any grid cells only have unit value weights
  tt = st_drop_geometry(x) %>% group_by(himgid) %>% 
    summarise(wdiff = sum(abs(.data[[weight]] - 1), na.rm = T)) %>% filter(wdiff < 0.1)
  if (dim(tt)[1] > 0) {
    x = x[!(x$himgid %in% tt$himgid), ]
  }
  t1 = proc.time()[3]
  if (verbose > 1) cat("Varestim - finished preprocessing - t= ", round(t1-t0,2), "secs \n")
  if (dim(x)[1] == 0) {
    out_var = data.frame(himgid = himgids, rse = 0)
  } else {
    if (split == 1) {
      df = x
      out_var = vardom(dataset = df, Y= var, H = "strat",
                       PSU = "ID",
                       w_final = weight, Dom = "himgid")$all_result
      t21 = proc.time()[3]
      if (verbose > 1) cat("varestim - finished single split vardom estimation in ", round(t21-t1, 2), "secs \n")
    } else {
      himgid <- unique(x$himgid)    
      #' @importFrom dplyr left_join mutate ungroup group_by distinct case_when n
      #' @importFrom sjmisc split_var 
      df_cl <- left_join(x, data.frame(himgid = himgid, cluster = split_var(himgid, n = split)), by = "himgid")
      out_var <- NULL
      t20 = proc.time()[3]
      if (verbose > 1) cat("varestim - will split reliability calcs in ", split, "cases\n")
      for (isp in 1:split){
        t21 = proc.time()[3]
        df = x[which(df_cl$cluster == isp),]
        if (verbose) print(paste("reliabilitySplit: ", isp, 
                                 "- Number of records: ", paste(dim(df)[1], 
                                                                " - Number of unique IDs: ", length(unique(df$himgid)))))
        
        icor = icor + 1
        t <- df %>% group_by(strat) %>% 
          summarise(hld=n(), w_sum = sum(.data[[weight]], na.rm = T)) %>% 
          filter(hld == 1 & w_sum > 1) %>% ungroup
        if (!is.null(pseudoreg)) pcor = as.numeric(as.factor(df[[pseudoreg]])) else pcor = 0
        if (dim(t)[1] > 0){
          h_st <- t %>% distinct(strat) %>% pull()
          df <- df %>% mutate(strat = case_when(strat %in% c(h_st)~(99999-pcor),
                                                T ~ strat))}
        #' @importFrom vardpoor vardom
        t22 = proc.time()[3]
        if (verbose > 1) cat("varestim - ready to call vardom for split ", isp, "of", split, "time:", round(t22-t21,2), "secs\n")
        est <- vardom(dataset = df, Y= var, H = "strat",
                      PSU = "ID",
                      w_final = weight, Dom = "himgid")
        t23 = proc.time()[3]
        if (verbose > 1) cat("varestim - ready to call vardom for split ", isp, "of", split, "time:", round(t23-t22,2), "secs\n")
        out_var<-rbind(out_var, est$all_result)
      }
    }
    if (verbose) cat("varestim - finished looping in totally ", round(proc.time()[3]-t1,2), "secs \n")
    if (icor > 0) {
      cat(icor, "of the subsets included strata with only one record. \n",
          "You might want to check the strata or consider a lower value for reliabilitySplit.")
    }
    out_var$himgid = as.numeric(out_var$himgid)
    if (dim(tt)[1] > 0) out_var = rbind(out_var[,c("himgid", "rse")], data.frame(himgid = tt$himgid, rse = 0))
    out_var = out_var[order(out_var$himgid),]
  }
  if (!missing(nhimg) && nhimg != dim(out_var)[1]) stop("The dimension of out_var does not match the dimension of himg")
  if (sum(duplicated(out_var$himgid)) > 0) stop("There are duplicated himgids in out_var")
  if (verbose > 1) cat("Finished mrg_varestim")
  out_var$rse
}





dominanceRule = function(ifglldat, nlarge, plim, domEstat = TRUE) {
  #' @importFrom dplyr summarise
  Y <- sum(ifglldat$gridvar*ifglldat$weight)
  ifglldat <- ifglldat[order(ifglldat$gridvar, ifglldat$weight, decreasing = TRUE),]
  if (domEstat) {
    # Extrapolated aggregated value of the cell Y
    # Need to loop to account for different nlarge values, even though nlarge=2 is the standard rule
    dominance = FALSE
    nlarge = min(dim(ifglldat)[1], nlarge)
    for (nc in 1:nlarge){
      #' @importFrom magrittr "%>%"
      #' @importFrom dplyr slice select pull arrange ungroup
      #' @importFrom rlang .data
      # wmax= nc largest contributor
      #      wmax<-ifglldat %>% slice(1:nc) %>% select(.data$weight) %>%  pull()
      wmax<-ifglldat$weight[1:nc]
      # This should only round the weights that are above 0.5, to avoid creation of zero weights
      # This should avoid any issues due to low weights, such as
      # wmax = c(0.1, 0.4, 0.7, 1.2, 1.4)
      wmaxr<- ifelse(wmax > 0.5, round(wmax), wmax)
      #      wmaxr<-wmax %>% ifelse(.data > 0.5, round(.data), .data)  #JON: Separated wmax from wmaxr, as only the second should be rounded
      # xmax nc largest values of the variable
      #      xmax<-ifglldat %>% slice(1:nc) %>% select(.data$gridvar) %>% pull()
      xmax<-ifglldat$gridvar[1:nc] 
      # sum of n largest contributor is <= nlarge and aggregated extrapolated value of n largest contributor is
      # greater than 85% of the extrapolated aggregated value of that cell (Y)
      if(sum(wmaxr)<=nlarge*1.01 && sum(wmax*xmax)>plim*Y){
        # No need to continue the loop if dominance is already TRUE 
        #       print(paste(ifglldat$ehimgid[1], Y, ifglldat$gridvar[1]))
        return(dominance = TRUE)
      } # JON: Not including "else dominance = FALSE" here, it is not necessary. 
      #      Also, there might be a case where dominance = TRUE for nc = 1, but not for nc = 2
    }
  } else {
    
    weight = ifglldat$weight
    dat = ifglldat$gridvar
    wmax = 0
    xmax = 0
    for (ii in 1:length(dat)) {
      lw = min(weight[ii], nlarge-wmax)
      xmax = xmax + dat[ii]*lw
      wmax = wmax + lw
      if (wmax > 0.999*nlarge) break # just to avoid potential numerical issues (FAQ 7.31) from sumWeight >= nlarge
    }
    if(sum(wmax) * 0.999 <=nlarge && xmax > plim*Y$total){
      dominance = TRUE
    } else dominance = FALSE
  }
  dominance
}
