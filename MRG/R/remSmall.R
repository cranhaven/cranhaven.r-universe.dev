#' Function that will move values from grid cells with small values to the ones with larger values
#' for disclosure control reasons
#'
#' Two main confidentiality rules are considered:
#' - Threshold rule (suppression due to a minimum number of counts)
#' - Dominance rule (suppression due to dominance by one or more units)
#'
#' @eval MRGparam("gdl")
#' @eval MRGparam("ress")
#' @param ires0 Which resolution level to use as base for the downscaling
#' @eval MRGparam("mincount")
#' @eval MRGparam("ifg")
#' @param var Variable of interest that should be aggregated (necessary when ifg is
#'         used for individual farm specific confidence rules)
#' @param weight Extrapolation factor (weight) wi of unit i in the sample of units nc falling into
#' a specific cell c. Weights are used for disclosure control measures.
#' @eval MRGparam("nlarge")
#' @eval MRGparam("plim")
#' @param sampleRandom Logical; if the value is TRUE, values from grid cells with values under the limit will be
#'         moved to a random neighbour if there are more neighbours above the limit. False will always
#'         pick the largest (and the first one in the list if they are equal)
#' @eval MRGparam("domEstat")
#' @eval MRGparam("verbose")
#' @eval MRGparam("nclus")
#' @eval MRGparam("clusType")
#' @eval MRGparam("outfile")
#' @eval MRGparam("checkDominance")
#' @eval MRGparam("checkReliability")
#'
#' @details This function uses the hierarchy of gridded data to associate values
#'        from grid cells that need to be anonymized to the grid cell with the highest values,
#'        within increasingly larger sub-grids.
#'
#'        The parameters nlarge and plim are used for setting value dependent confidentiality rules.
#'        If the rule is that the largest two holdings in a grid cell should not count for more than 85%
#'        of the total value (UAA, number of livestock, ...), then nlarge = 2 and plim = 0.85
#'
#'        The function will create set the value to NA for the grid cells where the 
#'        content has been moved to a neighbouring grid cells.
#'
#' @returns A gridded data set, where each grid cell respects the confidentiality rules.
#'
#' @examples
#' \donttest{
#' library(sf)
#' library(sf)
#' if (!require(ggplot2)) print("Plotting of results will not work without installation of ggplot2")
#' if (!require(viridis)) print("Some of the plots will not work without installation of ggplot2")
#' if (!require(patchwork)) print("Some of the plots will not work without installation of patchwork")
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
#' # Set the base resolutions, and create a hierarchical list with gridded data
#' ress = c(1,5,10,20,40,80, 160, 320, 640, 1280, 2560)*1000
#' # Create the grid with UAA as variable and EXT_CORE as weight
#' # These can be dropped if only the number of farms are of interest in the analyses
#' ifl = gridData(ifg, "UAA", weight = "EXT_CORE", res = ress)
#'
#' # Run the procedure for the third resolution level (10 km), only using number of holdings
#' # as confidentiality rule
#' # himg1 and himg2 should give the same result, but only when sampleRandom = FALSE
#' himg1 <- remSmall(ifl, ress, 3, sampleRandom = FALSE)
#' plot(himg1[, "count"])
#' himg12 <- remSmall(ifl, ress, 3, sampleRandom = FALSE, nclus = 2)
#' # Run the procedure for UAA, using the defaults for variable
#' # confidentiality rule (nlarge = 2 and plim = 0.85)
#' 
#' himg2 <- remSmall(ifl, ress, weight = "EXT_CORE", ires0 = 3, var = "UAA", ifg = ifg)
#' plot(himg2[, "count"])
#' plot(himg2[, "UAA"])
#' 
#' # Run the procedure for organic UAA, but still requiring 10 holdings of any kind per grid cell
#' # Using resolution level 5 (40 km)
#'  iflOuaaAll = gridData(ifg, "UAAXK0000_ORG", res = ress)
#' himg3 = remSmall(iflOuaaAll, ress, 5, ifg = ifg, var = "UAAXK0000_ORG")
#' plot(himg3[, "count"])
#' plot(himg3[, "UAAXK0000_ORG"])
#'
#' # Run the procedure for organic UAA, but require at least 10 organic holdings per grid cell
#' # Using resolution level 5 (40 km)
#' ifgOuaa = ifg[ifg$UAAXK0000_ORG > 0, ]
#' iflOuaa = list()
#' iflOuaa = gridData(ifgOuaa, "UAAXK0000_ORG",  res = ress)
#' himg4 = remSmall(iflOuaa, ress, 5, ifg = ifg, var = "UAAXK0000_ORG")
#' plot(himg4[, "count"])
#' plot(himg4[, "UAAXK0000_ORG"])
#' 
#' himg4l = list()
#' # Run the proceduure for organic UAA for different resolution levels
#' for (ipl in 1:6) himg4l[[ipl]] = remSmall(iflOuaa, ress, ipl, ifg = ifg, var = "UAAXK0000_ORG")
#' 
#' 
#'  # Create proper plots
#'   breaks = c(1,3,10,30,100)
#'   labels = breaks
#'   p1 = ggplot() + geom_sf(data = himg1, aes(fill = count, color = count)) +
#'   scale_fill_viridis( name = "number of \nholdings", trans = "log10", 
#'                   breaks = breaks, labels = labels, limits = c(1,100)) +
#'   scale_color_viridis( name = "number of \nholdings", trans = "log10", 
#'                  breaks = breaks, labels = labels, limits = c(1,100)) +
#'   geom_sf(data = dkb, fill = NA, colour='black', lwd = 1) +
#'   coord_sf(crs = 3035) +#, xlim = c(2377294, 6400000), ylim = c(1313597, 5628510)) +
#'   ggtitle("Number of holdings after swapping") +
#'   theme_bw()
#'
#' # For comparison the number of organic farms and organic UAA, without taking any
#' # confidentiality into account
#' gcompOfarms = ggplot() + geom_sf(data = ifl[[3]], aes(fill = count, color = count)) +
#'  scale_fill_viridis( name = "number of \nholdings", trans = "log10", 
#'                          breaks = breaks, labels = labels, limits = c(1,100)) +
#'  scale_color_viridis( name = "number of \nholdings", trans = "log10", 
#'                          breaks = breaks, labels = labels, limits = c(1,100)) +
#'  geom_sf(data = dkb, fill = NA, colour='black', lwd = 1) +
#'  coord_sf(crs = 3035) +
#'  ggtitle("Number of holdings - ordinary gridded data") +
#'  theme_bw()
#'
#'   gcompOfarms + p1 + plot_layout(guides = "collect") 
#'
#'   p2 = ggplot() + geom_sf(data = himg2, aes(fill = count, color = count)) +
#'   scale_fill_viridis( name = "number of \nholdings", trans = "log10") +
#'   scale_color_viridis( name = "number of \nholdings", trans = "log10") +
#'   geom_sf(data = dkb, fill = NA, colour='black', lwd = 1) +
#'   coord_sf(crs = 3035) +#, xlim = c(2377294, 6400000), ylim = c(1313597, 5628510)) +
#'   ggtitle("Number of farms - corrected for farm size") +
#'   theme_bw()
#'
#'   p3 = ggplot() + geom_sf(data = himg2, aes(fill = UAA, color = UAA)) +
#'   scale_fill_viridis( name = "UAA", trans = "log10") +
#'   scale_color_viridis( name = "UAA", trans = "log10") +
#'   geom_sf(data = dkb, fill = NA, colour='black', lwd = 1) +
#'   coord_sf(crs = 3035) +#, xlim = c(2377294, 6400000), ylim = c(1313597, 5628510)) +
#'   ggtitle("UAA - corrected for farm size") +
#'   theme_bw()
#'
#'   p4 = ggplot() + geom_sf(data = himg3, aes(fill = count, color = count)) +
#'   scale_fill_viridis( name = "number of \nholdings", trans = "log10") +
#'   scale_color_viridis( name = "number of \nholdings", trans = "log10") +
#'   geom_sf(data = dkb, fill = NA, colour='black', lwd = 1) +
#'   coord_sf(crs = 3035) +#, xlim = c(2377294, 6400000), ylim = c(1313597, 5628510)) +
#'   ggtitle("Number of farms - based on number of organic farms and organic farm size") +
#'   theme_bw()
#'
#'   p5 = ggplot() + geom_sf(data = himg3, aes(fill = UAAXK0000_ORG, color = UAAXK0000_ORG)) +
#'   scale_fill_viridis( name = "UAA organic", trans = "log10") +
#'   scale_color_viridis( name = "UAA organic", trans = "log10") +
#'   geom_sf(data = dkb, fill = NA, colour='black', lwd = 1) +
#'   coord_sf(crs = 3035) +#, xlim = c(2377294, 6400000), ylim = c(1313597, 5628510)) +
#'   ggtitle("UAA organic - based on organic farm numbers and size") +
#'   theme_bw()
#'
#'   p6 = ggplot() + geom_sf(data = himg4, aes(fill = count, color = count)) +
#'   scale_fill_viridis( name = "number of \nholdings", trans = "log10") +
#'   scale_color_viridis( name = "number of \nholdings", trans = "log10") +
#'   geom_sf(data = dkb, fill = NA, colour='black', lwd = 1) +
#'   coord_sf(crs = 3035) +#, xlim = c(2377294, 6400000), ylim = c(1313597, 5628510)) +
#'   ggtitle("Number of organic farms - based on organic farm numbers and size") +
#'   theme_bw()
#'
#'   uaalims = c(min(c(himg4$UAAXK0000_ORG, iflOuaa[[5]]$UAAXK0000_ORG), na.rm = TRUE),
#'               max(c(himg4$UAAXK0000_ORG, iflOuaa[[5]]$UAAXK0000_ORG), na.rm = TRUE))
#'   p7 = ggplot() + geom_sf(data = himg4, aes(fill = UAAXK0000_ORG, color = UAAXK0000_ORG)) +
#'   scale_fill_viridis( name = "UAA organic", trans = "log10", limits = uaalims) +
#'   scale_color_viridis( name = "UAA organic", trans = "log10", limits = uaalims) +
#'   geom_sf(data = dkb, fill = NA, colour='black', lwd = 1) +
#'   coord_sf(crs = 3035) +#, xlim = c(2377294, 6400000), ylim = c(1313597, 5628510)) +
#'   ggtitle("UAA organic after swapping  ") +
#'   theme_bw()
#'
#' # For comparison the number of organic farms and organic UAA, without taking any
#' # confidentiality into account
#'
#' gcompOUAA = ggplot() + geom_sf(data = iflOuaa[[5]], 
#'                    aes(fill = UAAXK0000_ORG, color = UAAXK0000_ORG)) +
#'  scale_fill_viridis( name = "UAA organic", trans = "log10", limits = uaalims) +
#'  scale_color_viridis( name = "UAA organic", trans = "log10", limits = uaalims) +
#'  geom_sf(data = dkb, fill = NA, colour='black', lwd = 1) +
#'  coord_sf(crs = 3035) +
#'  ggtitle("Organic UAA - ordinary gridded data") +
#'  theme_bw()
#'
#'   print(gcompOUAA) + p7 +  plot_layout(guides = "collect") 
#'   
#'   ppl = list()
#'   counts = do.call("rbind", himg4l[1:5])$count
#'   clim = c(min(counts, na.rm = TRUE), max(counts, na.rm = TRUE)) 
#'   for (ipl in 1:length(himg4l)) {
#'     ppl[[ipl]] = ggplot() + geom_sf(data = himg4l[[ipl]], aes(fill = count, color = count)) +
#'   scale_fill_viridis( name = "number of \nholdings", trans = "log10", limits = clim) +
#'   scale_color_viridis( name = "number of \nholdings", trans = "log10", limits = clim) +
#'   geom_sf(data = dkb, fill = NA, colour='black', lwd = 1) +
#'   coord_sf(crs = 3035) +#, xlim = c(2377294, 6400000), ylim = c(1313597, 5628510)) +
#'   ggtitle(paste("Base resolution", ress[ipl]/1000, "km")) +
#'   theme_bw()
#'   }
#'   ppl[[1]] + ppl[[2]] + ppl[[3]] + ppl[[4]] + plot_layout(guides = "collect")
#'   
#' MRGcluster(action = "stop")
#'}
#' @export
remSmall = function(gdl, ress, ires0, mincount = 10, ifg, var, weight, nlarge = 2, plim = 0.85,
                        sampleRandom = TRUE, domEstat = TRUE, 
                    verbose = FALSE, nclus = 1, clusType, outfile = NULL,
                    checkDominance = TRUE, checkReliability = TRUE) {

  if (!missing(var) && !is.null(var)) {
    if (missing(ifg)) stop(paste("Cannot create values for variable ", var, " without ifg"))
    ifg$gridvar = data.frame(ifg)[,var]
    if (missing(weight) || is.null(weight)) {
      ifg$weight = 1
    } else  {
      ifg$weight = data.frame(ifg)[, weight]
    }
  }
  # start/reuse cluster that can be used multiple times
  if (nclus > 1) cl = MRGcluster(nclus = nclus, clusType = clusType, outfile = outfile)
  himg = gdl[[ires0]]
  #  if (!missing(weight) & !missing(var))  himg[,var] = data.frame(himg)[,var]*data.frame(himg)[, weight]
  himg$gcount = 1
  if (!missing(ifg)) {
    if (!"ID" %in% names(ifg)) ifg$ID = 1:dim(ifg)[1]
    ifg = ifg[,c("ID", "gridvar", "weight")]
    ifg$himgid = st_join(ifg, himg, join = st_within)$ID.y
    ifg = data.frame(ifg)[, c("himgid", "gridvar", "weight")]
  }
  for (ires in (ires0+1):length(ress)) {
    #ires=4
    lres = ress[ires]
    limg = gdl[[ires]]
    #   The gridded data has already been multiplied with weights
    #    if (!missing(weight) & !missing(var))  limg[,var] = data.frame(limg)[,var]*data.frame(limg)[, weight]
    loh = st_join(himg, limg, join = st_within)
    IDlimgs = unique(limg$ID)


    if (nclus == 1) {
      applyRes = lapply( 1:length(IDlimgs), FUN = remSmallPixel, IDlimgs, loh, himg,
                         ifg, var, plim, mincount, sampleRandom, nlarge, domEstat, verbose, 
                         checkDominance = checkDominance, checkReliability = checkReliability)
    } else {
      if (nclus > 1) {
        if (missing(ifg)) ifg = NULL
        if (missing(var)) var = NULL
        if (missing(weight)) weight = NULL
        clusterEvalQ(cl, c(library(magrittr), library(dplyr), library(stats), library(rlang),library(sf)))
        clusterExport(cl, c("IDlimgs", "loh", "himg", "ifg", "var",
                                      "mincount", "sampleRandom", "verbose"), envir = environment())
        applyRes = parLapply(cl, 1:length(IDlimgs), fun = remSmallPixel,
                                            IDlimgs, loh, himg, ifg, var, plim, mincount, sampleRandom, 
                                            nlarge, domEstat, verbose, checkDominance = checkDominance, checkReliability = checkReliability)
      }
    }

    for (iID in 1:length(IDlimgs)) {
      ares = applyRes[[iID]]
      if (length(ares) > 1) {
        hlocs = ares$hlocs
        minds = ares$minds
        linds = ares$linds
        hIDs = ares$hIDs

        himg$weight[hlocs[minds]] = sum(himg$weight[hlocs[minds]], himg$weight[hlocs[linds]], na.rm = TRUE)
        himg$weight[hlocs[linds]] = NA
        himg$count[hlocs[minds]] = sum(himg$count[hlocs[minds]], himg$count[hlocs[linds]], na.rm = TRUE)
        himg$count[hlocs[linds]] = NA
        if (!missing(var)) {
          himg[hlocs[minds], var] = sum(data.frame(himg)[hlocs[minds], var], data.frame(himg)[hlocs[linds], var], na.rm = TRUE)
          himg[hlocs[linds], var] = NA
          if (!missing(ifg) && !is.null(ifg)) {
            ifg$himgid[ifg$himgid %in% hIDs[linds]] = hIDs[minds]
          }
        }
        himg$gcount[hlocs[minds]] = himg$gcount[hlocs[minds]] + length(linds)
      }
    }
    print(paste(ires, sum(himg$weight, na.rm = TRUE)))
  }
  if ("gridvar" %in% names(himg)) himg = himg[,-grep("gridvar", names(himg))]
  if (!missing(ifg) && !is.null(ifg)) attr(himg, "ifg") = ifg
  attr(himg, "applyRes") = applyRes
  himg
}




#   Function to be called from apply or clusterApply
remSmallPixel = function(iID, IDlimgs, loh, himg, ifg, var, plim, mincount, sampleRandom, 
                         nlarge, domEstat, verbose, checkDominance, checkReliability) {
  #iID=4
  ID = IDlimgs[iID]
  #' @importFrom sf st_drop_geometry
  himgdat = st_drop_geometry(himg)
  hIDs = loh$ID.x[which(loh$ID.y == ID)] # Which himg-IDs are in the limg-pixel
  hlocs = which(himg$ID %in% hIDs)
  hIDs = hIDs[!is.na(himg$weight[hlocs])]  # Remove himg-IDs and hlocs that have already been set to NA
  hlocs = hlocs[!is.na(himg$weight[hlocs])]
  hcs = round(himg$weight[hlocs]) # need to take the rounded weights instead of simple counts
  # hcs is a vector with 0-4 values, saying how many
  #     holdings there are in each subpixel.
  # length(hcs) == 1 means there is only sub pixel, hence not possible to merge this
  #     with any of the sub pixels
  if (length(hcs) > 1 & sum(hcs, na.rm = TRUE) > 0) {
    linds = which(round(himg$weight[hlocs]) < mincount)
    if (!missing(ifg) && !is.null(ifg)  && !missing(var) && !is.null(var)) {
      #        if (verbose) print(paste("Frequency rule:\n ID=",iID,
      #                                 "RESS=",ress[ires],sep=" ")) 	#NL included the line to replicate cases
      ihids = 1:length(hIDs)
      if (length(linds) > 0) ihids = ihids[-linds] # will only check the ones which passed the frequency rule
      for (ihid in ihids) {
        #ihid=2
        hID = hIDs[ihid]
        # Need to be sorted (descending order) by weight and value
        #' @importFrom sf st_drop_geometry
        #' @importFrom dplyr select arrange filter
        #' @importFrom magrittr %>%
        #' @importFrom stats na.omit
        ifglldat=st_drop_geometry(ifg) %>% filter(.data$himgid == hID) %>%
          na.omit() %>%  select(.data$gridvar, .data$weight) %>%
          arrange(-.data$gridvar, -.data$weight)
        
        if (dominanceRule(ifglldat, nlarge, plim, domEstat)) linds = c(linds, ihid)
      }
      linds = unique(linds)
    }
    if (length(linds) == 0) return(NA)
    minds = which(himg$weight[hlocs] > mincount)
    if (length(linds) == 1 && length(minds) == 1) return(NA)
    if (length(minds) == 0) minds = which.max(himg$weight[hlocs])
    if (length(minds) > 1) {
      rmind = NULL
      for (mind in minds) if (mind %in% linds) rmind = c(rmind, mind)
      if (length(rmind) > 0 & length(rmind) < length(minds)) minds = minds[-which(minds %in% rmind)]
      # Dont give all farms to the largest, sample from the possible pixels
      # This does not check which of the pixels that are within the % rule
      if (length(minds) > 1) if (sampleRandom == "random") minds = sample(minds, 1) else minds = minds[which.max(minds)]
    }
    if (minds %in% linds) {
      linds = linds[-which(linds == minds)]
      if (length(linds) == 0) return(NA)
    }
    list(hlocs = hlocs, minds = minds, linds = linds, hIDs = hIDs)
  } else NA
}
