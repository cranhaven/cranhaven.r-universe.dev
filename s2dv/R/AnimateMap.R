#'Animate Maps of Forecast/Observed Values or Scores Over Forecast Time
#'
#'Create animations of maps in an equi-rectangular or stereographic 
#'projection, showing the anomalies, the climatologies, the mean InterQuartile 
#'Range, Maximum-Mininum, Standard Deviation, Median Absolute Deviation, 
#'the trends, the RMSE, the correlation or the RMSSS, between modelled and 
#'observed data along the forecast time (lead-time) for all input experiments 
#'and input observational datasets.
#'
#'@param var Matrix of dimensions (nltime, nlat, nlon) or 
#'  (nexp/nmod, nltime, nlat, nlon) or (nexp/nmod, 3/4, nltime, nlat, nlon) or 
#'  (nexp/nmod, nobs, 3/4, nltime, nlat, nlon).
#'@param lon Vector containing longtitudes (degrees).
#'@param lat Vector containing latitudes (degrees).
#'@param toptitle c('','', \dots) array of main title for each animation, 
#'  optional. If RMS, RMSSS, correlations: first exp with successive obs, then 
#'  second exp with successive obs, etc ...
#'@param sizetit Multiplicative factor to increase title size, optional.
#'@param units Units, optional.
#'@param monini Starting month between 1 and 12. Default = 1.
#'@param freq 1 = yearly, 12 = monthly, 4 = seasonal ...
#'@param msk95lev TRUE/FALSE grid points with dots if 95\% significance level 
#'  reached. Default = FALSE.
#'@param brks Limits of colour levels, optional. For example: 
#'  seq(min(var), max(var), (max(var) - min(var)) / 10).
#'@param cols Vector of colours of length(brks) - 1, optional.
#'@param filled.continents Continents filled in grey (TRUE) or represented by 
#'  a black line (FALSE). Default = TRUE. Filling unavailable if crossing 
#'  Greenwich and equi = TRUE. Filling unavailable if square = FALSE and 
#'  equi = TRUE.
#'@param lonmin Westward limit of the domain to plot (> 0 or < 0). 
#'  Default : 0 degrees.
#'@param lonmax Eastward limit of the domain to plot (> 0 or < 0). 
#'  lonmax > lonmin. Default : 360 degrees.
#'@param latmin Southward limit of the domain to plot. Default : -90 degrees.
#'@param latmax Northward limit of the domain to plot. Default : 90 degrees.
#'@param intlat Interval between latitude ticks on y-axis for equi = TRUE or 
#'  between latitude circles for equi = FALSE. Default = 30 degrees.
#'@param intlon Interval between longitude ticks on x-axis. 
#'  Default = 20 degrees.
#'@param drawleg Draw a colorbar. Can be FALSE only if square = FALSE or 
#'  equi = FALSE. Default = TRUE.
#'@param subsampleg Supsampling factor of the interval between ticks on 
#'  colorbar. Default = 1 = every colour level.
#'@param colNA Color used to represent NA. Default = 'white'.
#'@param equi TRUE/FALSE == cylindrical equidistant/stereographic projection. 
#'  Default: TRUE.
#'@param fileout c('', '', \dots) array of output file name for each animation.
#'   If RMS, RMSSS, correlations : first exp with successive obs, then second 
#'  exp with successive obs, etc ...
#'@param ... Arguments to be passed to the method. Only accepts the following 
#'  graphical parameters:\cr
#'  adj ann ask bty cex cex.axis cex.lab cex.main cex.sub
#'   cin col.axis col.lab col.main col.sub cra crt csi cxy err family fg fig 
#'  font font.axis font.lab font.main font.sub las lheight ljoin lmitre lty 
#'  lwd mai mar mex mfcol mfrow mfg mgp mkh oma omd omi page pch plt pty smo 
#'  srt tck tcl usr xaxp xaxs xaxt xlog xpd yaxp yaxs yaxt ylbias ylog. \cr
#'  For more information about the parameters see `par`.
#'
#'@details
#'Examples of input:
#'\enumerate{
#'  \item{
#'  Outputs from clim (exp, obs, memb = FALSE):
#'         (nmod, nltime, nlat, nlon)
#'      or (nobs, nltime, nlat, nlon) 
#'  }
#'  \item{
#'  Model output from load/ano/smoothing:
#'           (nmod, nmemb, sdate, nltime, nlat, nlon)
#'  then passed through spread(var, posdim = 2, narm = TRUE)
#'                    & mean1dim(var, posdim = 3, narm = TRUE)
#'  or through trend(mean1dim(var, 2), posTR = 2):
#'           (nmod, 3, nltime, nlat, nlon)
#'  animates average along start dates of IQR/MaxMin/SD/MAD across members 
#'  or trends of the ensemble-mean computed accross the start dates.
#'  }
#'  \item{
#'  model and observed output from load/ano/smoothing:
#'           (nmod, nmemb, sdate, nltime, nlat, nlon)
#'         & (nobs, nmemb, sdate, nltime, nlat, nlon)
#'  then averaged along members mean1dim(var_exp/var_obs, posdim = 2):
#'           (nmod, sdate, nltime, nlat, nlon)
#'           (nobs, sdate, nltime, nlat, nlon)
#'  then passed through corr(exp, obs, posloop = 1, poscor = 2)
#'                   or RMS(exp, obs, posloop = 1, posRMS = 2):
#'           (nmod, nobs, 3, nltime, nlat, nlon)
#'  animates correlations or RMS between each exp & each obs against leadtime.
#'  }
#'}
#'
#'@examples
#'# See ?Load for explanations on the first part of this example
#'  \dontrun{
#'data_path <- system.file('sample_data', package = 's2dv')
#'expA <- list(name = 'experiment', path = file.path(data_path,
#'             'model/$EXP_NAME$/$STORE_FREQ$_mean/$VAR_NAME$_3hourly',
#'             '$VAR_NAME$_$START_DATE$.nc'))
#'obsX <- list(name = 'observation', path = file.path(data_path,
#'             '$OBS_NAME$/$STORE_FREQ$_mean/$VAR_NAME$',
#'             '$VAR_NAME$_$YEAR$$MONTH$.nc'))
#'
#'# Now we are ready to use Load().
#'startDates <- c('19851101', '19901101', '19951101', '20001101', '20051101')
#'sampleData <- Load('tos', list(expA), list(obsX), startDates,
#'                   output = 'lonlat', latmin = 27, latmax = 48, 
#'                   lonmin = -12, lonmax = 40)
#'  }
#'  \dontshow{
#'startDates <- c('19851101', '19901101', '19951101', '20001101', '20051101')
#'sampleData <- s2dv:::.LoadSampleData('tos', c('experiment'),
#'                                     c('observation'), startDates,
#'                                       output = 'lonlat',
#'                                       latmin = 27, latmax = 48,
#'                                       lonmin = -12, lonmax = 40)
#'  }
#'clim <- Clim(sampleData$mod, sampleData$obs, memb = FALSE)
#'  \dontrun{
#'AnimateMap(clim$clim_exp, sampleData$lon, sampleData$lat,
#'  toptitle = "climatology of decadal prediction", sizetit = 1, 
#'  units = "degree", brks = seq(270, 300, 3), monini = 11, freq = 12, 
#'  msk95lev = FALSE, filled.continents = TRUE, intlon = 10, intlat = 10,
#'  fileout = 'clim_dec.gif')
#'  }
#' # More examples in s2dverification but are deleted for now
#'
#'@importFrom grDevices postscript dev.off
#'@export
AnimateMap <- function(var, lon, lat, toptitle = rep("", 11), sizetit = 1, 
                       units = "", monini = 1, freq = 12, msk95lev = FALSE, 
                       brks = NULL, cols = NULL, filled.continents = FALSE, 
                       lonmin = 0, lonmax = 360, latmin = -90, latmax = 90, 
                       intlon = 20, intlat = 30, drawleg = TRUE, 
                       subsampleg = 1, colNA = "white", equi = TRUE, 
                       fileout = c("output1_animvsltime.gif", 
                                   "output2_animvsltime.gif", 
                                   "output3_animvsltime.gif"), ...) {
  # Process the user graphical parameters that may be passed in the call
  ## Graphical parameters to exclude
  excludedArgs <- c("bg", "col", "fin", "lab", "lend", "new", "pin", "ps")
  userArgs <- .FilterUserGraphicArgs(excludedArgs, ...)

  ## fileout content with extension for consistency between
  ## functions keeping only filename without extension
  ext <- regmatches(fileout, regexpr("\\.[a-zA-Z0-9]*$", fileout))
  if ((length(ext) != 0) && any(ext != ".gif")) {
    .warning("some or all extensions of the filenames provided in 'fileout' are not 'gif'. The extensions are being converted to 'gif'.")
  }
  fileout <- sub("\\.[a-zA-Z0-9]*$", "", fileout)

  #

  # Check var
  if (!is.numeric(var) || !is.array(var)) {
    stop("Parameter 'var' must be a numeric array.")
  }
  if (length(dim(var)) < 3 || length(dim(var)) > 6) {
    stop("Parameter 'var' must be an array with 3 to 6 dimensions.")
  }
  if (length(dim(var)) == 3) {
    var <- InsertDim(var, posdim = 1, lendim = 1)
  }
  if (length(dim(var)) == 4) {
    var <- InsertDim(var, posdim = 2, lendim = 3)
  }
  if (length(dim(var)) == 5) {
    var <- InsertDim(var, posdim = 2, lendim = 1)
  }

  nleadtime <- dim(var)[4]
  nexp <- dim(var)[1]
  nobs <- dim(var)[2]
  nlat <- dim(var)[5]
  nlon <- dim(var)[6]
  if (length(lon) != nlon | length(lat) != nlat) {
    stop("Inconsistent var dimensions / longitudes +  latitudes")
  }
  colorbar <- clim.palette()
  if (is.null(brks) == TRUE) {
    ll <- signif(min(var[, , 2, , , ], na.rm = TRUE), 4)
    ul <- signif(max(var[, , 2, , , ], na.rm = TRUE), 4)
    if (is.null(cols) == TRUE) {
      cols <- colorbar(10)
    }
    nlev <- length(cols)
    brks <- signif(seq(ll, ul, (ul - ll)/nlev), 4)
  } else {
    if (is.null(cols) == TRUE) {
      nlev <- length(brks) - 1
      cols <- colorbar(nlev)
    } else {
      nlev <- length(cols)
    }
  }
  lon[which(lon < lonmin)] <- lon[which(lon < lonmin)] + 360
  lon[which(lon > lonmax)] <- lon[which(lon > lonmax)] - 360
  latb <- sort(lat[which(lat >= latmin & lat <= latmax)], index.return = TRUE)
  lonb <- sort(lon[which(lon >= lonmin & lon <= lonmax)], index.return = TRUE)
  
  # Define some plot parameters ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  labind <- 1:nleadtime
  months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", 
    "Aug", "Sep", "Oct", "Nov", "Dec")
  years <- ((labind - 1) * 12/freq + monini - 1)%/%12
  suffixtit <- months[((labind - 1) * 12/freq + monini - 1)%%12 + 
    1]
  for (jx in 1:nleadtime) {
    y2o3dig <- paste("0", as.character(years[jx]), sep = "")
    suffixtit[jx] <- paste(suffixtit[jx], "-", substr(y2o3dig, 
      nchar(y2o3dig) - 1, nchar(y2o3dig)), sep = "")
  }
  
  # Loop on experimental & observational data
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for (jexp in 1:nexp) {
    for (jobs in 1:nobs) {
      postscript(paste(fileout[(jexp - 1) * nobs + jobs], 
        ".png", sep = ""), width = 550, height = 300, 
        bg = "white")
      # Load the user parameters
      par(userArgs)
      for (jt in 1:nleadtime) {
        title <- paste(toptitle[(jexp - 1) * nobs + jobs], 
          " Time=", suffixtit[jt], sep = "")
        varbis <- var[jexp, jobs, 2, jt, which(lat >= 
          latmin & lat <= latmax), which(lon >= lonmin & 
          lon <= lonmax)]
        varbis <- varbis[latb$ix, lonb$ix]
        flag <- array(FALSE, dim(varbis))
        if (msk95lev) {
          flag[which(var[jexp, jobs, 1, jt, latb$ix, 
          lonb$ix] > 0 & var[jexp, jobs, 3, jt, latb$ix, 
          lonb$ix] > 0)] <- TRUE
          flag[which(var[jexp, jobs, 1, jt, latb$ix, 
          lonb$ix] < 0 & var[jexp, jobs, 3, jt, latb$ix, 
          lonb$ix] < 0)] <- TRUE
        }
        varbis[which(varbis <= min(brks))] <- min(brks) + 
          (max(brks) - min(brks))/1000
        varbis[which(varbis >= max(brks))] <- max(brks) - 
          (max(brks) - min(brks))/1000
        if (equi) {
          PlotEquiMap(t(varbis), lonb$x, latb$x, toptitle = title, 
          sizetit = sizetit, units = units, filled.continents = filled.continents, 
          dots = t(flag), brks = brks, cols = cols, 
          intxlon = intlon, intylat = intlat, drawleg = drawleg, 
          subsampleg = subsampleg, colNA = colNA, ...)
        } else {
          PlotStereoMap(t(varbis), lonb$x, latb$x, latlims = c(latmin, 
          latmax), toptitle = title, sizetit = sizetit, 
          units = units, filled.continents = filled.continents, 
          dots = t(flag), brks = brks, cols = cols, 
          intlat = intlat, drawleg = drawleg, subsampleg = subsampleg, 
          colNA = colNA, ...)
        }
      }
      dev.off()
      system(paste("convert -rotate 90 -loop 10 -delay 50 ", 
        fileout[(jexp - 1) * nobs + jobs], ".png ", fileout[(jexp - 
          1) * nobs + jobs], ".gif", sep = ""))
      file.remove(paste0(fileout[(jexp - 1) * nobs + jobs], 
        ".png"))
    }
  }
} 
