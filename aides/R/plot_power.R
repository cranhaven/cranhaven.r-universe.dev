#' @title Illustrate statistical power plot of observed sequential analysis.
#'
#' @author Enoch Kang
#'
#' @description
#' **PlotPower()** is a function for plotting power of observed sequential analysis.
#'
#' @param object      OBJECT in **DoOSA** class that is an output of observed
#'                    sequential analysis using function `DoOSA()`.
#' @param txtTtl      CHARACTER for user-defined main title on the power plot of
#'                    observed sequential analysis.
#' @param lgcPwrO     LOGIC value for indicating whether to show original observed
#'                    power without sequential adjustment.
#' @param lgcLblStdy  LOGIC value for indicating whether to label each data source.
#' @param szFntTtl    NUMERIC value for indicating font size of main title.
#' @param szFntTtlX   NUMERIC value for indicating font size of title on axis X.
#' @param szFntTtlY   NUMERIC value for indicating font size of title on axis Y.
#' @param szFntAxsX   NUMERIC value for indicating font size of scale on axis X.
#' @param szFntAxsY   NUMERIC value for indicating font size of scale on axis Y.
#' @param szFntLgnd   NUMERIC value for indicating font size of legend.
#' @param szFntStdy   NUMERIC value(s) for indicating font size(s) of the label(s)
#'                    of each data source.
#' @param szPntPwrO   NUMERIC value for indicating size of observed point(s) of
#'                    statistical power without sequential adjustment.
#' @param szPntPwrS   NUMERIC value for indicating size of observed point(s) of
#'                    statistical power after sequential adjustment.
#' @param szLnPwrCtf  NUMERIC value for indicating width of line for assumed power.
#' @param szLnPwrO    NUMERIC value for indicating width of line for observed
#'                    power without sequential adjustment.
#' @param szLnPwrP    NUMERIC value for indicating width of line for predicted or
#'                    expected power after sequential adjustment.
#' @param szLnPwrS    NUMERIC value for indicating width of line for observed
#'                    power after sequential adjustment.
#' @param typPntPwrO  NUMERIC value(s) between 1 to 5 for indicating type(s) of
#'                    observed point(s) without sequential adjustment. Symbols in
#'                    the current version includes circle, square, diamond, triangle
#'                    point-up, and triangle point down.
#' @param typPntPwrS  NUMERIC value between 1 to 5 for indicating type of point(s)
#'                    after sequential adjustment. Symbols in the current version
#'                    includes circle, square, diamond, triangle point-up, and
#'                    triangle point down.
#' @param typLnPwrCtf NUMERIC value for indicating type of assumed power.
#' @param typLnPwrO   NUMERIC value for indicating type of line for observed
#'                    power without sequential adjustment.
#' @param typLnPwrP   NUMERIC value for indicating type of line for predicted or
#'                    expected power after sequential adjustment.
#' @param typLnPwrS   NUMERIC value for indicating type of line for observed
#'                    power after sequential adjustment.
#' @param clrTtl      CHARACTER of a color name for main title.
#' @param clrTtlX     CHARACTER of a color name for title on axis X.
#' @param clrTtlY     CHARACTER of a color name for title on axis Y.
#' @param clrAxsX     CHARACTER of a color name for scale on axis X.
#' @param clrAxsY     CHARACTER of a color name for scale on axis Y.
#' @param clrLgnd     CHARACTER of a color name for legend.
#' @param clrLblStdy  CHARACTER of color name(s) for the label(s) of each data source.
#' @param clrPntPwrO  CHARACTER of color name(s) for observed point(s) of power
#'                    without sequential adjustment..
#' @param clrPntPwrS  CHARACTER of a color name for observed point(s) of power
#'                    after sequential adjustment.
#' @param clrLnPwrCtf CHARACTER of a color name for assumed power.
#' @param clrLnPwrO   CHARACTER of a color name for line of observed power without
#'                    sequential adjustment.
#' @param clrLnPwrP   CHARACTER of a color name for line of predicted or expected
#'                    power after sequential adjustment.
#' @param clrLnPwrS   CHARACTER of a color name for line of observed power after
#'                    sequential adjustment.
#' @param anglStdy    NUMERIC value between 0 and 360 for indicating angle of data
#'                    source.
#'
#'
#' @return
#' **PlotPower()** returns a plot of statistical power of observed sequential analysis.
#'
#'
#' @references
#' Harrer, M., Cuijpers, P., Furukawa, T.A., & Ebert, D.D. (2021). *Doing*
#' *Meta-Analysis with R: A Hands-On Guide*. Boca Raton, FL and London: Chapman
#' & Hall/CRC Press. ISBN 978-0-367-61007-4.
#'
#' Jennison, C., & Turnbull, B. W. (2005). Meta-analyses and adaptive group
#' sequential designs in the clinical development process.
#' **Journal of biopharmaceutical statistics**, *15(4)*, 537–558.
#' https://doi.org/10.1081/BIP-200062273.
#'
#' Wetterslev, J., Jakobsen, J. C., & Gluud, C. (2017). Trial sequential analysis
#' in systematic reviews with meta-analysis. **BMC medical research methodology**,
#' *17(1)*, 1-18.
#'
#'
#' @seealso \code{\link{DoSA}}, \code{\link{DoOSA}}, \code{\link{PlotOSA}}
#'
#' @examples
#' ## Not run:
#' # 1. Import a dataset of study by Fleiss (1993)
#' library(meta)
#' data("Fleiss1993bin")
#'
#' # 2. Perform observed sequential analysis
#'  output <- DoOSA(Fleiss1993bin, study, year,
#'                  r1 = d.asp, n1 = n.asp,
#'                  r2 = d.plac, n2 = n.plac,
#'                  measure = "RR",
#'                  group = c("Aspirin", "Control"),
#'                  plot = TRUE)
#'
#' # 3. Illustrate statistical power plot of observed sequential analysis
#'  PlotPower(output)
#'
#' ## End(Not run)
#'
#' @export PlotPower



PlotPower <- function(object      = NULL,
                      txtTtl      = NULL,
                      lgcPwrO     = FALSE,
                      lgcLblStdy  = FALSE,
                      szFntTtl    = 1.8,
                      szFntTtlX   = 1.2,
                      szFntTtlY   = 1.2,
                      szFntAxsX   = 0.8,
                      szFntAxsY   = 0.8,
                      szFntLgnd   = 0.8,
                      szFntStdy   = 0.6,
                      szPntPwrO   = 0.8,
                      szPntPwrS   = 0.8,
                      szLnPwrCtf  = 1,
                      szLnPwrO    = 1.2,
                      szLnPwrP    = 1.2,
                      szLnPwrS    = 1.2,
                      typPntPwrO  = 2,
                      typPntPwrS  = 2,
                      typLnPwrCtf = 2,
                      typLnPwrO   = 1,
                      typLnPwrP   = 2,
                      typLnPwrS   = 1,
                      clrTtl      = "black",
                      clrTtlX     = "black",
                      clrTtlY     = "black",
                      clrAxsX     = "black",
                      clrAxsY     = "black",
                      clrLgnd     = "gray25",
                      clrLblStdy  = "gray25",
                      clrPntPwrO  = "gray75",
                      clrPntPwrS  = "green4",
                      clrLnPwrCtf = "gray75",
                      clrLnPwrO   = "gray75",
                      clrLnPwrP   = c("firebrick", "blue4"),
                      clrLnPwrS   = "green4",
                      anglStdy    = 90) {

  # 01. CHECK object -----
  lgcInObject <- ifelse(is.null(object), FALSE, TRUE)
  lgcReq1     <- ifelse(lgcInObject == TRUE, TRUE, FALSE)

  lgcStop1     <- ifelse(lgcReq1 == TRUE, FALSE, TRUE)
  infoLgcStop1 <- ifelse(lgcStop1 == TRUE,
                         'Parameter "object" should be used for assigning a DoOSA-class object.',
                         "")
  # 02. RETURN results of core argument checking  -----
  # 02.1. RETURN results of none object checking

  if (lgcStop1)
    stop(infoLgcStop1)

  # 02.2. RETURN results of object class checking
  lgcObject    <- inherits(object, "DoOSA")
  #lgcObject    <- ifelse("DoOSA" %in% class(object), FALSE, TRUE)
  lgcReq2      <- ifelse(lgcObject == TRUE, TRUE, FALSE)

  lgcStop2     <- ifelse(lgcReq2 == TRUE, FALSE, TRUE)
  infoLgcStop2 <- ifelse(lgcStop2 == TRUE,
                         'Parameter "object" should be used for assigning a DoOSA-class object.',
                         "")

  if (lgcStop2)
    stop(infoLgcStop2)


  # 03. DEFINE core data -----
  objIn        <- object

  infoOIS      <- objIn$OIS
  infoNumStud  <- objIn$studies
  infoCases    <- objIn$AIS
  infoAlpha    <- objIn$alpha
  infoBeta     <- objIn$beta
  infoMeasure  <- objIn$measure
  infoModel    <- objIn$model
  infoMethod   <- objIn$method
  infoPooling  <- objIn$pooling
  infoPrefer   <- objIn$prefer
  infoOES      <- objIn$OES
  infoRRR      <- objIn$RRR
  infoOV       <- objIn$variance
  infoDivers   <- objIn$diversity
  infoAdjust   <- objIn$adjust
  infoAF       <- objIn$AF
  infoOISOrg   <- objIn$OIS.org
  infoOISAdj   <- objIn$OIS.adj
  infoGroup    <- objIn$group
  infoRef      <- objIn$ref
  infoColorASB <- objIn$color.ASB
  infoPosLabel <- objIn$position.label
  dataOSA      <- objIn$data
  dataPlotPwr  <- objIn$data.bounds
  infoPwrObs   <- objIn$power

  dataOSA      <- dataOSA[!is.na(dataOSA$source), ]
  dataPlotPwr  <- dataPlotPwr[dataPlotPwr$sample %% floor(max(dataPlotPwr$sample) / 100) == 0, ]
  dataPwr      <- dataPlotPwr[dataPlotPwr$sample > infoCases, ]

  rownames(dataPlotPwr) <- 1:nrow(dataPlotPwr)
  rownames(dataPwr)     <- 1:nrow(dataPwr)


  setPar <- par(no.readonly = TRUE)
  on.exit(par(setPar))
  infoLgcWarning <- getOption("warn")
  options(warn = -1)
  on.exit(options(warn = infoLgcWarning))


  # 04. CHECK additive arguments -----

  lgcTxtTtl     <- ifelse(is.null(txtTtl),
                          FALSE,
                          ifelse(is.character(txtTtl),
                                 FALSE, TRUE)
  )

  lgcLgcPwrO    <- ifelse(is.logical(lgcPwrO),
                          FALSE, TRUE)

  lgcLgcLblStdy <- ifelse(is.logical(lgcLblStdy),
                          FALSE, TRUE)

  lgcSzFntTtl <- ifelse(is.null(szFntTtl),
                        FALSE,
                        ifelse(base::isFALSE(length(szFntTtl) == 1),
                               TRUE,
                               ifelse(base::isFALSE(is.numeric(szFntTtl)),
                                      TRUE,
                                      ifelse(FALSE %in% (szFntTtl >= 0),
                                             TRUE,
                                             ifelse(FALSE %in% (szFntTtl < 6),
                                                    TRUE, FALSE))))
  )

  lgcSzFntTtlX <- ifelse(is.null(szFntTtlX),
                         FALSE,
                         ifelse(base::isFALSE(length(szFntTtlX) == 1),
                                TRUE,
                                ifelse(base::isFALSE(is.numeric(szFntTtlX)),
                                       TRUE,
                                       ifelse(base::isFALSE(szFntTtlX > 0),
                                              TRUE,
                                              ifelse(szFntTtlX < 6,
                                                     FALSE, TRUE))))
  )

  lgcSzFntTtlY <- ifelse(is.null(szFntTtlY),
                         FALSE,
                         ifelse(base::isFALSE(length(szFntTtlY) == 1),
                                TRUE,
                                ifelse(base::isFALSE(is.numeric(szFntTtlY)),
                                       TRUE,
                                       ifelse(base::isFALSE(szFntTtlY > 0),
                                              TRUE,
                                              ifelse(szFntTtlY < 6,
                                                     FALSE, TRUE))))
  )

  lgcSzFntAxsX <- ifelse(is.null(szFntAxsX),
                         FALSE,
                         ifelse(base::isFALSE(length(szFntAxsX) == 1),
                                TRUE,
                                ifelse(base::isFALSE(is.numeric(szFntAxsX)),
                                       TRUE,
                                       ifelse(base::isFALSE(szFntAxsX > 0),
                                              TRUE,
                                              ifelse(szFntAxsX < 6,
                                                     FALSE, TRUE))))
  )

  lgcSzFntAxsY <- ifelse(is.null(szFntAxsY),
                         FALSE,
                         ifelse(base::isFALSE(length(szFntAxsY) == 1),
                                TRUE,
                                ifelse(base::isFALSE(is.numeric(szFntAxsY)),
                                       TRUE,
                                       ifelse(base::isFALSE(szFntAxsY > 0),
                                              TRUE,
                                              ifelse(szFntAxsY < 6,
                                                     FALSE, TRUE))))
  )

  lgcSzFntLgnd <- ifelse(is.null(szFntLgnd),
                         FALSE,
                         ifelse(base::isFALSE(length(szFntLgnd) == 1),
                                TRUE,
                                ifelse(base::isFALSE(is.numeric(szFntLgnd)),
                                       TRUE,
                                       ifelse(base::isFALSE(szFntLgnd > 0),
                                              TRUE,
                                              ifelse(szFntLgnd < 6,
                                                     FALSE, TRUE))))
  )

  lgcSzFntStdy <- ifelse(is.null(szFntStdy),
                         FALSE,
                         ifelse(base::isFALSE(length(szFntStdy) == 1 | length(szFntStdy) == infoNumStud),
                                TRUE,
                                ifelse(base::isFALSE(is.numeric(szFntStdy)),
                                       TRUE,
                                       ifelse(FALSE %in% (szFntStdy >= 0),
                                              TRUE,
                                              ifelse(FALSE %in% (szFntStdy < 6),
                                                     TRUE, FALSE))))
  )

  lgcszPntPwrO <- ifelse(is.null(szPntPwrO),
                         FALSE,
                         ifelse(base::isFALSE(length(szPntPwrO) == 1 | length(szPntPwrO) == infoNumStud),
                                TRUE,
                                ifelse(base::isFALSE(is.numeric(szPntPwrO)),
                                       TRUE,
                                       ifelse(FALSE %in% (szPntPwrO >= 0),
                                              TRUE,
                                              ifelse(FALSE %in% (szPntPwrO < 6),
                                                     TRUE, FALSE))))
  )

  lgcszPntPwrS  <- ifelse(is.null(szPntPwrS),
                         FALSE,
                         ifelse(base::isFALSE(length(szPntPwrS) == 1 | length(szPntPwrS) == infoNumStud),
                                TRUE,
                                ifelse(base::isFALSE(is.numeric(szPntPwrS)),
                                       TRUE,
                                       ifelse(FALSE %in% (szPntPwrS >= 0),
                                              TRUE,
                                              ifelse(FALSE %in% (szPntPwrS < 6),
                                                     TRUE, FALSE))))
  )

  lgcszLnPwrCtf  <- ifelse(is.null(szLnPwrCtf),
                      FALSE,
                      ifelse(base::isFALSE(length(szLnPwrCtf) == 1),
                             TRUE,
                             ifelse(base::isFALSE(is.numeric(szLnPwrCtf)),
                                    TRUE,
                                    ifelse(FALSE %in% (szLnPwrCtf >= 0),
                                           TRUE,
                                           ifelse(FALSE %in% (szLnPwrCtf < 6),
                                                  TRUE, FALSE))))
  )

  lgcszLnPwrO <- ifelse(is.null(szLnPwrO),
                      FALSE,
                      ifelse(base::isFALSE(length(szLnPwrO) == 1 | length(szLnPwrO) == 2),
                             TRUE,
                             ifelse(base::isFALSE(is.numeric(szLnPwrO)),
                                    TRUE,
                                    ifelse(FALSE %in% (szLnPwrO >= 0),
                                           TRUE,
                                           ifelse(FALSE %in% (szLnPwrO < 6),
                                                  TRUE, FALSE))))
  )

  lgcszLnPwrP <- ifelse(is.null(szLnPwrP),
                      FALSE,
                      ifelse(base::isFALSE(length(szLnPwrP) == 1),
                             TRUE,
                             ifelse(base::isFALSE(is.numeric(szLnPwrP)),
                                    TRUE,
                                    ifelse(FALSE %in% (szLnPwrP >= 0),
                                           TRUE,
                                           ifelse(FALSE %in% (szLnPwrP < 6),
                                                  TRUE, FALSE))))
  )

  lgcszLnPwrS  <- ifelse(is.null(szLnPwrS),
                      FALSE,
                      ifelse(base::isFALSE(length(szLnPwrS) == 1 | length(szLnPwrO) == 2),
                             TRUE,
                             ifelse(base::isFALSE(is.numeric(szLnPwrS)),
                                    TRUE,
                                    ifelse(FALSE %in% (szLnPwrS >= 0),
                                           TRUE,
                                           ifelse(FALSE %in% (szLnPwrS < 6),
                                                  TRUE, FALSE))))
  )

  lgctypPntPwrO <- ifelse(is.null(typPntPwrO),
                          FALSE,
                          ifelse(base::isFALSE(length(typPntPwrO) == 1 | length(typPntPwrO) == infoNumStud),
                                 TRUE,
                                 ifelse(base::isFALSE(is.numeric(typPntPwrO)),
                                        TRUE,
                                        ifelse(FALSE %in% (typPntPwrO >= 0),
                                               TRUE,
                                               ifelse(FALSE %in% (typPntPwrO < 6),
                                                      TRUE, FALSE))))
  )

  lgctypPntPwrS  <- ifelse(is.null(typPntPwrS),
                          FALSE,
                          ifelse(base::isFALSE(length(typPntPwrS) == 1 | length(typPntPwrS) == infoNumStud),
                                 TRUE,
                                 ifelse(base::isFALSE(is.numeric(typPntPwrS)),
                                        TRUE,
                                        ifelse(FALSE %in% (typPntPwrS >= 0),
                                               TRUE,
                                               ifelse(FALSE %in% (typPntPwrS < 6),
                                                      TRUE, FALSE))))
  )

  lgctypLnPwrCtf     <- ifelse(is.null(typLnPwrCtf),
                          FALSE,
                          ifelse(length(typLnPwrCtf) != 1,
                                 TRUE,
                                 ifelse(is.numeric(typLnPwrCtf),
                                        ifelse(typLnPwrCtf %in% c(0:6),
                                               FALSE,
                                               TRUE),
                                        ifelse(typLnPwrCtf %in% c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),
                                               FALSE,
                                               TRUE)))
  )

  lgctypLnPwrO   <- ifelse(is.null(typLnPwrO),
                          FALSE,
                          ifelse(length(typLnPwrO) != 1,
                                 TRUE,
                                 ifelse(is.numeric(typLnPwrO),
                                        ifelse(typLnPwrO %in% c(0:6),
                                               FALSE,
                                               TRUE),
                                        ifelse(typLnPwrO %in% c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),
                                               FALSE,
                                               TRUE)))
  )

  lgctypLnPwrP  <- ifelse(is.null(typLnPwrP),
                          FALSE,
                          ifelse(length(typLnPwrP) != 1,
                                 TRUE,
                                 ifelse(is.numeric(typLnPwrP),
                                        ifelse(typLnPwrP %in% c(0:6),
                                               FALSE,
                                               TRUE),
                                        ifelse(typLnPwrP %in% c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),
                                               FALSE,
                                               TRUE)))
  )

  lgctypLnPwrS   <- ifelse(is.null(typLnPwrS),
                          FALSE,
                          ifelse(length(typLnPwrS) != 1,
                                 TRUE,
                                 ifelse(is.numeric(typLnPwrS),
                                        ifelse(typLnPwrS %in% c(0:6),
                                               FALSE,
                                               TRUE),
                                        ifelse(typLnPwrS %in% c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),
                                               FALSE,
                                               TRUE)))
  )

  lgcClrTtl     <- ifelse(is.null(clrTtl),
                          FALSE,
                          ifelse(length(clrTtl) != 1,
                                 TRUE,
                                 ifelse(FALSE %in% (clrTtl %in% colors()),
                                        TRUE, FALSE))
  )

  lgcClrTtlX    <- ifelse(is.null(clrTtlX),
                          FALSE,
                          ifelse(length(clrTtlX) != 1,
                                 TRUE,
                                 ifelse(FALSE %in% (clrTtlX %in% colors()),
                                        TRUE, FALSE))
  )

  lgcClrTtlY    <- ifelse(is.null(clrTtlY),
                          FALSE,
                          ifelse(length(clrTtlY) != 1,
                                 TRUE,
                                 ifelse(FALSE %in% (clrTtlY %in% colors()),
                                        TRUE, FALSE))
  )

  lgcClrAxsX    <- ifelse(is.null(clrAxsX),
                          FALSE,
                          ifelse(base::isFALSE(length(clrAxsX) == 1 | length(clrAxsX) == infoNumStud),
                                 TRUE,
                                 ifelse(FALSE %in% (clrAxsX %in% colors()),
                                        TRUE, FALSE))
  )

  lgcClrAxsY    <- ifelse(is.null(clrAxsY),
                          FALSE,
                          ifelse(length(clrAxsY) != 1,
                                 TRUE,
                                 ifelse(FALSE %in% (clrAxsY %in% colors()),
                                        TRUE, FALSE))
  )

  lgcClrLgnd    <- ifelse(is.null(clrLgnd),
                          FALSE,
                          ifelse(length(clrLgnd) != 1,
                                 TRUE,
                                 ifelse(FALSE %in% (clrLgnd %in% colors()),
                                        TRUE, FALSE))
  )

  lgcClrLblStdy <- ifelse(is.null(clrLblStdy),
                          FALSE,
                          ifelse(base::isFALSE(length(clrLblStdy) == 1 | length(clrLblStdy) == infoNumStud),
                                 TRUE,
                                 ifelse(FALSE %in% (clrLblStdy %in% colors()),
                                        TRUE, FALSE))
  )

  lgcclrPntPwrO <- ifelse(is.null(clrPntPwrO),
                          FALSE,
                          ifelse(base::isFALSE(length(clrPntPwrO) == 1 | length(clrPntPwrO) == infoNumStud),
                                 TRUE,
                                 ifelse(FALSE %in% (clrPntPwrO %in% colors()),
                                        TRUE, FALSE))
  )

  lgcclrPntPwrS  <- ifelse(is.null(clrPntPwrS),
                          FALSE,
                          ifelse(base::isFALSE(length(clrPntPwrS) == 1 | length(clrPntPwrS) == infoNumStud),
                                 TRUE,
                                 ifelse(base::isFALSE(clrPntPwrS %in% colors()),
                                        TRUE, FALSE))
  )

  lgcclrLnPwrCtf    <- ifelse(is.null(clrLnPwrCtf),
                         FALSE,
                         ifelse(length(clrLnPwrCtf) != 1,
                                TRUE,
                                ifelse(FALSE %in% (clrLnPwrCtf %in% colors()),
                                       TRUE, FALSE))
  )

  lgcclrLnPwrO  <- ifelse(is.null(clrLnPwrO),
                         FALSE,
                         ifelse(length(clrLnPwrO) != 1,
                                TRUE,
                                ifelse(FALSE %in% (clrLnPwrO %in% colors()),
                                       TRUE, FALSE))
  )

  lgcclrLnPwrP <- ifelse(is.null(clrLnPwrP),
                         FALSE,
                         ifelse(length(clrLnPwrP) != 2,
                                TRUE,
                                ifelse(FALSE %in% (clrLnPwrP %in% colors()),
                                       TRUE, FALSE))
  )

  lgcclrLnPwrS  <- ifelse(is.null(clrLnPwrS),
                         FALSE,
                         ifelse(length(clrLnPwrS) != 1,
                                TRUE,
                                ifelse(FALSE %in% (clrLnPwrS %in% colors()),
                                       TRUE, FALSE))
  )

  lgcAnglStdy   <- ifelse(is.null(anglStdy),
                          FALSE,
                          ifelse(base::isFALSE(length(anglStdy) == 1 | length(anglStdy) == infoNumStud),
                                 TRUE,
                                 ifelse(anglStdy < 0,
                                        TRUE,
                                        ifelse(anglStdy > 360,
                                               TRUE, FALSE)))
  )


  if (lgcTxtTtl) {
    infoStopTxtTtl      <- 'Argument for parameter `txtTtl` must be characters for main title of sequential adjusted power plot.'
  }

  if (lgcLgcPwrO) {
    infoStopLgcLgcPwrO  <- 'Argument for parameter `lgcLgcPwrO` must be a logic value for indicating whether to show origintal observed power without sequential adjustment.'
  }

  if (lgcLgcLblStdy) {
    infoStopLgcLblStdy  <- 'Argument for parameter `lgcLblStdy` must be a logic value for indicating whether to show names of observed studies on the plot.'
  }

  if (lgcSzFntTtl) {
    infoStopSzFntTtl    <- 'Argument for parameter `szFntTtl` must be a numeric value between 0 and 5 for indicating font size of main title plot.'
  }

  if (lgcSzFntTtlX) {
    infoStopSzFntTtlX   <- 'Argument for parameter `szFntTtlX` must be a numeric value between 0 and 5 for indicating font size of title on X-axis.'
  }

  if (lgcSzFntTtlY) {
    infoStopSzFntTtlY   <- 'Argument for parameter `szFntTtlY` must be a numeric value between 0 and 5 for indicating font size of title on Y-axis.'
  }

  if (lgcSzFntAxsX) {
    infoStopSzFntAxsX   <- 'Argument for parameter `szFntAxsX` must be a numeric value between 0 and 5 for indicating font size of numeric scale on X-axis.'
  }

  if (lgcSzFntAxsY) {
    infoStopSzFntAxsY   <- 'Argument for parameter `szFntAxsY` must be a numeric value between 0 and 5 for indicating font size of numeric scale on Y-axis.'
  }

  if (lgcSzFntLgnd) {
    infoStopSzFntLgnd   <- 'Argument for parameter `szFntLgnd` must be a numeric value between 0 and 5 for indicating font size of legend.'
  }

  if (lgcSzFntStdy) {
    infoStopSzFntStdy   <- 'Argument for parameter `szFntStdy` must be a numeric value between 0 and 5 for indicating font size of study labels.'
  }

  if (lgcszPntPwrO) {
    infoStopszPntPwrO   <- 'Argument for parameter `szPntPwrO` must be a numeric value between 0 and 5 for indicating point size of observed power without sequential adjustment.'
  }

  if (lgcszPntPwrS) {
    infoStopszPntPwrS   <- 'Argument for parameter `szPntPwrS` must be a numeric value between 0 and 5 for indicating point size of observed power after sequential adjustment.'
  }

  if (lgcszLnPwrCtf) {
    infoStopszLnPwrCtf  <- 'Argument for parameter `szLnPwrCtf` must be a numeric value between 0 and 5 for indicating width of assumed power.'
  }

  if (lgcszLnPwrO) {
    infoStopszLnPwrO    <- 'Argument for parameter `szLnPwrO` must be a numeric value between 0 and 5 for indicating width of line for observed power without sequential adjustment.'
  }

  if (lgcszLnPwrP) {
    infoStopszLnPwrP    <- 'Argument for parameter `szLnPwrP` must be a numeric value between 0 and 5 for indicating width of line for predicted or expected power after sequential adjustment.'
  }

  if (lgcszLnPwrS) {
    infoStopszLnPwrS    <- 'Argument for parameter `szLnPwrS` must be a numeric value between 0 and 5 for indicating width of line for observed power after sequential adjustment.'
  }

  if (lgctypPntPwrO) {
    infoStoptypPntPwrO  <- 'Argument for parameter `typPntPwrO` must be integer(s) between 1 and 5 for indicating shape(s) of observed point(s) for power without sequential adjustment.'
  }

  if (lgctypPntPwrS) {
    infoStoptypPntPwrS  <- 'Argument for parameter `typPntPwrS` must be integer(s) between 1 and 5 for indicating shape(s) of observed point(s) for power after sequential adjustment.'
  }

  if (lgctypLnPwrCtf) {
    infoStoptypLnPwrCtf <- 'Argument for parameter `typLnPwrCtf` must be an integer between 1 and 6 for indicating segment type of assumed power.'
  }

  if (lgctypLnPwrO) {
    infoStoptypLnPwrO   <- 'Argument for parameter `typLnPwrO` must be an integer between 1 and 6 for indicating segment type of line for observed power without sequential adjustment.'
  }

  if (lgctypLnPwrP) {
    infoStoptypLnPwrP   <- 'Argument for parameter `typLnPwrP` must be an integer between 1 and 6 for indicating segment type of line for predicted or expected power after sequential adjustment.'
  }

  if (lgctypLnPwrS) {
    infoStoptypLnPwrS   <- 'Argument for parameter `typLnPwrS` must be an integer between 1 and 6 for indicating segment type of line for observed power after sequential adjustment.'
  }

  if (lgcClrTtl) {
    infoStopClrTtl      <- 'Argument for parameter `clrTtl` must be a color name for coloring main title.'
  }

  if (lgcClrTtlX) {
    infoStopClrTtlX     <- 'Argument for parameter `clrTtlX` must be a color name for coloring the title on axis X.'
  }

  if (lgcClrTtlY) {
    infoStopClrTtlY     <- 'Argument for parameter `clrTtlY` must be a color name for coloring the title on axis Y.'
  }

  if (lgcClrAxsX) {
    infoStopClrAxsX     <- 'Argument for parameter `clrAxsX` must be color name(s) for coloring the axis X.'
  }

  if (lgcClrAxsY) {
    infoStopClrAxsY     <- 'Argument for parameter `clrAxsY` must be a color name for coloring the axis Y.'
  }

  if (lgcClrLgnd) {
    infoStopClrLgnd     <- 'Argument for parameter `clrLgnd` must be a color name for coloring plot legend.'
  }

  if (lgcClrLblStdy) {
    infoStopClrLblStdy  <- 'Argument for parameter `clrLblStdy` must be a color name for coloring study labels on the plot.'
  }

  if (lgcclrPntPwrO) {
    infoStopclrPntPwrO  <- 'Argument for parameter `clrPntPwrO` must be color name(s) for coloring observed point(s) of power without sequential adjustment.'
  }

  if (lgcclrPntPwrS) {
    infoStopclrPntPwrS  <- 'Argument for parameter `clrPntPwrS` must be color name(s) for coloring observed point(s) of power after sequential adjustment.'
  }

  if (lgcclrLnPwrCtf) {
    infoStopclrLnPwrCtf <- 'Argument for parameter `clrLnPwrCtf` must be a color name for coloring segment of assumed power.'
  }

  if (lgcclrLnPwrO) {
    infoStopclrLnPwrO   <- 'Argument for parameter `clrLnPwrO` must be a color name for coloring segment of line for observed power without sequential adjustment.'
  }

  if (lgcclrLnPwrP) {
    infoStopclrLnPwrP   <- 'Argument for parameter `clrLnPwrP` must be a color name for coloring segment of line for predicted or expected power after sequential adjustment.'
  }

  if (lgcclrLnPwrS) {
    infoStopclrLnPwrS   <- 'Argument for parameter `clrLnPwrS` must be a color name for coloring segment of line for observed power after sequential adjustment.'
  }

  if (lgcAnglStdy) {
    infoStopAnglStdy    <- 'Argument for parameter `anglStdy` must be a numeric value between 0 and 360 for indicating angle of study labels.'
  }


  # 05. RETURN results of argument checking  -----

  if (lgcTxtTtl      | lgcLgcPwrO    | lgcLgcLblStdy |
      lgcSzFntTtl    | lgcSzFntTtlX  | lgcSzFntTtlY  |
      lgcSzFntAxsX   | lgcSzFntAxsY  | lgcSzFntLgnd  |
      lgcSzFntStdy   | lgcszPntPwrO  | lgcszPntPwrS  |
      lgcszLnPwrCtf  | lgcszLnPwrO   | lgcszLnPwrP   | lgcszLnPwrS   |
      lgctypPntPwrO  | lgctypPntPwrS |
      lgctypLnPwrCtf | lgctypLnPwrO  | lgctypLnPwrP  | lgctypLnPwrS  |
      lgcClrTtl      | lgcClrTtlX    | lgcClrTtlY    |
      lgcClrAxsX     | lgcClrAxsY    |  lgcClrLgnd   |
      lgcClrLblStdy  |
      lgcclrPntPwrO  | lgcclrPntPwrS |
      lgcclrLnPwrCtf | lgcclrLnPwrO  | lgcclrLnPwrP  | lgcclrLnPwrS  |
      lgcAnglStdy
      )

  stop(paste(ifelse(lgcTxtTtl,      paste(infoStopTxtTtl, "\n", sep = ""), ""),
             ifelse(lgcLgcPwrO,     paste(infoStopLgcLgcPwrO, "\n", sep = ""), ""),
             ifelse(lgcLgcLblStdy,  paste(infoStopLgcLblStdy, "\n", sep = ""), ""),
             ifelse(lgcSzFntTtl,    paste(infoStopSzFntTtl, "\n", sep = ""), ""),
             ifelse(lgcSzFntTtlX,   paste(infoStopSzFntTtlX, "\n", sep = ""), ""),
             ifelse(lgcSzFntTtlY,   paste(infoStopSzFntTtlY, "\n", sep = ""), ""),
             ifelse(lgcSzFntAxsX,   paste(infoStopSzFntAxsX, "\n", sep = ""), ""),
             ifelse(lgcSzFntAxsY,   paste(infoStopSzFntAxsY, "\n", sep = ""), ""),
             ifelse(lgcSzFntLgnd,   paste(infoStopSzFntLgnd, "\n", sep = ""), ""),
             ifelse(lgcSzFntStdy,   paste(infoStopSzFntStdy, "\n", sep = ""), ""),
             ifelse(lgcszPntPwrO,   paste(infoStopszPntPwrO, "\n", sep = ""), ""),
             ifelse(lgcszPntPwrS,   paste(infoStopszPntPwrS, "\n", sep = ""), ""),
             ifelse(lgcszLnPwrCtf,  paste(infoStopszLnPwrCtf, "\n", sep = ""), ""),
             ifelse(lgcszLnPwrO,    paste(infoStopszLnPwrO, "\n", sep = ""), ""),
             ifelse(lgcszLnPwrP,    paste(infoStopszLnPwrP, "\n", sep = ""), ""),
             ifelse(lgcszLnPwrS,    paste(infoStopszLnPwrS, "\n", sep = ""), ""),
             ifelse(lgctypPntPwrO,  paste(infoStoptypPntPwrO, "\n", sep = ""), ""),
             ifelse(lgctypPntPwrS,  paste(infoStoptypPntPwrS, "\n", sep = ""), ""),
             ifelse(lgctypLnPwrCtf, paste(infoStoptypLnPwrCtf, "\n", sep = ""), ""),
             ifelse(lgctypLnPwrO,   paste(infoStoptypLnPwrO, "\n", sep = ""), ""),
             ifelse(lgctypLnPwrP,   paste(infoStoptypLnPwrP, "\n", sep = ""), ""),
             ifelse(lgctypLnPwrS,   paste(infoStoptypLnPwrS, "\n", sep = ""), ""),
             ifelse(lgcClrTtl,      paste(infoStopClrTtl, "\n", sep = ""), ""),
             ifelse(lgcClrTtlX,     paste(infoStopClrTtlX, "\n", sep = ""), ""),
             ifelse(lgcClrTtlY,     paste(infoStopClrTtlY, "\n", sep = ""), ""),
             ifelse(lgcClrAxsX,     paste(infoStopClrAxsX, "\n", sep = ""), ""),
             ifelse(lgcClrAxsY,     paste(infoStopClrAxsY, "\n", sep = ""), ""),
             ifelse(lgcClrLgnd,     paste(infoStopClrLgnd, "\n", sep = ""), ""),
             ifelse(lgcClrLblStdy,  paste(infoStopClrLblStdy, "\n", sep = ""), ""),
             ifelse(lgcclrPntPwrO,  paste(infoStopclrPntPwrO, "\n", sep = ""), ""),
             ifelse(lgcclrPntPwrS,  paste(infoStopclrPntPwrS, "\n", sep = ""), ""),
             ifelse(lgcclrLnPwrCtf, paste(infoStopclrLnPwrCtf, "\n", sep = ""), ""),
             ifelse(lgcclrLnPwrO,   paste(infoStopclrLnPwrO, "\n", sep = ""), ""),
             ifelse(lgcclrLnPwrP,   paste(infoStopclrLnPwrP, "\n", sep = ""), ""),
             ifelse(lgcclrLnPwrS,   paste(infoStopclrLnPwrS, "\n", sep = ""), ""),
             ifelse(lgcAnglStdy,    paste(infoStopAnglStdy, "\n", sep = ""), ""),
             sep = "")
  )


  # 06. PROCESS additive setting -----

  infoLgcPwrO    <- lgcPwrO
  infoLgcLblStdy <- lgcLblStdy

  if (is.null(typPntPwrO)) {
    typPntPwrO         <- 22
    dataOSA$typPntPwrO <- typPntPwrO
  } else {
    typPntPwrO         <- typPntPwrO + 20
    dataOSA$typPntPwrO <- typPntPwrO + 20
  }

  if (is.null(typPntPwrS)) {
    typPntPwrS          <- 21
    dataOSA$typPntPwrS  <- typPntPwrS
  } else {
    typPntPwrS          <- typPntPwrS + 20
    dataOSA$typPntPwrS  <- typPntPwrS + 20
  }


  if (base::isFALSE(is.null(clrLnPwrS))) {
    infoColorASB <- clrLnPwrS
  }

  if (clrPntPwrS == "none") {
    clrPntPwrS <- rgb(1, 1, 1, 1)
  }



  # 07. ILLUSTRATE Sequential-adjusted power plot -----

  plot(dataPlotPwr$frctn * 1.2, # sample
       dataPlotPwr$pwrExpct,
       frame = FALSE,
       xlim = c(0, max(dataPlotPwr$frctn) * 1.2), # sample
       ylim = c(0, 1.2),
       pch = 16,
       col = rgb(1, 1, 1, 0),
       cex = 0.5,
       xlab = "",
       xaxt = "n", #"darkred"
       yaxt = "n",
       ylab = "",
       main = "")
  mtext(ifelse(is.null(txtTtl),
               "Sequential-adjusted power",
               paste("Sequential-adjusted power of ",
                     txtTtl,
                     sep = "")
               ),
        side = 3,
        line = 2,
        col = clrTtl,
        cex = szFntTtl)

  axis(side = 1,
       at   = c(0,
                0.2,
                0.4,
                0.6,
                0.8,
                1,
                ceiling(max(dataPlotPwr$frctn) * 1.2) # sample
                ),
       labels = c(0,
                  ceiling(max(dataPlotPwr$sample) * 0.2),
                  ceiling(max(dataPlotPwr$sample) * 0.4),
                  ceiling(max(dataPlotPwr$sample) * 0.6),
                  ceiling(max(dataPlotPwr$sample) * 0.8),
                  ceiling(max(dataPlotPwr$sample)),
                  ceiling(max(dataPlotPwr$sample) * 1.2)
                  ),
       col = clrAxsX,
       cex.axis = szFntAxsX)#,
       #padj = 0, hadj = 0, las = 1)
  mtext("Information size",
        side = 1,
        line = 3,
        col = clrTtlX,
        cex = szFntTtlX)

  axis(side = 2,
       at = c(0, 0.2, 0.4, 0.6, 0.8, 1.0),
       col = clrAxsY,
       cex.axis = szFntAxsY,
       padj = 0, hadj = 1, las = 1)
  mtext("Probability",
        side = 2,
        line = 2.5,
        at = 0.5,
        col = clrTtlY,
        cex = szFntTtlY
        )

  segments(c(0),
           c(1 - infoBeta),
           c(max(dataPlotPwr$frctn)), # sample
           c(1 - infoBeta),
           lty = typLnPwrCtf,
           col = clrLnPwrCtf,
           lwd = szLnPwrCtf)

  segments(head(dataPlotPwr$frctn, -1), # sample
           head(dataPlotPwr$pwrExpct, -1),
           tail(dataPlotPwr$frctn, -1), # sample
           tail(dataPlotPwr$pwrExpct, -1),
           lty = typLnPwrP,
           col = colorRampPalette(clrLnPwrP)(100), #colorRampPalette(c("red", "blue"))(nrow(dataPlotPwr))
           lwd = szLnPwrP)
  #points(dataPwr$sample,
  #       dataPwr$pwrExpct,
  #       pch = 16,
  #       col = colorRampPalette(clrLnPwrP)(nrow(dataPwr)), #colorRampPalette(c("red", "blue"))(nrow(dataPlotPwr))
  #       cex = szLnPwrP)

  #lines(dataOSA$nCum,
  #      dataOSA$pwrSqnt,
  #      lty = 1,
  #      col = clrPntPwrO,
  #      lwd = 1)
  segments(head(dataOSA$frctn, -1), # nCum
           head(dataOSA$pwrSqnt, -1),
           tail(dataOSA$frctn, -1), # nCum
           tail(dataOSA$pwrSqnt, -1),
           lty = typLnPwrS,
           col = clrLnPwrS,
           lwd = szLnPwrS)
  segments(head(dataPwr$frctn, -1), # sample
           head(dataPwr$pwrPrdct, -1),
           tail(dataPwr$frctn, -1), # sample
           tail(dataPwr$pwrPrdct, -1),
           lty = typLnPwrP,
           col = clrLnPwrS, #colorRampPalette(clrLnPwrP)(nrow(dataPwr))
           lwd = szLnPwrP)
  #points(dataPwr$sample,
  #       dataPwr$pwrPrdct,
  #       pch = 16,
  #       col = colorRampPalette(clrLnPwrP)(nrow(dataPwr)), #colorRampPalette(c("red", "blue"))(nrow(dataPlotPwr))
  #       cex = szLnPwrP)
  points(dataOSA$frctn, # nCum
         dataOSA$pwrSqnt,
         pch = typPntPwrS,
         col = clrPntPwrS,
         bg  = clrPntPwrS,
         cex = szPntPwrS)
  points(infoCases / infoOIS,
         infoPwrObs,
         pch = typPntPwrS,
         col = clrPntPwrS,
         bg  = clrPntPwrS,
         cex = szPntPwrS * 2)

  segments(max(dataPlotPwr$frctn) * 0.03, # sample
           c(1.14),
           max(dataPlotPwr$frctn) * 0.05, # sample
           c(1.14),
           lty = typLnPwrS,
           col = clrLnPwrS,
           lwd = szLnPwrS)
  points(max(dataPlotPwr$frctn) * 0.04,
         1.14,
         pch = typPntPwrS,
         col = clrPntPwrS,
         bg  = clrPntPwrS,
         cex = szPntPwrS * 2)
  text(max(dataPlotPwr$frctn) * 0.07, # sample
       1.14,
       paste("Observed power after sequential adjustment"),
       pos = 4,
       col = clrLgnd,
       cex = szFntLgnd)

  segments(max(dataPlotPwr$frctn) * 0.03, # sample
           c(1.09),
           max(dataPlotPwr$frctn) * 0.05, # sample
           c(1.09),
           lty = typLnPwrP,
           col = colorRampPalette(clrLnPwrP)(5), # colorRampPalette(clrLnPwrP)(nrow(dataPlotPwr))
           lwd = szLnPwrP)
  #points(ceiling(max(dataPlotPwr$sample) * 0.03):ceiling(max(dataPlotPwr$sample) * 0.05),
  #       rep(1.19, length(ceiling(max(dataPlotPwr$sample) * 0.03):ceiling(max(dataPlotPwr$sample) * 0.05))),
  #       pch = 16,
  #       col = colorRampPalette(clrLnPwrP)(length(ceiling(max(dataPlotPwr$sample) * 0.03):ceiling(max(dataPlotPwr$sample) * 0.05))),
  #       cex = szLnPwrP)
  text(max(dataPlotPwr$frctn) * 0.07, # sample
       1.09,
       paste("Expected power after sequential adjustment", sep = ""),
       pos = 4,
       col = clrLgnd,
       cex = szFntLgnd)

  segments(max(dataPlotPwr$frctn) * 0.03, # sample
           c(1.04),
           max(dataPlotPwr$frctn) * 0.05, # sample
           c(1.04),
           lty = typLnPwrP,
           col = clrLnPwrS,
           lwd = szLnPwrP)
  #points(ceiling(max(dataPlotPwr$sample) * 0.03):ceiling(max(dataPlotPwr$sample) * 0.05),
  #       rep(1.19, length(ceiling(max(dataPlotPwr$sample) * 0.03):ceiling(max(dataPlotPwr$sample) * 0.05))),
  #       pch = 16,
  #       col = colorRampPalette(clrLnPwrP)(length(ceiling(max(dataPlotPwr$sample) * 0.03):ceiling(max(dataPlotPwr$sample) * 0.05))),
  #       cex = szLnPwrP)
  text(max(dataPlotPwr$frctn) * 0.07, # sample
       1.04,
       paste("Predict power after sequential adjustment", sep = ""),
       pos = 4,
       col = clrLgnd,
       cex = szFntLgnd)

  segments(infoCases / infoOIS + 0.02, # infoCases + infoOIS * 0.02
           infoPwrObs - 0.02,
           infoCases / infoOIS + 0.04, # infoCases + infoOIS * 0.04
           infoPwrObs - 0.04,
           lty = 1,
           col = clrLblStdy,
           cex = 0.6)
  text(infoCases / infoOIS + 0.045, # infoCases + infoOIS * 0.045
       infoPwrObs - 0.05,
       paste("Observed power = ",
             round(infoPwrObs, 3),
             "\n",
             "(AIS = ",
             infoCases,
             ")",
             sep = ""),
       pos = 4,
       col = clrPntPwrS,
       cex = szPntPwrS)

  text(max(dataPlotPwr$frctn) * 1.2, # sample
       1 - infoBeta,
       paste("Assumed power = ",
             1 - infoBeta,
             "\n",
             "(OIS = ",
             ceiling(infoOIS),
             ")",
             sep = ""),
       pos = 2,
       col = clrLgnd,
       cex = szFntLgnd)


  text(max(dataPlotPwr$frctn) * 1.2, # sample
       ifelse(dataOSA$frctn[infoNumStud] < 0.7,
              0.1,
              ifelse(infoLgcLblStdy == FALSE,
                     0.1,
                     1.09)),
       paste("Setting: ",
             ifelse(infoModel == "random",
                    paste("random-effects model based on ",
                          infoMethod,
                          " method",
                          sep = ""),
                    "fixed-effect model"
             ),
             sep = ""),
       pos = 2,
       col = clrLgnd,
       cex = szFntLgnd)

  text(max(dataPlotPwr$frctn) * 1.2, # sample
       ifelse(dataOSA$frctn[infoNumStud] < 0.7,
              0.05,
              ifelse(infoLgcLblStdy == FALSE,
                     0.05,
                     1.04)),
       paste("Parameter: ",
             ifelse(infoMeasure %in% c("MD", "SMD"),
                    infoMeasure,
                    "Observed effect"),
             ifelse(abs(infoOES) < 0.001,
                    paste(" < 0.001", sep = ""),
                    paste(" = ", round(infoOES, 3), sep = "")),
             ifelse(infoMeasure %in% c("MD", "SMD"),
                    "",
                    paste(" (RRR",
                          ifelse(infoRRR < 0.001,
                                 " < 0.001)",
                                 paste(" = ", round(infoRRR, 3), ")",
                                       sep = "")
                          ),
                          sep = "")
             ),
             "; alpha: ", infoAlpha,
             ifelse(infoAdjust == "none",
                    "; no adjustment factor)",
                    paste("; ",
                          ifelse(infoAdjust == "D2",
                                 "diversity-based AF: ",
                                 ifelse(infoAdjust == "I2",
                                        "I-squared-based AF: ",
                                        paste(infoAdjust, "-based AF: ",
                                              sep = "")
                                 )
                          ),
                          round(infoAF, 3),
                          sep = "")
             ),
             sep = ""),
       pos = 2,
       col = clrLgnd,
       cex = szFntLgnd)

  if (infoLgcPwrO) {
    #lines(dataOSA$frctn, # nCum
    #      dataOSA$pwrCum,
    #      lty = typLnPwrO,
    #      col = clrPntPwrO,
    #      lwd = szLnPwrO)
    segments(head(dataOSA$frctn, -1),
             head(dataOSA$pwrCum, -1),
             tail(dataOSA$frctn, -1),
             tail(dataOSA$pwrCum, -1),
             lty = typLnPwrO,
             col = clrPntPwrO,
             lwd = szLnPwrO)
    points(dataOSA$frctn, # nCum
           dataOSA$pwrCum,
           pch = typPntPwrO,
           col = clrPntPwrO,
           bg  = clrPntPwrO,
           cex = szPntPwrO)
    segments(max(dataPlotPwr$frctn) * 0.03, # sample
             c(1.19),
             max(dataPlotPwr$frctn) * 0.05, # sample
             c(1.19),
             lty = typLnPwrO,
             col = clrPntPwrO,
             lwd = szLnPwrO)
    points(max(dataPlotPwr$frctn) * 0.04,
           1.19,
           pch = typPntPwrO,
           col = clrPntPwrO,
           bg  = clrPntPwrO,
           cex = szPntPwrO)
    text(max(dataPlotPwr$frctn) * 0.07, # sample
         1.19,
         paste("Observed power without adjustment"),
         pos = 4,
         col = clrLgnd,
         cex = szFntLgnd)
  }

  if (infoLgcLblStdy) {
    points(dataOSA$frctn, # nCum
           rep(-0.02, nrow(dataOSA)),
           pch = "|",
           col = clrLblStdy,
           cex = 0.6)
    #text(dataOSA$frctn, # nCum
    #     rep(-0.02, nrow(dataOSA)),
    #     "|",
    #     pos = rep(4, nrow(dataOSA)), #6 - infoPosLabel | infoPosLabel - (infoPosLabel - 3)
    #     col = clrLblStdy,
    #     cex = 0.6)
    text(dataOSA$frctn - 0.01, # nCum
         rep(0, nrow(dataOSA)),
         dataOSA$source,
         pos = rep(4, nrow(dataOSA)), #6 - infoPosLabel | infoPosLabel - (infoPosLabel - 3)
         col = clrLblStdy,
         cex = szFntStdy,
         srt = anglStdy)
  }

}
