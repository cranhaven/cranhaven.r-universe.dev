#' @title Plot of Continuous Restriction for Noting Error Thresholds
#'
#' @author Enoch Kang
#'
#' @description
#' **PlotCoRNET()** is a function for plotting *Co*ntinuous *R*estriction for *N*oting
#'                  *E*rror *T*hresholds. In other words, it returns sequential
#'                  monitoring and interim line and edge.
#'
#' @param object     OBJECT in **DoTSA** class that is an output of trial sequential
#'                   analysis using function `DoTSA()`.
#' @param sclAxsX    CHARACTER for indicating unit of scale on axis X.
#' @param txtTtl     CHARACTER for user-defined main title on the trial sequential
#'                   analysis plot.
#' @param group      CHARACTER for labeling two groups.
#' @param lgcZone    LOGIC value for indicating whether to show zones.
#' @param lgcLblStdy LOGIC value for indicating whether to label each data source.
#' @param lgcSAP     LOGIC value for indicating whether to show sequential-adjusted
#'                   power.
#' @param lgcInvert  LOGIC value for indicating whether to invert plot.
#' @param lgcSmooth  LOGIC value for indicating whether to smooth error boundaries.
#' @param szFntTtl   NUMERIC value for indicating font size of main title.
#' @param szFntTtlX  NUMERIC value for indicating font size of title on axis X.
#' @param szFntTtlY  NUMERIC value for indicating font size of title on axis Y.
#' @param szFntAxsX  NUMERIC value for indicating font size of scale on axis X.
#' @param szFntAxsY  NUMERIC value for indicating font size of scale on axis Y.
#' @param szFntLgnd  NUMERIC value for indicating font size of legend.
#' @param szFntLblY  NUMERIC value for indicating font size of the label of
#'                   "Cumulative z-score" on axis Y.
#' @param szFntStdy  NUMERIC value(s) for indicating font size(s) of the label(s)
#'                   of each data source.
#' @param szFntRIS   NUMERIC value for indicating font size of the label of required
#'                   information size.
#' @param szFntAIS   NUMERIC value for indicating font size of the label of acquired
#'                   information size.
#' @param szPntStdy  NUMERIC value(s) for indicating size(s) of observed point(s).
#' @param szPntASB   NUMERIC value for indicating size of point(s) on alpha-spending
#'                   boundaries.
#' @param szLn0      NUMERIC value for indicating width of null line.
#' @param szLnSig    NUMERIC value for indicating width of line for statistical
#'                   significance.
#' @param szLnZCum   NUMERIC value for indicating width of line for cumulative
#'                   z-score.
#' @param szLnASB    NUMERIC value for indicating width of line for alpha-spending
#'                   boundaries.
#' @param szLnRIS    NUMERIC value for indicating width of line for required
#'                   information size.
#' @param typPntStdy NUMERIC value(s) between 1 to 5 for indicating type(s) of
#'                   observed point(s). Symbols in the current version includes
#'                   circle, square, diamond, triangle point-up, and triangle
#'                   point down.
#' @param typPntASB  NUMERIC value between 1 to 5 for indicating type of point(s)
#'                   on alpha-spending boundaries. Symbols in the current version
#'                   includes circle, square, diamond, triangle point-up, and
#'                   triangle point down.
#' @param typLn0     NUMERIC value for indicating type of null line.
#' @param typLnSig   NUMERIC value for indicating type of line for statistical
#'                   significance.
#' @param typLnZCum  NUMERIC value for indicating type of line for cumulative
#'                   z-score.
#' @param typLnASB   NUMERIC value for indicating type of line for alpha-spending
#'                   boundaries.
#' @param typLnRIS   NUMERIC value for indicating type of line for required
#'                   information size.
#' @param clrTtl     CHARACTER of a color name for main title.
#' @param clrTtlX    CHARACTER of a color name for title on axis X.
#' @param clrTtlY    CHARACTER of a color name for title on axis Y.
#' @param clrAxsX    CHARACTER of a color name for scale on axis X.
#' @param clrAxsY    CHARACTER of a color name for scale on axis Y.
#' @param clrLgnd    CHARACTER of a color name for legend.
#' @param clrLblY    CHARACTER of a color name for the label "Cumulative z-score"
#'                   on axis Y.
#' @param clrLblStdy CHARACTER of color name(s) for the label(s) of each data source.
#' @param clrLblRIS  CHARACTER of a color name for the label of required information
#'                   size.
#' @param clrLblAIS  CHARACTER of a color name for the label of acquired information
#'                   size.
#' @param clrPntStdy CHARACTER of color name(s) for observed point(s) of data source.
#' @param clrPntASB  CHARACTER of a color name for point(s) on the alpha-spending
#'                   boundaries.
#' @param clrLn0     CHARACTER of a color name for null line.
#' @param clrLnSig   CHARACTER of a color name for line of statistical significance.
#' @param clrLnZCum  CHARACTER of a color name for line of cumulative z-score.
#' @param clrLnASB   CHARACTER of a color name for line of alpha-spending boundaries.
#' @param clrLnRIS   CHARACTER of a color name for line of required information size.
#' @param anglStdy   NUMERIC value between 0 and 360 for indicating angle of data
#'                   source.
#' @param BSB        LOGIC value for indicating whether to illustrate beta-spending
#'                   boundaries.
#'
#'
#' @return
#' **PlotCoRNET()** returns a plot of trial sequential analysis.
#'
#'
#' @references
#' Jennison, C., & Turnbull, B. W. (2005). Meta-analyses and adaptive group
#' sequential designs in the clinical development process.
#' **Journal of biopharmaceutical statistics**, *15(4)*, 537–558.
#' https://doi.org/10.1081/BIP-200062273.
#'
#' Wetterslev, J., Jakobsen, J. C., & Gluud, C. (2017). Trial sequential analysis
#' in systematic reviews with meta-analysis. **BMC medical research methodology**,
#' *17(1)*, 1-18.
#'
#' NCSS Statistical Software (2023). **Group-sequential analysis for two proportions**.
#' In *PASS Documentation*. Available online:
#' https://www.ncss.com/wp-content/themes/ncss/pdf/Procedures/NCSS/Group-Sequential_Analysis_for_Two_Proportions.pdf
#'
#'
#' @seealso \code{\link{DoTSA}}
#'
#' @examples
#' ## Not run:
#' # 1. Import a dataset of study by Fleiss (1993)
#' library(meta)
#' data("Fleiss1993bin")
#'
#' # 2. Perform trial sequential analysis
#'  output <- DoTSA(Fleiss1993bin, study, year,
#'                  r1 = d.asp, n1 = n.asp,
#'                  r2 = d.plac, n2 = n.plac,
#'                  measure = "RR", RRR = 0.2,
#'                  group = c("Aspirin", "Placebo"),
#'                  plot = TRUE)
#'
#' # 3. Illustrate plot of trial sequential analysis
#'  PlotCoRNET(output)
#'
#' ## End(Not run)
#'
#' @export PlotCoRNET



PlotCoRNET <- function(object     = NULL,
                      sclAxsX    = "sample",
                      txtTtl     = NULL,
                      group      = NULL,
                      lgcZone    = FALSE,
                      lgcLblStdy = FALSE,
                      lgcSAP     = FALSE,
                      lgcInvert  = FALSE,
                      lgcSmooth  = FALSE,
                      szFntTtl   = 1.8,
                      szFntTtlX  = 1.2,
                      szFntTtlY  = NULL,
                      szFntAxsX  = 0.8,
                      szFntAxsY  = 0.8,
                      szFntLgnd  = 0.7,
                      szFntLblY  = 1.2,
                      szFntStdy  = 0.8,
                      szFntRIS   = 0.8,
                      szFntAIS   = 0.8,
                      szPntStdy  = 1,
                      szPntASB   = 0.8,
                      szLn0      = 1,
                      szLnSig    = 1,
                      szLnZCum   = 2,
                      szLnASB    = 1,
                      szLnRIS    = 1,
                      typPntStdy = NULL,
                      typPntASB  = NULL,
                      typLn0     = 1,
                      typLnSig   = 2,
                      typLnZCum  = 1,
                      typLnASB   = 3,
                      typLnRIS   = 2,
                      clrTtl     = "black",
                      clrTtlX    = "black",
                      clrTtlY    = "black",
                      clrAxsX    = "black",
                      clrAxsY    = "black",
                      clrLgnd    = "black",
                      clrLblY    = "black",
                      clrLblStdy = "black",
                      clrLblRIS  = "black",
                      clrLblAIS  = "black",
                      clrPntStdy = "gray25",
                      clrPntASB  = "none",
                      clrLn0     = "gray25",
                      clrLnSig   = "gray",
                      clrLnZCum  = "blue4",
                      clrLnASB   = "red4",
                      clrLnRIS   = "red4",
                      anglStdy   = 30,
                      BSB        = FALSE) {

  # 01. CHECK object -----
  lgcInObject <- ifelse(is.null(object), FALSE, TRUE)
  lgcReq1     <- ifelse(lgcInObject == TRUE, TRUE, FALSE)

  lgcStop1     <- ifelse(lgcReq1 == TRUE, FALSE, TRUE)
  infoLgcStop1 <- ifelse(lgcStop1 == TRUE,
                         'Parameter "object" should be used for assigning a DoTSA-class object.',
                         "")
  # 02. RETURN results of core argument checking  -----
  # 02.1. RETURN results of none object checking

  if (lgcStop1)
    stop(infoLgcStop1)

  # 02.2. RETURN results of object class checking
  lgcObject    <- inherits(object, "DoTSA")
  #lgcObject    <- ifelse("DoTSA" %in% class(object), FALSE, TRUE)
  lgcReq2      <- ifelse(lgcObject == TRUE, TRUE, FALSE)

  lgcStop2     <- ifelse(lgcReq2 == TRUE, FALSE, TRUE)
  infoLgcStop2 <- ifelse(lgcStop2 == TRUE,
                         'Parameter "object" should be used for assigning a DoTSA-class object.',
                         "")

  if (lgcStop2)
    stop(infoLgcStop2)


  # 03. DEFINE core data -----
  objIn          <- object

  infoNumStud    <- objIn$studies
  infoCases      <- objIn$AIS
  infoAlpha      <- objIn$alpha
  infoBeta       <- objIn$beta
  infoMeasure    <- objIn$measure
  infoModel      <- objIn$model
  infoMethod     <- objIn$method
  infoPooling    <- objIn$pooling
  infoPrefer     <- objIn$prefer
  infoPES        <- objIn$PES
  infoRRR        <- objIn$RRR
  infoPV         <- objIn$variance
  infoDivers     <- objIn$diversity
  infoAdjust     <- objIn$adjust
  infoAF         <- objIn$AF
  infoRISOrg     <- objIn$RIS.org
  infoRISAdj     <- objIn$RIS.adj
  infoPwrObs     <- objIn$power
  infoGroup      <- objIn$group
  infoRef        <- objIn$ref
  infoColorASB   <- objIn$color.ASB
  infoPosLabel   <- objIn$position.label
  dataTSA        <- objIn$data
  dataPlotCoRNET <- objIn$data.bounds

  infoRIS        <- infoRISAdj
  if (infoRIS <= infoCases) {
    dataTSA$frctnLmt <- ifelse(dataTSA$frctn > 1, 1, dataTSA$frctn)
  }

  setPar <- par(no.readonly = TRUE)
  on.exit(par(setPar))
  #infoLgcWarning <- getOption("warn")
  #options(warn = -1)
  #on.exit(options(warn = infoLgcWarning))


  # 04. CHECK additive arguments -----
  suppressWarnings({

    lgcTxtTtl     <- ifelse(is.null(txtTtl),
                            FALSE,
                            ifelse(is.character(txtTtl),
                                   FALSE, TRUE)
    )

    lgcSclAxsX    <- ifelse(is.null(sclAxsX),
                            FALSE,
                            ifelse(is.character(sclAxsX),
                                   FALSE, TRUE)
    )

    lgcLgcZone    <- ifelse(is.logical(lgcZone),
                            FALSE, TRUE)

    lgcLgcLblStdy <- ifelse(is.logical(lgcLblStdy),
                            FALSE, TRUE)

    lgcLgcSAP     <- ifelse(is.logical(lgcSAP),
                            FALSE, TRUE)

    lgcLgcInvert  <- ifelse(is.logical(lgcInvert),
                            FALSE, TRUE)

    lgcLgcSmooth  <- ifelse(is.logical(lgcSmooth),
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

    lgcSzFntLblY <- ifelse(is.null(szFntLblY),
                           FALSE,
                           ifelse(base::isFALSE(length(szFntLblY) == 1 | length(szFntLblY) == 2),
                                  TRUE,
                                  ifelse(base::isFALSE(is.numeric(szFntLblY)),
                                         TRUE,
                                         ifelse(FALSE %in% (szFntLblY >= 0),
                                                TRUE,
                                                ifelse(FALSE %in% (szFntLblY < 6),
                                                       TRUE, FALSE))))
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

    lgcSzFntRIS <- ifelse(is.null(szFntRIS),
                          FALSE,
                          ifelse(base::isFALSE(length(szFntRIS) == 1),
                                 TRUE,
                                 ifelse(base::isFALSE(is.numeric(szFntRIS)),
                                        TRUE,
                                        ifelse(base::isFALSE(szFntRIS > 0),
                                               TRUE,
                                               ifelse(szFntRIS < 6,
                                                      FALSE, TRUE))))
    )

    lgcSzFntAIS <- ifelse(is.null(szFntAIS),
                          FALSE,
                          ifelse(base::isFALSE(length(szFntAIS) == 1),
                                 TRUE,
                                 ifelse(base::isFALSE(is.numeric(szFntAIS)),
                                        TRUE,
                                        ifelse(base::isFALSE(szFntAIS > 0),
                                               TRUE,
                                               ifelse(szFntAIS < 6,
                                                      FALSE, TRUE))))
    )

    lgcSzPntStdy <- ifelse(is.null(szPntStdy),
                           FALSE,
                           ifelse(base::isFALSE(length(szPntStdy) == 1 | length(szPntStdy) == infoNumStud),
                                  TRUE,
                                  ifelse(base::isFALSE(is.numeric(szPntStdy)),
                                         TRUE,
                                         ifelse(FALSE %in% (szPntStdy >= 0),
                                                TRUE,
                                                ifelse(FALSE %in% (szPntStdy < 6),
                                                       TRUE, FALSE))))
    )

    lgcSzPntASB  <- ifelse(is.null(szPntASB),
                           FALSE,
                           ifelse(base::isFALSE(length(szPntASB) == 1 | length(szPntASB) == infoNumStud),
                                  TRUE,
                                  ifelse(base::isFALSE(is.numeric(szPntASB)),
                                         TRUE,
                                         ifelse(FALSE %in% (szPntASB >= 0),
                                                TRUE,
                                                ifelse(FALSE %in% (szPntASB < 6),
                                                       TRUE, FALSE))))
    )

    lgcSzLn0  <- ifelse(is.null(szLn0),
                        FALSE,
                        ifelse(base::isFALSE(length(szLn0) == 1),
                               TRUE,
                               ifelse(base::isFALSE(is.numeric(szLn0)),
                                      TRUE,
                                      ifelse(FALSE %in% (szLn0 >= 0),
                                             TRUE,
                                             ifelse(FALSE %in% (szLn0 < 6),
                                                    TRUE, FALSE))))
    )

    lgcSzLnSig <- ifelse(is.null(szLnSig),
                         FALSE,
                         ifelse(base::isFALSE(length(szLnSig) == 1 | length(szLnSig) == 2),
                                TRUE,
                                ifelse(base::isFALSE(is.numeric(szLnSig)),
                                       TRUE,
                                       ifelse(FALSE %in% (szLnSig >= 0),
                                              TRUE,
                                              ifelse(FALSE %in% (szLnSig < 6),
                                                     TRUE, FALSE))))
    )

    lgcSzLnZCum <- ifelse(is.null(szLnZCum),
                          FALSE,
                          ifelse(base::isFALSE(length(szLnZCum) == 1),
                                 TRUE,
                                 ifelse(base::isFALSE(is.numeric(szLnZCum)),
                                        TRUE,
                                        ifelse(FALSE %in% (szLnZCum >= 0),
                                               TRUE,
                                               ifelse(FALSE %in% (szLnZCum < 6),
                                                      TRUE, FALSE))))
    )

    lgcSzLnASB  <- ifelse(is.null(szLnASB),
                          FALSE,
                          ifelse(base::isFALSE(length(szLnASB) == 1 | length(szLnSig) == 2),
                                 TRUE,
                                 ifelse(base::isFALSE(is.numeric(szLnASB)),
                                        TRUE,
                                        ifelse(FALSE %in% (szLnASB >= 0),
                                               TRUE,
                                               ifelse(FALSE %in% (szLnASB < 6),
                                                      TRUE, FALSE))))
    )

    lgcSzLnRIS  <- ifelse(is.null(szLnRIS),
                          FALSE,
                          ifelse(base::isFALSE(length(szLnRIS) == 1),
                                 TRUE,
                                 ifelse(base::isFALSE(is.numeric(szLnRIS)),
                                        TRUE,
                                        ifelse(FALSE %in% (szLnRIS >= 0),
                                               TRUE,
                                               ifelse(FALSE %in% (szLnRIS < 6),
                                                      TRUE, FALSE))))
    )

    lgcTypPntStdy <- ifelse(is.null(typPntStdy),
                            FALSE,
                            ifelse(base::isFALSE(length(typPntStdy) == 1 | length(typPntStdy) == infoNumStud),
                                   TRUE,
                                   ifelse(base::isFALSE(is.numeric(typPntStdy)),
                                          TRUE,
                                          ifelse(FALSE %in% (typPntStdy >= 0),
                                                 TRUE,
                                                 ifelse(FALSE %in% (typPntStdy < 6),
                                                        TRUE, FALSE))))
    )

    lgcTypPntASB  <- ifelse(is.null(typPntASB),
                            FALSE,
                            ifelse(base::isFALSE(length(typPntASB) == 1 | length(typPntASB) == infoNumStud),
                                   TRUE,
                                   ifelse(base::isFALSE(is.numeric(typPntASB)),
                                          TRUE,
                                          ifelse(FALSE %in% (typPntASB >= 0),
                                                 TRUE,
                                                 ifelse(FALSE %in% (typPntASB < 6),
                                                        TRUE, FALSE))))
    )

    lgcTypLn0     <- ifelse(is.null(typLn0),
                            FALSE,
                            ifelse(length(typLn0) != 1,
                                   TRUE,
                                   ifelse(is.numeric(typLn0),
                                          ifelse(typLn0 %in% c(0:6),
                                                 FALSE,
                                                 TRUE),
                                          ifelse(typLn0 %in% c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),
                                                 FALSE,
                                                 TRUE)))
    )

    lgcTypLnSig   <- ifelse(is.null(typLnSig),
                            FALSE,
                            ifelse(length(typLnSig) != 1,
                                   TRUE,
                                   ifelse(is.numeric(typLnSig),
                                          ifelse(typLnSig %in% c(0:6),
                                                 FALSE,
                                                 TRUE),
                                          ifelse(typLnSig %in% c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),
                                                 FALSE,
                                                 TRUE)))
    )

    lgcTypLnZCum  <- ifelse(is.null(typLnZCum),
                            FALSE,
                            ifelse(length(typLnZCum) != 1,
                                   TRUE,
                                   ifelse(is.numeric(typLnZCum),
                                          ifelse(typLnZCum %in% c(0:6),
                                                 FALSE,
                                                 TRUE),
                                          ifelse(typLnZCum %in% c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),
                                                 FALSE,
                                                 TRUE)))
    )

    lgcTypLnASB   <- ifelse(is.null(typLnASB),
                            FALSE,
                            ifelse(length(typLnASB) != 1,
                                   TRUE,
                                   ifelse(is.numeric(typLnASB),
                                          ifelse(typLnASB %in% c(0:6),
                                                 FALSE,
                                                 TRUE),
                                          ifelse(typLnASB %in% c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),
                                                 FALSE,
                                                 TRUE)))
    )

    lgcTypLnRIS   <- ifelse(is.null(typLnRIS),
                            FALSE,
                            ifelse(length(typLnRIS) != 1,
                                   TRUE,
                                   ifelse(is.numeric(typLnRIS),
                                          ifelse(typLnRIS %in% c(0:6),
                                                 FALSE,
                                                 TRUE),
                                          ifelse(typLnRIS %in% c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),
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

    lgcClrLblY    <- ifelse(is.null(clrLblY),
                            FALSE,
                            ifelse(length(clrLblY) != 1,
                                   TRUE,
                                   ifelse(FALSE %in% (clrLblY %in% colors()),
                                          TRUE, FALSE))
    )

    lgcClrLblStdy <- ifelse(is.null(clrLblStdy),
                            FALSE,
                            ifelse(base::isFALSE(length(clrLblStdy) == 1 | length(clrLblStdy) == infoNumStud),
                                   TRUE,
                                   ifelse(FALSE %in% (clrLblStdy %in% colors()),
                                          TRUE, FALSE))
    )

    lgcClrLblRIS  <- ifelse(is.null(clrLblRIS),
                            FALSE,
                            ifelse(length(clrLblRIS) != 1,
                                   TRUE,
                                   ifelse(FALSE %in% (clrLblRIS %in% colors()),
                                          TRUE, FALSE))
    )

    lgcClrLblAIS  <- ifelse(is.null(clrLblAIS),
                            FALSE,
                            ifelse(length(clrLblAIS) != 1,
                                   TRUE,
                                   ifelse(FALSE %in% (clrLblAIS %in% colors()),
                                          TRUE, FALSE))
    )

    lgcClrPntStdy <- ifelse(is.null(clrPntStdy),
                            FALSE,
                            ifelse(base::isFALSE(length(clrPntStdy) == 1 | length(clrPntStdy) == infoNumStud),
                                   TRUE,
                                   ifelse(FALSE %in% (clrPntStdy %in% colors()),
                                          TRUE, FALSE))
    )

    lgcClrPntASB  <- ifelse(is.null(clrPntASB),
                            FALSE,
                            ifelse(base::isFALSE(length(clrPntASB) == 1 | length(clrPntASB) == infoNumStud),
                                   TRUE,
                                   ifelse(base::isFALSE(FALSE %in% (clrPntASB %in% colors()) | "none" %in% clrPntASB), #FALSE %in% (clrPntASB %in% colors()),
                                          TRUE, FALSE))
    )

    lgcClrLn0    <- ifelse(is.null(clrLn0),
                           FALSE,
                           ifelse(length(clrLn0) != 1,
                                  TRUE,
                                  ifelse(FALSE %in% (clrLn0 %in% colors()),
                                         TRUE, FALSE))
    )

    lgcClrLnSig  <- ifelse(is.null(clrLnSig),
                           FALSE,
                           ifelse(length(clrLnSig) != 1,
                                  TRUE,
                                  ifelse(FALSE %in% (clrLnSig %in% colors()),
                                         TRUE, FALSE))
    )

    lgcClrLnZCum <- ifelse(is.null(clrLnZCum),
                           FALSE,
                           ifelse(length(clrLnZCum) != 1,
                                  TRUE,
                                  ifelse(FALSE %in% (clrLnZCum %in% colors()),
                                         TRUE, FALSE))
    )

    lgcClrLnASB  <- ifelse(is.null(clrLnASB),
                           FALSE,
                           ifelse(length(clrLnASB) != 1,
                                  TRUE,
                                  ifelse(FALSE %in% (clrLnASB %in% colors()),
                                         TRUE, FALSE))
    )

    lgcClrLnRIS  <- ifelse(is.null(clrLnRIS),
                           FALSE,
                           ifelse(length(clrLnRIS) != 1,
                                  TRUE,
                                  ifelse(FALSE %in% (clrLnRIS %in% colors()),
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

    lgcGroup      <- ifelse(is.null(group),
                            FALSE,
                            ifelse(length(group) == 2,
                                   FALSE, TRUE)
    )


    if (lgcTxtTtl) {
      infoStopTxtTtl     <- 'Argument for parameter `txtTtl` must be characters for main title of trial sequential analysis plot.'
    }

    if (lgcSclAxsX) {
      infoStopSclAxsX    <- 'Argument for parameter `sclAxsX` must be characters for indicating unit of scale on axis X.'
    }

    if (lgcLgcZone) {
      infoStopLgcZone    <- 'Argument for parameter `lgcZone` must be a logic value for indicating whether to show zones on the plot.'
    }

    if (lgcLgcLblStdy) {
      infoStopLgcLblStdy <- 'Argument for parameter `lgcLblStdy` must be a logic value for indicating whether to show names of observed studies on the plot.'
    }

    if (lgcLgcSAP) {
      infoStopLgcSAP     <- 'Argument for parameter `lgcSAP` must be a logic value for indicating whether to show sequential-adjusted power on the plot.'
    }

    if (lgcLgcInvert) {
      infoStopLgcInvert  <- 'Argument for parameter `lgcInvert` must be a logic value for indicating whether to invert the plot.'
    }

    if (lgcLgcSmooth) {
      infoStopLgcSmooth  <- 'Argument for parameter `lgcSmooth` must be a logic value for indicating whether to smooth error boundaries.'
    }

    if (lgcSzFntTtl) {
      infoStopSzFntTtl   <- 'Argument for parameter `szFntTtl` must be a numeric value between 0 and 5 for indicating font size of main title plot.'
    }

    if (lgcSzFntTtlX) {
      infoStopSzFntTtlX  <- 'Argument for parameter `szFntTtlX` must be a numeric value between 0 and 5 for indicating font size of title on X-axis.'
    }

    if (lgcSzFntTtlY) {
      infoStopSzFntTtlY  <- 'Argument for parameter `szFntTtlY` must be a numeric value between 0 and 5 for indicating font size of title on Y-axis.'
    }

    if (lgcSzFntAxsX) {
      infoStopSzFntAxsX  <- 'Argument for parameter `szFntAxsX` must be a numeric value between 0 and 5 for indicating font size of numeric scale on X-axis.'
    }

    if (lgcSzFntAxsY) {
      infoStopSzFntAxsY  <- 'Argument for parameter `szFntAxsY` must be a numeric value between 0 and 5 for indicating font size of numeric scale on Y-axis.'
    }

    if (lgcSzFntLgnd) {
      infoStopSzFntLgnd  <- 'Argument for parameter `szFntLgnd` must be a numeric value between 0 and 5 for indicating font size of legend.'
    }

    if (lgcSzFntLblY) {
      infoStopSzFntLblY  <- 'Argument for parameter `szFntLblY` must be a numeric value between 0 and 5 for indicating font size of labels on Y-axis.'
    }

    if (lgcSzFntStdy) {
      infoStopSzFntStdy  <- 'Argument for parameter `szFntStdy` must be a numeric value between 0 and 5 for indicating font size of study labels.'
    }

    if (lgcSzFntRIS) {
      infoStopSzFntRIS   <- 'Argument for parameter `szFntRIS` must be a numeric value between 0 and 5 for indicating font size of text of required information size.'
    }

    if (lgcSzFntAIS) {
      infoStopSzFntAIS   <- 'Argument for parameter `szFntAIS` must be a numeric value between 0 and 5 for indicating font size of text of acquired information size.'
    }

    if (lgcSzPntStdy) {
      infoStopSzPntStdy  <- 'Argument for parameter `szPntStdy` must be a numeric value between 0 and 5 for indicating point size of each study.'
    }

    if (lgcSzPntASB) {
      infoStopSzPntASB  <- 'Argument for parameter `szPntASB` must be a numeric value between 0 and 5 for indicating point size on alpha-spending boundaries.'
    }

    if (lgcSzLn0) {
      infoStopSzLn0      <- 'Argument for parameter `szLn0` must be a numeric value between 0 and 5 for indicating width of null line.'
    }

    if (lgcSzLnSig) {
      infoStopSzLnSig    <- 'Argument for parameter `szLnSig` must be a numeric value between 0 and 5 for indicating width of line for statistical significance.'
    }

    if (lgcSzLnZCum) {
      infoStopSzLnZCum   <- 'Argument for parameter `szLnZCum` must be a numeric value between 0 and 5 for indicating width of line for observed cumulative z-score.'
    }

    if (lgcSzLnASB) {
      infoStopSzLnASB    <- 'Argument for parameter `szLnASB` must be a numeric value between 0 and 5 for indicating width of line for alpha-spending boundaries.'
    }

    if (lgcSzLnRIS) {
      infoStopSzLnRIS    <- 'Argument for parameter `szLnRIS` must be a numeric value between 0 and 5 for indicating width of line for required information size.'
    }

    if (lgcTypPntStdy) {
      infoStopTypPntStdy <- 'Argument for parameter `typPntStdy` must be integer(s) between 1 and 5 for indicating shape(s) of observed point(s).'
    }

    if (lgcTypPntASB) {
      infoStopTypPntASB  <- 'Argument for parameter `typPntASB` must be integer(s) between 1 and 5 for indicating shape(s) of point(s) on the alpha-spending boundaries.'
    }

    if (lgcTypLn0) {
      infoStopTypLn0     <- 'Argument for parameter `typLn0` must be an integer between 1 and 6 for indicating segment type of null line.'
    }

    if (lgcTypLnSig) {
      infoStopTypLnSig   <- 'Argument for parameter `typLnSig` must be an integer between 1 and 6 for indicating segment type of line for statistical significance.'
    }

    if (lgcTypLnZCum) {
      infoStopTypLnZCum  <- 'Argument for parameter `typLnZCum` must be an integer between 1 and 6 for indicating segment type of line for observed cumulative z-score.'
    }

    if (lgcTypLnASB) {
      infoStopTypLnASB   <- 'Argument for parameter `typLnASB` must be an integer between 1 and 6 for indicating segment type of line for alpha-spending boundaries.'
    }

    if (lgcTypLnRIS) {
      infoStopTypLnRIS   <- 'Argument for parameter `typLnRIS` must be an integer between 1 and 6 for indicating segment type of line for required information size.'
    }

    if (lgcClrTtl) {
      infoStopClrTtl     <- 'Argument for parameter `clrTtl` must be a color name for coloring main title.'
    }

    if (lgcClrTtlX) {
      infoStopClrTtlX    <- 'Argument for parameter `clrTtlX` must be a color name for coloring the title on axis X.'
    }

    if (lgcClrTtlY) {
      infoStopClrTtlY    <- 'Argument for parameter `clrTtlY` must be a color name for coloring the title on axis Y.'
    }

    if (lgcClrAxsX) {
      infoStopClrAxsX    <- 'Argument for parameter `clrAxsX` must be color name(s) for coloring the axis X.'
    }

    if (lgcClrAxsY) {
      infoStopClrAxsY    <- 'Argument for parameter `clrAxsY` must be a color name for coloring the axis Y.'
    }

    if (lgcClrLgnd) {
      infoStopClrLgnd    <- 'Argument for parameter `clrLgnd` must be a color name for coloring plot legend.'
    }

    if (lgcClrLblY) {
      infoStopClrLblY    <- 'Argument for parameter `clrLblY` must be a color name for coloring label Y on the plot.'
    }

    if (lgcClrLblStdy) {
      infoStopClrLblStdy <- 'Argument for parameter `clrLblStdy` must be a color name for coloring study labels on the plot.'
    }

    if (lgcClrLblRIS) {
      infoStopClrLblRIS  <- 'Argument for parameter `clrLblRIS` must be a color name for coloring label of required information size on the plot.'
    }

    if (lgcClrLblAIS) {
      infoStopClrLblAIS  <- 'Argument for parameter `clrLblAIS` must be a color name for coloring label of acquired information size on the plot.'
    }

    if (lgcClrPntStdy) {
      infoStopClrPntStdy <- 'Argument for parameter `clrPntStdy` must be color name(s) for coloring observed point(s).'
    }

    if (lgcClrPntASB) {
      infoStopClrPntASB  <- 'Argument for parameter `clrPntASB` must be color name(s) for coloring point(s) on alpha-spending boundaries.'
    }

    if (lgcClrLn0) {
      infoStopClrLn0     <- 'Argument for parameter `clrLn0` must be a color name for coloring segment of null line.'
    }

    if (lgcClrLnSig) {
      infoStopClrLnSig   <- 'Argument for parameter `clrLnSig` must be a color name for coloring segment of line for statistical significance.'
    }

    if (lgcClrLnZCum) {
      infoStopClrLnZCum  <- 'Argument for parameter `clrLnZCum` must be a color name for coloring segment of line for cumulative z-score.'
    }

    if (lgcClrLnASB) {
      infoStopClrLnASB   <- 'Argument for parameter `clrLnASB` must be a color name for coloring segment of line for alpha-spending boundaries.'
    }

    if (lgcClrLnRIS) {
      infoStopClrLnRIS   <- 'Argument for parameter `clrLnRIS` must be a color name for coloring segment of line for required information size.'
    }

    if (lgcAnglStdy) {
      infoStopAnglStdy   <- 'Argument for parameter `anglStdy` must be a numeric value between 0 and 360 for indicating angle of study labels.'
    }

    if (lgcGroup) {
      infoStopGroup      <- 'Argument for parameter `group` must be two strings for renaming the two groups.'
    }


    # 05. RETURN results of argument checking  -----

    if (lgcTxtTtl     | lgcSclAxsX    |
        lgcLgcZone    | lgcLgcLblStdy | lgcLgcSAP    | lgcLgcInvert | lgcLgcSmooth |
        lgcSzFntTtl   | lgcSzFntTtlX  | lgcSzFntTtlY |
        lgcSzFntAxsX  | lgcSzFntAxsY  | lgcSzFntLgnd |
        lgcSzFntLblY  | lgcSzFntStdy  | lgcSzFntRIS  | lgcSzFntAIS  |
        lgcSzPntStdy  | lgcSzPntASB   |
        lgcSzLn0      | lgcSzLnSig    | lgcSzLnZCum  | lgcSzLnASB   | lgcSzLnRIS   |
        lgcTypPntStdy | lgcTypPntASB  |
        lgcTypLn0     | lgcTypLnSig   | lgcTypLnZCum | lgcTypLnASB  | lgcTypLnRIS  |
        lgcClrTtl     | lgcClrTtlX    | lgcClrTtlY   |
        lgcClrAxsX    | lgcClrAxsY    |  lgcClrLgnd  |
        lgcClrLblY    | lgcClrLblStdy | lgcClrLblRIS | lgcClrLblAIS |
        lgcClrPntStdy | lgcClrPntASB  |
        lgcClrLn0     | lgcClrLnSig   | lgcClrLnZCum | lgcClrLnASB  | lgcClrLnRIS  |
        lgcAnglStdy   | lgcGroup
    )

    stop(paste(ifelse(lgcTxtTtl,     paste(infoStopTxtTtl, "\n", sep = ""), ""),
               ifelse(lgcSclAxsX,    paste(infoStopSclAxsX, "\n", sep = ""), ""),
               ifelse(lgcLgcZone,    paste(infoStopLgcZone, "\n", sep = ""), ""),
               ifelse(lgcLgcLblStdy, paste(infoStopLgcLblStdy, "\n", sep = ""), ""),
               ifelse(lgcLgcSAP,     paste(infoStopLgcSAP, "\n", sep = ""), ""),
               ifelse(lgcLgcInvert,  paste(infoStopLgcInvert, "\n", sep = ""), ""),
               ifelse(lgcLgcSmooth,  paste(infoStopLgcSmooth, "\n", sep = ""), ""),
               ifelse(lgcSzFntTtl,   paste(infoStopSzFntTtl, "\n", sep = ""), ""),
               ifelse(lgcSzFntTtlX,  paste(infoStopSzFntTtlX, "\n", sep = ""), ""),
               ifelse(lgcSzFntTtlY,  paste(infoStopSzFntTtlY, "\n", sep = ""), ""),
               ifelse(lgcSzFntAxsX,  paste(infoStopSzFntAxsX, "\n", sep = ""), ""),
               ifelse(lgcSzFntAxsY,  paste(infoStopSzFntAxsY, "\n", sep = ""), ""),
               ifelse(lgcSzFntLgnd,  paste(infoStopSzFntLgnd, "\n", sep = ""), ""),
               ifelse(lgcSzFntLblY,  paste(infoStopSzFntLblY, "\n", sep = ""), ""),
               ifelse(lgcSzFntStdy,  paste(infoStopSzFntStdy, "\n", sep = ""), ""),
               ifelse(lgcSzFntRIS,   paste(infoStopSzFntRIS, "\n", sep = ""), ""),
               ifelse(lgcSzFntAIS,   paste(infoStopSzFntAIS, "\n", sep = ""), ""),
               ifelse(lgcSzPntStdy,  paste(infoStopSzPntStdy, "\n", sep = ""), ""),
               ifelse(lgcSzPntASB,   paste(infoStopSzPntASB, "\n", sep = ""), ""),
               ifelse(lgcSzLn0,      paste(infoStopSzLn0, "\n", sep = ""), ""),
               ifelse(lgcSzLnSig,    paste(infoStopSzLnSig, "\n", sep = ""), ""),
               ifelse(lgcSzLnZCum,   paste(infoStopSzLnZCum, "\n", sep = ""), ""),
               ifelse(lgcSzLnASB,    paste(infoStopSzLnASB, "\n", sep = ""), ""),
               ifelse(lgcSzLnRIS,    paste(infoStopSzLnRIS, "\n", sep = ""), ""),
               ifelse(lgcTypPntStdy, paste(infoStopTypPntStdy, "\n", sep = ""), ""),
               ifelse(lgcTypPntASB,  paste(infoStopTypPntASB, "\n", sep = ""), ""),
               ifelse(lgcTypLn0,     paste(infoStopTypLn0, "\n", sep = ""), ""),
               ifelse(lgcTypLnSig,   paste(infoStopTypLnSig, "\n", sep = ""), ""),
               ifelse(lgcTypLnZCum,  paste(infoStopTypLnZCum, "\n", sep = ""), ""),
               ifelse(lgcTypLnASB,   paste(infoStopTypLnASB, "\n", sep = ""), ""),
               ifelse(lgcTypLnRIS,   paste(infoStopTypLnRIS, "\n", sep = ""), ""),
               ifelse(lgcClrTtl,     paste(infoStopClrTtl, "\n", sep = ""), ""),
               ifelse(lgcClrTtlX,    paste(infoStopClrTtlX, "\n", sep = ""), ""),
               ifelse(lgcClrTtlY,    paste(infoStopClrTtlY, "\n", sep = ""), ""),
               ifelse(lgcClrAxsX,    paste(infoStopClrAxsX, "\n", sep = ""), ""),
               ifelse(lgcClrAxsY,    paste(infoStopClrAxsY, "\n", sep = ""), ""),
               ifelse(lgcClrLgnd,    paste(infoStopClrLgnd, "\n", sep = ""), ""),
               ifelse(lgcClrLblY,    paste(infoStopClrLblY, "\n", sep = ""), ""),
               ifelse(lgcClrLblStdy, paste(infoStopClrLblStdy, "\n", sep = ""), ""),
               ifelse(lgcClrLblRIS,  paste(infoStopClrLblRIS, "\n", sep = ""), ""),
               ifelse(lgcClrLblAIS,  paste(infoStopClrLblAIS, "\n", sep = ""), ""),
               ifelse(lgcClrPntStdy, paste(infoStopClrPntStdy, "\n", sep = ""), ""),
               ifelse(lgcClrPntASB,  paste(infoStopClrPntASB, "\n", sep = ""), ""),
               ifelse(lgcClrLn0,     paste(infoStopClrLn0, "\n", sep = ""), ""),
               ifelse(lgcClrLnSig,   paste(infoStopClrLnSig, "\n", sep = ""), ""),
               ifelse(lgcClrLnZCum,  paste(infoStopClrLnZCum, "\n", sep = ""), ""),
               ifelse(lgcClrLnASB,   paste(infoStopClrLnASB, "\n", sep = ""), ""),
               ifelse(lgcClrLnRIS,   paste(infoStopClrLnRIS, "\n", sep = ""), ""),
               ifelse(lgcAnglStdy,   paste(infoStopAnglStdy, "\n", sep = ""), ""),
               ifelse(lgcGroup,      paste(infoStopGroup, "\n", sep = ""), ""),
               sep = "")
    )


    # 06. PROCESS additive setting -----

    if (base::isFALSE(is.null(group))) {
      infoGroup <- group
    }

    infoLgcZone          <- lgcZone
    infoLgcLblStdy       <- lgcLblStdy
    infoLgcSAP           <- lgcSAP

    if (lgcInvert) {
      dataTSA$zCum       <- dataTSA$zCum * -1
      infoGroup[c(1, 2)] <- infoGroup[c(2, 1)]
    }

    if (is.null(typPntStdy)) {
      typPntStdy         <- 22
      dataTSA$typPntStdy <- typPntStdy
    } else {
      typPntStdy         <- typPntStdy + 20
      dataTSA$typPntStdy <- typPntStdy + 20
    }

    if (is.null(typPntASB)) {
      typPntASB          <- 21
      dataTSA$typPntASB  <- typPntASB
    } else {
      typPntASB          <- typPntASB + 20
      dataTSA$typPntASB  <- typPntASB + 20
    }


    if (base::isFALSE(is.null(clrLnASB))) {
      infoColorASB <- clrLnASB
    }

    if (clrPntASB == "none") {
      clrPntASB <- rgb(1, 1, 1, 1)
    }


    infoBSB      <- BSB




    # 07. ILLUSTRATE proportion of alpha-spending monitoring plot -----

    plot(dataTSA$frctn * 1.1, # nCum
         dataTSA$asub,
         type = "l", frame = FALSE,
         xlim = c(0, max(dataTSA$frctn) * 1.1), # nCum
         ylim = c(ceiling(min(dataTSA$aslb)) * (-10) / ceiling(min(dataTSA$aslb)),
                  ceiling(max(dataTSA$asub)) * 10 / ceiling(max(dataTSA$asub)) + 1),
         col = rgb(1, 1, 1, 0),
         xlab = "",
         xaxt = "n", #"darkred"
         yaxt = "n",
         ylab = "",
         main = "")

    mtext(ifelse(is.null(txtTtl),
                 "Trial sequential analysis",
                 paste("Trial sequential analysis of ",
                       txtTtl,
                       sep = "")
    ),
    side = 3,
    line = 2,
    col = clrTtl,
    cex = szFntTtl)


    if (sclAxsX == "sample") {
      axis(side = 1,
           at   = c(0,
                    0.2,
                    0.4,
                    0.6,
                    0.8,
                    1,
                    max(dataTSA$frctn) * 1.1), # nCum
           labels = c(0,
                      ceiling(infoRIS * 0.2),
                      ceiling(infoRIS * 0.4),
                      ceiling(infoRIS * 0.6),
                      ceiling(infoRIS * 0.8),
                      ceiling(infoRIS),
                      ceiling(max(dataTSA$nCum) * 1.1)),
           col = clrAxsX,
           cex.axis = szFntAxsX,
           padj = 0, hadj = 1, las = 1)
    } else {
      axis(side = 1,
           at   = c(0,
                    0.2,
                    0.4,
                    0.6,
                    0.8,
                    infoRIS,
                    max(dataTSA$frctn) * 1.1), # nCum
           labels = c(0, 0.2, 0.4, 0.6, 0.8, 1,
                      round(max(dataTSA$nCum) * 1.1 / infoRIS, 1)
           ),
           col = clrAxsX,
           cex.axis = szFntAxsX,
           padj = 0, hadj = 1, las = 1)
    }

    mtext(ifelse(sclAxsX == "sample",
                 "Information size",
                 ifelse(sclAxsX == "fraction", "Information fraction",
                        "Fraction")),
          side = 1,
          line = 3,
          col = clrTtlX,
          cex = szFntTtlX)
    mtext(paste("(Note: the meta-analysis was conducted in ",
                ifelse(infoModel == "random",
                       paste("random-effects model based on ",
                             infoPooling, " with ", infoMethod,
                             " method)",
                             sep = ""),
                       paste("fixed-effect model based on ",
                             infoPooling,
                             " method)",
                             sep = "")
                ),
                sep = ""),
          col = clrLgnd,
          cex = szFntLgnd,
          side = 1, line = 4)

    axis(side = 2,
         at = c(seq(ceiling(min(dataTSA$aslb)) * (-10) / ceiling(min(dataTSA$aslb)),
                    ceiling(max(dataTSA$asub)) * 10 / ceiling(max(dataTSA$asub)), 2)),
         col = clrAxsY,
         cex.axis = szFntAxsY,
         padj = 0, hadj = 1, las = 1)
    mtext("Cumulative\n z score",
          side = 3,
          line = 0,
          at   = -0.05, # -infoRIS * 0.05
          col  = clrLblY,
          cex  = szFntLblY)
    mtext(paste("Favors\n", ifelse(infoPrefer == "small", infoGroup[2], infoGroup[1])),
          side = 2,
          line = 2,
          at   = 5,
          col  = clrTtlY,
          cex  = ifelse(max(nchar(infoGroup[2]), nchar(infoGroup[1])) > 10, (1 / sqrt(max(nchar(infoGroup[2]), nchar(infoGroup[1]))))^2 * 10, 1)
    ) #(1/sqrt(seq(11,100,by=1)))^2*10
    mtext(paste("Favors\n", ifelse(infoPrefer == "small", infoGroup[1], infoGroup[2])),
          side = 2,
          line = 2,
          at   = -5,
          col  = clrTtlY,
          cex  = ifelse(is.null(szFntTtlY),
                        ifelse(max(nchar(infoGroup[2]), nchar(infoGroup[1])) > 10, (1 / sqrt(max(nchar(infoGroup[2]), nchar(infoGroup[1]))))^2 * 10, 1), #(1/sqrt(seq(11,100,by=1)))^2*10
                        szFntTtlY)
    )

    if (infoLgcZone) {
      asbStart <- max(which(dataPlotCoRNET$aslb == min(dataPlotCoRNET$aslb)))
      asbStart <- max(which(dataPlotCoRNET$asub == max(dataPlotCoRNET$asub)))
      bsbStart <- max(which(dataPlotCoRNET$bslb == min(dataPlotCoRNET$bslb)))
      bsbEnd   <- max(which(dataPlotCoRNET$bslb == 0))

      # inconclusive zone
      polygon(c(0,
                dataPlotCoRNET$frctn[asbStart],
                dataPlotCoRNET$frctn,
                dataPlotCoRNET$frctn[c(bsbStart:bsbEnd)],
                dataPlotCoRNET$frctn[c(bsbEnd:bsbStart)],
                rev(dataPlotCoRNET$frctn),
                dataPlotCoRNET$frctn[asbStart],
                0),
              c(-10,
                -10,
                dataPlotCoRNET$aslb,
                dataPlotCoRNET$bslb[c(bsbStart:bsbEnd)],
                dataPlotCoRNET$bsub[c(bsbEnd:bsbStart)],
                rev(dataPlotCoRNET$asub),
                10,
                10),
              col = rgb(.4, .4, .4, .2),
              border = NA)

      # Benefit or harm zone
      polygon(c(dataPlotCoRNET$frctn,
                max(dataPlotCoRNET$frctn),
                dataPlotCoRNET$frctn[asbStart]),
              c(dataPlotCoRNET$aslb,
                -10, #min(dataPlotCoRNET$aslb),
                -10), #dataPlotCoRNET$aslb[aslbStart]),
              col = rgb(.0, .2, .7, .2),
              border = NA)

      polygon(c(dataPlotCoRNET$frctn,
                max(dataPlotCoRNET$frctn),
                dataPlotCoRNET$frctn[asbStart]),
              c(dataPlotCoRNET$asub,
                10, #min(dataPlotCoRNET$aslb),
                10), #dataPlotCoRNET$aslb[aslbStart]),
              col = rgb(.7, .3, .3, .2),
              border = NA)

      # Futility zone
      polygon(c(max(dataTSA$frctn),
                dataPlotCoRNET$frctn[c(bsbStart:bsbEnd)],
                dataPlotCoRNET$frctn[c(bsbEnd:bsbStart)],
                max(dataTSA$frctn)
      ),
      c(dataPlotCoRNET$bslb[bsbStart],
        dataPlotCoRNET$bslb[c(bsbStart:bsbEnd)],
        dataPlotCoRNET$bsub[c(bsbEnd:bsbStart)],
        dataPlotCoRNET$bsub[bsbStart]
      ),
      col = rgb(.1, .1, .1, .3),
      border = NA)

      # Established zone
      rect(1, -10,
           max(dataTSA$frctn), -2,
           col = rgb(.0, .1, .9, .4),
           border = NA)

      rect(1, 10,
           max(dataTSA$frctn), 2,
           col = rgb(.9, .2, .2, .4),
           border = NA)
    }

    if (lgcSmooth) {
      lines(dataPlotCoRNET$frctn, # sample
            dataPlotCoRNET$asub,
            lty = typLnASB,
            col = clrLnASB,
            lwd = szLnASB)
      lines(dataPlotCoRNET$frctn, # sample
            dataPlotCoRNET$aslb,
            lty = typLnASB,
            col = clrLnASB,
            lwd = szLnASB)
    } else {
      lines(dataTSA$frctnLmt, # nCum
            dataTSA$asub,
            lty = typLnASB,
            col = clrLnASB,
            lwd = szLnASB)
      lines(dataTSA$frctnLmt, # nCum
            dataTSA$aslb,
            lty = typLnASB,
            col = clrLnASB,
            lwd = szLnASB)
    }

    points(dataTSA[which(!is.na(dataTSA[, "source"]) & dataTSA[, "nCum"] < infoRIS), ]$frctn, # nCum
           dataTSA[which(!is.na(dataTSA[, "source"]) & dataTSA[, "nCum"] < infoRIS), ]$asub,
           pch = typPntASB,
           col = infoColorASB,
           bg  = clrPntASB,
           cex = szPntASB)
    points(dataTSA[which(!is.na(dataTSA[, "source"]) & dataTSA[, "nCum"] < infoRIS), ]$frctn, # nCum
           dataTSA[which(!is.na(dataTSA[, "source"]) & dataTSA[, "nCum"] < infoRIS), ]$aslb,
           pch = typPntASB,
           col = infoColorASB,
           bg  = clrPntASB,
           cex = szPntASB)

    if (infoBSB) {
      if (lgcSmooth) {
        lines(dataPlotCoRNET$frctn, # sample
              dataPlotCoRNET$bsub,
              lty = typLnASB,
              col = clrLnASB,
              lwd = szLnASB)

        lines(dataPlotCoRNET$frctn, # sample
              dataPlotCoRNET$bslb,
              lty = typLnASB,
              col = clrLnASB,
              lwd = szLnASB)
      } else {
        lines(dataTSA$frctn, # nCum
              dataTSA$bsub,
              lty = typLnASB,
              col = clrLnASB,
              lwd = szLnASB)
        lines(dataTSA$frctn, # nCum
              dataTSA$bslb,
              lty = typLnASB,
              col = clrLnASB,
              lwd = szLnASB)
      }
    }

    #segments(c(0),
    #         c(-2, 0, 2),
    #         c(max(dataTSA$nCum) * 1.1),
    #         c(-2, 0, 2),
    #         lty = c(2, 1, 2), lwd = 1, col = "gray25")
    segments(c(0),
             c(0),
             c(max(dataTSA$frctn) * 1.1), # nCum
             c(0),
             lty = typLn0,
             col = clrLn0,
             lwd = szLn0)
    segments(c(0),
             c(-2, 2),
             c(max(dataTSA$frctn) * 1.1), # nCum
             c(-2, 2),
             lty = typLnSig,
             col = clrLnSig,
             lwd = szLnSig)
    lines(dataTSA$frctn, # nCum
          dataTSA$zCum,
          lty = typLnZCum,
          col = clrLnZCum,
          lwd = szLnZCum)
    segments(c(0), c(0), dataTSA[1, "frctn"], dataTSA[1, "zCum"], # "nCum"
             col = clrLnZCum,
             lwd = szLnZCum,
             lty = typLnZCum)
    points(dataTSA$frctn, # nCum
           dataTSA$zCum,
           pch = typPntStdy,
           col = clrPntStdy,
           bg  = clrPntStdy,
           cex = szPntStdy) # szPntStdy + dataTSA$weight^2

    arrows(max(dataTSA$frctn), # nCum
           0,
           max(dataTSA$frctn) * 1.1, # nCum
           0,
           length = 0.1,
           lty = typLn0,
           col = clrLn0,
           lwd = szLn0)

    if (infoLgcLblStdy) {
      points(dataTSA$frctn, # nCum
             dataTSA$zCum - dataTSA$zCum,
             pch = "|",
             col = clrLblStdy,
             cex = 0.6)
      text(dataTSA$frctn, # nCum
           #ifelse(dataTSA$zCum > 0,
           #      dataTSA$zCum + 0.5,
           #      dataTSA$zCum - 0.5),
           ifelse(dataTSA$zCum > 0, -0.5, 0.5),
           dataTSA$source,
           #pos = infoPosLabel,
           pos = 6 - infoPosLabel,
           col = clrLblStdy,
           cex = szFntStdy,
           srt = anglStdy)
    }

    #text(dataTSA$time, dataTSA$zCum - 0.5,
    #     c(round(dataTSA$zCum, 2)),
    #     col = c("gray20"))

    if (!infoLgcZone) {
      rect(0,
           10,
           max(dataTSA$frctn) * 0.99, # infoRIS * 0.8
           8,
           lty = 0, col = rgb(1, 1, 1, 0.5))
    }

    #points(dataTSA[which(!is.na(dataTSA[, "source"])), ]$nCum,
    #       dataTSA[which(!is.na(dataTSA[, "source"])), ]$aslb,
    #       pch = typPntASB,
    #       col = infoColorASB,
    #       bg  = infoColorASB,
    #       cex = szPntASB)
    segments(c(max(dataTSA$frctn) * 0.01), #c(0.01),
             c(10),
             c(max(dataTSA$frctn) * 0.05), # infoRIS / 20
             c(10),
             lty = typLnZCum,
             col = clrLnZCum,
             lwd = szLnZCum)
    text(max(dataTSA$frctn) * 0.07, # infoRIS / 15
         10,
         paste("Cumulative z score", sep = ""),
         pos = 4,
         col = clrLgnd,
         cex = szFntLgnd)
    segments(c(max(dataTSA$frctn) * 0.01), # c(0.05),
             c(9),
             c(max(dataTSA$frctn) * 0.05), # c(infoRIS / 20)
             c(9),
             lty = typLnASB,
             col = clrLnASB,
             lwd = szLnASB)
    text(max(dataTSA$frctn) * 0.07, # infoRIS / 15
         9,
         paste("Alpha-spending boundary"),
         pos = 4,
         col = clrLgnd,
         cex = szFntLgnd)
    text(max(dataTSA$frctn) * 0.07, # infoRIS / 15
         8.3,
         paste("(",
               ifelse(infoMeasure %in% c("MD", "SMD"),
                      infoMeasure,
                      "Observed effect"),
               ifelse(abs(infoPES) < 0.001,
                      paste(" < 0.001", sep = ""),
                      paste(" = ", round(infoPES, 3), sep = "")),
               ifelse(infoMeasure %in% c("MD", "SMD"),
                      "",
                      paste("; RRR",
                            ifelse(infoRRR < 0.001,
                                   ifelse(infoRRR > 0,
                                          " < 0.1%",
                                          paste(" = ",
                                                -floor(infoRRR * 100),
                                                "%",
                                                sep = "")
                                   ),
                                   paste(" = ",
                                         floor(infoRRR * 100),
                                         "%",
                                         sep = "")
                            ),
                            sep = "")
               ),
               "; alpha: ", infoAlpha,
               "; power: ", round(1 - infoBeta, 2),
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
                            round(infoAF, 3), ")",
                            sep = "")
               ),
               sep = ""),
         pos = 4,
         col = clrLgnd,
         cex = szFntLgnd)

    segments(c(1), # infoRIS
             c(-9),
             c(1), # infoRIS
             c(9),
             lty = typLnRIS,
             col = clrLnRIS,
             lwd = szLnRIS)
    text(ifelse((infoCases / infoRIS) < 1,
                1.05,
                0.95), # 1, infoRIS
         -10,
         paste("RIS = ",
               ceiling(infoRIS),
               sep = ""),
         pos = ifelse((infoCases / infoRIS) < 1, 2, 4),
         col = clrLblRIS,
         cex = szFntRIS)

    segments(dataTSA$frctn[nrow(dataTSA[!is.na(dataTSA$source), ])],
             dataTSA$zCum[nrow(dataTSA[!is.na(dataTSA$source), ])],
             dataTSA$frctn[nrow(dataTSA[!is.na(dataTSA$source), ])],
             c(-8.5),
             lty = typLnRIS,
             col = clrLnZCum,
             lwd = szLnRIS)
    text(ifelse((infoCases / infoRIS) < 0.5,
                dataTSA$frctn[nrow(dataTSA[!is.na(dataTSA$source), ])] * 0.95,
                dataTSA$frctn[nrow(dataTSA[!is.na(dataTSA$source), ])] * 1.05), # dataTSA$frctn[nrow(dataTSA[!is.na(dataTSA$source), ])], # infoRIS,
         -9, # 9,
         paste("AIS = ",
               ceiling(max(dataTSA[which(!is.na(dataTSA[, "source"])), ]$nCum)),
               ifelse(infoLgcSAP == TRUE,
                      paste("\n",
                            "(SAP = ",
                            round(infoPwrObs, 3),
                            ")",
                            sep = ""),
                      ""),
               sep = ""),
         pos = ifelse((infoCases / infoRIS) < 0.5, 4, 2),
         col = clrLblAIS,
         cex = szFntAIS)
  })
}
