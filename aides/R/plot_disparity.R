#' @title Illustrate disparity plot.
#'
#' @author Enoch Kang
#'
#' @description
#' **PlotDisparity()** is a function for illustrating graphics of disparities in sample size analysis.
#'
#' @param object     OBJECT of the disparity test output in **disparity** class.
#' @param which      CHARACTER for indicating type of disparity plot. Current
#'                   version consists of five plots, including disparity plot of
#'                   variability and outliers based on:
#'                   (1) coefficient of variance ("CV"),
#'                   (2) IQR-outlier ("IQR"),
#'                   (3) Z-outlier ("Z"),
#'                   (4) GESD-outlier ("GESD"),
#'                   (5) MAD-outlier ("MAD").
#' @param lgcTtl     LOGIC value for indicating whether to show main title.
#' @param lgcTtlX    LOGIC value for indicating whether to show title on axis X.
#' @param lgcTtlY    LOGIC value for indicating whether to show title on axis Y.
#' @param lgcLgnd    LOGIC value for indicating whether to show legend.
#' @param lgcDtls    LOGIC value for indicating whether to show full information
#'                   of the disparity test rather than plot-related information.
#' @param lgcLblZn   LOGIC value for indicating whether to show labels of
#'                   variability zone.
#' @param txtLbl     CHARACTER for indicating numeric information of each study
#'                   disparity plot (outlier). Current version provides options
#'                   for no label (NULL), numbers of cases ("n"), numbers of
#'                   excessive cases ("n.excessive"), and proportion of excessive
#'                   cases ("prop.excessive").
#' @param szFntTtl   NUMERIC value for indicating font size of main title.
#' @param szFntTtlX  NUMERIC value for indicating font size of title on axis X.
#' @param szFntTtlY  NUMERIC value for indicating font size of title on axis Y.
#' @param szFntAxsX  NUMERIC value(s) for indicating font size of study label(s).
#' @param szFntEC    NUMERIC value(s) for indicating font size of study label(s)
#'                   for those studies with excessive case.
#' @param szFntAxsY  NUMERIC value for indicating font size of scale on axis Y.
#' @param szFntLgnd  NUMERIC value for indicating font size of legend.
#' @param szFntLbl   NUMERIC value(s) for indicating font size of label(s) for
#'                   observed value(s).
#' @param szFntLblEC NUMERIC value(s) for indicating font size of label(s) for
#'                   observed value(s) with excessive case.
#' @param szPnt     NUMERIC value(s) for indicating size(s) of observed point(s).
#' @param szPntEC   NUMERIC value for indicating size of observed point(s)
#'                  with excessive cases.
#' @param szPntNEC  NUMERIC value for indicating size of observed point(s)
#'                  without excessive cases.
#' @param typPltCV  CHARACTER for indicating sub-type of disparity plot for
#'                  showing variability. Current version provides two sub-types:
#'                  "half" and "full" plot.
#' @param typPnt    NUMERIC value(s) for indicating type(s) of observed point(s).
#' @param typPntEC  NUMERIC value for indicating type of observed point(s).
#'                  with excessive cases.
#' @param typPntNEC NUMERIC value for indicating type of observed point(s).
#'                  without excessive cases.
#' @param typLn0    NUMERIC value for indicating type of horizontal line for no
#'                  excessive case.
#' @param typLnEC   NUMERIC value for indicating type of vertical line(s) for
#'                  excessive case(s).
#' @param clrTtl    CHARACTER of a color name for main title.
#' @param clrTtlX   CHARACTER of a color name for title on axis X.
#' @param clrTtlY   CHARACTER of a color name for title on axis Y.
#' @param clrAxsX   CHARACTER of color name(s) for study label.
#' @param clrAxsY   CHARACTER of a color name for scale on axis Y.
#' @param clrLgnd   CHARACTER of a color name for legend.
#' @param clrVrtnL  CHARACTER of a color name for low variability zone.
#' @param clrVrtnM  CHARACTER of a color name for moderate variability zone.
#' @param clrVrtnH  CHARACTER of a color name for high variability zone.
#' @param clrLblZn  CHARACTER of color name(s) for variability zone(s).
#' @param clrLbl    CHARACTER of color name(s) for observed value(s).
#' @param clrLblEC  CHARACTER of color name(s) for observed value(s) of studies with excessive cases.
#' @param clrPnt    CHARACTER of color name(s) for every observed point.
#' @param clrPntEC  CHARACTER of a color name for proportion of excessive cases.
#' @param clrPntNEC CHARACTER of a color name for observed point without
#'                  excessive case.
#' @param clrLn0    CHARACTER of a color name for horizontal line of no excessive
#'                  case.
#' @param clrLnEC   CHARACTER of color name for vertical line(s) of excessive
#'                  case(s).
#' @param clrLnCV   CHARACTER of color name for line of the association between
#'                  standard deviation and cases.
#' @param anglAxsX  NUMERIC value between 0 and 360 for indicating angle of study
#'                  labels on x axis on the disparity plot (outlier).
#' @param anglLbl   NUMERIC value between 0 and 360 for indicating angle of
#'                  observed values on the disparity plot (outlier).
#' @param sort      CHARACTER of data sorting reference for disparity plot.
#'                  Currentversion consists of "time", "size", and "excessive"
#'                  for displaying observations on disparity plot of outlier(s).
#'
#'
#' @return
#' **PlotDisparity()** returns a disparity plot.
#'
#'
#' @references
#' Shapiro, S. S., & Wilk, M. B. (1965). An analysis of variance test for
#' normality (complete samples). **Biometrika**, *52(3)*, 591-611.
#'
#' Rosner, B. (1983). Percentage Points for a Generalized ESD Many-Outlier
#' Procedure. **Technometrics**, *25(2)*, 165-172.
#'
#' Rousseeuw, P. J.  & Croux C. (1993). Alternatives to the Median Absolute
#' Deviation, **Journal of the American Statistical Association**, *88(424)*,
#' 1273-1283. http://dx.doi.org/10.1080/01621459.1993.10476408
#'
#' Hendricks, W. A., & Robey, K. W. (1936). The sampling distribution of the
#' coefficient of variation. **The Annals of Mathematical Statistics**, *7(3)*,
#' 129-132.
#'
#' Sokal, R. R., & Braumann, C. A. (1980). Significance tests for coefficients
#' of variation and variability profiles. **Systematic Biology**, *29(1)*, 50-66.
#'
#'
#' @seealso \code{\link{TestDisparity}}
#'
#'
#' @export PlotDisparity



PlotDisparity <- function(object,
                          which      = NULL,
                          lgcTtl     = TRUE,
                          lgcTtlX    = TRUE,
                          lgcTtlY    = TRUE,
                          lgcLgnd    = TRUE,
                          lgcDtls    = FALSE,
                          lgcLblZn   = TRUE,
                          txtLbl     = NULL,
                          szFntTtl   = NULL,
                          szFntTtlX  = NULL,
                          szFntTtlY  = NULL,
                          szFntAxsX  = NULL,
                          szFntEC    = NULL,
                          szFntAxsY  = NULL,
                          szFntLgnd  = NULL,
                          szFntLbl   = NULL,
                          szFntLblEC = NULL,
                          szPnt      = NULL,
                          szPntEC    = NULL,
                          szPntNEC   = NULL,
                          typPltCV   = NULL,
                          typPnt     = NULL,
                          typPntEC   = NULL,
                          typPntNEC  = NULL,
                          typLn0     = NULL,
                          typLnEC    = NULL,
                          clrTtl     = NULL,
                          clrTtlX    = NULL,
                          clrTtlY    = NULL,
                          clrAxsX    = NULL,
                          clrAxsY    = NULL,
                          clrLgnd    = NULL,
                          clrVrtnL   = NULL,
                          clrVrtnM   = NULL,
                          clrVrtnH   = NULL,
                          clrLblZn   = NULL,
                          clrLbl     = NULL,
                          clrLblEC   = NULL,
                          clrPnt     = NULL,
                          clrPntEC   = NULL,
                          clrPntNEC  = NULL,
                          clrLn0     = NULL,
                          clrLnEC    = NULL,
                          clrLnCV    = NULL,
                          anglAxsX   = NULL,
                          anglLbl    = NULL,
                          sort       = NULL) {


  # 01. CHECK core arguments -----
  lgcObject <- ifelse(inherits(object, "disparity"),
                         FALSE, TRUE)

  if (lgcObject) {
    infoStopObject <- 'Argument for parameter `object` must be a `TestDisparity()` output that is an object in **disparity** class.'
    }



  # 02. RETURN results of core argument checking  -----
  if (lgcObject)

    stop(infoStopObject)



  # 03. DEFINE core data -----

  dataDsprty           <- object$data.disparity
  infoNumStdy          <- nrow(dataDsprty)
  infoCases            <- sum(dataDsprty$n)
  infoMCases           <- mean(dataDsprty$n)
  infoSDCases          <- sd(dataDsprty$n)
  #infoCV0.1            <- infoMCases * 0.1
  #infoCV0.3            <- infoMCases * 0.3
  #infoCasesMSD3CV0.1   <- infoMCases - infoCV0.1 * 3  # minus SD 3
  #infoCasesPSD3CV0.1   <- infoMCases + infoCV0.1 * 3  # plus SD 3
  #infoCasesMSD3CV0.3   <- infoMCases - infoCV0.3 * 3  # minus SD 3
  #infoCasesPSD3CV0.3   <- infoMCases + infoCV0.3 * 3  # plus SD 3
  #infoCasesMSD3.5CV0.1 <- infoMCases - infoCV0.1 * 3.5  # minus SD 3.5
  #infoCasesPSD3.5CV0.1 <- infoMCases + infoCV0.1 * 3.5  # plus SD 3.5
  #infoCasesMSD3.5CV0.3 <- infoMCases - infoCV0.3 * 3.5  # minus SD 3.5
  #infoCasesPSD3.5CV0.3 <- infoMCases + infoCV0.3 * 3.5  # plus SD 3.5
  #infoCasesMSD4CV0.1   <- infoMCases - infoCV0.1 * 4  # minus SD 4
  #infoCasesPSD4CV0.1   <- infoMCases + infoCV0.1 * 4  # plus SD 4
  #infoCasesMSD4CV0.3   <- infoMCases - infoCV0.3 * 4  # minus SD 4
  #infoCasesPSD4CV0.3   <- infoMCases + infoCV0.3 * 4  # plus SD 4
  #infoMSDOrg           <- -infoCasesMSD3CV0.1 / infoSDCases
  #infoPSDOrg           <- infoCasesMSD3CV0.1 / infoSDCases
  #infoMSDCV0.3         <- -infoCasesMSD3CV0.1 / infoCV0.3 #(infoMCases - infoCV0.3)
  #infoPSDCV0.3         <- infoCasesMSD3CV0.1 / infoCV0.3 #(infoMCases - infoCV0.3)

  infoCutoffL          <- object$ctf.lwr.cv
  infoCutoffH          <- object$ctf.upr.cv
  infoCVL              <- infoMCases * infoCutoffL
  infoCVH              <- infoMCases * infoCutoffH
  infoCasesMSD3CVL     <- infoMCases - infoCVL * 3  # minus SD 3
  infoCasesPSD3CVL     <- infoMCases + infoCVL * 3  # plus SD 3
  infoCasesMSD3CVH     <- infoMCases - infoCVH * 3  # minus SD 3
  infoCasesPSD3CVH     <- infoMCases + infoCVH * 3  # plus SD 3
  infoCasesMSD3.5CVL   <- infoMCases - infoCVL * 3.5  # minus SD 3.5
  infoCasesPSD3.5CVL   <- infoMCases + infoCVL * 3.5  # plus SD 3.5
  infoCasesMSD3.5CVH   <- infoMCases - infoCVH * 3.5  # minus SD 3.5
  infoCasesPSD3.5CVH   <- infoMCases + infoCVH * 3.5  # plus SD 3.5
  infoCasesMSD4CVL     <- infoMCases - infoCVL * 4  # minus SD 4
  infoCasesPSD4CVL     <- infoMCases + infoCVL * 4  # plus SD 4
  infoCasesMSD4CVH     <- infoMCases - infoCVH * 4  # minus SD 4
  infoCasesPSD4CVH     <- infoMCases + infoCVH * 4  # plus SD 4
  infoMSDCVL           <- -infoCasesMSD3CVL / infoSDCases
  infoPSDCVL           <- infoCasesMSD3CVL / infoSDCases
  infoMSDCVH           <- -infoCasesMSD3CVL / infoCVH #(infoMCases - infoCV0.3)
  infoPSDCVH           <- infoCasesMSD3CVL / infoCVH #(infoMCases - infoCV0.3)

  infoVrblty           <- object$variability
  infoPValVrblty       <- object$p.variability
  infoLCIVrblty        <- object$lci.variability
  infoUCIVrblty        <- object$uci.variability

  infoUnbsCV           <- object$cv.unbiased
  infoStatsT           <- object$t.cv
  infoPValCV           <- object$p.cv

  infoRCVMAD           <- object$cv.robust.MAD
  infoPvaRCVMAD        <- object$p.cv.robust.MAD
  infoLCIRCVMAD        <- object$lci.cv.robust.MAD
  infoUCIRCVMAD        <- object$uci.cv.robust.MAD

  setPar <- par(no.readonly = TRUE)
  on.exit(par(setPar))
  infoLgcWarning <- getOption("warn")
  options(warn = -1)
  on.exit(options(warn = infoLgcWarning))

  # 04. CHECK additive arguments -----
  lgcWhich     <- ifelse(is.null(which),
                         FALSE,
                         ifelse(which %in% c("CV", "RCV",
                                             "IQR", "Z", "GESD",
                                             "MAD"),
                                FALSE, TRUE)
                         )

  lgcLgcTtl    <- ifelse(is.logical(lgcTtl),
                         FALSE, TRUE)

  lgcLgcTtlX   <- ifelse(is.logical(lgcTtlX),
                         FALSE, TRUE)

  lgcLgcTtlY   <- ifelse(is.logical(lgcTtlY),
                         FALSE, TRUE)

  lgcLgcLgnd   <- ifelse(is.logical(lgcLgnd),
                         FALSE, TRUE)

  lgcLgcDtls   <- ifelse(is.logical(lgcDtls),
                         FALSE, TRUE)

  lgcLgcLblZn  <- ifelse(is.logical(lgcLblZn),
                         FALSE, TRUE)

  lgcTxtLbl    <- ifelse(is.null(txtLbl),
                         FALSE,
                         ifelse(txtLbl %in% c("n",
                                              "n.excessive",
                                              "prop.excessive"),
                                FALSE, TRUE)
                         )

  lgcSzFntTtl  <- ifelse(is.null(szFntTtl),
                         FALSE,
                         ifelse(length(szFntTtl) != 1,
                                TRUE,
                                ifelse(base::isFALSE(is.numeric(szFntTtl)),
                                       TRUE,
                                       ifelse(base::isFALSE(szFntTtl >= 0),
                                              TRUE,
                                              ifelse(szFntTtl < 6,
                                                     FALSE, TRUE))))
                         )

  lgcSzFntTtlX <- ifelse(is.null(szFntTtlX),
                         FALSE,
                         ifelse(length(szFntTtlX) != 1,
                                TRUE,
                                ifelse(base::isFALSE(is.numeric(szFntTtlX)),
                                       TRUE,
                                       ifelse(base::isFALSE(szFntTtlX >= 0),
                                              TRUE,
                                              ifelse(szFntTtlX < 6,
                                                     FALSE, TRUE))))
                         )

  lgcSzFntTtlY <- ifelse(is.null(szFntTtlY),
                         FALSE,
                         ifelse(length(szFntTtlY) != 1,
                                TRUE,
                                ifelse(base::isFALSE(is.numeric(szFntTtlY)),
                                       TRUE,
                                       ifelse(base::isFALSE(szFntTtlY >= 0),
                                              TRUE,
                                              ifelse(szFntTtlY < 6,
                                                     FALSE, TRUE))))
                         )

  lgcSzFntAxsX <- ifelse(is.null(szFntAxsX),
                         FALSE,
                         ifelse(base::isFALSE(length(szFntAxsX) == 1 | length(szFntAxsX) == infoNumStdy),
                                TRUE,
                                ifelse(base::isFALSE(is.numeric(szFntAxsX)),
                                       TRUE,
                                       ifelse(FALSE %in% (szFntAxsX >= 0),
                                              TRUE,
                                              ifelse(FALSE %in% (szFntAxsX < 6),
                                                     TRUE, FALSE))))
                         )

  lgcSzFntEC <- ifelse(is.null(szFntEC),
                         FALSE,
                         ifelse(length(szFntEC) != 1,
                                TRUE,
                                ifelse(base::isFALSE(is.numeric(szFntEC)),
                                       TRUE,
                                       ifelse(base::isFALSE(szFntEC >= 0),
                                              TRUE,
                                              ifelse(szFntEC < 6,
                                                     FALSE, TRUE))))
                         )

  lgcSzFntAxsY <- ifelse(is.null(szFntAxsY),
                         FALSE,
                         ifelse(length(szFntAxsY) != 1,
                                TRUE,
                                ifelse(base::isFALSE(is.numeric(szFntAxsY)),
                                       TRUE,
                                       ifelse(FALSE %in% (szFntAxsY >= 0),
                                              TRUE,
                                              ifelse(FALSE %in% (szFntAxsY < 6),
                                                     TRUE, FALSE))))
                         )

  lgcSzFntLgnd <- ifelse(is.null(szFntLgnd),
                         FALSE,
                         ifelse(length(szFntLgnd) != 1,
                                TRUE,
                                ifelse(base::isFALSE(is.numeric(szFntLgnd)),
                                       TRUE,
                                       ifelse(FALSE %in% (szFntLgnd >= 0),
                                              TRUE,
                                              ifelse(FALSE %in% (szFntLgnd < 6),
                                                     TRUE, FALSE))))
                         )

  lgcSzFntLbl <- ifelse(is.null(szFntLbl),
                         FALSE,
                         ifelse(length(szFntLbl) != 1,
                                TRUE,
                                ifelse(base::isFALSE(is.numeric(szFntLbl)),
                                       TRUE,
                                       ifelse(FALSE %in% (szFntLbl >= 0),
                                              TRUE,
                                              ifelse(FALSE %in% (szFntLbl < 6),
                                                     TRUE, FALSE))))
                         )

  lgcSzFntLblEC <- ifelse(is.null(szFntLblEC),
                          FALSE,
                          ifelse(length(szFntLblEC) != 1,
                                 TRUE,
                                 ifelse(base::isFALSE(is.numeric(szFntLblEC)),
                                        TRUE,
                                        ifelse(FALSE %in% (szFntLblEC >= 0),
                                               TRUE,
                                               ifelse(FALSE %in% (szFntLblEC < 6),
                                                      TRUE, FALSE))))
                          )

  lgcSzPnt     <- ifelse(is.null(szPnt),
                         FALSE,
                         ifelse(base::isFALSE(length(szPnt) == 1 | length(szPnt) == infoNumStdy),
                                TRUE,
                                ifelse(base::isFALSE(is.numeric(szPnt)),
                                       TRUE,
                                       ifelse(FALSE %in% (szPnt >= 0),
                                              TRUE,
                                              ifelse(FALSE %in% (szPnt < 6),
                                                     TRUE, FALSE))))
                         )

  lgcSzPntEC   <- ifelse(is.null(szPntEC),
                         FALSE,
                         ifelse(length(szPntEC) != 1,
                                TRUE,
                                ifelse(base::isFALSE(is.numeric(szPntEC)),
                                       TRUE,
                                       ifelse(FALSE %in% (szPntEC >= 0),
                                              TRUE,
                                              ifelse(FALSE %in% (szPntEC < 6),
                                                     TRUE, FALSE))))
                         )

  lgcSzPntNEC  <- ifelse(is.null(szPntNEC),
                         FALSE,
                         ifelse(length(szPntNEC) != 1,
                                TRUE,
                                ifelse(base::isFALSE(is.numeric(szPntNEC)),
                                       TRUE,
                                       ifelse(FALSE %in% (szPntNEC >= 0),
                                              TRUE,
                                              ifelse(FALSE %in% (szPntNEC < 6),
                                                     TRUE, FALSE))))
                         )

  lgcTypPltCV  <- ifelse(is.null(typPltCV),
                         FALSE,
                         ifelse(typPltCV %in% c("half", "full"),
                                FALSE, TRUE)
                         )

  lgcTypPnt    <- ifelse(is.null(typPnt),
                         FALSE,
                         ifelse(base::isFALSE(length(typPnt) == 1 | length(typPnt) == infoNumStdy),
                                TRUE,
                                ifelse(FALSE %in% (typPnt %in% c(1:5)),
                                       TRUE, FALSE))
                         )

  lgcTypPntEC  <- ifelse(is.null(typPntEC),
                         FALSE,
                         ifelse(length(typPntEC) != 1,
                                TRUE,
                                ifelse(FALSE %in% (typPntEC %in% c(1:5)),
                                       TRUE, FALSE))
                         )

  lgcTypPntNEC <- ifelse(is.null(typPntNEC),
                         FALSE,
                         ifelse(length(typPntNEC) != 1,
                                TRUE,
                                ifelse(FALSE %in% (typPntNEC %in% c(1:5)),
                                       TRUE, FALSE))
                         )

  lgcTypLn0    <- ifelse(is.null(typLn0),
                         FALSE,
                         ifelse(length(typLn0) != 1,
                                TRUE,
                                ifelse(FALSE %in% (typLn0 %in% c(0:6)),
                                       TRUE, FALSE))
                         )

  lgcTypLnEC   <- ifelse(is.null(typLnEC),
                         FALSE,
                         ifelse(length(typLnEC) != 1,
                                TRUE,
                                ifelse(FALSE %in% (typLnEC %in% c(0:6)),
                                       TRUE, FALSE))
                         )

  lgcClrTtl    <- ifelse(is.null(clrTtl),
                         FALSE,
                         ifelse(length(clrTtl) != 1,
                                TRUE,
                                ifelse(FALSE %in% (clrTtl %in% colors()),
                                       TRUE, FALSE))
                         )

  lgcClrTtlX   <- ifelse(is.null(clrTtlX),
                         FALSE,
                         ifelse(length(clrTtlX) != 1,
                                TRUE,
                                ifelse(FALSE %in% (clrTtlX %in% colors()),
                                       TRUE, FALSE))
                         )

  lgcClrTtlY   <- ifelse(is.null(clrTtlY),
                         FALSE,
                         ifelse(length(clrTtlY) != 1,
                                TRUE,
                                ifelse(FALSE %in% (clrTtlY %in% colors()),
                                       TRUE, FALSE))
                         )

  lgcClrAxsX   <- ifelse(is.null(clrAxsX),
                         FALSE,
                         ifelse(base::isFALSE(length(clrAxsX) == 1  | length(clrAxsX) == infoNumStdy),
                                TRUE,
                                ifelse(FALSE %in% (clrAxsX %in% colors()),
                                       TRUE, FALSE))
                         )

  lgcClrAxsY   <- ifelse(is.null(clrAxsY),
                         FALSE,
                         ifelse(length(clrAxsY) != 1,
                                TRUE,
                                ifelse(FALSE %in% (clrAxsY %in% colors()),
                                       TRUE, FALSE))
                         )

  lgcClrLgnd   <- ifelse(is.null(clrLgnd),
                         FALSE,
                         ifelse(length(clrLgnd) != 1,
                                TRUE,
                                ifelse(FALSE %in% (clrLgnd %in% colors()),
                                       TRUE, FALSE))
                         )

  lgcClrVrtnL    <- ifelse(is.null(clrVrtnL),
                         FALSE,
                         ifelse(base::isFALSE(length(clrVrtnL) == 1),
                                TRUE,
                                ifelse(FALSE %in% (clrVrtnL %in% colors()),
                                       TRUE, FALSE))
                         )

  lgcClrVrtnM    <- ifelse(is.null(clrVrtnM),
                         FALSE,
                         ifelse(base::isFALSE(length(clrVrtnM) == 1),
                                TRUE,
                                ifelse(FALSE %in% (clrVrtnM %in% colors()),
                                       TRUE, FALSE))
                         )

  lgcClrVrtnH    <- ifelse(is.null(clrVrtnH),
                         FALSE,
                         ifelse(base::isFALSE(length(clrVrtnH) == 1),
                                TRUE,
                                ifelse(FALSE %in% (clrVrtnH %in% colors()),
                                       TRUE, FALSE))
                         )

  lgcClrLblZn    <- ifelse(is.null(clrLblZn),
                         FALSE,
                         ifelse(base::isFALSE(length(clrLblZn) == 1 | length(clrLblZn) == 3),
                                TRUE,
                                ifelse(FALSE %in% (clrLblZn %in% colors()),
                                       TRUE, FALSE))
                         )

  lgcClrLbl     <- ifelse(is.null(clrLbl),
                         FALSE,
                         ifelse(base::isFALSE(length(clrLbl) == 1 | length(clrLbl) == infoNumStdy),
                                TRUE,
                                ifelse(FALSE %in% (clrLbl %in% colors()),
                                       TRUE, FALSE))
                         )

  lgcClrLblEC  <- ifelse(is.null(clrLblEC),
                         FALSE,
                         ifelse(length(clrLblEC) != 1,
                                TRUE,
                                ifelse(FALSE %in% (clrLblEC %in% colors()),
                                       TRUE, FALSE))
                         )

  lgcClrPnt    <- ifelse(is.null(clrPnt),
                         FALSE,
                         ifelse(base::isFALSE(length(clrPnt) == 1 | length(clrPnt) == infoNumStdy),
                                TRUE,
                                ifelse(FALSE %in% (clrPnt %in% colors()),
                                       TRUE, FALSE))
                         )

  lgcClrPntEC  <- ifelse(is.null(clrPntEC),
                         FALSE,
                         ifelse(length(clrPntEC) != 1,
                                TRUE,
                                ifelse(FALSE %in% (clrPntEC %in% colors()),
                                       TRUE, FALSE))
                         )

  lgcClrPntNEC <- ifelse(is.null(clrPntNEC),
                         FALSE,
                         ifelse(length(clrPntNEC) != 1,
                                TRUE,
                                ifelse(FALSE %in% (clrPntNEC %in% colors()),
                                       TRUE, FALSE))
                         )

  lgcClrLn0    <- ifelse(is.null(clrLn0),
                         FALSE,
                         ifelse(length(clrLn0) != 1,
                                TRUE,
                                ifelse(FALSE %in% (clrLn0 %in% colors()),
                                       TRUE, FALSE))
                         )

  lgcClrLnEC   <- ifelse(is.null(clrLnEC),
                         FALSE,
                         ifelse(length(clrLnEC) != 1,
                                TRUE,
                                ifelse(FALSE %in% (clrLnEC %in% colors()),
                                       TRUE, FALSE))
                         )

  lgcClrLnCV   <- ifelse(is.null(clrLnCV),
                         FALSE,
                         ifelse(length(clrLnCV) != 1,
                                TRUE,
                                ifelse(FALSE %in% (clrLnCV %in% colors()),
                                       TRUE, FALSE))
                         )

  lgcAnglAxsX  <- ifelse(is.null(anglAxsX),
                         FALSE,
                         ifelse(length(anglAxsX) != 1,
                                TRUE,
                                ifelse(anglAxsX < 0,
                                       TRUE,
                                       ifelse(anglAxsX > 360,
                                                    TRUE, FALSE)))
                         )

  lgcAnglLbl   <- ifelse(is.null(anglLbl),
                         FALSE,
                         ifelse(length(anglLbl) != 1,
                                TRUE,
                                ifelse(anglLbl < 0,
                                       TRUE,
                                       ifelse(anglLbl > 360,
                                              TRUE, FALSE)))
                         )

  lgcSort      <- ifelse(is.null(sort), FALSE,
                         ifelse(length(sort) == 1,
                                ifelse(sort %in% c("time", "size", "excessive"),
                                       FALSE, TRUE),
                                TRUE)
                         )

  if (lgcWhich) {
    infoStopWhich     <- 'Argument for parameter `which` must be characters ("CV", "RCV", "IQR", "Z", "GESD", or "MAD") for indicating type of disparity plot.'
    }

  if (lgcLgcTtl) {
    infoStopTtl       <- 'Argument for parameter `lgcTtl` must be a logical value in terms of "TRUE" or "FALSE" for indicating whether to show main title on the plot.'
    }

  if (lgcLgcTtlX) {
    infoStopTtlX      <- 'Argument for parameter `lgcTtlX` must be a logical value in terms of "TRUE" or "FALSE" for indicating whether to show title of axis X.'
    }

  if (lgcLgcTtlY) {
    infoStopTtlY      <- 'Argument for parameter `lgcTtlY` must be a logical value in terms of "TRUE" or "FALSE" for indicating whether to show title of axis Y.'
    }

  if (lgcLgcLgnd) {
    infoStopLgnd      <- 'Argument for parameter `lgcLgnd` must be a logical value in terms of "TRUE" or "FALSE" for indicating whether to show legend on the plot.'
    }

  if (lgcLgcDtls) {
    infoStopDtls      <- 'Argument for parameter `lgcDtls` must be a logical value in terms of "TRUE" or "FALSE" for indicating whether to show full summary legend on the plot.'
    }

  if (lgcLgcLblZn) {
    infoStopLblZn     <- 'Argument for parameter `lgcLblZn` must be a logical value in terms of "TRUE" or "FALSE" for indicating whether to show labels of variability zone.'
    }

  if (lgcTxtLbl) {
    infoStopTxtLbl    <- 'Argument for parameter `txtLbl` must be characters ("n", "n.excessive", or "prop.excessive") for indicating which information of each study would like to be shown on disparity plot (outlier).'
    }

  if (lgcSzFntTtl) {
    infoStopSzFntTtl  <- 'Argument for parameter `szFntTtl` must be a numeric value between 0 and 5 for indicating font size of the main title.'
    }

  if (lgcSzFntTtlX) {
    infoStopSzFntTtlX <- 'Argument for parameter `szFntTtlX` must be a numeric value between 0 and 5 for indicating font size of the title on axis X.'
    }

  if (lgcSzFntTtlY) {
    infoStopSzFntTtlY <- 'Argument for parameter `szFntTtlY` must be a numeric value between 0 and 5 for indicating font size of the title on axis Y.'
    }

  if (lgcSzFntAxsX) {
    infoStopSzFntAxsX <- 'Argument for parameter `szFntAxsX` must be numeric value(s) between 0 and 5 for indicating font size of study label on axis X.'
    }

  if (lgcSzFntEC) {
    infoStopSzFntEC   <- 'Argument for parameter `szFntEC` must be a numeric value between 0 and 5 for indicating font size of study label on axis X for those studies with excessive cases.'
    }

  if (lgcSzFntAxsY) {
    infoStopSzFntAxsY <- 'Argument for parameter `szFntAxsY` must be a numeric value between 0 and 5 for indicating font size of axis Y.'
    }

  if (lgcSzFntLgnd) {
    infoStopSzFntLgnd <- 'Argument for parameter `szFntLgnd` must be a numeric value between 0 and 5 for indicating font size of plot legend.'
    }

  if (lgcSzFntLbl) {
    infoStopSzFntLbl  <- 'Argument for parameter `szFntLbl` must be a numeric value between 0 and 5 for indicating font size of observed values.'
    }

  if (lgcSzFntLblEC) {
    infoStopSzFntLblEC <- 'Argument "szFntLblEC" must be a numeric value between 0 and 5 for indicating font size of observed values among the studies with excessive cases.'
    }

  if (lgcSzPnt) {
    infoStopSzPnt     <- 'Argument for parameter `szPnt` must be numeric value(s) between 0 and 5 for indicating size(s) of observed point(s).'
    }

  if (lgcSzPntEC) {
    infoStopSzPntEC   <- 'Argument for parameter `szPntEC` must be a numeric value between 0 and 5 for indicating size of observed point(s) with excessive cases.'
    }

  if (lgcSzPntNEC) {
    infoStopSzPntNEC  <- 'Argument for parameter `szPntNEC` must be a numeric value between 0 and 5 for indicating size of observed point(s) without excessive cases.'
    }

  if (lgcTypPltCV) {
    infoStopTypPltCV  <- 'Argument for parameter `typPltCV` must be characters ("half" or "full") for indicating sub-type of disparity plot for variability.'
    }

  if (lgcTypPnt) {
    infoStopTypPnt    <- 'Argument for parameter `typPnt` must be integer(s) between 1 and 5 for indicating shape(s) of observed point(s).'
    }

  if (lgcTypPntEC) {
    infoStopTypPntEC  <- 'Argument for parameter `typPntEC` must be an integer between 1 and 5 for indicating shape of observed point(s) with excessive cases.'
    }

  if (lgcTypPntNEC) {
    infoStopTypPntNEC <- 'Argument for parameter `typPntNEC` must be an integer between 1 and 5 for indicating shape of observed point(s) without excessive cases.'
    }

  if (lgcTypLn0) {
    infoStopTypLn0    <- 'Argument for parameter `typLn0` must be an integer between 1 and 6 for indicating segment type of null line.'
    }

  if (lgcTypLnEC) {
    infoStopTypLnEC   <- 'Argument for parameter `typLnEC` must be an integer between 1 and 6 for indicating segment type of observed point(s) with excessive cases.'
    }

  if (lgcClrTtl) {
    infoStopClrTtl    <- 'Argument for parameter `clrTtl` must be a color name for coloring main title.'
    }

  if (lgcClrTtlX) {
    infoStopClrTtlX   <- 'Argument for parameter `clrTtlX` must be a color name for coloring the title on axis X.'
    }

  if (lgcClrTtlY) {
    infoStopClrTtlY   <- 'Argument for parameter `clrTtlY` must be a color name for coloring the title on axis Y.'
    }

  if (lgcClrAxsX) {
    infoStopClrAxsX   <- 'Argument for parameter `clrAxsX` must be color name(s) for coloring the axis X.'
    }

  if (lgcClrAxsY) {
    infoStopClrAxsY   <- 'Argument for parameter `clrAxsY` must be a color name for coloring the axis Y.'
    }

  if (lgcClrLgnd) {
    infoStopClrLgnd   <- 'Argument for parameter `clrLgnd` must be a color name for coloring plot legend.'
    }

  if (lgcClrVrtnL) {
    infoStopClrVrtnL  <- 'Argument for parameter `clrVrtnL` must be a color name for coloring low variability zone.'
    }

  if (lgcClrVrtnM) {
    infoStopClrVrtnM  <- 'Argument for parameter `clrVrtnM` must be a color name for coloring moderate variability zone.'
    }

  if (lgcClrVrtnH) {
    infoStopClrVrtnH  <- 'Argument for parameter `clrVrtnH` must be a color name for coloring high variability zone.'
    }

  if (lgcClrLblZn) {
    infoStopClrLblZn  <- 'Argument for parameter `clrLblZn` must be color name(s) for coloring variability zone(s).'
    }

  if (lgcClrLbl) {
    infoStopClrLbl    <- 'Argument for parameter `clrLbl` must be color name(s) for coloring observed value(s).'
    }

  if (lgcClrLblEC) {
    infoStopClrLblEC  <- 'Argument for parameter `clrLblEC` must be a color name for coloring observed value(s) of the studies with excessive cases.'
    }

  if (lgcClrPnt) {
    infoStopClrPnt    <- 'Argument for parameter `clrPnt` must be color name(s) for coloring observed point(s).'
    }

  if (lgcClrPntEC) {
    infoStopClrPntEC  <- 'Argument for parameter `clrPntEC` must be a color name for coloring observed point(s) with excessive cases.'
    }

  if (lgcClrPntNEC) {
    infoStopClrPntNEC <- 'Argument for parameter `clrPntNEC` must be a color name for coloring observed point(s) without excessive cases.'
    }

  if (lgcClrLn0) {
    infoStopClrLn0    <- 'Argument for parameter `clrLn0` must be a color name for coloring segment of null line.'
    }

  if (lgcClrLnEC) {
    infoStopClrLnEC   <- 'Argument for parameter `clrLnEC` must be a color name for coloring segment of observed point(s) with excessive cases.'
    }

  if (lgcClrLnCV) {
    infoStopClrLnCV   <- 'Argument for parameter `clrLnCV` must be a color name for coloring line of the association between standard deviation and cases.'
    }

  if (lgcAnglAxsX) {
    infoStopAnglAxsX  <- 'Argument for parameter `anglAxsX` must be a numeric value between 0 and 360 for indicating angle of study labels on disparity plot (outlier).'
    }

  if (lgcAnglLbl) {
    infoStopAnglLbl   <- 'Argument for parameter `anglLbl` must be a numeric value between 0 and 360 for indicating angle of observed values on disparity plot (outlier).'
    }

  if (lgcSort) {
    infoStopSort      <- 'Argument for parameter `sort` must be characters ("time", "size", or "excessive") for indicating data sort reference in order to display disparity plot.'
    }



  # 05. RETURN results of argument checking  -----
  if (lgcWhich |
      lgcLgcTtl | lgcLgcTtlX | lgcLgcTtlY |
      lgcLgcLgnd | lgcLgcDtls | lgcLgcLblZn | lgcTxtLbl |
      lgcSzFntTtl | lgcSzFntTtlX | lgcSzFntTtlY |
      lgcSzFntAxsX | lgcSzFntEC |lgcSzFntAxsY |
      lgcSzFntLgnd | lgcSzFntLbl | lgcSzFntLblEC |
      lgcSzPnt | lgcSzPntEC | lgcSzPntNEC |
      lgcTypPltCV | lgcTypPnt | lgcTypPntEC | lgcTypPntNEC |
      lgcTypLn0 | lgcTypLnEC |
      lgcClrTtl | lgcClrTtlX | lgcClrTtlY |
      lgcClrAxsX | lgcClrAxsY | lgcClrLgnd |
      lgcClrVrtnL | lgcClrVrtnM | lgcClrVrtnH | lgcClrLblZn |
      lgcClrLbl | lgcClrLblEC |
      lgcClrPnt | lgcClrPntEC | lgcClrPntNEC |
      lgcClrLn0 | lgcClrLnEC | lgcClrLnCV |
      lgcAnglAxsX | lgcAnglLbl | lgcSort)

    stop(paste(ifelse(lgcWhich, paste(infoStopWhich, "\n", sep = ""), ""),
               ifelse(lgcLgcTtl, paste(infoStopTtl, "\n", sep = ""), ""),
               ifelse(lgcLgcTtlX, paste(infoStopTtlX, "\n", sep = ""), ""),
               ifelse(lgcLgcTtlY, paste(infoStopTtlY, "\n", sep = ""), ""),
               ifelse(lgcLgcLgnd, paste(infoStopLgnd, "\n", sep = ""), ""),
               ifelse(lgcLgcDtls, paste(infoStopDtls, "\n", sep = ""), ""),
               ifelse(lgcLgcLblZn, paste(infoStopLblZn, "\n", sep = ""), ""),
               ifelse(lgcTxtLbl, paste(infoStopTxtLbl, "\n", sep = ""), ""),
               ifelse(lgcSzFntTtl, paste(infoStopSzFntTtl, "\n", sep = ""), ""),
               ifelse(lgcSzFntTtlX, paste(infoStopSzFntTtlX, "\n", sep = ""), ""),
               ifelse(lgcSzFntTtlY, paste(infoStopSzFntTtlY, "\n", sep = ""), ""),
               ifelse(lgcSzFntAxsX, paste(infoStopSzFntAxsX, "\n", sep = ""), ""),
               ifelse(lgcSzFntEC, paste(infoStopSzFntEC, "\n", sep = ""), ""),
               ifelse(lgcSzFntAxsY, paste(infoStopSzFntAxsY, "\n", sep = ""), ""),
               ifelse(lgcSzFntLgnd, paste(infoStopSzFntLgnd, "\n", sep = ""), ""),
               ifelse(lgcSzFntLbl, paste(infoStopSzFntLbl, "\n", sep = ""), ""),
               ifelse(lgcSzFntLblEC, paste(infoStopSzFntLblEC, "\n", sep = ""), ""),
               ifelse(lgcSzPnt, paste(infoStopSzPnt, "\n", sep = ""), ""),
               ifelse(lgcSzPntEC, paste(infoStopSzPntEC, "\n", sep = ""), ""),
               ifelse(lgcSzPntNEC, paste(infoStopSzPntNEC, "\n", sep = ""), ""),
               ifelse(lgcTypPltCV, paste(infoStopTypPltCV, "\n", sep = ""), ""),
               ifelse(lgcTypPnt, paste(infoStopTypPnt, "\n", sep = ""), ""),
               ifelse(lgcTypPntEC, paste(infoStopTypPntEC, "\n", sep = ""), ""),
               ifelse(lgcTypPntNEC, paste(infoStopTypPntNEC, "\n", sep = ""), ""),
               ifelse(lgcTypLn0, paste(infoStopTypLn0, "\n", sep = ""), ""),
               ifelse(lgcTypLnEC, paste(infoStopTypLnEC, "\n", sep = ""), ""),
               ifelse(lgcClrTtl, paste(infoStopClrTtl, "\n", sep = ""), ""),
               ifelse(lgcClrTtlX, paste(infoStopClrTtlX, "\n", sep = ""), ""),
               ifelse(lgcClrTtlY, paste(infoStopClrTtlY, "\n", sep = ""), ""),
               ifelse(lgcClrAxsX, paste(infoStopClrAxsX, "\n", sep = ""), ""),
               ifelse(lgcClrAxsY, paste(infoStopClrAxsY, "\n", sep = ""), ""),
               ifelse(lgcClrLgnd, paste(infoStopClrLgnd, "\n", sep = ""), ""),
               ifelse(lgcClrVrtnL, paste(infoStopClrVrtnL, "\n", sep = ""), ""),
               ifelse(lgcClrVrtnM, paste(infoStopClrVrtnM, "\n", sep = ""), ""),
               ifelse(lgcClrVrtnH, paste(infoStopClrVrtnH, "\n", sep = ""), ""),
               ifelse(lgcClrLblZn, paste(infoStopClrLblZn, "\n", sep = ""), ""),
               ifelse(lgcClrLbl, paste(infoStopClrLbl, "\n", sep = ""), ""),
               ifelse(lgcClrLblEC, paste(infoStopClrLblEC, "\n", sep = ""), ""),
               ifelse(lgcClrPnt, paste(infoStopClrPnt, "\n", sep = ""), ""),
               ifelse(lgcClrPntEC, paste(infoStopClrPntEC, "\n", sep = ""), ""),
               ifelse(lgcClrPntNEC, paste(infoStopClrPntNEC, "\n", sep = ""), ""),
               ifelse(lgcClrLn0, paste(infoStopClrLn0, "\n", sep = ""), ""),
               ifelse(lgcClrLnEC, paste(infoStopClrLnEC, "\n", sep = ""), ""),
               ifelse(lgcClrLnCV, paste(infoStopClrLnCV, "\n", sep = ""), ""),
               ifelse(lgcAnglAxsX, paste(infoStopAnglAxsX, "\n", sep = ""), ""),
               ifelse(lgcAnglLbl, paste(infoStopAnglLbl, "\n", sep = ""), ""),
               ifelse(lgcSort, paste(infoStopSort, "\n", sep = ""), ""),
               sep = "")
         )



  # 06. PREPARE data set for disparity plot according to settings -----
  infoWhich    <- ifelse(is.null(which), "CV", which)
  #infoMethodCV <- "CV"
  infoTypPltCV <- ifelse(is.null(typPltCV), "half", typPltCV)

  infoMethodVrblty <- ifelse(object$vrblty.method == "CV",
                             "common coefficient of variability",
                             "robust coefficient of variability")

  if (infoWhich == "CV") {
    if (infoMethodVrblty == "robust coefficient of variability") {
      infoMSDCVL  <- -infoCasesMSD3CVL / (infoMCases * infoVrblty) # infoVrblty
      infoPSDCVL  <- infoCasesMSD3CVL / (infoMCases * infoVrblty)  # infoVrblty
      #infoMSDCVH  <- -infoCasesMSD3CVL / infoCVH #(infoMCases - infoCV0.3)
      #infoPSDCVH  <- infoCasesMSD3CVL / infoCVH #(infoMCases - infoCV0.3)
    }

    infoMethodOtlr <- object$outlier.method

    if (infoMethodOtlr == "IQR") {
      dataPlot <- dataDsprty[, c("source", "study", "n", "time", "z.val",
                                 "outlier.IQR", "cases.excessive.IQR", "prop.excessive.IQR")]
    } else if (infoMethodOtlr == "Z") {
      dataPlot <- dataDsprty[, c("source", "study", "n", "time", "z.val",
                                 "outlier.Z", "cases.excessive.Z", "prop.excessive.Z")]
    } else if (infoMethodOtlr == "GESD") {
      dataPlot <- dataDsprty[, c("source", "study", "n", "time", "z.val",
                                 "outlier.GESD", "cases.excessive.GESD", "prop.excessive.GESD")]
    } else if (infoMethodOtlr == "MAD") {
      dataPlot <- dataDsprty[, c("source", "study", "n", "time", "z.val",
                                 "outlier.MAD", "cases.excessive.MAD", "prop.excessive.MAD")]
    }

  } else {
    infoMethodOtlr <- infoWhich

    if (infoWhich == "IQR") {
      dataPlot <- dataDsprty[, c("source", "study", "n", "time", "z.val",
                                 "outlier.IQR", "cases.excessive.IQR", "prop.excessive.IQR")]
    } else if (infoWhich == "Z") {
      dataPlot <- dataDsprty[, c("source", "study", "n", "time", "z.val",
                                 "outlier.Z", "cases.excessive.Z", "prop.excessive.Z")]
    } else if (infoWhich == "GESD") {
      dataPlot <- dataDsprty[, c("source", "study", "n", "time", "z.val",
                                 "outlier.GESD", "cases.excessive.GESD", "prop.excessive.GESD")]
    } else if (infoWhich == "MAD") {
      dataPlot <- dataDsprty[, c("source", "study", "n", "time", "z.val",
                                 "outlier.MAD", "cases.excessive.MAD", "prop.excessive.MAD")]
    }
  }

  colnames(dataPlot)[-which(colnames(dataPlot) %in% c("source", "study", "n", "time", "z.val"))] <- c("outlier", "cases.excessive", "prop.excessive")

  #dataPlot$prop.excessive     <- round(dataPlot$prop.excessive, 3)

  dataPlot$prop.excessive.abs <- abs(dataPlot$prop.excessive)
  dataPlot$cases.excessive.CV <- dataDsprty[, c("cases.excessive.CV")]
  dataPlot$prop.excessive.CV  <- dataDsprty[, c("prop.excessive.CV")]

  infoOutliers                <- sum(dataPlot$outlier == TRUE)
  infoOtlrExcssvCases         <- sum(abs(dataPlot[dataPlot$outlier == TRUE, "cases.excessive"]))

  rsltOtlrProp  <- binom.test(x = infoOtlrExcssvCases,
                              n = infoCases,
                              p = 0.1,
                              alternative = "greater"
                              )

  infoOtlrProp  <- rsltOtlrProp$estimate
  infoOtlrPVal  <- rsltOtlrProp$p.value
  infoOtlrLCI   <- rsltOtlrProp$conf.int[1][1]
  infoOtlrUCI   <- rsltOtlrProp$conf.int[1][2]

  infoLgcTtl    <- ifelse(is.null(lgcTtl), TRUE, lgcTtl)
  infoLgcTtlX   <- ifelse(is.null(lgcTtlX), TRUE, lgcTtlX)
  infoLgcTtlY   <- ifelse(is.null(lgcTtlY), TRUE, lgcTtlY)
  infoLgcLgnd   <- ifelse(is.null(lgcLgnd), TRUE, lgcLgnd)
  infoLgcDtls   <- ifelse(is.null(lgcDtls), FALSE, lgcDtls)

  if (infoLgcLgnd) {
    if (infoLgcDtls) {
      txtLgnd <- paste("Disparities in sample size test (outlier detection based on ",
                        infoMethodOtlr,
                        "):\n",
                        "Number of outliers = ", infoOutliers,
                        " (Excessive cases = ", round(infoOtlrExcssvCases, 3),
                        "; P-value",
                        ifelse(infoOtlrPVal < 0.001,
                               " < 0.001",
                               paste(" = ",
                                     round(infoOtlrPVal, 3),
                                     sep = "")
                        ),
                        ")\n",
                        "Variability (based on ",
                       infoMethodVrblty,
                       "):\n",
                       "Value",
                        ifelse(infoVrblty < 0.001,
                               " < 0.001",
                               paste(" = ",
                                     round(infoVrblty, 3),
                                     sep = "")
                        ),
                        " (mean cases = ", ceiling(infoMCases),
                        "; SD = ", round(infoSDCases, 3),
                        "; P-value",
                        ifelse(infoPValVrblty < 0.001,
                               " < 0.001",
                               paste(" = ",
                                     round(infoPValVrblty, 3),
                                     sep = "")
                        ),
                        ")\n",
                        sep = "")
    } else {
      txtLgnd <- paste("Disparities in sample size test",
                        ifelse(infoWhich %in% c("IQR", "Z", "MAD", "GESD"),
                               paste(" (outlier detection based on ",
                                     infoMethodOtlr,
                                     "):\n",
                                     "Number of outliers = ", infoOutliers,
                                     " (Excessive cases = ", round(infoOtlrExcssvCases, 3),
                                     "; P-value",
                                     ifelse(infoOtlrPVal < 0.001,
                                            " < 0.001",
                                            paste(" = ",
                                                  round(infoOtlrPVal, 3),
                                                  sep = "")
                                     ),
                                     ")\n",
                                     sep = ""),
                               paste(" (variability based on ",
                                     infoMethodVrblty,
                                     "):\n",
                                     "Variability",
                                     ifelse(infoVrblty < 0.001,
                                            " < 0.001",
                                            paste(" = ",
                                                  round(infoVrblty, 3),
                                                  sep = "")
                                     ),
                                     " (mean cases = ", ceiling(infoMCases),
                                     "; SD = ", round(infoSDCases, 3),
                                     "; P-value",
                                     ifelse(infoPValVrblty < 0.001,
                                            " < 0.001",
                                            paste(" = ",
                                                  round(infoPValVrblty, 3),
                                                  sep = "")
                                     ),
                                     ")\n",
                                     sep = "")
                        ),
                        sep = "")
    }
  } else {
    txtLgnd <- ""
  }

  infoLgcLblZn  <- ifelse(is.null(lgcLblZn), TRUE, lgcLblZn)

  infoSzFntTtl  <- ifelse(is.null(szFntTtl), 1.5, szFntTtl)
  infoSzFntTtlX <- ifelse(is.null(szFntTtlX), 1.2, szFntTtlX)
  infoSzFntTtlY <- ifelse(is.null(szFntTtlY), 1.2, szFntTtlY)

  if (is.null(szFntAxsX)) {
    infoSzFntAxsX      <- ifelse(infoNumStdy < 11, 1,
                                 1 / sqrt(infoNumStdy / 10))
    dataPlot$szFntAxsX <- infoSzFntAxsX
  } else {
    infoSzFntAxsX      <- szFntAxsX
    dataPlot$szFntAxsX <- szFntAxsX
  }

  if (infoWhich %in% c("IQR", "Z", "MAD", "GESD")) {
    if (is.null(szFntEC)) {
      infoSzFntEC <- ifelse(infoNumStdy < 11, 1,
                            1 / sqrt(infoNumStdy / 10))
    } else {
      dataPlot[dataPlot$prop.excessive.abs > 0, "szFntAxsX"] <- szFntEC
    }
  }

  infoSzFntAxsY  <- ifelse(is.null(szFntAxsY), 1, szFntAxsY)
  infoSzFntLgnd  <- ifelse(is.null(szFntLgnd), 1, szFntLgnd)

  if (is.null(szFntLbl)) {
    infoSzFntLbl      <- ifelse(infoNumStdy < 11, 1,
                                1 / sqrt(infoNumStdy / 10))
    dataPlot$szFntLbl <- infoSzFntLbl
  } else {
    infoSzFntLbl      <- szFntLbl
    dataPlot$szFntLbl <- szFntLbl
  }

  if (infoWhich %in% c("IQR", "Z", "MAD", "GESD")) {
    if (is.null(szFntLblEC)) {
      infoSzFntLblEC <- ifelse(infoNumStdy < 11, 1,
                               1 / sqrt(infoNumStdy / 10))
    } else {
      dataPlot[dataPlot$prop.excessive.abs > 0, "szFntLbl"] <- szFntLblEC
    }
  }

  if (is.null(szPnt)) {
    infoSzPnt      <- 1
    dataPlot$szPnt <- infoSzPnt
  } else {
    infoSzPnt      <- szPnt
    dataPlot$szPnt <- szPnt
  }

  if (is.null(szPntEC)) {
    #infoSzPntEC <- ifelse(is.null(szPntEC), 1, szPntEC)
    infoSzPntEC <- 1
    #dataPlot[dataPlot$prop.excessive.abs > 0, "szPnt"] <- infoSzPntEC
  } else {
    dataPlot[dataPlot$prop.excessive.abs > 0, "szPnt"] <- szPntEC
  }

  if (is.null(szPntNEC)) {
    #infoSzPntNEC <- ifelse(is.null(szPntNEC), 1, szPntNEC)
    infoSzPntNEC <- 1
    #dataPlot[dataPlot$prop.excessive.abs == 0, "szPnt"] <- infoSzPntNEC
  } else {
    dataPlot[dataPlot$prop.excessive.abs == 0, "szPnt"] <- szPntNEC
  }

  if (is.null(typPnt)) {
    infoSzPnt       <- 21
    dataPlot$typPnt <- infoSzPnt
  } else {
    infoTypPnt      <- typPnt + 20
    dataPlot$typPnt <- typPnt + 20
  }

  if (is.null(typPntEC)) {
    #infoTypPntEC <- ifelse(is.null(typPntEC), 21, typPntEC + 20)
    #infoTypPntEC <- 21
    #dataPlot[dataPlot$prop.excessive.abs > 0, "typPnt"] <- infoTypPntEC
  } else {
    dataPlot[dataPlot$prop.excessive.abs > 0, "typPnt"] <- typPntEC + 20
  }

  if (is.null(typPntNEC)) {
    #infoTypPntNEC <- ifelse(is.null(typPntNEC), 21, typPntNEC + 20)
    #infoTypPntNEC <- 21
    #dataPlot[dataPlot$prop.excessive.abs == 0, "typPnt"] <- infoTypPntNEC
  } else {
    dataPlot[dataPlot$prop.excessive.abs == 0, "typPnt"] <- typPntNEC + 20
  }

  infoTypLn0  <- ifelse(is.null(typLn0), 1, typLn0)
  infoTypLnEC <- ifelse(is.null(typLnEC), 2, typLnEC)
  infoClrTtl  <- ifelse(is.null(clrTtl), "black", clrTtl)
  infoClrTtlX <- ifelse(is.null(clrTtlX), "black", clrTtlX)
  infoClrTtlY <- ifelse(is.null(clrTtlY), "black", clrTtlY)

  if (is.null(clrAxsX)) {
    infoClrAxsX      <- "black"
    dataPlot$clrAxsX <- infoClrAxsX
  } else {
    infoClrAxsX      <- clrAxsX
    dataPlot$clrAxsX <- clrAxsX
  }

  infoClrAxsY   <- ifelse(is.null(clrAxsY), "black", clrAxsY)
  infoClrLgnd   <- ifelse(is.null(clrLgnd), "black", clrLgnd)

  infoClrVrtnL  <- ifelse(is.null(clrVrtnL),
                          "darkseagreen2",
                          clrVrtnL)

  infoClrVrtnM  <- ifelse(is.null(clrVrtnM),
                          "khaki1",
                          clrVrtnM)

  infoClrVrtnH  <- ifelse(is.null(clrVrtnH),
                          "lightpink1",
                          clrVrtnH)

  if (is.null(clrLblZn)) {
    infoClrLblZn <- c("darkseagreen4", "khaki4", "lightpink4")
  } else {
    infoClrLblZn <- clrLblZn
  }

  infoClrLblZnL <- infoClrLblZn[1]

  infoClrLblZnM <- ifelse(length(infoClrLblZn) == 3,
                          infoClrLblZn[2],
                          infoClrLblZn[1])

  infoClrLblZnH <- ifelse(length(infoClrLblZn) == 3,
                          infoClrLblZn[3],
                          infoClrLblZn[1])


  if (is.null(clrLbl)) {
    infoClrLbl      <- "gray25"
    dataPlot$clrLbl <- infoSzFntAxsX
  } else {
    infoClrLbl      <- clrLbl
    dataPlot$clrLbl <- clrLbl
  }

  if (infoWhich %in% c("IQR", "Z", "MAD", "GESD")) {
    if (is.null(clrLblEC)) {
      infoClrLblEC <- "gray25"
      dataPlot[dataPlot$prop.excessive.abs > 0, "clrLbl"] <- infoClrLblEC
    } else {
      dataPlot[dataPlot$prop.excessive.abs > 0, "clrLbl"] <- clrLblEC
    }
  }

  if (is.null(clrPnt)) {
    infoClrPnt      <- "gray"
    dataPlot$clrPnt <- infoClrPnt
  } else {
    infoClrPnt      <- clrPnt
    dataPlot$clrPnt <- clrPnt
  }

  if (is.null(clrPntEC)) {
    #infoClrPntEC <- ifelse(is.null(clrPntEC), "firebrick3", clrPntEC)
    infoTypPntEC <- "firebrick3"
    #dataPlot[dataPlot$prop.excessive.abs > 0, "clrPnt"] <- infoTypPntEC
  } else {
    dataPlot[dataPlot$prop.excessive.abs > 0, "clrPnt"] <- clrPntEC
  }

  if (is.null(clrPntNEC)) {
    #infoClrPntNEC <- ifelse(is.null(clrPntNEC), "skyblue", clrPntNEC)
    infoClrPntNEC <- "skyblue"
    #dataPlot[dataPlot$prop.excessive.abs == 0, "clrPnt"] <- infoClrPntNEC
  } else {
    dataPlot[dataPlot$prop.excessive.abs == 0, "clrPnt"] <- clrPntNEC
  }

  infoClrLn0   <- ifelse(is.null(clrLn0), "gray", clrLn0)
  infoClrLnEC  <- ifelse(is.null(clrLnEC), "firebrick3", clrLnEC)
  dataPlot$pos <- ifelse(dataPlot$prop.excessive < 0, 3, 1)

  infoClrLnCV  <- ifelse(is.null(clrLnCV),
                         ifelse(infoSDCases > infoCVH,
                                "firebrick4",
                                "navyblue"),
                         clrLnCV)

  infoAnglAxsX <- ifelse(is.null(anglAxsX), 30, anglAxsX)
  infoAnglLbl  <- ifelse(is.null(anglLbl), 0, anglLbl)
  infoSort     <- ifelse(is.null(sort), "excessive", sort)

  if (infoSort == "time") {
    dataPlot <- dataPlot[order(dataPlot$time), ]
  } else if (infoSort == "size") {
    dataPlot <- dataPlot[order(dataPlot$n), ]
  } else {
    dataPlot <- dataPlot[order(dataPlot$prop.excessive), ]
  }

  if (is.null(txtLbl)) {
    infoTxtLbl <- ""
  } else if (txtLbl == "n") {
    infoTxtLbl <- dataPlot$n
  } else if (txtLbl == "n.excessive") {
    infoTxtLbl <- dataPlot$cases.excessive
  } else if (txtLbl == "prop.excessive") {
    infoTxtLbl <- round(dataPlot$prop.excessive, 3)
  }

  if (is.null(txtLbl)) {
    dataPlot$txtLbl <- ""
  } else if (txtLbl == "n") {
    dataPlot$txtLbl <- dataPlot$n
  } else if (txtLbl == "n.excessive") {
    dataPlot$txtLbl <- dataPlot$cases.excessive
  } else if (txtLbl == "prop.excessive") {
    dataPlot$txtLbl <- round(dataPlot$prop.excessive, 3)
  }


  dataPlot$position.label.axis.x    <- ifelse(infoAnglAxsX == 0, 1, 2)
  dataPlot$y.label.excessive        <- ifelse(dataPlot$prop.excessive > 0, -0.1, 0.1)
  dataPlot$position.label.excessive <- ifelse(dataPlot$prop.excessive > 0,
                                              ifelse(infoAnglLbl == 0,
                                                     1, 2),
                                              ifelse(infoAnglLbl == 0,
                                                     3, 4)
                                              )
  dataPlot$source <- c(1:nrow(dataPlot))



  # 07. ILLUSTRATE proportion of excessive cases plot -----

  if (infoWhich == "CV") {
    # Disparity plot (variability)

    if (infoTypPltCV == "full") {
      plot(infoMCases + infoCVL * c(-3.5:3.5),
           c(-3.5:3.5),
           type = "n", frame = FALSE,
           ylim = c(-3, 3),
           xaxt = "n",
           yaxt = "n",
           xlab = "", ylab = "")

      rect(infoCasesMSD3.5CVL, -3,
           infoMCases, 0,
           col = infoClrVrtnM, lty = 0)

      rect(infoMCases, 0,
           infoCasesPSD3.5CVL, 3,
           col = infoClrVrtnM, lty = 0)

      polygon(c(infoCasesMSD3CVL,
                infoMCases,
                infoMCases,
                infoCasesPSD3CVL),
              c(-3, -3, 3, 3),
              col = infoClrVrtnL, lty = 0)

      polygon(c(infoCasesMSD3.5CVL,
                infoCasesMSD3.5CVL,
                infoCasesPSD3.5CVL,
                infoCasesPSD3.5CVL),
              c(infoMSDCVH, 0,
                0, infoPSDCVH),
              lty = 0,
              col = infoClrVrtnH)


      segments(rep(infoMCases * 1.05, 4), c(-1, -1.5, -2, -2.5),
               rep(infoMCases * 1.07, 4), c(-1, -1.5, -2, -2.5),
               lwd = c(2.5, 20, 20, 20),
               col = c(infoClrLnCV,
                       infoClrVrtnL,
                       infoClrVrtnM,
                       infoClrVrtnH)
               )

      text(rep(infoMCases * 1.08, 4),
           c(-1, -1.5, -2, -2.5),
           c("Observed coefficient of variation",
             "Low variability zone",
             "Moderate variability zone",
             "High variability zone"),
           pos = 4,
           cex = infoSzFntLgnd,
           col = infoClrLgnd)

      } else {

      plot(infoMCases + infoCVL * c(0:4),
           c(0:4),
           type = "n", frame = FALSE,
           ylim = c(0, 3),
           xaxt = "n",
           yaxt = "n",
           xlab = "", ylab = "")

      rect(infoMCases, 0,
           infoCasesPSD4CVL, 3,
           col = infoClrVrtnM, lty = 0)

      polygon(c(infoMCases, infoMCases, infoCasesPSD3CVL),
              c(0, 3, 3),
              col = infoClrVrtnL, lty = 0)

      polygon(c(infoMCases,
                infoCasesPSD4CVL,
                infoCasesPSD4CVL),
              c(0, 0, infoPSDCVH),
              lty = 0,
              col = infoClrVrtnH)

      rect(infoMCases, 3,
           infoCasesPSD4CVL, 4,
           lty = 0,
           col = "white")

      if (infoLgcLblZn == TRUE) {
        text(infoMCases + (infoCasesPSD4CVL - infoMCases) * 0.29, #(infoMCases + infoCasesPSD4CV0.1) / 2,
             c(1.5),
             c("Low variability zone"),
             pos = c(4),
             srt = c(33),
             cex = infoSzFntLgnd,
             col = infoClrLblZnL) #infoClrLgnd

        text(infoMCases + (infoCasesPSD4CVL - infoMCases) * 0.3, #(infoMCases + infoCasesPSD4CV0.1) / 2,
             c(1),
             c("Moderate variability zone"),
             pos = c(4),
             srt = c(26), # 23
             cex = infoSzFntLgnd,
             col = infoClrLblZnM) #infoClrLgnd

        text(infoMCases + (infoCasesPSD4CVL - infoMCases) * 0.3, #(infoMCases + infoCasesPSD4CV0.1) / 2,
             c(0.5),
             c("High variability zone"),
             pos = c(4),
             srt = c(20),
             cex = infoSzFntLgnd,
             col = infoClrLblZnH) #infoClrLgnd
      }

    }

    #polygon(c(mean(test$n) - mean(test$n) * 0.1 * 3, mean(test$n) - mean(test$n) * 0.1 * 3, mean(test$n) + mean(test$n) * 0.1 * 3, mean(test$n) + mean(test$n) * 0.1 * 3),
    #        c(-(mean(test$n) - mean(test$n) * 0.1 * 3) / (mean(test$n) - mean(test$n) * 0.3), 0, 0, (mean(test$n) - mean(test$n) * 0.1 * 3) / (mean(test$n) - mean(test$n) * 0.3)),
    #        col = "pink", lty = 0)
    #segments(infoMCases - infoSDCases * 3, -3,
    #         infoMCases + infoSDCases * 3, 3,
    #         lty = 1,
    #         lwd = 2.5,
    #         col = "blue4")

    segments(infoMCases, 0, #infoCasesMSD4CVL, infoMSDCVL,
             infoCasesPSD4CVL, infoPSDCVL,
             lty = 1,
             lwd = 2.5,
             col = infoClrLnCV)

    axis(1, las = 1,
         cex.axis = infoSzFntAxsX[1],
         col.ticks = infoClrAxsX[1],
         col = infoClrAxsX[1])

    axis(2, las = 2,
         cex.axis = infoSzFntAxsY,
         col.ticks = infoClrAxsY,
         col = infoClrAxsY)

    if (infoLgcLgnd == TRUE) {
      ### TEXT legend
      text(ifelse(infoTypPltCV == "full",
                  infoCasesMSD3.5CVL + (infoCasesPSD3.5CVL - infoCasesMSD3.5CVL) * 0.02,
                  infoMCases + (infoCasesPSD3CVL - infoMCases) * 0.05),
           ifelse(infoTypPltCV == "full",
                  par("usr")[4] * 0.7,
                  par("usr")[4] * 0.8),
           txtLgnd,
           pos = 4,
           cex = infoSzFntLgnd,
           col = infoClrLgnd)
    }

    if (infoLgcTtl == TRUE) {
      mtext("Disparity plot (variability)",
            side = 3,
            cex = infoSzFntTtl,
            col = infoClrTtl)
    }

    if (infoLgcTtlX == TRUE) {
      mtext("Sample size",
            side = 1, line = 3,
            cex = infoSzFntTtlX,
            col = infoClrTtlX)
    }

    if (infoLgcTtlY == TRUE) {
      mtext("Number of standrd deviations",
            side = 2, line = 3,
            cex = infoSzFntTtlY,
            col = infoClrTtlY)
    }

  } else {
    # 7.2. Disparity plot (outlier)

    plot(dataPlot$source,
         dataPlot$prop.excessive,
         type = "n", frame = FALSE,
         xaxt = "n", yaxt = "n",
         ylim = c(ifelse(min(dataPlot$prop.excessive) > -0.5,
                         -0.5,
                         ifelse(min(dataPlot$prop.excessive) > -1,
                                -1.2,
                                min(dataPlot$prop.excessive) * 1.1
                                )
                         ),
                  ifelse(max(dataPlot$prop.excessive) < 0.5,
                         0.5,
                         ifelse(max(dataPlot$prop.excessive) < 1,
                                1.2,
                                max(dataPlot$prop.excessive) * 1.1
                                )
                         )
                  ),
         xlab = "", ylab = "")

    segments(0, 0,
             nrow(dataPlot), 0,
             col = infoClrLn0,
             lty = infoTypLn0)
    segments(dataPlot$source, 0,
             dataPlot$source, dataPlot$prop.excessive,
             col = infoClrLnEC,
             lty = infoTypLnEC)
    points(dataPlot$source,
           dataPlot$prop.excessive,
           cex = dataPlot$szPnt,
           col = "gray",
           bg = dataPlot$clrPnt,
           pch = dataPlot$typPnt)
    axis(2, las = 2,
         cex.axis = infoSzFntAxsY,
         col.ticks = infoClrAxsY,
         col = infoClrAxsY)
    text(dataPlot$source,
         par("usr")[3],
         dataPlot$study,
         cex = dataPlot$szFntAxsX,
         col = dataPlot$clrAxsX,
         xpd = TRUE,
         pos = dataPlot$position.label.axis.x,
         srt = infoAnglAxsX)

    if (infoLgcLgnd == TRUE) {
      ### TEXT legend
      text(1,
           par("usr")[4] * 0.7,
           txtLgnd,
           pos = 4,
           cex = infoSzFntLgnd,
           col = infoClrLgnd)
    }

    if (infoLgcTtl == TRUE) {
      mtext("Disparity plot (outlier)",
            side = 3,
            cex = infoSzFntTtl,
            col = infoClrTtl)
    }

    if (infoLgcTtlX == TRUE) {
      mtext("Study",
            side = 1, line = 4,
            cex = infoSzFntTtlX,
            col = infoClrTtlX)
    }

    if (infoLgcTtlY == TRUE) {
      mtext("Proportion of excessive cases",
            side = 2, line = 3,
            cex = infoSzFntTtlY,
            col = infoClrTtlY)
    }

    if (is.null(txtLbl)) {
      text(dataPlot$source,
           dataPlot$y.label.excessive,
           "")
    } else {
      text(dataPlot$source,
           dataPlot$y.label.excessive, #rep(0, infoNumStdy),
           dataPlot$txtLbl,
           cex = dataPlot$szFntLbl,
           pos = dataPlot$position.label.excessive,
           col = dataPlot$clrLbl,
           srt = infoAnglLbl)
    }

  }

}
