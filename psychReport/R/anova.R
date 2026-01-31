#' @title aovTidyTable
#'
#' @description Take output from base aov function and produce a "tidy" ANOVA table
#' similar to the output of ezANOVA. The output also contains the marginal means.
#'
#' @param aovObj Output from aov function
#'
#' @return list
#'
#' @examples
#' # create dataframe
#' dat <- createDF(nVP = 6, nTrl = 1,
#'                 design = list("Comp" = c("comp", "incomp")))
#'
#' dat <- addDataDF(dat, RT = list("Comp comp"   = c(500, 150, 100),
#'                                 "Comp incomp" = c(520, 150, 100)))
#'
#' aovObj <- aov(RT ~ Comp + Error(VP/(Comp)), dat)
#' aovObj <- aovTable(aovObj)
#' aovObj$ANOVA
#' printTable(aovObj$ANOVA)
#'
#' @export
aovTidyTable <- function(aovObj) {

  # create ANOVA table structure similar to ezANOVA
  aovTable <- broom::tidy(aovObj)

  residuals <- aovTable[aovTable$term == "Residuals", ]
  aovTable  <- aovTable[!aovTable$term == "Residuals", ]

  aovTable$DFd <- 0
  aovTable$SSd <- 0
  for (row in 1:nrow(aovTable)) {
    aovTable$DFd[row] <- residuals$df[residuals$stratum == aovTable$stratum[row]]
    aovTable$SSd[row] <- residuals$sumsq[residuals$stratum == aovTable$stratum[row]]
  }

  aovTable        <- aovTable[, c(2, 3, 8, 4, 9, 6, 7)]
  names(aovTable) <- c("Effect", "DFn", "DFd", "SSn", "SSd", "F", "p")

  aovTable$"p<.05" <- pValueSummary(aovTable$p)

  out       <- NULL
  out$ANOVA <- as.data.frame(aovTable)
  out$means <- stats::model.tables(aovObj, type = "mean")  # marginal means

  return(out)

}



#' @title aovDispTable
#'
#' @description Display formatted ANOVA table in command window.
#'
#' @param aovObj Output from aov or ezANOVA
#' @param caption Required for heading
#'
#' @return NULL
#'
#' @examples
#' # Example 1:
#' # create dataframe
#' dat <- createDF(nVP = 6, nTrl = 1,
#'                 design = list("Comp" = c("comp", "incomp")))
#'
#' dat <- addDataDF(dat, RT = list("Comp comp"   = c(500, 150, 100),
#'                                 "Comp incomp" = c(520, 150, 100)))
#'
#' aovObj <- aov(RT ~ Comp + Error(VP/(Comp)), dat)
#' aovDispTable(aovObj)
#'
#' # or with ezANOVA
#' library(ez)
#' aovRT <- ezANOVA(dat, dv=.(RT), wid = .(VP), within = .(Comp), return_aov = TRUE, detailed = TRUE)
#' aovDispTable(aovRT)
#'
#' @export
aovDispTable <- function(aovObj, caption=sys.call()) {

  if (is.null(aovObj$ANOVA)) {
    aovObj <- aovTidyTable(aovObj)  # convert base aov output
  }

  if (!is.character(caption)) {
    caption <- paste0("ANOVA:", unlist(lapply(caption[2], as.character)))
  }
  width <- apply(aovObj$ANOVA, 2, function(x)  max(unlist(lapply(x, nchar))))
  width <- sum(pmax(width, unlist(lapply(names(aovObj$ANOVA), nchar)))) + ncol(aovObj$ANOVA) + 1

  print(cli::rule(line = 2, center = caption, width = width))
  print(aovObj$ANOVA, row.names = FALSE)
  print(cli::rule(width = width))

}



#' @title aovDispMeans
#'
#' @description Displays marginal means from model.tables in the command window.
#'
#' @param aovObj Output from aov or ezANOVA  (NB. ezANOVA must be called with \"return_aov = TRUE\"")
#' @param value String for column name
#' @param caption Required for heading
#'
#' @return NULL
#'
#' @examples
#' # Example 1:
#' # create dataframe
#' dat <- createDF(nVP = 50, nTrl = 1,
#'                 design = list("Comp" = c("comp", "incomp")))
#'
#' dat <- addDataDF(dat, RT = list("Comp comp"   = c(500, 100, 100),
#'                                 "Comp incomp" = c(520, 100, 100)))
#'
#' aovRT <- aov(RT ~ Comp + Error(VP/(Comp)), dat)
#' aovDispMeans(aovRT)
#'
#' # or with ezANOVA
#' library(ez)
#' aovRT <- ezANOVA(dat, dv=.(RT), wid = .(VP), within = .(Comp),
#'                  return_aov = TRUE, detailed = TRUE)
#' aovRT <- aovTable(aovRT)
#' aovDispMeans(aovRT)
#'
#' @export
aovDispMeans <- function(aovObj, value="value", caption=sys.call()) {

  if (is.null(aovObj$means)) {
    if (!is.null(aovObj$ANOVA)) {
      stop("Call ezANOVA with return_aov = TRUE for marginal means.")
    }
    aovObj <- aovTidyTable(aovObj)  # convert base aov output
  }

  nTotal <- length(aovObj$means$n) + 1
  for (i in 2:nTotal) {

    dat     <- as.data.frame.table(aovObj$means$tables[[i]], responseName = value)
    heading <- names(aovObj$means$tables[i])

    width   <- apply(dat, 2, function(x) max(unlist(lapply(x, nchar))))
    width   <- sum(pmax(width, unlist(lapply(names(dat), nchar)))) + ncol(dat) + 1

    if (i == 2) {
      if (!is.character(caption)) {
        caption <- paste0("ANOVA:", unlist(lapply(caption[2], as.character)))
      }
      print(cli::rule(line = 2, center = caption, width = width))
    }
    print(cli::rule(center = heading, width = width))
    print(dat, row.names = FALSE)
    if (i < nTotal) {
      cat("\n")
    } else {
      print(cli::rule(line = 2, width = width))
    }
  }

}



#' @title aovEffectSize
#'
#' @description Add effect size to ANOVA table. Effect sizes: partial eta squared (pes),
#' vs. ges (generalized eta squared, NB: default when using ezANOVA).
#'
#' @param aovObj Output from aov or ezANOVA
#' @param effectSize Effect size (pes vs. ges)
#'
#' @return list
#'
#' @examples
#' # Example 1:
#' # create dataframe with 2(Comp: comp vs. incomp) and 2(Side: left vs. right) factors/levels
#' dat <- createDF(nVP = 20, nTrl = 1,
#'                 design = list("Comp" = c("comp", "incomp", "neutral"),
#'                               "Side" = c("left", "right")))
#'
#' dat <- addDataDF(dat,
#'                  RT = list("Comp:Side comp:left"     = c(500, 150, 150),
#'                            "Comp:Side comp:right"    = c(500, 150, 150),
#'                            "Comp:Side incomp:left"   = c(550, 150, 150),
#'                            "Comp:Side incomp:right"  = c(550, 150, 150),
#'                            "Comp:Side neutral:left"  = c(525, 150, 150),
#'                            "Comp:Side neutral:right" = c(525, 150, 150)))
#'
#' aovRT <- aov(RT ~ Comp * Side + Error(VP/(Comp*Side)), dat)
#' aovDispMeans(aovRT)
#' aovRT <- aovEffectSize(aovRT)
#' aovRT <- aovDispTable(aovRT)
#'
#' # or with ezANOVA
#' library(ez)
#' aovRT <- ezANOVA(dat, dv=.(RT), wid = .(VP), within = .(Comp, Side),
#'                  return_aov = TRUE, detailed = TRUE)
#' aovRT <- aovEffectSize(aovRT)
#' aovDispTable(aovRT)
#'
#' @export
aovEffectSize <- function(aovObj, effectSize = "pes") {

  if (is.null(aovObj$ANOVA)) {
    aovObj <- aovTidyTable(aovObj)  # convert base aov output
  }

  if (effectSize == "pes") {
    aovObj$ANOVA$ges <- NULL
    aovObj$ANOVA$pes <- aovObj$ANOVA$SSn / (aovObj$ANOVA$SSn + aovObj$ANOVA$SSd)
  } else if (effectSize == "ges") {
    aovObj$ANOVA$pes <- NULL
    # NB assumes no observed variables within initial call to ezANOVA!
    aovObj$ANOVA$ges <- aovObj$ANOVA$SSn / (aovObj$ANOVA$SSn + sum(unique(aovObj$ANOVA$SSd)))
  }

  return(aovObj)

}



#' @title adjustJackknifeAdjustment
#'
#' @description Adjust ezANOVA table with corrected F (Fc = F/(n-1)^2) and p values for jackkniffed data (see Ulrich and Miller, 2001.
#' Using the jackknife-based scoring method for measuring LRP onset effects in factorial designs. Psychophysiology, 38, 816-827.)
#'
#' @param aovObj Output from aov or ezANOVA
#' @param numVPs The number of participants
#'
#' @return list
#'
#' @examples
#' # Example 1:
#' # create dataframe with 2(Comp: comp vs. incomp) and 2(Side: left vs. right) factors/levels
#' dat <- createDF(nVP = 20, nTrl = 1,
#'                 design = list("Comp" = c("comp", "incomp"),
#'                               "Side" = c("left", "right")))
#'
#' dat <- addDataDF(dat,
#'                  RT = list("Comp:Side comp:left"    = c(500, 150, 150),
#'                            "Comp:Side comp:right"   = c(500, 150, 150),
#'                            "Comp:Side incomp:left"  = c(500, 150, 150),
#'                            "Comp:Side incomp:right" = c(500, 150, 150)))
#'
#' aovRT <- aov(RT ~ Comp*Side + Error(VP/(Comp*Side)), dat)
#' aovRT <- aovJackknifeAdjustment(aovRT, length(unique(dat$VP)))
#' aovDispTable(aovRT)
#'
#' # or with ezANOVA
#' library(ez)
#' aovRT <- ezANOVA(dat, dv=.(RT), wid = .(VP), within = .(Comp, Side),
#'                  return_aov = TRUE, detailed = TRUE)
#' aovRT <- aovJackknifeAdjustment(aovRT, length(unique(dat$VP)))
#' aovDispTable(aovRT)
#'
#'
#' @export
aovJackknifeAdjustment <- function(aovObj, numVPs) {

  if (is.null(aovObj$ANOVA)) {
    aovObj <- aovTidyTable(aovObj)  # convert base aov output
  }

  aovObj$ANOVA$SSd     <- aovObj$ANOVA$SSd*((numVPs - 1) ^ 2)
  aovObj$ANOVA$F       <- aovObj$ANOVA$F/((numVPs - 1) ^ 2)
  aovObj$ANOVA$p       <- 1 - stats::pf(aovObj$ANOVA$F, aovObj$ANOVA$DFn, aovObj$ANOVA$DFd)
  aovObj$ANOVA$"p<.05" <- pValueSummary(aovObj$ANOVA$p)

  return(aovObj)

}



#' @title aovRoundDigits
#'
#' @description Round digits to n decimal places in ezANOVA table
#'
#' @param aovObj Output from aov or ezANOVA
#'
#' @return dataframe
#'
#' @examples
#' # Example 1:
#' # create dataframe with 2(Comp: comp vs. incomp) and 2(Side: left vs. right) factors/levels
#' dat <- createDF(nVP = 20, nTrl = 1,
#'                 design = list("Comp" = c("comp", "incomp"),
#'                               "Side" = c("left", "right")))
#'
#' dat <- addDataDF(dat,
#'                  RT = list("Comp:Side comp:left"    = c(500, 150, 150),
#'                            "Comp:Side comp:right"   = c(500, 150, 150),
#'                            "Comp:Side incomp:left"  = c(500, 150, 150),
#'                            "Comp:Side incomp:right" = c(500, 150, 150)))
#'
#' aovRT <- aov(RT ~ Comp*Side + Error(VP/(Comp*Side)), dat)
#' aovRT <- aovRoundDigits(aovRT)
#' aovDispTable(aovRT)
#'
#' # or using ezANOVA
#' library(ez)
#' aovRT <- ezANOVA(dat, dv=.(RT), wid = .(VP), within = .(Comp, Side),
#'                  return_aov = TRUE, detailed = TRUE)
#' aovRT <- aovRoundDigits(aovRT)
#' aovDispTable(aovRT)
#'
#' @export
aovRoundDigits <- function(aovObj) {

  if (is.null(aovObj$ANOVA)) {
    aovObj <- aovTidyTable(aovObj)  # convert base aov output
  }

  # round to 2 sig. decimal places
  colNames <- c("SSn", "SSd", "F", "eps", "ges", "pes")
  colIdx   <- which(names(aovObj$ANOVA) %in% colNames)
  colNames <- names(aovObj$ANOVA)[colIdx]

  aovObj$ANOVA <- aovObj$ANOVA %>%
    mutate_at(vars(all_of(colNames)), list(~ trimws(format(round(., digits = 2), nsmall = 2))))

  aovObj$ANOVA$DFn <- ifelse(aovObj$ANOVA$DFn == as.integer(aovObj$ANOVA$DFn),
                             trimws(as.integer(aovObj$ANOVA$DFn)),
                             trimws(format(round(aovObj$ANOVA$DFn, digits = 2), nsmall = 2)))
  aovObj$ANOVA$DFd <- ifelse(aovObj$ANOVA$DFd == as.integer(aovObj$ANOVA$DFd),
                             trimws(as.integer(aovObj$ANOVA$DFd)),
                             trimws(format(round(aovObj$ANOVA$DFd, digits = 2), nsmall = 2)))

  # round p-values to 3 sig. decimal places
  aovObj$ANOVA$p <- format(round(aovObj$ANOVA$p, digits = 3), nsmall = 3)

  return(aovObj)

}



#' @title aovSphericityAdjustment
#'
#' @description Adjust ezANOVA table with corrections for sphericity (Greenhouse-Geisser or
#' Huynh-Feldt). Called by default within aovTable
#'
#' @param aovObj The returned object from a call to ezANOVA
#' @param type "GG" (Greenhouse-Geisser) or "HF" (Huynh-Feldt)
#' @param adjDF TRUE/FALSE Should DF's be adjusted?
#'
#' @return list
#'
#' @examples
#' # Example 1:
#' # create dataframe with 3(Comp: neutral vs. comp vs. incomp) factors/levels
#' dat <- createDF(nVP = 20, nTrl = 1,
#'                 design = list("Comp" = c("neutral", "comp", "incomp")))
#'
#' dat <- addDataDF(dat,
#'                  RT = list("Comp neutral" = c(510, 150, 100),
#'                            "Comp comp"    = c(500, 150, 100),
#'                            "Comp incomp"  = c(520, 150, 100)))
#'
#' # using ezANOVA
#' library(ez)
#' aovRT <- ezANOVA(dat, dv=.(RT), wid = .(VP), within = .(Comp),
#'                  return_aov = TRUE, detailed = TRUE)
#' aovDispTable(aovRT)
#' aovRT <- aovSphericityAdjustment(aovRT)
#' aovDispTable(aovRT)
#'
#' @export
aovSphericityAdjustment <- function(aovObj, type = "GG", adjDF = TRUE) {

  hasSphericity <- aovObj$"Sphericity Corrections"
  if (is.null(hasSphericity)) {
      stop("aovObj does not have sphericity corrections.")
  }
  if (!type %in% c("GG", "HF")) {
    stop("Sphericity correction type not recognized!")
  }

  sphericityRows <- match(rownames(aovObj$"Sphericity Corrections"), rownames(aovObj$ANOVA))
  # Adjust p-values where Mauchls's test significant
  if (type == "GG") {
    aovObj$ANOVA$p[sphericityRows]   <- ifelse(aovObj$"Mauchly's Test for Sphericity"$"p" < 0.05, aovObj$"Sphericity Corrections"$"p[GG]", aovObj$ANOVA$p[sphericityRows])
    aovObj$ANOVA$eps                 <- rep(0, length(aovObj$ANOVA$"Effect"))
    aovObj$ANOVA$eps[sphericityRows] <- aovObj$"Sphericity Corrections"$"GGe"
  } else if (type == "HF") {
    aovObj$ANOVA$p[sphericityRows]   <- ifelse(aovObj$"Mauchly's Test for Sphericity"$"p" < 0.05, aovObj$"Sphericity Corrections"$"p[HF]", aovObj$ANOVA$p[sphericityRows])
    aovObj$ANOVA$eps                 <- rep(0, length(aovObj$ANOVA$"Effect"))
    aovObj$ANOVA$eps[sphericityRows] <- aovObj$"Sphericity Corrections"$"HFe"
  }

  aovObj$ANOVA$"eps_p<.05"                 <- rep("", length(aovObj$ANOVA$"Effect"))
  aovObj$ANOVA$"eps_p<.05"[sphericityRows] <- aovObj$"Mauchly's Test for Sphericity"$"p<.05"
  aovObj$ANOVA$"p<.05"                     <- ifelse(aovObj$ANOVA$p < .05, "*", "")

  # Adjust degrees of freedom where Mauchls's test significant
  sphericityRows <- sphericityRows[aovObj$"Mauchly's Test for Sphericity"$"p" < 0.05]
  if (adjDF) {
    aovObj$ANOVA$DFn[sphericityRows] <- aovObj$ANOVA$DFn[sphericityRows] * aovObj$ANOVA$eps[sphericityRows]
    aovObj$ANOVA$DFd[sphericityRows] <- aovObj$ANOVA$DFd[sphericityRows] * aovObj$ANOVA$eps[sphericityRows]
  }

  return(aovObj)

}



#' @title aovTable
#'
#' @description Adjust ezANOVA table output. Options include calculation of alternative
#' effect sizes (eta squared, partial eta squared), the calculation of marginal
#' means and formatting options for the ANOVA table (e.g., detailed, rounding).
#'
#' @param aovObj Output from aov or ezANOVA (NB. ezANOVA must be called with detailed = TRUE)
#' @param effectSize Effect size (pes vs. ges)
#' @param sphericityCorrections TRUE/FALSE (ezANOVA)
#' @param sphericityCorrectionType "GG" (default) vs. "HF" (ezANOVA)
#' @param sphericityCorrectionAdjDF TRUE/FALSE Should DF's values be corrected?
#' @param removeSumSquares TRUE/FALSE Remove SSn/SSd columns from the ANOVA table
#'
#' @return list
#'
#' @examples
#' # Example 1:
#' # create dataframe with 2(Comp: comp vs. incomp) and 2(Side: left vs. right) factors/levels
#' dat <- createDF(nVP = 20, nTrl = 1,
#'                 design = list("Comp" = c("comp", "incomp"),
#'                               "Side" = c("left", "right")))
#'
#' dat <- addDataDF(dat,
#'                  RT = list("Comp:Side comp:left"    = c(500, 150, 150),
#'                            "Comp:Side comp:right"   = c(500, 150, 150),
#'                            "Comp:Side incomp:left"  = c(500, 150, 150),
#'                            "Comp:Side incomp:right" = c(500, 150, 150)))
#'
#' aovRT <- aov(RT ~ Comp*Side + Error(VP/(Comp*Side)), dat)
#' aovRT <- aovTable(aovRT)
#'
#' # or using ezANOVA
#' library(ez)
#' aovRT <- ezANOVA(dat, dv=.(RT), wid = .(VP), within = .(Comp, Side),
#'                  return_aov = TRUE, detailed = TRUE)
#' aovRT <- aovTable(aovRT)
#'
#' @export
aovTable <- function(aovObj,
                     effectSize = "pes",
                     sphericityCorrections = TRUE,
                     sphericityCorrectionType = "GG",
                     sphericityCorrectionAdjDF = FALSE,
                     removeSumSquares = TRUE) {

  if (is.null(aovObj$ANOVA)) {
    aovObj <- aovTidyTable(aovObj)  # convert base aov output
  }

  if (!"SSn" %in% names(aovObj$ANOVA)) {
    stop("Call ezANOVA with \"detailed = TRUE\"!")
  }


  # add partial eta-squared
  if (effectSize == "pes") {
    aovObj <- aovEffectSize(aovObj, effectSize = "pes")
  } else if (effectSize == "ges") {
    if (is.null(aovObj$ANOVA$ges)) {
      aovObj <- aovEffectSize(aovObj, effectSize = "ges")
    }
  }

  if (sphericityCorrections & any(aovObj$ANOVA$DFn > 1)) {
    if (is.null(aovObj$"Sphericity Correction")) {
      stop("Sphericity Corrections not within aov(). Use ezANOVA().")
    }
    aovObj <- aovSphericityAdjustment(aovObj, sphericityCorrectionType, sphericityCorrectionAdjDF)
  }

  # p-value summary *** vs. ** vs *
  aovObj$ANOVA$"p<.05" <- pValueSummary(aovObj$ANOVA$p)
  aovObj$ANOVA         <- aovObj$ANOVA[aovObj$ANOVA$Effect != "(Intercept)", ]

  # round digits in table
  aovObj <- aovRoundDigits(aovObj)

  if (removeSumSquares) {
    aovObj$ANOVA$SSn <- NULL
    aovObj$ANOVA$SSd <- NULL
  }

  if (is.null(aovObj$means)) {
    if (is.null(aovObj$aov)) {
      stop("Call ezANOVA with return_aov = TRUE for marginal means.")
    }
    aovObj$means <- stats::model.tables(aovObj$aov, type = "mean")
  }

  aovDispTable(aovObj)

  return(aovObj)

}



#' @title effectsizeValueString
#'
#' @description Returns required Latex formatted string for effect
#' size (partial eta squared) = XXX for R/knitr integration.
#' Returns values to 2 sig decimal places.
#'
#' @param aovObj Output from aov or ezANOVA (NB. ezANOVA must be called with detailed = TRUE)
#' @param effect The effect within the ANOVA table to return
#' @param effectSize pes (partial eta squared) vs. ges (generalised eta squared)
#'
#' @return character
#'
#' @examples
#' # Example 1:
#' # create dataframe and add data with 2(Comp: comp vs. incomp) and 2(Side: left vs. right)
#' dat <- createDF(nVP = 20, nTrl = 1,
#'                 design = list("Comp" = c("comp", "incomp"),
#'                               "Side" = c("left", "right")))
#'
#' dat <- addDataDF(dat, RT = list("Comp:Side comp:left"    = c(500, 150, 100),
#'                                 "Comp:Side comp:right"   = c(500, 150, 100),
#'                                 "Comp:Side incomp:left"  = c(520, 150, 100),
#'                                 "Comp:Side incomp:right" = c(520, 150, 100)))
#'
#' aovRT <- aov(RT ~ Comp*Side + Error(VP/(Comp*Side)), dat)
#' aovRT <- aovTable(aovRT)
#'
#' pesString <- effectsizeValueString(aovRT, "Comp")  # partial eta squared
#' pesString <- effectsizeValueString(aovRT, "Comp:Side")
#'
#' # or using ezANOVA
#' library(ez)
#' aovRT <- ezANOVA(dat, dv=.(RT), wid = .(VP), within = .(Comp, Side),
#'                  return_aov = TRUE, detailed = TRUE)
#' aovRT <- aovTable(aovRT)
#'
#' pesString <- effectsizeValueString(aovRT, "Comp")  # partial eta squared
#' pesString <- effectsizeValueString(aovRT, "Comp:Side")
#'
#' @export
effectsizeValueString <- function(aovObj, effect, effectSize = "pes"){

  if (is.null(aovObj$ANOVA)) {
    stop("aovObj does not have appropriate ANOVA table")
  }
  if (!effectSize %in% c("pes", "ges")) {
    stop("effectSize not recognized")
  }

  if (!effectSize %in% names(aovObj$ANOVA)) {
    stop(paste0(effectSize, " not in ANOVA table!"))
  }

  if (effectSize == "pes") {
    effectSizeValue  <- aovObj$ANOVA[, "pes"][aovObj$ANOVA$Effect == effect]
    return(paste0("$\\eta_{p}^2$ = ", effectSizeValue))
  } else if (effectSize == "ges") {
    effectSizeValue <- aovObj$ANOVA[, "ges"][aovObj$ANOVA$Effect == effect]
    return(paste0("$\\eta_{G}^2$ = ", effectSizeValue))
  }

}



#' @title fValueString
#'
#' @description Returns required Latex formatted string for \emph{F}(df1, df2) = XXX
#' for R/knitr integration. For example, \emph{F}(1, 23) = 3.45.
#' Returns values to 2 sig decimal places.
#'
#' @param aovObj Output from aov or ezANOVA (NB. ezANOVA must be called with detailed = TRUE)
#' @param effect The effect within the ANOVA table to return
#'
#' @return character
#'
#' @examples
#' # Example 1:
#' # create dataframe and add data with 2(Comp: comp vs. incomp) and 2(Side: left vs. right)
#' dat <- createDF(nVP = 20, nTrl = 1,
#'                 design = list("Comp" = c("comp", "incomp"),
#'                               "Side" = c("left", "right")))
#'
#' dat <- addDataDF(dat, RT = list("Comp:Side comp:left"    = c(500, 150, 100),
#'                                 "Comp:Side comp:right"   = c(500, 150, 100),
#'                                 "Comp:Side incomp:left"  = c(520, 150, 100),
#'                                 "Comp:Side incomp:right" = c(520, 150, 100)))
#'
#' # or using ezANOVA
#' library(ez)
#' aovRT <- ezANOVA(dat, dv=.(RT), wid = .(VP), within = .(Comp, Side),
#'                  return_aov = TRUE, detailed = TRUE)
#' aovRT <- aovTable(aovRT)
#'
#' fString <- fValueString(aovRT, "Comp")
#' fString <- fValueString(aovRT, "Comp:Side")
#'
#' @export
fValueString <- function(aovObj, effect){

  if (is.null(aovObj$ANOVA)) {
    stop("aovObj does not have appropriate ANOVA table")
  }

  DFn    <- aovObj$ANOVA[, "DFn"][aovObj$ANOVA$Effect == effect]
  DFd    <- aovObj$ANOVA[, "DFd"][aovObj$ANOVA$Effect == effect]
  fValue <- aovObj$ANOVA[, "F"][aovObj$ANOVA$Effect == effect]

  return(paste0("\\emph{F}", "(", DFn, ", ", DFd, ") = ", fValue))

}



#' @title meanStrAov
#'
#' @description Returns marginal means from ezANOVA object for requested effect in Latex format.
#' Assumes means added to aovObj (e.g., aovObj$means <- model.tables(aovObj$aov, type = "mean").
#'
#' @param aovObj Output from aov or ezANOVA (NB. ezANOVA must be called with detailed = TRUE)
#' @param effect Effect to return
#' @param level Level of effect
#' @param unit "ms" vs. "mv" vs. "\%"
#' @param numDigits "ms" vs. "mv" vs. "\%"
#'
#' @return character
#'
#' @examples
#' # Example 1:
#' # create dataframe and add data with 2(Comp: comp vs. incomp) and 2(Side: left vs. right)
#' dat <- createDF(nVP = 20, nTrl = 1,
#'                 design = list("Comp" = c("comp", "incomp"),
#'                               "Side" = c("left", "right")))
#'
#' dat <- addDataDF(dat, RT = list("Comp:Side comp:left"    = c(500, 150, 100),
#'                                 "Comp:Side comp:right"   = c(500, 150, 100),
#'                                 "Comp:Side incomp:left"  = c(520, 150, 100),
#'                                 "Comp:Side incomp:right" = c(520, 150, 100)))
#'
#' aovRT <- aov(RT ~ Comp*Side + Error(VP/(Comp*Side)), dat)
#' aovRT <- aovTable(aovRT)
#'
#' meanString <- meanStrAov(aovRT, "Comp", "comp")
#' meanString <- meanStrAov(aovRT, "Comp:Side", "incomp:left")
#'
#' # or using ezANOVA
#' library(ez)
#' aovRT <- ezANOVA(dat, dv=.(RT), wid = .(VP), within = .(Comp, Side),
#'                  return_aov = TRUE, detailed = TRUE)
#' aovRT <- aovTable(aovRT)
#'
#' meanString <- meanStrAov(aovRT, "Comp", "comp")
#' meanString <- meanStrAov(aovRT, "Comp:Side", "incomp:left")
#'
#' @export
meanStrAov <- function(aovObj, effect, level, unit = "ms", numDigits = 0) {

  if (is.null(aovObj$ANOVA)) {
    aovObj <- aovTable(aovObj)  # convert base aov output
  }

  row <- which(aovObj$ANOVA$Effect == effect) + 1
  dat <- as.data.frame.table(aovObj$means$tables[[row]], responseName = "DV")

  effect <- unlist(strsplit(effect, ":"))
  level  <- unlist(strsplit(level, ":"))

  out <- matrix(0, nrow(dat), length(effect))
  for (i in 1:length(effect)) {
    out[, i] <- dat[, effect[i]] == level[i]
  }

  if (length(effect) == 1) {
    reqRow <- which(out[, 1] == 1)
  } else {
    reqRow <- which(rowSums(out[, 1:length(effect)]) == length(effect))
  }

  return(numValueString(dat[reqRow, "DV"], unit = unit, numDigits = numDigits))

}



#' @title printAovMeans
#'
#' @description Returns Latex formatted table of marginal means from model.tables.
#' Uses printTable (xtable) latex package with some basic defaults.
#' For more examples, see R package xtable
#' @param ... Output from aov or ezANOVA (NB. ezANOVA must be called with detailed = TRUE)
#' @param caption Title for the table
#' @param digits Number of digits to round to
#' @param dv Name of the dependent variable (e.g., "ms", "\%")
#'
#' @return character
#'
#' @examples
#' # Example 1:
#' # create dataframe
#' dat <- createDF(nVP = 6, nTrl = 1,
#'                 design = list("Comp" = c("comp", "incomp")))
#'
#' dat <- addDataDF(dat, RT = list("Comp comp"   = c(500, 150, 100),
#'                                 "Comp incomp" = c(520, 150, 100)))
#'
#' aovRT <- aov(RT ~ Comp + Error(VP/(Comp)), dat)
#' aovRT <- aovTable(aovRT)
#' printAovMeans(aovRT, digits = 3, dv = "ms")  # latex formatted
#'
#' # or using ezANOVA
#' library(ez)
#' aovRT <- ezANOVA(dat, dv=.(RT), wid = .(VP), within = .(Comp), return_aov = TRUE, detailed = TRUE)
#' aovRT <- aovTable(aovRT)
#' printAovMeans(aovRT, digits = 0, dv = "ms")  # latex formatted
#'
#' @export
printAovMeans <- function(..., caption = "Mean", digits = 3, dv = "ms") {

  aovObj <- list(...)
  for (i in seq(1:length(aovObj))) {
    if (is.null(aovObj[[i]]$means)) {
      stop("aovObj does not have appropriate marginal means")
    }
  }
  if (!length(digits) %in% c(1, length(aovObj))) {
    stop("length digits must equal 1 or number of ezObj inputs")
  }
  if (!length(dv) %in% c(1, length(aovObj))) {
    stop("dv length must equal 1 or number of ezObj inputs")
  }

  # format some common Latex strings within the caption label
  caption <- gsub("%", "\\%",      caption, fixed = TRUE)
  caption <- gsub("_", "\\_",      caption, fixed = TRUE)
  caption <- gsub("mV", "$\\mu$V", caption, fixed = TRUE)

  # format some common Latex strings within the dv label
  dv <- gsub("%", "\\%",      dv, fixed = TRUE)
  dv <- gsub("_", "\\_",      dv, fixed = TRUE)
  dv <- gsub("mV", "$\\mu$V", dv, fixed = TRUE)

  for (i in 2:(length(aovObj[[1]]$means$n) + 1)) {

    tab <- as.data.frame.table(aovObj[[1]]$means$tables[[i]])
    names(tab)[ncol(tab)] <- dv[1]
    if (length(aovObj) > 1) {
      for (j in 2:length(aovObj)) {
        tab <- cbind(tab, as.data.frame.table(aovObj[[j]]$means$tables[[i]]))
        names(tab)[ncol(tab)] <- dv[j]
      }
    }

    # remove duplicated factor columns
    colsToRemove <- intersect(which(duplicated(colnames(tab))), which(unlist(lapply(tab, is.factor))))
    if (length(colsToRemove) > 0) {
      tab <- tab[, -c(colsToRemove)]
    }

    printTable(tab,
               caption = paste0(caption, ": ", names(aovObj[[1]]$means$tables[i])),
               digits = digits)
  }

}



#' @title statStrAov
#'
#' @description Returns Latex formatted string from ANOVA required for R/knitr integration.
#' For example, \deqn{F(1, 20) = 8.45, p < 0.01, pes = 0.45}
#' Returns values to 2 sig decimal places and < 0.01, < 0.001 for p values.
#'
#' @param aovObj Output from aov or ezANOVA (NB. ezANOVA must be called with detailed = TRUE)
#' @param effect The effect required from the anova table
#'
#' @return NULL
#'
#' @examples
#' # Example 1:
#' # create dataframe and add data with 2(Comp: comp vs. incomp) and 2(Side: left vs. right)
#' dat <- createDF(nVP = 20, nTrl = 1,
#'                 design = list("Comp" = c("comp", "incomp"),
#'                               "Side" = c("left", "right")))
#'
#' dat <- addDataDF(dat, RT = list("Comp:Side comp:left"    = c(500, 150, 100),
#'                                 "Comp:Side comp:right"   = c(500, 150, 100),
#'                                 "Comp:Side incomp:left"  = c(520, 150, 100),
#'                                 "Comp:Side incomp:right" = c(520, 150, 100)))
#'
#' aovRT <- aov(RT ~ Comp*Side + Error(VP/(Comp*Side)), dat)
#' aovRT <- aovTable(aovRT)
#'
#' aovString <- statStrAov(aovRT, "Comp")
#' aovString <- statStrAov(aovRT, "Comp:Side")
#'
#'
#' # or using ezANOVA
#' library(ez)
#' aovRT <- ezANOVA(dat, dv=.(RT), wid = .(VP), within = .(Comp, Side),
#'                  return_aov = TRUE, detailed = TRUE)
#' aovRT <- aovTable(aovRT)
#'
#' aovString <- statStrAov(aovRT, "Comp")
#' aovString <- statStrAov(aovRT, "Comp:Side")
#'
#' @export
statStrAov <- function(aovObj, effect) {

  if (is.null(aovObj$ANOVA)) {
    aovObj <- aovTable(aovObj)  # convert base aov output
  }

  fString <- fValueString(aovObj, effect)
  pString <- pValueString(aovObj$ANOVA[, "p"][aovObj$ANOVA$Effect == effect])
  eString <- effectsizeValueString(aovObj, effect)
  sString <- sphericityValueString(aovObj, effect)

  if (is.null(sString)) {
     return(paste0(fString, ", ", pString, ", ", eString))
  } else {
    return(paste0(fString, ", ", pString, ", ", eString, ", ", sString))
  }

}



#' @title sphericityValueString
#'
#' @description Returns required Latex formatted string for sphericity epsilon
#' values (HF, GG) = XXX for R/knitr integration. Returns values
#' to 2 sig decimal places.
#'
#' @param aovObj The returned object from a call to ezANOVA
#' @param effect The effect within the ANOVA table to return
#'
#' @return character
#'
#' @examples
#' # Example 1
#' # create dataframe and add data with 3(Comp: neutral vs. comp vs. incomp) levels
#' dat <- createDF(nVP = 20, nTrl = 1,
#'                 design = list("Comp" = c("neutral", "comp", "incomp")))
#'
#' dat <- addDataDF(dat, RT = list("Comp neutral" = c(510, 150, 100),
#'                                 "Comp comp"    = c(500, 150, 100),
#'                                 "Comp incomp"  = c(520, 150, 100)))
#'
#' # repeated measures ANOVA using ezANOVA
#' library(ez)
#' aovRT <- ezANOVA(dat, dv=.(RT), wid = .(VP), within = .(Comp),
#'                  return_aov = TRUE, detailed = TRUE)
#' aovRT <- aovTable(aovRT)
#'
#' sphericityValue <- sphericityValueString(aovRT, "Comp")
#'
#' @export
sphericityValueString <- function(aovObj, effect){

  sphericityString = NULL
  if ("eps" %in% names(aovObj$ANOVA)) {
    if (aovObj$ANOVA[, "DFn"][aovObj$ANOVA$Effect == effect] != 1) {
      epsValue <- aovObj$ANOVA[, "eps"][aovObj$ANOVA$Effect == effect]
      sphericityString <- paste0("$\\epsilon$ = ", epsValue)
    }
  }
  return(sphericityString)
}
