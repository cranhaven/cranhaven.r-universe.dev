#' Example \code{catmap} Data
#'
#' An example data set for use with \code{catmap}. All input data should
#'  have the header as part of the file and either 0 or NA values for entries
#'  not relevant to that particular study design. For example, TDT studies
#'  should have the caserisk, controlrisk, casenotrisk and controlnotrisk
#'  values set to either 0 or NA.
#'
#' @docType data
#' @format A \code{data.frame} with 5 observations and 8 variables.
#' \itemize{
#' \item{name:}{ a factor with study name and optionally year of publication.
#'  NOTE: if year of publication is included there must be no space between
#'  study name and year. A comma or underscore works nicely (e.g.,
#'  \code{Abrams,2001} \code{Peter,2002} \code{Todd,2003}
#'  \code{Wei,2007} \code{Yu,2007})
#' }\item{study:}{ a numeric vector containing 1 if the study is TDT and 2 if the study is case-control
#' }\item{t:}{ a numeric vector containing counts of alleles transmitted in the TDT study
#' }\item{nt:}{ a numeric vector containing counts of alleles not transmitted in the TDT study
#' }\item{caserisk:}{ a numeric vector containing counts of risk alleles in cases
#' }\item{controlrisk:}{ a numeric vector containing counts of risk alleles in controls
#' }\item{casenotrisk:}{ a numeric vector containing counts of non-risk alleles in cases
#' }\item{controlnotrisk:}{ a numeric vector containing counts of non-risk alleles in controls
#'  }}
#'
#' @author Algorithm designed and implemented by Kristin K. Nicodemus.
#'  Code modified and updated by Thom Quinn.
#' @seealso \code{\link{catmap}}, \code{\link{catmap.forest}},
#'  \code{\link{catmap.sense}}, \code{\link{catmap.cumulative}},
#'  \code{\link{catmap.funnel}}
"catmapdata"

#' @importFrom grDevices dev.off graphics.off pdf
#' @importFrom graphics abline mtext par plot points segments
#' @importFrom stats pchisq qnorm
#' @importFrom utils read.table
NULL

#' Make Forest Plot
#'
#' A back-end wrapper function used to make forest plots.
#'
#' @inheritParams catmap.funnel
#' @param summary A character string. The kind of summary statistic
#'  to plot. Select from "fixed" or "random".
#' @param mean,lower,upper,study Numeric or character vectors.
#'  Used to guide the construction of the forest plot.
#' @param main A character string. The figure title.
makeForest <- function(catmapobject, summary = "", main = "Main Title", mean = exp(catmapobject$logOR),
                       lower = catmapobject$lbci.fe, upper = catmapobject$ubci.fe,
                       study = c("Study", sub(",", " ", catmapobject$studyname))){

  dat <- data.frame(
    lower = c(NA, lower),
    mean  = c(NA, mean),
    upper = c(NA, upper)
  )

  summaryv <- c(TRUE, rep(FALSE, length(mean)))
  gridline <- NULL # excluded unless 'summary' is specified
  weights <- NULL # excluded unless 'summary' is specified

  if(summary == "fixed"){

    main <- "Inverse Variance (Fixed-Effects) ORs"
    dat <- rbind(dat, c(NA, NA, NA))
    dat <- rbind(dat, c(catmapobject$lbci, catmapobject$combinedOR, catmapobject$ubci))
    study <- c(study, NA, "Summary")
    summaryv <- c(summaryv, FALSE, TRUE)
    gridline <- list("grid" = log(catmapobject$combinedOR))
    weights <- c("Weight (%)", round(catmapobject$weight / sum(catmapobject$weight), 2), NA, NA)

  }else if(summary == "random"){

    main <- "DerSimonian & Laird (Random-Effects) ORs"
    dat <- rbind(dat, c(NA, NA, NA))
    dat <- rbind(dat, c(catmapobject$lbci.dsl, catmapobject$OR.dsl, catmapobject$ubci.dsl))
    study <- c(study, NA, "Summary")
    summaryv <- c(summaryv, FALSE, TRUE)
    gridline <- list("grid" = log(catmapobject$OR.dsl))
    weights <- c("Weight (%)", round(catmapobject$weight.dsl / sum(catmapobject$weight.dsl), 2), NA, NA)
  }

  tex <- cbind(
    study,
    c(as.character(round(dat$mean, 2))),
    ifelse(!is.na(dat$lower), paste0("(", as.character(round(dat$lower, 2)), ", "), ""),
    ifelse(!is.na(dat$upper), paste0(as.character(round(dat$upper, 2)), ")"), "")
  )

  tex[1, ] <- c(tex[1, 1], "OR", paste0("(", catmapobject$ci*100, "%"), "CI)")
  if(!is.null(weights)) tex <- cbind(tex, weights)
  colnames(tex) <- NULL

  f <- do.call(forestplot::forestplot, c(list(
    labeltext = tex, mean = dat$mean, lower = dat$lower, upper = dat$upper,
    graph.pos = 2, zero = 1, is.summary = summaryv, xlog = TRUE, new_page = FALSE,
    col = forestplot::fpColors(box = "black", line = "grey", summary = "red"),
    title = main), gridline)
  )

  return(f)
}

cleanSink <- function(name, results, sep = ""){

  cat(name, "\n")
  for(i in 1:ncol(results)) cat(cat(results[1, i], results[2, i], sep = ": "), "\n")
}
