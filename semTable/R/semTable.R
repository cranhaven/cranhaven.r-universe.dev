##' Creates Structural Equation Modeling Tables
##'
##' Creates LaTeX markup for structural equation modeling output
##' tables in the style of the American Psychological
##' Association(APA). Input objects should be created by the
##' "\code{lavaan}" package.
##'
##' The argument paramSets determines the inclusion of estimate sections.
##' One can specify "all", which means that all types of parameters that
##' we can find in the fitted model are presented.  Otherwise, a subset
##' of parameter sets can be chosen by the user.
##' \itemize{
##' \item "composites" are predictor coefficients in formative constructs
##' \item "loadings" are the factor loadings in the model.
##' \item "slopes" are the regression slopes in the model.
##' \item "intercepts" are the estimated constants in the measurement
##'      models.
##' \item "residualvariances" are the observed variable residual variances.
##' \item "residualcovariances" are the observed covariances among
##' residuals of observed variables.
##' \item "latentvariances" are the variances of unobserved variables.
##' \item "latentcovariances" are the covariances between unobserved variables.
##' \item "latentmeans" are means of unobserved variables
##' \item "thresholds" arise in latent
##' response variates (non-numeric indicator data).
##' \item "constructed" are parameters that are calculated from a formula
##'       in the model specification, such as an indirect path c=a*b.
##' \item "fits" the summary indicators of the mismatch between
##' the theoretical and observed covariance matrices, such as
##' RMSEA, CLI, TFI. While the fits are not technically parameters, they
##' are displayed in the same block style as parameters
##' }
##'
##' The columns parameter is used to specify different columns,
##' while columnLabels will alter the displayed labels for them.
##' 
##' @param object A lavaan object (e.g., returned by cfa() or sem()),
##'     or a named list of lavaan objects, e.g., \code{list("Model A"
##'     = obj1, "Model B" = obj2)}. Results will be displayed side by
##'     side.
##' @param paramSets Parameter sets to be included for each fitted
##'     object.  Valid values of the vector are \code{"all"} or a any
##'     of the following: \code{c("composites", "loadings", "slopes",
##'     "intercepts", "residualvariances", "residualcovariances",
##'     "latentmeans", "latentvariances", "latentcovariances",
##'     "thresholds", "constructed", "fits")}. Default is "all", any
##'     of the estimates present in the fitted model that are listed
##'     in the previous sentence will be included in the output. For
##'     the sake of simplicity, we now allow one vector here, which
##'     applies to all models in the object list.
##' @param paramSetLabels Named vector, used to supply alternative
##'     pretty printing labels for parameter sets. The default values
##'     are \code{c("composites" = "Composites",
##'     "loadings"= "Factor Loadings", "slopes" = "Regression Slopes",
##'     "intercepts" = "Intercepts", "means"= "Means",
##'     "residualvariances" = "Residual Variances",
##'     "residualcovariances" = "Residual Covariances", "variances" =
##'     "Variances", "latentvariances" = "Latent Variances",
##'     "latentcovariances" = "Latent Covariances", "latentmeans" =
##'     "Latent Intercepts", "thresholds" = "Thresholds",
##'     "constructed" = "Constructed", "fits" = "Fit Indices")}.  The
##'     paramSetLabels argument must be a named vector that overrides
##'     some or all of the default names.
##' @param columns A vector naming estimates to appear for each model.
##'     The allowed columns are "est", "se", "z", "p", "rsquare",
##'     "estse", "eststars", "estsestars". The first 5 have the usual
##'     meanings, while "estse" (can also be written \code{"est(se)"})
##'     displays as, for example "1.21(0.23)", and the last 2 are to
##'     include "significance stars".  \code{"eststars"} shows as
##'     "1.21***" and \code{"estsestars"} (or \code{"est(se)stars"})
##'     displays as "1.21(0.23)**". See parameter \code{alpha}. One
##'     may request different columns for each model by providing a
##'     named list of vectors.  Use model names in the list,
##'     \code{list("Model A" = c("est", "se"), "Model B" = c("estse",
##'     "p"))}.
##' @param columnLabels A named vector of "pretty labels" for the
##'     headings in the table. The default labels are \code{c("est" =
##'     "Estimate", se = "Std. Err.", z = "z", p = "p", rsquare =
##'     "R Square", estse = "Estimate(Std.Err."), eststars =
##'     "Estimate", estsestars = "Estimate(Std.Err.)")}.
##' @param fits Summary indicators to be included. May be a list, one
##'     for each model provided, otherwise the same fit indicators
##'     will be presented for each model. Any of the fit indicators
##'     provided by \code{lavaan::fitMeasures(object)} are allowed:
##'     \code{c("npar", "fmin", "chisq", "df", "pvalue",
##'     "baseline.chisq", "baseline.df", "baseline.pvalue", "cfi",
##'     "tli", "nnfi", "rfi", "nfi", "pnfi", "ifi", "rni", "logl",
##'     "unrestricted.logl", "aic", "bic", "ntotal", "bic2", "rmsea",
##'     "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue", "rmr",
##'     "rmr_nomean", "srmr", "srmr_bentler", "srmr_bentler_nomean",
##'     "srmr_bollen", "srmr_bollen_nomean", "srmr_mplus",
##'     "srmr_mplus_nomean", "cn_05", "cn_01", "gfi", "agfi", "pgfi",
##'     "mfi", "ecvi")}. The return for "chisq" will include markup
##'     for degrees of freedom and p value. If user specifies
##'     \code{NULL}, or if "fits" is excluded from \code{paramSets},
##'     all fit indicators are omitted.
##' @param fitLabels Labels for some or all of the fit measures
##'     requested by the fits parameter, e.g. \code{c(rmsea =
##'     "Root Mean Square Error of Approximation", cli = "CLI")}. The
##'     default labels are the upper-case fits names (except for
##'     "chisq", where a Greek letter is supplied when possible).
##' @param varLabels Named vector of labels to replace variable names
##'     in column 1 of SEM table.
##' @param groups All groups will be printed, unless a subset is
##'     requested here. Estimates for all groups will be displayed
##'     side by side. If ONLY SOME groups should be included, then
##'     specify groups as either names of fit objects or as integers
##'     for elements of the groups vector.
##' @param type Choose "latex", "html", or "csv"
##' @param file Base name for output file. This function will insert
##'     suffix, either "tex", "html" and "csv".
##' @param table.float If TRUE, create a LaTeX floating table object
##'     in which the tabular created here will reside. Default is
##'     FALSE.
##' @param caption Caption for table (if table.float=TRUE) or
##'     longtable output. Ignored otherwise.
##' @param label LaTeX label for this object (for
##'     cross-references). Only used if table.float = TRUE or
##'     longtable = TRUE.
##' @param longtable If TRUE, use longtable for LaTeX
##'     documents. Default is FALSE. If true, \code{table.float}
##'     argument is ignored.
##' @param print.results If TRUE, marked up result will be displayed
##'     within the session. Otherwise, result is returned silently and
##'     user can use \code{cat} to dislay it. Don't use \code{print}
##'     because it inserts unwanted decorations.
##' @param centering Default "siunitx". For method used in previous
##'     editions, replace with "none".
##' @param alpha Thresholds for p-values that determine number of
##'     stars.  Defaults as \code{c(0.05, 0.01, 0.001)} for
##'     \code{c("*", "**", "***")}.
##' @importFrom stats pnorm
##' @importFrom lavaan lavInspect
##' @importFrom plyr mapvalues
##' @importFrom kutils checkCoercion
##' @importFrom kutils modifyVector
##' @importFrom stationery rnw2pdf
##' @return Markup for SEM table. Includes an attribute
##'     "markedResults", which can be converted to other markup
##'     formats by the function markupConvert.
##' @export
##' @author Ben Kite <bakite@@ku.edu> Paul Johnson <pauljohn@@ku.edu>
##' @examples 
##' ## Most of the examples were moved to the semTable vignette
##' require(lavaan)
##'
##' tempdir <- tempdir()
##' ## The example from lavaan's docs
##'  HS.model <- ' visual  =~ x1 + x2 + x3
##'                textual =~ x4 + x5 + x6
##'                speed   =~ x7 + x8 + x9'
##'  fit1 <- cfa(HS.model, data = HolzingerSwineford1939,
##'              std.lv = TRUE, meanstructure = TRUE)
##'  ## Try a LaTeX file first
##'  fit1.t1 <- semTable(fit1, columns = c("estse", "p"),
##'                      fits = c("chisq", "rmsea"), file = file.path(tempdir, "fit1.t1"),
##'                      varLabels = c("x1" = "hello"), type = "latex", print.results = FALSE)
##'  ## If you have a working version of pdflatex in your system path, 
##'  if (interactive()) testtable("fit1.t1.tex", tempdir)
##'  
##'  model <- "factor =~ .7*y1 + .7*y2 + .7*y3 + .7*y4
##'                   y1 | -1*t1 + 1*t2
##'                   y2 | -.5*t1 + 1*t2
##'                   y3 | -.2*t1 + 1*t2
##'                   y4 | -1*t1 + 1*t2"
##'  dat <- simulateData(model, sample.nobs = 300)
##'  
##'  testmodel <- "ExampleFactor =~ y1 + y2 + y3 + y4"
##'  
##'  fit4 <- cfa(testmodel, data = dat, ordered = colnames(dat),
##'              std.lv = FALSE)
##'  
##'  fit4.t1 <- semTable(fit4, paramSets = c("loadings", "thresholds",
##'      "residualvariances"), fits = c("tli", "chisq"),
##'      fitLabels = c(tli = "TLI", chisq = "chisq"), type = "html",
##'      file=file.path(tempdir, "fit4.t1") )
##'  if(interactive()) browseURL(attr(fit4.t1, "file"))
##'  fit4.t2 <- semTable(fit4, fits = c("rmsea", "cfi", "chisq"),
##'                fitLabels = c(rmsea = "Root M.SQ.E.A", cfi = "CompFitIdx", chisq = "chisq"),
##'                type = "latex", file=file.path(tempdir, "fit4.t2"))
##'  if (interactive()) testtable("fit4.t2.tex", tempdir)
semTable <- function(object, file = NULL, paramSets = "all", paramSetLabels,
                     columns = c(est = "Estimate", se = "SE", z = "z", p = "p"),
                     columnLabels, fits = c("chisq", "cfi", "tli", "rmsea"),
                     fitLabels = toupper(fits), varLabels = NULL,
                     groups = NULL, type = "latex", table.float = FALSE,
                     caption = NULL, label = NULL, longtable = FALSE,
                     print.results = TRUE, centering = "siunitx",
                     alpha =  c(0.05, 0.01, 0.001)) {
    ## do.call(rbind, alist) unexpectedly converts characters to factors.
    ## it does not accept stringsAsFactors=FALSE,
    ## So set globally to avoid hassle
    options.orig <- options()
    options(stringsAsFactors = FALSE)
    on.exit(options(options.orig))
    
    ## local shorthand to replace the verbose
    ## formatC(round(chimeas$stat, 3), format = 'f', digits = 2)
    ## with frnd(chimeas$stat)
    ## For reasons I don't understand, most prevalent example here
    ## is round to 3, then show 2 digits
    frnd <- function(x, rnd = 2, digits = 2) {
        y <- formatC(round(x, rnd), format = 'f', digits = digits)
        ## regex replace " NA" with "". 
        y[grep("\\s*NA",y )] <- ""
        y
    }

    ## 20171021
    ## The Maker functions all had copies of a standard stanza
    ## that used formatC and round in this way. Aggregate those
    ## actions
    ## TODO: safety check for operations on columns that don't exist
    ## trows must be a row subset of the parameters table
    roundSubtable <- function(trows){
        cols <- intersect(c("est", "se", "stdest", "stdse", "z", "rsquare"), colnames(trows))
        for (i in cols) trows[ , i] <- frnd(trows[ , i])
        trows$p <- frnd(trows$p, 3, 3)
        trows$p <- gsub("0\\.", "\\.", trows$p)
        trows$est <- ifelse(trows$free == 0, paste0(trows$est, "_FIXED_"), trows$est)
        if (any(trows$free == 0)) FIXED <<- TRUE
        trows[trows$free == 0, intersect(colnames(trows), c("se", "z", "p", "stdse"))] <- ""
        ## missings are a problem, maybe in other variables too
        if("rsquare" %in% colnames(trows)){
            trows$rsquare <- ifelse(!is.na(trows$rsquare), trows$rsquare, "")
        }
        trows
    }


    ## If paramSets != "all", follow user request to select paramSets for table
    ## If paramSets == "all", then find out what params are available and
    ## possibly (not now!) organize them in correct order
    ## These don't need to be in order!
    fixParamSets <- function(parameters, paramSets, paramSetLabels) {
        if (length(paramSets) > 1 || paramSets != "all") {
            paramSets <- unique(paramSets)
            if (any(!paramSets %in% names(paramSetLabels))){
                MESSG <- "fixParamSets: invalid paramSets"
                stop(MESSG)
            }
            return(paramSets)
        }

        params <- unique(parameters$paramType)
        params
    }

    ## relabel the "paramType" column in a parTable data frame
    ##
    ## Re-label the "paramType", the parameterSet indicator for each
    ## row in a parTable. Identifies "residualvariances",
    ## "residualcovariances", "latentvariances" "latentcovariances",
    ## "intercepts" and "latentmeans"
    ##  
    ## @param parameters A parTable data frame, with estimates and some labels. 
    ## @return 
    ## @author Paul Johnson
    insertParamTypes <- function(parameters){
        paramops <- c("=~" = "loadings", "<~" = "composites",
                      "~" = "slopes", "~1" = "means",
                      "~~" = "variances", "|" = "thresholds",
                      ":=" = "constructed", "==" = "constraints")

        variables <- attr(parameters, "variables")
        latents <- attr(parameters, "latents")
        
        parameters[ , "paramType"] <- NA
        parameters[ , "paramType"] <- paramops[parameters$op]
        
        parameters[(parameters$lhs %in% variables &
                    parameters$rhs %in% variables &
                    parameters$lhs == parameters$rhs &
                    parameters$op == "~~"), "paramType"] <- "residualvariances"

        parameters[parameters$lhs %in% variables &
                   parameters$rhs %in% variables &
                   parameters$lhs != parameters$rhs &
                   parameters$op == "~~", "paramType"] <- "residualcovariances"
        parameters[parameters$rhs %in% latents &
                    parameters$lhs %in% latents &
                    parameters$lhs == parameters$rhs &
                    parameters$op == "~~", "paramType"] <- "latentvariances"
        parameters[parameters$rhs %in% latents &
                   parameters$lhs %in% latents &
                   parameters$lhs != parameters$rhs &
                   parameters$op == "~~", "paramType"] <- "latentcovariances"
        parameters[parameters$lhs %in% variables &
                            parameters$op == "~1", "paramType"] <- "intercepts"
        parameters[parameters$lhs %in% latents &
                   parameters$op == "~1", "paramType"] <- "latentmeans"
        ## only keep rows that are not contraint rows
        parameters <- parameters[parameters$op != "==", ]
        ## Remove rows also that have "~*~"
        ## TODO: 20171107, find out what that means, it happens in ordinal output
        parameters <- parameters[parameters$op != "~*~", ]
        parameters
    }
    
    getParamTable <- function(onemodel, paramSets, paramSetLabels){
        createEstSE <- function(dframe, se = TRUE, stars = FALSE){
            dframe <- roundSubtable(dframe)
            separt <- if(se) paste0("{(", dframe[ , "se"], ")}") else ""
            starpart <- if(stars)dframe[ , "starsig"] else ""
            ifelse(dframe[ , "free"] != 0,
                   paste0(dframe[ , "est"], separt,
                          starpart),
                   paste0(dframe[ , "est"]))
        }
        if(class(onemodel)[1] != "lavaan"){
            stop("onemodel is not a lavaan output onemodel.")
        }
        parList <- onemodel@ParTable[
                              intersect(names(onemodel@ParTable),
                                        c("lhs", "op", "rhs", "free", "group",
                                          "est", "se", "label", "plabel"))]
        parameters <- as.data.frame(parList, stringsAsFactors=FALSE)
        ## items previously global are specialized to this model
        attr(parameters, "variables") <- unique(unlist(onemodel@Data@ov.names))
        attr(parameters, "latents") <- unique(unlist(onemodel@pta$vnames$lv))
        parameters <- insertParamTypes(parameters)
        attr(parameters, "params") <- fixParamSets(parameters, paramSets, paramSetLabels)
        ## keep only rows that are in desired params sets
        parameters <- parameters[parameters$paramType %in% attr(parameters, "params"), ]
        
        ## fixed cannot be 0 for constructed variables
        parameters$free[parameters$paramType == "constructed"] <- 99
        
        rsquare <- lavInspect(onemodel, what = "rsquare")
        
        parameters$z <- ifelse(parameters$free != 0 & parameters$se != 0,
                               parameters$est/parameters$se, NA)
        parameters$p <- 2*pnorm(abs(parameters$z), lower.tail = FALSE)
        parameters$starsig <- starsig(parameters$p, alpha = alpha, symbols = starsymbols)
        parameters$starsig <- ifelse(grepl("^\\s*$", parameters$starsig), "_STAR0_", parameters$starsig)
        parameters$estse <- createEstSE(parameters)
        parameters$eststars <- createEstSE(parameters, se = FALSE, stars = TRUE)
        parameters$estsestars <- createEstSE(parameters, stars = TRUE)

        if(length(onemodel@Data@group.label) > 0L) {
            parameters$group.label <- unlist(onemodel@Data@group.label)[parameters$group]
        } else {
            parameters$group.label <- parameters$group
        }

        ## parse rsquare among
        ## "indicators"
        parameters[ , "rsquare"] <- as.numeric("")
        if (is.list(rsquare)){
            ## multi group model
            for(i in names(rsquare)){
                ##rsq is rsquare for group
                rsq <- rsquare[[i]]
                parameters[parameters$paramType == "loadings" &
                            parameters$group.label == i, "rsquare"] <-
                    rsq[parameters$rhs[parameters$paramType == "loadings" &
                                   parameters$group.label == i]]
                latentrsquares <- rsquare[[i]][parameters[parameters$paramType == "latentvariances" &
                                                           parameters$group.label == i, "lhs"]]
                parameters[parameters$paramType == "latentvariances" &
                           parameters$group.label == i , "rsquare"] <-
                    ifelse(!is.na(latentrsquares), latentrsquares, NA)
            }
        } else {
            ## one group model
            ## convoluted retrieval of rsquare by loading names
            parameters[parameters$paramType == "loadings", "rsquare"] <-
              rsquare[parameters$rhs[parameters$paramType == "loadings"]]
            ## "latentvariances"
            ## "latentsquares" are NA if there is no regression fitted,
            ## must be cautious here
            latentrsquares <- rsquare[parameters[parameters$paramType == "latentvariances", "lhs"]]
            parameters[parameters$paramType == "latentvariances", "rsquare"] <-
                ifelse(!is.na(latentrsquares), latentrsquares, NA)
        }
        parameters <- roundSubtable(parameters)
        parameters
    }

    ## The trows objects have an attribute "blocktitle" and this
    ## formats that
    applyTitleMarkup <- function(blocktitle){
        header  <- "_BR_"
        ## Tricky business b/c row markup does not call for _BOC_ in column 1,
        ## that's provided by "_BR_".
        header <- paste0(header, gsub("_CONTENT_", blocktitle$content, blocktitle$markup), "_EOR_")
        header
    }
    
    makeSubtableTitle <- function(blocktitle, colnum = 1, width = 1, center = TRUE, underline = TRUE){
        colalign <- if(center) "_BOMC" else "_BOML"
        ## Need a separator if colnum > 1. This is ripple effect of removing & from _BOMC_ definition.
        ## Because we assume it is always BOMC, now must add _BOC_ before _BOMC_
        prefix <- if (colnum > 1) paste0("_EOC_", rep("_BOC__EOC_", colnum - 2), "_BOC_")
        markup <- if(underline) {
                      paste0(prefix, colalign, width, "__UL__CONTENT__EOUL__EOMC_")
                  } else {
                      paste0(prefix, colalign, width, "__CONTENT__EOMC_")
                  }
        blocktitle <- list(content = blocktitle, markup = markup,
                           colnum = colnum, width = width)
        blocktitle
    }

    ##works for paramSets = "composites", "loadings" or "slopes"
    loadingMaker <- function(parameters, paramType = "loadings", colLabels, modelName) {
        report <- names(colLabels[[modelName]])
        totalNcolumns <- min(9,  length(unname(unlist(colLabels))))
        trows <- parameters[parameters$paramType == paramType,  , drop = TRUE]
        ## The split gotcha
        ## prevent accidental reordering of rows when the factor is created
        ## automatially
        lhsfactor <- factor(trows$lhs, levels = unique(trows$lhs))
        trowsplit <- split(trows, f = lhsfactor, drop = FALSE)
        info <- lapply(trowsplit, function(x){
            ## drop gotcha, if x has one row from df now it is a list.
            if(class(x)[1] == "list") x <- as.data.frame(x)
            vname <- unique(x$lhs)
            rownames(x) <- paste(paramType, vname, x[ , "rhs"], sep = ".")
            xlabels <- replace(x$rhs, names(varLabels), varLabels)
            ## 20180509: Get rid of underscores in var names, no matter what
            xlabels <- gsub("_", ".", xlabels, fixed = TRUE)
            x <- data.frame(col1 = xlabels,  x[ , report, drop = FALSE])
            ## don't put "_BOC_" at beginning if in colnum 1
            ## 20180327: use varLabels for factor names too
            attr(x, "blocktitle") <- makeSubtableTitle(replace(vname, names(varLabels), varLabels),
                                                       colnum = 1,
                                                       width = 1,
                                                       center = FALSE,
                                                       underline = TRUE)
            class(x) <- c("trows", class(x))
            x
        })
        info
    }

    ## local immitation of mapvalues, with no warnings or NA checking
    replace <- function(x, from, to){
        if(is.null(from) || is.null(to)) return(x)
        idx <- match(x, from)
        idxNNA <- !is.na(idx)
        x[idxNNA] <- to[idx[idxNNA]]
        x
    }
    ## works for other paramSets, ones that are simple tables, no lists.
    ## replace the "maker" functions with one function.
    ## Only difference between maker functions was in calculation of col1 and
    ## the rownames, which we can customize here
    ## paramType in c("intercepts", "means" "latentmeans", "latentvariances","residualcovariances"
    ## Check for complications: "latentcovariances" "residualvariances"
    ## "thresholds" "covariances"
    parTableMaker <- function(trows, paramType, colLabels, modelName){
        report <- names(colLabels[[modelName]])
        totalNcolumns <- min(9,  length(unname(unlist(colLabels))))
        ## trows <- parameters[parameters$paramType == paramType,, drop = FALSE]
        if(dim(trows)[1] == 0) return (NULL)
        varlabslhs <- replace(trows$lhs, names(varLabels), varLabels)
        varlabsrhs <- replace(trows$rhs, names(varLabels), varLabels)
        varlabslhs <- gsub("_", ".",  varlabslhs, fixed = TRUE)
        varlabsrhs <- gsub("_", ".",  varlabsrhs, fixed = TRUE)
        if (paramType == "thresholds"){
            thresnum <- substring(trows$rhs, 2, nchar(trows$rhs))
            trows$col1 <- paste0(varlabslhs, "(", thresnum, ")")
            rownames(trows) <- paste(paramType, trows[ , "lhs"], thresnum, sep = ".")
        } else if (paramType == "residualvariances"){
            rownames(trows) <- paste(paramType, trows[ , "lhs"], sep = ".")
            trows$col1 <- varlabslhs
        } else if (paramType %in% c("variances", "latentcovariances",  "residualcovariances")){
            trows$col1 <- paste0(varlabslhs, " w/", varlabsrhs)
            rownames(trows) <- paste(paramType, trows[ , "lhs"], trows[ , "rhs"],  sep = ".")
        ## } else if (paramType == "latentcovariances"){
        ##     trows$col1 <- paste0(varlabslhs, " w/", varlabsrhs)
        ##     rownames(trows) <- paste0(paramType, ".", trows[ , "lhs"], ".", trows[ , "rhs"])
        } else {
            trows$col1 <- varlabslhs
            rownames(trows) <- paste(paramType, trows[ , "lhs"], sep = ".")
        }
        trows <- data.frame(col1 = trows$col1, trows[ , report, drop = FALSE])
        attr(trows, "blocktitle") <- makeSubtableTitle(paramSetLabels[paramType],
                                                  colnum = 2,
                                                  width = totalNcolumns,
                                                  underline = TRUE)
        class(trows) <- c("trows", class(trows))
        trows
    }
    

    ## Create elaborate markup for Chi-Square value
    ## uses "report" = columns to decide where to put p.
    getChiSq <- function(object, report){
        newdf <- data.frame(col1 = "_CHI2_", row.names = "chisq", stringsAsFactors = FALSE)
        newdf[ , report] <- ""
        ## If there is more than 1 test, use last one, and change label to
        ## "Scaled chisq"
        nOfTests <- length(object@Fit@test)
        chimeas <- object@Fit@test[[nOfTests]]
        ## address problem that similar named rows "chisq" were confused.
        if (nOfTests > 1){
            rname <- "chisqscaled"
            newdf[1 , "col1"] <- "Scaled _CHI2_"
            rownames(newdf)[1] <- rname
        }
        chimeas$stat <- frnd(chimeas$stat)
        chimeas$pvalue <- frnd(chimeas$pvalue, 3,  3)
        chimeas$pvalue <- gsub("0\\.", "\\.", chimeas$pvalue)
        chipstars <- if (any(c("eststars", "estsestars") %in% colnames(newdf))){
                         starsig(chimeas$pvalue, alpha = alpha, symbols = starsymbols)
                     } else {
                         ""
                     } 
        newdf[ , 2] <- paste0(chimeas$stat, "(", chimeas$df, ")", chipstars)
        if ("p" %in% colnames(newdf)) newdf[ , "p"] <- chimeas$pvalue
        newdf
    }


    ## 20180513: handle markup of paraset lists for loading and slopes
    markupList <- function(alist) {
        header <- applyTitleMarkup(attr(alist, "blocktitle"))
        #trowsf <- trows[ , c("col1", report)]
        res <- vapply(alist, markupTable, character(1))
        res2 <- paste(header, paste(res, collapse = " "))
        return(res2)
    } 
          
    ## A trows object is a matrix, this inserts formatting markup.
    ## trows also has attribute "blocktitle", which is used to create
    ## row 1 in the markup result
    markupTable <- function(trows) {
        trowsf <- trows
        for(i in 1:(NCOL(trowsf) -1)){
            trowsf[ , i] <- paste0(trowsf[ , i], "_EOC__BOC_")
        }
        trowsf[ , 1] <- paste0("_BR_", trowsf[, 1])
        trowsf[ , NCOL(trowsf)] <- paste0(trowsf[ , NCOL(trowsf)], "_EOC__EOR_")
        res <- paste0(apply(trowsf, 1, paste, collapse = ""), collapse = "\n")
        ## TODO 20171028 CAUTION: workaround here b/c blocktitle info sometimes missing, must
        ## go back find why it is missing
        if (!is.null(attr(trows, "blocktitle"))){
            header <- applyTitleMarkup(attr(trows, "blocktitle"))
            res <- paste0(header, "\n", res, "\n")
        }
        return(res)
    } 


    ## Work on one fitted model.  Cycle through process of
    ## 1. Retrieve parameters,
    ## 2. insert title attributes to be used when formatting
    ## Creates a list of tables, one for each parameter type.
    ## These are found by splitting on the "paramType" variable in
    ## the paramTable.
    extractParameters <- function(paramTable, colLabels, modelName){
        params <- attr(paramTable, "params")
        ## report was name used for varnames of columns for
        ## keeing, ex: c("est" "se" "z" "p")
        report <- names(colLabels[[modelName]])

        ## explore following as alternative concept to manage calculations
        ## creates a list of tables for each parameter type, so no need to do
        ## data selection within the table maker function
        paramTableSplit <- split(paramTable, f = paramTable$paramType, drop = FALSE)
        ## Still must treat loadings and slopes differently
        reslt <- list()
        for(jj in names(paramTableSplit)){
            if (jj %in% c("composites", "loadings", "slopes")){
                
                info <- loadingMaker(paramTableSplit[[jj]], paramType = jj,
                                     colLabels, modelName)
                if (!is.null(info)){
                    ## 20190517: following looks like error, why fixed at "loadings"
                    ## attr(info, "blocktitle") <- makeSubtableTitle(paramSetLabels["loadings"],
                    attr(info, "blocktitle") <- makeSubtableTitle(paramSetLabels[jj],
                                                                  colnum = 2,
                                                                  width = length(report))
                    class(info) <- c("trowsList", class(info))
                }
                reslt[[jj]] <- info
            } else {
                reslt[[jj]] <- parTableMaker(paramTableSplit[[jj]], paramType = jj, colLabels,
                                            modelName = modelName)
            }
        }

        reslt
    }

    ## Gather summary fit indicators
    fitMaker <- function(object, fitsLabeled, colLabels, modelName) {
        report <- names(colLabels[[modelName]])
        fits <- names(fitsLabeled[[modelName]])
        fitLabels <- fitsLabeled[[modelName]]
        totalNcolumns <- min(9,  length(unname(unlist(colLabels))))
        ## retrieve fit values, will be named vector
        fitmeas <- lavaan::fitMeasures(object)[]
        fitmeas <- sapply(fitmeas, frnd)
        fitmeas <- fitmeas[fits]
        fitmeas[is.na(fitmeas)] <- ""
        
        ## re-label the fit indices
        
        info <- data.frame(col1 = fitLabels[names(fitmeas)], row.names = names(fitmeas))
        info[ , report] <- ""
        info[ , report[1]] <- fitmeas
        
        ## chisq is a special presentation, comes back as a one row data frame
        if ("chisq" %in% names(fitmeas)){
            yy <- getChiSq(object, report)["chisq", ]
            rname <- rownames(yy)[1]
            info[rname, ] <- yy
        }
        
        attr(info, "blocktitle") <- makeSubtableTitle(paramSetLabels["fits"],
                                                      colnum = 2,
                                                      width = totalNcolumns,
                                                      underline = TRUE)
        
        class(info) <- c("trowsList", class(info))
        info
    }

    

    ## Given a list of tables (say, 3 variances tables) this puts them
    ## together side by side, with markup.
    ## If one table is NULL, replace with empty DF
    ## for others, expand rows to same size
    cbindParamSet <- function(tablList, colLabels){
        ## xxx table unique rownames and col1 values, to find all possible vars
        xxx <- lapply(tablList, function(x) cbind(rownames = rownames(x),
                                                  col1 = x[ , "col1"]))
        paramnames <- unique(do.call(rbind, xxx))
        rownames(paramnames) <- paramnames[ , "rownames"]
        for(jj in names(tablList)){
            ## if paramSet lacks desired table, make empty table for it
            if (is.null(tablList[[jj]])) {
                y <- data.frame(col1 = paramnames[ , "col1"],
                                matrix("", ncol = length(colLabels[[jj]]),
                                       nrow = NROW(paramnames),
                                       dimnames = list(paramnames[ , "rownames"],
                                                       names(colLabels[[jj]]))))
                y[ , match("col1", colnames(y))] <- NULL
                tablList[[jj]] <- y
            } else {
                y <- tablList[[jj]][rownames(paramnames), ]
                ## y[ , "col1"] <- paramnames[ , "col1"]
                rownames(y) <- rownames(paramnames)
                y[ , match("col1", colnames(y))] <- NULL
                tablList[[jj]] <- y
                if (is.null(attr(tablList, "blocktitle"))){
                    attr(tablList, "blocktitle") <- attr(tablList[[jj]], "blocktitle")
                }
            }
        }
        ## that has a blocktitle attribute
        tablMatrix <- do.call(cbind,  tablList)
        tablMatrix <- cbind("col1" = paramnames[ , "col1"], tablMatrix)
        tablMatrix[is.na(tablMatrix)] <- ""
        attr(tablMatrix, "blocktitle") <- attr(tablList, "blocktitle")
        tablMatrix
    }

    ###########
    ## 20180513
    ## Creates a list of tables, not applying markup yet
    ## trying to postpone markup application. 
    gatherParamSetsList <- function(paramSetNames, colLabels){
        colNameCounts <- lapply(colLabels, length)
        colHeaderRow <- unname(unlist(colLabels))
        totalNcolumns <- min(9,  length(colHeaderRow))## If more than 9, give up at centering

        buildSubtableForParamSet <- function(jj) {
            ## retrieve param jj from each model
            tablList <- lapply(paramList, function(x) x[[jj]])
            ## treat loadings and slopes differently because they are
            ## lists of tables
            if(jj %in% c("composites", "loadings", "slopes")){
                ## get the varnames
                varnames <- unique(unlist(lapply(tablList, names)))
                hh <- list()
                for(i in varnames) {
                    subList <- lapply(tablList, function(x) x[[i]])
                    hh[[i]] <- cbindParamSet(subList, colLabels)
                }
                ##res <- paste0(hh, collapse = " ")
                ## Improvise a section heading for loadings and slopes
                blocktitle <- makeSubtableTitle(paramSetLabels[jj], colnum = 2,
                                           width = totalNcolumns, underline = TRUE)
                ## qqq header <- applyTitleMarkup(blocktitle)
                ## qqq res <- paste0(header, res)
                attr(hh, "blocktitle") <- blocktitle
                return(hh)
            } else {
                hh <- cbindParamSet(tablList, colLabels)
                return(hh)
            }
            stop("semTable: bug in buildSubtableForParamSet. Please report")
        }
        
        results <- lapply(paramSetNames, buildSubtableForParamSet)
        results
    }
    
    ############
    ## take paramList from environment, then iterate through the sections
    ## and impose markup needed.
    finalizeMarkup <- function(paramSetsList, colLabels) {
        colNameCounts <- lapply(colLabels, length)
        colHeaderRow <- unname(unlist(colLabels))
        totalNcolumns <- min(9,  length(colHeaderRow))## If more than 9, give up at centering
          
        results2 <- lapply(paramSetsList, function(x) if(class(x)[1] == "data.frame"){
                                                          markupTable(x)}
                                                      else {
                                                          markupList(x)
                                                          })
        results2 <- paste(results2, collapse = "")
        
        colHeaderRow <- paste0("_BR__EOC_",
                              paste0("_BOC__BOMC1_", paste0(colHeaderRow, collapse = "_EOMC__BOC__BOMC1_")), "_EOMC__EOR__HL_\n")
        
        modelHeaderRow <- paste0("_BR__EOC__BOC_",
                                 paste0("_BOMC", colNameCounts, "_", names(colNameCounts), "_EOMC_", collapse = "_BOC_"),
                                 "_EOR_ _HL_\n", collapse = " ")
        starnote <- if (any(c("eststars", "estsestars") %in% sapply(colLabels, names))) {
                        paste0(starsymbols, "p<", alpha, collapse = ", ")
                    } else ""
        fixnote <-  if(TRUE) paste0("_FIXED_", "Fixed parameter") else ""
        tablesuffix <-  paste0(if(fixnote != "") paste0("_BR__BOML", totalNcolumns + 1, "_", fixnote, "_EOMC__EOR_"),
                               if(starnote != "") paste0("_BR__BOML", totalNcolumns + 1, "_", starnote, "_EOMC__EOR_"))
                               
        resmark <- paste0("_BTABULAR_", modelHeaderRow, colHeaderRow, results2, "_HL_",
                         tablesuffix, "\n_ETABULAR__LB_\n")
        attr(resmark, "colLabels") <- colLabels
        resmark
    }

    ## Now actual work begins.
    ##    
    starsymbols <- c("_STAR1_", "_STAR2_", "_STAR3_")
    if(!is.null(varLabels)) varLabels <- gsub("_", ".", varLabels, fixed = TRUE)
    ## Import and clarify the "object" argument.  If names are
    ## missing, will create.  If object is NOT a list, it is just one
    ## lavaan object, create a list, put that inside a list, so we can
    ## iterate over it.  A generic name for models. If list is not
    ## provided, there's no name, use mname
    mname <- "Model"

    ## Is list, but no names, so make some up
    ## if (is.list(object) && is.null(names(object))){
    ##     names(object) <- paste0(mname, " ", 1:length(object))
    ## }
    
    ## If one model is supplied, create list holder
    if (!is.list(object)) {
        object <- list(object)
        names(object)[1] <- mname
    } else if (is.null(names(object))){
        ## no names, so create names
        names(object) <- paste0(mname, " ", 1:length(object))
    }

    ## apply columnLabels, which may be a partial list of new labels.
    ## columnLabels has a defult, which is used unless
    ## augmented by the columnLabels argument
    columnx <- c(est = "Estimate", se = "Std. Err.", z = "z", p = "p",
                 rsquare = "R Square", estse = "Estimate(Std.Err.)",
                 eststars = "Estimate",
                 estsestars = "Estimate(Std.Err.)")
    if (missing(columnLabels)){
        columnLabels <- columnx
    } else {
        columnLabels <- modifyVector(columnx, columnLabels)
    }
    
    ## paramSetLabels default.
    paramx <- c("loadings" = "Factor Loadings",
                "composites" = "Composites", 
                "slopes" = "Regression Slopes",
                "intercepts" = "Intercepts",
                "means" = "Means",
                "residualvariances" = "Residual Variances",
                "residualcovariances" = "Residual Covariances",
                "variances" = "Variances",
                "latentmeans" = "Latent Intercepts",
                "latentvariances" = "Latent Variances",
                "latentcovariances" = "Latent Covariances", 
                "thresholds" = "Thresholds",
                "constructed" = "Constructed",
                "fits" = "Fit Indices")
    if (missing(paramSetLabels)){
        paramSetLabels <- paramx
    } else {
        paramSetLabels <- modifyVector(paramx, paramSetLabels)
    }

    ## Fill in fitLabels against default "toupper"
    if (missing(fitLabels)){
        fitLabels <- toupper(fits)
        names(fitLabels) <- fits
        if("chisq" %in% fits){
            fitLabels["chisq"] <- "_CHI2_"
        }
    } else {
        fitLabelsX <- toupper(fits)
        names(fitLabelsX) <- fits
        if("chisq" %in% fits){
            fitLabelsX["chisq"] <- "_CHI2_"
        }
        ## blended labels, defaults with user labels on top
        fitLabels <-  modifyVector(fitLabelsX, fitLabels)
    }

    
    if(is.null(fits)) paramSets["fits"] <- NULL
    
    if(!is.null(fits) && !is.list(fits)) {
        names(fits) <- fits
        fits <- toupper(fits)
        fitsX <- modifyVector(fits, fitLabels)
        ## copy fitsX for each object
        fitsLabeled <- lapply(object, function(x) fitsX)
    } else {
        ## fits is a list
        if (!is.null(fits) && length(fits) != length(object) ||
            any(!names(fits) %in% names(object))){
            MESSG <- "object list and fits list must match"
            stop(MESSG)
        }
        fitsLabeled <- lapply(names(object), function(xname){
            fitsone <- fits[[xname]]
            names(fitsone) <- fitsone
            fitsone <- toupper(fitsone)
            fitsX <- modifyVector(fits, fitLabels)
        })
    }
    
    ## Next fixes the colLabels
    ## convert columns to a list, one per object, like
    ## list("Model A" = c(est = "Estimates", se = "Std.Err.")
    if (!is.list(columns)) {
        columns.orig <- columns
        columns <- gsub("est(se)", "estse", columns)
        if (is.list(object)){
            colLabels <- lapply(object, function(x){
                cols <- modifyVector(columns, columnLabels)
            })
        } else {
            colLabels <- list(modifyVector(columns, columnLabels))
            names(colLabels) <- mname
        }
    } else {
        if (length(columns) != length(object) || !all.equal(names(columns), names(object))){
            MESSG <- "object list and columns list must match"
            stop(MESSG)
        }
        columns <- lapply(columns, function(x){
            names(x) <- gsub("est(se)", "estse", names(x))
            x
        })
        colLabels <- lapply(columns, function(x) {
            zz <- columnLabels[x]
            names(zz) <- x
            zz})
    }

    paramList <- list()
    ## if one fitted model and 2 or more groups found in there
    if ((length(object) == 1) && ((G <- (object[[1]])@Data@ngroups) > 1)){
       
        onemodel <- object[[1]] 
        ## has both group and group.label variables from @Data@group.label.
        parTable <- getParamTable(onemodel, paramSets, paramSetLabels)
        groupnumbers <- unique(parTable$group)
        grouplabels <- unique(parTable$group.label)
       
        if (!missing(groups)){
            ## If groups provided, remove ones not in groups parameter
            ## if they give a character vector
            if(is.character(groups)){
                if(any(!groups %in% grouplabels)){
                    MESSG <- "invalid group names"
                    stop(MESSG)
                }
                parTable <- parTable[parTable$group.label %in% groups, ]
            } else if (isTRUE(checkCoercion(groups, "integer"))){
                groups <- as.integer(groups)
                if(any(!groups %in% groupnumbers)){
                    MESSG <- "invalid group numbers"
                    stop(MESSG)
                }
                parTable <- parTable[parTable$group %in% groups, ]  
            } else {
                MESSG <- "invalid group numbers"
                stop(MESSG)
            }
        }
        ## Process remaining groups
        parTableSplit <- split(parTable,
                               f = factor(parTable$group.label,
                                          levels = unique(parTable$group.label)))
        colLabels <- lapply(parTableSplit, function(x) colLabels[[1]])
        if (!is.null(fits)){
            fitsLabeled <- lapply(parTableSplit, function(x) fitsLabeled[[1]])
            }
        for(ii in names(parTableSplit)){
            paramList[[ii]] <- extractParameters(parTableSplit[[ii]],
                                                colLabels = colLabels,
                                                modelName = ii)
            if (!is.null(fits) && ii == names(parTableSplit)[1]){
                paramList[[ii]][["fits"]] <- fitMaker(onemodel, fitsLabeled, colLabels, ii)
            }
        }
        attr(paramList, "G") <- G
    } else {
        ## more models, abort if any are multigroup
        G <- lapply(object, function(x) x@Data@ngroups)
        if((length(object) > 1) && any(G > 1)) {
            MESSG <- "Several Multi Group models are not understandable"
            stop(MESSG)
        }
        ## each model object's parameters are pulled
        ## paramList <- lapply(names(object, extractParameters, columns)
        for(ii in names(object)){
            paramTable <- getParamTable(object[[ii]], paramSets, paramSetLabels)
            paramList[[ii]] <- extractParameters(paramTable, colLabels, modelName = ii)
            if(!is.null(fits)){
                paramList[[ii]][["fits"]] <- fitMaker(object[[ii]], fitsLabeled, colLabels, ii)
            }
        }
    }

    ## This is a marker that will turn TRUE if the table needs FIXED param label
    FIXED <- FALSE
    paramSetsFound <- unique(unlist(lapply(paramList, function(x) names(x))))
    ## re-order paramSetsFound according to standard list
    paramSetNames <- intersect(names(paramSetLabels), paramSetsFound)
   
    paramSetsList <- gatherParamSetsList(paramSetNames, colLabels)
    markedResults <- finalizeMarkup(paramSetsList, colLabels)
  
    result <- markupConvert(markedResults, type = type, longtable = longtable,
                            table.float = table.float, caption = caption, label = label, file = file,
                            columns = colLabels, centering = centering)
    attr(result, "markedResults") <- markedResults
    attr(result, "unmarkedResults") <- paramSetsList
    class(result) <- "kutable"    
    if (print.results) cat(result, "\n") else cat("\n")
    
    invisible(result)
}
NULL



##' Convert marked-up semTable structures to latex, html, or csv
##'
##' The conversion key tables are included in the code of the function.
##'
##' The semTable uses a customized markup framework that uses
##' character sequences that begin and end with underscores, such as
##' "_BOMC2_ for "begin of multi-column entity that will use 2
##' columns". These special markups need to be converted into "tex", "html", or "csv" formats.
##' @param marked A character string
##' @param type Output type, latex", "html", or "csv".
##' @param table.float TRUE if you want insertion of '\\begin{table}'
##' @param longtable should a tabular or a longtable object be
##'     created?
##' @param caption A caption to use if either longtable or table is
##'     TRUE
##' @param label A LaTeX label for cross-references
##' @param file A file stub, to which ".tex", ".html", or ".csv" can
##'     be added
##' @param columns For SEM table, the list of columns objects
##' @param centering Default "siunitx". Specify "none" to return to
##'     behavior of semTable before 1.50.
##' @return a list of marked up character objects
##' @export
##' @author Paul Johnson
markupConvert <- function(marked, type = c("latex", "html", "csv"),
                          table.float = FALSE, longtable = FALSE,
                          caption = NULL, label = NULL, 
                          file = NULL, columns, centering = "siunitx")
{
    if(missing(columns)) columns <- attr(marked, "colLabels")
    ##num of columns, except for col1
    Ncolumns <- length(unname(unlist(columns)))

    ## a tabular
    tabularmarkup <-   paste0("{@{}r",
                         if(centering == "siunitx")paste0("*{", Ncolumns, "}{S[
                         input-symbols = ( ) +,
                         group-digits = false,
                         table-number-alignment = center,
                         %table-space-text-pre = (,
                         table-align-text-pre = false,
                         table-align-text-post = false,
                         table-space-text-post = {***},
                         parse-units = false]}") else paste0(rep("c", Ncolumns), collapse = ""), "@{}}\n")
    ## longtable
    if(!longtable && !table.float){
        tcode <- paste0("\\\\begin{tabular}", tabularmarkup)
    } else {
        if(longtable){
            tcode <-  paste0("\\\\begin{longtable}", tabularmarkup)
            if (!is.null(caption)) tcode <- paste0(tcode, "\n\\\\caption{", caption, "}")
            if (!is.null(label)) tcode <- paste0(tcode, "\n\\\\label{", label, "}")
            tcode <- paste0(tcode, "\n\\\\endfirsthead\n\\\\endhead\n")
        } else if (table.float){
            tcode <- "\\\\begin{table}"
            if (!is.null(caption)) tcode <- paste0(tcode, "\n\\\\caption{", caption, "}")
            if (!is.null(label)) tcode <- paste0(tcode, "\n\\\\label{", label, "}")
            tcode <- paste0(tcode, "\\\\begin{tabular}")
            tcode <- paste0(tcode, tabularmarkup)
        } else {
            MESSG <- "Logic error in markupConvert"
            stop(MESSG)
        }
    } 
    ## Replacement strings for LaTeX output
    latexreplace <- c(
        "_LB_" = "\\\n",
        "_EOC_" =  "",
        "_BOC_" = "& ", 
        "_EOMC_" = "}",
        "_EOR_" = "\\\\tabularnewline",
        "_BRU_" = "",
        "_BRT_" = "", 
        "_BOCU_" = "& ",
        "_BR_" = "",
        "_BTABULAR_" = tcode,  
        "_EOL_" = "\n",
        "_HL_" = "\\\\hline", 
        "_UL_" = "\\\\underline{",
        "_EOUL_" = "}",
        "_SEPU_" = " &", 
        "_SEP_" = " &", 
        "_ETABULAR_" = if (longtable) {
                      "\\\\end{longtable}"
                  } else if (table.float) {
                      "\\\\end{tabular}
                       \\\\end{table}"
                  } else {
                      "\\\\end{tabular}"
                  },
        "_BOMR1_" = "& \\\\multirow{1}{c}{",
        "_BOMR2_" = "& \\\\multirow{2}{c}{",
        "_BOMC1_" = "\\\\multicolumn{1}{c}{",
        "_BOMC2_" = "\\\\multicolumn{2}{c}{",
        "_BOMC3_" = "\\\\multicolumn{3}{c}{",
        "_BOMC4_" = "\\\\multicolumn{4}{c}{",
        "_BOMC5_" = "\\\\multicolumn{5}{c}{",
        "_BOMC6_" = "\\\\multicolumn{6}{c}{",
        "_BOMC7_" = "\\\\multicolumn{7}{c}{",
        "_BOMC8_" = "\\\\multicolumn{8}{c}{",
        "_BOMC9_" = "\\\\multicolumn{9}{c}{",
        "_BOML1_" = "\\\\multicolumn{1}{l}{",
        "_BOML2_" = "\\\\multicolumn{2}{l}{",
        "_BOML3_" = "\\\\multicolumn{3}{l}{",
        "_BOML4_" = "\\\\multicolumn{4}{l}{",
        "_BOML5_" = "\\\\multicolumn{5}{l}{",
        "_BOML6_" = "\\\\multicolumn{6}{l}{",
        "_BOML7_" = "\\\\multicolumn{7}{l}{",
        "_BOML8_" = "\\\\multicolumn{8}{l}{",
        "_BOML9_" = "\\\\multicolumn{9}{l}{",
        "_BOMCT1_" = "\\\\multicolumn{1}{c}{",
        "_BOMCT2_" = "\\\\multicolumn{2}{c}{",
        "_BOMCT3_" = "\\\\multicolumn{3}{c}{",
        "_BOMCT4_" = "\\\\multicolumn{4}{c}{",
        "_HTMLHL_" = "",
        "_CHI2_" = "$\\\\chi^{2}(\\\\mathrm{df})$",
        "_R2_" = "$R^2$",
        "_LEQ_" = "$\\\\leq$",
        "_SIGMA_" = "$\\\\sigma$",
        "_NBSP_" = " ",
        "_FIXED_" = "$^+$",
        "_STAR0_" = "$\\\\phantom{{^{***}}}$",
        "_STAR1_" = "$^{*}\\\\phantom{{^{**}}}$",
        "_STAR2_" = "$^{**}\\\\phantom{{^{*}}}$",
        "_STAR3_" = "$^{***}$"
    )

    ## Replacement strings for HTML output
    ## TODO: 20171102: refactor abbreviations
    ## Problem in output is duplicate <td><td ..>, workaround in last item"
    htmlreplace <- c(
        "_LB_" = "<br>",
        "_EOC_" = "</td>",
        "_BOC_" = "<td>",
        "_EOMC_" = "</td>",
        "_EOR_" = "</tr>",
        "_BRU_" = paste("<tr><td style=\"border-bottom: solid thin black; border-collapse:collapse;\">&nbsp;"),
        "_BRT_" = paste("<tr><td style=\"border-top: solid thin black; border-collapse:collapse;\">&nbsp;"),
        "_BOCU_" = paste("<td style=\"border-bottom: solid thin black; border-collapse:collapse;\">&nbsp;"),
        "_BR_" = "<tr><td>",
        "_BTABULAR_" =  "<table style=\"padding-right:20px;padding-left:20px;\">\n",
        "_EOL_" = "\n",
        "_HL_" =  "",
        "_UL_" =  "<span style=\"text-decoration: underline;\">",
        "_EOUL_" = "</span>",
        "_SEPU_" = "</td><td style=\"border-bottom: solid thin black; border-collapse:collapse;\">&nbsp;",
        "_SEP_" = "</td><td>",
        "_ETABULAR_" = "</table>",
        "_BOMR1_" = "<td rowspan = '1'>",
        "_BOMR2_" = "<td rowspan = '2'>",
        "_BOMC1_" = "<td colspan = '1'; align = 'center'>",
        "_BOMC2_" = "<td colspan = '2'; align = 'center'>",
        "_BOMC3_" = "<td colspan = '3'; align = 'center'>",
        "_BOMC4_" = "<td colspan = '4'; align = 'center'>",
        "_BOMC5_" = "<td colspan = '5'; align = 'center'>",
        "_BOMC6_" = "<td colspan = '6'; align = 'center'>",
        "_BOMC7_" = "<td colspan = '7'; align = 'center'>",
        "_BOMC8_" = "<td colspan = '8'; align = 'center'>",
        "_BOMC9_" = "<td colspan = '9'; align = 'center'>",
        "_BOML1_" = "<td colspan = '1'; align = 'left'>",
        "_BOML2_" = "<td colspan = '2'; align = 'left'>",
        "_BOML3_" = "<td colspan = '3'; align = 'left'>",
        "_BOML4_" = "<td colspan = '4'; align = 'left'>",
        "_BOML5_" = "<td colspan = '5'; align = 'left'>",
        "_BOML6_" = "<td colspan = '6'; align = 'left'>",
        "_BOML7_" = "<td colspan = '7'; align = 'left'>",
        "_BOML8_" = "<td colspan = '8'; align = 'left'>",
        "_BOML9_" = "<td colspan = '9'; align = 'left'>",
        "_BOMCT1_" = "<td colspan = '1'; style=\"border-top: solid thin black; border-collapse:collapse;\">&nbsp;",
        "_BOMCT2_" = "<td colspan = '2'; style=\"border-top: solid thin black; border-collapse:collapse;\">&nbsp;",
        "_BOMCT3_" = "<td colspan = '3'; style=\"border-top: solid thin black; border-collapse:collapse;\">&nbsp;",
        "_BOMCT4_" = "<td colspan = '4'; align = 'center'; ; style=\"border-top: solid thin black; border-collapse:collapse;\">&nbsp;",
        "_HTMLHL_" = "<tr><td colspan = '5'; align = 'center'; ; style=\"border-top: solid thin black; border-collapse:collapse;\">&nbsp;</tr>",
        "_CHI2_" = "&chi;<sup>2</sup>",
        "_R2_" = "R<sup>2</sup>",
        "_LEQ_" = "&le;",
        "_SIGMA_" = "&sigma;",
        "_NBSP_" = "&nbsp;",
        "_FIXED_" = "<sup>+</sup>",
        "_STAR0_" = "&nbsp;",
        "_STAR1_" = "<sup>*</sup>",
        "_STAR2_" = "<sup>**</sup>",
        "_STAR3_" = "<sup>***</sup>",
        "<td><td" = "<td"
    )
    
    ## Replacement strings for CSV output
    csvreplace <- c(
        "_LB_" = "\n",
        "_EOC_" =  ",",
        "_BOC_" = "", 
        "_EOMC_" = ",",
        "_EOR_" = "\n",
        "_BRU_" = "",
        "_BRT_" = "", 
        "_BOCU_" = ",",
        "_BR_" = "",
        "_BTABULAR_" = "",
        "_EOL_" = "\n",
        "_HL_" = "", 
        "_UL_" = "",
        "_EOUL_" = "",
        "_SEPU_" = "", 
        "_SEP_" = ",", 
        "_ETABULAR_" = "",
        "_BOMR1_" = "",
        "_BOMR2_" = "",
        "_BOMC1_" = "",
        "_BOMC2_" = "",
        "_BOMC3_" = "",
        "_BOMC4_" = "",
        "_BOMC5_" = "",
        "_BOMC6_" = "",
        "_BOMC7_" = "",
        "_BOMC8_" = "",
        "_BOMC9_" = "",
        "_BOML1_" = "",
        "_BOML2_" = "",
        "_BOML3_" = "",
        "_BOML4_" = "",
        "_BOML5_" = "",
        "_BOML6_" = "",
        "_BOML7_" = "",
        "_BOML8_" = "",
        "_BOML9_" = "",
        "_BOMCT1_" = "",
        "_BOMCT2_" = "",
        "_BOMCT3_" = "",
        "_BOMCT4_" = "",
        "_HTMLHL_" = "",
        "_CHI2_" = "chi^2",
        "_R2_" = "R^2",
        "_LEQ_" = "<=",
        "_SIGMA_" = "sigma",
        "_NBSP_" = " ",
        "_FIXED_" = "+",
        "_STAR0_" = "", 
        "_STAR1_" = "*",
        "_STAR2_" = "**",
        "_STAR3_" = "**"
    )

    if (tolower(type) %in% c("latex", "tex")) {
        result <- mgsub(names(latexreplace), latexreplace, marked)
        if (!is.null(file)){
            if (!isTRUE(grepl(".tex$", file))) file <- paste0(file, ".tex")
            cat(result, file = file)
        }
    } else if (tolower(type) %in% c("html")){
        result <- mgsub(names(htmlreplace), htmlreplace, marked)
        result <- gsub("\\{\\(", "(", result)
        result <- gsub("\\)\\}", ")", result)
        if (!is.null(file)){
            if (!isTRUE(grepl(".html$", file))) file <- paste0(file, ".html")
            cat(result, file = file)
        }
    } else if (!is.na(match("csv", type))) {
        result <- mgsub(names(csvreplace), csvreplace, marked)
        if (!is.null(file)){
            if (!isTRUE(grepl(".csv$", file))) file <- paste0(file, ".csv")
            cat(result, file = file)
        }
    } else {
        MESSG <- "convertMarkup type argument incorrect"
        stop(MESSG)
    }
    attr(result, "file") <- file
    invisible(result)
}
NULL

##' Test viewer for tex tables
##'
##' Creates a small latex template file that includes a table
##' file. Compiles it, then displays in viewer if system has
##' \code{xdg-open} settings.
##' 
##' @param tablefile The base name of the table file
##' @param dir Directory where table is saved, same will be used for build.
##' @param tmpfn File name to be used by example document
##' @return LaTeX log, returned from shell function.
##' @export
##' @author Paul Johnson <pauljohn@@ku.edu>
##' @examples
##' \donttest{
##' require(lavaan)
##' tempdir <- tempdir()
##' HS.model <- ' visual  =~ x1 + x2 + x3
##'               textual =~ x4 + x5 + x6
##'               speed   =~ x7 + x8 + x9'
##' fit1 <- cfa(HS.model, data = HolzingerSwineford1939,
##'             std.lv = TRUE, meanstructure = TRUE)
##' fit1.t <- semTable(fit1, fits = c("chisq", "rmsea"),
##'                columns = c("estsestars", "rsquare"),
##'                columnLabels = c("estsestars" = "Est(SE)"),
##'                file = file.path(tempdir, "fit1.t"))
##' if (interactive()) testtable("fit1.t", tempdir)
##' }
testtable <- function(tablefile, dir, tmpfn = "tmp.tex"){
    wd.orig <- getwd()
    isWindoze <- if(Sys.info()[['sysname']] == "Windows") TRUE else FALSE
    type <- if(isWindoze) "cmd" else "sh"
    mynull <-  if(isWindoze) "> nul" else " > /dev/null"

    ## If user forgets to remove .tex on end of filename, do it
    ## for them
    tablefile <- gsub("\\.tex$", "", tablefile)
    
    x1 <- 
"\\documentclass[english]{article}
\\usepackage[T1]{fontenc}
\\usepackage[utf8]{inputenc}
\\usepackage{geometry}
\\geometry{verbose,tmargin=1in,bmargin=1in,lmargin=1in,rmargin=1in}
\\usepackage{babel}
\\usepackage{longtable}
\\usepackage[normalem]{ulem}
\\usepackage{siunitx}
\\begin{document}
"
    x2 <- paste0("\\include{", tablefile, "}\n")

    x3 <- "\\end{document}\n"

    cat(x1, x2, x3, file = file.path(dir, tmpfn))
    cmd <- paste("pdflatex -interaction=batchmode ",
                 shQuote(tmpfn, type),
                 " && pdflatex -interaction=batchmode ",
                 shQuote(tmpfn, type))
    wd.orig <- getwd()
    setwd(dir)
    on.exit(setwd(wd.orig))
    resltfn <- file.path(dir, gsub(".tex", ".pdf", tmpfn))
    if (isWindoze){
        out1 <- tryCatch(shell(cmd, intern = TRUE))
        if(file.exists(resltfn)){
            MESSG <- paste("Please open this file:", resltfn)
            print(MESSG)
        } else {
            MESSG <- paste("pdflatex failed to compile a test document including", tablefile)
            stop(MESSG)
        }
    } else {
        out1 <- tryCatch(system(cmd, intern = TRUE))
        if(file.exists(resltfn)){
            cmd2 <- paste("xdg-open", resltfn)
            out2 <- tryCatch(system(cmd2, intern = TRUE))
        } else {
            MESSG <- paste("pdflatex failed to compile a test document including", tablefile)
            stop(MESSG)
        }
    }
  
    invisible(resltfn)
}
