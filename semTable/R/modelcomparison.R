##' Discern nesting pattern of SEM coefficients
##'
##' Receives a list of models and orders them by best guess at intended nesting
##' @param models A List of lavaan-fitted SEM models
##' @export
##' @return matrix indicating nesting relationships
##' @author Ben Kite <bakite@@ku.edu>
detectNested <- function(models){
    dfval <- ifelse("df.scaled" %in% lavaan::fitMeasures(models[[1]]), "df.scaled", "df")
    dfs <- lapply(models, lavaan::fitMeasures, dfval)
    dfsrank <- rank(unlist(dfs))
    names(dfsrank) <- names(models)
    sortedmods <- sort(dfsrank)
    nested <- matrix(NA, length(sortedmods), length(sortedmods))
    diag(nested) <- 0
    colnames(nested) <- names(sortedmods)
    rownames(nested) <- names(sortedmods)
    ## Columns represent the possible larger model, rows inform about
    ## whether or not the model is nested in the column
    for (i in rownames(nested)){
        for (j in colnames(nested)){
            if(i != j){
                dfnest <- dfsrank[i] > dfsrank[j]
                iparams <- data.frame(models[[i]]@ParTable)[,c("lhs", "op", "rhs")]
                jparams <- data.frame(models[[j]]@ParTable)[,c("lhs", "op", "rhs")]
                iparams <- apply(iparams, 1, paste, collapse = "")
                jparams <- apply(jparams, 1, paste, collapse = "")
                iparams <- iparams[!iparams %in% iparams[grep("==", iparams)]]
                jparams <- jparams[!jparams %in% jparams[grep("==", jparams)]]
                parnest <- prod(jparams %in% iparams)
                nested[i,j] <- ifelse(dfnest*parnest == 1, 1, 0)
            }
        }
    }
    pairs <- matrix(NA, nrow(nested), 2)
    rownames(pairs) <- rownames(nested)
    colnames(pairs) <- c("nested", "parent")
    ## Make nested pairs
    for(i in rownames(nested)){
        pairs[i,"nested"] <- i
        nestedin <- which(nested[i,] == 1)
        if (length(nestedin) > 0){
            pairs[i,"parent"] <- names(nestedin[max(nestedin)])
        }
    }
    pairs
}

##' Prepare a table to compare fit measures of confirmatory factor analyses (CFA)
##'
##' If the parameter \code{nesting} is not specified, then this
##' function attempts to discern which models are nested and they are
##' ordered accordingly. The user can override that by specifing a
##' nesting structure. This uses a new notation to represent nesting
##' of models.  See the parameter \code{nesting}.  The syntax uses the
##' symbols ">" and "+" in an obvious way to indicate that one model
##' is the superset or on the same level as another. If the
##' 
##' In May 2018, the output approach was changed. The functions
##' \code{xtable} and \code{print.xtable} are used to render the final
##' result and any of the arguments allowed by \code{print.xtable} can
##' be used here (via the \code{...} argument). We have some default
##' settings for some \code{print.xtable}, such as \code{type = NULL},
##' \code{file = NULL}, \code{print.results = TRUE}, and
##' \code{math.style.exponents = TRUE}. There are some other specific
##' defaults for type = "latex" and type = "html", but they can all be
##' overridden by the user. We include a model legend at the bottom of
##' the table, indicating which models are compared by the Chi-squared
##' statistic.
##' 
##' If the type argument is not specified, then the output will be a
##' simple text display of the model table.  If type is either "latex"
##' or "html", then a marked up table will be displayed and the file
##' argument can be used to ask for a saved version. If the user wants
##' to simply save the result in a file, and not display on screen,
##' insert the argument \code{print.results = FALSE}.
##' 
##' @param models list of lavaan cfa or sem models. Model names can be
##'     supplied. See examples.
##' @param fitmeas A vector of fit measures. One or more of these
##'     \code{c("chisq", "df", "pvalue", "rmsea", "cfi", "tli",
##'     "srmr", "aic", "bic", "srmr", "aic", "bic",
##'     "srmr_mplus")}. Other fit measures present in the lavaan
##'     objects will be allowed; fit measures that are requested but
##'     not found are ignored.
##' @param nesting character string indicating the nesting structure
##'     of the models.  Must only contain model names, ">", and "+"
##'     separated by spaces.  The model to the left of a ">" is the
##'     parent model for all models to the right of the same ">", up
##'     until another ">" is reached. When multiple models are nested
##'     in the same parent, they are separated by a "+".
##' @param scaled should scaled versions of the fit measures requested
##'     be used if available?  The scaled statistic is determined by
##'     the model estimation method.  The defaul value is TRUE.
##' @param chidif should the nested models be compared by using the
##'     anova function? The anova function may pass the model
##'     comparison on to another lavaan function.  The results are
##'     added to the last three columns of the comparison table. The
##'     default value is TRUE.
##' @param digits The digits argument that will be passed to xtable.
##' @param ... Arguments that will be passed to print.xtable. These
##'     arguments can be used to control table caption, label, and so
##'     forth. See \code{?print.xtable}.  If \code{type = "latex"} or
##'     \code{"html"}, this function sets additional default values
##'     for print.xtable that can be overridden by specifying
##'     arguments here. Default type is an R data.frame, which is printed on
##'     screen. Note the print.xtable parameter \code{print.results} determines
##'     whether the markup is displayed before it is returned. The \code{file}
##'     parameter can specify a file into which the markup is to be saved.
##' @author Ben Kite <bakite@@ku.edu> and Paul Johnson
##'     <pauljohn@@ku.edu>
##' @export
##' @importFrom stats anova update
##' @importFrom kutils mgsub
##' @importFrom utils modifyList
##' @importFrom xtable xtable print.xtable
##' @return If type = NULL, a data.frame object includes an attribute
##'     called "noteinfo". If type = "tex", return is a character
##'     vector created by xtable. If type = "html", a vector of HTML
##'     markup created by xtable.
##' @examples
##' library(lavaan)
##' library(xtable)
##' set.seed(123)
##' genmodel <- "f1 =~ .7*v1 + .7*v2 + .7*v3 + .7*v4 + .7*v5 + .7*v6
##' f1 ~~ 1*f1"
##' genmodel2 <- "f1 =~ .7*v1 + .7*v2 + .7*v3 + .7*v4 + .7*v5 + .2*v6
##' f1 ~~ 1*f1"
##' ##'
##' dat1 <- simulateData(genmodel, sample.nobs = 300)
##' dat2 <- simulateData(genmodel2, sample.nobs = 300)
##' dat1$group <- 0
##' dat2$group <- 1
##' dat <- rbind(dat1, dat2)
##' ## In order from less constrained to restricted
##' ## Model cc1 configural model
##' cc1.model <- "
##'               f1 =~ 1*v1 + v2 + v3 + v4 + v5 + v6
##'     		  f1 ~~ f1
##'     		  f1 ~0*1
##'     		 "
##' ## Model2: cc2 partial weak model (AKA partial metric)
##' cc2.model <- "
##'               f1 =~ 1*v1 + c(L2,L2)*v2 + c(L3,L3)*v3 + c(L4,L4)*v4 + c(L5,L5)*v5 + v6
##'     		  f1 ~~ f1
##'     		  f1 ~0*1
##'     		"
##' ## Model 3: weak model (AKA metric)
##' cc3.model <- "
##'             f1 =~ 1*v1 + c(L2,L2)*v2 + c(L3,L3)*v3 + c(L4,L4)*v4 + c(L5,L5)*v5 + c(L6,L6)*v6
##'     		f1 ~~ f1
##'     		 f1 ~0*1
##'     		"
##' ## Model 4: scalar model (AKA strong)
##' cc4.model <- "
##'               f1 =~ 1*v1 + c(L2,L2)*v2 + c(L3,L3)*v3 + c(L4,L4)*v4 + c(L5,L5)*v5 + v6
##'     		  f1 ~~ f1
##'     		  f1 ~ c(0,NA)*1
##'     		  v1 ~ c(I1,I1)*1
##'     		  v2 ~ c(I2,I2)*1
##'     		  v3 ~ c(I3,I3)*1
##'     		  v4 ~ c(I4,I4)*1
##'     		  v5 ~ c(I5,I5)*1
##'     		  v6 ~ c(I6,I6)*1
##'     		"
##'
##' ## Reload saved models if available: avoids slow re-estimation that bothers CRAN
##' cc1 <-  tryCatch(readRDS(system.file("cfa/cc1.rds", package = "semTable")),
##'                  error = function(e) cfa(cc1.model, data=dat, group="group",
##'                                          meanstructure=TRUE, estimator = "MLR"))
##' cc2 <- tryCatch(readRDS(system.file("cfa/cc2.rds", package = "semTable")),
##'                 error = function(e) cfa(cc2.model, data=dat, group="group",
##'                                         meanstructure=TRUE, estimator = "MLR"))
##' cc3 <- tryCatch(readRDS(system.file("cfa/cc3.rds", package = "semTable")),
##'                  error = function(e) cfa(cc3.model, data=dat, group="group",
##'                                          meanstructure=TRUE, estimator = "MLR"))
##' cc4 <- tryCatch(readRDS(system.file("cfa/cc4.rds", package = "semTable")),
##'                 error = function(e) cfa(cc4.model, data=dat, group="group",
##'                                         meanstructure=TRUE, estimator = "MLR"))
##' 
##' models <- list(cc1, cc2, cc3, cc4)
##' ## Note, nesting is not specified, so built-in nesting detection applies
##' compareLavaan(models)
##' compareLavaan(models, type = "latex")
##' compareLavaan(models, type = "html")
##' ##'
##' ## Now we specify model labels in the list
##' models <- list("Configural" = cc1,  "PartialMetric" = cc2, "Metric" = cc3, "Scalar" = cc4)
##' ## The model labels are used in the nesting parameter
##' compareLavaan(models, nesting = "Configural > PartialMetric > Metric > Scalar")
##' ##' Previous incorrect, treat cc2 and cc3 as children of cc1 instead:
##' compareLavaan(models, nesting = "Configural > PartialMetric + Metric > Scalar")
##' ##' 
##' compareLavaan(models, fitmeas = c("chisq", "df", "cfi", "rmsea", "tli"),
##'               nesting = "Configural > Metric + PartialMetric > Scalar")
##' 
##' compareLavaan(models, fitmeas = c("chisq", "df", "cfi", "rmsea", "tli"),
##'              nesting = "Configural > PartialMetric + Metric > Scalar")
##' ##'
##' ## Creates output file
##' ## compareLavaan(models, fitmeas = c("chisq", "df", "cfi", "rmsea", "tli"),
##' ## nesting = "Configural > Metric + PartialMetric > Scalar", type = "tex",
##' ## file = "/tmp/table.tex")
compareLavaan <- function(models,
                       fitmeas = c("chisq", "df",  "pvalue", "rmsea", "cfi", "tli", "srmr", "aic", "bic"),
                       nesting = NULL, scaled = TRUE, chidif = TRUE, digits = 3,  ...)
{
    dots <- list(...)
    if (!is.null(dots$type)){
        type <- dots$type
        if(type %in% c("latex", "tex")) type <- "latex"
        dots$type <- NULL
    } else {
        type <- NULL
    }
    print.results <- if (!is.null(dots$print.results)) dots$print.results else TRUE
    
    if (is.null(names(models))){
        names(models) <- paste0("Model", seq(1, length(models)))
    }
    estimators <- lapply(models, function(x) x@Options$estimator)
    if (length(unlist(unique(estimators))) > 1L){
        stop(paste("The models provided do not have the same estimator.",
                   "This function cannot handle models with different estimators."))
    }
    if(is.null(nesting)){
        nestedPairs <- detectNested(models)
    }else{
        xx <- unlist(strsplit(nesting, "\\s+"))
        ops <- c("+", ">")
        mods <- names(models)
        if (any(!xx %in% c(ops, mods))) {
            MESSG <- paste("nesting parameter has unexpected symbols:",
                          paste(xx[!xx %in% c(ops, mods)], collapse = " "))
            stop(MESSG)
        }
        yy <- grep(">", xx)
        parents <- xx[c(yy-1)]
        nested <- list()
        for (i in parents){
            nested[[i]] <-  strsplit(unlist(strsplit(paste0(xx[which(xx == i):length(xx)], collapse = ""), ">"))[2], "\\+")
        }
        pairs <- cbind("nested" = names(nested[1]), "parent" = NA)
        for (i in 1:length(nested)){
            pairs <- rbind(pairs, cbind("nested" = nested[[i]][[1]], "parent" = names(nested)[i]))
        }
        nestedPairs <- pairs
    }
    fitmeas_f <- if(scaled) c(fitmeas, paste0(fitmeas, ".scaled")) else  fitmeas
    observed <- names(lavaan::fitMeasures(models[[1]], fitmeas_f[fitmeas_f %in% names(lavaan::fitMeasures(models[[1]]))]))
    for (i in fitmeas){
        if(paste0(i, ".scaled") %in% observed){
            observed[which(observed == i)] <- paste0(i, ".scaled")
        }
    }
    observed <- observed[!duplicated(observed)]
    orderedMods <- list()
    for (i in nestedPairs[,"nested"]){
        orderedMods[[i]] <- models[[i]]
    }
    modelsums <- lapply(orderedMods, function(x) lavaan::fitMeasures(x, observed))
    sumtable <- do.call(rbind, modelsums)
    sumtable <- apply(sumtable, 2, round, 3)
    sumtable <- data.frame(sumtable)
    if(chidif){
        sumtable[,c("dchi", "ddf", "npval")] <- "-"
        letter <- 1
        noteinfo <- list()
        for (i in rownames(sumtable)){
            comparison <- nestedPairs[which(nestedPairs[,"nested"] == i),]
            if (!is.na(comparison["parent"])){
                tmp <- round(lavaan::anova(models[[comparison["nested"]]],
                       models[[comparison["parent"]]])[2, c("Chisq diff", "Df diff", "Pr(>Chisq)")], 3)
                sumtable[i, c("dchi", "ddf", "npval")] <- tmp
                sumtable[i, "dchi"] <- paste0(sumtable[i, "dchi"], letters[letter])
                noteinfo[[letter]] <- paste0(letters[letter], " = ", comparison["nested"], " vs ", comparison["parent"])
                letter <- letter + 1
            }
        }
        dframe <- sumtable
        attr(dframe, "noteinfo") <- unlist(noteinfo)
    } else {
        dframe <- sumtable
        noteinfo <- NULL
    }

    sanitize.colnames <- function(z){
        name_old <- c("^chisq$", "^pvalue$", "^dchi$", "^ddf$", "^npval$")
        name_new <- c("$\\\\chi^{2}$", "\\\\textit{p}-value",
                      "$\\\\Delta\\\\chi^{2}$", "$\\\\Delta df$", "\\\\textit{p}")
        texcode <- mgsub(name_old, name_new, z)
    }

    if(!is.null(type)){
        tableinfo <- dframe
        names(tableinfo) <- gsub(".scaled", "", names(tableinfo))
        xtab1 <- xtable(tableinfo)
        xtab1.legend <- if(!is.null(noteinfo)) noteinfo else NULL
        if(tolower(type) %in% c("tex", "latex")){
            print.xtable.argz <- list(type = NULL,
                                      file = NULL,
                                      print.results = TRUE,
                                      sanitize.colnames.function = sanitize.colnames,
                                      sanitize.rownames.function = escape,
                                      math.style.exponents = TRUE)
            if(!is.null(xtab1.legend)){
                print.xtable.argz[["add.to.row"]] =
                    list(list(NROW(xtab1)),
                         paste0("\\hline\n", paste0("\\multicolumn{4}{l}{", xtab1.legend,
                                                    "} \\\\\n", collapse="")))
            }
                                  
            print.xtable.args <- modifyList(print.xtable.argz, dots)
            print.xtable.args <- modifyList(list(x = xtab1), print.xtable.args)
            
            texcode <- do.call(print.xtable, print.xtable.args)

            if(!is.null(print.xtable.args$file)) {
                cat(texcode, file = print.xtable.args$file)
            }
            class(texcode) <- "kutable"
            return(invisible(texcode))
        } else {
            print.xtable.argz <- list(type = "html",
                                      file = NULL,
                                      print.results = TRUE,
                                      math.style.exponents = TRUE,
                                      html.table.attributes = list(border="0"))
            if(!is.null(xtab1.legend)){
                print.xtable.argz[["add.to.row"]] =
                    list(list(NROW(xtab1)), paste0("\n",
                       paste0("<tr><td colspan=\"4\">", xtab1.legend,
                              "</td></tr>\n", collapse="")))
            }
            print.xtable.args <- modifyList(print.xtable.argz, dots)
            print.xtable.args <- modifyList(list(x = xtab1), print.xtable.args)
            htmlcode <- do.call("print.xtable", print.xtable.args)
            if(!is.null(print.xtable.args$file)) {
                cat(htmlcode, file = print.xtable.args$file)
            }
            class(htmlcode) <- "kutable"
            return(invisible(htmlcode))
        }
    }
    if(print.results){
        print(dframe)
        cat(attr(dframe, "noteinfo"), fill = TRUE)
    }
    invisible(dframe)
}


##' A print method for kutable objects
##' 
##' @method print kutable
##' @export
##' @param x   object to be printed
##' @param ... optional arguments, corrently ignored
##' @return x unchanged
print.kutable <- function(x, ...){
    if(is.character(x)) cat(x)
    else print(x)
    x
}

