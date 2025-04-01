addScales <- function(obj,...)UseMethod("addScales")

addScales.trellis <-
    function(obj, scaleline = list(h = TRUE, v = FALSE),
             legend = list(h = TRUE, v = TRUE),
             ndig.legend = c(h = 2L, v = 2L),
             legend.aes = list(), ## list of parameters to be fed to gpar()
             legend.loc = c("top","bottom","right","left"),
             panelFUN = panel.addScales,
             ...)
        tryCatch(
            {
                if(inherits(obj, "scaledTrellis")){
                    stop("Use update() to update a scaledTrellis object")
                }
                ## Setup ####
                hv <- c("h","v")
                olegend <- obj$legend
                legendval <- legend
                lims <- list(h = obj$y.limits, v = obj$x.limits)
                axisType <- sapply(lims,function(x)
                    if(is.numeric(x[[1]]))"numeric" else "nonnum")
                lloc<- match.arg(legend.loc)
                msg.nonnum <- "Scalelines can't be drawn for non-numeric axes"

                ## Can scalelines be drawn at all?
                if(!any(axisType == "numeric"))
                    stop(msg.nonnum)
                #####  check/fixup ndig.legend argument  ####
                stopifnot(
                    0 < length(ndig.legend),
                    3 > length(ndig.legend),
                    all(is.numeric(ndig.legend)),
                    all(is.finite(ndig.legend))
                )
                ndig.legend <- round(ndig.legend,0)
                ndig.legend[ndig.legend < 1 | ndig.legend >15] <- 2L
                if(length(ndig.legend) == 1) ndig.legend <- rep(ndig.legend,2)
                names(ndig.legend) <- hv

                ## parse/check/fixup scaleline
                scaleline <- parseInputs(scaleline)
                for(w in hv) if(axisType[w] != "numeric") scaleline[[w]] <- FALSE
                if(all(sapply(scaleline,isFALSE)))stop("Nothing to draw")
                if(!isTRUE(all(sapply(scaleline, function(x)length(x) == 1 &&
                                      (is.logical(x) || (is.numeric(x) && is.finite(x)))))))
                    stop(msg.nonnum)
                ## fill in scaleline list for TRUE entries or
                ## give warning for numeric values that are too big
                ## Create and fill in allMids list for possible midline
                ## color coding to pass on to panelFun
                for(x in hv){
                    sl <- scaleline[[x]]
                    if(!isFALSE(sl)){
                        lt <- lims[[x]]
                        if(is.list(lt)){
                            minrange <- min(sapply(lt,diff))
                        } else  {
                            minrange <- diff(lt)
                        }
                        if(isTRUE(sl)) {
                            scaleline[[x]] <- .45*minrange
                        }
                        if(2*scaleline[[x]] > minrange){
                            warning(paste("Scaleline value for",x,"is too large",
                                          "for lines to fit in all panels.",
                                          sep = " " ))
                        }
                    }
                }

                ## ************** parse/check/fix up legend ******
                legend <- parseInputs(legend)
                ## compute legends only if needed:
                for(w in hv)if(isFALSE(scaleline[[w]])) legend[[w]] <- FALSE
                if(!all(sapply(legend,isFALSE))){
                    ## first a corner case
                    if(any(sapply(legend, function(x) !is.language(x) &&
                                  (length(x) >1 ||is.na(x)))))
                        stop("Non-language legend components must have length 1",
                             "and not be NA")
                    stopifnot(sapply(legend, function(x) is.language(x) ||
                                         (length(x)==1 && is.logical(x)) ||
                                         is.character(x)))
                    if(lloc %in%  names(olegend))
                        stop(sprintf("'%s' component already used for existing plot legend.",lloc))
                }
                ## function to make textGrobs from addScales arguments
                makeLegendGrob <- function(legend, wh)
                {
                    sl <- signif(scaleline[[wh]],
                                 digits =  ndig.legend[wh])
                    if(is.character(legend)){lbl <- legend}
                    else if(is.language(legend)){
                        lbl <- eval(eval(substitute(legend, list(sl = sl))))}
                    else lbl <- paste(wh, "= \U00B1",sl)  ## default: unicode ±
                    textGrob(lbl, gp = do.call(gpar, legend.aes))
                }
                #### change legend components to Grobs as necessary #####
                for(x in hv){
                    if(isFALSE(scaleline[[x]]) || isFALSE(legend[[x]]))
                    {legend[[x]] <- FALSE}
                    else legend[[x]] <-
                            tryCatch(makeLegendGrob(legend[[x]], x),
                                     error = function(e)
                                         stop("\nerror: Unable to convert legend argument to grob\n",
                                              "message:" ,conditionMessage(e))
                            )
                }
                ########### modify the object panel function  ############
                arglist <- c(list(scaleline = sapply(scaleline, as.numeric),
                             all.panel.limits = lims),
                             list(...))
                opanel <- obj$panel
                obj$panel = function(...){
                    match.fun(opanel)(...)
                    do.call(panelFUN,arglist)
                }
                ## Now add legend if needed
                hasgrob <- sapply(legend,inherits, "grob")
                if(any(hasgrob)){
                    howmany <- sum(hasgrob)
                    if(howmany == 1) {
                        wh <- hv[hasgrob]
                        new <- list(fun = legend[[wh]])
                        obj$legend = c(olegend, structure(list(new),
                                                          names = lloc))
                    } else {
                        fg <- buildLegend(legend,lloc)
                        obj$legend <- c(olegend,structure(list(list(fun = fg)),
                                                          names = lloc))
                    }
                }
                # for update() function
                obj$addScales <- list(orig = list(panel = opanel,
                                                  legend = olegend),
                                      args = c(list(
                                          scaleline = scaleline,
                                          legend = legendval,
                                          ndig.legend = ndig.legend,
                                          legend.aes = legend.aes,
                                          legend.loc = lloc,
                                          panelFUN = panelFUN),
                                          list(...)))
                class(obj) <- c("scaledTrellis", "trellis")
                obj},
            ### rest of enclosing tryCatch
            simpleError = function(e){
                message( "** addScales error **", conditionMessage(e))
                invisible(NULL)
            })


