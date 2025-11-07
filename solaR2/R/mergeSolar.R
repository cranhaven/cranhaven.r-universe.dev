setGeneric('mergesolaR', signature = '...', function(...){standardGeneric('mergesolaR')})

fooMeteo <- function(object, var){yY <- getData(object)[, .SD,
                                                        by = Dates,
                                                        .SDcols = var]}

fooG0 <- function(object, var){yY <- as.data.tableD(object)[, .SD,
                                                            by = Dates,
                                                            .SDcols = var]}

mergeFunction <- function(..., foo, var){
    dots <- list(...)
    dots <- lapply(dots, as, class(dots[[1]])) ##the first element is the one that dictates the class to everyone
    nms0 <- substitute(list(...))
    if (!is.null(names(nms0))){ ##do.call
        nms <- names(nms0[-1])
    } else { 
        nms <- as.character(nms0[-1])
    }
    cdata <- sapply(dots, FUN = foo, var, simplify = FALSE)
    z <- cdata[[1]]
    for (i in 2:length(cdata)){
        z <- merge(z, cdata[[i]], by = 'Dates', suffixes = c("", paste0('.', i)))
    }
    names(z)[-1] <- nms
    z
}

setMethod('mergesolaR',
          signature = 'Meteo',
          definition = function(...){
            res <- mergeFunction(..., foo = fooMeteo, var = 'G0')
            res
          }
          )

setMethod('mergesolaR',
          signature = 'G0',
          definition = function(...){
            res <- mergeFunction(..., foo = fooG0, var = 'G0d')
            res
          }
          )

setMethod('mergesolaR',
          signature = 'Gef',
          definition = function(...){
            res <- mergeFunction(..., foo = fooG0, var = 'Gefd')
            res
          }
          )

setMethod('mergesolaR',
          signature = 'ProdGCPV',
          definition = function(...){
            res <- mergeFunction(..., foo = fooG0, var = 'Yf')
            res
          }
          )

setMethod('mergesolaR',
          signature = 'ProdPVPS',
          definition = function(...){
            res <- mergeFunction(..., foo = fooG0, var = 'Yf')
            res
          }
          )
