#' S3 method to plot a DTDAcif object by using the generic plot function.
#'
#' @title plot.DTDAcif
#'
#' @aliases  plot.DTDAcif
#'
#' @param x DTDAcif object.
#' @param intervals Logical. If TRUE confidence intervals are calculated if standard deviation was calculated before.
#' @param level Confidence level of the standard deviation of the cifs. Default is 0.95.
#' @param main An overall title for the plot.
#' @param xlab A title for the x axis.
#' @param ylab A title for the y axis.
#' @param ylim Limit over the y axis.
#' @param xlim Limit over the x axis.
#' @param ... Additional parameters.
#'
#'
#'
#' @author
#' \itemize{
#' \item{de Uña-Álvarez, Jacobo.}
#' \item{Soage González, José Carlos.}
#' \item{Maintainer: José Carlos Soage González. \email{jsoage@@uvigo.es}}
#' }
#'
#'
#' @export
plot.DTDAcif <- function(x, intervals = FALSE, level = 0.95,  main = "", xlab = "", ylab = "", ylim, xlim, ...) {

  if (!inherits(x, "DTDAcif")) {
    stop("'x' must be of class 'DTDAcif'")
  }
  r <- NULL
  r <- x

  if(intervals == TRUE & is.null(r$sd.boot)){
    warning("In order to plot confidence intervals execute the main function with boot = TRUE")
  }

  if(is.null(r$sd.boot)){
    intervals == FALSE
  }

  if(missing(ylim)) ylim = c(0,1)
  if(missing(xlim)) xlim = c(min(r$data$x), max(r$data$x))

  if (is.null(r$method)) {

    graphics::plot(c(min(r$data$x), r$data$x[order(r$data$x)]), c(0, cumsum(r$cif.mas[order(r$data$x)])),
                   type = "s", ylim = ylim, xlim = xlim,
                   main = main, xlab = xlab, ylab = ylab, ... = ...)

    if(!is.null(r$sd.boot)){
    if(intervals == TRUE) {
      cif <- unlist(lapply(1:length(unique(r$data$x)),
                           function(i) stats::approx(r$data$x[order(r$data$x)], cumsum(r$cif.mas[order(r$data$x)]),
                                                     xout = unique(r$data$x[order(r$data$x)])[i],
                                                     ties = max, yleft = stats::approx(r$data$x[order(r$data$x)], cumsum(r$cif.mas[order(r$data$x)]),
                                                                                       xout = min(r$data$x[order(r$data$x)]), ties = min)$y,
                                                     method = "constant", rule = 2)$y))


      lim.sup <- cif + stats::qnorm((1 - level) / 2,  lower.tail = FALSE) * r$sd.boot
      lim.sup <- unlist(lapply(1:length(lim.sup), function(i) if(lim.sup[i] > 1) lim.sup[i] = 1 else lim.sup[i] = lim.sup[i]))

      lim.inf <- cif - stats::qnorm((1 - level) / 2, lower.tail = FALSE) * r$sd.boot
      lim.inf <- unlist(lapply(1:length(lim.inf), function(i) if(lim.inf[i] < 0) lim.inf[i] = 0  else lim.inf[i] = lim.inf[i]))

      graphics::lines(unique(r$data$x)[order(unique(r$data$x))], lim.sup, type = "s", col = "lightgray", lty = 2)
      graphics::lines(unique(r$data$x)[order(unique(r$data$x))], lim.inf, type = "s", col = "lightgray", lty = 2)
    }
  }

  } else {

    if (r$method == "dep") {

      x <- vector("list", length(unique(r$data$z)))

      for (i in  length(unique(r$data$z)):1) {

        x[i]   <- list(r$data$x[r$data$z == i])
      }

      nz <- length(unique(r$data$z))
      cif <- vector("list", nz)

      for(j in 1:nz){

        if(length(r$data$z[r$data$z == j]) == 1){
          cif[[j]] <- unlist(lapply(1:length(unique(unlist(x[j]))),
                                    function(i) stats::approx(unlist(x[j])[order(unlist(x[j]))],
                                                              cumsum(unlist(r$cif[[j]])),
                                                              xout = unique(unlist(x[j])[order(unlist(x[j]))])[i],
                                                              ties = max,
                                                              method = "constant", rule = 2)$y))

        } else {
        cif[[j]] <- unlist(lapply(1:length(unique(unlist(x[j]))),
                                  function(i) stats::approx(unlist(x[j])[order(unlist(x[j]))],
                                                            cumsum(unlist(r$cif[[j]])),
                                                            xout = unique(unlist(x[j])[order(unlist(x[j]))])[i],
                                                            ties = max, yleft = stats::approx(unlist(x[j])[order(unlist(x[j]))],
                                                                                              cumsum(unlist(r$cif[[j]])),
                                                                                              xout = min(unlist(x[j])[order(unlist(x[j]))]),
                                                                                              ties = min)$y,
                                                            method = "constant", rule = 2)$y))

        }
        # pointsb[[j]] <- na.omit(unlist(pointsb[[j]]))
      }



      lim.sup <- vector("list", nz)
      lim.inf <- vector("list", nz)

      if(!is.null(r$sd.boot)){
      for(p in 1:nz){
        lim.sup1 <- cif[[p]] + stats::qnorm((1-level )/2, lower.tail = FALSE) * r$sd.boot[[p]]
        lim.sup[[p]] <- unlist(lapply(1:length(lim.sup1), function(i) if(lim.sup1[i] > 1) lim.sup1[i] = 1 else lim.sup1[i] = lim.sup1[i]))

        lim.inf1 <- cif[[p]] - stats::qnorm((1-level )/2, lower.tail = FALSE) * r$sd.boot[[p]]
        lim.inf[[p]] <- unlist(lapply(1:length(lim.inf1), function(i) if(lim.inf1[i] < 0) lim.inf1[i] = 0  else lim.inf1[i] = lim.inf1[i]))

      }
      }


      invisible(lapply(1:length(r$cif),
                       function(i){
                         if  (i == 1) {

                           graphics::plot(c(min(unlist(x[i])[order(unlist(x[i]))]),unlist(x[i])[order(unlist(x[i]))]),
                                          c(0, cumsum(unlist(r$cif[i][order(r$data$x[r$data$z == i])]))), type = "s",
                                          ylim = ylim, xlim = xlim,
                                          main = main, xlab = xlab, ylab = ylab)
                         } else {
                           graphics::lines(c(min(unlist(x[i])[order(unlist(x[i]))]),unlist(x[i])[order(unlist(x[i]))]), c(0,cumsum(unlist(r$cif[[i]][order(order(r$data$x[r$data$z == i]))]))),
                                           type = "s", col = i, ... = ...)
                           if(!is.null(r$sd.boot)){
                           if (intervals == T) {
                             for(u in 1:nz) {
                               graphics::lines(unique(unlist(x[u])[order(unlist(x[u]))]), lim.sup[[u]], type = "s",
                                               col = u, lty = 2)
                               graphics::lines(unique(unlist(x[u])[order(unlist(x[u]))]), lim.inf[[u]], type = "s",
                                               col = u, lty = 2)
                             }
                           }
                           }
                         }
                       }))
    }


    if (r$method == "indep") {

      nz <- length(unique(r$data$z))
      cif <- vector("list", nz)

      data <- data.frame(cbind(r$data$x, r$data$z))
      colnames(data) <- c("x", "z")



      for(j in 1:nz){

        if(length(r$data$z[r$data$z == j]) == 1){

      cif[[j]] <- unlist(lapply(1:length(unique(subset(data.frame(data),
                                                       data.frame(data)$z == j))$x),
                                function(i) stats::approx(unlist(unlist(r$data$x[r$data$z == j]))[order(unlist(unlist(r$data$x[r$data$z == j])))],
                                                          cumsum(unlist(r$cif.mas[[j]][order(r$data$x[r$data$z == j])])),
                                                          xout = unique(subset(data.frame(data),
                                                                               data.frame(data)$z == j)$x[order(subset(data.frame(data), data.frame(data)$z == j)$x)])[i],
                                                          ties = max,
                                                          method = "constant", rule = 2)$y))
        } else {


          cif[[j]] <- unlist(lapply(1:length(unique(subset(data.frame(data),
                                                           data.frame(data)$z == j))$x),
                                    function(i) stats::approx(unlist(unlist(r$data$x[r$data$z == j]))[order(unlist(unlist(r$data$x[r$data$z == j])))],
                                                              cumsum(unlist(r$cif.mas[[j]][order(r$data$x[r$data$z == j])])),
                                                              xout = unique(subset(data.frame(data),
                                                                                   data.frame(data)$z == j)$x[order(subset(data.frame(data), data.frame(data)$z == j)$x)])[i],
                                                              ties = max,
                                                              yleft = stats::approx(r$data$x[r$data$z == j][order(r$data$x[r$data$z == j])],
                                                                                    cumsum(unlist(r$cif.mas[[j]][order(r$data$x[r$data$z == j])])),
                                                                                    xout = min(r$data$x[r$data$z == j][order(r$data$x[r$data$z == j])]),
                                                                                    ties = min)$y,
                                                              method = "constant", rule = 2)$y))
        }


      }

      for(w in 1:nz){
        cif[[w]] <- stats::na.omit(cif[[w]])
      }

      lim.sup <- vector("list", nz)
      lim.inf <- vector("list", nz)

      if(!is.null(r$sd.boot)){
        for(p in 1:nz){
          lim.sup1 <- cif[[p]] + stats::qnorm((1 - level )/2, lower.tail = FALSE) * r$sd.boot[[p]]
          lim.sup[[p]] <- unlist(lapply(1:length(lim.sup1), function(i) if(lim.sup1[i] > 1) lim.sup1[i] = 1 else lim.sup1[i] = lim.sup1[i]))

          lim.inf1 <- cif[[p]]- stats::qnorm((1 - level )/2, lower.tail = FALSE) * r$sd.boot[[p]]
          lim.inf[[p]] <- unlist(lapply(1:length(lim.inf1), function(i) if(lim.inf1[i] < 0) lim.inf1[i] = 0  else lim.inf1[i] = lim.inf1[i]))
        }
      }

      invisible(lapply(1:length(r$cif),
                       function(i){
                         if (i == 1) {
                           graphics::plot(c(min(r$data$x[r$data$z == i][order(r$data$x[r$data$z == i])]), r$data$x[r$data$z == i][order(r$data$x[r$data$z == i])]),
                                          c(0, cumsum(unlist(r$cif.mas[i][order(r$data$x[r$data$z == i])]))), type = "s",
                                          ylim = ylim, xlim = xlim,
                                          main = main, xlab = xlab, ylab = ylab, ... = ...)
                         } else {
                           graphics::lines(c(min(r$data$x[r$data$z == i][order(r$data$x[r$data$z == i])]), r$data$x[r$data$z == i][order(r$data$x[r$data$z == i])]), c(0,cumsum(unlist(r$cif[[i]][order(r$data$x[r$data$z == i])]))),
                                           type = "s", col = i, ... = ...)
                           if(!is.null(r$sd.boot)){
                           if(intervals == TRUE) {
                             for(i in 1:nz){

                               graphics::lines(unique(subset(data.frame(data), data.frame(data)$z == i)$x[order(subset(data.frame(data), data.frame(data)$z == i)$x)]), lim.sup[[i]], type = "s", col = i, lty = 2)
                               graphics::lines(unique(subset(data.frame(data), data.frame(data)$z == i)$x[order(subset(data.frame(data), data.frame(data)$z == i)$x)]), lim.inf[[i]], type = "s", col = i, lty = 2)
                             }
                           }
                         }
                         }
                       }))
    }
  }
}

