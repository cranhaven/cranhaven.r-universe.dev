#' @name methods_localjc
#' @title Methods for class localjc
#' @description The \code{plot()} function allows the user to plot significant observations.
#' The \code{print()} function is used to print the number of runs in each localization. Additional information of
#' expected values and standard deviation, z-value ans p-value is prited for each observation.
#'
#' @param x a \code{localjc} object created by \code{\link{Q.test}}.
#' @param sig significant level for each observation in \code{plot()} method. Default \code{sig = 0.05}
#' @param sf optional argument for \code{plot()} method to include a sf object (default = NULL)
#' @param coor optional argument for \code{plot()} method to include coordinates of points (default = NULL)
#' @param ... further arguments passed to or from other methods.
#' @return No return value, called for side effects
#' @examples
#' # Example 1: Local spatial runs test based on knn
#' N <- 100
#' cx <- runif(N)
#' cy <- runif(N)
#' x <- cbind(cx,cy)
#' listw <- spdep::knearneigh(cbind(cx,cy), k = 10)
#' p <- c(1/6,3/6,2/6)
#' rho <- 0.5
#' fx <- dgp.spq(p = p, listw = listw, rho = rho)
#'
#' # Asymtotic version
#' lsrq <- local.sp.runs.test(fx = fx, listw = listw, alternative = "less")
#' print(lsrq)
#' plot(lsrq, sig = 0.05)
#'
#' @author
#'   \tabular{ll}{
#'   Fernando López  \tab \email{fernando.lopez@@upct.es} \cr
#'   Román Mínguez  \tab \email{roman.minguez@@uclm.es} \cr
#'   Antonio Páez \tab \email{paezha@@gmail.com} \cr
#'   Manuel Ruiz \tab \email{manuel.ruiz@@upct.es} \cr
#'   }
#' @references
#'   \itemize{
#'     \item Ruiz, M., López, F., and Páez, A. (2021).
#'     A test for global and local homogeneity of categorical data based on spatial runs.
#'       \emph{working paper}.
#'   }
#'

NULL

#' @name print.localjc
#' @rdname methods_localjc
#' @export
print.localjc <- function(x, ...) {
  if (!inherits(x, "localjc")) stop("Argument must be a localjc object")
  print(x$ljc)
  invisible(x)
  }

#' @name plot.localjc
#' @rdname methods_localjc
#' @export
#'
#'
#'
plot.localjc <- function(x, ..., sf = NULL, coor = NULL,  sig = 0.05){
  if (!inherits(x, "localjc")) stop("Argument must be a localjc object")
  # if (x$sf == TRUE & is.null(sf)) stop("Include the sf object using the sf argument")
  ljc <- x
  a <- as.factor((ljc$ljc$pseudo.value < sig)*1)
  #####################
  ### Plot JC Local
  #####################
  # if (is.null(sf)){
  #   if (is.null(coor) &&
  #       (inherits(lsrq$listw, "knn"))){
  #   coor <- as.data.frame(lsrq$listw$x)
  #   }
  #   if (!is.null(coor) &&
  #       (!inherits(lsrq$listw, "knn"))){
  #     coor <- as.data.frame(coor)
  #   }
  #   sf <- st_as_sf(coor,coords = names(coor))
  #   mysize = 4
  # }

  if (is.null(sf)){
    if (!is.null(coor)){
      coor <- as.data.frame(coor)
      sf <- st_as_sf(coor,coords = names(coor))
      mysize = 4
    }
  }

    if (!is.null(sf)){
      mysize = .2
      if (inherits(st_geometry(sf),
          "sfc_MULTIPOLYGON")) mysize = .2
      if (inherits(st_geometry(sf),
          "sfc_POLYGON")) mysize = .2
      if (inherits(st_geometry(sf),
          "sfc_POINT")) mysize = 4
    }

  sf$levels <- addNA(a)
  levels(sf$levels)[is.na(levels(sf$levels))] <- "NA"
  levels(sf$levels)[levels(sf$levels)=="0"] <- "non-sig"
  levels(sf$levels)[levels(sf$levels)=="1"] <- "sig"
  cols <- c("NA" = "white", "non-sig" = "grey77", "sig" = "red")
  plot_jc <- ggplot(sf) +
    geom_sf(aes(fill = levels),
            color = "black", shape = 21,
            size = mysize) +
    theme_bw() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank()) +
    xlab(paste0("Significance p-value = ",
                sig)) +
    scale_fill_manual(values = cols,
                      na.value ="grey",
                      drop = FALSE)
  plot_jc
}
