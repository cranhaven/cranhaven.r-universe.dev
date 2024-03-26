#' @name methods_localsrq
#' @title Methods for class localsrq
#' @description The \code{plot()} function allows the user to plot significant observations.
#' The \code{print()} function is used to print the number of runs in each localization. Additional information of
#' expected values and standard deviation, z-value ans p-value is prited for each observation.
#'
#' @param x a \code{localsrq} object created by \code{\link{Q.test}}.
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

#' @name print.localsrq
#' @rdname methods_localsrq
#' @export
print.localsrq <- function(x, ...) {
  if (!inherits(x, "localsrq")) stop("Argument must be a localsrq object")
print(x$local.SRQ)
invisible(x)
  }

#' @name plot.localsrq
#' @rdname methods_localsrq
#' @export
#'
#'
#'
plot.localsrq <- function(x, ..., sf = NULL, coor = NULL,  sig = 0.05){
  if (!inherits(x, "localsrq")) stop("Argument must be a localsrq object")
  if (x$sf == TRUE & is.null(sf)) stop("Include the sf object using the sf argument")

  lsrq <- x
  if (!is.null(lsrq$nsim)){
    a <- as.factor((lsrq$local.SRQ$pseudo.value < sig)*(lsrq$local.SRQ$zseudo.value > 0)*1 +
                     (lsrq$local.SRQ$pseudo.value < sig)*(lsrq$local.SRQ$zseudo.value < 0)*2)
  } else {
    a <- as.factor((lsrq$local.SRQ$p.value < sig)*(lsrq$local.SRQ$z.value > 0)*1 +
                     (lsrq$local.SRQ$p.value < sig)*(lsrq$local.SRQ$z.value < 0)*2)
  }
  #####################
  ### Plot SR Local
  #####################
  if (is.null(sf)){
    if (is.null(coor) &&
        (inherits(lsrq$listw, "knn"))){
    coor <- as.data.frame(lsrq$listw$x)
    }
    if (!is.null(coor) &&
        (!inherits(lsrq$listw, "knn"))){
      coor <- as.data.frame(coor)
    }
    sf <- st_as_sf(coor,coords = names(coor))
    mysize = 4
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
      levels(sf$levels)[levels(sf$levels)=="0"] <- "non-sig"
      levels(sf$levels)[levels(sf$levels)=="1"] <- "sig +"
      levels(sf$levels)[levels(sf$levels)=="2"] <- "sig -"
      cols <- c("non-sig" = "grey77", "sig +" = "red", "sig -" = "blue")
      lplot_runs <- ggplot(sf) +
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
      lplot_runs
}
