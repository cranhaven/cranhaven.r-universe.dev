#' @name methods_scantest
#' @title Methods for class scantest
#' @description The \code{plot()} function allows the user plot the significant cluster(s).\cr
#'  \code{\link{summary}} list information about the most likelihood cluster.\cr
#' @param x is a \code{scantest} object created by \code{\link{scan.test}} for \code{plot()} method.
#' @param sf optional argument for \code{plot()} method to include a sf object (default = NULL)
#' @param coor optional argument for \code{plot()} method to include coordinates of points (default = NULL)
#' @param object a \code{scantest} object created by \code{\link{scan.test}} for \code{summary()} method.
#' @param ... further arguments passed to or from other methods.
#' @return No return value, called for side effects
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
#'       \emph{Working paper}.
#'   }
#'

NULL

#' @name plot.scantest
#' @rdname methods_scantest
#' @export
#'
#'
plot.scantest <- function(x, ..., sf = NULL, coor = NULL){
  z <- x
  if (z$sf == TRUE & is.null(sf)) stop("Include the sf object using the sf argument")
  if (!inherits(z, "scantest")) stop("Argument must be a scantest object")
  # if (is.null(z$coor)) stop("Include the sf object to generate the plot")
  a <- matrix(0,ncol = 1,nrow = z$N)
  a[z$MLC] <- 1

  # To include in plot secondary clusters
  if (z$distr == "multinomial"){
  if  (sum(z$p.value.secondary < 0.1)>0){
  for (f in 1:sum(z$p.value.secondary < 0.1)){
  a[z$secondary.clusters[[f]]] <- 2
  }
  }
  }
  if (z$distr == "bernoulli"){
    if  (sum(z$p.value.secondary < 0.1)>0){

    if (z$alternative == "High"){
      for (f in 1:sum(z$p.value.secondary < 0.1)){
        a[z$secondary.clusters[[f]]] <- 2
      }
    }
  if (z$alternative == "Low"){
      for (f in 1:sum(z$p.value.secondary < 0.1)){
        a[z$secondary.clusters[[f]]] <- 3
      }
    }

  if (z$alternative == "Both"){
    for (f in 1:sum(z$p.value.secondary < 0.1)){
      ## ff calcula el porcentaje de casos del tipo 'casos' dentro del cluster secundario
      ff <- sum(z$fx[z$secondary.clusters[[f]]]==as.character(z$case))/length(z$secondary.clusters[[f]])
      if (ff > 0.5){
      a[z$secondary.clusters[[f]]] <- 2
      } else {
      a[z$secondary.clusters[[f]]] <- 3
      }
    }
  }
  }
  }
  #####################
  ### Plot
  #####################
  if (!is.null(z$coor)){
    coor <- as.data.frame(z$coor)
    sf <- st_as_sf(coor,coords = names(coor))
    mysize = 4
  }
  if (!is.null(sf)){
    mysize = .2
    if (inherits(st_geometry(sf)[1],
        "sfc_MULTIPOLYGON")) mysize = .2
    if (inherits(st_geometry(sf)[1],
        "sfc_POLYGON")) mysize = .2
    if (inherits(st_geometry(sf)[1],
        "sfc_POINT")) mysize = 2
  }
  sf$levels <- addNA(a)
  if (z$distr == "multinomial"){
    levels(sf$levels)[levels(sf$levels) == "0"] <- "non-sig"
    levels(sf$levels)[levels(sf$levels) == "1"] <- "sig"
    levels(sf$levels)[levels(sf$levels) == "2"] <- "sig (secondary)"
    cols <- c("non-sig" = "white", "sig" = "red", "sig (secondary)" = "#ff726f")
  }
  if (z$distr=="bernoulli"){
    levels(sf$levels)[levels(sf$levels)=="0"] <- "non-sig"
    if (z$cases.expect < z$cases.observ)
      levels(sf$levels)[levels(sf$levels)=="1"] <- "sig High"
    if (z$cases.expect > z$cases.observ)
      levels(sf$levels)[levels(sf$levels)=="1"] <- "sig Low"

    levels(sf$levels)[levels(sf$levels)=="2"] <- "sig High (secondary)"
    levels(sf$levels)[levels(sf$levels)=="3"] <- "sig Low (secondary)"
    cols <- c("non-sig" = "white", "sig High" = "red", "sig Low" = "blue",
              "sig High (secondary)"="#FC856B","sig Low (secondary)"="#86E8FE")
  }

  lplot_scan <- ggplot(sf) +
    geom_sf(aes(fill = levels),
                     color = "black",
                     shape = 21, size = mysize) +
    theme_bw() +
    theme(axis.text.x = element_blank(),
                   axis.text.y = element_blank()) +
    xlab(paste0("Significance p-value of MLC = ",
                         z$p.value)) +
    scale_fill_manual(values = cols,
                               na.value ="orange",
                               drop = FALSE)

  lplot_scan
}

#' @name summary.scantest
#' @rdname methods_scantest
#' @export
#'
summary.scantest <- function(object, ...) {
  z <- object
  # Print output
  if (z$distr=="bernoulli"){
  cat("\nSummary of data:\n")
  cat(paste0("Distribution....................: ",z$distr,"\n"))
  cat(paste0("Type of cluster (alternative)...: ",z$alternative,"\n"))
  cat(paste0("Number of locations.............: ",z$N,"\n"))
  if (z$distr=="bernoulli"){
  cat("Cathegory case..................: ")
  cat(as.character(z$case))
  cat("\n")
  }
  cat(paste0("Total number of observations....: ",sum(z$fx == z$case),"\n"))
  cat("Names of cathegories............: ")
  cat(z$cases.names)
  cat("\n")
  cat("Total per category..............: ")
  cat(round(as.vector(table(z$fx))))
  cat("\n")
  cat("Percent per category............: ")
  cat(round(as.vector(table(z$fx))/z$N, digits = 2))
  cat("\n")
  cat("---------------------------------\n")
  cat("\nScan statistic: Most Likely Cluster\n")
  cat("Total observations in the MLC........: ")
  cat(length(z$MLC))
  cat("\n")
  cat("Names of cathegories.................: ")
  cat(z$cases.names)
  cat("\n")
  cat("Percent per category total...........: ")
  cat(round(as.vector(table(z$fx))/z$N,digits = 2))
  cat("\n")
  cat("Percent per category inside cluster..: ")
  cat(round(as.vector(table(z$fx[z$MLC]))/length(z$MLC),digits = 2))
  cat("\n")
  cat(paste0("Value of statisitic (loglik ratio)...: ",round(z$statistic, digits = 4) ,"\n"))
  cat(paste0("p-value..............................: ",round(z$p.value, digits = 4),"\n"))
  cat("\nIDs of cluster detect:\n")
  cat("Location IDs included...: ",z$MLC)
  if (length(z$Alternative.MLC)>0){
    for (i in 1:length(z$Alternative.MLC)){
      cat("\n\n")
      cat("Location IDs of alternative MLC with the same loglik: ",z$MLC)
    }
  }
  ################################
  # print secondary clusters
  cat("\n---------------------------------")
  for (f in 1:length(z$loglik.second)){
  cat("\n\n")
  cat("\nSecondary Cluster. Number", f,"\n")
  cat("Total observations in secondary cluster.: ",length(z$secondary.clusters[[f]]) ,"\n")
  cat("Names of cathegories.................: ")
  cat(z$cases.names)
  cat("\n")
  cat("Percent per category total...........: ")
  cat(round(as.vector(table(z$fx))/z$N,digits = 2))
  cat("\n")
  cat("Percent per category inside cluster..: ")
  cat(round(as.vector(table(z$fx[z$secondary.clusters[[f]]]))/length(z$secondary.clusters[[f]]),digits = 2))
  cat("\n")
  cat(paste0("Value of statisitic (loglik ratio)...: ",round(z$loglik.second[f], digits = 4) ,"\n"))
  cat(paste0("p-value..............................: ",round(z$p.value.secondary[f], digits = 4),"\n"))
  cat("Location IDs included................: ",z$secondary.clusters[[f]])
  }
  }
  if (z$distr=="multinomial"){
    cat("\nSummary of data:\n")
    cat(paste0("Distribution....................: ",z$distr,"\n"))
    cat(paste0("Number of locations.............: ",z$N,"\n"))
    cat(paste0("Total number of cases...........: ",z$N,"\n"))
    cat("Names of cathegories...........: ")
    cat(z$cases.names)
    cat("\n")
    cat("Total cases per category........: ")
    cat(round(as.vector(table(z$fx))))
    cat("\n")
    cat("Percent cases per category......: ")
    cat(round(as.vector(table(z$fx))/z$N, digits = 2))
    cat("\n")
    cat("\nScan statistic:\n")
    cat("Total cases in the MLC.........: ")
    cat(length(z$MLC))
    cat("\n")
    cat("Names of cathegories...........: ")
    cat(z$cases.names)
    cat("\n")
    cat("Observed cases in the MLC......: ")
    cat(round(z$cases.expect[1:length(unique(z$fx))], digits = 2))
    cat("\n")
    cat("Expected cases in the MLC......: ")
    cat(z$cases.observ[1:length(unique(z$fx))])
    cat("\n")
    cat(paste0("Value of statistic (loglik ratio)....: ",round(z$statistic, digits = 4) ,"\n"))
    cat(paste0("p-value........................: ",round(z$p.value, digits = 4),"\n"))
    cat("\nIDs of cluster detect:\n")
    cat("Location IDs included.....: ",z$MLC)
    if (length(z$Alternative.MLC)>0){
      for (i in 1:length(z$Alternative.MLC)){
      cat("\n\n")
      cat("Location IDs of alternative MLC with the same loglik: ",z$MLC)
      }
    }

    ################################
    # print secondary clusters
    for (f in 1:length(z$loglik.second)){
      cat("\n\n")
      cat("\nSecondary Scan statistic. Number", f,"\n")
      cat("Total cases in secondary cluster......: ",length(z$secondary.clusters[[f]]) ,"\n")
      cat("Names of cathegories.................: ")
      cat(z$cases.names)
      cat("\n")
      cat("Percent per category total...........: ")
      cat(round(as.vector(table(z$fx))/z$N,digits = 2))
      cat("\n")
      cat("Percent per category inside cluster..: ")
      cat(round(as.vector(table(z$fx[z$secondary.clusters[[f]]]))/length(z$secondary.clusters[[f]]),digits = 2))
      cat("\n")
      cat(paste0("Value of statisitic (loglik ratio)....: ",round(z$loglik.second[f], digits = 4) ,"\n"))
      cat(paste0("p-value.........................: ",round(z$p.value.secondary[f], digits = 4),"\n"))
      cat("Location IDs included..................: ",z$secondary.clusters[[f]])
    }

  }
}

