#' @title spEMD
#' @aliases spEMD
#'
#' @description 2D EMD for spatial objects
#' @author Pierre Roudier
#'
#' @param data Input dataset, either a `data.frame` or a `Spatial*DataFrame`
#' @param zcol Name of the column containing the attribute of interest.
#' @param method Interpolation method. Currently only `splines` is supported.
#' @param n.imf.max Maximum depth of decomposition (maximum number of IMF).
#' @param n.sp.max Number of iterations in the sifting process.
#' @param n.extrema.min Minimum number of extrema.
#' @param stoprule Rule used to stop the EMD process. Currently only `mean.imf` is implemented.
#' @param stoprule.extrema Should `spEMD` checks for the number of extrema to be similar? Defaults to `TRUE`.
#' @param thresh.extrema Significative threshold for the extrema. Defaults to 1.
#' @param tol Value that the avergae of the IMF candidate need to reach so to be considered as a valid IMF.
#' @param diff.nb.extrema Percentage limit difference maxima/minima. If smaller, more permissive on the mean of the IMF candidate.
#' @param abs.nb.extrema Absolute difference between number of extrema.
#' @param nb.nn Number of nearest neighbours to take into account (when data is on a regular grid).
#' @param n.pts.spline Number of points to locally interpolate IMFs.
#' @param neig Option the re-use a formerly existing neig object in order to save time.
#' @param save_neig Option to save the neig object as a .RData file once created.
#' @param verbose Prints progress information messages. Defaults to TRUE.
#'
#' @return .
#'
#' @examples
#'
#' # Getting sample data from the gstat package
#' if (require(gstat)) {
#' library(sp)
#'
#' # Example for gridded data
#' data(ncp.grid, package = 'gstat')
#' coordinates(ncp.grid) <- ~x+y
#' gridded(ncp.grid) <- TRUE
#' res.ncp <- spEMD(ncp.grid, zcol = "depth", thresh.extrema = 0.1, verbose = FALSE)
#'
#' # Plot results
#' spplot(res.ncp[, c('imf1', "imf2", "imf3")])
#' }
#'
#' #
#'
#'
#' @include create_neig.r extrema_irr.r extract_extrema.r mean_enveloppe.r
#'
#' @importFrom sp coordnames gridded coordinates "coordinates<-" "coordnames<-" "gridded<-"
#' @export
spEMD <- function(
  # DATA
  data,	# Input matrix at the data.frame or sp format
  zcol = "z",
  # METHOD
  method = 'splines', # Enveloppe interpolation method
  n.imf.max=10,	# Maximum depth of decomposition (number of IMFs)
  n.sp.max=5,	# Number of iterations in the sifting process
  n.extrema.min=1, # Number of extrema below which the data is monotone. Defaults to 1.
  stoprule="mean.imf",
  stoprule.extrema=TRUE, # Also checks for the number of extrema to be similar
  # THRESHOLDS
  thresh.extrema=1, # Significative threshold for the extrema
  tol=0, # Mean of the IMF candidate to be considered as a proper IMF
  diff.nb.extrema=0.05, # % Limit difference maxima/minima. If smaller, more permissive on the mean of the IMF candidate
  abs.nb.extrema=5, # Absolute difference between number of extrema
  # PARAMETERS FOR LOCAL INTERPOLATION
  nb.nn=4, # Number of nearest neighbours to take into account when data is on a grid
  n.pts.spline=4,	# number of points to locally interpolate
  # NEIGHBOURHOOD MANAGEMENT
  neig=NULL, # Option the re-use a formerly existing neig object in order to save time
  save_neig=TRUE, # Option to save the neig object as a .RData file once created
  # USER INTERACTION
  verbose=TRUE
){

  # Make sure to work with x and y as coordinates names
  if (all(coordnames(data) == c("x","y"))) {
    change_coords_names <- FALSE
  } else {
    # Backup old coord names if need be
    change_coords_names <- TRUE
    old_coord_names <- coordnames(data)
  }
  # Setting coordnames to 'x' and 'y'
  coordnames(data) <- c("x","y")

  # testing if data is gridded
  gridded.data <- gridded(data)

  dat.to.process <- data

  result <- as.data.frame(coordinates(data))
  result[[zcol]] <- data[[zcol]]

  n.imf <- 1
  n.max.prec <- n.min.prec <- nrow(data)

  # Creation of the neighbourhood description object
  # one time for every step (only the z attribute is updated)
  #
  if (is.character(neig)) {

    if (verbose) cat("Retrieving existing neighbourhood object...\t")

    neig <- try(readRDS(neig), silent = TRUE)

    if (any(class(neig) == "try-error")) stop("The neighbourhood object cannot be loaded. Wrong path to the .RData file.")
    # Verifications that the neig object is correct
    if (!any(class(neig) != "neig")) stop("The neighbourhood object needs to be generated through the create.neig() function.")

  }
  else {

    if (verbose) cat("Creation of neighbourhood object...\n")

    neig <- create.neig(
      data.set = data,
      nb.nn = nb.nn,
      duplicate = 'remove',
      # gridded.data = gridded.data,
      verbose = FALSE
    )

    if (save_neig) {
      if (verbose) cat("Saving neighbourhood object as a .RData file...\t")
      saveRDS(neig, file=".neig")
    }

  }

  if (verbose) cat('\n')

  # For each IMF
  while (TRUE) {

    if (verbose) cat(paste0('IMF #', n.imf, '\n'))

    criterion.sifting <- FALSE
    n.curr.sp <- 1
    input.imf <- dat.to.process

    if (stoprule != "mean.enveloppe") best.stop.criterion <- Inf
    else best.stop.criterion <- -Inf

    # SIFTING PROCESS
    while (TRUE) {

      if (verbose) cat(paste('\tSifting #',n.curr.sp,'\n',sep=''))

      # Identify all the local extrema
      if (verbose) cat('\t\tIdentification of the regional extrema...\n')

      curr.extrema <- extrema.irr(
        input.imf,
        zcol = zcol,
        gridded.data = gridded.data,
        nb.nn = nb.nn,
        thresh.extrema = thresh.extrema,
        verbose = verbose,
        neig = neig
      )

      if (verbose) cat('\t\tExtracting extrema...\n')

      curr.mat.extrema <- extract.extrema(
        curr.extrema,
        n.extrema.min = n.extrema.min
      )

      n.min <- curr.mat.extrema$nb.min
      n.max <- curr.mat.extrema$nb.max

      if (!curr.mat.extrema$monotone) {

        # Interpolate the local extrema to get a mean enveloppe
        if (verbose) cat('\t\tInterpolation of the enveloppe...\n')

        mean.enveloppe <- return.mean.enveloppe(
          extrema = curr.mat.extrema$extrema,
          data = input.imf,
          method = method,
          n.pts.spline = n.pts.spline,
          zcol = zcol,
          verbose = verbose
        )

        imf.candidate <- as.data.frame(coordinates(data))
        imf.candidate[[zcol]] <- input.imf[[zcol]] - mean.enveloppe[[zcol]]

        if (verbose) cat('\t\tChecking if the candidate is an IMF...\n')
        # Different ways to check if IMF : mean of the resulting signal VS mean of the enveloppe of the resulting signal (Linderhed, 2002)
        #

        if (stoprule == "mean.enveloppe") {

          # After Linderhed, 2002 (every meanenveloppe pt is < epsilon)

          stop.sifting <- all(mean.enveloppe[[zcol]] <= tol)
          stop.criterion <- length(which(mean.enveloppe[[zcol]] <= tol))/length(mean.enveloppe[[zcol]])

          # if (verbose) cat(paste("\t\t\tThe current mean enveloppe has ",
          #                        100*round(length(which(mean.enveloppe[[zcol]] <= tol))/length(mean.enveloppe[[zcol]]),digits=4),
          #                        "% of its values < ",tol,"\n",sep=""))

        }

        if (stoprule == "mean.imf") {

          # Mean of the IMF candiate is close to zeros, and similar number of extremas

          stop.sifting <- (abs(mean(imf.candidate[[zcol]])) <= tol)
          stop.criterion <- abs(mean(imf.candidate[[zcol]]))

          # if (verbose) cat(paste("\t\t\tThe mean of the current IMF candidate is: ",
          #                        round(abs(mean(imf.candidate[[zcol]])),digits=4)," vs. ",tol,"\n",sep=""))

        }

        if (stoprule == "huang") {

          # Huang's criterion

          stop.sifting <- (sum((imf.candidate[[zcol]] - input.imf[[zcol]])^2/(input.imf[[zcol]])^2) < tol)
          stop.criterion <- sum((imf.candidate[[zcol]] - input.imf[[zcol]])^2/(input.imf[[zcol]])^2)

          # if (verbose) cat(paste("\t\t\tThe IMF candidate has the following Huang criterion: ",
          #                        sum((imf.candidate[[zcol]] - input.imf[[zcol]])^2/(input.imf[[zcol]])^2)," vs. ",tol,"\n",sep=""))

        }

        # Also checking if number of extrema is similar
        if (stoprule.extrema) {

          pct.min <- n.min/(n.min+n.max)
          stop.sifting <- stop.sifting && ((abs(pct.min - 0.5) <= diff.nb.extrema) || (abs(n.min-n.max) <= abs.nb.extrema))

          if (verbose) cat(paste0("\t\t\tExtrema ratio is: ", round(pct.min,digits = 4)," vs. 0.5 +- ", diff.nb.extrema, "\n"))

        }

        if (stop.sifting) {

          if (verbose) cat('\t\t\tOK, moving to next IMF\n')
          break

        } else {

          if (verbose) cat('\t\t\tSifting process continues.\n')
          input.imf[[zcol]] <- imf.candidate[[zcol]]

          # Keep result in memory in case it's the better after N sifting processes :
          if (stoprule != "mean.enveloppe") {
            if (stop.criterion < best.stop.criterion) {
              best.stop.criterion <- stop.criterion
              best.imf <- imf.candidate
              id.best.imf <- n.curr.sp
            }
          }
          else {
            if (stop.criterion > best.stop.criterion) best.imf <- imf.candidate
          }
        }

        if (n.curr.sp == n.sp.max) {
          if (verbose) cat('\t\tThe maximum number of sifting operations has been reached.\n')
          if (verbose) cat('\t\tOK, moving to next IMF\n')
          # Check if the current IMF is the best, if not we got the former best one in memory
          if (stoprule != "mean.enveloppe") {
            if (stop.criterion < best.stop.criterion) {
              best.imf <- imf.candidate
              id.best.imf <- n.curr.sp
            }
          }
          else {
            if (stop.criterion > best.stop.criterion) best.imf <- imf.candidate
          }
          imf.candidate <- best.imf
          if (verbose) cat(paste('\t\tThe best IMF was obtained on Sifting #',id.best.imf,'.\n',sep=''))

          rm(best.imf)
          break
        }

        n.curr.sp <- n.curr.sp + 1
      } else { # monotony

        if (verbose) cat('At least one of the enveloppes is monotone. This ends the decomposition.\n')
        if (verbose) cat(paste('Found',n.imf-1,'IMF.\n',sep=' '))
        if (change_coords_names) coordnames(result) <- old_coord_names

        return(result)
      }
    }

    # Creating output of the current IMF and Residual
    if (verbose) cat('\t\tUpdating IMF and residue for the next IMF...\n')
    result[["residue"]] <- dat.to.process[[zcol]] - imf.candidate[[zcol]]
    result[[paste("imf",n.imf,sep="")]] <- imf.candidate[[zcol]]
    if (n.imf == 1) {
      coordinates(result) <- ~x+y
      if (gridded.data) gridded(result) <- TRUE
    }
    dat.to.process[[zcol]] <- result[["residue"]]

    # For the next iteration
    n.min.prec <- n.min
    n.max.prec <- n.max
    n.imf <- n.imf + 1

    if (n.imf > n.imf.max) {
      #       if (verbose) cat('\t\tThe maximum number of IMF has been reached.\n\n')
      #       if (verbose) cat(paste('Found',n.imf-1,'IMF.\n',sep=' '))
      cat('\t\tThe maximum number of IMF has been reached.\n\n')
      cat(paste('Found',n.imf-1,'IMF.\n',sep=' '))
      if (change_coords_names) coordnames(result) <- old_coord_names
      return(result)
    }
  }

  return(result)
}
