#' Interference colors for polarized light microscopy
#'
#' Based on a 2013 paper by Sorensen, this package  
#' automates computation and plotting of interference
#' colors produced when a transparent material  
#' is observed between crossed polarizer and analyzer. 
#' Two kinds of plots may be produced: 
#' the classical 
#' Michel-Levy chart (parralel color boundaries and 
#' oblique iso-birefringence 
#' lines in a rectangular plot with thickness and retardation axes) and the 
#' more recent Raith-Sorensen chart (hyperbolic color boundaries in a
#' thickness-birefringence rectangular plot).
#' Functions for automatic extraction of birefringence profiles from images,
#' and their spatial or statistical interpretation 
#' are still under development.
#' 
#' Using the interfr package, users can compute and display their own
#' interference chart, customizing sample thickness and birefringence ranges. 
#' This may be of special interest when dealing
#' with thick samples (that exceed the 20-40 micrometers range
#' of classical thin plates).
#' It makes use of the colorSpec package for physically realistic 
#' representation of colors.
#' A function is provided to illustrate the color shifts given by the use of
#' quarter and lambda plates.
#'
#' @references
#' Sorensen, B.E. (2013) A revised Michel-Levy interference colour chart based
#' on first-principles calculations. Eur. J. Mineral., 2013, 25, 5-10.
#' DOI:10.1127/0935-1221/2013/0025-2252 
#'
#' @docType package
#' @name interfr-package
#' @aliases interfr package-interfr
NULL

