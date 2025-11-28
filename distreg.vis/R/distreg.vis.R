#' distreg.vis: Interactively visualizing distributional regression models
#'
#' The package \code{distreg.vis} is a framework for the visualization of
#' distributional regression models estimated with the R packages \code{bamlss},
#' \code{gamlss} and \code{betareg}. Current supported model classes can be
#' found under \link{distreg_checker}.
#'
#' @details The main functions are:
#'
#' \itemize{ \item \code{vis()}: Starts the Graphical User Interface. \item
#' \code{moments()}: Obtain predicted moments of the target distribution based
#' on user-specified values of the explanatory variables. \item
#' \code{plot_dist()}: Create a graph displaying the predicted probability
#' density function or cumulative density function based on the same
#' user-specified values. \item \code{plot_moments()}: View the marginal
#' influence of a selected effect on the predicted moments of the target
#' distribution.}
#'
#' To get a feel for the main capabilities of \code{distreg.vis}, you can run
#' the examples or the demo called \code{vis-demo.R} which fits a couple of
#' distributional regression models and then calls the Graphical User Interface.
#'
#' For the main functions, certain target distributions from both \code{bamlss}
#' and \code{gamlss} are supported. Check the \code{distreg.vis::dists} dataset
#' to find out which distributions are supported for \code{plot_dist()} (column
#' \code{implemented}) and which are also supported for \code{plot_moments()}
#' (column \code{moment_funs}).
#'
#' To make the process of interpreting fitted distributional regression models
#' as easy as possible, \code{distreg.vis} features a rich Graphical User
#' Interface (GUI) built on the \code{shiny} framework. Using this GUI, the user
#' can (a) obtain an overview of the selected model fit, (b) easily select
#' explanatory values for which to display the predicted distributions, (c)
#' obtain marginal influences of selected covariates and (d) change aesthetical
#' components of each displayed graph. After a successful analysis, the user can
#' quickly obtain the R code needed to reproduce all displayed plots, without
#' having to start the application again.
#'
#' Maintainer: \itemize{ \item Stanislaus Stadlmann,
#' \email{stadlmann@@uni-goettingen.de} }
#' @docType package
#' @name distreg.vis
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
