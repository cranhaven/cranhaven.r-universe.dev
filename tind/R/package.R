#
# This file is a part of tind.
#
# Copyright (c) Grzegorz Klima 2025
#
# ############ #
# tind package #
# ############ #


#' A Common Representation of Time Indices of Different Types
#'
#' @description
#' The goal of \pkg{tind} project is to provide users with a \emph{single}
#' class capable of representing time indices of different types.
#' Currently, these include: years, quarters, months, ISO 8601 weeks, dates,
#' time of day, date-time, and arbitrary integer/numeric indices.
#' \pkg{tind} provides a unified interface and a collection of methods, which
#' can be used with all supported index types.
#'
#' The package provides an extensive collection of functions for calendrical
#' computations (including business applications), index conversions,
#' index parsing, and other operations. Examples of use of \pkg{tind}
#' functions and methods can be found in their documentation and in package vignettes.
#'
#' All routines have been optimised for speed in order to facilitate computations
#' on large datasets.
#'
#' @details
#' \strong{Options Controlling \pkg{tind} Behaviour}
#'
#' \code{tind.abbr.year.start} is a number in range 0--99 determining how
#' two-digit years are interpreted during parsing. By default (69), two-digit
#' numbers smaller than 69 are interpreted as years in 2000s and equal or greater
#' than 69 as years in 1900s.
#'
#' \code{tind.warn.diff.tz} is a logical value (\code{TRUE} by default)
#' determining whether warnings should be raised when performing computations
#' involving date-time indices with different time zone attributes.
#'
#' \strong{Implementation}
#'
#' The code structure is hierarchical by design. The first layer consists
#' of low-level C and R functions working with particular time index types
#' and converting between them. The second layer consists of exported
#' (user-accessible) functions, which are responsible for argument checks
#' and dispatching to the 1st layer functions.
#'
#' Computations with \pkg{tind} for types other than date-time are
#' implemented in C from scratch. Many algorithms were taken
#' from \emph{Calendar FAQ} by Claus Tøndering. For date-time computations,
#' \pkg{tind} partially relies on base R infrastructure (\code{\link[base]{POSIXlt}}
#' class), that is on conversion \code{as.POSIXlt.numeric}.
#'
#' @references
#' Claus Tøndering, \emph{Calendar FAQ}, \url{https://www.tondering.dk/claus/calendar.html}.
#'
#' @name tind-package
#'
#' @author Grzegorz Klima <dever@@post.pl>
#'
#' @keywords package
#'
#' @rawNamespace useDynLib(tind, .registration = TRUE, .fixes = "C_")
#'
NULL


# tind package options and startup
# ###################################################################

.tind.options <- list(
    abbr.year.start =
        list(ok = function(x) (is.numeric(x) && (length(x) == 1L) &&
                              (x %in% 0L:99L)),
             default = 69L),
    warn.diff.tz =
        list(ok = function(x) .isTRUEFALSE(x),
             default = TRUE)
)


.tind.getOption <- function(x)
{
    xx <- paste0("tind.", x)
    opt <- getOption(xx)
    if (is.null(opt)) return (.tind.options[[x]]$default)
    if (.tind.options[[x]]$ok(opt)) return (opt)
    opt <- .tind.options[[x]]$default
    arglist <- list(opt)
    names(arglist) <- xx
    do.call("options", arglist)
    mes0 <- gettextf("invalid value of option %s", dQuote(xx))
    mes1 <- gettextf("using default settings")
    warning(paste0(mes0, "; ", mes1, " (", opt, ")"), call. = FALSE, domain = NA)
    return (opt)
}


.onLoad <- function(libname, pkgname)
{
    args <- lapply(.tind.options, function(x) x$default)
    names(args) <- paste0("tind.", names(.tind.options))
    do.call(options, args)
}


.onUnload <- function(libpath)
{
    args <- rep_len(list(NULL), length(.tind.options))
    names(args) <- paste0("tind.", names(.tind.options))
    do.call(options, args)
}

