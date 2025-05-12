#' @title Dynamic Settings File
#' @description Any package (subsequently called 'target package') is enabled
#' to provide its users an easily accessible, user-friendly and human readable
#' text file where key=value pairs (used by functions defined in the target
#' package) can be saved. This settings file lives in a location defined by the
#' user of the target package, and its user-defined values remain unchanged even
#' when the author of the target package is introducing or deleting keys, or
#' when the target package is updated or re-installed. In order to enable the
#' target package to make use of the functionality offered by package 'uniset',
#' three files have to be exported by 'uniset' and placed into the target package.
#' @details There are two ways to generate the files required for the target
#' package to make use of 'uniset':
#' \describe{
#' \item{-) Export and move manually}{Use \code{\link{uniset_getFiles}}, then move
#' the 'xxx_settings.R' file ('xxx' for the name of the target package) into the
#' 'inst' folder (create one if not already done) of the target package.
#' Move the files 'uniset_globals.R' and 'uniset_functions.R' into the
#' 'R' folder of the target package.}
#' \item{-) Write directly to target package}{(Recommended) Use
#' \code{\link{uniset_copyFilesToPackage}} to copy the required files directly into
#' the target package.}
#' }
#' Every variable defined in the xxx_settings.R file is accessible in the code of
#' the target package. \cr
#' The target package has to list 'uniset' as an 'import', and then
#' \code{\link{uniset_updateSettings}} or \code{\link{uniset_autoUpS}} can be used
#' to manually or automatically update the settings,i.e. to read in the key=value
#' pairs stored in the xxx_settings.R file.
#' For an introduction and more detailed information please see
#' \url{https://bpollner.github.io/uniset/}.
#' @section Advantage: The most imminent advantage of the 'uniset' settings-file
#' system over using any static file for permanently storing settings for any
#' package is the fact that the key=value pairs in the xxx_settings.R
#' file get updated (added / deleted) dynamically. \cr
#' So the developer of a package can delete keys or introduce new ones, and
#' the new key=value pairs will be automatically added to or deleted from
#' the local xxx_settings.R file. \strong{Values changed by the user of the
#' target package will be preserved}. \cr
#' So the author of the target package can add or delete keys from the
#' xxx_settings.R file without worrying that this will cause any effort or
#' troubles for the user of the target package.
#' @section Links: Please see \url{https://bpollner.github.io/uniset/} for a
#' practical example; bug reports can be made at
#' \url{https://github.com/bpollner/uniset/issues}.
#' @author Bernhard Pollner, Zoltan Kovacs
#' @section Maintainer: Bernhard Pollner <bernhard.pollner@@mac.com>
#' @section Functions for preparing the target package:
#' \code{\link{uniset_copyFilesToPackage}}, \code{\link{uniset_getFiles}}
#' @section Functions to be called from within the target package:
#' \code{\link{uniset_autoUpS}}, \code{\link{uniset_updateSettings}},
#' \code{\link{uniset_test}},  \code{\link{uniset_getstn}},
#' and only once: \code{\link{uniset_setup}}.
#' @section Examples: As the functions to update the settings file and to
#' (automatically) source this settings are intended to be called from
#' \strong{within} the (installed) target package, please go to
#' \url{https://bpollner.github.io/uniset/} for a walk-through and for
#' a real-life demonstration and examples how to use these functions
#' in the code of the target package.
#' @importFrom utils str
#' @importFrom easycsv choose_dir
#' @importFrom methods is
#' @docType package
#' @name uniset
NULL
