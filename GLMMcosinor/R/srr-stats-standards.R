#' srr_stats
#'
#' All of the following standards initially have `@srrstatsTODO` tags.
#' These may be moved at any time to any other locations in your code.
#' Once addressed, please modify the tag from `@srrstatsTODO` to `@srrstats`,
#' or `@srrstatsNA`, ensuring that references to every one of the following
#' standards remain somewhere within your code.
#' (These comments may be deleted at any time.)
#'
#' @srrstatsVerbose TRUE
#'


#' @srrstats {G1.0}
#' @srrstats {G1.1}
#' @srrstats {G1.2}
#' @srrstats {G1.3}
#' @srrstats {G1.4}
#' @srrstats {G1.4a}
#' @srrstats {G2.0}
#' @srrstats {G2.0a}
#' @srrstats {G2.1}
#' @srrstats {G2.1a}
#' @srrstats {G2.2}
#' @srrstats {G2.3}
#' @srrstats {G2.3a}
#' @srrstats {G2.3b}
#' @srrstats {G2.4}
#' @srrstats {G2.4a}
#' @srrstats {G2.4b}
#' @srrstats {G2.4c}
#' @srrstats {G2.4d}
#' @srrstats {G2.5}
#' @srrstats {G2.6}
#' @srrstats {G2.7}
#' @srrstats {G2.8}
#' @srrstats {G2.9}
#' @srrstats {G2.10}
#' @srrstats {G2.11}
#' @srrstats {G2.12}
#' @srrstats {G2.13}
#' @srrstats {G2.14}
#' @srrstats {G2.14a}
#' @srrstats {G2.14b}
#' @srrstats {G2.14c}
#' @srrstats {G2.15}
#' @srrstats {G2.16}
#' @srrstats {G3.0}
#' @srrstats {G5.0}
#' @srrstats {G5.1}
#' @srrstats {G5.2}
#' @srrstats {G5.2a}
#' @srrstats {G5.2b}
#' @srrstats {G5.4}

#' @srrstats {RE1.0}
#' @srrstats {RE1.1}
#' @srrstats {RE1.2}
#' @srrstats {RE1.3}
#' @srrstats {RE1.3a}
#' @srrstats {RE1.4}
#' @srrstats {RE2.0}
#' @srrstats {RE2.1}
#' @srrstats {RE2.2}
#' @srrstats {RE3.0}
#' @srrstats {RE3.1}
#' @srrstats {RE4.0}
#' @srrstats {RE4.2}
#' @srrstats {RE4.4}
#' @srrstats {RE4.9}
#' @srrstats {RE4.8}
#' @srrstats {RE4.10}
#' @srrstats {RE4.11}
#' @srrstats {RE4.12}
#' @srrstats {RE4.17}
#' @srrstats {RE4.18}
#' @srrstats {RE6.0}
#' @srrstats {RE6.1}
#' @srrstats {RE6.2}
#' @srrstats {RE6.3}
#' @srrstats {RE7.2}
#' @srrstats {RE7.3}
#' @srrstats {G5.4a}
#' @srrstats {G5.6}
#' @srrstats {G5.6a}
#' @srrstats {G5.6b}
#' @srrstats {G5.9b}
#' @noRd
NULL

#' NA_standards
#'
#' No performance claims are being made in this package:
#' @srrstatsNA {G1.5}
#' @srrstatsNA {G1.6}
#'
#' No instance of converstion from a factor:
#' @srrstatsNA {G2.4e}
#'
# glmmTMB:::vcov.glmmTMB() does not offer any options for alternate algorithms
# to calculate the variance-covariance matrix:
#' @srrstatsNA {G3.1a}
#' @srrstatsNA {G3.1}
#'
#' The package does not enable outputs to be written to local files
#' @srrstatsNA {G4.0}
#'
#' There are currently no extended tests in this package
#' @srrstatsNA {G5.10}
#' @srrstatsNA {G5.11}
#' @srrstatsNA {G5.11a}
#' @srrstatsNA {G5.12}
#'
#' There are no such functions in our package, although there may be in
#' the glmmTMB package
#' @srrstatsNA {G5.3}
#'
# There outputs that will be featured in the paper have not been chosen yet,
# although they will most likely be accessible through a seeded
# simulate_cosinor output
#' @srrstatsNA {G5.4c}
#' There are currently no algorithm performance tests in this package
#' @srrstatsNA {G5.7}
#'
#' The following standards are not directly met in our package, but may be met
#' in the glmmTMB package
#' @srrstatsNA {G5.8}
#' @srrstatsNA {G5.8a}
#' @srrstatsNA {G5.8b}
#' @srrstatsNA {G5.8c}
#' @srrstatsNA {G5.8d}
#' @srrstatsNA {G5.9}
#' @srrstatsNA {G5.9a}

#' The following regression standards are not directly applicable to our
#' package, although they may be applicable within the glmmTMB package
#' @srrstatsNA {RE2.3}
#' @srrstatsNA {RE2.4}
#' @srrstatsNA {RE2.4a}
#' @srrstatsNA {RE2.4b}
#' @srrstatsNA {RE3.2}
#' @srrstatsNA {RE3.3}
#' @srrstatsNA {RE4.1}
#' @srrstatsNA {RE4.3}
#' @srrstatsNA {RE4.5}
#' @srrstatsNA {RE4.6}
#' @srrstatsNA {RE4.7}
#' @srrstatsNA {RE4.14}
#' @srrstatsNA {RE4.15}
#' @srrstatsNA {RE4.16}
#' @srrstatsNA {RE5.0}
#' @srrstatsNA {RE7.0}
#' @srrstatsNA {RE7.0a}
#' @srrstatsNA {RE7.1}
#' @srrstatsNA {RE7.1a}
#'
#' GLMMcosinor does not make forecasts
#' @srrstatsNA {RE7.4}
#'
#' Any non-applicable standards can have their tags changed from `@srrstatsTODO`
#' to `@srrstatsNA`, and placed together in this block, along with explanations
#' for why each of these standards have been deemed not applicable.
#' (These comments may also be deleted at any time.)
#' @noRd
NULL
