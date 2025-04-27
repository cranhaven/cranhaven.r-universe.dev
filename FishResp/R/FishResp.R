#' FishResp: Analytical Tool for Aquatic Respirometry
#'
#' Calculates metabolic rate of fish and other aquatic organisms measured using an intermittent-flow respirometry approach. The tool is used to run a set of graphical QC tests of raw respirometry data, correct it for background respiration and chamber effect, filter and extract target values of absolute and mass-specific metabolic rate. Experimental design should include background respiration tests and measuring of one or more metabolic rate traits. The R package is ideally integrated with the pump controller 'PumpResp' and the DO meter 'SensResp' (open-source hardware by FishResp). Raw respirometry data can be also imported from 'AquaResp' (free software), 'AutoResp', ('LoligoSystems'), 'OxyView' ('PreSens'), 'Pyro Oxygen Logger' ('PyroScience'), and 'Q-box Aqua' ('QubitSystems').
#'
#' _PACKAGE
#' @name FishResp
#' @keywords internal
#' @author Sergey Morozov \email{sergey.morozov@@helsinki.fi} Scott McCairns \email{scott.mccairns@@inra.fr}
#'
#' @references {
#' Before using the R package 'FishResp', we recommend reading the publication describing this software and two keystone reviews devoted to metabolic rate measurements of fish using an intermittent-flow approach:
#' \enumerate{
#'   \item Morozov, S., McCairns, R. J. S., Merila, J. (2019). FishResp: R package and GUI application for analysis of aquatic respirometry data. Conservation Physiology, 7(1), coz003.
#'   \item Clark, T. D., Sandblom, E., Jutfelt, F. (2013). Aerobic scope measurements of fishes in an era of climate change: respirometry, relevance and recommendations. Journal of Experimental Biology, 216(15), 2771-2782.
#'   \item Svendsen, M. B. S., Bushnell, P. G., Steffensen, J. F. (2016). Design and setup of intermittent-flow respirometry system for aquatic organisms. Journal of Fish Biology, 88(1), 26-50.
#'   }
#' }
#'
NULL
