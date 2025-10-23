### Raw data used to build exTropicalMoth.RData
# rawDevEggs <- matrix(c(10, 0.031, 10, 0.039, 13, 0.072, 15, 0.047, 15, 0.059, 15.5, 0.066,
#                        16, 0.083, 16, 0.100, 17, 0.100, 20, 0.100, 20, 0.143, 25, 0.171,
#                        25, 0.200, 30, 0.200, 30, 0.180, 35, 0.001), ncol = 2, byrow = TRUE)
# rawDevLarva <- matrix(c(10, 0.010, 10, 0.014, 10, 0.019, 13, 0.034, 15, 0.024,
#                         15.5, 0.029, 15.5, 0.034, 15.5, 0.039, 17, 0.067, 20, 0.050, 25, 0.076,
#                         25, 0.056, 30, 0.0003, 35, 0.0002), ncol = 2, byrow = TRUE)
# rawDevPupa <- matrix(c(10, 0.001, 10, 0.008, 10, 0.012, 13, 0.044, 15, 0.017,
#                        15, 0.044, 15.5, 0.039, 15.5, 0.037, 16, 0.034, 16, 0.051, 17, 0.051,
#                        20, 0.080, 20, 0.092, 25, 0.102, 25, 0.073, 30, 0.005,
#                        35, 0.0002), ncol = 2, byrow = TRUE)
# exTropicalMothRaw <- list(eggs = rawDevEggs, larva = rawDevLarva, pupa = rawDevPupa)
# m1 <- devRateModel(eq = taylor_81, temp = exTropicalMothRaw[[1]][,1], devRate = exTropicalMothRaw[[1]][,2], startValues = list(Rm = 0.05, Tm = 30, To = 5))
# m2 <- devRateModel(eq = taylor_81, temp = exTropicalMothRaw[[2]][,1], devRate = exTropicalMothRaw[[2]][,2], startValues = list(Rm = 0.05, Tm = 25, To = 5))
# m3 <- devRateModel(eq = taylor_81, temp = exTropicalMothRaw[[3]][,1], devRate = exTropicalMothRaw[[3]][,2], startValues = list(Rm = 0.05, Tm = 30, To = 5))
# exTropicalMoth <- list(raw = exTropicalMothRaw, NLSmodel = list(eggs = m1, larva = m2, pupa = m3))
# save(exTropicalMoth, file = "./data/exTropicalMoth.RData")
### end

#' @title Tropical moth development rate at constant temperatures.
#'
#' @description This is a sample dataset to be used in the package examples. In this example,
#' we used data from Crespo-Perez et al. (2011) on the potato tuber moth Tecia
#' solanivora (Lepidoptera: Gelechiidae), a major crop pest in the central Andes
#' of Ecuador. We used Web Plot Digitizer (Rohatgi 2015) to extract the data on
#' development rate as a function of temperature.
#'
#' @description Crespo-Perez, V., Rebaudo, F., Silvain, J.-F. & Dangles, O. (2011).
#' Modeling invasive species spread in complex landscapes: the case of potato moth
#' in Ecuador. Landscape ecology, 26, 1447-1461.
#'
#' @description Rohatgi, A. (2015). WebPlotDigitalizer: HTML5 based online tool
#' to extract numerical data from plot images.
#'
#' @format A list of two elements with a list of three elements.
#' \describe{
#'   \item{raw}{The raw data extracted from Crespo-Perez et al. 2011.
#'     \describe{
#'       \item{eggs}{raw temperatures and development rates}
#'       \item{larva}{raw temperatures and development rates}
#'       \item{pupa}{raw temperatures and development rates}
#'     }
#'   }
#'   \item{model}{The nls object returned by the devRateModel function.
#'     \describe{
#'       \item{eggs}{nls object}
#'       \item{larva}{nls object}
#'       \item{pupa}{nls object}
#'     }
#'   }
#' }
#' @docType data
#' @keywords datasets
"exTropicalMoth"
