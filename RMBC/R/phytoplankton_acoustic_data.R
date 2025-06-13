#' Phytoplankton_acoustic_data
#' 
#' Data obtained by taking laboratory measurements of ultrasonic acoustic signals: 
#' a pulse is emitted by a transducer, this pulse interacts with phytoplankton 
#' suspended in the water and produces an acoustic dispersion (scattering), 
#' which is recorded by an electronic acquisition device.
#' A filtering process of the signal is performed in a first stage. 
#' Portions of the signal belong o one of the two main cases:
#' \itemize{
#' \item{(a) Signals corresponding to the acoustic response of phytoplankton}
#' \item{(b) Signals corresponding to spurious dispersers, such as bubbles or 
#' particles in suspension, whose intensity is greater than in case (a).} 
#' }
#' To classify a signal in one of these two groups biologists create a vector 
#' (X1, X2) defined as follows:
#' \itemize{
#' \item{ X1 = ratio of filtered to non-filtered signal power}
#' \item{ X2 = filtered signal power expressed in dB.}
#' }
#' The available data consists of 375 such measurements. These data is particularly 
#' useful to compare robust procedures because 20% of these measurements are known
#' to be outliers produced by a communication failure between the electronic device 
#' (digital oscilloscope) and the software for acquiring the acoustic signal. 
#' This failure occurs once every 5 microseconds, which allows the scientists 
#' to identify the outliers. The outliers appear 
#' as a separated group in the region X1 < 0.5 and X2 > 20.
#' 
#' @format a list of length 2, where its elements are  
#' \itemize{
#' \item \code{Y}: A matrix of dimension 375 x 2, each row contains X1 and X2 values
#' \item \code{outliers_index}: An array with the outliers index-locations 
#'}
#'
#'@examples
#' ################################
#' # upload matrix ################
#' ################################
#' 
#' Y <- phytoplankton_acoustic_data$Y
#' 
#' outliers_index <- phytoplankton_acoustic_data$outliers_index
#' 
#' Yclean=Y[-outliers_index,]
#' 
#' trueOutliers=Y[outliers_index,]
#' 
#' ################################
#' # plot results ################
#' ################################
#' 
#' plot(Y, main = "Phytoplankton acoustic data", cex.main = 3, lwd = 1,pch = 19, cex = 1, 
#'      type = "n", xlab = "x1", ylab = "x2",  xlim = c(0,1.1), ylim = c(0,43)
#'      )
#'      
#' points(trueOutliers,lwd=2,cex=1,pch=4)
#' 
#' points(Yclean,col=1,lwd=1.5,pch=21, bg=4, cex=1)
#'
#' @references 
#'  \itemize{
#'  \item{[1] Cinquini, M., Bos, P., Prario, I and Blanc, S. (2016), “Advances
#'  on modelling, simulation  and signal processing of ultrasonic scattering 
#'  responses from phytoplankton cultures,” in 
#'  Proceedings of Meetings on Acoustics 22ICA, 28, American Society of Acoustics.}
#'  \item{[2]
#'   Gonzalez J.D, Maronna R., Yohai V., & and Zamar . (2021). Robust Model-Based Clustering.  
#'   arXiv preprint <https://arxiv.org/abs/2102.06851>}
#'  }
#' 
"phytoplankton_acoustic_data"


