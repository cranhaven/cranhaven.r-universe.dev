#' @name ccrs-package
#' @docType package
#' @title Correcting and Clustering preference data in the presence of response style bias.
#' @description Corrects and clusters response-style-biased data.
#' @docType package
#' @author Mariko Takagishi
#' @references Takagishi, M., Velden, M. van de and Yadohisa, H. (2019). Clustering preference data in the presence of response style bias, to appear in British Journal of Mathematical and Statistical Psychology.
#' @keywords package
#' @importFrom cds createcdsdata ispline
#' @importFrom colorspace rainbow_hcl
#' @importFrom dplyr as_data_frame bind_rows
#' @importFrom graphics par matplot text lines plot
#' @importFrom limSolve lsei
#' @importFrom lsbclust  indarr
#' @importFrom msm rtnorm
#' @importFrom parallel parLapply makeCluster detectCores clusterExport stopCluster
#' @importFrom stats dist kmeans runif rnorm
#' @importFrom utils write.csv
NULL
