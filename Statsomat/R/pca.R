#' pca
#' pca()
#' @export
pca <- function() {

  pca_pack <- c("FactoMineR", "FactoInvestigate", "factoextra", "rrcov", "methods", "parallel", "graphics", "imputeMissings", "onewaytests")

  for (i in 1:length(pca_pack)){
    requireNamespace(pca_pack[i], quietly = TRUE)
  }

  appDir <- system.file("shiny-apps", "PCA", package = "Statsomat")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing Statsomat.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}



