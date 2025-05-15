#' Voronoi Plot SVM
#'
#' @param datos a data.frame object.
#' @param varpred variable to predict.
#' @param vars predictor variables.
#' @param kernel the kernel used in training and predicting.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return plot
#' @export voronoi_svm_plot
#' @importFrom traineR train.svm
#' @examples
#' voronoi_svm_plot(iris, "Species", c("Sepal.Length", "Sepal.Width"), "linear")
#' 
voronoi_svm_plot <- function(datos, varpred, vars, kernel = "linear") {
  if (length(vars) == 2) {
    f  <- paste0(varpred, "~", paste(vars, collapse = "+"))
    f2 <- paste(vars, collapse = "~")
    modelo <- train.svm(as.formula(f), data = datos, kernel = kernel) 
    slices <- lapply(1:(ncol(datos) - 1), function(i) i)
    names(slices) <- colnames(datos[, -which(colnames(datos) == varpred)])
    plot(modelo, datos, as.formula(f2), slice = slices)
  } else {
    return(NULL)
  }
}