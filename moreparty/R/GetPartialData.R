# @importFrom 

#' @export

GetPartialData <- function(object, xnames=NULL, ice = FALSE, center = FALSE,
                           grid.resolution = NULL, quantiles = TRUE, probs = 1:9/10,
                           trim.outliers = FALSE, which.class = 1L, prob = TRUE,
                           pred.fun = NULL, parallel = FALSE, paropts = NULL) {
  input = object@data@get("input")
  if(is.null(xnames)) xnames = colnames(input)
  pdep <- lapply(xnames, function(x) pdp::partial(object, x, pred.fun = pred.fun,
                                                  grid.resolution = grid.resolution, ice = ice, center = center,
                                                  quantiles = quantiles, probs = probs, trim.outliers = trim.outliers,
                                                  which.class = which.class, prob = prob,
                                                  parallel = parallel, paropts = paropts))
  for(i in 1:length(pdep)) {
    pdep[[i]]$var <- rep(names(pdep[[i]])[1], nrow(pdep[[i]]))
    names(pdep[[i]])[1] <- 'cat'
    pdep[[i]]$cat <- as.character(pdep[[i]]$cat)
  }
  pdep <- do.call('rbind.data.frame', pdep)
  names(pdep)[2] <- 'value'
  pdep$var <- factor(pdep$var)
  pdep <- pdep[,c('var','cat','value')]
  return(pdep)
}