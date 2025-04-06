#' Background correction function
#'
#' This function allows you to base-line correct and edge-step normalize XANES spectra (background correction).
#' Raw spectra are loaded, then base-line corrected and edge-step normalized. The spectrum is flattened after E0.
#' The function returns the corrected, normalized and flattened spectrum
#' @param raw.spec Raw spectrum
#' @param corr.norm Vector of the base-line correction and edge-step normalization values (vector of length 4)
#' @keywords normalization, correction, flattening
#' @export
#' @importFrom stats lm predict
#' @examples
#' data(stdmix)
#' corr.spec.samples <- initial_load(specdat[5:8], 
#'   corr.norm = c(-36, -15, 37, 58))
#' corr.spec <- bkg_corr(raw.spec = corr.spec.samples[[1]], 
#'   corr.norm = c(-36, -15, 37, 58))
#' print(corr.spec)


bkg_corr <- function(raw.spec, corr.norm) {
  
  ## extract correction parameters needed furtheron
  pre.1 <- corr.norm[1]
  pre.2 <- corr.norm[2]
  post.1 <- corr.norm[3]
  post.2 <- corr.norm[4]
  
  ## extract E zero and sample data
  E.zero <- raw.spec$data$E0
  spectrum <- raw.spec$data$raw.spec
  
  ## create linear model of the energy (x) and the absorption (y) from the given pre-edge positions
  pre.pos.1 <- which(abs(spectrum[[c("energy")]]-(E.zero+pre.1)) == min(abs(spectrum[[c("energy")]]-(E.zero+pre.1))))
  pre.pos.2 <- which(abs(spectrum[[c("energy")]]-(E.zero+pre.2)) == min(abs(spectrum[[c("energy")]]-(E.zero+pre.2))))
  pre.x <- spectrum[[c("energy")]][pre.pos.1:pre.pos.2]
  pre.y <- spectrum[[c("raw.absorption")]][pre.pos.1:pre.pos.2]
  pre.fit <- lm(pre.y ~ pre.x)
  
  ## create linear model of the energy (x) and the absorption (y) from the given post-edge positions
  post.pos.1 <- which(abs(spectrum[[c("energy")]]-(E.zero+post.1)) == min(abs(spectrum[[c("energy")]]-(E.zero+post.1))))
  post.pos.2 <- which(abs(spectrum[[c("energy")]]-(E.zero+post.2)) == min(abs(spectrum[[c("energy")]]-(E.zero+post.2))))
  post.x <- spectrum[[c("energy")]][post.pos.1:post.pos.2]
  post.y <- spectrum[[c("raw.absorption")]][post.pos.1:post.pos.2]
  post.fit <- lm(post.y ~ post.x)
  
  ## calculate edge step parameter (predict linear models to E zero and divide pre from post)
  pre.pred <- predict(pre.fit, data.frame(pre.x = E.zero))
  post.pred <- predict(post.fit, data.frame(post.x = E.zero))
  edge.step <- post.pred-pre.pred
  
  ## predict full base-line to perform base-line correction
  full.pred.pre <- predict(pre.fit, data.frame(pre.x = spectrum[[c("energy")]]))
  base.line.corr <- spectrum[[c("raw.absorption")]]-full.pred.pre
  
  ## find position of E zero in the data frame
  pos.E.zero <- which(abs(spectrum[[c("energy")]]-E.zero) == min(abs(spectrum[[c("energy")]]-E.zero)))
  
  ## establish normalized spectrum
  norm.spec <- base.line.corr/edge.step
  norm.spec <- cbind(spectrum[[c("energy")]], norm.spec)
  norm.spec <- as.data.frame(norm.spec)
  colnames(norm.spec) <- c("energy", "cor.absorption")
  
  ## flattening algorithm (create linear models of the pre- and post-edge regions and predict after E zero until the end)
  flat.pre.x <- norm.spec[[c("energy")]][pre.pos.1:pre.pos.2]
  flat.pre.y <- norm.spec[[c("cor.absorption")]][pre.pos.1:pre.pos.2]
  flat.pre.fit <- lm(flat.pre.y ~ flat.pre.x)
  flat.post.x <- norm.spec[[c("energy")]][post.pos.1:post.pos.2]
  flat.post.y <- norm.spec[[c("cor.absorption")]][post.pos.1:post.pos.2]
  flat.post.fit <- lm(flat.post.y ~ flat.post.x)
  
  flat.pred.pre <- predict(flat.pre.fit, data.frame(flat.pre.x = norm.spec[[c("energy")]][(pos.E.zero+1):length(norm.spec[[c("energy")]])]))
  flat.pred.post <- predict(flat.post.fit, data.frame(flat.post.x = norm.spec[[c("energy")]][(pos.E.zero+1):length(norm.spec[[c("energy")]])]))
  
  ## actual flattening, devide the post-prediction from the pre-prediction to flatten 1
  diff.flattening <- flat.pred.pre-flat.pred.post+1
  
  ## create a data frame with base-line corrected, edge-step normalized and flattened spectrum
  flat.corr <- norm.spec[[c("cor.absorption")]][(pos.E.zero+1):length(norm.spec[[c("energy")]])]+diff.flattening
  flat.spec <- norm.spec
  flat.spec[[c("cor.absorption")]][(pos.E.zero+1):length(flat.spec[[c("energy")]])] <- flat.corr
  
  ## return list of initial and corrected spectra
  return(flat.spec)
  
  ## close function
}
