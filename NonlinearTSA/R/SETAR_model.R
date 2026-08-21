#' SETAR model estimation
#'
#' This function allows you to estimate SETAR model
#'
#' @param y series name,
#' @param delay_order Delay order,
#' @param lag_length lag length
#' @param trim_value trimmed value, .15, .10, .5
#' @return "Model" Estimated model
#' @return "threshold" the value of threshold
#' @keywords nonlinear model estimation
#' @references
#'
#' Burak Guris, R Uygulamalı Dogrusal Olmayan Zaman Serileri Analizi, DER Yayinevi, 2020.
#' @export
#' @importFrom stats embed lm resid
#'
#' @examples
#'
#'\donttest{
#'x <- rnorm(100)
#'SETAR_model(x, 1, 12, .15)
#'
#'
#'data(IBM)
#'SETAR_model(IBM, 1, 12, .05)
#'}
#'
#'

SETAR_model <- function(y, delay_order, lag_length, trim_value){

  mat = embed(y,(lag_length+1))
  thres = sort(y)
  gy = mat[,2]
  sayi = round(length(thres)*trim_value)
  trim_thres = thres[(sayi+1):(length(thres)-sayi)]
  sabit = rep(1,dim(mat)[1])
  den = matrix(0 ,nrow = dim(mat)[1], ncol = length(trim_thres))
  for(i in 1:length(trim_thres)){
    for(ii in 1:dim(mat)[1]){
      if(gy[ii] < trim_thres[i]){
        den[ii,i] = 1
      } else {
        den[ii,i] = 0
      }
    }
  }

  var_SSR = NULL
  for(iii in 1:ncol(den)){
    model <- lm(mat[,1]~I(sabit*den[,iii])+I(mat[,2:ncol(mat)]*den[,iii])+I(sabit*(1-den[,iii]))+I(mat[,2:ncol(mat)]*(1-den[,iii]))-1)
    SSR = sum((resid(model))^2)
    var_SSR[iii] = SSR/length(gy)
  }
  son = which.min(var_SSR)
  threshold = trim_thres[son]
  Constant1 = (sabit*den[,son])
  Regime1_ = (mat[,2:ncol(mat)]*den[,son])
  Constant2 = (sabit*(1-den[,son]))
  Regime2_ = (mat[,2:ncol(mat)]*(1-den[,son]))
  model_son = lm(mat[,1]~Constant1+Regime1_+Constant2+Regime2_-1)
  kontrol = sum((resid(model_son))^2)/length(gy)
  my_list <- list("model"=summary(model_son),"threshold"=threshold)
  return(my_list)
}

