#'Terasvirta (1994) nonlinearity test
#'
#' This function allows you to make Terasvirta (1994) nonlinearity test
#' @param x series name,
#' @param d delay parameter,
#' @param maxp maximum p
#' @return "Linearity" the value of the test statistic and the probability of the test statistic
#' @return "H01" the value of the test statistic and the probability of the test statistic
#' @return "H02" the value of the test statistic and the probability of the test statistic
#' @return "H03" the value of the test statistic and the probability of the test statistic
#' @return "H12" the value of the test statistic and the probability of the test statistic
#' @references
#' Teräsvirta, T. (1994). Specification, estimation, and evaluation of smooth transition autoregressive models. Journal of the american Statistical association, 89(425), 208-218.
#'
#'
#' Burak Guris, R Uygulamalı Dogrusal Olmayan Zaman Serileri Analizi, DER Yayinevi, 2020.
#'
#' @keywords nonlinearity test
#' @export
#' @importFrom car linearHypothesis
#' @importFrom stats AIC residuals pf embed lm residuals
#' @examples
#'
#' x <- rnorm(1000)
#' Terasvirta1994test(x, 3, 4)
#'
#'
#' data(IBM)
#' Terasvirta1994test(IBM, 4, 4)
#'
#'

Terasvirta1994test <- function(x,d,maxp){
  if(d>maxp){
    stop("d must be less than maxp")
  }
    maxp = maxp+1
  xx = embed(x,maxp)

  AICs = NULL
  for (p in 2:maxp){
    xx = embed(x,p)
    model = lm(xx[,1]~xx[,2:p])
    AICs[p-1] = AIC(model)
  }
  uygunp = which.min(AICs)
  xx = embed(x,(uygunp+1))
  newy = xx[,1]
  newx = xx[,2:(uygunp+1)]
  ytd = xx[,(d+1)]
  ytd2 = xx[,d+1]^2
  ytd3 = xx[,d+1]^3
  a = newx*ytd
  b = newx*ytd2
  c = newx*ytd3
  newx = as.matrix(newx)
  a = as.matrix(a)
  b = as.matrix(b)
  c = as.matrix(c)
  modelesass<-lm(newy~newx+a+b+c)

  m1u = lm(newy~newx+a+b+c)
  SSEu = sum((residuals(m1u))^2)
  m1r = lm(newy~newx)
  SSEr = sum((residuals(m1r))^2)
  h1 = dim(a)[2] + dim(b)[2] + dim(c)[2]
  h2 = length(newy)[1]- (dim(newx)[2] + dim(a)[2] + dim(b)[2] + dim(c)[2])
  Linearity = ((SSEr-SSEu)/h1) / (SSEu/h2)
  pvalueLinearity = pf(Linearity, h1, h2, lower.tail = FALSE)


  m2u = lm(newy~newx+a)
  SSEu = sum((residuals(m2u))^2)
  m2r = lm(newy~newx)
  SSEr = sum((residuals(m2r))^2)
  h1 = dim(a)[2]
  h2 = length(newy)[1]- (dim(newx)[2] + dim(a)[2] )
  H01 = ((SSEr-SSEu)/h1) / (SSEu/h2)
  pvalueH01 = pf(H01, h1, h2, lower.tail = FALSE)


  m3u = lm(newy~newx+a+b)
  SSEu = sum((residuals(m3u))^2)
  m3r = lm(newy~newx+a)
  SSEr = sum((residuals(m3r))^2)
  h1 = dim(b)[2]
  h2 = length(newy)[1]- (dim(newx)[2] + dim(a)[2] +dim(b)[2] )
  H02 = ((SSEr-SSEu)/h1) / (SSEu/h2)
  pvalueH02 = pf(H02, h1, h2, lower.tail = FALSE)

  m4u = lm(newy~newx+a+b+c)
  SSEu = sum((residuals(m4u))^2)
  m4r = lm(newy~newx+a+b)
  SSEr = sum((residuals(m4r))^2)
  h1 = dim(c)[2]
  h2 = length(newy)[1]- (dim(newx)[2] + dim(a)[2] +dim(b)[2] + dim(c)[2] )
  H03 = ((SSEr-SSEu)/h1) / (SSEu/h2)
  pvalueH03 = pf(H03, h1, h2, lower.tail = FALSE)

  m5u = lm(newy~newx+a+b)
  SSEu = sum((residuals(m5u))^2)
  m5r = lm(newy~newx)
  SSEr = sum((residuals(m5r))^2)
  h1 = dim(a)[2] + dim(b)[2]
  h2 = length(newy)[1]- (dim(newx)[2] + dim(a)[2] +dim(b)[2] )
  H12 = ((SSEr-SSEu)/h1) / (SSEu/h2)
  pvalueH12 = pf(H12, h1, h2, lower.tail = FALSE)


  my_list <- list("Linearity"=c(Linearity,pvalueLinearity),"H01"=c(H01,pvalueH01),"H02"=c(H02,pvalueH02),"H03"=c(H03,pvalueH03),"H12"=c(H12,pvalueH12))

  return(my_list)
  }




