#' Analysis: Piecewise regression
#'
#' Fit a degree 1 spline with 1 knot point where the location of the knot point is unknown.
#' @param trat Numerical or complex vector with treatments
#' @param resp Numerical vector containing the response of the experiment.
#' @param middle	A scalar in [0,1]. This represents the range that the change-point can occur in. 0 means the change-point must occur at the middle of the range of x-values. 1 means that the change-point can occur anywhere along the range of the x-values.
#' @param CI Whether or not a bootstrap confidence interval should be calculated. Defaults to FALSE because the interval takes a non-trivial amount of time to calculate
#' @param bootstrap.samples	 The number of bootstrap samples to take when calculating the CI.
#' @param sig.level	What significance level to use for the confidence intervals.
#' @param error Error bar (It can be SE - \emph{default}, SD or FALSE)
#' @param ylab Variable response name (Accepts the \emph{expression}() function)
#' @param xlab treatments name (Accepts the \emph{expression}() function)
#' @param theme ggplot2 theme (\emph{default} is theme_classic())
#' @param legend.position legend position (\emph{default} is c(0.3,0.8))
#' @param cardinal defines the value of y considered extreme (\emph{default} considers 0 germination)
#' @param width.bar bar width
#' @param textsize Font size
#' @param pointsize shape size
#' @param linesize line size
#' @param pointshape format point (\emph{default} is 21)
#' @param font.family Font family (\emph{default} is sans)
#' @note if the maximum predicted value is equal to the maximum x, the curve does not have a maximum point within the studied range. If the minimum value is less than the lowest point studied, disregard the value.
#' @return
#'   \describe{
#'   \item{\code{Coefficients}}{Coefficients and their p values}
#'   \item{\code{Optimum temperature}}{Optimum temperature (equivalent to the maximum point)}
#'   \item{\code{Optimum temperature response}}{Response at the optimal temperature (equivalent to the maximum point)}
#'   \item{\code{Minimal temperature}}{Temperature that has the lowest response}
#'   \item{\code{Minimal temperature response}}{Lowest predicted response}
#'   \item{\code{Predicted maximum basal value}}{Lower basal limit temperature based on the value set by the user (default is 0)}
#'   \item{\code{Predicted minimum basal value}}{Upper basal limit temperature based on the value set by the user (default is 0)}
#'   \item{\code{AIC}}{Akaike information criterion}
#'   \item{\code{BIC}}{Bayesian Inference Criterion}
#'   \item{\code{r-squared}}{Determination coefficient}
#'   \item{\code{RMSE}}{Root mean square error}
#'   \item{\code{grafico}}{Graph in ggplot2 with equation}
#'   }
#' @export
#' @author Model imported from the SiZer package
#' @author Gabriel Danilo Shimizu
#' @author Leandro Simoes Azeredo Goncalves
#' @references Chiu, G. S., R. Lockhart, and R. Routledge. 2006. Bent-cable regression theory and applications. Journal of the American Statistical Association 101:542-553.
#' @references Toms, J. D., and M. L. Lesperance. 2003. Piecewise regression: a tool for identifying ecological thresholds. Ecology 84:2034-2041.
#' @examples
#' library(seedreg)
#' data("aristolochia")
#' attach(aristolochia)
#'
#' #================================
#' # Germination
#' #================================
#' piecewise_model(trat,germ)
#'
#' #================================
#' # Germination speed
#' #================================
#' piecewise_model(trat, vel, ylab=expression("v"~(dias^-1)))


piecewise_model=function (trat,
                          resp,
                          middle = 1,
                          CI = FALSE,
                          bootstrap.samples = 1000,
                          sig.level = 0.05,
                          error="SE",
                          ylab="Germination (%)",
                          xlab=expression("Temperature ("^"o"*"C)"),
                          theme=theme_classic(),
                          cardinal=0,
                          width.bar=NA,
                          legend.position="top",
                          textsize=12,
                          pointsize=4.5,
                          linesize=0.8,
                          pointshape=21,
                          font.family="sans"){
  if(is.na(width.bar)==TRUE){width.bar=0.01*mean(trat)}
  piecewise.linear.simple <- function(x, y, middle=1){
    piecewise.linear.likelihood <- function(alpha, x, y){
      N <- length(x);
      w <- (x-alpha);
      w[w<0] <- 0;
      fit <- stats::lm(y ~ x + w);
      Beta <- stats::coefficients(fit);
      Mu <- Beta[1] + Beta[2]*x + Beta[3]*w;
      SSE <- sum(fit$residuals^2);
      sigma2 <- SSE/N;                    # MLE estimate of sigma^2
      likelihood <- sum( log( stats::dnorm(y, mean=Mu, sd=sqrt(sigma2)) ) );
      return(likelihood);
    }

    r <- range(x);
    offset <- r * (1-middle)/2;
    low <- min(x)  + offset;
    high <- max(x) - offset;
    temp <- stats::optimize(piecewise.linear.likelihood, c(low, high), x=x, y=y, maximum=TRUE);
    return(temp$maximum);
  }
  requireNamespace("ggplot2")
  requireNamespace("crayon")
  x=trat
  y=resp
  alpha <- piecewise.linear.simple(x, y, middle)
  w <- x - alpha
  w[w < 0] <- 0
  model <- stats::lm(y ~ x + w)
  out <- NULL
  out$change.point <- alpha
  out$model <- model
  out$x <- seq(min(x), max(x), length = 1000)
  w <- out$x - alpha
  w[w < 0] <- 0
  out$y <- stats::predict(out$model, data.frame(x = out$x,
                                                w = w))
  out$CI <- CI
  class(out) <- "PiecewiseLinear"
  if (CI == TRUE){
    data <- data.frame(x = x, y = y)
    my.cp <- function(data, index) {
      x <- data[index, 1]
      y <- data[index, 2]
      cp <- piecewise.linear.simple(x, y)
      w <- x - cp
      w[w < 0] <- 0
      model <- stats::lm(y ~ x + w)
      out <- c(cp, model$coefficients[2], model$coefficients[3],
               model$coefficients[2] + model$coefficients[3])
      return(out)
    }
    boot.result <- boot::boot(data, my.cp, R = bootstrap.samples)
    out$intervals <- apply(boot.result$t, 2, stats::quantile,
                           probs = c(sig.level/2, 1 - sig.level/2))
    colnames(out$intervals) <- c("Change.Point", "Initial.Slope",
                                 "Slope.Change", "Second.Slope")
    out$CI <- t(out$CI)
  }
  ymean=tapply(y,x,mean)
  if(error=="SE"){ysd=tapply(resp,trat,sd)/sqrt(tapply(resp,trat,length))}
  if(error=="SD"){ysd=tapply(resp,trat,sd)}
  if(error=="FALSE"){ysd=0}
  desvio=ysd
  xmean=tapply(x,x,mean)
  mod=out
  breaks=mod$change.point
  b0=mod$model$coefficients[1]
  b1=mod$model$coefficients[2]
  b11=mod$model$coefficients[3]
  r2=round(summary(mod$model)$r.squared,2)
  r2=floor(r2*100)/100
  equation=sprintf("~~~y==%0.3e %s %0.3e*x~(x<%0.3e)~%s %0.3e*x~(x>%0.3e)~~~R^2==%0.2e",
                   b0,
                   ifelse(b1 >= 0, "+", "-"),
                   abs(b1),
                   breaks,
                   ifelse(b11 >= 0, "+", "-"),
                   abs(b11),
                   breaks,
                   r2)
  s=equation
  temp1=mod$x
  result=mod$y
  preditos1=data.frame(x=temp1,
                        y=result)
  data=data.frame(xmean,ymean)
  data1=data.frame(trat=xmean,resp=ymean)
  graph=ggplot(data1,aes(x=xmean,y=ymean))
  if(error!="FALSE"){graph=graph+geom_errorbar(aes(ymin=ymean-ysd,
                                                   ymax=ymean+ysd),
                                               width=width.bar,size=linesize)}
  graph=graph+geom_point(aes(color="black"),size=pointsize,shape=pointshape,fill="gray")+
    theme+
    geom_line(data=preditos1,aes(x=x,y=y,color="black"),size=linesize)+
    scale_color_manual(name="",values=1,label=parse(text = equation))+
    theme(axis.text = element_text(size=textsize,color="black",family = font.family),
          legend.position = legend.position,
          legend.text = element_text(size=textsize,family = font.family),
          axis.title = element_text(family = font.family),
          legend.direction = "vertical",
          legend.text.align = 0,
          legend.justification = 0)+
    ylab(ylab)+xlab(xlab)
  maximo=breaks
  respmax=b0+b1*breaks
  mini=temp1[which.min(result)]
  respmin=result[which.min(result)]

  result1=round(result,0)
  fa=temp1[result1<=cardinal & temp1>breaks]
  if(length(fa)>0){maxl=max(temp1[result1<=cardinal & temp1>breaks])}else{maxl=NA}
  fb=temp1[result1<=cardinal & temp1<breaks]
  if(length(fb)>0){minimo=max(temp1[result1<=cardinal & temp1<breaks])}else{minimo=NA}
  rmse=sqrt(mean((result1-resp)^2))

  aic=AIC(mod$model)
  bic=BIC(mod$model)
  #print(summary(mod$model))
  graphs=data.frame("Parameter"=c("Optimum temperature",
                                  "Optimum temperature response",
                                  "Minimal temperature",
                                  "Minimal temperature response",
                                  "Predicted maximum basal value",
                                  "Predicted minimum basal value",
                                  "AIC","BIC","r-squared","RMSE"),
                    "values"=round(c(maximo,
                               respmax,
                               mini,
                               respmin,
                               maxl,
                               minimo,
                               aic,bic,r2,rmse),7))
  models=data.frame(summary(mod$model)$coefficients)
  models$Sig=ifelse(models$Pr...t..>0.05,"ns",ifelse(models$Pr...t..<0.01,"**","*"))
  colnames(models)=c("Estimate","Std Error","t value","P-value","")
  graficos=list("Coefficients"=models,
                "values"=graphs,
                graph)
  print(graficos)
}
