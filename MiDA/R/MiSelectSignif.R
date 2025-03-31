#' Select biological markers with high fold change and classification importance
#'
#' Choose probes which change is biologically significant based on binary classification feature importance,
#' gene expression fold change and statistical significance.
#'
#'@param probes character vector of probe (gene, transcript) names.
#'@param mean1 numeric vector of mean values for probes expression in the first group of comparison.
#'@param mean2 numeric vector of mean values for probes expression in the second group of comparison.
#'@param FC.method character specifying the method of fold change counting. Possible values are:
#'"absolute" (mean1-mean2),
#'"percent" ((mean1*100/mean2)-100),
#'"ratio" (mean1/ mean2),
#'"Log2.ratio" (log2(mean1/mean2)).
#'@param infl numeric vector of mean values for probes feature importance (relative influence) from binary classification.
#'@param stat.val numeric vector of statistical significance (p-value, q-value) for testing differences
#' of \code{mean1} and \code{mean2}.
#'@param tresh.FC numeric from o to 1 specifying the tresh hold for fold change \code{FC} parameters (quantile).The significant
#'fold change is bigger than \code{tresh.FC}.
#'@param tresh.infl numeric from o to 1 specifying the tresh hold for feature importance \code{infl} parameters (quantile).The significant
#'feature importance is bigger than \code{tresh.infl}.
#'@param tresh.stat numeric from o to 1 specifying the tresh hold for statistical significance \code{stat.val}.The significant
#'fold change is lesser than \code{tresh.stat}.
#'
#'@details The order must be the same for all parameters.
#'\cr
#'This function marks as "markers" probes that statistically significant change their expression in two
#'groups of comparison with high (over tresh hold) fold change and feature importance from binary classification.
#'
#'@return data frame of probe names, their fold change values, statistical significance values,
#'feature inportance values and marker values.
#'
#'@author Elena N. Filatova
#'
#'@examples
#' probes<-paste("probe", 1:50, sep="") #probes
#' mean1<-rnorm(50, mean=0, sd=1) #means
#' mean2<-rnorm(50, mean=5, sd=1)
#' infl<-c(1:50) # influence
#' stat.val<-rep(c(0.05, 0.04), c(20, 30))
#' Result<-MiSelectSignif(probes, mean1, mean2, FC.method="absolute", infl, stat.val,
#'                       tresh.FC=0.75, tresh.infl=0.75, tresh.stat=0.05)
#' Result[1:5,]

#'
#'@export


MiSelectSignif <- function(probes, mean1, mean2, FC.method, infl, stat.val,
                           tresh.FC=0.75, tresh.infl=0.75, tresh.stat=0.05){
  FC.name<-c()
  if (FC.method == "absolute"){FC<-mean1-mean2; FC.name<-"FC.diff.means"} # count fold change for all methods
  if (FC.method == "percent"){FC<-(mean1*100/mean2)-100; FC.name<-"FC.percents"}
  if (FC.method == "ratio"){FC<-mean1/mean2; FC.name<-"FC.mean.ratio"}
  if (FC.method == "Log2.ratio"){FC<-log2(mean1/mean2); FC.name<-"FC.Log2.ratio"}
  treshFC <- stats::quantile(abs(FC), tresh.FC) #treshholds
  treshInf <- stats::quantile(infl, tresh.infl)
  marker <- c(); tresh <- c()
  for (i in 1:length(probes)){
    ifelse (infl[i] > treshInf | abs(FC[i]) > treshFC, tresh[i] <- TRUE, tresh[i] <- FALSE) # choose importante probes with high FC
    ifelse (stat.val[i] < tresh.stat & tresh[i] == TRUE, marker[i] <- TRUE, marker[i] <- FALSE)
  }
  data.marker <- data.frame(probes, FC, stat.val, infl, marker) # marker - significant probes
  colnames(data.marker)<-c("Probes", FC.name, "Stat.val", "Importance", "Marker")
  pointtype <- c() # plotting
  for (i in 1:length(stat.val)){ifelse(stat.val[i] < tresh.stat, pointtype[i] <- 19, pointtype[i] <- 13)}
  plot.text <- c()
  for (i in 1:length(probes)){ifelse(marker[i] == TRUE, plot.text[i] <- as.character(probes[i]), plot.text[i] <- NA)}
  graphics::plot(FC, infl, pch=pointtype, xlab = "Fold Change", ylab = "Importance", col="black")
  graphics::abline(h=treshInf, lty=2)
  graphics::abline(v=c(-treshFC, treshFC), lty = 2)
  graphics::text(FC, infl, plot.text, cex = 0.5, pos = 4)
  return(data.marker)
}
