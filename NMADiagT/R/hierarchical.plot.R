#' Plotting Results from \code{nmadt.hierarchical} and \code{nmadt.hierarchical.MNAR}
#' @description \code{hierarchical.plot} plots the results from \code{nmadt.hierarchical} and \code{nmadt.hierarchical.MNAR}.Graphics representation includes density plots, forest plots, SROC curves, and contour plots of the results.
#' @param nstu an integer indicating the number of studies included in the dataset.
#' @param K an integer indicating the number of candiate test in the dataset.
#' @param data a list conating the input dataset to be used for meta-analysis.
#' @param testname a string vector of the names of the candidate tests in the dataset in the same order as presetned in the dataset.
#' @param output a list object obtained by function \code{nmadt.hierarchical} and \code{nmadt.hierarchical.MNAR}.
#' @param directory a string specifying the designated directory to save the plots.
#' @param typeofplot a string vector containing the types of plot to be generated including "density", "forest","sroc", and "contour".
#' @param n.chains a number indicating number of MCMC chains generated using \code{nmadt.hierarchical} and \code{nmadt.hierarchical.MNAR}. The default is 3.
#' @importFrom ks Hscv kde
#' @import stats
#' @import graphics
#' @import ggplot2
#' @import reshape2
#' @import plotrix
#' @import stats
#' @import grDevices
#' @import imguR
#' @examples
#' \donttest{
#' kangdata<-read.csv(file=system.file("extdata","kangdata.csv",package="NMADiagT"),
#' header=TRUE, sep=",")
#' set.seed(9)
#' kang.out <- nmadt.hierarchical(nstu=12, K=2, data=kangdata, testname=c("D-dimer","Ultrasonography"))
#' hierarchical.plot(nstu=12, K=2, data=kangdata, directory = tempdir(),
#' testname=c("D-dimer","Ultrasonography"),output=kang.out)
#' }
#' @export
hierarchical.plot=function(nstu, K, data, testname, output, directory,
                           typeofplot=c("density", "forest","sroc","contour"),n.chains=3){

  #options(warn = 1)
  if (missing(data))
    stop("need to input dataset.")
  if (missing(directory))
    stop("need to input directory for saving plots.")
  if (ncol(data)!=2*K+4|typeof(data)!="list")
    stop("dataset in incorrect format.")
  if (missing(nstu))
    stop("need to number of studies.")
  if (length(unique(data[,1]))!= nstu)
    stop("number of studies incorrect")
  if (missing(K))
    stop("need to specify number of diagnostic tests.")
  if (K<1|K>5)
    stop("number of diagnostic tests out of bound (should be between 1-5).")
  if(output$model!="hierarchical")
    stop("incorrect output type")
  if(length(testname)!=K)
    stop("number of test names does not match number of tests")
  if (missing(testname)) {
    for(i in 1:K){
      testname[i]<-paste("Test", i, sep = "")
    }
  }
  param = c("density", "forest","sroc","contour")
  if (!any(is.element(typeofplot, param)))
    stop("the type of plot is not available.")

  if(is.element("density", typeofplot)){
    message("Start saving density plots...\n")
    density.hierarchical(samp.gen=output$rawOutput,testname = testname,K=K, n.chains=n.chains, dirc=directory)
  }
  if(is.element("forest", typeofplot)){
    message("Start saving forest plots...\n")
    h_forestplot(K=K,nstu=nstu,dat=data,testname=testname, samp=output$rawOutput, dirc=directory)
  }
  if(is.element("sroc", typeofplot)){
    message("Start saving SROC plots...\n")
    h_sroc(K=K,nstu=nstu,dat=data,testname=testname, samp=output$rawOutput, dirc=directory)
  }
  if(is.element("contour", typeofplot)){
    message("Start saving contour plots...\n")
    contour.h(samp.gen=output$rawOutput,K=K,testname=testname, dirc=directory)
  }
  #options(warn = 0)
}
