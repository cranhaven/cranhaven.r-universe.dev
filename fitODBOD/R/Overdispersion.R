#' Overdispersion
#'
#' After fitting the distribution using this function we can extract the
#' overdispersion value. This function works for fitTriBin, fitBetaBin,
#' fitKumBin, fitGHGBB and fitMcGBB for Binomial Mixture Distributions.
#' Similarly, Alternate Binomial Distributions also support this function for
#' fitAddBin,fitBetaCorrBin, fitCOMPBin, fitCorrBin and fitMultiBin.
#'
#' @usage
#' Overdispersion(object)
#'
#' @param object  An object from one of the classes of fitTB,fitBB,fitKB,fitGB,fitMB.
#'
#' @details
#' \strong{Note} : Only objects from classes of above mentioned classes
#'  can be used.
#'
#' @return
#' The output of \code{Overdispersion} gives a single value which is the
#' overdispersion.
#'
#' @examples
#' No.D.D=0:7      #assigning the random variables
#' Obs.fre.1=c(47,54,43,40,40,41,39,95)  #assigning the corresponding frequencies
#'
#' #estimating mode value for given data
#' results<-EstMLETriBin(No.D.D,Obs.fre.1)
#' results
#' mode<-results$mode
#'
#' #fitting the Triangular Bionomial distribution for estimated parameters
#' TriBin<-fitTriBin(No.D.D,Obs.fre.1,mode)
#' TriBin
#'
#' #extracting the overdispersion
#' Overdispersion(TriBin)
#'
#' @export
Overdispersion<-function(object)
{
  UseMethod("Overdispersion",object)
}

#' @method Overdispersion fitTB
#' @export
Overdispersion.fitTB<-function(object)
{
  return(object$over.dis.para)
}

#' @method Overdispersion fitBB
#' @export
Overdispersion.fitBB<-function(object)
{
  names(object$over.dis.para)<-NULL
  return(object$over.dis.para)
}

#' @method Overdispersion fitKB
#' @export
Overdispersion.fitKB<-function(object)
{
  names(object$over.dis.para)<-NULL
  return(object$over.dis.para)
}

#' @method Overdispersion fitGB
#' @export
Overdispersion.fitGB<-function(object)
{
  names(object$over.dis.para)<-NULL
  return(object$over.dis.para)
}

#' @method Overdispersion fitGrIIB
#' @export
Overdispersion.fitGrIIB<-function(object)
{
  names(object$over.dis.para)<-NULL
  return(object$over.dis.para)
}

#' @method Overdispersion fitGaB
#' @export
Overdispersion.fitGaB<-function(object)
{
  names(object$over.dis.para)<-NULL
  return(object$over.dis.para)
}

#' @method Overdispersion fitMB
#' @export
Overdispersion.fitMB<-function(object)
{
  names(object$over.dis.para)<-NULL
  return(object$over.dis.para)
}
