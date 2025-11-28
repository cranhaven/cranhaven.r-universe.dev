##############################################################
#' Query the probabilistic space of a pbox object.
#'
#' This function queries the probabilistic space of a pbox object to calculate probabilities
#' associated with specific marginal or conditional distributions. It supports conditional
#' probability calculations and can optionally estimate confidence intervals through bootstrapping.
#'
#' @name qpbox
#' @export
#' @param pbx An object of class \code{pbox} from which to query the probabilistic space.
#' @param mj A character string specifying the marginal and or joint distribution of the variable.
#'        It must specify the variable and the value in the format 'Var:Val'.
#' @param co A character string specifying the marginal and conditional distribution of the variable.
#'        It must specify the variable and the value in the format 'Var:Val'.
#' @param lower.tail Logical; if TRUE (default), probabilities are calculated for the area to the right of the specified value.
#' @param fixed Logical; if TRUE, calculates conditional probabilities with conditions treated as fixed.
#' @param CI Logical; if TRUE, calculates bootstrap confidence intervals.
#' @param iter Integer; the number of replications for the confidence interval calculation. Default is 1000.
#' @return Estimated probabilities as a numeric value or a named vector including confidence intervals if requested.
#' @examples
#'   data("SEAex")
#'   pbx <- set_pbox(SEAex)
#'   # Get marginal distribution
#'   qpbox(pbx, mj="Malaysia:33")
#'   # Get conditional distribution
#'   qpbox(pbx, mj="Malaysia:33 & Vietnam:31", co="avgRegion:26")
#' @importFrom copula pMvdc cCopula
#' @importFrom data.table setDT
#' @importFrom stats setNames

setGeneric("qpbox",
           def = function(pbx,mj="character",co="character", lower.tail=TRUE,fixed=FALSE,CI=FALSE,iter=1000) {
             standardGeneric("qpbox")
           })


#' @rdname qpbox
#' @description
#' This method processes the \code{pbox} object to compute probabilities based on the specified marginal
#' and conditional parameters. It handles both simple probability calculations and complex queries involving
#' joint and conditional distributions, with an option for bootstrap confidence interval estimation.

setMethod("qpbox", signature = "pbox",
          definition = function(pbx,mj="character",co="character", lower.tail=TRUE,fixed=FALSE,CI=FALSE,iter=1000) {

            if (!inherits(pbx, c("pbox"))) {
              stop("Input must be a pbox object!")
            }
            if (fixed==TRUE & missing(co)) {
              stop("Conditional query is missing!")
            }

            Varnames<-names(pbx@data)
            Value<-rep(Inf,ncol(pbx@data))
            varSet<-cbind.data.frame(Varnames,Value)
            if (missing(co)) {
              mj<- gsub("[[:blank:]]", "",mj)
              valid_format <- grepl("^([a-zA-Z]+:(\\d+(\\.\\d+)?|[a-zA-Z]+\\(.*\\)),?(&[a-zA-Z]+:(\\d+(\\.\\d+)?|[a-zA-Z]+\\(.*\\)),?)*$)", mj)
              if (!valid_format) {
                stop("Please specify the marginal in the following format 'Variable1:Value1 & Variable2:Value2'")
              }
              if (!is.character(mj)) {
                stop("Expecting a string to query the pbox!")
              }

              if(CI){
                varSet<-match_maker(varSet,q_parser(mj),pbx@data)
                res<-pMvdc(c(varSet$Value), pbx@copula)
                probres<-probCI(replicate(iter, perProb(pbx,varSet$Value)))
                probres<-c(res,probres)
                if(lower.tail==FALSE){probres<-1-probres}
                probres<-setNames(probres, c("P", "2.5%", "97.5%"))
                probres
              }else{
              #browser()
              varSet<-match_maker(varSet,q_parser(mj),pbx@data)
              probres<-pMvdc(c(varSet$Value), pbx@copula)
              if(lower.tail==FALSE){probres<-1-probres}
              probres <- setNames(probres, "P")
              probres
              }
            } else {
              cond<-lapply(list(mj,co),function(z){
                z<-gsub("[[:blank:]]", "",z)
                valid_format <- grepl("^([a-zA-Z]+:(\\d+(\\.\\d+)?|[a-zA-Z]+\\(.*\\)),?(&[a-zA-Z]+:(\\d+(\\.\\d+)?|[a-zA-Z]+\\(.*\\)),?)*$)", z)

                if (!valid_format) {
                  stop("Please specify the conditional in the following format 'Variable1:Value1 & Variable2:Value2'")
                }
                if (!is.character(z)) {
                  stop("Expecting a string to query the pbox!")
                }

                if(CI){
                  varSet<-match_maker(varSet,q_parser(z),pbx@data)
                  res<-pMvdc(c(varSet$Value), pbx@copula)
                  probres<-probCI(replicate(iter, perProb(pbx,varSet$Value)))
                  probres<-c(res,probres)
                  probres<-setNames(probres, c("P", "2.5%", "97.5%"))
                  probres
                }else{
                  varSet<-match_maker(varSet,q_parser(z),pbx@data)
                  #query copula
                  p<-pMvdc(varSet$Value,pbx@copula)
                  p <- setNames(p, "P")
                  p
                }
              })
              if (fixed) {
                condFix <- cCopula(
                  cbind(cond[[1]], cond[[2]]),
                  indices = 2,
                  copula = pbx@copula@copula)
                if(lower.tail==FALSE){condFix<-1-condFix}
                condFix<-as.vector(condFix)
                if (length(condFix) == 1) {
                  condFix <- setNames(condFix, "P")
                } else {
                  condFix <- setNames(condFix, c("P", "2.5%", "97.5%"))
                }
                condFix
              }
              else{
                probrez<-cond[[1]]/cond[[2]]

                if(lower.tail==FALSE){probrez<-1-probrez}
                if (length(probrez) == 1) {
                  probrez <- setNames(probrez, "P")
                } else {
                  probrez<-deltaCI(cond)
                }
                probrez
              }}

          })


