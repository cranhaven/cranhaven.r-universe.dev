#' @title    Examp2.6.1 from Duchateau, L. and Janssen, P. and Rowlands, G. J. (1998).\emph{Linear Mixed Models. An Introduction with applications in Veterinary Research}. International Livestock Research Institute.
#' @name     Examp2.6.1
#' @docType  data
#' @keywords datasets
#' @description Examp2.6.1 is used for inspecting probability distribution and to define a plausible process through
#' linear models and generalized linear models.
#' @author \enumerate{
#'          \item  Muhammad Yaseen (\email{myaseen208@@gmail.com})
#'          }
#' @references \enumerate{
#' \item Duchateau, L. and Janssen, P. and Rowlands, G. J. (1998).\emph{Linear Mixed Models. An Introduction with applications in Veterinary Research}.
#'              International Livestock Research Institute.
#'  }
#' @seealso
#'    \code{\link{ex124}}
#' @importFrom ggplot2 ggplot
#' @importFrom lme4 lmer
#' @importFrom multcomp glht
#' @examples
#' #-------------------------------------------------------------
#' ## Example 2.6.1 p-76
#' #-------------------------------------------------------------
#'  # PROC MIXED DATA=ex125;
#'  # CLASS drug dose region;
#'  # MODEL pcv=drug dose drug*dose / ddfm=satterth;
#'  # RANDOM region drug*region;
#'  # CONTRAST 'drug dif' drug -1 1 drug*dose -0.5 -0.5 0.5 0.5;
#'  # CONTRAST 'all' drug 1 -1 dose 0  0 drug*dose 0.5  0.5 -0.5 -0.5,
#'  #                drug 0  0 dose 1 -1 drug*dose 0.5 -0.5  0.5 -0.5,
#'  #                drug 0  0 dose 0  0 drug*dose 0.5 -0.5 -0.5  0.5;
#'  # RUN;
#'
#'  library(lmerTest)
#'  str(ex125)
#'  ex125$Region1 <- factor(ex125$Region)
#'  fm2.14 <-
#'   lmerTest::lmer(
#'          formula    = Pcv ~ dose*Drug + (1|Region/Drug)
#'        , data       = ex125
#'        , REML       = TRUE
#'        , control    = lmerControl()
#'        , start      = NULL
#'        , verbose    = 0L
#'     #  , subset
#'     #  , weights
#'     #  , na.action
#'     #  , offset
#'        , contrasts  = list(dose = "contr.SAS", Drug = "contr.SAS")
#'        , devFunOnly = FALSE
#'     #  , ...
#'        )
#'  summary(fm2.14)
#'  anova(object = fm2.14, ddf = "Satterthwaite")
#'
#'  library(multcomp)
#'  Contrasts3 <-
#'            matrix(c(
#'                     0, 0, -1, -0.5
#'                    )
#'                 , ncol = 4
#'                 , byrow = TRUE
#'                 , dimnames = list(
#'                    c("C1")
#'                  , rownames(summary(fm2.14)$coef)
#'                 )
#'                )
#'
#'  Contrasts3
#'  summary(glht(fm2.14, linfct=Contrasts3))
#'
#' if(packageVersion("lmerTest") >= "3.0")
#'    contest(fm2.14, Contrasts3, joint = FALSE)
#'
NULL
