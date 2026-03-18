#' @title    Examp2.4.2.2 from Duchateau, L. and Janssen, P. and Rowlands, G. J. (1998).\emph{Linear Mixed Models. An Introduction with applications in Veterinary Research}. International Livestock Research Institute.
#' @name     Examp2.4.2.2
#' @docType  data
#' @keywords datasets
#' @description Examp2.4.2.2 is used for inspecting probability distribution and to define a plausible process through
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
#' @importFrom lmerTest lsmeansLT
#' @examples
#' #-------------------------------------------------------------
#' ## Example 2.4.2.2 p-64
#' #-------------------------------------------------------------
#' # PROC MIXED DATA=ex125 METHOD=ML;
#' # CLASS drug dose region;
#' # MODEL pcv=drug dose drug*dose;
#' # RANDOM region drug*region;
#' # RUN;
#' # 
#' # PROC MIXED DATA=ex125 METHOD=REML;
#' # CLASS drug dose region;
#' # MODEL pcv=drug dose drug*dose;
#' # RANDOM region drug*region;
#' # RUN;
#'  
#'  
#' library(lme4)
#' str(ex125)
#' 
#' fm2.4 <- 
#'   lme4::lmer(
#'          formula    = Pcv ~ dose*Drug + (1|Region/Drug)
#'        , data       = ex125
#'        , REML       = FALSE
#'        , control    = lmerControl()
#'        , start      = NULL
#'        , verbose    = 0L
#'     #  , subset
#'     #  , weights
#'     #  , na.action
#'     #  , offset
#'        , contrasts  = NULL
#'        , devFunOnly = FALSE
#'     #  , ...
#'        ) 
#' summary(fm2.4)
#' anova(fm2.4)
#' 
#' fm2.5 <- 
#'   lme4::lmer(
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
#'        , contrasts  = NULL
#'        , devFunOnly = FALSE
#'     #  , ...
#'        )  
#' summary(fm2.5)
#' anova(fm2.5)
#' 
#' library(lmerTest)
#' 
#' fm2.6 <- 
#'     lmerTest::lmer(
#'          formula    = Pcv ~ dose*Drug + (1|Region/Drug)
#'       	, data       = ex125
#'       	, REML       = FALSE
#'       	, control    = lmerControl()
#'       	, start      = NULL
#'       	, verbose    = 0L
#'       #	, subset
#'       #	, weights
#'       #	, na.action
#'       #	, offset
#'       	, contrasts  = NULL
#'       	, devFunOnly = FALSE
#'       #	, ...
#'       	)
#' summary(fm2.6)
#' anova(fm2.6)
#' 
#' fm2.7 <- 
#'     lmerTest::lmer(
#'          formula    = Pcv ~ dose*Drug + (1|Region/Drug)
#'       	, data       = ex125
#'       	, REML       = TRUE
#'       	, control    = lmerControl()
#'       	, start      = NULL
#'       	, verbose    = 0L
#'       #	, subset
#'       #	, weights
#'       #	, na.action
#'       #	, offset
#'       	, contrasts  = NULL
#'       	, devFunOnly = FALSE
#'       #	, ...
#'       	) 
#' summary(fm2.7)
#' anova(fm2.7)
#' 
NULL