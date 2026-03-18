#' @title    Examp2.4.3.1 from Duchateau, L. and Janssen, P. and Rowlands, G. J. (1998).\emph{Linear Mixed Models. An Introduction with applications in Veterinary Research}. International Livestock Research Institute.
#' @name     Examp2.4.3.1
#' @docType  data
#' @keywords datasets
#' @description Examp2.4.3.1 is used for inspecting probability distribution and to define a plausible process through
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
#' @examples
#' #-------------------------------------------------------------
#' ## Example 2.4.3.1 p-66
#' #-------------------------------------------------------------
#'  # PROC MIXED DATA=ex127;
#'  # CLASS sire;
#'  # MODEL ww=;
#'  # RANDOM sire/solution;
#'  # RUN;
#'  
#' library(lme4)
#' str(ex127)
#' fm2.8 <- 
#'   lme4::lmer(
#'          formula    = Ww~(1|sire)
#'        , data       = ex127
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
#' summary(fm2.8)
#' lme4::fixef(fm2.8)
#' lme4::ranef(fm2.8)
#' 
NULL