#' @title    Examp2.2.1.7 from Duchateau, L. and Janssen, P. and Rowlands, G. J. (1998).\emph{Linear Mixed Models. An Introduction with applications in Veterinary Research}. International Livestock Research Institute.
#' @name     Examp2.2.1.7
#' @docType  data
#' @keywords datasets
#' @description Examp2.2.1.7 is used for inspecting probability distribution and to define a plausible process through
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
#' ## Example 2.2.1.7 p-42
#' #-------------------------------------------------------------
#'  # PROC GLM DATA=ex121;
#'  # CLASS dose;
#'  # MODEL PCVdif=dose;
#'  # ESTIMATE 'l vs mh' dose -0.5 1 -0.5;
#'  # CONTRAST 'l vs mh' dose -0.5 1 -0.5;
#'  # RUN;
#'
#' library(lme4)
#' str(ex121)
#' fm2.1 <-
#'  aov(
#'      formula     = PCVdiff ~ dose
#'    , data        = ex121
#'    , projections = FALSE
#'    , qr          = TRUE
#'    , contrasts   = NULL
#'  #  , ...
#'    )
#' summary(fm2.1)
#' anova(fm2.1)
#' 
#' LvsMHConc <-
#'          matrix(
#'              data    = c(-0.5, 1, -0.5)
#'            , nrow    = length(levels(ex121$dose))
#'            , byrow   = FALSE
#'            , dimnames = list(
#'                              c(levels(ex121$dose))
#'                            , c("Low vs Mediam and Hight")
#'                            )
#'          )
#' 
#' contrasts(ex121$dose) <- LvsMHConc
#' fm2.2 <-
#'  aov(
#'      formula     = PCVdiff ~ dose
#'    , data        = ex121
#'    , projections = FALSE
#'    , qr          = TRUE
#'    , contrasts   = NULL
#'  #  , ...
#'    )
#' summary(fm2.2, split = list(dose = list("Low vs Mediam and Hight" = 1)))
#' 
#' library(multcomp)
#' fm2.3 <-
#'      lm(
#'           formula     = PCVdiff ~ dose
#'         , data        = ex121
#'      #  , subset
#'      #  , weights
#'      #  , na.action
#'         , method      = "qr"
#'         , model       = TRUE
#'         , x           = FALSE
#'         , y           = FALSE
#'         , qr          = TRUE
#'         , singular.ok = TRUE
#'         , contrasts   = NULL
#'      #  , offset
#'      #  , ...
#'      )
#' summary(fm2.3)
#' anova(fm2.3)
#' # multcomp::glht(
#' #      model       = fm2.3
#' #    , linfct      = LvsMHConc
#' #    , alternative = "two.sided" # c("two.sided", "less", "greater")
#' #    , rhs         = 0
#' #    )
#' 
#' NULL
#' 