#' @title    Examp3.3 from Duchateau, L. and Janssen, P. and Rowlands, G. J. (1998).\emph{Linear Mixed Models. An Introduction with applications in Veterinary Research}. International Livestock Research Institute.
#' @name     Examp3.3
#' @docType  data
#' @keywords datasets
#' @description Examp3.3 is used for inspecting probability distribution and to define a plausible process through
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
#' ## Example 3.3 Model 1 p-88
#' #-------------------------------------------------------------
#' # PROC MIXED DATA=ex33;
#' # CLASS breed animal_id;
#' # MODEL pcv = breed breed*time/SOLUTION;
#' # RANDOM animal_id(breed)/SOLUTION;
#' # RUN;
#'
#'  library(lme4)
#'  options(contrasts = c(factor = "contr.SAS", ordered = "contr.poly"))
#'  str(ex33)
#'
#'  fm3.5 <-
#'   lme4::lmer(
#'          formula    = PCV ~ breed + breed:time + (1|animal_id:breed)
#'        , data       = ex33
#'        , REML       = TRUE
#'        , control    = lmerControl()
#'        , start      = NULL
#'        , verbose    = 0L
#'     #  , subset
#'     #  , weights
#'     #  , na.action
#'     #  , offset
#'        , contrasts  = list(breed = "contr.SAS")
#'        , devFunOnly = FALSE
#'     #  , ...
#'        )
#'  summary(fm3.5)
#'  anova(fm3.5)
#'
#'  library(lmerTest)
#'  fm3.6 <-
#'   lmerTest::lmer(
#'          formula    = PCV ~ breed + breed:time + (1|animal_id:breed)
#'        , data       = ex33
#'        , REML       = TRUE
#'        , control    = lmerControl()
#'        , start      = NULL
#'        , verbose    = 0L
#'     #  , subset
#'     #  , weights
#'     #  , na.action
#'     #  , offset
#'        , contrasts  = list(breed = "contr.SAS")
#'        , devFunOnly = FALSE
#'     #  , ...
#'        )
#'  summary(fm3.6)
#'  anova(object = fm3.6, ddf = "Satterthwaite")
#'
#'
#' # PROC MIXED DATA=ex33;
#' # CLASS breed animal_id;
#' # MODEL pcv = breed breed*time/SOLUTION;
#' # REPEATED/TYPE=CS SUB = animal_id(breed) R;
#' # RUN;
#'
#'
#'  library(nlme)
#'  fm3.7 <-
#'       nlme::gls(
#'             model       = PCV ~ breed + breed:time
#'           , data        = ex33
#'           , correlation = corCompSymm(, form = ~ 1|animal_id/breed)
#'           , weights     = NULL
#'         # , subset      =
#'           , method      = "REML" # c("REML", "ML")
#'           , na.action   = na.fail
#'           , control     = list()
#'           )
#'  summary(fm3.7)
#'  anova(fm3.7)
#'
#'
#'
#' # PROC MIXED DATA=ex33;
#' # CLASS breed animal_id;
#' # MODEL pcv = time breed breed*time/SOLUTION;
#' # RANDOM animal_id(breed)/SOLUTION;
#' # RUN;
#'
#'  fm3.8 <-
#'   lme4::lmer(
#'          formula    = PCV ~ time + breed + breed:time + (1|animal_id:breed)
#'        , data       = ex33
#'        , REML       = TRUE
#'        , control    = lmerControl()
#'        , start      = NULL
#'        , verbose    = 0L
#'     #  , subset
#'     #  , weights
#'     #  , na.action
#'     #  , offset
#'        , contrasts  = list(breed = "contr.SAS")
#'        , devFunOnly = FALSE
#'     #  , ...
#'        )
#'  summary(fm3.8)
#'  anova(fm3.8)
#'
#'
#'  fm3.9 <-
#'   lmerTest::lmer(
#'          formula    = PCV ~ time + breed + breed:time + (1|animal_id:breed)
#'        , data       = ex33
#'        , REML       = TRUE
#'        , control    = lmerControl()
#'        , start      = NULL
#'        , verbose    = 0L
#'     #  , subset
#'     #  , weights
#'     #  , na.action
#'     #  , offset
#'        , contrasts  = list(breed = "contr.SAS")
#'        , devFunOnly = FALSE
#'     #  , ...
#'        )
#'  summary(fm3.9)
#'  anova(object = fm3.9, ddf = "Satterthwaite", type = 3)
#'
#'
#' # PROC MIXED DATA=ex33;
#' # CLASS breed animal_id;
#' # MODEL pcv = breed breed*time/SOLUTION;
#' # REPEATED/TYPE=AR(1) SUBJET = animal_id(breed) R;
#' # RUN;
#'
#'
#'  library(nlme)
#'  fm3.10 <-
#'       nlme::gls(
#'             model       = PCV ~ breed + breed:time
#'           , data        = ex33
#'           , correlation = corAR1(, form = ~ 1|animal_id/breed)
#'           , weights     = NULL
#'         # , subset      =
#'           , method      = "REML" # c("REML", "ML")
#'           , na.action   = na.fail
#'           , control     = list()
#'           )
#'  summary(fm3.10)
#'  anova(fm3.10)
#'
#' # PROC MIXED DATA=ex33;
#' # CLASS breed animal_id;
#' # MODEL pcv = breed breed*time/SOLUTION;
#' # RANDOM INTERCEPT time/TYPE=UN SUBJET = animal_id(breed) SOLUTION;
#' # RUN;
#'
#'
#'  library(nlme)
#' # fm3.11 <-
#' #      nlme::gls(
#' #            model       = PCV ~ breed + breed:time
#' #          , data        = ex33
#' #          , random      = ~1|animal_id/breed
#' #          , correlation = corAR1(, form = ~ 1|animal_id/breed)
#' #          , weights     = NULL
#' #        # , subset      =
#' #          , method      = "REML" # c("REML", "ML")
#' #          , na.action   = na.fail
#' #          , control     = list()
#' #          )
#' # summary(fm3.11)
#' # anova(fm3.11)
#'
NULL
