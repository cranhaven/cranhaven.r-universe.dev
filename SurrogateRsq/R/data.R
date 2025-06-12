#' Red wine quality dataset of the Portuguese "Vinho Verde" wine
#'
#' A red wine tasting preference data used in the study of Cortez, Cerdeira, Almeida, Matos,
#' and Reis 2009. This red wine contains 1599 samples and 12 variables including the tasting
#' preference score of red wine and its physicochemical characteristics.
#'
#' @docType data
#'
#' @keywords datasets
#'
#' @format A data frame with 1599 rows, quality score, and 11 variables of physicochemical properties of wines.
#' \itemize{
#'   \item `quality` Tasting preference is a rating score provided by a minimum of three sensory with ordinal values from
#'   0 (very bad) to 10 (excellent). The final sensory score is the median of these evaluations.
#'
#'   \item `fixed.acidity` The fixed acidity is the physicochemical property in unit (g(tartaric acid)/dm^3).
#'   \item `volatile.acidity` The volatile acidity is in unit g(acetic acid)/dm^3.
#'   \item `citric.acid` The citric acidity is in unit g/dm^3.
#'   \item `residual.sugar` The residual sugar is in unit g/dm^3.
#'   \item `chlorides` The chlorides is in unit g(sodium chloride)/dm^3.
#'   \item `free.sulfur.dioxide` The free sulfur dioxide is in unit mg/dm^3.
#'   \item `total.sulfur.dioxide` The total sulfur dioxide is in unit mg/dm^3.
#'   \item `density` The density is in unit g/cm^3.
#'   \item `pH` The wine's pH value.
#'   \item `sulphates` The sulphates is in unit g(potassium sulphates)/dm^3.
#'   \item `alcohol` The alcohol is in unit \% vol.
#'   }
#'
#' @references
#' Cortez, P., Cerdeira, A., Almeida, F., Matos, T., and Reis, J. (2009), “Modeling wine
#' preferences by data mining from physicochemical properties,” Decision Support Systems,
#' 47, 547–553. \doi{10.1016/j.dss.2009.05.016}
#'
#' @name RedWine
#'
#' @usage
#' data(RedWine)
#'
#' @examples
#' head(RedWine)
NULL

#' White wine quality dataset of the Portuguese "Vinho Verde" wine
#'
#' A white wine tasting preference data used in the study of Cortez, Cerdeira, Almeida, Matos,
#' and Reis 2009. This white wine contains 4898 white vinho verde wine samples and 12
#' variables including the tasting preference score of white wine and its physicochemical characteristics.
#'
#' @docType data
#'
#' @keywords datasets
#'
#' @format A data frame with 4898 rows, quality score, and 11 variables of physicochemical properties of wines.
#' \itemize{
#'   \item `quality` Tasting preference is a rating score provided by a minimum of three sensory with ordinal values from
#'   0 (very bad) to 10 (excellent). The final sensory score is the median of these evaluations.
#'
#'   \item `fixed.acidity` The fixed acidity is the physicochemical property in unit (g(tartaric acid)/dm^3).
#'   \item `volatile.acidity` The volatile acidity is in unit g(acetic acid)/dm^3.
#'   \item `citric.acid` The citric acidity is in unit g/dm^3.
#'   \item `residual.sugar` The residual sugar is in unit g/dm^3.
#'   \item `chlorides` The chlorides is in unit g(sodium chloride)/dm^3.
#'   \item `free.sulfur.dioxide` The free sulfur dioxide is in unit mg/dm^3.
#'   \item `total.sulfur.dioxide` The total sulfur dioxide is in unit mg/dm^3.
#'   \item `density` The density is in unit g/cm^3.
#'   \item `pH` The wine's pH value.
#'   \item `sulphates` The sulphates is in unit g(potassium sulphates)/dm^3.
#'   \item `alcohol` The alcohol is in unit \% vol.
#'   }
#'
#' @references
#' Cortez, P., Cerdeira, A., Almeida, F., Matos, T., and Reis, J. (2009), “Modeling wine
#' preferences by data mining from physicochemical properties,” Decision Support Systems,
#' 47, 547–553. \doi{10.1016/j.dss.2009.05.016}
#'
#' @name WhiteWine
#'
#' @usage
#' data(WhiteWine)
#'
#' @examples
#' head(WhiteWine)
NULL
