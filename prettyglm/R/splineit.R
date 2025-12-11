#' @title splineit
#'
#' @description Splines a continuous variable
#'
#' @param var Continuous vector to spline.
#' @param min Min of spline.
#' @param max Max of spline.
#'
#' @return
#' Splined Column
#'
#' @examples
#'library(dplyr)
#'library(prettyglm)
#'data('titanic')
#'
#'columns_to_factor <- c('Pclass',
#'                       'Sex',
#'                       'Cabin',
#'                       'Embarked',
#'                       'Cabintype',
#'                       'Survived')
#'meanage <- base::mean(titanic$Age, na.rm=TRUE)
#'
#'titanic  <- titanic  %>%
#'  dplyr::mutate_at(columns_to_factor, list(~factor(.))) %>%
#'  dplyr::mutate(Age =base::ifelse(is.na(Age)==TRUE,meanage,Age)) %>%
#'  dplyr::mutate(Age_0_25 = prettyglm::splineit(Age,0,25),
#'                Age_25_50 = prettyglm::splineit(Age,25,50),
#'                Age_50_120 = prettyglm::splineit(Age,50,120)) %>%
#'  dplyr::mutate(Fare_0_250 = prettyglm::splineit(Fare,0,250),
#'                Fare_250_600 = prettyglm::splineit(Fare,250,600))
#'
#' @export
#'
splineit <- function(var,min,max) {
  x = base::ifelse(var-min < 0 , 0, var-min)
  x = base::ifelse(var-min >= max-min , max-min , x)
  return (x)
}
