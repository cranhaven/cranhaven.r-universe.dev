
#' Project.portfolio.sample.generation
#'
#' Project.portfolio.sample.generation
#'
#' @param number.of.projects
#' @param number.of.factors
#'
#' @return
#' @export
#'
#' @include project-portfolio.R
#' @include project.R
#' @include project-criterion.R
#' @include project-criteria.R
#' @include factor.R
#'
#'
#'
Project.portfolio.sample.generation <- function(number.of.projects, number.of.factors){

  specific <- c(TRUE, FALSE, FALSE, FALSE, FALSE)
  factor.evaluation <-  c("Cr", "C", "LC","I", "LC","I")

  Project.portfolio(
    lapply( x <- 1:number.of.projects, function(x)
      Project(paste0("Project", as.character(x)), Project.criteria(
        lapply(y <- 1:number.of.factors, function(y)
          Project.criterion(
            Factor(paste0("Factor", as.character(y))),
            sample(factor.evaluation, 1),
            sample(specific, 1)
          )
        )
      )
      )
    )
  )
}





#' Option.portfolio.sample.generation
#'
#' Option.portfolio.sample.generation
#'
#' @param number.of.option
#' @param number.of.factors
#'
#' @return
#' @export
#'
#' @include option-portfolio.R
#' @include option.R
#' @include option-factor-availability.R
#' @include option-resources.R
#' @include factor.R
#'
#'
#' @examples
Option.portfolio.sample.generation <- function(number.of.options, number.of.factors){


  factor.evaluation <-   c("Ex", "G", "R", "W", "Ex", "G", "R", "W", "Ex", "G", "R", "W", "Em", "Z", "In")

  Option.portfolio(
    lapply( x <- 1:number.of.options, function(x)
      Option(paste0("Option", as.character(x)), Option.resources(
        lapply(y <- 1:number.of.factors, function(y)
          Option.factor.availability(
            Factor(paste0("Factor", as.character(y))),
            sample(factor.evaluation, 1)
          )
        )
      )
      )
    )
  )
}



#' Factors.of.interest.sample.generetion
#'
#' Factors.of.interest.sample.generetion
#'
#' @param nr.fatores
#'
#' @return
#' @export
#'
#'
Factors.of.interest.sample.generetion <- function(nr.fatores){
  Factors.of.interest(
    lapply( x <- 1:nr.fatores, function(x)
      Factor(paste0("Factor", as.character(x)))
      )
  )
}


Project.portfolio.sample.generation

Option.portfolio.sample.generation

Factors.of.interest.sample.generetion

