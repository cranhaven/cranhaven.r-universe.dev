#' Mortality Rate
#'
#' This function calculates the mortality rate which is also known as the crude
#' death rate for a given population.
#'
#'
#'
#'
#'
#' @param number_people_dead numeric vector that contains the number of people
#'     dead for a time period
#' @param population_size numeric vector that contains the total population
#'     size of which the deaths occurred
#' @param n numeric vector that contains the population size units (ex., 3 for
#'     1,000 people, 5 for 100,000 people)
#'
#'
#'
#' @return the mortality rate as a numeric vector
#'
#'
#'
#' @references
#' \enumerate{
#'    \item Giovanni Scerra, Published Sep 26, 2021, "The Math of the Pandemic: COVID-19 Mortality Rate", LinkedIn, \url{https://www.linkedin.com/pulse/math-pandemic-covid-19-mortality-rate-giovanni-scerra-}.
#'    \item Michael Darcy and Łucja Zaborowska, MD, PhD, Last updated on Nov 05, 2022, "Mortality Rate Calculator", Omni Calculator, \url{https://www.omnicalculator.com/health/mortality-rate}.
#'  }
#'
#'
#'
#' @author Irucka Embry
#'
#'
#'
#' @encoding UTF-8
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' @examples
#' 
#' # Example from Reference 1
#'
#' library(iemisc)
#'
#' mortality_rate(369369, 331534662, 5)
#'
#'
#'
#'
#'
#'
#'
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#'
#' @export
# Check
mortality_rate <- function(number_people_dead, population_size, n) {

checks <- c(number_people_dead, population_size, n)

assert_that(!any(qtest(checks, "N+(0,)") == FALSE), msg = "Either number_people_dead, population_size, or n is 0, NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values and provide an error message if the check fails


mortality_rate <- (number_people_dead / population_size) * 10 ^ n

return(mortality_rate)

}







#' Mortality Rate Percent
#'
#' This function calculates the mortality rate percent which is also known as
#' the crude death rate percent for a given population.
#'
#'
#'
#'
#'
#' @param mortality_rate numeric vector that contains the mortality rate
#' @param n numeric vector that contains the population size units (ex., 3 for
#'     1,000 people, 5 for 100,000 people)
#'
#'
#'
#' @return the mortality rate percent as a numeric vector
#'
#'
#'
#' @references
#' \enumerate{
#'    \item Giovanni Scerra, Published Sep 26, 2021, "The Math of the Pandemic: COVID-19 Mortality Rate", LinkedIn, \url{https://www.linkedin.com/pulse/math-pandemic-covid-19-mortality-rate-giovanni-scerra-}.
#'    \item Michael Darcy and Łucja Zaborowska, MD, PhD, Last updated on Nov 05, 2022, "Mortality Rate Calculator", Omni Calculator, \url{https://www.omnicalculator.com/health/mortality-rate}.
#'  }
#'
#'
#'
#' @author Irucka Embry
#'
#'
#'
#' @encoding UTF-8
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' @examples
#' 
#' # Example from Reference 1
#'
#' library(iemisc)
#'
#' mr_2020 <- mortality_rate(369369, 331534662, 5)
#' 
#' mortality_rate_pct(mortality_rate(15, 331534662, 5), 5)
#'
#' mortality_rate_pct(mr_2020, 5)
#'
#' mortality_rate_pct(15, 5)
#'
#'
#'
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#'
#' @export
mortality_rate_pct <- function(mortality_rate, n) {


checks <- c(mortality_rate, n)

# Check
assert_that(!any(qtest(checks, "N+(0,)") == FALSE), msg = "Either mortality_rate or n is 0, NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values and provide an error message if the check fails


mortality_rate_pct <- (mortality_rate * 100) / 10 ^ n

return(mortality_rate_pct)

}











#' Proportional Mortality Ratio
#'
#' This function calculates the proportional mortality ratio for a given population.
#'
#'
#'
#'
#'
#'
#' @param cause_deaths numeric vector that contains the deaths from a single
#'     cause
#' @param total_deaths numeric vector that contains the total deaths in the
#'     given population
#'
#'
#'
#' @return the proportional mortality ratio (as a percent) as a numeric vector
#'
#'
#'
#' @references
#' \enumerate{
#' Michael Darcy and Łucja Zaborowska, MD, PhD, Last updated on Nov 05, 2022, "Mortality Rate Calculator", Omni Calculator, \url{https://www.omnicalculator.com/health/mortality-rate}.
#' Florida Museum of Natural History: International Shark Attack File, Last updated on 07/19/2022, "Risk of Death: 18 Things More Likely to Kill You Than Sharks", \url{https://www.floridamuseum.ufl.edu/shark-attacks/odds/compare-risk/death/}.
#' Farida B. Ahmad, MPH, Jodi A. Cisewski, MPH, Robert N. Anderson, PhD, \emph{MMWR Morb Mortal Wkly Rep} 2022, 71:597-600, "Provisional Mortality Data — United States, 2021", \url{https://www.cdc.gov/mmwr/volumes/71/wr/mm7117e1.htm}.
#'  }
#'
#'
#'
#'
#' @author Irucka Embry
#'
#'
#'
#' @encoding UTF-8
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' @examples
#' 
#' # Data from Reference 2 and Reference 3
#'
#' library(iemisc)
#'
#' prop_mortality_ratio(cause_deaths = 652486, total_deaths = 3458697)
#' # annual heart disease deaths & total deaths in the US in 2021
#'
#'
#'
#'
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#'
#' @export
prop_mortality_ratio <- function(cause_deaths, total_deaths) {


checks <- c(cause_deaths, total_deaths)

# Check
assert_that(!any(qtest(checks, "N+(0,)") == FALSE), msg = "Either mortality_rate or n is 0, NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values and provide an error message if the check fails


prop_mortality_ratio <- (cause_deaths / total_deaths) * 100

return(prop_mortality_ratio)

}
