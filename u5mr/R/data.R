#' Coale-Demeny Model Life Tables
#'
#' The Coale-Demeny life tables consist of four sets of models, each representing
#' a distinct mortality pattern. Each model is arranged in terms of 25 mortality
#' levels, associated with different expectations of life at birth for females in
#' such a way that `e0` of 20 years corresponds to level 1 and `e0` of 80 years
#' corresponds to level 25.
#'
#' The four underlying mortality patterns of the Coale-Demeny models are called
#' "North", "South", "East" and "West". They were identified through statistical
#' and graphical analysis of a large number of life tables of acceptable quality,
#' mainly for European countries.
#'
#' Reference: United Nations (1990) "Step-by-step guide to the estimation of the child mortality"
#' <https://www.un.org/en/development/desa/population/publications/pdf/mortality/stepguide_childmort.pdf>
#'
#' @docType data
#'
#' @usage data(coale_demeny_ltm)
#'
#' @format An object of class \code{"list"}; consist of four `data.frame` for
#' `male`, `female` and `both sexes`.
#'
#' @keywords datasets
#'
#' @references United Nations Population Studies (1990) Step-by-Step Guide
#' to the Estimation of Child Mortality No.107:1-83
#' (\href{https://www.un.org/en/development/desa/population/publications/pdf/mortality/stepguide_childmort.pdf}{United Nations})
#'
"coale_demeny_ltm"


#' Coefficients for the estimation of child mortality multipliers `k(i)`
#'
#' This is a dataset of coefficients used to estimate multipliers `k(i)` in the TRUSSELL version of
#' the BRASS method, using Coale-Demeny mortality models.
#'
#' @details
#' The basic estimation equation for the Trussell method (equation 4.3) is
#'
#' \deqn{k(i) = a(i) + b(i) P(1)/P(2) + c(i) P(2)/P(3)}
#'
#' - extracted from page 26, Table 4.
#'
#' @docType data
#'
#' @usage data(coeff_trussell_ki)
#'
#' @format A data frame
#'
#' @keywords datasets
#'
#' @references United Nations Population Studies (1990) Step-by-Step Guide
#' to the Estimation of Child Mortality No.107:1-83
#' (\href{https://www.un.org/en/development/desa/population/publications/pdf/mortality/stepguide_childmort.pdf}{United Nations})
#'
"coeff_trussell_ki"


#' Coefficients for the estimation of the time reference `t(i)`
#'
#' This is a dataset of coefficients used to derive the time reference `t(i)`,
#' for values of `q(x)` in the TRUSSELL version of
#' the BRASS method, using Coale-Demeny mortality models.
#'
#' @details
#' The basic estimation equation for the Trussell method (equation 4.3) is
#'
#' \deqn{t(i) = a(i) + b(i) P(1)/P(2) + c(i) P(2)/P(3)}
#'
#' The names of coefficients were changed from `e`, `f`, and `g` to `a`, `b`, and `c`.
#'
#' - extracted from page 27, Table 5.
#'
#' @docType data
#'
#' @usage data(coeff_trussell_ti)
#'
#' @format A data frame
#'
#' @keywords datasets
#'
#' @references United Nations Population Studies (1990) Step-by-Step Guide
#' to the Estimation of Child Mortality No.107:1-83
#' (\href{https://www.un.org/en/development/desa/population/publications/pdf/mortality/stepguide_childmort.pdf}{United Nations})
#'
"coeff_trussell_ti"


#' Bangladesh 1974
#'
#' The data gathered by the 1974 Bangladesh Retrospective Survey of Fertility and Mortality
#' can be used to demonstrate the estimation of child mortality from summary birth histories
#' using the Trussell version of the BRASS method and the Coale-Demeny model life tables
#' \code{\link{coale_demeny_ltm}}.
#'
#' - extracted from Display 6 on page 28 and Display 7 on page 29.
#'
#' @docType data
#'
#' @usage data(bangladesh)
#'
#' @format A data frame
#'
#' @keywords datasets
#'
#' @references United Nations Population Studies (1990) Step-by-Step Guide
#' to the Estimation of Child Mortality No.107:1-83
#' (\href{https://www.un.org/en/development/desa/population/publications/pdf/mortality/stepguide_childmort.pdf}{United Nations})
#'
"bangladesh"




#' Panama 1976
#'
#' The data gathered by a survey in Panama between August and October 1976
#' can be used to demonstrate the estimation of child mortality from summary birth histories
#' using the Trussell version of the BRASS method and the Coale-Demeny model life tables
#' \code{\link{coale_demeny_ltm}}.
#'
#' - extracted from Table49 on page 78.
#'
#' @docType data
#'
#' @usage data(panama)
#'
#' @format A data frame
#'
#' @keywords datasets
#'
#' @references United Nations (1983) Manual X: indirect techniques for demographic
#' estimation. Population studies No. 81. New York: United Nations Department
#' of International Economic and Social Affairs
#' (\href{https://unstats.un.org/unsd/demographic/standmeth/handbooks/Manual_X-en.pdf}{United Nations})
#'
#' @source \href{https://www.un.org/en/development/desa/population/publications/manual/estimate/demographic-estimation.asp}{United Nations Population Division}
"panama"




#' Fake data for Cambodia
#'
#' Fake data used to demonstrate the application of Cohort-derived and Period-derived methods
#' developed by Rajaratnam et al in 2010.
#'
#' `iso3` - the iso3 code of the country from which your microdata come
#' `region` - the region that the country belongs in
#' `country` - name of the country
#' `svy_wt` - sample weight given to the respondent. If no sample weights are provided, then
#' generate this variable with a value of 1 for each respondent
#' `age` - age of the respondent in years: or time since first birth of the respondent in years
#' `sex` - sex of the respondent where 1 indicates male and 2 is female.
#' `ceb` - number of children ever born
#' `cd` - number of children that died
#'
#' @docType data
#'
#' @usage data(microdata)
#'
#' @format A data frame
#'
#' @keywords datasets
#'
#' @references Rajaratnam JK, Tran LN, Lopez AD, Murray CJL (2010) Measuring Under-Five Mortality: Validation of New Low-Cost Methods. PLOS Medicine 7(4): e1000253.
#' (\doi{10.1371/journal.pmed.1000253}{10.1371/journal.pmed.1000253})
#'
#' @source \href{https://journals.plos.org/plosmedicine/article?id=10.1371/journal.pmed.1000253}{PLoS MEDICINE}
"microdata"


#' Aggregated summary birth histories derived from microdata
#'
#' Fake summary data used to demonstrate the application of Cohort-derived and Period-derived methods
#' developed by Rajaratnam et al in 2010.
#'
#' ```r
#' ## codes used to derive the dataset `cambodia`
#'
#' ## install.packages("tidyverse", dependencies = TRUE)
#' ## install.packages("devtools", dependencies = TRUE)
#' ## devtools::install_github("myominnoo/mStats")
#'
#' library(tidyverse)
#' library(mStats)
#' data(microdata)
#' cambodia <- microdata %>%
#'   filter(sex == 2) %>%
#'   filter(age >= 15 & age < 50) %>%
#'   egen(age, seq(15, 45, 5), new_var = "agegroup") %>%
#'   generate(n, 1 * wtper) %>%
#'   replace(ceb, ceb * wtper) %>%
#'   replace(cd, cd * wtper) %>%
#'   group_by(iso3, svdate, agegroup) %>%
#'   summarise(women = sum(n),
#'             child_born = sum(ceb),
#'             child_dead = sum(cd)) %>%
#'   rename(agegrp = agegroup) %>%
#'   data.frame()
#' ```
#'
#' @docType data
#'
#' @usage data(cambodia)
#'
#' @format A data frame
#'
#' @keywords datasets
#'
#' @references Rajaratnam JK, Tran LN, Lopez AD, Murray CJL (2010) Measuring Under-Five Mortality: Validation of New Low-Cost Methods. PLOS Medicine 7(4): e1000253.
#' (\doi{10.1371/journal.pmed.1000253}{10.1371/journal.pmed.1000253})
#'
#' @source \href{https://journals.plos.org/plosmedicine/article?id=10.1371/journal.pmed.1000253}{PLoS MEDICINE}
"cambodia"
