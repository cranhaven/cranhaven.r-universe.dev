## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = FALSE,
  comment = ""
)
library(kDGLM)

## -----------------------------------------------------------------------------
fitted.data <- kdglm(c(AirPassengers) ~ 1, family = Poisson)
plot(fitted.data)

## -----------------------------------------------------------------------------
fitted.data <- kdglm(c(AirPassengers) ~ pol(D = 0.99), family = Poisson)
plot(fitted.data)

## -----------------------------------------------------------------------------
fitted.data <- kdglm(c(AirPassengers) ~ pol(D = 0.99, order = 2) + har(period = 12) + noise(R1 = 0.01), family = Poisson)
plot(fitted.data)

## -----------------------------------------------------------------------------
# Total number of cases
chickenPox$Total <- rowSums(chickenPox[, c(2, 3, 4, 6, 5)])
# Indicator of the introcution of the chicken pox vaccine to the national program of immunization
chickenPox$Vaccine <- chickenPox$date >= as.Date("2013-09-01")

fitted.data <- kdglm(`< 5 year` ~ pol(2, D = 0.95) + har(12, D = 0.975) + Vaccine,
  N = chickenPox$Total,
  family = Multinom,
  data = chickenPox
)
plot(fitted.data, plot.pkg = "base")

## -----------------------------------------------------------------------------
fitted.data <- kdglm(
  `< 5 year` ~ pol(2, D = 0.95) + har(12, D = 0.975) + Vaccine,
  `10 to 14 years` ~ pol(2, D = 0.95) + har(12, D = 0.975) + Vaccine,
  `15 to 49 years` ~ pol(2, D = 0.95) + har(12, D = 0.975) + Vaccine,
  `50 years or more` ~ pol(2, D = 0.95) + har(12, D = 0.975) + Vaccine,
  N = chickenPox$Total,
  family = Multinom,
  data = chickenPox
)
plot(fitted.data, plot.pkg = "base")

## -----------------------------------------------------------------------------
fitted.data <- kdglm(corn.log.return ~ 1,
  V = ~1,
  family = Normal,
  data = cornWheat[1:500, ]
)
plot(fitted.data, plot.pkg = "base")

## ----eval=FALSE, include=FALSE------------------------------------------------
# rmarkdown::render("vignettes/intro.Rmd")
# rmarkdown::render("vignettes/structures.Rmd")
# rmarkdown::render("vignettes/outcomes.Rmd")
# rmarkdown::render("vignettes/fitting.Rmd")
# rmarkdown::render("vignettes/example1.Rmd")

