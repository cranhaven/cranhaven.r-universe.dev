## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
	collapse = TRUE,
	comment = "#>"
)

## ----library-epoxy------------------------------------------------------------
library(epoxy)

## -----------------------------------------------------------------------------
glue::glue("The average speed of the cars was **{mean(cars$speed)} mph**.
But on average the distance traveled was only _{mean(cars$dist)} ft_.")

## -----------------------------------------------------------------------------
movie <- list(
  year = 1989,
  title = "Back to the Future Part II",
  budget = 4e+07,
  domgross = 118450002,
  imdb_rating = 7.8,
  actors = c(
    "Michael J. Fox",
    "Christopher Lloyd",
    "Lea Thompson",
    "Thomas F. Wilson"
  ),
  runtime = 108L
)

## -----------------------------------------------------------------------------
mpg <- data.frame(
	manufacturer = c("Chevrolet", "Dodge", "Ford"),
	model = c("Malibu", "Caravan", "Expedition"),
	cty = c(19, 7, 11),
	hwy = c(27, 24, 17)
)

## -----------------------------------------------------------------------------
contestant <- list(name = "R User", value = 1000, taxed = 600, in_ca = TRUE)

