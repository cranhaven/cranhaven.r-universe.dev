## ----include = FALSE----------------------------------------------------------
library(epoxy)

knitr::opts_chunk$set(
	collapse = TRUE,
	comment = "#>"
)

## -----------------------------------------------------------------------------
movie <- list(
	year = 1989,
	title = "Back to the Future Part II",
	budget = 4e+07
)

epoxy(
	"The movie {.titlecase movie$title}",
	"was released in {movie$year}",
	"and was filmed with a budget of",
	"{.dollar movie$budget}.",
	.sep = "\n"
)

