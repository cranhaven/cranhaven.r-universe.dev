## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
	collapse = TRUE,
	comment = "#>"
)

emo_ji <- function(...) {
	"&#x1F606;"
}

## ----setup, include = FALSE---------------------------------------------------
library(epoxy)

## ----ref.label="orange-summary-stats", eval = TRUE, echo = FALSE--------------
n_trees <- length(levels(Orange$Tree))
n_timepoints <- length(unique(Orange$age))

## ----orange-summary-stats-----------------------------------------------------
n_trees <- length(levels(Orange$Tree))
n_timepoints <- length(unique(Orange$age))

## -----------------------------------------------------------------------------
knitted <- list(
	when = format(Sys.Date()),
	where = knitr::current_input(),
	with = format(utils::packageVersion("knitr")),
	doc_url = "https://rdrr.io/pkg/knitr/man/knit.html"
)

## -----------------------------------------------------------------------------
text_ready <-
	data.frame(
		term = c("intercept", "hund_days", "ozone", "hund_days_ozone"),
		estimate = c("4.25", "0.34", "&minus;0.14", "&minus;0.04"),
		se = c(0.131, 0.013, 0.158, 0.015),
		ci = c("[4.00, 4.51]", "[0.31, 0.36]", "[&minus;0.45, 0.17]","[&minus;0.07, &minus;0.01]"),
		stringsAsFactors = FALSE
	)

## -----------------------------------------------------------------------------
stats <- split(text_ready, text_ready$term)

## -----------------------------------------------------------------------------
str(stats)

## ----orange_summary-----------------------------------------------------------
summarize_tree_growth <- function(tree) {
	tree <- Orange[Orange$Tree == tree, ]
	tree <- data.frame(
		tree = tree$Tree[1],
		age_range = diff(range(tree$age)),
		circumference_first = tree$circumference[1],
		circumference_last = tree$circumference[nrow(tree)]
	)
	tree$growth_rate <- with(tree, (circumference_last - circumference_first) / age_range)
	tree
}

orange_summary <- lapply(1:5, summarize_tree_growth)
orange_summary <- do.call(rbind, orange_summary)
orange_summary

