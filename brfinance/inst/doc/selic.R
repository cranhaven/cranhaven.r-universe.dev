## ----include = FALSE----------------------------------------------------------
options(repos = c(CRAN = "https://cloud.r-project.org/"))
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----message=FALSE, warning=FALSE, paged.print=FALSE--------------------------
library(brfinance)

# English version
selic_eng <- get_selic_rate(2020, 2024)
head(selic_eng)

# Portuguese version  
selic_pt <- get_selic_rate(2020, 2024, language = "pt")
head(selic_pt)

## ----selic-plot-improved, fig.height=5, fig.width=10, fig.align='center', out.width='90%'----
# Get data
selic_data <- get_selic_rate(2020, 2024)

# Create plot
selic_plot <- plot_selic_rate(selic_data, language = "eng")
print(selic_plot)

## ----selic-plot-improved2, fig.height=5, fig.width=10, fig.align='center', out.width='90%'----
# Portuguese version
selic_data_pt <- get_selic_rate(2000,2005, language = "pt")

selic_plot_pt <- plot_selic_rate(selic_data_pt, language = "pt")
print(selic_plot_pt)

