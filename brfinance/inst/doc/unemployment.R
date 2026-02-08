## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----unemployment-usage, eval=FALSE-------------------------------------------
# library(brfinance)
# 
# # English version
# unemp_eng <- get_unemployment(2018, 2024, language = "eng")
# head(unemp_eng)
# 
# # Portuguese version
# unemp_pt <- get_unemployment(2018, 2024, language = "pt")
# head(unemp_pt)

## ----unemployment-plot, message=FALSE, warning=FALSE, fig.height=5, fig.width=10, fig.align='center', out.width='90%'----
library(brfinance)

# Get data
unemp_data <- get_unemployment(2020, 2024)

# Create plot
unemp_plot <- plot_unemployment(unemp_data, language = "eng")
print(unemp_plot)

# Portuguese version
unemp_data2 <- get_unemployment(2020, 2024, language = "pt")
unemp_plot_pt <- plot_unemployment(unemp_data2, language = "pt")
print(unemp_plot_pt)

