## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  fig.alt = "Default descriptive text",
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
# # Install from CRAN
# install.packages("rbrsa")
# 
# # Or install the development version from GitHub
# # install.packages("pak")
# pak.::pkg_install("obakis/rbrsa")

## -----------------------------------------------------------------------------
# Load the package
library(rbrsa)

## -----------------------------------------------------------------------------
# List available tables in the Monthly Bulletin
bulletin_tables <- list_tables("bddk", lang="en")
head(bulletin_tables)

# List available banking groups for the Monthly Bulletin
bulletin_groups <- list_groups("bddk", lang="en")
head(bulletin_groups)

## -----------------------------------------------------------------------------
# List available tables in Finturk
finturk_tables <- list_tables("finturk", lang="en")
finturk_tables

# List available banking groups for Finturk
finturk_groups <- list_groups("finturk", lang="en")
finturk_groups

# List of cities for Finturk
cities <- list_cities()
head(cities)


## -----------------------------------------------------------------------------
my_dat <- fetch_bddk(
  start_year = 2020,
  start_month = 1,
  end_year = 2020,
  end_month = 12,
  table_no = 4,
  grup_kod = 10001,
  verbose=TRUE
)
# Examine the structure of the returned data
cat("Dimensions:", dim(my_dat), "\n")
colnames(my_dat)
head(my_dat)
## To save the results:
# temp_file <- tempfile() # filename should be without extension
# save_data(my_dat, temp_file, format = "csv")

## ----message=FALSE, warning=FALSE---------------------------------------------
library(dplyr)
library(ggplot2)
colnames(my_dat)
cols = c("Consumer Loans - Housing","Consumer Loans - Personal Finance")
p = my_dat |>
  select(Ad,TRY,period) |>
  filter(Ad %in% cols) |>
  mutate(date=as.Date(paste0(period, "-01"))) |>
  ggplot(aes(x=date, y=TRY, color=Ad, group=Ad, shape=Ad)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.4, alpha = 0.7) +
  scale_x_date(
    date_breaks = "2 months",      # Show tick every 3 months
    date_labels = "%b %Y",         # Format as "Jan 2020"
    expand = c(0.01, 0)           # Reduce padding
  ) +
  scale_y_continuous(
    labels = scales::comma       # Format numbers with commas
  ) +
  labs(
    title = "Consumer Loans Trends, Jan 2020-Dec 2020 (TRY)",
    subtitle = "Monthly data for Housing vs Personal Finance loans",
    x = "",
    y = ""
  ) +
  theme_minimal() +
theme(
    legend.position = "bottom",  # This moves the legend to the bottom
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 14),
  )
p

## -----------------------------------------------------------------------------
my_dat2 <- fetch_finturk(
  start_year = 2023,
  start_month = 3,
  end_year = 2024,
  end_month = 12,
  table_no = 2,
  grup_kod = 10001,
  il=0,
  verbose=FALSE
)
# Examine the structure of the returned data
cat("Dimensions:", dim(my_dat2), "\n")
colnames(my_dat2)
head(my_dat2)
## To save the results:
# temp_file <- tempfile() # filename should be without extension
# save_data(my_dat, temp_file, format = "csv")


## -----------------------------------------------------------------------------
sel_cities =c("ADANA","MALATYA","MUGLA","KAYSERI")
cols = c("il_adi", "period","Toplam Mevduat")
lookup <- c(city="il_adi", deposit="Toplam Mevduat")
p2 = my_dat2[,cols] |>
  rename(all_of(lookup)) |>
  mutate(date=as.Date(paste0(period, "-01"))) |>
  mutate(.by=period, sh = 100*deposit/sum(deposit, na.rm=TRUE)) |>
  filter( city %in% sel_cities) |>
    ggplot(aes(x=date, y=sh, color=city, group=city, shape=city)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.4, alpha = 0.7) +
  scale_x_date(
    date_breaks = "3 months",      # Show tick every 3 months
    date_labels = "%b %Y",         # Format as "Jan 2020"
    expand = c(0.01, 0)           # Reduce padding
  ) +
  labs(
    title = "Share of Selected Provinces in Deposits, 2020-2023 (TRY)",
    x = "",
    y = ""
  ) +
  theme_minimal() +
theme(
    legend.position = "bottom",  # This moves the legend to the bottom
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 14),
  )
p2

## ----eval=FALSE---------------------------------------------------------------
# # Save to different formats. file name must be without extension
# save_data(my_dat, "filename_you_prefer", format = "csv")
# save_data(my_dat, "filename_you_prefer", format = "rds")
# save_data(my_dat, "filename_you_prefer", format = "xlsx")
# 
# # Using tempfile() for examples (as in README)
# temp_file <- tempfile()
# save_data(my_dat, temp_file, format = "csv")
# cat("Data saved to:", temp_file, "\n")

## ----include=FALSE------------------------------------------------------------
# Optional: Ensure all packages needed for building are available
# This chunk won't appear in the final vignette
knitr::opts_chunk$set(
  echo = TRUE,
  message = FALSE,
  warning = FALSE,
  fig.width = 7,
  fig.height = 5,
  fig.align = "center"
)

