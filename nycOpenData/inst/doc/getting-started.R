## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
knitr::opts_chunk$set(warning = FALSE, message = FALSE)
library(nycOpenData)
library(ggplot2)

## ----small-sample-------------------------------------------------------------
small_sample <- nyc_311(limit = 3)
small_sample

# Seeing what columns are in the dataset
colnames(small_sample)

## ----filter-brooklyn----------------------------------------------------------

brooklyn_311 <- nyc_311(limit = 3, filters = list(borough = "BROOKLYN"))
brooklyn_311

# Checking to see the filtering worked
unique(brooklyn_311$borough)

## ----filter-brooklyn-nypd-----------------------------------------------------
# Creating the dataset
brooklyn_nypd <- nyc_311(limit = 50, filters = list(agency = "NYPD", borough = "BROOKLYN"))

# Calling head of our new dataset
head(brooklyn_nypd)

# Quick check to make sure our filtering worked
nrow(brooklyn_nypd)
unique(brooklyn_nypd$agency)
unique(brooklyn_nypd$borough)

## ----compaint-type-graph, fig.alt="Bar chart showing the frequency of NYPD-related 311 complaint types in Brooklyn from the 50 most recent service requests.", fig.cap="Bar chart showing the frequency of NYPD-related 311 complaint types in Brooklyn from the 50 most recent service requests.", fig.height=5, fig.width=7----
# Visualizing the distribution, ordered by frequency
ggplot(brooklyn_nypd, aes(y = reorder(complaint_type, complaint_type, length))) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  labs(
    title = "Most Recent NYPD 311 Complaints (Brooklyn)",
    subtitle = "Top 50 service requests",
    x = "Number of Complaints",
    y = "Type of Complaint"
  )

