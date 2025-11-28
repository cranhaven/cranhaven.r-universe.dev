## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## -----------------------------------------------------------------------------

library(pbox)
library(data.table)
library(ggplot2)

data("SEAex", package = "pbox")
SEAex$Year <- 1901:2022
SEAex_long <- melt(SEAex, id.vars = "Year", variable.name = "Country")

## -----------------------------------------------------------------------------
ggplot(SEAex_long, aes(x = Year, y = value, color = Country)) +
  geom_line(color = "black") +  # Set all lines to black
  labs(x = "Year", y = "Temperature °C") +
  ggtitle("") +
  facet_grid(Country ~ ., scales = "free_y") +
  theme(legend.position = "none", panel.spacing.y = unit(10, "pt")) +
  theme_bw()


## -----------------------------------------------------------------------------
# Set pbox
pbx <- set_pbox(SEAex)
print(pbx)

## -----------------------------------------------------------------------------
# Marginal Distribution

qpbox(pbx, mj = "Malaysia:33")

# Joint Distribution

qpbox(pbx, mj = "Malaysia:33 & Vietnam:34")

# Conditional Distribution

qpbox(pbx, mj = "Vietnam:31", co = "avgRegion:26")

#Conditional Distribution with Fixed Conditions

qpbox(pbx, mj = "Malaysia:33 & Vietnam:31", co = "avgRegion:26", fixed = TRUE)

#Joint Distribution with Mean Values

qpbox(pbx, mj = "mean:c(Vietnam, Thailand)", lower.tail = TRUE)

# Joint Distribution with Median Values

qpbox(pbx, mj = "median:c(Vietnam, Thailand)", lower.tail = TRUE)

# Joint Distribution with Specific Values

qpbox(pbx, mj = "Malaysia:33 & mean:c(Vietnam, Thailand)", lower.tail = TRUE)

# Conditional Distribution with Mean Conditions

qpbox(pbx, mj = "Malaysia:33 & median:c(Vietnam,Thailand)", co = "mean:c(avgRegion)")

## -----------------------------------------------------------------------------
qpbox(pbx, mj = "Malaysia:33 & median:c(Vietnam,Thailand)", co = "mean:c(avgRegion)", CI = TRUE, fixed = TRUE)

## -----------------------------------------------------------------------------
grid_results <- grid_pbox(pbx, mj = c("Vietnam", "Malaysia"))
print(grid_results)
print(grid_results[which.max(grid_results$probs),])
print(grid_results[which.min(grid_results$probs),])


## -----------------------------------------------------------------------------
scenario_results <- scenario_pbox(pbx, mj = "Vietnam:31 & avgRegion:26", param_list = list(Vietnam = "mu"))
print(scenario_results)

