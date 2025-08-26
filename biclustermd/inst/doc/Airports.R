## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE, 
  message = FALSE, 
  warning = FALSE, 
  fig.align = 'center',
  fig.width = 7,
  fig.height = 5
)

## ----setup--------------------------------------------------------------------
library(biclustermd)

## -----------------------------------------------------------------------------
# install.packages("nycflights13")
library(nycflights13)
data("flights")

## -----------------------------------------------------------------------------
library(dplyr)
flights <- flights %>%
  select(month, dest, arr_delay)

## -----------------------------------------------------------------------------
library(tidyr)
flights <- flights %>%
  group_by(month, dest) %>%
  summarise(mean_arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  spread(dest, mean_arr_delay) %>% 
  as.data.frame()

## -----------------------------------------------------------------------------
rownames(flights) <- month.name[flights$month]
flights <- as.matrix(flights[, -1])
flights[1:5, 1:5]

## -----------------------------------------------------------------------------
library(biclustermd)
bc <- biclustermd(data = flights, col_clusters = 6, row_clusters = 4)
bc

## -----------------------------------------------------------------------------
library(ggplot2)
?autoplot.biclustermd_sse
autoplot(bc$SSE)

## -----------------------------------------------------------------------------
?autoplot.biclustermd_sim
autoplot(bc$Similarities)

## -----------------------------------------------------------------------------
?autoplot.biclustermd
autoplot(bc) +
  scale_fill_viridis_c(na.value = "white") +
  labs(x = "Destination Airport", y = "Month", fill = "Average Delay")

## -----------------------------------------------------------------------------
autoplot(bc, transform_colors = TRUE, c = 1/10) +
  scale_fill_viridis_c(na.value = "white", limits = c(0, 1)) +
  labs(x = "Destination Airport", y = "Month", fill = "Average Delay")

## -----------------------------------------------------------------------------
autoplot(bc, transform_colors = TRUE, c = 1/10, reorder = TRUE) +
  scale_fill_viridis_c(na.value = "white", limits = c(0, 1)) +
  labs(x = "Destination Airport", y = "Month", fill = "Average Delay")

## -----------------------------------------------------------------------------
repeated_bc <- rep_biclustermd(flights, nrep = 100, col_clusters = 6, row_clusters = 4)
repeated_bc

## -----------------------------------------------------------------------------
repeated_bc$runtime

## -----------------------------------------------------------------------------
plot(cummin(repeated_bc$rep_sse), type = 'o', ylab = 'Cumulative Minimum', xlab = 'Repeat Number')

## -----------------------------------------------------------------------------
training <- gather(repeated_bc$best_bc)
training %>% head()

## -----------------------------------------------------------------------------
autoplot(repeated_bc$best_bc, row_clusts = 1, col_clusts = 4) +
  scale_fill_viridis_c(na.value = "white") +
  labs(x = "Destination Airport", y = "Month", fill = "Average Delay")

model <- training %>% 
  filter(row_cluster == 1, col_cluster == 4) %>% 
  lm(value ~ row_name + col_name, data = .)
summary(model)
sqrt(mean(resid(model) ^ 2))

