## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(PRA)

# 4 resources x 5 tasks
S <- matrix(c(
  1, 0, 1, 0,
  1, 1, 0, 0,
  0, 1, 0, 1,
  0, 0, 1, 1,
  0, 1, 1, 0
), nrow = 4, ncol = 5)
rownames(S) <- paste0("R", 1:4)
colnames(S) <- paste0("T", 1:5)
S

## -----------------------------------------------------------------------------
p <- parent_dsm(S)
print(p)

## ----fig.alt="Heatmap of the Parent DSM showing resource-based task coupling"----
plot(p)

## -----------------------------------------------------------------------------
# 3 risks x 4 resources
R <- matrix(c(
  1, 0, 1,
  1, 1, 0,
  0, 1, 0,
  0, 0, 1
), nrow = 3, ncol = 4)
rownames(R) <- paste0("Risk", 1:3)
colnames(R) <- paste0("R", 1:4)
R

## -----------------------------------------------------------------------------
g <- grandparent_dsm(S, R)
print(g)

## ----fig.alt="Heatmap of the Grandparent DSM showing risk-based task coupling"----
plot(g)

