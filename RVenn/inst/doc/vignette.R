## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- message=FALSE------------------------------------------------------
library(purrr)
library(RVenn)
library(ggplot2)

## ------------------------------------------------------------------------
set.seed(42)
toy = map(sample(5:25, replace = TRUE, size = 10),
          function(x) sample(letters, size = x))
toy[1:3]  # First 3 of the sets.

## ------------------------------------------------------------------------
toy = Venn(toy)

## ------------------------------------------------------------------------
overlap(toy)

## ------------------------------------------------------------------------
overlap(toy, c("Set_1", "Set_2", "Set_5", "Set_8"))

## ------------------------------------------------------------------------
overlap(toy, c(1, 2, 5, 8))

## ------------------------------------------------------------------------
overlap_pairs(toy, slice = 1:4)

## ------------------------------------------------------------------------
unite(toy)

## ------------------------------------------------------------------------
unite(toy, c("Set_3", "Set_8"))

## ------------------------------------------------------------------------
unite(toy, c(3, 8))

## ------------------------------------------------------------------------
unite_pairs(toy, slice = 1:4)

## ------------------------------------------------------------------------
discern(toy, 1, 8)

## ------------------------------------------------------------------------
discern(toy, "Set_1", "Set_8")

## ------------------------------------------------------------------------
discern(toy, c(3, 4), c(7, 8))

## ------------------------------------------------------------------------
discern_pairs(toy, slice = 1:4)

## ---- fig.height=5, fig.width=8, fig.retina=3----------------------------
ggvenn(toy, slice = c(1, 5))

## ---- fig.height=8, fig.width=8, fig.retina=3----------------------------
ggvenn(toy, slice = c(3, 6, 8))

## ---- fig.height=8, fig.width=8, fig.retina=3----------------------------
setmap(toy)

## ---- fig.height=8, fig.width=8, fig.retina=3----------------------------
setmap(toy, element_clustering = FALSE, set_clustering = FALSE)

## ---- fig.width=8, fig.height=5, fig.retina=3----------------------------
er = enrichment_test(toy, 6, 7)
er$Significance

qplot(er$Overlap_Counts, geom = "blank") +
  geom_histogram(fill = "lemonchiffon4", bins = 8, color = "black") +
  geom_vline(xintercept = length(overlap(toy, c(6, 7))), color = "firebrick2",
             size = 2, linetype = "dashed", alpha = 0.7) +
  ggtitle("Null Distribution") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(name = "Overlap Counts") +
  scale_y_continuous(name = "Frequency")

