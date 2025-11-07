## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo = FALSE-------------------------------------------------------------
# Use ggplot2 to create the visualizations.
library(ggplot2)

# Load the Anscombe Quartet.
data(anscombe)

# Create the ggplot objects to display.
computational_components <- list(
  Linear = ggplot(anscombe, aes(x = x1, y = y1)) + geom_point() + theme_bw(),
  `Non Linear` = ggplot(anscombe, aes(x = x2, y = y2)) + geom_point() + theme_bw(),
  `Outlier Vertical`= ggplot(anscombe, aes(x = x3, y = y3)) + geom_point() + theme_bw(),
  `Outlier Horizontal` = ggplot(anscombe, aes(x = x4, y = y4)) + geom_point() + theme_bw())

# Save the file to disk to be read by the output R Markdown document.
#saveRDS(computational_components, "comp-comp.rds")

## ----eval = FALSE-------------------------------------------------------------
#  # Use ggplot2 to create the visualizations.
#  library(ggplot2)
#  
#  # Load the Anscombe Quartet.
#  data(anscombe)
#  
#  # Create the ggplot objects to display.
#  computational_components <- list(
#    Linear = ggplot(anscombe, aes(x = x1, y = y1)) + geom_point() + theme_bw(),
#    `Non Linear` = ggplot(anscombe, aes(x = x2, y = y2)) + geom_point() + theme_bw(),
#    `Outlier Vertical`= ggplot(anscombe, aes(x = x3, y = y3)) + geom_point() + theme_bw(),
#    `Outlier Horizontal` = ggplot(anscombe, aes(x = x4, y = y4)) + geom_point() + theme_bw())
#  
#  # Save the file to disk to be read by the output R Markdown document.
#  saveRDS(computational_components, "comp-comp.rds")

## -----------------------------------------------------------------------------
library(listdown)

ld <- listdown(load_cc_expr = readRDS("comp-comp.rds"),
               package = "ggplot2")

## -----------------------------------------------------------------------------
doc <- c(
  as.character(ld_rmarkdown_header("Anscombe's Quartet",
                                   author = "Francis Anscombe",
                                   date = "1973")),
  ld_make_chunks(ld))

cat("\n", paste(doc, collapse = "\n"))

## -----------------------------------------------------------------------------
ld <- listdown(load_cc_expr = readRDS("comp-comp.rds"), 
               package = "ggplot2",
               echo = FALSE)

cat(paste(ld_make_chunks(ld), collapse = "\n"))

## ----echo = FALSE-------------------------------------------------------------
computational_components$Data <- anscombe
#saveRDS(computational_components, "comp-comp.rds")
cat(paste(ld_make_chunks(ld), collapse = "\n"))

## ----eval = FALSE-------------------------------------------------------------
#  computational_components$Data <- anscombe
#  saveRDS(computational_components, "comp-comp.rds")
#  cat(paste(ld_make_chunks(ld), collapse = "\n"))

## -----------------------------------------------------------------------------
library(DT)
ld <- listdown(load_cc_expr = readRDS("comp-comp.rds"), 
               package = c("ggplot2", "DT"),
               decorator = list(data.frame = datatable))
cat(paste(ld_make_chunks(ld), collapse = "\n"))

