## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.width = 7, fig.height = 6, comment = "#>") 

## ------------------------------------------------------------------------
library(foodingraph)

## ------------------------------------------------------------------------

# Food intakes (ordinaly- or binary-encoded)
obs_data <- data.frame(
#| Foods | Subject 1   2   3   4   5   6   7   8   9  10  11  12  13  |
#|-------|------------------------------------------------------------|
  alcohol_cat  = c(8,  1,  3,  0, 10,  5,  1, 10,  2,  8,  1,  3,  9),
  bread_cat    = c(7,  4,  3,  4,  0,  9,  4,  5,  7,  3,  4,  0,  9),
  coffee_cat   = c(3,  6,  6,  6,  2,  3,  5,  8,  8,  6,  6,  2,  3),
  duck_cat     = c(0,  3,  1,  0,  0,  2, 13,  1,  0,  0,  2, 13,  1),
  eggs_cat     = c(5,  5,  4,  5,  8,  8,  6,  9,  6,  8,  2,  3,  1),
  fruit_cat    = c(1,  7,  5,  8,  2,  3,  1,  0,  7,  7,  5,  8,  2),
  gin_bin      = c(1,  0,  1,  0,  1,  0,  0,  1,  0,  0,  1,  0,  1),
  ham_bin      = c(1,  1,  1,  1,  1,  1,  1,  0,  1,  1,  1,  0,  1)
)

head(obs_data)

# The legend for the graph
legend <- data.frame(
  name   = colnames(obs_data),
  title  = c("Alcohol", "Bread",   "Coffee",    "Duck",    "Eggs", "Fruit", "Gin",     "Ham"),
  family = c("Alcohol", "Cereals", "Beverages", "Poultry", "Eggs", "Fruit", "Alcohol", "Meats")
)

# Transform family intro factors?


## ------------------------------------------------------------------------
adjacency_matrix <-  mic_adj_matrix(obs_data)

## ------------------------------------------------------------------------
# Ordinal vs. ordinal
thresh_ord_ord <- boot_simulated_cat_bin("cat", method = "mic", size = 500)

# Binary vs. binary
thresh_bin_bin <- boot_simulated_cat_bin("bin", method = "mic", size = 500)

# Ordinal vs. binary
thresh_ord_bin <- boot_simulated_cat_bin("bincat", method = "mic", size = 500)

## ------------------------------------------------------------------------

cat_var <- c("alcohol_cat", "bread_cat", "coffee_cat", "duck_cat", "eggs_cat",
             "fruit_cat")
bin_var <- c("gin_bin", "ham_bin")

inferred_adj_matrix <- boot_cat_bin(obs_data,
                                    list_cat_var = cat_var,
                                    list_bin_var = bin_var,
                                    method = "mic",
                                    threshold_cat = thresh_ord_ord,
                                    threshold_bin = thresh_bin_bin,
                                    threshold_bin_cat = thresh_ord_bin,
                                    boots = 5000,
                                    show_progress = FALSE)

# Print how many edges have been removed
n_null_before <- (length(which(adjacency_matrix==0))-ncol(obs_data))/2
n_null_after <- (length(which(inferred_adj_matrix==0))-ncol(obs_data))/2
print(paste(n_null_after - n_null_before, "edges have been removed"))

## ------------------------------------------------------------------------
graph1 <- graph_from_matrix(adjacency_matrix, legend, main_title = "My graph", layout = "graphopt")
graph1

## ------------------------------------------------------------------------
# Extract the links and nodes from the adjacency matrix
links_nodes <- links_nodes_from_mat(adjacency_matrix, legend)
  
# Transform negative weights into positive ones
links_nodes$links <- transform(links_nodes$links, weight = abs(weight))
   
# Display the graph
graph2 <- graph_from_links_nodes(links_nodes, main_title = "My graph")
graph2

## ----eval=F--------------------------------------------------------------
#  save_graph(graph1)

## ----message=F-----------------------------------------------------------
library(ggplot2)

custom1 <- graph_from_matrix(adjacency_matrix, legend, main_title = "Node label as name",
                             layout = "graphopt", node_label_title = F, node_label_size = 5)
custom2 <- graph_from_matrix(adjacency_matrix, legend, main_title = "Node type as label",
                             layout = "graphopt", node_type = "label")
custom3 <- graph_from_matrix(adjacency_matrix, legend, main_title = "Grid layout",
                             layout = "grid", node_label_size = 5)
custom4 <- graph_from_matrix(adjacency_matrix, legend, main_title = "Circle layout",
                             layout = "circle", node_label_size = 5)

## ----eval=F--------------------------------------------------------------
#  custom1$net
#  custom2$net
#  custom3$net
#  custom4$net

## ----echo=F--------------------------------------------------------------
# Cookbook for R, simplified here
multiplot <- function(..., cols=2) {
  library(grid)
  plots <- list(...)
  numPlots = length(plots)
  layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                   ncol = cols, nrow = ceiling(numPlots/cols))
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
  for (i in 1:numPlots) {
    matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
    
    print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                    layout.pos.col = matchidx$col))
  }
}

multiplot(custom1$net + theme(legend.position="none"),
          custom2$net + theme(legend.position="none"),
          custom3$net + theme(legend.position="none"),
          custom4$net + theme(legend.position="none"))

## ------------------------------------------------------------------------

# New set of observation data
obs_data_2 <- matrix(c(round(runif(78, 0, 13)), round(runif(26))), nrow = 13, ncol = 8)
colnames(obs_data_2) <- colnames(obs_data)

# Compute the MIC adjacency matrix
adjacency_matrix_2 <- mic_adj_matrix(obs_data_2)

graph2 <- graph_from_matrix(adjacency_matrix_2, legend, main_title = "My graph 2",
                            layout = "graphopt")

## ---- fig.width = 7, fig.height=5----------------------------------------
comp1_2 <- compare_graphs(graph1, graph2, position = "horizontal")
comp1_2

## ----eval=F--------------------------------------------------------------
#  save_graph(comp1_2)

