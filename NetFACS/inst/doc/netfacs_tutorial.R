## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "man/figures/README-",
  out.width = "100%", 
  message = FALSE, 
  warning = FALSE
)

## ----setup, include = FALSE---------------------------------------------------
library(NetFACS)
library(dplyr)
library(ggplot2)
library(knitr)

## ----load.netfacs, echo = T, message=F, eval = F------------------------------
#  # install NetFACS from CRAN
#  install.packages("NetFACS")
#  
#  # read library
#  library(NetFACS)

## ----instal.dev, echo = T, message=F, eval = F--------------------------------
#  # install NetFACS from GitHub
#  devtools::install_github("NetFACS/NetFACS")

## ----read.data, echo = T------------------------------------------------------
data("letternet") # this is the Manifesto #
data("emotions_set") # this is the CK Database #

## ----netfacs.table, echo=FALSE------------------------------------------------
kable(
  head(angry.face$result[angry.face$result$count > 0,], 20),
  row.names = FALSE,
  align = "c",
  caption = "Top rows of the netfacs function results"
)

## ----first.level.table, echo=FALSE--------------------------------------------
kable(anger.aus[order(-1 * anger.aus$effect.size),],
      align = "c",
      row.names = FALSE,
      caption = "Result of netfacs_extract for single elements")

## ----element.plot, fig.width=6, fig.height=4, fig.align='center', message=F----
# create plot showing the importance of each AU for the angry faces

element.plot(netfacs.data = angry.face)

## ----distribution.plot, fig.width=6, fig.height=4, fig.align='center', message=F----
# create plot showing the distribution of the null probabilities and how the observed probability compares

distribution.plot(netfacs.data = angry.face)$"4"
distribution.plot(netfacs.data = angry.face)$"9"

## ----third.level.anger--------------------------------------------------------
# extract information for three-element-combinations in angry faces

anger.aus3 <- netfacs_extract(
  netfacs.data = angry.face,
  combination.size = 3, # only looking at combinations with 3 elements (here, Action Units)
  min.count = 5, # minimum number of times that the combination should occur
  min.prob = 0, # minimum observed probability of the combination
  significance = 0.01 # significance level we are interested in
) 

## ----third.level.table, echo=FALSE--------------------------------------------
kable(head(anger.aus3[order(-1 * anger.aus3$effect.size),]),
      align = "c",
      row.names = FALSE,
      caption = "Results of netfacs_extract function for combinations of three elements")

## ----element.specificity------------------------------------------------------
spec <- specificity(angry.face)
spec.increase <- specificity_increase(spec)

## ----element.specificity.table, echo=FALSE, align = "c"-----------------------
kable(
  spec.increase[1],
  align = "c",
  row.names = FALSE,
  digits = 2,
  booktabs = TRUE,
  caption = "Results of the specificity increase in combinations due to to inclusion of each element"
)

## ----conditional.probs--------------------------------------------------------
conditional.probs <- conditional_probabilities(angry.face)

## ----cond.probs.table, echo=FALSE---------------------------------------------
conditional.probs %>% 
  slice(c(1:6, 30:36)) %>% 
  kable(
  row.names = FALSE,
  align = "c",
  caption = "Conditional probabilities for a subset of dyadic combinations"
)

## ----multi.facs---------------------------------------------------------------
multi.facs <- netfacs_multiple(
  data = au.data,
  condition = au.info$emotion,
  ran.trials = 1000,
  combination.size = 2,
  use_parallel = TRUE
)
# calculate element specificity
multi.spec <- specificity(multi.facs)

## ----overlap, fig.height=8, fig.width=10, fig.align='center', message=F-------
overlap.net <- overlap_network(
  multi.spec,
  min.prob = 0, # minimum probability of a connection to be included
  min.count = 3, # minimum count of co-occurrences for a connection to be included
  significance = 0.01, # significance level for combinations to be considered
  clusters = FALSE, # should the bipartite network be clustered
  plot.bubbles = TRUE,
)

plot(overlap.net$specificity)
plot(overlap.net$occurrence)

## ----conditional.plot, fig.height=8, fig.width=10, fig.align='center', message=F----
conditional.probs <- network_conditional(
  netfacs.data = angry.face,
  min.prob = 0.5,
  min.count = 5,
  ignore.element = NULL,
  plot.bubbles = TRUE
)

# plot conditional probabilities
conditional.probs$plot

## ----angry.net----------------------------------------------------------------
angry.net <- netfacs_network(
  netfacs.data = angry.face,
  link = "unweighted", # edges are linked for significant results only
  significance = 0.01,
  min.count = 3, # remove rare elements as they might be random variation
  min.prob = 0
)

## ----angry.plot, fig.width=8, fig.height=8, fig.align='center', message=F-----
network_plot(
  netfacs.graph = angry.net,
  title = "angry network",
  clusters = FALSE,
  plot.bubbles = TRUE,
  hide.unconnected = TRUE
)

## ----multi.net----------------------------------------------------------------
multi.net <- multiple_netfacs_network(
  multi.facs,
  link = "weighted", # network contains edges where significantly connected
  significance = 0.01,
  min.count = 3, # again remove rare connections
  min.prob = 0
)

## ----multi.plot, fig.width=10, fig.height=8, fig.align='center', message=F----
multiple_network_plot(multi.net)

## ----all.face, cache=F--------------------------------------------------------
all.face <-
  netfacs(
    data = au.data,
    condition = NULL,
    ran.trials = 1000,
    combination.size = 2,
    use_parallel = TRUE
  )
all.net <-
  netfacs_network(all.face,
                  min.count = 3,
                  link = "unweighted")

## ----all.plot, fig.width=8, fig.height=8, fig.align='center', message=F-------
network_plot(
  all.net,
  title = "all network with clusters",
  clusters = TRUE,
  plot.bubbles = TRUE
)

## ---- network.summary---------------------------------------------------------
net.sum <- network_summary(angry.net)

## ----net.sum.table, echo=FALSE------------------------------------------------
# show only a number of the conditional probabilities
kable(
  net.sum,
  align = "c",
  row.names = FALSE,
  digits = 3,
  caption = "Network centrality measures for angry faces"
)

## ----network.summary.graph----------------------------------------------------
net.sum.graph <- network_summary_graph(angry.net)

## ----net.graph.table, echo=FALSE----------------------------------------------
kable(
  net.sum.graph,
  align = "c",
  row.names = FALSE,
  digits = 3,
  caption = "Network graph measures for anry faces"
)

## ----multinet.summary---------------------------------------------------------
xx <- lapply(multi.net, function(x) {
  network_summary_graph(x)
})
xx <- do.call(rbind, xx)
xx <- cbind(emotion = names(multi.net), xx)

## ----net.sum.all.table, echo=FALSE--------------------------------------------
kable(
  xx,
  align = "c",
  row.names = FALSE,
  digits = 3,
  caption = "Network graph measures for all faces"
)

## ----event.size.angry---------------------------------------------------------
event.size.angry <- angry.face$event.size.information
size.plot <- event.size.plot(netfacs.data = angry.face)

## ----event.size angry.table, echo=FALSE---------------------------------------
kable(
  event.size.angry,
  align = "c",
  row.names = FALSE,
  digits = 2,
  caption = "Combination sizes of facial expressions in the angry condition"
)

## ----size.plot, fig.width=10, fig.height=8, fig.align='center', message=F, echo=F----
plot(size.plot)

## ----happy, echo=F------------------------------------------------------------
happy.face <-
  netfacs(
    data = au.data,
    condition = au.info$emotion,
    test.condition = "happy",
    ran.trials = 1000,
    use_parallel = TRUE
  )

## ----event.size.happy, echo = F-----------------------------------------------
kable(
  happy.face$event.size.information,
  align = "c",
  row.names = FALSE,
  digits = 2,
  caption = "Combination sizes of happy expressions in the angry condition"
)

## ----entropy------------------------------------------------------------------
xx <- lapply(multi.facs, function(x) {
  entropy_overall(x)
})
xx <- do.call(rbind, xx)
xx <- cbind(emotion = names(multi.facs), xx)

## ----entropy.results, echo = F------------------------------------------------
kable(
  xx,
  align = "c",
  row.names = FALSE,
  digits = 3,
  caption = "Ratios between expected and observed entropies in different emotions"
)

## ---- eval = F----------------------------------------------------------------
#  # create an edge table
#  anger.tab <- igraph::as_data_frame(multi.net$anger)
#  
#  # create an adjacency matrix
#  anger.adj.mat <- as.matrix(igraph::as_adjacency_matrix(multi.net$anger))
#  
#  # save as CSV file
#  # write.csv(anger.tab, "anger_net_tab.csv")
#  # write.csv(anger.adj.mat, "adj_net_mat.csv")

