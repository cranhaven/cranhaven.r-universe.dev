## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
#   collapse = TRUE,
  comment = "#>"
)
old <- options(digits = 3)

## ----CRAN installation, eval = FALSE------------------------------------------
#  install.packages("netcom")

## ----GitHub installation, eval = FALSE----------------------------------------
#  devtools::install_github("langendorfr/netcom")

## ----Setup, message = FALSE---------------------------------------------------
library(netcom)     # This library
library(tibble)     # Data format
library(dplyr)      # Data processing
library(reshape2)   # Data manipulation
library(expm)       # Matrix multiplication
library(igraph)     # General puprose network library
library(ggplot2)    # Plotting using the grammar of graphics
library(ggraph)     # Graph plotting extension for ggplot2
library(ggfortify)  # Statistical plotting extension for ggplot2

## ----Easily simulate dynamic mixture network, fig.height = 4, fig.width = 4, fig.align = "left"----
## Start by creating a sequence of network evolutions. There are four components to this sequence that can be defined for each step in the network's evolution, or once which will be used for every step in the newtwork's evolution.
mechanism <- c(
    rep("ER", 7),
    rep("PA", 2),
    rep("ER", 3)
)

kind <- c(
    rep("grow", 7),
    rep("rewire", 2),
    rep("grow", 3)
)

parameter <- c(
    rep(0.3, 7),
    rep(2, 2),
    c(0.2, 0.4, 0.3)
)

directed <- c(
    rep(TRUE, 7),
    rep(FALSE, 2),
    c(FALSE, FALSE, TRUE)
)

## Simulate a network according to the rules of this system evolution.
network <- netcom::make_Mixture(mechanism = mechanism, kind = kind, parameter = parameter, directed = directed)

## ----Plot te network, fig.height = 4, fig.width = 4, fig.align = "left"-------
## Lastly, plot the network you've simulated.
network %>% 
    reshape2::melt() %>% 
    dplyr::filter(value == 1) %>% 
    ggraph::ggraph(layout = "stress") + 
        theme_void() +
        geom_node_point(size = 10,
                        color = "slateblue") +
        geom_edge_link(arrow = arrow(type = "closed",
                                     length = unit(3, "mm")),
                       start_cap = circle(4, "mm"),
                       end_cap = circle(4, "mm"))

## ----Align networks, fig.height = 4, fig.width = 4, fig.align = "left"--------
## Create two sets of 100 networks each, all with 20 nodes. One of these sets is grown
## according to the Duplication and Divergence mechanism (DD). The other is also grown in this
## manner, but with one randomly rewired node for every three nodes grown, mimicking a 
## disturbed system.
num_networks <- 100

## First set of undisturbed networks.
networks_undisturbed <- list()
for (net in 1:num_networks) {
    networks_undisturbed[[net]] = netcom::make_Mixture(
        mechanism = rep("DD", 20), 
        parameter = 0.2, 
        directed = TRUE
    )
}

## Second set of disturbed networks
networks_disturbed <- list()
for (net in 1:num_networks) {
    networks_disturbed[[net]] = netcom::make_Mixture(
        mechanism = rep(c("DD", "DD", "DD", "ER"), 5),
        parameter = rep(c(0.2, 0.2, 0.2, 0.76), 5),
        kind = rep(c("grow", "grow", "grow", "rewire"), 5),
        directed = TRUE
    )
}

## Pairwise compare all of the networks to each other 
networks <- c(networks_undisturbed, networks_disturbed)
comparisons <- netcom::compare(networks, method = "align")

## PCA of comparisons
stats::prcomp(comparisons) %>%
    ggplot2::autoplot(
        data = tibble(
            Kind = c(
                rep("Undisturbed", num_networks),
                rep("Disturbed", num_networks)
            )
        ), 
        colour = "Kind",
        size = 5
    ) + 
    theme_bw() +
    theme(
        legend.position = "bottom",
        legend.title = element_blank()
    )

## ----Classify networks, fig.height = 4, fig.width = 4, fig.align = "left"-----
## Classification of an undisturbed Small-World network made above
netcom::classify(
    network = networks_undisturbed[[1]], 
    processes = c("SW", "DD", "NM", "PA", "ER"),
    directed = TRUE,
    mechanism_kind = "grow"
)

## Classification of a disturbed Small-World network made above
netcom::classify(
    network = networks_disturbed[[1]], 
    processes = c("SW", "DD", "NM", "PA", "ER"),
    directed = TRUE,
    mechanism_kind = "grow"
)

## ---- include = FALSE---------------------------------------------------------
options(old)

