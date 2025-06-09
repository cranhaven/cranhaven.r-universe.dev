## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(abn)
library(lme4)
library(Rgraphviz)

# Set seed for reproducibility
set.seed(123)

## -----------------------------------------------------------------------------
n_groups <- 5

# Number of observations per group
n_obs_per_group <- 1000

# Total number of observations
n_obs <- n_groups * n_obs_per_group

# Simulate group effects
group <- factor(rep(1:n_groups, each = n_obs_per_group))
group_effects <- rnorm(n_groups)

# Simulate variables
G1 <- rnorm(n_obs) + group_effects[group]
B1 <- rbinom(n_obs, 1, plogis(group_effects[group]))
G2 <- 1.5 * B1 + 0.7 * G1 + rnorm(n_obs) + group_effects[group]
B2 <- rbinom(n_obs, 1, plogis(2 * G2 + group_effects[group]))

# Normalize the continuous variables
G1 <- (G1 - mean(G1)) / sd(G1)
G2 <- (G2 - mean(G2)) / sd(G2)

# Create data frame
data <- data.frame(group = group, G1 = G1, G2 = G2, B1 = factor(B1), B2 = factor(B2))

# Look at data
str(data)
summary(data)

## ----echo=FALSE---------------------------------------------------------------
# Define the nodes and edges of the graph
nodes <- c("G1", "B1", "G2", "B2", "group_effects")
edges <- c("group_effects", "G1",
           "group_effects", "G2",
           "group_effects", "B1",
           "group_effects", "B2",
           "G1", "G2",
           "B1", "G2",
           "G2", "B2")

# Create a graphNEL object with specified nodes and edges
graph <- new("graphNEL", nodes=nodes, edgemode="directed")
for (i in seq(1, length(edges), by=2)) {
  graph <- addEdge(edges[i], edges[i+1], graph)
}

# Layout the graph
g <- layoutGraph(graph)

# Set the node attributes
nodeRenderInfo(g) <- list(
  shape = c(G1="ellipse", B1="box", G2="ellipse", B2="box", group_effects="box"),
  fill = c(G1="lightblue", B1="lightgreen", G2="lightblue", B2="lightgreen", group_effects="lightgrey")
)

# set edge attributes
edgeRenderInfo(g) <- list(
  col = c("G1~G2"="black", "B1~G2"="black", "G2~B2"="black", 
          "group_effects~G1"="lightgrey", "group_effects~G2"="lightgrey", 
          "group_effects~B1"="lightgrey", "group_effects~B2"="lightgrey"))

renderGraph(g)

## -----------------------------------------------------------------------------
# Build the score cache
score_cache <- buildScoreCache(data.df = data,
                               data.dists = list(G1 = "gaussian", 
                                                 G2 = "gaussian", 
                                                 B1 = "binomial", 
                                                 B2 = "binomial"),
                               group.var = "group",
                               max.parents = 2,
                               method = "mle")

# Structure learning
mp_dag <- mostProbable(score.cache = score_cache)

# Plot the DAG
plot(mp_dag)

## -----------------------------------------------------------------------------
# Parameter estimation
abn_fit <- fitAbn(object = mp_dag,
                  method = "mle")

# Print the fitted model
print(abn_fit)

## -----------------------------------------------------------------------------
# Fit a lmer model for G2
model_g2 <- lmer(G2 ~ G1 + B1 + (1 | group), data = data)

# Print summary
summary(model_g2)

## -----------------------------------------------------------------------------
# Fit a glmer model for B2
model_b2 <- glmer(B2 ~ G1 + G2 + B1 + (1 | group), data = data, family = binomial)

# Print summary
summary(model_b2)

