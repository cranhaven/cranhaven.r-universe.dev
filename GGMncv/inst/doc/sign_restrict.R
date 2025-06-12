## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, warning=FALSE, message=FALSE--------------------------------------
library(GGMncv)
library(corrplot)

## -----------------------------------------------------------------------------
corrplot::corrplot(cor(ptsd), method = "shade")

## -----------------------------------------------------------------------------
pcors <- -cov2cor(solve(cor(ptsd))) + diag(ncol(ptsd))

corrplot::corrplot(pcors, 
                   method = "shade")

## ---- message=FALSE, warning=FALSE--------------------------------------------
# fit model
fit <- GGMncv::ggmncv(cor(ptsd),
                      n = nrow(ptsd),
                      progress = FALSE,
                      penalty = "atan")

# plot graph
plot(GGMncv::get_graph(fit),
     edge_magnify = 10,
     node_names = colnames(ptsd))

## -----------------------------------------------------------------------------
# set negatives to zero (sign restriction)
adj_new <- ifelse(fit$P <= 0, 0, 1)

check_zeros <- TRUE

# track trys
iter <- 0

# iterate until all positive
while(check_zeros){
  iter <- iter + 1
  fit_new <- GGMncv::constrained(cor(ptsd), adj = adj_new)
  check_zeros <- any(fit_new$wadj < 0)
  adj_new <- ifelse(fit_new$wadj <= 0, 0, 1)
}

# make graph object
new_graph <- list(P = fit_new$wadj,
                  adj = adj_new)
class(new_graph) <- "graph"

# plot graph
plot(new_graph,
     edge_magnify = 10,
     node_names = colnames(ptsd))

## -----------------------------------------------------------------------------
R <- cor(ptsd)
n <- nrow(ptsd)
p <- ncol(ptsd)

# store fitted models
fit <- ggmncv(R = R, 
              n = n, 
              progress = FALSE, 
              store = TRUE, 
              n_lambda = 50)

# all fitted models
# sol: solution
sol_path <- fit$fitted_models

# storage
bics <- NA
Thetas <- list()

for(i in seq_along(sol_path)){
  
  # positive in wi is a negative partial
  adj_new <- ifelse(sol_path[[i]]$wi >= 0, 0, 1)
  
  check_zeros <- TRUE
  
  # track trys
  iter <- 0
  
  # iterate until all positive
  while(check_zeros){
    iter <- iter + 1
    fit_new <- GGMncv::constrained(R, adj = adj_new)
    check_zeros <- any(fit_new$wadj < 0)
    adj_new <- ifelse(fit_new$wadj <= 0, 0, 1)
}
  
  bics[i] <- GGMncv:::gic_helper(
    Theta = fit_new$Theta,
    R = R,
    n = n,
    p = p,
    type = "bic",
    edges = sum(fit_new$Theta[upper.tri(fit_new$Theta)] != 0)
  )
  
  Thetas[[i]] <- fit_new$Theta
}

# select via minimizing bic
# (then convert to partial correlatons)
pcors <- -(cov2cor(Thetas[[which.min(bics)]]) - diag(p))

# make graph class
new_graph <- list(P = pcors,
                  adj = ifelse(pcors == 0, 0, 1))
class(new_graph) <- "graph"

# plot graph
plot(new_graph,
     edge_magnify = 10,
     node_names = colnames(ptsd))

