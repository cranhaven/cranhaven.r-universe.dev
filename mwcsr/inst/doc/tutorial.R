## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(cache = FALSE)

## ----eval=F-------------------------------------------------------------------
# library(devtools)
# install_github("ctlab/mwcsr")

## ----message=FALSE------------------------------------------------------------
library(mwcsr)
library(igraph)

## -----------------------------------------------------------------------------
data("mwcs_example")
print(mwcs_example)
summary(V(mwcs_example)$weight)

## -----------------------------------------------------------------------------
rcsolver <- rmwcs_solver()

## -----------------------------------------------------------------------------
m <- solve_mwcsp(rcsolver, mwcs_example)
print(m$graph)
print(m$weight)

## -----------------------------------------------------------------------------
get_instance_type(mwcs_example)

## -----------------------------------------------------------------------------
mwcs_example
summary(V(mwcs_example)$weight)

## -----------------------------------------------------------------------------
budget_mwcs_example <- mwcs_example
set.seed(42)
V(budget_mwcs_example)$budget_cost <- runif(vcount(budget_mwcs_example))
get_instance_type(budget_mwcs_example)

## -----------------------------------------------------------------------------
data(gmwcs_example)
gmwcs_example
summary(V(gmwcs_example)$weight)
summary(E(gmwcs_example)$weight)

## -----------------------------------------------------------------------------
data("sgmwcs_example")
sgmwcs_example
str(V(sgmwcs_example)$signal)
str(E(sgmwcs_example)$signal)
head(sgmwcs_example$signals)

## -----------------------------------------------------------------------------
data("gatom_example")
print(gatom_example)

## -----------------------------------------------------------------------------
get_instance_type(gatom_example)

## -----------------------------------------------------------------------------
gatom_instance <- normalize_sgmwcs_instance(gatom_example)
get_instance_type(gatom_instance)

## -----------------------------------------------------------------------------
gatom_instance <- normalize_sgmwcs_instance(gatom_example,
                                            nodes.weight.column = "weight",
                                            edges.weight.column = "weight",
                                            nodes.group.by = "signal",
                                            edges.group.by = "signal", 
                                            group.only.positive = TRUE)

## -----------------------------------------------------------------------------
rmwcs <- rmwcs_solver()
m <- solve_mwcsp(rmwcs, mwcs_example)
print(m$weight)
print(m$solved_to_optimality)

## -----------------------------------------------------------------------------
 m <- solve_mwcsp(rmwcs, mwcs_example, max_cardinality = 10)
 print(vcount(m$graph))
 print(m$weight)

## -----------------------------------------------------------------------------
 m <- solve_mwcsp(rmwcs, budget_mwcs_example, budget = 10)
 print(sum(V(m$graph)$budget_cost))
 print(m$weight)

## -----------------------------------------------------------------------------
  rnc <- rnc_solver()
  m <- solve_mwcsp(rnc, gmwcs_example)
  print(m$weight)
  print(m$solved_to_optimality)

## -----------------------------------------------------------------------------
  rnc <- rnc_solver()
  m <- solve_mwcsp(rnc, sgmwcs_example)
  print(m$weight)

## -----------------------------------------------------------------------------
m <- NULL
for (i in 0:15) {
  asolver <- annealing_solver(schedule = "boltzmann", initial_temperature = 8.0 / (2 ** i),
                                final_temperature = 1 / (2 ** i))
  if (i != 0) {
    m <- solve_mwcsp(asolver, gmwcs_example, warm_start = m)
  } else {
    m <- solve_mwcsp(asolver, gmwcs_example)
  }
  print(m$weight)
}

## -----------------------------------------------------------------------------
mst_solver <- virgo_solver(cplex_dir=NULL)
m <- solve_mwcsp(mst_solver, sgmwcs_example)
print(m$weight)
print(m$solved_to_optimality)

## ----message=FALSE,eval=FALSE-------------------------------------------------
# scip <- scipjack_solver(scipstp_bin=Sys.which("scipstp"))
# sol <- solve_mwcsp(scip, mwcs_example)

## ----message=FALSE------------------------------------------------------------
BioNetInstalled <- FALSE
if (requireNamespace("BioNet") && requireNamespace("DLBCL")) {
    BioNetInstalled <- TRUE    
}

## ----message=FALSE------------------------------------------------------------
if (BioNetInstalled) {
    library("BioNet")
    library("DLBCL")
    data(dataLym)
    data(interactome)
    pvals <- cbind(t = dataLym$t.pval, s = dataLym$s.pval)
    rownames(pvals) <- dataLym$label
    pval <- aggrPvals(pvals, order = 2, plot = FALSE)
    logFC <- dataLym$diff
    names(logFC) <- dataLym$label
    subnet <- subNetwork(dataLym$label, interactome)
    subnet <- rmSelfLoops(subnet)
    fb <- fitBumModel(pval, plot = FALSE)
    scores <- scoreNodes(subnet, fb, fdr = 0.001)
}

## -----------------------------------------------------------------------------
if (BioNetInstalled) {
    subnet
    str(scores)
}

## -----------------------------------------------------------------------------
if (BioNetInstalled) {
    bionet_h <- runFastHeinz(subnet, scores)
    plotModule(bionet_h, scores=scores, diff.expr=logFC)
    sum(scores[nodes(bionet_h)])
}

## -----------------------------------------------------------------------------
if (BioNetInstalled) { 
    bionet_example <- igraph.from.graphNEL(subnet, weight=FALSE) # ignoring edge weights of 1
    V(bionet_example)$weight <- scores[V(bionet_example)]
    get_instance_type(bionet_example)
}

## -----------------------------------------------------------------------------
if (BioNetInstalled) {
    rmwcs <- rmwcs_solver()
    bionet_m <- solve_mwcsp(rmwcs, bionet_example)
    plotModule(bionet_m$graph, scores=scores, diff.expr=logFC)
}

## -----------------------------------------------------------------------------
if (BioNetInstalled) {
    print(bionet_m$weight)
}

