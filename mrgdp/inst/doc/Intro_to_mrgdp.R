## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(mrgdp)

## ----eval=FALSE, include=FALSE------------------------------------------------
#  library(DiagrammeR)
#  
#  DiagrammeR::grViz("digraph {
#  
#  # initiate graph
#  graph [layout = dot, rankdir = LR, label = 'mrgdp package process\n\n',labelloc = t]
#  
#  # global node settings
#  node [shape = rectangle, style = filled, fillcolor = Linen]
#  
#  # label nodes
#  data1 [label = 'INEGI \n Economic \n Census \n  URL', shape = folder, fillcolor = Beige]
#  mrgdp [label =  'mrgdp \n package \n functions']
#  fa [label= 'function \n arguments' , shape= parallelogram]
#  economic_census [label = 'economic_census() ']
#  economic_units [label = 'economic_units() ']
#  pbtm [label = 'PBTM_04_19() ']
#  results [label= 'Results', shape = diamond]
#  results2 [label= 'Results2', shape = diamond]
#  results3 [label= 'Results3', shape = diamond]
#  results4 [label= 'Results4', shape = diamond]
#  ecd [label='Economic \n census \n downloaded', shape=cylinder]
#  pbtmun [label= 'pbt_mun()' ]
#  pbtmun2 [label = 'Municpal \n gross \n domestic \n product', shape=cylinder]
#  ecu [label='Economic \n units \n downloaded', shape=cylinder]
#  pbtmun4 [label = 'Municpal \n gross \n domestic \n product \n panel', shape=cylinder]
#  
#  
#  # edge definitions with the node IDs
#  {data1}  -> fa -> mrgdp -> {economic_census economic_units pbtm}
#  economic_census -> results
#  results -> {good bad}
#  bad -> {'wrong arguments'} -> fa
#  good -> ecd -> pbtmun
#  pbtmun -> results2 -> {good2 bad2}
#  good2 -> pbtmun2
#  bad2 -> {'wrong arguments'} -> fa
#  economic_units -> results3 ->{good3, bad3}
#  good3 -> ecu
#  bad3 -> {'wrong arguments'} -> fa
#  pbtm -> results4 -> {good4 bad4}
#  good4 -> pbtmun4
#  bad4 -> {'wrong arguments'} -> fa
#  }")

## ----eval=FALSE, include=FALSE------------------------------------------------
#  DiagrammeR::grViz("digraph {
#  
#  # initiate graph
#  graph [layout = twopi, rankdir = RL, label = 'Results_i internal process',labelloc = t]
#  
#  # global node settings
#  node [shape = box, style = filled, fillcolor = Linen]
#  
#  # label nodes
#  functions [label='Select mrgdp function', shape=parallelogram]
#  arguments [label='function arguments', shape=parallelogram]
#  conect [label = 'INEGI Economic \n Census URL conection']
#  download [label= 'Download \n Economic Census in \n temporal memory']
#  filter [label= 'Filter data \n  by \n federal entitie \n code']
#  build [label='build a dataframe \n of filtered data', shape=cylinder]
#  clean [label='clean temporal memory']
#  
#  # edge definitions with the node IDs
#  functions -> arguments -> conect -> download ->filter -> build -> clean
#  }")

## -----------------------------------------------------------------------------
#data <- economic_census(2004, "ags")
#names(data)

## -----------------------------------------------------------------------------
#ags04 <- pbt_mun(data)
#tail(ags04)

## -----------------------------------------------------------------------------
#ags_04_19 <- PIBM_04_19(data, "ags")
#tail(ags_04_19)

