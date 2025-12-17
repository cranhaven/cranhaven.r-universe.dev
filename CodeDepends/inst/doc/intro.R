## ----scriptnodeinfo-----------------------------------------------------------
library(CodeDepends)
getInputs(quote(x <- y + rnorm(10, sd = z)))


## ----updateexprs, eval=FALSE--------------------------------------------------
#  x = x + 5
#  rownames(x) = 5
#  x[1:3] = 5
#  x  = lapply(1:5, function(i) x[i]^2)
#  x$y = 5

## ----custhandler--------------------------------------------------------------

col = inputCollector(library = function(e, collector, ...) {
    print(paste("Hello", asVarName(e)))
    defaultFuncHandlers$library(e, collector, ...)
})
getInputs(quote(library(CodeDepends)), collector = col)

## ----variablegraph------------------------------------------------------------

 f = system.file("samples", "results-multi.R", package = "CodeDepends")
 sc = readScript(f)
 g = makeVariableGraph( info = getInputs(sc))
 if(require(Rgraphviz))
   plot(g)

## ----callgraphs---------------------------------------------------------------
  gg = makeCallGraph("package:CodeDepends")
  if(require(Rgraphviz)) {
     gg = layoutGraph(gg, layoutType = "circo")
     graph.par(list(nodes = list(fontsize=55)))
     renderGraph(gg) ## could also call plot directly
  } 

## ----timelines----------------------------------------------------------------
f = system.file("samples", "results-multi.R", package = "CodeDepends")
sc = readScript(f)
dtm = getDetailedTimelines(sc, getInputs(sc))
plot(dtm)

 # A big/long function
info = getInputs(arima0)
dtm = getDetailedTimelines(info = info)
plot(dtm, var.cex = .7, mar = 4, srt = 30)

