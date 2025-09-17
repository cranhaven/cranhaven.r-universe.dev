getFormulaInfo = function(formula, data) {
  
  call = match.call()
  matchCall = match.call(expand.dots = FALSE)
  namesMatchCall = names(matchCall)
  match = match(c("formula", "data"), namesMatchCall, 0L)
  matchCall = matchCall[c(1L, match)]
  matchCall[[1]] = quote(stats::model.frame)
  matchCall = eval(matchCall, parent.frame())
  matchCallAttributes = attr(matchCall, "terms")
  
  return(list(call = call, matchCall = matchCall, namesMatchCall = namesMatchCall, match = match, matchCallAttributes = matchCallAttributes))
  
}