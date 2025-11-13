controlcatpredi.survival <-
function(
	min.p.cat = 5,
	grid = 100,
	B = 50,
  b.method = c("ncoutcome","coutcome"),
  print.gen = 0)	
	list(min.p.cat = min.p.cat, grid = grid, B = B , b.method = match.arg(b.method), print.gen = print.gen )
