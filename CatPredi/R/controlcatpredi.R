controlcatpredi <-
function(min.p.cat = 1,
	grid = 100,
	B = 50,
	eps=0.001,
  b.method = c("ncoutcome","coutcome"),
  print.gen = 0)		
	list(min.p.cat = min.p.cat, grid = grid, B = B, eps = eps, b.method = match.arg(b.method), print.gen = print.gen)
