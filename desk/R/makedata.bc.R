makedata.bc = function(lambda.x = 1, lambda.y = 1, a = 0, x.max = 5, n = 200, sigma = 1, seed = NULL){
 set.seed(seed)
 x = def.log(seq(from = 1, to = x.max + 1, length.out = n), lambda = lambda.x)
 y = def.exp(x + a + rnorm(n, sd = sigma), lambda = lambda.y)
 return(data.frame(x = x, y = y))
}
