f2apply <-
function( x, y, fun, knot.n=10, I.O.plot="TRUE", ... ){

x.input <- x  #This is need for plot x at the end
y.input <- y  #This is need for plot y at the end

if( class(x) == "numeric" ){
       x <- x.input.fuzzy <- TriangularFuzzyNumber(x,x,x)
       }
if( class(x) == "TriangularFuzzyNumber" | class(x) == "TrapezoidalFuzzyNumber" ){
       x.input.fuzzy <- x
       x <- as.PiecewiseLinearFuzzyNumber(x, knot.n)
       }
if( class(x) == "FuzzyNumber"  |  class(x) == "PowerFuzzyNumber" | class(x) == "PiecewiseLinearFuzzyNumber" ){
       x.input.fuzzy <- x
       x <- piecewiseLinearApproximation(x, method='Naive')
       }

if( class(y) == "numeric" ){
       y <- y.input.fuzzy <- TriangularFuzzyNumber(y,y,y)
       }
if( class(y) == "TriangularFuzzyNumber" | class(y) == "TrapezoidalFuzzyNumber" ){
       y.input.fuzzy <- y
       y <- as.PiecewiseLinearFuzzyNumber(y, knot.n)
       }
if( class(y) == "FuzzyNumber"  |  class(y) == "PowerFuzzyNumber" | class(y) == "PiecewiseLinearFuzzyNumber" ){
       y.input.fuzzy <- y
       y <- piecewiseLinearApproximation(y, method='Naive')
       }

  step.x = length(supp(x)) / 30
  step.y = length(supp(y)) / 30

if( class(x.input) == "numeric" ){ is.inc.on.x <- TRUE  
                                   is.dec.on.x <- FALSE }
  else{ 
      is.inc.on.x = is.increasing.on.x(fun, x.bound=supp(x), y.bound=supp(y), step.x)
      is.dec.on.x = is.decreasing.on.x(fun, x.bound=supp(x), y.bound=supp(y), step.x)
      }
if( class(y.input) == "numeric" ){ is.inc.on.y <- TRUE  
                                   is.dec.on.y <- FALSE }
  else{ 
      is.inc.on.y = is.increasing.on.y(fun, x.bound=supp(x), y.bound=supp(y), step.y)
      is.dec.on.y = is.decreasing.on.y(fun, x.bound=supp(x), y.bound=supp(y), step.y)
      }


  if(( is.inc.on.x == TRUE ) &
     ( is.inc.on.y == TRUE ) ) 
          {
           fun.rep = "fun is an increasing function from x and y on introduced bounds" 
           L.result = fun( alphacut(x.input.fuzzy, seq(0, 1, len=knot.n))[,"L"] ,
                           alphacut(y.input.fuzzy, seq(0, 1, len=knot.n))[,"L"] )
           U.result = fun( alphacut(x.input.fuzzy, seq(0, 1, len=knot.n))[,"U"] ,
                           alphacut(y.input.fuzzy, seq(0, 1, len=knot.n))[,"U"] )
           result = c(L.result, U.result[length(U.result):1])
          }
  else{
  if(( is.dec.on.x == TRUE ) &
     ( is.inc.on.y == TRUE ) ) 
          {
           fun.rep = "fun is a decreasing function on x and increasing function on y on introduced bounds" 
           L.result = fun( alphacut(x.input.fuzzy, seq(0, 1, len=knot.n))[,"U"] ,
                           alphacut(y.input.fuzzy, seq(0, 1, len=knot.n))[,"L"] )
           U.result = fun( alphacut(x.input.fuzzy, seq(0, 1, len=knot.n))[,"L"] ,
                           alphacut(y.input.fuzzy, seq(0, 1, len=knot.n))[,"U"] )
           result = c(L.result, U.result[length(U.result):1])
          }
  else{
  if(( is.inc.on.x == TRUE ) &
     ( is.dec.on.y == TRUE ) ) 
          {
           fun.rep = "fun is an increasing function on x and decreasing function on y on introduced bounds" 
           L.result = fun( alphacut(x.input.fuzzy, seq(0, 1, len=knot.n))[,"L"] ,
                           alphacut(y.input.fuzzy, seq(0, 1, len=knot.n))[,"U"] )
           U.result = fun( alphacut(x.input.fuzzy, seq(0, 1, len=knot.n))[,"U"] ,
                           alphacut(y.input.fuzzy, seq(0, 1, len=knot.n))[,"L"] )
           result = c(L.result, U.result[length(U.result):1])
          }
  else{
  if(( is.dec.on.x == TRUE ) &
     ( is.dec.on.y == TRUE ) ) 
          {
           fun.rep = "fun is a decreasing function from x and y on introduced bounds"
           L.result = fun( alphacut(x.input.fuzzy, seq(0, 1, len=knot.n))[,"U"] ,
                           alphacut(y.input.fuzzy, seq(0, 1, len=knot.n))[,"U"] )
           U.result = fun( alphacut(x.input.fuzzy, seq(0, 1, len=knot.n))[,"L"] ,
                           alphacut(y.input.fuzzy, seq(0, 1, len=knot.n))[,"L"] )
           result = c(L.result, U.result[length(U.result):1])
          }
  else{
    return( print("fun is not a monoton function on x and y for the introduced bounds. Therefore this function is not appliable for computation.") )
  }
  }
  }
  }


if( class(x.input) == "numeric" | class(y.input) == "numeric" )
   { 
   fun.rep = "supports of one/both inputted points are crisp and the exact report on function is not needed"
   }

    Alphacuts = c( seq(0,1,len=knot.n) , seq(1,0,len=knot.n) )
if (I.O.plot == TRUE)
   {
   op <- par(mfrow = c(3, 1))
   if( class(x.input) == "numeric" ){ plot(TriangularFuzzyNumber(x.input,x.input,x.input), ylab="membership func. of x") }
      else{ plot(x.input, ylab="membership func. of x") }
   if( class(y.input) == "numeric" ){ plot(TriangularFuzzyNumber(y.input,y.input,y.input), xlab="y", ylab="membership func. of y") }
      else{ plot(y.input, col=1, xlab="y", ylab="membership func. of y") }
   plot(result, Alphacuts, xlab="fun(x,y)", ylab="membership func. of fun(x,y)", ...)
   abline(v=fun(core(x),core(y)), lty=3)
   par(op)
   }

if (I.O.plot == "FALSE")
   {
   plot(result, Alphacuts, xlab="fun(x,y)", ylab="membership func. of fun(x,y)", ...)
   }

    result2 <- c(L.result[length(L.result):1], U.result[length(U.result):1])
    cuts <- matrix(result2, ncol=2, byrow=FALSE, 
                   dimnames = list( round((length(L.result)-1):0/(length(L.result)-1),3), c("L", "U"))) 

  return( list( 
               fun.rep = noquote( fun.rep ) , 
               cuts = cuts , 
               core = cuts[1,] , 
               support = cuts[dim(cuts)[1],] 
               ) 
        )

}
