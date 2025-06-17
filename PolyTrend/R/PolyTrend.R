PolyTrend <-
function(Y, alpha) {
  
  X <- c(1:length(Y))
  p3 <- lm(Y ~ poly(X, 3, raw=TRUE))
  Pcubic <- unname(coef(summary(p3))[4,4])
  
  Roots3 <- polyroot(c( unname(coef(p3)[2]), 2 * unname(coef(p3)[3]), 3 * unname(coef(p3)[4])))
  Roots3 <- sort(Roots3)
  
  if (all.equal(Im(Roots3),c(0,0))==TRUE && Roots3[1]!=Roots3[2] && X[1]<=Re(Roots3[1]) 
      && Re(Roots3[1])<=X[length(X)] && X[1]<= Re(Roots3[2]) && Re(Roots3[2])<=X[length(X)] && Pcubic<alpha) {
    
    p1 <- lm(Y ~ X)
    Plin <- unname(coef(summary(p1))[2,4])
    Slope <- unname(coef(p1)[2])
    Direction <- sign(Slope)
    
    if(Plin<alpha) {
      
      Trend_type <- 3
      Significance <- 1
      Poly_degree <- 3
      
    } else {
      
      Trend_type <- -1
      Significance <- -1
      Poly_degree <- 3
    }
    
    
  } else {
    
    p2 <- lm(Y ~ poly(X, 2, raw=TRUE))
    Pquadratic <- unname(coef(summary(p2))[3,4])
    Roots2 <- polyroot( c(unname(coef(p2)[2]), 2 * unname(coef(p2)[3])) )
    Roots2 <- Re(Roots2)
    
    if (X[1]<=Roots2 && Roots2<=X[length(X)] && Pquadratic<alpha) {
      
      p1 <- lm(Y ~ X)
      Plin <- unname(coef(summary(p1))[2,4])
      Slope <- unname(coef(p1)[2])
      Direction <- sign(Slope)
      
      if(Plin<alpha) {
        
        Trend_type <- 2
        Significance <- 1
        Poly_degree <- 2
        
      } else {
        
        Trend_type <- -1
        Significance <- -1
        Poly_degree <- 2
      }
      
      
    } else {
      
      p1 <- lm(Y ~ poly(X, raw=TRUE))
      Plin <- unname(coef(summary(p1))[2,4])
      Slope <- unname(coef(p1)[2])
      Direction <- sign(Slope)
      
      if(Plin<alpha) {
        
        Trend_type <- 1
        Significance <- 1
        Poly_degree <- 1
        
      } else {
        
        Trend_type <- 0
        Significance <- -1
        Poly_degree <- 0
      }
      
    }
    
  }
  
  PT.values <- list(
    "Y" = as.vector(Y),
    "alpha" = alpha,
    "TrendType" = Trend_type,
    "Slope" = Slope,
    "Direction" = Direction,
    "Significance" = Significance,
    "PolynomialDegree" = Poly_degree
  )
  class(PT.values) <- "PolyTrend"
  
  return(PT.values)
}
