nn_ellipse = function(coor = coor , nv = nv, p = p){
  repmat = function(X,m,n){
    ##R equivalent of repmat (matlab)
    X <- as.matrix(X)
    mx = dim(X)[1]
    nx = dim(X)[2]
    matrix(t(matrix(X,mx,nx*n)),mx*m,nx*n,byrow=T)}
  ##
  Cx <- coor[,1]
  Cy <- coor[,2]
  R <- length(Cx)
  XX <- (repmat(t(Cx),R,1)-repmat(Cx,1,R))
  YY=(repmat(t(Cy),R,1)-repmat(Cy,1,R));
  S<- c(1,2,3,4) # S <- c(1,1.5, 2, 3, 4, 5, 10)
  results <- list()
  ## get the circle
  sh <- 1
  a <- 1
  b=a*sh
  # p <- 30; # el angulo de rotacion. No es importante para obtener los circulos
  angulo <- (pi/180)*p
  X=XX*cos(angulo)+YY*sin(angulo)
  Y <- -XX*sin(angulo)+YY*cos(angulo);
  F <- (X/a)^2+(Y/b)^2
  e2 <- apply(F,2,order)
  F1 <- t(e2[t(seq(1,R)),])
  F1 <- F1[,1:nv]
  results$circles <- F1
  ## Ellipses
  F2 <- as.numeric(matrix(, nrow = 0, ncol = nv))
  for (s in 2:length(S)){
    sh=S[s];
    a=1;
    b=a*sh;
    for(i in seq(10, 180, by = 30)){
      angulo=(pi/180)*p
      X <- XX*cos(angulo)+YY*sin(angulo)
      Y <- -XX*sin(angulo)+YY*cos(angulo)
      F <- (X/a)^2+(Y/b)^2
      e2 <- apply(F,2,order)
      F1 <- t(e2[t(seq(1,R)),])
      F1 <- F1[,1:nv]
      F2 <-rbind(F2,F1)
      # results.nne(p/10,s-1).eq=int16(F1(:,1:nn));
    }
  }
  results$ellipses <- F2
  return(results)
}
