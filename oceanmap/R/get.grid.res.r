.get.grid.res <- function(r){
  if(length(r$grid.res) > 0){
    grid.res <- r$grid.res[1]
#     grid.res <- grid.res/2
  }else{
    grids <- c(0.25,0.5,1,2,5,10,15,20,25,45,60,90,120,180)
    r[,1] <- range(r[,1])
    diffs <- abs(min(apply(r[,1:2],2,FUN=diff)))/2
    grid.res <- apply(as.matrix(diffs),1,FUN=function(x){ a <- (x-grids); b <- tail(grids[which(a >= 0)],1)})
  }
  return(grid.res)
}