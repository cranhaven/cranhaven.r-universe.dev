Miney <- function(n=9, # width (and height) of the square field; if n=5 then field is 5 x 5
 ucol="#b8ff73", # non-tested cells, law green
 gcol="#f0f0f0", # tested cells, gray
 bcol="red", # cell with bomb
 space=0.05, # white space between cells
 pbombs=0.15 # proportion of cells with bombs in the field
)
{
 tick <- as.numeric(Sys.time()) # start to count time
 oldpar <- par(mar=rep(2, 4), xaxs="i", yaxs="i")
 ## make game field
 bombs <- sample(x=1:n^2, size=floor(pbombs * n^2), replace=FALSE)
 elements <- rep(0, n^2)
 elements[bombs] <- 1
 field <- matrix(elements, ncol=n, nrow=n, byrow=TRUE)
 ## count neighbors
 temp <- rbind(0, cbind(0, field, 0), 0)
 neigh <- matrix(0, ncol=ncol(temp), nrow=nrow(temp))
 for (i in 2:(nrow(temp) - 1)) {
  for (j in 2:(ncol(temp) - 1)) {
   neigh[i, j] <- sum(temp[(i-1):(i+1), (j-1):(j+1)]) - temp[i, j]
  }
 }
 neigh <- neigh[-(c(1, nrow(neigh))), -(c(1, ncol(neigh)))]
 add <- matrix(0, ncol=n, nrow=n)
 ## coordinates of cells
 allx <- rep(1:n, n)
 xleft <- allx-1+space
 xright <- allx-space
 ally <- rep(1:n, each=n)
 ybottom <- ally-1+space
 ytop <- ally-space
 ## main plot
 .plotfield <- function(text=TRUE) {
  ccol <- ifelse(add == 1, gcol, ucol)
  plot(NULL, type="n", xlab="", ylab="", axes=FALSE, xlim=c(0, n), ylim=c(0, n))
  rect(xleft=xleft, ybottom=ybottom, xright=xright, ytop=ytop, col=ccol, border=NA)
  if(text) text(x=allx[add > 0]-0.5, y=ally[add > 0]-0.5, labels=neigh[add > 0])
 }
 .plotfield(text=FALSE)
 ## main cycle
 game.over <- FALSE
 while(!game.over) {
  click <- locator(1)
  cx <- floor(click$x) + 1
  cy <- floor(click$y) + 1
  add[cx, cy] <- 1
  if (field[cx, cy]==1) { # if blast
   rect(xleft=cx-1+space, xright=cx-space, ybottom=cy-1+space, ytop=cy-space, col=bcol, border=NA)
   mtext(paste0("Game over...\tTime: ", round(as.numeric(Sys.time()) - tick), " sec"), side=1, line=0)
   game.over <- TRUE
   } else {
   if (all(field + add==1)) { # if all done
    .plotfield()
    mtext(paste0("Success!\tTime: ", round(as.numeric(Sys.time()) - tick), " sec"), side=1, line=0)
    game.over <- TRUE
    } else { # otherwise, continue
    .plotfield()
    }
   }
 }
 par(oldpar)
}
