Life <- function(
 n.rows=40,
 n.cols=40,
 n.cycles=100,
 sleep.time=0.12,
 cols=c("#f0f0f0", "#2f81c1"),
 random=TRUE, # if false, then function start to be interactive
 rnd.threshold=0.3) # rnd_threshold 0 empty board; 1 all squares are filled
{
.shiftmatrix <- function(mx, dr, dc)
 { # shift the matrix by dr (delta r) rows and dc columns by adding e.g. dr rows of zeros and removing dr rows from the other side
 nr <- nrow(mx)
 nc <- ncol(mx)
 ## if the matrix is shifted by more than its nrow or ncol, we get a matrix of zeros
 if (abs(dr) >= nr || abs(dc) >= nc) { mx <- matrix(0, nrow = nr, ncol = nc) ; return(mx) }
 ## rows:
 if (dr > 0) {
  mx <- rbind(mat.or.vec(dr, nc), mx)
  mx <- mx[1:nr,]
 } else if (dr < 0) {
  mx <- rbind(mx, mat.or.vec(-dr, nc))
  mx <- mx[(1 - dr):(nr - dr),]
 }
 ## columns:
 if (dc > 0) {
  mx <- cbind(mat.or.vec(nr, dc), mx)
  mx <- mx[,1:nc]
 } else if (dc < 0) {
  mx <- cbind(mx, mat.or.vec(nr, -dc))
  mx <- mx[,(1 - dc):(nc - dc)]
 }
 return(mx)
 }
##
.lifecycle <- function(mx) 
 { # move the board one generation forward
 mx0 <- matrix(0, nrow = nrow(mx), ncol = ncol(mx))
 ## produce 8 "shifted" boards and add them up
 for (n in (-1:1)) {
  for (m in (-1:1)) {
   if (n !=0 || m !=0) mx0 <- mx0 + .shiftmatrix(mx, n, m)
  }
 }
 ## Deaths and births
 mx[mx0 > 3 | mx0 < 2] <- 0
 mx[mx0 == 3] <- 1
 return(mx)
 }
##
## Create a board and plot it
.board <- matrix(0, nrow=n.rows, ncol=n.cols)
old.par <- par(mar=rep(0, 4))
if(random) {
 .board[runif(n.rows * n.cols, 0, 1) < rnd.threshold] <- 1
 image(.board, col=cols)
 grid(n.cols, n.rows, lty=1)
 } else {
 image(.board, col=cols)
 grid(n.cols, n.rows, lty=1)
 points(0, 0, pch=15, cex=2, col="red")
 while(TRUE) { # "paint" cells
  click <- locator(1)
  click$x <- floor(click$x * n.cols) + 1
  click$y <- floor(click$y * n.rows) + 1
  click$x[click$x > n.cols] <- n.cols
  click$y[click$y > n.rows] <- n.rows
  click$x[click$x < 1] <- 1
  click$y[click$y < 1] <- 1
  if(click$x == 1 & click$y == 1) break # if click was on the bottom left corner
  sel <- as.matrix(as.data.frame(click)) # prepare matrix selection
  .board[sel] <- as.numeric(.board[sel] == 0) # reverse 0 and 1
  image(.board, col=cols)
  grid(n.cols, n.rows, lty=1)
  points(0, 0, pch=15, cex=2, col="red")
  }
 }
##
## The main cycle
for (i in (1:n.cycles)) {
 Sys.sleep(sleep.time)
 .board <- .lifecycle(.board)
 image(.board, col=cols)
 grid(n.cols, n.rows, lty=1)
}
par(old.par)
}
