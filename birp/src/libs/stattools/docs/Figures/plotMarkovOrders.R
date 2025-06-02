
plotRect <- function(numRows, numCols, labels, picker, panel_label, width = 1){
  height <- width
  
  plot(0, type = "n", 
       xlim = c(0, width), ylim = c(0, height), 
       axes = F, 
       xlab = "", ylab = "")
  
  
  # draw horizontal lines
  dist_horizontal <- height / numRows
  for (i in 0:numRows){
    lines(x = c(0, width),
          y = rep(dist_horizontal * i, 2))
  }
  
  # draw vertical lines
  dist_vertical <- width / numCols
  for (i in 0:numCols){
    lines(x = rep(dist_vertical * i, 2),
          y = c(0, height))
  }
  
  # add text
  colors <- c("navy", "blue", "cyan", "purple", "firebrick", "orange2", "gold")
  for (i in 1:numRows){
    for (j in 1:numCols){
      text(x = dist_vertical/2 + dist_vertical * (j-1), 
           y = 1 - (dist_horizontal/2 + dist_horizontal * (i-1)), 
           labels = labels[i, j],
           col = colors[picker[i,j]])
    }
  }
  
  text(-0.1, 1, labels = panel_label, xpd = NA, font = 2)
}

pdf("/data/git/stattools/docs/Figures/picker.pdf", width = 6, height = 3)
par(mfrow = c(1, 2), mar = c(2,2,0,0))

# 7 times 5 matrix, markov order = (1, 2)
numRows <- 7
numCols <- 5
labels <- matrix(c(0,0,0,1,1,
                  0,0,0,1,1,
                  2,2,1,3,3,
                  2,2,1,3,3,
                  4,4,2,5,5,
                  4,4,2,5,5,
                  6,6,3,7,7), numRows, numCols, byrow = T)
picker <- matrix(c(1,2,3,1,2,
                   4,5,6,4,5,
                   1,2,3,1,2,
                   4,5,6,4,5,
                   1,2,3,1,2,
                   4,5,6,4,5,
                   1,2,3,1,2), numRows, numCols, byrow = T)

plotRect(numRows, numCols, labels, picker, "(A)")

# 7 times 5 matrix, markov order = (max, 0)
numRows <- 7
numCols <- 5
labels <- matrix(c(0,1,2,3,4), numRows, numCols, byrow = T)
picker <- matrix(c(1:numRows), numRows, numCols, byrow = F)

plotRect(numRows, numCols, labels, picker, "(B)")
dev.off()

