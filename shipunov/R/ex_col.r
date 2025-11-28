Ex.col <- Ex.cols <- function(all=FALSE)
{
if(!all)
{
 cols <- c("white", palette())
 nums <- 0:(length(cols)-1)
 labels <- paste(nums, cols, sep="  ")
 oldpar <- par(mar=c(0, 0, 0, 0))
 plot(1, ylim=range(nums), xlim=c(0, 0.7), axes=FALSE, type="n", xlab="", ylab="")
 for(i in nums) rect(0.2, i-0.2, 0.7, i+0.2, col=cols[length(nums)-i], border=NA)
 text(0, rev(nums), labels=labels, pos=4)
 par(oldpar)
}
else
{
 ccolors <- c("transparent", colors())
 color.rgb <- t(col2rgb(ccolors))
 color.text <- ifelse(apply(color.rgb, 1, mean) > 127, "black", "white")
 color.df <- data.frame(name=ccolors, red=color.rgb[, "red"], green=color.rgb[, "green"], blue=color.rgb[, "blue"], text=color.text)
 color.df <- color.df[-grep("gray[1-9][1-9]|grey[1-9]", ccolors),] # remove most of greys/grays
 color.df <- droplevels(color.df)
 cols <- 10
 rows <- 48
 op <- par(mar=rep(0,4))
 plot(c(0, cols), c(0, rows), type="n", bty="n", ylab="", xlab="", axes=FALSE)
 for(i in 1:cols) {
  color.count <- (i-1) * rows
  color.mod <- length(ccolors) - color.count
  y.val <- ifelse(color.mod < rows, rows - color.mod + 1, 1)
  color.names <- as.character(color.df[color.count + 1:rows, "name"])
  rect(i - 1, y.val - 0.5, i, rows:y.val + 0.5, border="black", col=color.names)
  color.text <- as.character(color.df[color.count + 1:rows, "text"])
  text(i-0.5, rows:y.val, labels=color.names, cex=0.55, col=color.text)
 }
 par(op)
}}
