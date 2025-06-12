"ldplot" <-
function(ld.mat, ld.type, color = heat.colors(50), title=NULL){
    if (ld.type=="r") ld.mat <- ld.mat^2
    else if (ld.type=="d") ld.mat <- abs(ld.mat)
    else stop("Please select a correct type for the LD coefficients")	 
    mybreak <- 0:length(color)/length(color)
    mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
    oma <- c(2, 1, 1, 2)
    def.par <- par(no.readonly = TRUE)
    m <- matrix(c(1, 1, 1, 2), 2, 2)
    h <- (3 + mar.orig[2]) * par("csi") * 2.54
    layout(m, heights = c(1, lcm(0.5 * h)))
    image(1-ld.mat, axes=FALSE, col=color, cex.main = 1, breaks=mybreak,main=title)
    box()
    par(mar = c(2, 1, 1.5, 2))
    a <- matrix(1:length(color), ncol = 1)
    image(a, axes = FALSE, col = color[length(color):1])
    box(bty = "o")
    mylable <- as.character(seq(from = 0, to = 1, length = 11))
    axis(side = 1, at = seq(from = 0 - 1/11/2, to = 1 + 1/11/2, length = 11), labels = mylable)
    par(def.par)
}

