legendplot <-
function(){
plot(1,type="n", axes=FALSE, frame.plot=FALSE, xlab=" ", ylab=" ", xlim=c(1,16), ylim=c(0,15))
points(1, 14, pch=21, bg="#40E0D0", col="black")
text(5, 14, "Observed in both variables", adj=c(0,NA), cex=0.9)

arrows(1, 12.2, 1, 12.8, length=0.06,   code=2, lwd=1, col= "darkblue")
arrows(2, 12.8, 2, 12.2, length=0.06,   code=2, lwd=1, col= "darkblue")
arrows(3.3, 12.5, 2.7, 12.5, length=0.06,   code=2, lwd=1, col= "darkblue")
arrows(3.7, 12.5, 4.3, 12.5, length=0.06,   code=2, lwd=1, col= "darkblue")
text(5, 12.5, "Observed in one variable,", adj=c(0,NA), cex=0.9)

lines(x=c(0.7, 1.3), y=c(11.5, 11.5), lwd=1, col= "darkblue")
lines(x=c(2, 2), y=c(11.2, 11.8), lwd=1, col= "darkblue")
text(5, 11.5, "cencored in the other variable", adj=c(0,NA), cex=0.9)

arrows(1.3, 9.2, 0.7, 9.8, length=0.06,   code=2, lwd=1, col= "darkblue")
arrows(2.3, 9.8, 1.7, 9.2, length=0.06,   code=2, lwd=1, col= "darkblue")
arrows(2.7, 9.2, 3.3, 9.8, length=0.06,   code=2, lwd=1, col= "darkblue")
arrows(3.7, 9.8, 4.3, 9.2, length=0.06,   code=2, lwd=1, col= "darkblue")
text(5, 9.5, "Censored in both variables", adj=c(0,NA), cex=0.9)

arrows(1, 8.2, 1, 8.8, length=0.06,   code=2, lwd=1, col= "darkblue")
lines(x=c(0.7, 1.3), y=c(8.2, 8.2), lwd=1, col= "darkblue")
arrows(2, 8.8, 2, 8.2, length=0.06,   code=2, lwd=1, col= "darkblue")
lines(x=c(1.7, 2.3), y=c(8.8, 8.8), lwd=1, col= "darkblue")
arrows(3.3, 8.5, 2.7, 8.5, length=0.06,   code=2, lwd=1, col= "darkblue")
lines(x=c(3.3, 3.3), y=c(8.2, 8.8), lwd=1, col= "darkblue")
arrows(3.7, 8.5, 4.3, 8.5, length=0.06,   code=2, lwd=1, col= "darkblue")
lines(x=c(3.7, 3.7), y=c(8.2, 8.8), lwd=1, col= "darkblue")
rect(0.7, 7.2, 1.3, 7.8, col= "lightblue")

text(1, 6, "x",col= "darkred", font=2)
text(5, 6, "Missing in one or both", adj=c(0,NA), cex=0.9)
text(5, 5, "variables", adj=c(0,NA), cex=0.9)

arrows(1, 3.2, 1, 3.8, length=0.06,   code=2, lwd=1, col= "darkred")
arrows(2, 3.8, 2, 3.2, length=0.06,   code=2, lwd=1, col= "darkred")
arrows(3.3, 3.5, 2.7, 3.5, length=0.06,   code=2, lwd=1, col= "darkred")
arrows(3.7, 3.5, 4.3, 3.5, length=0.06,   code=2, lwd=1, col= "darkred")
text(5, 3.5, "Censored in one variable,", adj=c(0,NA), cex=0.9)

lines(x=c(0.7, 1.3), y=c(2.5, 2.5), lwd=1, col= "darkred")
lines(x=c(2, 2), y=c(2.2, 2.8), lwd=1, col= "darkred")
text(5, 2.5, "missing in the other variable", adj=c(0,NA), cex=0.9)
}
