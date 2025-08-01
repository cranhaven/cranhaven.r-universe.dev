cv_vif_plot <-
function(x, limit=40){
  cut_off = 0
  if (is.data.frame(x) == FALSE) {
    #message("The first argument has to be a matrix with two columns. The first argument has to be a two-column matrix containing the coefficients of variation and variance inflation factors of the variables.")
    cut_off = 1
  } else if (ncol(x) != 2){
    message("The input is the output of the function 'cv_vif'.")
    cut_off = 1
  } 
  #
  if (cut_off == 0){
    cvs = x[[1]]
    fivs = x[[2]]
    #
    plot(cvs, fivs, type = "n", xlim=c(0, max(cvs)+2), ylim=c(-limit, max(fivs)+limit), xlab="Coefficient of Variation", ylab="Variance Inflation Factor")
    for (i in 1:length(cvs)){
      points(cvs[i], fivs[i], col = "blue", pch = 20)
    }
    abline(h=10, col="red", lwd=2, lty=2)
    abline(h=0, col="black", lwd=1)
    abline(v=0.1002506, col="red", lwd=2, lty=3)
    #abline(v=0, col="black", lwd=1)
    #
    etiquetas = c()
    for(i in 1:length(cvs)){etiquetas = c(etiquetas, i+1)}
    text(cvs, fivs, labels = etiquetas, pos=1)
  }
}
