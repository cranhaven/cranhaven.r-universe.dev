norm.test <- function(x, alpha = 0.05){
  p.value <- shapiro.test(x)$p.value
  if(p.value > alpha){
    return("normal")
  }else{
    return("kernel")
  }
}
