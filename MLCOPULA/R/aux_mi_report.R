aux_mi_report <- function(x,y){
  frec_join <- table(x, y)
  f <- as.data.frame(frec_join)
  f$prob <- f$Freq/sum(f$Freq)
  fy_x  <- prop.table(frec_join,margin = 1)
  fx <- table(x)/length(x)
  fy <- table(y)/length(y)
  
  res <- list(f = f, fy_x = fy_x, fx = fx, fy = fy)
}
