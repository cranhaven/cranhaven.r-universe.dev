MB <-
function (x, thr, ths, alp){
.value <- thr + (ths-thr)*exp(-x*alp)
.value
}
