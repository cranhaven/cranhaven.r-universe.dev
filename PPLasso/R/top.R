top <-
function(vect, thresh){
  sorted_vect <- sort(abs(vect),decreasing=TRUE)
  v=sorted_vect[thresh]
  ifelse(abs(vect)>=v,vect,0)
}
