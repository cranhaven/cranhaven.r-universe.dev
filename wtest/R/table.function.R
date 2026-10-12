#'@useDynLib wtest, .registration = TRUE
table.e1<-function(x,y){
  .Call("table_e1",x,y)
}

table.e2<-function(x1,x2,y){
  .Call("table_e2",x1,x2,y)
}

