Ugyrosegment <- function(A, B, s, n){
  t(vapply(seq(0, 1, length.out = n), function(t){
    UgyroABt(A, B, t, s)
  }, numeric(length(A))))
}

Mgyrosegment <- function(A, B, s, n){
  t(Mgyrosegment_cpp(A, B, s, n))
  # t(vapply(seq(0, 1, length.out = n), function(t){
  #   MgyroABt(A, B, t, s)
  # }, numeric(length(A))))
}
