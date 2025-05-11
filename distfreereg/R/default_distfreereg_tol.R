default_distfreereg_tol <- function(){
  list(matsqrt_tol = -(.Machine[["double.eps"]])^0.25,
       solve_tol = .Machine[["double.eps"]],
       qr_tol = sqrt(.Machine[["double.eps"]]),
       orth_tol = sqrt(.Machine[["double.eps"]]),
       trans_tol = sqrt(.Machine[["double.eps"]]))
}