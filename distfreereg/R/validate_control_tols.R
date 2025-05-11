validate_control_tols <- function(control){
  lapply(control[c("solve_tol", "qr_tol", "orth_tol", "trans_tol")],
         validate_numeric, positive = TRUE, len = 1,
         message = "Invalid tolerance specification: ")
  validate_numeric(control[["matsqrt_tol"]], max_val = 0, len = 1)
}
