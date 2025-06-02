matrix_to_hyp <- function(hypothesis, param_names){
  mapply(function(hyp_mat, n_ec){
    hyps <- apply(hyp_mat[, -ncol(hyp_mat), drop = FALSE], 1, function(x){
      include_term <- !x == 0
      hyp_scalar <- x[include_term]
      hyp_param <- param_names[include_term]
      hyp <- paste(paste0("+", hyp_scalar), hyp_param, sep = "*", collapse = "")
      hyp <- gsub("\\+-", "-", hyp)
      hyp <- gsub("1\\*", "", hyp)
      hyp <- gsub("^\\+", "", hyp)
      hyp
    })

    paste(hyps,
          paste0(c(rep("=", n_ec), rep(">", (nrow(hyp_mat) - n_ec))), hyp_mat[, ncol(hyp_mat)]),
          sep = "",
          collapse = "&"
    )
  }, hyp_mat = hypothesis$hyp_mat, n_ec = hypothesis$n_ec)
}
