mrdivide <-
function(A, B, pinv = TRUE) {
    stopifnot(is.numeric(A) || is.complex(A),
              is.numeric(B) || is.complex(B))
    if (is.vector(A)) A <- t(A)
    if (is.vector(B)) B <- t(B)
    if (ncol(A) != ncol(B))
        stop("Matrices 'A' and 'B' must have the same number of columns.")

    t(mldivide(t(B), t(A), pinv = pinv))
}
