set.seed(20240220)

n <- 20

Sig <- rWishart(1, df = n, Sigma = diag(n))[,,1]
P <- distfreereg:::matinv(Sig, tol = eval(as.list(solve.default)[["tol"]]))

message('all.equal(diag(1, n), Sig %*% P) (should be TRUE): ', all.equal(diag(1, n), Sig %*% P))

Q <- distfreereg:::matsqrt(P)

message('all.equal(Q %*% Q, P) (should be TRUE): ', all.equal(Q %*% Q, P))
