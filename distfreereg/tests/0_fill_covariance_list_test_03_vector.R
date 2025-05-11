set.seed(20240223)

fill_covariance_list <- function(need, covariance_list)
  distfreereg:::fill_covariance_list(need = need, covariance_list = covariance_list,
                                     matsqrt_tol = distfreereg:::default_distfreereg_tol()[["matsqrt_tol"]],
                                     solve_tol = distfreereg:::default_distfreereg_tol()[["solve_tol"]])
matsqrt <- distfreereg:::matsqrt

n <- 20
Sigma <- rexp(n)
P <- 1/Sigma
SqrtSigma <- sqrt(Sigma)
Q <- sqrt(P)

# Create a bunch of lists to be filled, grouped by how many elements they
# contain.

CL_1.1 <- list(Sigma = Sigma)
CL_1.2 <- list(P = P)
CL_1.3 <- list(SqrtSigma = SqrtSigma)
CL_1.4 <- list(Q = Q)

CL_2.1 <- list(Sigma = Sigma, P = P)
CL_2.2 <- list(Sigma = Sigma, SqrtSigma = SqrtSigma)
CL_2.3 <- list(Sigma = Sigma, Q = Q)
CL_2.4 <- list(P = P, SqrtSigma = SqrtSigma)
CL_2.5 <- list(P = P, Q = Q)
CL_2.6 <- list(SqrtSigma = SqrtSigma, Q = Q)

CL_3.1 <- list(Sigma = Sigma, P = P, SqrtSigma = SqrtSigma)
CL_3.2 <- list(Sigma = Sigma, P = P, Q = Q)
CL_3.3 <- list(Sigma = Sigma, SqrtSigma = SqrtSigma, Q = Q)
CL_3.4 <- list(P = P, SqrtSigma = SqrtSigma, Q = Q)

CL_4 <- list(Sigma = Sigma, P = P, SqrtSigma = SqrtSigma, Q = Q)


# Verify validation of "need".
tryCatch(fill_covariance_list(need = "bad", covariance_list = CL_1),
         error = function(e) warning(e))


# For each list created above, fill it for each of the four possible needed
# matrices, and then validate the results.

# CL_1.1: Only Sigma supplied.

fcl_1.1.Sigma <- fill_covariance_list(need = "Sigma", covariance_list = CL_1.1)
fcl_1.1.P <- fill_covariance_list(need = "P", covariance_list = CL_1.1)
fcl_1.1.SqrtSigma <- fill_covariance_list(need = "SqrtSigma", covariance_list = CL_1.1)
fcl_1.1.Q <- fill_covariance_list(need = "Q", covariance_list = CL_1.1)

message('identical(fcl_1.1.Sigma$Sigma, Sigma) (should be TRUE): ', identical(fcl_1.1.Sigma$Sigma, Sigma))
is.null(fcl_1.1.Sigma$P)# TRUE
is.null(fcl_1.1.Sigma$SqrtSigma)# TRUE
is.null(fcl_1.1.Sigma$Q)# TRUE

message('identical(fcl_1.1.P$Sigma, Sigma) (should be TRUE): ', identical(fcl_1.1.P$Sigma, Sigma))
message('identical(fcl_1.1.P$P, P) (should be TRUE): ', identical(fcl_1.1.P$P, P))
is.null(fcl_1.1.P$SqrtSigma)# TRUE
is.null(fcl_1.1.P$Q)# TRUE

message('identical(fcl_1.1.SqrtSigma$Sigma, Sigma) (should be TRUE): ', identical(fcl_1.1.SqrtSigma$Sigma, Sigma))
is.null(fcl_1.1.SqrtSigma$P)# TRUE
message('identical(fcl_1.1.SqrtSigma$SqrtSigma, SqrtSigma) (should be TRUE): ', identical(fcl_1.1.SqrtSigma$SqrtSigma, SqrtSigma))
is.null(fcl_1.1.SqrtSigma$Q)# TRUE

message('identical(fcl_1.1.Q$Sigma, Sigma) (should be TRUE): ', identical(fcl_1.1.Q$Sigma, Sigma))
message('identical(fcl_1.1.Q$P, P) (should be TRUE): ', identical(fcl_1.1.Q$P, P))
is.null(fcl_1.1.Q$SqrtSigma)
message('identical(fcl_1.1.Q$Q, Q) (should be TRUE): ', identical(fcl_1.1.Q$Q, Q))


# CL_1.2: Only P supplied.

fcl_1.2.Sigma <- fill_covariance_list(need = "Sigma", covariance_list = CL_1.2)
fcl_1.2.P <- fill_covariance_list(need = "P", covariance_list = CL_1.2)
fcl_1.2.SqrtSigma <- fill_covariance_list(need = "SqrtSigma", covariance_list = CL_1.2)
fcl_1.2.Q <- fill_covariance_list(need = "Q", covariance_list = CL_1.2)

message('identical(fcl_1.2.Sigma$Sigma, 1/P) (should be TRUE): ', identical(fcl_1.2.Sigma$Sigma, 1/P))
message('identical(fcl_1.2.Sigma$P, P) (should be TRUE): ', identical(fcl_1.2.Sigma$P, P))
is.null(fcl_1.2.Sigma$SqrtSigma)# TRUE
is.null(fcl_1.2.Sigma$Q)# TRUE

is.null(fcl_1.2.P$Sigma)# TRUE
message('identical(fcl_1.2.P$P, P) (should be TRUE): ', identical(fcl_1.2.P$P, P))
is.null(fcl_1.2.P$SqrtSigma)# TRUE
is.null(fcl_1.2.P$Q)# TRUE

is.null(fcl_1.2.SqrtSigma$Sigma)# TRUE
message('identical(fcl_1.2.SqrtSigma$P, P) (should be TRUE): ', identical(fcl_1.2.SqrtSigma$P, P))
message('identical(fcl_1.2.SqrtSigma$SqrtSigma, 1/matsqrt(P)) (should be TRUE): ', identical(fcl_1.2.SqrtSigma$SqrtSigma, 1/matsqrt(P)))
message('identical(fcl_1.2.SqrtSigma$Q, matsqrt(P)) (should be TRUE): ', identical(fcl_1.2.SqrtSigma$Q, matsqrt(P)))

is.null(fcl_1.2.Q$Sigma)# TRUE
message('identical(fcl_1.2.Q$P, P) (should be TRUE): ', identical(fcl_1.2.Q$P, P))
is.null(fcl_1.2.Q$SqrtSigma)
message('identical(fcl_1.2.Q$Q, Q) (should be TRUE): ', identical(fcl_1.2.Q$Q, Q))



# CL_1.3: Only SqrtSigma supplied.

fcl_1.3.Sigma <- fill_covariance_list(need = "Sigma", covariance_list = CL_1.3)
fcl_1.3.P <- fill_covariance_list(need = "P", covariance_list = CL_1.3)
fcl_1.3.SqrtSigma <- fill_covariance_list(need = "SqrtSigma", covariance_list = CL_1.3)
fcl_1.3.Q <- fill_covariance_list(need = "Q", covariance_list = CL_1.3)

message('identical(fcl_1.3.Sigma$Sigma, SqrtSigma * SqrtSigma) (should be TRUE): ', identical(fcl_1.3.Sigma$Sigma, SqrtSigma * SqrtSigma))
is.null(fcl_1.3.Sigma$P)# TRUE
message('identical(fcl_1.3.Sigma$SqrtSigma, SqrtSigma) (should be TRUE): ', identical(fcl_1.3.Sigma$SqrtSigma, SqrtSigma))
is.null(fcl_1.3.Sigma$Q)# TRUE

is.null(fcl_1.3.P$Sigma)# TRUE
message('identical(fcl_1.3.P$P, (1/SqrtSigma) * (1/SqrtSigma)) (should be TRUE): ', identical(fcl_1.3.P$P, (1/SqrtSigma) * (1/SqrtSigma)))
message('identical(fcl_1.3.P$SqrtSigma, SqrtSigma) (should be TRUE): ', identical(fcl_1.3.P$SqrtSigma, SqrtSigma))
message('identical(fcl_1.3.P$Q, 1/SqrtSigma) (should be TRUE): ', identical(fcl_1.3.P$Q, 1/SqrtSigma))

is.null(fcl_1.3.SqrtSigma$Sigma)# TRUE
is.null(fcl_1.3.SqrtSigma$P)# TRUE
message('identical(fcl_1.3.SqrtSigma$SqrtSigma, SqrtSigma) (should be TRUE): ', identical(fcl_1.3.SqrtSigma$SqrtSigma, SqrtSigma))
is.null(fcl_1.3.SqrtSigma$Q)# TRUE

is.null(fcl_1.3.Q$Sigma)# TRUE
is.null(fcl_1.3.Q$P)# TRUE
message('identical(fcl_1.3.Q$SqrtSigma, SqrtSigma) (should be TRUE): ', identical(fcl_1.3.Q$SqrtSigma, SqrtSigma))
message('identical(fcl_1.3.Q$Q, 1/SqrtSigma) (should be TRUE): ', identical(fcl_1.3.Q$Q, 1/SqrtSigma))




# CL_1.4: Only Q supplied.

fcl_1.4.Sigma <- fill_covariance_list(need = "Sigma", covariance_list = CL_1.4)
fcl_1.4.P <- fill_covariance_list(need = "P", covariance_list = CL_1.4)
fcl_1.4.SqrtSigma <- fill_covariance_list(need = "SqrtSigma", covariance_list = CL_1.4)
fcl_1.4.Q <- fill_covariance_list(need = "Q", covariance_list = CL_1.4)

message('identical(fcl_1.4.Sigma$Sigma, 1/(Q * Q)) (should be TRUE): ', identical(fcl_1.4.Sigma$Sigma, 1/(Q * Q)))
message('identical(fcl_1.4.Sigma$P, Q * Q) (should be TRUE): ', identical(fcl_1.4.Sigma$P, Q * Q))
is.null(fcl_1.4.Sigma$SqrtSigma)# TRUE
message('identical(fcl_1.4.Sigma$Q, Q) (should be TRUE): ', identical(fcl_1.4.Sigma$Q, Q))

is.null(fcl_1.4.P$Sigma)# TRUE
message('identical(fcl_1.4.P$P, Q * Q) (should be TRUE): ', identical(fcl_1.4.P$P, Q * Q))
is.null(fcl_1.4.P$SqrtSigma)# TRUE
message('identical(fcl_1.4.P$Q, Q) (should be TRUE): ', identical(fcl_1.4.P$Q, Q))

is.null(fcl_1.4.SqrtSigma$Sigma)# TRUE
is.null(fcl_1.4.SqrtSigma$P)# TRUE
message('identical(fcl_1.4.SqrtSigma$SqrtSigma, 1/Q) (should be TRUE): ', identical(fcl_1.4.SqrtSigma$SqrtSigma, 1/Q))
message('identical(fcl_1.4.SqrtSigma$Q, Q) (should be TRUE): ', identical(fcl_1.4.SqrtSigma$Q, Q))

is.null(fcl_1.4.Q$Sigma)# TRUE
is.null(fcl_1.4.Q$P)# TRUE
is.null(fcl_1.4.Q$SqrtSigma)# TRUE
message('identical(fcl_1.4.Q$Q, Q) (should be TRUE): ', identical(fcl_1.4.Q$Q, Q))




# CL_2.2: Sigma and P supplied.

fcl_2.1.Sigma <- fill_covariance_list(need = "Sigma", covariance_list = CL_2.1)
fcl_2.1.P <- fill_covariance_list(need = "P", covariance_list = CL_2.1)
fcl_2.1.SqrtSigma <- fill_covariance_list(need = "SqrtSigma", covariance_list = CL_2.1)
fcl_2.1.Q <- fill_covariance_list(need = "Q", covariance_list = CL_2.1)

message('identical(fcl_2.1.Sigma$Sigma, Sigma) (should be TRUE): ', identical(fcl_2.1.Sigma$Sigma, Sigma))
message('identical(fcl_2.1.Sigma$P, P) (should be TRUE): ', identical(fcl_2.1.Sigma$P, P))
is.null(fcl_2.1.Sigma$SqrtSigma)# TRUE
is.null(fcl_2.1.Sigma$Q)# TRUE

message('identical(fcl_2.1.P$Sigma, Sigma) (should be TRUE): ', identical(fcl_2.1.P$Sigma, Sigma))
message('identical(fcl_2.1.P$P, P) (should be TRUE): ', identical(fcl_2.1.P$P, P))
is.null(fcl_2.1.P$SqrtSigma)# TRUE
is.null(fcl_2.1.P$Q)# TRUE

message('identical(fcl_2.1.SqrtSigma$Sigma, Sigma) (should be TRUE): ', identical(fcl_2.1.SqrtSigma$Sigma, Sigma))
message('identical(fcl_2.1.SqrtSigma$P, P) (should be TRUE): ', identical(fcl_2.1.SqrtSigma$P, P))
message('identical(fcl_2.1.SqrtSigma$SqrtSigma, matsqrt(Sigma)) (should be TRUE): ', identical(fcl_2.1.SqrtSigma$SqrtSigma, matsqrt(Sigma)))
is.null(fcl_2.1.SqrtSigma$Q)# TRUE

message('identical(fcl_2.1.Q$Sigma, Sigma) (should be TRUE): ', identical(fcl_2.1.Q$Sigma, Sigma))
message('identical(fcl_2.1.Q$P, P) (should be TRUE): ', identical(fcl_2.1.Q$P, P))
is.null(fcl_2.1.Q$SqrtSigma)# TRUE
message('identical(fcl_2.1.Q$Q, matsqrt(P)) (should be TRUE): ', identical(fcl_2.1.Q$Q, matsqrt(P)))




# CL_2.2: Sigma and SqrtSigma supplied.

fcl_2.2.Sigma <- fill_covariance_list(need = "Sigma", covariance_list = CL_2.2)
fcl_2.2.P <- fill_covariance_list(need = "P", covariance_list = CL_2.2)
fcl_2.2.SqrtSigma <- fill_covariance_list(need = "SqrtSigma", covariance_list = CL_2.2)
fcl_2.2.Q <- fill_covariance_list(need = "Q", covariance_list = CL_2.2)

message('identical(fcl_2.2.Sigma$Sigma, Sigma) (should be TRUE): ', identical(fcl_2.2.Sigma$Sigma, Sigma))
is.null(fcl_2.2.Sigma$P)# TRUE
message('identical(fcl_2.2.Sigma$SqrtSigma, SqrtSigma) (should be TRUE): ', identical(fcl_2.2.Sigma$SqrtSigma, SqrtSigma))
is.null(fcl_2.2.Sigma$Q)# TRUE

message('identical(fcl_2.2.P$Sigma, Sigma) (should be TRUE): ', identical(fcl_2.2.P$Sigma, Sigma))
message('identical(fcl_2.2.P$P, 1/Sigma) (should be TRUE): ', identical(fcl_2.2.P$P, 1/Sigma))
message('identical(fcl_2.2.P$SqrtSigma, SqrtSigma) (should be TRUE): ', identical(fcl_2.2.P$SqrtSigma, SqrtSigma))
is.null(fcl_2.2.P$Q)# TRUE

message('identical(fcl_2.2.SqrtSigma$Sigma, Sigma) (should be TRUE): ', identical(fcl_2.2.SqrtSigma$Sigma, Sigma))
is.null(fcl_2.2.SqrtSigma$P)# TRUE
message('identical(fcl_2.2.SqrtSigma$SqrtSigma, SqrtSigma) (should be TRUE): ', identical(fcl_2.2.SqrtSigma$SqrtSigma, SqrtSigma))
is.null(fcl_2.2.SqrtSigma$Q)# TRUE

message('identical(fcl_2.2.Q$Sigma, Sigma) (should be TRUE): ', identical(fcl_2.2.Q$Sigma, Sigma))
is.null(fcl_2.2.Q$P)# TRUE
message('identical(fcl_2.2.Q$SqrtSigma, SqrtSigma) (should be TRUE): ', identical(fcl_2.2.Q$SqrtSigma, SqrtSigma))
message('identical(fcl_2.2.Q$Q, 1/SqrtSigma) (should be TRUE): ', identical(fcl_2.2.Q$Q, 1/SqrtSigma))




# CL_2.3: Sigma and Q supplied.

fcl_2.3.Sigma <- fill_covariance_list(need = "Sigma", covariance_list = CL_2.3)
fcl_2.3.P <- fill_covariance_list(need = "P", covariance_list = CL_2.3)
fcl_2.3.SqrtSigma <- fill_covariance_list(need = "SqrtSigma", covariance_list = CL_2.3)
fcl_2.3.Q <- fill_covariance_list(need = "Q", covariance_list = CL_2.3)

message('identical(fcl_2.3.Sigma$Sigma, Sigma) (should be TRUE): ', identical(fcl_2.3.Sigma$Sigma, Sigma))
is.null(fcl_2.3.Sigma$P)# TRUE
is.null(fcl_2.3.Sigma$SqrtSigma)# TRUE
message('identical(fcl_2.3.Sigma$Q, Q) (should be TRUE): ', identical(fcl_2.3.Sigma$Q, Q))

message('identical(fcl_2.3.P$Sigma, Sigma) (should be TRUE): ', identical(fcl_2.3.P$Sigma, Sigma))
message('identical(fcl_2.3.P$P, Q * Q) (should be TRUE): ', identical(fcl_2.3.P$P, Q * Q))
is.null(fcl_2.3.P$SqrtSigma)# TRUE
message('identical(fcl_2.3.P$Q, Q) (should be TRUE): ', identical(fcl_2.3.P$Q, Q))

message('identical(fcl_2.3.SqrtSigma$Sigma, Sigma) (should be TRUE): ', identical(fcl_2.3.SqrtSigma$Sigma, Sigma))
is.null(fcl_2.3.SqrtSigma$P)# TRUE
message('identical(fcl_2.3.SqrtSigma$SqrtSigma, 1/Q) (should be TRUE): ', identical(fcl_2.3.SqrtSigma$SqrtSigma, 1/Q))
message('identical(fcl_2.3.SqrtSigma$Q, Q) (should be TRUE): ', identical(fcl_2.3.SqrtSigma$Q, Q))

message('identical(fcl_2.3.Q$Sigma, Sigma) (should be TRUE): ', identical(fcl_2.3.Q$Sigma, Sigma))
is.null(fcl_2.3.Q$P)# TRUE
is.null(fcl_2.3.Q$SqrtSigma)# TRUE
message('identical(fcl_2.3.Q$Q, Q) (should be TRUE): ', identical(fcl_2.3.Q$Q, Q))




# CL_2.4: P and SqrtSigma supplied.

fcl_2.4.Sigma <- fill_covariance_list(need = "Sigma", covariance_list = CL_2.4)
fcl_2.4.P <- fill_covariance_list(need = "P", covariance_list = CL_2.4)
fcl_2.4.SqrtSigma <- fill_covariance_list(need = "SqrtSigma", covariance_list = CL_2.4)
fcl_2.4.Q <- fill_covariance_list(need = "Q", covariance_list = CL_2.4)

message('identical(fcl_2.4.Sigma$Sigma, SqrtSigma * SqrtSigma) (should be TRUE): ', identical(fcl_2.4.Sigma$Sigma, SqrtSigma * SqrtSigma))
message('identical(fcl_2.4.Sigma$P, P) (should be TRUE): ', identical(fcl_2.4.Sigma$P, P))
message('identical(fcl_2.4.Sigma$SqrtSigma, SqrtSigma) (should be TRUE): ', identical(fcl_2.4.Sigma$SqrtSigma, SqrtSigma))
is.null(fcl_2.4.Sigma$Q)# TRUE

is.null(fcl_2.4.P$Sigma)# TRUE
message('identical(fcl_2.4.P$P, P) (should be TRUE): ', identical(fcl_2.4.P$P, P))
message('identical(fcl_2.4.P$SqrtSigma, SqrtSigma) (should be TRUE): ', identical(fcl_2.4.P$SqrtSigma, SqrtSigma))
is.null(fcl_2.4.P$Q)# TRUE

is.null(fcl_2.4.SqrtSigma$Sigma)# TRUE
message('identical(fcl_2.4.SqrtSigma$P, P) (should be TRUE): ', identical(fcl_2.4.SqrtSigma$P, P))
message('identical(fcl_2.4.SqrtSigma$SqrtSigma, SqrtSigma) (should be TRUE): ', identical(fcl_2.4.SqrtSigma$SqrtSigma, SqrtSigma))
is.null(fcl_2.4.SqrtSigma$Q)# TRUE

is.null(fcl_2.4.Q$Sigma)# TRUE
message('identical(fcl_2.4.Q$P, P) (should be TRUE): ', identical(fcl_2.4.Q$P, P))
message('identical(fcl_2.4.Q$SqrtSigma, SqrtSigma) (should be TRUE): ', identical(fcl_2.4.Q$SqrtSigma, SqrtSigma))
message('identical(fcl_2.4.Q$Q, 1/SqrtSigma) (should be TRUE): ', identical(fcl_2.4.Q$Q, 1/SqrtSigma))




# CL_2.5: P and Q supplied.

fcl_2.5.Sigma <- fill_covariance_list(need = "Sigma", covariance_list = CL_2.5)
fcl_2.5.P <- fill_covariance_list(need = "P", covariance_list = CL_2.5)
fcl_2.5.SqrtSigma <- fill_covariance_list(need = "SqrtSigma", covariance_list = CL_2.5)
fcl_2.5.Q <- fill_covariance_list(need = "Q", covariance_list = CL_2.5)

message('identical(fcl_2.5.Sigma$Sigma, 1/P) (should be TRUE): ', identical(fcl_2.5.Sigma$Sigma, 1/P))
message('identical(fcl_2.5.Sigma$P, P) (should be TRUE): ', identical(fcl_2.5.Sigma$P, P))
is.null(fcl_2.5.Sigma$SqrtSigma)# TRUE
message('identical(fcl_2.5.Sigma$Q, Q) (should be TRUE): ', identical(fcl_2.5.Sigma$Q, Q))

is.null(fcl_2.5.P$Sigma)# TRUE
message('identical(fcl_2.5.P$P, P) (should be TRUE): ', identical(fcl_2.5.P$P, P))
is.null(fcl_2.5.P$SqrtSigma)# TRUE
message('identical(fcl_2.5.P$Q, Q) (should be TRUE): ', identical(fcl_2.5.P$Q, Q))

is.null(fcl_2.5.SqrtSigma$Sigma)# TRUE
message('identical(fcl_2.5.SqrtSigma$P, P) (should be TRUE): ', identical(fcl_2.5.SqrtSigma$P, P))
message('identical(fcl_2.5.SqrtSigma$SqrtSigma, 1/Q) (should be TRUE): ', identical(fcl_2.5.SqrtSigma$SqrtSigma, 1/Q))
message('identical(fcl_2.5.SqrtSigma$Q, Q) (should be TRUE): ', identical(fcl_2.5.SqrtSigma$Q, Q))

is.null(fcl_2.5.Q$Sigma)# TRUE
message('identical(fcl_2.5.Q$P, P) (should be TRUE): ', identical(fcl_2.5.Q$P, P))
is.null(fcl_2.5.Q$SqrtSigma)# TRUE
message('identical(fcl_2.5.Q$Q, Q) (should be TRUE): ', identical(fcl_2.5.Q$Q, Q))




# CL_2.6: SqrtSigma and Q supplied.

fcl_2.6.Sigma <- fill_covariance_list(need = "Sigma", covariance_list = CL_2.6)
fcl_2.6.P <- fill_covariance_list(need = "P", covariance_list = CL_2.6)
fcl_2.6.SqrtSigma <- fill_covariance_list(need = "SqrtSigma", covariance_list = CL_2.6)
fcl_2.6.Q <- fill_covariance_list(need = "Q", covariance_list = CL_2.6)

message('identical(fcl_2.6.Sigma$Sigma, SqrtSigma * SqrtSigma) (should be TRUE): ', identical(fcl_2.6.Sigma$Sigma, SqrtSigma * SqrtSigma))
is.null(fcl_2.6.Sigma$P)# TRUE
message('identical(fcl_2.6.Sigma$SqrtSigma, SqrtSigma) (should be TRUE): ', identical(fcl_2.6.Sigma$SqrtSigma, SqrtSigma))
message('identical(fcl_2.6.Sigma$Q, Q) (should be TRUE): ', identical(fcl_2.6.Sigma$Q, Q))

is.null(fcl_2.6.P$Sigma)# TRUE
message('identical(fcl_2.6.P$P, Q * Q) (should be TRUE): ', identical(fcl_2.6.P$P, Q * Q))
message('identical(fcl_2.6.P$SqrtSigma, SqrtSigma) (should be TRUE): ', identical(fcl_2.6.P$SqrtSigma, SqrtSigma))
message('identical(fcl_2.6.P$Q, Q) (should be TRUE): ', identical(fcl_2.6.P$Q, Q))

is.null(fcl_2.6.SqrtSigma$Sigma)# TRUE
is.null(fcl_2.6.SqrtSigma$P)# TRUE
message('identical(fcl_2.6.SqrtSigma$SqrtSigma, SqrtSigma) (should be TRUE): ', identical(fcl_2.6.SqrtSigma$SqrtSigma, SqrtSigma))
message('identical(fcl_2.6.SqrtSigma$Q, Q) (should be TRUE): ', identical(fcl_2.6.SqrtSigma$Q, Q))

is.null(fcl_2.6.Q$Sigma)# TRUE
is.null(fcl_2.6.Q$P)# TRUE
message('identical(fcl_2.6.Q$SqrtSigma, SqrtSigma) (should be TRUE): ', identical(fcl_2.6.Q$SqrtSigma, SqrtSigma))
message('identical(fcl_2.6.Q$Q, Q) (should be TRUE): ', identical(fcl_2.6.Q$Q, Q))




# CL_3.1: Sigma. P, and SqrtSigma supplied.

fcl_3.1.Sigma <- fill_covariance_list(need = "Sigma", covariance_list = CL_3.1)
fcl_3.1.P <- fill_covariance_list(need = "P", covariance_list = CL_3.1)
fcl_3.1.SqrtSigma <- fill_covariance_list(need = "SqrtSigma", covariance_list = CL_3.1)
fcl_3.1.Q <- fill_covariance_list(need = "Q", covariance_list = CL_3.1)

message('identical(fcl_3.1.Sigma$Sigma, Sigma) (should be TRUE): ', identical(fcl_3.1.Sigma$Sigma, Sigma))
message('identical(fcl_3.1.Sigma$P, P) (should be TRUE): ', identical(fcl_3.1.Sigma$P, P))
message('identical(fcl_3.1.Sigma$SqrtSigma, SqrtSigma) (should be TRUE): ', identical(fcl_3.1.Sigma$SqrtSigma, SqrtSigma))
is.null(fcl_3.1.Sigma$Q)# TRUE

message('identical(fcl_3.1.P$Sigma, Sigma) (should be TRUE): ', identical(fcl_3.1.P$Sigma, Sigma))
message('identical(fcl_3.1.P$P, P) (should be TRUE): ', identical(fcl_3.1.P$P, P))
message('identical(fcl_3.1.P$SqrtSigma, SqrtSigma) (should be TRUE): ', identical(fcl_3.1.P$SqrtSigma, SqrtSigma))
is.null(fcl_3.1.P$Q)# TRUE

message('identical(fcl_3.1.SqrtSigma$Sigma, Sigma) (should be TRUE): ', identical(fcl_3.1.SqrtSigma$Sigma, Sigma))
message('identical(fcl_3.1.SqrtSigma$P, P) (should be TRUE): ', identical(fcl_3.1.SqrtSigma$P, P))
message('identical(fcl_3.1.SqrtSigma$SqrtSigma, SqrtSigma) (should be TRUE): ', identical(fcl_3.1.SqrtSigma$SqrtSigma, SqrtSigma))
is.null(fcl_3.1.SqrtSigma$Q)# TRUE

message('identical(fcl_3.1.Q$Sigma, Sigma) (should be TRUE): ', identical(fcl_3.1.Q$Sigma, Sigma))
message('identical(fcl_3.1.Q$P, P) (should be TRUE): ', identical(fcl_3.1.Q$P, P))
message('identical(fcl_3.1.Q$SqrtSigma, SqrtSigma) (should be TRUE): ', identical(fcl_3.1.Q$SqrtSigma, SqrtSigma))
message('identical(fcl_3.1.Q$Q, 1/SqrtSigma) (should be TRUE): ', identical(fcl_3.1.Q$Q, 1/SqrtSigma))




# CL_3.2: Sigma, P, and Q supplied.

fcl_3.2.Sigma <- fill_covariance_list(need = "Sigma", covariance_list = CL_3.2)
fcl_3.2.P <- fill_covariance_list(need = "P", covariance_list = CL_3.2)
fcl_3.2.SqrtSigma <- fill_covariance_list(need = "SqrtSigma", covariance_list = CL_3.2)
fcl_3.2.Q <- fill_covariance_list(need = "Q", covariance_list = CL_3.2)

message('identical(fcl_3.2.Sigma$Sigma, Sigma) (should be TRUE): ', identical(fcl_3.2.Sigma$Sigma, Sigma))
message('identical(fcl_3.2.Sigma$P, P) (should be TRUE): ', identical(fcl_3.2.Sigma$P, P))
is.null(fcl_3.2.Sigma$SqrtSigma)# TRUE
message('identical(fcl_3.2.Sigma$Q, Q) (should be TRUE): ', identical(fcl_3.2.Sigma$Q, Q))

message('identical(fcl_3.2.P$Sigma, Sigma) (should be TRUE): ', identical(fcl_3.2.P$Sigma, Sigma))
message('identical(fcl_3.2.P$P, P) (should be TRUE): ', identical(fcl_3.2.P$P, P))
is.null(fcl_3.2.P$SqrtSigma)# TRUE
message('identical(fcl_3.2.P$Q, Q) (should be TRUE): ', identical(fcl_3.2.P$Q, Q))

message('identical(fcl_3.2.SqrtSigma$Sigma, Sigma) (should be TRUE): ', identical(fcl_3.2.SqrtSigma$Sigma, Sigma))
message('identical(fcl_3.2.SqrtSigma$P, P) (should be TRUE): ', identical(fcl_3.2.SqrtSigma$P, P))
message('identical(fcl_3.2.SqrtSigma$SqrtSigma, 1/Q) (should be TRUE): ', identical(fcl_3.2.SqrtSigma$SqrtSigma, 1/Q))
message('identical(fcl_3.2.SqrtSigma$Q, Q) (should be TRUE): ', identical(fcl_3.2.SqrtSigma$Q, Q))

message('identical(fcl_3.2.Q$Sigma, Sigma) (should be TRUE): ', identical(fcl_3.2.Q$Sigma, Sigma))
message('identical(fcl_3.2.Q$P, P) (should be TRUE): ', identical(fcl_3.2.Q$P, P))
is.null(fcl_3.2.Q$SqrtSigma)# TRUE
message('identical(fcl_3.2.Q$Q, Q) (should be TRUE): ', identical(fcl_3.2.Q$Q, Q))




# CL_3.3: Sigma, SqrtSigma, and Q supplied.

fcl_3.3.Sigma <- fill_covariance_list(need = "Sigma", covariance_list = CL_3.3)
fcl_3.3.P <- fill_covariance_list(need = "P", covariance_list = CL_3.3)
fcl_3.3.SqrtSigma <- fill_covariance_list(need = "SqrtSigma", covariance_list = CL_3.3)
fcl_3.3.Q <- fill_covariance_list(need = "Q", covariance_list = CL_3.3)

message('identical(fcl_3.3.Sigma$Sigma, Sigma) (should be TRUE): ', identical(fcl_3.3.Sigma$Sigma, Sigma))
is.null(fcl_3.3.Sigma$P)# TRUE
message('identical(fcl_3.3.Sigma$SqrtSigma, SqrtSigma) (should be TRUE): ', identical(fcl_3.3.Sigma$SqrtSigma, SqrtSigma))
message('identical(fcl_3.3.Sigma$Q, Q) (should be TRUE): ', identical(fcl_3.3.Sigma$Q, Q))

message('identical(fcl_3.3.P$Sigma, Sigma) (should be TRUE): ', identical(fcl_3.3.P$Sigma, Sigma))
message('identical(fcl_3.3.P$P, Q * Q) (should be TRUE): ', identical(fcl_3.3.P$P, Q * Q))
message('identical(fcl_3.3.P$SqrtSigma, SqrtSigma) (should be TRUE): ', identical(fcl_3.3.P$SqrtSigma, SqrtSigma))
message('identical(fcl_3.3.P$Q, Q) (should be TRUE): ', identical(fcl_3.3.P$Q, Q))

message('identical(fcl_3.3.SqrtSigma$Sigma, Sigma) (should be TRUE): ', identical(fcl_3.3.SqrtSigma$Sigma, Sigma))
is.null(fcl_3.3.SqrtSigma$P)# TRUE
message('identical(fcl_3.3.SqrtSigma$SqrtSigma, SqrtSigma) (should be TRUE): ', identical(fcl_3.3.SqrtSigma$SqrtSigma, SqrtSigma))
message('identical(fcl_3.3.SqrtSigma$Q, Q) (should be TRUE): ', identical(fcl_3.3.SqrtSigma$Q, Q))

message('identical(fcl_3.3.Q$Sigma, Sigma) (should be TRUE): ', identical(fcl_3.3.Q$Sigma, Sigma))
is.null(fcl_3.3.Q$P)# TRUE
message('identical(fcl_3.3.Q$SqrtSigma, SqrtSigma) (should be TRUE): ', identical(fcl_3.3.Q$SqrtSigma, SqrtSigma))
message('identical(fcl_3.3.Q$Q, Q) (should be TRUE): ', identical(fcl_3.3.Q$Q, Q))




# CL_3.4: P, SqrtSigma, and Q supplied.

fcl_3.4.Sigma <- fill_covariance_list(need = "Sigma", covariance_list = CL_3.4)
fcl_3.4.P <- fill_covariance_list(need = "P", covariance_list = CL_3.4)
fcl_3.4.SqrtSigma <- fill_covariance_list(need = "SqrtSigma", covariance_list = CL_3.4)
fcl_3.4.Q <- fill_covariance_list(need = "Q", covariance_list = CL_3.4)

message('identical(fcl_3.4.Sigma$Sigma, SqrtSigma * SqrtSigma) (should be TRUE): ', identical(fcl_3.4.Sigma$Sigma, SqrtSigma * SqrtSigma))
message('identical(fcl_3.4.Sigma$P, P) (should be TRUE): ', identical(fcl_3.4.Sigma$P, P))
message('identical(fcl_3.4.Sigma$SqrtSigma, SqrtSigma) (should be TRUE): ', identical(fcl_3.4.Sigma$SqrtSigma, SqrtSigma))
message('identical(fcl_3.4.Sigma$Q, Q) (should be TRUE): ', identical(fcl_3.4.Sigma$Q, Q))

is.null(fcl_3.4.P$Sigma)# TRUE
message('identical(fcl_3.4.P$P, P) (should be TRUE): ', identical(fcl_3.4.P$P, P))
message('identical(fcl_3.4.P$SqrtSigma, SqrtSigma) (should be TRUE): ', identical(fcl_3.4.P$SqrtSigma, SqrtSigma))
message('identical(fcl_3.4.P$Q, Q) (should be TRUE): ', identical(fcl_3.4.P$Q, Q))

is.null(fcl_3.4.SqrtSigma$Sigma)# TRUE
message('identical(fcl_3.4.SqrtSigma$P, P) (should be TRUE): ', identical(fcl_3.4.SqrtSigma$P, P))
message('identical(fcl_3.4.SqrtSigma$SqrtSigma, SqrtSigma) (should be TRUE): ', identical(fcl_3.4.SqrtSigma$SqrtSigma, SqrtSigma))
message('identical(fcl_3.4.SqrtSigma$Q, Q) (should be TRUE): ', identical(fcl_3.4.SqrtSigma$Q, Q))

is.null(fcl_3.4.Q$Sigma)# TRUE
message('identical(fcl_3.4.Q$P, P) (should be TRUE): ', identical(fcl_3.4.Q$P, P))
message('identical(fcl_3.4.Q$SqrtSigma, SqrtSigma) (should be TRUE): ', identical(fcl_3.4.Q$SqrtSigma, SqrtSigma))
message('identical(fcl_3.4.Q$Q, Q) (should be TRUE): ', identical(fcl_3.4.Q$Q, Q))




# CL_4: Sigma, P, SqrtSigma, and Q supplied.

fcl_4.Sigma <- fill_covariance_list(need = "Sigma", covariance_list = CL_4)
fcl_4.P <- fill_covariance_list(need = "P", covariance_list = CL_4)
fcl_4.SqrtSigma <- fill_covariance_list(need = "SqrtSigma", covariance_list = CL_4)
fcl_4.Q <- fill_covariance_list(need = "Q", covariance_list = CL_4)

message('identical(fcl_4.Sigma$Sigma, Sigma) (should be TRUE): ', identical(fcl_4.Sigma$Sigma, Sigma))
message('identical(fcl_4.Sigma$P, P) (should be TRUE): ', identical(fcl_4.Sigma$P, P))
message('identical(fcl_4.Sigma$SqrtSigma, SqrtSigma) (should be TRUE): ', identical(fcl_4.Sigma$SqrtSigma, SqrtSigma))
message('identical(fcl_4.Sigma$Q, Q) (should be TRUE): ', identical(fcl_4.Sigma$Q, Q))

message('identical(fcl_4.P$Sigma, Sigma) (should be TRUE): ', identical(fcl_4.P$Sigma, Sigma))
message('identical(fcl_4.P$P, P) (should be TRUE): ', identical(fcl_4.P$P, P))
message('identical(fcl_4.P$SqrtSigma, SqrtSigma) (should be TRUE): ', identical(fcl_4.P$SqrtSigma, SqrtSigma))
message('identical(fcl_4.P$Q, Q) (should be TRUE): ', identical(fcl_4.P$Q, Q))

message('identical(fcl_4.SqrtSigma$Sigma, Sigma) (should be TRUE): ', identical(fcl_4.SqrtSigma$Sigma, Sigma))
message('identical(fcl_4.SqrtSigma$P, P) (should be TRUE): ', identical(fcl_4.SqrtSigma$P, P))
message('identical(fcl_4.SqrtSigma$SqrtSigma, SqrtSigma) (should be TRUE): ', identical(fcl_4.SqrtSigma$SqrtSigma, SqrtSigma))
message('identical(fcl_4.SqrtSigma$Q, Q) (should be TRUE): ', identical(fcl_4.SqrtSigma$Q, Q))

message('identical(fcl_4.Q$Sigma, Sigma) (should be TRUE): ', identical(fcl_4.Q$Sigma, Sigma))
message('identical(fcl_4.Q$P, P) (should be TRUE): ', identical(fcl_4.Q$P, P))
message('identical(fcl_4.Q$SqrtSigma, SqrtSigma) (should be TRUE): ', identical(fcl_4.Q$SqrtSigma, SqrtSigma))
message('identical(fcl_4.Q$Q, Q) (should be TRUE): ', identical(fcl_4.Q$Q, Q))
