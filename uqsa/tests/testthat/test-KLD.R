test_that("KLD for multivariate normal, with fitCopula",{
	muA <- c(0,0)
	SigmaA <- matrix(c(1,0.2,0.2,1),2,2)
	muB <- c(0.1,-0.2)
	SigmaB <- matrix(c(1,0.4,0.4,1),2,2)
	if (requireNamespace("MASS", quietly = TRUE)){
		set.seed(42)
		N <- 1e2
		X <- MASS::mvrnorm(N,muA,SigmaA)
		Y <- MASS::mvrnorm(N,muB,SigmaB)
		D <- KLD(X,Y)
		E <- exact_normal_kld(muA,SigmaA,muB,SigmaB)
		expect_lt(abs(D-E),1e-1*length(muA))
	}
})

test_that("KLD for multivariate normal, with ks",{
	muA <- c(0,0)
	SigmaA <- matrix(c(1,0.2,0.2,1),2,2)
	muB <- c(0.1,-0.2)
	SigmaB <- matrix(c(1,0.4,0.4,1),2,2)
	if (requireNamespace("MASS", quietly = TRUE)){
		set.seed(42)
		N <- 1e2
		X <- MASS::mvrnorm(N,muA,SigmaA)
		Y <- MASS::mvrnorm(N,muB,SigmaB)
		D <- KLD(X,Y,de="ks")
		E <- exact_normal_kld(muA,SigmaA,muB,SigmaB)
		expect_lt(abs(D-E),1e-1*length(muA))
	}
})

test_that("KLD for multivariate normal, with mvtnorm",{
	muA <- c(0,0)
	SigmaA <- matrix(c(1,0.2,0.2,1),2,2)
	muB <- c(0.1,-0.2)
	SigmaB <- matrix(c(1,0.4,0.4,1),2,2)
	if (requireNamespace("MASS", quietly = TRUE)){
		set.seed(42)
		N <- 1e2
		X <- MASS::mvrnorm(N,muA,SigmaA)
		Y <- MASS::mvrnorm(N,muB,SigmaB)
		D <- KLD(X,Y,de="mvtnorm")
		E <- exact_normal_kld(muA,SigmaA,muB,SigmaB)
		expect_lt(abs(D-E),1e-1*length(muA))
	}
})
