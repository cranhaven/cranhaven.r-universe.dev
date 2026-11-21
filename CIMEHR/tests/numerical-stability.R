library(CIMEHR)
ns <- asNamespace("CIMEHR")
checked_se <- get(".cimehr_checked_se", ns)
covariance_factor <- get(".cimehr_covariance_factor", ns)
factor_variance <- get(".cimehr_factor_variance", ns)
stable_matvec <- get(".cimehr_stable_matvec", ns)
add_curve_se <- get(".trajectory_add_se", ns)

# Component-specific missing and invalid variances.
variance <- c(positive=4, zero=0, negative=-1e-30,
    missing=NA_real_, infinite=Inf, not_a_number=NaN)
warnings <- list()
se <- withCallingHandlers(checked_se(variance), warning=function(w) {
    warnings[[length(warnings)+1L]] <<- w
    invokeRestart("muffleWarning")
})
stopifnot(identical(names(se),names(variance)),
    identical(unname(se[1:2]),c(2,0)), all(is.na(se[3:6])),
    length(warnings)==1L,
    inherits(warnings[[1L]],"cimehr_numerical_warning"),
    identical(warnings[[1L]]$code,"invalid_reported_variance"))
V <- diag(c(1,4));dimnames(V)<-list(c("a","b"),c("a","b"))
stopifnot(identical(checked_se(V),c(a=1,b=2)))

# Column order and rank of the compact factor.
IF <- cbind(beta=seq(-2,2,length.out=9),
    compensation=c(1,-1,2,-2,0,1,-1,2,-2),
    baseline=rep(c(-1,0,1),3))
F <- covariance_factor(IF)
stopifnot(identical(colnames(F),colnames(IF)),
    nrow(F)<=ncol(IF), ncol(F)==ncol(IF),
    max(abs(crossprod(F)-crossprod(IF)))<1e-11)
D <- rbind(c(1,-.25,.5),c(0,1,-1))
colnames(D)<-colnames(IF)
expected <- rowSums((D%*%t(IF))^2)
stopifnot(max(abs(factor_variance(D,F)-expected))<1e-11,
    max(abs(factor_variance(D,F[,c(3,1,2),drop=FALSE])-expected))<1e-11)
bad_D <- D;colnames(bad_D)[1L]<-"unknown"
stopifnot(inherits(tryCatch(factor_variance(bad_D,F),error=identity),"error"))

# Contrast variance under covariance rounding.
near_IF <- rbind(c(1e8,1e8),c(0,1))
colnames(near_IF)<-c("beta","compensation")
near_F <- covariance_factor(near_IF)
contrast <- matrix(c(1,-1),1L,dimnames=list(NULL,colnames(near_IF)))
truth_variance <- 1
stopifnot(abs(factor_variance(contrast,near_F)-truth_variance)<1e-10,
    abs(rowSums((contrast%*%crossprod(near_IF))*contrast))<1e-8)
# Curve contrasts with a factor and legacy covariance.
invalid_V <- matrix(-1,2L,2L)
curve <- add_curve_se(data.frame(time=0,fit=.5),contrast,invalid_V,
    se.fit=TRUE,level=.95,factor=near_F)
stopifnot(abs(curve$se-1)<1e-10,
    abs(curve$lower-(.5-qnorm(.975)))<1e-10,
    abs(curve$upper-(.5+qnorm(.975)))<1e-10)

# Compensated dot products with exactly representable inputs.
M <- rbind(first=c(2^55,1,-2^55),second=c(2^40,.25,-2^40))
x <- rep(1,3)
stopifnot(identical(stable_matvec(M,x),c(first=1,second=.25)))
# Well-scaled dot product.
small_M <- rbind(c(1,2,3),c(-2,0,.5))
stopifnot(max(abs(stable_matvec(small_M,c(.5,1,-2))-
    drop(small_M%*%c(.5,1,-2))))<1e-14)
