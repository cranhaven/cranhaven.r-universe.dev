library("sparseHessianFD")
library("Matrix")
library("mvtnorm")
library("microbenchmark")
library("doParallel")
library("dplyr")
library("tidyr")
library("numDeriv")
library("reshape2")
library("ggplot2")

set.seed(1234)

registerDoParallel(cores=10)

binary_sim <- function(N, k, T) {
  x.mean <- rep(0,k)
  x.cov <- diag(k)
  x.cov[1,1] <- .02
  x.cov[k,k] <- x.cov[1,1]
  mu <- seq(-2,2,length=k)
  Omega <- diag(k)
  X <- t(rmvnorm(N, mean=x.mean, sigma=x.cov)) ## k x N
  B <- t(rmvnorm(N, mean=mu, sigma=Omega)) ## k x N
  XB <- colSums(X * B)
  log.p <- XB - log1p(exp(XB))
  Y <- sapply(log.p, function(q) return(rbinom(1,T,exp(q))))
  list(Y=Y, X=X, T=T)
}

priors_sim <- function(k) {
  list(inv.Omega = solve(diag(k)),
       inv.Sigma = rWishart(1,k+5,diag(k))[,,1])
}


make_funcs <- function(D, priors, order.row=FALSE) {
  res <- vector("list", length=3)
  names(res) <- c("fn", "gr", "hessian")
  res$fn <-  function(pars) binary.f(pars, data=D, priors=priors, order.row=order.row)
  res$gr <-  function(pars) binary.grad(pars, data=D, priors=priors, order.row=order.row)
  res$hessian <-  function(pars) binary.hess(pars, data=D, priors=priors, order.row=order.row)
  return(res)
}

make_perm <- function(iRow, jCol) {
  tmp <- sparseMatrix(i=iRow, j=jCol, index1=TRUE, symmetric=TRUE)
  perm <- order(Matrix::rowSums(tmp), decreasing=TRUE)
  invperm <- invPerm(perm)
  L <- tril(tmp[perm,perm])
  ptr <- Matrix.to.Pointers(L, order="row", index1=TRUE)
  idx <- ptr[[1]]
  pntr <- ptr[[2]]
}

get_id <- function(x) 1:length(x)

run_test_fig4 <- function(NkT, reps=50, order.row=FALSE) {
  ## Replication function for Figure 4
  N <- as.numeric(NkT["N"])
  k <- as.numeric(NkT["k"])
  T <- as.numeric(NkT["T"])
  data <- binary_sim(N, k, T)
  priors <- priors_sim(k)
  F <- make_funcs(D=data, priors=priors, order.row=order.row)
  nvars <- N*k+k

  if (order.row) {
      mm <- Matrix::kronecker(Matrix(1,k,k),Matrix::Diagonal(N))
  } else {
      mm <- Matrix::kronecker(Matrix::Diagonal(N),Matrix(1,k,k))
  }
  M <- rbind(mm,Matrix(TRUE,k,N*k)) %>%
       cbind(Matrix(TRUE, k*(N+1), k)) %>%
       as("sparseMatrix") %>%
       as("nMatrix")

  pat <- Matrix.to.Coord(tril(M))
  X <- rnorm(nvars)

  obj <- sparseHessianFD(X, F$fn, F$gr, pat$rows, pat$cols, complex=TRUE)
  colors <- obj$partition()
  perm <- obj$get_perm()
  ncolors <- length(unique(colors))
  nvars <- N*k+k

  bench <- microbenchmark(
      setup = sparseHessianFD(X, F$fn, F$gr, pat$rows, pat$cols),
      colors = coloring(tril(M[perm,perm])),
      perm = make_perm(pat$rows, pat$cols),
      f = obj$fn(X),
      df = obj$gr(X),
      hess = obj$hessian(X),
      times = reps)

  vals <- plyr::ddply(data.frame(bench), "expr",
                function(x) return(data.frame(expr=x$expr,
                                              time=x$time,
                                              rep=1:length(x$expr))))

  res <- data.frame(N=N, k=k, T=T,
                    bench=vals,
                    ncolors=ncolors)

  cat("Completed N = ",N,"\tk = " , k , "\tT = ",T,"\n")

  return(res)

}


run_test_tab4 <- function(Nk, reps=50, order.row=TRUE) {
    ## Replication function for Table 4
    N <- as.numeric(Nk["N"])
    k <- as.numeric(Nk["k"])
    data <- binary_sim(N, k, T=20)
    priors <- priors_sim(k)
    F <- make_funcs(D=data, priors=priors, order.row=order.row)
    nvars <- N*k+k

    if (order.row) {
        mm <- Matrix::kronecker(Matrix(1,k,k),Matrix::Diagonal(N))
    } else {
        mm <- Matrix::kronecker(Matrix::Diagonal(N),Matrix(1,k,k))
    }
    M <- rbind(mm,Matrix(TRUE,k,N*k)) %>%
        cbind(Matrix(TRUE, k*(N+1), k)) %>%
        as("sparseMatrix") %>%
        as("nMatrix")

    pat <- Matrix.to.Coord(tril(M))
    X <- rnorm(nvars)
    obj <- sparseHessianFD(X, F$fn, F$gr, pat$rows, pat$cols, complex=FALSE)
    obj2 <- sparseHessianFD(X, F$fn, F$gr, pat$rows, pat$cols, complex=TRUE)

    h1 <- obj$hessian(X)
    h2 <- obj2$hessian(X)
    h3 <- drop0(numDeriv::jacobian(obj$gr, X, method="complex"),tol=1e-7)

    stopifnot(all.equal(h1,h2, tolerance=1e-7))
    stopifnot(all.equal(h1,h3,tolerance=1e-7))

    bench <- microbenchmark(
        jac = numDeriv::jacobian(obj$gr, X, method="simple"),
        cplx = numDeriv::jacobian(obj$gr, X, method="complex"),
        df = obj$gr(X),
        sp = obj$hessian(X),
        sp_cplx = obj2$hessian(X)
        )
    vals <- plyr::ddply(data.frame(bench), "expr",
                  function(x) return(data.frame(expr=x$expr,
                                                time=x$time,
                                                rep=1:length(x$expr))))
    res <- data.frame(N=N, k=k,
                      bench=vals)
    cat("Completed N = ",N,"\tk = " , k ,"\n")

    return(res)
}

## Replicate Figure 4

cases_fig4 <- expand.grid(k=c(8, 6, 4, 2),
                          N=c(25, 50, 75, seq(100,2000, by=100)),
                          T=20
)
reps_fig4 <- 100

runs_fig4 <- plyr::adply(cases_fig4, 1, run_test_fig4, reps=reps_fig4,
                         order.row=TRUE, .parallel=TRUE)

tab_fig4 <- mutate(runs_fig4, ms=bench.time/1000000) %>%
  dcast(N+k+T+bench.rep+ncolors~bench.expr, value.var="ms")  %>%
  mutate(nvars=N*k+k, hess_df=hess/df) %>%
  gather(stat, ms, c(f,df,hess,colors,setup,hess_df)) %>%
  group_by(N, k, T, stat, nvars) %>%
  summarize(mean=mean(ms))


D2 <- filter(data.frame(tab_fig4), stat %in% c("f", "df", "hess",
                                               "colors", "setup","hess_df"))
D2$stat <- plyr::revalue(D2$stat, c("f"="Function", "df"="Gradient", "hess"="Hessian",
                              "colors"="Partitioning",
                              "setup"="Initialization",
                              "hess_df"="Hessian/Gradient"))

D2$stat <- factor(D2$stat, levels=c("Function","Gradient","Hessian",
                                    "Partitioning","Initialization",
                                    "Hessian/Gradient"))

theme_set(theme_bw())
fig4 <- ggplot(D2, aes(x=N,y=mean, color=as.factor(k), linetype=as.factor(k))) %>%
    + geom_line(size=.4) %>%
    + scale_x_continuous("Number of heterogeneous units") %>%
    + scale_y_continuous("Computation time (milliseconds)") %>%
    + guides(color=guide_legend("k"), linetype=guide_legend("k")) %>%
    + facet_wrap(~stat, scales="free") %>%
    + theme(text=element_text(size=10), legend.position="right")


## Replicate Table 4

cases_tab4 <- expand.grid(k=c(2,4,8),
                          N=c(15, 50, 100, 500))

runs_tab4 <- plyr::adply(cases_tab4, 1, run_test_tab4, reps=100,
                         order.row=TRUE, .parallel=FALSE)

tab4 <-  mutate(runs_tab4, ms=bench.time/1000000) %>%
  select(-bench.time) %>%
  spread(bench.expr, ms) %>%
  gather(method, hessian, c(cplx, jac, sp, sp_cplx)) %>%
  mutate(M=N*k+k, hessian.df=hessian/df) %>%
  gather(stat, time, c(hessian, hessian.df)) %>%
    group_by(N, k, method, M, stat)  %>%
    summarize(mean=mean(time), sd=sd(time)) %>%
    gather(stat2, value, c(mean,sd)) %>%
    dcast(N+k+M~stat+method+stat2,value.var="value") %>%
    arrange(k,N)

save(runs_tab4, tab4, D2, file="repl.Rdata")


