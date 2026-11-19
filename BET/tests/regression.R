# Small, deterministic/frozen-fixture tests; no full study or benchmark.
library(BET)
stopifnot(as.character(packageVersion("BET")) == "0.6.0")
fixtures <- readRDS("fixtures/inputs.rds")
expected <- readRDS("fixtures/expected_054.rds")
api <- readRDS("fixtures/api_054.rds")
checks <- 0L
ok <- function(x) {stopifnot(isTRUE(x));checks <<- checks+1L;invisible(TRUE)}
error <- function(expr, pattern=NULL) {
    ans <- tryCatch({force(expr);NULL},error=function(e)e)
    ok(inherits(ans,"error"))
    if (!is.null(pattern)) ok(grepl(pattern,conditionMessage(ans),fixed=TRUE))
}
inside <- function(name) getFromNamespace(paste0(".beast_asym_",name),"BET")
RNGkind("Mersenne-Twister","Inversion","Rejection")
for (id in names(fixtures)) {
    f <- fixtures[[id]]; X <- f$X
    for (name in names(api)) {
        fun <- getExportedValue("BET",name)
        ok(identical(formals(fun),api[[name]]$formals))
        ok(identical(deparse(body(fun),width.cutoff=500L),api[[name]]$body))
    }
    set.seed(f$statistic_seed)
    fit <- BEAST(X,3,subsample.percent=f$fraction,B=f$B,lambda=f$lambda,
                 index=list(1L,2L),method="stat")
    after <- .Random.seed
    ok(identical(fit,expected[[id]]$BEAST))
    set.seed(f$statistic_seed)
    repeat_fit <- BEAST(X,3,subsample.percent=f$fraction,B=f$B,lambda=f$lambda,
                        index=list(1L,2L),method="stat")
    ok(identical(fit,repeat_fit) && identical(.Random.seed,after))
    ok(identical(MaxBET(X,3,index=list(1L,2L)),expected[[id]]$MaxBET))
    ok(identical(MaxBETs(X,3,index=list(1L,2L)),expected[[id]]$MaxBETs))
    ok(identical(cell.counts(X,3),expected[[id]]$cell.counts))
    ok(identical(symm(X,3,print.sample.size=FALSE),expected[[id]]$symm))
    ok(identical(get.signs(X,3),expected[[id]]$get.signs))
}
# Plotting API source/formals frozen above; output goes only to tempdir().
p <- tempfile(fileext=".pdf");grDevices::pdf(p)
bet.plot(fixtures[[1]]$X,3,index=list(1L,2L));grDevices::dev.off()
ok(file.info(p)$size>1000);unlink(p)
set.seed(819L);saved_rng <- .Random.seed; saved_kind <- RNGkind()
cal <- BEAST.asymptotic.calibrate(64,dep=2,subsample.percent=.25,B=4,
                                  lambda=0,H=12,G=64,seed=73002L)
ok(identical(.Random.seed,saved_rng) && identical(RNGkind(),saved_kind))
cal2 <- BEAST.asymptotic.calibrate(64,dep=2,subsample.percent=.25,B=4,
                                   lambda=0,H=12,G=64,seed=73002L)
ok(identical(cal,cal2))
ok(inherits(cal,"BEASTAsymptoticCalibration"))
ok(cal$q==(2^cal$dep-1)^2 && cal$r==16L && cal$B==4L && cal$H==12L && cal$G==64L)
ok(identical(dim(cal$Gamma),c(18L,18L)) && length(cal$G0)==64L)
ok(cal$lambda==0 && cal$package.version=="0.6.0" && cal$rng$ncores==1L)
ok(cal$nonzero.fraction==mean(cal$G0!=0))
ok(cal$interaction.collection=="bivariate-cross-ascending-masks")
ok(identical(cal$index,list(1L,2L)))
negative <- inside("clip_covariance")(diag(c(-1,2)))
ok(negative$minimum_raw_eigenvalue==-1 && negative$clipped_eigenvalues==1L)
ok(isTRUE(all.equal(negative$Gamma,diag(c(0,2)),tolerance=1e-14)))
ok(identical(inside("map")(matrix(c(0,0,3,4),nrow=1),2,0),0))
p <- tempfile(fileext=".rds");saveRDS(cal,p);ok(identical(readRDS(p),cal));unlink(p)
X1 <- fixtures$deterministic_negative_n64$X
X2 <- cbind(exp(X1[,1]),X1[,2]^3)
for(X in list(X1,X2)) for(alpha in c(.05,.10,.20)) {
    set.seed(63L)
    direct <- BEAST(X,2,subsample.percent=cal$r/cal$n,B=cal$B,
                    lambda=cal$lambda,index=list(1L,2L),method="stat")
    before <- .Random.seed
    fit <- BEAST.asymptotic(X,cal,alpha=alpha,seed=63L)
    ok(identical(.Random.seed,before))
    ok(identical(fit$BEAST.Statistic,unname(direct$BEAST.Statistic)))
    ok(identical(fit$Interaction,direct$Interaction))
    ok(identical(fit$Scaled.Statistic,sqrt(63)*fit$BEAST.Statistic))
    ok(identical(fit$p.value,mean(cal$G0>=fit$Scaled.Statistic)))
    expected_cv <- sort(cal$G0)[ceiling((1-alpha)*cal$G)]
    ok(identical(fit$critical.value,unname(expected_cv)))
    ok(identical(fit$reject,unname(fit$Scaled.Statistic>expected_cv)))
}
# Original lambda default and fractional truncation, not rounded fractions.
def <- BEAST.asymptotic.calibrate(128,dep=3,subsample.percent=.19,
                                  B=2,H=4,G=8,seed=2)
ok(def$r==24L && def$lambda==sqrt(log(2^6)/(8*128)))
set.seed(891L); c1 <- BEAST.asymptotic.calibrate(32,2,.5,B=2,H=4,G=8)
set.seed(891L); c2 <- BEAST.asymptotic.calibrate(32,2,.5,B=2,H=4,G=8)
ok(identical(c1,c2))
args <- list(n=64,dep=2,subsample.percent=.25,B=2,H=4,G=8,seed=1)
for (change in list(list(n=1),list(n=63),list(dep=0),list(dep=1.5),list(dep=6),
                    list(subsample.percent=0),list(subsample.percent=1.1),list(subsample.percent=NA_real_),
                    list(B=0),list(B=1.2),list(H=1),list(H=Inf),list(G=0),list(G=NA_real_),
                    list(lambda=-1),list(lambda=Inf),list(ncores=2),list(seed=-1)))
    error(do.call(BEAST.asymptotic.calibrate,utils::modifyList(args,change)))
error(BEAST.asymptotic.calibrate(176,dep=3,subsample.percent=.682,
                                  B=2,H=4,G=8,seed=1),"native integer truncation")
error(BEAST.asymptotic(X1[-1,],cal),"nrow(X)")
error(BEAST.asymptotic(cbind(X1,X1[,1]),cal),"only for bivariate independence")
error(BEAST.asymptotic(X1[,1,drop=FALSE],cal),"only for bivariate independence")
error(BEAST.asymptotic(matrix("a",64,2),cal),"numeric")
error(BEAST.asymptotic(replace(X1,1,NA_real_),cal),"finite")
error(BEAST.asymptotic(replace(X1,1,Inf),cal),"finite")
error(BEAST.asymptotic(cbind(1,X1[,2]),cal),"nonconstant")
error(BEAST.asymptotic(X1,list()),"Invalid BEAST")
for(field in c("n","dep","B","r","lambda","q","H","G")) {
    z <- cal;z[[field]] <- z[[field]]+1
    error(BEAST.asymptotic(X1,z),"Invalid BEAST")
}
z<-cal;z$schema.version<-2L;error(BEAST.asymptotic(X1,z),"schema")
z<-cal;z$Gamma<-matrix(0,2,2);error(BEAST.asymptotic(X1,z),"Gamma")
z<-cal;z$G0[1]<-NA_real_;error(BEAST.asymptotic(X1,z),"G0")
z<-cal;z$basis[1,1]<-!z$basis[1,1];error(BEAST.asymptotic(X1,z),"interaction")
z<-cal;z$G0[]<-0;z$nonzero.fraction<-0;error(BEAST.asymptotic(X1,z),"Nonpositive")
for(a in c(0,1,NA_real_,Inf)) error(BEAST.asymptotic(X1,cal,alpha=a),"alpha")
warn <- character();tied <- X1;tied[2,1] <- tied[1,1]
ans <- withCallingHandlers(BEAST.asymptotic(tied,cal,seed=63),warning=function(w) {
    warn <<- c(warn,conditionMessage(w));invokeRestart("muffleWarning")
})
ok(ans$ties && length(warn)==1L && grepl("continuous margins",warn) && grepl("permutation",warn))
capture.output(print(cal));capture.output(print(ans))
cat("PASS:",checks,"lightweight regression assertions; six frozen fixtures; no full benchmark or power simulation.\n")
