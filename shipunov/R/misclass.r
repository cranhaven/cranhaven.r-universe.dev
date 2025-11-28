Misclass <- function (pred, obs, best=FALSE, ignore=NULL, quiet=FALSE, force=FALSE, ...)
{
if (!is.null(ignore)) {
 pred[as.character(pred) %in% as.character(ignore)] <- NA
 obs[as.character(obs) %in% as.character(ignore)] <- NA
}
predl <- levels(factor(pred))
obsl <- levels(factor(obs))
predn <- length(predl)
obsn <- length(obsl)
if (predn < 2 || obsn < 2) stop("Both 'pred' and 'obs' must have > 2 classes")
if (best) {
 if (!force & (predn > 8 || obsn > 8)) stop("Too many classes (> 8) for 'best=TRUE'; override with 'force=TRUE'")
 if (force & length(list(...))) warning("The tabulate()-based code does not take table()-specific arguments, results might be inappropriate")
 .permutations <- function(n, r=n, v) { # adopted from 'gtools' package
  sub <- function(n, r, v) {
   if (r == 1) matrix(v, n, 1)
   else if (n == 1) matrix(v, 1, r)
   else { X <- NULL
    for (i in 1:n) X <- rbind(X, cbind(v[i], Recall(n - 1, r - 1, v[-i])))
    X }}
  sub(n, r, v[1:n])
 }
 if (obsn >= predn) {
  alln <- .permutations(n=obsn, v=obsl)
  alll <- numeric(nrow(alln))
  for (x in seq_len(nrow(alln))) {
   obs <- factor(obs, levels=as.character(alln[x, ]))
   if (force) {
    tbl <- matrix(tabulate(pred + predn*as.integer(obs), nbins=predn+obsn^2)[-(1:predn)], nrow=obsn, byrow=TRUE)
    } else {
    tbl <- table(pred, obs, ...)
   }
   dif <- nrow(tbl) - ncol(tbl)
   if (dif < 0) tbl <- rbind(tbl, matrix(0, nrow=-dif, ncol=ncol(tbl)))
   summ <- colSums(tbl)
   dia <- tbl[seq.int(1, obsn^2, obsn+1)]
   alll[x] <- mean(100 * (summ - dia)/summ)
   }
  allmin <- which.min(alll)
  obs <- factor(obs, levels=as.character(alln[allmin, ]))
  tbl <- table(pred, obs, ...)
  dif <- nrow(tbl) - ncol(tbl)
  if (dif < 0) tbl <- rbind(tbl, matrix(0, nrow=-dif, ncol=ncol(tbl)))
  summ <- colSums(tbl)
  dia <- diag(tbl)
  msc <- 100 * (summ - dia)/summ
  m.m <- mean(msc)
  } else {
  alln <- .permutations(n=predn, v=predl)
  alll <- numeric(nrow(alln))
  for (x in seq_len(nrow(alln))) {
   pred <- factor(pred, levels=as.character(alln[x, ]))
   if (force) {
    tbl <- matrix(tabulate(as.integer(pred) + predn*obs, nbins=predn+predn^2)[-(1:predn)], nrow=predn, byrow=TRUE)
    } else {
    tbl <- table(pred, obs, ...)
   }
   dif <- nrow(tbl) - ncol(tbl)
   tbl <- cbind(tbl, matrix(0, ncol=dif, nrow=nrow(tbl)))
   summ <- colSums(tbl) # some of them could be 0
   dia <- tbl[seq.int(1, predn^2, predn+1)]
   msc <- 100 * (summ - dia)/summ
   msc <- msc[is.finite(msc)]
   alll[x] <- mean(msc)
   }
  allmin <- which.min(alll)
  pred <- factor(pred, levels=as.character(alln[allmin, ]))
  tbl <- table(pred, obs, ...)
  dif <- nrow(tbl) - ncol(tbl)
  tbl <- cbind(tbl, matrix(0, ncol=dif, nrow=nrow(tbl)))
  summ <- colSums(tbl)
  dia <- diag(tbl)
  msc <- 100 * (summ - dia)/summ
  msc <- msc[is.finite(msc)] # remove division by 0 results
  m.m <- mean(msc)
  }
} else {
 tbl <- table(pred, obs, ...)
 dif <- nrow(tbl) - ncol(tbl)
 if (dif !=0) stop("When 'best=FALSE' (default), numbers of classes must be equal")
 sum <- colSums(tbl)
 dia <- diag(tbl)
 msc <- 100 * (sum - dia)/sum
 m.m <- mean(msc)
}
if (!quiet) {
if (best) cat("Best c") else cat("C")
cat("lassification table:\n")
print(tbl)
cat("Misclassification errors (%):\n")
print(round(msc, 1))
cat("Mean misclassification error: ", round(m.m, 1), "%\n", sep="")
if (sum(is.na(c(pred, obs))) > 0) cat("Note: data contains NAs\n")
}
invisible(tbl)
}
