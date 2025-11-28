Overlap <- function(ppts, symmetric=FALSE, negative=FALSE)
{
ppol <- ppts
len <- length(ppol)
for (i in 1:len) {
 ppol[[i]] <- data.frame(ppol[[i]], PID=i, POS=seq_len(nrow(ppol[[i]])))
 names(ppol[[i]])[1:2] <- c("X", "Y")
 }
over.m <- matrix(ncol=len, nrow=len)
for (i in 2:len) {
 for (j in 1:i) {
  p.i <- ppol[[i]]
  p.j <- ppol[[j]]
  i.a <- PBSmapping::calcArea(p.i)$area
  j.a <- PBSmapping::calcArea(p.j)$area
  ij <- PBSmapping::joinPolys(p.i, p.j, operation="INT")
  if (is.null(ij)) {
   if (j != i) {
    if (negative) {
     ij <- rbind(p.i, p.j)[, 1:2]
     ij <- ij[chull(ij), ]
     p.ij <- data.frame(X=ij[, 1], Y=ij[, 2], PID=3, POS=seq_len(nrow(ij)))
     ij.a <- PBSmapping::calcArea(p.ij)$area
     over.m[j, i] <- over.m[i, j] <- -(ij.a - (i.a + j.a))/ij.a
     } else {
     over.m[j, i] <- over.m[i, j] <- 0
    }
   }
  } else {
   ij.a <- PBSmapping::calcArea(ij, rollup=1)$area # least detailed area
   if (symmetric) {
    over.m[i, j] <- over.m[j, i] <- ij.a/sqrt(i.a * j.a) # geometric
    } else {
    over.m[j, i] <- ij.a/i.a
    over.m[i, j] <- ij.a/j.a
   }
  }
 }
}
diag(over.m) <- 1
dimnames(over.m) <- list(names(ppts), names(ppts))
class(over.m) <- "Overlap"
return(over.m)
}

# ===

summary.Overlap <- function(object, ...) {
 object[object <= 0] <- NA # this is to make mean overlap calculated for true overlaps only
 diag(object) <- NA # same
 total.overlap <- round(rowSums(object, na.rm=TRUE)*100, 2)
 total.overlap[total.overlap > 100] <- 100 # tangled overlaps sometimes > 100
 mean.overlap <- round(rowMeans(object, na.rm=TRUE)*100, 2)
 mean.overlap[is.na(mean.overlap) | is.nan(mean.overlap)] <- 0
 res <- data.frame(mean.overlap, total.overlap)
 overall.overlap <- round(mean(object, na.rm=TRUE)*100, 2)
 cat("Overlaps for each hull, %:\n")
 print(res)
 cat("Mean of all overlaps", overall.overlap, "%\n")
}

## ===

BestOverlap <- function(xylabels, ci="95%", round=4)
{
LVL <- levels(factor(xylabels[[1]][, "labels"]))
RES <- vector("list", length=length(xylabels))
for (i in seq_along(xylabels)) {
 if (!identical(levels(factor(xylabels[[i]][, "labels"])), LVL)) next
 tmp <- Hulls(pts=xylabels[[i]][, c("x", "y")], groups=xylabels[[i]][, "labels"],  plot=FALSE)
 tmpo <- Overlap(tmp)
 RES[[i]]$tmpm <- mean(tmpo, na.rm=TRUE)
 RES[[i]]$tmpo <- tmpo
}
TMPO <- lapply(RES, `[[`, "tmpo")
NUMS <- sapply(TMPO, function(.x) as.data.frame(as.table(.x))[, "Freq"])
NUMS <- rbind(NUMS, sapply(TMPO, rowMeans, na.rm=TRUE))
NUMS <- rbind(NUMS, sapply(TMPO, rowSums, na.rm=TRUE))
NUMS <- rbind(NUMS, sapply(RES, `[[`, "tmpm"))
lower <- (1 - as.numeric(sub("%", "", ci))/100) / 2
CI <- t(apply(NUMS, 1, function(.x) round(quantile(.x, c(0, lower, 0.5, 1-lower, 1), na.rm=TRUE), round)))
NAMES <- as.data.frame(table(Class1=LVL, Class2=LVL), stringsAsFactors=FALSE)[, 1:2]
ADD <- matrix(c(LVL, LVL, "Total:", rep("mean:", length(LVL)), rep("total:", length(LVL)), ""), ncol=2)
NAMES <- rbind(as.matrix(NAMES), ADD)
SUMM <- cbind(data.frame(NAMES, stringsAsFactors=FALSE), CI)
SUMM <- SUMM[!(SUMM$Class1 == SUMM$Class2), ]
row.names(SUMM) <- NULL
MIN <- which.min(sapply(RES, `[[`, "tmpm"))
BESTO <- RES[[MIN]]$tmpo
return(list(best=MIN, best.overlap=BESTO, summary=SUMM))
}
