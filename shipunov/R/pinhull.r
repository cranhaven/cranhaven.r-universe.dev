Pinhull <- function(pts, ppts) {
ncol <- length(ppts)
out <- matrix(FALSE, ncol=ncol, nrow=nrow(pts))
colnames (out) <- names(ppts)
events <- data.frame(EID=1:nrow(pts), X=pts[, 1], Y=pts[, 2])
for (i in seq_len(ncol)) {
 ppol <- data.frame(ppts[[i]], PID=i, POS=seq_len(nrow(ppts[[i]])))
 names(ppol)[1:2] <- c("X", "Y")
 eids <- PBSmapping::findPolys(events, ppol)$EID
 out[eids, i] <- TRUE
 }
out
}
