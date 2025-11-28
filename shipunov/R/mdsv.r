MDSv <- function(scores)
{
 scores <- na.omit(as.matrix(scores))
 res <- numeric(length=ncol(scores))
 scores.dc <- c(dist(scores))
 for (i in seq_len(ncol(scores))) {
  scores.i.dc <- c(dist(scores[, i]))
  res[i] <- summary(lm(scores.i.dc ~ scores.dc))$adj.r.squared
 }
 100*res/sum(res)
}

