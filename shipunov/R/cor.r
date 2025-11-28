Cor <- function(X, # matrix or data frame with values
stars=TRUE, # replaces p-values with stars if it not greater than "p.level"
dec=4, # round to 4
p.level=0.05, ...)
{
 nc <- ncol(X)
 cor.mat <- matrix(0, nc, nc)
 p.mat <- matrix(0, nc, nc)
 low.mat <- lower.tri(p.mat)
 for (i in 1:nc)
 {
 for (j in 1:nc)
 {
 if(low.mat[i,j])
 {
 cor.res <- cor.test(X[,i], X[,j], ...)
 cor.mat[j,i] <- cor.mat[i,j] <- cor.res$estimate
 p.mat[j,i] <- p.mat[i,j] <- cor.res$p.value
 }
 }
 }
 cor.mat <- round(cor.mat, dec)
 p.mat <- round(p.mat, dec)
 if (stars)
 {
 p <- ifelse(p.mat <= p.level, "*", " ")
 sep <- ""
 } else {
 p <- p.mat
 sep <- "/"
 }
 result <- matrix(paste(cor.mat, p, sep=sep), ncol=nc)
 result <- gsub("NANA", "NA", result)
 dimnames(result) <- list(colnames(X), colnames(X))
 diag(result) <- "- "
 return(data.frame(result))
}
##
Cor2 <- function(X, dec=4, p.level=0.05)
{
 R <- cor(X)
 above <- row(R) < col(R)
 R[!above] <- round(R[!above], dec)
 r2 <- R[above]^2
 dfr <- nrow(X)-2
 Fstat <- r2 * dfr / (1 - r2)
 R[above] <- ifelse(1 - pf(Fstat, 1, dfr) > p.level, " ", "*")
 diag(R) <- "- "
 noquote(R)
}

# ===

Coeff.det <- function(X, ...)
{
X.cor <- cor(X, ...) # X is matrix or data frame with values
X.det <- NULL
X.dim <- dimnames(X.cor)[[2]]
for (i in seq_along(X.dim)) X.det <- c(X.det, mean(X.cor[,i]^2))
names(X.det) <- X.dim
return(X.det)
}

## ===

Cor.vec <- function(X, ...)
{
X.cor <- cor(X, ...)
X.nam <- row.names(X.cor)
X.tri <- as.vector(lower.tri(X.cor))
X.rep.g <- rep(X.nam, length(X.nam))
X.rep.e <- rep(X.nam, each=length(X.nam))
X.pas <- paste(X.rep.g, X.rep.e, sep=" & ")
X.vec <- as.vector(X.cor)[X.tri]
names(X.vec) <- X.pas[X.tri]
return(X.vec)
}

## ===

Rostova.tbl <- function(X, GROUP, ...)
{
 r.table <- NULL
 r.names <- unique(X[[GROUP]])
 for (i in r.names) r.table <- cbind(r.table, Cor.vec(subset(X, X[[GROUP]]==i)[,-GROUP], ...))
 dimnames(r.table)[[2]] <- r.names
 r.table[is.na(r.table)] <- 0
 r.table <- t(r.table)
 return(r.table)
}

## ===

Topm <- function(X,
level=0.45, # treshold
values=0, # if > 0, ignores "level" and outputs until reaches number, if "all", outputs all values
corr=TRUE, # if FALSE, does not show magnitude
square=TRUE) # if FALSE, does not use lower triangle, some rows could be redundant
{
X.nam <- dimnames(X)[[1]]
X.col <- dimnames(X)[[2]]
X.rep.g <- rep(X.nam, length(X.col))
X.rep.e <- rep(X.col, each=length(X.nam))
X.vec <- as.vector(X)
X.df <- data.frame(Var1=X.rep.g, Var2=X.rep.e, Value=X.vec)
##
if(square) X.df <- X.df[as.vector(lower.tri(X)),]
if(values == "all") values <- nrow(X.df)
{if (!values)
 {X.df <- X.df[abs(X.df$Value) >= level, ]
 X.df <- X.df[order(-abs(X.df$Value)), ]}
else
 {X.df <- X.df[order(-abs(X.df$Value)), ]
 X.df <- X.df[1:min(values, nrow(X.df)), ]}}
X.df <- na.omit(X.df)
if (nrow(X.df) > 0) row.names(X.df) <- seq_len(nrow(X.df))
##
magnitudev <- c(0.1, 0.3, 0.5, 0.7)
magnitudes <- c("negligible", "low", "medium", "high", "very high")
if(corr) X.df$Magnitude <- magnitudes[findInterval(abs(X.df$Value), magnitudev) + 1]
##
return(X.df)
}
