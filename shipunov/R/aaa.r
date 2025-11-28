Rresults <- function() {
 if (Sys.info()[["sysname"]] == "Windows") {
  cat("Windows users: first, install all required software\n",
  "(bash, tee, UNIX date and mv, and optionally pdftk)\n",
  "somewhere in your PATH. Second, copy there 'Rresults' script from\n",
  paste0(system.file("bin", "Rresults", package="shipunov"), ".\n"))
  } else {
  cat("Linux and macOS users: to run 'Rresults' script installed in\n",
  paste0(system.file("bin", "Rresults", package="shipunov"), ",\n"),
  "link it from anywhere in your PATH.\n")
 }
}

## ===

Cdate <- function() gsub("-", "", Sys.Date())
Ctime <- function() format(Sys.time(), "%Y%m%d_%H%M%S")
Save.history <- function() { name <- paste0(Ctime(), ".r"); savehistory(name); cat("Created", paste0("\"", name, "\""), "history file\n") }

# ===

Toclip <- function(x, sep="\t", row.names=FALSE, col.names=TRUE, ...) {
if (Sys.info()[["sysname"]] == "Linux") {
 con <- pipe("xclip -selection clipboard -i", open="w")
 write.table(x, con, sep=sep, row.names=row.names, col.names=col.names, ...)
 close(con)
}
}

## ===

Xpager <- function(pager="xterm")
{
if (Sys.info()[["sysname"]] == "Linux") {
if (pager == "old") { options(old.pager) }
old.pager <- NULL
if (is.null(old.pager)) { old.pager <<- options("pager") }
if (pager == "xterm") { options(pager=function(file, header, title, delete.file) system(paste("xterm", "-fa 'Monospace' -fs 10.5 -e less", file, "&"))) }
if (pager == "mate") { options(pager=function(file, header, title, delete.file) system(paste("mate-terminal", "--sm-client-disable --disable-factory -x less", file, "&"))) }
}
}

## ===

Ls <- function(pos=1, pattern, mode="any", type="any", exclude="function", sort="name")
{
Name <- ls(pos=pos, envir=as.environment(pos), pattern=pattern)
Mode <- rep("", length(Name))
Type <- rep("", length(Name))
Vars <- rep("-", length(Name))
Obs <- rep("-", length(Name))
Size <- rep("-", length(Name))
OSize <- rep(0, length(Name))
for (i in seq_along(Name))
 {
 Mode[[i]] <- mode(get(Name[[i]]))
 Size[[i]] <- capture.output(print(object.size(get(Name[[i]])), units="auto"))
 OSize[[i]] <- object.size(get(Name[[i]]))
 if(is.list(get(Name[[i]])))
 {
 if((is.null(class(get(Name[[i]]))) | is.null(attributes(get(Name[[i]]))$class)))
 {
 Type[[i]] <- c("unknown")
 } else {
 Object.Attrib <- attributes(get(Name[[i]]))
 Type[[i]] <- Object.Attrib$class
 if(Type[[i]]=="data.frame")
 {
 Vars[[i]] <- as.character(length(Object.Attrib$names))
 Obs[[i]] <- as.character(length(Object.Attrib$row.names))
 }
 }
 }
 if(is.matrix(get(Name[[i]])))
 {
 Object.Attrib <- dim(get(Name[[i]]))
 Type[[i]] <- c("matrix")
 Vars[[i]] <- as.character(Object.Attrib[2])
 Obs[[i]] <- as.character(Object.Attrib[1])
 }
 if(is.vector(get(Name[[i]])) && (Mode[[i]]=="character" || Mode[[i]]=="numeric"))
 {
 Type[[i]] <- c("vector")
 Vars[[i]] <- c("1")
 Obs[[i]] <- as.character(length(get(Name[[i]])))
 }
 if(is.factor(get(Name[[i]])))
 {
 Type[[i]] <- c("factor")
 Vars[[i]] <- c("1")
 Obs[[i]] <- as.character(length(get(Name[[i]])))
 }
 if(is.function(get(Name[[i]]))) Type[[i]] <- c("function")
 }
res <- data.frame(Name, Mode, Type, Obs, Vars, Size)
if(sort == "size") res <- res[rev(order(OSize)), ]
if(mode != "any") res <- res[res[["Mode"]] == mode, ]
if(type != "any") res <- res[res[["Type"]] == type, ]
if(exclude != "none") res <- res[res[["Type"]] != exclude, ]
row.names(res) <- NULL
return(res)
}

## ===

Table2df <- function(table)
{
 F <- array(table, dim(table), dimnames(table))
 as.data.frame(F)
}

# ===

Read.tri.nts <- function(file, ...)
{
 elements <- scan(file, ...)
 n <- (sqrt(1 + 8 * length(elements)) - 1)/2
 U <- matrix(0, n, n)
 U[outer(1:n, 1:n, '<=')] <- elements
 return(t(U))
}

# ===

Tobin <- function(var, convert.names=TRUE)
{
 u.var <- sort(unique(var))
 if (convert.names)
 {
 mat.var <- matrix((rep(var, length(u.var)) == rep(u.var, each=length(var)))*1, ncol=length(u.var))
 colnames(mat.var) <- paste(deparse(substitute(var)), u.var, sep=".")
 } else {
 mat.var <- sapply(levels(factor(var)), function(.x) {d <- rep(0, length(var)); d[var==.x] <- 1; d})
 }
 return(mat.var)
}

## ===

Aggregate1 <- function(df, by, ...) {
if (!is.atomic(by)) stop("'by' should be atomic")
tmp.ag <- aggregate(df, list(by), ...)
row.names(tmp.ag) <- tmp.ag[, 1]
tmp.ag[, 1] <- NULL
tmp.ag
}

## ===

Normality <- function(x, p=.05)
{
 if (length(x) < 25) warning("Normality tests do not work well on small samples (< 25)")
 ifelse(shapiro.test(x)$p.value >= p, "NORMAL", "NOT NORMAL")
}

## ===

CVs <- function(sample, na.rm=TRUE)
{
if(na.rm) sample <- na.omit(sample)
cv <- 100 * sd(sample)/mean(sample)
cvcorrected <- (1 + 1/(4*length(sample))) * cv
iqrv <- 100 * IQR(sample)/(IQR(sample) + median(sample))
madv <- 100 * mad(sample)/(mad(sample) + median(sample))
##
values <- c(cv, cvcorrected, iqrv, madv)
names(values) <- c("CV, %", "CV.corr, %", "IQR.V, %", "MAD.V, %")
values
}

## ===

Mag <- function(x, squared=TRUE)
{
magnitudev <- c(0.1, 0.3, 0.5, 0.7)
magnitudes <- c("negligible", "low", "medium", "high", "very high")
magnitudes[findInterval(ifelse(squared, sqrt(abs(x)), abs(x)), magnitudev) + 1]
}

## ===

pairwise.Eff <- function(vec, fac, eff="K", dec=2, mad=FALSE) {
lst <- split(vec, fac)
if (eff == "K") {
 ee <- sapply(lst, function(.x) sapply(lst, function(.y) K(.x, .y, mad=mad)))
 mm <- sapply(lst, function(.x) sapply(lst, function(.y) summary(K(.x, .y))[[2]]))
 }
if (eff == "cohen.d") {
 ee <- sapply(lst, function(.x) sapply(lst, function(.y) effsize::cohen.d(.x, .y)$estimate))
 mm <- sapply(lst, function(.x) sapply(lst, function(.y) effsize::cohen.d(.x, .y)$magnitude))
 }
if (eff == "cliff.delta") {
 ee <- sapply(lst, function(.x) sapply(lst, function(.y) effsize::cohen.d(.x, .y)$estimate))
 mm <- sapply(lst, function(.x) sapply(lst, function(.y) effsize::cohen.d(.x, .y)$magnitude))
 }
rr <- paste(round(ee, dec), " (", mm, ")", sep="")
attributes(rr) <- attributes(mm)
rr[upper.tri(rr, diag=TRUE)] <- ""
noquote(rr)
}

## ===

pairwise.Table2.test <- function(tbl, names=rownames(tbl), p.adjust.method="BH", exact=FALSE, ...)
{
if(length(dim(tbl)) > 2) stop("Only tables with 2 dimensions accepted")
compare.levels <- function(i, j) chisq.test(matrix(c(tbl[i,], tbl[j,]), nrow=2, byrow=TRUE), ...)$p.value
if (exact) compare.levels <- function(i, j) fisher.test(matrix(c(tbl[i,], tbl[j,]), nrow=2, byrow=TRUE), ...)$p.value
PVAL <- pairwise.table(compare.levels, level.names=names, p.adjust.method)
ifelse(exact, method <- "Fisher's Exact Test", method <- "Pearson's Chi-squared test")
ans <- list(method=method, data.name=deparse(substitute(tbl)), p.value=PVAL, p.adjust.method=p.adjust.method)
class(ans) <- "pairwise.htest"
return(ans)
}

## ===

Fibonacci <- function(x)
{
 if (x < 0 | x%%1 != 0) stop("Only whole non-negative numbers expected")
 num <- numeric(x)
 num[1:2] <- 1
 if (x > 2) for(i in 3:x) num[i] <- num[i-1] + num[i-2]
 if (x == 0) 0 else num[x]
}

Phyllotaxis <- function(n, angle=FALSE)
{
 numerator <- Fibonacci(n)
 denominator <- Fibonacci(n+2)
 if (!angle) paste(numerator, denominator, sep="/") else 180*numerator/denominator
}

## ===

BootA <- function(dat, FUN=function(.x) ape::nj(dist(.x)), iter=1000, mc.cores=1, tresh=50, cons=TRUE, prop=0.5) {
tree <- FUN(dat)
boots <- ape::boot.phylo(tree, dat, FUN, B=iter, trees=TRUE, mc.cores=mc.cores)
pclad <- round((boots$BP/iter)*100)
tree$node.label <- ifelse(pclad >= tresh, pclad, "")
result <- list(boot.tree=NA, cons.tree=NA)
result$boot.tree <- tree
if(cons) result$cons.tree <- ape::consensus(boots$trees, p=prop)
invisible(result)
}

## ===

Hclust.match <- function(hc1, hc2, scale=FALSE) {
obj <- hc1$labels
if (!all(obj == hc2$labels)) stop("Labels are not identical")
nobj <- length(obj)
res <- matrix(0, ncol=nobj, nrow=nobj, dimnames=list(obj, obj))
for (n in 2:(nobj - 1)) {
 hc1.c <- cutree(hc1, n)
 hc2.c <- cutree(hc2, n)
 hc1.o <- outer(hc1.c, hc1.c, "==")
 hc2.o <- outer(hc2.c, hc2.c, "==")
 res <- res + hc1.o + hc2.o
 }
if (scale) res <- res / (length(2:(nobj - 1)) * 2)
res
}

## ===

Missing.map <- function(df)
{
nas <- as.data.frame(lapply(df, function(.x) as.numeric(is.na(.x))))
total <- colSums(nas)
percent <- round(100*total/nrow(nas), 1)
## alternative: findInterval(seq_len(nrow(df)), pretty(seq_len(nrow(df)), 50))
groups <- rep(1:50, each=ceiling(nrow(df)/50))[seq_len(nrow(df))]
nas.agg <- t(round(aggregate(nas, list(groups), mean),1)[,-1])
nas.agg <- ifelse(nas.agg == 1, "!", nas.agg)
nas.agg <- ifelse(nas.agg == "0", "_", nas.agg)
nas.agg <- ifelse(nas.agg != "_" & nas.agg != "!", ":", nas.agg)
nas.agg.str <- apply(nas.agg, 1, function(.x) paste(.x, collapse=""))
##
M <- data.frame(var=names(df), missing.map=nas.agg.str, total=total, percent=percent)
row.names(M) <- NULL
cat(paste("\n", "Legend:", "'_' no,", "':' some,", "'!' all", "\n", collapse=" "))
return(M)
}

## ===

Histr <- function(x, overlay="normal", rug=FALSE, col="gray80", ...) {
stopifnot(is.numeric(x) & is.vector(x))
stopifnot(overlay == "normal" | overlay == "density")
if (any(is.na(x)))
 {
 warning(paste(sum(is.na(x)), "missing values"))
 x <- na.omit(x)
 }
h <- hist(x, plot=FALSE, ...)
if (overlay=="normal")
 {
 xfit <- seq(min(x), max(x), length=40)
 yfit <- dnorm(xfit, mean=mean(x), sd=sd(x))
 yfit <- yfit * diff(h$mids[1:2]) * length(x)
 hist(x, ylim=c(0, max(yfit, h$counts)), col=col, ...)
 lines(xfit, yfit, col="blue", lwd=2)
 } else {
 if (overlay=="density")
 {
 hist(x, probability=TRUE, ylim=c(0, max(density(x)$y, h$density)), col=col, ...)
 lines(density(x), col = "red", lwd=2)
 }
 }
if (rug) rug(x)
}

## ===

Cladd <- function(model, data, level=.95, lty=2, ab.lty=0, col="black", ab.col="black")
{
if (!is(model, "lm")) stop("Not an object of class 'lm'")
var <- names(model$model)[2]
sel <- data[, var]
new.var <- seq(min(sel, na.rm=TRUE), max(sel, na.rm=TRUE), length.out=length(sel))
new <- data.frame(new.var)
names(new)[1] <- var
pp <- predict(model, interval="confidence", level=level, newdata=new)
matlines(new.var, pp, lty=c(ab.lty,lty,lty), col=c(ab.col,col,col))
}

## ===

Boxplots <- function(vars, groups, boxcols=Pastels, legpos="topleft", srt=45, adj=1, slty=3, yticks=FALSE, ymarks=FALSE, ...)
{
 Pastels <- c("white", "lightblue", "mistyrose", "lightcyan", "lavender", "cornsilk")
 if (!is.factor(groups)) stop("Grouping variable must be a factor")
 svars <- scale(vars)
 nvars <- ncol(vars)
 groups <- droplevels(groups)
 ngroups <- length(levels(groups))
 oldpar <- par(c(xaxt="n", yaxt="n"))
 xlim <- c(0, nvars+1)
 ylim <- c(min(svars, na.rm=TRUE), max(svars, na.rm=TRUE))
 if (ngroups == 1)
 {
 boxplot(svars, boxwex=.5, col=boxcols[1], ...)
 shift2 <- 0
 }
 if (ngroups == 2)
 {
 boxwex <- .3
 shift2 <- seq(-0.5, 0.5, along=1:nvars)
 boxplot(svars[groups==levels(groups)[1],], at=1:nvars-.2+shift2, boxwex=boxwex, col=boxcols[1], ylim=ylim, xlim=xlim, ...)
 boxplot(svars[groups==levels(groups)[2],], at=1:nvars+.2+shift2, boxwex=boxwex, col=boxcols[2], add=TRUE)
 shift3 <- .5 + (shift2[1:(nvars-1)] + shift2[2:nvars])/2
 segments(1:(nvars-1)+shift3, ylim[1]-.2, 1:(nvars-1)+shift3, ylim[2]+.2, lty=slty)
 }
 if (ngroups > 2)
 {
 span <- .64
 begin <- -(span/2)
 step <- span/(ngroups-1)
 shift <- begin + step*((1:ngroups)-1)
 boxwex <- .7*step
 shift2 <- seq(-0.5, 0.5, along=1:nvars)
 boxplot(svars[groups==levels(groups)[1],], at=1:nvars+shift[1]+shift2, boxwex=boxwex, col=boxcols[1], ylim=ylim, xlim=xlim, ...)
 for (i in 2:ngroups) boxplot(svars[groups==levels(groups)[i],], at=1:nvars+shift[i]+shift2, boxwex=boxwex, col=boxcols[i], add=TRUE)
 shift3 <- .5 + (shift2[1:(nvars-1)] + shift2[2:nvars])/2
 segments(1:(nvars-1)+shift3, ylim[1]-.2, 1:(nvars-1)+shift3, ylim[2]+.2, lty=slty)
 }
 par(oldpar)
 axis(side=1, at=1:nvars+shift2, labels=FALSE)
 if(yticks) axis(side=2, labels=ymarks)
 text(1:nvars+shift2, par("usr")[3]-.28, srt=srt, adj=adj, labels=colnames(vars), xpd=TRUE, cex=.9)
 legend(legpos, legend=levels(groups), fill=boxcols[1:ngroups], bg="white")
 invisible()
}

## ===

Ex.lty <- Ex.lines <- function(custom="431313")
{
oldpar <- par(mar=c(0,0,0,0))
plot(1, ylim=c(0,7), xlim=c(0,.7), axes=FALSE, type="n", xlab="", ylab="")
ltypes <- c(0:6)
print(ltypes)
lnames <- c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
labels <- paste(ltypes, lnames, sep="  ")
labels <- c(labels, paste("custom:", custom))
for(i in 0:7) lines(c(.3,.7), c(i,i), lty=ifelse(i==0, custom, ltypes[8-i]), lwd=3)
text(rep(.1,7), 7:0, labels=labels, pos=4)
par(oldpar)
}

## ===

Ex.pch <- Ex.points <- function(extras=c("*",".","+","a"), cex=2, col="black", bg="gray", coltext="black", cextext=1.2, main="")
{
nex <- length(extras)
np <- 26 + nex
ipch <- 0:(np-1)
k <- floor(sqrt(np))
dd <- c(-1,1)/2
rx <- dd + range(ix <- ipch %/% k)
ry <- dd + range(iy <- 3 + (k-1)- ipch %% k)
pch <- as.list(ipch) # list with integers & strings
if(nex > 0) pch[26+ 1:nex] <- as.list(extras)
oldpar <- par(mar=c(0,0,0,0))
plot(rx, ry, type="n", axes=FALSE, xlab="", ylab="", main=main)
abline(v=ix, h=iy, col="lightgray", lty="dotted")
for(i in 1:np)
 {
 pc <- pch[[i]]
 ## 'col' symbols with a 'bg'-colored interior (where available):
 points(ix[i], iy[i], pch=pc, col=col, bg=bg, cex=cex)
 if(cextext > 0) text(ix[i]-.3, iy[i], pc, col=coltext, cex=cextext)
 }
par(oldpar)
}

# ===

Ex.font <- Ex.fonts <- function()
{
oldpar <- par(mar=c(0,0,0,0))
plot(1, ylim=c(0,5), xlim=c(0,.7), axes=FALSE, type="n", xlab="", ylab="")
types <- c("plain text", "bold face", "italic", "bold italic")
nums <- c(1:4)
text(rep(.35,6), 5-(1:4), labels=types[1:4], font=nums[1:4], cex=3)
text(rep(.1,6), 5-(1:4), labels=nums[1:4], pos=4, cex=3)
par(oldpar)
}

# ===

Ex.plots <- Ex.types <- function()
{
oldpar <- par(mfrow=c(3,3))
types <- c("p", "l", "b", "c", "o", "h", "s", "S", "n")
labels <- paste(types, c("points", "lines", "both", "lines of both", "overplotted", "hist", "steps", "other steps", "no plotting"), sep="  ")
for (n in 1:9) plot(1:3, main=labels[n], xlab="", ylab="", type=types[n])
par(oldpar)
}

# ===

Ex.margins <- function()
{
oldpar <- par(oma=rep(3, 4), bg="gray80")
plot(c(0, 1), c(0, 1), type="n", ann=FALSE, axes=FALSE)
box("outer", col="gray")
par(xpd=TRUE)
rect(-1, -1, 2, 2, col="gray90")
box("figure")
par(xpd=FALSE)
rect(-1, -1, 2, 2, col="gray80")
box("plot", lty="dashed")
text(.5, .5, "Plot Region")
mtext("Figure Region", side=3, line=2)
for (i in 1:4) mtext(paste("Outer margin", i), side=i, line=1, outer=TRUE)
par(oldpar)
}

# ===

Ex.boxplot <- function(...)
{
set.seed(1); bxp <- boxplot(rnorm(60), axes=FALSE, ...)
box()
st <- bxp$stats
out <- bxp$out
text(1.15, st[1], "Minimum*", pos=4, offset=-1)
text(1.05, mean(st[1:2]), "Lower Tail", srt=90)
text(1.25, st[2], "Lower Quartile*", pos=4, offset=-1)
text(1.35, mean(st[2:4]), "IQR*\n(InterQuartile\nRange)", srt=90)
text(.7, st[3], "Median\n(3rd Quartile)")
text(1.25, st[4], "Upper Quartile*", pos=4, offset=-1)
text(1.05, mean(st[4:5]), "Upper Tail", srt=90)
text(1.15, st[5], "Maximum*", pos=4, offset=-1)
text(1.15, out, "Outlier", pos=4, offset=-1)
legend("bottomright", pch="*", legend="adjusted")
}

## ===

Saynodynamite <- function()
{
s.means <- with(datasets::sleep, tapply(extra, group, mean, na.rm=TRUE))
s.sds <- with(datasets::sleep, tapply(extra, group, sd, na.rm=TRUE))
s.sds.adj <- 1.96*s.sds/(sqrt(table(datasets::sleep$group)))
barx <- barplot(s.means, ylim=c(0, max(s.means)+max(s.sds.adj)), col=grey(.9))
arrows(barx, s.means+s.sds.adj, barx, s.means, angle=90, code=1, length=.1)
lines(c(0.2,2.4), c(3.4,0.05), lwd=12, col="red", lend="square")
lines(c(0.2,2.4), c(0.05,3.4), lwd=12, col="red", lend="square")
text(1.3, 3.4, "Say \"no\" to dynamite plots!", col="red", cex=2, font=2)
}

## ===

Gridmoon <- function(Skyres=50, Nightsky=TRUE, Daysky="deepskyblue", Moon=TRUE, Moonsize=0.05, Stars=TRUE, Hillcol="black", Text=c("Once upon a time..."), Textsize=22, Textpos=c(.15, .51), Textcol="white")
{
grid::pushViewport(grid::viewport(xscale=c(0, 1), yscale=c(0.5, 1), clip=TRUE))
##
sky <- function(res=Skyres)
{
for (i in 1:res) grid::grid.rect(y=1 - (i-1)/res, just="top", gp=grid::gpar(col=grey(0.5*i/res), fill=grey(0.5*i/res)))
}
##
if (Nightsky) sky() else grid::grid.rect(gp=grid::gpar(col=Daysky, fill=Daysky))
##
moon <- function(x, y, size)
{
 angle <- seq(-90, 90, length=50)/180*pi
 x1 <- x + size*cos(angle)
 y1 <- y + size*sin(angle)
 mod <- 0.8
 x2 <- x + mod*(x1 - x)
 grid::grid.polygon(c(x1, rev(x2)), c(y1, rev(y1)), default.unit="native", gp=grid::gpar(col=NULL, fill="white"))
}
if (Moon) moon(.1, .9, Moonsize)
##
star <- function(x, y, size)
{
 x1 <- c(x, x + size*.1, x + size*.5, x + size*.1, x, x - size*.1, x - size*.5, x - size*.1) + .05
 y1 <- c(y - size, y - size*.1, y, y + size*.1, y + size*.7, y + size*.1, y, y - size*.1) + .05
 grid::grid.polygon(x1, y1, default.unit="native", gp=grid::gpar(col=NULL, fill="white"))
}
##
if (Stars)
{
star(.5, .7, .02)
star(.8, .9, .02)
star(.72, .74, .02)
star(.62, .88, .02)
grid::grid.circle(runif(20, .2, 1), runif(20, .6, 1), r=.002, default.unit="native", gp=grid::gpar(col=NULL, fill="white"))
}
##
hill <- function(height=0.1, col=Hillcol)
{
 n <- 100
 x <- seq(0, 1, length=n)
 y1 <- sin(runif(1) + x*2*pi)
 y2 <- sin(runif(1) + x*4*pi)
 y3 <- sin(runif(1) + x*8*pi)
 y <- 0.6 + height*((y1 + y2 + y3)/3)
 grid::grid.polygon(c(x, rev(x)), c(y, rep(0, n)), default.unit="native", gp=grid::gpar(col=NULL, fill=col))
}
##
hill()
##
grid::grid.text(Text, Textpos[1], Textpos[2], just="bottom", default.unit="native", gp=grid::gpar(col=Textcol, fontface="italic", fontsize=Textsize))
##
grid::popViewport()
grid::grid.rect()
}

## ===

Ell <- function(x, y, width, height=width, theta=2*pi, npoints=100, plot=TRUE, ...)
{
a <- width/2
b <- height/2
xcoord <- seq(-a, a, length=npoints)
ycoord.neg <- sqrt(b^2 * (1-(xcoord)^2 / a^2))
ycoord.pos <- -sqrt(b^2 * (1-(xcoord)^2 / a^2))
xx <- c(xcoord, xcoord[npoints:1])
yy <- c(ycoord.neg, ycoord.pos)
x.theta <- xx*cos(2*pi-theta) + yy*sin(2*pi-theta) + x
y.theta <- yy*cos(2*pi-theta) - xx*sin(2*pi-theta) + y
if(plot)
 invisible(polygon(x.theta, y.theta, ...))
else
 invisible(list(coords=data.frame(x=x.theta, y=y.theta), center=c(x, y), theta=theta))
}

## ===

Read.fasta <- function(file) {
 fasta <- readLines(file)
 ind <- grep(">", fasta)
 s <- data.frame(ind=ind, from=ind+1, to=c((ind-1)[-1], length(fasta)))
 seqs <- rep(NA, length(ind))
 for(i in seq_along(ind))
 {
 seqs[i] <- paste(fasta[s$from[i]:s$to[i]], collapse="")
 }
 data.frame(name=gsub(">", "", fasta[ind]), sequence=seqs, stringsAsFactors=FALSE)
}

## ===

Write.fasta <- function(df, file) {
 if (ncol(df) > 2) warning("Only two first columns used!")
 write(file=file, paste(">", df[, 1], "\n", df[, 2], "\n", collapse="", sep=""))
}

## ===

Gap.code <- function(seqs)
{
bb <- gsub("[^-N]", "_", seqs)
bb <- do.call(rbind, strsplit(bb, split=""))
## remove consecutively duplicated columns
aa <- rle(apply(bb, 2, function(.x) paste(.x, collapse="")))$values
aa <- do.call(rbind, strsplit(aa, split=""))
bb <- apply(aa, 2, function(.x) paste(.x, collapse=""))
## (preallocation does not improve result)
gc <- matrix(nrow=length(bb), ncol=0)
nn <- nchar(bb[1])
for (pos in 1:(nn-2))
{
 cat(".")
 for (gap in (nn-pos-1):1)
 {
 r1 <- paste("^", "[N_-]", "{", pos, "}", "-", "{", gap, "}", "[N_-]", sep="")
 a1 <- ifelse(grepl(r1, bb, perl=TRUE), "-", "C")
 r2 <- paste("^", "[N_-]", "{", pos-1, "}", "_", "-", "{", gap, "}", "_", sep="")
 a2 <- ifelse(grepl(r2, bb, perl=TRUE), "A", "?")
 a1[a2 == "A"] <- "A"
 if (grepl("A", paste(a1, collapse=""))) gc <- cbind(gc, a1, deparse.level=0)
 }
}
cat("\n")
gc
return(gc)
}

# ===

Infill <- function(x, n=10) {
x <- as.matrix(x)
x <- x[, colSums(x)!= 0]
mat <- numeric(0)
for (j in 1:n) {
 ini <- rep(0, nrow(x))
 sam <- sample(seq_len(nrow(x)), nrow(x))
 dat <- cbind(seq_len(nrow(x)), x[sam, ])
 for (i in 2:ncol(dat)){
 nums <- dat[dat[, i] > 0, 1]
 ini[nums[1]] <- ini[nums[1]] + 1}
 ini <- cumsum(ini)
 mat <- cbind(mat, ini)}
dimnames(mat)[[2]] <- 1:n
mat <- apply(mat, 1, mean)
class(mat) <- "Infill"
attr(mat, "nspecies") <- ncol(x)
attr(mat, "nperm") <- n
mat}
#
plot.Infill <- function(x, ...) {
sp <- attr(x, "nspecies")
n <- attr(x, "nperm")
plot(unclass(x), type="l", ylab="species", xlab="sites", sub=paste(n, "permutations"),  axes=FALSE, ...)
abline(h=.5*sp, lty=2, col="green")
abline(h=.75*sp, lty=2, col="blue")
abline(h=.9*sp, lty=2, col="red")
axis(1, seq(0, length(x), by=1))
axis(2, seq(0, max(x)))
box()}
##
summary.Infill <- function(object, ...) {
sp <- attr(object, "nspecies")
n <- attr(object, "nperm")
cat("One site infill:", (object[1]/sp)*100, "%", "\n")
cat("50% infill:", which(abs(object - .5*sp) == min(abs(object - .5*sp)))[1], "sites", "\n")
cat("75% infill:", which(abs(object - .75*sp) == min(abs(object - .75*sp)))[1], "sites", "\n")
cat("90% infill:", which(abs(object - .9*sp) == min(abs(object - .9*sp)))[1], "sites", "\n")
cat(sp, "species", "/", length(object), "sites", "/", n, "permutations", "\n")}

## ===

Coml <- function(df1, df2)
{
df1.sp <- (rowSums(df1) > 0) * 1
df2.sp <- (rowSums(df2) > 0) * 1
per <- sum((df1.sp > 0) * (df2.sp > 0)) / sum((((df1.sp + df2.sp) > 0) * 1))
p1 <- apply(df1 > 0, 1, function(x) round(sum(x) / ncol(df1) * 100, 2))
p2 <- apply(df2 > 0, 1, function(x) round(sum(x) / ncol(df2) * 100, 2))
ind.1 <- rev(sort(p1 - p2))
ind.2 <- rev(sort(p2 - p1))
C.list <- list(per=per, ind.1=ind.1, ind.2=ind.2)
class(C.list) <- "Coml"
invisible(C.list)
}
##
summary.Coml <- function(object, ..., n=10)
{
cat("Mean difference between two groups:", object$per, "\n")
cat("======================\n")
cat("Group I top", deparse(substitute(n)), "indicators:\n")
print(head(object$ind.1, n=n))
cat("======================\n")
cat("Group II top", deparse(substitute(n)), "indicators:\n")
print(head(object$ind.2, n=n))
}
