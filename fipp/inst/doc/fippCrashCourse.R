## ------------------------------------------------------------------------
library("fipp")

## ----fig.height = 5, fig.width = 7---------------------------------------
pmfDPM <- nClusters(Kplus = 1:30, type = "DPM", N = 100, alpha = 1/3)
barplot(pmfDPM(),
        main = expression("DPM (" * alpha == 1/3 * ") with N = 100"),
        xlab = expression(K["+"]), ylab = "probability")

## ----fig.height = 5, fig.width = 7---------------------------------------
pmfstatic <- nClusters(Kplus = 1:30, type = "static", N = 100, gamma = 1, maxK = 30)

# First, specify a function for the discrete uniform distribution U(min, max) on K
ddunif <- function(x, min, max, log = FALSE) {
  n <- max - min + 1 
  val <- ifelse(x < min | max < x, 0, 1/n)
  if (log) {
    val <- log(val)
  }
  return(val)
}

# Now, evaluate the closure with U(0, 29)
Kpstatic <- pmfstatic(priorK = ddunif, priorKparams = list(min = 0, max = 29))
# Plot the pmf of K+ side by side with that of K
barplot(rbind(Kpstatic, 1/30), beside = TRUE,
        main = expression("static MFM (" * gamma == 1 * ") with"~
	  K-1 %~%~"U(0, 29) and N = 100"),
        xlab = expression(K["+"]/K), ylab = "probability")
legend("topright", c(expression(K["+"]), "K"), fill = gray.colors(2))

## ----fig.height = 7, fig.width = 7---------------------------------------
pmfstatic2 <- nClusters(Kplus = 1:30, type = "static", N = 100, gamma = 1, maxK = 150)
oldpar <- par(mfrow = c(2, 1))

# with K-1 ~ Pois(3)
KpstaticPois <- pmfstatic2(priorK = dpois, priorKparams = list(lambda = 3))
Pois3 <- sapply(1:30, function(k) dpois(k-1, lambda = 3))

barplot(rbind(KpstaticPois, Pois3), beside = TRUE,
        main = expression("static MFM (" * gamma == 1 * ") with"~
	  K-1 %~%~"Pois(3) and N = 100"),
        xlab = expression(K["+"]/K), ylab = "probability")
legend("topright", c(expression(K["+"]), "K"), fill = gray.colors(2))

# now with K-1 ~ Geom(0.3)
KpstaticGeom <- pmfstatic2(priorK = dgeom, priorKparams = list(prob = 0.3))
Geom3 <- sapply(1:30, function(k) dgeom(k-1, prob = 0.3))

barplot(rbind(KpstaticGeom, Geom3), beside = TRUE,
        main = expression("static MFM (" * gamma == 1 * ") with"~
	  K-1 %~%~"Geom(.3) and N = 100"),
        xlab = expression(K["+"]/K), ylab = "probability")
legend("topright", c(expression(K["+"]), "K"), fill = gray.colors(2))
par(oldpar)

## ----fig.height = 7, fig.width = 7---------------------------------------
pmfdynamic <- nClusters(Kplus = 1:30, type = "dynamic", N = 100, alpha = 1, maxK = 150)
oldpar <- par(mfrow = c(2, 1))

# with K-1 ~ Pois(3)
KpdynamicPois <- pmfdynamic(priorK = dpois, priorKparams = list(lambda = 3))

barplot(rbind(KpdynamicPois, Pois3), beside = TRUE,
        main = expression("dynamic MFM (" * alpha == 1 * ") with"~
	  K-1 %~%~"Pois(3) and N = 100"),
        xlab = expression(K["+"]/K), ylab = "probability")
legend("topright", c(expression(K["+"]), "K"), fill = gray.colors(2))

# now with K-1 ~ Geom(0.3)
KpdynamicGeom <- pmfdynamic(priorK = dgeom, priorKparams = list(prob = 0.3))

barplot(rbind(KpdynamicGeom, Geom3), beside = TRUE,
        main = expression("dynamic MFM (" * alpha == 1 * ") with"~
	  K-1 %~%~"Geom(.3) and N = 100"),
        xlab = expression(K["+"]/K), ylab = "probability")
legend("topright", c(expression(K["+"]), "K"), fill = gray.colors(2))
par(oldpar)

## ------------------------------------------------------------------------
N <- 100
Kp <- 4
entrDPM <- fipp(function(n) log(n/N) + log(log(N) - log(n)),
                Kplus = Kp, N = N, type = "DPM", alpha = 1/3, maxK = 150)
relentr <- entrDPM()
cat("Statistics computed over the prior partitions: Relative entropy\n",
    "Model: DPM (alpha = 1/3)\n",
    "conditional on: K+ = 4\n mean =", relentr[[1]]/log(Kp),
    "\n sd =", sqrt(relentr[[2]]/(log(Kp)^2)))

## ------------------------------------------------------------------------
entrstatic <- fipp(function(n) log(n/N) + log(log(N) - log(n)),
                   Kplus = Kp, N = N, type = "static", gamma = 1, maxK = 150)

# with K-1 ~ Pois(3)
relentrPois <- entrstatic(priorK = dpois, priorKparams = list(lambda = 3))

# with K-1 ~ Geom(0.3)
relentrGeom <- entrstatic(priorK = dgeom, priorKparams = list(prob = 0.3))

cat("Statistics computed over the prior partitions: Relative entropy\n",
    "Model: static MFM (gamma = 1)\n",
    "conditional on: K+ = 4\n",
    "case 1 with K-1 ~ dpois(3): mean =", relentrPois[[1]]/log(Kp),
    " sd =", sqrt(relentrPois[[2]]/(log(Kp)^2)),"\n",
    "case 2 with K-1 ~ dgeom(.3): mean =", relentrGeom[[1]]/log(Kp),
    " sd =", sqrt(relentrGeom[[2]]/(log(Kp)^2)))

## ------------------------------------------------------------------------
entrdynamic <- fipp(function(n) log(n/N) + log(log(N) - log(n)),
                    Kplus = Kp, N = N, type = "dynamic", alpha = 1, maxK = 150)
# with K-1 ~ Pois(3)
relentrPois <- entrdynamic(priorK = dpois, priorKparams = list(lambda = 3))

# with K-1 ~ Geom(0.3)
relentrGeom <- entrdynamic(priorK = dgeom, priorKparams = list(prob = 0.3))

cat("Statistics computed over the prior partitions: Relative entropy\n",
    "Model: dynamic MFM (alpha = 1)\n",
    "conditional on: K+ = 4\n",
    "case 1 with K-1 ~ dpois(3): mean =", relentrPois[[1]]/log(Kp),
    " sd =", sqrt(relentrPois[[2]]/(log(Kp)^2)),"\n",
    "case 2 with K-1 ~ dgeom(.3): mean =", relentrGeom[[1]]/log(Kp),
    " sd =", sqrt(relentrGeom[[2]]/(log(Kp)^2)))

## ----echo = FALSE--------------------------------------------------------
funcDPM <- fipp(function(n) log(n < 0.1*N),
	        Kplus = Kp, N = N, type = "DPM", alpha = 1/3, maxK = 150)
func <- funcDPM()
cat("Statistics computed over the prior partitions:\n",
    "Number of clusters with less than 10% of the obs\n",
    "Model: DPM (alpha = 1/3)\n",
    "conditional on: K+ = 4\n mean =", func[[1]],
    "\n sd =", sqrt(func[[2]]))

# static
funcstatic <- fipp(function(n) log(n < 0.1*N),
                   Kplus = Kp, N = N, type = "static", gamma = 1, maxK = 150)
# with K-1 ~ Pois(3)
funcPois <- funcstatic(priorK = dpois, priorKparams = list(lambda = 3))

# with K-1 ~ Geom(0.3)
funcGeom <- funcstatic(priorK = dgeom, priorKparams = list(prob = 0.3))

cat("Statistics computed over the prior partitions:\n",
    "Number of clusters with less than 10% of the obs\n",
    "Model: static MFM (gamma = 1)\n",
    "conditional on: K+ = 4\n",
    "case 1 with K-1 ~ dpois(3): mean =", funcPois[[1]],
    " sd =", sqrt(funcPois[[2]]),"\n",
    "case 2 with K-1 ~ dgeom(.3): mean =", funcGeom[[1]],
    " sd =", sqrt(funcGeom[[2]]))
# dynamic
funcdynamic <- fipp(function(n) log(n < 0.1*N),
                    Kplus = Kp, N = N, type = "dynamic", alpha = 1, maxK = 150)

# with K-1 ~ Pois(3)
funcPois <- funcdynamic(priorK = dpois, priorKparams = list(lambda = 3))

# with K-1 ~ Geom(0.3)
funcGeom <- funcdynamic(priorK = dgeom, priorKparams = list(prob = 0.3))

cat("Statistics computed over the prior partitions:\n",
    "Number of clusters with less than 10% of the obs\n",
    "Model: dynamic MFM (alpha = 1)\n",
    "conditional on: K+ = 4\n",
    "case 1 with K-1 ~ dpois(3): mean =", funcPois[[1]],
    " sd =", sqrt(funcPois[[2]]),"\n",
    "case 2 with K-1 ~ dgeom(.3): mean =", funcGeom[[1]],
    " sd =", sqrt(funcGeom[[2]]))

