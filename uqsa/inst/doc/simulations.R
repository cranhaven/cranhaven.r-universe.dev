## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(uqsa)
library(parallel)
library(errors)

## ----shared-library-----------------------------------------------------------
m <- model_from_tsv(uqsa_example("AKAP79"))
o <- as_ode(m)
c_path(o) <- write_c_code(generate_code(o))
so_path(o) <- shlib(o)
print(o)
if (!file.exists(so_path(o))) stop('creation of shared library failed')

## ----experiments--------------------------------------------------------------
ex <- experiments(m,o)

## ----sim----------------------------------------------------------------------
stopifnot(all(m$Parameter$scale=="log10")) # just to make sure
opt <- options(mc.cores = 2)               # required by CRAN to be 2, set this to a bigger value for yourself
s <- simulator.c(ex,o,parMap=log10ParMap)

p0 <- values(m$Parameter) # default parameters, in log10-space
rprior <- rNormalPrior(p0,rep(0.1,length(p0))) # a small neighborhood
y <- s(t(rprior(300)))
status <- unlist(lapply(y,\(E) as.logical(E$status))) # non-zero means an error occurred
print(status)
if (any(status)) stop("simulation failed.")

## ----plotting, out.width="100%", res=200, fig.width=12, fig.height=10---------
e.g. <- 18
plot(
	as.errors(ex[[e.g.]]$outputTimes), # exact
	ex[[e.g.]]$data,                   # uncertain
	xlab="time",
	ylab="AKAR4p",
	main=sprintf("AKAP79 model, expr. %i: %s",e.g.,names(ex)[e.g.]),
	ylim=c(90,200)
)
matplot(
	ex[[e.g.]]$outputTimes,
	y[[e.g.]]$func['AKAR4pOUT',,],
	lwd=2,                                   # fat
	col=rgb(200,0,255,12,maxColorValue=255), # low opacity
	lty=1,                                   # solid
	type="l",                                # line
	add=TRUE
)

## ----plot-generic, out.width="100%", res=300, fig.width=12, fig.height=18-----
print(class(y))
oldpar <- par()      # remember graphics values
on.exit(par(oldpar)) # restore original values
par(mfrow=c(3,2))
plot(ex[seq(6)],y[seq(6)],ylim=c(90,140))

## ----label="print-y"----------------------------------------------------------
print(y)

