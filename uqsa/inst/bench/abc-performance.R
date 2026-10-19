#!/usr/bin/env Rscript

library(uqsa)
library(errors)
library(bench)
library(parallel)
options(mc.cores=detectCores())

f <- uqsa_example("AKAR4")
m <- model_from_tsv(f)
o <- as_ode(m)
c_path(o) <- write_c_code(generate_code(o))
so_path(o) <- shlib(o)
ex <- experiments(m,o)
time_out_seconds <- 2.5
integrator_step_limit <- 10000

s <- simulator.c(
	ex,             # experiments
	o,              # the model
	log10ParMap,    # reverse map, to get back from Markov chain space to model
	omit=2,         # omit Fisher-Information and Gradient calculations
	num.steps=integrator_step_limit,
	time.out=time_out_seconds
)
Obj <- makeObjective(ex,s)

p0 <- log10(values(m$Parameter))
dprior <- dUniformPrior(p0-3,p0+3)
rprior <- rUniformPrior(p0-3,p0+3)
batchSize <- 128    # how many points are simulated in one call to the simulator
P <- p0 + matrix(rnorm(length(p0)*batchSize),length(p0),batchSize)

## rough estimate
auto_correlation <- function(x){
	if (is.matrix(x)){
		ACF <- rowMeans(apply(x,1,\(ROW) acf(ROW,plot=FALSE)$acf),na.rm=TRUE)
	} else {
		ACF <- acf(x,plot=FALSE)$acf
	}
	return(sum(ACF[ACF>0.2]))
}

## based on estimate of tau (auto-correlation length)
effective_size <- function(N, tau){
	return(N/(2*tau))
}

simulation_benchmark <- bench::mark(
	"simulation p0"={
		y <- s(p0)
		n <- 1                                              # result
	},
	"simulation P/2"={
		n <- NCOL(P)/2
		y <- s(P[,seq(n)])
		n                                                   # result
	},
	"simulation P"={
		y <- s(P)
		n <- NCOL(P)                                        # result
	},
	max_iterations=6,
	check=\(a,b){TRUE},
	memory=FALSE
)

print(simulation_benchmark)
SB <- simulation_benchmark |> dplyr::mutate(v_eff=unlist(result)/as.numeric(median))
print(SB[,c("median","v_eff")])

sampling_benchmark <- bench::mark(
	"abc smc" = {
		ret <- ABCSMC(Obj,t(rprior(700)),dprior=dprior)
		tau <- auto_correlation(ret$distances)
		n_eff <- effective_size(length(ret$distances),tau) # result
	},
	"abc mcmc" = {
		ret <- abc_mcmc(Obj,P,100,burnIn=50,Sigma0=cov(t(P))*0.1,dprior=dprior)
		d <- array(ret$distances,dim=c(batchSize,100))
		tau <- auto_correlation(ret$distances)
		n_eff <- effective_size(length(ret$distances),tau) # result
	},
	max_iterations=1,
	min_time=Inf,
	check=\(a,b){TRUE},
	memory = FALSE
)

## bench::mark stores all values that a block calculates as a list, no matter what they were:
AB <- sampling_benchmark |> dplyr::mutate(v_eff=unlist(result)/as.numeric(median))
print(AB[,c("median","v_eff")])
