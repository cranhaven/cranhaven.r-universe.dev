
#' @title runMCMC_btadjust
#' @description  returns a mcmc.list object which is the output of a Markov Chain Monte Carlo obtained after adjusting burn-in & thinning parameters to meet pre-specified criteria in terms of convergence & effective sample size - i.e. sample size adjusted for autocorrelation - of the MCMC output
#'
#' @param MCMC_language character value: designates the \code{MCMC_language} used to write & fit the Bayesian model in R. Current choices are "Nimble" - the default-, "Greta" or "Jags". Note that in case it is "Nimble", package \code{nimble} should be loaded in your search list if not using parallelization.
#' @param code R object: code for the model that will be used to build the MCMC when \code{MCMC_language} is "Nimble" or "Jags". If "Nimble", must be the name (in R) of the object which is the result of the function \code{nimbleCode}. If "Jags", should be either: (i) a character string which is the name of a txt file that contains the code of the model (as used in the function jags.model): should then end up by ".txt"; or (ii) a character string that contains the text of the Jags code.
#' @param data R list: a list that will contain the data when \code{MCMC_language} is "Nimble" or "Jags". If "Nimble", will be sent to the \code{data} argument of the \code{nimbleModel} function in \code{nimble} package, i.e. the data that have a random distribution in the model. If \code{MCMC_language} is "Greta", can be used just to document the summary of data in the output.
#' @param constants R list: a list that will contain the rest of the data (in addition to data) when \code{MCMC_language} is "Nimble". Will be sent to the \code{constants} argument of the \code{nimbleModel} function in \code{nimble} package, i.e. the data that do not have a random distribution in the model. If \code{MCMC_language} is "Greta", can be used just to document the summary of other data in the output.
#' @param model R object: should be the result of the \code{model} command of Greta when \code{MCMC_language} is "Greta".
#' @param Nchains integer value : the number of Markov chains to run in the MCMC.
#' @param inits either NULL, a function or an R list, with \code{Nchains} components. Each component is then a named list that contains the initial values of the parameters for the MCMC.
#' In case \code{MCMC_language=="Greta"}, each component should be the result of the \code{initials} function in \code{greta} package.
#' If a function, it will generate values for one chain.
#' @param params character vector: contains the names of the parameters to save at the end of the MCMC and to monitor for convergence and effective sample size;
#' inactive for convergence/effective sample size if \code{params.conv} is specified;
#' inactive for saving if \code{params.save} is specified.
#' @param params.conv character vector: contains the names of the parameters to monitor for convergence and effective sample size.
#' @param params.save character vector: contains the names of the parameters to be saved at the end of the MCMC.
#' @param niter.min integer value: the minimum number of iterations in each chain of the MCMC.
#' @param niter.max integer value: the maximum number of iterations in each chain of the MCMC. Will stop the MCMC once the number of iterations will reach this limit.
#' @param nburnin.min integer value: the minimum number of burn-in (=transitory) iterations in each chain of the MCMC.
#' @param nburnin.max integer value: the maximum number of burn-in (=transitory) iterations in each chain of the MCMC. Will stay at this burn-in value once this limit is reached.
#' @param thin.min integer value: the minimum value of the thin parameter of the MCMC.
#' @param thin.max integer value: the maximum value of the thin parameter of the MCMC. Will stay at this thin value once this limit is reached.
#' @param neff.min positive real number: minimum effective sample size - over parameters used to diagnose convergence & effective sample size- , as calculated with \code{neff.method} (specified in \code{Control}).
#' The algorithm will not stop if the minimum number of effective values is not above this value (unless another limit - e.g. \code{niter.max} - is reached).
#' @param neff.med positive real number: median effective sample size - over parameters used to diagnose convergence & effective sample size- , as calculated with \code{neff.method} (specified in \code{Control}).
#' The algorithm will not stop if the median number of effective values is not above this value (unless another limit - e.g. \code{niter.max} - is reached).
#' @param neff.mean positive real number: mean effective sample size - over parameters used to diagnose convergence & effective sample size-, as calculated with \code{neff.method} (specified in \code{Control}).
#' The algorithm will not stop if the mean number of effective values is not above this value (unless another limit - e.g. \code{niter.max} - is reached).
#' @param conv.max positive real number: maximum - over parameters used to diagnose convergence & effective sample size - convergence diagnostic, as calculated with \code{convtype} method (specified in \code{Control}).
#' The algorithm will not stop if the maximum convergence diagnostic is not below this value (unless another limit - e.g. \code{niter.max} - is reached).
#' @param conv.med positive real number: median - over parameters used to diagnose convergence & effective sample size - convergence diagnostic, as calculated with \code{convtype} method(specified in \code{Control}).
#' The algorithm will not stop if the median convergence diagnostic is not below this value (unless another limit - e.g. \code{niter.max} - is reached).
#' @param conv.mean positive real number: mean - over parameters used to diagnose convergence & effective sample size - convergence diagnostic, as calculated with \code{convtype} method (specified in \code{Control}).
#' The algorithm will not stop if the mean convergence diagnostic is not below this value (unless another limit - e.g. \code{niter.max} - is reached).

#' @param control list of \code{runMCMC_btadjust} control parameters: with the following components:\cr
#'  \itemize{ \item \code{time.max}: positive number (units: seconds): maximum time of the process in seconds, not including WAIC calculations and extra.calculations; the program will organize itself to stop (not including WAIC calculation and extra calculations as specified in \code{control.MCMC$extraCalculations}) before roughly \code{time.max}. Default to NULL, corresponding to no time constraint except in case of parallelization (see below).
#'   \item \code{check.convergence}: logical value: should the program check convergence at all? Default to TRUE. See Details.
#'   \item \code{check.convergence.firstrun}: logical value: should we check convergence after the first run? Default to NULL in which case will depend on \code{MCMC_language}: if MCMC_language!="Greta" & control.MCMC$n.adapt<=0, will be FALSE otherwise will be TRUE because warmup phase separated from the rest in case of Greta.
#'   \item \code{recheck.convergence}: logical value: should the algorithm recheck convergence once convergence has been found in a previous run? Default to TRUE.
#'   \item \code{convtype}: character or NULL value: specifies the type of convergence diagnostic used. Currently implemented: "Gelman" for original Gelman-Rubin diagnostic (only possible if \code{Nchains>=2}), "Gelman_new" for the version of the Gelman-Rubin diagnostic in the second version of "Bayesian Data Analysis" (Gelman, Carlin, Stern and Rubin)(only possible if \code{Nchains>=2}), "Geweke" for Geweke diagnostic (at present applied only in case \code{Nchains==1}) and "Heidelberger" for the reciprocal of Heidelberger-Welch first part of convergence diagnostic based on the Cramer-von Mises test statistic. If NULL (the default), chooses "Geweke" in case Nchains==1 and "Gelman" in case Nchains>1.
#'   \item \code{convtype.Gelman}: integer value: when \code{convtype=="Gelman"}, do we target the Point estimate diagnostic (value 1) or the Upper C.I. diagnostic (value 2). Default to 2.
#'   \item \code{convtype.Geweke}: real vector with two components between 0 and 1: (i) the fraction of samples to consider as the beginning of the chain (frac1 in geweke.diag); (ii) the fraction of samples to consider as the end of the chain (frac2 in \code{gewke.diag}). Default to c(0.1,0.5) as in \code{geweke.diag}.
#'   \item \code{convtype.alpha}: real value between 0 and 1: significance level used in case \code{convtype=="Gelman"} and \code{convtype.Gelman==2}, or \code{convtype=="Heidelberger"}
#'   \item \code{props.conv}: numeric vector: quantiles of number of iterations removed to recheck convergence and number of effective values (if not converged before or \code{conv.thorough.check} is TRUE). Values should be between 0 and 1.
#'   \item \code{ip.nc}: real value: inflexion point for log(scaleconvs-1); if (very) negative will tend to double the number of iterations in case of non convergence (i.e. add niter iterations) ; if (very) positive will tend to add niter/Ncycles iterations. Default to 0.
#'   \item \code{conv.thorough.check}: logical value: whether one goes through all the \code{props.conv} proportions to find the best one in terms first of convergence (just in terms of respecting the criterion) and then of neffs (if TRUE) or stops at the first \code{props.conv} corresponding to convergence or does not change if there is no convergence (if FALSE, the default).
#'   \item \code{neff.method}: character value: method used to calculate the effective sample sizes. Current choice between "Stan" (the default) and "Coda". If "Stan", uses the function \code{monitor} in package \code{rstan}. If "Coda", uses the function \code{effectiveSize} in package \code{coda}.
#'   \item \code{Ncycles.target}: integer value: targeted number of MCMC runs. Default to 2.
#'   \item \code{min.Nvalues}: integer value or expression giving an integer value or NULL: minimum number of values to diagnose convergence or level of autocorrelation. Default to NULL in which case is will be given the value neff.max calculated within the program.
#'   \item \code{round.thinmult}: logical value: should the thin multiplier be rounded to the nearest integer so that past values are precisely positioned on the modified iteration sequence? Default to TRUE. Value of FALSE may not be rigorous or may not converge well.
#'   \item \code{thinmult.in.resetMV.temporary}: logical value: should the thin multiplier be taken into account in resetting parameter collection in case MCMC_language is "Nimble". Important mainly if control.MCMC$WAIC is TRUE. If TRUE, resetting will be more frequent, and WAIC calculation will be longer and more rigorous. Default to TRUE.
#'   \item \code{check.thinmult}: integer value between 1, 2 and 3: how should we check thinmult value after thinmult calculation? If 3, it is tested whether thinmult meets specific criteria -relative to convergence reaching (i.e. if no convergence, no change), number of effective value reaching conservation, minimum number of output values - min.Nvalues- and proportional reduction of number of effective values - and if not decreased values are tested with the same criteria. If 2, the same checkings are done except the one on proportional reduction of effective values. If 1, only the min.Nvalues criterion is taken into account. Default to 2. A value of 3 should produce shorter MCMCs, more values in the output, with more autocorrelation, than a value of 1.
#'   \item \code{decrease.thinmult.multiplier}: positive number below 1: when adapting the proposed multiplier of thin (thinmult), the multiplier of the current thinmult used to propose a new - smaller - thinmult value, provided thinmult is above decrease.thinmult.threshold. Default to 0.8.
#'   \item \code{decrease.thinmult.threshold}: positive number above 3: when adapting the proposed multiplier of thin (thinmult), the threshold value for thinmult below which decreases of proposed thinmult are substractions of one unit. Default to 20.
#'   \item \code{only.final.adapt.thin}: logical value: should the thin parameter be adapted only at the end - so that during running of the MCMC we conserve a sufficient number of values - esp. with respect to min.Nvalues. Default to FALSE.
#'   \item \code{min.thinmult}: numeric value: minimum value of thin multiplier: if diagnostics suggest to multiply by less than this, this multiplication is not done. Default to 1.1.
#'   \item \code{force.niter.max}: logical value: if TRUE, the number of iterations is forced to go to niter.max - except for time.max constraints. Default to FALSE.
#'   \item \code{force.time.max}: logical value: if TRUE, the number of iterations is forced to go to approximately time.max seconds - except for niter.max constraints. Default to FALSE.
#'   \item \code{time.max.turns.off.niter.max}: logical value: if TRUE, and if time.max is finite, the number of iterations can be greater than niter.max (except if we are only in the phase of force.niter.max or force.time.max). Default to FALSE.
#'   \item \code{seed}: integer number or NULL value: seed for the pseudo-random number generator inside runMCMC_btadjust. Default to NULL in which case here is no control of this seed.
#'   \item \code{identifier.to.print}: character string: printed each time an MCMC update is ran to identify the model (esp. if multiple successive calls to \code{runMCMC_btadjust} are made).
#'   \item \code{safemultiplier.Nvals}: positive number: number bigger than 1 used to multiply the targeted number of effective values in calculations of additional number of iterations. Default to 1.2.
#'   \item \code{max.prop.decr.neff}: number between 0 and 1 used, if check.thinmult==3, to decide if we accept dimension reduction - through augmentation of thin parameter with thinmult -: maximum acceptable Proportional Decrease of the number of effective values: guaranties that at least (1-max.prop.decr.neff) times the number of effective values estimated prior to dimension reduction are kept. Default to 0.1.
#'   \item \code{print.diagnostics}: logical value: should diagnostics be printed each time they are calculated? Default to FALSE.
#'   \item \code{print.thinmult}: logical value: should the raw multiplier of thin be printed each time it is calculated? Default to TRUE.
#'   \item \code{innerprint}: logical value: should printings be done inside the function \code{monitor} of \code{rstan} in case \code{neff.method=="Stan"}? Default to FALSE.
#'   \item \code{remove.fixedchains}: logical value: should we remove Markov chains that do not vary during the first cycle (i.e. whose all parameters have zero variances)? Default to TRUE. If so, those chains are removed from the diagnostics, from the output values and associated chains are no longer updated if MCMC_language=="Nimble".
#'   \item \code{check.installation}: logical value: should the function check installation of packages and programs? Default to TRUE.
#'   \item \code{save.data}: logical value: should the program save the entire data in the call.params section of the attributes? Default to FALSE, in which case only a summary of data is saved.
#'   \item \code{conveff.final.allparams}: logical value: should the final convergence/number of effective values calculations in final.diags be done on all parameters? Default to TRUE.
#'    }
#' @param control.MCMC list of MCMC control parameters: with the following components - that depend on \code{MCMC_language}:
#'  \itemize{ \item \code{confModel.expression.toadd} (only for \code{MCMC_language=="Nimble"}): expression to add to \code{confModel} to specify samplers, remove nodes... \code{confModel} should be referred to by \code{confModel[[i]]}. See Details for an example.
#' \item \code{sampler} (only for \code{MCMC_language=="Greta"}): expression used to specify the sampler used.
#' \item \code{warmup} (only for \code{MCMC_language=="Greta"}): integer value used as warmup parameter in the mcmc.
#' \item \code{n.adapt} (only for \code{MCMC_language=="Jags"} or \code{MCMC_language=="Nimble"}): integer value: number of iterations used for adaptation (in function \code{jags.model} in \code{rjags} package in case \code{MCMC_language=="Jags"} and otherwise in Nimble - added to burnin: first iterations that are not saved).
#' \item \code{RNG.names} (only for \code{MCMC_language=="Jags"}): character vector: name of pseudo-random number generators for each chain. Each component of the vector should be among "base::Wichmann-Hill", "base::Marsaglia-Multicarry", "base::Super-Duper", "base::Mersenne-Twister". If less values than \code{Nchains} are provided, they are specified periodically.
#' \item \code{n_cores} (only for \code{MCMC_language=="Greta"}): integer or NULL: maximum number of cores to use by each sampler.
#' \item \code{showCompilerOutput} (only for \code{MCMC_language=="Nimble"}): logical value indicating whether details of C++ compilation should be printed. Default to FALSE to get default printings of limited size.
#' \item \code{buildDerivs} (only for \code{MCMC_language=="Nimble"}): logical value indicating whether derivatives should be prepared when preparing Nimble model (will esp. allow to use HMC sampler). Default to FALSE.
#' \item \code{resetMV} (only for \code{MCMC_language=="Nimble"}): logical value to be passed to $run specifying whether previous parameter samples should be reset or not. Default to FALSE to speed up WAIC calculations. You can turn it to TRUE if you wish to speed up runs of MCMC (cf. https://groups.google.com/g/nimble-users/c/RHH9Ybh7bSI/m/Su40lgNRBgAJ).
#' \item \code{parallelize} (only for \code{MCMC_language=="Jags"} and \code{MCMC_language=="Nimble"}): logical value specifying whether the MCMC should be parallelized within the \code{runMCMC_btadjust} function with the \code{parallel} package (and for the moment default settings of this package). Default to FALSE. If TRUE, library \code{parallel} should be loaded. If TRUE and \code{control$time.max} is unspecified or infinite, each parallelized process will have a maximum duration of 30 days. In case \code{MCMC_language=="Greta"}, parallelization is managed directly by Greta.
#' \item \code{parallelizeInitExpr} (only for \code{MCMC_language=="Jags"} and \code{MCMC_language=="Nimble"}): expression to add in each cluster created by parallelization. Default to \code{expression(if(MCMC_language=="Nimble"){library(nimble);if(control.MCMC$APT) {library(nimbleAPT)}} else {NULL})}.
#' \item \code{useConjugacy} (only for \code{MCMC_language=="Nimble"}): logical value specifying whether Nimble should search for conjugate priors in the model. Default to FALSE. If TRUE, can render model configuration shorter (https://groups.google.com/g/nimble-users/c/a6DFCefYfjU/m/kqUWx9UXCgAJ) at the expense of not allowing any conjugate sampler
#' \item \code{WAIC} (only for \code{MCMC_language=="Nimble"}): logical value specifying whether WAIC should be calculated online within Nimble. Default to FALSE.
#' \item \code{WAIC.Nsamples} (only for \code{MCMC_language=="Nimble"}): integer number: number of (nearly independent) samples of parameters in the posterior distribution over which to calculate WAIC. If WAIC is calculated on more than one chain, this number will be required for each of these chains. Default to 2000.
#' \item \code{WAIC.control} (only for \code{MCMC_language=="Nimble"}): named list or list of such named lists: list (or lists) specifying the control parameters to calculate WAIC online within Nimble. Default to list(online=TRUE,dataGroups=NULL,marginalizeNodes=NULL,niterMarginal=1000,convergenceSet=c(0.25,0.5,0.75),thin=TRUE,nburnin_extra=0). Given the way WAIC is here calculated (after convergence and over the last sample outputs for one chain by named list), components thin will be turned to TRUE and nburnin_extra to 0. If several lists are used, only at most the first Nchains - reduced by removal of fixed chains - lists will be taken into account to calculate WAICs in different ways on different Markov Chains.
#' \item \code{APT} (only for \code{MCMC_language=="Nimble"}): logical value specifying whether \code{NimbleAPT} should be used to allow Adaptive Parallel Tempering for at least a subset of the model parameters. If so, the sampler_RW_tempered sampler is declared for all parameters. If wishing to change sampler, use \code{confModel.expression.toadd}. Default to FALSE.
#' \item \code{APT.NTemps} (only for \code{MCMC_language=="Nimble"}): integer number: number of temperatures for \code{NimbleAPT}. Default to 7.
#' \item \code{APT.initTemps} (only for \code{MCMC_language=="Nimble"}): NULL or double vector of length \code{APT.NTemps}: initial temperatures for Nimble APT. Default to NULL in which case initial temperatures will be 1:APT.NTemps. The values should be increasing with a first value of 1.
#' \item \code{APT.tuneTemps} (only for \code{MCMC_language=="Nimble"}): numerical vector of length 2: values to feed the parameters \code{tuneTemper1} and \code{tuneTemper2} in \code{NimbleAPT}. See documentation of \code{NimbleAPT}. Default to c(10,0.7).
#' \item \code{APT.thinPrintTemps} (only for \code{MCMC_language=="Nimble"}): expression or numerical value : thinning parameter for printing temperatures in case APT. Default to expression(niter/5).
#' \item \code{includeAllStochNodes} (only for \code{MCMC_language=="Nimble"}): logical value specifying whether all stochastic nodes should be made available in \code{runMCMC_btadjust}. Default to FALSE. Maybe useful for the \code{extraCalculations} component of \code{control.MCMC}. Can include more parameters than \code{includeParentNodes}. Can be useful for example to calculate goodness-of-fit p-values.
#' \item \code{monitorAllStochNodes} (only for \code{MCMC_language=="Nimble"}): logical value specifying whether all stochastic nodes should be monitored for convergence and number of effective values in \code{runMCMC_btadjust}. Default to FALSE.
#' \item \code{saveAllStochNodes} (only for \code{MCMC_language=="Nimble"}): logical value specifying whether all stochastic nodes should be made available in \code{runMCMC_btadjust} and kept in the result of \code{runMCMC_btadjust}. Default to FALSE. Note that if TRUE, will change the content of \code{params_saved}.
#' \item \code{includeParentNodes} (only for \code{MCMC_language=="Nimble"}): logical value specifying whether parent stochastic nodes of data should be made available in \code{runMCMC_btadjust}. Default to FALSE. Maybe useful for the \code{extraCalculations} component of \code{control.MCMC} - for example for "offline" calculations of WAIC or DIC (Deviance information criterion).
#' \item \code{monitorParentNodes} (only for \code{MCMC_language=="Nimble"}): logical value specifying whether parent stochastic nodes of data should be monitored for convergence and number of effective values in \code{runMCMC_btadjust}. Default to FALSE.
#' \item \code{saveParentNodes} (only for \code{MCMC_language=="Nimble"}): logical value specifying whether parent stochastic nodes of data should be made available in \code{runMCMC_btadjust} and kept in the result of \code{runMCMC_btadjust}. Default to FALSE. Note that if TRUE, will change the content of \code{params_saved}.
#' \item \code{extraCalculations} (mainly useful for \code{MCMC_language=="Nimble"} but can be used with other languages as well): NULL value or expression that will be evaluated at the end of \code{runMCMC_btadjust}. The value of this expression will be saved in the \code{extraResults} component of the \code{final.params} component of the attributes of the result of \code{runMCMC_btadjust}. See the vignette devoted to the use of it. Default to NULL.
#'        }
#'
#'
#' @return a \code{mcmc.list} object with attributes with the following components:
#'       \itemize{ \item \code{call.params}: a list containing most of the important arguments of the \code{runMCMC_btadjust} call as well as either a summary of dimensions/lengths and mean of components of \code{data} and \code{constants} arguments or their entire values.
#'       \item \code{final.params}: a list with the parameters of the MCMC at the end of fitting:
#'             \itemize{ \item \code{converged}: logical: TRUE if model has converged when stopping the MCMC, FALSE otherwise
#'             \item \code{neffs.reached}: logical: TRUE if model has converged and reached the objectives in terms of effective sample size, FALSE otherwise
#'             \item \code{final.Nchains}: number of Markov chains finally retained - since some chains could be excluded if invariable.
#'             \item \code{removed.chains}: identity of the Markov chains finally removed - those that would have been invariable during the first cycle.
#'             \item \code{burnin}: number of iterations of the transient (burn-in) period
#'             \item \code{thin}: number of iterations used for thinning the final output
#'             \item \code{niter.tot}: total number of iterations (of each MCMC chain)
#'             \item \code{Nvalues}: number of saved values (over all the MCMC chains)
#'             \item \code{neff.min}: minimum number of effective values in the final MCMC (over params.conv)
#'             \item \code{neff.median}: median number of effective values (over params.conv)
#'             \item \code{WAIC}: results of the calculus of online WAIC (only if control.MCMC$WAIC and \code{MCMC_language=="Nimble"}). One component per component of control.MCMC$control.WAIC. Each component has a WAIC component and then a WAICDetails component.
#'             \item \code{extraResults}: results of the implementation of control.MCMC$extraCalculations. Can be any kind of R object.
#'             \item \code{Temps}: results of the series of Temperatures met in APT (only if control.MCMC$APT and \code{MCMC_language=="Nimble"}). One line for each end of $run (=MCMC run).
#'             \item \code{duration}: total duration (elapsed time) of the fit (in seconds)
#'             \item \code{duration.MCMC.preparation}: duration (elapsed time) of MCMC preparation (in seconds)
#'             \item \code{duration.MCMC.transient}: duration (elapsed time) of the MCMC transient (burn-in) phase (in seconds)
#'             \item \code{duration.MCMC.asymptotic}: duration (elapsed time) of the MCMC asymptotic phase (in seconds)
#'             \item \code{duration.MCMC.after}: duration (elapsed time) of the MCMC phase after the asymptotic phase of sampling (e.g. to calculate WAIC) (in seconds)
#'             \item \code{duration.btadjust}: duration (elapsed time) outside MCMC preparation & fitting (in seconds)
#'             \item \code{CPUduration}: total CPU duration (user+system, self+child if not NA - otherwise only self) of the fit (in seconds)
#'             \item \code{CPUduration.MCMC.preparation}: CPU duration (user+system, self+child if not NA - otherwise only self) of MCMC preparation (in seconds)
#'             \item \code{CPUduration.MCMC.transient}: CPU duration (user+system, self+child if not NA - otherwise only self) of the MCMC transient (burn-in) phase (in seconds)
#'             \item \code{CPUduration.MCMC.asymptotic}: CPU duration (user+system, self+child if not NA - otherwise only self) of the MCMC asymptotic phase (in seconds)
#'             \item \code{CPUduration.MCMC.after}: CPU duration (user+system, self+child if not NA - otherwise only self) of the MCMC phase after the asymptotic phase of sampling (e.g. to calculate WAIC) (in seconds)
#'             \item \code{CPUduration.btadjust}: CPU duration (user+system, self+child if not NA - otherwise only self) outside MCMC preparation & fitting (in seconds)
#'             \item \code{childCPUduration}: total child CPU duration (user+system) of the fit (in seconds)
#'             \item \code{childCPUduration.MCMC.preparation}: child CPU duration (user+system) of MCMC preparation (in seconds)
#'             \item \code{childCPUduration.MCMC.transient}: child CPU duration (user+system) of the MCMC transient (burn-in) phase (in seconds)
#'             \item \code{childCPUduration.MCMC.asymptotic}: child CPU duration (user+system) of the MCMC asymptotic phase (in seconds)
#'             \item \code{childCPUduration.MCMC.after}: child CPU duration (user+system) of the MCMC phase after the asymptotic phase of sampling (e.g. to calculate WAIC) (in seconds)
#'             \item \code{childCPUduration.btadjust}: child CPU duration (user+system) outside MCMC preparation & fitting (in seconds)
#'             \item \code{time}: time (from Sys.time) at the end of model fitting
#'             }
#'        \item \code{final.diags}: a list with final diagnostics of the fit:
#'             \itemize{ \item \code{params}: parameters of the MCMC (burn-in, thin, niter...)
#'             \item \code{conv_synth}: synthetic output of convergence diagnostics
#'             \item \code{neff_synth}: synthetic output for calculations of effective sample sizes
#'             \item \code{conv}: raw convergence values for all the parameters being diagnosed if control$conveff.final.allparams is FALSE and all the parameters otherwise
#'             \item \code{neff}: raw effective sample size values for all the parameters being diagnosed if control$conveff.final.allparams is FALSE and all the parameters otherwise
#'             }
#'        \item \code{sessionInfo}: a list containing the result of the call to sessionInfo() function at the end of runMCMC_btadjust function; contains info on the platform, versions of packages, R version...;
#'        \item \code{warnings}: a list of the warning messages issued during fitting; unsure it still works with this version
#'        \item \code{error}: a list with the error messages issued during fitting; unsure it still works with this version
#'        }
#'
#'@details
#' Recap: \cr
#' If \code{MCMC_language=="Nimble"}, the code, data and constants arguments should be specified according to the requirements of \code{nimble} package. \cr
#' If \code{MCMC_language=="Jags"}, the code and  data arguments need to be specified as required by \code{rjags} package.  \cr
#' If \code{MCMC_language=="Greta"}, the model argument must be specified and should be the result of the \code{model} command in \code{greta} package.  \cr
#'
#' Details on \code{check.convergence}: \cr
#' If FALSE, no check of convergence at all, after \code{nburnin.min} (& \code{recheck.convergence} is put to FALSE & \code{check.convergence.firstrun} is dominated by \code{check.convergence}). \cr
#' If TRUE, the convergence behavior is governed by \code{check.convergence.firstrun} & \code{recheck.convergence}.\cr
#'
#' Example for \code{confModel.expression.toadd} component of \code{control.MCMC}:\cr
#' 		\code{confModel.expression.toadd<-expression({ConfModel[[i]]$removeSamplers(c("alpha","dzetad","beta","exper_bias[2]","exper_bias[3]","exper_precision[2]","exper_precision[3]"))
#' 			ConfModel[[i]]$addSampler(target = c("alpha","dzetad","beta"),type = "RW_block")
#' 			ConfModel[[i]]$addSampler(target = c("exper_bias[2]","exper_bias[3]"),type = "RW_block")
#' 			ConfModel[[i]]$addSampler(target = c("exper_precision[2]","exper_precision[3]"),type = "RW_block")
#' 			})}
#'
#' Remark for \code{params, params.conv, params.save}:\cr
#' in cases of parameters that are vectors, matrices... the \code{params} vector can contain only the name of the vector or matrix... in which case all its components will be used. It can also contain the names of individual components.
#'
#'
#' @examples
#' # for examples with Nimble or Greta, see the Presentation Vignette.
#' \dontrun{
#' #generating data
#' set.seed(1)
#' y1000<-rnorm(n=1000,mean=600,sd=30)
#' ModelData <-list(mass = y1000,nobs = length(y1000))
#'
#' #writing the Jags code as a character chain in R
#' modeltotransfer<-"model {
#'
#' # Priors
#' population.mean ~ dunif(0,5000)
#' population.sd ~ dunif(0,100)
#'
#' # Precision = 1/variance: Normal distribution parameterized by precision in Jags
#' population.variance <- population.sd * population.sd
#' precision <- 1 / population.variance
#'
#' # Likelihood
#' for(i in 1:nobs){
#'   mass[i] ~ dnorm(population.mean, precision)
#'  }
#'  }"
#'
#' #specifying the initial values
#' ModelInits <- function()
#' {list (population.mean = rnorm(1,600,90), population.sd = runif(1, 1, 30))}
#' params <- c("population.mean", "population.sd", "population.variance")
#' K<-3
#' set.seed(1)
#' Inits<-lapply(1:K,function(x){ModelInits()})
#'
#' # running runMCMC_btadjust with MCMC_language="Jags":
#' set.seed(1)
#' out.mcmc.Coda<-runMCMC_btadjust(MCMC_language="Jags", code=modeltotransfer,
#' data=ModelData,
#' Nchains=K, params=params, inits=Inits,
#' niter.min=1000, niter.max=300000,
#' nburnin.min=100, nburnin.max=200000,
#' thin.min=1, thin.max=1000,
#' neff.min=1000, conv.max=1.05,
#' control=list(print.diagnostics=TRUE, neff.method="Coda"))
#'
#' summary(out.mcmc.Coda)
#' }
#' @importFrom stats median rnorm qnorm quantile update var window
#' @importFrom utils sessionInfo capture.output
#' @export
#'
runMCMC_btadjust<-function(code=NULL,data=NULL,constants=NULL,model=NULL,MCMC_language="Nimble",
                           Nchains,inits=NULL,
                           params=NULL,params.conv=NULL,params.save=NULL,
                           niter.min=100,niter.max=Inf,nburnin.min=10,nburnin.max=Inf,thin.min=1,thin.max=Inf,
                           neff.min=NULL,neff.med=NULL,neff.mean=NULL,
                           conv.max=NULL,conv.med=NULL,conv.mean=NULL,
                           control=list(time.max=NULL,
                                        check.convergence=TRUE,check.convergence.firstrun=NULL,recheck.convergence=TRUE,
                                        convtype=NULL,convtype.Gelman=2,convtype.Geweke=c(0.1,0.5),convtype.alpha=0.05,ip.nc=0,neff.method="Stan",
                                        Ncycles.target=2,props.conv=c(0.25,0.5,0.75),min.Nvalues=NULL,
                                        min.thinmult=1.1, force.niter.max=FALSE, force.time.max=FALSE, time.max.turns.off.niter.max=FALSE, safemultiplier.Nvals=1.2,max.prop.decr.neff=0.1,round.thinmult=TRUE,thinmult.in.resetMV.temporary=TRUE,check.thinmult=2,decrease.thinmult.multiplier=0.8, decrease.thinmult.threshold=20,only.final.adapt.thin=FALSE,
                                        identifier.to.print="",print.diagnostics=FALSE,conv.thorough.check=FALSE,print.thinmult=TRUE,innerprint=FALSE,seed=NULL,remove.fixedchains=TRUE,check.installation=TRUE,save.data=FALSE,conveff.final.allparams=TRUE),
                           control.MCMC=list(confModel.expression.toadd=NULL,sampler=expression(hmc()),warmup=1000,n.adapt=-1,RNG.names=c("base::Wichmann-Hill", "base::Marsaglia-Multicarry", "base::Super-Duper","base::Mersenne-Twister"),
                                             n_cores=NULL,showCompilerOutput=FALSE,buildDerivs=FALSE,resetMV=FALSE,parallelize=FALSE, parallelizeInitExpr= expression(if(MCMC_language=="Nimble"){library("nimble");if(control.MCMC$APT) {library("nimbleAPT")}} else {NULL}), useConjugacy=FALSE,
                                             WAIC=FALSE,WAIC.Nsamples=2000,
                                             WAIC.control=list(online=TRUE,dataGroups=NULL,marginalizeNodes=NULL,niterMarginal=1000,convergenceSet=c(0.25,0.5,0.75),thin=TRUE,nburnin_extra=0),
                                             APT=FALSE, APT.NTemps=7, APT.initTemps=NULL,APT.tuneTemps=c(10,0.7),APT.thinPrintTemps=expression(niter/5), includeAllStochNodes=FALSE, monitorAllStochNodes=FALSE, saveAllStochNodes=FALSE,includeParentNodes=FALSE,monitorParentNodes=FALSE, saveParentNodes=FALSE, extraCalculations=NULL))

{
  time.MCMC.Preparation0<-Sys.time()
  CPUtime.MCMC.Preparation<-0
  CPUtime.MCMC<-0
  CPUtime.MCMC.after<-0
  CPUtime.btadjust<-0

  childCPUtime.MCMC.Preparation<-0
  childCPUtime.MCMC<-0
  childCPUtime.MCMC.after<-0
  childCPUtime.btadjust<-0

  #temporarily added waiting for the resolution of mere call to 'nimble::': cf. https://groups.google.com/g/nimble-users/c/-pDY9dB9ovE
  #if (MCMC_language=="Nimble") {library(nimble)}


  current.CPU.time<-system.time({
    #starting counting time
    time.start<-Sys.time()

    ########################################
    ## 00- code of functions that will be used in the sequel:
    ########################################

    getseeds <- function(ntasks, iseed) {
      RNGkind("L'Ecuyer-CMRG")
      set.seed(iseed)
      seeds <- vector("list", ntasks)
      seeds[[1]] <- .Random.seed
      for (i in seq_len(ntasks - 1)) {
        seeds[[i + 1]] <- parallel::nextRNGSubStream(seeds[[i]])
      }
      seeds
    }
    worker.seed <- function(seed) {
      assign(".Random.seed", seed, envir=.GlobalEnv)}

    window.seq<-function(out,start=NULL,end=NULL,thin=1)
    {#out should be a mcmc.list
      #this function does the same thing as window on mcmc.list except that is allows going backwards thanks to the possibilities of function seq
      #actually it reverses the order to start from the end first to start from the end then reverses the result
      if (is.null(start)) {start<-1}
      if (is.null(end)) {end<-dim(out[[1]])[1]}
      indices<-rev(seq(from=end,to=start,by=-thin))
      res<-out
      for (i in 1:length(out))
      {
        res[[i]]<-res[[i]][indices,,drop=FALSE]
      }
      res<-coda::as.mcmc.list(lapply(res,function(x){y=coda::as.mcmc(as.matrix(x));attributes(y)[["mcpar"]]<-c(min(indices), max(indices), thin);y}))
      res
    }

    formatC_adapted<-function(x,digits=3)
    {#puts numbers in an adequate format for printing
      #as.double(formatC(as.double(formatC(x,digits=digits,format="f")),format="g"))
      as.double(formatC(x,digits=digits,format="f"))
    }
    logit<-function(p) {log(p/(1-p))}

    conveff<-function(out,print.diagnostics=FALSE)
    {
      if (length(grep("logProb_",dimnames(out[[1]])[[2]]))>0)
      {
        out<-coda::as.mcmc.list(lapply(out,function(x){coda::as.mcmc(as.matrix(x[,-grep("logProb_",dimnames(x)[[2]])]))}))
      }
      tempres_params<-cbind.data.frame(Nchains=length(out),thin=thin*(attributes(out[[1]]))$mcpar[3],niter.tot=niter.tot,Nvalues=dim(out[[1]])[1]*length(out),nu.burn= nburnin.min0+sum(numIter.samplesList[1:(index.conv.local-1)]))
      tempres_params_print<-tempres_params
      row.names(tempres_params_print)<-"MCMC parameters"

      if (control$convtype=="Gelman") {
        tempg<-coda::gelman.diag(coda::as.mcmc.list(lapply(out,function(x){coda::as.mcmc(as.matrix(x))})),confidence=1-control$convtype.alpha,autoburnin=FALSE,multivariate=FALSE)[[1]]
        tempres_conv<-cbind.data.frame(max_gelm_Upper_C_I=max(tempg[,2])[1],med_gelm_Upper_C_I=median(tempg[,2])[1],mean_gelm_Upper_C_I=mean(tempg[,2]),max_gelm_Point_Est=max(tempg[,1])[1],name_max_gelm_Point_Est=(dimnames(tempg)[[1]])[tempg[,1]==max(tempg[,1])][1],med_gelm_Point_Est=median(tempg[,1])[1],mean_gelm_Point_Est=mean(tempg[,1]),prop_gelm_Point_Est_above_1p2=mean(tempg[,1]>1.2),prop_gelm_Point_Est_above_1p05=mean(tempg[,1]>1.05),prop_gelm_Point_Est_above_1p01=mean(tempg[,1]>1.01))
        tempres_conv_print<-cbind.data.frame(max=formatC_adapted(c(max(tempg[,2])[1],max(tempg[,1])[1])),median=formatC_adapted(c(median(tempg[,2])[1],median(tempg[,1])[1])),mean=formatC_adapted(c(mean(tempg[,2]),mean(tempg[,1]))),name_max=c((dimnames(tempg)[[1]])[tempg[,2]==max(tempg[,2])][1],(dimnames(tempg)[[1]])[tempg[,1]==max(tempg[,1])][1]),prop_ab_1p2=formatC_adapted(c(mean(tempg[,2]>1.2),mean(tempg[,1]>1.2))),prop_ab_1p05=formatC_adapted(c(mean(tempg[,2]>1.05),mean(tempg[,1]>1.05))),prop_ab_1p01=formatC_adapted(c(mean(tempg[,2]>1.01),mean(tempg[,1]>1.01))))
        row.names(tempres_conv_print)<-c("Gelman_Upper_C_I","Gelman_Point_Est")
      }

      if (control$convtype=="Gelman_new") {
        tempg<-as.data.frame(ggmcmc::ggs_Rhat(ggmcmc::ggs(coda::as.mcmc.list(lapply(out,function(x){coda::as.mcmc(as.matrix(x))}))),plot=FALSE))
        row.names(tempg)<-as.character(tempg[,1])
        tempg<-tempg[,2,drop=FALSE]
        tempres_conv<-cbind.data.frame(max_gelm_Point_Est=max(tempg[,1])[1],name_max_gelm_Point_Est=row.names(tempg)[tempg[,1]==max(tempg[,1])][1],med_gelm_Point_Est=median(tempg[,1])[1],mean_gelm_Point_Est=mean(tempg[,1]),prop_gelm_Point_Est_above_1p2=mean(tempg[,1]>1.2),prop_gelm_Point_Est_above_1p05=mean(tempg[,1]>1.05),prop_gelm_Point_Est_above_1p01=mean(tempg[,1]>1.01))
        tempres_conv_print<-cbind.data.frame(max=formatC_adapted(c(max(tempg[,1])[1])),median=formatC_adapted(c(median(tempg[,1])[1])),mean=formatC_adapted(c(mean(tempg[,1]))),name_max=c(row.names(tempg)[tempg[,1]==max(tempg[,1])][1]),prop_ab_1p2=formatC_adapted(c(mean(tempg[,1]>1.2))),prop_ab_1p05=formatC_adapted(c(mean(tempg[,1]>1.05))),prop_ab_1p01=formatC_adapted(c(mean(tempg[,1]>1.01))))
        row.names(tempres_conv_print)<-c("Gelman_Point_Est")
      }


      if (control$convtype=="Geweke") {
        tempg<-abs(coda::geweke.diag(coda::as.mcmc.list(lapply(out,function(x){coda::as.mcmc(as.matrix(x))}))[[1]],frac1=control$convtype.Geweke[1],frac2=control$convtype.Geweke[2])$z)
        tempres_conv<-cbind.data.frame(max_gew=max(tempg),med_gew=median(tempg),mean_gew=mean(tempg),name_max_gew=names(tempg)[tempg==max(tempg)][1],prop_gew_above_p975=mean(tempg>qnorm(0.975)),prop_gew_above_p995=mean(tempg>qnorm(0.995)),prop_gew_above_p9995=mean(tempg>qnorm(0.9995)))
        tempres_conv_print<-cbind.data.frame(max=formatC_adapted(max(tempg)),median=formatC_adapted(median(tempg)),mean=formatC_adapted(mean(tempg)),name_max=names(tempg)[tempg==max(tempg)][1],prop_ab_p975=formatC_adapted(mean(tempg>qnorm(0.975))),prop_ab_p995=formatC_adapted(mean(tempg>qnorm(0.995))),prop_ab_p9995=formatC_adapted(mean(tempg>qnorm(0.9995))))
        row.names(tempres_conv_print)<-c("Geweke")
      }

      if (control$convtype=="Heidelberger") {
        tempg<-1-(coda::heidel.diag(coda::as.mcmc.list(lapply(out,function(x){coda::as.mcmc(as.matrix(x))}))[[1]],pvalue=control$convtype.alpha)[,"stest"])
        tempres_conv<-cbind.data.frame(max_gew=max(tempg),med_gew=median(tempg),mean_gew=mean(tempg),name_max_gew=names(tempg)[tempg==max(tempg)][1],prop_gew_above_p975=mean(tempg>qnorm(0.975)),prop_gew_above_p995=mean(tempg>qnorm(0.995)),prop_gew_above_p9995=mean(tempg>qnorm(0.9995)))
        tempres_conv_print<-cbind.data.frame(max=formatC_adapted(max(tempg)),median=formatC_adapted(median(tempg)),mean=formatC_adapted(mean(tempg)),name_max=names(tempg)[tempg==max(tempg)][1],prop_ab_p975=formatC_adapted(mean(tempg>qnorm(0.975))),prop_ab_p995=formatC_adapted(mean(tempg>qnorm(0.995))),prop_ab_p9995=formatC_adapted(mean(tempg>qnorm(0.9995))))
        row.names(tempres_conv_print)<-c("Heidelberger")
      }

      ### calculs squares_Ses_ratios
      tempsu<-summary(out)[[1]]
      if (is.null(dimnames(tempsu)[[1]])) {names.tempsu<-c("only.parameter")} else {names.tempsu<-dimnames(tempsu)[[1]]}
      tempsu<-matrix(tempsu,ncol=4)
      squared_SE_ratio<-((tempsu[,4])/(tempsu[,3]))^2
      names(squared_SE_ratio)<-names.tempsu
      #print(squared_SE_ratio)
      tempres_SEs<-cbind.data.frame(max_sqSEratio=max(squared_SE_ratio)[1],name_max_sqSEratio=names(squared_SE_ratio)[squared_SE_ratio==max(squared_SE_ratio)][1],med_sqSEratio=median(squared_SE_ratio)[1],prop_sqSEratio_above_2=mean(squared_SE_ratio>2),prop_sqSEratio_above_1p2=mean(squared_SE_ratio>1.2))

      if (control$neff.method=="Stan")
      {
        MonModelStan<- aperm(as.array(out,drop=FALSE),c(1,3,2))
        dimnames(MonModelStan)<-list(NULL,NULL,{toto<-dimnames(out[[1]])[[2]]; duplicates<-duplicated(toto); duplicates<-(1:length(duplicates))[duplicates]; if (length(duplicates)>0) {toto[duplicates]<-paste0(toto[duplicates],".",1:length(duplicates))};toto})
        tempm<-rstan::monitor(MonModelStan,print=control$innerprint,warmup=0)[,"n_eff"]
      }

      if (control$neff.method=="Coda")
      {
        tempm<-coda::effectiveSize(out)
      }


      tempres_neff<-cbind.data.frame(min_neff=min(tempm)[1],name_min_neff=names(tempm)[tempm==min(tempm)][1],med_neff=median(tempm),mean_neff=mean(tempm),prop_neff_below_1000=mean(tempm<1000),prop_neff_below_5000=mean(tempm<5000),prop_neff_below_10000=mean(tempm<10000))
      tempres_neff_print<-cbind.data.frame(min=formatC_adapted(min(tempm)[1]),median=formatC_adapted(median(tempm)),mean=formatC_adapted(mean(tempm)),name_min=names(tempm)[tempm==min(tempm)][1],prop_bel_1000=formatC_adapted(mean(tempm<1000)),prop_bel_5000=formatC_adapted(mean(tempm<5000)),prop_bel_10000=formatC_adapted(mean(tempm<10000)))
      row.names(tempres_neff_print)<-"Neff            "
      #tempressimple<-tempres[,c("maxgelm_Point_Est","medelm1","min_neff","med_neff")]
      if (print.diagnostics) {
        print("###################################################################################")
        print("Current state of diagnostics:"); print(tempres_params_print)
        print("###################################################################################")
        print(tempres_conv_print)
        print("###################################################################################")
        print(tempres_neff_print)
        print("###################################################################################")
      }
      tempres<-cbind.data.frame(tempres_params,tempres_conv,tempres_neff,tempres_SEs)

      tempres
    }	#END conveff function


    conveff_final<-function(out,indices.conv,conveff.final.allparams=TRUE,print.diagnostics=FALSE)
    {if (length(dim(out[[1]]))==2)
		{condition.conv<-is.element(1:(dim(out[[1]])[2]),indices.conv) }
		else {out<-coda::as.mcmc.list(out)[,indices.conv,drop=FALSE]
		condition.conv<-rep(TRUE,dim(out[[1]])[2])}
      if (!conveff.final.allparams) {
        out<-coda::as.mcmc.list(lapply(out,function(x){coda::as.mcmc(as.matrix(x[,condition.conv]))}))
        condition.conv<-rep(TRUE,dim(out[[1]])[2])
        }


      if (length(grep("logProb_",dimnames(out[[1]])[[2]]))>0)
      {
        condition.conv<-condition.conv[-grep("logProb_",dimnames(out[[1]])[[2]])]
        out<-coda::as.mcmc.list(lapply(out,function(x){coda::as.mcmc(as.matrix(x[,-grep("logProb_",dimnames(x)[[2]])]))}))
      }

      tempres_params<-cbind.data.frame(Nchains=length(out),thin=thin*(attributes(out[[1]]))$mcpar[3],niter.tot=niter.tot,Nvalues=dim(out[[1]])[1]*length(out),nu.burn= nburnin.min0+sum(numIter.samplesList[1:(index.conv.local-1)]))
      tempres_params_print<-tempres_params
      row.names(tempres_params_print)<-"MCMC parameters"

      if (control$convtype=="Gelman") {
        tempg<-coda::gelman.diag(coda::as.mcmc.list(lapply(out,function(x){coda::as.mcmc(as.matrix(x))})),confidence=1-control$convtype.alpha,autoburnin=FALSE,multivariate=FALSE)[[1]]
        tempres_conv<-cbind.data.frame(max_gelm_Upper_C_I=max(tempg[,2])[1],med_gelm_Upper_C_I=median(tempg[,2])[1],mean_gelm_Upper_C_I=mean(tempg[,2]),max_gelm_Point_Est=max(tempg[,1])[1],name_max_gelm_Point_Est=(dimnames(tempg)[[1]])[tempg[,1]==max(tempg[,1])][1],med_gelm_Point_Est=median(tempg[,1])[1],mean_gelm_Point_Est=mean(tempg[,1]),prop_gelm_Point_Est_above_1p2=mean(tempg[,1]>1.2),prop_gelm_Point_Est_above_1p05=mean(tempg[,1]>1.05),prop_gelm_Point_Est_above_1p01=mean(tempg[,1]>1.01))
        tempres_conv_print<-cbind.data.frame(max=formatC_adapted(c(max(tempg[condition.conv,2])[1],max(tempg[condition.conv,1])[1])),median=formatC_adapted(c(median(tempg[condition.conv,2])[1],median(tempg[condition.conv,1])[1])),mean=formatC_adapted(c(mean(tempg[condition.conv,2]),mean(tempg[condition.conv,1]))),name_max=c((dimnames(tempg[condition.conv,,drop=FALSE])[[1]])[tempg[condition.conv,2]==max(tempg[condition.conv,2])][1],(dimnames(tempg[condition.conv,,drop=FALSE])[[1]])[tempg[condition.conv,1]==max(tempg[condition.conv,1])][1]),prop_ab_1p2=formatC_adapted(c(mean(tempg[condition.conv,2]>1.2),mean(tempg[condition.conv,1]>1.2))),prop_ab_1p05=formatC_adapted(c(mean(tempg[condition.conv,2]>1.05),mean(tempg[condition.conv,1]>1.05))),prop_ab_1p01=formatC_adapted(c(mean(tempg[condition.conv,2]>1.01),mean(tempg[condition.conv,1]>1.01))))
        row.names(tempres_conv_print)<-c("Gelman_Upper_C_I","Gelman_Point_Est")
      }

      if (control$convtype=="Gelman_new") {
        tempg<-as.data.frame(ggmcmc::ggs_Rhat(ggmcmc::ggs(coda::as.mcmc.list(lapply(out,function(x){coda::as.mcmc(as.matrix(x))}))),plot=FALSE))
        row.names(tempg)<-as.character(tempg[,1])
        tempg<-tempg[,2,drop=FALSE]
        tempres_conv<-cbind.data.frame(max_gelm_Point_Est=max(tempg[,1])[1],name_max_gelm_Point_Est=row.names(tempg)[tempg[,1]==max(tempg[,1])][1],med_gelm_Point_Est=median(tempg[,1])[1],mean_gelm_Point_Est=mean(tempg[,1]),prop_gelm_Point_Est_above_1p2=mean(tempg[,1]>1.2),prop_gelm_Point_Est_above_1p05=mean(tempg[,1]>1.05),prop_gelm_Point_Est_above_1p01=mean(tempg[,1]>1.01))
        tempres_conv_print<-cbind.data.frame(max=formatC_adapted(c(max(tempg[condition.conv,1])[1])),median=formatC_adapted(c(median(tempg[condition.conv,1])[1])),mean=formatC_adapted(c(mean(tempg[condition.conv,1]))),name_max=c(row.names(tempg)[tempg[condition.conv,1]==max(tempg[condition.conv,1])][1]),prop_ab_1p2=formatC_adapted(c(mean(tempg[condition.conv,1]>1.2))),prop_ab_1p05=formatC_adapted(c(mean(tempg[condition.conv,1]>1.05))),prop_ab_1p01=formatC_adapted(c(mean(tempg[condition.conv,1]>1.01))))
        row.names(tempres_conv_print)<-c("Gelman_Point_Est")
      }

      if (control$convtype=="Geweke") {
        tempg<-abs(coda::geweke.diag(coda::as.mcmc.list(lapply(out,function(x){coda::as.mcmc(as.matrix(x))}))[[1]],frac1=control$convtype.Geweke[1],frac2=control$convtype.Geweke[2])$z)
        tempres_conv<-cbind.data.frame(max_gew=max(tempg),med_gew=median(tempg),mean_gew=mean(tempg),name_max_gew=names(tempg)[tempg==max(tempg)][1],prop_gew_above_p975=mean(tempg>qnorm(0.975)),prop_gew_above_p995=mean(tempg>qnorm(0.995)),prop_gew_above_p9995=mean(tempg>qnorm(0.9995)))
        tempres_conv_print<-cbind.data.frame(max=formatC_adapted(max(tempg[condition.conv])),median=formatC_adapted(median(tempg[condition.conv])),mean=formatC_adapted(mean(tempg[condition.conv])),name_max=names(tempg[condition.conv])[tempg[condition.conv]==max(tempg[condition.conv])][1],prop_ab_p975=formatC_adapted(mean(tempg[condition.conv]>qnorm(0.975))),prop_ab_p995=formatC_adapted(mean(tempg[condition.conv]>qnorm(0.995))),prop_ab_p9995=formatC_adapted(mean(tempg[condition.conv]>qnorm(0.9995))))
        row.names(tempres_conv_print)<-c("Geweke")
      }

      if (control$convtype=="Heidelberger") {
        tempg<-1-(coda::heidel.diag(coda::as.mcmc.list(lapply(out,function(x){coda::as.mcmc(as.matrix(x))}))[[1]],pvalue=control$convtype.alpha)[,"stest"])
        tempres_conv<-cbind.data.frame(max_gew=max(tempg),med_gew=median(tempg),mean_gew=mean(tempg),name_max_gew=names(tempg)[tempg==max(tempg)][1],prop_gew_above_p975=mean(tempg>qnorm(0.975)),prop_gew_above_p995=mean(tempg>qnorm(0.995)),prop_gew_above_p9995=mean(tempg>qnorm(0.9995)))
        tempres_conv_print<-cbind.data.frame(max=formatC_adapted(max(tempg[condition.conv])),median=formatC_adapted(median(tempg[condition.conv])),mean=formatC_adapted(mean(tempg[condition.conv])),name_max=names(tempg[condition.conv])[tempg[condition.conv]==max(tempg[condition.conv])][1],prop_ab_p975=formatC_adapted(mean(tempg[condition.conv]>qnorm(0.975))),prop_ab_p995=formatC_adapted(mean(tempg[condition.conv]>qnorm(0.995))),prop_ab_p9995=formatC_adapted(mean(tempg[condition.conv]>qnorm(0.9995))))
        row.names(tempres_conv_print)<-c("Heidelberger")
      }

      ### calculs squares_Ses_ratios
      #used matrix and specvial treatment of dimnames due to trasnge behaviour of summary.mcmc in case only one parameter
      tempsu<-summary(out)[[1]]
      if (is.null(dimnames(tempsu)[[1]])) {names.tempsu<-c("only.parameter")} else {names.tempsu<-dimnames(tempsu)[[1]]}
        tempsu<-matrix(tempsu,ncol=4)
        squared_SE_ratio<-((tempsu[,4])/(tempsu[,3]))^2
        names(squared_SE_ratio)<-names.tempsu

      #print(squared_SE_ratio)
      tempres_SEs<-cbind.data.frame(max_sqSEratio=max(squared_SE_ratio)[1],name_max_sqSEratio=names(squared_SE_ratio)[squared_SE_ratio==max(squared_SE_ratio)][1],med_sqSEratio=median(squared_SE_ratio)[1],prop_sqSEratio_above_2=mean(squared_SE_ratio>2),prop_sqSEratio_above_1p2=mean(squared_SE_ratio>1.2))

      if (control$neff.method=="Stan")
      {
        MonModelStan<- aperm(as.array(out,drop=FALSE),c(1,3,2))
        dimnames(MonModelStan)<-list(NULL,NULL,{toto<-dimnames(out[[1]])[[2]]; duplicates<-duplicated(toto); duplicates<-(1:length(duplicates))[duplicates]; if (length(duplicates)>0) {toto[duplicates]<-paste0(toto[duplicates],".",1:length(duplicates))};toto})
        tempm<-rstan::monitor(MonModelStan,print=control$innerprint,warmup=0)[,"n_eff"]
      }

      if (control$neff.method=="Coda")
      {
        tempm<-coda::effectiveSize(out)
      }


      tempres_neff<-cbind.data.frame(min_neff=min(tempm)[1],name_min_neff=names(tempm)[tempm==min(tempm)][1],med_neff=median(tempm),mean_neff=mean(tempm),prop_neff_below_1000=mean(tempm<1000),prop_neff_below_5000=mean(tempm<5000),prop_neff_below_10000=mean(tempm<10000))
      tempres_neff_print<-cbind.data.frame(min=formatC_adapted(min(tempm[condition.conv])[1]),median=formatC_adapted(median(tempm[condition.conv])),mean=formatC_adapted(mean(tempm[condition.conv])),name_min=names(tempm[condition.conv])[tempm[condition.conv]==min(tempm[condition.conv])][1],prop_bel_1000=formatC_adapted(mean(tempm[condition.conv]<1000)),prop_bel_5000=formatC_adapted(mean(tempm[condition.conv]<5000)),prop_bel_10000=formatC_adapted(mean(tempm[condition.conv]<10000)))
      row.names(tempres_neff_print)<-"Neff            "
      #tempressimple<-tempres[,c("maxgelm_Point_Est","medelm1","min_neff","med_neff")]
      if (print.diagnostics) {
        print("###################################################################################")
        print("Current state of diagnostics:"); print(tempres_params_print)
        print("###################################################################################")
        print(tempres_conv_print)
        print("###################################################################################")
        print(tempres_neff_print)
        print("###################################################################################")
      }
      #removed because too much unstructured: ,global_synth=cbind.data.frame(tempres_params,tempres_conv,tempres_neff,tempres_SEs)
      tempres<-list(params=tempres_params_print,conv_synth=tempres_conv_print,neff_synth=tempres_neff_print,conv=tempg,neff=tempm)

      tempres
    }	#END conveff_final function


    checking.convergence<-function(diags,convtype,convtype.Gelman,conv.max,conv.med,conv.mean)
    {
      converged <-TRUE
      #print(conv.max)
      #print(diags$max_gelm_Upper_C_I)
      if (convtype=="Gelman")
      {
        if (convtype.Gelman==1)
        {
          if (!is.null(conv.max)) {if (conv.max<diags$max_gelm_Point_Est) {converged<-FALSE}}
          if (!is.null(conv.mean)) {if (conv.mean<diags$mean_gelm_Point_Est) {converged<-FALSE}}
          if (!is.null(conv.med)) {if (conv.med<diags$med_gelm_Point_Est) {converged<-FALSE}}
        }
        if (convtype.Gelman==2)
        {
          if (!is.null(conv.max)) {if (conv.max<diags$max_gelm_Upper_C_I) {converged<-FALSE}}
          if (!is.null(conv.mean)) {if (conv.mean<diags$mean_gelm_Upper_C_I) {converged<-FALSE}}
          if (!is.null(conv.med)) {if (conv.med<diags$med_gelm_Upper_C_I) {converged<-FALSE}}
        }
      }

      if (convtype=="Gelman_new")
      {

        if (!is.null(conv.max)) {if (conv.max<diags$max_gelm_Point_Est) {converged<-FALSE}}
        if (!is.null(conv.mean)) {if (conv.mean<diags$mean_gelm_Point_Est) {converged<-FALSE}}
        if (!is.null(conv.med)) {if (conv.med<diags$med_gelm_Point_Est) {converged<-FALSE}}
      }

      if (convtype=="Geweke")
      {
        if (!is.null(conv.max)) {if (conv.max<diags$max_gew) {converged<-FALSE}}
        if (!is.null(conv.mean)) {if (conv.mean<diags$mean_gew) {converged<-FALSE}}
        if (!is.null(conv.med)) {if (conv.med<diags$med_gew) {converged<-FALSE}}
      }

      if (convtype=="Heidelberger")
      {
        if (!is.null(conv.max)) {if (conv.max<diags$max_gew) {converged<-FALSE}}
        if (!is.null(conv.mean)) {if (conv.mean<diags$mean_gew) {converged<-FALSE}}
        if (!is.null(conv.med)) {if (conv.med<diags$med_gew) {converged<-FALSE}}
      }
      converged
    }	#END checking.convergence function


    scale.available.convs<-function(diags,convtype,convtype.Gelman,conv.max,conv.med,conv.mean)
    {
      convergence <-0
      #print(conv.max)
      #print(diags$max_gelm_Upper_C_I)

      if (convtype=="Gelman")
      {
        if (convtype.Gelman==1)
        {
          if (!is.null(conv.max)) {if (conv.max<diags$max_gelm_Point_Est) {convergence<-max(convergence,(diags$max_gelm_Point_Est-1)/(conv.max-1))}}
          if (!is.null(conv.mean)) {if (conv.mean<diags$mean_gelm_Point_Est) {convergence<-max(convergence,(diags$mean_gelm_Point_Est-1)/(conv.mean-1))}}
          if (!is.null(conv.med)) {if (conv.med<diags$med_gelm_Point_Est) {convergence<-max(convergence,(diags$med_gelm_Point_Est-1)/(conv.med-1))}}
        }
        if (convtype.Gelman==2)
        {
          if (!is.null(conv.max)) {if (conv.max<diags$max_gelm_Upper_C_I) {convergence<-max(convergence,(diags$max_gelm_Upper_C_I-1)/(conv.max-1))}}
          if (!is.null(conv.mean)) {if (conv.mean<diags$mean_gelm_Upper_C_I) {convergence<-max(convergence,(diags$mean_gelm_Upper_C_I-1)/(conv.mean-1))}}
          if (!is.null(conv.med)) {if (conv.med<diags$med_gelm_Upper_C_I) {convergence<-max(convergence,(diags$med_gelm_Upper_C_I-1)/(conv.med-1))}}
        }
      }

      if (convtype=="Gelman_new")
      {

        if (!is.null(conv.max)) {if (conv.max<diags$max_gelm_Point_Est) {convergence<-max(convergence,(diags$max_gelm_Point_Est-1)/(conv.max-1))}}
        if (!is.null(conv.mean)) {if (conv.mean<diags$mean_gelm_Point_Est) {convergence<-max(convergence,(diags$mean_gelm_Point_Est-1)/(conv.mean-1))}}
        if (!is.null(conv.med)) {if (conv.med<diags$med_gelm_Point_Est) {convergence<-max(convergence,(diags$med_gelm_Point_Est-1)/(conv.med-1))}}
      }

      if (convtype=="Geweke")
      {
        if (!is.null(conv.max)) {if (conv.max<diags$max_gew) {convergence<-max(convergence,(diags$max_gew)/(conv.max))}}
        if (!is.null(conv.mean)) {if (conv.mean<diags$mean_gew) {convergence<-max(convergence,(diags$mean_gew)/(conv.mean))}}
        if (!is.null(conv.med)) {if (conv.med<diags$med_gew) {convergence<-max(convergence,(diags$med_gew)/(conv.med))}}
      }

      if (convtype=="Heidelberger")
      {
        if (!is.null(conv.max)) {if (conv.max<diags$max_gew) {convergence<-max(convergence,logit(diags$max_gew)/logit(conv.max))}}
        if (!is.null(conv.mean)) {if (conv.mean<diags$mean_gew) {convergence<-max(convergence,logit(diags$mean_gew)/logit(conv.mean))}}
        if (!is.null(conv.med)) {if (conv.med<diags$med_gew) {convergence<-max(convergence,logit(diags$med_gew)/logit(conv.med-1))}}
      }
      #added max(1,) because there MIGHT be a problem of sign for Heidelberger
      return(max(1,convergence))
    }


    ## function to check if criteria of neff are reached
    checking.neffs.reached<-function(diags,neff.min,neff.med,neff.mean)
    {
      neffs.reached<-TRUE
      if (!is.null(neff.min)) {if(diags$min_neff<neff.min) {neffs.reached<-FALSE} }
      if (!is.null(neff.med)) {if(diags$med_neff<neff.med) {neffs.reached<-FALSE}}
      if (!is.null(neff.mean)) {if(diags$mean_neff<neff.mean) {neffs.reached<-FALSE}}

      neffs.reached
    }

    ## function to check if neff is sufficiently conserved at attempts to reduce size in the end
    checking.neffs.conserved<-function(diags,diagsref,max.prop.decr.neff,neff.min,neff.med,neff.mean,neffs.reached)
    {
      neffs.conserved<-TRUE
      refratio<-Inf
      if (!is.null(neff.min)) {refratio=diagsref$min_neff/neff.min}
      if (!is.null(neff.med)) {if((diagsref$med_neff/neff.med)<refratio) {refratio=diagsref$med_neff/neff.med}}
      if (!is.null(neff.mean)) {if((diagsref$mean_neff/neff.mean)<refratio) {refratio=diagsref$mean_neff/neff.mean}}

      if (!is.null(neff.min)) {if(diags$min_neff<(1-max.prop.decr.neff)*refratio*neff.min) {neffs.conserved<-FALSE}}
      if (!is.null(neff.med)) {if(diags$med_neff<(1-max.prop.decr.neff)*refratio*neff.med) {neffs.conserved<-FALSE}}
      if (!is.null(neff.mean)) {if(diags$mean_neff<(1-max.prop.decr.neff)*refratio*neff.mean) {neffs.conserved<-FALSE}}

      #if (!is.null(neff.min)) {if(!neffs.reached & diags$min_neff<refratio*neff.min) {neffs.conserved<-FALSE}}
      #if (!is.null(neff.med)) {if(!neffs.reached & diags$med_neff<refratio*neff.med) {neffs.conserved<-FALSE}}
      #if (!is.null(neff.mean)) {if(!neffs.reached & diags$mean_neff<refratio*neff.mean) {neffs.conserved<-FALSE}}

      return(neffs.conserved)
    }

    ## function to calculate the multiplier of thin to do in the next step to render replicates nearly independent
    calculate.thinmult.target<-function(diags,adapt.thin,neff.min,neff.med,neff.mean)
    {
      thinmult<-1
      if(adapt.thin)
      {#neffmax<-max(c(neff.min,neff.med,neff.mean))
        N<-diags$Nvalues
        #added minimum in the following formulas to guarantee that the resulting thinned Markov Chains will have at least around 10 values per chain
        if (!is.null(neff.min)) {thinmult<-min(max(thinmult,N/diags$min_neff*neff.min/neff.max),N/Nchains/10) }
        if (!is.null(neff.med)) {thinmult<-min(max(thinmult,N/diags$med_neff*neff.med/neff.max),N/Nchains/10)}
        if (!is.null(neff.mean)) {thinmult<-min(max(thinmult,N/diags$mean_neff*neff.mean/neff.max),N/Nchains/10)}
      }

      return(thinmult)
    }

    ## function to calculate the new thin value: different formulas depending on the value of roundthinmult
    update.thin<-function(thin,thinmult,thin.max,roundthinmult)
    {if (roundthinmult)
    {newthin<-min(thin*thinmult,(thin.max%/%thin)*thin)

    } else {
      newthin<-min(thin*thinmult,thin.max)
    }
      return(newthin)
    }

    ##function to put all the neffs on the same scale and then taking the minimum
    scale.available.neffs<-function(diags,neff.min,neff.med,neff.mean)
    {
      #neffmax<-max(c(neff.min,neff.med,neff.mean))
      neffs<-min(c(diags$min_neff/neff.min*neff.max,diags$med_neff/neff.med*neff.max,diags$mean_neff/neff.mean*neff.max))
      neffs
    }

    ## function to "summarize" data (or if Null: ModelData) and constants (or if Null, ModelCosnts)
    summarize.data<-function(data)
    {
      names.data<-names(data)
      res<-as.list(rep(1,2*length(names.data)))
      names(res)<-paste0(rep(c("dims.","mean."),times=length(names.data)),rep(names.data,each=2))
      for (i in seq_len(length(names.data)))
      {if (length(dim(data[[i]]))>0)
      {
        res[[2*(i-1)+1]]<-dim(data[[i]])
      }
        else {
          res[[2*(i-1)+1]]<-length(data[[i]])

        }
        res[[2*(i-1)+2]]<-mean(data[[i]],na.rm=TRUE)
      }
      res
    }
    ########################################
    ###0-1. Initialisation & checkings
    ########################################

    neff.max<-max(neff.min,neff.med,neff.mean)

    ### variable that will contain the info on previous convergences
    previously.converged<-FALSE

    ### putting the control argument in the good format in case it is specified partially
    control0<-list(time.max=NULL,check.convergence=TRUE,check.convergence.firstrun=NULL,recheck.convergence=TRUE,convtype=NULL,
                   convtype.Gelman=2,convtype.Geweke=c(0.1,0.5),convtype.alpha=0.05,ip.nc=0,neff.method="Stan",Ncycles.target=2,props.conv=c(0.25,0.5,0.75),min.Nvalues=NULL,
                   min.thinmult=1.1, force.niter.max=FALSE, force.time.max=FALSE, time.max.turns.off.niter.max=FALSE, safemultiplier.Nvals=1.2,max.prop.decr.neff=0.1,round.thinmult=TRUE,thinmult.in.resetMV.temporary=TRUE,check.thinmult=2,decrease.thinmult.multiplier=0.8, decrease.thinmult.threshold=20,only.final.adapt.thin=FALSE,
                   identifier.to.print="",print.diagnostics=FALSE,conv.thorough.check=FALSE,print.thinmult=TRUE,innerprint=FALSE,seed=NULL,remove.fixedchains=TRUE,check.installation=TRUE,save.data=FALSE,conveff.final.allparams=TRUE)

    ### checking all the names of arguments of control are in control0; otherwise stops because arguments are not intrepretable
    if (length(setdiff(names(control),names(control0)))>0)
    {stop(paste0("The names of the control argument do not match the default names (unmatched names: ",paste(setdiff(names(control),names(control0)),collapse=", "),")"))}

    ### puts the components of control into control0:
    control0[names(control)]<-control

    ### INACTIVATED: putting in control0 parameters that could have been passed directly to the function, not in the control argument:
    #if (length(intersect(names(c(as.list(environment()), list(...))),names(control0)))>0) {control0[intersect(names(c(as.list(environment()), list(...))),names(control0))]<-c(as.list(environment()), list(...))[intersect(names(c(as.list(environment()), list(...))),names(control0))]}

    ### transferring control0 to control
    control<-control0


    ### putting the control.MCMC argument in the good format in case it is specified partially: same sequence as for control/control0:
    control.MCMC0<-list(confModel.expression.toadd=NULL,sampler=expression(hmc()),warmup=1000,n.adapt=-1,RNG.names=c("base::Wichmann-Hill", "base::Marsaglia-Multicarry", "base::Super-Duper","base::Mersenne-Twister"),n_cores=NULL,showCompilerOutput=FALSE,buildDerivs=FALSE,resetMV=FALSE,parallelize=FALSE,parallelizeInitExpr= expression(if(MCMC_language=="Nimble"){library("nimble");if(control.MCMC$APT) {library("nimbleAPT")}} else {NULL}),useConjugacy=FALSE,WAIC=FALSE,WAIC.Nsamples=2000,
                        WAIC.control=list(online=TRUE,dataGroups=NULL,marginalizeNodes=NULL,niterMarginal=1000,convergenceSet=c(0.25,0.5,0.75),thin=TRUE,nburnin_extra=0),
                        APT=FALSE, APT.NTemps=7, APT.initTemps=NULL,APT.tuneTemps=c(10,0.7),APT.thinPrintTemps=expression(niter/5),includeAllStochNodes=FALSE, monitorAllStochNodes=FALSE, saveAllStochNodes=FALSE, includeParentNodes=FALSE, monitorParentNodes=FALSE, saveParentNodes=FALSE, extraCalculations=NULL)
    if (length(setdiff(names(control.MCMC),names(control.MCMC0)))>0)
    {stop(paste0("The names of the control.MCMC argument do not match the default names (unmatched names: ",paste(setdiff(names(control.MCMC),names(control.MCMC0)),collapse=", "),")"))}
    if (length(intersect(names(control.MCMC),names(control.MCMC0)))<length(names(control.MCMC0)))
    {control.MCMC0[names(control.MCMC)]<-control.MCMC}
    ### INACTIVATED: putting in control.MCMC parameters that could have been passed directly to the function, not in the control.MCMC argument:
    #	if (length(intersect(names(c(as.list(environment()), list(...))),names(control.MCMC0)))>0) {control.MCMC0[intersect(names(c(as.list(environment()), list(...))),names(control.MCMC0))]<-c(as.list(environment()), list(...))[intersect(names(c(as.list(environment()), list(...))),names(control.MCMC0))]}
    control.MCMC<-control.MCMC0

    ########################################
    ###0-1.1 Checking control
    ########################################
    #checking and potentially reframing control$time.max
    if (length(control$time.max)>1) {print("control$time.max has more than one element. Reduced to its first element"); control$time.max<-control$time.max[[1]]}
    if (length(control$time.max)==0) {control$time.max=Inf}
    if (length(control$time.max)==1 & !(is.numeric(control$time.max))) {stop("control$time.max is not of numeric type.")}

    if (length(control$check.convergence)>1) {print("control$check.convergence has more than one element. Reduced to its first element"); control$check.convergence<-control$check.convergence[[1]]}
    if (length(control$check.convergence)==0) {stop("control$check.convergence has zero length. Not possible")}
    ## stop if control$check.convergence has one element and is not logical
    if (length(control$check.convergence)==1 & !is.logical(control$check.convergence)) {stop("control$check.convergence is not of logical type.")}

    if (length(control$check.convergence.firstrun)>1) {print("control$check.convergence.firstrun has more than one element. Reduced to its first element"); control$check.convergence.firstrun<-control$check.convergence.firstrun[[1]]}
    ## stop if control$check.convergence.firstrun has one element and is not logical
    if (length(control$check.convergence.firstrun)==1 & !is.logical(control$check.convergence.firstrun)) {stop("control$check.convergence.firstrun is not of logical type.")}

    if (length(control$recheck.convergence)>1) {print("control$recheck.convergence has more than one element. Reduced to its first element"); control$recheck.convergence<-control$recheck.convergence[[1]]}
    if (length(control$recheck.convergence)==0) {stop("control$recheck.convergence has zero length. Not possible")}
    ## stop if control$recheck.convergence has one element and is not logical
    if (length(control$recheck.convergence)==1 & !is.logical(control$recheck.convergence)) {stop("control$recheck.convergence is not of logical type.")}

    ## checking adequacy of control$convtype
    if (length(control$convtype)==0) {control$convtype<-ifelse(Nchains==1,"Geweke","Gelman")}

    ## checking adequacy of control$convtype
    if (length(control$convtype)>1) {print("control$convtype has more than one element. Reduced to its first element"); control$convtype<-control$convtype[[1]]}
    if (!is.character(control$convtype)) {stop("control$convtype is not of character type as it should be")}
    if (!is.element(control$convtype, c("Gelman","Gelman_new","Geweke","Heidelberger"))) {stop("Parameter convtype in control argument should be equal either to \"Gelman\", \"Gelman_new\", \"Geweke\" or \"Heidelberger\": please change this parameter")}

    ## checking adequacy between convtype and number of MCMC chains
    if ((control$convtype=="Gelman"|control$convtype=="Gelman_new")&Nchains==1) {stop("Impossible to use the Gelman-Rubin convergence diagnostic with only one MCMC chain: please change Nchain or control$convtype")}
    if ((control$convtype=="Geweke")&Nchains>1) {stop("Impossible to use the Geweke convergence diagnostic with more than one MCMC chain: please change Nchain or control$convtype")}
    if ((control$convtype=="Heidelberger")&Nchains>1) {stop("Impossible to use the Heidelberger convergence diagnostic with more than one MCMC chain: please change Nchain or control$convtype")}

    ## checking adequacy of control$neff.method
    if (length(control$neff.method)>1) {print("control$neff.method has more than one element. Reduced to its first element"); control$neff.method<-control$neff.method[[1]]}
    if (!is.character(control$neff.method)) {stop("control$neff.method is not of character type as it should be")}
    if (!is.element(control$neff.method, c("Stan","Coda"))) {stop("Parameter neff.method in control argument should be equal either to \"Stan\", or \"Coda\": please change this parameter")}

    if (length(control$check.installation)>1) {print("control$check.installation has more than one element. Reduced to its first element"); control$check.installation<-control$check.installation[[1]]}
    if (length(control$check.installation)==0) {stop("control$check.installation has zero length. Not possible")}
    ## stop if control$check.installation has one element and is not logical
    if (length(control$check.installation)==1 & !is.logical(control$check.installation)) {stop("control$check.installation is not of logical type.")}

    if (length(control$save.data)>1) {print("control$save.data has more than one element. Reduced to its first element"); control$save.data<-control$save.data[[1]]}
    if (length(control$save.data)==0) {stop("control$save.data has zero length. Not possible")}
    ## stop if control$save.data has one element and is not logical
    if (length(control$save.data)==1 & !is.logical(control$save.data)) {stop("control$save.data is not of logical type.")}

    if (length(control$conveff.final.allparams)>1) {print("control$conveff.final.allparams has more than one element. Reduced to its first element"); control$conveff.final.allparams<-control$conveff.final.allparams[[1]]}
    if (length(control$conveff.final.allparams)==0) {stop("control$conveff.final.allparams has zero length. Not possible")}
    ## stop if control$conveff.final.allparams has one element and is not logical
    if (length(control$conveff.final.allparams)==1 & !is.logical(control$conveff.final.allparams)) {stop("control$conveff.final.allparams is not of logical type.")}

    if (length(control$conv.thorough.check)>1) {print("control$conv.thorough.check has more than one element. Reduced to its first element"); control$conv.thorough.check<-control$conv.thorough.check[[1]]}
    if (length(control$conv.thorough.check)==0) {stop("control$conv.thorough.check has zero length. Not possible")}
    ## stop if control$conv.thorough.check has one element and is not logical
    if (length(control$conv.thorough.check)==1 & !is.logical(control$conv.thorough.check)) {stop("control$conv.thorough.check is not of logical type.")}


    if (length(control$round.thinmult)>1) {print("control$round.thinmult has more than one element. Reduced to its first element"); control$round.thinmult<-control$round.thinmult[[1]]}
    if (length(control$round.thinmult)==0) {stop("control$round.thinmult has zero length. Not possible")}
    ## stop if control$round.thinmult has one element and is not logical
    if (length(control$round.thinmult)==1 & !is.logical(control$round.thinmult)) {stop("control$round.thinmult is not of logical type.")}

    if (length(control$check.thinmult)>1) {print("control$check.thinmult has more than one element. Reduced to its first element"); control$check.thinmult<-control$check.thinmult[[1]]}
    if (length(control$check.thinmult)==0) {stop("control$check.thinmult has zero length. Not possible")}
    ## stop if control$check.thinmult has one element and is not logical
    if (length(control$check.thinmult)==1 & !is.numeric(control$check.thinmult)) {stop("control$check.thinmult is not of numeric type.")}
    if (length(control$check.thinmult)==1 & !is.element(control$check.thinmult,c(1,2,3))) {stop("control$check.thinmult should take its value among 1, 2 or 3.")}


    if (length(control$decrease.thinmult.multiplier)>1) {print("control$decrease.thinmult.multiplier has more than one element. Reduced to its first element"); control$decrease.thinmult.multiplier<-control$decrease.thinmult.multiplier[[1]]}
    if (length(control$decrease.thinmult.multiplier)==0) {stop("control$decrease.thinmult.multiplier has zero length. Not possible")}
    if (length(control$decrease.thinmult.multiplier)==1 & !is.numeric(control$decrease.thinmult.multiplier)) {stop("control$decrease.thinmult.multiplier is not of numeric type.")}
    if (length(control$decrease.thinmult.multiplier)==1 & (control$decrease.thinmult.multiplier>=1)) {stop("control$decrease.thinmult.multiplier should take its value below 1.")}
    if (length(control$decrease.thinmult.multiplier)==1 & (control$decrease.thinmult.multiplier<=0)) {stop("control$decrease.thinmult.multiplier should take its value above 0.")}

    if (length(control$decrease.thinmult.threshold)>1) {print("control$decrease.thinmult.threshold has more than one element. Reduced to its first element"); control$decrease.thinmult.threshold<-control$decrease.thinmult.threshold[[1]]}
    if (length(control$decrease.thinmult.threshold)==0) {stop("control$decrease.thinmult.threshold has zero length. Not possible")}
    if (length(control$decrease.thinmult.threshold)==1 & !is.numeric(control$decrease.thinmult.threshold)) {stop("control$decrease.thinmult.threshold is not of numeric type.")}
    if (length(control$decrease.thinmult.threshold)==1 & (control$decrease.thinmult.threshold<=3)) {stop("control$decrease.thinmult.threshold should take its value above 3.")}
    #to ensure the value is integer, we round it:
    control$decrease.thinmult.threshold<-round(control$decrease.thinmult.threshold)

    if (length(control$thinmult.in.resetMV.temporary)>1) {print("control$thinmult.in.resetMV.temporary has more than one element. Reduced to its first element"); control$thinmult.in.resetMV.temporary<-control$thinmult.in.resetMV.temporary[[1]]}
    if (length(control$thinmult.in.resetMV.temporary)==0) {stop("control$thinmult.in.resetMV.temporary has zero length. Not possible")}
    ## stop if control$thinmult.in.resetMV.temporary has one element and is not logical
    if (length(control$thinmult.in.resetMV.temporary)==1 & !is.logical(control$thinmult.in.resetMV.temporary)) {stop("control$thinmult.in.resetMV.temporary is not of logical type.")}

    if (length(control$only.final.adapt.thin)>1) {print("control$only.final.adapt.thin has more than one element. Reduced to its first element"); control$only.final.adapt.thin<-control$only.final.adapt.thin[[1]]}
    if (length(control$only.final.adapt.thin)==0) {stop("control$only.final.adapt.thin has zero length. Not possible")}
    ## stop if control$only.final.adapt.thin has one element and is not logical
    if (length(control$only.final.adapt.thin)==1 & !is.logical(control$only.final.adapt.thin)) {stop("control$only.final.adapt.thin is not of logical type.")}

    ## checking adequacy of print.diagnostics
    if (length(control$print.diagnostics)>1) {print("control$print.diagnostics has more than one element. Reduced to its first element"); control$print.diagnostics<-control$print.diagnostics[[1]]}
    if (length(control$print.diagnostics)==0) {stop("control$print.diagnostics has zero length. Not possible")}
    ## stop if control$print.diagnostics has one element and is not logical
    if (length(control$print.diagnostics)==1 & !is.logical(control$print.diagnostics)) {stop("control$print.diagnostics is not of logical type.")}

    ## checking adequacy of print.thinmult
    if (length(control$print.thinmult)>1) {print("control$print.thinmult has more than one element. Reduced to its first element"); control$print.thinmult<-control$print.thinmult[[1]]}
    if (length(control$print.thinmult)==0) {stop("control$print.thinmult has zero length. Not possible")}
    ## stop if control$print.thinmult has one element and is not logical
    if (length(control$print.thinmult)==1 & !is.logical(control$print.thinmult)) {stop("control$print.thinmult is not of logical type.")}

    ## checking adequacy of control$force.niter.max
    if (length(control$force.niter.max)>1) {print("control$force.niter.max has more than one element. Reduced to its first element"); control$force.niter.max<-control$force.niter.max[[1]]}
    if (length(control$force.niter.max)==0) {stop("control$force.niter.max has zero length. Not possible")}
    ## stop if control$force.niter.max has one element and is not logical
    if (length(control$force.niter.max)==1 & !is.logical(control$force.niter.max)) {stop("control$force.niter.max is not of logical type.")}
    if (control$force.niter.max & !is.finite(niter.max)) {control$force.niter.max<-FALSE; print("control$force.niter.max turned to FALSE because niter.max is not finite")}

    ## checking adequacy of control$force.time.max
    if (length(control$force.time.max)>1) {print("control$force.time.max has more than one element. Reduced to its first element"); control$force.time.max<-control$force.time.max[[1]]}
    if (length(control$force.time.max)==0) {stop("control$force.time.max has zero length. Not possible")}
    ## stop if control$force.time.max has one element and is not logical
    if (length(control$force.time.max)==1 & !is.logical(control$force.time.max)) {stop("control$force.time.max is not of logical type.")}
    if (control$force.time.max & !is.finite(control$time.max)) {control$force.time.max<-FALSE; print("control$force.time.max turned to FALSE because time.max is not finite")}

    ## checking adequacy of control$time.max.turns.off.niter.max
    if (length(control$time.max.turns.off.niter.max)>1) {print("control$time.max.turns.off.niter.max has more than one element. Reduced to its first element"); control$time.max.turns.off.niter.max<-control$time.max.turns.off.niter.max[[1]]}
    if (length(control$time.max.turns.off.niter.max)==0) {stop("control$time.max.turns.off.niter.max has zero length. Not possible")}
    ## stop if control$time.max.turns.off.niter.max has one element and is not logical
    if (length(control$time.max.turns.off.niter.max)==1 & !is.logical(control$time.max.turns.off.niter.max)) {stop("control$time.max.turns.off.niter.max is not of logical type.")}
    if (control$time.max.turns.off.niter.max & !is.finite(control$time.max)) {control$time.max.turns.off.niter.max<-FALSE; print("control$time.max.turns.off.niter.max turned to FALSE because control$time.max is not finite")}

    ## checking adequacy of innerprint
    if (length(control$innerprint)>1) {print("control$innerprint has more than one element. Reduced to its first element"); control$innerprint<-control$innerprint[[1]]}
    if (length(control$innerprint)==0) {stop("control$innerprint has zero length. Not possible")}
    ## stop if control$innerprint has one element and is not logical
    if (length(control$innerprint)==1 & !is.logical(control$innerprint)) {stop("control$innerprint is not of logical type.")}

    ## checking adequacy of remove.fixedchains
    if (length(control$remove.fixedchains)>1) {print("control$remove.fixedchains has more than one element. Reduced to its first element"); control$remove.fixedchains<-control$remove.fixedchains[[1]]}
    if (length(control$remove.fixedchains)==0) {stop("control$remove.fixedchains has zero length. Not possible")}
    ## stop if control$remove.fixedchains has one element and is not logical
    if (length(control$remove.fixedchains)==1 & !is.logical(control$remove.fixedchains)) {stop("control$remove.fixedchains is not of logical type.")}


    if (length(control$convtype.Gelman)>1) {print("control$convtype.Gelman has more than one element. Reduced to its first element"); control$convtype.Gelman<-control$convtype.Gelman[[1]]}
    if (length(control$convtype.Gelman)==0) {stop("control$convtype.Gelman has zero length. Not possible")}
    if (length(control$convtype.Gelman)==1 & !is.element(control$convtype.Gelman,c(1,2))) {stop("control$convtype.Gelman is not either 1 or 2.")}


    if (length(control$convtype.Geweke)>2) {print("control$convtype.Geweke has more than two elements. Reduced to its first two elements"); control$convtype.Geweke<-control$convtype.Geweke[[1:2]]}
    if (length(control$convtype.Geweke)<=1) {stop("control$convtype.Geweke has zero or one length. Not possible")}
    if (length(control$convtype.Geweke)==2 & mode(control$convtype.Geweke)!="numeric") {print(control$convtype.Geweke); stop("control$convtype.Geweke is not of mode double.")}
    control$convtype.Geweke<-sort(control$convtype.Geweke)
    if (length(control$convtype.Geweke)==2 & min(control$convtype.Geweke)<=0 & max(control$convtype.Geweke)>=1) {stop("control$convtype.Geweke does not have components strictly between 0 and 1.")}
    if (length(control$convtype.Geweke)==2 & sum(control$convtype.Geweke)>=1) {stop("control$convtype.Geweke should have a sum strictly less than 1.")}


    ## checking adequacy of convtype.alpha
    if (length(control$convtype.alpha)>1) {print("control$convtype.alpha should be of length 1; first element kept");control$convtype.alpha<-control$convtype.alpha[1]}
    if (length(control$convtype.alpha)==0) {stop("control$convtype.alpha is void; not possible")}
    if ((control$convtype.alpha)<=0|(control$convtype.alpha)>=1) {stop("control$convtype.alpha should be between 0 and 1")}

    ## checking adequacy of ip.nc
    if (length(control$ip.nc)>1) {print("control$ip.nc should be of length 1; first element kept");control$ip.nc<-control$ip.nc[1]}
    if (length(control$ip.nc)==0) {stop("control$ip.nc is void; not possible")}

    ## checking adequacy of Ncycles.target
    if (length(control$Ncycles.target)>1) {print("control$Ncycles.target has more than one elements. Reduced to its first element"); control$Ncycles.target<-control$Ncycles.target[[1]]}
    if (length(control$Ncycles.target)<1) {stop("control$Ncycles.target has zero length. Not possible")}
    if (length(control$Ncycles.target)==1 & mode(control$Ncycles.target)!="numeric") {stop("control$Ncycles.target is not of mode double.")}
    control$Ncycles.target=round(control$Ncycles.target)
    if (length(control$Ncycles.target)==1 & (control$Ncycles.target)<1) {stop("control$Ncycles.target should be greater than 1.")}

    ## checking adequacy of min.Nvalues
    if (is.expression(control$min.Nvalues)) {control$min.Nvalues<-eval(control$min.Nvalues)}
    if (length(control$min.Nvalues)>1) {print("control$min.Nvalues has more than one elements. Reduced to its first element"); control$min.Nvalues<-control$min.Nvalues[[1]]}
    if (length(control$min.Nvalues)<1) { control$min.Nvalues<-neff.max}
    if (length(control$min.Nvalues)>1) {print("control$min.Nvalues has more than one elements. Reduced to its first element"); control$min.Nvalues<-control$min.Nvalues[[1]]}
    if (length(control$min.Nvalues)==1 & mode(control$min.Nvalues)!="numeric") {stop("control$min.Nvalues is not of mode double.")}
    control$min.Nvalues=round(control$min.Nvalues)
    if (length(control$min.Nvalues)==1 & (control$min.Nvalues)<1) {stop("control$min.Nvalues should be greater than 1.")}
    min.Nvalues<-control$min.Nvalues

    ## checking adequacy of props.conv; other checks done in 0.1.4
    if (length(control$props.conv)<1) {stop("control$props.conv has zero length. Not possible")}
    if (length(control$props.conv)==1 & mode(control$props.conv)!="numeric") {stop("control$props.conv is not of mode double.")}
    ## checking that control$props.conv are >=0 (if ==0 will double the first diagnostic) and <=1:
    if (min(control$props.conv)<0) {stop("props.conv values in control should all be nonnegative")}
    if (max(control$props.conv)>1) {stop("props.conv values in control should all be less than 1")}
    ## if control$props.conv does not contain 1: add 1 because the algorithm requires it to stop correctly
    if (max(control$props.conv)<1) {control$props.conv<-c(control$props.conv,1)}

    ## checking adequacy of min.thinmult
    if (length(control$min.thinmult)>1) {print("control$min.thinmult has more than one elements. Reduced to its first element"); control$min.thinmult<-control$min.thinmult[[1]]}
    if (length(control$min.thinmult)<1) {stop("control$min.thinmult has zero length. Not possible")}
    if (length(control$min.thinmult)==1 & mode(control$min.thinmult)!="numeric") {stop("control$min.thinmult is not of mode double.")}
    if (length(control$min.thinmult)==1 & (control$min.thinmult)<1) {stop("control$min.thinmult should be greater than 1.")}

    ## checking adequacy of safemultiplier.Nvals
    if (length(control$safemultiplier.Nvals)>1)
    {
      print("safemultiplier.Nvals in control of length >1 although should be of length 1; Only first value kept")
      control$safemultiplier.Nvals<-control$safemultiplier.Nvals[1]
    }
    if (length(control$safemultiplier.Nvals)<1) {stop("control$safemultiplier.Nvals has zero length. Not possible")}
    if (!is.numeric(control$safemultiplier.Nvals)) {stop("safemultiplier.Nvals in control is not numerical although it should be")}
    if (control$safemultiplier.Nvals<1) {stop("safemultiplier.Nvals in control is less than 1.0 although it should not be")}

    ## checking adequacy of max.prop.decr.neff
    if (length(control$max.prop.decr.neff)>1)
    {
      print("max.prop.decr.neff in control of length >1 although should be of length 1; Only first value kept")
      control$max.prop.decr.neff<-control$max.prop.decr.neff[1]
    }
    if (!is.numeric(control$max.prop.decr.neff)) {stop("max.prop.decr.neff in control is not numerical although it should be")}
    if (control$max.prop.decr.neff>1) {stop("max.prop.decr.neff in control is greater than 1.0 although it should not be")}
    if (control$max.prop.decr.neff<0) {stop("max.prop.decr.neff in control is smaller than 0.0 although it should not be")}

    ## checking adequacy of seed
    if (length(control$seed)>1) {print("control$seed has more than one elements. Reduced to its first element"); control$seed<-control$seed[[1]]}
    if (length(control$seed)==1 & mode(control$seed)!="numeric") {stop("control$seed is not of mode double.")}
    if (length(control$seed)==0) {print("control$seed is NULL. Replaced by 1"); control$seed<-1}

    ########################################
    ###0-1.2 Checking control.MCMC
    ########################################

    if (length(control.MCMC$showCompilerOutput)>1) {print("control.MCMC$showCompilerOutput has more than one element. Reduced to its first element"); control.MCMC$showCompilerOutput<-control.MCMC$showCompilerOutput[[1]]}
    if (length(control.MCMC$showCompilerOutput)==0) {stop("control.MCMC$showCompilerOutput has zero length. Not possible")}
    ## stop if control.MCMC$showCompilerOutput has one element and is not logical
    if (length(control.MCMC$showCompilerOutput)==1 & !is.logical(control.MCMC$showCompilerOutput)) {stop("control.MCMC$showCompilerOutput is not of logical type.")}

    if (length(control.MCMC$buildDerivs)>1) {print("control.MCMC$buildDerivs has more than one element. Reduced to its first element"); control.MCMC$buildDerivs<-control.MCMC$buildDerivs[[1]]}
    if (length(control.MCMC$buildDerivs)==0) {stop("control.MCMC$buildDerivs has zero length. Not possible")}
    ## stop if control.MCMC$buildDerivs has one element and is not logical
    if (length(control.MCMC$buildDerivs)==1 & !is.logical(control.MCMC$buildDerivs)) {stop("control.MCMC$buildDerivs is not of logical type.")}

    if (length(control.MCMC$resetMV)>1) {print("control.MCMC$resetMV has more than one element. Reduced to its first element"); control.MCMC$resetMV<-control.MCMC$resetMV[[1]]}
    if (length(control.MCMC$resetMV)==0) {stop("control.MCMC$resetMV has zero length. Not possible")}
    ## stop if control.MCMC$resetMV has one element and is not logical
    if (length(control.MCMC$resetMV)==1 & !is.logical(control.MCMC$resetMV)) {stop("control.MCMC$resetMV is not of logical type.")}

    if (length(control.MCMC$parallelize)>1) {print("control.MCMC$parallelize has more than one element. Reduced to its first element"); control.MCMC$parallelize<-control.MCMC$parallelize[[1]]}
    if (length(control.MCMC$parallelize)==0) {stop("control.MCMC$parallelize has zero length. Not possible")}
    ## stop if control.MCMC$parallelize has one element and is not logical
    if (length(control.MCMC$parallelize)==1 & !is.logical(control.MCMC$parallelize)) {stop("control.MCMC$parallelize is not of logical type.")}

    if (length(control.MCMC$parallelizeInitExpr)>1) {print("control.MCMC$parallelizeInitExpr has more than one element. Reduced to its first element"); control.MCMC$parallelizeInitExpr<-control.MCMC$parallelizeInitExpr[[1]]}
    if (length(control.MCMC$parallelizeInitExpr)==1 & !is.expression(control.MCMC$parallelizeInitExpr)) {stop("control.MCMC$parallelizeInitExpr is not of expression type.")}

    if (length(control.MCMC$useConjugacy)>1) {print("control.MCMC$useConjugacy has more than one element. Reduced to its first element"); control.MCMC$useConjugacy<-control.MCMC$useConjugacy[[1]]}
    if (length(control.MCMC$useConjugacy)==0) {stop("control.MCMC$useConjugacy has zero length. Not possible")}
    ## stop if control.MCMC$useConjugacy has one element and is not logical
    if (length(control.MCMC$useConjugacy)==1 & !is.logical(control.MCMC$useConjugacy)) {stop("control.MCMC$useConjugacy is not of logical type.")}

    if (length(control.MCMC$WAIC)>1) {print("control.MCMC$WAIC has more than one element. Reduced to its first element"); control.MCMC$WAIC<-control.MCMC$WAIC[[1]]}
    if (length(control.MCMC$WAIC)==0) {stop("control.MCMC$WAIC has zero length. Not possible")}
    ## stop if control.MCMC$WAIC has one element and is not logical
    if (length(control.MCMC$WAIC)==1 & !is.logical(control.MCMC$WAIC)) {stop("control.MCMC$WAIC is not of logical type.")}

    if (length(control.MCMC$APT)>1) {print("control.MCMC$APT has more than one element. Reduced to its first element"); control.MCMC$APT<-control.MCMC$APT[[1]]}
    if (length(control.MCMC$APT)==0) {stop("control.MCMC$APT has zero length. Not possible")}
    ## stop if control.MCMC$APT has one element and is not logical
    if (length(control.MCMC$APT)==1 & !is.logical(control.MCMC$APT)) {stop("control.MCMC$APT is not of logical type.")}


    if (length(control.MCMC$includeAllStochNodes)>1) {print("control.MCMC$includeAllStochNodes has more than one element. Reduced to its first element"); control.MCMC$includeAllStochNodes<-control.MCMC$includeAllStochNodes[[1]]}
    if (length(control.MCMC$includeAllStochNodes)==0) {stop("control.MCMC$includeAllStochNodes has zero length. Not possible")}
    ## stop if control.MCMC$includeAllStochNodes has one element and is not logical
    if (length(control.MCMC$includeAllStochNodes)==1 & !is.logical(control.MCMC$includeAllStochNodes)) {stop("control.MCMC$includeAllStochNodes is not of logical type.")}

    if (length(control.MCMC$monitorAllStochNodes)>1) {print("control.MCMC$monitorAllStochNodes has more than one element. Reduced to its first element"); control.MCMC$monitorAllStochNodes<-control.MCMC$monitorAllStochNodes[[1]]}
    if (length(control.MCMC$monitorAllStochNodes)==0) {stop("control.MCMC$monitorAllStochNodes has zero length. Not possible")}
    ## stop if control.MCMC$monitorAllStochNodes has one element and is not logical
    if (length(control.MCMC$monitorAllStochNodes)==1 & !is.logical(control.MCMC$monitorAllStochNodes)) {stop("control.MCMC$monitorAllStochNodes is not of logical type.")}

    if (length(control.MCMC$saveAllStochNodes)>1) {print("control.MCMC$saveAllStochNodes has more than one element. Reduced to its first element"); control.MCMC$saveAllStochNodes<-control.MCMC$saveAllStochNodes[[1]]}
    if (length(control.MCMC$saveAllStochNodes)==0) {stop("control.MCMC$saveAllStochNodes has zero length. Not possible")}
    ## stop if control.MCMC$saveAllStochNodes has one element and is not logical
    if (length(control.MCMC$saveAllStochNodes)==1 & !is.logical(control.MCMC$saveAllStochNodes)) {stop("control.MCMC$saveAllStochNodes is not of logical type.")}

    if (length(control.MCMC$includeParentNodes)>1) {print("control.MCMC$includeParentNodes has more than one element. Reduced to its first element"); control.MCMC$includeParentNodes<-control.MCMC$includeParentNodes[[1]]}
    if (length(control.MCMC$includeParentNodes)==0) {stop("control.MCMC$includeParentNodes has zero length. Not possible")}
    ## stop if control.MCMC$includeParentNodes has one element and is not logical
    if (length(control.MCMC$includeParentNodes)==1 & !is.logical(control.MCMC$includeParentNodes)) {stop("control.MCMC$includeParentNodes is not of logical type.")}

    if (length(control.MCMC$monitorParentNodes)>1) {print("control.MCMC$monitorParentNodes has more than one element. Reduced to its first element"); control.MCMC$monitorParentNodes<-control.MCMC$monitorParentNodes[[1]]}
    if (length(control.MCMC$monitorParentNodes)==0) {stop("control.MCMC$monitorParentNodes has zero length. Not possible")}
    ## stop if control.MCMC$monitorParentNodes has one element and is not logical
    if (length(control.MCMC$monitorParentNodes)==1 & !is.logical(control.MCMC$monitorParentNodes)) {stop("control.MCMC$monitorParentNodes is not of logical type.")}

    if (length(control.MCMC$saveParentNodes)>1) {print("control.MCMC$saveParentNodes has more than one element. Reduced to its first element"); control.MCMC$saveParentNodes<-control.MCMC$saveParentNodes[[1]]}
    if (length(control.MCMC$saveParentNodes)==0) {stop("control.MCMC$saveParentNodes has zero length. Not possible")}
    ## stop if control.MCMC$saveParentNodes has one element and is not logical
    if (length(control.MCMC$saveParentNodes)==1 & !is.logical(control.MCMC$saveParentNodes)) {stop("control.MCMC$saveParentNodes is not of logical type.")}

    if (length(control.MCMC$WAIC.Nsamples)>1) {print("control.MCMC$WAIC.Nsamples has more than one element. Reduced to its first element"); control.MCMC$WAIC.Nsamples<-control.MCMC$WAIC.Nsamples[[1]]}
    if (length(control.MCMC$WAIC.Nsamples)==0) {stop("control.MCMC$WAIC.Nsamples has zero length. Not possible")}
    ## stop if control.MCMC$WAIC.Nsamples has one element and is not logical
    if (length(control.MCMC$WAIC.Nsamples)==1 & (!is.integer(as.integer(control.MCMC$WAIC.Nsamples))|(is.na(as.integer(control.MCMC$WAIC.Nsamples))))) {stop("control.MCMC$WAIC.Nsamples is not of integer type.")}

    if (length(control.MCMC$APT.NTemps)>1) {print("control.MCMC$APT.NTemps has more than one element. Reduced to its first element"); control.MCMC$APT.NTemps<-control.MCMC$APT.NTemps[[1]]}
    if (length(control.MCMC$APT.NTemps)==0) {stop("control.MCMC$APT.NTemps has zero length. Not possible")}
    ## stop if control.MCMC$APT.NTemps has one element and is not logical
    if (length(control.MCMC$APT.NTemps)==1 & (!is.integer(as.integer(control.MCMC$APT.NTemps))|(is.na(as.integer(control.MCMC$APT.NTemps))))) {stop("control.MCMC$APT.NTemps is not of integer type.")}

    if (length(control.MCMC$APT.initTemps)>=1 & length(control.MCMC$APT.initTemps)!=control.MCMC$APT.NTemps) {stop(paste("if supplied, control.MCMC$APT.initTemps should have ",control.MCMC$APT.NTemps," elements",sep=""))}
    if (length(control.MCMC$APT.initTemps)>=1)
    {
      if(mode(control.MCMC$APT.initTemps)!="numeric") {stop("if supplied, control.MCMC$APT.initTemps should be of numeric mode")}
      if (min(control.MCMC$APT.initTemps)<1) {stop("if supplied, control.MCMC$APT.initTemps should be at least 1")}
      if (!is.finite(max(control.MCMC$APT.initTemps))) {stop("if supplied, control.MCMC$APT.initTemps should have all its elements that are finite")}
    }

    if (length(control.MCMC$APT.tuneTemps)!=2) {stop(paste("control.MCMC$APT.tuneTemps should be of length 2",sep=""))}
    if (mode(control.MCMC$APT.tuneTemps)!="numeric") {stop("if supplied, control.MCMC$APT.tuneTemps should be of numeric mode")}
    if ( min(control.MCMC$APT.tuneTemps)<0) {stop("if supplied, control.MCMC$APT.tuneTemps should be at least 0")}
    if (!is.finite(max(control.MCMC$APT.tuneTemps))) {stop("if supplied, control.MCMC$APT.tuneTemps should have all its elements that are finite")}


    if (length(control.MCMC$APT.thinPrintTemps)>1) {print("control.MCMC$APT.thinPrintTemps has more than one element. Reduced to its first element"); control.MCMC$APT.thinPrintTemps<-control.MCMC$APT.thinPrintTemps[[1]]}
    if (length(control.MCMC$APT.thinPrintTemps)==1 & (!is.expression(control.MCMC$APT.thinPrintTemps)&!is.numeric(control.MCMC$APT.thinPrintTemps))) {stop("control.MCMC$APT.thinPrintTemps is not of expression type or numeric.")}
    if (length(control.MCMC$APT.thinPrintTemps)==0) {control.MCMC$APT.thinPrintTemps<-NULL}


    if (length(control.MCMC$warmup)>1) {print("control.MCMC$warmup has more than one element. Reduced to its first element"); control.MCMC$warmup<-control.MCMC$warmup[[1]]}
    if (length(control.MCMC$warmup)==0) {stop("control.MCMC$warmup has zero length. Not possible")}
    ## stop if control.MCMC$warmup has one element and is not logical
    if (length(control.MCMC$warmup)==1 & (!is.integer(as.integer(control.MCMC$warmup))|(is.na(as.integer(control.MCMC$warmup))))) {stop("control.MCMC$warmup is not of integer type.")}

    if (length(control.MCMC$n.adapt)>1) {print("control.MCMC$n.adapt has more than one element. Reduced to its first element"); control.MCMC$n.adapt<-control.MCMC$n.adapt[[1]]}
    if (length(control.MCMC$n.adapt)==0) {stop("control.MCMC$n.adapt has zero length. Not possible")}
    ## stop if control.MCMC$n.adapt has one element and is not logical
    if (length(control.MCMC$n.adapt)==1 & (!is.integer(as.integer(control.MCMC$n.adapt))|(is.na(as.integer(control.MCMC$n.adapt)))))  {stop("control.MCMC$n.adapt is not of integer type.")}

    if (length(control.MCMC$n_cores)>1) {print("control.MCMC$n_cores has more than one element. Reduced to its first element"); control.MCMC$n_cores<-control.MCMC$n_cores[[1]]}
    ## stop if control.MCMC$n_cores has one element and is not logical
    if (length(control.MCMC$n_cores)==1) {if (!is.integer(as.integer(control.MCMC$n_cores))|(is.na(as.integer(control.MCMC$n_cores)))) {stop("control.MCMC$n_cores is not of integer type.")}}

    if (length(control.MCMC$confModel.expression.toadd)>1) {print("control.MCMC$confModel.expression.toadd has more than one element. Reduced to its first element"); control.MCMC$confModel.expression.toadd<-control.MCMC$confModel.expression.toadd[[1]]}
    if (length(control.MCMC$confModel.expression.toadd)==1 & !is.expression(control.MCMC$confModel.expression.toadd)) {stop("control.MCMC$confModel.expression.toadd is not of expression type.")}

    if (length(control.MCMC$extraCalculations)>1) {print("control.MCMC$extraCalculations has more than one element. Reduced to its first element"); control.MCMC$extraCalculations<-control.MCMC$extraCalculations[[1]]}
    if (length(control.MCMC$extraCalculations)==1 & !is.expression(control.MCMC$extraCalculations)) {stop("control.MCMC$extraCalculations is not of expression type.")}

    if (length(control.MCMC$sampler)>1) {print("control.MCMC$sampler has more than one element. Reduced to its first element"); control.MCMC$sampler<-control.MCMC$sampler[[1]]}
    if (length(control.MCMC$sampler)==0) {stop("control.MCMC$sampler is void which should not be the case.")}
    if (length(control.MCMC$sampler)==1 & !is.expression(control.MCMC$sampler)) {stop("control.MCMC$sampler is not of expression type.")}

    if (length(control.MCMC$RNG.names)==0) {stop("control.MCMC$RNG.names is void which should not be the case.")}
    if (mode(control.MCMC$RNG.names)!="character") {stop("control.MCMC$RNG.names is not of character type.")}

    ## checking control.MCMC$WAIC.control and putting it in the right formaty: lengtht(control.MCMC$WAIC.control) is the number of WAIC methods to test
    if (length((control.MCMC$WAIC.control))==0 & control.MCMC$WAIC) {print("Since control.MCMC$WAIC.control is void, control.MCMC$WAIC turned to FALSE"); control.MCMC$WAIC<-FALSE}
    if (is.null(names(control.MCMC$WAIC.control))) {control.MCMC$WAIC.control<-lapply(control.MCMC$WAIC.control,function(x){temp<-min(is.element(names(x),c("online", "dataGroups","marginalizeNodes","niterMarginal","convergenceSet","thin","nburnin_extra"))); if (temp!=1) {stop("components of control.MCMC$WAIC.control should have names among \"online\", \"dataGroups\",\"marginalizeNodes\",\"niterMarginal\",\"convergenceSet\",\"thin\",\"nburnin_extra\" which is not the case.")};x$online=TRUE;x$thin=TRUE;x$nburnin_extra=0;x})} else {temp<-min(is.element(names(control.MCMC$WAIC.control),c("online", "dataGroups","marginalizeNodes","niterMarginal","convergenceSet","thin","nburnin_extra"))); if (temp!=1) {print(names(control.MCMC$WAIC.control)); print(temp);stop("components of control.MCMC$WAIC.control shpuld have names among \"online\", \"dataGroups\",\"marginalizeNodes\",\"niterMarginal\",\"convergenceSet\",\"thin\",\"nburnin_extra\" which is not the case.")}; control.MCMC$WAIC.control$online=TRUE;control.MCMC$WAIC.control$thin=TRUE;control.MCMC$WAIC.control$nburnin_extra=0; control.MCMC$WAIC.control<-list(control.MCMC$WAIC.control)}
    if (length((control.MCMC$WAIC.control))>Nchains) {print("control.MCMC$WAIC.control has more than Nchains=",Nchains," elements; truncated to the first Nchains elements"); control.MCMC$WAIC.control<-control.MCMC$WAIC.control[1:Nchains] }
    lapply(control.MCMC$WAIC.control,function(x){
      ## checking adequacy of online component
      if (length(x$online)>1) {print("x$online has more than one element. Reduced to its first element"); x$online<-x$online[[1]]}
      if (length(x$online)==0) {stop("x$online has zero length. Not possible")}
      if (length(x$online)==1 & !is.logical(x$online)) {stop("x$online is not of logical type.")}

      ## checking adequacy of thin component
      if (length(x$thin)>1) {print("x$thin has more than one element. Reduced to its first element"); x$thin<-x$thin[[1]]}
      if (length(x$thin)==0) {stop("x$thin has zero length. Not possible")}
      if (length(x$thin)==1 & !is.logical(x$thin)) {stop("x$thin is not of logical type.")}

      ## checking adequacy of niterMarginal component
      if (length(x$niterMarginal)>1) {print("x$niterMarginal has more than one element. Reduced to its first element"); x$niterMarginal<-x$niterMarginal[[1]]}
      if (length(x$niterMarginal)==0) {stop("x$niterMarginal has zero length. Not possible")}
      if (length(x$niterMarginal)==1 & !is.numeric(x$niterMarginal)) {stop("x$niterMarginal is not of numeric type.")}

      ## checking adequacy of nburnin_extra component
      if (length(x$nburnin_extra)>1) {print("x$nburnin_extra has more than one element. Reduced to its first element"); x$nburnin_extra<-x$nburnin_extra[[1]]}
      if (length(x$nburnin_extra)==0) {stop("x$nburnin_extra has zero length. Not possible")}
      if (length(x$nburnin_extra)==1 & !is.numeric(x$nburnin_extra)) {stop("x$nburnin_extra is not of numeric type.")}


      ## checking adequacy of convergenceSet component
      if (length(x$convergenceSet)>=1 & !is.numeric(x$convergenceSet)) {stop("x$convergenceSet is not of numeric type.")}
      if (length(x$convergenceSet)>=1 & min(x$convergenceSet)<0) {stop("x$convergenceSet should be positive.")}
      if (length(x$convergenceSet)>=1 & max(x$convergenceSet)>1) {stop("x$convergenceSet should be below 1.")}

      NULL
    })


    ########################################
    ###0-1.3 Checking the installation of adequate packages and programs
    ########################################

    if (control$check.installation)
    {
      if (MCMC_language=="Nimble" & !control.MCMC$parallelize & sum(search()=="package:nimble")==0) {stop("Since MCMC_language is \"Nimble\", library \"nimble\" should be loaded which is not the case")}
      if ((MCMC_language=="Nimble"|MCMC_language=="Jags") & control.MCMC$parallelize & sum(search()=="package:parallel")==0) {stop("Since MCMC_language is \"Nimble\" or \"Jags\", and parallelization is required, library \"parallel\" should be loaded which is not the case")}
      if (MCMC_language=="Nimble" & control.MCMC$APT & !control.MCMC$parallelize & sum(search()=="package:nimbleAPT")==0) {stop("Since MCMC_language is \"Nimble\" and APT is TRUE, library \"nimbleAPT\" should be loaded which is not the case")}
      if (control$neff.method=="Stan" & nchar(system.file(package='rstan'))==0) {stop("Since parameter neff.method in control argument is \"Stan\", package \"rstan\" should be installed, which is not the case")}
      if (control$convtype=="Gelman_new" & nchar(system.file(package='ggmcmc'))==0) {stop("Since parameter convtype in control argument is \"Stan\", package \"ggmcmc\" should be installed, which is not the case")}
      if (MCMC_language=="Greta"& nchar(system.file(package='greta'))==0) {stop("Since MCMC_language is \"Greta\", package \"greta\" should be installed, which is not the case")}
      if (is.element(MCMC_language,c("Nimble")) & control.MCMC$parallelize & nchar(system.file(package='parallel'))==0) {stop("Since MCMC_language is \"Nimble\", and parameter parallelize in control.MCMC is TRUE, package \"parallel\" should be installed, which is not the case")}
      if (is.element(MCMC_language,c("Jags")) & control.MCMC$parallelize & nchar(system.file(package='parallel'))==0) {stop("Since MCMC_language is \"Jags\", and parameter parallelize in control.MCMC is TRUE, package \"parallel\" should be installed, which is not the case")}
      if (MCMC_language=="Nimble" & control.MCMC$APT & nchar(system.file(package='nimbleAPT'))==0) {stop("Since MCMC_language is \"Nimble\", and parameter  APTin control.MCMC is TRUE, package \"nimbleAPT\" should be installed, which is not the case")}
      if (MCMC_language=="Jags" & control.MCMC$parallelize & nchar(system.file(package='parallel'))==0) {stop("Since MCMC_language is \"Jags\", and parameter parallelize in control.MCMC is TRUE, package \"parallel\" should be installed, which is not the case")}
      if (MCMC_language=="Greta"& nchar(system.file(package='R6'))==0) {stop("Since MCMC_language is \"Greta\", package \"R6\" should be installed, which is not the case")}
      if (MCMC_language=="Greta"& nchar(system.file(package='tensorflow'))==0) {stop("Since MCMC_language is \"Greta\", package \"tensorflow\" should be installed, which is not the case")}
      #if (MCMC_language=="Nimble" & control.MCMC$APT & control.MCMC$WAIC) {print("Since MCMC_language is \"Nimble\", and parameter APT in control.MCMC is TRUE, parameter WAIC in control.MCMC turned to FALSE since this methods seems no to work with APT"); control.MCMC$WAIC<-FALSE}

      # this control has been temporarily removed since it takes some time and seems not to work within RMarkdown => dangerous
      # much as in: https://github.com/rstudio/rmarkdown/issues/1150
      # request made on greta forum: https://forum.greta-stats.org/t/request-for-a-modification-of-function-greta-sitrep/345
      #if (MCMC_language=="Greta") {test<-capture.output(greta::greta_sitrep(),type="message")
      #                           if (max(max(regexpr("greta is ready to use!",test))<=0))
      #                             {stop("Since MCMC_language is \"Greta\", greta should be ready to use, which is not the case - call \"greta_sitrep()\" for more details")}
      #                          }
      if (MCMC_language=="Jags"& nchar(system.file(package='rjags'))==0) {stop("Since MCMC_language is \"Jags\", package \"rjags\" should be installed, which is not the case")}
      if (MCMC_language=="Jags"& nchar(system.file(package='runjags'))==0) {stop("Since MCMC_language is \"Jags\", package \"runjags\" should be installed, which is not the case")}
      if (MCMC_language=="Jags") {suppressWarnings(temp<-runjags::testjags(silent=TRUE))
        if(!(temp$JAGS.available&temp$JAGS.found&temp$JAGS.major==4)) {stop("Since MCMC_language is \"Jags\", program \"JAGS\" with version greater than 4.x.y should be installed, which is not the case")}
      }

      if (MCMC_language=="Nimble"& nchar(system.file(package='nimble'))==0) {stop("Since MCMC_language is \"Nimble\", package \"nimble\" should be installed, which is not the case")}
    }


    ########################################
    ###0-1.4 modifications of parameters & checks that were brought through control or control.MCMC or checks of other parameters & potential messages:
    ########################################

    if (MCMC_language=="Greta" & control.MCMC$parallelize) {print("Since MCMC_language is \"Greta\", and Greta already paralleizes, component parallelize of control.MCMC will have no effect")}

    ## if control.MCMC$saveAllStochNodes and !control.MCMC$includeAllStochNodes & MCMC_language=="Nimble": turn the second to TRUE, otherwise the first will not be implemented.
    if ( control.MCMC$saveAllStochNodes & !control.MCMC$includeAllStochNodes & MCMC_language=="Nimble") {control.MCMC$includeAllStochNodes<-TRUE; print("Component includeAllStochNodes of control.MCMC turned to TRUE because saveAllStochNodes is TRUE") }
    if ( control.MCMC$monitorAllStochNodes & !control.MCMC$includeAllStochNodes & MCMC_language=="Nimble") {control.MCMC$includeAllStochNodes<-TRUE; print("Component includeAllStochNodes of control.MCMC turned to TRUE because monitorAllStochNodes is TRUE") }

    ## if control.MCMC$saveParentNodes and !control.MCMC$includeParentNodes & MCMC_language=="Nimble": turn the second to TRUE, otherwise the first will not be implemented.
    if ( control.MCMC$saveParentNodes & !control.MCMC$includeParentNodes & MCMC_language=="Nimble") {control.MCMC$includeParentNodes<-TRUE; print("Component includeParentNodes of control.MCMC turned to TRUE because saveParentNodes is TRUE") }
    if ( control.MCMC$monitorParentNodes & !control.MCMC$includeParentNodes & MCMC_language=="Nimble") {control.MCMC$includeParentNodes<-TRUE; print("Component includeParentNodes of control.MCMC turned to TRUE because monitorParentNodes is TRUE") }

    if (control.MCMC$WAIC & control.MCMC$resetMV & MCMC_language=="Nimble") {message("Be careful, considering simultaneously resetMV & WAIC in control.MCMC may somewhat lenghten the duration of the MCMC; consider turning resetMV to FALSE") }

    if (control.MCMC$APT){control.MCMC$resetMV<-FALSE}

    ### checking main  parameters of runMCMC_btadjust:

    ### checking params, params.conv, params.save
    if (length(params)>0) {if (mode(params)!="character") {stop("parameter params is not of character mode, but should be")}}
    if (length(params.conv)>0) {if (mode(params.conv)!="character") {stop("parameter params.conv is not of character mode, but should be")}}
    if (length(params.save)>0) {if (mode(params.save)!="character") {stop("parameter params.save is not of character mode, but should be")}}
    if (((length(params)+length(params.save)+length(params.conv))==0)) {warning("be careful, no parameter supplied: could be problematic")}

    ## checking adequacy of MCMC_language
    if (!is.element(MCMC_language,c("Nimble","Jags","Greta"))) {stop("MCMC_language should be either \"Nimble\", \"Jags\" or \"Greta\"; please respecify this parameter accordingly")}

    ## checking adequacy of Nchains parameter
    if (length(Nchains)>1) {print("Nchains has more than one element. Reduced to its first element"); Nchains<-Nchains[[1]]}
    if (length(Nchains)==0) {stop("Nchains has zero length. Not possible")}
    if (length(Nchains)==1 & !is.numeric(Nchains)) {stop("Nchains is not of numeric type.")}

    ##checking inits input
    if (length(inits)>0)
    {if (length(inits)==1) {if (is.function(inits)) {inits<-lapply(1:Nchains,function(x){inits()})}}
      if (length(inits)!=Nchains) stop("inits should have Nchains components, which is not the case")
    }

    ## checking adequacy of niter.min parameter
    if (length(niter.min)>1) {print("niter.min has more than one element. Reduced to its first element"); niter.min<-niter.min[[1]]}
    if (length(niter.min)==0) {stop("niter.min has zero length. Not possible")}
    if (length(niter.min)==1 & !is.numeric(niter.min)) {stop("niter.min is not of numeric type, although it should.")}
    if (length(niter.min)==1 & niter.min<0) {warning("niter.min is negative, which is strange")}

    ## checking adequacy of niter.max parameter
    if (length(niter.max)>1) {print("niter.max has more than one element. Reduced to its first element"); niter.max<-niter.max[[1]]}
    if (length(niter.max)==0) {stop("niter.max has zero length. Not possible")}
    if (length(niter.max)==1 & !is.numeric(niter.max)) {stop("niter.max is not of numeric type, although it should.")}
    if (length(niter.max)==1 & niter.max<0) {warning("niter.max is negative, which is strange")}

    ## checking adequacy of nburnin.min parameter
    if (length(nburnin.min)>1) {print("nburnin.min has more than one element. Reduced to its first element"); nburnin.min<-nburnin.min[[1]]}
    if (length(nburnin.min)==0) {stop("nburnin.min has zero length. Not possible")}
    if (length(nburnin.min)==1 & !is.numeric(nburnin.min)) {stop("nburnin.min is not of numeric type.")}
    if (length(nburnin.min)==1 & nburnin.min<0) {warning("nburnin.min is negative, which is strange")}

    ## checking adequacy of nburnin.max parameter
    if (length(nburnin.max)>1) {print("nburnin.max has more than one element. Reduced to its first element"); nburnin.max<-nburnin.max[[1]]}
    if (length(nburnin.max)==0) {stop("nburnin.max has zero length. Not possible")}
    if (length(nburnin.max)==1 & !is.numeric(nburnin.max)) {stop("nburnin.max is not of numeric type, although it should.")}
    if (length(nburnin.max)==1 & nburnin.max<0) {warning("nburnin.max is negative, which is strange")}

    ## checking adequacy of thin.min parameter
    if (length(thin.min)>1) {print("thin.min has more than one element. Reduced to its first element"); thin.min<-thin.min[[1]]}
    if (length(thin.min)==0) {stop("thin.min has zero length. Not possible")}
    if (length(thin.min)==1 & !is.numeric(thin.min)) {stop("thin.min is not of numeric type, although it should.")}
    if (length(thin.min)==1 & thin.min<0) {warning("thin.min is negative, which is strange")}

    ## checking adequacy of thin.max parameter
    if (length(thin.max)>1) {print("thin.max has more than one element. Reduced to its first element"); thin.max<-thin.max[[1]]}
    if (length(thin.max)==0) {stop("thin.max has zero length. Not possible")}
    if (length(thin.max)==1 & !is.numeric(thin.max)) {stop("thin.max is not of numeric type, although it should.")}
    if (length(thin.max)==1 & thin.max<0) {warning("thin.max is negative, which is strange")}

    ## checking adequacy of neff.min parameter
    if (length(neff.min)>1) {print("neff.min has more than one element. Reduced to its first element"); neff.min<-neff.min[[1]]}
    if (length(neff.min)==1 & !is.numeric(neff.min)) {stop("neff.min is not of numeric type, although it should.")}
    if (length(neff.min)==1) {if (neff.min<1) {stop("neff.min is less than 1, which should not be the case")}}


    ## checking adequacy of neff.mean parameter
    if (length(neff.mean)>1) {print("neff.mean has more than one element. Reduced to its first element"); neff.mean<-neff.mean[[1]]}
    if (length(neff.mean)==1 & !is.numeric(neff.mean)) {stop("neff.mean is not of numeric type, although it should.")}
    if (length(neff.mean)==1) {if (neff.mean<1) {stop("neff.mean is less than 1, which should not be the case")}}

    ## checking adequacy of neff.med parameter
    if (length(neff.med)>1) {print("neff.med has more than one element. Reduced to its first element"); neff.med<-neff.med[[1]]}
    if (length(neff.med)==1 & !is.numeric(neff.med)) {stop("neff.med is not of numeric type, although it should.")}
    if (length(neff.med)==1) {if (neff.med<1) {stop("neff.med is less than 1, which should not be the case")}}

    if ((length(neff.min)+length(neff.med)+length(neff.mean))==0){stop("neff.min & neff.mean & neff.med all have zero length which leaves the function without any information for neff. Provide one of these.")}


    ## checking adequacy of conv.max parameter
    if (length(conv.max)>1) {print("conv.max has more than one element. Reduced to its first element"); conv.max<-conv.max[[1]]}
    if (length(conv.max)==1 & !is.numeric(conv.max)) {stop("conv.max is not of numeric type, although it should.")}
    if (length(conv.max)==1) {if (conv.max<1) {stop("conv.max is less than 1, which should not be the case")}}


    ## checking adequacy of conv.mean parameter
    if (length(conv.mean)>1) {print("conv.mean has more than one element. Reduced to its first element"); conv.mean<-conv.mean[[1]]}
    if (length(conv.mean)==1 & !is.numeric(conv.mean)) {stop("conv.mean is not of numeric type, although it should.")}
    if (length(conv.mean)==1 ) {if (conv.mean<1) {stop("conv.mean is less than 1, which should not be the case")}}

    ## checking adequacy of conv.med parameter
    if (length(conv.med)>1) {print("conv.med has more than one element. Reduced to its first element"); conv.med<-conv.med[[1]]}
    if (length(conv.med)==1 & !is.numeric(conv.med)) {stop("conv.med is not of numeric type, although it should.")}
    if (length(conv.med)==1 ) {if (conv.med<1) {stop("conv.med is less than 1, which should not be the case")}}

    if ((length(conv.max)+length(conv.med)+length(conv.mean))==0) {stop("conv.max & conv.mean & conv.med all have zero length which leaves the function without any information for conv. Provide one of these.")}


    ## if control$check.convergence.firstrun is NULL: will depend on MCMC_language: if "Greta", will be TRUE because warmup phase separated from the rest; same for "Jags" in which the adaptation phase is also separated from the rest; otherwise will be FALSE.
    if (is.null(control$check.convergence.firstrun)) { if (MCMC_language!="Greta" & control.MCMC$n.adapt<=0) {control$check.convergence.firstrun<-FALSE} else {control$check.convergence.firstrun<-TRUE}}

    ##if control$check.convergence is FALSE, this should be the same for control$recheck.convergence
    if (!control$check.convergence) {control$recheck.convergence<-FALSE}



    if (!is.finite(control$time.max) & !is.finite(niter.max)) {stop("time.max in control & niter.max are not finite; not possible: can cause MCMC not to stop")}



    ## if control$identifier.to.print is specified add ", " afetr it because will be coerced with other characters
    if (control$identifier.to.print!="") { control$identifier.to.print<-paste(control$identifier.to.print,", ",sep="")}



    ## checking adequacy of control$neff.method
    if (control$neff.method!="Stan"&control$neff.method!="Coda") {stop("Parameter neff.method in control argument should be equal either to \"Stan\" or \"Coda\": please change this parameter")}



    ## checking adequacy of arguments in case MCMC_language=="Nimble"
    if (MCMC_language=="Nimble"&(is.null(code)|is.null(data)|is.null(constants)))
    {stop("Either code or data or constants argument is null although it is required by Nimble to define the model and fit MCMC; please provide this missing parameter")}

    if (MCMC_language=="Nimble"&!is.element("package:nimble",search()))
    {stop("It is required that you have \"package:nimble\" in your search list for runMCMC_btadajust to run nicely. Consider doing library(nimble) or require(nimble). You should already have loaded it to build the argument code")}

    ## checking adequacy of arguments in case MCMC_language=="Greta"
    if (MCMC_language=="Greta"&(is.null(model)))
    {stop("Model argument is null although it is required by Greta to define the model and fit MCMC; please provide this missing parameter")}
    if (MCMC_language=="Greta"&((!is.null(inits))&!(is.element("initials",c(class(inits),class(inits[[1]]))))))
    {stop("Initial values should be buildt with the greta::initials function in case Greta is the language")}

    ## checking adequacy of arguments in case MCMC_language=="Jags"
    if (MCMC_language=="Jags"&(is.null(code)|is.null(data)))
    {stop("Either code or data argument is null although it is required by Jags to define the model and fit MCMC; please provide this missing parameter")}

    if (MCMC_language=="Jags"&(!is.character(code)))
    {stop("Code should be a character containing either the link to the text file containing the code or the code itself")}

    if (is.finite(niter.max))
    {if ((niter.max/thin.max)>(100*neff.max)) {warning("There is a high risk of very oversized Rdata files since (niter.max/thin.max)>(100*neff.max); consider increasing thin.max")}
      else if ((niter.max/thin.max)>(10*neff.max)) {warning("There is a high risk of oversized Rdata files since (niter.max/thin.max)>(10*neff.max); consider increasing thin.max")}
      else if ((niter.max/thin.max)>(3*neff.max)) {warning("There is a risk of slightly oversized Rdata files since (niter.max/thin.max)>(3*neff.max); consider increasing thin.max")}
    }

    ## initial values of variables used on the algorithm
    if (length(control$seed)>0)
    {set.seed(control$seed)}

    ## initialisation of samplesList
    ## samplesList will be the list that will store the MCMC outputs; one the central objects of this package
    samplesList <- vector("list", Nchains)
    names(samplesList) <- paste0("chain", 1:Nchains)

    TempsList <- vector("list", Nchains)
    names(TempsList) <- paste0("chain", 1:Nchains)

    numIter.samplesList<-NULL ##will contain the number of iterations between successive values in samplesList; same dimension as the number of rows in samplesList[[1]]
    #recoding control.MCMC$n.adapt if is has its default value of -1 depending on whether we are with Nimble or Jags:
    if (control.MCMC$n.adapt==-1 & MCMC_language=="Nimble") {control.MCMC$n.adapt=0}
    if (control.MCMC$n.adapt==-1 & MCMC_language=="Jags") {control.MCMC$n.adapt=1000}
    force.niter.max<-control$force.niter.max
    force.time.max<-control$force.time.max
    index.conv<-1 ## will contain the index (in rows) of the transient period in number of rows in samplesList[[1]] (as diagnosed by convergence diagnostics)
    thin<-thin.min ## will contain the active thin value
    nburnin<-nburnin.min
    niter<-min(max(niter.min,nburnin+ceiling(ifelse(control$conv.thorough.check,length(control$props.conv)+1,2)*min.Nvalues/Nchains)*thin+10),niter.max)
    thinmult<-1
    Ncycles<-1
    niter.tot<-0
    neffs.reached<-FALSE


    if (MCMC_language!="Nimble")
    {
      ## making potential changes in params, params.save & params.conv if necessary : only in case MCMC_language is not "Nimble" since otherwise, done in 0-2
      params<-union(params,union(params.conv,params.save))
      if (is.null(params)) stop("Program stopped: at least one of the following arguments should be specified: params, params.conv, params.save.")
      if (is.null(params.conv)) {params.conv<-params}
      if (is.null(params.save)) {params.save<-params}
      ## replicating params in case there is only one to avoid errors in the code
      if (length(params)==1) {params<-rep(params,2)}
      if (length(params.conv)==1) {params.conv<-rep(params.conv,2)}
      #if (length(params.save)==1) {params.save<-rep(params.save,2)}
    }


    ########################################
    ###0-2. initial preparation of MCMC models: process differs depending on MCMC_language
    ########################################
    ## NB: nothing to be done here if MCMC_language=="Greta" as all is already prepared inside the R session

    if (MCMC_language=="Nimble") {
      if (!control.MCMC$parallelize)
      {
        Model <- vector("list", Nchains)
        CModelMCMC <- vector("list", Nchains)
        CModel <- vector("list", Nchains)
        ConfModel <- vector("list", Nchains)
        ModelMCMC <- vector("list", Nchains)

        for (i in 1:Nchains)
        {message("      Preparing chain ", i, " ...")

          Model[[i]] <- nimble::nimbleModel(code = code, name = 'Nimble', constants = constants, data = data, inits=inits[[i]],buildDerivs=control.MCMC$buildDerivs,calculate=FALSE)

          ### by default: taking params for params.save if not provided
          params<-union(params,union(params.conv,params.save))
          if (is.null(params.save)) {params.save<-params}
          if (is.null(params.conv)) {params.conv<-params}


          ## changing params in case control.MCMC$includeAllStochNodes
          if (control.MCMC$includeAllStochNodes) {params<-union(params,Model[[1]]$getNodeNames(stochOnly=TRUE,includeData=FALSE))}
          ## changing params.save in case control.MCMC$saveAllStochNodes
          if (control.MCMC$saveAllStochNodes) {params.save<-union(params.save,Model[[1]]$getNodeNames(stochOnly=TRUE,includeData=FALSE))}
          if (control.MCMC$monitorAllStochNodes) {params.conv<-union(params.conv,Model[[1]]$getNodeNames(stochOnly=TRUE,includeData=FALSE))}


          ## changing params in case control.MCMC$includeParentNodes
          if (control.MCMC$includeParentNodes) { for (j in Model[[1]]$getNodeNames(dataOnly=TRUE)) {params<-union(params,Model[[1]]$getParents(j))}}
          ## changing params.save in case control.MCMC$saveParentNodes
          if (control.MCMC$saveParentNodes) {for (j in Model[[1]]$getNodeNames(dataOnly=TRUE)) {params.save<-union(params.save,Model[[1]]$getParents(j))}}
          if (control.MCMC$monitorParentNodes) {for (j in Model[[1]]$getNodeNames(dataOnly=TRUE)) {params.conv<-union(params.conv,Model[[1]]$getParents(j))}}

          ## making potential changes in params, params.save & params.conv if necessary
          params<-union(params,union(params.conv,params.save))
          if (is.null(params)) stop("Program stopped: at least one of the following arguments should be specified: params, params.conv, params.save.")
          ## replicating params in case there is only one to avoid errors in the code
          if (length(params)==1) {params<-rep(params,2)}
          if (length(params.conv)==1) {params.conv<-rep(params.conv,2)}
          #if (length(params.save)==1) {params.save<-rep(params.save,2)}

          CModel[[i]] <- nimble::compileNimble(Model[[i]], showCompilerOutput = control.MCMC$showCompilerOutput)
          if (!control.MCMC$APT)
          {if (i<=length(control.MCMC$WAIC.control))
          {ConfModel[[i]] <- nimble::configureMCMC(Model[[i]], thin = thin, monitors = params, enableWAIC=control.MCMC$WAIC, WAIC.control=control.MCMC$WAIC.control[[i]])
          } else {
            ConfModel[[i]] <- nimble::configureMCMC(Model[[i]], thin = thin, monitors = params)
          }
            if (!is.null(control.MCMC$confModel.expression.toadd))
            {eval(control.MCMC$confModel.expression.toadd)}
            ModelMCMC[[i]] <- nimble::buildMCMC(ConfModel[[i]])
            CModelMCMC[[i]] <- nimble::compileNimble(ModelMCMC[[i]], project = CModel[[i]], showCompilerOutput = control.MCMC$showCompilerOutput)
          } else {
            nimble::nimCopy(Model[[i]], CModel[[i]], logProb=TRUE)
            if (i<=length(control.MCMC$WAIC.control))
            { ConfModel[[i]] <- nimble::configureMCMC(Model[[i]],enableWAIC=control.MCMC$WAIC, WAIC.control=control.MCMC$WAIC.control[[i]])} else
            { ConfModel[[i]] <- nimble::configureMCMC(Model[[i]]) }

            ConfModel[[i]]$removeSamplers()
            for (name in CModel[[i]]$getNodeNames(stochOnly=TRUE,includeData=FALSE)) {
              ConfModel[[i]]$addSampler(target = name,type = "sampler_RW_tempered",control=list(temperPriors=FALSE))
            }
            if (!is.null(control.MCMC$confModel.expression.toadd))
            {eval(control.MCMC$confModel.expression.toadd)}
            ConfModel[[i]]$addMonitors(params)
            ModelMCMC[[i]] <- nimbleAPT::buildAPT(ConfModel[[i]], Temps=if (length(control.MCMC$APT.initTemps)>0) {control.MCMC$APT.initTemps} else {1:control.MCMC$APT.NTemps}, monitorTmax=TRUE)
            CModelMCMC[[i]] <- nimble::compileNimble(ModelMCMC[[i]],showCompilerOutput = control.MCMC$showCompilerOutput)

          }
        }



      } else {#then: control.MCMC$parallelize
        message("      Preparing ", Nchains, " chains in parallel...")
        cl<-parallel::makeCluster(Nchains, timeout = ifelse(is.finite(control$time.max), 3*control$time.max+3600,30*24*3600))
        # next line: cf. https://stackoverflow.com/questions/21560363/seed-and-clusterapply-how-to-select-a-specific-run
        seeds <- getseeds(Nchains, control$seed); results.temp <- parallel::clusterApply(cl, seeds, worker.seed)
        parallel::clusterExport(cl, c("code", "inits", "data", "constants", "params","params.conv","params.save",
                                      "niter", "thin", "control","control.MCMC","MCMC_language"),envir=environment())
        for (j in seq_along(cl))
        {
          clusterNumber<-j
          parallel::clusterExport(cl[j], "clusterNumber",envir=environment())
          #s <- parallel::nextRNGStream(s)
        }

        out1 <- parallel::clusterEvalQ(cl, {
          eval(control.MCMC$parallelizeInitExpr)
          Nchains<-i<-1
          Model <- vector("list", Nchains)
          CModelMCMC <- vector("list", Nchains)
          CModel <- vector("list", Nchains)
          ConfModel <- vector("list", Nchains)
          ModelMCMC <- vector("list", Nchains)

          TempsList <- vector("list", Nchains)
          names(TempsList) <- paste0("chain", 1:Nchains)

          Model[[i]] <- nimble::nimbleModel(code = code, name = 'Nimble', constants = constants, data = data, inits=inits[[clusterNumber]],buildDerivs=control.MCMC$buildDerivs,calculate=FALSE)

          ### by default: taking params for params.save if not provided
          if (is.null(params.save)) {params.save<-params}

          ## changing params in case control.MCMC$includeAllStochNodes
          if (control.MCMC$includeAllStochNodes) {params<-union(params,Model[[1]]$getNodeNames(stochOnly=TRUE,includeData=FALSE))}
          ## changing params.save in case control.MCMC$saveAllStochNodes
          if (control.MCMC$saveAllStochNodes) {params.save<-union(params.save,Model[[1]]$getNodeNames(stochOnly=TRUE,includeData=FALSE))}


          ## changing params in case control.MCMC$includeParentNodes
          if (control.MCMC$includeParentNodes) { for (j in Model[[1]]$getNodeNames(dataOnly=TRUE)) {params<-union(params,Model[[1]]$getParents(j))}}
          ## changing params.save in case control.MCMC$saveParentNodes
          if (control.MCMC$saveParentNodes) {for (j in Model[[1]]$getNodeNames(dataOnly=TRUE)) {params.save<-union(params.save,Model[[1]]$getParents(j))}}

          ## making potential changes in params, params.save & params.conv if necessary
          params<-union(params,union(params.conv,params.save))
          if (is.null(params)) stop("Program stopped: at least one of the following arguments should be specified: params, params.conv, params.save.")
          if (is.null(params.conv)) {params.conv<-params}
          ## replicating params in case there is only one to avoid errors in the code
          if (length(params)==1) {params<-rep(params,2)}
          if (length(params.conv)==1) {params.conv<-rep(params.conv,2)}
          #if (length(params.save)==1) {params.save<-rep(params.save,2)}

          CModel[[i]] <- nimble::compileNimble(Model[[i]], showCompilerOutput = control.MCMC$showCompilerOutput)
          if (!control.MCMC$APT)
          {if (clusterNumber<=length(control.MCMC$WAIC.control))
          {ConfModel[[i]] <- nimble::configureMCMC(Model[[i]], thin = thin, monitors = params, enableWAIC=control.MCMC$WAIC, WAIC.control=control.MCMC$WAIC.control[[clusterNumber]])
          } else {
            ConfModel[[i]] <- nimble::configureMCMC(Model[[i]], thin = thin, monitors = params)
          }
            if (!is.null(control.MCMC$confModel.expression.toadd))
            {eval(control.MCMC$confModel.expression.toadd)}
            ModelMCMC[[i]] <- nimble::buildMCMC(ConfModel[[i]])
            CModelMCMC[[i]] <- nimble::compileNimble(ModelMCMC[[i]], project = CModel[[i]], showCompilerOutput = control.MCMC$showCompilerOutput)
          } else {
            nimble::nimCopy(Model[[i]], CModel[[i]], logProb=TRUE)
            if (clusterNumber<=length(control.MCMC$WAIC.control))
            { ConfModel[[i]] <- nimble::configureMCMC(Model[[i]],enableWAIC=control.MCMC$WAIC, WAIC.control=control.MCMC$WAIC.control[[clusterNumber]])} else
            { ConfModel[[i]] <- nimble::configureMCMC(Model[[i]]) }

            ConfModel[[i]]$removeSamplers()
            for (name in CModel[[i]]$getNodeNames(stochOnly=TRUE,includeData=FALSE)) {
              ConfModel[[i]]$addSampler(target = name,type = "sampler_RW_tempered",control=list(temperPriors=FALSE))
            }
            if (!is.null(control.MCMC$confModel.expression.toadd))
            {eval(control.MCMC$confModel.expression.toadd)}
            ConfModel[[i]]$addMonitors(params)
            ModelMCMC[[i]] <- nimbleAPT::buildAPT(ConfModel[[i]], Temps=if (length(control.MCMC$APT.initTemps)>0) {control.MCMC$APT.initTemps} else {1:control.MCMC$APT.NTemps}, monitorTmax=TRUE)
            CModelMCMC[[i]] <- nimble::compileNimble(ModelMCMC[[i]],showCompilerOutput = control.MCMC$showCompilerOutput)

          }
          paramsParentData<-NULL
          for (j in Model[[i]]$getNodeNames(dataOnly=TRUE))
          {
            paramsParentData<-union(paramsParentData,Model[[i]]$getParents(j))
          }
          #return(coda::as.mcmc.list(CModelMCMC[[i]]$mvSamples))
          return(list(Model[[i]]$getNodeNames(stochOnly=TRUE,includeData=FALSE),paramsParentData,params,params.conv,params.save))
          gc(verbose = FALSE)
        })

        params<-out1[[1]][[3]]
        params.conv<-out1[[1]][[4]]
        params.save<-out1[[1]][[5]]


      }
    } ## END: MCMC_language=="Nimble"

    if (MCMC_language=="Jags") {
      code0<-code
      if (!control.MCMC$parallelize)
      {message("      Preparing the ", Nchains, " chains...")
        if (is.character(code)) {
          if (substring(code,first=nchar(code)-3,last=nchar(code))!=".txt")
          {
            code<-textConnection(code)
          }
        }
        if (length(control$seed)>0)
        {if (is.null(inits))
        {inits<-lapply(1:Nchains,function(x){list(".RNG.name" = control.MCMC$RNG.names[(x-1)%%Nchains+1],".RNG.seed" = control$seed+x)})
        } else
        {
          inits<-lapply(1:Nchains,function(x){c(inits[[x]],list(".RNG.name" = control.MCMC$RNG.names[(x-1)%%Nchains+1],".RNG.seed" = control$seed+x))})
        }
        } else {
          if (is.null(inits))
          {inits<-lapply(1:Nchains,function(x){list(".RNG.name" = control.MCMC$RNG.names[(x-1)%%Nchains+1])})
          } else
          {
            inits<-lapply(1:Nchains,function(x){c(inits[[x]],list(".RNG.name" = control.MCMC$RNG.names[(x-1)%%Nchains+1]))})
          }
        }
        myModel<-rjags::jags.model(code, data=data, n.chains = Nchains, n.adapt = control.MCMC$n.adapt, inits=inits)
      } else {#then: control.MCMC$parallelize
        message("      Preparing the ", Nchains, " chains in parallel...")
        cl<-parallel::makeCluster(Nchains, timeout = ifelse(is.finite(control$time.max), 3*control$time.max+3600,30*24*3600))
        # next line: cf. https://stackoverflow.com/questions/21560363/seed-and-clusterapply-how-to-select-a-specific-run
        seeds <- getseeds(Nchains, control$seed); results.temp <- parallel::clusterApply(cl, seeds, worker.seed)
        #s <- .Random.seed
        if (length(control$seed)>0)
        {if (is.null(inits))
        {inits<-lapply(1:Nchains,function(x){list(".RNG.name" = control.MCMC$RNG.names[(x-1)%%Nchains+1],".RNG.seed" = control$seed+x)})
        } else
        {
          inits<-lapply(1:Nchains,function(x){c(inits[[x]],list(".RNG.name" = control.MCMC$RNG.names[(x-1)%%Nchains+1],".RNG.seed" = control$seed+x))})
        }
        } else {
          if (is.null(inits))
          {inits<-lapply(1:Nchains,function(x){list(".RNG.name" = control.MCMC$RNG.names[(x-1)%%Nchains+1])})
          } else
          {
            inits<-lapply(1:Nchains,function(x){c(inits[[x]],list(".RNG.name" = control.MCMC$RNG.names[(x-1)%%Nchains+1]))})
          }
        }
        parallel::clusterExport(cl, c("code", "inits", "data", "params",
                                      "niter", "thin", "control","control.MCMC","MCMC_language"),envir=environment())

        for (j in seq_along(cl)) {
          clusterNumber<-j
          parallel::clusterExport(cl[j], "clusterNumber",envir=environment())
          #s <- parallel::nextRNGStream(s)
        }
        out1 <- parallel::clusterEvalQ(cl, {
          eval(control.MCMC$parallelizeInitExpr)
          if (is.character(code)) {
            if (substring(code,first=nchar(code)-3,last=nchar(code))!=".txt")
            {
              code<-textConnection(code)
            }
          }
          myModel<-rjags::jags.model(code, data=data, n.chains = 1, n.adapt = control.MCMC$n.adapt, inits=inits[clusterNumber])

          return(myModel)
          gc(verbose = FALSE)
        })
      }
    }## END: MCMC_language=="Jags"
  })
  ## END: current.CPU.time<-system.time({
  time.MCMC.Preparation<-(Sys.time()-time.MCMC.Preparation0)
  units(time.MCMC.Preparation)<-"secs"
  time.MCMC.Preparation.num<-as.double(time.MCMC.Preparation)
  CPUtime.MCMC.Preparation<-CPUtime.MCMC.Preparation+current.CPU.time[1]+current.CPU.time[2]+ifelse(is.na(current.CPU.time[4]),0, current.CPU.time[4])+ifelse(is.na(current.CPU.time[5]),0, current.CPU.time[5])
  childCPUtime.MCMC.Preparation<-childCPUtime.MCMC.Preparation+current.CPU.time[4]+current.CPU.time[5]

  ########################################
  ###0-2.1 running first control.MCMC$na.adapt iterations in case MCMC_language=="Nimble"
  ########################################
  current.CPU.time<-system.time({
    time.MCMC0<-Sys.time()
    if (MCMC_language=="Nimble") {
      if (!control.MCMC$parallelize)
      {
        for (i in 1:Nchains)
        {message("      Running adaptation for chain ", i, "...")
          if (length(control$seed)>0)
          {set.seed(control$seed+i)}
          Modeltemp <- {if (nimble::is.Cnf(CModelMCMC[[i]]))
            CModelMCMC[[i]]$Robject$model$CobjectInterface
            else CModelMCMC[[i]]$model}
          Modeltemp$setInits(inits[[i]])
          if (control.MCMC$n.adapt>0)
          {
            if (!control.MCMC$APT) { CModelMCMC[[i]]$run(control.MCMC$n.adapt, nburnin = control.MCMC$n.adapt, thin = thin, progressBar =TRUE) } else {
              nIter   <- control.MCMC$n.adapt
              CModelMCMC[[i]]$run(nIter,thin=nIter,
                                  reset          = FALSE, ## Do not reset the adaptive MCMC, let adaptation continue as it is
                                  adaptTemps     = TRUE,  ## Allows temperature ladder to adjust
                                  resetTempering = FALSE, ## Keeps the adjustments modest so avoids volatile behaviour. Setting to TRUE can make the temperature ladder make vary large changes
                                  printTemps     = FALSE,  ## Prevents verbose printing of temperature ladder updates
                                  progressBar    = TRUE,
                                  resetMV        = FALSE,
                                  tuneTemper1=control.MCMC$APT.tuneTemps[1], tuneTemper2=control.MCMC$APT.tuneTemps[2])}

          }
        }
      } else {#then: control.MCMC$parallelize
        message("      Running adaptation for ", Nchains, " chains in parallel...")
        out1 <- parallel::clusterEvalQ(cl, {
          #if (length(control$seed)>0)
          #{set.seed(control$seed+clusterNumber)}
          Modeltemp <- {if (nimble::is.Cnf(CModelMCMC[[i]]))
            CModelMCMC[[i]]$Robject$model$CobjectInterface
            else CModelMCMC[[i]]$model}
          Modeltemp$setInits(inits[[i]])
          if (control.MCMC$n.adapt>0)
          {
            if (!control.MCMC$APT) { CModelMCMC[[i]]$run(control.MCMC$n.adapt, nburnin = control.MCMC$n.adapt, thin = thin, progressBar =TRUE) } else {
              nIter   <- control.MCMC$n.adapt
              CModelMCMC[[i]]$run(nIter,thin=nIter,
                                  reset          = FALSE, ## Do not reset the adaptive MCMC, let adaptation continue as it is
                                  adaptTemps     = TRUE,  ## Allows temperature ladder to adjust
                                  resetTempering = FALSE, ## Keeps the adjustments modest so avoids volatile behaviour. Setting to TRUE can make the temperature ladder make vary large changes
                                  printTemps     = FALSE,  ## Prevents verbose printing of temperature ladder updates
                                  progressBar    = TRUE,
                                  resetMV        = FALSE,
                                  tuneTemper1=control.MCMC$APT.tuneTemps[1], tuneTemper2=control.MCMC$APT.tuneTemps[2])}
          }
          #return(coda::as.mcmc(as.matrix(coda::as.mcmc.list(CModelMCMC[[i]]$mvSamples))))
          #return(coda::as.mcmc.list(CModelMCMC[[i]]$mvSamples))
          return(NULL)
          gc(verbose = FALSE)
        })
      }
    }	## END: MCMC_language=="Nimble"

  })	## END: current.CPU.time<-

  time.MCMC<-(Sys.time()-time.MCMC0)
  units(time.MCMC)<-"secs"
  time.MCMC.num<-as.double(time.MCMC)
  CPUtime.MCMC<-CPUtime.MCMC+current.CPU.time[1]+current.CPU.time[2]+ifelse(is.na(current.CPU.time[4]),0, current.CPU.time[4])+ifelse(is.na(current.CPU.time[5]),0, current.CPU.time[5])
  childCPUtime.MCMC<-childCPUtime.MCMC+current.CPU.time[4]+current.CPU.time[5]

  ########################################
  ###1.1- First run of MCMCs:
  ########################################
  current.CPU.time<-system.time({
    time.MCMC0<-Sys.time()
    samplesList <- vector("list", Nchains)
    names(samplesList) <- paste0("chain", 1:Nchains)

    message(control$identifier.to.print,"Cycle ", Ncycles, "...")
    print("###################################################################################")

    if (MCMC_language=="Nimble") {
      nburnin.min0<-nburnin.min

      if (!control.MCMC$parallelize)
      {
        for (i in 1:Nchains)
        {message("      Running chain ", i, ", with ", niter, " iterations...")
          if (length(control$seed)>0)
          {set.seed(control$seed+i+Nchains)}
          if (!control.MCMC$APT) {CModelMCMC[[i]]$run(niter, nburnin=nburnin, progressBar =TRUE)
            samplesList[[i]] <- as.matrix((CModelMCMC[[i]]$mvSamples))} else {

              nIter   <- nburnin
              #CModelMCMC[[i]]$thin<-nburnin
              #CModelMCMC[[i]]$thin2<-nburnin
              CModelMCMC[[i]]$thinPrintTemps<-eval(control.MCMC$APT.thinPrintTemps)
              CModelMCMC[[i]]$run(nIter,thin=nburnin,
                                  reset          = FALSE, ## Do not reset the adaptive MCMC, let adaptation continue as it is
                                  adaptTemps     = TRUE,  ## Allows temperature ladder to adjust
                                  resetTempering = FALSE, ## Keeps the adjustments modest so avoids volatile behaviour. Setting to TRUE can make the temperature ladder make vary large changes
                                  printTemps     = FALSE,  ## Prevents verbose printing of temperature ladder updates
                                  progressBar    = TRUE,
                                  resetMV        = FALSE,
                                  tuneTemper1=control.MCMC$APT.tuneTemps[1], tuneTemper2=control.MCMC$APT.tuneTemps[2])

              nIter   <- niter-nburnin
              #CModelMCMC[[i]]$thin<-thin
              #CModelMCMC[[i]]$thin2<-thin
              CModelMCMC[[i]]$thinPrintTemps<-eval(control.MCMC$APT.thinPrintTemps)
              CModelMCMC[[i]]$run(nIter,thin=thin,
                                  reset          = FALSE, ## Do not reset the adaptive MCMC, let adaptation continue as it is
                                  adaptTemps     = TRUE,  ## Allows temperature ladder to adjust
                                  resetTempering = FALSE, ## Keeps the adjustments modest so avoids volatile behaviour. Setting to TRUE can make the temperature ladder make vary large changes
                                  printTemps     = TRUE,  ## Prevents verbose printing of temperature ladder updates
                                  progressBar    = TRUE,
                                  resetMV        = FALSE,
                                  tuneTemper1=control.MCMC$APT.tuneTemps[1], tuneTemper2=control.MCMC$APT.tuneTemps[2])

              samplesList[[i]] <- as.matrix(CModelMCMC[[i]]$mvSamples)
              TempsList[[i]]<-rbind(TempsList[[i]],CModelMCMC[[i]]$Temps)
            }
        }
        if (!control.MCMC$APT)
          {
          samplesList <- coda::as.mcmc.list(lapply(samplesList, function(x){coda::as.mcmc(as.matrix(x))}))
          mvSamples.previous<-dim(samplesList[[1]])[1]
          } else  {
          samplesList <- window(coda::as.mcmc.list(lapply(samplesList, function(x){coda::as.mcmc(as.matrix(x))})),start=3)
          mvSamples.previous<-dim(samplesList[[1]])[1]
          }

      } else {#then: control.MCMC$parallelize
        message("      Running ", Nchains, " chains in parallel, with ", niter, " iterations...")
        parallel::clusterExport(cl, c("niter", "thin", "nburnin"),envir=environment())
        out1 <- parallel::clusterEvalQ(cl, {
          #if (length(control$seed)>0)
          #{set.seed(control$seed+clusterNumber+Nchains)}
          if (!control.MCMC$APT) {CModelMCMC[[i]]$run(niter, nburnin=nburnin, progressBar =TRUE)
            samplesList <- coda::as.mcmc(as.matrix(CModelMCMC[[i]]$mvSamples))
            return(list(coda::as.mcmc(as.matrix(CModelMCMC[[i]]$mvSamples))))} else {
              nIter   <- nburnin
              #CModelMCMC[[i]]$thin<-nburnin
              #CModelMCMC[[i]]$thin2<-nburnin
              CModelMCMC[[i]]$thinPrintTemps<-eval(control.MCMC$APT.thinPrintTemps)
              CModelMCMC[[i]]$run(nIter,thin=nburnin,
                                  reset          = FALSE, ## Do not reset the adaptive MCMC, let adaptation continue as it is
                                  adaptTemps     = TRUE,  ## Allows temperature ladder to adjust
                                  resetTempering = FALSE, ## Keeps the adjustments modest so avoids volatile behaviour. Setting to TRUE can make the temperature ladder make vary large changes
                                  printTemps     = TRUE,  ## Prevents verbose printing of temperature ladder updates
                                  progressBar    = TRUE,
                                  resetMV        = FALSE,
                                  tuneTemper1=control.MCMC$APT.tuneTemps[1], tuneTemper2=control.MCMC$APT.tuneTemps[2])
              #samplesList0[[i]] <- as.matrix(CModelMCMC[[i]]$mvSamples)

              nIter   <- niter-nburnin
              #CModelMCMC[[i]]$thin<-thin
              #CModelMCMC[[i]]$thin2<-thin
              CModelMCMC[[i]]$thinPrintTemps<-eval(control.MCMC$APT.thinPrintTemps)
              CModelMCMC[[i]]$run(nIter,thin=thin,
                                  reset          = FALSE, ## Do not reset the adaptive MCMC, let adaptation continue as it is
                                  adaptTemps     = TRUE,  ## Allows temperature ladder to adjust
                                  resetTempering = FALSE, ## Keeps the adjustments modest so avoids volatile behaviour. Setting to TRUE can make the temperature ladder make vary large changes
                                  printTemps     = TRUE,  ## Prevents verbose printing of temperature ladder updates
                                  progressBar    = TRUE,
                                  resetMV        = FALSE,
                                  tuneTemper1=control.MCMC$APT.tuneTemps[1], tuneTemper2=control.MCMC$APT.tuneTemps[2])
              samplesList <- coda::as.mcmc(as.matrix(CModelMCMC[[i]]$mvSamples))

              return(list(samplesList,CModelMCMC[[i]]$Temps))
            }

          #return(coda::as.mcmc(as.matrix(coda::as.mcmc.list(CModelMCMC[[i]]$mvSamples))))
          #return(as.matrix(CModelMCMC[[i]]$mvSamples))

          gc(verbose = FALSE)
        })
        if (!control.MCMC$APT)
          {
            samplesList <- coda::as.mcmc.list(lapply(out1, function(x){coda::as.mcmc(as.matrix(x[[1]]))}))
            mvSamples.previous<-dim(out1[[1]])[1]
          } else  {
            samplesList <- window(coda::as.mcmc.list(lapply(out1, function(x){coda::as.mcmc(as.matrix(x[[1]]))})),start=3)
            mvSamples.previous<-dim(window(coda::as.mcmc.list(lapply(out1, function(x){coda::as.mcmc(as.matrix(x[[1]]))})),start=3)[[1]])[1]
          }


        if (control.MCMC$APT) {TempsList<-lapply(1:Nchains,function(x){rbind(TempsList[[x]],out1[[x]][[2]])})}
      }
    }	 ## END: MCMC_language=="Nimble"

    if (MCMC_language=="Jags") {

      nburnin.min0<-nburnin.min
      if (!control.MCMC$parallelize)
      {
        #running unstored nburnin.min iterations:
        update(myModel, n.iter=nburnin.min)

        #changing nburnin.min in case MCMC_language=="Jags", after update, for consistency of calculations of number of iterations:
        nburnin.min0<-nburnin.min
        nburnin.min<-0

        ## as of 2022/12/14: added n.adapt to niter because otherwise output is void. probably because in the adaptive phase. Removed on 2023/04/27 because reversed behaviour: too much values.
        ## as of 2023/04/20: nburnin.min0 is removed from niter for cross-compatibility with Nimble's behaviour
        resultat<-rjags::coda.samples(myModel,var=params, n.iter = niter-nburnin.min0, thin = thin)
        #matres<-as.matrix(resultat)
        #format changed due to particular value for start with Jags: https://stackoverflow.com/questions/63507610/regarding-a-warning-message-in-jags-for-r
        #samplesList<- coda::as.mcmc.list(lapply(resultat,function(x){y<-x; attributes(y)$mcpar<-c(1, attributes(y)$mcpar[2]- attributes(y)$mcpar[1]+1, attributes(y)$mcpar[3]);y}))
        samplesList<- coda::as.mcmc.list(lapply(resultat,function(x){coda::as.mcmc(as.matrix(x))}))

      } else {#then: control.MCMC$parallelize

        message("      Running ", Nchains, " chains in parallel, with ", niter, " iterations...")

        parallel::clusterExport(cl, c("Nchains","niter", "thin", "nburnin.min"),envir=environment())
        out1 <- parallel::clusterEvalQ(cl, {
          update(myModel, n.iter=nburnin.min)
          #changing nburnin.min in case MCMC_language=="Jags", after update, for consistency of calculations of number of iterations:
          nburnin.min0<-nburnin.min
          nburnin.min<-0
          resultat<-rjags::coda.samples(myModel,var=params, n.iter = niter-nburnin.min0, thin = thin)
          return(resultat)
          gc(verbose = FALSE)
        })
        #samplesList<- coda::as.mcmc.list(lapply(out1,function(x){y<-x; attributes(y)$mcpar<-c(1, attributes(y)$mcpar[2]- attributes(y)$mcpar[1]+1, attributes(y)$mcpar[3]);coda::as.mcmc(y)}))
        samplesList<- coda::as.mcmc.list(lapply(out1,function(x){coda::as.mcmc(as.matrix(x))}))

      } ## END if (!control.MCMC$parallelize)
    }	## END: MCMC_language=="Jags"


    if (MCMC_language=="Greta") {
      #changing nburnin.min in case MCMC_language=="Greta", for consistency of calculations of number of iterations:
      nburnin.min0<-0
      nburnin.min<-0
      ## added to try to resolve problems with Greta
      niter<-(floor(niter/thin)+1*as.double((niter/thin-floor(niter/thin))>0))*thin
      if (!is.null(inits))
      {samplesList<-m<-greta::mcmc(model,sampler = eval(control.MCMC$sampler),initial_values=inits, warmup=control.MCMC$warmup, n_cores=control.MCMC$n_cores,chains=Nchains, n_samples=niter, thin=thin, pb_update=(floor(50/thin)+1*as.double((50/thin-floor(50/thin))>0))*thin)}
      else
      {samplesList<-m<-greta::mcmc(model,sampler = eval(control.MCMC$sampler),warmup=control.MCMC$warmup, n_cores=control.MCMC$n_cores,chains=Nchains, n_samples=niter, thin=thin, pb_update=(floor(50/thin)+1*as.double((50/thin-floor(50/thin))>0))*thin)}
      #to be checked whether niter is the correct argument here:
      ## should be: number of MCMC samples to draw per chain (after any warm-up, but before thinning)
    }	## END: MCMC_language=="Greta"
    ## adding this line to make the link with coda library for operations such as as.matrix, summary...
    samplesList <- coda::as.mcmc.list(samplesList)
    ## adding this line to make SamplesList of standard format (thin=1 & burnin=1 in it)
    samplesList<- coda::as.mcmc.list(lapply(samplesList,function(x){coda::as.mcmc(as.matrix(x))}))
  }) ## END: current.CPU.time<-system.time({
  time.MCMC<-(Sys.time()-time.MCMC0)
  units(time.MCMC)<-"secs"
  time.MCMC.num<-as.double(time.MCMC)
  CPUtime.MCMC<-CPUtime.MCMC+current.CPU.time[1]+current.CPU.time[2]+ifelse(is.na(current.CPU.time[4]),0, current.CPU.time[4])+ifelse(is.na(current.CPU.time[5]),0, current.CPU.time[5])
  childCPUtime.MCMC<-childCPUtime.MCMC+current.CPU.time[4]+current.CPU.time[5]

  current.CPU.time<-system.time({
    numIter.samplesList<-c(numIter.samplesList,rep(thin,dim(samplesList[[1]])[1]))
    size.samplesList<-dim(samplesList[[1]])[1]
    resetMV.previous<-FALSE
    niter.tot.previous<-niter.tot
    niter.tot<-niter.tot+niter

    ## indices.conv wil contain the indices in samplesList of the parameters in params.conv to select only those columns for diagnosing convergence & neff
    indices.conv<-NULL
    for (i in params.conv)
    {indices.conv<-c(indices.conv,which(i==dimnames(samplesList[[1]])[[2]]|regexpr(paste(i,"\\[",sep=""),dimnames(samplesList[[1]])[[2]])==1))
    }
    indices.conv<-sort(indices.conv)

    ## indices.save wil contain the indices in samplesList of the parameters in params.save to select only those columns for saving. Only used in the end.
    indices.save<-NULL
    for (i in params.save)
    {indices.save<-c(indices.save,which(i==dimnames(samplesList[[1]])[[2]]|regexpr(paste(i,"\\[",sep=""),dimnames(samplesList[[1]])[[2]])==1))
    }
    indices.save<-sort(indices.save)


    ########################################
    ###1.2- removing chains in case some chains get stuck
    ########################################

    #variable that will contain the updated number of (valid) MCMC chains
    Nchains.updated<-Nchains
	chains.to.remove<-NULL
    if (control$remove.fixedchains)
    {chains.to.remove<-(1:Nchains)[apply(matrix(sapply(samplesList,function(x){apply(as.matrix(x),2,var)}),ncol=Nchains),2,sum)==0]
    Nchains.updated<-Nchains-length(chains.to.remove)
	if (length((control.MCMC$WAIC.control))>Nchains.updated) {print("control.MCMC$WAIC.control has more than Nchains.updated=",Nchains.updated," elements; truncated to the first Nchains.updated elements"); control.MCMC$WAIC.control<-control.MCMC$WAIC.control[1:Nchains.updated] }

    if (Nchains.updated==0)
    {stop("All MCMC chains have fixed parameters. Consider revising the initial values or the model.")
    }
	if (Nchains.updated==1 & length(chains.to.remove)>0)
    {
	#impossible to do the following because targets were not defined for Geweke
	#print("control$convtype turned to Geweke because only one acvtive chain remaining")
	#control$convtype=="Geweke"
	stop("Impossible to use the Gelman-Rubin convergence diagnostic with only one MCMC chain (***after update of Nchains***): please change Nchain or control$convtype or update your initial values")

	}

    }



    ########################################
    ###1.3- first diagnostic
    ########################################

    index.conv0<-index.conv
    index.props.conv<-1
    #print(index.conv)
    #print(size.samplesList)
    #print(lapply(samplesList,function(x){attributes(x)$mcpar}))
    #print(samplesList)
    #assign("samplesList",samplesList,1)

    if (length(chains.to.remove)>0)
    {index.conv.local<-index.conv
    diags<-conveff(window(coda::as.mcmc.list(samplesList[-chains.to.remove])[,indices.conv],start=index.conv,end=size.samplesList),control$print.diagnostics)}
    else
    {index.conv.local<-index.conv
    diags<-conveff(window(samplesList[,indices.conv],start=index.conv,end=size.samplesList),control$print.diagnostics)}

    ########################################
    ###1.4- interpreting convergence of first run:
    ########################################


    if (!control$conv.thorough.check)
    {index.conv.new<-floor(quantile(index.conv0:size.samplesList,control$props.conv[index.props.conv]))
    ##in case has not converged, re-checks convergence at index.conv.new if acceptable in terms of
    ## (i) nburnin.max, (ii) control$props.conv[index.props.conv] & (iii) number of remaining values (must be >min.Nvalues)
    while (!checking.convergence(diags,control$convtype,control$convtype.Gelman,conv.max,conv.med,conv.mean)&((nburnin.min+sum(numIter.samplesList[1:(index.conv.new-1)]))<nburnin.max)&(control$props.conv[index.props.conv]<1)&((length(index.conv.new:size.samplesList)*Nchains.updated)>min.Nvalues))
    {
      index.conv.local<-index.conv.new
      nburnin<-nburnin.min+sum(numIter.samplesList[1:(index.conv.local-1)])
      if (length(chains.to.remove)>0)
      {diags<-conveff(window(coda::as.mcmc.list(samplesList[-chains.to.remove])[,indices.conv],start=index.conv.local,end=size.samplesList),control$print.diagnostics)}
      else
      {diags<-conveff(window(samplesList[,indices.conv],start=index.conv.local,end=size.samplesList),control$print.diagnostics)}

      index.props.conv<-index.props.conv+1
      index.conv.new<-floor(quantile(index.conv0:size.samplesList,control$props.conv[index.props.conv]))
    }
    } else {##control$conv.thorough.check
      diags.collection<-list(diags)
      for (index.props.conv in 1:length(control$props.conv))
      {
        index.conv.new<-floor(quantile(index.conv0:size.samplesList,control$props.conv[index.props.conv]))
        index.conv.local<-index.conv.new
        if (((nburnin.min+sum(numIter.samplesList[1:(index.conv.new-1)]))<nburnin.max)&(control$props.conv[index.props.conv]<1)&((length(index.conv.new:size.samplesList)*Nchains.updated)>min.Nvalues))
        {
          if (length(chains.to.remove)>0)
          {diags.collection<-c(diags.collection,list(conveff(window(coda::as.mcmc.list(samplesList[-chains.to.remove])[,indices.conv],start=index.conv.local,end=size.samplesList),control$print.diagnostics)))}
          else
          {diags.collection<-c(diags.collection,list(conveff(window(samplesList[,indices.conv],start=index.conv.local,end=size.samplesList),control$print.diagnostics)))}
        }
      }
      ### determine best index.props.conv ::: 1- convergence; 2- best overallneff within category of conv.??
      analysisconv.collection<-sapply(diags.collection,function(x){checking.convergence(x,control$convtype,control$convtype.Gelman,conv.max,conv.med,conv.mean)})
      analysisneff.collection<-sapply(diags.collection,function(x){scale.available.neffs(x,neff.min,neff.med,neff.mean)})
      indicesconv<-(1:length(analysisconv.collection))[analysisconv.collection]
      if (length(indicesconv)>0)
      {indexopt<-indicesconv[which(analysisneff.collection[indicesconv]==max(analysisneff.collection[indicesconv]))[1]]
      } else
      {indexopt<-(1:length(analysisconv.collection))[which(analysisneff.collection==max(analysisneff.collection))[1]]
      }
      if (indexopt>1)
      {
        index.conv.new<-floor(quantile(index.conv0:size.samplesList,control$props.conv[indexopt-1]))
        diags<-diags.collection[[indexopt]]
      }
      else
      {
        index.conv.new<-index.conv0
        diags<-diags.collection[[1]]
      }
      print (paste0("Retained case with Nvalues=",diags$Nvalues," and nu.burn=",diags$nu.burn))
      print("###################################################################################")

      index.conv.local<-index.conv.new
      nburnin<-nburnin.min+sum(numIter.samplesList[1:(index.conv.local-1)])

    }
    ##final convergence diagnostic:
    converged<-checking.convergence(diags,control$convtype,control$convtype.Gelman,conv.max,conv.med,conv.mean)
    previously.converged<-as.logical(max(previously.converged,converged))

    ## changing converged state in cases where we should not have checked convergence at first run, not checked convergence at all or if iterations above nburnin.max
    if (!control$check.convergence.firstrun) {converged<-FALSE}
    if (!control$check.convergence) {converged<-TRUE}
    if (converged) {index.conv<-index.conv.local}

    ### FG:  inactivated 2023/03/11: I don't understand it : if ((nburnin.min+sum(numIter.samplesList[1:(index.conv-1)]))>=nburnin.max) {converged<-TRUE}

    ########################################
    ###1.5 in case of convergence, calculation of thinmult for future update of thin
    ########################################

    thinmult<-1
    thinmult.proposed<-1
    thinmult0<-1
    if (converged)
    {
      if ((length(index.conv:size.samplesList)*Nchains.updated)>min.Nvalues) {
        ## first checking if neffs reached
        neffs.reached<-checking.neffs.reached(diags,neff.min,neff.med,neff.mean)

        ## second, if neffs not reached, calculates multiplier of thin to be targeted
        if (!neffs.reached)
        {
          thinmult<-min(calculate.thinmult.target(diags,!control$only.final.adapt.thin,neff.min,neff.med,neff.mean),thin.max/thin)
          if (control$print.thinmult)
          {
            print(paste("Raw multiplier of thin: ", formatC_adapted(thinmult)))
            print("###################################################################################")
          }
          if (thinmult<control$min.thinmult){thinmult<-1}
          if (control$round.thinmult) {thinmult<-round(thinmult)}
          thinmult.proposed<-thinmult

          if(control$check.thinmult==3)
          {
            ########################################
            ### 1.5.1: check.thinmult==3: Going through thinning levels in decreasing order from thinmult, giving retained value of thinmult
            ########################################
            stop_decrease<-FALSE
            while (thinmult>1&!stop_decrease)
            {
              if (control$print.thinmult)
              {
                print(paste("Testing multiplier of thin: ", formatC_adapted(thinmult),":"))
              }
              diagstemp<-conveff(window.seq(samplesList[,indices.conv],start=index.conv,end=size.samplesList,thin=round(thinmult)),control$print.diagnostics)
              convergedtemp<-	checking.convergence(diagstemp,control$convtype,control$convtype.Gelman,conv.max,conv.med,conv.mean)
              neffs.reachedtemp<-ifelse(neffs.reached,checking.neffs.reached(diagstemp,neff.min,neff.med,neff.mean),TRUE)
              neffs.conserved<-checking.neffs.conserved(diagstemp,diags,control$max.prop.decr.neff,neff.min,neff.med,neff.mean,neffs.reached)
              min.Nvalues.OK<-(dim(as.matrix(window.seq(samplesList[,indices.conv],start=index.conv,end=size.samplesList,thin=round(thinmult))))[1])>min.Nvalues
              stop_decrease<-convergedtemp&neffs.reachedtemp&neffs.conserved&min.Nvalues.OK
              thinmult.prev<-thinmult
              thinmult<-ifelse(thinmult>control$decrease.thinmult.threshold, max(floor(control$decrease.thinmult.multiplier*thinmult), control$decrease.thinmult.threshold),thinmult-1)
            }
            if (stop_decrease) {diags<-diagstemp; thinmult<-thinmult.prev}

            if (control$print.thinmult)
            {
              print(paste("Retained multiplier of thin: ", formatC_adapted(thinmult),":"))
              print("###################################################################################")
            }
          } else { if(control$check.thinmult==2)
            {
              ########################################
              ### 1.5.2: check.thinmult==2: Going through thinning levels in decreasing order from thinmult, giving retained value of thinmult
              ########################################
              stop_decrease<-FALSE
              while (thinmult>1&!stop_decrease)
              {
                if (control$print.thinmult)
                {
                  print(paste("Testing multiplier of thin: ", formatC_adapted(thinmult),":"))
                }
                diagstemp<-conveff(window.seq(samplesList[,indices.conv],start=index.conv,end=size.samplesList,thin=round(thinmult)),control$print.diagnostics)
                convergedtemp<-	checking.convergence(diagstemp,control$convtype,control$convtype.Gelman,conv.max,conv.med,conv.mean)
                neffs.reachedtemp<-ifelse(neffs.reached,checking.neffs.reached(diagstemp,neff.min,neff.med,neff.mean),TRUE)
                #neffs.conserved<-checking.neffs.conserved(diagstemp,diags,control$max.prop.decr.neff,neff.min,neff.med,neff.mean,neffs.reached)
                min.Nvalues.OK<-(dim(as.matrix(window.seq(samplesList[,indices.conv],start=index.conv,end=size.samplesList,thin=round(thinmult))))[1])>min.Nvalues
                stop_decrease<-convergedtemp&neffs.reachedtemp&min.Nvalues.OK
                thinmult.prev<-thinmult
                thinmult<-ifelse(thinmult>control$decrease.thinmult.threshold, max(floor(control$decrease.thinmult.multiplier*thinmult), control$decrease.thinmult.threshold),thinmult-1)
              }
              if (stop_decrease) {diags<-diagstemp; thinmult<-thinmult.prev}

              if (control$print.thinmult)
              {
                print(paste("Retained multiplier of thin: ", formatC_adapted(thinmult),":"))
                print("###################################################################################")
              }
          } else { #check.thinmult==1

            ########################################
            ### 1.5.3: check.thinmult==1: checking min.Nvalues.OK criterion & calculating - approximate if !control$round.thinmult - convergence and neffs.reached with thinmult (if thinmult>1)
            ########################################
            if (thinmult>1)
            { stop_decrease<-FALSE
            while (thinmult>1&!stop_decrease)
            {
              if (control$print.thinmult)
              {
                print(paste("Testing multiplier of thin: ", formatC_adapted(thinmult),":"))
              }
              min.Nvalues.OK<-(dim(as.matrix(window.seq(samplesList[,indices.conv],start=index.conv,end=size.samplesList,thin=round(thinmult))))[1])>min.Nvalues
              stop_decrease<-min.Nvalues.OK
              thinmult.prev<-thinmult
              thinmult<-ifelse(thinmult>control$decrease.thinmult.threshold, max(floor(control$decrease.thinmult.multiplier*thinmult), control$decrease.thinmult.threshold),thinmult-1)
            }
            if (stop_decrease) {thinmult<-thinmult.prev}
            if (control$print.thinmult)
            {
              print(paste("Retained multiplier of thin: ", formatC_adapted(thinmult),":"))
              print("###################################################################################")
            }
            if (stop_decrease) {
              diags<-conveff(window.seq(samplesList[,indices.conv],start=index.conv,end=size.samplesList,thin=round(thinmult)),control$print.diagnostics)
              converged<-	checking.convergence(diags,control$convtype,control$convtype.Gelman,conv.max,conv.med,conv.mean)
              neffs.reached<-checking.neffs.reached(diags,neff.min,neff.med,neff.mean)
              }
            }

          }
        } #END: if(control$check.thinmult)
        }
      }
    }


    ##duplicating samplesList to sampledList.temp
    if (length(chains.to.remove)>0)
    {samplesList.temp<-coda::as.mcmc.list(samplesList[-chains.to.remove])}
    else
    {samplesList.temp<-samplesList}
    size.samplesList.temp<-size.samplesList
    index.conv.temp<-index.conv

    ## adding component to calculate indices.samplesList
    numIter.sum<-sum(numIter.samplesList)
    numIter.sum.conv<-sum(numIter.samplesList[1:index.conv])
    iters.ref<-rev(seq(from=numIter.sum,to=numIter.sum.conv,by=-thin))
    numIter.cumsum<-cumsum(numIter.samplesList)
    indices.samplesList<-iters.ref
    for (i in 1:length(iters.ref))
    {
      indices.samplesList[i]<-which.min(abs(numIter.cumsum-iters.ref[i]))
    }

  }) ## END: current.CPU.time<-system.time({
  CPUtime.btadjust<-CPUtime.btadjust+current.CPU.time[1]+current.CPU.time[2]+ifelse(is.na(current.CPU.time[4]),0, current.CPU.time[4])+ifelse(is.na(current.CPU.time[5]),0, current.CPU.time[5])
  childCPUtime.btadjust<-childCPUtime.btadjust+current.CPU.time[4]+current.CPU.time[5]


  ########################################
  ## 2. Rerunning the model sequentially:
  ########################################
  #default value for previously.converged0
  previously.converged0<-previously.converged

  while((!converged |!neffs.reached|force.niter.max|force.time.max)&niter>=thin)
  {
    current.CPU.time<-system.time({
      ##store previously.converged at the beginning of the last cycle
      previously.converged0<-previously.converged

      ########################################
      ### 2.1: specifying the new thinning level and the new number of iterations
      ########################################
      thin.init<-thin
      thin.theoretical<-thin*thinmult.proposed
      thin<-update.thin(thin,thinmult,thin.max,control$round.thinmult)

      #estimation of number of effective values already available
      if (!converged|((length(index.conv:size.samplesList)*Nchains.updated)<=min.Nvalues))
      {available.neffs<-0}
      else {available.neffs<-scale.available.neffs(diags,neff.min,neff.med,neff.mean)}

      current.time<-Sys.time()
      duration<-(current.time-time.start)
      units(duration)<-"secs"
      niter.previous<-niter
      niter<--1
      duration<-as.double(duration)

      ##if not converged: then roughly plan to redo niter.tot iterations - under conditions
      ## implies that at the end of next run niter.tot would be approximately doubled
      ## except especially if control$time.max might be exceeded
      if (! converged) {
        if (!previously.converged)
        {scaleconvs<-scale.available.convs(diags,control$convtype,control$convtype.Gelman,conv.max,conv.med,conv.mean)
        niter<-ceiling(min(c(max(round(niter.tot/Ncycles)*1/(1+exp(log(scaleconvs-1)-control$ip.nc))+niter.tot*1/(1+exp(-(log(scaleconvs-1)-control$ip.nc))),nburnin+ceiling(ifelse(control$conv.thorough.check,length(control$props.conv)+1,2)*min.Nvalues/Nchains)*thin+10-niter.tot),ifelse(control$time.max.turns.off.niter.max,Inf,niter.max)-niter.tot,(control$time.max-duration)*max(0.5,time.MCMC.num/(duration-time.MCMC.Preparation.num))*(niter.tot+ifelse(MCMC_language=="Jags"|MCMC_language=="Nimble",control.MCMC$n.adapt,0))/time.MCMC.num)))
        } else {
          #niter<-ceiling(min(c(niter.previous,niter.max-niter.tot,(control$time.max-duration)*0.95*(niter.tot+ifelse(MCMC_language=="Jags"|MCMC_language=="Nimble",control.MCMC$n.adapt,0))/duration)))
          niter<-ceiling(min(c(max(round(niter.tot/Ncycles),nburnin+ceiling(ifelse(control$conv.thorough.check,length(control$props.conv)+1,2)*min.Nvalues/Nchains)*thin+10- niter.tot),ifelse(control$time.max.turns.off.niter.max,Inf,niter.max)-niter.tot,(control$time.max-duration)*max(0.5,time.MCMC.num/(duration-time.MCMC.Preparation.num))*(niter.tot+ifelse(MCMC_language=="Jags"|MCMC_language=="Nimble",control.MCMC$n.adapt,0))/time.MCMC.num)))
        }

        if (Ncycles>1|control$check.convergence.firstrun) {
          print("Case of niter update: Non convergence")
          print("###################################################################################")
        }
      }

      ## if converged & !neffs.reached but not enough values to estimate thinmult
      ### then add few iterations to just exceed min.Nvalues while fulfilling nitex.max & control$time.max conditions
      if ( converged & !neffs.reached & ((length(index.conv:size.samplesList)*Nchains.updated)<=min.Nvalues))
      {
        niter<-ceiling(min(c(max((min.Nvalues-(length(index.conv:size.samplesList)*Nchains.updated)/Nchains.updated)+10,nburnin+ceiling(ifelse(control$conv.thorough.check,length(control$props.conv)+1,2)*min.Nvalues/Nchains)*thin+10- niter.tot),ifelse(control$time.max.turns.off.niter.max,Inf,niter.max)-niter.tot,(control$time.max-duration)*max(0.5,time.MCMC.num/(duration-time.MCMC.Preparation.num))*(niter.tot+ifelse(MCMC_language=="Jags"|MCMC_language=="Nimble",control.MCMC$n.adapt,0))/time.MCMC.num)))
        if (Ncycles>1|control$check.convergence.firstrun) {
          print("Case of niter update: Convergence but not enough values after convergence to safely update thin")
          print("###################################################################################")
        }
      }

      ## if converged & !neffs.reached and enough values to estimate thinmult
      if ( converged & !neffs.reached & ((length(index.conv:size.samplesList)*Nchains.updated)>min.Nvalues))
      {


        if ((Ncycles+1)/control$Ncycles.target<0.95)
        {
          ## if (Ncycles+1)/control$Ncycles.target<0.95
          ## then do not plan to reach the number of effective values in the next Cycle while fulfilling nitex.max & control$time.max conditions
          niter<-ceiling((Ncycles+1)/control$Ncycles.target*min(c(max(ceiling((control$safemultiplier.Nvals*neff.max-available.neffs)/Nchains.updated)*thin.theoretical,nburnin+ceiling(ifelse(control$conv.thorough.check,length(control$props.conv)+1,2)*min.Nvalues/Nchains)*thin+10- niter.tot),ifelse(control$time.max.turns.off.niter.max,Inf,niter.max)-niter.tot,(control$time.max-duration)*max(0.5,time.MCMC.num/(duration-time.MCMC.Preparation.num))*(niter.tot+ifelse(MCMC_language=="Jags"|MCMC_language=="Nimble",control.MCMC$n.adapt,0))/time.MCMC.num)))
          if (Ncycles>1|control$check.convergence.firstrun) {
            print("Case of niter update: Convergence but not in the last planned cycle")
            print("###################################################################################")
          }
        }
        else {
          ## if (Ncycles+1)/control$Ncycles.target>=0.95
          ## then plan to reach the number of effective values in the next Cycle while fulfilling nitex.max & control$time.max conditions
          niter<-min(c(max(ceiling((control$safemultiplier.Nvals*neff.max-available.neffs)/Nchains.updated)*thin.theoretical,nburnin+ceiling(ifelse(control$conv.thorough.check,length(control$props.conv)+1,2)*min.Nvalues/Nchains)*thin+10- niter.tot),ifelse(control$time.max.turns.off.niter.max,Inf,niter.max)-niter.tot,(control$time.max-duration)*max(0.5,time.MCMC.num/(duration-time.MCMC.Preparation.num))*(niter.tot+ifelse(MCMC_language=="Jags"|MCMC_language=="Nimble",control.MCMC$n.adapt,0))/time.MCMC.num))
          if (Ncycles>1|control$check.convergence.firstrun) {
            print("Case of niter update: Convergence and trying to reach end of MCMC at the end of next cycle")
            print("###################################################################################")
          }
        }
      }

      ## if converged and neffs.reached & force.niter.max:
      if ( converged & neffs.reached & (force.niter.max|force.time.max))
      {
        niter<-ceiling(min(c(niter.max-niter.tot,(control$time.max-duration)*max(0.5,time.MCMC.num/(duration-time.MCMC.Preparation.num))*(niter.tot+ifelse(MCMC_language=="Jags"|MCMC_language=="Nimble",control.MCMC$n.adapt,0))/time.MCMC.num)))
        if (Ncycles>1|control$check.convergence.firstrun) {
          print("Case of niter update: Convergence and enough values after convergence; since force.niter.max, performing iterations to reach niter.max")
          print("###################################################################################")
        }
      }

      ## corrections of values of niter in case these values are inadequate
      if (niter<=thin) {niter<--1}
      #if (niter==Inf){niter<-niter.tot}
      if (niter==Inf){ceiling(min(c(niter.tot,ifelse(control$time.max.turns.off.niter.max,Inf,niter.max)-niter.tot,(control$time.max-duration)*max(0.5,time.MCMC.num/(duration-time.MCMC.Preparation.num))*(niter.tot+ifelse(MCMC_language=="Jags"|MCMC_language=="Nimble",control.MCMC$n.adapt,0))/time.MCMC.num)))}
      if (niter<=0)
      {
        if (Ncycles>1|control$check.convergence.firstrun) {
          print("Number of planned new iterations non-positive: end of MCMC cycles")
          print("###################################################################################");
        }
        niter<--1
        thin<-thin.init
      }


      ########################################
      ### 2.1.bis: specifying resetMV.temporary (will be useful with "Nimble" MCMC_language)
      ########################################

      if (control$thinmult.in.resetMV.temporary)
      {
        resetMV.temporary<-!converged|(nburnin>niter.tot.previous)|thinmult>1
      } else {
        resetMV.temporary<-!converged|(nburnin>niter.tot.previous)
      }

    }) ## END: current.CPU.time<-system.time({
    CPUtime.btadjust<-CPUtime.btadjust+current.CPU.time[1]+current.CPU.time[2]+ifelse(is.na(current.CPU.time[4]),0, current.CPU.time[4])+ifelse(is.na(current.CPU.time[5]),0, current.CPU.time[5])
    childCPUtime.btadjust<-childCPUtime.btadjust+current.CPU.time[4]+current.CPU.time[5]

    ########################################
    ### 2.2: rerunning the model:
    ########################################
    if (niter>=thin)
    {
      current.CPU.time<-system.time({
        time.MCMC0<-Sys.time()
        ##slight modifications of niter & thin prior to re-running models
        niter<-ceiling(niter)
        thin<-ceiling(thin)
        ## added to try to resolve problems with Greta:
        ## cf. https://forum.greta-stats.org/t/problem-with-extra-samples-command-when-thin-pb-update-and-or-n-samples-are-not-multiples/338
        if (MCMC_language=="Greta") {niter<-ceiling((floor(niter/thin)+1*as.double((niter/thin-floor(niter/thin))>0))*thin)}

        ##updating Ncycles
        Ncycles<-Ncycles+1
        message(control$identifier.to.print,"Cycle ", Ncycles, "...")
        print("###################################################################################")

        if (MCMC_language=="Nimble")
        {chains.to.update<-setdiff(1:Nchains,chains.to.remove)
          if (!control.MCMC$parallelize)
          {
            for (i in chains.to.update)
            {message("      Running chain ", i, ", with ", niter, " iterations...")
			  if (length(control$seed)>0)
              {set.seed(control$seed+i+Ncycles*Nchains)}
              if (!control.MCMC$APT) {CModelMCMC[[i]]$run(niter, thin=thin, progressBar =TRUE,reset=FALSE,resetMV=as.logical(max(control.MCMC$resetMV,resetMV.temporary)))
                #if (! control.MCMC$resetMV) {samplesList[[i]] <- as.matrix(CModelMCMC[[i]]$mvSamples)}
				##NB: allocation of values of CModelMCMC[[i]] to samplesList done a dozen of lines below
				} else {

                  nIter   <- niter
                  CModelMCMC[[i]]$thinPrintTemps<-eval(control.MCMC$APT.thinPrintTemps)
                  CModelMCMC[[i]]$run(nIter,thin=thin,
                                      reset          = FALSE, ## Do not reset the adaptive MCMC, let adaptation continue as it is
                                      adaptTemps     = TRUE,  ## Allows temperature ladder to adjust
                                      resetTempering = FALSE, ## Keeps the adjustments modest so avoids volatile behaviour. Setting to TRUE can make the temperature ladder make vary large changes
                                      printTemps     = TRUE,  ## Prevents verbose printing of temperature ladder updates
                                      progressBar    = TRUE,
                                      resetMV        = FALSE,
                                      tuneTemper1=control.MCMC$APT.tuneTemps[1], tuneTemper2=control.MCMC$APT.tuneTemps[2])
                  samplesList[[i]] <- as.matrix(CModelMCMC[[i]]$mvSamples)
                  TempsList[[i]]<-rbind(TempsList[[i]],CModelMCMC[[i]]$Temps)
                }
            }

            if (!control.MCMC$APT) {

              samplesList[chains.to.update]<- lapply(chains.to.update,function(x){rbind(as.matrix(samplesList[[x]]),if(as.logical(max(control.MCMC$resetMV,resetMV.temporary))){as.matrix(CModelMCMC[[x]]$mvSamples)}else{as.matrix(CModelMCMC[[x]]$mvSamples)[-(1:mvSamples.previous),,drop=FALSE]})})
              samplesList[chains.to.remove]<-lapply(chains.to.remove,function(x){samplesList[[chains.to.update[1]]]})
			  samplesList <- window(coda::as.mcmc.list(lapply(samplesList, function(x){coda::as.mcmc(as.matrix(x))})))
			  mvSamples.previous<-dim(as.matrix(CModelMCMC[[1]]$mvSamples))[1]

            } else  { #then control.MCMC$APT : for the moment no possibility of resetMV in this: hence old formula
              samplesList[chains.to.remove]<-lapply(chains.to.remove,function(x){samplesList[[chains.to.update[1]]]})
			  samplesList <- window(coda::as.mcmc.list(lapply(samplesList, function(x){coda::as.mcmc(as.matrix(x))})),start=3)
              mvSamples.previous<-dim(as.matrix(CModelMCMC[[1]]$mvSamples))[1]
            }

          } else {#then: control.MCMC$parallelize
            message("      Running ", Nchains, " chains in parallel, with ", niter, " iterations...")

            parallel::clusterExport(cl, c("Ncycles","niter", "thin", "nburnin","resetMV.temporary","mvSamples.previous"),envir=environment())
            out1 <- parallel::clusterEvalQ(cl[chains.to.update], {
              #if (length(control$seed)>0)
              #{set.seed(control$seed+clusterNumber+Ncycles*Nchains)}
              if (!control.MCMC$APT) {CModelMCMC[[i]]$run(niter, thin = thin, progressBar =TRUE,reset = FALSE,resetMV=as.logical(max(control.MCMC$resetMV,resetMV.temporary)))
                 samplesList<- coda::as.mcmc(rbind(as.matrix(samplesList),if(as.logical(max(control.MCMC$resetMV,resetMV.temporary))){as.matrix(CModelMCMC[[i]]$mvSamples)}else{as.matrix(CModelMCMC[[i]]$mvSamples)[-(1:mvSamples.previous),,drop=FALSE]}))


                return(list(samplesList,dim(as.matrix(CModelMCMC[[1]]$mvSamples))[1]))} else {
                  nIter   <- niter
                  #CModelMCMC[[i]]$thin<-thin
                  #CModelMCMC[[i]]$thin2<-thin
                  CModelMCMC[[i]]$thinPrintTemps<-eval(control.MCMC$APT.thinPrintTemps)
                  CModelMCMC[[i]]$run(nIter,thin=thin,
                                      reset          = FALSE, ## Do not reset the adaptive MCMC, let adaptation continue as it is
                                      adaptTemps     = TRUE,  ## Allows temperature ladder to adjust
                                      resetTempering = FALSE, ## Keeps the adjustments modest so avoids volatile behaviour. Setting to TRUE can make the temperature ladder make vary large changes
                                      printTemps     = TRUE,  ## Prevents verbose printing of temperature ladder updates
                                      progressBar    = TRUE,
                                      resetMV        = FALSE,
                                      tuneTemper1=control.MCMC$APT.tuneTemps[1], tuneTemper2=control.MCMC$APT.tuneTemps[2])
                  #if (! control.MCMC$resetMV) {samplesList <- coda::as.mcmc(as.matrix(CModelMCMC[[i]]$mvSamples))}
                  #if (control.MCMC$resetMV) {samplesList<- coda::as.mcmc(rbind(as.matrix(samplesList),as.matrix(CModelMCMC[[i]]$mvSamples)))}
                  samplesList <- coda::as.mcmc(as.matrix(CModelMCMC[[i]]$mvSamples))

                  return(list(samplesList,CModelMCMC[[i]]$Temps,dim(as.matrix(CModelMCMC[[i]]$mvSamples))[1]))
                }

              #return(coda::as.mcmc(as.matrix(coda::as.mcmc.list(CModelMCMC[[i]]$mvSamples))))
              #return(as.matrix(CModelMCMC[[i]]$mvSamples))

              gc(verbose = FALSE)
            })
            if (!control.MCMC$APT)
              {
              samplesList[chains.to.update] <- coda::as.mcmc.list(lapply(out1, function(x){coda::as.mcmc(as.matrix(x[[1]]))}))
			  samplesList[chains.to.remove]<-lapply(chains.to.remove,function(x){samplesList[[chains.to.update[1]]]})
			  samplesList <- window(coda::as.mcmc.list(lapply(samplesList, function(x){coda::as.mcmc(as.matrix(x))})))
			  mvSamples.previous<-out1[[1]][[2]]

              } else  {#then control.MCMC$APT : for the moment possibility of resetMV in this not used: hence old formula
              samplesList[chains.to.update] <- coda::as.mcmc.list(lapply(out1, function(x){coda::as.mcmc(as.matrix(x[[1]]))}))
			  samplesList[chains.to.remove]<-lapply(chains.to.remove,function(x){samplesList[[chains.to.update[1]]]})
			  samplesList <- window(coda::as.mcmc.list(lapply(samplesList, function(x){coda::as.mcmc(as.matrix(x))})),start=3)
              mvSamples.previous<-out1[[1]][[3]]
              TempsList<-lapply(1:Nchains,function(x){rbind(TempsList[[x]],out1[[x]][[2]])})
              }
          }   ## END: if (!control.MCMC$parallelize)
        }	## END: MCMC_language=="Nimble"

        if (MCMC_language=="Jags")
        {
          if (!control.MCMC$parallelize)
          { resultatt<-rjags::coda.samples(myModel,var=params, n.iter = niter, thin = thin)
          #matres <- rbind(matres,as.matrix(resultatt))
          samplesList<- coda::as.mcmc.list(lapply(seq_len(length(samplesList)),function(x){coda::as.mcmc(rbind(as.matrix(samplesList[[x]]),as.matrix(resultatt[[x]])))}))
          } else {  ## then: control.MCMC$parallelize
            message("      Running ", Nchains, " chains in parallel, with ", niter," iterations...")
            parallel::clusterExport(cl, c("niter", "thin"),envir=environment())
            out1 <- parallel::clusterEvalQ(cl, {
              resultatt<-rjags::coda.samples(myModel,var=params, n.iter = niter, thin = thin)

              return(resultatt)
              gc(verbose = FALSE)
            })
            samplesList<- coda::as.mcmc.list(lapply(seq_len(length(samplesList)),function(x){coda::as.mcmc(rbind(as.matrix(samplesList[[x]]),as.matrix(out1[[x]])))}))

          }   ## END: if (!control.MCMC$parallelize)
        }		## END: MCMC_language=="Jags"

        if (MCMC_language=="Greta")
        {
          samplesList<-m<-greta::extra_samples(m, n_samples=niter, thin=thin, n_cores=control.MCMC$n_cores, pb_update=(floor(50/thin)+1*as.double((50/thin-floor(50/thin))>0))*thin)
        }		## END: MCMC_language=="Greta"
        ## adding this line to make SamplesList of standard format (thin=1 & burnin=1 in it)
        samplesList<- coda::as.mcmc.list(lapply(samplesList,function(x){coda::as.mcmc(as.matrix(x))}))
      }) ## END: current.CPU.time<-system.time({
      ##updating time.MCMC
      time.MCMC.duration<-(Sys.time()-time.MCMC0)
      units(time.MCMC.duration)<-"secs"
      time.MCMC<-time.MCMC+time.MCMC.duration
      time.MCMC.num<-as.double(time.MCMC)
      CPUtime.MCMC<-CPUtime.MCMC+current.CPU.time[1]+current.CPU.time[2]+ifelse(is.na(current.CPU.time[4]),0, current.CPU.time[4])+ifelse(is.na(current.CPU.time[5]),0, current.CPU.time[5])
      childCPUtime.MCMC<-childCPUtime.MCMC+current.CPU.time[4]+current.CPU.time[5]

      current.CPU.time<-system.time({
        ##updating counters around samplesList & niter
        numIter.samplesList<-c(numIter.samplesList,rep(thin,dim(samplesList[[1]])[1]-size.samplesList))
        size.samplesList<-dim(samplesList[[1]])[1]
        resetMV.previous<-as.logical(max(control.MCMC$resetMV,resetMV.temporary))
        niter.tot.previous<-niter.tot
        niter.tot<-niter.tot+niter


        ########################################
        ###2.3- Reshaping samplesList so that the spacing between values is roughly of thin: result in samplesList.temp
        ########################################

        index.conv0<-1

        ### we will start from the following number of iterations and go backwards: motivation: to be sure to have all the thin values in case conbtrol$roundthinmult; this is not sure in the reverse order
        numIter.sum<-sum(numIter.samplesList)
        numIter.sum.conv<-sum(numIter.samplesList[1:index.conv])
        iters.ref<-rev(seq(from=numIter.sum,to=numIter.sum.conv,by=-thin))
        numIter.cumsum<-cumsum(numIter.samplesList)
        indices.samplesList<-iters.ref
        for (i in 1:length(iters.ref))
        {
          indices.samplesList[i]<-which.min(abs(numIter.cumsum-iters.ref[i]))
        }

        ## transferring the part of samplesList corresponding to indices.samplesList to samplesList.temp
        samplesList.temp<-samplesList
        ## associated index.conv is 1 by definition
        index.conv.temp<-1
        index.conv0.temp<-index.conv.temp
        for (i in 1:Nchains)
        {
          samplesList.temp[[i]]<-samplesList[[i]][indices.samplesList,,drop=FALSE]
        }

        samplesList.temp <- coda::as.mcmc.list(lapply(samplesList.temp, function(x){coda::as.mcmc(as.matrix(x))}))

        if (length(chains.to.remove)>0)
        {samplesList.temp<-coda::as.mcmc.list(samplesList.temp[-chains.to.remove])}
        else
        {samplesList.temp<-samplesList.temp}
        size.samplesList.temp<-dim(as.matrix(samplesList.temp[[1]]))[1]

        ########################################
        ###2.4- diagnostics based on new model
        ########################################
        index.conv.local<-indices.samplesList[index.conv.temp]
        diags<-conveff(window(samplesList.temp[,indices.conv],start=index.conv.temp,end=size.samplesList.temp),control$print.diagnostics)

        ########################################
        ###2.4.1- interpreting convergence of next runs (done only if not already converged or if we have to control$recheck.convergence or control$conv.thorough.check):
        ########################################

        if (!converged|control$recheck.convergence|control$conv.thorough.check)
        {
          if (!control$conv.thorough.check)
          {index.props.conv<-1
          index.conv.new<-floor(quantile(index.conv0.temp:size.samplesList.temp,control$props.conv[index.props.conv]))
          index.conv.new0<-indices.samplesList[index.conv.new]
          ##in case has not converged, re-checks convergence at index.conv.new if acceptable in terms of
          ## (i) nburnin.max, (ii) control$props.conv[index.props.conv] & (iii) number of remaining values (must be >min.Nvalues)
          while (!checking.convergence(diags,control$convtype,control$convtype.Gelman,conv.max,conv.med,conv.mean)&((nburnin.min+sum(numIter.samplesList[1:(index.conv.new0-1)]))<nburnin.max)&(control$props.conv[index.props.conv]<1)&((length(index.conv.new:size.samplesList.temp)*Nchains.updated)>min.Nvalues))
          {
            index.conv.temp<-index.conv.new
            index.conv.local<-indices.samplesList[index.conv.temp]
            nburnin<-nburnin.min+sum(numIter.samplesList[1:(index.conv.local-1)])
            diags<-conveff(window(samplesList.temp[,indices.conv],start=index.conv.temp,end=size.samplesList.temp),control$print.diagnostics)
            index.props.conv<-index.props.conv+1
            index.conv.new<-floor(quantile(index.conv0.temp:size.samplesList.temp,control$props.conv[index.props.conv]))
            index.conv.new0<-indices.samplesList[index.conv.new]
          }
          } else {##control$conv.thorough.check
            diags.collection<-list(diags)
            for (index.props.conv in 1:length(control$props.conv))
            {
              index.conv.new<-floor(quantile(index.conv0.temp:size.samplesList.temp,control$props.conv[index.props.conv]))
              index.conv.local<-indices.samplesList[index.conv.new]
              if (((nburnin.min+sum(numIter.samplesList[1:(index.conv.local-1)]))<nburnin.max)&(control$props.conv[index.props.conv]<1)&((length(index.conv.new:size.samplesList.temp)*Nchains.updated)>min.Nvalues))
              {
                diags.collection<-c(diags.collection,list(conveff(window(samplesList.temp[,indices.conv],start=index.conv.new,end=size.samplesList.temp),control$print.diagnostics)))
              }
            }
            ### determine best index.props.conv ::: 1- convergence; 2- best overallneff within category of conv.??
            analysisconv.collection<-sapply(diags.collection,function(x){checking.convergence(x,control$convtype,control$convtype.Gelman,conv.max,conv.med,conv.mean)})
            analysisneff.collection<-sapply(diags.collection,function(x){scale.available.neffs(x,neff.min,neff.med,neff.mean)})
            indicesconv<-(1:length(analysisconv.collection))[analysisconv.collection]
            if (length(indicesconv)>0)
            {indexopt<-indicesconv[which(analysisneff.collection[indicesconv]==max(analysisneff.collection[indicesconv]))[1]]
            } else
            {indexopt<-(1:length(analysisconv.collection))[which(analysisneff.collection==max(analysisneff.collection))[1]]
            }
            if (indexopt>1)
            {
              index.conv.new<-floor(quantile(index.conv0.temp:size.samplesList.temp,control$props.conv[indexopt-1]))
              diags<-diags.collection[[indexopt]]
            }
            else
            {
              index.conv.new<-index.conv0.temp
              diags<-diags.collection[[1]]
            }
            print (paste0("Retained case with Nvalues=",diags$Nvalues," and nu.burn=",diags$nu.burn))
            print("###################################################################################")
            index.conv.new0<-indices.samplesList[index.conv.new]
            index.conv.temp<-index.conv.new
            index.conv.local<-indices.samplesList[index.conv.temp]
            nburnin<-nburnin.min+sum(numIter.samplesList[1:(index.conv.new0-1)])
          }

          ##final convergence diagnostic:
          converged<-checking.convergence(diags,control$convtype,control$convtype.Gelman,conv.max,conv.med,conv.mean)
          previously.converged<-as.logical(max(previously.converged,converged))
          if (converged) {index.conv<-indices.samplesList[index.conv.temp]}
          ## changing converged state in cases where we should not have checked convergence at first run, not checked convergence at all or if iterations above nburnin.max
          ###FG: inactivated 2023/03/11: I don't understand it : if ((nburnin.min+sum(numIter.samplesList[1:(index.conv-1)]))>=nburnin.max) {converged<-TRUE}
        }	## END if (!converged|control$recheck.convergence|control$conv.thorough.check)

        ########################################
        ###2.4.2. in case of convergence, calculating thinmult for future update of thin
        ########################################
        thinmult<-1
        if (converged)
        {
          if ((length(index.conv.temp:size.samplesList.temp)*Nchains.updated)>min.Nvalues)
          {
            ## first checking if neffs reached
            neffs.reached<-checking.neffs.reached(diags,neff.min,neff.med,neff.mean)
            ## second, if neffs not reached, calculates multiplier of thin to be targeted
            if (!neffs.reached)
            {
              thinmult<-min(calculate.thinmult.target(diags,!control$only.final.adapt.thin,neff.min,neff.med,neff.mean),thin.max/thin)
              if (control$print.thinmult)
              {
                print(paste("Raw multiplier of thin: ", formatC_adapted(thinmult)))
                print("###################################################################################")
              }
              if (thinmult<control$min.thinmult){thinmult<-1}
              if (control$round.thinmult) {thinmult<-round(thinmult)}
              thinmult.proposed<-thinmult

              if(control$check.thinmult==3)
              {
                ########################################
                ### 2.4.2.1: check.thinmult==3: Going through thinning levels in decreasing order from thinmult, giving retained value of thinmult
                ########################################
                stop_decrease<-FALSE
                while (thinmult>1&!stop_decrease)
                {
                  if (control$print.thinmult)
                  {
                    print(paste("Testing multiplier of thin: ", formatC_adapted(thinmult),":"))
                  }
                  diagstemp<-conveff(window.seq(samplesList.temp[,indices.conv],start=index.conv.temp,end=size.samplesList.temp,thin=round(thinmult)),control$print.diagnostics)
                  convergedtemp<-	checking.convergence(diagstemp,control$convtype,control$convtype.Gelman,conv.max,conv.med,conv.mean)
                  neffs.reachedtemp<-ifelse(neffs.reached,checking.neffs.reached(diagstemp,neff.min,neff.med,neff.mean),TRUE)
                  neffs.conserved<-checking.neffs.conserved(diagstemp,diags,control$max.prop.decr.neff,neff.min,neff.med,neff.mean,neffs.reached)
                  min.Nvalues.OK<-(dim(as.matrix(window.seq(samplesList.temp[,indices.conv],start=index.conv.temp,end=size.samplesList.temp,thin=round(thinmult))))[1])>min.Nvalues
                  stop_decrease<-convergedtemp&neffs.reachedtemp&neffs.conserved&min.Nvalues.OK
                  thinmult.prev<-thinmult
                  thinmult<-ifelse(thinmult>control$decrease.thinmult.threshold, max(floor(control$decrease.thinmult.multiplier*thinmult), control$decrease.thinmult.threshold),thinmult-1)
                }
                if (stop_decrease) {diags<-diagstemp; thinmult<-thinmult.prev}

                if (control$print.thinmult)
                {
                  print(paste("Retained multiplier of thin: ", formatC_adapted(thinmult),":"))
                  print("###################################################################################")
                }

              } else { if(control$check.thinmult==2)
              {
                ########################################
                ### 2.4.2.2: check.thinmult==2: Going through thinning levels in decreasing order from thinmult, giving retained value of thinmult
                ########################################
                stop_decrease<-FALSE
                while (thinmult>1&!stop_decrease)
                {
                  if (control$print.thinmult)
                  {
                    print(paste("Testing multiplier of thin: ", formatC_adapted(thinmult),":"))
                  }
                  diagstemp<-conveff(window.seq(samplesList.temp[,indices.conv],start=index.conv.temp,end=size.samplesList.temp,thin=round(thinmult)),control$print.diagnostics)
                  convergedtemp<-	checking.convergence(diagstemp,control$convtype,control$convtype.Gelman,conv.max,conv.med,conv.mean)
                  neffs.reachedtemp<-ifelse(neffs.reached,checking.neffs.reached(diagstemp,neff.min,neff.med,neff.mean),TRUE)
                  #neffs.conserved<-checking.neffs.conserved(diagstemp,diags,control$max.prop.decr.neff,neff.min,neff.med,neff.mean,neffs.reached)
                  min.Nvalues.OK<-(dim(as.matrix(window.seq(samplesList.temp[,indices.conv],start=index.conv.temp,end=size.samplesList.temp,thin=round(thinmult))))[1])>min.Nvalues
                  stop_decrease<-convergedtemp&neffs.reachedtemp&min.Nvalues.OK
                  thinmult.prev<-thinmult
                  thinmult<-ifelse(thinmult>control$decrease.thinmult.threshold, max(floor(control$decrease.thinmult.multiplier*thinmult), control$decrease.thinmult.threshold),thinmult-1)
                }
                if (stop_decrease) {diags<-diagstemp; thinmult<-thinmult.prev}

                if (control$print.thinmult)
                {
                  print(paste("Retained multiplier of thin: ", formatC_adapted(thinmult),":"))
                  print("###################################################################################")
                }

              } else { #control$check.thinmult==1

                ########################################
                ### 2.4.2.3: check.thinmult==1: checking condition min.Nvalues condition & calculating - approximate if !control$round.thinmult - convergence and neffs.reached with thinmult (if thinmult>1)
                ########################################
                stop_decrease<-FALSE
                while (thinmult>1&!stop_decrease)
                {
                  if (control$print.thinmult)
                  {
                    print(paste("Testing multiplier of thin: ", formatC_adapted(thinmult),":"))
                  }
                  min.Nvalues.OK<-(dim(as.matrix(window.seq(samplesList.temp[,indices.conv],start=index.conv.temp,end=size.samplesList.temp,thin=round(thinmult))))[1])>min.Nvalues
                  stop_decrease<-min.Nvalues.OK
                  thinmult.prev<-thinmult
                  thinmult<-ifelse(thinmult>control$decrease.thinmult.threshold, max(floor(control$decrease.thinmult.multiplier*thinmult), control$decrease.thinmult.threshold),thinmult-1)
                }
                if (stop_decrease) {thinmult<-thinmult.prev}
                if (control$print.thinmult)
                {
                  print(paste("Retained multiplier of thin: ", formatC_adapted(thinmult),":"))
                  print("###################################################################################")
                }
                if (stop_decrease) {
                  diags<-conveff(window.seq(samplesList.temp[,indices.conv],start=index.conv.temp,end=size.samplesList.temp,thin=round(thinmult)),control$print.diagnostics)
                  converged<-	checking.convergence(diags,control$convtype,control$convtype.Gelman,conv.max,conv.med,conv.mean)
                  neffs.reached<-checking.neffs.reached(diags,neff.min,neff.med,neff.mean)
                  }

              }
              } #END: if(control$check.thinmult==...)
            }
          }

        }

        #print(paste("underlying indices: ",index.conv.temp,", ",size.samplesList.temp))
      }) ## END: current.CPU.time<-system.time({
      CPUtime.btadjust<-CPUtime.btadjust+current.CPU.time[1]+current.CPU.time[2]+ifelse(is.na(current.CPU.time[4]),0, current.CPU.time[4])+ifelse(is.na(current.CPU.time[5]),0, current.CPU.time[5])
      childCPUtime.btadjust<-childCPUtime.btadjust+current.CPU.time[4]+current.CPU.time[5]
    } ## END: if (niter>=thin)
  }	##END of while((!converged |!neffs.reached|force.niter.max)&niter>=thin)


  print("###################################################################################")
  print("Main MCMC sampling finished.")
  print("###################################################################################")



    ########################################
    ### 2.5- Testing size reduction due to much higher Nvalues than neffs & potentially reshaping samplesList accordingly in case converged: otherwise: much too big object
    ### only done here for converged situations as for non convergence cases this is treated directly at the end
    ########################################
    current.CPU.time<-system.time({

      if (converged) {

        ########################################
        ### 2.5.1: specifying the new thinmult level
        ########################################
        ###here only take the rounded version to ease work with window

        diags<-conveff(window.seq(samplesList.temp[,indices.conv],start=index.conv.temp,end=size.samplesList.temp,thin=1),control$print.diagnostics)
        thinmult<-min(calculate.thinmult.target(diags,TRUE,neff.min,neff.med,neff.mean),thin.max/thin)
        if (control$print.thinmult)
        {
          print(paste("Final max raw multiplier of thin: ", formatC_adapted(thinmult)))
          print("###################################################################################")
        }
        if (thinmult<control$min.thinmult){thinmult<-1}
        if (control$round.thinmult) {thinmult<-round(thinmult)}

          ########################################
          ### 2.5.2: Going through thinning levels in decreasing order from thinmult, giving retained value of thinmult
          ### we here take the same conditions as above in case control$check.thinmult==3 (2.4.2.1) but with the addition that if !neffs.reached, in minValues, we add 2*min(c(neff.min,neff.med,neff.mean),na.rm=TRUE) as in case of non-convergence
          ########################################
          stop_decrease<-FALSE
          refNvalues<-dim(as.matrix(window(samplesList.temp[,indices.conv],start=index.conv.temp,end=size.samplesList.temp)))[1]
          while (thinmult>1&!stop_decrease)
          {if (control$print.thinmult)
          {
            print(paste("Testing final multiplier of thin: ", formatC_adapted(thinmult),":"))
          }
            diagstemp<-conveff(window.seq(samplesList.temp[,indices.conv],start=index.conv.temp,end=size.samplesList.temp,thin=round(thinmult)),control$print.diagnostics)
            convergedtemp<-	checking.convergence(diagstemp,control$convtype,control$convtype.Gelman,conv.max,conv.med,conv.mean)
            neffs.reachedtemp<-ifelse(neffs.reached,checking.neffs.reached(diagstemp,neff.min,neff.med,neff.mean),TRUE)
            neffs.conserved<-checking.neffs.conserved(diagstemp,diags,control$max.prop.decr.neff,neff.min,neff.med,neff.mean,neffs.reached)
            min.Nvalues.OK<-(dim(as.matrix(window.seq(samplesList.temp[,indices.conv],start=index.conv.temp,end=size.samplesList.temp,thin=round(thinmult))))[1])>=max(min.Nvalues,ifelse(neffs.reached,0,min(refNvalues,2*min(c(neff.min,neff.med,neff.mean),na.rm=TRUE))))
            stop_decrease<-convergedtemp&neffs.reachedtemp&ifelse(control$check.thinmult==3,neffs.conserved,TRUE)&min.Nvalues.OK
            thinmult.prev<-thinmult
            thinmult<-ifelse(thinmult>control$decrease.thinmult.threshold, max(floor(control$decrease.thinmult.multiplier*thinmult), control$decrease.thinmult.threshold),thinmult-1)
          }
          if (stop_decrease) {thinmult<-thinmult.prev}

          if (control$print.thinmult)
          {
            print(paste("Retained final multiplier of thin: ", formatC_adapted(thinmult)))
            print("###################################################################################")
          }

          thin<-update.thin(thin,thinmult,thin.max,control$round.thinmult)


        ########################################
        ###  2.5.3- Reshaping samplesList so that the spacing between values is roughly of thin: result in samplesList.temp
        ########################################

          index.conv0<-1

          ### we will start from the following number of iterations and go backwards: motivation: to be sure to have all the thin values in case conbtrol$roundthinmult; this is not sure in the reverse order
          numIter.sum<-sum(numIter.samplesList)
          numIter.sum.conv<-sum(numIter.samplesList[1:index.conv])
          iters.ref<-rev(seq(from=numIter.sum,to=numIter.sum.conv,by=-thin))
          numIter.cumsum<-cumsum(numIter.samplesList)
          indices.samplesList<-iters.ref
          for (i in 1:length(iters.ref))
          {
            indices.samplesList[i]<-which.min(abs(numIter.cumsum-iters.ref[i]))
          }

          ## tranferring the part of samplesList corresponding to indices.samplesList to samplesList.temp
          samplesList.temp<-samplesList
          ## associated index.conv is 1 by definition
          index.conv.temp<-1
          index.conv0.temp<-index.conv.temp
          for (i in 1:Nchains)
          {
            samplesList.temp[[i]]<-samplesList[[i]][indices.samplesList,,drop=FALSE]
          }

          samplesList.temp <- coda::as.mcmc.list(lapply(samplesList.temp, function(x){coda::as.mcmc(as.matrix(x))}))

          if (length(chains.to.remove)>0)
          {samplesList.temp<-coda::as.mcmc.list(samplesList.temp[-chains.to.remove])}
          else
          {samplesList.temp<-samplesList.temp}
          size.samplesList.temp<-dim(as.matrix(samplesList.temp[[1]]))[1]

          index.conv.local<-indices.samplesList[index.conv.temp]


      } #END if (converged) (2.5)
    }) ## END: current.CPU.time<-system.time({
    CPUtime.btadjust<-CPUtime.btadjust+current.CPU.time[1]+current.CPU.time[2]+ifelse(is.na(current.CPU.time[4]),0, current.CPU.time[4])+ifelse(is.na(current.CPU.time[5]),0, current.CPU.time[5])
    childCPUtime.btadjust<-childCPUtime.btadjust+current.CPU.time[4]+current.CPU.time[5]




  ########################################
  ###2.6- In case Nimble and control.MCMC$WAIC, online calculus of WAIC
  ########################################
  #### NB: here we choose the online version of WAIC which is more demanding in terms of calculus yet richer that offline (e.g. allows marginalization)
  #### furthermore, a 14/04/24 try with offline WAIC did not work well with $calculateWAIC (cf. Onenote Note)
  current.CPU.time<-system.time({
    time.MCMC0<-Sys.time()
    WAIC.results<-NULL
    if (control.MCMC$WAIC & MCMC_language=="Nimble" & converged) {

      print("###################################################################################")
      print("Performing extra MCMC sampling for WAIC calculation")
      print("###################################################################################")


      if (control$thinmult.in.resetMV.temporary)
      {
        resetMV.temporary<-!converged|(nburnin>niter.tot.previous)|thinmult>1
      } else {
        resetMV.temporary<-!converged|(nburnin>niter.tot.previous)
      }

      if (!control.MCMC$parallelize)
      {WAIC.results<-lapply (chains.to.update[1:length(control.MCMC$WAIC.control)],function(i)
      {
        if (length(control$seed)>0)
        {set.seed(control$seed+i+(Ncycles+1)*Nchains)}
        #change 1: added resetMV in the following:
        if (!control.MCMC$APT)
          {
          if (as.logical(max(control.MCMC$resetMV,resetMV.temporary))|(control.MCMC$WAIC.Nsamples>mvSamples.previous))
            {
              message("      Running chain ", i, ", with ", (control.MCMC$WAIC.Nsamples-ifelse(as.logical(max(control.MCMC$resetMV,resetMV.temporary)),0,mvSamples.previous))*thin, " iterations for WAIC calculation...")
              CModelMCMC[[i]]$run((control.MCMC$WAIC.Nsamples-ifelse(as.logical(max(control.MCMC$resetMV,resetMV.temporary)),0,mvSamples.previous))*thin, thin=thin, progressBar =TRUE,reset=FALSE,resetMV=as.logical(max(control.MCMC$resetMV,resetMV.temporary)))

              list(WAIC=CModelMCMC[[i]]$getWAIC(),WAIC.details=CModelMCMC[[i]]$getWAICdetails())}
          } else {# control.MCMC$APT

          nIter   <- control.MCMC$WAIC.Nsamples*thin
          CModelMCMC[[i]]$run(nIter,thin=thin,
                              reset          = FALSE, ## Do not reset the adaptive MCMC, let adaptation continue as it is
                              adaptTemps     = FALSE,  ## Does not allows temperature ladder to adjust
                              resetTempering = FALSE, ## Keeps the adjustments modest so avoids volatile behaviour. Setting to TRUE can make the temperature ladder make vary large changes
                              printTemps     = FALSE,  ## Prevents verbose printing of temperature ladder updates
                              progressBar    = TRUE,
                              resetMV        = TRUE,
                              tuneTemper1=control.MCMC$APT.tuneTemps[1], tuneTemper2=control.MCMC$APT.tuneTemps[2])

          list(WAIC=CModelMCMC[[i]]$calculateWAIC())
          }
        })
      } else {
        message("      Running ", Nchains.updated, " chains in parallel for WAIC calculation...")
        parallel::clusterExport(cl, c("chains.to.update","Ncycles","niter", "thin", "control","control.MCMC","resetMV.temporary","mvSamples.previous"),envir=environment())
        WAIC.results <- parallel::clusterEvalQ(cl[chains.to.update[1:length(control.MCMC$WAIC.control)]], {
          #if (length(control$seed)>0)
          #{set.seed(control$seed+clusterNumber+(Ncycles+1)*Nchains)}
          #change 1: added resetMV in the following:
             if (!control.MCMC$APT)
              {
                if (as.logical(max(control.MCMC$resetMV,resetMV.temporary))|(control.MCMC$WAIC.Nsamples>mvSamples.previous))
                  {
                    CModelMCMC[[i]]$run((control.MCMC$WAIC.Nsamples-ifelse(as.logical(max(control.MCMC$resetMV,resetMV.temporary)),0,mvSamples.previous))*thin, thin = thin, progressBar =TRUE,reset = FALSE,resetMV=as.logical(max(control.MCMC$resetMV,resetMV.temporary)))
                  }
                return(list(WAIC=CModelMCMC[[i]]$getWAIC(),WAIC.details=CModelMCMC[[i]]$getWAICdetails()))
              } else {#control.MCMC$APT
                nIter   <- control.MCMC$WAIC.Nsamples*thin
                #CModelMCMC[[i]]$thin<-thin
                #CModelMCMC[[i]]$thin2<-thin
                CModelMCMC[[i]]$run(nIter,thin=thin,
                                    reset          = FALSE, ## Do not reset the adaptive MCMC, let adaptation continue as it is
                                    adaptTemps     = TRUE,  ## Allows temperature ladder to adjust
                                    resetTempering = FALSE, ## Keeps the adjustments modest so avoids volatile behaviour. Setting to TRUE can make the temperature ladder make vary large changes
                                    printTemps     = FALSE,  ## Prevents verbose printing of temperature ladder updates
                                    progressBar    = TRUE,
                                    resetMV        = TRUE,
                                    tuneTemper1=control.MCMC$APT.tuneTemps[1], tuneTemper2=control.MCMC$APT.tuneTemps[2])

                return(list(WAIC=CModelMCMC[[i]]$calculateWAIC()))


              }

          gc(verbose = FALSE)
        })
        WAIC.results<-WAIC.results[1:length(control.MCMC$WAIC.control)]
      }


    } #END if (control.MCMC$WAIC & MCMC_language=="Nimble") (2.6)
    time.MCMC.after<-(Sys.time()-time.MCMC0)
    units(time.MCMC.after)<-"secs"

  }) ## END: current.CPU.time<-system.time({
  CPUtime.MCMC.after<-CPUtime.MCMC.after+current.CPU.time[1]+current.CPU.time[2]+ifelse(is.na(current.CPU.time[4]),0, current.CPU.time[4])+ifelse(is.na(current.CPU.time[5]),0, current.CPU.time[5])
  childCPUtime.MCMC.after<-childCPUtime.MCMC.after+current.CPU.time[4]+current.CPU.time[5]

  ########################################
  ###2.7- Performing extra calculations defined in control.MCMC$extraCalculations
  ########################################
  current.CPU.time<-system.time({
    time.MCMC0<-Sys.time()
    if (length(control.MCMC$extraCalculations)!=0)
    {
      print("###################################################################################")
      print("Performing extra calculations")
      print("###################################################################################")
    }
    extraResults<-try(eval(control.MCMC$extraCalculations))
    time.MCMC.after<-(Sys.time()-time.MCMC0)
    units(time.MCMC.after)<-"secs"

  }) ## END: current.CPU.time<-system.time({
  CPUtime.MCMC.after<-CPUtime.MCMC.after+current.CPU.time[1]+current.CPU.time[2]+ifelse(is.na(current.CPU.time[4]),0, current.CPU.time[4])+ifelse(is.na(current.CPU.time[5]),0, current.CPU.time[5])
  childCPUtime.MCMC.after<-childCPUtime.MCMC.after+current.CPU.time[4]+current.CPU.time[5]

  current.CPU.time<-system.time({
    if (MCMC_language=="Nimble") {
      ########################################
      ###3. uncompiling Nimble objects: https://groups.google.com/g/nimble-users/c/-eoYs__eg0o
      ### Only useful outside Windows
      ########################################
      ### Only useful if MCMC_language=="Nimble"
      if (Sys.info()["sysname"]!="Windows") {
        for (i in 1:Nchains)
        {try(nimble::clearCompiled(CModelMCMC[[i]]),silent=TRUE)
          try(nimble::clearCompiled(Model[[i]]),silent=TRUE)
        }
        try(nimble::clearCompiled(Modeltemp),silent=TRUE)
        try({rm(CModelMCMC)})
        try({rm(Modeltemp)})
      }
    }	## END: MCMC_language=="Nimble"


    if (control.MCMC$parallelize&exists("cl")) {
      ########################################
      ###3.1 removing clusters
      ########################################
      parallel::stopCluster(cl)
    }	## END: MCMC_language=="Nimble"


    ########################################
    ####4. changing names of MCMC parameters
    ########################################

    ### this is done so that names of parameters are coherent between MCMC.languages
    ### there is especially a discrepancy between Nimble & Greta in the names of elements of matrices (adding or not a space after commas)
    ### not done if only one parameter because name can be lost and no matrix format
    for (i in seq_len(length(samplesList.temp)))
    {if (length(dim(samplesList.temp[[i]]))>0)
    {dimnames(samplesList.temp[[i]])[[2]]<-gsub(", ",",",dimnames(samplesList.temp[[i]])[[2]])}
      ## not done because name lost anyway in this case: else		{names(samplesList.temp[[i]])<-params[1]}
    }



    ########################################
    ####5. assigning the results
    ########################################

    if (!converged) warning("The MCMC did not converge")
    if (!neffs.reached) warning("The expected effective sample size was not reached")


    if (converged)
    {result<-window(samplesList.temp[,indices.save],start=index.conv.temp,end=size.samplesList.temp)} else
    {result<-samplesList.temp[,indices.save,drop=FALSE]}
  }) ## END: current.CPU.time<-system.time({
  CPUtime.btadjust<-CPUtime.btadjust+current.CPU.time[1]+current.CPU.time[2]+ifelse(is.na(current.CPU.time[4]),0, current.CPU.time[4])+ifelse(is.na(current.CPU.time[5]),0, current.CPU.time[5])
  childCPUtime.btadjust<-childCPUtime.btadjust+current.CPU.time[4]+current.CPU.time[5]
  total.duration<-Sys.time()-time.start
  units(total.duration)<-"secs"

  final.diags<-if (converged){index.conv.local<-indices.samplesList[index.conv.temp];conveff_final(window(samplesList.temp,start=index.conv.temp,end=size.samplesList.temp),indices.conv,control$conveff.final.allparams,control$print.diagnostics)} else {index.conv.local<-indices.samplesList[index.conv.temp];conveff_final(window(samplesList.temp),indices.conv,control$conveff.final.allparams,control$print.diagnostics)}

  original.atrributes<-list(call.params=list(summarized.data= {if(!is.null(data)&!control$save.data) {summarize.data(data)} else {NULL} },
                                             summarized.consts= {if(!is.null(constants)&!control$save.data) {summarize.data(constants)} else {NULL} },
                                             data= {if(!is.null(data)&control$save.data) {data} else {if(!is.null(data)) {"data only summarized; see summarized.data component"} else {NULL} }},
                                             constants= {if(!is.null(constants)&control$save.data) {constants} else {if(!is.null(constants)) {"constants only summarized; see summarized.consts component"} else {NULL}} },
                                             code=code,
                                             Nchains=Nchains,
                                             params=params,params.conv=params.conv,params.save=params.save,
                                             niter.min=niter.min,niter.max=niter.max,nburnin.min=nburnin.min,nburnin.max=nburnin.max,thin.min=thin.min,thin.max=thin.max,
                                             time.max=control$time.max,
                                             inits=inits,
                                             neff.min=neff.min,neff.med=neff.med,neff.mean=neff.mean,
                                             conv.max=conv.max,conv.med=conv.med,conv.mean=conv.mean,
                                             control=control,control.MCMC=control.MCMC),
                            final.params=list(converged=converged,neffs.reached=neffs.reached,final.Nchains=Nchains.updated,removed.chains=chains.to.remove,burnin=nburnin.min0+ifelse((index.conv==1&converged)|!converged,0,sum(numIter.samplesList[1:ifelse(converged,index.conv-1,size.samplesList)])),thin=thin,niter.tot=niter.tot,
                                              Nvalues=unname(final.diags$params["Nvalues"]),neff.min=unname(final.diags$neff_synth["min"]),neff.median=unname(final.diags$neff_synth["median"]),
                                              WAIC=WAIC.results,
                                              extraResults=extraResults,
                                              Temps=TempsList,
                                              duration=total.duration,duration.MCMC.preparation=time.MCMC.Preparation,
                                              duration.MCMC.transient=time.MCMC*(ifelse(MCMC_language=="Greta",1,0)*control.MCMC$warmup+ifelse(MCMC_language=="Nimble",1,0)*control.MCMC$n.adapt+nburnin.min0+ifelse(index.conv==1&converged,0,sum(numIter.samplesList[1:ifelse(converged,index.conv-1,size.samplesList)])))/(ifelse(MCMC_language=="Greta",1,0)*control.MCMC$warmup+ifelse(MCMC_language=="Nimble",1,0)*control.MCMC$n.adapt+nburnin.min0+sum(numIter.samplesList)),
                                              duration.MCMC.asymptotic=time.MCMC*(1-(ifelse(MCMC_language=="Greta",1,0)*control.MCMC$warmup+ifelse(MCMC_language=="Nimble",1,0)*control.MCMC$n.adapt+nburnin.min0+ifelse(index.conv==1&converged,0,sum(numIter.samplesList[1:ifelse(converged,index.conv-1,size.samplesList)])))/(ifelse(MCMC_language=="Greta",1,0)*control.MCMC$warmup+ifelse(MCMC_language=="Nimble",1,0)*control.MCMC$n.adapt+nburnin.min0+sum(numIter.samplesList))),
                                              duration.MCMC.after=time.MCMC.after,
                                              duration.btadjust=total.duration-time.MCMC.Preparation-time.MCMC-time.MCMC.after,
                                              CPUduration=unname(CPUtime.MCMC.Preparation+CPUtime.MCMC+CPUtime.btadjust),
                                              CPUduration.MCMC.preparation=unname(CPUtime.MCMC.Preparation),
                                              CPUduration.MCMC.transient=unname(CPUtime.MCMC*(ifelse(MCMC_language=="Greta",1,0)*control.MCMC$warmup+ifelse(MCMC_language=="Nimble",1,0)*control.MCMC$n.adapt+nburnin.min0+ifelse(index.conv==1&converged,0,sum(numIter.samplesList[1:ifelse(converged,index.conv-1,size.samplesList)])))/(ifelse(MCMC_language=="Greta",1,0)*control.MCMC$warmup+ifelse(MCMC_language=="Nimble",1,0)*control.MCMC$n.adapt+nburnin.min0+sum(numIter.samplesList))),
                                              CPUduration.MCMC.asymptotic=unname(CPUtime.MCMC*(1-(ifelse(MCMC_language=="Greta",1,0)*control.MCMC$warmup+ifelse(MCMC_language=="Nimble",1,0)*control.MCMC$n.adapt+nburnin.min0+ifelse(index.conv==1&converged,0,sum(numIter.samplesList[1:ifelse(converged,index.conv-1,size.samplesList)])))/(ifelse(MCMC_language=="Greta",1,0)*control.MCMC$warmup+ifelse(MCMC_language=="Nimble",1,0)*control.MCMC$n.adapt+nburnin.min0+sum(numIter.samplesList)))),
                                              CPUduration.MCMC.after=unname(CPUtime.MCMC.after),
                                              CPUduration.btadjust=unname(CPUtime.btadjust),
                                              childCPUduration=unname(childCPUtime.MCMC.Preparation+childCPUtime.MCMC+childCPUtime.btadjust),
                                              childCPUduration.MCMC.preparation=unname(childCPUtime.MCMC.Preparation),
                                              childCPUduration.MCMC.transient=unname(childCPUtime.MCMC*(ifelse(MCMC_language=="Greta",1,0)*control.MCMC$warmup+ifelse(MCMC_language=="Nimble",1,0)*control.MCMC$n.adapt+nburnin.min0+ifelse(index.conv==1&converged,0,sum(numIter.samplesList[1:ifelse(converged,index.conv-1,size.samplesList)])))/(ifelse(MCMC_language=="Greta",1,0)*control.MCMC$warmup+ifelse(MCMC_language=="Nimble",1,0)*control.MCMC$n.adapt+nburnin.min0+sum(numIter.samplesList))),
                                              childCPUduration.MCMC.asymptotic=unname(childCPUtime.MCMC*(1-(ifelse(MCMC_language=="Greta",1,0)*control.MCMC$warmup+ifelse(MCMC_language=="Nimble",1,0)*control.MCMC$n.adapt+nburnin.min0+ifelse(index.conv==1&converged,0,sum(numIter.samplesList[1:ifelse(converged,index.conv-1,size.samplesList)])))/(ifelse(MCMC_language=="Greta",1,0)*control.MCMC$warmup+ifelse(MCMC_language=="Nimble",1,0)*control.MCMC$n.adapt+nburnin.min0+sum(numIter.samplesList)))),
                                              childCPUduration.MCMC.after=unname(childCPUtime.MCMC.after),
                                              childCPUduration.btadjust=unname(childCPUtime.btadjust),
                                              time_end=Sys.time()),
                            #NB: burnin and niter.tot are in MCMC iteration units for one chain; thin is in MCMC iteration units
                            final.diags=final.diags,
                            sessionInfo=sessionInfo(),
                            # removed because a priori useless outside runMCMC_btadjust							numIter.samplesList=numIter.samplesList,
                            warnings=c(NULL))
  result<- coda::as.mcmc.list(lapply(result,function(x){y<-coda::as.mcmc(as.matrix(x)); attributes(y)$mcpar<-c(original.atrributes$final.params$burnin,original.atrributes$final.params$burnin+original.atrributes$final.params$thin*(attributes(y)$dim[1]-1), original.atrributes$final.params$thin);y}))
  attributes(result)<-original.atrributes

  ## if the MCMC has not converged: risk that there are much too many values.
  if (!converged)
  {current.result.size<-length(result)*dim(result[[1]])[1]
  thin.end<-floor(current.result.size/(2*min(c(neff.min,neff.med,neff.mean),na.rm=TRUE)))
  if (thin.end>1)
  {thin<-thin*thin.end
  result<-window.seq(result,thin=thin.end)
  samplesList.temp<-window.seq(samplesList.temp,thin=thin.end)

  final.diags<-conveff_final(coda::as.mcmc.list(lapply(samplesList.temp,function(x){coda::as.mcmc(as.matrix(x))})),indices.conv,control$conveff.final.allparams,control$print.diagnostics)

  new.atrributes<-list(call.params=list(summarized.data= {if(!is.null(data)&!control$save.data) {summarize.data(data)} else {NULL} },
                                        summarized.consts= {if(!is.null(constants)&!control$save.data) {summarize.data(constants)} else {NULL} },
                                        data= {if(!is.null(data)&control$save.data) {data} else {if(!is.null(data)) {"data only summarized; see summarized.data component"} else {NULL} }},
                                        constants= {if(!is.null(constants)&control$save.data) {constants} else {if(!is.null(constants)) {"constants only summarized; see summarized.consts component"} else {NULL}} },
                                        code=code,
                                        Nchains=Nchains,
                                        params=params,params.conv=params.conv,params.save=params.save,
                                        niter.min=niter.min,niter.max=niter.max,nburnin.min=nburnin.min,nburnin.max=nburnin.max,thin.min=thin.min,thin.max=thin.max,
                                        time.max=control$time.max,
                                        inits=inits,
                                        neff.min=neff.min,neff.med=neff.med,neff.mean=neff.mean,
                                        conv.max=conv.max,conv.med=conv.med,conv.mean=conv.mean,
                                        control=control,control.MCMC=control.MCMC),
                       final.params=list(converged=converged,neffs.reached=neffs.reached,final.Nchains=Nchains.updated,removed.chains=chains.to.remove,burnin=nburnin.min0+ifelse((index.conv==1&converged)|!converged,0,sum(numIter.samplesList[1:ifelse(converged,index.conv-1,size.samplesList)])),thin=thin,niter.tot=niter.tot,
                                         Nvalues=unname(final.diags$params["Nvalues"]),neff.min=unname(final.diags$neff_synth["min"]),neff.median=unname(final.diags$neff_synth["median"]),
                                         WAIC=WAIC.results,
                                         extraResults=extraResults,
                                         Temps=TempsList,
                                         duration=total.duration,duration.MCMC.preparation=time.MCMC.Preparation,
                                         duration.MCMC.transient=time.MCMC*(ifelse(MCMC_language=="Greta",1,0)*control.MCMC$warmup+ifelse(MCMC_language=="Nimble",1,0)*control.MCMC$n.adapt+nburnin.min0+ifelse(index.conv==1&converged,0,sum(numIter.samplesList[1:ifelse(converged,index.conv-1,size.samplesList)])))/(ifelse(MCMC_language=="Greta",1,0)*control.MCMC$warmup+ifelse(MCMC_language=="Nimble",1,0)*control.MCMC$n.adapt+nburnin.min0+sum(numIter.samplesList)),
                                         duration.MCMC.asymptotic=time.MCMC*(1-(ifelse(MCMC_language=="Greta",1,0)*control.MCMC$warmup+ifelse(MCMC_language=="Nimble",1,0)*control.MCMC$n.adapt+nburnin.min0+ifelse(index.conv==1&converged,0,sum(numIter.samplesList[1:ifelse(converged,index.conv-1,size.samplesList)])))/(ifelse(MCMC_language=="Greta",1,0)*control.MCMC$warmup+ifelse(MCMC_language=="Nimble",1,0)*control.MCMC$n.adapt+nburnin.min0+sum(numIter.samplesList))),
                                         duration.MCMC.after=time.MCMC.after,
                                         duration.btadjust=total.duration-time.MCMC.Preparation-time.MCMC,
                                         CPUduration=unname(CPUtime.MCMC.Preparation+CPUtime.MCMC+CPUtime.btadjust),
                                         CPUduration.MCMC.preparation=unname(CPUtime.MCMC.Preparation),
                                         CPUduration.MCMC.transient=unname(CPUtime.MCMC*(ifelse(MCMC_language=="Greta",1,0)*control.MCMC$warmup+ifelse(MCMC_language=="Nimble",1,0)*control.MCMC$n.adapt+nburnin.min0+ifelse(index.conv==1&converged,0,sum(numIter.samplesList[1:ifelse(converged,index.conv-1,size.samplesList)])))/(ifelse(MCMC_language=="Greta",1,0)*control.MCMC$warmup+ifelse(MCMC_language=="Nimble",1,0)*control.MCMC$n.adapt+nburnin.min0+sum(numIter.samplesList))),
                                         CPUduration.MCMC.asymptotic=unname(CPUtime.MCMC*(1-(ifelse(MCMC_language=="Greta",1,0)*control.MCMC$warmup+ifelse(MCMC_language=="Nimble",1,0)*control.MCMC$n.adapt+nburnin.min0+ifelse(index.conv==1&converged,0,sum(numIter.samplesList[1:ifelse(converged,index.conv-1,size.samplesList)])))/(ifelse(MCMC_language=="Greta",1,0)*control.MCMC$warmup+ifelse(MCMC_language=="Nimble",1,0)*control.MCMC$n.adapt+nburnin.min0+sum(numIter.samplesList)))),
                                         CPUduration.MCMC.after=unname(CPUtime.MCMC.after),
                                         CPUduration.btadjust=unname(CPUtime.btadjust),
                                         childCPUduration=unname(childCPUtime.MCMC.Preparation+childCPUtime.MCMC+childCPUtime.btadjust),
                                         childCPUduration.MCMC.preparation=unname(childCPUtime.MCMC.Preparation),
                                         childCPUduration.MCMC.transient=unname(childCPUtime.MCMC*(ifelse(MCMC_language=="Greta",1,0)*control.MCMC$warmup+ifelse(MCMC_language=="Nimble",1,0)*control.MCMC$n.adapt+nburnin.min0+ifelse(index.conv==1&converged,0,sum(numIter.samplesList[1:ifelse(converged,index.conv-1,size.samplesList)])))/(ifelse(MCMC_language=="Greta",1,0)*control.MCMC$warmup+ifelse(MCMC_language=="Nimble",1,0)*control.MCMC$n.adapt+nburnin.min0+sum(numIter.samplesList))),
                                         childCPUduration.MCMC.asymptotic=unname(childCPUtime.MCMC*(1-(ifelse(MCMC_language=="Greta",1,0)*control.MCMC$warmup+ifelse(MCMC_language=="Nimble",1,0)*control.MCMC$n.adapt+nburnin.min0+ifelse(index.conv==1&converged,0,sum(numIter.samplesList[1:ifelse(converged,index.conv-1,size.samplesList)])))/(ifelse(MCMC_language=="Greta",1,0)*control.MCMC$warmup+ifelse(MCMC_language=="Nimble",1,0)*control.MCMC$n.adapt+nburnin.min0+sum(numIter.samplesList)))),
                                         childCPUduration.MCMC.after=unname(childCPUtime.MCMC.after),
                                         childCPUduration.btadjust=unname(childCPUtime.btadjust),
                                         time_end=Sys.time()),
                       #NB: burnin and niter.tot are in MCMC iteration units for one chain; thin is in MCMC iteration units
                       final.diags=final.diags,
                       sessionInfo=sessionInfo(),
                       # removed because a priori useless outside runMCMC_btadjust							numIter.samplesList=numIter.samplesList,
                       warnings=c(NULL),
                       original.attributes=original.atrributes)
  attributes(result)<-new.atrributes
  }
  }
  if (length(control$seed)>0)
  {### adding a new seed setting based on current time so that the following of the R session will not be influenced by the seed set in the present code
    set.seed(NULL)}

  print("###################################################################################")
  if (converged)
    {
      print("MCMC has reached the required level of convergence.")
    } else {
      print("MCMC has NOT reached the required level of convergence.")
    }
  print("###################################################################################")
  if (neffs.reached)
  {
    print("MCMC has reached the required level of effective values.")
  } else {
    print("MCMC has NOT reached the required level of effective values.")
  }
  print("###################################################################################")

  coda::as.mcmc.list(result)
}
