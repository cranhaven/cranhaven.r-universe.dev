# runMCMCbtadjust 1.1.2

Mainly technical bug fixes:
- in section 1.5, added component to calculate indices.samplesList
- replaced result by samplesList.temp in the final call to conveff_final function - to avoid undefined results
- changed functions conveff_final & conveff so that they can work in case there is only one parameter
- changed the default value of min.Nvalues in control to NULL which will then be replaced in the code by which will be replaced by neff.max
- added chains.to.remove<-NULL in 1.2 to avoid errors in case if control$remove.fixedchains is FALSE
- added affectation in samplesList in 2.2 in case Nimble, non parallelized and not APT - took the formula from within parallelized version; & added calculation of chains.to.update
- rewritten the Nimble part of the 2.2 section so as to not update removed chains - their values are then replaced by those of the first updated chain for the sake of dimension coherence
- correction in section 2.2 of allocation to samplesList in case Nimble and parallelize: the code was erroneous because did not take into account the nature out object out1
- adding chains.to.remove in call to conveff_final
- removing removed chains from final result/output in mcmc.list format and adding the info of which chains were removed in the final.params component called removed.chains - can be useful e.g. to relate with the Initial values
- added drop=FALSE in many parts of the code for mcmc objects and added as.matrix after coda::as.mcmc so that they do not become vectors in case only one parametger is monitored-
- minor clarity changhes in help (of control$conv.thorough.check, control$remove.fixedchains)
- rather minor changes in vignettes


---

# runMCMCbtadjust 1.1.1

Major revisions:
- Added 1000 to control$time.max/Nchains in timeout argument to avoid "error reading from connection"; then replaced timeout = min(5184000,control$time.max/Nchains+1000) by timeout = ifelse(is.finite(control$time.max), 3*control$time.max+3600,30*24*3600) to have a maximum length of 30 days in case time.max is unspecified and allowing extra-time for WAIC & extra-calculations otherwise.
- Modified multiplier of Nchains in set.seed in sections 2 and greater - using Ncycles to avoid repeated same values
- Changed formula for niter related to duration/time to promote reproducibility in case of time.max not reached
- Changed the way control$check.thinmult behaves by adding a third level and changing the default - to conserve neffs.reached & convergence if they occurred prior to change
- Changed the beginning of sections 2.3 & 2.5.3 - "Reshaping samplesList" sections -  to better control samples and iterations being controlled from the end of the Markov chains
- Added code for controlling sees in case of parallelization (control.MCMC$parallelize), for reproducibility issues: seeds <- getseeds(Nchains, control$seed); results.temp <- parallel::clusterApply(cl, seeds, worker.seed), with specifications of functions getseeds and worker.seed.
- Added function window.seq to allow coherence with modifications in 2.3 and 2.5.3 and used it instead of window (in case thin was specified)
- Added arguments decrease.thinmult.multiplier and decrease.thinmult.threshold to control and changed the formula for thinmult decrease when adapting it from thinmult<-thinmult-1 to thinmult<-ifelse(thinmult>control$decrease.thinmult.threshold, max(floor(control$decrease.thinmult.multiplier*thinmult), control$decrease.thinmult.threshold),thinmult-1)
- Added time.MCMC.Preparation.num and shifted time-dependent formulas for niter from (control$time.max-duration)*0.95 to (control$time.max-duration)*max(0.5,time.MCMC.num/(duration-time.MCMC.Preparation.num)), so that control$time.max is the targeted maximum time
- Added two vignettes to give expanations on two specific points: changing samplers with Nimble and doing extra-calaulcations with Nimble

Minor revisions:
- Put back the call to library instead of requireNamespace in parallelizeInitExpr to avoid some problems.
- Added a stop of the program if parallelize and library parallel is not loaded. Updated help accordingly.
- Changed error message if APT and Nimble and not parallelized, and nimbleAPT is not loaded
- Revised burnin in the output - in case of non-convergence - and revised thinning of result in case of non-convergence - to ensure proper resizing
- Revised 2.5 section and especially 2.5.2 to reduce number of samples in case of convergence - done also if not check.thinmult; associated changes in checking.neffs.conserved and in section 2.1 for thin in case niter<0
- Removed a print command in 2.5.2 that was there just for diagnosing
- Corrected calculate.thinmult.target function to have a maximum value that thinmult cannot overcome so that there are at least 10 values left in each Markov Chain.
- Shifted index.conv.local<-index.conv.temp to index.conv.local<-indices.samplesList[index.conv.temp] before conveff_final calls due to burnin problems with conveff_final
- Slight changes in the first Vignette
- Slight changes in the text of the help
- Changed default and help for component check.convergence.firstrun of control
- Changed the error message in case names of control or control.MCMC do not match those in the function, to be more informative
- Changed final thin multiplier section to make check.thinmult more active: added if else in: stop_decrease<-convergedtemp&neffs.reachedtemp&ifelse(control$check.thinmult==3,neffs.conserved,TRUE)&min.Nvalues.OK
- Changed examples in functions runMCMC_btadjust and findMCMC_strong_corrs: not running them - due to duration constraints on CRAN - and removing condition
- Replaced: nu.burn=nburnin.min+sum(numIter.samplesList[1:(index.conv.local-1)]) by: nu.burn= nburnin.min0+sum(numIter.samplesList[1:(index.conv.local-1)]) for nu.burn to be better estimated with Jags.
- Added printing messages at the end of MCMC sampling and at the beginning of WAIC sampling; Added printing of extra-calculation phase - otherwise silent…
- Added components Nvalues, neff.min & neff.median in final.params component of the attributes of the output
- Introduced arguments monitorParentNodes and monitorAllStochNodes in control.MCMC and slight changes in params, params.conv & params.save
- Added argument conveff.final.allparams to control with a default to TRUE and changed the conveff_final function and its calls accordingly. Behavior in previous versions corresponded to conveff.final.allparams=FALSE in case of MCMC convergence.
- Added arguments force.niter.max and force.time.max (respectively time.max.turns.off.niter.max) to control with a default to FALSE to be able to force the MCMC to go to niter.max (for force.niter.max) and control$time.max (for force.time.max) (provided control$time.max constraints and niter.max contrainsts are met) (resp. to disable niter.max - except if in force.niter.max or force.time.max phase - if control$time.max is specified).
- In section 2 - for in thinmult adaptations - turned samplesList to samplesList.temp & adapted related indices for diagstemp and minNvalues.OK calculations. 

---

# runMCMCbtadjust 1.1.0

- Replaced functions list.as.matrixbisdim1 and list.as.array and replaced them by as.matrix and as.array (from coda package)
- Complete reshaping of section 2.5
- Introduction of components ip.nc, conv.thorough.check, thinmult.in.resetMV.temporary, check.thinmult, only.final.adapt.thin, max.prop.decr.neff, save.data in parameter control
- Introduction of components resetMV, parallelize, parallelizeInitExpr, useConjugacy, WAIC, WAIC.Nsamples, WAIC.control, APT, APT.NTemps, APT.initTemps, APT.tuneTemps, APT.thinPrintTemps, includeAllStochNodes, saveAllStochNodes, includeParentNodes, saveParentNodes and extraCalculations in parameter control.MCMC.
- Replacing updates of thin with the function update.thin
- Added component save.data in control argument, allowing to save entire data instead of summary of data.
- Added code in the call.params component of the attributes output
- Added possibility that parameter inits is a function, and further checkings for inits
- Considerably developed checkings of arguments
- Revised formula for calculating thinmult and put it in function calculate.thinmult.target
- Allowed control$convtype to be NULL - which is now the default.
- Introduction of control$conv.thorough.check and associated programmation in sections 1.4 & 2.4.1
- Changed function checking.neffs.conserved to make all kinds of neff potentially active
- Introduced component only.final.adapt.thin to control and changed the function calculate.thinmult.target and its call accordingly
- Reframed initial checking of control$time.max so that if initially NULL becomes Inf and keeping only first condition in 2.1
- Reframed the proposed number of iterations to run in 2.1 in case  ! Converged & ! previously.converged (to be close to case where previously converged in case the diagnostic is close the target, using a sigmoid function) & Heidleberger replaced by Heidelberger. Added associated component ip.nc to control
- In case MCMC.language is Nimble: added set.seed (…+Nchains) before $run in 1.1 and turned the one in 2.2 to set.seed (…+Ncycles*Nchains)
- Added component resetMV in control.MCMC active in case MCMC.language is Nimble: and adapted code accordingly in section 2.2
- Implemented parallelization for Jags and Nimble as driven by the parallelize & parallelizeInitExpr components of control.MCMC (and added section 3.1)
- Added calculate=FALSE in nimbleModel commands to speed up model preparation with Nimble: see: https://groups.google.com/g/nimble-users/c/a6DFCefYfjU/m/kqUWx9UXCgAJ
- Added useConjugacy in control.MCMC and then in configureMCMC commands to speed up model preparation with Nimble if TRUE: see: https://groups.google.com/g/nimble-users/c/a6DFCefYfjU/m/kqUWx9UXCgAJ
- Removed code: require(coda) through adding more calls to coda::as.mcmc.list to link with coda library sonner - for functions such as summary, as.matrix and as.array
- In case MCMC.language is Nimble: coded of (online) WAIC and introduced associated components in control.MCMC
- In case MCMC.language is Nimble: added possibility to use NimbleAPT through new code and associated components in control.MCMC (components starting with APT)
- Added components to the final.params element of the attributes of the output: WAIC, Temps, extraResults, the various components starting by "childCPUduration"
- Added near the end: result<- coda::as.mcmc.list(lapply(result,function(x){y<-x; attributes(y)$mcpar<-c(original.atrributes$final.params$burnin,original.atrributes$final.params$burnin+original.atrributes$final.params$thin*(attributes(y)$dim[1]-1), original.atrributes$final.params$thin);y})) - to render the mcmc.list with its usual format.
- Replaced final.diags=conveff_final(result) with final.diags=conveff_final(coda::as.mcmc.list(lapply(result,function(x){coda::as.mcmc(as.matrix(x))}))) and updated thin in the last condition for final diagnostics to be OK
- To correct problems with calculus of nburnin in case index.conv==1, repaced througout: nburnin.min0+sum(numIter.samplesList[1:ifelse(converged,index.conv-1,size.samplesList)]) by: nburnin.min0+ifelse(index.conv==1&converged,0,sum(numIter.samplesList[1:ifelse(converged,index.conv-1,size.samplesList)]))
- Added lines at the end of sections 1.1 and 2.2 to make samplesList of standard format: samplesList<- coda::as.mcmc.list(lapply(samplesList,function(x){coda::as.mcmc(as.matrix(x))}))
- Added a last argument to function checking.neffs.conserved to have a different behaviour depending on whether neffs criteria are reached or not; and recoded the function accordingly: now max.prop.decr.neff is active only if neffs.reached
- Added two suggested libraries in DESCRIPTION file: nimbleAPT & parallel
- Modifications in ways samples are collected with Nimble and in 2.6 (this is actually done for it) ; associated introduction of argument thinmult.in.resetMV.temporary in control
- Turned default of showCompilerOutput to FALSE to limit printing size with Nimble.
- Redesigned calculations of CPUtime/duration: includes child processes time & also reports childCPUtime as well (which can/should be NA on Windows)
- Removed components of the attributes of the output: R.version and package.versions, replacing it by sessionInfo that conatins these info (+ extra info)
- Modification of function formatC_adapted so that it does what we ant (digits=3); otherwise was returning to the default for the second formatC
- Due to bug with Nchains=1 & rstan: replaced as.array(out) with as.array(out,drop=FALSE) in two conveff functions
- Changed way of declaring initial values in Nimble - strangely appeared undeclared so far - and, if parallelize, Jags
- Introduced component check.thinmult in control and associated code
- Various changes in printed texts
- Introduced min.Nvalues in calculation of niter to avoid  having so few values after fitting that no diagnostic would be produced
- Changes in the Vignette

---

# runMCMCbtadjust 1.0.5

- Correction of a bug in the section 2.1 section of the code for the calculation of the new number of iterations: the number of iterations was too small when thin.max was reached (acknowledged with thin.max=1): replaced thin by thin.theoretical (which has been introduced at the beginning or section 2.1); replaced (index.conv-1) by ifelse(converged,index.conv-1,size.samplesList) in final calculations of durations. Otherwise, erroneous in case of non convergence..
- Made corrections regarding how control$seed was treated: (i) added the possibility that control$seed could be NULL - in which case there is no control of seeds for MCMCs; (ii) put the default to NULL instead of 1; (iii) in case length(control$seed) was 1, added set.seed(NULL) at the end to not constrain the seed of the R environment after the end of the function by the seed used in the function.
- Added neffs.reached in the final.params component of the attributes of the result; added help for final.params$converged and final.params$neffs.reached
- Allowed control.MCMC$n.adapt to also apply in case Nimble was the MCMC.language; changed the default value accordingly (so that it is in the program 1000 in case Jags is used and 0 in case Nimble is used); corrected an error in the inclusion of n.adapt in duration.MCMC.transient in case Jags is used.

---

# runMCMCbtadjust 1.0.4

- Replaced (index.conv-1) by ifelse(converged,index.conv-1,size.samplesList) in final calculations of durations. Otherwise, erroneous in case of non convergence..

---

# runMCMCbtadjust 1.0.3

- Added buildDerivs component to control.MCMC parameter.

---

# runMCMCbtadjust 1.0.2

- Changed default value for round.thinmult in parameter control to TRUE, which is more rigorous.

---

# runMCMCbtadjust 1.0.1

- Added a `NEWS.md` file to track changes to the package.
- Changed title of package - referring to 'JAGS', 'nimble' or 'greta' explicitly
- Changed function runMCMC_btadjust(): placing neff.max at the beginning, replacing neffmax by neff.max in functions scale.available.neffs & calculate.thinmult.target, adding warnings if niter.max is finite and niter.max/thin.max is greater than 3*neff.max
- Replaced niter>0 by niter>=thin in sections 2 and 2.2 in function runMCMC_btadjust(), to avoid a bug
- Slight changes to the vignette named 'runMCMCbtadjust_Presentation'

---
# runMCMCbtadjust 1.0.0
