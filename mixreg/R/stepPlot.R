stepPlot <- function(fmla,ncomp=NULL,eqVar=FALSE,thetaStart=NULL,
                     nsteps=100,eps=1e-6,digits=7,maxTry=5,
                     seed=NULL,data) {
#
# Function stepPlot.  To fit a mixture of regression models, one EM step
# at a time, plotting the result after each step.
#

# Check on "data".
if(missing(data)) {
    stop("Argument \"data\" must be supplied.\n")
}
if(!inherits(data,"data.frame")){
    stop("Argument \"data\" must be a data frame.\n")
}

# Get the variable names.
vnms <- all.vars(fmla)

# Search for variables in "data".
for(vnm in vnms) {
    if(is.null(data[[vnm]])) {
        whinge <- paste0("Cannot find ",vnm," in \"data\".\n")
        stop(whinge)
    }
}

# Step through.
for(i in 1:nsteps) {
    rslt <- mixregEngine(fmla=fmla,data=data,ncomp=ncomp,eqVar=eqVar,
                         thetaStart=thetaStart,itmax=1,eps=eps,
                         verb=TRUE,digits=digits,maxTry=maxTry,
                         seed=seed,cawl=NULL,covMat=FALSE,MC=NA,warn=FALSE)
    plot(rslt,main=paste0(i," EM ",ngettext(i,"step","steps")," completed."))
    if(rslt$converged) {
        cat("Convergence achieved.\n")
        break
    }
    ans <- readline("Continue? (\"n\" to break) ")
    if(ans=="n") break
    thetaStart <- rslt$theta
    cat("Step",i,"done.\n")
}
if(!rslt$converged) {
    cat(nsteps,"steps completed without convergence.\n")
}
invisible(rslt)
}
