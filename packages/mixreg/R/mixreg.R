mixreg <- function(x,y,ncomp=NULL,intercept=TRUE,eqVar=FALSE,
                           thetaStart=NULL,itmax=1000,eps=1e-6,verb=TRUE,
                           digits=7,maxTry=5,seed=NULL,data=NULL,
                           covMat=FALSE,MC=FALSE,warn=TRUE,...) {
#
# Function mixreg.  To fit a mixture of regression models using the
# EM algorithm.
#

# Check for spurious "..." arguments.
dotargNms <- names(list(...))
okNms     <- c("semiPar","conditional","cMseed")
nok       <- !(dotargNms %in% okNms)
if(any(nok)) {
    sparg  <- paste(dotargNms[nok],collapse=", ")
    argh   <- ngettext(sum(nok),"argument","arguments")
    whinge <- paste0("Spurious ",argh," supplied to mixreg: ",sparg,".\n")
    stop(whinge)
}

# Check on "data".
if(!is.null(data)) {
    if(!inherits(data,c("list","data.frame")))
        stop("Argument \"data\" must be a list or a data frame.\n")
}

# Determine which method should be used.
if(missing(y)) {
    meth <- "form"
} else {
    meth <- "var"
}

# Get the formula and variable names.
if(meth=="form") {
    x1 <- try(x,silent=TRUE)
    if(!inherits(x1,"formula"))
        stop("When \"y\" is not supplied, \"x\" must be a formula.\n")
    fmla <- x
    vnms <- all.vars(fmla)
} else {
    x1 <- try(x,silent=TRUE)
    if(inherits(x1,"formula")) {
        stop("When \"y\" is supplied, \"x\" cannot be a formula.\n")
    }
    nmy      <- deparse(substitute(y))
    nmx      <- deparse(substitute(x))
    vnms     <- c(nmy,nmx)
    charFmla <- if(intercept) paste(nmy,"~",nmx) else paste(nmy,"~",nmx,"-1")
    fmla     <- as.formula(charFmla)
}

# Search for variables, first in "data", then in the global environment.
# Note that if argument "data" was NULL or was a list, the object "data"
# produced by the following code will be a list.  Otherwise it will
# be a data frame.
for(vnm in vnms) {
    if(is.null(data[[vnm]])) {
        ok <- exists(vnm,where=1,inherits=FALSE)
        if(ok) {
            data[[vnm]] <- get(vnm)
        } else {
            whinge <- paste0("Cannot find ",vnm,".\n")
            stop(whinge)
        }
    }
}

# When "meth" is "vars" check on whether "x" is a matrix, and
# if so, whether it has any redundant columns.  Note:  things
# could also be fucked up when "meth" is "form", but in this case
# it is not clear whether "x" exists as such and the setting is
# just too convoluted to check on this effectively.  So I'm not
# going to bother.
if(meth=="vars") {
    if(is.matrix(data[[vnms[2]]])) {
       xm <- model.matrix(fmla,data=data)
       if(any(duplicated(t(xm)))) {
           whinge <- paste("The model matrix has redundant columns.",
                           "  Probably\n  the predictor \"x\" is a",
                           " matrix that has a column\n  of 1-s.  It",
                           " should not have such a column!\n\n")
           stop(whinge)
        }
    }
}
 
# If "data" is already a data frame it should be OK.  If it's a
# list, converting via as.data.frame() may not cut it.  When there
# is a single predictor, this could be a matrix (with "nc" columns,
# nc > 1) in which case as.data.frame() will produce a data frame
# containing "nc" predictors and "fmla" won't work.
if(!inherits(data,"data.frame")) {
    npred <- length(vnms) - 1
    if(npred>1) {
       data <- as.data.frame(data)
    } else {
        nr <- length(data[[vnms[1]]])
        xxx <- data.frame(row.names=1:nr)
        xxx[[vnms[1]]] <- data[[vnms[1]]]
        xxx[[vnms[2]]] <- data[[vnms[2]]]
        data <- xxx
    }
}

# Get the call (so that covMixMC() can use update()
cawl <- match.call()

# Do the grunt work.
rslt <- mixregEngine(fmla=fmla,data=data,ncomp=ncomp,eqVar=eqVar,
                     thetaStart=thetaStart,itmax=itmax,eps=eps,
                     verb=verb,digits=digits,maxTry=maxTry,
                     seed=seed,covMat=covMat,MC=MC,cawl=cawl,
                     warn=warn,...)

# Aw' done!!!
rslt
}
