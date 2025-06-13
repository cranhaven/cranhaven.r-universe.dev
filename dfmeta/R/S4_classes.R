setClassUnion("ClassNewDose", c("numeric", "logical", "NULL"))

#' An S4 class to represent the results using the ZKO algorithm.
#'
#' @slot dataTox A data frame containing information for each patient at each simulation and trial.
#' @slot target The target of probability of toxicity.
#' @slot doses A vector with the doses panel.
#' @slot ti A vector including the total number of toxicities at each dose level.
#' @slot ni A vector consisting the total number of patients at each dose level.
#' @slot realite A vector with the observed toxicity probabilities at each dose level.
#' @slot relfreq A vector with the mean relative frequencies of allocation at each dose level.
#' @slot estimAlpha The estimated value for variable alpha.
#' @slot estimProb  The estimated probability of toxicity. 
#' @import methods
#' @export
setClass("ZKO", slots = list(dataTox="data.frame", target = "numeric", doses = "numeric", ti = "numeric", ni = "numeric", realite = "numeric", relfreq="matrix",
        estimAlpha="numeric", estimProb="numeric"))



setGeneric("show")
#' @export 
setMethod(f = "show", signature = "ZKO", definition = function(object)
    {
        cat("Today: ", date(), "\n") 
        cat("\n","A. Data Toxicity", "\n")
        cat("DataTox:", "\n")
        print(object@dataTox)
        cat("\n")
        cat("\n","B. ZKO Algorithm's Results: \n")
        cat("The estimated alpha is:", round(object@estimAlpha, digits = 4), "\n")
        relfreq <- as.vector(object@relfreq)
        t <- matrix(NA, nrow=5, ncol=length(object@relfreq))
        rownames(t) <- c("Dose level (mg)", "Observed Toxicity Probability", "Estimated Toxicity Probability", "Mean relative frequencies of allocation", "Pooled Estimated MTD")
        colnames(t) <- rep("", length(unique(object@doses)))
        dd <- paste("", sort(unique(object@doses)), sep = "")
        t[1, ] <- dd
        t[2, ] <- round(object@realite, digits = 3)
        t[3, ] <- round(object@estimProb, digits = 3)
        t[4, ] <- object@relfreq
        pos <- which(object@relfreq == max(object@relfreq), arr.ind = TRUE)
        t[5, ] <- rep("", length(unique(object@doses)))
        t[5, pos[2]] <- t[1, pos[2]]
        print(t, quote = FALSE)
        cat("\n", "Recommendation is based on a target toxicity probability of:", object@target, "\n")
    }
)


setGeneric("plot")

#' The graphical representation of dose-toxicity curve. 
#' 
#' @param x a "ZKO" object.
#' @param y the "y" argument is not used in the plot-method for "ZKO" object.
#' @param \dots other arguments to the \code{\link[=graphics]{plot.default}} function can be passed here.
#'
#' @description A plotting function representing graphically the dose-toxicity relationship including the target probability of toxicity. In addition you can see as circles the proportional sizes of the number of patients evaluated for toxicity at each dose level. 
#' @author Artemis Toumazi <artemis.toumazi@gmail.com>, Sarah Zohar <sarah.zohar@inserm.fr>, Anaund N. Vidyashankar <avidyash@gmu.edu>, Jie Xu <jxu13@gmu.edu> and Moreno Ursino <moreno.ursino@inserm.fr>
#' 
#' @references Zohar, S. , Katsahian, S. and O'Quigley, J. (2011), An approach to meta analysis of dose finding studies. Statist. Med., <doi:10.1002/sim.4121>.
#' 
#' @import methods
#' @import stats
#' @import graphics
#' @importFrom grDevices  rainbow
#' @importFrom utils menu
#' @export
setMethod(f = "plot", signature =c("ZKO", "missing"), definition = function(x, y=NA,...){
	## Ploting function of the dose-toxicity curve ##

	dfx = data.frame(ev1=x@doses, ev2=x@ti/x@ni, ev3=x@ni)

	with(dfx, symbols(x=ev1, y=ev2, circles=ev3, inches=1/6,
	                bg="steelblue2", fg=NULL, xlab = "Doses (mg/m2)", ylab = "Toxicity Probability", ylim = c(0, max(x@estimProb) + 0.1)), add=TRUE)

	lines(x = x@doses, y = x@estimProb, type = "l", lwd = 2)
	abline(h = x@target, lty = 3)
	mtext("Updated (solid) dose-toxicity curve with the target probability (dotted).", line=1.5)
	mtext("The circles indicate the proportional sizes about the number of patients evaluated for toxicity at each dose level.", line=0.5)
})

##########################################
################## REMB ##################
##########################################

#' An S4 class to represent the results using the random effect model-based (REMB) algorithm.
#'
#' @slot dataTox A data frame containing information for each patient at each simulation and trial.
#' @slot sim0 The simulation starting point; must be numeric. 
#' @slot sim1 The simulation ending point; must be numeric.
#' @slot family A distribution family for the response variable; defaults to binomial distribution. See \code{\link{glm}} and \code{\link{glmer}} for details. 
#' @slot link A specification for the model link function. This can be a name/expression, a literal character string or a length-one character vector; defaults to logit link function. See \code{\link{glmer}} for details. 
#' @slot estimates A data frame including the predicted toxicity probabilities (column named as "rndpt") using either the predicted random effect center "mu" (column named as "PredMu"), if it's available, or the value ratio (i.e. # of toxicities / # of subjects) if the corresponding "mu" is not available. Note that, "PredMu" value is equal to "NA" when there is no prediction for it.
#' @import methods
#' @export
setClass("REMB", slots = list(dataTox = "data.frame", sim0 = "numeric", sim1 = "numeric", family = "function", link = "character", estimates = "data.frame"))


setGeneric("show")
#' @export
setMethod(f = "show", signature = "REMB", definition = function(object)
    {
        cat("Today: ", date(), "\n")
        cat("\n","A. Data Toxicity (from Simulation:", object@sim0,"to Simulation:", object@sim1, ")", "\n")
        cat("Using a random effect model with:")
        print(object@family())
        cat("The structure of the selected DataTox:", "\n")
        print(str(object@dataTox))
        cat("\n")
        cat("\n","B. REMB Algorithm's Results (from Simulation:", object@sim0,"to Simulation:", object@sim1, ")", "\n")
        cat("\n")
        print(object@estimates)
        cat("\n")
        cat("Note (1): if PredMu is NA then it means that there is no prediction for it.", "\n")
        cat("Note (2): The column rndpt presents the predicted toxicity probability using either the variable PredMu when it's available or the variable ratio.", "\n")
    }
)

