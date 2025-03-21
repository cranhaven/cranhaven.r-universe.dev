\name{BRugs}
\alias{BRugs}
\alias{help.BRugs}
\title{Introduction to BRugs}
\description{This manual describes how to use the BRugs software}
\usage{
help.BRugs(browser = getOption("browser"))
}
\arguments{
\item{browser}{the name of the program to be used as hypertext browser.
    It should be in the PATH, or a full path specified.}
}
\details{
    BRugs is a collection of R functions that
    allow users to analyze graphical models using MCMC techniques. Most of the R functions in BRugs provide a
    interface to the BRugs dynamic link library (shared object file). The BRugs dynamic link library is able to make
    use of many of the WinBUGS components, in particular those components concerned with graphical models and MCMC
    simulation. BRugs lacks the GUI interface of WinBUGS but is able to use R to create graphical displays of the
    MCMC simulation. BRugs uses the same model specification language as WinBUGS and the same format for data and
    initial values. However BRugs always uses plain text files for input inplace of WinBUGS compound documents. The
    BRugs functions can be split into two groups: those associated with setting up and simulating the graphical model
    and those associated with making statistical inference. In general the R functions in BRugs correspond to the
    command buttons and text entry fields in the menus of WinBUGS. Each WinBUGS text entry field splits into two
    R functions, one to set the quantity and the other to get the value of the quantity.

    Andrew Gelman suggests to use the function \code{bugs} in the \pkg{R2WinBUGS} package
    with argument \code{program="openbugs"} as a wrapper.
}
\section{Permission and Disclaimer}{
    BRugs is released under the GNU GENERAL PUBLIC LICENSE.
    For details type \code{help.BRugs()}.

    More informally, potential users are reminded to be extremely careful if using this program for serious
    statistical analysis. We have tested the program on quite a wide set of examples, but be particularly careful
    with types of model that are currently not featured. If there is a problem, BRugs might just crash, which is not
    very good, but it might well carry on and produce answers that are wrong, which is even worse. Please let us know
    of any successes or failures.

    If BRugs does cause R to crash, it is advised to run the model from
    within the Windows interface to OpenBUGS.  This should give a "Trap"
    window, which indicates an internal problem with OpenBUGS.

    %%Seex
    %%rl{https://www.openbugs.net/Manuals/TipsTroubleshooting.html#TrapMessages}

    for suggestions for how to interpret these problematic error
    messages.
}
%\references{}
\seealso{\code{\link{help.WinBUGS}} (which currently is called from \code{help.BRugs}) and the meta function \code{\link{BRugsFit}}}
\examples{
\donttest{
###    Step by step example:    ###
library("BRugs") # loading BRugs

## Prepare the example files in a temporary directory
exfiles <- dir(options()$OpenBUGSExamples, pattern="^Rats.*txt$", full.names=TRUE)
ok <- file.copy(exfiles, tempdir())

## Now setting the working directory to the temporary one:
oldwd <- setwd(tempdir())

## some usual steps (like clicking in WinBUGS):
modelCheck("Ratsmodel.txt")          # check model file
modelData("Ratsdata.txt")            # read data file
modelCompile(numChains=2)            # compile model with 2 chains
modelInits(rep("Ratsinits.txt", 2))  # read init data file
modelUpdate(1000)                    # burn in
samplesSet(c("alpha0", "alpha"))     # alpha0 and alpha should be monitored
modelUpdate(1000)                    # 1000 more iterations ....

samplesStats("*")                    # the summarized results

## some plots
samplesHistory("*", mfrow = c(4, 2)) # plot the chain,
samplesDensity("alpha")              # plot the densities,
samplesBgr("alpha[1:6]")             # plot the bgr statistics, and
samplesAutoC("alpha[1:6]", 1)        # plot autocorrelations of 1st chain

## switch back to the previous working directory:
setwd(oldwd)
}
\dontrun{
# Getting more (online-)help:
help.BRugs()
}}
\keyword{interface}
\keyword{documentation}
\concept{OpenBUGS}
\concept{WinBUGS}
\concept{MCMC}
