\name{TukeyC}
\alias{TukeyC}
\alias{TukeyC.default}
\alias{TukeyC.lm}
\alias{TukeyC.formula}
\alias{TukeyC.aovlist}
\alias{TukeyC.lmerMod}

\title{
  The TukeyC Test for Single Experiments
}

\description{
  These are methods for objects of class \code{formula}, \code{lm}, \code{aov}, \code{aovlist} and \code{lmerMod} for single, factorial, split-plot and split-split-plot experiments.
}

\usage{
TukeyC(x,...)

\method{TukeyC}{formula}(formula,
       data            = NULL,
       which           = NULL,
       fl1             = NULL,
       fl2             = NULL,
       error           = NULL,
       sig.level       = .05,
       round           = 2,
       adjusted.pvalue = 'none',
       \dots)

\method{TukeyC}{lm}(x,
       which           = NULL,
       fl1             = NULL,
       fl2             = NULL,
       error           = NULL,
       sig.level       = .05,
       round           = 2,
       adjusted.pvalue = 'none',
       \dots)

\method{TukeyC}{aovlist}(x,
       which           = NULL,
       fl1             = NULL,
       fl2             = NULL,
       error           = NULL,
       sig.level       = .05,
       round           = 2,
       adjusted.pvalue = 'none',
       \dots)

\method{TukeyC}{lmerMod}(x,
       which           = NULL,
       fl1             = NULL,
       fl2             = NULL,
       error           = NULL,
       sig.level       = .05,
       round           = 2,
       adjusted.pvalue = 'none',
       \dots)

}

\arguments{
  \item{x,formula}{A \code{formula}, \code{lm}, \code{aov}, \code{aovlist} and \code{lmerMod} class object. Objects of the \code{formula} class follow ``response variable ~ predicted variable.}
  \item{data}{A object of the \code{data.frame} class. Use only objects of \code{formula} class.}
  \item{which}{The name of the treatment to be used in the comparison.
    The name must be inside quoting marks.}
  \item{fl1}{A vector of length 1 giving the level of the first factor in nesting order tested.}
  \item{fl2}{A vector of length 1 giving the level of the second factor in nesting order tested.} 
  \item{error}{The error to be considered. If from experiment at split plot or split-split plot pay attention! See details!}
  \item{sig.level}{Level of Significance used in the TukeyC algorithm to create
    the groups of means. The default value is 0.05.}
  \item{round}{Integer indicating the number of decimal places.}
  \item{adjusted.pvalue}{Method for adjusting p values (see \code{p.adjust} to more details). The possible values are: \code{"holm"}, \code{"hochberg"}, \code{"hommel"}, \code{"bonferroni"}, \code{"BH"}, \code{"BY"}, \code{"fdr"} and \code{"none"}. The default is \code{"none"}. }
  \item{\dots}{Potential further arguments (required by generic).}
}

\details{
  The function \code{TukeyC} returns an object of class \code{TukeyC} 
  containing the groups of means plus other
  necessary variables for summary and plot.

  The generic functions \code{summary} and \code{plot} are used to obtain and
  print a summary and a plot of the results.

  The error arguments may be used whenever the user want a specific error other than the experimental error. At the split plot and split-split plot experiment, combination of error may be specified with "/" in the sequence of the \code{which} argument. For example, a object of \code{aovlist} class, a possible combination would be \code{error = 'Within/blk:plot'} at case block split plot experiment with \code{which = 'subplot:plot'} argument.
}

\value{
  The function \code{TukeyC} returns a list of the class \code{TukeyC} with the slots:
  \item{Result}{A \code{data.frame} storing the result of Tukey test.}
  \item{Sig.level}{A scalar giving the level of significance of the test.} 
  \item{Diff_Prob}{A \code{matrix} at the lower diagonal with p-values and upper diagonal with means differences.}
  \item{MSD}{A \code{matrix} with minimum significance differences by Tukey methodology. If balanced data, then all values are equal.}
}

\author{
  Jose Claudio Faria (\email{joseclaudio.faria@gmail.com})\cr
  Enio Jelihovschi (\email{eniojelihovs@gmail.com})\cr
  Ivan Bezerra Allaman (\email{ivanalaman@gmail.com})
}

\references{
  Miller, R.G. (1981) \emph{Simultaneous Statistical Inference}. Springer.

  Ramalho M.A.P, Ferreira D.F and Oliveira A.C. (2000) \emph{Experimentacao em Genetica
    e Melhoramento de Plantas}. Editora UFLA.

  Steel, R.G., Torrie, J.H and Dickey D.A. (1997) \emph{Principles and procedures of statistics:
    a biometrical approach}. Third Edition.

  Yandell, B.S. (1997) \emph{Practical Data Analysis for Designed Experiments}. 
  Chapman and Hall.
}

\examples{
##
## Examples:Randomized Complete Block Design (RCBD)
## More details: demo(package='TukeyC')
##

## The parameters can be: formula, aov, lm, aovlist and lmerMod

data(RCBD)

## From: formula
tk1 <- with(RCBD,
            TukeyC(y ~ blk + tra,
                   data=dfm,
                   which='tra'))
summary(tk1)

## From: merMod
## This class is specific of the lme4 package.
\dontrun{
  if(require(lme4)){
    lmer1 <- with(RCBD,
                  lmer(y ~ (1|blk) + tra,
                       data=dfm))

    tk2 <-  TukeyC(lmer1,
                   which='tra')
    summary(tk2)
  }
}
##
## Example: Latin Squares Design (LSD)
## More details: demo(package='TukeyC')
##

data(LSD)

## From: formula
tk3 <- with(LSD,
            TukeyC(y ~ rows + cols + tra,
                   data=dfm,
                   which='tra'))
summary(tk3)

## From: aov
av1 <- with(LSD,
            aov(y ~ rows + cols + tra,
                data=dfm))

tk4 <- TukeyC(av1,
              which='tra')
summary(tk4)

## From: lm
lm1 <- with(LSD,
            lm(y ~ rows + cols + tra,
               data=dfm))

tk5 <- TukeyC(lm1,
              which='tra')
summary(tk5)

##
## Example: Factorial Experiment (FE)
## More details: demo(package='TukeyC')
##

data(FE)
## From: formula
## Main factor: N
tk6 <- with(FE,
            TukeyC(y ~ blk + N*P*K,
                   data=dfm,
                   which='N'))
summary(tk6)

## Nested: p1/N
# From: formula
n_tk1 <- with(FE,
              TukeyC(y ~ blk + N*P*K,
                     data=dfm,
                     which='P:N',
                     fl1=1))
summary(n_tk1) 

## Nested: p2/N
# From: lm
lm2 <- with(FE,
            lm(y ~ blk + N*P*K, 
               dfm))

n_tk2 <- with(FE,
              TukeyC(lm2,
                     which='P:N',
                     fl1=2))
summary(n_tk2) 

## Nested: n1/P
# From: aov
av2 <- with(FE,
            aov(y ~ blk + N*P*K,
                dfm))

n_tk3 <- with(FE,
              TukeyC(av2,
                     which='N:P',
                     fl1=1))
summary(n_tk3) 

# From: merMod
\dontrun{
  if(require(lme4)){
    lmer2 <- with(FE,
                  lmer(y ~ (1|blk) + N*P*K,
                       dfm))

    n_tk4 <- with(FE,
                  TukeyC(lmer2,
                         which='N:P',
                         fl1=1))
    summary(n_tk4)
  }
}

##
## Example: Split-plot Experiment (SPET)
## More details: demo(package='TukeyC')
##
data(SPET)

## From lm
lm3 <- with(SPET,
            lm(y ~ blk*tra + tra*year,
               dfm))

# crotgrantiana/year
sp_tk1 <- TukeyC(lm3,
                 which='tra:year',
                 fl1=1)
summary(sp_tk1) 

# year1/tra
# It is necessary to set year error with trat error in the order of the "which" argument.
# It is necessary to inform how to combinate the errors
sp_tk2 <-  TukeyC(lm3,
                  which='year:tra',
                  error='Residuals/blk:tra',
                  fl1=1)
summary(sp_tk2)

# From merMod
# Onty tra
\dontrun{
  if(require(lme4)){
    lmer3 <- with(SPET,
                  lmer(y ~ blk + (1|blk:tra) + tra*year,
                       dfm))

    # comparison only tra
    sp_tk3 <- TukeyC(lmer3,
                     which = 'tra',
                     error = 'blk:tra')
    summary(sp_tk3)

    # year1/tra
    sp_tk4 <- TukeyC(lmer3,
                     which='year:tra',
                     error='Residual/blk:tra',
                     fl1=1)
    summary(sp_tk4)
  }
}

## Example: Split-split-plot Experiment (SSPE)
## More details: demo(package='TukeyC')
##

data(SSPE)
## From: formula
## Main factor: P
## It is necessary to inform the appropriate error for the test
ssp_tk1 <- with(SSPE,
                TukeyC(y ~ blk + P*SP*SSP + Error(blk/P/SP),
                       data=dfm,
                       which='P',
                       error='blk:P'))
summary(ssp_tk1)

## Main factor: SP
## It is necessary to inform the appropriate error for the test
ssp_tk2 <- with(SSPE,
                TukeyC(y ~ blk + P*SP*SSP + Error(blk/P/SP),
                       data=dfm,
                       which='SP',
                       error='blk:P:SP'))
summary(ssp_tk2)

## Main factor: SSP
ssp_tk3 <- with(SSPE,
                TukeyC(y ~ blk + P*SP*SSP + Error(blk/P/SP),
                       data=dfm,
                       which='SSP'))
summary(ssp_tk3)

## From: aov
## Main factor: SSP
av3 <- with(SSPE,
            aov(y ~ blk + P*SP*SSP + Error(blk/P/SP),
                data=dfm))

ssp_tk4 <- TukeyC(av3,
                  which='SSP')
summary(ssp_tk4)

## Nested: p1/SP
## It is necessary to inform the appropriate error for the test
ssp_tk5 <- TukeyC(av3,
                  which='P:SP',
                  error='blk:P:SP',
                  fl1=1)
summary(ssp_tk5)

## Nested: p1/SSP
ssp_tk6 <- TukeyC(av3,
                  which='P:SSP',
                  fl1=1)
summary(ssp_tk6)

## Nested: p1/sp1/SSP
## Testing SSP inside of level one of P and level one of SP
ssp_tk7 <- TukeyC(av3,
                  which='P:SP:SSP',
                  fl1=1,
                  fl2=1)
summary(ssp_tk7)

## Nested: p2/sp1/SSP
ssp_tk8 <- TukeyC(av3,
                  which='P:SP:SSP',
                  fl1=2,
                  fl2=1)
summary(ssp_tk8)

## Nested: sp1/P
## It is necessary to inform the appropriate error for the test
ssp_tk9 <- TukeyC(av3,
                  which='SP:P',
                  error='blk:P:SP/blk:P',
                  fl1=1)

summary(ssp_tk9)

## Nested: ssp1/SP
ssp_tk10 <- TukeyC(av3,
                   which='SSP:SP',
                   error='Within/blk:P:SP',
                   fl1=1)
summary(ssp_tk10)

## Nested: ssp1/sp1/P
## It is necessary to inform the appropriate error for the test
ssp_tk11 <- TukeyC(av3,
                   which='SSP:SP:P',
                   error='Within/blk:P:SP/blk:P',
                   fl1=1,
                   fl2=1)
summary(ssp_tk11)

## UNBALANCED DATA
## The average are adjusted by "Least-Square-Means" methodology.
## From: formula
data(CRD2)

uCRD2 <- CRD2$dfm
uCRD2[c(3, 5, 10, 44, 45), 3] <- NA

utk1 <-  TukeyC(y ~ x,
                data=uCRD2,
                which='x')
summary(utk1)

## From: lm
ulm1 <- lm(y ~ x,
           data=uCRD2)

utk2 <- TukeyC(ulm1,
               which='x')
summary(utk2)


## Factorial Experiments
## Nested: p1/N
# From: lm

uFE <- FE$dfm
uFE[c(3, 6, 7, 20, 31, 32), 5] <- NA

ulm2 <- lm(y ~ blk + N*P*K,
           uFE)

## Nested: p1/N
utk3 <- TukeyC(ulm2,
               data=uFE,
               which='P:N',
               fl1=1)
summary(utk3) 

## Nested: p2/n2/K
utk4 <- TukeyC(ulm2,
               data=uFE,
               which='P:N:K',
               fl1=2,
               fl2=2)
summary(utk4) 
}

\keyword{package}
