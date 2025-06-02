\name{LIMEcoli}
\docType{data}
\alias{LIMEcoli}
\title{
  The Escherichia Coli Core Metabolism: Reaction network model specificiation
}

\description{
  Linear inverse model specification for performing Flux Balance Analysis
  of the E.coli metabolism

  (as from \code{http://gcrg.ucsd.edu/Downloads/Flux_Balance_Analysis}).

  The original input file can be found in the package subdirectory
  \code{/examples/Reactions/E_coli.lim}

  There are 53 substances:

  GLC, G6P, F6P, FDP, T3P2, T3P1, 13PDG, 3PG, 2PG, PEP,
  PYR, ACCOA, CIT, ICIT, AKG, SUCCOA, SUCC, FUM, MAL, OA,
  ACTP, ETH, AC, LAC, FOR, D6PGL, D6PGC, RL5P, X5P,
  R5P, S7P, E4P, RIB, GLX, NAD, NADH, NADP, NADPH, HEXT,
  Q, FAD, FADH, AMP, ADP,
  ATP, GL3P, CO2, PI, PPI, O2, COA, GL, QH2

  and 13 externals:

  Biomass, GLCxt, GLxt, RIBxt, ACxt, LACxt, FORxt, ETHxt, SUCCxt,
  PYRxt, PIxt, O2xt, CO2xt


  There are 70 unknown reactions (named by the gene encoding for it):

  GLK1,  PGI1,  PFKA,  FBP, FBA, TPIA, GAPA, PGK, GPMA, ENO,
  PPSA, PYKA, ACEE, ZWF, PGL, GND,
  RPIA, RPE, TKTA1, TKTA2, TALA, GLTA, ACNA, ICDA, SUCA,
  SUCC1, SDHA1, FRDA, FUMA, MDH,
  DLD1, ADHE2, PFLA, PTA, ACKA, ACS, PCKA, PPC, MAEB, SFCA,
  ACEA, ACEB, PPA, GLPK, GPSA1,
  RBSK, NUOA, FDOH, GLPD, CYOA, SDHA2, PNT1A, PNT2A, ATPA,
  GLCUP, GLCPTS, GLUP, RIBUP,
  ACUP, LACUP, FORUP, ETHUP, SUCCUP, PYRUP, PIUP, O2TX,
  CO2TX, ATPM, ADK, Growth

  The model contains:
  \itemize{
    \item 54 equalities (Ax=B): the 53 mass balances
      (one for each substance) and one equation that sets the ATP
      drain flux for constant maintenance requirements to a fixed value (5.87)
    \item 70 unknowns (x), the reaction rates
    \item 62 inequalities (Gx>h). The first 28 inequalities impose
      bounds on some reactions. The last 34 inequalities impose that
      the reaction rates have to be positive (for unidirectional reactions
      only).
    \item 2 functions that have to be maximised, the biomass production
      (growth).
  }
}

\usage{
LIMEcoli
}

\format{
  LIMEcoli is of type \code{lim}, which is a list of matrices, vectors,
  names and values that specify the linear inverse model problem.

  see the return value of \code{\link{Setup}} for more information
  about this list

  A more complete description of this structures is in vignette("LIM")
}

\author{
  Karline Soetaert <karline.soetaert@nioz.nl>
}

\examples{


# 1. parsimonious (simplest) solution
pars <- Ldei(LIMEcoli)

# 2. the ranges of each reaction
xr  <- Xranges(LIMEcoli, central = TRUE, full = TRUE)

# 3. the optimal solution - solved with linear programming
LP  <- Linp(LIMEcoli)
Optimal <- t(LP$X)

# show the results
data.frame(pars = pars$X, Optimal, xr[ ,1:3])

# The central value of linear programming problem is a valid solution
# the central point is a valid solution:
X   <- xr[ ,"central"]
max(abs(LIMEcoli$A\%*\%X - LIMEcoli$B))
min(LIMEcoli$G\%*\%X - LIMEcoli$H)

# 4. Sample solution space  - this takes a while - note that iter is not enough
print(system.time(
  xs <- Xsample(LIMEcoli, iter = 200, type = "mirror", test = TRUE)  ))

pairs(xs[ ,1:10], pch = ".", cex = 2)

# Print results:
data.frame(pars = pars$X, Optimal = Optimal, xr[ ,1:2],
           Mean = colMeans(xs), sd = apply(xs,2,sd))

# Plot results
par(mfrow = c(1, 2))
nr <- LIMEcoli$NUnknowns
ii <- 1:(nr/2)
dotchart(Optimal[ii, 1], xlim = range(xr), pch = 16, cex = 0.8)
segments(xr[ii, 1], 1:nr, xr[ii, 2], 1:nr)
ii <- (nr/2+1):nr
dotchart(Optimal[ii, 1], xlim = range(xr), pch = 16, cex = 0.8)
segments(xr[ii, 1], 1:nr, xr[ii, 2], 1:nr)
mtext(side = 3, cex = 1.5, outer = TRUE, line = -1.5,
      "E coli Core Metabolism, optimal solution and ranges")
}
\seealso{
  browseURL(paste(system.file(package="LIM"), "/doc/examples/Reactions/", sep=""))

  contains "E_coli.lim", the input file; read this with \code{Setup}

}

\references{
  Edwards,J.S., Covert, M., and Palsson, B..,  (2002)
  Metabolic Modeling of Microbes: the Flux Balance Approach,
  Environmental Microbiology, 4(3): pp. 133-140.
}

\keyword{datasets}

