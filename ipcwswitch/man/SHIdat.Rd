\name{SHIdat}
\alias{SHIdat}
\docType{data}
\title{
A real example dataset from the randomized clinical trial SHIVA
}
\description{
Dataset SHIdat contains an anonymized excerpt of data from the SHIVA01 trial. This was the first randomized cli,ical trial that aimed at comparing molecularly targeted therapy based on tumour profiling (MTA) versus conventiona therapy (CT) for advanced cancer. A switch to the other arm was scheduled to be proposed at disease progression for patients in both treatment groups.
}
\usage{data("SHIdat")}
\format{
  A data frame with 197 observations on the following 306 variables.
  \describe{
    \item{\code{id}}{a numeric vector corresponding to the patient's identifier}
    \item{\code{bras.f}}{a vector containing the patient's randomized arm}
    \item{\code{agerand}}{a numeric vector containing patient's age (in years) at randomization}
    \item{\code{sex.f}}{a vector containing the patient's gender}
    \item{\code{tt_Lnum}}{a numeric vector containing the number of previous lines of treatment}
    \item{\code{rmh_alea.c}}{a numeric vector containing  the Royal Marsden Hospital score segregated into two categories}
    \item{\code{pathway.f}}{a vector the molecular pathway altered (pathway.f: the hormone receptors pathway, the PI3K/ AKT/mTOR pathway, and the RAF/MEK pathway) }
    \item{\code{myps.v2},\code{ps.v3},\code{ps1.v1},\code{ps1.v2},\code{ps1.v3},\code{ps1.v4},\code{ps1.v5},\code{ps1.v6},\code{ps1.v7},\code{ps1.v8},\code{ps1.v9},\code{ps1.v10},\code{ps1.v11},\code{ps1.v12}, \code{ps1.v13},\code{ps1.v14},\code{ps1.v15},\code{ps1.v16},\code{ps1.v17},\code{ps1.v18},\code{ps1.v19},\code{ps1.v20}, \code{ps1.v21}}{numeric vectors containing the ECOG performance status measured at the randomization visit, the visit before the potential switch and the planned visits (maximum number of planned visits: 21)}
    \item{\code{mytran.v1},\code{tran.v2},\code{tran.v3},\code{tran.v4},\code{tran.v5},\code{tran.v6},\code{tran.v7},\code{tran.v8},\code{tran.v9},\code{tran.v10},\code{tran.v11},\code{tran.v12}, \code{tran.v13},\code{tran.v14},\code{tran.v15},\code{tran.v16},\code{tran.v17},\code{tran.v18},\code{tran.v19},\code{tran.v20},\code{tran.v21}}{numeric vectors containing the use of platelet transfusions at each of the potential 21 planned visits}
    \item{\code{myttc.v2},\code{ttc.v3},\code{ttc1.v1},\code{ttc1.v2},\code{ttc1.v3},\code{ttc1.v4},\code{ttc1.v5},\code{ttc1.v6},\code{ttc1.v7},\code{ttc1.v8},\code{ttc1.v9},\code{ttc1.v10}, \code{ttc1.v11},\code{ttc1.v12},\code{ttc1.v13},\code{ttc1.v14},\code{ttc1.v15},\code{ttc1.v16},\code{ttc1.v17},\code{ttc1.v18},\code{ttc1.v19},\code{ttc1.v20}, \code{ttc1.v21}}{numeric vectors containing the presence of concomitant treatments at the randomization visit, the visit before the potential switch and the planned visits (maximum number of planned visits: 21)}
    \item{\code{tox.t1},\code{tox.t2},\code{tox.t3},\code{tox.t4},\code{tox.t5},\code{tox.t6},\code{tox.t7},\code{tox.t8},\code{tox.t9},\code{tox.t10},\code{tox.t11},\code{tox.t12},\code{tox.t13},\code{tox.t14}, \code{tox.t15},\code{tox.t16},\code{tox.t17},\code{tox.t18},\code{tox.t19},\code{tox.t20},\code{tox.t21},\code{tox.t22},\code{tox.t23},\code{tox.t24},\code{tox.t25},\code{tox.t26}, \code{tox.t27},\code{tox.t28},\code{tox.t29},\code{tox.t30},\code{tox.t31},\code{tox.t32},\code{tox.t33},\code{tox.t34},\code{tox.t35},\code{tox.t36},\code{tox.t37},\code{tox.t38}, \code{tox.t39},\code{tox.t40},\code{tox.t41},\code{tox.t42},\code{tox.t43},\code{tox.t44},\code{tox.t45},\code{tox.t46},\code{tox.t47},\code{tox.t48},\code{tox.t49},\code{tox.t50}, \code{tox.t51},\code{tox.t52},\code{tox.t53},\code{tox.t54},\code{tox.t55},\code{tox.t56},\code{tox.t57},\code{tox.t58},\code{tox.t59},\code{tox.t60},\code{tox.t61},\code{tox.t62}, \code{tox.t63},\code{tox.t64},\code{tox.t65},\code{tox.t66},\code{tox.t67},\code{tox.t68},\code{tox.t69},\code{tox.t70},\code{tox.t71},\code{tox.t72},\code{tox.t73},\code{tox.t74}, \code{tox.t75},\code{tox.t76},\code{tox.t77},\code{tox.t78},\code{tox.t79},\code{tox.t80},\code{tox.t81},\code{tox.t82},\code{tox.t83},\code{tox.t84},\code{tox.t85},\code{tox.t86}, \code{tox.t87},\code{tox.t88},\code{tox.t89},\code{tox.t90},\code{tox.t91},\code{tox.t92},\code{tox.t93},\code{tox.t94},\code{tox.t95},\code{tox.t96},\code{tox.t97},\code{tox.t98}, \code{tox.t99}, \code{tox.t100}}{numeric vectors corresponding to the presence of an adveerse event. \code{tox.ti} contains 1 if the patient started an adverse event linked with the treatment at \code{datetox.ti}, 0 if the patient ended an adverse event linked with the treatment at \code{datetox.ti}, and NA otherwise}
    \item{\code{ddn}}{a vector containing the date of latest news}
    \item{\code{debttCO}}{a vector containing the date of initiation of the other arm treatment}
    \item{\code{ddeath}}{a vector containg the death date}
    \item{\code{ddt.v1}}{a vector containing the date of initiation of the randomized treatment}
    \item{\code{datt}}{a vector containing the date of the interruption of the randomized treatment}
    \item{\code{dexac.v2}}{a vector containing the date of randomization}
    \item{\code{dexac.v3}}{a vector containing the date of the visit before the potential switch}
    \item{\code{dexac1.v1},\code{dexac1.v2},\code{dexac1.v3},\code{dexac1.v4},\code{dexac1.v5},\code{dexac1.v6},\code{dexac1.v7},\code{dexac1.v8},\code{dexac1.v9}, \code{dexac1.v10},\code{dexac1.v11},\code{dexac1.v12},\code{dexac1.v13},\code{dexac1.v14},\code{dexac1.v15},\code{dexac1.v16},\code{dexac1.v17}, \code{dexac1.v18},\code{dexac1.v19},\code{dexac1.v20}, \code{dexac1.v21}}{vectors containing the dates of the potential 21 planned visits}
    \item{\code{datetox.t1},\code{datetox.t2},\code{datetox.t3},\code{datetox.t4},\code{datetox.t5},\code{datetox.t6},\code{datetox.t7},\code{datetox.t8}, \code{datetox.t9},\code{datetox.t10},\code{datetox.t11},\code{datetox.t12},\code{datetox.t13},\code{datetox.t14},\code{datetox.t15}, \code{datetox.t16},\code{datetox.t17},\code{datetox.t18},\code{datetox.t19},\code{datetox.t20},\code{datetox.t21},\code{datetox.t22}, \code{datetox.t23},\code{datetox.t24},\code{datetox.t25},\code{datetox.t26},\code{datetox.t27},\code{datetox.t28},\code{datetox.t29}, \code{datetox.t30},\code{datetox.t31},\code{datetox.t32},\code{datetox.t33},\code{datetox.t34},\code{datetox.t35},\code{datetox.t36}, \code{datetox.t37},\code{datetox.t38},\code{datetox.t39},\code{datetox.t40},\code{datetox.t41},\code{datetox.t42},\code{datetox.t43}, \code{datetox.t44},\code{datetox.t45},\code{datetox.t46},\code{datetox.t47},\code{datetox.t48},\code{datetox.t49},\code{datetox.t50}, \code{datetox.t51},\code{datetox.t52},\code{datetox.t53},\code{datetox.t54},\code{datetox.t55},\code{datetox.t56},\code{datetox.t57}, \code{datetox.t58},\code{datetox.t59},\code{datetox.t60},\code{datetox.t61},\code{datetox.t62},\code{datetox.t63},\code{datetox.t64}, \code{datetox.t65},\code{datetox.t66},\code{datetox.t67},\code{datetox.t68},\code{datetox.t69},\code{datetox.t70},\code{datetox.t71}, \code{datetox.t72},\code{datetox.t73},\code{datetox.t74},\code{datetox.t75},\code{datetox.t76},\code{datetox.t77},\code{datetox.t78}, \code{datetox.t79},\code{datetox.t80},\code{datetox.t81},\code{datetox.t82},\code{datetox.t83},\code{datetox.t84},\code{datetox.t85}, \code{datetox.t86},\code{datetox.t87},\code{datetox.t88},\code{datetox.t89},\code{datetox.t90},\code{datetox.t91},\code{datetox.t92}, \code{datetox.t93},\code{datetox.t94},\code{datetox.t95},\code{datetox.t96},\code{datetox.t97},\code{datetox.t98},  \code{datetox.t99}, \code{datetox.t100}}{vectors containing the dates related to adverse events (as explained above)}
    \item{\code{CO}}{a vector containing 1 if the patient changed treatment arm (i.e., did a switch)}
    \item{\code{progDate}}{a vector containing the date of a potential progression}
    \item{\code{progStatus}}{a vector containing 1 if the patient did a progression (and 0 otherwise)}
    \item{\code{status}}{a vector containing the patient's status at the date of latest news (1 if died, 0 otherwise)}
  }
}
\details{
Note that some variables were built from the original data for illustration purpose. We provided an excerpt containing only the covariates that are useful for our analysis. Note also that the SHIVA data were anonymized.

Acknowledgments: we thank the patients who volunteered to participate in this study for their dedication and the study-site staff who cared for them. This work is supported by grant ANR-10-EQPX-03 from the Agence Nationale de la Recherche (Investissements d'avenir) and Site de Recherche Integre contre le Cancer (SiRIC). High-throughput sequencing was done by the NGS platform of the Institut Curie, supported by grants ANR-10-EQPX-03 and ANR-10-INBS-09-08 from the Agence Nationale de la Recherche (Investissements d'avenir) and the Canceropole Ile-de-France.
}

\references{
Le Tourneau, C., Delord, J. P., Goncalves, A., et al. (2015). "Molecularly targeted therapy based on tumour molecular profiling versus conventional therapy for advanced cancer (SHIVA): a multicentre, open-label, proof-of-concept, randomised, controlled phase 2 trial". The Lancet Oncology, 16(13), 1324-1334. doi : "10.1016/S1470-2045(15)00188-6"

Graffeo, N., Latouche, A., Le Tourneau C., Chevret, S. (2019) "ipcwswitch: an R package for inverse probability of censoring weighting with an application to switches in clinical trials". Computers in biology and medicine, 111, 103339. doi : "10.1016/j.compbiomed.2019.103339"
}
\examples{
\donttest{
# To obtain the times parameter, we can apply the timesTokeep function on the same
# dataframe in the wide format
# names of the repeated measurements
vect.ps   <- c("myps.v2", "ps.v3", c(paste("ps1.v", seq(1,21), sep="")))
vect.ttc  <- c("myttc.v2", "ttc.v3", c(paste("ttc1.v", seq(1,21), sep="")))
vect.tran <- c("mytran.v1", paste("tran.v", seq(2,21), sep=""))
# corresponding dates
dates <- c("dexac.v2", "dexac.v3", c(paste("dexac1.v", seq(21), sep="")))
dates2 <- dates[!(dates \%in\% c("dexac.v2","dexac.v3"))]

# times to keep
kept.t <- timesTokeep(SHIdat, id = "id",
                      tstart = "dexac.v2", tstop = "ddn",
                      mes.cov = list(vect.ps, vect.ttc, vect.tran),
                      time.cov = list(dates, dates, dates2))
# Now, we can build the long format
SHIlong <- wideToLongTDC(SHIdat, id = "id",
                         tstart = "dexac.v2", tstop = "ddn",
                         event = "status",
                         bas.cov = c("agerand", "sex.f","tt_Lnum", "rmh_alea.c", "pathway.f",
                         "bras.f","debttCO","ddt.v1", "datt"),
                         mes.cov = list(f1=vect.ps, f2=vect.ttc, f3=vect.tran),
                         time.cov = list(dates, dates, dates2),
                         times = kept.t[[1]])

# Put dates in numeric format with tstart at 0
tabi <- split(SHIlong, SHIlong$id)
L.tabi   <- length(tabi)
tablist <- lapply(1:L.tabi, function(i){
  refstart <- tabi[[i]]$tstart[1]

  tabi[[i]]$tstart  <- tabi[[i]]$tstart - refstart
  tabi[[i]]$tstop <- tabi[[i]]$tstop - refstart
  tabi[[i]]$debttCO <- tabi[[i]]$debttCO - refstart # to be used in next step
  tabi[[i]]$ddt.v1 <- tabi[[i]]$ddt.v1 - refstart # to be used in the final step
  tabi[[i]]$datt <- tabi[[i]]$datt - refstart # to be used in the final step

  return(tabi[[i]])
})
SHIlong <- do.call( rbind, tablist )
colnames(SHIlong)[14:16] <- c("ps", "ttc", "tran")

# Eliminating patient not having initiated the treatment arm
SHIlong2 <- SHIlong[!is.na(SHIlong$ddt.v1),]

# Patients are censored when initiating the other arm treatment, that is, at time swtrtdt
SHIlong2 <- cens.ipw(SHIlong2, id = "id", tstart = "tstart", tstop  = "tstop",
                     event = "event", arm = "bras.f", realtrt = FALSE,
                     censTime ="debttCO")
# We collect all event times 
# (death for both arms and treatment censoring according to the trt arm)
replic.times.MTA <-
    unique(c(SHIlong2$tstop[SHIlong2$cens == 1 &
    SHIlong2$bras.f == "MTA"],
    SHIlong2$tstop[SHIlong2$event == 1]))
replic.times.CT <-
    unique(c(SHIlong2$tstop[SHIlong2$cens == 1 &
    SHIlong2$bras.f == "CT"],
    SHIlong2$tstop[SHIlong2$event == 1]))
# to put times in same order as arms levels
levels(SHIlong2[,"bras.f"])
SHIrep  <- replicRows(SHIlong2, tstart = "tstart", tstop = "tstop",
                      event = "event", cens = "cens", 
                      times1 = replic.times.MTA, times2=replic.times.CT, 
                      arm = "bras.f")

# Estimation of the stabilized weights
library(survival)
SHIres <- ipcw(SHIrep, id = "id", tstart = tstart, tstop = tstop, cens = cens,
               arm = "bras.f", 
               bas.cov = c("agerand", "sex.f", "tt_Lnum", "rmh_alea.c", "pathway.f"),
               conf = c("ps", "ttc", "tran"),
               trunc = 0.05, type = 'kaplan-meier')

# To have conventional therapy (CT) as reference
SHIres$bras.f <- relevel(SHIres$bras.f, ref="CT")

# Using the IPCW weights in Cox likelihood...
fit.stab.w <- coxph(Surv(tstart, tstop, event) ~ bras.f + agerand + sex.f +
                        tt_Lnum + rmh_alea.c + pathway.f
                      + cluster(id),
                      data = SHIres, weights = SHIres$weights.trunc)
fit.stab.w

}
}
\seealso{
\code{\link{cens.ipw}}, \code{\link{ipcw}}, \code{\link{replicRows}}, \code{\link{timesTokeep}}, \code{\link{wideToLongTDC}}
}
\keyword{datasets}
