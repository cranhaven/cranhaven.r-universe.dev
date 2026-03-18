\name{dulxparx}
\alias{dulxparx}
\docType{data}
\title{Data for the High Uncertainty numerical example of Obenchain et al. (2005)}
\description{The data are from two arms of a double-blind clinical trial in which 91 patients
 were randomized to the SNRI duloxetine 80 mg/d (40 mg BID) and 87 patients were randomized to
 the SSRI paroxetine 20 mg/d for treatment of major depressive disorder (MDD).  Missing-data-
 imputation and sensitivity-analyses were needed to make meaningful cost-effectiveness
 comparisons in this study.
}
\usage{data(dulxparx)}
\format{
  A data frame of 3 variables on 178 patients; no NAs.
  \describe{
    \item{idb}{This measure of overall effectiveness is \emph{integrated decrease in HAMD-17
     score from baseline to endpoint}, Hamilton (1967).  This is a (signed) area-under-the-curve
     measure with larger values more favorable.  Missing values were imputed via the MMRM models
     reported in Goldstein et al. (2004).
     }
    \item{ru}{Patient self-reported health-care resource utilization above and beyond that
     provided within study protocol was collected using the Resource Utilization Survey,
     Copley-Merriman et al. (1992), with published 1998 dollars-per-unit costs, Schoenbaum et al. (2001),
     rounded to the nearest 50 dollars.  Dollars/week were then calculated by multiplying (total accumulated
     cost) for a patient by 7 and dividing by the (total days of cost accumulation) for that
     patient.  For patients who discontinued early, this is Average-Value-Carried-Forward
     imputation.
     }
    \item{dulx}{Treatment indicator variable. dulx = 1 implies receipt of duloxetine 80 mg/d
     (40 mg BID). dulx = 0 implies receipt of paroxetine 20 mg/d.
     }
  }
}
\references{
  Copley-Merriman C, Egbuonu-Davis L, Kotsanos JG, Conforti P, Franson T, Gordon G.  Clinical
  economics: a method for prospective health resource data collection.  \emph{Pharmacoeconomics}
  1992; \bold{1(5)}: 370--376.

  Goldstein DJ, Lu Y, Detke MJ, Wiltse C, Mallincrodt C, Demitrack MA.  Duloxetine in the
  treatment of depression - A double-blind, placebo-controlled comparison with paroxetine.
  \emph{J Clin Psychopharmacol}  2004;  \bold{24}: 389--399.
  
  Hamilton M.  Development of a rating scale for primary depressive illness.  \emph{British
  Journal of Social and Clinical Psychology} 1967; \bold{6}: 278--296.
   
  Obenchain RL, Robinson RL, Swindle RW.  Cost-effectiveness inferences from bootstrap quadrant
  confidence levels:  three degrees of dominance.  \emph{J Biopharm Stat}  2005; \bold{15(3)}:
  419--436.
  
  Schoenbaum M, Unutzer J, Sherbourne C, Duan N, Rubenstein LV, Miranda J, Meredith LS, Carney
  MF, Wells K.  Cost-effectiveness of practice-initiated quality improvement for depression:
  results of a randomized controlled trial. \emph{JAMA} 2001; \bold{286(11)}: 1325--1330.
}
\examples{
    data(dulxparx)
    ICEscale(dulxparx, dulx, idb, ru)
}
\keyword{datasets}