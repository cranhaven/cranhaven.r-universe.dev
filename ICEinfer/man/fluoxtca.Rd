\name{fluoxtca}
\alias{fluoxtca}
\docType{data}
\title{Cost-Effectiveness data for 1242 MDD patients from Marketscan(SM) claims database}
\description{
 In 1990-1992, the Marketscan(SM) database included medical and pharmacy claims for
 approximately 700,000 individuals whose health insurance was provided by large corporations
 throughout the United States.  Outcomes for 1242 patients treated with either fluoxetine (SSRI)
 or with a TCA / HCA for major depressive disorder (MDD) were discussed in Croghan et al. (1996)
 and Obenchain et al. (1997).  All 1242 patients were continuously enrolled for at least 4
 months prior to their initial antidepressant prescription and for the following 12 months.
}
\usage{data(fluoxtca)}
\format{
  A data frame of 3 variables on 1242 patients; no NAs.
  \describe{
    \item{stable}{stable = 1 indicates that the patient remained on his/her initial
     antidepressant medication for at least six consecutive months.
     }
    \item{cost}{cost is the Marketscan(SM) 12 month total annual charge for a patient. 
     }
    \item{fluox}{Treatment indicator variable; fluox = 1 indicates receipt of fluoxetine 20
     mg/d by 799 patients.  fluox = 0 implies receipt of either a tricyclic (TCA) or a
     heterocyclic (HCA) by 443 patients.
     }
  }
}
\details{
 This dataset contains measures of cost and efffectiveness for 799 patients treated with
 fluoxetine (a Selective Serotonin Reuptake Inhibitor or SSRI), 104 patients treated with a
 first generation tricyclic, TCA (amitriptyline or imipramine), 250 patients treated with a
 second generation TCA (desipramine or nortriptyline), and 89 patients treated with trazodone
 (a heterocyclic, HCA).
}
\references{
  Croghan TW, Lair TJ, Engelhart L, et al. Effect of antidepressant therapy on health
  care utilization and costs in primary care.  Working paper, Eli Lilly and Company,
  1996. (Presented in part at the \emph{Association for Health Services Research} meeting,
  Chicago, June 9, 1995.)

  Obenchain RL, Melfi CA, Croghan TW, Buesching DP. Bootstrap analyses of cost-effectiveness
  in antidepressant pharmacotherapy.  \emph{PharmacoEconomics} 1997; \bold{17}: 1200--1206.
  
  Sclar DA, Robison LM, Skaer TL, Legg RF, Nemec NL, Galin RS, Hugher TE, Buesching DP.
  Antidepressant pharmacotherapy: economic outcomes in a health maintenance organization.
  \emph{Clin Ther} 1994; \bold{16}: 715--730.
}
\examples{
  data(fluoxtca)
  ICEscale(fluoxtca, fluox, stable, cost)
}
\keyword{datasets}
