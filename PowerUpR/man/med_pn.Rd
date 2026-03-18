\name{med_pn}
\alias{med_pn}
\alias{med_pn21}
\alias{med_pn31}
\alias{med_pn32}
\alias{power.med_pn21}
\alias{power.med_pn31}
\alias{power.med_pn32}
\title{Partially Nested Designs Probing Multilevel Mediation}

\description{
Two/One partially nested design occurs when subjects are recruited into groups for the treatment condition, but the remaining participants (or waiting list) are left for the control group (without any intervention clusters). If the interest is to design a sufficiently powered study to detect mediation effect in Two/One design, use \code{power.med_pn21()}.

Three/One partially nested design occurs when subjects are recruited into groups, and a number of groups are guided by the same leader for the treatment condition, but the remaining participants (or waiting list) are left for the control group (without any intervention clusters). If the interest is to design a sufficiently powered study to detect mediation effect in Three/One design, use \code{power.med_pn31()}.

Three/Two partially nested design occurs when subjects are recruited into groups, and a number of groups are guided by the same leader for the treatment condition, however, separate from Two/One and Three/One designs, the remaining participants are nested within providers in the control group (without any intervention clusters, but with nested structure). If the interest is to design a sufficiently powered study to detect mediation effect in Three/Two design, use \code{power.med_pn31()}.

}

\usage{
power.med_pn21(esa = .50, esB = .50, esb1 = .10,
               two.tailed = TRUE, alpha = .05,
               mc = TRUE, nsims = 1000, ndraws = 1000,
               rhom_trt = .20, rho_trt = .20,
               r2m1_ctrl = .20, r2m1_trt = .20, r2m2_trt = .20,
               r2y1z_trt = 0, r2y2z_trt = 0,
               r2y1_trt = NULL, r2y2_trt = NULL,
               g1_ctrl = 0, n_ctrl =  20,
               g2_trt = 0, n_trt = 30, J_trt = 20)

power.med_pn31(esa = .50, esB = .50, esb1 = .10,  esb2 = .10,
               two.tailed = TRUE, alpha = .05,
               mc = TRUE, nsims = 1000, ndraws = 1000,
               rhom3_trt = .20, rhom2_trt = .20,
               rho3_trt = .20, rho2_trt = .20,
               r2m1_ctrl = .20, r2m1_trt = .20, r2m2_trt = .20, r2m3_trt = .20,
               r2y1z_trt = 0, r2y2z_trt = 0, r2y3z_trt = 0,
               r2y1_trt = NULL, r2y2_trt = NULL, r2y3_trt = NULL,
               g1_ctrl = 0, n_ctrl =  20,
               g3_trt = 0, n_trt = 30, J_trt = 20, K_trt = 20)

power.med_pn32(esa = .50, esB = .50, esb1 = .10,  esb2 = .10,
               two.tailed = TRUE, alpha = .05,
               mc = TRUE, nsims = 1000, ndraws = 1000,
               rhom2_ctrl = .20, rhom3_trt = .20, rhom2_trt = .20,
               rho3_trt = .20, rho2_trt = .20,
               r2m1_ctrl = .20, r2m2_ctrl = .20,
               r2m1_trt = .20, r2m2_trt = .20, r2m3_trt = .20,
               r2y1z_trt = 0, r2y2z_trt = 0, r2y3z_trt = 0,
               r2y1_trt = NULL, r2y2_trt = NULL, r2y3_trt = NULL,
               g2_ctrl = 0, n_ctrl = 30, J_ctrl = 20,
               g3_trt = 0, n_trt =  20, J_trt = 20,  K_trt = 20)
}

\arguments{
  \item{esa}{Standardized mean difference in mediator values between treatment and control groups (esa = esa_trt - esa_ctrl).}
  \item{esB}{Standardized regression coefficient for mediator - outcome path, capturing level 1 and level 2 (between intervention clusters) effects (esB = esb1 + esb2 for Two/One design, or esB = esb1 + esb2 + esb3 for Three/One or Three/Two designs).}
  \item{esb1}{Standardized regression coefficient for mediator - outcome path at level 1 in the treatment group (within first intervention clusters).}
  \item{esb2}{Standardized regression coefficient for aggregate mediator - outcome path at level 2 in the treatment group (between first intervention clusters).}
  \item{alpha}{probability of type I error.}
  \item{two.tailed}{logical; \code{TRUE} for two-tailed hypothesis testing, \code{FALSE} for one-tailed hypothesis testing.}
  \item{rhom2_ctrl}{Proportion of unconditional mediator variance between level 2 units (intervention clusters) in the control group.}
  \item{rhom3_trt}{Proportion of unconditional mediator variance between level 3 units (second intervention clusters) in the treatment group.}
  \item{rhom2_trt}{Proportion of unconditional mediator variance between level 2 units (first intervention clusters) in the treatment group.}
  \item{rhom_trt}{Proportion of unconditional mediator variance between level 2 units (intervention clusters) in the treatment group.}
  \item{rho3_trt}{Proportion of unconditional outcome variance between level 3 units (second intervention clusters) in the treatment group.}
  \item{rho2_trt}{Proportion of unconditional outcome variance between level 2 units (first intervention clusters) in the treatment group.}
  \item{rho_trt}{Proportion of unconditional outcome variance between level 2 units (intervention clusters) in the control group.}
  \item{r2m1_ctrl}{Proportion of variance in the mediator explained by level 1 covariates in the control group.}
  \item{r2m2_ctrl}{Proportion of variance in the aggregate mediator explained by level 2 covariates in the control group.}
  \item{r2m1_trt}{Proportion of variance in the level 1 mediator explained by level 1 covariates in the treatment group}
  \item{r2m2_trt}{Proportion of variance in the aggregate mediator (at level 2) explained by level 2 covariates in the treatment group.}
  \item{r2m3_trt}{Proportion of variance in the aggregate mediator (at level 3) explained by level 3 covariates in the treatment group.}
  \item{r2y1z_trt}{Proportion of variance in the level 1 outcome explained by level 1 covariates in the treatment group.}
  \item{r2y2z_trt}{Proportion of variance in the aggregate outcome (at level 2) explained by level 2 covariates in the treatment group.}
  \item{r2y3z_trt}{Proportion of variance in the aggregate outcome (at level 3) explained by level 3 covariates in the treatment group.}
  \item{r2y1_trt}{Proportion of variance in the level 1 outcome explained by level 1 predictors (including mediator) in the treatment group.}
  \item{r2y2_trt}{Proportion of variance in the aggregate outcome (level 2) explained by level 2 predictors (including aggregate mediator) in the treatment group.}
  \item{r2y3_trt}{Proportion of variance in the aggregate outcome (level 3) explained by level 3 predictors (including aggregate mediator) in the treatment group.}
  \item{g1_ctrl}{Number of covariates introduced at level 1 in the control group.}
  \item{g2_ctrl}{Number of covariates introduced at level 2 (intervention clusters) in the control group.}
  \item{n_ctrl}{Total number of subjects for two/one mediation. Number of subjects per level 2 unit (intervention cluster) in the control group for Three/One and Three/Two mediation.}
  \item{J_ctrl}{Number of level 2 units (intervention clusters) in the control group.}
  \item{g2_trt}{Number of covariates introduced at level 2 (intervention clusters) in the treatment group.}
  \item{g3_trt}{Number of covariates introduced at level 3 (second intervention clusters) in the treatment group.}
  \item{n_trt}{Number of subjects per level 2 unit (first intervention cluster).}
  \item{J_trt}{Number of level 2 units per level 3 unit (second intervention cluster.)}
  \item{K_trt}{Number of level 3 units (second intervention clusters).}
  \item{mc}{logical; \code{TRUE} for monte carlo simulation based power.}
  \item{nsims}{number of replications, if \code{mc = TRUE}.}
  \item{ndraws}{number of draws from the distribution of the path coefficients for each replication, if \code{mc = TRUE}.}
}

\value{
  \item{fun}{function name.}
  \item{parms}{list of parameters used in power calculation.}
  \item{df}{degrees of freedom.}
  \item{ncp}{noncentrality parameter.}
  \item{power}{statistical power \eqn{(1-\beta)}.}
}

\examples{
# Two/One partially nested design
power.med_pn21(esa = .40, esB = .40, esb1 = .40,
               two.tailed = TRUE, alpha = .05,
               rhom_trt = .20, rho_trt = .20,
               r2m1_ctrl = .60, r2m1_trt = .60, r2m2_trt = .60,
               r2y1_trt = .50, r2y2_trt = .50,
               n_ctrl = 50, n_trt =  6, J_trt = 50)

# Three/One partially nested design
power.med_pn31(esa = .50, esB = .50, esb1 = .10, esb2 = .10,
               rhom3_trt = .10, rhom2_trt = .20, rho3_trt = .10, rho2_trt = .20,
               r2m1_ctrl = .20, r2m1_trt = .20, r2m2_trt = .20, r2m3_trt = .20,
               r2y1_trt = .20, r2y2_trt = .20, r2y3_trt = .20,
               n_ctrl = 60, n_trt =  20, J_trt = 10, K_trt = 60)

# Three/Two partially nested design
power.med_pn32(esa = .50, esB = .50, esb1 = .10, esb2 = .10,
               rhom2_ctrl = .20, rhom3_trt = .10, rhom2_trt = .20,
               rho3_trt = .10, rho2_trt = .20,
               r2m1_ctrl = .20, r2m2_ctrl = .20,
               r2m1_trt = .20, r2m2_trt = .20, r2m3_trt = .20,
               r2y1_trt = .20, r2y2_trt = .20, r2y3_trt = .20,
               n_ctrl = 24, n_trt =  24, J_trt = 12, J_ctrl = 60, K_trt = 60)
}

\references{
Kelcey, B., Bai, F., \& Xie, Y. (2020). Statistical power in partially nested designs probing multilevel mediation. \emph{Psychotherapy Research}. Advance online publication. \doi{10.1080/10503307.2020.1717012}
}


