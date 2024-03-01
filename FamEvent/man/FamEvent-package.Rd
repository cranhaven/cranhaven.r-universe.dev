\name{FamEvent-package}
\alias{FamEvent-package}
\docType{package}
\title{
Family age-at-onset data simulation and penetrance estimation
}
\description{

Family-based studies are used to characterize the disease risk associated
with being a carrier of a major gene. When the disease risk can vary with 
age of onset, penetrance or disease risk functions need to provide age-dependent
estimates of this disease risk over lifetime.
This FamEvent package can generate age-at-onset data in the context of familial studies,
with correction for ascertainment (selection) bias arising from a specified
study design based on proband's mutation and disease statuses.
Possible study designs are: \code{"pop"} for population-based design where
families are ascertained through affected probands, \code{"pop+"} are similar to
\code{"pop"} but probands are also known mutation carriers, \code{"cli"} for
clinic-based design that includes affected probands with at least one
parent and one sib affected, \code{"cli+"} are similar to \code{"cli"} but probands are
also known mutation carrriers. And \code{"twostage"} for two-stage design that randomly samples families from
the population in the first stage and oversamples high risk families that includes at least two
affected members in the family at the second stage.

Ages at disease onset are generated specific to family members' gender and
mutation status according to the specified study design with residual
familial correlations induced by either a shared frailty or a second gene.
For estimating age at onset risks with family data, an ascertainment
corrected prospective likelihood approach is used to account for the
population or clinic-based study designs while a composite likelihood
approach is used for the two-stage sampling design. The Expectation and
Maximization (EM) algorithm has been implemented for inferring missing
genotypes conditional on observed genotypes and phenotypes in the families. 
For family members who have missing
genotypes, their carrier probabilities are obtained either from the fitted
model or from Mendelian transmission probabilities. This package also
provides functions to plot the age-dependent penetrance curves estimated
parametrically from the fitted model or non-parametrically from the data,
pedigree plots of simulated families and penetrance function curves for
carriers and non-carriers of a major and second gene based on specified
parameter values.

Generation and model fit of competing risks data are also available based on frailty models.
%\packageDescription{FamilyData}
}
\author{
Yun-Hee Choi, Karen Kopciuk, Laurent Briollais, Wenqing He

Maintainer: Yun-Hee Choi < yun-hee.choi@schulich.uwo.ca >
}
\references{
Choi, Y.-H., Jung, H., Buys, S., Daly, M., John, E.M., Hopper, J., Andrulis, I., Terry, M.B., Briollais, L. (2021) A Competing Risks Model with Binary Time Varying Covariates for Estimation of Breast Cancer Risks in BRCA1 Families, Statistical Methods in Medical Research 30 (9), 2165-2183. https://doi.org/10.1177/09622802211008945.

Choi, Y.-H., Briollais, L., He, W. and Kopciuk, K. (2021) FamEvent: An R Package for Generating and Modeling Time-to-Event Data in Family Designs, Journal of Statistical Software 97 (7), 1-30. doi:10.18637/jss.v097.i07

Choi, Y.-H., Kopciuk, K. and Briollais, L. (2008) Estimating Disease Risk Associated Mutated Genes in Family-Based Designs, Human Heredity 66, 238-251.

Choi, Y.-H. and Briollais (2011) An EM Composite Likelihood Approach for Multistage Sampling of Family Data with Missing Genetic Covariates, Statistica Sinica 21, 231-253.
}
\keyword{ package }
\seealso{
\code{\link{simfam}}, \code{\link{summary.simfam}}, \code{\link{plot.simfam}}, \code{\link{penplot}}, \code{\link{carrierprob}}, \code{\link{penmodel}}, \code{\link{penmodelEM}}, 
\code{\link{print.penmodel}}, \code{\link{summary.penmodel}},\code{\link{print.summary.penmodel}}, \code{\link{plot.penmodel}},
\code{\link{simfam_c}}, \code{\link{summary.simfam_c}}, \code{\link{plot.simfam_c}}, \code{\link{penplot_c}}, \code{\link{penmodel_c}},  
\code{\link{print.penmodel_c}}, \code{\link{summary.penmodel_c}}, 
\code{\link{print.summary.penmodel_c}}, \code{\link{plot.penmodel_c}}
}
\examples{
\dontrun{
# Example1:  Simulate family data 
set.seed(4321)
fam <- simfam(N.fam = 100, design = "pop+", variation = "none", base.dist = "Weibull", 
       base.parms = c(0.01, 3), vbeta = c(-1.13, 2.35), allelefreq = 0.02)

# summary of simulated family data
summary(fam) 

# Pedigree plots for family 1 and 2
plot(fam, famid = c(1,2))

# penetrance function plots given model parameter values for Weibull baseline 
penplot(base.parms = c(0.01, 3), vbeta = c(-1.3, 2.35), base.dist = "Weibull", 
        variation = "none", agemin = 20)

# model fit of family data 
fit <- penmodel(Surv(time, status) ~ gender + mgene, cluster = "famID", design = "pop+",
       parms=c(0.01, 3, -1.13, 2.35), data = fam, base.dist = "Weibull", robust = TRUE)

# summary of estimated model parameters and penetrance estimates 
summary(fit)

# penetrance curves useful for model checking 
plot(fit) 


### Example 2: Simulate correlated competing risks family data 
set.seed(4321)
fam2 <- simfam_c(N.fam = 200, design = "pop+", variation = "frailty", 
       base.dist = "Weibull", frailty.dist = "cgamma", depend=c(1, 2, 0.5), 
       allelefreq = 0.02, base.parms = list(c(0.01, 3), c(0.01, 3)), 
       vbeta = list(c(-1.13, 2.35), c(-1, 2)))

# summary of simulated family data
summary(fam2) 

# Pedigree plots for family 1 
plot(fam2, famid = 1)

# penetrance function plot for event 1 given model parameter values for Weibull baseline 
penplot_c(event = 1, base.parms = list(c(0.01, 3), c(0.01, 3)), 
          vbeta = list(c(-1.3, 2.35), c(-1, 2)), base.dist = "Weibull", 
          variation = "frailty", frailty.dist = "cgamma", 
          depend=c(1,2,0.5), agemin = 20)


# Fitting shared correlated gamma frailty Penetrance model for simulated competing risk data

fit2 <- penmodel_c(
        formula1 = Surv(time, status==1) ~ gender + mgene, 
        formula2 = Surv(time, status==2) ~ gender + mgene,
        cluster = "famID", gvar = "mgene", design = "pop+",  
        parms = list(c(0.01, 3, -1, 2), c(0.01, 3, -1, 2), c(0.5, 1, 0.5)),
        base.dist = "Weibull", frailty.dist = "cgamma", data = fam2, robust = TRUE)

# Summary of the model parameter estimates from the model fit
summary(fit2)

# Plot the lifetime penetrance curves with 95% CIs from the model fit for specific  
# gender and mutation status groups along with their nonparametric penetrance curves  
# based on data excluding probands. 

plot(fit2, add.CIF = TRUE, conf.int = TRUE, MC = 100)
}


}
