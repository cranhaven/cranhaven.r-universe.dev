#'  Hazard ratios and correlations for the 2x2 statistics
#'
#' Computes  the hazard ratios, confidence intervals, p-values, and correlations
#' for the overall A, simple A, and simple AB logrank statistics.
#' @param time follow-up times
#' @param event event indicators (0/1)
#' @param indA treatment A indicators (0/1)
#' @param indB treatment B indicators (0/1)
#' @param covmat matrix of covariates; one row per subject.  NOTE!! Factor variables must
#' 	use 0/1 indicator variables
#' @details This function computes
#' (i) correlation between the overall A test and the simple A test
#' (ii) correlation between the overall A test and the simple AB test
#' (iii) correaltion between the simple A and simple AB test.
#' The correlation estimates are derived in
#' Lin, Gong, Gallo, et al. (Biometrics 2016).
#' @references Lin, D.Y., Glong , J., Gallo, P., Bunn, P.H., Couper, D.
#' Simultaneous inference on treatment effects in survival studies
#' with factorial designs. Biometrics, 2016; 72: 1078-1085.
#' @return \item{loghrA }{overall A log hazard ratio}
#' @return \item{seA }{standard error of the overall A log hazard ratio}
#' @return \item{hrA }{overall A hazard ratio}
#' @return \item{ciA }{95\% confidence interval for overall A hazard ratio}
#' @return \item{pvalA }{two-sided p-value for overall A hazard ratio}
#' @return \item{loghra }{simple A log hazard ratio}
#' @return \item{sea }{standard error of the simple A log hazard ratio}
#' @return \item{hra }{simple A hazard ratio}
#' @return \item{cia }{95\% confidence interval for simple A hazard ratio}
#' @return \item{pvala }{two-sided p-value for simple A hazard ratio}
#' @return \item{loghrab }{simple AB log hazard ratio}
#' @return \item{seab}{standard error of the simple AB log hazard ratio}
#' @return \item{hrab }{simple AB hazard ratio}
#' @return \item{ciab}{95\% confidence interval for simple AB hazard ratio}
#' @return \item{pvalab }{two-sided p-value for simple AB hazard ratio}
#' @return \item{corAa }{correlation between the overall A and simple A test statistics}
#' @return \item{corAab }{correlation between the overall A and simple AB test statistics}
#' @return \item{coraab }{correlation between the simple A and simple AB test statistics}
#' @import survival
#' @import stats
#' @export cor2x2
#' @examples
#' # First load the simulated data variables. The "simdat" file is
#' # a 100-by-9 matrix which is loaded with the factorial2x2 package.
#' time <- simdat[, "time"]
#' event <- simdat[, "event"]
#' indA <- simdat[, "indA"]
#' indB <- simdat[, "indB"]
#' covmat <- simdat[, 6:10]
#'
#' cor2x2(time, event, indA, indB, covmat)
#' # $loghrA
#' # [1] 0.05613844
#'
#' # $seA
#' # [1] 0.4531521
#'
#' # $hrA
#' # [1] 1.057744
#'
#' # $ciA
#' # [1] 0.4351608 2.5710556
#'
#' # $pvalA
#' # [1] 0.9014069
#'
#' # $loghra
#' # [1] 0.1987329
#'
#' # $sea
#' # [1] 0.6805458
#'
#' # $hra
#' # [1] 1.219856
#'
#' # $cia
#' # [1] 0.3213781 4.6302116
#'
#' # $pvala
#' # [1] 0.7702714
#'
#' # $loghrab
#' # [1] 0.2864932
#'
#' # $seab
#' # [1] 0.6762458
#'
#' # $hrab
#' # [1] 1.331749
#'
#' # $ciab
#' # [1] 0.3538265 5.0125010
#'
#' # $pvalab
#' # [1] 0.6718193
#'
#' # $corAa
#' # [1] 0.6123399
#'
#' # $corAab
#' # [1] 0.5675396
#'
#' # $coraab
#' # [1] 0.4642737



cor2x2 <- function(time, event, indA, indB, covmat){

	npat <- length(time)
# cbind the treatment indicator to the covariate matrix
	covmat <- cbind(indA, covmat)
	ncov <- ncol(covmat) # number of covariates + 1
	indaux <- table(indB, indA)
	ntrt00 <- indaux[1,1] # number of B-A- patients
	ntrt01 <- indaux[1,2] # number of B-A+ patients
	ntrt10 <- indaux[2,1] # number of B+A- patients
	ntrt11 <- indaux[2,2] # number of B+A+ patients
	nstr0 <- ntrt00 + ntrt01 # number of B- patients
	nstr1 <- ntrt10 + ntrt11 # number of B+ patients


# Order all of the inputs according to increasing times within each
# stratum.  All B- data comes before B+ data.  Given the
# stratum, A- group data comes before A+ group data.

	ord <- order(indB, indA, time, -event)
	time <- time[ord]
	event <- event[ord]
	indA <- indA[ord]
	indB <- indB[ord]
	covmat <- covmat[ord, ]

# Fit the Cox model for the overall (i.e., stratified) statistic.
	fit <- coxph(Surv(time, event) ~ strata(indB)
						 + covmat, method = "breslow")
	# For reasons I don't understand in the next 2 lines,
	# I need to do the following to clear the name
	# assigned to loghrAa and seAa from "fit".
	# I guess using rep(1,2) in this matter makes
	# the list namely genuinely null.
	loghrA <- (fit$coef[1] * rep(1,2))[1]
	seA <- (sqrt(fit$var[1,1]) * rep(1,2))[1]
	hrA <- exp(loghrA)
	ciA <- exp(loghrA + 1.96 * c(-seA, seA))
	pvalA <- 2 * (1 - pnorm(abs(loghrA/seA)))
	fitD <- coxph.detail(fit)
	imatover <- apply(fitD$imat, c(1,2), sum)
	invover <- solve(imatover)

# Make the npat-by-1 risk vector exp(covmat %*% beta).
	riskvec <- exp(covmat %*% fit$coef)

# Want to create the at-risk matrix, called "atrskmat,"
# for all patients.  The matrix
# will be a square npat-by-npat matrix.  Rows will correspond to each
# patient; columns will correspond to the follow-up time for each
# patient.

# Make the nstr0-by-nstr0 at-risk matrix for B-.
	aux0atrsk <-  matrix(rep(1, nstr0), ncol = 1) %*% time[1:nstr0]
	str0atrsk <- apply(aux0atrsk, 2, ">=", time[1:nstr0])
# str0atrsk has each patient corresponding to a column.  We want each
# patient corresponding to a row.
	str0atrsk <- t(str0atrsk)

# Post-cbind npat - nstr0 columns of nstr0 zeros each to make a
# nstr0-by-npat matrix which we'll rbind with the corresponding matrix
# for B+ below.
	str0atrsk <- cbind(str0atrsk, matrix(rep(0,
					nstr0 *(npat - nstr0)), nrow = nstr0))

# Make the nstr1-by-nstr1 at-risk matrix for B+.
	aux1atrsk <-  matrix(rep(1, nstr1), ncol = 1) %*%
				time[(nstr0 + 1) : npat]
	str1atrsk <- apply(aux1atrsk, 2, ">=", time[(nstr0 + 1) : npat])
# str1atrsk has each patient corresponding to a column.  We want each
# patient corresponding to a row.
	str1atrsk <- t(str1atrsk)

# Pre-cbind npat - nstr1 columns of nstr1 zeros each to make a
# nstr1-by-npat matrix which we'll rbind with the corresponding matrix
# for B+ below.
	str1atrsk <- cbind(matrix(rep(0, nstr1 *(npat - nstr1)), nrow = nstr1),
						str1atrsk )

# Make the npat-by-npat atrskmat where each row corresponds to a patient.
  	atrskmat <- rbind(str0atrsk, str1atrsk)

# Make the npat-by-1 S0 vector "S0vec."
	S0vec <- t(atrskmat) %*% riskvec

# Make the npat-by-ncov S1 matrix "S1mat."
	S1aux <- diag(c(riskvec)) %*% covmat
	S1mat <- t(atrskmat) %*% S1aux

# Make the npat-by-ncov means matrix S1mat/S0vec
	meansmat <- diag(c(1/S0vec)) %*% S1mat

# Compute eq.(3) of Lin, i.e., the W matrix Wover.
# We break it into three terms as follows (supressing the k subscript).
# term1 = Delta_j {X_j - meanmat_j}
# term2 = riskvec_j X_j \sum_{l=1}^{n} \dfrac{\Delta_l}{S0vec_l}
# term3 = riskvec_j \sum_{l=1}^{n} \dfrac{\Delta_l}{S0vec_l}*meansmat_l
# Wover = term1 - term2 + term3

term1 <- diag(I(event == 1)) %*% (covmat - meansmat)

term2aux <- riskvec * (atrskmat %*% ((1/S0vec)*I(event == 1)))
term2 <- diag(c(term2aux)) %*% covmat

term3aux <- diag(c((1/S0vec) * I(event==1))) %*% meansmat
term3 <- diag(c(riskvec)) %*% (atrskmat %*% term3aux)
Wover <- term1 - term2 + term3

# Now we repeat the same steps to compute the W matrix for the simple A
# statistic.

# Fit the Cox model for the simple A statistic.
	timeA <- time[1:nstr0]
	eventA <- event[1:nstr0]
	covmatA <- covmat[1:nstr0,]
	fitA <- coxph(Surv(timeA, eventA) ~ covmatA, method = "breslow")
	loghra <- (fitA$coef[1] * rep(1,2))[1]
	sea <- (sqrt(fitA$var[1,1]) * rep(1,2))[1]
	hra <- exp(loghra)
	cia <- exp(loghra + 1.96 * c(-sea, sea))
	pvala <- 2 * (1 - pnorm(abs(loghra/sea)))
	fitAD <- coxph.detail(fitA)
	imatA <- apply(fitAD$imat, c(1,2), sum)
	invA <- solve(imatA)

# Make the nstr0-by-1 risk vector exp(covmatA %*% beta).
	riskvecA <- exp(covmatA %*% fitA$coef)

# Want to create the at-risk matrix, called "atrskmat,"
# for all simple A comparison patients.  The matrix
# will be a square nstr0-by-nstr0 matrix.  Rows will correspond to each
# patient; columns will correspond to the follow-up time for each
# patient.

	auxatrskA <-  matrix(rep(1, nstr0), ncol = 1) %*% timeA
	atrskmatA <- apply(auxatrskA, 2, ">=", timeA)
# atrskmat has each patient corresponding to a column.  We want each
# patient corresponding to a row.
	atrskmatA <- t(atrskmatA)

# Make the nstr0-by-1 S0 vector "S0vecA"
	S0vecA <- t(atrskmatA) %*% riskvecA

# Make the nstr0-by-ncov S1 matrix "S1matA."
	S1auxA <- diag(c(riskvecA)) %*% covmatA
	S1matA <- t(atrskmatA) %*% S1auxA

# Make the nstr0-by-ncov means matrix S1matA/S0vecA
	meansmatA <- diag(c(1/S0vecA)) %*% S1matA

# Compute eq.(3) of Lin, i.e. the W matrix Wsimple.
# We break it into three terms as follows
# (supressing the k subscript).
# term1A = Delta_j {X_j - meanmatA_j}
# term2A = riskvecA_j X_j \sum_{l=1}^{n} \dfrac{\Delta_l}{S0vecA_l}
# term3A = riskvecA_j \sum_{l=1}^{n} \dfrac{\Delta_l}{S0vecA_l}*meansmatA_l
# eq.(3) = term1A - term2A + term3A

term1A <- diag(I(eventA == 1)) %*% (covmatA - meansmatA)

term2auxA <- riskvecA * (atrskmatA %*% ((1/S0vecA)*I(eventA == 1)))
term2A <- diag(c(term2auxA)) %*% covmatA

term3auxA <- diag(c((1/S0vecA) * I(eventA==1))) %*% meansmatA
term3A <- diag(c(riskvecA)) %*% (atrskmatA %*% term3auxA)
WmatA <- term1A - term2A + term3A

# Compute the ncov-by-ncov R(beta, gamma) matrix in Lin.

RmatA <- matrix(rep(0, ncov^2), nrow = ncov)
for(i in 1:nstr0){
	aux <- matrix(Wover[i, ], ncol = 1) %*% matrix(WmatA[i, ], nrow = 1)
	RmatA <- RmatA + aux
}
# Obtain the covariance matrix between beta and gamma
	covBGmatA <- invover %*% RmatA %*% invA

# correlation between overall and simple statistics
	corAa <- covBGmatA[1,1]/(invover[1,1] * invA[1,1])^(1/2)

####
# Now we repeat the same steps to compute the W matrix for the simple AB
# statistic.

# Fit the Cox model for the simple AB statistic.
# Compare the B- & A- patients to the
# B+ & A+ patients.
# Make an indicator vector of such patients.
	indvec <- c(1:ntrt00, (nstr0 + ntrt10 + 1) : npat)
	nind <- length(indvec)
	timeAB <- time[indvec]
	eventAB <- event[indvec]
	covmatAB <- covmat[indvec,]
	fitAB <- coxph(Surv(timeAB, eventAB) ~ covmatAB, method = "breslow")
	loghrab <- (fitAB$coef[1] * rep(1,2))[1]
	seab <- (sqrt(fitAB$var[1,1]) * rep(1,2))[1]
	hrab <- exp(loghrab)
	ciab <- exp(loghrab + 1.96 * c(-seab, seab))
	pvalab <- 2 * (1 - pnorm(abs(loghrab/seab)))
	fitABD <- coxph.detail(fitAB)
	imatAB <- apply(fitABD$imat, c(1,2), sum)
	invAB <- solve(imatAB)

# Make the nind-by-1 risk vector exp(covmatAB %*% beta).
	riskvecAB <- exp(covmatAB %*% fitAB$coef)

# Want to create the at-risk matrix, called "atrskmat,"
# for all simple A comparison patients.  The matrix
# will be a square nind-by-nind matrix.  Rows will correspond to each
# patient; columns will correspond to the follow-up time for each
# patient.

	auxatrskAB <-  matrix(rep(1, nind), ncol = 1) %*% timeAB
	atrskmatAB <- apply(auxatrskAB, 2, ">=", timeAB)
# atrskmat has each patient corresponding to a column.  We want each
# patient corresponding to a row.
	atrskmatAB <- t(atrskmatAB)

# Make the nind-by-1 S0 vector "S0vecAB"
	S0vecAB <- t(atrskmatAB) %*% riskvecAB

# Make the nind-by-ncov S1 matrix "S1matAB."
	S1auxAB <- diag(c(riskvecAB)) %*% covmatAB
	S1matAB <- t(atrskmatAB) %*% S1auxAB

# Make the nind-by-ncov means matrix S1matAB/S0vecAB
	meansmatAB <- diag(c(1/S0vecAB)) %*% S1matAB

# Compute eq.(3) of Lin, i.e. the W matrix WmatAB.
# We break it into three terms as follows
# (supressing the k subscript).
# term1AB = Delta_j {X_j - meanmatAB_j}
# term2AB = riskvecAB_j X_j \sum_{l=1}^{n} \dfrac{\Delta_l}{S0vecAB_l}
# term3AB = riskvecAB_j \sum_{l=1}^{n} \dfrac{\Delta_l}{S0vecAB_l}*meansmatAB_l
# eq.(3) = term1AB - term2AB + term3AB

term1AB <- diag(I(eventAB == 1)) %*% (covmatAB - meansmatAB)

term2auxAB <- riskvecAB * (atrskmatAB %*% ((1/S0vecAB)*I(eventAB == 1)))
term2AB <- diag(c(term2auxAB)) %*% covmatAB

term3auxAB <- diag(c((1/S0vecAB) * I(eventAB==1))) %*% meansmatAB
term3AB <- diag(c(riskvecAB)) %*% (atrskmatAB %*% term3auxAB)
WmatAB <- term1AB - term2AB + term3AB

# Compute the ncov-by-ncov R(beta, gamma) matrix in Lin.
# Recall that the first ntrt00 patients in Wover and WmatAB
# correspond to B- & A- patients.  The last ntrt11
# patients in Wover and WmatAB correspond to B+ and
# A+ patients.

RmatAB <- matrix(rep(0, ncov^2), nrow = ncov)

for(i in 1:ntrt00){
	aux <- matrix(Wover[i, ], ncol = 1) %*% matrix(WmatAB[i, ],
						nrow = 1)
	RmatAB <- RmatAB + aux
}
for(i in 1:ntrt11){
	aux <- matrix(Wover[(nstr0 + ntrt10 + i), ], ncol = 1) %*%
				matrix(WmatAB[ntrt00 + i, ], nrow = 1)
	RmatAB <- RmatAB + aux
}

# Obtain the covariance matrix between beta and gamma
	covBGmatAB <- invover %*% RmatAB %*% invAB

# correlation between overall and simple statistics
	corAab <- covBGmatAB[1,1]/(invover[1,1] * invAB[1,1])^(1/2)

##### Compute the correlation between the simple A and simple AB
# statistics.  We exclude the B+ & A- patients
# Compute the ncov-by-ncov R(beta, gamma) matrix in Lin.
# Recall that the first ntrt00 patients in WmatA and WmatAB
# correspond to B- & A- patients which is the overlap
# between the A and AB cells.

	RmatAAB <- matrix(rep(0, ncov^2), nrow = ncov)

	for(i in 1:ntrt00){
	aux <- matrix(WmatA[i, ], ncol = 1) %*% matrix(WmatAB[i, ],
						nrow = 1)
	RmatAAB <- RmatAAB + aux
	}
# Obtain the covariance matrix between beta and gamma
	covBGmatAAB <- invA %*% RmatAAB %*% invAB

# correlation between the simple A and AB statistics
	coraab <- covBGmatAAB[1,1]/(invA[1,1] * invAB[1,1])^(1/2)

# New as of 2/20/2020
# Want to compute the interaction and its p-value
# Fit the Cox model for the overall (i.e., stratified) statistic.
# We need to remove the indA column from covmat, which was put there
# in the second line of this function.
	covmat <- covmat[, 2:ncol(covmat)]
	fit <- coxph(Surv(time, event) ~ indA + indB + indA * indB +
	             covmat, method = "breslow")
	fullvar <- fit$var
	aux <- length(fit$coef)
# A*B interaction quantities
	lhrABint <-   fit$coef[aux]
	hrABint <- exp(fit$coef[aux])
	seABint <- sqrt(fit$var[aux, aux])
	ciABint <- exp(lhrABint + 1.96 * c(-seABint, seABint))
	pvalABint <- 2 *  (1 - pnorm(abs(lhrABint/seABint)))

	list(loghrA = loghrA, seA = seA, hrA = hrA, ciA = ciA, pvalA = pvalA,
		   loghra = loghra, sea = sea, hra = hra, cia = cia, pvala = pvala,
		   loghrab = loghrab, seab = seab, hrab = hrab, ciab = ciab, pvalab = pvalab,
		   corAa = corAa, corAab = corAab, coraab = coraab, hrABint = hrABint,
		   seABint = seABint, ciABint = ciABint, pvalABint = pvalABint)
}

