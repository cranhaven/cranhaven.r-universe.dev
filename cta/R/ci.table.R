ci.table <- function(y, h.fct = 0, h.mean = FALSE, S.fct, S.mean = FALSE, S.P = FALSE,
                     S.space.H0 = NULL, method = "all", cc = 0.95,
                     pdlambda = 2/3, trans.g = NULL, trans.g.epsilon = 0,
                     trans.g.inv = NULL, strata = rep(1, length(y)),
                     fixed.strata = "all", delta = 0.5, max.iter = 50,
                     tol = 1e-2, tol.psi = 1e-4, adj.epsilon = 0.03,
                     iter.robust.max = 30, iter.robust.eff = 10,
                     check.homog.tol = 1e-9, check.zero.order.homog.tol = 1e-9,
                     max.mph.iter = 1000, step = 1,
                     change.step.after = 0.25 * max.mph.iter, y.eps = 0,
                     iter.orig = 5, norm.diff.conv = 1e-6,
                     norm.score.conv = 1e-6, max.score.diff.iter = 10,
                     h0.fct.deriv = NULL, S0.fct.deriv = NULL,
                     trans.g.deriv = NULL, plot.CIs = TRUE) {
  # ci.table (version 1.3, 08/14/2021)
  # This program computes test-inversion approximate confidence intervals for the parameter or estimand in
  # contingency table(s) subject to several equality constraint(s).
  #
  # Imports: The following R packages are required in order to run ci.table:
  #          {intervals}, {numDeriv}, {limSolve}.
  #
  # Input:
  # Required:
  # y = observed cell counts in the contingency table(s), in vector form.
  # h.fct = the imposed equality constraint(s). Note that the sampling constraints are not included in h.fct.
  #         Also note that the imposed equality constraints should be non-redundant.
  #         If h.mean = F (default), h(p) should be the input, where p is the vector of data model
  #         probabilities, or it can be called the vector of table probabilities;
  #         If h.mean = T, h(m) should be the input, where m is the vector of expected cell counts, i.e. m = E(Y).
  #         In the case of h(m) being the input, the function h(.) should be Z-homogeneous, where Z is the
  #         population matrix. For the definition of Z-homogeneity and population matrix, cf. Lang (2004):
  #         Lang, J.B. (2004).  "Multinomial-Poisson Homogeneous Models for Contingency Tables",
  #                              Annals of Statistics, 32, 340-383.
  #         Note that if there is no imposed equality constraint, we should input h.fct = 0 (real number 0).
  #         Please do not specify h.fct as a zero function in this case. On the contrary, if there is (are)
  #         imposed equality constraint(s), please specify h.fct as an R function. Another important note is that
  #         if there are multiple imposed equality constraints, please use rbind(), not c(), to concatenate the
  #         imposed equality constraints into a column vector.
  # S.fct = parameter or estimand of interest. It should be an R function, and the output is a real number.
  #         i.e. S(.) is a real-valued function.
  #         If S.mean = F and S.P = F (default), S(p) should be the input;
  #         If S.mean = T, S(m) should be the input;
  #         If S.P = T, S(P) should be the input, where P is the vector of joint probabilities, or it can be
  #         called the vector of predata probabilities.
  #         In the case of S(m) or S(P) being the input, the function S(.) should be zero-order Z-homogeneous,
  #         then S(P) is Z-estimable with S(P) = S(m). In addition, when we are in the process of computing
  #         test-inversion confidence intervals other than the Wald intervals, we have to fit several models
  #         and obtain constrained MLEs of table cell counts. These models have equality constraints h0*(m) = 0,
  #         where h0*(m) = [h0(m); S0(m) - psi; samp0(m)]. Restriction of S(m) [or S(P)] to zero-order
  #         Z-homogeneity guarantees the Z-homogeneity of h0*(m).
  # Optional:
  # h.mean = logical argument, T or F. If h.mean = F (default), the input h.fct is treated as a function of p;
  #          If h.mean = T, the input h.fct is treated as a function of m.
  # S.mean, S.P = logical argument, T or F. If S.mean = F and S.P = F (default), the input S.fct is treated as
  #               a function of p; If S.mean = T, the input S.fct is treated as a function of m; If S.P = T,
  #               the input S.fct is treated as a function of P.
  # S.space.H0 = restricted estimand space of the input S(.) under H0, i.e. subject to the imposed equality
  #              constraints along with the sampling constraints. If S.space.H0 is not specified or the input
  #              S.space.H0 = NULL, the restricted estimand space is treated as (-Inf, Inf), i.e. the whole real
  #              line. If S.space.H0 is specified, it can either be input as a vector of length of an even number,
  #              or be input in class Intervals_full {intervals}. As an example, if the restricted estimand space
  #              is (-Inf, -1] union [1, Inf), then the input S.space.H0 could be c(-Inf, -1, 1, Inf), or
  #              Intervals_full(matrix(c(-Inf, -1, 1, Inf), ncol = 2, byrow = T),
  #                             closed = matrix(c(F, T, T, F), ncol = 2, byrow = T), type = "R").
  #              It is strongly recommended that S.space.H0 be specified. It will improve the accuracy and
  #              (possibly) speed in the interval estimation. However, sometimes it is really difficult to have
  #              an idea of the restricted estimand space exactly. In this scenario, specification of one
  #              (or several) possibly larger interval(s) that cover(s) the exact restricted estimand space
  #              is also helpful.
  # method = the test statistic in constructing the test-inversion approximate interval. There are eight different
  #          test statistics, and the user is allowed to choose any number of the test statistics out of the eight.
  #          The eight test statistics are listed as follows: "Wald", "trans.Wald" (need specification of the
  #          transformation), "diff.Xsq", "nested.Xsq", "diff.Gsq" (same as "PL" or "LR"), "nested.Gsq", "diff.PD",
  #          "nested.PD" (need specification of the power-divergence parameter lambda). If the input method = "all"
  #          (default), all the eight test statistics will be used to compute intervals.
  # cc = confidence coefficient, or the nominal level of the confidence interval.
  # pdlambda = the lambda parameter in the power-divergence statistic.
  # trans.g = the transformation g used in trans.Wald interval. First, we construct a confidence interval for
  #           g(S(.)), then we back-transform, i.e. apply g-inverse to the endpoints in order to obtain the
  #           confidence interval for S(.). There are several built-in options for the transformation: "Fisher's z",
  #           "log", "-log" (same as "negative log"), "[A,B]" (same as "[A, B]"). "[A,B]" or "[A, B]" refers to the
  #           reparameterization trick as stated in the Discussion part of Lang (2008):
  #           Lang, J.B. (2008).  "Score and Profile Likelihood Confidence Intervals for Contingency Table Parameters",
  #                               Statistics in Medicine, 27, 5975-5990. DOI: 10.1002/sim.3391
  #           The user is also allowed to input their own choice of trans.g. Ordinarily, the transformation g
  #           should be a bijection. Ideally, g should be smooth, strictly monotonically increasing, and "to
  #           parameterize away the boundary" (cf. Lang (2008)).
  # trans.g.epsilon = the small epsilon adjustment included in the transformation g. For example, the "[A,B]"
  #                   transformation g with the small epsilon is
  #                   g(x) = log(x - A + trans.g.epsilon) - log(B + trans.g.epsilon - x).
  #                   By default, trans.g.epsilon = 0, i.e. no small epsilon adjustment.
  # trans.g.inv = g-inverse function used in back-transformation step in the construction of the trans.Wald interval.
  #               If trans.g is any one of the built-in options, then trans.g.inv is automatically specified
  #               accordingly.
  # strata = vector of the same length as y that gives the stratum membership identifier. By default, strata =
  #          rep(1, length(y)) refers to the single stratum (non-stratified) setting. As another example,
  #          strata = c(1,1,2,2) means that the first and second table cells belong to the first stratum, and
  #          the third and fourth table cells belong to the second stratum.
  # fixed.strata = the object that gives information on which stratum have fixed sample sizes. It can equal one
  #                of the keywords, fixed.strata = "all" or fixed.strata = "none", or it can be a vector of
  #                stratum membership identifiers, e.g. fixed.strata=c(1,3) or fixed.strata=c("pop1", "pop5").
  # max.iter = one of the stopping criteria. It is the maximum number of iterations in the sliding quadratic
  #            root-finding algorithm for searching the two roots to the test-inversion equation.
  # delta = the constant delta that is in the expressions of the moving critical values within each sliding
  #         quadratic step. By default, delta = 0.5.
  # tol = one of the stopping criteria. In solving for the roots of the test-inversion equation, if the
  #       test statistic for testing H0(psi) vs. H0\H0(psi), for a certain psi, is within tol of the critical
  #       value, then we stop the iterations, and then this current psi is treated as one root. Note that
  #       since we are constructing approximate (contrary to exact) intervals based on the asymptotic
  #       distribution under null, tol need not be too small.
  # tol.psi = one of the stopping criteria. In solving for the roots of the test-inversion equation, if the
  #           two psi's that are in nearby iterates in the corresponding tests H0(psi) vs. H0\H0(psi) are
  #           less than tol.psi apart in distance, then we stop the iterations, and the current psi is
  #           treated as one root. Notice that we should specify tol.psi to be sufficiently small (compared
  #           to the size of the restricted estimand space) so that the iterations are to be terminated
  #           mainly because of the closeness of test statistic to critical value.
  # adj.epsilon, iter.robust.max, iter.robust.eff = the parameters used in the robustifying procedure.
  #              We first attempt to construct confidence intervals based on the original data, but an
  #              error might occur during this process. The reason for the occurrence of the error might
  #              be the non-existence of constrained MLEs under H0, or it might be because of the fact
  #              that the psi in the hypothesis test H0(psi) vs. H0\H0(psi) is, on some scale, too far
  #              away from psi^hat which is the constrained MLE of the estimand under H0, although it is
  #              still within the restricted estimand space. If an error, or non-convergence issue occurs,
  #              then the program will go through the robustifying procedure, with the goal of reporting
  #              a confidence interval anyway, even in the most extreme configuration and/or with the most
  #              "extreme" data.
  #              In the robustifying procedure, we adjust the original data by adding 1 * adj.epsilon to
  #              each original table cell count, and compute the confidence interval based on the adjusted
  #              data. Note that, however, even the adjusted data can lead to non-convergence issue sometimes.
  #              We also adjust the original data by adding 2 * adj.epsilon, ..., iter.robust.max *
  #              adj.epsilon, and compute the confidence intervals for these adjusted data, respectively.
  #              For computing purposes, as soon as iter.robust.eff confidence intervals based on the
  #              adjusted data have been successfully computed, we will not proceed further into adjustment
  #              and interval estimation based on the adjusted data. Now, by exploiting the property that
  #              lim_{adj.epsilon -> 0+} CI(y + adj.epsilon; H0) = CI(y; H0), we extrapolate using a
  #              polynomial fit of degree at most three based on the lower and upper endpoints of the
  #              intervals on the adjusted data. It is advised that adj.epsilon should not exceed 0.1,
  #              and it should not be too small. By default, adj.epsilon = 0.03.
  # check.homog.tol = round-off tolerance for Z-homogeneity check. If the function h(.) with respect to m
  #                   is not Z-homogeneous, the algorithm will stop immediately and report an error.
  # check.zero.order.homog.tol = round-off tolerance for zero-order Z-homogeneity check. If the function
  #                              S(.) with respect to m or P is not zero-order Z-homogeneous, the algorithm
  #                              will stop immediately and report an error.
  # max.mph.iter = maximum number of iterations as used in mph.fit.
  # step = step halving parameter as used in mph.fit.
  # change.step.after = in mph.fit, if the score value increases for more than change.step.after iterations
  #                     in a row, then the initial step size is halved.
  # y.eps = amount to initially add to each count in y, as used in mph.fit.
  # iter.orig = iteration at which the original table cell counts will be used, as used in mph.fit.
  # norm.diff.conv = one of the convergence criteria as used in mph.fit. In the fitting algorithm mph.fit,
  #                  if the distance between the last and second last 'theta' iterates ('theta' is the vector
  #                  of log fitted values and Lagrange multipliers) exceeds norm.diff.conv, the algorithm
  #                  will continue.
  # norm.score.conv = one of the convergence criteria as used in mph.fit. In the fitting algorithm mph.fit,
  #                   if the distance between the score vector and zero exceeds norm.score.conv, the
  #                   algorithm will continue.
  # max.score.diff.iter = in the fitting algorithm mph.fit, the variable score.diff.iter keeps track of how
  #                       long norm.score is smaller than norm.score.conv, but norm.diff is greater than
  #                       norm.diff.conv. If this is the case too long (i.e. score.diff.iter >=
  #                       max.score.diff.iter), then stop the iterations because the solution likely
  #                       includes at least one ML fitted value of 0.
  # h0.fct.deriv = the R function object that computes analytic derivative of the transpose of the constraint
  #                function h0(.) with respect to m. In this algorithm, if the input function h.fct is with
  #                respect to p, then the algorithm automatically rewrites it into a function with respect to
  #                m, that is, h(p) = h(Diag^{-1}(ZZ'm)m) = h0(m). If the input function h.fct is with respect
  #                to m, then we let h0(m) = h(m). h0.fct.deriv, if it is specified, equals
  #                partial h0(m)' / partial m. Note that if h0(.) maps from Rp to Rq (i.e. there are q
  #                constraints), then h0.fct.deriv returns a p-by-q matrix of partial derivatives. If
  #                h0.fct.deriv is not specified or h0.fct.deriv = NULL, numerical derivatives will be used.
  # S0.fct.deriv = the R function object that computes analytic derivative of the transpose of the estimand
  #                function S0(.) with respect to m. In this algorithm, if the input function S.fct is with
  #                respect to p, then the algorithm automatically rewrites it into a function with respect
  #                to m, that is, S(p) = S(Diag^{-1}(ZZ'm)m) = S0(m). If the input function S.fct is with
  #                respect to m, then we let S0(m) = S(m). If the input function S.fct is with respect to P,
  #                since S(.) is required to be zero-order Z-homogeneous, S(P) = S(m), and thus we let
  #                S0(m) = S(P). S0.fct.deriv, if it is specified, equals partial S0(m)' / partial m.
  #                It is a column vector, whose length is the same as the length of m. If it is not specified
  #                or S0.fct.deriv = NULL, numerical derivatives will be used.
  # trans.g.deriv = the derivative function of the transformation g, i.e. d g(w) / d w. If it is specified, it
  #                 should be an R function, even if the derivative function is a constant function.
  # plot.CIs = logical argument, T or F. If plot.CIs = T, a visual display of the computed confidence intervals
  #            will be plotted. If plot.CIs = F, no plots will be created.
  #
  # Output:
  # result.table = a table that displays the lower and upper endpoints of the computed confidence interval(s).
  #                The length(s) of the interval(s) is (are) reported in the last column.
  # CIs = an object of class Intervals_full {intervals} that includes all the computed confidence interval(s).
  # Shat = the constrained MLE of S(.) under H0. If there is an error or non-convergence issue during the
  #        process of fitting the model under H0 by the fitting algorithm mph.fit, Shat is set to be NA.
  #        Or if the constrained MLE does not exist, Shat is also set to be NA.
  # ase.Shat = the asymptotic standard error (ase) of the constrained MLE of S(.) under H0. If there is an
  #            error or non-convergence issue during the process of fitting the model under H0 by the
  #            fitting algorithm mph.fit, ase.Shat is set to be NA. Or if the constrained MLE does not exist,
  #            ase.Shat is also set to be NA.
  # S.space.H0 = restricted estimand space of S(.) under H0. It might be different from the input S.space.H0.
  #              If the input S.space.H0 is the union of at least two disjoint intervals, then the output
  #              S.space.H0 displays the particular interval in which the constrained MLE of S(.) under H0,
  #              i.e. Shat, lies. If the input S.space.H0 is an interval, then the output S.space.H0 is the
  #              same as the input. If S.space.H0 is unspecified or S.space.H0 = NULL in the input, then
  #              the output S.space.H0 = NULL.
  # cc = confidence coefficient, or the nominal level of the confidence interval. It is the same as the cc in
  #      the input.
  # method = the test statistic(s) that is (are) actually used to construct the test-inversion approximate
  #          confidence interval(s).
  # pdlambda = the lambda parameter in the power-divergence statistic. It is the same as the pdlambda in the
  #            input.
  # warnings.collection = includes all the warning messages that occur during the construction of confidence
  #                       interval(s). They might be on evoking of the robustifying procedure:
  #                       "xxx.CI: Adjustment used. Not on original data.", or they might be on the
  #                       unsuccessful construction of the confidence interval(s): "xxx.CI: NA."
  #
  # Examples:
  # I. A 3-by-4-by-2 three-way contingency table, each 3-by-4 two-way sub-table forms a (fixed) stratum,
  #    and the two strata are independent. We assume common correlation of the two sub-tables. The scores
  #    assigned to the two variables are (1,2,3) and (1,2,3,4), respectively. The observed table cell
  #    counts are (1,2,3,4,5,6,7,8,9,10,11,12) for the first sub-table, and (13,14,15,16,17,18,19,20,
  #    21,22,23,24) for the second sub-table. We wish to construct a confidence interval for this
  #    common correlation, or equivelently, for the correlation of the first sub-table. The code is
  #    shown as follows.
  # corr_freq_prob <- function(freq, score.X, score.Y) {
  #   # Compute the correlation based on the frequency or probability vector.
  #   # Note that the input freq is a vector.
  #   c <- length(score.X)
  #   d <- length(score.Y)
  #   freq <- matrix(freq, nrow = c, ncol = d, byrow = T)
  #   P <- freq / sum(freq)
  #   P.row.sum <- apply(P, 1, sum)
  #   P.column.sum <- apply(P, 2, sum)
  #   EX <- t(score.X) %*% P.row.sum
  #   EY <- t(score.Y) %*% P.column.sum
  #   EXsq <- t(score.X^2) %*% P.row.sum
  #   EYsq <- t(score.Y^2) %*% P.column.sum
  #   sdX <- sqrt(EXsq - EX^2)
  #   sdY <- sqrt(EYsq - EY^2)
  #   EXY <- 0
  #   for (i in seq(1, c)) {
  #     for (j in seq(1, d)) {
  #       EXY <- EXY + score.X[i] * score.Y[j] * P[i, j]
  #     }
  #   }
  #   Cov.X.Y <- EXY - EX * EY
  #   if (Cov.X.Y == 0) {
  #     corr <- 0
  #   }
  #   else {
  #     corr <- as.numeric(Cov.X.Y / (sdX * sdY))
  #   }
  #   corr
  # }
  #
  # h.fct <- function(p) {
  #   corr_1 <- corr_freq_prob(p[seq(1,12)], c(1,2,3), c(1,2,3,4))
  #   corr_2 <- corr_freq_prob(p[seq(13,24)], c(1,2,3), c(1,2,3,4))
  #   corr_1 - corr_2
  # }
  # S.fct <- function(p) {
  #   corr_freq_prob(p[seq(1,12)], c(1,2,3), c(1,2,3,4))
  # }
  # eg_corr_result <- ci.table(y = seq(1,24), h.fct = h.fct, S.fct = S.fct, S.space.H0 = c(-1,1),
  #                            trans.g = "Fisher's z", strata = rep(c(1,2), each = 12))
  #
  # II. Mice-Fungicide data: A 2-by-2-by-4 three-way contingency table. For each of the four 2-by-2
  #     two-way sub-tables, there is one fixed stratum for the treated group, and there is one fixed
  #     stratum for the control group. We assume common relative risks among the four sub-tables,
  #     and we wish to construct a confidence interval for this common relative risk, i.e. for the
  #     relative risk of the first two-way sub-table.
  #     For a detailed description of the data set, please refer to Gart (1971):
  #     Gart, John J. "The comparison of proportions: a review of significance tests, confidence
  #     intervals and adjustments for stratification." Revue de l'Institut International de
  #     Statistique (1971): 148-169.
  #     The code is as follows.
  # h.fct <- function(p) {
  #   RR_1 <- p[1] / p[3]
  #   RR_2 <- p[5] / p[7]
  #   RR_3 <- p[9] / p[11]
  #   RR_4 <- p[13] / p[15]
  #   rbind(RR_1 - RR_2, RR_1 - RR_3, RR_1 - RR_4)
  # }
  # S.fct <- function(p) {
  #   p[1] / p[3]
  # }
  # obs.y <- c(4,12,5,74,2,14,3,84,4,14,10,80,1,14,3,79)
  # mice_result <- ci.table(obs.y, h.fct = h.fct, S.fct = S.fct, S.space.H0 = c(0, Inf),
  #                         trans.g = "log", strata = c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8))
  #
  # III. Crying baby data: Gordon and Foss (1966) conducted an experiment to investigate the
  #      effect of rocking on the crying of full term babies. For a detailed description of
  #      the data set, please refer to Cox (1966):
  #      Cox, D. R. "A simple example of a comparison involving quanta."
  #                  Biometrika 53.1-2 (1966): 213-220.
  #      The data set can be reproduced as a 2-by-2-by-18 three-way contingency table. For
  #      each of the eighteen 2-by-2 two-way sub-tables, there is one fixed stratum for the
  #      experimental group and one fixed stratum for the control group. We assume common
  #      odds ratios among the eighteen two-way sub-tables, and we wish to construct a
  #      confidence interval for this common odds ratio, i.e. for the odds ratio for the
  #      first two-way sub-table. The code is as follows.
  # obs.y <- c(0,1,5,3,0,1,4,2,0,1,4,1,1,0,5,1,0,1,1,4,0,1,5,4,0,1,3,5,0,1,4,4,0,1,2,3,1,0,1,8,
  #            0,1,1,5,0,1,1,8,0,1,3,5,0,1,1,4,0,1,2,4,0,1,1,7,1,0,2,4,0,1,3,5)
  # strata <- rep(seq(1, 36), each = 2)
  # h.fct <- function(p) {
  #   OR_1 <- p[1]*p[4] / (p[2]*p[3])
  #   OR_2 <- p[5]*p[8] / (p[6]*p[7])
  #   OR_3 <- p[9]*p[12] / (p[10]*p[11])
  #   OR_4 <- p[13]*p[16] / (p[14]*p[15])
  #   OR_5 <- p[17]*p[20] / (p[18]*p[19])
  #   OR_6 <- p[21]*p[24] / (p[22]*p[23])
  #   OR_7 <- p[25]*p[28] / (p[26]*p[27])
  #   OR_8 <- p[29]*p[32] / (p[30]*p[31])
  #   OR_9 <- p[33]*p[36] / (p[34]*p[35])
  #   OR_10 <- p[37]*p[40] / (p[38]*p[39])
  #   OR_11 <- p[41]*p[44] / (p[42]*p[43])
  #   OR_12 <- p[45]*p[48] / (p[46]*p[47])
  #   OR_13 <- p[49]*p[52] / (p[50]*p[51])
  #   OR_14 <- p[53]*p[56] / (p[54]*p[55])
  #   OR_15 <- p[57]*p[60] / (p[58]*p[59])
  #   OR_16 <- p[61]*p[64] / (p[62]*p[63])
  #   OR_17 <- p[65]*p[68] / (p[66]*p[67])
  #   OR_18 <- p[69]*p[72] / (p[70]*p[71])
  #   rbind(OR_1 - OR_2, OR_1 - OR_3, OR_1 - OR_4, OR_1 - OR_5, OR_1 - OR_6,
  #         OR_1 - OR_7, OR_1 - OR_8, OR_1 - OR_9, OR_1 - OR_10, OR_1 - OR_11,
  #         OR_1 - OR_12, OR_1 - OR_13, OR_1 - OR_14, OR_1 - OR_15, OR_1 - OR_16,
  #         OR_1 - OR_17, OR_1 - OR_18)
  # }
  # S.fct <- function(p) {
  #   p[1]*p[4] / (p[2]*p[3])
  # }
  # crying_baby_result <- ci.table(obs.y, h.fct = h.fct, S.fct = S.fct, S.space.H0 = c(0, Inf),
  #                                trans.g = "log", strata = strata, fixed.strata = "all", y.eps = 1)
  #
  # IV. Binomial success rate parameter p, without additionally imposed equality constraints.
  #     Observe 0=x <- X|p ~ Bin(n=5, p)
  #     The code is as follows.
  # bin_p_result <- ci.table(c(0,5), h.fct = 0, S.fct = function(p) {p[1]}, S.space.H0 = c(0,1))
  #

  Z_ZF <- create.Z.ZF(strata = strata, fixed.strata = fixed.strata)
  Z <- Z_ZF[[1]]

  if (h.mean) {
    # h.mean = T, need to check Z-homogeneity of the function h.fct.
    if (!is.function(h.fct)) {
      h0.fct <- h.fct <- 0
    }
    else {
      # cat("CHECKING whether h(m) is Z-homogeneous...\n")
      check.homog.result <- check.homog(h.fct, Z, check.homog.tol)
      if (check.homog.result != "") {
        stop(paste("h(m) is not Z-homogeneous [ based on tol =", check.homog.tol, "]! You may input h(p)."))
      }
      else {
        # cat(sep = "", "Necessary condition [tol = ", check.homog.tol, "] passed.\n")
        h0.fct <- h.fct
      }
    }
  }
  else {
    # h.mean = F, i.e. h(p) is the input.
    if (!is.function(h.fct)) {
      h0.fct <- h.fct <- 0
    }
    else {
      h0.fct <- function(m) {
        p <- m*c(1/Z%*%t(Z)%*%m)
        h.fct(p)
      }
    }
  }
  if (S.mean) {
    # S.mean = T, need to check zero-order Z-homogeneity of the function S.fct.
    # cat("CHECKING whether S(m) is zero-order Z-homogeneous...\n")
    check.zero.order.homog.result <- check.zero.order.homog(S.fct, Z, check.zero.order.homog.tol)
    if (check.zero.order.homog.result != "") {
      stop(paste("S(m) is not zero-order Z-homogeneous [ based on tol =",
                 check.zero.order.homog.tol, "]! You may input S(p)."))
    }
    else {
      # cat(sep = "", "Necessary condition [tol = ", check.zero.order.homog.tol, "] passed.\n")
      S0.fct <- S.fct
    }
  }
  else if (S.P) {
    # S.P = T, need to check zero-order Z-homogeneity of the function S.fct.
    # cat("CHECKING whether S(P) is zero-order Z-homogeneous...\n")
    check.zero.order.homog.result <- check.zero.order.homog(S.fct, Z, check.zero.order.homog.tol)
    if (check.zero.order.homog.result != "") {
      stop(paste("S(P) is not zero-order Z-homogeneous [ based on tol =",
                 check.zero.order.homog.tol, "]! You may input S(p)."))
    }
    else {
      # cat(sep = "", "Necessary condition [tol = ", check.zero.order.homog.tol, "] passed.\n")
      # S(.) is zero-order Z-homogeneous, then S(P) = S(m).
      S0.fct <- S.fct
    }
  }
  else {
    # S.mean = F and S.P = F, then S(p) is the input.
    S0.fct <- function(m) {
      p <- m*c(1/Z%*%t(Z)%*%m)
      S.fct(p)
    }
  }

  if (tol.psi > 1) {
    tol.psi <- 1
  }
  if (!is.null(S.space.H0)) {
    if (!(class(S.space.H0) %in% c("Intervals_full", "Intervals"))) {
      S.space.H0 <- Intervals_full(matrix(S.space.H0, ncol = 2, byrow = TRUE),
                                   closed = c(TRUE, TRUE), type = "R")
    }
  }
  method[method %in% c("PL", "LR")] <- "diff.Gsq"
  if (any(method == "all")) {
    method <- method[!method %in% c("all")]
    if (!is.null(trans.g)) {
      method_plus <- c("Wald", "trans.Wald", "diff.Xsq", "nested.Xsq",
                       "diff.Gsq", "nested.Gsq", "diff.PD", "nested.PD")
      method <- unique(c(method, method_plus))
    }
    else {
      method_plus <- c("Wald", "diff.Xsq", "nested.Xsq", "diff.Gsq",
                       "nested.Gsq", "diff.PD", "nested.PD")
      method <- unique(c(method, method_plus))
    }
  }
  # specify built-in trans.g and trans.g.inv
  if (is.null(trans.g)) {
    method <- method[!method %in% c("trans.Wald")]
  }
  else {
    if (is.character(trans.g)) {
      if (trans.g == "Fisher's z") {
        trans.g <- function(t, small.epsilon = trans.g.epsilon) {
          log(t + 1 + small.epsilon) - log(1 + small.epsilon - t)
        }
        trans.g.inv <- function(t, small.epsilon = trans.g.epsilon) {
          (1 + small.epsilon) * (exp(t) - 1) / (1 + exp(t))
        }
      }
      else if (trans.g == "log") {
        trans.g <- function(t, small.epsilon = trans.g.epsilon) {
          log(t + small.epsilon)
        }
        trans.g.inv <- function(t, small.epsilon = trans.g.epsilon) {
          exp(t) - small.epsilon
        }
      }
      else if (trans.g %in% c("-log", "negative log")) {
        trans.g <- function(t, small.epsilon = trans.g.epsilon) {
          -log(small.epsilon - t)
        }
        trans.g.inv <- function(t, small.epsilon = trans.g.epsilon) {
          small.epsilon - exp(-t)
        }
      }
      else if (trans.g %in% c("[A,B]", "[A, B]")) {
        if (!is.null(S.space.H0)) {
          if (length(S.space.H0) == 2) {
            A <- S.space.H0[[1]]
            B <- S.space.H0[[2]]
            if (is.infinite(A)) {
              if (is.infinite(B)) {
                # A = -Inf, B = Inf
                trans.g <- function(t, small.epsilon = trans.g.epsilon) {t}
                trans.g.inv <- function(t, small.epsilon = trans.g.epsilon) {t}
              }
              else {
                # A = -Inf
                trans.g <- function(t, small.epsilon = trans.g.epsilon) {
                  -log(B + small.epsilon - t)
                }
                trans.g.inv <- function(t, small.epsilon = trans.g.epsilon) {
                  B + small.epsilon - exp(-t)
                }
              }
            }
            else {
              if (is.infinite(B)) {
                # B = Inf
                trans.g <- function(t, small.epsilon = trans.g.epsilon) {
                  log(t - A + small.epsilon)
                }
                trans.g.inv <- function(t, small.epsilon = trans.g.epsilon) {
                  exp(t) + A - small.epsilon
                }
              }
              else {
                trans.g <- function(t, small.epsilon = trans.g.epsilon) {
                  log(t - A + small.epsilon) - log(B + small.epsilon - t)
                }
                trans.g.inv <- function(t, small.epsilon = trans.g.epsilon) {
                  (exp(t) * (B + small.epsilon) + (A - small.epsilon)) / (1 + exp(t))
                }
              }
            }
          }
          else {
            message("Please specify the transformation g, trans.g, along with its inverse, trans.g.inv.\n")
          }
        }
        else {
          message("Please specify the transformation g, trans.g, along with its inverse, trans.g.inv.\n")
        }
      }
    }
  }
  # end specification of built-in trans.g and trans.g.inv
  MLE.ase.S0hat <- tryCatch(compute_cons_MLE_ase(y, strata, fixed.strata, h0.fct, h0.fct.deriv, S0.fct, S0.fct.deriv,
                                                 max.mph.iter, step, change.step.after, y.eps, iter.orig, norm.diff.conv,
                                                 norm.score.conv, max.score.diff.iter),
                            error = function(e) {c(NA, NA)})
  S0.fct.m_H0 <- MLE.ase.S0hat[1]
  ase.S0.fct.m_H0 <- MLE.ase.S0hat[2]
  if (!is.na(S0.fct.m_H0)) {
    if (!is.null(S.space.H0)) {
      if (length(S.space.H0) == 2) {
        lower.boundary <- S.space.H0[[1]]
        upper.boundary <- S.space.H0[[2]]
      }
      else {
        # S.space.H0 is composed of at least two disjoint intervals.
        for (s_f_index in seq(1, length(S.space.H0) / 2)) {
          s_f_lower_temp <- S.space.H0[s_f_index][[1]]
          s_f_upper_temp <- S.space.H0[s_f_index][[2]]
          if ((S0.fct.m_H0 >= s_f_lower_temp) & (S0.fct.m_H0 <= s_f_upper_temp)) {
            lower.boundary <- s_f_lower_temp
            upper.boundary <- s_f_upper_temp
            break
          }
        }
      }
      S.space.H0 <- Intervals_full(matrix(c(lower.boundary, upper.boundary), ncol = 2, byrow = TRUE),
                                   closed = c(TRUE, TRUE), type = "R")
    }
  }
  cut.off <- qchisq(cc, 1)
  warnings_col <- NULL
  Wald.CI <- trans.Wald.CI <- diff.Gsq.CI <- diff.Xsq.CI <- diff.PD.CI <- nested.Gsq.CI <- nested.Xsq.CI <- nested.PD.CI <- NULL
  if (any(method %in% c("Wald", "trans.Wald"))) {
    Wald_and_or_trans.Wald_w <- Wald_trans.Wald_robust(y = y, strata = strata, fixed.strata = fixed.strata, h0.fct = h0.fct,
                                                       h0.fct.deriv = h0.fct.deriv, S0.fct = S0.fct, S0.fct.deriv = S0.fct.deriv,
                                                       max.mph.iter = max.mph.iter, step = step, change.step.after = change.step.after,
                                                       y.eps = y.eps, iter.orig = iter.orig, norm.diff.conv = norm.diff.conv,
                                                       norm.score.conv = norm.score.conv, max.score.diff.iter = max.score.diff.iter,
                                                       cut.off = cut.off, S.space.H0 = S.space.H0, trans.g = trans.g,
                                                       trans.g.deriv = trans.g.deriv, trans.g.inv = trans.g.inv, adj.epsilon = adj.epsilon,
                                                       iter.robust.max = iter.robust.max, iter.robust.eff = iter.robust.eff)
    Wald_and_or_trans.Wald <- Wald_and_or_trans.Wald_w[[1]]
    warnings_col <- c(warnings_col, Wald_and_or_trans.Wald_w[[2]])
    Wald.CI <- Wald_and_or_trans.Wald[1, ]
    if (!any(is.na(Wald.CI))) {
      Wald.CI_interval <- Intervals_full(matrix(Wald.CI, ncol = 2, byrow = TRUE),
                                         closed = c(TRUE, TRUE), type = "R")
      if (!is.null(S.space.H0)) {
        if (!is.na(S0.fct.m_H0)) {
          if (abs(S0.fct.m_H0 - upper.boundary) < tol.psi) {
            Wald.CI_interval[[2]] <- upper.boundary
          }
          if (abs(S0.fct.m_H0 - lower.boundary) < tol.psi) {
            Wald.CI_interval[[1]] <- lower.boundary
          }
        }
        Wald.CI <- interval_intersection(Wald.CI_interval, as(S.space.H0, "Intervals_full"))
      }
      else {
        Wald.CI <- Wald.CI_interval
      }
      rownames(Wald.CI) <- rep("Wald.CI", length(Wald.CI) / 2)
    }
    else {
      warnings_col <- c(warnings_col, "Wald.CI: NA.\n")
      Wald.CI <- NULL
    }
    if (any(method %in% c("trans.Wald"))) {
      trans.Wald.CI <- Wald_and_or_trans.Wald[2, ]
      if (!any(is.na(trans.Wald.CI))) {
        trans.Wald.CI_interval <- Intervals_full(matrix(trans.Wald.CI, ncol = 2, byrow = TRUE),
                                                 closed = c(TRUE, TRUE), type = "R")
        if (!is.null(S.space.H0)) {
          if (!is.na(S0.fct.m_H0)) {
            if (abs(S0.fct.m_H0 - upper.boundary) < tol.psi) {
              trans.Wald.CI_interval[[2]] <- upper.boundary
            }
            if (abs(S0.fct.m_H0 - lower.boundary) < tol.psi) {
              trans.Wald.CI_interval[[1]] <- lower.boundary
            }
          }
          trans.Wald.CI <- interval_intersection(trans.Wald.CI_interval, as(S.space.H0, "Intervals_full"))
        }
        else {
          trans.Wald.CI <- trans.Wald.CI_interval
        }
        rownames(trans.Wald.CI) <- rep("trans.Wald.CI", length(trans.Wald.CI) / 2)
      }
      else {
        warnings_col <- c(warnings_col, "trans.Wald.CI: NA.\n")
        trans.Wald.CI <- NULL
      }
    }
  }
  if (any(method == "diff.Gsq")) {
    diff.Gsq.CI_w <- diff_Gsq_robust(y = y, strata = strata, fixed.strata = fixed.strata, h0.fct = h0.fct,
                                     h0.fct.deriv = h0.fct.deriv, S0.fct = S0.fct, S0.fct.deriv = S0.fct.deriv,
                                     max.mph.iter = max.mph.iter, step = step, change.step.after = change.step.after,
                                     y.eps = y.eps, iter.orig = iter.orig, norm.diff.conv = norm.diff.conv,
                                     norm.score.conv = norm.score.conv, max.score.diff.iter = max.score.diff.iter,
                                     S.space.H0 = S.space.H0, tol.psi = tol.psi, tol = tol, max.iter = max.iter,
                                     cut.off = cut.off, delta = delta, adj.epsilon = adj.epsilon,
                                     iter.robust.max = iter.robust.max, iter.robust.eff = iter.robust.eff)
    diff.Gsq.CI <- diff.Gsq.CI_w[[1]]
    warnings_col <- c(warnings_col, diff.Gsq.CI_w[[2]])
    if (!any(is.na(diff.Gsq.CI))) {
      diff.Gsq.CI_interval <- Intervals_full(matrix(diff.Gsq.CI, ncol = 2, byrow = TRUE),
                                             closed = c(TRUE, TRUE), type = "R")
      if (!is.null(S.space.H0)) {
        if (!is.na(S0.fct.m_H0)) {
          if (abs(S0.fct.m_H0 - upper.boundary) < tol.psi) {
            diff.Gsq.CI_interval[[2]] <- upper.boundary
          }
          if (abs(S0.fct.m_H0 - lower.boundary) < tol.psi) {
            diff.Gsq.CI_interval[[1]] <- lower.boundary
          }
        }
        diff.Gsq.CI <- interval_intersection(diff.Gsq.CI_interval, as(S.space.H0, "Intervals_full"))
      }
      else {
        diff.Gsq.CI <- diff.Gsq.CI_interval
      }
      rownames(diff.Gsq.CI) <- rep("diff.Gsq.CI", length(diff.Gsq.CI) / 2)
    }
    else {
      warnings_col <- c(warnings_col, "diff.Gsq.CI: NA.\n")
      diff.Gsq.CI <- NULL
    }
  }
  if (any(method == "diff.Xsq")) {
    diff.Xsq.CI_w <- diff_Xsq_robust(y = y, strata = strata, fixed.strata = fixed.strata, h0.fct = h0.fct,
                                     h0.fct.deriv = h0.fct.deriv, S0.fct = S0.fct, S0.fct.deriv = S0.fct.deriv,
                                     max.mph.iter = max.mph.iter, step = step, change.step.after = change.step.after,
                                     y.eps = y.eps, iter.orig = iter.orig, norm.diff.conv = norm.diff.conv,
                                     norm.score.conv = norm.score.conv, max.score.diff.iter = max.score.diff.iter,
                                     S.space.H0 = S.space.H0, tol.psi = tol.psi, tol = tol, max.iter = max.iter,
                                     cut.off = cut.off, delta = delta, adj.epsilon = adj.epsilon,
                                     iter.robust.max = iter.robust.max, iter.robust.eff = iter.robust.eff)
    diff.Xsq.CI <- diff.Xsq.CI_w[[1]]
    warnings_col <- c(warnings_col, diff.Xsq.CI_w[[2]])
    if (!any(is.na(diff.Xsq.CI))) {
      diff.Xsq.CI_interval <- Intervals_full(matrix(diff.Xsq.CI, ncol = 2, byrow = TRUE),
                                             closed = c(TRUE, TRUE), type = "R")
      if (!is.null(S.space.H0)) {
        if (!is.na(S0.fct.m_H0)) {
          if (abs(S0.fct.m_H0 - upper.boundary) < tol.psi) {
            diff.Xsq.CI_interval[[2]] <- upper.boundary
          }
          if (abs(S0.fct.m_H0 - lower.boundary) < tol.psi) {
            diff.Xsq.CI_interval[[1]] <- lower.boundary
          }
        }
        diff.Xsq.CI <- interval_intersection(diff.Xsq.CI_interval, as(S.space.H0, "Intervals_full"))
      }
      else {
        diff.Xsq.CI <- diff.Xsq.CI_interval
      }
      rownames(diff.Xsq.CI) <- rep("diff.Xsq.CI", length(diff.Xsq.CI) / 2)
    }
    else {
      warnings_col <- c(warnings_col, "diff.Xsq.CI: NA.\n")
      diff.Xsq.CI <- NULL
    }
  }
  if (any(method == "diff.PD")) {
    diff.PD.CI_w <- diff_PD_robust(y = y, strata = strata, fixed.strata = fixed.strata, h0.fct = h0.fct,
                                   h0.fct.deriv = h0.fct.deriv, S0.fct = S0.fct, S0.fct.deriv = S0.fct.deriv,
                                   max.mph.iter = max.mph.iter, step = step, change.step.after = change.step.after,
                                   y.eps = y.eps, iter.orig = iter.orig, norm.diff.conv = norm.diff.conv,
                                   norm.score.conv = norm.score.conv, max.score.diff.iter = max.score.diff.iter,
                                   S.space.H0 = S.space.H0, tol.psi = tol.psi, tol = tol, max.iter = max.iter,
                                   cut.off = cut.off, delta = delta, pdlambda = pdlambda, adj.epsilon = adj.epsilon,
                                   iter.robust.max = iter.robust.max, iter.robust.eff = iter.robust.eff)
    diff.PD.CI <- diff.PD.CI_w[[1]]
    warnings_col <- c(warnings_col, diff.PD.CI_w[[2]])
    if (!any(is.na(diff.PD.CI))) {
      diff.PD.CI_interval <- Intervals_full(matrix(diff.PD.CI, ncol = 2, byrow = TRUE),
                                            closed = c(TRUE, TRUE), type = "R")
      if (!is.null(S.space.H0)) {
        if (!is.na(S0.fct.m_H0)) {
          if (abs(S0.fct.m_H0 - upper.boundary) < tol.psi) {
            diff.PD.CI_interval[[2]] <- upper.boundary
          }
          if (abs(S0.fct.m_H0 - lower.boundary) < tol.psi) {
            diff.PD.CI_interval[[1]] <- lower.boundary
          }
        }
        diff.PD.CI <- interval_intersection(diff.PD.CI_interval, as(S.space.H0, "Intervals_full"))
      }
      else {
        diff.PD.CI <- diff.PD.CI_interval
      }
      rownames(diff.PD.CI) <- rep("diff.PD.CI", length(diff.PD.CI) / 2)
    }
    else {
      warnings_col <- c(warnings_col, "diff.PD.CI: NA.\n")
      diff.PD.CI <- NULL
    }
  }
  if (any(method == "nested.Gsq")) {
    nested.Gsq.CI_w <- nested_Gsq_robust(y = y, strata = strata, fixed.strata = fixed.strata, h0.fct = h0.fct,
                                         h0.fct.deriv = h0.fct.deriv, S0.fct = S0.fct, S0.fct.deriv = S0.fct.deriv,
                                         max.mph.iter = max.mph.iter, step = step, change.step.after = change.step.after,
                                         y.eps = y.eps, iter.orig = iter.orig, norm.diff.conv = norm.diff.conv,
                                         norm.score.conv = norm.score.conv, max.score.diff.iter = max.score.diff.iter,
                                         S.space.H0 = S.space.H0, tol.psi = tol.psi, tol = tol, max.iter = max.iter,
                                         cut.off = cut.off, delta = delta, adj.epsilon = adj.epsilon,
                                         iter.robust.max = iter.robust.max, iter.robust.eff = iter.robust.eff)
    nested.Gsq.CI <- nested.Gsq.CI_w[[1]]
    warnings_col <- c(warnings_col, nested.Gsq.CI_w[[2]])
    if (!any(is.na(nested.Gsq.CI))) {
      nested.Gsq.CI_interval <- Intervals_full(matrix(nested.Gsq.CI, ncol = 2, byrow = TRUE),
                                               closed = c(TRUE, TRUE), type = "R")
      if (!is.null(S.space.H0)) {
        if (!is.na(S0.fct.m_H0)) {
          if (abs(S0.fct.m_H0 - upper.boundary) < tol.psi) {
            nested.Gsq.CI_interval[[2]] <- upper.boundary
          }
          if (abs(S0.fct.m_H0 - lower.boundary) < tol.psi) {
            nested.Gsq.CI_interval[[1]] <- lower.boundary
          }
        }
        nested.Gsq.CI <- interval_intersection(nested.Gsq.CI_interval, as(S.space.H0, "Intervals_full"))
      }
      else {
        nested.Gsq.CI <- nested.Gsq.CI_interval
      }
      rownames(nested.Gsq.CI) <- rep("nested.Gsq.CI", length(nested.Gsq.CI) / 2)
    }
    else {
      warnings_col <- c(warnings_col, "nested.Gsq.CI: NA.\n")
      nested.Gsq.CI <- NULL
    }
  }
  if (any(method == "nested.Xsq")) {
    nested.Xsq.CI_w <- nested_Xsq_robust(y = y, strata = strata, fixed.strata = fixed.strata, h0.fct = h0.fct,
                                         h0.fct.deriv = h0.fct.deriv, S0.fct = S0.fct, S0.fct.deriv = S0.fct.deriv,
                                         max.mph.iter = max.mph.iter, step = step, change.step.after = change.step.after,
                                         y.eps = y.eps, iter.orig = iter.orig, norm.diff.conv = norm.diff.conv,
                                         norm.score.conv = norm.score.conv, max.score.diff.iter = max.score.diff.iter,
                                         S.space.H0 = S.space.H0, tol.psi = tol.psi, tol = tol, max.iter = max.iter,
                                         cut.off = cut.off, delta = delta, adj.epsilon = adj.epsilon,
                                         iter.robust.max = iter.robust.max, iter.robust.eff = iter.robust.eff)
    nested.Xsq.CI <- nested.Xsq.CI_w[[1]]
    warnings_col <- c(warnings_col, nested.Xsq.CI_w[[2]])
    if (!any(is.na(nested.Xsq.CI))) {
      nested.Xsq.CI_interval <- Intervals_full(matrix(nested.Xsq.CI, ncol = 2, byrow = TRUE),
                                               closed = c(TRUE, TRUE), type = "R")
      if (!is.null(S.space.H0)) {
        if (!is.na(S0.fct.m_H0)) {
          if (abs(S0.fct.m_H0 - upper.boundary) < tol.psi) {
            nested.Xsq.CI_interval[[2]] <- upper.boundary
          }
          if (abs(S0.fct.m_H0 - lower.boundary) < tol.psi) {
            nested.Xsq.CI_interval[[1]] <- lower.boundary
          }
        }
        nested.Xsq.CI <- interval_intersection(nested.Xsq.CI_interval, as(S.space.H0, "Intervals_full"))
      }
      else {
        nested.Xsq.CI <- nested.Xsq.CI_interval
      }
      rownames(nested.Xsq.CI) <- rep("nested.Xsq.CI", length(nested.Xsq.CI) / 2)
    }
    else {
      warnings_col <- c(warnings_col, "nested.Xsq.CI: NA.\n")
      nested.Xsq.CI <- NULL
    }
  }
  if (any(method == "nested.PD")) {
    nested.PD.CI_w <- nested_PD_robust(y = y, strata = strata, fixed.strata = fixed.strata, h0.fct = h0.fct,
                                       h0.fct.deriv = h0.fct.deriv, S0.fct = S0.fct, S0.fct.deriv = S0.fct.deriv,
                                       max.mph.iter = max.mph.iter, step = step, change.step.after = change.step.after,
                                       y.eps = y.eps, iter.orig = iter.orig, norm.diff.conv = norm.diff.conv,
                                       norm.score.conv = norm.score.conv, max.score.diff.iter = max.score.diff.iter,
                                       S.space.H0 = S.space.H0, tol.psi = tol.psi, tol = tol, max.iter = max.iter,
                                       cut.off = cut.off, delta = delta, pdlambda = pdlambda, adj.epsilon = adj.epsilon,
                                       iter.robust.max = iter.robust.max, iter.robust.eff = iter.robust.eff)
    nested.PD.CI <- nested.PD.CI_w[[1]]
    warnings_col <- c(warnings_col, nested.PD.CI_w[[2]])
    if (!any(is.na(nested.PD.CI))) {
      nested.PD.CI_interval <- Intervals_full(matrix(nested.PD.CI, ncol = 2, byrow = TRUE),
                                              closed = c(TRUE, TRUE), type = "R")
      if (!is.null(S.space.H0)) {
        if (!is.na(S0.fct.m_H0)) {
          if (abs(S0.fct.m_H0 - upper.boundary) < tol.psi) {
            nested.PD.CI_interval[[2]] <- upper.boundary
          }
          if (abs(S0.fct.m_H0 - lower.boundary) < tol.psi) {
            nested.PD.CI_interval[[1]] <- lower.boundary
          }
        }
        nested.PD.CI <- interval_intersection(nested.PD.CI_interval, as(S.space.H0, "Intervals_full"))
      }
      else {
        nested.PD.CI <- nested.PD.CI_interval
      }
      rownames(nested.PD.CI) <- rep("nested.PD.CI", length(nested.PD.CI) / 2)
    }
    else {
      warnings_col <- c(warnings_col, "nested.PD.CI: NA.\n")
      nested.PD.CI <- NULL
    }
  }
  test_inv_CI_cmbd <- c(Wald.CI, trans.Wald.CI, diff.Gsq.CI, diff.Xsq.CI, diff.PD.CI,
                        nested.Gsq.CI, nested.Xsq.CI, nested.PD.CI)
  if (!is.null(test_inv_CI_cmbd)) {
    if (class(test_inv_CI_cmbd) != "Intervals_full") {
      test_inv_CI_cmbd <- Intervals_full(matrix(test_inv_CI_cmbd, byrow = TRUE, ncol = 2),
                                         closed = c(TRUE, TRUE), type = "R")
      method_sorting_order <- c("Wald", "trans.Wald", "diff.Gsq", "diff.Xsq", "diff.PD",
                                "nested.Gsq", "nested.Xsq", "nested.PD")
      rownames(test_inv_CI_cmbd) <- method[order(match(method, method_sorting_order))]
    }
  }
  if (!is.null(warnings_col)) {
    warning(warnings_col)
  }
  # plot CIs
  if (plot.CIs) {
    if (!is.null(test_inv_CI_cmbd)) {
      if (length(which(is.infinite(test_inv_CI_cmbd))) > 0) {
        if (length(test_inv_CI_cmbd) > 2) {
          which_infinite_left <- which(is.infinite(test_inv_CI_cmbd[, 1]))
          which_infinite_right <- which(is.infinite(test_inv_CI_cmbd[, 2]))
          which_infinite <- union(which_infinite_left, which_infinite_right)
          test_inv_CI_cmbd_plot <- test_inv_CI_cmbd[-which_infinite, ]
        }
        else {
          test_inv_CI_cmbd_plot <- NULL
        }
      }
      else {
        test_inv_CI_cmbd_plot <- test_inv_CI_cmbd
      }
      if (length(test_inv_CI_cmbd_plot) > 0) {
        if ((!is.null(S.space.H0)) & (all(is.finite(S.space.H0)))) {
          plot(test_inv_CI_cmbd_plot, xlab = "test-inversion CIs", xlim = c(min(S.space.H0), max(S.space.H0)),
               col = seq(1, length(test_inv_CI_cmbd) / 2), use_names = TRUE)
        }
        else {
          CI_range <- max(test_inv_CI_cmbd_plot) - min(test_inv_CI_cmbd_plot)
          xlim_low <- min(test_inv_CI_cmbd_plot) - CI_range * 0.2
          xlim_high <- max(test_inv_CI_cmbd_plot) + CI_range * 0.2
          plot(test_inv_CI_cmbd_plot, xlab = "test-inversion CIs", xlim = c(xlim_low, xlim_high),
               col = seq(1, length(test_inv_CI_cmbd) / 2), use_names = TRUE)
        }
      }
    }
  }
  if (!is.null(test_inv_CI_cmbd)) {
    result.table <- cbind(test_inv_CI_cmbd, size(test_inv_CI_cmbd))
    colnames(result.table) <- c("lower", "upper", "length")
  }
  else {
    result.table <- NULL
  }
  list(result.table = result.table, CIs = test_inv_CI_cmbd, Shat = S0.fct.m_H0,
       ase.Shat = ase.S0.fct.m_H0, S.space.H0 = S.space.H0, cc = cc, method = method,
       pdlambda = pdlambda, warnings.collection = warnings_col)
}
