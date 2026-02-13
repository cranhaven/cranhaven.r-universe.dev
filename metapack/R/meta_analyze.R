#' bmeta_analyze supersedes the previous two functions: bayes_parobs, bayes_nmr
#' 
#' @description
#' All other worker functions are superseded by this function, so that users can forget about the implementation details and focus on modeling. Meta-analytic data can be either aggregate or individual participant data (IPD). Aggregate data implies that the response consists of estimated effect sizes and their corresponding standard errors, whereas IPD is raw data. Data sets to be used for metapack should be formatted as follows:
#' 
#' | Outcome | SD | DesignM1 | DesignM2 | Trial indicator (`k`) | Treatment indicator (`t`) | n |
#' |:-------:|:--:|:--------:|:--------:|:---------------------:|:-------------------------:|:-:|
#' |  \eqn{y_{13}} | \eqn{S_{13}} | \eqn{x_{13}} | \eqn{w_{13}} | 1   | 3 | 1000 |
#' |  \eqn{y_{10}} | \eqn{S_{10}} | \eqn{x_{10}} | \eqn{w_{10}} | 1   | 0 | 545 |
#' |  \eqn{y_{20}} | \eqn{S_{20}} | \eqn{x_{20}} | \eqn{w_{20}} | 2   | 0 | 1200 |
#' 
#' The first treatment indicator is intentionally selected to be 3, a number greater than 1, to indicate that this data format works for both meta-regression and network meta-regression. Meta-regression refers to when trials included have 2 treatments (i.e., \eqn{t = 0, 1} for all \eqn{k}), and the treatments are compared head to head. On the other hand, network meta-regression includes more than two treatments, where each trial can have a different set of treatments, allowing indirect comparison between treatments that are not compared head to head as long as *consistency* holds (see [Higgins et al. (2012)](https://onlinelibrary.wiley.com/doi/10.1002/jrsm.1044) for consistency).
#' 
#' `bmeta_analyze()` and `bmeta_analyse()` are synonyms.
#' @author Daeyoung Lim, \email{daeyoung.lim@uconn.edu}
#' @param formula an object of class \link[Formula]{Formula}: a symbolic description of the meta-analytic model to fit. For aggregate models, the vector of arm sample sizes must be provided using the function `ns()`. For example, `y1 + y2 | sd1 + sd2 ~ x1 + x2 + ns(n)`---an incomplete formula only for illustration purposes. If no `ns()` is found, individual participant data (IPD) model is assumed.
#' @param data a data frame, list, or environment (or an object coercible by \link[base]{as.data.frame} to a data frame) containing the variables in the model. If not found in `data`, the variables are taken from `environment(formula)`, typically the environment from which `bmeta_analyze` is called.
#' @param prior an optional object that contains the hyperparameter values for the model. To see the complete list of hyperparameters for a specific model, refer to the corresponding worker function's help page, e.g., `help(bayes_parobs)` or `help(bayes_nmr)`. For meta-analysis, `model` is required in the `prior` argument, which is passed to `fmodel` as an integer. If the response is univariate, `NoRecovery` is the only valid option.
#' 
#' * `model="NoRecovery"` - \eqn{\Sigma_{tk} = diag(\sigma_{tk,11}^2,\ldots,\sigma_{tk,JJ}^2)} where \eqn{\sigma_{tk,jj}^2 \sim IG(a_0,b_0)} and \eqn{IG(a,b)} is [the inverse-gamma distribution](https://en.wikipedia.org/wiki/Inverse-gamma_distribution). This specification is useful if the user does not care about the correlation recovery. (`c0`, `dj0`, `a0`, `b0`, `Omega0`)
#' * `model="EquiCovariance"` - \eqn{\Sigma_{tk}=\Sigma} for every combination of \eqn{(t,k)}{`(t,k)`} and \eqn{\Sigma^{-1}\sim Wish_{s_0}(\Sigma_0)}{Sig^{-1} ~ Wish(s0, Sigma0)}. This specification assumes that the user has prior knowledge that the correlation structure does not change across the arms included. (`c0`, `dj0`, `s0`, `Omega0`, `Sigma0`)
#' * `model="EquiWithinTreat"` - \eqn{\Sigma_{tk}=\Sigma_t} and \eqn{\Sigma_t^{-1}\sim  Wish_{s_0}(\Sigma_0)}. This is a relaxed version of `model=2`, allowing the correlation structure to differ across trials but forcing it to stay identical within a trial. (`c0`, `dj0`, `s0`, `Omega0`, `Sigma0`)
#' * `model="EquiCorrelation"` - \eqn{\Sigma_{tk}=\delta_{tk} \rho \delta_{tk}} where \eqn{\delta_{tk}=diag(\Sigma_{tk,11}^{1/2},\ldots,\Sigma_{tk,JJ}^{1/2})}, and \eqn{\rho} is the correlation matrix. This specification allows the variances to vary across arms but requires that the correlations be the same. This is due to the lack of correlation information in the data, which would in turn lead to the nonidentifiability of the correlations if they were allowed to vary. However, this still is an ambitious model which permits maximal degrees of freedom in terms of variance and correlation estimation. (`c0`, `dj0`, `a0`, `b0`, `Omega0`)
#' * `model="Hierarchical"` - The fifth model is hierarchical and thus may require more data than the others: \eqn{(\Sigma_{tk}^{-1}\mid \Sigma)\sim  Wish_{\nu_0}((\nu_0-J-1)^{-1}\Sigma^{-1})} and \eqn{\Sigma \sim  Wish_{d_0}(\Sigma_0)}. \eqn{\Sigma_{tk}} encodes the within-treatment-arm variation while \eqn{\Sigma} captures the between-treatment-arm variation. The hierarchical structure allows the "borrowing of strength" across treatment arms. (`c0`, `dj0`, `d0`, `nu0`, `Sigma0`, `Omega0`)
#' 
#' For network meta-analysis,
#' 
#' * `df` - the degrees of freedom of the multivariate t-distribution for the random effects. Any positive value can be assigned; if `df=Inf`, multivariate normal random effects will be assumed.
#' * `c01` - the variance of the fixed-effect coefficients' prior distribuiton, a multivariate normal distribution, i.e., \eqn{\theta \sim N(0, c_{1}I)}.
#' * `c02` - the variance of the random-effects' variance-related coefficients' prior distribution, a multivariate normal distribution, i.e., \eqn{\phi \sim N(0, c_{2}I)}.
#' * `a4`, `b4`, `a5`, `b5` - the hyperparameters related to when the degrees of freedom for the random effects are treated as unknown/random. `df` is then considered to follow \eqn{Ga(\nu_a, \nu_a/\nu_b)}, \eqn{\nu_a \sim Ga(a_4, b_4)}, and \eqn{\nu_b \sim IG(a_5, b_5)}. All gamma and inverse-gamma distributions are rate-parameterized.
#' 
#' @param mcmc an optional object containing MCMC specification. `ndiscard` is the number of burn-in iterations. `nskip` configures the thinning of the MCMC. For instance, if `nskip=5`, parameters will be saved every 5 iterations. `nkeep` is the size of the posterior sample. The total number of iterations will be `ndiscard + nskip * nkeep`.
#' @param control an optional object that contains the control tuning parameters for the Metropolis-Hastings algorithm. Similar to `prior`, the complete list of control parameters for a specific model is given in the corresponding worker function's help page (see \code{\link{bayes_parobs}} or \code{\link{bayes_nmr}}).
#' These are the lists of available tuning parameters in `control` for meta-analysis and network meta-analysis. Keep in mind that `model` will render some irrelevant tuning parameters ineffective.
#' 
#' * Meta-analysis - `model` (string), `sample_Rho` (logical), `Rho_stepsize` (double), `R_stepsize` (double), `delta_stepsize` (double), `sample_Rho` (logical)
#' * Network meta-analysis - `sample_df` (logical), `sample_Rho` (logical), `lambda_stepsize` (double), `phi_stepsize` (double), `Rho_stepsize` (double)
#' 
#' @param init (Optional) a list of initial values for the parameters to be sampled. The following is the list of available parameters for meta-analysis and network meta-analysis.
#' 
#' * Meta-analysis - `theta` (vector), `gamR` (matrix), `Omega` (matrix), `Rho` (matrix)
#' * Network meta-analysis - `theta` (vector), `phi` (vector), `sig2` (vector), `Rho` (matrix)
#' 
#' The dimensions of the initial values must be conformable for matrix operations. If dimensions don't agree, `bmeta_analyze` will tell you the correct dimension.
#' 
#' @return `bmeta_analyze` returns a classed object of `bsynthesis` for *Bayesian synthesis*
#' @import Formula
#' @details `bmeta_analyze` currently subsumes two worker functions: `bayes_parobs` and `bayes_nmr`. `bmeta_analyze` offers a formula interface.
#' All formulas are parsed using \link[Formula]{Formula}. Formulas for `bmeta_analyze` are constrained to have a strict structure: one or two LHS, and two or three RHS. That is, `lhs_1 ~ rhs_1 | rhs2 | rhs3` or `lhs_1 | lhs_2 ~ rhs_1 | rhs2 | rhs3` (see Examples for more). The tilde (`~`) separates the LHS's and RHS's, each side further separated into parts by vertical bars (`|`).
#' The meaning of each part is syntactically determined by its location inside the formula, like an English sentence. Therefore, all parts **must** come in the exact order as prescribed for `bmeta_analyze` to correctly configure your model.
#'
#'  + The first LHS, the responses, is required for all models.
#'  + The second LHS is only required for aggregate models, corresponding to the standard deviations of the responses.
#'  + The first RHS corresponds to fixed-effects covariates.
#'  + The second RHS corresponds to the variables in either the random-effects matrix (\eqn{w_{tk}' * \gamma_{k}}`) for multivariate meta-analysis or modeling the variances (\eqn{\log\tau_{tk}} = \eqn{z_{tk}' * \phi}) for univariate network meta-analysis.
#'  + The third RHS corresponds to the treatment and trial indicators, and optionally the grouping variable if it exists. The order must be `treat + trial + group`, or `treat + trial` if no grouping exists. Variables here must be supplied in the exact order described; otherwise, model will not be correctly identified.
#' 
#' Internally, `bmeta_analyze` looks for three things: multivariate/univariate, meta-analyis/network meta-analysis, and [aggregate/IPD](https://en.wikipedia.org/wiki/Meta-analysis#Approaches).
#' 
#'  + multivariate/univariate: the dimension of the response is explicit in the formula, and determines univariate versus multivariate.
#'  + meta-analysis/network meta-analysis: the number of levels (`nlevels`) of treatments determines this. If `treat` is not already a factor variable, it is coerced to one.
#'  + aggregate/IPD: `bmeta_analyze` looks for `ns()` in the first RHS. Aggregate models **must** provide the arm sample sizes using the function `ns()` (e.g., if `n` is the sample sizes, `y1 + y2 | sd1 + sd2 ~ x1 + x2 + ns(n))`). If there is no `ns()`, IPD is assumed. Currently, IPD models are a work in progress and not supported yet.
#' 
#' Currently, only `univariate/multivariate` + `meta-analysis` and `univariate` + `network meta-analysis` are allowed. More models will be added in the future.
#' @aliases bmeta_analyse
#' @seealso \code{\link{bayes_parobs}} for multivariate meta-analysis, and \code{\link{bayes_nmr}} for univariate network meta-analysis.
#' @examples
#' set.seed(2797542)
#' data("cholesterol")
#' f_1 <- 'pldlc + phdlc + ptg | sdldl + sdhdl + sdtg ~ 0 + bldlc + bhdlc + btg +
#'   age + durat + white + male + dm + ns(n) | treat | treat + trial + onstat'
#' out_1 <- bmeta_analyze(as.formula(f_1), data = cholesterol,
#'   prior = list(model="NoRecovery"),
#'   mcmc = list(ndiscard = 3, nskip = 1, nkeep = 1),
#'   control=list(scale_x = TRUE, verbose=FALSE))
#'
#' set.seed(2797542)
#' data("TNM")
#' TNM$group <- factor(match(TNM$treat, c("PBO", "R"), nomatch = 0))
#' f_2 <- 'ptg | sdtg ~
#'   0 + bldlc + bhdlc + btg + age + white + male + bmi +
#'   potencymed + potencyhigh + durat + ns(n) |
#'   scale(bldlc) + scale(btg) + group | treat  + trial'
#' out_2 <- bmeta_analyze(as.formula(f_2), data = TNM,
#'   mcmc = list(ndiscard = 1, nskip = 1, nkeep = 1),
#'   control=list(scale_x = TRUE, verbose=FALSE))
#' @references 
#' Lim, D., Chen, M. H., Ibrahim, J. G., Kim, S., Shah, A. K., & Lin, J. (2022). metapack: An R Package for Bayesian Meta-Analysis and Network Meta-Analysis with a Unified Formula Interface. The R journal, 14(3), 142.
#' 
#' Yao, H., Kim, S., Chen, M. H., Ibrahim, J. G., Shah, A. K., & Lin, J. (2015). Bayesian inference for multivariate meta-regression with a partially observed within-study sample covariance matrix. *Journal of the American Statistical Association*, **110(510)**, 528-544.
#' 
#' Li, H., Chen, M. H., Ibrahim, J. G., Kim, S., Shah, A. K., Lin, J., & Tershakovec, A. M. (2019). Bayesian inference for network meta-regression using multivariate random effects with applications to cholesterol lowering drugs. *Biostatistics*, **20(3)**, 499-516.
#' 
#' Li, H., Lim, D., Chen, M. H., Ibrahim, J. G., Kim, S., Shah, A. K., & Lin, J. (2021). Bayesian network meta-regression hierarchical models using heavy-tailed multivariate random effects with covariate-dependent variances. *Statistics in Medicine*.
#' @md
#' @export
'bmeta_analyze' <- function(formula, data, prior = list(), mcmc = list(), control = list(), init = list()) {
    # initial version: DY 2021/07/08
    # - DY Jul 09 2021: add univariate/multivariate detection and individual/aggregate detection
    #                   models for individual participant data will come in the future
    cl <- match.call()
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data"), names(mf), 0)
    mf <- mf[c(1, m)]

    ff <- Formula(formula)
    mf[[1]] <- as.name("model.frame")
    mf$formula <- ff
    mf <- eval(mf, parent.frame())
    
    # Detect whether the model is IPD or aggregate meta-analysis
    # ! Metapack will not check whether the nsample is integer
    # ! although it will be forced to integer in the final step.
    # ! If you use noninteger nsample, you deserve what you get
    #
    # agg_flag = aggregate meta-analysis if 1; IPD if 0
    pnames <- dimnames(mf)[[2]]
    nsample_idx <- which(substr(pnames, 1, 3) == 'ns(')
    if (length(nsample_idx) > 1) {
        stop(paste("Function", sQuote("ns"), "used", length(nsample_idx), "times, which requires 1"))
    }
    stopifnot(length(nsample_idx) <= 1)
    agg_flag <- length(nsample_idx) > 0
    # ! Remove the folowing if statement once resolved
    if (!agg_flag) {
        stop("Models for individual participant data will be implemented soon")
    }
    nsample <- if (agg_flag) {
        mf[, nsample_idx, drop = TRUE]
    } else {
        0
    }

    

    # RHS should consist of (1) X matrix; (2) W/Z matrix; and (3) treatments and trials
    # Any RHS length different from 3 will break the code
    if (!is.na(length(ff)[2]) && length(ff)[2] != 3) {
        if (length(ff)[2] == 2) {
            warning(paste(length(ff)[2], "RHS's identified. Second design matrix defaults to a vector of ones ..."))
            z <- matrix(1, nrow=nrow(mf), ncol=1) # second 
            # Process treatments and trials (or also groups if first-line/second-line grouping exists)
            # ! If not factors already, treatments and trials are converted to factors
            # ! to extract the levels and labels.
            # ! Check if grouping exists
            last_part <- gsub(" ", "", strsplit(as.character(ff)[2], split = "[|]")[[1]][3])
        } else {
            stop(paste("Formula has", length(ff)[2], "RHS's"))
        }
    } else {
        z <- model.matrix(ff, data = mf, rhs = 2)
        # Process treatments and trials (or also groups if first-line/second-line grouping exists)
        # ! If not factors already, treatments and trials are converted to factors
        # ! to extract the levels and labels.
        # ! Check if grouping exists
        last_part <- gsub(" ", "", strsplit(as.character(ff)[3], split = "[|]")[[1]][3])
    }

    last_vars <- strsplit(last_part, split = "[+]")[[1]]
    groups_exist <- ifelse(length(last_vars) == 3, TRUE, FALSE)
    groups_ <- NULL
    group_labels <- NULL
    if (groups_exist) {
        groups <- mf[,ncol(mf)]
        if (is.factor(groups)) {
            ngroups <- nlevels(groups)
            stopifnot(ngroups == 2)
            group_labels <- levels(groups)
            groups_ <- unfactor(groups)
        } else {
            groups_ <- factor(groups)
            ngroups <- nlevels(groups_)
            stopifnot(ngroups == 2)
            group_labels <- levels(groups_)
            groups_ <- unfactor(groups_)
        }

        trt_idx <- ncol(mf)-2
        trial_idx <- ncol(mf)-1
    } else {
        trt_idx <- ncol(mf)-1
        trial_idx <- ncol(mf)
    }
    if (all(check.numeric(group_labels))) {
        group_labels <- as.numeric(group_labels)
    }

    treat <- mf[,trt_idx]
    if (inherits(treat, "factor")) {
        ntreatment <- nlevels(treat)
        treatment_labels <- levels(treat)
        treat_ <- unfactor(treat)
    } else {
        treat_ <- factor(treat)
        ntreatment <- nlevels(treat_)
        treatment_labels <- levels(treat_)
        treat_ <- unfactor(treat_)
    }
    if (ntreatment > 2) {
        network_meta <- TRUE
    } else if (ntreatment == 2) {
        network_meta <- FALSE
    } else {
        stop(paste("Invalid", sQuote("treat")))
    }
    trial <- mf[,trial_idx]
    if (!inherits(trial, "factor")) {
        trial_ <- factor(trial)
        trial_labels <- levels(trial_)
        trial_ <- unfactor(trial_)
    } else {
        trial_labels <- levels(trial)
        trial_ <- unfactor(trial_)
    }
    if (all(check.numeric(trial_labels))) {
        trial_labels <- as.numeric(trial_labels)
    }
    if (all(check.numeric(treatment_labels))) {
        treatment_labels <- as.numeric(treatment_labels)
    }

    y <- model.part(ff, data = mf, lhs = 1)
    if (agg_flag) {
        # Individual participant data will have only one LHS.
        # If it's aggregate meta-analysis, there should be 2 LHS's (one for y, the other for SD)
        stopifnot(length(ff)[1] == 2)
        std_dev <- model.part(ff, data = mf, lhs = 2)
    }
    # Detect multivariate/univariate depending on the dimension of the response
    multivariate_flag <- ncol(y) > 1
    if (network_meta && multivariate_flag) {
        stop("Multivariate network meta-analysis will be implemented soon")
    }

    x <- model.matrix(ff, data = mf, rhs = 1)
    if (agg_flag) {
        # Remove sample size from X matrix
        x <- x[,-which(colnames(x) == pnames[nsample_idx])]
    }
    
    
    if (!is.null(control$scale_x)) {
        scale_x <- control$scale_x
    } else {
        scale_x <- FALSE
    }
    if (!is.null(control$verbose)) {
        verbose <- control$verbose
    } else {
        verbose <- FALSE
    }

    if (!network_meta && agg_flag) {
        if (is.null(prior$model)) {
            warning(paste(sQuote("model"), "in", sQuote("prior"), "argument is not specified. Defaulting to", sQuote("NoRecovery")))
            prior$model <- "NoRecovery"
        }
        fmodel_ <- match.arg(prior$model, c("NoRecovery", "EquiCovariance", "EquiWithinTreat", "EquiCorrelation", "Hierarchical"), several.ok = FALSE)
        fmodel <- if (fmodel_ == "NoRecovery") {
            1
        } else if (fmodel_ == "EquiCovariance") {
            2
        } else if (fmodel_ == "EquiWithinTreat") {
            3
        } else if (fmodel_ == "EquiCorrelation") {
            4
        } else if (fmodel_ == "Hierarchical") {
            5
        }

        if (!multivariate_flag && fmodel != 1) {
            stop(paste("Univariate meta-analysis only valid with", sQuote("NoRecovery")))
        }
        
        out <- bayes_parobs(y, std_dev, x, z, treat_, trial, nsample, fmodel, prior, mcmc, control, init, Treat_order = treatment_labels, Trial_order = trial_labels, group = groups_, group_order = group_labels, scale_x = scale_x, verbose = verbose)
        out$call <- cl
        out$multivariate <- multivariate_flag
        out$aggregate <- agg_flag
        class(out) <- c(class(out), "bsynthesis")
        return(out)
    } else {
        out <- bayes_nmr(unlist(y), unlist(std_dev), x, z, treat_, trial, nsample, prior, mcmc, control, init, Treat_order = treatment_labels, Trial_order = trial_labels, scale_x = scale_x, verbose = verbose)
        out$call <- cl
        out$multivariate <- multivariate_flag
        out$aggregate <- agg_flag
        class(out) <- c(class(out), "bsynthesis")
        return(out)
    }
}


#' @rdname bmeta_analyze
#' @export
bmeta_analyse <- bmeta_analyze