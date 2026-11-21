\name{CIMEHR-package}
\alias{CIMEHR-package}
\docType{package}
\title{Longitudinal Analysis of Electronic Health Records with Informative
  Visiting and Recording}
\description{
  Longitudinal outcomes in electronic health records (EHR) are seen only when
  a patient visits and the outcome is recorded at that visit. Both processes
  can depend on the patient's latent health. \pkg{CIMEHR} fits the CIMEHR
  estimator for this setting together with comparison methods (summary
  statistics, linear mixed models, Lin-Ying, Liang, pairwise likelihood, joint
  models, inverse intensity rate ratio weighting and balancing, multiple
  imputation, linear increments and EHRJoint). All methods share one
  interface, \code{\link{fit_method}}, and each method is implemented in its
  own source file.
}
\details{
  \subsection{Common interface}{
    \code{fit_method(method, data, ...)} fits one method, where \code{method}
    is a method key (the \code{method} column of
    \code{\link{available_comparison_methods}()}, for example \code{"lmm"} or
    \code{"cimehr"}) or a label from the \code{label} column. Methods have
    no numbers; a numeric \code{method} is an error. The column names
    (\code{id}, \code{time}, \code{outcome}, \code{obs_indicator},
    \code{censor_col}, \code{entry_col}) and the covariate sets
    (\code{visit_covs}, \code{long_covs}, \code{outcome_random_covs},
    \dots) are common arguments; options specific to one method go in
    \code{controls = list(...)}. \code{\link{method_comparisons}} fits
    several methods to the same data and \code{\link{cimehr_methods}} gives
    the default comparison set.

    Wrapper functions call \code{fit_method()} for a fixed method or family:
    \code{\link{Summary_stat}} (\code{summary_min}, \code{summary_mean},
    \code{summary_median}, \code{summary_max}), \code{\link{LMM}}
    (\code{lmm}), \code{\link{Lin_Ying}} (\code{lin_ying_marker},
    \code{lin_ying_visit}), \code{\link{Liang}} (\code{liang_marker},
    \code{liang_visit}), \code{\link{Pairwise_likelihood}} (\code{pairlik}),
    \code{\link{JMVL_G}} (\code{jmvl_g_marker}, \code{jmvl_g_visit}),
    \code{\link{IIRR}} (\code{iirr_weighting_marker},
    \code{iirr_weighting_visit}), \code{\link{IIRR_balancing}}
    (\code{iirr_balancing_marker}, \code{iirr_balancing_visit}),
    \code{\link{Multiple_imputation_IP}} (\code{mi_lmm}, \code{mi_liang},
    \code{mi_iirr}), \code{\link{Linear_increment_IP}} (\code{li_lmm},
    \code{li_liang}, \code{li_iirr}),
    \code{\link{EHRJoint}} (\code{ehrjoint}), \code{\link{CIMEHR_spline}}
    (\code{cimehr_spline}), \code{\link{CIMEHR}} (\code{cimehr}),
    \code{\link{CIMEHR_HL}} (\code{cimehr_hl}) and
    \code{\link{CIMEHR_spline_HL}} (\code{cimehr_spline_hl}).
  }

  \subsection{Methods}{
    Several estimators have a marker version, whose event process is the
    recorded outcomes (visits with \code{R = 1}), and a visit version, whose
    event process is all visits.
    \tabular{lll}{
      Key \tab Label \tab Source file \cr
      \code{summary_min} \tab Summary-Min \tab \file{R/summary_min.R} \cr
      \code{summary_mean} \tab Summary-Mean \tab \file{R/summary_mean.R} \cr
      \code{summary_median} \tab Summary-Median \tab \file{R/summary_median.R} \cr
      \code{summary_max} \tab Summary-Max \tab \file{R/summary_max.R} \cr
      \code{lmm} \tab LMM \tab \file{R/lmm.R} \cr
      \code{oa_lmm} \tab OA-LMM \tab \file{R/oa_lmm.R} \cr
      \code{va_lmm} \tab VA-LMM \tab \file{R/va_lmm.R} \cr
      \code{lin_ying_marker} \tab Lin-Ying (marker) \tab \file{R/lin_ying_marker.R} \cr
      \code{liang_marker} \tab Liang (marker) \tab \file{R/liang_marker.R} \cr
      \code{pairlik} \tab PairLik \tab \file{R/pairlik.R} \cr
      \code{jmvl_g_marker} \tab JMVL-G (marker) \tab \file{R/jmvl_g_marker.R} \cr
      \code{iirr_weighting_marker} \tab IIRR-Weighting (marker) \tab \file{R/iirr_weighting_marker.R} \cr
      \code{iirr_balancing_marker} \tab IIRR-Balancing (marker) \tab \file{R/iirr_balancing_marker.R} \cr
      \code{mi_lmm} \tab MI-LMM \tab \file{R/mi_lmm.R} \cr
      \code{mi_liang} \tab MI-Liang \tab \file{R/mi_liang.R} \cr
      \code{mi_iirr} \tab MI-IIRR \tab \file{R/mi_iirr.R} \cr
      \code{li_lmm} \tab LI-LMM \tab \file{R/li_lmm.R} \cr
      \code{li_liang} \tab LI-Liang \tab \file{R/li_liang.R} \cr
      \code{li_iirr} \tab LI-IIRR \tab \file{R/li_iirr.R} \cr
      \code{ehrjoint} \tab EHRJoint \tab \file{R/ehrjoint.R} \cr
      \code{iirr_weighting_visit} \tab IIRR-Weighting (visit) \tab \file{R/iirr_weighting_visit.R} \cr
      \code{cimehr_spline} \tab CIMEHR spline \tab \file{R/cimehr_spline.R} \cr
      \code{lin_ying_visit} \tab Lin-Ying (visit) \tab \file{R/lin_ying_visit.R} \cr
      \code{liang_visit} \tab Liang (visit) \tab \file{R/liang_visit.R} \cr
      \code{jmvl_g_visit} \tab JMVL-G (visit) \tab \file{R/jmvl_g_visit.R} \cr
      \code{iirr_balancing_visit} \tab IIRR-Balancing (visit) \tab \file{R/iirr_balancing_visit.R} \cr
      \code{cimehr} \tab CIMEHR \tab \file{R/cimehr.R} \cr
      \code{cimehr_hl} \tab CIMEHR-HL \tab \file{R/cimehr_hl.R} \cr
      \code{cimehr_spline_hl} \tab Spline CIMEHR-HL \tab \file{R/cimehr_spline_hl.R} \cr
    }
    \describe{
      \item{\code{summary_min}, \code{summary_mean}, \code{summary_median},
        \code{summary_max}}{least-squares regression of each subject's
        minimum, mean, median or maximum recorded outcome on the
        subject-level mean covariates, with HC3 standard errors.}
      \item{\code{lmm}, \code{oa_lmm}, \code{va_lmm}}{Gaussian linear mixed
        model (\pkg{nlme}) for the recorded outcomes (\code{lmm}), adjusted
        for the cumulative number of recorded outcomes (\code{oa_lmm}) or of
        visits (\code{va_lmm}).}
      \item{\code{lin_ying_marker}, \code{lin_ying_visit}}{estimating
        equations for a marginal mean with an unspecified time trend and a
        proportional-rate event process. Patient-level stacked standard
        errors propagate the fitted rate regression and empirical profile
        means, including the nearest-history outcome smoother.}
      \item{\code{liang_marker}, \code{liang_visit}}{joint model of a
        proportional-rate gamma-frailty event process and the longitudinal
        outcome. Patient-level stacked standard errors propagate the rate
        regression, Breslow baseline, frailty moments, centering weights
        and outcome equation. At an inactive frailty component, inference
        conditions on the selected no-frailty branch.}
      \item{\code{pairlik}}{cross-swapped pairwise likelihood of Chen, Ning and
        Cai.}
      \item{\code{jmvl_g_marker}, \code{jmvl_g_visit}}{parametric joint model
        with exponential gap times and bivariate Gaussian random effects for
        the event process and the outcome.}
      \item{\code{iirr_weighting_marker}, \code{iirr_weighting_visit},
        \code{iirr_balancing_marker}, \code{iirr_balancing_visit}}{inverse
        intensity rate ratio weighting from a proportional-rate event model
        (\code{iirr_weighting_marker}, \code{iirr_weighting_visit}), or
        entropy-calibrated balancing weights (\code{iirr_balancing_marker},
        \code{iirr_balancing_visit}).}
      \item{\code{mi_lmm}, \code{mi_liang}, \code{mi_iirr}}{multiple
        imputation of unrecorded outcomes at visits under missing at random
        (\pkg{mice}, \code{2l.pan}) followed by LMM, Liang or visit-process
        IIRR, pooled by Rubin's rules. These need the suggested packages
        \pkg{mice} and \pkg{pan}.}
      \item{\code{li_lmm}, \code{li_liang}, \code{li_iirr}}{linear-increments
        reconstruction of unrecorded outcomes followed by LMM, Liang or
        visit-process IIRR. All three default to reverse reconstruction of
        leading missing outcomes and forward reconstruction after the first
        observation, using separately estimated increment models. Subjects
        with visits but no observed outcome are excluded before estimation
        by default. Patient bootstrap optionally refits the entire procedure.}
      \item{\code{ehrjoint}}{the EHRJoint method of Du, Shi and Mukherjee: a
        shared visiting frailty and a covariate-based recording model with
        logit or probit link.}
      \item{\code{cimehr}}{three stages. (1) A proportional-rate visiting model
        with a shared frailty whose variance is estimated by moments
        (\code{gamma.*}, \code{sigma2_zeta}). (2) A within-patient pairwise
        probit likelihood for the recording indicators, with fixed effects
        (\code{alpha.*}), loadings on the visiting frailty (\code{delta.*})
        and recording random-effect variances (\code{sigma2_q.*}); the
        frailty enters through its Laplace (Gaussian) posterior
        approximation. (3) An estimating equation for the outcome built from
        the posterior moments and the fitted recording model, with covariate
        effects \code{beta.*} and random-effect link terms \code{theta.*}.
        Standard errors come from a patient-level stacked sandwich over all
        three stages.}
      \item{\code{cimehr_hl}}{CIMEHR with higher-order (third and fourth
        derivative) corrections to the Laplace posterior moments and an
        all-stage patient-level stacked sandwich. The approximation and
        sandwich do not guarantee removal of approximation bias.}
      \item{\code{cimehr_spline}, \code{cimehr_spline_hl}}{CIMEHR and
        CIMEHR-HL with a spline baseline time trend and optional
        covariate-specific time trajectories; fitted curves are available
        from \code{predict()} (see \code{\link{CIMEHR_spline}}).
        The default stacked covariance propagates all three stages,
        conditional on the chosen spline basis and selected variance face.}
    }
    Methods \code{mi_iirr} and \code{li_iirr} require common
    follow-up (the same censoring time for every subject). The CIMEHR methods
    (\code{cimehr}, \code{cimehr_hl}, \code{cimehr_spline},
    \code{cimehr_spline_hl}) require \code{censor_col}.
  }

  \subsection{Data format}{
    One row per visit with columns \code{id}, \code{time}, \code{Y} (outcome,
    \code{NA} when not recorded), \code{R} (1 if the outcome was recorded at
    the visit, else 0), subject-level covariates such as \code{Z} and
    \code{X} (numeric and constant within subject), \code{C} (censoring time,
    constant within subject) and optionally \code{E} (entry time). A subject
    with no visits has exactly one row with \code{time = NA}, \code{Y = NA}
    and \code{R = NA}. Column names are set through the arguments of
    \code{fit_method()}.
  }

  \subsection{Risk sets, entry and censoring}{
    Visits must lie in \code{[E, C]}. The risk set of the visit process is
    \code{(E, C]}: a visit exactly at the entry time is not part of the visit
    process and is removed automatically; the number removed is returned as
    \code{diagnostics$n_visits_at_entry_excluded}. Without \code{entry_col}
    there is no left truncation. Delayed entry is supported by
    \code{lin_ying_marker}, \code{lin_ying_visit}, \code{liang_marker},
    \code{liang_visit}, \code{jmvl_g_marker}, \code{jmvl_g_visit},
    \code{iirr_weighting_marker}, \code{iirr_weighting_visit},
    \code{iirr_balancing_marker}, \code{iirr_balancing_visit},
    \code{ehrjoint} and the CIMEHR methods.
  }

  \subsection{Results}{
    Every fit is a list of class \code{"cimehr_fit"} with components
    \code{coefficients}, \code{se}, \code{vcov} (outcome parameters),
    \code{vcov_all} (all stages, when available) and \code{diagnostics}.
    \code{\link{summary_outcome}}, \code{\link{summary_visiting}} and
    \code{\link{summary_observation}} return one table per stage;
    \code{\link{extract_coefficient}} and \code{\link{coef_stage}} extract
    estimates; \code{coef}, \code{vcov}, \code{confint}, \code{summary} and
    \code{print} methods are provided.

    Inference differs by method. Direct Lin-Ying and Liang methods use
    \code{inference = "stacked"}; all four CIMEHR methods also default to
    stacked inference. Their stacked covariances account for the fitted
    stages described above, but do not correct model misspecification or
    approximation bias. Covariance regularity and boundary limitations are
    recorded in \code{diagnostics}; a returned standard error alone does
    not establish valid regular Wald inference. Other methods retain their
    own model-based, robust, multiple-imputation, conditional or bootstrap
    inference rules (see \code{\link{comparison_methods}}).
  }

  \subsection{File layout}{
    Each method is implemented in one file, \file{R/<key>.R}, named by its
    method key (see the table above); file names carry no numbers. A named
    wrapper is defined in the file of a method it fits, for example
    \code{Liang()} in \file{R/liang_marker.R}. Shared code is kept in helper
    files: \file{registry.R} (method registry, keys and
    \code{cimehr_methods()}), \file{dispatch.R} (\code{fit_method()} and
    \code{method_comparisons()}), \file{input_data.R} (data checks and
    entry-visit removal), \file{native_adapter.R} (argument mapping and
    result standardization), \file{outcome_summary.R} (outcome table and S3
    methods), \file{stages.R} and \file{stages_spline.R} (stage tables),
    \file{cimehr_shared.R} (engine shared by the CIMEHR methods),
    \file{cimehr_hl_moments.R} and \file{cimehr_hl.R} (higher-order Laplace
    moments for \code{cimehr_hl} and \code{cimehr_spline_hl};
    \file{cimehr_hl.R} is also the method file of \code{cimehr_hl} and
    holds \code{CIMEHR_HL()}).
  }
}
\seealso{
  \code{\link{fit_method}}, \code{\link{CIMEHR}},
  \code{\link{available_comparison_methods}}
}
\examples{
data("cimehr_example", package = "CIMEHR")
dat <- merge(cimehr_example$participants, cimehr_example$visits,
             by = "id", all.x = TRUE)
dat <- dat[dat$id <= 200, ]  # 200 of the 1,000 participants keeps the example fast
dat <- dat[order(dat$id, dat$time, na.last = TRUE), ]
rownames(dat) <- NULL

fit <- fit_method("cimehr", dat, visit_covs = c("Z", "X"),
                  obs_fixed_covs = c("Z", "X"), obs_random_covs = "Z",
                  long_covs = c("Z", "X"), outcome_random_covs = "Z",
                  censor_col = "C")
summary_outcome(fit)
summary_visiting(fit)
summary_observation(fit)
# Complete four-method workflow: vignette("cimehr-workflow")
}
\keyword{package}
