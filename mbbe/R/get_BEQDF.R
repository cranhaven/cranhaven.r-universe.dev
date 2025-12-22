#' Get a data frame with conclusion regarding bioequivalence
#'
#' This function calculates criteria required for conclusion of Bioequivalence
#'  using the NCA set data.
#'
#' @param NCA.Set The NCA set data frame.
#' @param MetricColumn The column name for the metric of interest.
#' Default is `Cmax`
#' @param SubjectColumn The column name to be used as subject term.
#' Default is `ID`.
#' @param TreatmentColumn The column name to be used as treatment term.
#' Default is `treatment`
#' @param SequenceColumn The column name to be used as sequence term.
#' Default is `sequence`.
#' @param PeriodColumn The column name to be used as period term.
#' Default is `period`.
#' @param RefValue The reference value in `treatment` value.
#' Default is `Reference`
#' @param alpha The significance level for confidence intervals.
#' Default is 0.05.
#' @param PartialReplicate A logical value indicating whether partial replicate design is used.
#' Default is `FALSE`
#' @param NTID A logical value indicating whether RSABE criterion from Warfarin guidance should
#' be used.
#'
#' @return The data frame with results for the current `NCA.Set`.
#'
#' @noRd

get_BEQDF <-
  function(NCA.Set = data.frame(),
           MetricColumn = "Cmax",
           SubjectColumn = "ID",
           TreatmentColumn = "treatment",
           SequenceColumn = "sequence",
           PeriodColumn = "period",
           RefValue = "Reference",
           alpha = 0.05,
           PartialReplicate = FALSE,
           NTID) {
    RequestedColumnNames <-
      c(
        MetricColumn,
        SubjectColumn,
        TreatmentColumn,
        SequenceColumn,
        PeriodColumn
      )

    MissingColumns <-
      is.na(match(RequestedColumnNames, colnames(NCA.Set)))

    if (any(MissingColumns)) {
      stop(paste(
        "Please check the dataset, cannot find the column",
        RequestedColumnNames[MissingColumns]
      ))
    }

    NCA.Set$logpk <- log(NCA.Set[[MetricColumn]])
    NCA.Set$ID <- NCA.Set[[SubjectColumn]]
    NCA.Set$treatment <-
      ifelse(NCA.Set[[TreatmentColumn]] == RefValue, "R", "T")
    NCA.Set$period <- NCA.Set[[PeriodColumn]]
    NCA.Set$sequence <- NCA.Set[[SequenceColumn]]

    critbound.s2wR <-
      get_NCA.Set.RSABE(
        NCA.Set = NCA.Set,
        alpha = alpha,
        PartialReplicate = PartialReplicate,
        NTID = NTID
      )

    ABE <- get_NCA.Set.ABE(NCA.Set, alpha = alpha)
    OverallDF <-
      cbind.data.frame(MetricColumn = MetricColumn, ABE, critbound.s2wR)
    rownames(OverallDF) <- NULL

    if (NTID) {
      OverallDF$Assessment <- "NTID criteria used"
      OverallDF$BE <-
        OverallDF$lower.CL < 0.8 &
          OverallDF$upper.CL > 1.25 &
          OverallDF$critbound <= 0 &
          OverallDF$VarianceCriterion
    } else {
      OverallDF$Assessment <-
        ifelse(OverallDF$swR < 0.294, "Unscaled ABE used", "RSABE used")
      if (OverallDF$swR < 0.294) {
        OverallDF$BE <-
          OverallDF$lower.CL >= 0.8 &
            OverallDF$upper.CL <= 1.25
      } else {
        OverallDF$BE <-
          OverallDF$pe >= 0.8 &
            OverallDF$pe <= 1.25 &
            OverallDF$critbound <= 0
      }
    }

    return(OverallDF)
  }

#' Get NCA Set ABE
#'
#' This function calculates the ABE (Average Bioequivalence) using the NCA set data.
#'
#' @inheritParams get_BEQDF
#'
#' @return The ABE data frame with columns Ratio, lower.CL, upper.CL
#' as a result of mixed model analysis.
#' @noRd
get_NCA.Set.ABE <- function(NCA.Set, alpha = 0.05) {
  modelABE <-
    nlme::lme(
      logpk ~ treatment + period + sequence,
      subset = !is.na(logpk),
      random = ~ treatment |
        ID,
      weights = nlme::varIdent(form = ~treatment),
      data = NCA.Set,
      method = "REML",
      na.action = na.exclude,
      control = list(
        opt = "optim",
        msMaxIter = 10000,
        msMaxEval = 10000
      )
    )

  # EMA method B modelABE <- lmer(logpk ~ sequence + period + treatment + (1 | ID), data = NCA.Set)

  ConfIntRestults <-
    confint(emmeans::emmeans(modelABE, pairwise ~ treatment),
      level = 1 - 2 * alpha
    )
  Ratio <- exp(-ConfIntRestults$contrasts$estimate) * 100
  # note the difference lower-upper
  lower.CL <- exp(-ConfIntRestults$contrasts$upper.CL)
  upper.CL <- exp(-ConfIntRestults$contrasts$lower.CL)
  return(data.frame(
    Ratio = Ratio,
    lower.CL = lower.CL,
    upper.CL = upper.CL
  ))
}

#' get_NCA.Set.RSABE
#'
#' This function calculates the RSABE (Reference Scaled Average Bioequivalence) criterion
#' using the NCA set data.
#'
#' @inheritParams get_BEQDF
#'
#' @details
#' VarianceCriterion is calculated only when `NTID` == `TRUE`.
#' Otherwise is is `NA`.
#'
#' @return A data frame with the RSABE results.
#' with the columns swR, pe, critbound, VarianceCriterion
#' @noRd
get_NCA.Set.RSABE <-
  function(NCA.Set = data.frame(),
           alpha = 0.05,
           PartialReplicate = FALSE,
           NTID) {
    NCA.Set <- .get_NCA.Set.ReplicateNumber(NCA.Set)

    # find not necessary columns
    ColumnNamesToExclude <-
      colnames(NCA.Set)[!colnames(NCA.Set) %in% c("ID", "sequence", "code2", "logpk")]

    # make a column with treatment and repl, i.e T1,T2, R1,R2
    NCA.Set$code2 <- paste0(NCA.Set$treatment, NCA.Set$repl)
    NCA.Set <- NCA.Set[order(NCA.Set$ID, NCA.Set$period), ]

    # now reshape to wide with cols subj, seq, pk.R1, pk.R2, pk.T1, pk.T2

    NCA.Set_all <-
      reshape(
        data = NCA.Set,
        direction = "wide",
        v.names = "logpk",
        idvar = c("ID", "sequence"),
        timevar = "code2",
        drop = ColumnNamesToExclude
      )
    # change logpk.T1, T2 ... to T1, T2 ... to avoid keystrokes
    names(NCA.Set_all) <- sub("logpk.", "", names(NCA.Set_all))

    # now T-R analysis, ilat in the SAS code next will give TR = NA if any T1, T2, R1, R2
    # is missing must adapted if 3-period TRT|RTR is used
    if (PartialReplicate) {
      NCA.Set_all$TR <-
        NCA.Set_all$T1 - 0.5 * (NCA.Set_all$R1 + NCA.Set_all$R2)
    } else {
      NCA.Set_all$TR <-
        0.5 * (NCA.Set_all$T1 + NCA.Set_all$T2 - NCA.Set_all$R1 - NCA.Set_all$R2)
    }

    # now get rid of subjects not having all 4 periods which have TR = NA
    NCA.Set_ilat <-
      NCA.Set_all[!is.na(NCA.Set_all$TR), ] # ilat analysis, ANOVA with seq as effect
    NCA.Set_ilat$sequence <- as.factor(NCA.Set_ilat$sequence)
    # with standard contr.treatment we get not the wanted intercept!  with that the intercept is intercept+seq1
    oc <- options("contrasts")
    on.exit(options(oc))
    options(contrasts = c("contr.sum", "contr.poly"))

    tryCatch(
      {
        m1 <- lm(TR ~ sequence, data = NCA.Set_ilat)

        # intercept is estimate of ?t-?R
        est <- coef(m1)[1]
        pe <- exp(est)

        # lower, upper 90% CI
        CI <- confint(m1, 1, level = 1 - 2 * alpha)
        dfTR <- m1$df.residual

        # stderr, 'the unknown x' for unbiased estimate of (?T-?R)^2
        stderr <-
          summary(m1)$coefficients["(Intercept)", "Std. Error"]

        # linearized scABE criterion component x
        x <- est^2 - stderr^2
        boundx <- max(abs(CI))^2

        # now the equivalent of SAS code for R-R, dlat analysis
        NCA.Set_dlat <- NCA.Set_all
        NCA.Set_dlat$RR <- NCA.Set_dlat$R1 - NCA.Set_dlat$R2
        NCA.Set_dlat <- NCA.Set_dlat[!is.na(NCA.Set_dlat$RR), ]
        m2 <- lm(RR ~ sequence, data = NCA.Set_dlat)
        dfRR <- m2$df.residual
        s2wR <- summary(m2)$sigma^2 / 2

        if (NTID) {
          # warfarin guidance
          theta <- ((log(1.11111)) / 0.1)^2
        } else {
          # progesterone guidance for HVD's
          theta <- ((log(1.25)) / 0.25)^2
        }

        # linearized scABE criterion component y
        y <- -theta * s2wR
        boundy <- y * dfRR / qchisq(0.95, dfRR)
        swR <- sqrt(s2wR)
        # linearized scABE criterion, 95% upper bound
        crit <- x + y
        critbound <-
          (x + y) + sqrt(((boundx - x)^2) + ((boundy - y)^2))

        if (NTID) {
          # TT variability for NTID
          # now the equivalent of SAS code for R-R, dlat analysis
          NCA.Set_dlatTT <- NCA.Set_all
          NCA.Set_dlatTT$TT <- NCA.Set_dlatTT$T1 - NCA.Set_dlatTT$T2
          NCA.Set_dlatTT <- NCA.Set_dlatTT[!is.na(NCA.Set_dlatTT$TT), ]

          m3 <- lm(TT ~ sequence, data = NCA.Set_dlatTT)
          dfTT <- m3$df.residual
          s2wT <- summary(m3)$sigma^2 / 2
          swT <- sqrt(s2wT)
          # ratio of swT/swR and 90% CI
          alpha2 <- 0.1
          sRatio <- swT / swR
          sRatioCI <-
            c(
              swT / swR / sqrt(qf(
                alpha2 / 2,
                df1 = dfTT,
                df2 = dfRR,
                lower.tail = FALSE
              )),
              swT / swR / sqrt(qf(
                1 - alpha2 / 2,
                df1 = dfTT,
                df2 = dfRR,
                lower.tail = FALSE
              ))
            )

          VarianceCriterion <- sRatioCI[2] <= 2.5
        } else {
          VarianceCriterion <- NA
        }

        critbound.s2wR <- data.frame(
          swR = swR,
          pe = pe,
          critbound = critbound,
          VarianceCriterion = VarianceCriterion
        )
      },
      error = function(cond) {
        message("Error during linearized scABE criterion calculation:")
        message(cond)
        critbound.s2wR <- data.frame(
          swR = NA,
          pe = NA,
          critbound = NA,
          VarianceCriterion = NA
        )
      }
    )

    critbound.s2wR
  }

#' Get NCA Set Replicate Number
#'
#' This function calculates the replicate number for each observation in the NCA set.
#'
#' @inheritParams get_BEQDF
#'
#' @return A data frame with the replicate numbers added.
#'
#' @noRd
.get_NCA.Set.ReplicateNumber <- function(NCA.Set) {
  NCA.Set <-
    data.frame(NCA.Set[order(NCA.Set$ID, NCA.Set$treatment, NCA.Set$period), ])
  NCA.Set$repl <- replicate(nrow(NCA.Set), 1)
  for (i in c(2:nrow(NCA.Set))) {
    if (NCA.Set$ID[i] != NCA.Set$ID[i - 1] |
      NCA.Set$treatment[i] != NCA.Set$treatment[i - 1]) {
      NCA.Set$repl[i] <- 1
    } else {
      NCA.Set$repl[i] <- NCA.Set$repl[i - 1] + 1
    }
  }

  return(NCA.Set)
}
