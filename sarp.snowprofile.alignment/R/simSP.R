#' Similarity measure between snow profile pairs
#'
#' This function calculates a similarity measure for two snow profiles
#' that have been aligned onto the same height grid (either through DTW or resampling).
#' If one profile contains more layers than the other one, the layers with a non-matched height
#' represent missing layers and will be treated accordingly.
#' The similarity measure is compatible with top-down alignments and is symmetric with respect to its inputs, i.e.
#' `simSP(P1, P2) == simSP(P2, P1)`. **Several different approaches of computing the measure have been implemented by now,
#' see Details below.**
#'
#' Several approaches of computing the similarity measure (**simple**, **HerlaEtAl2021**, **tsa_WLdetection**, **rta_WLdetection**) represent different flavours of the approach detailed in
#' Herla et al (2021). In essence, they are a simple approach to incorporate avalanche hazard relevant characteristics into the score by
#' computing the score as arithmetic mean of 4 different grain type classes:
#'   - weak layers (wl): SH and DH
#'   - new snow (pp): PP and DF
#'   - crusts (cr): MFcr and IF
#'   - bulk: the rest (i.e., predominantly RG, FC, FCxr --- MF falls also in here, will maybe be adjusted in future.)
#'
#' Additionally, for classes wl and cr, vertical windows are computed to weigh layers more heavily that have no other wl or cr
#' grain types in their neighborhood.
#'
#' Type **simple** deviates from *simple2* (= *HerlaEtAl2021*) by computing the aforementioned vertical windows based on heuristic
#' depth ranges (i.e., Surface--30cm depth--80cm depth--150cm depth--Ground). It is otherwise identical to the **simple2**
#' type, which computes as many numbers of equidistant vertical windows as number of wl or cr are present in the profile.
#'
#' Type **tsa_WLdetection** employs a similar approach as *simple*, but it identifies weak layers (wl) based on the Threshold Sum Approach
#' (>= 5 TSA, lemons, German 'Nieten'). Therefore, the original profiles need to contain grain size information, which allows you to pre-compute the lemons
#' for all layers (additionally to the otherwise
#' necessary gain type and hardness information). It is thus more targeted to simulated profiles or detailed manual profiles of very high quality.
#' While the former two types neglect hardness information of wl and cr classes, this type does not.
#' Type **rta_WLdetection** works analogous, but uses RTA instead of TSA and a threshold of >= 0.8.
#'
#' Unlike the former types, **layerwise** applies no weighting at all if used as per default. That means that the similarity of each individual layer
#' contributes equally to the overall similarity measure. It is, however, very flexible in that any custom scaling factor can be applied to each layer. The resulting similarity score is then computed by
#'   - sim = sim_gtype x sim_hardness (i.e., an array of similarities, one for each layer)
#'   - simSP = sum(sim * scalingFactor) / sum(scalingFactor),
#'
#' where the denominator ensures that the resulting score will be within `[0, 1]`. If you want to explore your own scaling approach,
#' both input snow profiles need to contain a column called `$layers$scalingFactor` that store the desired factor.
#' Type **rta_scaling** is a special case of `layerwise`, where the scaling is determined by the relative lemons of each layer (RTA, see Monti et al 2013).
#' Type **remotesensing** makes use of the layerwise algorithm, but triggers an alternative similarity computation beforehand. Similarity is first computed from density and Optical Grain Size (ogs),
#' and then the layerwise similarity is called upon to compute the global sim score.
#'
#' The newest approach **wsum_scaled** differs from all approaches before on a foundational level. While all other approaches compute the similarity of two layers by *multiplying* their
#' similarities in various layer properties (e.g., gtype, hardness), this approach computes a *weighted sum* of the similarities of three layer properties:
#' gtype, hardness, layer stability. Differently than previous approaches, the layer stability is not only used for scaling purposes but also for the similarity calculation itself.
#' By scaling the similarity with stability, unstable layers get more weight in the resulting score. By additionally including the similarity of layer stability in the similarity calculation,
#' profiles with similar stability patterns get a higher score.
#' By using a weighted sum to combine the three layer properties, the approach is identical to how the underlying alignment of the profiles is computed. The resulting similarity score is computed by
#'   - sim = w1 x sim_gtype + w2 x sim_hardness + w3 x sim_stability  (i.e., an array of similarities, one for each layer)
#'   - simSP = sum(sim * stability) / sum(stability),
#'
#' where layer stability defaults to p_unstable, or to scalingFactor (if apply_scalingFactor is TRUE).
#'
#'
#'
#' **NOTE** that for all types that include stability indices (TSA, RTA, p_unstable, scalingFactor), these measures need to be computed *prior to aligning* the profiles
#' (and therefore need to be present in the profiles provided to this function!)
#'
#' @param ref snowprofile object 1
#' @param qw snowprofile object 2 (matched layers need to be on the same height grid of ref)
#' @param gtype_distMat_simSP a distance matrix that stores **distance** information of grain types (*Be careful* to convert
#' similarities, as in [grainSimilarity_evaluate], into dissimilarities with [sim2dist].)
#' @param simType the similarity measure can be computed in several different ways (of sophistication). See Details section.
#' Possible choices
#'   - `simple`
#'   - `HerlaEtAl2021` (= `simple2`)
#'   - `tsa_WLdetection` & `rta_WLdetection`
#'   - `layerwise` & `rta_scaling`
#'   - `remotesensing`
#'   - `wsum_scaled`
#' @param nonMatchedSim sets the similarity value of non-matched layers `[0, 1]`. "indifference" = 0.5, penalty < 0.5.
#' Note that [dtwSP] sets the same value and overrides the default value in this function!
#' @param nonMatchedThickness If `NA`, every unique non-matched layer (i.e., contiguous resampled layers with identical properties)
#' contributes to the overall similarity by 1 x `nonMatchedSim`. In that case, 5cm of non-matched new snow has the same effect on
#' the overall similarity as 50cm of non-matched new snow. To make the effect of non-matched layers dependent on the layer thickness,
#' provide a positive number to `nonMatchedThickness`. For `nonMatchedThickness = 10`, every 10cm of a unique non-matched layer
#' contribute to the overall similarity by 1 x `nonMatchedSim`. So, 50cm of non-matched new snow would contribute 5 times stronger
#' than 5cm of non-matched new snow.
#' Note that [dtwSP] sets the same value and overrides the default value in this function!
#' @param verbose print similarities of different grain classes to console? default FALSE
#' @param returnDF additionally return the similarities of the grain classes as data.frame (analogously to verbose);
#' the return object then has the fields `$sim` and `$simDF`
#' @param apply_scalingFactor Only applicable to `type`s `layerwise` and `wsum_scaled`: `TRUE` or `FALSE`, see Details.
#' @param simWeights a numeric vector with exact names that specifies the weights for the weighted averaging in `wsum_scaled`
#' @param ... not used, but necessary to absorb unused inputs from [dtwSP]
#'
#' @return Either a scalar similarity between `[0, 1]` with 1 referring to the two profiles being identical, or
#' (if `returnDF` is TRUE) a list with the elements `$sim` and `$simDF`.
#'
#' @references
#' Herla, F., Horton, S., Mair, P., & Haegeli, P. (2021). Snow profile alignment and similarity assessment for aggregating, clustering,
#' and evaluating of snowpack model output for avalanche forecasting. Geoscientific Model Development, 14(1), 239–258. https://doi.org/10.5194/gmd-14-239-2021
#'
#' Monti, F., & Schweizer, J. (2013). A relative difference approach to detect potential weak layers within a snow profile.
#' Proceedings of the 2013 International Snow Science Workshop, {G}renoble, {F}rance, 339–343. Retrieved from https://arc.lib.montana.edu/snow-science/item.php?id=1861
#'
#' @examples
#'
#' ## first align two profiles, then assess the similarity of the aligned profiles
#' alignment <- dtwSP(SPpairs$A_modeled, SPpairs$A_manual)
#' SIM <- simSP(alignment$queryWarped, alignment$reference, verbose = TRUE)
#'
#' ## similarity of identical profiles
#' SIM <- simSP(alignment$queryWarped, alignment$queryWarped, verbose = TRUE)
#'
#' ## non-matched layers become apparent here:
#' alignment <- plotSPalignment(SPpairs$C_day1, SPpairs$C_day2, keep.alignment = TRUE,
#'                              rescale2refHS = FALSE, checkGlobalAlignment = FALSE)
#' simSP(alignment$queryWarped, alignment$reference, nonMatchedSim = 0.5)
#' ## smaller similarity score due to 'penalty' of non-matched layers:
#' simSP(alignment$queryWarped, alignment$reference, nonMatchedSim = 0)
#' ## even smaller similarity score due to higher impact of non-matched layer thickness:
#' simSP(alignment$queryWarped, alignment$reference, nonMatchedSim = 0, nonMatchedThickness = 1)
#'
#' ## detect WL based on lemons (instead of grain type alone):
#' P1 <- computeTSA(SPpairs$D_generalAlignment1)
#' P2 <- computeTSA(SPpairs$D_generalAlignment2)
#' alignment <- dtwSP(P1, P2, simType = "tsa_wldetection")
#' # sim based on WL-detection with TSA:
#' simSP(alignment$queryWarped, alignment$reference, type = "tsa_wldetection", verbose = TRUE)
#' # sim solely based on grain type, neglecting TSA information
#' simSP(alignment$queryWarped, alignment$reference, type = "simple", verbose = TRUE)
#'
#' ## RTA scaling type
#' P1 <- computeRTA(P1)
#' P2 <- computeRTA(P2)
#' alignment <- dtwSP(P1, P2, simType = "rta_scaling")
#' # sim based on scaling with RTA
#' simSP(alignment$queryWarped, alignment$reference, type = "rta_scaling")
#' # sim based on WL-detection with RTA
#' simSP(alignment$queryWarped, alignment$reference, type = "rta_wldetection")
#' # sim based on WL-detection with TSA
#' simSP(alignment$queryWarped, alignment$reference, type = "tsa_wldetection")
#'
#' ## layerwise similarity (i) unscaled...
#' simSP(alignment$queryWarped, alignment$reference, type = "layerwise", verbose = TRUE)
#'
#' ##... or (ii) with custom scaling factor (example only illustrative)
#' alignment$queryWarped$layers$scalingFactor <- 0.1
#' alignment$queryWarped$layers$scalingFactor[findPWL(alignment$queryWarped)] <- 1
#' alignment$reference$layers$scalingFactor <- 0.1
#' alignment$reference$layers$scalingFactor[findPWL(alignment$reference)] <- 1
#' simSP(alignment$queryWarped, alignment$reference, type = "layerwise",
#'       apply_scalingFactor = TRUE, verbose = TRUE)
#'
#' @export
simSP <- function(ref, qw, gtype_distMat_simSP = sim2dist(grainSimilarity_evaluate(triag = FALSE)), simType = "HerlaEtAl2021",
                  nonMatchedSim = 0, nonMatchedThickness = 10, verbose = FALSE, returnDF = FALSE, apply_scalingFactor = FALSE,
                  simWeights = c(gtype = 1/3, hardness = 1/3, stability = 1/3), ...) {

  if (!is.snowprofile(ref) | !is.snowprofile(qw)) stop("At least one of ref and qw is not a snowprofile object!")
  simType <- tolower(simType)

  ## --- height grid operations ----
  ## get number of layers from both profiles
  refNL <- nrow(ref$layers)
  qwNL <- nrow(qw$layers)

  ## find matching layer heights in profiles while accounting for numeric tolerance (round to 2 decimal points)
  id_qw <- which(match_with_tolerance(qw$layers$height, ref$layers$height, d = 2))
  id_ref <- which(match_with_tolerance(ref$layers$height, qw$layers$height, d = 2))

  if (length(id_ref) != length(id_qw)) stop("Matching layers cannot be determined correctly.
                                            Something might be wrong with the height grid of the profiles..")
  n_id <- length(id_ref)

  if (n_id < 0) {
    warning(paste0("Profiles have no single layer interface at the same height!
                    queryWarped: ", paste0(qw$layers$height, collapse = " "), "
                    ref:         ", paste0(ref$layers$height, collapse = " "), "
                    returning 'NA'.."))
    return(NA)
  }

  ## determine relevant properties based on which framework we are operating (avy or remote sensing)
  if (simType != "remotesensing") {
    if (simType %in% c("simple", "herlaetal2021", "simple2", "layerwise", "wsum", "wsum_scaled")) {
      relevantProperties <- c("gtype", "hardness")
      if (simType == "layerwise" & apply_scalingFactor) {
        relevantProperties <- c(relevantProperties, "scalingFactor")
      } else if (simType == "wsum_scaled" & apply_scalingFactor) {
        relevantProperties <- c(relevantProperties, "scalingFactor")
      } else if (simType == "wsum_scaled" & "p_unstable" %in% intersect(names(ref$layers), names(qw$layers))) {
        relevantProperties <- c(relevantProperties, "p_unstable")
      }
    } else if (simType == "tsa_wldetection") {
      relevantProperties <- c("gtype", "hardness", "tsa")
    } else if (simType %in% c("rta_wldetection", "rta_scaling")) {
      relevantProperties <- c("gtype", "hardness", "rta")
    } else {
      stop("Unknown similarity type!")
    }
  } # end avy possibilities
  else if (simType == "remotesensing") {
    relevantProperties <- c("density", "ogs")
  }

  ## merge and resample layers that are on same height grid:
  RES <- resampleSPpairs(qw$layers[id_qw,], ref$layers[id_ref,], mergeBeforeResampling = TRUE, dims = relevantProperties)
  rl <- RES$ref
  qwl <- RES$query  # note: RES$query is indeed correct here (naming convention of resampleSPpairs)

  ## extract layer information from layers on different height grid (i.e., non-matched layers):
  missingLayers <- data.table::as.data.table(NULL)
  nonMatchedIn <- NULL
  if (n_id < qwNL) {
    qwNonMatchedLayers <- qw$layers[-(id_qw), ]
    qwNonMatchedLayers$height <- cumsum(qwNonMatchedLayers$thickness)  # reset height, so that mergeIdentLayers (below) will derive correct thickness
    qwNonMatchedLayers <- mergeIdentLayers(qwNonMatchedLayers, relevantProperties)
    if (!is.na(nonMatchedThickness)) {
      qwNonMatchedLayers <- qwNonMatchedLayers[rep(seq_len(nrow(qwNonMatchedLayers)), ceiling(qwNonMatchedLayers$thickness / nonMatchedThickness)), ]
    }
    missingLayers <- rbind(missingLayers, qwNonMatchedLayers)
    nonMatchedIn <- c(nonMatchedIn, "qw")
  }
  if (n_id < refNL) {
    refNonMatchedLayers <- ref$layers[-(id_ref), ]
    refNonMatchedLayers$height <- cumsum(refNonMatchedLayers$thickness)  # reset height, so that mergeIdentLayers (below) will derive correct thickness
    refNonMatchedLayers <- mergeIdentLayers(refNonMatchedLayers, relevantProperties)
    if (!is.na(nonMatchedThickness)) {
      refNonMatchedLayers <- refNonMatchedLayers[rep(seq_len(nrow(refNonMatchedLayers)), ceiling(refNonMatchedLayers$thickness / nonMatchedThickness)), ]
    }
    missingLayers <- rbind(missingLayers, refNonMatchedLayers, fill = TRUE)
    nonMatchedIn <- c(nonMatchedIn, "ref")
  }

  if (length(nonMatchedIn) > 1) {  # unmatched layers in both profiles: that is odd! print to user:
    warning("simSP: non-matched layers in both profiles. Investigate why!")
    # print(missingLayers)
  }

  ## --- generic similarity calculations ----
  if (simType != "remotesensing") {
    refGrains <- as.character(rl$gtype)
    qwGrains <- as.character(qwl$gtype)
    matchedGrid <- rep(rl$height, times = 2)  # stacked, analogous to matchedDF (further down)
    nGrains <- length(refGrains)

    ## distances and according similarities from various dims:
    dGT <- extractFromScoringMatrix(ScoringFrame = gtype_distMat_simSP,
                                    grainType1 = qwGrains,
                                    grainType2 = refGrains)  # vector
    simGT <- sim2dist(dGT)  # vector

    dHHI <- hardnessDistance(qwl$hardness, rl$hardness, normalize = TRUE, absDist = TRUE)  # vector
    simHHI <- sim2dist(dHHI)  # vector
    ## if some hardness values are NA, assign 0.5 to prevent routine from breaking
    ## (i.e., similary as in grainSimilarity_evaluate()):
    simHHI[is.na(simHHI)] <- 0.5

    ## combine dimensions
    sim <- (simGT * simHHI)[, 1]
  } else if (simType == "remotesensing") {
    ## distances and according similarities from density and ogs:
    dDensity <- densityDistance(qwl$density, rl$density, normalize = TRUE, absDist = TRUE) # vector
    simDensity <- sim2dist(dDensity)  # vector

    dOGS <- ogsDistance(qwl$ogs, rl$ogs, normalize = TRUE, absDist = TRUE)  # vector
    simOGS <- sim2dist(dOGS)  # vector

    ## combine dimensions
    sim <- (simDensity * simOGS)[, 1]
  }

  ## DIFFERENT simSP APPROACHES:


  ## --- HerlaEtAl2021, simple2 ----
  if (simType %in% c("herlaetal2021", "simple2")) {
    ## separate further evaluation of similarity into grain type categories
    ## (both profiles are important i.e. aim at 'symmetric' similarity score)
    ## (1) unmatched layers get similarity 'indifferent' i.e. no penalty and no score
    if (is.data.frame(missingLayers) && nrow(missingLayers) > 0) {
      missingDF <- data.frame(grains = as.factor(as.character(missingLayers[, "gtype"])),  # update levels of grain types
                              sim = nonMatchedSim)
    } else {
      missingDF <- data.frame()
    }
    ## (2) matched layers:
    ## stack into data.frame, first refGrains, second qwGrains:
    matchedDF <- data.frame(grains = as.factor(c(as.character(refGrains), as.character(qwGrains))),
                            sim = sim)
    if (any(is.na(matchedDF$sim))) print("simSP: NAs produced in similarity assessment of matched layers. Investigate why!")

    ## combine similarities within the categories
    ## (missing layers get weight factor 2 b/c matched layers come in once for ref and once for qw)
    cat_wls <- c("SH", "DH")
    cat_cr <- c("MFcr", "IF")
    cat_pps <- c("PP", "DF")
    cat_special <- c(cat_wls, cat_cr, cat_pps)  # will be used exclusively later to get bulk grains

    ## eliminate hardness information for WL, MFcr:
    ## matchedDF is stacked with two repetitions of sim (see above)
    iNoHHI <- which(matchedDF$grains %in% c(cat_wls, cat_cr))
    iExc <- which(iNoHHI > nGrains)
    iNoHHI_trans <- iNoHHI
    iNoHHI_trans[iExc] <- iNoHHI[iExc] - nGrains  # translate indices
    matchedDF[iNoHHI, "sim"] <- simGT[iNoHHI_trans, 1]

    ## Calculate specifc weighting scheme for WL, based on how many WLs are apparent in the profiles:
    nWLs <- max(c(length(which(matchedDF$grains[1:nGrains] %in% cat_wls)),
                  length(which(matchedDF$grains[(nGrains+1):(2*nGrains)] %in% cat_wls))))
    if (nWLs > 1) {
      ## more than one WL:
      maxMatchedGrid <- max(matchedGrid)
      gridBoundsStep <- maxMatchedGrid/(nWLs+1)
      gridBounds <- seq(gridBoundsStep, maxMatchedGrid+1, length.out = nWLs)
      gridBounds <- matrix(c(0, gridBounds[1:(nWLs-1)], gridBounds), ncol = 2)

      iMatchedWLs <- c(which(matchedDF$grains %in% cat_wls))
      matchedWLs <- matrix(c(matchedGrid[iMatchedWLs], matchedDF[iMatchedWLs, "sim"]), ncol = 2)
      simWLs <- sapply(seq(nWLs), function(i) {
        mean(matchedWLs[which(matchedWLs[, 1] >= gridBounds[i, 1] & matchedWLs[, 1] < gridBounds[i, 2]), 2], na.rm = TRUE)
      })
    } else {
      ## one or none WL:
      simWLs <- mean(matchedDF[matchedDF$grains %in% cat_wls, "sim"])
    }
    ## Calculate specifc weighting scheme for MFcr, based on how many crusts are apparent in the profiles:
    nCRs <- max(c(length(which(matchedDF$grains[1:nGrains] %in% cat_cr)),
                  length(which(matchedDF$grains[(nGrains+1):(2*nGrains)] %in% cat_cr))))
    if (nCRs > 1) {
      ## more than one CR:
      maxMatchedGrid <- max(matchedGrid)
      gridBoundsStep <- maxMatchedGrid/(nCRs+1)
      gridBounds <- seq(gridBoundsStep, maxMatchedGrid+1, length.out = nCRs)
      gridBounds <- matrix(c(0, gridBounds[1:(nCRs-1)], gridBounds), ncol = 2)

      iMatchedCRs <- c(which(matchedDF$grains %in% cat_cr))
      matchedCRs <- matrix(c(matchedGrid[iMatchedCRs], matchedDF[iMatchedCRs, "sim"]), ncol = 2)
      simCRs <- sapply(seq(nCRs), function(i) {
        mean(matchedCRs[which(matchedCRs[, 1] >= gridBounds[i, 1] & matchedCRs[, 1] < gridBounds[i, 2]), 2], na.rm = TRUE)
      })
    } else {
      ## one or none CR:
      simCRs <- mean(matchedDF[matchedDF$grains %in% cat_cr, "sim"])
    }

    simDF <- data.frame(
      wl = mean(c(simWLs, suppressWarnings(mean(missingDF[missingDF$grains %in% cat_wls, "sim"]))),
                na.rm = TRUE),
      # fc = mean(c(matchedDF[matchedDF$grains %in% cat_fcs, "sim"],
      #             rep(missingDF[missingDF$grains %in% cat_fcs, "sim"], times = 2))),
      cr = mean(c(simCRs, suppressWarnings(mean(missingDF[missingDF$grains %in% cat_cr, "sim"]))),
                na.rm = TRUE),
      pp = mean(c(matchedDF[matchedDF$grains %in% cat_pps, "sim"],
                  rep(missingDF[missingDF$grains %in% cat_pps, "sim"], times = 1))),
      bulk = mean(c(matchedDF[!matchedDF$grains %in% cat_special, "sim"],
                    rep(missingDF[!missingDF$grains %in% cat_special, "sim"], times = 1)))
    )

    rownames(simDF) <- "sim [0, 1]: "
    ## combine grain type categories to simple similarity:
    simSP <- mean(as.double(simDF[1, ]), na.rm = TRUE)

    ## return NA in case of NULL/problem:
    if (length(simSP) == 0) {
      simSP <- NA
      warning("simSP: problem in calculating similarity score, returning NA")
    }

    simDF <- round((simDF)*100)/100
    if (verbose) {
      print(simDF)
      cat(paste("simple similarity =", round(simSP*1000)/1000, "\n"))
    }

    if (returnDF) return(list(sim = simSP, simDF = simDF))
    else return(simSP)
  }  # END IF simType == herlaetal2021, simple2



  ## --- Simple ----
  else if (simType %in% c("simple")) {
    ## separate further evaluation of similarity into grain type categories
    ## (both profiles are important i.e. aim at 'symmetric' similarity score)
    ## (1) unmatched layers get similarity 'nonMatchedSim'
    if (is.data.frame(missingLayers) && nrow(missingLayers) > 0) {
      missingDF <- data.frame(grains = as.factor(as.character(missingLayers[, "gtype"])),  # update levels of grain types
                              sim = nonMatchedSim)
    } else {
      missingDF <- data.frame()
    }
    ## (2) matched layers:
    ## stack into data.frame, first refGrains, second qwGrains:
    matchedDF <- data.frame(depth = c(rl$depth, qwl$depth),
                            grains = as.factor(c(as.character(refGrains), as.character(qwGrains))),
                            sim = sim)
    if (any(is.na(matchedDF$sim))) print("simSP: NAs produced in similarity assessment of matched layers. Investigate why!")

    ## combine similarities within the categories
    ## (missing layers get weight factor 2 b/c matched layers come in once for ref and once for qw)
    cat_wls <- c("SH", "DH")
    cat_cr <- c("MFcr", "IF")
    cat_pps <- c("PP", "DF")
    cat_special <- c(cat_wls, cat_cr, cat_pps)  # will be used exclusively later to get bulk grains

    ## eliminate hardness information for WL, MFcr:
    ## matchedDF is stacked with two repetitions of sim (see above)
    iNoHHI <- which(matchedDF$grains %in% c(cat_wls, cat_cr))
    iExc <- which(iNoHHI > nGrains)
    iNoHHI_trans <- iNoHHI
    iNoHHI_trans[iExc] <- iNoHHI[iExc] - nGrains  # translate indices
    matchedDF[iNoHHI, "sim"] <- simGT[iNoHHI_trans, 1]

    ## Calculate specifc weighting scheme for WL, based on heuristic depth ranges:
    maxMatchedGrid <- max(matchedGrid)
    gridBounds <- c(0, 30, 80, 150, maxMatchedGrid)
    gridBounds <- sort(unique(gridBounds[gridBounds <= maxMatchedGrid]))

    iMatchedWLs <- which(matchedDF$grains %in% cat_wls)
    matchedWLs <- matchedDF[iMatchedWLs, c("depth", "sim")]
    simWLs <- sapply(seq(length(gridBounds)-1), function(i) {
      mean(matchedWLs$sim[matchedWLs$depth >= gridBounds[i] & matchedWLs$depth < gridBounds[i+1]], na.rm = TRUE)
    })

    ## Calculate specifc weighting scheme for MFcr, based on heuristic depth ranges:
    iMatchedCRs <- which(matchedDF$grains %in% cat_cr)
    matchedCRs <- matchedDF[iMatchedCRs, c("depth", "sim")]
    simCRs <- sapply(seq(length(gridBounds)-1), function(i) {
      mean(matchedCRs$sim[matchedCRs$depth >= gridBounds[i] & matchedCRs$depth < gridBounds[i+1]], na.rm = TRUE)
    })

    simDF <- data.frame(
      wl = mean(c(simWLs, suppressWarnings(mean(missingDF[missingDF$grains %in% c(cat_wls), "sim"]))),
                na.rm = TRUE),
      cr = mean(c(simCRs, suppressWarnings(mean(missingDF[missingDF$grains %in% cat_cr, "sim"]))),
                na.rm = TRUE),
      pp = mean(c(matchedDF[matchedDF$grains %in% cat_pps, "sim"],
                  rep(missingDF[missingDF$grains %in% cat_pps, "sim"], times = 1))),
      bulk = mean(c(matchedDF$sim[!matchedDF$grains %in% cat_special],
                    rep(missingDF$sim[!missingDF$grains %in% cat_special], times = 1)))
    )

    rownames(simDF) <- "sim [0, 1]: "
    ## combine grain simType categories to simple similarity:
    simSP <- mean(as.double(simDF[1, ]), na.rm = TRUE)

    ## return NA in case of NULL/problem:
    if (length(simSP) == 0) {
      simSP <- NA
      warning("simSP: problem in calculating similarity score, returning NA")
    }

    simDF <- round((simDF)*100)/100
    if (verbose) {
      print(simDF)
      cat(paste("simple similarity =", round(simSP*1000)/1000, "\n"))
    }

    if (returnDF) return(list(sim = simSP, simDF = simDF))
    else return(simSP)
  }  # END IF simType == simple



  ## --- TSA_WLdetection ----
  else if (simType %in% c("tsa_wldetection")) {
    ## separate further evaluation of similarity into grain type categories
    ## (both profiles are important i.e. aim at 'symmetric' similarity score)
    ## (1) unmatched layers get similarity 'nonMatchedSim'
    if (is.data.frame(missingLayers) && nrow(missingLayers) > 0) {
      missingDF <- data.frame(grains = as.factor(as.character(missingLayers[, "gtype"])),  # update levels of grain types
                              tsa = missingLayers[, "tsa"], sim = nonMatchedSim)
    } else {
      missingDF <- data.frame()
    }
    ## (2) matched layers:
    ## stack into data.frame, first refGrains, second qwGrains:
    matchedDF <- data.frame(depth = c(rl$depth, qwl$depth),
                            grains = as.factor(c(as.character(refGrains), as.character(qwGrains))),
                            tsa = c(rl$tsa, qwl$tsa), sim = sim)
    if (any(is.na(matchedDF$sim))) print("simSP: NAs produced in similarity assessment of matched layers. Investigate why!")

    ## combine similarities within the categories
    ## (missing layers get weight factor 2 b/c matched layers come in once for ref and once for qw)
    cat_wls <- c("SH", "DH")
    cat_fcs <- c("FC", "FCxr")
    cat_cr <- c("MFcr", "IF")
    cat_pps <- c("PP", "DF")
    cat_special <- c(cat_cr, cat_pps)  # will be used exclusively later to get bulk grains

    ## Calculate specifc weighting scheme for WL, based on heuristic depth ranges:
    maxMatchedGrid <- max(matchedGrid)
    gridBounds <- c(0, 30, 80, 150, maxMatchedGrid)
    gridBounds <- sort(unique(gridBounds[gridBounds <= maxMatchedGrid]))

    iMatchedWLs <- which(matchedDF$grains %in% c(cat_wls, cat_fcs) & matchedDF$tsa >= 5)
    matchedWLs <- matchedDF[iMatchedWLs, c("depth", "sim")]
    simWLs <- sapply(seq(length(gridBounds)-1), function(i) {
      mean(matchedWLs$sim[matchedWLs$depth >= gridBounds[i] & matchedWLs$depth < gridBounds[i+1]], na.rm = TRUE)
    })

    ## Calculate specifc weighting scheme for MFcr, based on heuristic depth ranges:
    iMatchedCRs <- which(matchedDF$grains %in% cat_cr)
    matchedCRs <- matchedDF[iMatchedCRs, c("depth", "sim")]
    simCRs <- sapply(seq(length(gridBounds)-1), function(i) {
      mean(matchedCRs$sim[matchedCRs$depth >= gridBounds[i] & matchedCRs$depth < gridBounds[i+1]], na.rm = TRUE)
    })

    simDF <- data.frame(
      wl = mean(c(simWLs, suppressWarnings(mean(missingDF[missingDF$grains %in% c(cat_wls, cat_fcs) & missingDF$tsa >= 5, "sim"]))),
                na.rm = TRUE),
      cr = mean(c(simCRs, suppressWarnings(mean(missingDF[missingDF$grains %in% cat_cr, "sim"]))),
                na.rm = TRUE),
      pp = mean(c(matchedDF[matchedDF$grains %in% cat_pps, "sim"],
                  rep(missingDF[missingDF$grains %in% cat_pps, "sim"], times = 1))),
      bulk = mean(c(matchedDF$sim[!matchedDF$grains %in% cat_special & !(matchedDF$grains %in% c(cat_wls, cat_fcs) & matchedDF$tsa >= 5)],
                    rep(missingDF$sim[!missingDF$grains %in% cat_special & !(missingDF$grains %in% c(cat_wls, cat_fcs) & missingDF$tsa >= 5)], times = 1)))
    )

    rownames(simDF) <- "sim [0, 1]: "
    ## combine grain type categories to simple similarity:
    simSP <- mean(as.double(simDF[1, ]), na.rm = TRUE)

    ## return NA in case of NULL/problem:
    if (length(simSP) == 0) {
      simSP <- NA
      warning("simSP: problem in calculating similarity score, returning NA")
    }

    simDF <- round((simDF)*100)/100
    if (verbose) {
      print(simDF)
      cat(paste("similarity =", round(simSP*1000)/1000, "\n"))
    }

    if (returnDF) return(list(sim = simSP, simDF = simDF))
    else return(simSP)
  }  # END IF simType == tsa_WLdtection


  ## --- RTA_WLdetection ----
  else if (simType %in% c("rta_wldetection")) {
    ## separate further evaluation of similarity into grain type categories
    ## (both profiles are important i.e. aim at 'symmetric' similarity score)
    ## (1) unmatched layers get similarity 'nonMatchedSim'
    if (is.data.frame(missingLayers) && nrow(missingLayers) > 0) {
      missingDF <- data.frame(grains = as.factor(as.character(missingLayers[, "gtype"])),  # update levels of grain types
                              rta = missingLayers[, "rta"], sim = nonMatchedSim)
    } else {
      missingDF <- data.frame()
    }
    ## (2) matched layers:
    ## stack into data.frame, first refGrains, second qwGrains:
    matchedDF <- data.frame(depth = c(rl$depth, qwl$depth),
                            grains = as.factor(c(as.character(refGrains), as.character(qwGrains))),
                            rta = c(rl$rta, qwl$rta), sim = sim)
    if (any(is.na(matchedDF$sim))) print("simSP: NAs produced in similarity assessment of matched layers. Investigate why!")

    ## combine similarities within the categories
    ## (missing layers get weight factor 2 b/c matched layers come in once for ref and once for qw)
    cat_wls <- c("SH", "DH")
    cat_fcs <- c("FC", "FCxr")
    cat_cr <- c("MFcr", "IF")
    cat_pps <- c("PP", "DF")
    cat_special <- c(cat_cr, cat_pps)  # will be used exclusively later to get bulk grains

    ## Calculate specifc weighting scheme for WL, based on heuristic depth ranges:
    maxMatchedGrid <- max(matchedGrid)
    gridBounds <- c(0, 30, 80, 150, maxMatchedGrid)
    gridBounds <- sort(unique(gridBounds[gridBounds <= maxMatchedGrid]))

    iMatchedWLs <- which(matchedDF$grains %in% c(cat_wls, cat_fcs) & matchedDF$rta >= 0.8)
    matchedWLs <- matchedDF[iMatchedWLs, c("depth", "sim")]
    simWLs <- sapply(seq(length(gridBounds)-1), function(i) {
      mean(matchedWLs$sim[matchedWLs$depth >= gridBounds[i] & matchedWLs$depth < gridBounds[i+1]], na.rm = TRUE)
    })

    ## Calculate specifc weighting scheme for MFcr, based on heuristic depth ranges:
    iMatchedCRs <- which(matchedDF$grains %in% cat_cr)
    matchedCRs <- matchedDF[iMatchedCRs, c("depth", "sim")]
    simCRs <- sapply(seq(length(gridBounds)-1), function(i) {
      mean(matchedCRs$sim[matchedCRs$depth >= gridBounds[i] & matchedCRs$depth < gridBounds[i+1]], na.rm = TRUE)
    })

    simDF <- data.frame(
      wl = mean(c(simWLs, suppressWarnings(mean(missingDF[missingDF$grains %in% c(cat_wls, cat_fcs) & missingDF$rta >= 0.8, "sim"]))),
                na.rm = TRUE),
      cr = mean(c(simCRs, suppressWarnings(mean(missingDF[missingDF$grains %in% cat_cr, "sim"]))),
                na.rm = TRUE),
      pp = mean(c(matchedDF[matchedDF$grains %in% cat_pps, "sim"],
                  rep(missingDF[missingDF$grains %in% cat_pps, "sim"], times = 1))),
      bulk = mean(c(matchedDF$sim[!matchedDF$grains %in% cat_special & !(matchedDF$grains %in% c(cat_wls, cat_fcs) & matchedDF$rta >= 0.8)],
                    rep(missingDF$sim[!missingDF$grains %in% cat_special & !(missingDF$grains %in% c(cat_wls, cat_fcs) & missingDF$rta >= 0.8)], times = 1)))
    )

    rownames(simDF) <- "sim [0, 1]: "
    ## combine grain type categories to simple similarity:
    simSP <- mean(as.double(simDF[1, ]), na.rm = TRUE)

    ## return NA in case of NULL/problem:
    if (length(simSP) == 0) {
      simSP <- NA
      warning("simSP: problem in calculating similarity score, returning NA")
    }

    simDF <- round((simDF)*100)/100
    if (verbose) {
      print(simDF)
      cat(paste("similarity =", round(simSP*1000)/1000, "\n"))
    }

    if (returnDF) return(list(sim = simSP, simDF = simDF))
    else return(simSP)
  }  # END IF simType == rta_WLdtection



  ## --- Layerwise ----
  else if (simType %in% c("layerwise", "remotesensing")) {
    # remotesensing is included in the if clause because it is a layerwise alignment but with a different input generic sim
    ## create a data.frame with columns sim and (if desired) scalingFactor
    if (apply_scalingFactor){
      if (is.data.frame(missingLayers) && nrow(missingLayers) > 0) {
        layerDF <- data.frame(scalingFactor = c(rl$scalingFactor, qwl$scalingFactor, missingLayers$scalingFactor),
                              sim = c(sim, sim, rep(nonMatchedSim, times = nrow(missingLayers))))
      } else {
        layerDF <- data.frame(scalingFactor = c(rl$scalingFactor, qwl$scalingFactor),
                              sim = c(sim, sim))
      }
    } else {
      if (is.data.frame(missingLayers) && nrow(missingLayers) > 0) {
        layerDF <- data.frame(sim = c(sim, sim, rep(nonMatchedSim, times = nrow(missingLayers))))
      } else {
        layerDF <- data.frame(sim = c(sim, sim))
      }
    }
    if (any(is.na(layerDF$sim))) print("simSP: NAs produced in similarity assessment of profiles. Investigate why!")

    ## scale sim with scalingFactor if desired and provided
    if (apply_scalingFactor) simSP <- sum(layerDF$sim * layerDF$scalingFactor, na.rm = TRUE) / sum(layerDF$scalingFactor, na.rm = TRUE)
    else simSP <- sum(layerDF$sim) / nrow(layerDF)


    if (verbose) {
      if (apply_scalingFactor) print(paste0("custom scaled similarity = sum(sim * scalingFactor) / sum(scalingFactor) = ", round(simSP, digits = 3)))
      else print(paste0("layerwise (unscaled) similarity = sum(sim) / length(sim) = ", round(simSP, digits = 3)))
    }

    if (returnDF) return(list(sim = simSP, simDF = layerDF))
    else return(simSP)
  }  # END IF simType == layerwise



  ## --- RTA_Scaling ----
  else if (simType %in% c("rta_scaling")) {
    ## create a data.frame with sim and rta columns
    if (is.data.frame(missingLayers) && nrow(missingLayers) > 0) {
      layerDF <- data.frame(rta = c(rl$rta, qwl$rta, missingLayers$rta),
                            sim = c(sim, sim, rep(nonMatchedSim, times = nrow(missingLayers))))
    } else {
      layerDF <- data.frame(rta = c(rl$rta, qwl$rta),
                            sim = c(sim, sim))
    }
    if (any(is.na(layerDF$sim))) print("simSP: NAs produced in similarity assessment of profiles. Investigate why!")

    ## scale sim with rta
    simSP <- sum(layerDF$sim * layerDF$rta) / sum(layerDF$rta)

    if (verbose) {
      print(paste0("rta-scaled similarity = sum(sim * scalingFactor) / sum(scalingFactor) = ", round(simSP, digits = 3)))
    }

    if (returnDF) return(list(sim = simSP, simDF = layerDF))
    else return(simSP)
  }  # END IF simType == rta_scaling



  ## --- wsum_scaled ----
  else if (simType %in% c("wsum_scaled")) {
    ## create a data.frame with columns sim of each dim and (if desired) scalingFactor
    if ("scalingFactor" %in% relevantProperties) {
      if (!"scalingFactor" %in% intersect(names(rl), names(qwl))) stop("scalingFactor not avaiable in provided profile layers for wsum_scaled in simSP!")
      simSF <- 1 - (abs(rl$scalingFactor - qwl$scalingFactor))
      if (is.data.frame(missingLayers) && nrow(missingLayers) > 0) {
        layerDF <- data.frame(scalingFactor = c(rl$scalingFactor, qwl$scalingFactor, missingLayers$scalingFactor),
                              simHHI = c(simHHI$SimMat, simHHI$SimMat, rep(nonMatchedSim, times = nrow(missingLayers))),
                              simGT = c(simGT$SimMat, simGT$SimMat, rep(nonMatchedSim, times = nrow(missingLayers))),
                              simSF = c(simSF, simSF, rep(nonMatchedSim, times = nrow(missingLayers))))
      } else {
        layerDF <- data.frame(scalingFactor = c(rl$scalingFactor, qwl$scalingFactor),
                              simHHI = c(simHHI$SimMat, simHHI$SimMat),
                              simGT = c(simGT$SimMat, simGT$SimMat),
                              simSF = c(simSF, simSF))
      }
    } else if ("p_unstable" %in% relevantProperties) {
      simSF <- 1 - (abs(rl$p_unstable - qwl$p_unstable))
      ## hardcode NA p_unstable values to be not NA
      ## (somewhat dangerous if entire p_unstable column happens to be accidentally NA, but necessary to avoid NA similarities in layers. compute simSF before so that only scalingFactor is modified.)
      rl$p_unstable[is.na(rl$p_unstable)] <- 0.2
      qwl$p_unstable[is.na(qwl$p_unstable)] <- 0.2
      if (is.data.frame(missingLayers) && nrow(missingLayers) > 0) {
        layerDF <- data.frame(scalingFactor = c(rl$p_unstable, qwl$p_unstable, missingLayers$p_unstable),
                              simHHI = c(simHHI$SimMat, simHHI$SimMat, rep(nonMatchedSim, times = nrow(missingLayers))),
                              simGT = c(simGT$SimMat, simGT$SimMat, rep(nonMatchedSim, times = nrow(missingLayers))),
                              simSF = c(simSF, simSF, rep(nonMatchedSim, times = nrow(missingLayers))))
      } else {
        layerDF <- data.frame(scalingFactor = c(rl$p_unstable, qwl$p_unstable),
                              simHHI = c(simHHI$SimMat, simHHI$SimMat),
                              simGT = c(simGT$SimMat, simGT$SimMat),
                              simSF = c(simSF, simSF))
      }
    } else {
      stop("p_unstable is not available in the provided profile layers for wsum_scaled approach in simSP. Either compute it, or use a custom scalingFactor!")
    }

    ## compute weighted sum of sim
    layerDF$sim <- layerDF$simGT * simWeights["gtype"] + layerDF$simHHI * simWeights["hardness"] + layerDF$simSF * simWeights["stability"]
    layerDF$sim[is.na(layerDF$sim)] <- rowMeans(layerDF[is.na(layerDF$sim), c("simHHI", "simGT", "simSF")], na.rm = TRUE)  # line above will yield NA sometimes; recompute those by just using standard average..

    if (any(is.na(layerDF$sim))) print("simSP: NAs produced in similarity assessment of profiles. Investigate why!")

    ## scale sim with scalingFactor
    simSP <- sum(layerDF$sim * layerDF$scalingFactor, na.rm = TRUE) / sum(layerDF$scalingFactor, na.rm = TRUE)


    if (verbose) {
      if (apply_scalingFactor) {
        print(paste0("custom-scaled similarity = sum(sim * scalingFactor) / sum(scalingFactor) = ", round(simSP, digits = 3)))
      } else {
        print(paste0("p_unstable-scaled similarity = sum(sim * p_unstable) / sum(p_unstable) = ", round(simSP, digits = 3)))
      }
    }

    if (returnDF) return(list(sim = simSP, simDF = layerDF))
    else return(simSP)
  }  # END IF simType == wsum_scaled



  ## --- wsum ----
  else if (simType %in% c("wsum")) {
    ## create a data.frame with columns sim of each dim
      if (is.data.frame(missingLayers) && nrow(missingLayers) > 0) {
        layerDF <- data.frame(simHHI = c(simHHI$SimMat, simHHI$SimMat, rep(nonMatchedSim, times = nrow(missingLayers))),
                              simGT = c(simGT$SimMat, simGT$SimMat, rep(nonMatchedSim, times = nrow(missingLayers))))
      } else {
        layerDF <- data.frame(simHHI = c(simHHI$SimMat, simHHI$SimMat),
                              simGT = c(simGT$SimMat, simGT$SimMat))
      }
    ## compute weighted sum of sim
    layerDF$sim <- layerDF$simGT * simWeights["gtype"] + layerDF$sim_HHI * simWeights["hardness"]
    layerDF$sim[is.na(layerDF$sim)] <- rowMeans(layerDF[is.na(layerDF$sim), c("simHHI", "simGT")], na.rm = TRUE)  # line above will yield NA sometimes; recompute those by just using standard average..

    if (any(is.na(layerDF$sim))) print("simSP: NAs produced in similarity assessment of profiles. Investigate why!")

    simSP <- sum(layerDF$sim) / nrow(layerDF)


    if (verbose) {
      print(paste0("unscaled similarity = sum(sim) / length(sim) = ", round(simSP, digits = 3)))
    }

    if (returnDF) return(list(sim = simSP, simDF = layerDF))
    else return(simSP)
  }  # END IF simType == wsum


}
