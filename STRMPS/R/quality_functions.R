#' @name phredQualityScore
#' @aliases solexaQualityScore
#'
#' @title Convert probability to quality score
#'
#' @description Calculates the quality score (Phred or Solexa) given a probability of error.
#'
#' @param p Probability of error.
#'
#' @rdname qualityScore
#' @return \code{phredQualityScore(p)} returns a Phred quality score.
#' @example inst/examples/qualityScores.R
phredQualityScore <- function(p) {
    q_p <- -10*log10(p)
    return(q_p)
}

#' @rdname qualityScore
#' @return \code{solexaQualityScore(p)} returns a Solexa quality score.
solexaQualityScore <- function(p) {
    q_s <- -10 * log10( p / (1 - p))
    return(q_s)
}

#' @name phredQualityProbability
#' @aliases solexaQualityProbability
#'
#' @title Quality score to probability
#'
#' @description Converts a quality score (Phred or Solexa) to a probability of error.
#'
#' @param q Quality score.
#'
#' @rdname probabilityOfError
#' @return \code{phredQualityScore(q_phred)} and \code{solexaQualityScore(q_solexa)} returns a probability of error.
#' @example inst/examples/probabilityOfError.R
phredQualityProbability <- function(q) {
    p <- 10^(-q/10)
    return(p)
}

#' @rdname probabilityOfError
solexaQualityProbability <- function(q) {
    p <- 1 / (10^(q / 10) + 1)
    return(p)
}

.geometricMean <- function(x) {
    return(exp(1/length(x) * sum(log(x))))
}

.aggregateQuality <- function(q) {
    qS <- as.matrix(PhredQuality(as.character(q)))
    qAvg <- PhredQuality(apply(qS, 2, function(x) .geometricMean(phredQualityProbability(x))))
    cAvg <- as.character(qAvg)

    if (length(cAvg) == 0)
        cAvg = ""

    return(cAvg)
}

.createRanking <- function(x) {
    unique_x <- unique(x)
    rank = c()
    coverage = c()
    for (i in seq_along(unique_x)) {
        rank = c(rank, rep(i, length(which(x == unique_x[i]))))
        coverage = c(coverage, rep(length(which(x == unique_x[i])), length(which(x == unique_x[i]))))
    }

    final <- data.frame(Rank = rank, Coverage = coverage)
    return(final)
}

.baseErrorProbabilityConditinal <- function(quality_variant, matching_bases, windowSize = 5) {
    probability_error_variant <- phredQualityProbability(c(as(PhredQuality(quality_variant), "matrix")))

    if (length(probability_error_variant) == 0)
        return(0)

    leftmost_neighbour <- ifelse(seq(1, length(probability_error_variant)) - windowSize < 1, 1, seq(1, length(probability_error_variant)) - windowSize)
    rightmost_neighbour <- ifelse(seq(1, length(probability_error_variant)) + windowSize > length(probability_error_variant), length(probability_error_variant), seq(1, length(probability_error_variant)) + windowSize)
    base_probability_error_variant <- rep(NA, length(probability_error_variant))

    for (i in 1:length(base_probability_error_variant)) {
        weighted_base_probability_variant <- probability_error_variant / (abs(1:length(probability_error_variant) - i) + 1)

        base_probability_error_variant[i] <- max(weighted_base_probability_variant[leftmost_neighbour[i]:rightmost_neighbour[i]]) #/ sum(weighted_base_probability_variant)
    }

    conditional_probability <- exp(sum(log(base_probability_error_variant[-matching_bases])) + sum(log(1 - base_probability_error_variant[matching_bases])))
    return(conditional_probability)
}

.baseErrorProbabilityMarginal <- function(quality_variant, windowSize = 5) {
    probability_error_variant <- phredQualityProbability(c(as(PhredQuality(quality_variant), "matrix")))

    if (length(probability_error_variant) == 0)
        return(0)

    leftmost_neighbour <- ifelse(seq(1, length(probability_error_variant)) - windowSize < 1, 1, seq(1, length(probability_error_variant)) - windowSize)
    rightmost_neighbour <- ifelse(seq(1, length(probability_error_variant)) + windowSize > length(probability_error_variant), length(probability_error_variant), seq(1, length(probability_error_variant)) + windowSize)

    base_probability_error_variant <- rep(NA, length(probability_error_variant))
    for (i in 1:length(base_probability_error_variant)) {
        weighted_base_probability_variant <- probability_error_variant / (abs(1:length(probability_error_variant) - i) + 1)

        base_probability_error_variant[i] <- max(weighted_base_probability_variant[leftmost_neighbour[i]:rightmost_neighbour[i]]) #/ sum(weighted_base_probability_variant)
    }

    marginal_probability <- exp(sum(log(1 - base_probability_error_variant)))
    return(marginal_probability)
}

.sampleQualityCleaning <- function(stringCoverageList, variantDatabase, trace = FALSE) {
    class(stringCoverageList) <- "list"

    sampleTibbleBind <- bind_rows(stringCoverageList)
    sampleTibble <- sampleTibbleBind %>%
        left_join(variantDatabase %>% ungroup() %>% mutate(Observed = 1) %>% select(Marker, Type, Region, Observed),
                  by = c("Marker", "Type", "Region")) %>%
        mutate(Observed = ifelse(is.na(Observed), 0, Observed))
    markers <- unique(sampleTibble$Marker)

    sampleTibbleCleaned_m <- vector("list", length(markers))
    for (mm in seq_along(markers)) {
        if (trace)
            cat("\tMarker:", mm, "/", length(markers), "\n")

        sampleTibble_m <- sampleTibble %>% filter(Marker == markers[mm]) %>%
            mutate(BasePairs = nchar(Region))

        sampleTibble_m_split <- split(sampleTibble_m, sampleTibble_m$Allele)
        sampleTibble_m_cleaned <- bind_rows(lapply(seq_along(sampleTibble_m_split), function(a) {
            sampleTibble_ma <- sampleTibble_m_split[[a]]
            observed <- sampleTibble_ma$Observed
            regions <- sampleTibble_ma$Region
            true_regions_index <- which(observed == 1)

            true_regions <- regions[observed == 1]
            varient_regions <- regions[observed == 0]

            qualities <- sampleTibble_ma$AggregateQuality
            na_regions <- qualities == ""

            probability_error_variant <- rep(1, length(qualities))

            if (sum(!na_regions) > 0) {
                probability_error_variant[!na_regions] <- unname(sapply(qualities[!na_regions], .baseErrorProbabilityMarginal)) *
                    sampleTibble_ma$Coverage[!na_regions] / sum(sampleTibble_m$Coverage)
            }

            if (length(true_regions) > 0) {
                all_combinations <- expand.grid(seq(1, length(true_regions)), seq(1, length(regions)))

                weight_matrix <- matrix(0, ncol = length(regions), nrow = length(true_regions))
                for (j in 1:dim(all_combinations)[1]) {
                    s_1 <- true_regions[all_combinations[j, 1]]
                    s_2 <- regions[all_combinations[j, 2]]

                    if (s_1 == s_2) {
                        if (is.na(qualities[all_combinations[j, 1]]) & is.na(qualities[all_combinations[j, 2]])) {
                            weight_matrix[all_combinations[j, 1], all_combinations[j, 2]] <- 1
                        }
                        else if (is.na(qualities[all_combinations[j, 1]]) | is.na(qualities[all_combinations[j, 2]])) {
                            weight_matrix[all_combinations[j, 1], all_combinations[j, 2]] <- 0
                        }
                        else {
                            weight_matrix[all_combinations[j, 1], all_combinations[j, 2]] <- 1
                        }
                    }
                    else {
                        if (is.na(qualities[all_combinations[j, 1]]) | is.na(qualities[all_combinations[j, 2]])) {
                            weight_matrix[all_combinations[j, 1], all_combinations[j, 2]] <- 0
                        }
                        else {
                            strings_split <- str_split(c(s_1, s_2), "")

                            positions_equal <- which(strings_split[[1]] == strings_split[[2]])

                            probability_of_error <- .baseErrorProbabilityConditinal(qualities[all_combinations[j, 2]], positions_equal)
                            weight_matrix[all_combinations[j, 1], all_combinations[j, 2]] <- probability_of_error
                        }
                    }
                }

                weight_matrix_extended <- rbind(weight_matrix, diag(probability_error_variant, ncol = length(probability_error_variant), nrow = length(probability_error_variant))[observed == 0, ])
                which_max <- apply(weight_matrix_extended, 2, which.max)
                alternative_region <- c(true_regions, varient_regions)[which_max]
            }
            else {
                weight_matrix_extended <- diag(probability_error_variant, nrow = length(probability_error_variant), ncol = length(probability_error_variant))
                which_max <- apply(weight_matrix_extended, 2, which.max)
                alternative_region <- varient_regions[which_max]
            }

            res <- sampleTibble_ma %>% mutate(AlternativeRegion = alternative_region)
            return(res)
        })) %>%
            group_by(Marker, Type, Allele, MotifLength, AlternativeRegion) %>%
            summarise(Coverage = sum(Coverage)) %>% ungroup() %>%
            rename("Region" = "AlternativeRegion") %>%
            filter(!is.na(Region))

        sampleTibbleCleaned_m[[mm]] <- sampleTibble_m_cleaned
    }

    sampleTibbleCleaned <- bind_rows(sampleTibbleCleaned_m)
    return(sampleTibbleCleaned)
}
