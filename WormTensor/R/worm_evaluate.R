#' Evaluates clustering result
#' An evaluation result is generated from a WormTensor object.
#' @param object WormTensor object with a result of worm_clustering
#' @param labels Labels for external evaluation
#' @return WormTensor object with an evaluation result added
#' @examples
#' \donttest{
#'     # Pipe Operation
#'     worm_download("mSBD", qc = "PASS")$Ds |>
#'         as_worm_tensor() |>
#'             worm_membership(k = 6) |>
#'                 worm_clustering() -> object
#'      # Internal evaluation
#'      worm_evaluate(object) -> object_internal
#'
#'      # External evaluation by sample labels
#'      labels <- list(
#'          label1 = sample(3, length(object@clustering), replace = TRUE),
#'          label2 = sample(4, length(object@clustering), replace = TRUE),
#'          label3 = sample(5, length(object@clustering), replace = TRUE)
#'      )
#'      worm_evaluate(object, labels) -> object_external
#'
#'      # External evaluation by worm_download labels
#'      Ds_mSBD <- worm_download("mSBD", qc = "PASS")
#'      labels <- list(
#'          label1 = replace(
#'              Ds_mSBD$labels$Class,
#'              which(is.na(Ds_mSBD$labels$Class)),
#'              "NA"
#'          ),
#'          label2 = sample(4, length(object@clustering), replace = TRUE),
#'          label3 = sample(5, length(object@clustering), replace = TRUE)
#'      )
#'      worm_evaluate(object, labels) -> object_external_Class
#' }
#' @importFrom clusterSim index.G1
#' @importFrom clValid connectivity
#' @importFrom aricode ARI
#' @importFrom cluster silhouette
#' @importFrom stats as.dist
#' @importFrom stats cutree
#' @importFrom stats dist
#' @importFrom stats hclust
#' @importFrom stats na.omit
#' @export
setMethod(
    "worm_evaluate", "WormTensor",
    function(object, labels) {
        # Argument Check
        .check_worm_evaluate(object, labels)

        cluster <- object@clustering
        if (object@clustering_algorithm %in% c("MCMI", "OINDSCAL")) {
            data <- object@factor
        }
        if (object@clustering_algorithm == "CSPA") {
            data <- 1 - object@consensus
        }
        # Clustering results for each animals
        Cs <- lapply(object@dist_matrices,
            function(d, k) {
                d |>
                    hclust(method = "ward.D2") |>
                    cutree(k)
            },
            k = object@k
        )
        # Dist object for silhouette, connectivity
        if (object@clustering_algorithm %in% c("MCMI", "OINDSCAL")) {
            cls_dist <- dist(data)
        }
        if (object@clustering_algorithm == "CSPA") {
            cls_dist <- as.dist(data)
        }

        # Cellwise
        if (!is.null(labels)) {
            consistency <- .consistency(object, labels, Cs)
        } else {
            consistency <- NULL
        }
        no_identified <- .no_identified(object)
        silhouette <- silhouette(cluster, cls_dist)
        cellwise <- list(
            consistency = consistency,
            no_identified = no_identified,
            silhouette = silhouette
        )

        # Internal Validity Indices
        # Silhouette_Ave. one animal
        sil_ave <- mean(cellwise$silhouette[, 3])
        psf <- .pseudoF(data, cluster)
        cty <- .connectivity(cls_dist, cluster)
        int_out <- list(PseudoF = psf, Connectivity = cty, silhouette = sil_ave)

        # External Validity Indices &Labels
        if (!is.null(labels)) {
            ext_out <- lapply(labels, function(l) {
                list(
                    Fmeasure = .fmeasure(cluster, l),
                    Entropy = .entropy(cluster, l),
                    Purity = .purity(cluster, l),
                    ARI = ARI(cluster, l)
                )
            })
            names(ext_out) <- names(labels)
            # Add labels
            ext_labels <- labels
        } else {
            ext_out <- list(Fmeasure = NULL, Entropy = NULL, Purity = NULL)
            # Add labels
            ext_labels <- NULL
        }

        # Each_animal Clustering Similarities
        if (object@clustering_algorithm == "MCMI") {
            # Preparation dataframe for ARI calculation
            lapply(Cs, function(x) {
                data.frame(
                    CellType = attr(x, "names"),
                    Clusters = as.numeric(x),
                    stringsAsFactors = FALSE,
                    row.names = NULL
                )
            }) -> df_cls_list
            data.frame(
                CellType = object@union_cellnames,
                Classes = object@clustering,
                stringsAsFactors = FALSE,
                row.names = NULL
            ) -> df_merged_cls
            lapply(df_cls_list, function(x) {
                merge(x,
                    df_merged_cls,
                    by.x = "CellType",
                    by.y = "CellType",
                    all.y = TRUE
                ) -> df_cls_label_NA
                na.omit(df_cls_label_NA)
            }) -> df_cls_label
            # Calculation of ARI
            lapply(df_cls_label, function(x) {
                clusters <- x$Clusters
                classes <- x$Classes
                ARI(clusters, classes)
            }) |>
                unlist() |>
                as.numeric() -> ARI_value
            # Annotated count
            lapply(object@dist_matrices, function(x) {
                attr(x, "Size")
            }) |>
                unlist() |>
                as.numeric() -> annotated_count
            # Each animal object
            df_eval_animal <- data.frame(
                animals = names(object@dist_matrices),
                weight = object@weight,
                ARI = ARI_value,
                ann_count = annotated_count,
                stringsAsFactors = FALSE
            )
        } else {
            # case CSPA or OINDSCAL
            df_eval_animal <- NULL
        }
        # Ouput
        out <- list(
            internal = int_out,
            external = ext_out,
            cellwise = cellwise,
            external_label = ext_labels, # add labels
            each_animal = df_eval_animal # add df
        )
        object@eval <- out
        object
    }
)

.check_worm_evaluate <- function(object, labels) {
    # Backword Check
    if (prod(object@clustering) == 1) {
        msg <- paste0("Perform worm_clustering() first.")
        message(msg)
    }
    # Argument Check
    if (!is.null(labels)) {
        if (!is.list(labels)) {
            stop("Specify labels as a list.")
        }
    }
}

######### Internal Validity Indices (w/o Labels) #########
.pseudoF <- function(data, cluster) {
    index.G1(data, cluster)
}

.connectivity <- function(cls_dist, cluster) {
    connectivity(cls_dist, cluster)
}
######### External Validity Indices (w Labels) #########
.fmeasure <- function(cluster, label) {
    ctbl <- table(cluster, label)
    # All combination of Recall
    R <- ctbl / colSums(ctbl)
    # All combination of Precision
    P <- ctbl / rowSums(ctbl)
    # All combination of F-measure
    F <- 2 * R * P / (R + P)
    # NaN => 0
    F[is.nan(F)] <- 0
    # Weight
    w <- apply(ctbl, 2, sum) / sum(ctbl)
    # Total Micro-averaged F value
    sum(w * apply(F, 2, max))
}

.entropy <- function(cluster, label) {
    # Cross tabulation
    ctbl <- table(cluster, label)
    # Weight
    w <- apply(ctbl, 1, sum) / sum(ctbl)
    # Total Entropy
    sum(w * apply(ctbl, 1, .calcEntropy0))
}

.calcEntropy0 <- function(pv) {
    p1 <- pv / sum(pv)
    p2 <- p1[p1 != 0]
    # Entropy each column of ctbl
    -sum(p2 * log2(p2))
}

.purity <- function(cluster, label) {
    # Cross tabulation
    ctbl <- table(cluster, label)
    # Weight
    w <- apply(ctbl, 1, sum) / sum(ctbl)
    # Purity
    sum(w * apply(ctbl, 1, max) / rowSums(ctbl))
}
######### consistency #########
.consistency <- function(object, labels, Cs) {
    consistency_l <- lapply(labels, function(l) {
        df_eval_label <- data.frame(
            CellType = object@union_cellnames,
            Classes = l,
            stringsAsFactors = FALSE
        )

        df_count_union_list <- lapply(Cs, function(cs) {
            df_cls <- data.frame(
                CellType = names(cs),
                Cluster = cs,
                stringsAsFactors = FALSE,
                row.names = NULL
            )
            df_cls_label <- merge(df_cls,
                df_eval_label,
                by.x = "CellType",
                by.y = "CellType",
                all.x = TRUE
            )
            label <- df_cls_label$Classes
            cluster <- df_cls_label$Cluster
            H_label <- .H(label)
            H_cluster <- .H(cluster)

            other_member <- .other_member(H_label)

            # count
            count1 <- rep(0, length = length(label))
            for (i in seq_len(nrow(H_label))) {
                idx1 <- which(H_label[i, ] == 1)
                idx2 <- which(H_cluster[i, ] == 1)
                count1[i] <- sum(H_label[, idx1] * H_cluster[, idx2]) - 1
            }
            # normalization
            count1 <- count1 / other_member
            # 0/0 NaN support
            count1 <- ifelse(is.nan(count1), 0, count1)
            # for one animal, count1
            df_count <- cbind(df_cls_label, Count = count1)
            # assign count1 to object@union_cellnames
            df_union <- data.frame(
                CellType = object@union_cellnames,
                stringsAsFactors = FALSE
            )
            df_count_union <- merge(df_union,
                df_count,
                by.x = "CellType",
                by.y = "CellType",
                all.x = TRUE
            )
            df_count_union_ <- df_count_union[, c("Count")]
            df_count_union_[is.na(df_count_union_)] <- 0
            return(df_count_union_)
        })
        consistency <- Reduce("+", df_count_union_list)
        return(consistency)
    })
    return(consistency_l)
}
# Convert to conjunctive matrix
.H <- function(vec) {
    uniq_vec <- unique(vec)
    out <- matrix(0, nrow = length(vec), ncol = length(uniq_vec))
    rownames(out) <- names(vec)
    colnames(out) <- uniq_vec
    for (i in seq_len(length(vec))) {
        idx <- which(vec[i] == uniq_vec)
        out[i, idx] <- 1
    }
    out
}
# Number of members other than oneself, for normalization
.other_member <- function(H_label) {
    out <- rep(0, length = nrow(H_label))
    for (i in seq_len(nrow(H_label))) {
        idx1 <- which(H_label[i, ] == 1)
        out[i] <- sum(H_label[, idx1]) - 1
    }
    out
}
######### no_identified #########
.no_identified <- function(object) {
    lapply(object@dist_matrices, function(d) {
        attr(d, "Labels")
    }) |>
        unlist() -> all_cellname
    all_cellname |>
        table() |>
        as.numeric() -> cell_count
    return(cell_count)
}
