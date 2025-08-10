#'qsort_score
#'
#'qsort_score returns a data frame with scores derived from criteria Q-sorts and
#'from scales created from subsets of items.
#'
#'@param x data frame. x should be in wide format, with each subject data in one
#'  row and the scores for qset items in different columns. Columns for qset
#'  items' scores should be ordered sequentially from left to right, starting
#'  with item 1.
#'
#'@param qset A data frame containing Q-set criterion scores and / or derived scales.
#'  For details see for example ?qset_aqs, ?qset_cqq, ?qset_mbqs and ?qset_pq.
#'@param item1 Column name of x containing item 1 score
#'@param subj_id Optional. Column name of x with subjects' identification codes.
#'@param group_id Optional. Column name of x with groups' identification codes.
#'
#'@param qsort_length The number of items in the qsort.
#'
#'@return qsort_score function returns a data frame. This data frame will have a
#'  varying number of columns depending on the number of available criteria
#'  sorts and scales for the selected Q-set. Column names ending in "_c" refer
#'  to criteria sorts' scores, while column names ending in "_s" refer to
#'  scales' scores. Following Waters et al.'s (1985) suggestion about the
#'  influence of social desirability bias in Q-sort data, for Q-sets that have
#'  social desirability criterion scores, this data frame will also include
#'  criteria sorts' scores controlled for social desirability (i.e., partial
#'  correlations). Column names starting with "partial_" refer to these scores.
#'  The different criteria sorts and scales for each Q-set can be consulted in
#'  the documentation of qsets object (?qsets).
#'
#'@export
#'
#'@importFrom purrr modify_if
#'@importFrom stats aggregate.data.frame cor
#'
#'@examples
#'data_ccq <- qsort_score(ex_qsort$ccq,
#'                        qset_ccq,
#'                        qsort_length = 100,
#'                        item1 = "ccq1",
#'                        subj_id = "participant",
#'                        group_id = "classroom")
#'data_ccq
#'
#'data_aqs <- qsort_score(ex_qsort$aqs,
#'                        qset_aqs,
#'                        qsort_length = 90,
#'                        item1 = "aqs1")
#'
#'@references Baumrind, D. (1968). Manual for the Preschool Behaviour Q-set.
#'  Parental Research Project. Berkeley, CA: Institute of Human Development,
#'  University of California.
#'
#'  Block, J. H., & Block, J. (1969). The California Child Q-Set. Berkeley, CA:
#'  Institute of Human Development, University of California.
#'
#'  Pederson, D. R., Moran, G., & Bento, S. (1999). Maternal Behaviour
#'  Q-sort (version 3.1). London, ON: Psychology Department, Western University.
#'
#'  Waters, E. (1995). Appendix A: The attachment Q-set (Version 3. 0).
#'  Monographs of the Society for Research in Child Development, 60, 234-246.
#'
#'  Waters, E., Noyes, D. M., Vaughn, B. E., & Ricks, M. (1985). Q-sort
#'  definitions of social competence and self-esteem: Discriminant validity of
#'  related constructs in theory and data. Developmental Psychology, 21, 508-522.

qsort_score <- function(x, qset, qsort_length, item1, subj_id = NULL, group_id = NULL) {

# assures that x is a data frame
# it will turn a tibble back to a data.frame for instance
  x <- as.data.frame(x)

# create a selection vector with column numbers of x qset items
  start_x <- match(item1, (colnames(x)))
  end_x <- start_x + qsort_length - 1
  sel_x <- start_x : end_x

# identify criteria scores' column(s) in selected qset
# column names ending in "_c"
  cscores <- grep("_c$", names(qset), value = T)

# identify scales' column(s) in selected qset
# column names ending in "_s"
  scales <- grep("_s$", names(qset), value = T)

# identify column(s) detailing items to be inverted for scale computation
# column names ending in "_inv"
  scales_inv <- grep("_inv$", names(qset), value = T)

# create an empty data frame to store output values
  qsort_data <- data.frame()

  for(i in 1:nrow(x)){
# for all rows/cases in x
# first create empty data frames to store temporary values used in the computations below
    temp_id <- cbind.data.frame(x[i, subj_id], x[i, group_id])
    names(temp_id) <- c(subj_id, group_id)
    temp_cs <- data.frame()[1, ]
    temp_s <- data.frame()[1,]
    temp_s3 <- data.frame()[1, ]

    if(length(cscores) > 0){
# if selected qset contains criteria scores
      for(j in 1:length(cscores)){
# compute the correlation between the individual profile and each criterion sort
# store correlation values in j number of columns in temp_cs
# and name them accordingly
        temp_cs[1, j] <- cor(t(x[i, sel_x]),
                         qset[ , cscores[[j]]],
                         use = "pairwise.complete.obs")
      }
      names(temp_cs) <- cscores

      if("sdes_c" %in% cscores){
# if one of the criteria sorts corresponds to social desirability
        cscores2 <- cscores[cscores != "sdes_c"]
        for(k in 1:length(cscores2)){
# compute partial correlations between the individual profile data and each criterion sort,
# while controlling for social desirability
# add partial correlation values to temp_cs
# and name columns accordingly
          cor_ab <- temp_cs[cscores2[[k]]]
          cor_ac <- temp_cs["sdes_c"]
          cor_bc <- stats::cor(qset[ , cscores2[[k]]],
                               qset[ , "sdes_c"],
                           use = "pairwise.complete.obs")

          temp_cs[1, (length(cscores) + k)] <- (cor_ab - cor_ac * cor_bc) /
                                                (sqrt(1 - cor_ac^2) * sqrt(1 - cor_bc^2))
        }
        names(temp_cs) <- c(cscores, paste("partial_", cscores2, sep =""))
      }
    }

    if(length(scales) > 0){
# if selected qset contains scales
      for(j in 1:length(scales)){
# match individual profile data with columns indicating scales' items
# and which items need to be inverted
        temp_s <- cbind.data.frame(as.numeric(t(x[i, sel_x])),
                                   qset[ , scales[j]],
                                   qset[ , scales_inv[j]])
        names(temp_s) <- c("item_score", "scales", "scales_inv")

# invert items and compute scales' scores
        temp_s$item_score <- ifelse(temp_s$scales_inv == 1,
                                    10 - temp_s$item_score,
                                    temp_s$item_score)

        temp_s <- stats::aggregate.data.frame(temp_s$item_score,
                                       by = list(scales = temp_s$scales),
                                       FUN = mean, na.rm = T)

# store each score in a separate column of temp_s2
# name them accordingly
        temp_s2 <- as.data.frame(t(temp_s[[2]]))
        names(temp_s2) <- paste(scales[j], "_", temp_s[[1]], sep = "")

# bind scores for all scales presented in selected qset
        temp_s3 <- cbind.data.frame(temp_s3, temp_s2)
      }
    }

# bind each case identification columns (if given),
# correlation values computed from criteria sorts,
# and scale scores
    temp_data <- cbind.data.frame(temp_id, temp_cs, temp_s3)

# add a row to output data frame
    qsort_data <- rbind(qsort_data, temp_data)
  }

  row.names(qsort_data) <- NULL

# round numeric variables to three digits
  qsort_data <- purrr::modify_if(qsort_data, ~is.numeric(.), ~round(., 3))

# remove attributes of numeric variables
  for(i in 1:ncol(qsort_data)){
    if(is.numeric(qsort_data[[i]])){
      attributes(qsort_data[[i]]) <- NULL
    }
  }

# return output
  qsort_data
}
