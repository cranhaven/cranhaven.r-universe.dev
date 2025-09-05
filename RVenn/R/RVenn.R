#' \code{RVenn}: A package for set operations for many sets.
#'
#' Set operations for many sets. The base functions for set operations in R can
#' be used for only two sets. This package uses 'purr' to find the union,
#' intersection and difference of three or more sets. This package also provides
#' functions for pairwise set operations among several sets. Further, based on
#' 'ggplot2' and 'ggforce', a Venn diagram can be drawn for two or three sets.
#' For bigger data sets, a clustered heatmap showing presence/absence of the
#' elements of the sets can be drawn based on the 'pheatmap' package. Finally,
#' enrichment test can be applied to two sets whether an overlap is
#' statistically significant or not.
#'
#' @docType package
#' @name RVenn
NULL


#' Build a \code{Venn} object.
#'
#' \code{Venn} builds a \code{Venn} object from a list.
#'
#' @param sets (Required) A list containing vectors in the same class. If a
#'   vector contains duplicates they will be discarded. If the list doesn't have
#'   names the sets will be named as "Set_1", "Set_2", "Set_3" and so on.
#' @return A \code{Venn} object.
#' @examples
#' venn = Venn(list(letters[1:10], letters[3:12], letters[6:15]))
#' print(venn)
#' @name Venn
NULL


#' Intersection of many sets.
#'
#' \code{overlap} returns the same elements of the sets in a \code{Venn} object.
#'
#' @param venn (Required) A \code{Venn} object.
#' @param slice (Optional) The names or the indices of sets of interest. Default
#'   is "all", meaning the intersection will be calculated for all the sets.
#' @return A vector showing the intersection of the sets.
#' @examples
#' venn = Venn(list(letters[1:10], letters[3:12], letters[6:15]))
#' overlap(venn)
#' overlap(venn, slice = c(1, 2))
#' @name overlap
NULL


#' Pairwise intersections of many sets.
#'
#' \code{overlap_pairs} returns the pairwise intersections of the sets in a
#' \code{Venn} object.
#'
#' @param venn (Required) A \code{Venn} object.
#' @param slice (Optional) The names or the indices of sets of interest. Default
#'   is "all", meaning the pairwise intersections will be calculated for all the
#'   sets.
#' @return A list showing the pairwise intersections of the sets.
#' @examples
#' venn = Venn(list(letters[1:10], letters[3:12],
#'                  letters[6:15], letters[9:18]))
#' overlap_pairs(venn)
#' overlap_pairs(venn, slice = 1:3)
#' @name overlap_pairs
NULL


#' Union of many sets.
#'
#' \code{unite} returns the union of the sets in a \code{Venn} object.
#'
#' @param venn (Required) A \code{Venn} object.
#' @param slice (Optional) The names or the indices of sets of interest. Default
#'   is "all", meaning the union will be calculated for all the sets.
#' @return A vector showing the union of the sets.
#' @examples
#' venn = Venn(list(letters[1:10], letters[3:12], letters[6:15]))
#' unite(venn)
#' unite(venn, slice = c(1, 2))
#' @name unite
NULL


#' Pairwise unions of many sets.
#'
#' \code{unite_pairs} returns the pairwise unions of the sets in a \code{Venn}
#' object.
#'
#' @param venn (Required) A \code{Venn} object.
#' @param slice (Optional) The names or the indices of sets of interest. Default
#'   is "all", meaning the pairwise intersections will be calculated for all the
#'   sets.
#' @return A list showing the pairwise unions of the sets.
#' @examples
#' venn = Venn(list(letters[1:10], letters[3:12],
#'                  letters[6:15], letters[9:18]))
#' unite_pairs(venn)
#' unite_pairs(venn, slice = 1:3)
#' @name unite_pairs
NULL


#' Set difference.
#'
#' \code{discern} returns the difference between two group of sets selected from
#'   a \code{Venn} object. If multiple sets are chosen for the slices, union of
#'   those sets will be used.
#'
#' @param venn (Required) A \code{Venn} object.
#' @param slice1 (Required) The name or the index of the set of interest.
#' Multiple sets can be selected.
#' @param slice2 (Optional) The name or the index of the set of interest.
#'   Multiple sets can be selected. Default is all the sets except the sets of
#'   slice1.
#' @return A vector showing the difference between slice1 and slice2.
#' @examples
#' venn = Venn(list(letters[1:10], letters[3:12], letters[6:15]))
#' discern(venn, slice1 = 1)
#' discern(venn, slice1 = c(1, 2), slice2 = 3)
#' @name discern
NULL


#' Pairwise difference of many sets.
#'
#' \code{discern_pairs} returns the pairwise differences of the sets in a
#' \code{Venn} object.
#'
#' @param venn (Required) A \code{Venn} object.
#' @param slice (Optional) The names or the indices of sets of interest. Default
#'   is "all", meaning the pairwise differences will be calculated for all the
#'   sets.
#' @return A list showing the pairwise differences of the sets.
#' @examples
#' venn = Venn(list(letters[1:10], letters[3:12],
#'                  letters[6:15], letters[9:18]))
#' discern_pairs(venn)
#' discern_pairs(venn, slice = 1:3)
#' @name discern_pairs
NULL


#' Draw the Venn diagram.
#'
#' Draw the Venn diagram for 2 or 3 sets.
#'
#' This function is based on the packages 'ggplot2' and 'ggforce.' It
#' has been designed for 2 or 3 sets because Venn diagrams are terrible for
#' showing the interactions of 4 or more sets. If you need to visualize such
#' interactions, consider using \code{\link{setmap}}.
#'
#' @param venn (Required) A \code{Venn} object.
#' @param slice (Optional) The names or the indices of the sets of interest.
#'   Default is "all", which is for the cases the \code{Venn} object only
#'   contains 2 or 3 sets. If you have 4 or more sets, this argument is
#'   required.
#' @param fill (Optional) Fill color of the sets.
#' @param alpha (Optional) Opacity of the fill colors. Default is 0.5 in the
#'   range of (0, 0.5).
#' @param thickness (Optional) Stroke size of the sets.
#' @return The function returns the plot in ggplot2 style.
#' @examples
#' venn = Venn(list(letters[1:10], letters[3:12], letters[6:15]))
#' ggvenn(venn)
#' ggvenn(venn, slice = c(1, 2), thickness = 0, alpha = 0.3)
#' @name ggvenn
NULL


#' Draw a clustered heatmap showing presence/absence of the elements.
#'
#' This function is based on the package 'pheatmap'. \code{\link{ggvenn}}
#' function is useful for 2-3 sets, if you need to show interactions between
#' many sets, you can show the presence/absence of the elements among all the
#' sets and cluster both the sets and the elements based on Jaccard distances.
#'
#' @param venn (Required) A \code{Venn} object.
#' @param slice (Optional) The names or the indices of sets of interest. Default
#'   is "all", meaning the union will be calculated for all the sets.
#' @param element_clustering (Optional) Boolean values determining if elements
#'   should be clustered.
#' @param set_clustering (Optional) Boolean values determining if sets should be
#'   clustered.
#' @param method (Optional) Clustering method used. Accepts the same values as
#'   \code{\link[stats]{hclust}}.
#' @param legend (Optional) Boolean values determining if the legend should be
#'   drawn.
#' @param title (Optional) Title of the heatmap.
#' @param element_fontsize (Optional) Font size of the elements.
#' @param set_fontsize (Optional) Font size of the sets.
#' @return Presence/absence heatmap of the sets.
#' @examples
#' venn = Venn(list(letters[1:10], letters[3:12], letters[6:15], letters[9:16],
#' letters[15:25], letters[12:20]))
#' setmap(venn)
#' setmap(venn, slice = 1:4, element_clustering = FALSE, set_clustering = FALSE)
#' @name setmap
NULL


#' Perform an enrichment test.
#'
#' Calculate the p-value of occurrence of an overlap between two sets by chance.
#'
#' This type of analysis can also be performed by hypergeometric test or
#' Fisher's exact test. Here, the approach is similar to that described in
#' (\href{https://onlinelibrary.wiley.com/doi/full/10.1111/tpj.13261}{Austin et
#' al., 2016}). Briefly, the test is based on randomly generation of sets with
#' equal size to \code{set1} from the background (universal) set. After creating
#' n (default is 10,000) random sets, the overlap between these and \code{set2}
#' is calculated to make a null distribution. When this distribution is true,
#' the probability of seeing an overlap at least as extreme as what was observed
#' (overlap between \code{set1} and \code{set2}) will be returned as the
#' p-value.
#' @param venn (Required) A \code{Venn} object.
#' @param set1 (Required) The name or the index of the set of interest.
#' @param set2 (Required) The name or the index of the set to be checked whether
#'   enriched in \code{set1}.
#' @param univ (Optional) Population size. Default is "all", implying the union
#'   of all the sets in the \code{Venn} object will be used. Another set as the
#'   whole population can be assigned as well.
#' @param n (Optional) Number of randomly generated sets. Default is 10,000 and
#'   minimum is 1,000.
#' @param seed (Optional) An integer passed to set.seed function. It is
#'   used to fix a seed for reproducibly random number generation. Default is
#'   42.
#' @return Returns a list containing the probability (Significance) of occurrence
#'   of an overlap between two sets by chance and the number of occurrences
#'   (Overlap_Counts) in randomly generated sets.
#' @examples
#' set1 = c(1:20, letters[1:10])
#' set2 = letters[-26]
#' univ = unique(c(set1, set2, 21:200))
#' venn = Venn(list(set1, set2, univ))
#' e = enrichment_test(venn, 1, 2)
#' e$Significance
#' @name enrichment_test
NULL

