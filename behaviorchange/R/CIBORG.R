#
# ### ordered factor maken; argumetn toevogen met levels en labels (en dus impliciet
# ### de volgorde)
#
# ### Issue 2
#
# ### Issue 3
#
# ###
#
# ###
#
#
# #' Confidence Interval-Based Ordering/Organization of Relative Group patterns (CIBORG plots)
# #'
# #' Also see https://web.archive.org/web/20241004175837/https://fediscience.org/@mjskay/111128765582788364
# #' and https://web.archive.org/web/20241004175726/https://github.com/mjskay/uncertainty-examples/blob/master/diamond_plot.md
# #'
# #' @param data
# #' @param determinants
# #' @param targets
# #' @param group The variable with the groups
# #' @param groups Optionally, the groups to include. Only rows with a value for
# #' the `group` variable that occurs in `groups` are retained for the CIBORG
# #' plot. If both names and values are supplied here, the `group` variable will
# #' be transformed into a factor using [factor()], with the values in `groups`
# #' as levels and the names as labels. Note that that will also reorder the data
# #' in that order.
# #' @param conf.level
# #' @param ...
# #'
# #' @return An object with the results, including the CIBORG plot.
# #'
# #' @seealso [behaviorchange::CIBER()] for "regular" CIBER plots.
# #'
# #' @export
# #' @examples
# #' \donttest{### This example uses the determinant study Party Panel 17.1;
# #' ### see ?behaviorchange::BBC_data for more information.
# #' data(BBC_pp17.1);
# #'
# #' ### Create a three-level group variable
# #' BBC_pp17.1$genderPrivilege <-
# #'   ifelse(
# #'     is.na(BBC_pp17.1$gender),
# #'     3,
# #'     as.numeric(BBC_pp17.1$gender != "Male") + 1
# #'   );
# #'
# #' ### Produce the CIBORG plot
# #'
# #' ### Note that all question start with
# #' ### (translated from Dutch) "If I'm in a venue
# #' ### with loud music, and I wear earplugs, then..."
# #'
# #' behaviorchange::CIBORG(
# #'   data = BBC_pp17.1,
# #'   determinants = c(
# #'     'epw_AttExpect_hearingDamage',
# #'     'epw_AttExpect_highTone',
# #'     'epw_AttExpect_musicVolume',
# #'     'epw_AttExpect_musicFidelity',
# #'     'epw_AttExpect_loudConversation',
# #'     'epw_AttExpect_musicFocus',
# #'     'epw_AttExpect_musicEnjoy'
# #'   ),
# #'   subQuestions = c(
# #'     "...the probability of hearing damage is...",
# #'     "...the probability of hearing a beep the next day is...",
# #'     "...I hear the music...",
# #'     "...I hear the music...",
# #'     "...loud conversation bothers me...",
# #'     "...focusing on the music is...",
# #'     "...I enjoy the music..."
# #'   ),
# #'   leftAnchors = c(
# #'     "very small",
# #'     "very small",
# #'     "very softly",
# #'     "exactly the same",
# #'     "not at all",
# #'     "much harder",
# #'     "much less"
# #'   ),
# #'   rightAnchors = c(
# #'     "very large",
# #'     "very large",
# #'     "very loudly",
# #'     "very distorted",
# #'     "a lot",
# #'     "much easier",
# #'     "much more"
# #'   ),
# #'   targets = 'epw_attitude',
# #'   group = 'genderPrivilege',
# #'   groupLabel = 'Gender privilege',
# #'   groups = c(
# #'     "Most" = 1,
# #'     "Least" = 2,
# #'     "Unknown" = 3
# #'   )
# #' );
# #'
# #' }
# CIBORG <- function(data,
#                    determinants,
#                    targets,
#                    group,
#                    groups = NULL,
#                    groupLabel = NULL,
#                    conf.level = list(means = .9999,
#                                      associations = .95),
#                    subQuestions = NULL,
#                    leftAnchors = rep("Lo", length(determinants)),
#                    rightAnchors = rep("Hi", length(determinants)),
#                    outputFile = NULL,
#                    outputWidth = NULL,
#                    outputHeight = NULL,
#                    outputUnits = "in",
#                    outputParams = list(),
#                    orderBy = NULL,
#                    decreasing = NULL,
#                    # numberSubQuestions = FALSE,
#                    groupColorPalette = NULL,
#                    strokeColors = c("white", "black"),
#                    # vLines = c(-0.5, 0, 0.5),
#                    # vLineColors = "grey",
#                    # titlePrefix = "Means and associations (r) with",
#                    # titleVarLabels = NULL,
#                    # titleSuffix = "",
#                    # fullColorRange = NULL,
#                    # associationsAlpha = .5,
#                    returnPlotOnly = TRUE,
#                    drawPlot = TRUE,
#                    jitterWidth = .45,
#                    baseSize = 1,
#                    dotSize = 2.5 * baseSize,
#                    baseFontSize = 11 * baseSize,
#                    theme = ggplot2::theme_bw(base_size=baseFontSize),
#                    # xbreaks=NULL,
#                    # rsq = TRUE,
#                    ...) {
#
#   if (!requireNamespace("patchwork", quietly = TRUE)) {
#     stop("You need at the {patchwork} package, in at least version ",
#          "1.3.0. To install the newest version from CRAN, you can use:\n\n",
#          "  install.packages('patchwork');");
#
#   } else if (packageVersion("patchwork") < "1.3.0") {
#     stop("You need at the {patchwork} package, in at least version ",
#          "1.3.0. To install the newest version from CRAN, you can use:\n\n",
#          "  install.packages('patchwork');");
#   }
#
#   if (!all(c(determinants, targets, group) %in% names(data))) {
#     stop("Not all variables names you passed in arguments ",
#          "'determinants' or 'targets' are in the dataset!\n",
#          "Specifically, ",
#          ufs::vecTxtQ(c(determinants, targets, group)[!(c(determinants, targets, group) %in% names(data))]),
#          " is or are not in the provided dataset.");
#   }
#
#   res <- list(input = as.list(environment()),
#               intermediate = list(),
#               output = list());
#
#   ###---------------------------------------------------------------------------
#   ### Set group variable label
#
#   if (is.null(groupLabel)) {
#     groupLabel <- group;
#   }
#
#   ###---------------------------------------------------------------------------
#   ### Set subquestion vector
#
#   if (is.null(subQuestions)) {
#     subQuestions <- determinants;
#     names(subQuestions) <- determinants;
#   } else {
#     if (is.null(names(subQuestions))) {
#       names(subQuestions) <- determinants;
#     }
#   }
#
#   ###---------------------------------------------------------------------------
#   ### Create a factor with the group levels
#
#   if (!is.null(groups)) {
#
#     data <-
#       data[
#         data[, group] %in% groups,
#       ];
#
#     if (is.null(names(groups))) {
#
#       names(groups) <- groups;
#
#     }
#
#   } else {
#
#     groups <- sort(unique(data[, group]));
#     groups <- groups[!is.na(groups)];
#     names(groups) <- groups;
#
#   }
#
#   data[, group] <-
#     factor(
#       data[, group],
#       levels = groups,
#       labels = names(groups),
#       ordered = TRUE
#     );
#
#   ###---------------------------------------------------------------------------
#   ### Set the group palette
#
#   if (is.null(groupColorPalette)) {
#
#     groupColorPalette <-
#       behaviorchange::opts$get("palette_okabe_ito_reorganized");
#
#     groupColorPalette <-
#       groupColorPalette[
#         seq_along(groups)
#       ];
#
#     names(groupColorPalette) <- names(groups);
#
#   }
#
#   ###---------------------------------------------------------------------------
#   ### Vectors with indices and labels
#
#   determinantIndices <- stats::setNames(
#     seq_along(determinants),
#     nm = determinants
#   );
#
#   ### Potentially reorder to ensure same order as determinants
#   subQuestions <- subQuestions[determinants];
#
#   labels <- paste0(subQuestions, "\n[", leftAnchors, "-", rightAnchors, "]");
#
#   ###---------------------------------------------------------------------------
#   ### Make long ('tidy') data frame
#
#   res$intermediate$determinantDat_long <-
#     data.frame(
#       determinant = rep(determinants, each = nrow(data)),
#       subQuestion = rep(subQuestions, each = nrow(data)),
#       leftAnchor = rep(leftAnchors, each = nrow(data)),
#       rightAnchor = rep(rightAnchors, each = nrow(data)),
#       label = rep(labels, each = nrow(data)),
#       determinantIndex = rep(determinantIndices, each = nrow(data)),
#       group = rep(data[, group], times = length(determinants)),
#       value = unlist(data[, determinants])
#     );
#
#   ### Store determinants as a factor to preserve order
#   res$intermediate$determinantDat_long$determinant <-
#     factor(
#       res$intermediate$determinantDat_long$determinant,
#       levels = rev(determinants),
#       labels = rev(determinants),
#       ordered = TRUE
#     );
#
#   res$intermediate$determinantDat_long <-
#     res$intermediate$determinantDat_long[
#       !is.na(res$intermediate$determinantDat_long$value),
#     ];
#
#   res$intermediate$datForCors <- data[, c(determinants, targets, group)];
#
#   res$intermediate$meanPartDats <- list();
#   res$intermediate$assocPartDats <- list();
#
#   ### This first bit is legacy code; since the edits on 2025-03-16 this
#   ### should normally not run any more.
#   if (is.null(groups)) {
#     groupLevels <- unique(res$intermediate$datForCors[[group]]);
#     groupLevels <- groupLevels[!is.na(groupLevels)];
#   } else {
#     if (is.null(names(groups))) {
#       ### This should actually also normally never run any more, since
#       ### `groups` is created if it didn't exist, and names are set if
#       ### it didn't have names yet.
#       groupLevels <- groups;
#     } else {
#       groupLevels <- names(groups);
#     }
#   }
#
#   ###---------------------------------------------------------------------------
#   ### Compute means and correlations per group
#   ###---------------------------------------------------------------------------
#
#   for (currentGroup in groupLevels) {
#
#     tmpDat <- res$intermediate$datForCors[
#       res$intermediate$datForCors[[group]] == currentGroup,
#     ];
#
#     res$intermediate$assocPartDats[[currentGroup]] <-
#       sapply(targets, function(currentTarget) {
#
#         tryCatch({
#           tmpCors <-
#             lapply(tmpDat[, determinants, drop=FALSE],
#                    stats::cor.test,
#                    tmpDat[, currentTarget]);
#         }, error = function(errorMsg) {
#           stop("Error when computing correlation confidence intervals. ",
#                "Determinants: ", vecTxt(determinants, useQuote="`"),
#                ". Target: `", currentTarget, "`. ",
#                "The error message was: ", errorMsg);
#         });
#
#         res <-
#           data.frame(
#             determinant = determinants,
#             determinantIndex = determinantIndices,
#             target = currentTarget,
#             group = currentGroup,
#             lo=unlist(lapply(tmpCors, function(x) return(x$conf.int[1]))),
#             es=unlist(lapply(tmpCors, function(x) return(x$estimate))),
#             hi=unlist(lapply(tmpCors, function(x) return(x$conf.int[2])))
#           );
#
#         res$width <- res$hi - res$lo;
#
#         return(res);
#
#       }, simplify=FALSE);
#
#     res$intermediate$assocPartDats[[currentGroup]] <-
#       do.call(
#         rbind,
#         res$intermediate$assocPartDats[[currentGroup]]
#       );
#
#     ###-------------------------------------------------------------------------
#     ### CIs for means
#     ###-------------------------------------------------------------------------
#
#     res$intermediate$meanPartDats[[currentGroup]] <-
#       ufs::rbind_df_list(
#         lapply(tmpDat[, determinants, drop=FALSE],
#                function(vector) {
#                  confIntObject <-
#                    ufs::meanConfInt(
#                      vector,
#                      conf.level = conf.level$mean
#                    );
#                  res <- as.data.frame(confIntObject$output$ci);
#                  res$mean <- mean(vector, na.rm=TRUE);
#                  res$width <- res$ci.hi - res$ci.lo;
#                  return(res);
#                })
#       );
#
#     res$intermediate$meanPartDats[[currentGroup]]$determinant <-
#       determinants;
#
#     res$intermediate$meanPartDats[[currentGroup]]$determinantIndex <-
#       determinantIndices[res$intermediate$meanPartDats[[currentGroup]]$determinant];
#
#     res$intermediate$meanPartDats[[currentGroup]]$group <-
#       rep(currentGroup, length(determinants));
#
#   }
#
#   ###---------------------------------------------------------------------------
#   ### Combining into one dataframe for correlations
#
#   res$intermediate$assocPartDats <-
#     do.call(
#       rbind,
#       res$intermediate$assocPartDats
#     );
#
#   ### Restore factor
#   res$intermediate$assocPartDats$group <-
#     factor(
#       res$intermediate$assocPartDats$group,
#       levels = names(groups),
#       labels = names(groups),
#       ordered = TRUE
#     );
#
#   res$intermediate$assocPartDats_long <-
#     data.frame(
#       determinant = rep(res$intermediate$assocPartDats$determinant, 3),
#       determinantIndex = rep(determinantIndices[res$intermediate$assocPartDats$determinant], 3),
#       target = rep(res$intermediate$assocPartDats$target, 3),
#       group = rep(res$intermediate$assocPartDats$group, 3),
#       is_point = rep(c(0, 1, 0), each = nrow(res$intermediate$assocPartDats)),
#       value = c(res$intermediate$assocPartDats$lo,
#                 res$intermediate$assocPartDats$es,
#                 res$intermediate$assocPartDats$hi),
#       width = rep(res$intermediate$assocPartDats$width, 3)
#     );
#
#   res$intermediate$assocPartDats_long$slabHeight <-
#     res$intermediate$assocPartDats_long$is_point /
#     res$intermediate$assocPartDats_long$width;
#
#   ###---------------------------------------------------------------------------
#   ### Combining into one dataframe for means
#
#   res$intermediate$meanPartDats <-
#     do.call(
#       rbind,
#       res$intermediate$meanPartDats
#     );
#
#   ### Restore factor
#   res$intermediate$meanPartDats$group <-
#     factor(
#       res$intermediate$meanPartDats$group,
#       levels = names(groups),
#       labels = names(groups),
#       ordered = TRUE
#     );
#
#   res$intermediate$meanPartDats_long <-
#     data.frame(
#       determinant = rep(res$intermediate$meanPartDats$determinant, 3),
#       determinantIndex = rep(determinantIndices[res$intermediate$meanPartDats$determinant], 3),
#       group = rep(res$intermediate$meanPartDats$group, 3),
#       is_point = rep(c(0, 1, 0), each = nrow(res$intermediate$meanPartDats)),
#       value = c(res$intermediate$meanPartDats$ci.lo,
#                 res$intermediate$meanPartDats$mean,
#                 res$intermediate$meanPartDats$ci.hi),
#       width = rep(res$intermediate$meanPartDats$width, 3)
#     );
#
#   res$intermediate$meanPartDats_long$slabHeight <-
#     res$intermediate$meanPartDats_long$is_point /
#     res$intermediate$meanPartDats_long$width;
#
#   ###---------------------------------------------------------------------------
#   ### Creating left plot
#   ###---------------------------------------------------------------------------
#
#   res$intermediate$leftPlot <-
#     ggplot2::ggplot(
#       data = res$intermediate$determinantDat_long,
#       mapping = ggplot2::aes(
#         x = value,
#         y = determinant, #determinantIndex,
#         fill = group,
#         group = group
#       )
#     ) +
#     ggplot2::geom_point(
#       shape = 21,
#       alpha = .2,
#       mapping = ggplot2::aes(fill = group, color = "black"),
#       position = ggplot2::position_jitterdodge(
#           jitter.width = .4,
#           jitter.height = .4,
#           dodge.width = .6,
#         )
#     ) +
#
#     ###-------------------------------------------------------------------------
#     ### Diamond layer
#     ggdist::geom_slab(
#       data = res$intermediate$meanPartDats_long,
#       mapping = ggplot2::aes(x = value,
#                              y = determinant,
#                              thickness = slabHeight),
#       position = ggdist::position_dodgejust(width = .6),
#       normalize = "groups",
#       side = "both",
#       scale = 0.35,
#       alpha = 0.80,
#       color = strokeColors[1],
#       linewidth = 0.5
#     ) +
#
#     ###-------------------------------------------------------------------------
#     ### General settings
#
#     ggplot2::scale_fill_discrete(
#       type = groupColorPalette
#     ) +
#
#     ggplot2::scale_x_continuous(
#       breaks = 1:7,
#     ) +
#
#     #ggplot2::scale_y_continuous(
#     ggplot2::scale_y_discrete(
#       breaks = determinants,
#       labels = labels #leftAnchors
#       # ,
#       # sec.axis =
#       #   ggplot2::dup_axis(
#       #     labels = rightAnchors
#       #   )
#     ) +
#
#     theme +
#
#     ggplot2::theme(
#       legend.position = "bottom",
#       legend.direction = "horizontal"
#     ) +
#
#     ggplot2::guides(
#       color = ggplot2::guide_none()
#     ) +
#
#     ggplot2::labs(
#       x = NULL,
#       y = NULL,
#       group = groupLabel,
#       fill = groupLabel
#     );
#
#   ###---------------------------------------------------------------------------
#   ### Right plot
#   ###---------------------------------------------------------------------------
#
#   res$intermediate$rightPlot <-
#     ggplot2::ggplot(
#       data = res$intermediate$assocPartDats_long,
#       mapping = ggplot2::aes(x = value,
#                              y = determinant, #determinantIndex,
#                              fill = group,
#                              thickness = slabHeight)
#     ) +
#     ggdist::geom_slab(
#       data = res$intermediate$assocPartDats_long,
#       position = ggdist::position_dodgejust(width = .6),
#       normalize = "groups",
#       side = "both",
#       scale = 0.35,
#       alpha = 0.80,
#       color = strokeColors[2],
#       linewidth = 0.5
#     ) +
#
#     ###-------------------------------------------------------------------------
#     ### General settings
#
#     ggplot2::coord_cartesian(xlim = c(-1, 1)) +
#
#     ggplot2::scale_fill_discrete(
#       type = groupColorPalette
#     ) +
#
#     ggplot2::scale_x_continuous(
#       breaks = c(-1, -.7, -.5, -.3, 0,
#                  .3, .5, .7, 1),
#       labels = c("-1", "-.7", "-.5", "-.3", "0",
#                  ".3", ".5", ".7", "1")
#     ) +
#
#     #ggplot2::scale_y_continuous(
#     ggplot2::scale_y_discrete(
#       breaks = determinants, #determinantIndices,
#       labels = labels, #leftAnchors
#     ) +
#
#     # ggplot2::scale_fill_discrete(groupColorPalette) +
#     # ggplot2::scale_color_discrete(groupColorPalette) +
#
#     theme +
#
#     ggplot2::theme(
#       legend.position = "bottom",
#       legend.direction = "horizontal"
#     ) +
#
#     ggplot2::guides(
#       color = ggplot2::guide_none(),
#       fill = ggplot2::guide_none(),
#       group = ggplot2::guide_none()
#     ) +
#
#     ggplot2::labs(
#       x = NULL,
#       y = NULL
#     );
#
#   ###---------------------------------------------------------------------------
#   ### Combine plots
#   res$output$plot <-
#     patchwork::wrap_plots(
#       res$intermediate$leftPlot,
#       res$intermediate$rightPlot,
#       guides = "collect",
#       axes = "collect"
#     ) +
#     patchwork::plot_annotation(
#       title = "CIBORG plot",
#       theme = ggplot2::theme(
#         legend.position = 'bottom'
#       )
#     );
#
#   ###---------------------------------------------------------------------------
#   ### Post processing, drawing, saving, etc
#   ###---------------------------------------------------------------------------
#
#   ### Default sizes ; first compute in centimeters, then convert to inches
#   attr(res$output$plot, 'height') <- baseSize + 1.25 * baseSize * max(length(determinants), 1.5);
#   attr(res$output$plot, 'width') <- 21 - 3;
#   attr(res$output$plot, 'height') <- attr(res$output$plot, 'height') / 2.54;
#   attr(res$output$plot, 'width') <- attr(res$output$plot, 'width') / 2.54;
#
#   if (drawPlot) {
#     grid::grid.newpage();
#     grid::grid.draw(res$output$plot);
#   }
#
#   if (!is.null(outputFile)) {
#     if ((nchar(dirname(outputFile)) == 0) | (!dir.exists(dirname(outputFile)))) {
#       warning("The directory specified to save the the outputFile to ('",
#               dirname(outputFile),
#               "') does not exist, so not saving the plot!");
#     } else {
#       if (is.null(outputWidth)) {
#         outputWidth <- attr(res$output$plot, 'width');
#         outputUnits <- "in";
#       }
#       if (is.null(outputHeight)) {
#         outputHeight <- attr(res$output$plot, 'height');
#         outputUnits <- "in";
#       }
#       if (is.null(outputUnits)) {
#         outputUnits <- "in";
#       }
#       do.call(ggplot2::ggsave,
#               c(list(file=outputFile,
#                      plot=res$output$plot,
#                      width = outputWidth,
#                      height = outputHeight,
#                      units = outputUnits),
#                 outputParams));
#     }
#   }
#
#   if (returnPlotOnly) {
#     return(invisible(res$output$plot));
#   } else {
#     return(invisible(res));
#   }
#
# }
#
