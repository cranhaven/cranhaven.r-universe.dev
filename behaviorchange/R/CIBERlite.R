#' CIBERlite
#'
#' CIBERlite plots can be used to quickly get an idea of means and correlations
#' of a small number of determinants. They were developed to facilitate
#' conducting and interpreting determinant studies by prevention
#' professionals.
#'
#' More details are provided in <doi:10/js9b>.
#'
#' @param data The dataframe containing the variables.
#' @param determinants Either a character vector with the names of the
#'   determinants, or a list of named character vectors, where each vector
#'   contains a number of subdeterminants, and each vector's name is the name
#'   of the more proximal determinant (i.e. that 'contains' those subdeterminants).
#' @param targets A character vector with the names of the targets (i.e. more
#'   proximal determinants, behavior, etc).
#' @param determinantOrder The order in which to display the determinants (if
#'   this needs to be different from the order as provided in `determinants`).
#' @param determinantLabels The labels to use for the determinants.
#' @param subDeterminantLabels The labels to use for the subdeterminants.
#' @param title The title of the plot.
#' @param conf.level The confidence levels: a list with two named values; the
#'   confidence level for the means, named `means`, and the confidence level
#'   for the associations, named `associations`.
#' @param scaleRange The full range of the scale of the
#'   determinants/subdeterminants; the minimum and maximum values are used if
#'   this is not provided.
#' @param determinantAesthetics,subDeterminantAesthetics,rDiamondAesthetics The
#'   aesthetics for the determinants, subdeterminants, and correlation diamonds,
#'   each a list containing three named values: `fill`, `color`, and `alpha`.
#'
#' @return A `ggplot`.
#' @export
#'
#' @examples ### This example uses the determinant study Party Panel 15.1;
#' ### see ?behaviorchange::BBC_data for more information.
#' data(BBC_pp15.1);
#' CIBERlite(data=BBC_pp15.1,
#'           determinants=c('highDose_attitude',
#'                          'highDose_perceivedNorm',
#'                          'highDose_pbc'),
#'           targets=c('highDose_intention'));
#'
CIBERlite <- function(data,
                      determinants,
                      targets,
                      determinantOrder = NULL,
                      determinantLabels = NULL,
                      subDeterminantLabels = NULL,
                      title=NULL,
                      conf.level = 0.95,
                      scaleRange = NULL,
                      determinantAesthetics = list(fill = 'black',
                                                   color=NA,
                                                   alpha=.5),
                      subDeterminantAesthetics = list(fill = 'black',
                                                      color=NA,
                                                      alpha=.5),
                      rDiamondAesthetics = list(fill = '#c4c4c4',
                                                color=NA,
                                                alpha=.75)) {

  if (is.list(determinants)) {
    subDeterminantNames <- unlist(determinants);
    determinantNames <- names(determinants);
  } else {
    subDeterminantNames <- c();
    determinantNames <- determinants;
  }
  if (is.null(determinantOrder)) {
    determinantOrder <- seq_along(determinantNames);
  }

  if (getOption('ufs.debug', FALSE)) {
    cat("\n", ufs::repStr("-", 50), "\n");
    print(targets);
    print(determinantNames);
    print(subDeterminantNames);
  }

  ### Select relevant rows from dataset
  dat <- data[, c(targets, determinantNames, subDeterminantNames)];

  ### Set minimum and maximum for the used scales, if not provided
  if (is.null(scaleRange)) {
    scaleRange <- c(min(dat[, c(determinantNames, subDeterminantNames)], na.rm=TRUE),
                    max(dat[, c(determinantNames, subDeterminantNames)], na.rm=TRUE));
  }

  if (getOption('ufs.debug', FALSE)) {
    cat("\n", ufs::repStr("-", 50), "\n");
    print(determinantOrder);
    print(scaleRange);
  }

  # print(dim(is.na(dat[, c(determinantNames, subDeterminantNames)])));
  # print((scaleRange[2] - scaleRange[1]));
  # print(class(dim(((dat[, c(determinantNames, subDeterminantNames)] - scaleRange[1]) /
  #             (scaleRange[2] - scaleRange[1])))));

  ### Translate subdeterminants and determinants to 0-1 range
  dat[, c(determinantNames, subDeterminantNames)] <-
    ((dat[, c(determinantNames, subDeterminantNames)] - scaleRange[1]) /
       (scaleRange[2] - scaleRange[1]));

  if (is.list(determinants)) {
    ### We also have subdeterminants, so also get the means for those.

    subDeterminantDat<- sapply(dat[, subDeterminantNames],
                               function(x) {
                                 return(c(mean = mean(x, na.rm=TRUE),
                                          sd = stats::sd(x, na.rm=TRUE)));
                               });
    subDeterminantDat <- as.data.frame(t(subDeterminantDat));

    subDeterminantDat$subdeterminant <- row.names(subDeterminantDat);
    subDeterminantDat$determinant <- rep(determinantNames,
                                         sapply(determinants, length));
    subDeterminantDat$xPos <- rep(determinantOrder,
                                  sapply(determinants, length));
  }

  determinantDat <- sapply(dat[, determinantNames],
                           function(x) {
                             return(c(mean = mean(x, na.rm=TRUE),
                                      sd = stats::sd(x, na.rm=TRUE)));
                           });
  determinantDat <- as.data.frame(t(determinantDat));

  determinantDat$determinant <- row.names(determinantDat);
  determinantDat$xPos <- determinantOrder;

  determinantTargetCorObject <-
    ufs::associationMatrix(dat,
                           x = determinantNames,
                           y = targets,
                           conf.level=conf.level);

  determinantTarget.r <-
    determinantTargetCorObject$output$raw$es;
  determinantTarget.ci.lo <-
    determinantTargetCorObject$output$raw$ci.lo;
  determinantTarget.ci.hi <-
    determinantTargetCorObject$output$raw$ci.hi;

  rDiamondCoordinates <-
    lapply(1:length(targets), function(targetIndex) {
      tmpDf <- cbind(determinantTargetCorObject$output$raw$ci.lo[, targetIndex],
                     determinantTargetCorObject$output$raw$es[, targetIndex],
                     determinantTargetCorObject$output$raw$ci.hi[, targetIndex]);
      return(lapply(1:nrow(tmpDf),
                    function(determinantIndex) {
                      return(ufs::diamondCoordinates(tmpDf[determinantIndex, ],
                                                     otherAxisValue = determinantIndex));
                    }));
    });

  rDiamondLayer <-
    lapply(1:length(targets), function(targetIndex) {
      tmpDf <-
        cbind(determinantTargetCorObject$output$raw$ci.lo[, targetIndex],
              determinantTargetCorObject$output$raw$es[, targetIndex],
              determinantTargetCorObject$output$raw$ci.hi[, targetIndex]);
      tmpDf <-
        as.data.frame(tmpDf);
      return(ufs::ggDiamondLayer(tmpDf,
                                 direction="vertical",
                                 lineColor=rDiamondAesthetics$color,
                                 alpha=rDiamondAesthetics$alpha));
    });

  if (getOption('ufs.debug', FALSE)) {
    cat("\n", ufs::repStr("-", 50), "\n");
    print(determinantDat);
    cat("\n", ufs::repStr("-", 50), "\n");
    print(subDeterminantDat);
    cat("\n", ufs::repStr("-", 50), "\n");
    print(rDiamondCoordinates);
    cat("\n", ufs::repStr("-", 50), "\n");
  }

  if (is.null(determinantLabels)) {
    determinantLabels <- determinantNames;
  }
  if (is.null(subDeterminantLabels)) {
    subDeterminantLabels <- subDeterminantNames;
  }

  res <-
    ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = c(0, 1),
                        color="black");

  if (exists('subDeterminantDat')) {
    res <- res +
      ggplot2::geom_bar(data = subDeterminantDat,
                        ggplot2::aes_string(x = 'xPos',
                                            y = 'mean',
                                            group = 'subdeterminant'),
                        stat='identity',
                        position='dodge',
                        fill=subDeterminantAesthetics$fill,
                        color=subDeterminantAesthetics$color,
                        alpha=subDeterminantAesthetics$alpha);
  }
  res <- res +
    ggplot2::geom_bar(data = determinantDat,
                      ggplot2::aes_string(x = 'xPos',
                                          y = 'mean'),
                      stat='identity',
                      position='identity',
                      fill=determinantAesthetics$fill,
                      color=determinantAesthetics$color,
                      alpha=determinantAesthetics$alpha) +
    rDiamondLayer +
    ggplot2::theme_minimal() +
    ggplot2::coord_cartesian(ylim=c(0,1)) +
    ggplot2::scale_x_continuous(breaks=determinantOrder,
                                labels=determinantLabels,
                                sec.axis=ggplot2::dup_axis(breaks=c(determinantOrder-.25,
                                                                    determinantOrder+.25),
                                                           labels=c(subDeterminantLabels[ufs::is.odd(seq_along(subDeterminantLabels))],
                                                                    subDeterminantLabels[ufs::is.even(seq_along(subDeterminantLabels))]))) +
    ggplot2::ggtitle(title) +
    ggplot2::xlab(NULL) +
    ggplot2::ylab(NULL) +
    NULL;
  return(res);
}
