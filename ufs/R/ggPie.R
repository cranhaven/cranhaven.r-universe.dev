### Credits:
### http://mathematicalcoffee.blogspot.nl/2014/06/ggpie-pie-graphs-in-ggplot2.html



#' A ggplot pie chart
#'
#' This function creates a pie chart. Note that these are generally quite
#' strongly advised against, as people are not good at interpreting relative
#' frequencies on the basis of pie charts.
#'
#' @param vector The vector (best to pass a factor).
#' @param scale_fill The ggplot scale fill function to use for the colors.
#' @return A ggplot pie chart.
#' @note This function is very strongly based on the Mathematical Coffee post
#' at
#' http://mathematicalcoffee.blogspot.com/2014/06/ggpie-pie-graphs-in-ggplot2.html.
#' @examples ufs::ggPie(mtcars$cyl);
#'
#' @export
ggPie <- function (vector, scale_fill = ggplot2::scale_fill_viridis_d()) {

  dat <- data.frame(table(vector));
  names(dat) <- c('labels', 'totals');
  totals = 'totals';
  by = 'labels';

  dat <- dat[dat$totals > 0, ];

  return(
    ggplot2::ggplot(data = dat,
                    ggplot2::aes_string(x=factor(1), y=totals, fill=by)) +
      ggplot2::geom_bar(stat='identity', color='black') +
      # removes black borders from legend
      ggplot2::guides(fill=ggplot2::guide_legend(override.aes=list(color=NA))) +
      ggplot2::coord_polar(theta='y') +
      ggplot2::scale_y_continuous(
        breaks=(sum(dat[[totals]]) - (cumsum(dat[[totals]]) - dat[[totals]] / 2)),
        labels=dat[[by]]) +
      scale_fill +
      ggplot2::theme(axis.ticks=ggplot2::element_blank(),
                     axis.text.y=ggplot2::element_blank(),
                     axis.text.x=ggplot2::element_text(color='black'),
                     axis.title=ggplot2::element_blank(),
                     legend.position="none",
                     panel.background = ggplot2::element_rect(fill = "white"))
  );
}
