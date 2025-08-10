#' print_cards
#'
#' print_cards creates a pdf document in a specified directory, with item’s
#' descriptions displayed on separate cards.
#'
#' @param qset A data frame containing the Q-set items' descriptions.
#'  For details see for example ?qset_aqs, ?qset_cqq, ?qset_mbqs and ?qset_pq.
#' @param desc_col Column name of qsets data frame containing items'
#'   descriptions.
#'
#' @param dir.print Directory path where .pdf file will be saved
#'
#' @return a pdf document with item’s descriptions displayed on separate cards.
#'
#' @export
#'
#' @importFrom ggplot2 element_blank ggplot theme_bw theme scale_x_continuous
#'   scale_y_continuous coord_fixed annotate
#'
#' @importFrom gridExtra marrangeGrob
#'
#' @importFrom cowplot save_plot

#' @examples
#' \donttest{print_cards(qset_aqs, desc_col = "description", dir.print = tempdir())}
#'
#'@references Baumrind, D. (1968). Manual for the Preschool Behaviour Q-set.
#'  Parental Research Project. Berkeley, CA: Institute of Human Development,
#'  University of California.
#'
#'  Block, J. H., & Block, J. (1969). The California Child Q-Set. Berkeley, CA:
#'  Institute of Human Development, University of California.
#'
#'  Pederson, D. R., Moran, G., & Bento, S. (1999). Maternal Behaviour Q-sort
#'  (version 3.1). London, ON: Psychology Department, Western University.
#'
#'  Waters, E. (1995). Appendix A: The attachment Q-set (Version 3. 0).
#'  Monographs of the Society for Research in Child Development, 60, 234-246.

print_cards <- function(qset, desc_col = "description", dir.print){
cards <- list()
for(i in 1:nrow(qset)){
# add item number to item description
    item_desc <- paste(i,
                     qset[[desc_col]][[i]],
                     sep=". ")
# break item description in 40 characters' sentences
  item_desc_wrap <- strwrap(item_desc, width = 40)
# define y axis position of each sentence
# y axis position decreases 0.4 for each additional sentence
  desc_ypos <- seq(2, length.out = length(item_desc_wrap), by = -0.4)

# create an empty plot for each item description
  cards[[i]] <- ggplot2::ggplot() +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          axis.text = ggplot2::element_blank(),
          axis.title = ggplot2::element_blank()) +
    ggplot2::scale_x_continuous(limits = c(0, 3.5)) +
    ggplot2::scale_y_continuous(limits = c(0, 2.5)) +
    ggplot2::coord_fixed(ratio = 1) +
# paste item description (item_desc_wrap)
    ggplot2::annotate("text", x = 1.75, y = desc_ypos, size = 4, label = item_desc_wrap)
}

# arrange 9 cards/plots per page
cards2 <- gridExtra::marrangeGrob(cards, ncol = 3, nrow = 3)
# save plots in a pdf file in the working directory
cowplot::save_plot(filename = paste(dir.print, "cards_", deparse(substitute(qset)), ".pdf", sep = ""),
                   cards2, ncol = 3, nrow = 3,
                   base_height = 2.5, base_width = 3.5)
}

