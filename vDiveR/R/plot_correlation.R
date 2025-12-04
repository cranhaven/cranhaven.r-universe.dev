#' Entropy and total variant incidence correlation plot
#'
#' This function plots the correlation between entropy and total variant incidence
#' of all the provided protein(s).
#'
#' @param df DiMA JSON converted csv file data
#' @param host number of host (1/2)
#' @param alpha any number from 0 (transparent) to 1 (opaque)
#' @param line_dot_size dot size in scatter plot
#' @param ylabel y-axis label
#' @param xlabel x-axis label
#' @param ymax maximum y-axis
#' @param ybreak y-axis breaks
#' @param base_size base font size in plot
#' @examples plot_correlation(proteins_1host)
#' @examples plot_correlation(protein_2hosts, base_size = 2, ybreak=1, ymax=10, host = 2)
#' @return A scatter plot
#' @importFrom ggplot2 ggplot geom_point aes labs scale_x_continuous scale_y_continuous theme_classic theme element_rect facet_grid
#' @importFrom grid unit
#' @importFrom dplyr vars
#' @export
plot_correlation <- function(df, 
                             host = 1 , 
                             alpha = 1/3, 
                             line_dot_size = 3, 
                             base_size = 11,
                             ylabel = "k-mer entropy (bits)\n", 
                             xlabel = "\nTotal variants (%)", 
                             ymax = ceiling(max(df$entropy)),
                             ybreak=0.5){
    totalVariants.incidence <- entropy <- NULL
    plot2<-ggplot(df)+geom_point(mapping = aes(x=totalVariants.incidence,y=entropy),alpha=alpha,size=line_dot_size)+
        labs(y = ylabel,x= xlabel)+
        scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20))+
        scale_y_continuous(limits = c(0, ymax), breaks = seq(0, ymax, ybreak))+
        theme_classic(base_size = base_size)+
        theme(
            panel.border = element_rect(colour = "#000000", fill=NA, linewidth=1)
        )

    if (host == 1){ #single host
        #plot the scatter plot with density
        plot2
    }else{ #multiple host
        plot2+facet_grid(rows = vars(df$host),space = "free",switch = "x")
    }

}
