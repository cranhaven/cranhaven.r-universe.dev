#' Time Distribution of Sequences Plot
#'
#' This function plots the time distribution of provided sequences in the form of bar plot with 'Month' as x-axis and
#' 'Number of Sequences' as y-axis. Aside from the plot, this function also returns a dataframe with 2 columns: 'Date' and 'Number of sequences'.
#' The input dataframe of this function is obtainable from metadata_extraction(), with NCBI Protein / GISAID (EpiFlu/EpiCoV/EpiPox/EpiArbo) FASTA file as input.
#'
#' @param metadata a dataframe with 3 columns, 'ID', 'region', and 'date'
#' @param base_size word size in plot
#' @param date_format date format of the input dataframe
#' @param date_break date break for the scale_x_date
#' @param scale plot counts or log scale the data
#'
#' @return A single plot or a list with 2 elements (a plot followed by a dataframe, default)
#' @examples time_plot <- plot_time(metadata, date_format="%d/%m/%Y")$plot
#' @examples time_df <- plot_time(metadata, date_format="%d/%m/%Y")$df
#' @importFrom ggplot2 scale_x_date geom_bar scale_y_log10
#' @importFrom scales date_format trans_breaks trans_format math_format
#' @importFrom stringr str_to_title
#' @importFrom magrittr %>%
#' @importFrom dplyr filter group_by summarize
#' @export
plot_time <- function(metadata, date_format = "%Y-%m-%d", base_size=8,
                      date_break = "2 month", scale = "count"){
    Month <- .x <- Date <- NULL
    colnames(metadata) <- stringr::str_to_title(colnames(metadata))
    
    dates_3rows<-paste(metadata$Date[1:3], collapse = ", ")
    metadata <- metadata %>%
        filter(!is.na(as.Date(metadata$Date, format=date_format, na.rm = TRUE)))

    if (nrow(metadata) < 1){
        error_msg <- paste("No records remain after filtering out those not in date format of", date_format, ". Have a peek on some 'Date' values:", dates_3rows)
        return(list(plot = NULL, df = error_msg))
    }

    metadata$Month <- as.Date(cut(as.Date(metadata$Date, format = date_format),
                                  breaks = "month"))

    p <- ggplot(data = metadata, aes(x = Month)) + 
        geom_bar() +
        ylab('Number of Sequences') +
        scale_x_date(date_breaks = date_break, labels = scales::date_format("%b %Y"))  +
        theme_classic(base_size = base_size) +
        theme(axis.text.x = element_text(angle = 90, vjust = .5))

    if (scale == 'log') {
        p <- p + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                 labels = trans_format("log10", scales::math_format(10^.x)))
    }

    final_df <- metadata %>%
        dplyr::group_by(Date) %>%
        dplyr::summarize("Number of Sequences" = n()) %>%
        as.data.frame()

    return(list(plot=p,df=final_df))
}

