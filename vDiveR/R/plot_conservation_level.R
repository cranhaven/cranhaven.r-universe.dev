#' Conservation Levels Distribution Plot
#'
#' This function plots conservation levels distribution of k-mer positions, which consists of
#' completely conserved (black) (index incidence = 100\%), highly conserved (blue)
#' (90\% <= index incidence < 100\%), mixed variable (green) (20\% < index incidence <= 90\%),
#' highly diverse (purple) (10\% < index incidence <= 20\%) and
#' extremely diverse (pink) (index incidence <= 10\%).
#'
#' @param df DiMA JSON converted csv file data
#' @param protein_order order of proteins displayed in plot
#' @param conservation_label 0 (partial; show present conservation labels only) or 1 (full; show ALL conservation labels) in plot
#' @param host number of host (1/2)
#' @param base_size base font size in plot
#' @param line_dot_size lines and dots size
#' @param label_size conservation labels font size
#' @param alpha any number from 0 (transparent) to 1 (opaque)
#' @examples plot_conservation_level(proteins_1host, conservation_label = 1,alpha=0.8, base_size = 15)
#' @examples plot_conservation_level(protein_2hosts, conservation_label = 0, host=2)
#' @return A plot
#' @importFrom dplyr case_when
#' @importFrom grid unit
#' @importFrom cowplot plot_grid
#' @export
plot_conservation_level <- function(df,
                                   protein_order=NULL,
                                   conservation_label=1,
                                   host=1,
                                   base_size = 11,
                                   line_dot_size = 2,
                                   label_size = 2.6,
                                   alpha=0.6){
    df <- df %>% data.frame() %>%
        dplyr::mutate(
        ConservationLevel = case_when(
            index.incidence == 100 ~ "Completely conserved (CC)",
            index.incidence >= 90 ~ "Highly conserved (HC)",
            index.incidence >= 20 ~ "Mixed variable (MV)",
            index.incidence >= 10  ~ "Highly diverse (HD)",
            index.incidence < 10 ~ "Extremely diverse (ED)"
        )
        )

    #single host
    if (host == 1){
        plot_plot7(data = df,protein_order = protein_order,conservation_label = conservation_label, base_size = base_size, label_size = label_size, line_dot_size = line_dot_size, alpha = alpha)
    }else{ #multihost

        #split the data into multiple subsets (if multiple hosts detected)
        plot7_list<-split(df,df$host)
        plot7_multihost<-lapply(plot7_list,plot_plot7, protein_order,conservation_label, base_size,  line_dot_size, label_size, alpha)

        # Remove legends from each individual plot
        plot7_no_legend <- lapply(plot7_multihost, function(p) p + theme(legend.position = "none"))
        # Combine the plots and add a single legend at the bottom
        combined_plot <- plot_grid(plotlist = plot7_no_legend,
                                ncol = 1)
        # Extract the legend from one of the plots (assuming all plots have the same legend)
        shared_legend <- cowplot::get_plot_component(plot7_multihost[[1]], "guide-box", return_all = TRUE)[[3]]
        # Combine the grid with the legend at the bottom
        plot_grid(combined_plot, shared_legend, ncol = 1, nrow = length(unique(df$host)), rel_heights = c(1, .1))
    }
}


#' @importFrom ggplot2 position_jitter scale_colour_manual
#' @importFrom ggplot2 position_dodge coord_cartesian geom_jitter
#' @importFrom gghalves geom_half_boxplot geom_half_point
#' @importFrom ggtext geom_richtext
#plotting function
plot_plot7<- function(data,
                      protein_order=NULL,
                      conservation_label=1,
                      base_size = 11,
                      line_dot_size = 2,
                      label_size = 2.6,
                      alpha =0.6){
    proteinName <- Total <- index.incidence <- NULL
    label <- ConservationLevel <- level_data <- NULL
    C_level<- c("Completely conserved (CC)",
                "Highly conserved (HC)",
                "Mixed variable (MV)",
                "Highly diverse (HD)",
                "Extremely diverse (ED)")


    #add word 'protein' in front of each protein name
    data$proteinName <- toupper(data$proteinName)
    data$proteinName<-paste("Protein",data$proteinName)
    #create data for proteome bar "ALL" from existing data
    data1<-data
    data1$proteinName <- "ALL"
    data1$level <- "ALL"
    #set up the order of proteins in plot from left to right
    if (is.null(protein_order) || protein_order ==""){ #follow the default order in csv file
        level<-c("ALL",unique(data$proteinName))
    }else{ #order the proteins based on user input
        protein_order<-toupper(protein_order)
        level<-c("ALL",paste("Protein",trimws(strsplit(protein_order, ',')[[1]])))
    }

    #determine the protein order
    data$level = factor(data$proteinName, levels=level)
    #combine proteome bar with protein bars
    data<-rbind(data1,data)
    #determine the protein order
    data$level = factor(data$proteinName, levels=level)

    #--- calculation for total and percentage of conservation levels for each protein ----
    #sum up the total positions for each conservation level of proteins
    plot7_data <- data %>%
        dplyr::group_by(proteinName, ConservationLevel) %>%
        dplyr::summarise(count = n()) %>%
        dplyr::ungroup()
    names(plot7_data)[3]<-"Total"

    #check the presence of conservation level: insert value 0 if it is absent
    if (conservation_label == 1){ #full label
        #check the presence of conservation level: insert value 0 if it is absent
        for ( conservation in C_level){ #conservation level
            for (name in level){ #proteinName
                if (!(conservation %in% plot7_data[plot7_data$proteinName==name,]$ConservationLevel)){
                    plot7_data<-rbind(plot7_data,c(name,conservation,0))
                }}}
    }

    #sort the dataframe
    plot7_data[order(plot7_data$proteinName),]
    plot7_data$Total<- as.integer(plot7_data$Total)
    #get the percentage of each conservation level for each protein
    plot7_data <- plot7_data %>%
        dplyr::group_by(proteinName) %>%
        dplyr::mutate(percent = Total / sum(Total) * 100) %>%
        dplyr::ungroup()

    #gather the protein label in multicolor
    plot7_data<-plot7_data%>%mutate(label = case_when(
        plot7_data$ConservationLevel == "Completely conserved (CC)" ~ paste0(sprintf("<span style =
    'color:#000000;'>CC: %.0f (%.1f %%) </span>",plot7_data$Total, round(plot7_data$percent,1))),
        plot7_data$ConservationLevel == "Highly conserved (HC)" ~ paste0(sprintf("<span style =
    'color:#0057d1;'>HC: %.0f (%.1f %%) </span>",plot7_data$Total, round(plot7_data$percent,1))),
        plot7_data$ConservationLevel == "Mixed variable (MV)" ~ paste0(sprintf("<span style =
    'color:#02d57f;'>MV: %.0f (%.1f %%) </span>",plot7_data$Total, round(plot7_data$percent,1))),
        plot7_data$ConservationLevel == "Highly diverse (HD)" ~ paste0(sprintf("<span style =
    'color:#A022FF;'>HD: %.0f (%.1f %%) </span>",plot7_data$Total, round(plot7_data$percent,1))),
        plot7_data$ConservationLevel == "Extremely diverse (ED)" ~ paste0(sprintf("<span style =
    'color:#ff617d;'>ED: %.0f (%.1f %%) </span>",plot7_data$Total, round(plot7_data$percent,1))),
    ))

    # set conservation level in specific order (CC,HC,MV,HD,ED)
    # Ensure 'conservation' is a factor with all the levels
    plot7_data$ConservationLevel <- factor(plot7_data$ConservationLevel,
                                        levels=C_level)
    plot7_data<-plot7_data[order(plot7_data$ConservationLevel),]

    #combine all conservation level labels into one for each protein
    protein_labels<- aggregate(label~proteinName, plot7_data, paste, collapse="<br>")
    #get number of protein for labelling
    nProtein<-nrow(protein_labels)

    #--- append 5 dummy rows with each represents one conservation level ----
    # to ensure all 5 levels are shown in legend
    # level_data = 1 for protein data to be plotted, 0 for dummy data
    data$level_data <- 1
    first_row <- data[1, ]

    # Replicate the first row 5 times, each representing a different level
    new_rows <- do.call(rbind, replicate(5, first_row, simplify = FALSE))
    new_rows$ConservationLevel <- factor(C_level)
    new_rows$level_data <- 0
    # Append the new rows to the original data
    data <- rbind(data, new_rows)
    data$ConservationLevel <- factor(data$ConservationLevel)

    #--- plotting ----
    ggplot(data %>% filter(level_data == 1) , aes(x=level,y=index.incidence)) +
        # if gghalves is installed, use a true half box; else, a normal box
        {
            if (requireNamespace("gghalves", quietly = TRUE)) {
                gghalves::geom_half_boxplot(outlier.shape = NA, side = "l")
            } else {
                ggplot2::geom_boxplot(outlier.shape = NA, width = 0.5)
            }
        } +
        geom_jitter(
            aes(x = as.numeric(level) + 0.22, colour = ConservationLevel),
            width = 0.12, height = 0,
            size = line_dot_size, alpha = alpha, show.legend = TRUE
        )+

       ylim(0,105) +
        labs(x=NULL, y="Index incidence (%)\n", color="Conservation level")+
        theme_classic(base_size = base_size)+
        theme(
            legend.key = element_rect(fill = "transparent", colour = "transparent"),
            legend.position = 'bottom',
            plot.margin = unit(c(1, 1, 1, 1), "lines"),
            axis.ticks.x = element_blank(),
            axis.text.x = element_text(vjust = 0.5, hjust=0.5)
        ) +
        scale_colour_manual('Conservation Level',
                            breaks = C_level,
                            values = c("Completely conserved (CC)"="black",
                                       "Highly conserved (HC)"="#0057d1",
                                       "Mixed variable (MV)"="#02d57f",
                                       "Highly diverse (HD)"="#8722ff",
                                       "Extremely diverse (ED)"="#ff617d"),
                            drop = FALSE) +
        coord_cartesian(clip = "off")+ #allow ggtext outside of the plot
        ggtitle(unique(data$host)) +
        theme(plot.title = element_text(margin=margin(b = 50, unit = "pt"))) +
        guides(color = guide_legend(override.aes = list(size = 2), nrow=2))+
        geom_richtext(data = protein_labels,
                      aes(x=proteinName,label = label, y=c(rep(105,nProtein)),
                          label.size=0, label.color="transparent"),
                      position = position_dodge(width=0.1),
                      size=label_size, color="black", hjust=0, angle=90)

}

