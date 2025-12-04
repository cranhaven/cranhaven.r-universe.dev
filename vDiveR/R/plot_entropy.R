#' Entropy plot
#'
#' This function plot entropy (black) and total variant (red) incidence of each
#' k-mer position across the studied proteins and highlight region(s) with zero entropy in yellow.
#' k-mer position with low support is marked with a red triangle underneath the x-axis line.
#'
#' @param df DiMA JSON converted csv file data
#' @param host number of host (1/2)
#' @param protein_order order of proteins displayed in plot
#' @param kmer_size size of the k-mer window
#' @param ymax maximum y-axis
#' @param line_size size of the horizontal (reference) line in plot
#' @param base_size word size in plot
#' @param all plot both the entropy and total variants (pass FALSE in to plot only the entropy)
#' @param highlight_zero_entropy highlight region with zero entropy (default: TRUE)
#' @return A plot
#' @examples plot_entropy(proteins_1host)
#' @examples plot_entropy(protein_2hosts, host = 2)
#' @importFrom ggplot2 geom_rect geom_area geom_hline geom_line facet_grid
#' @importFrom ggplot2 sec_axis element_line scale_colour_manual scale_linetype_manual
#' @export
plot_entropy <- function(df,
                         host=1,
                         protein_order="",
                         kmer_size=9, 
                         ymax = 10,
                         line_size=2,
                         base_size=8,
                         all= TRUE, 
                         highlight_zero_entropy=TRUE){
    entropy <- end <- lowSupportPos <- totalVariants.incidence <- NULL
    
    df$proteinName <- toupper(df$proteinName)

    #determine number of host
    #scale the amino acid position for each protein
    if (host ==1){ #single host
        if (protein_order =="" || is.null(protein_order) || protein_order == "NULL"){
            a<-table(df$proteinName)
            proteinName<-as.vector(names(a))
            position<-as.vector(a)
            df$size_f = factor(df$proteinName,levels = proteinName)
            scales_x<-mapply(function(x,y){
                x = scale_x_continuous(limits = c(0,y),breaks = seq(0,y,50))
            }, proteinName,position)
        }else{
            #order the proteins based on user input
            level<-strsplit(protein_order, ',')[[1]]
            level<- sapply(level, function(x) toupper(trimws(x)))
            position<-c()
            for (i in level){
                position<-append(position,table(df$proteinName)[names(table(df$proteinName)) == i])
            }
            df$size_f = factor(df$proteinName,levels = level)
            scales_x<-mapply(function(x,y){
                x = scale_x_continuous(limits = c(0,y),breaks = seq(0,y,50))
            }, level,position)
        }
    }else{ # multihost
        #categorise data based on host
        df$host<- factor(df$host)
        #count the aa length for each proteins (each host is expected to have same number of proteins with same length)
        df_sub<-df[df$host==unique(df$host[1]),]
        if (protein_order =="" || is.null(protein_order) || protein_order == "NULL"){
            a<-table(df_sub$proteinName)
            proteinName<-as.vector(names(a))
            position<-as.vector(a)
            df$size_f = factor(df$proteinName,levels = proteinName)
            scales_x<-mapply(function(x,y){
                x = scale_x_continuous(limits = c(0,y),breaks = seq(0,y,50))
            }, proteinName,position)
        }else{
            #order the proteins based on user input
            level<-strsplit(protein_order, ',')[[1]]
            level<- sapply(level, function(x) toupper(trimws(x)))
            position<-c()
            for (i in level){
                position<-append(position,table(df_sub$proteinName)[names(table(df_sub$proteinName)) == i])
            }
            df$size_f = factor(df$proteinName,levels = level)
            scales_x<-mapply(function(x,y){
                x = scale_x_continuous(limits = c(0,y),breaks = seq(0,y,50))
            }, level,position)
        }
    }

    #---------------set zero entropy region-----------------#
    #check if the kmer positions are zero entropy
    #Reference for getting all the positions of kmer based on kmer size and the starting position of kmer:
    #https://stackoverflow.com/questions/31120552/generate-a-sequence-of-numbers-with-repeated-intervals
    #https://stackoverflow.com/questions/41637518/adding-a-shaded-rectangle-to-an-existing-plot-using-geom-rect

    if(highlight_zero_entropy){
        #identify the kmer position with zero entropy
        #position: startPosition with zero entropy
        #end: startPosition + kmersize - 1
        df_zeroEntropy<- df %>%
            dplyr::group_by(proteinName)%>%
            dplyr::summarize(
                end = position[which(entropy == min(entropy))+kmer_size-1], #end: startPosition + kmersize - 1
                position = position[which(entropy == min(entropy))] #extract those starting positions with zero entropy
            )%>%
            as.data.frame()

        #concatenate df with df_zeroEntropy
        df <-merge(df,df_zeroEntropy,id="position",all=T)
        #replace NAN in column "zero entropy" with FALSE
        df$end[is.na(df$end)]<- -1
    }
    
    #---------------set maximum y limit-----------------#
    #if the max y value set by user is less than the max y value in data, then set the max y value to the max y value in data
    if (ymax < max(df$entropy) && max(df$entropy) > 10){
       ymax <- ceiling(max(df$entropy))
    } else if (ymax < 10){ # to ensure the reference hline is visible
        ymax <- 10
    }

    breaks_fun <- function(x) {
        seq(0,max(x),50)
    }

    limits_fun <- function(x) {
        c(0,max(x))
    }

    #----------------plotting--------------------#
    df$lowSupportPos <- -0.3
    df$lowSupportPos[df$lowSupport ==TRUE]<- -0.5

    plot1<-ggplot(df) +
        geom_area(mapping = aes(x = position, y = entropy,color= "k-mer Entropy", linetype="k-mer Entropy"), show.legend=F)+
        geom_hline(mapping = aes(yintercept=9.2, color = "Reference: Maximum Entropy (9.2) for HIV-1 Clade B (Env Protein)", linetype = "Reference: Maximum Entropy (9.2) for HIV-1 Clade B (Env Protein)"), linewidth= (line_size/10))+
        labs(y = "k-mer entropy (bits)\n",x= "\nk-mer position (aa)",color = "#f7238a")+
        scale_x_continuous(limits = limits_fun,breaks = breaks_fun)+
        theme_classic(base_size = base_size) +
        theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line.y.right = element_line(color = "#f7238a"),
            axis.ticks.y.right = element_line(color = "#f7238a"),
            axis.title.y.right =  element_text(color = "#f7238a"),
            legend.position="bottom"
        )+
        scale_colour_manual("",
                            values = c("Reference: Maximum Entropy (9.2) for HIV-1 Clade B (Env Protein)"="black",
                                        "k-mer Entropy" = "black"),
                            guide = guide_legend(override.aes=aes(fill=NA)))+
        scale_linetype_manual("",values=c("Reference: Maximum Entropy (9.2) for HIV-1 Clade B (Env Protein)"=5,
                                            "k-mer Entropy" = 1)) +
        ylim(0, ymax)

        
    # detect if low support present
    if (TRUE %in% df$lowSupport){
        plot1 <- plot1 +
            geom_point(mapping = aes(x = position,y=lowSupportPos),col=ifelse(df$lowSupportPos==-0.5, 'black', ifelse(df$lowSupportPos==-0.3, 'white', 'white')), alpha=ifelse(df$lowSupportPos==-0.5, 1, ifelse(df$lowSupportPos==-0.3, 0,0)),pch=17)
    }


    # highlight region with zero entropy    
    if(highlight_zero_entropy){
        plot1<- plot1+ 
            geom_rect(inherit.aes = FALSE,
            aes(xmin=position, xmax=end, ymin=-Inf, ymax=+Inf),
                fill='#FFECAF', alpha=ifelse(df$end == -1, 0, 0.5))
    }

    # plot both entropy and total variants
    if(all){
        plot1<- plot1 + geom_line(mapping = aes(x = position, y = totalVariants.incidence * ymax / 100, color = "Total Variants",linetype="Total Variants"), linewidth= (line_size/10) )+
        geom_hline(mapping = aes( yintercept=98* ymax / 100, color = "Reference: Maximum Total Variants (98%) for HIV-1 Clade B (Env Protein)",linetype ="Reference: Maximum Total Variants (98%) for HIV-1 Clade B (Env Protein)"), linewidth= (line_size/10))+
        #how to second y-axis: https://whatalnk.github.io/r-tips/ggplot2-rbind.nb.html
        scale_y_continuous(sec.axis = sec_axis(~ . * 100 / ymax , name = "Total variants (%)",breaks = c(0,25,50,75,100),labels=c("0","25","50","75","100")),
                            breaks = seq(0.0, ymax, length.out = 5),labels= sprintf(seq(0.0, ymax, length.out = 5), fmt = "%.1f")) +
        scale_colour_manual("",
                            values = c("Reference: Maximum Total Variants (98%) for HIV-1 Clade B (Env Protein)"="#f7238a",
                                        "Reference: Maximum Entropy (9.2) for HIV-1 Clade B (Env Protein)"="black",
                                        "k-mer Entropy" = "black",
                                        "Total Variants"="#f7238a"),
                            guide = guide_legend(override.aes=aes(fill=NA)))+
        scale_linetype_manual("",values=c("Reference: Maximum Total Variants (98%) for HIV-1 Clade B (Env Protein)"=5,
                                            "Reference: Maximum Entropy (9.2) for HIV-1 Clade B (Env Protein)"=5,
                                            "k-mer Entropy" = 1,
                                            "Total Variants"=1))
    }

    #number of host
    if (host == 1){ #one host
        plot1 +facet_grid(cols=vars(df$size_f),scales = 'free',space = "free",switch = "x")
    }else{ # multi host
        plot1 +facet_grid(rows = vars(df$host),cols=vars(df$size_f),scales = 'free',space = "free",switch = "both")
    }

}
