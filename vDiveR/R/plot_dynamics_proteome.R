#' Dynamics of Diversity Motifs (Proteome) Plot
#'
#' This function compactly display the dynamics of diversity motifs (index and its variants: major, minor and unique)
#' in the form of dot plot as well as violin plot for all the provided proteins at proteome level.
#'
#' @param df DiMA JSON converted csv file data
#' @param host number of host (1/2)
#' @param line_dot_size size of dot in plot
#' @param base_size word size in plot
#' @param alpha any number from 0 (transparent) to 1 (opaque)
#' @param bw smoothing bandwidth of violin plot (default: nrd0)
#' @param adjust adjust the width of violin plot (default: 1)
#' @return A plot
#' @examples plot_dynamics_proteome(proteins_1host)
#' @importFrom cowplot plot_grid
#' @export
plot_dynamics_proteome <- function(df,
                                   host=1,
                                   line_dot_size=2,
                                   base_size=10,
                                   alpha=1/3,
                                   bw = "nrd0",
                                   adjust = 1){
    #single host
    if (host == 1){
        generate_plots(data=df, base_size= base_size, alpha=alpha, line_dot_size=line_dot_size, bw = bw, adjust = adjust)
    }else{ #multihost
        #split the data into multiple subsets (if multiple hosts detected)
        data_list<-split(df,df$host)
        multihost_plots <- lapply(data_list, function(df) {
            generate_plots(df, line_dot_size = line_dot_size, base_size = base_size, 
                    alpha = alpha, bw = bw, adjust = adjust, host = host)
        })
        
        #create spacing between multihost plots
        theme = theme(plot.margin = unit(c(0,0.1,0,0), "cm"))
        plot_grid(plotlist = lapply(multihost_plots, '+', theme),
                  ncol = length(unique(df$host)))
    }

}

#' @importFrom ggplot2 element_blank facet_wrap scale_colour_manual
#' @importFrom ggplot2 guides guide_legend scale_color_manual ggtitle element_text geom_violin geom_boxplot ylim scale_color_grey margin scale_fill_manual
#' @importFrom ggpubr annotate_figure ggarrange text_grob
generate_plots<-function(data,
                        line_dot_size=2,
                        base_size=10,
                        host = 1,
                        alpha=1/3,
                        bw = "nrd0",
                        adjust = 1){

    Total_Variants <- Incidence <- Group <- x <- NULL
    df<-data.frame()
    group_names<-c("Index",
                   "Major", "Minor", "Unique",
                   "Total variants","Distinct variants")

    #transpose the data format
    for (i in 7:12){
        tmp<-data.frame(proteinName=data[1],
                        position=data[2],
                        incidence=data[i],
                        total_variants=data[11],
                        Group=group_names[i-6],
                        Multiindex=data[13])
        names(tmp)[3]<-"Incidence"
        names(tmp)[4]<-"Total_Variants"
        df<-rbind(df,tmp)
    }

    df_violin<-df
    minor<-rbind(df[df$Group == "Index",],df[df$Group == "Total variants",])
    uniq<-rbind(df[df$Group == "Index",],df[df$Group == "Total variants",])
    minor$motif<- "Minor"
    uniq$motif<-"Unique"

    df<-df%>%mutate(motif = case_when(
        df$Group == "Index" ~ "Major",
        df$Group == "Total variants" ~ "Major",
        df$Group == "Major" ~ "Major",
        df$Group == "Minor"  ~ "Minor",
        df$Group == "Unique" ~ "Unique",
        df$Group == "Distinct variants" ~ "Distinct variants"
    ))
    df<- rbind(df,minor,uniq)
    df$motif<-factor(df$motif,levels = c("Major","Minor","Unique","Distinct variants"))

    #plotting 3a
    proteins_point_plot<-ggplot()+geom_point(df,mapping=aes(x=Total_Variants,y=Incidence,color=Group),alpha=alpha,size= line_dot_size)+
        geom_point(df,mapping = aes(x =Total_Variants,y=Incidence),col=ifelse(df$multiIndex== TRUE & df$Group== "Index", 'red', ifelse(df$multiIndex== FALSE, 'white', 'white')), alpha=ifelse(df$multiIndex ==TRUE & df$Group== "Index", 1, ifelse(df$multiIndex== TRUE, 0,0)),pch=1,size=3,stroke=1.05)+ #multiIndex
        scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20))+
        scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20))+
        theme_classic(base_size = base_size)+
        theme(
            panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
            strip.text.x = element_blank(),
            legend.position="bottom")+
        labs(y= "Incidence (%)", x="\nTotal variants (%)")+
        facet_wrap(~ motif,ncol = 1)+
        guides(colour = guide_legend(override.aes = list(alpha = 1,size=2), nrow = host, byrow=T, keywidth = 1, keyheight = .1 ))+
        scale_colour_manual('',breaks=c("Index","Total variants","Major","Minor","Unique","Distinct variants"),
                            values = c("Index"="black", "Total variants"="#f7238a","MultiIndex"="red",
                                       "Major"="#37AFAF" , "Minor"="#42aaff","Unique"="#af10f1", "Distinct variants"="#c2c7cb"))
    #host label
    if("host" %in% colnames(data)){
        proteins_point_plot<-proteins_point_plot+ggtitle(unique(data$host))+theme(plot.title = element_text(hjust = 0.5))
    }

    #PLOT 3b
    index<-df_violin[df_violin$Group %in% "Index",]
    nonatypes<-df_violin[df_violin$Group %in% "Distinct variants",]
    variants<-df_violin[df_violin$Group %in% c("Major","Minor","Unique"),]
    variants$x<-"x"
    variants_max_yaxis<-ceiling((max(as.numeric(variants$Incidence))/10))*10

    #plot 3b
    violin_plot_index<-ggplot(index, aes(x=Group,y=Incidence))+
        geom_violin(color="black",fill="black",alpha=0.9, adjust=adjust, bw=bw)+
        geom_boxplot(width=0.08,alpha=0.20,fill="white",outlier.shape=NA,color="white")+
        ylim(c(0,100))+
        labs(y=NULL,x="Index")+
        theme_classic(base_size = base_size)+
        theme(
            panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
            axis.text.x  = element_blank(),
            axis.ticks.x = element_blank())+
        scale_color_grey()

    violin_plot_tv<-ggplot(index, aes(x=Group,y=Total_Variants))+
        geom_violin(color="#f7238a",fill="#f7238a", alpha=0.9, adjust=adjust, bw=bw)+
        geom_boxplot(width=0.08,alpha=0.20,color="black",fill="white",outlier.shape=NA)+
        ylim(c(0,100))+
        labs(y=NULL,x="Total Variants")+
        theme_classic(base_size = base_size)+
        theme(
            plot.margin = margin( t=5,
                                  b=5,
                                  r = -0.25),
            panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
            axis.text=element_text(colour="white"),
            axis.text.x  = element_blank(),
            axis.ticks = element_blank())

    violin_plot_nonatype<-ggplot(nonatypes, aes(x=Group,y=Incidence))+
        geom_violin(color="#c2c7cb",fill="#c2c7cb", alpha=0.9, adjust=adjust, bw=bw)+
        geom_boxplot(width=0.08,alpha=0.20,fill="white",outlier.shape=NA)+
        ylim(c(0,100))+
        labs(y=NULL,x="Distinct variants")+
        theme_classic(base_size = base_size)+
        theme(
            panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
            axis.text=element_text(colour="white"),
            axis.text.x  = element_blank(),
            axis.ticks = element_blank())

    violin_plot_variants<-ggplot(variants)+
        geom_violin(mapping=aes(x=x,y=Incidence,color=Group,fill=Group),alpha=0.9, adjust=adjust, bw=bw)+
        geom_boxplot(mapping = aes(x=x,y=Incidence),width=0.08,alpha=0.20,fill="white",outlier.shape=NA)+
        scale_y_continuous(position = "right",limits = c(0,variants_max_yaxis))+
        theme_classic(base_size = base_size-2)+
        labs(y=NULL,x=NULL)+
        facet_wrap(Group ~ .,ncol=1,strip.position ="right")+
        theme(strip.placement = "outside",
              panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
              axis.text.x=element_blank (),
              axis.ticks.x=element_blank (),
              panel.spacing = unit(0, "lines"),
              strip.background=element_blank (),
              axis.text.y = element_text(size=base_size/2-1),
              plot.margin = margin(t=5,l = -0.1)
        )+
        scale_colour_manual('',values = c( "Major"="#37AFAF","Minor"="#42aaff","Unique"="#af10f1" ))+
        scale_fill_manual('',values = c( "Major"="#37AFAF","Minor"="#42aaff","Unique"="#af10f1" ))+
        guides(fill="none",color='none')

    #annotate the violin plot
    violin_plot_variants<-annotate_figure(violin_plot_variants,bottom = text_grob("a\n",size=ceiling(base_size/2) ,color = "white"))
    proteins_violin_plot<-ggarrange(violin_plot_index, violin_plot_tv,violin_plot_variants, violin_plot_nonatype, ncol=4,widths = c(1,0.9,0.95,1))
    proteins_violin_plot<-annotate_figure(proteins_violin_plot,left = text_grob("Incidence (%)",size=base_size,rot=90,hjust=0.3))
    #combine both the top scatter plot and bottom violin plot
    ggarrange(proteins_point_plot,proteins_violin_plot,ncol=1,heights = c(1,0.3))

}





