#' Dynamics of Diversity Motifs (Protein) Plot
#'
#' This function compactly display the dynamics of diversity motifs (index and its variants: major, minor and unique)
#' in the form of dot plot(s) as well as violin plots for all the provided individual protein(s).
#'
#' @param df DiMA JSON converted csv file data
#' @param host number of host (1/2)
#' @param protein_order order of proteins displayed in plot
#' @param base_size base font size in plot
#' @param alpha any number from 0 (transparent) to 1 (opaque)
#' @param line_dot_size dot size in scatter plot
#' @param bw smoothing bandwidth of violin plot (default: nrd0)
#' @param adjust adjust the width of violin plot (default: 1)
#' @return A plot
#' @examples plot_dynamics_protein(proteins_1host)
#' @importFrom cowplot plot_grid
#' @export
plot_dynamics_protein<-function(df, 
                                host=1, 
                                protein_order=NULL, 
                                base_size=8, 
                                alpha = 1/3, 
                                line_dot_size = 3,
                                bw = "nrd0",
                                adjust = 1){
    #single host
    if (host == 1){
        generate_protein_plots(data=df, protein_order=protein_order, base_size=base_size,alpha=alpha,  line_dot_size=line_dot_size, bw = bw, adjust = adjust)
    }else{ #multihost
        #split the data into multiple subsets (if multiple hosts detected)
        data_list<-split(df,df$host)
        multihost_plots <- lapply(data_list, function(df) {      
          generate_protein_plots(df, protein_order = protein_order, base_size = base_size, alpha = alpha,
                  line_dot_size = line_dot_size, bw = bw, adjust = adjust, host=host)
        })

        plot_grid(plotlist = multihost_plots,
              ncol = length(unique(df$host)))
    }
}

#' @importFrom ggplot2 guides guide_legend scale_colour_manual ggtitle element_text
#' @importFrom ggplot2 geom_violin geom_boxplot ylim scale_color_grey margin element_line
#' @importFrom ggplot2 scale_fill_manual theme_bw facet_grid xlab ylab
#' @importFrom ggpubr annotate_figure ggarrange text_grob
#' @importFrom cowplot plot_grid
generate_protein_plots<-function(data, protein_order=NULL,alpha=1/3, line_dot_size=3, base_size=8, host=1, bw = "nrd0", adjust = 1){
    Total_Variants <- Incidence <- Group <- x <- proteinName <- entropy <- NULL

    plot4_data<-data.frame()
    group_names<-c("Index",
                   "Major","Minor",
                   "Unique",
                   "Total variants","Distinct variants")

    for (i in 7:12){
        tmp<-data.frame(proteinName=data[1],position=data[2],incidence=data[i],total_variants=data[11],Group=group_names[i-6],Multiindex=data[13])

        names(tmp)[3]<-"Incidence"
        names(tmp)[4]<-"Total_Variants"
        plot4_data<-rbind(plot4_data,tmp)
    }

    plot4_data$proteinName <- toupper(plot4_data$proteinName)
    if (!is.null(protein_order) && protein_order != ""){
        #order the proteins based on user input
        protein_order <- toupper(trimws(protein_order))
        level<-strsplit(protein_order, ',')[[1]]
        level <- sapply(level, function(x) toupper(trimws(x)))
        #set protein order as factor
        
        plot4_data$proteinName<-factor(plot4_data$proteinName, levels=level)
        plot4_data$size_f = factor(plot4_data$proteinName,levels = level)
    }
    plot4_data$Group<-factor(plot4_data$Group, levels=c("Index","Total variants", "Major", "Minor", "Unique", "Distinct variants"))
    plot5_data<-plot4_data

    #plot plot 4
    plot4<-ggplot()+geom_point(plot4_data,mapping=aes(x=Total_Variants,y=Incidence,color=Group),alpha=alpha,size=line_dot_size)+
        scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20))+
        scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20))+
        labs(y = "Incidence (%)",x= NULL)+
        theme_classic(base_size = base_size)+
        theme(
            legend.background = element_rect(fill = "transparent"),
            panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
            legend.position = "bottom"
        )+ guides(colour = guide_legend(override.aes = list(alpha = 1,size=2),keywidth = 1,keyheight = 0.1,nrow=1, byrow=TRUE))+
        scale_colour_manual('',values = c("Index"="black","Total variants"="#f7238a", "Major"="#37AFAF","Minor"="#42aaff","Unique"="#af10f1","Distinct variants"="#c2c7cb" ))
    plot4<-plot4+facet_grid(cols=vars(plot4_data$proteinName))

    #host label
    if("host" %in% colnames(data)){
        plot4<-plot4+ggtitle(unique(data$host))+
            theme(plot.title = element_text(hjust = 0.5))
    }
    
    if (length(unique(data$proteinName)) <=10){
      #prepare the data for each subplot of plot5
      index<-plot5_data[plot5_data$Group %in% c("Index"),]
      major<-plot5_data[plot5_data$Group %in% c("Major"),]
      minor<-plot5_data[plot5_data$Group %in% c("Minor"),]
      unique<-plot5_data[plot5_data$Group %in% c("Unique"),]
      nonatypes<-plot5_data[plot5_data$Group %in% c("Distinct variants"),]
      variants_max_yaxis<-ceiling((max(as.numeric(major$Incidence),as.numeric(minor$Incidence),as.numeric(unique$Incidence))/10))*10
  
      #plot 5
      plot5_index<-ggplot(index, aes(x=proteinName, y=Incidence))+
          geom_violin(fill="black",trim = TRUE, color="black",alpha=0.9, adjust=adjust, bw=bw)+ylim(0,100)+ylab("Index k-mer (%)")+xlab("") +theme_bw() +
          geom_boxplot(outlier.shape = NA,width=0.05, color="white",alpha=0.15,fill="white")+
          theme_classic(base_size = base_size)+
          theme(plot.margin = unit(c(0,0.1,0,0.1), "cm"),
                panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
                axis.ticks.x = element_blank())
  
      plot5_tv<-ggplot(index, aes(x=proteinName, y=Total_Variants))+
          geom_violin(fill="#f7238a",trim = TRUE, color="#f7238a",alpha=0.9, adjust=adjust, bw=bw)+ylim(0,100)+ylab("Total variant (%)")+xlab("") +theme_bw() +
          geom_boxplot(outlier.shape = NA,width=0.05, color="black",alpha=0.15,fill="white")+
          theme_classic(base_size = base_size)+
          theme(plot.margin = unit(c(0,0.1,0.1,0.1), "cm"),
                panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
                axis.ticks.x = element_blank())
  
      plot5_major<-ggplot(major, aes(x=proteinName, y=Incidence)) +
          geom_violin(fill="#37AFAF",trim = TRUE, color="#37AFAF", adjust=adjust, bw=bw)+ylim(0,variants_max_yaxis)+ylab("Major variant (%)")+xlab("")+theme_bw() +
          geom_boxplot(outlier.shape = NA,width=0.04, color="black", alpha=0.15,fill="white")+
          theme_classic(base_size = base_size)+
          theme(plot.margin = unit(c(0,0.1,0,0.1), "cm"),
                panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
                axis.ticks.x = element_blank(),
                axis.text.y  = element_text(face="bold"))
  
      plot5_minor<-ggplot(minor, aes(x=proteinName, y=Incidence))+
          geom_violin(fill="#42aaff",trim = TRUE,color="#42aaff", adjust=adjust, bw=bw)+ylim(0,variants_max_yaxis)+ylab("Minor variants (%)")+xlab("") +theme_bw() +
          geom_boxplot(outlier.shape = NA,width=0.04, color="black", alpha=0.15,fill="white")+
          theme_classic(base_size = base_size)+
          theme(plot.margin = unit(c(0,0.1,0,0.1), "cm"),
                panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
                axis.ticks.x = element_blank(),
                axis.text.y  = element_text(face="bold"))
  
      plot5_unique<-ggplot(unique, aes(x=proteinName, y=Incidence)) +
          geom_violin(fill="#af10f1",trim = TRUE, color="#af10f1", adjust=adjust, bw=bw)+ylim(0,variants_max_yaxis)+ylab("Unique variants (%)")+xlab("")+theme_bw() +
          geom_boxplot(outlier.shape = NA,width=0.05, color="black", alpha=0.15,fill="white")+
          theme_classic(base_size = base_size)+
          theme(plot.margin = unit(c(0,0.1,0,0.1), "cm"),
                panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
                axis.ticks.x = element_blank(),
                axis.text.y  = element_text(face="bold"))
  
      plot5_nonatypes<-ggplot(nonatypes, aes(x=proteinName, y=Incidence)) +
          geom_violin(fill="#c2c7cb",trim = TRUE, color="#c2c7cb", adjust=adjust, bw=bw)+ylim(0,100)+ylab("Distinct variants (%)")+xlab("")+theme_bw()+
          geom_boxplot(outlier.shape = NA,width=0.05, color="black", alpha=0.15,fill="white") +
          theme_classic(base_size = base_size)+
          theme(plot.margin = unit(c(0,0.1,0,0.1), "cm"),
                panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
                axis.line.x = element_line(linewidth = 0.5, linetype = "solid", colour = "black"),
                axis.ticks.x = element_blank())
      plot5<-ggarrange(plot5_index,plot5_tv,plot5_nonatypes,plot5_major,plot5_minor,plot5_unique,ncol=3,nrow=2)
    } else {
      plot5_data$Group[plot5_data$Group == "Index"] <- "Index k-mer" 
      plot5_data$Group[plot5_data$Group == "Major"] <- "Major variant" 
      plot5_data$Group[plot5_data$Group == "Minor"] <- "Minor variants" 
      plot5_data$Group[plot5_data$Group == "Unique"] <- "Unique variants" 
      
      plot5_data$Group<-factor(plot5_data$Group, levels=c("Index k-mer","Total variants", "Distinct variants", "Major variant", "Minor variants", "Unique variants"))
      variants<-subset(plot5_data, Group=="Major variant" | Group=="Minor variants" | Group=="Unique variants")
      max_ylim<-ceiling((max(variants$Incidence)/10))*10
      
      breaks_fun <- function(x) {
        if (max(x)<= max_ylim){
          seq(0,max_ylim,10)
        }else{
          seq(0,100,20)
        }
      }
      
      limits_fun <- function(x) {
        if (max(x)<= max_ylim){
          c(0,max_ylim)
        }else{
          c(0,100)
        }
      }
      
      plot5<-ggplot()+
        geom_violin(data=plot5_data,aes(x=proteinName,y=Incidence, fill=Group, color=Group), trim=TRUE, adjust=adjust, bw=bw)+
        theme_classic(base_size = base_size)+xlab("Protein")+ylab("Incidence (%)\n")+
        theme(panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
              legend.position="none")+
        scale_y_continuous(limits = limits_fun,breaks = breaks_fun)+
        facet_grid(rows = vars(Group),switch="y",scales = 'free')+
        scale_colour_manual('',values = c("Index k-mer"="black","Total variants"="#f7238a", "Major variant"="#37AFAF","Minor variants"="#42aaff","Unique variants"="#af10f1","Nonatypes"="#c2c7cb" ))+
        scale_fill_manual('',values = c("Index k-mer"="black","Total variants"="#f7238a", "Major variant"="#37AFAF","Minor variants"="#42aaff","Unique variants"="#af10f1","Nonatypes"="#c2c7cb" ))
      
    }
    
    #plot4_5
    plot_grid(plot4, plot5, ncol = 1, rel_heights = c(1, 0.5))
}

