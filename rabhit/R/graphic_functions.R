# HaplotypeR graphic functions -----------------------------------------------------

#' @include rabhit.R
#' @include internal_functions.R
NULL

#' Graphical output of an inferred haplotype
#'
#' The \code{plotHaplotype} functions visualizes an inferred haplotype.
#'
#'
#' @param    hap_table            haplotype summary table. See details.
#' @param    html_output          if TRUE, a html5 interactive graph is outputed. Default is FALSE.
#' @param    genes_order           A vector of the genes by the desired order. Default is by GENE.loc
#' @param    text_size            the size of graph labels. Default is 14 (pts).
#' @param    removeIGH            if TRUE, 'IGH'\'IGK'\'IGL'\'TRB' prefix is removed from gene names.
#' @param    plotYaxis            if TRUE, Y axis labels (gene names) are plotted on the middle and right plots. Default is TRUE.
#' @param    chain                the Ig/TR chain: IGH,IGK,IGL,TRB. Default is IGH.
#' @param    dir                  The output folder for saving the haplotype map for multiple individuals.
#'
#' @return
#'
#' A haplotype map visualization. If more than one subject is visualized, a pdf is created. If html_output is TRUE, a folder named html_output is created with individual graphs.
#'
#' @details
#'
#' A \code{data.frame} in a haplotype format created by \code{createFullHaplotype} function.
#'
#' @examples
#'
#' # Selecting a single individual from the haplotype samples data
#' haplo_db = samplesHaplotype[samplesHaplotype$subject=='I5', ]
#'
#' # plot haplotype
#' plotHaplotype(haplo_db)
#'
#' @export
plotHaplotype <-
  function(hap_table,
           html_output = FALSE,
           genes_order = NULL,
           text_size = 14,
           removeIGH = TRUE,
           plotYaxis = TRUE,
           chain = c("IGH",
                     "IGK", "IGL",  "TRB"),
           dir) {
    if (missing(chain)) {
      chain = "IGH"
    }
    chain <- match.arg(chain)

    if (is.null(genes_order)) {
      genes_order <- GENE.loc[[chain]]
    }

    id_GENE_col <- which(names(hap_table) == "gene")
    hapBy_col_id <- c(id_GENE_col + 1, id_GENE_col + 2)
    hapBy_cols = names(hap_table)[c(id_GENE_col + 1, id_GENE_col + 2)]

    hapBy_alleles = gsub("_", "*", hapBy_cols)

    if (!("subject" %in% names(hap_table))) {
      hap_table$subject <- rep("S1", nrow(hap_table))
    }

    plot_list <- c()
    for (sample_name in unique(hap_table$subject)) {
      haplo.db <-
        parseHapTab(
          hap_table[hap_table$subject == sample_name &
                      grepl(chain, hap_table$gene),],
          chain = chain,
          sample_name = sample_name,
          hapBy_cols = hapBy_cols,
          hapBy_alleles = hapBy_alleles
        )

      geno.df <-
        sortDFByGene(
          haplo.db$geno.df,
          chain = chain,
          genes_order = genes_order,
          removeIGH = removeIGH
        )

      kval.df <-
        sortDFByGene(
          haplo.db$kval.df,
          chain = chain,
          genes_order = genes_order,
          removeIGH = removeIGH
        )

      count.df <-
        sortDFByGene(
          haplo.db$count.df,
          chain = chain,
          genes_order = genes_order,
          removeIGH = removeIGH
        )

      ########################################################################################################

      ### Prepare All panels
      geno.df$ALLELE_TEXT <- as.character(geno.df$alleles)
      count.df$ALLELE_TEXT <- as.character(count.df$alleles)


      if (length(grep("[0-9][0-9]_[0-9][0-9]", geno.df$alleles)) != 0) {
        geno.df <-
          as.data.frame(
            geno.df %>% dplyr::group_by(.data$hapBy, .data$gene) %>% dplyr::mutate(n = dplyr::n())
          )
        geno.df$freq <-
          ifelse(geno.df$n == 2, 0.5, ifelse(geno.df$n != 1, 0.25, 1))
        non_reliable_alleles_text <-
          nonReliableAllelesText_V2(non_reliable_alleles_text = geno.df[grep("[0-9][0-9]_[0-9][0-9]", geno.df$alleles),])
      } else {
        non_reliable_alleles_text <- c()
      }

      geno.df$ALLELE_TEXT <- sapply(1:nrow(geno.df), function(i) {
        if (grepl("[0-9][0-9]_[0-9][0-9]", geno.df$ALLELE_TEXT[i])) {
          unique(non_reliable_alleles_text$text_bottom[geno.df$gene[i] == non_reliable_alleles_text$gene &
                                                         grepl(geno.df$ALLELE_TEXT[i],
                                                               non_reliable_alleles_text$text_bottom)])
        } else{
          geno.df$ALLELE_TEXT[i]
        }
      })

      count.df$ALLELE_TEXT <- sapply(1:nrow(count.df), function(i) {
        if (grepl("[0-9][0-9]_[0-9][0-9]", count.df$ALLELE_TEXT[i])) {
          unique(non_reliable_alleles_text$text_bottom[count.df$gene[i] == non_reliable_alleles_text$gene &
                                                         grepl(count.df$ALLELE_TEXT[i],
                                                               non_reliable_alleles_text$text_bottom)])
        } else{
          count.df$ALLELE_TEXT[i]
        }
      })

      ## Middle panel
      geno.df$alleles[grep("[0-9][0-9]_[0-9][0-9]", geno.df$alleles)] <-
        "NRA"

      allele_palette <- alleleHapPalette(geno.df$alleles)
      AlleleCol <- allele_palette$AlleleCol
      transper <- allele_palette$transper

      geno.df$alleles <-
        factor(geno.df$alleles, levels = AlleleCol)
      geno.df$text <-
        paste0("Gene: ",
               geno.df$gene,
               '<br />',
               "Allele: ",
               geno.df$ALLELE_TEXT)
      #options(warn = -1)
      p = ggplot() +
        geom_bar(
          data = geno.df,
          mapping = aes_string(x = "gene", fill = "alleles", text = "text"),
          position = "fill",
          width = 0.9,
          na.rm = T
        ) + coord_flip() +
        xlab("") + ylab("") + facet_grid(paste0(".~", "hapBy"), switch = "x") + theme_bw() + theme(
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          text = element_text(size = text_size),
          strip.background = element_blank(),
          strip.text = element_text(face = "bold"),
          axis.text = element_text(colour = "black"),
          panel.spacing = unit(0, "cm"),
          strip.switch.pad.grid = unit(0,
                                       "cm"),
          plot.margin = unit(c(0.25, 0, 0.2, 0), "cm"),
          legend.key = element_rect("#DCDCDC")
        ) + scale_fill_manual(
          values = alpha(names(AlleleCol), transper),
          name = "Alleles",
          drop = F
        )

      if (!plotYaxis) {
        p = p + theme(axis.text.y = element_blank())
      }

      ## Right panel plot K values
      kval.df$text <-
        paste0(
          "Gene: ",
          kval.df$gene,
          '<br />',
          "lK: ",
          round(as.numeric(kval.df$k), 4),
          '<br />',
          "lK group: ",
          kval.df$K_GROUPED
        )
      pk <-
        ggplot(kval.df,
               aes_string(x = "gene", fill = "K_GROUPED", text = "text")) + theme_bw() + theme(
                 axis.ticks = element_blank(),
                 axis.text = element_blank(),
                 axis.title = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 text = element_text(size = text_size),
                 strip.background = element_blank(),
                 strip.text = element_text(face = "bold"),
                 panel.spacing = unit(0, "cm"),
                 strip.switch.pad.grid = unit(0, "cm"),
                 plot.margin = unit(c(0.25, 0,
                                      0.2, 0), "cm"),
                 legend.key = element_rect("#DCDCDC")
               ) + geom_bar(position = "fill",
                            width = 0.7,
                            na.rm = T) + coord_flip() + xlab("") + ylab("") + facet_grid(paste0(".~", "hapBy"),
                                                                                         switch = "x")

      count.df$alleles <- as.character(count.df$alleles)
      count.df$alleles[grep("[0-9][0-9]_[0-9][0-9]", count.df$alleles)] <-
        "NRA"
      count.df$border <-
        factor(ifelse(count.df$alleles == "NRA", "black", "white"),
               levels = c("black", "white"))
      count.df$alleles <-
        factor(count.df$alleles, levels = AlleleCol)
      count.df$text <-
        paste0(
          "Gene: ",
          count.df$gene,
          '<br />',
          "Allele: ",
          count.df$ALLELE_TEXT,
          '<br />',
          "Count: ",
          round(count.df$count, 4),
          '<br />',
          "log<sub>10</sub>(Count+1):",
          abs(round(count.df$count2, 4))
        )
      ## Left panel
      p2 <-
        ggplot(count.df,
               aes_string(
                 x = "gene",
                 y = "count2",
                 fill = "alleles",
                 text = "text"
               )) +
        geom_bar(
          stat = "identity",
          position = "Dodge",
          width = 0.9,
          na.rm = T,
          aes_string(colour = "border")
        ) + coord_flip() + cowplot::background_grid(minor = "none") +
        scale_fill_manual(
          values = alpha(names(AlleleCol),
                         transper),
          name = "alleles",
          drop = F
        ) + scale_color_manual(values = alpha(c("black", "white"), c(0.5, 0)), drop = F) +
        theme(
          legend.position = "none",
          strip.text = element_text(face = "bold"),
          axis.text = element_text(colour = "black"),
          text = element_text(size = text_size),
          plot.margin = unit(c(0.25,
                               0,-0.05, 0), "cm"),
          panel.background = element_blank(),
          legend.key = element_rect("#DCDCDC")
        ) +
        scale_y_continuous(breaks = seq(-3, 3, by = 1),
                           labels = c(3:0, 1:3)) + ylab(expression("log"[10] *
                                                                     "(Count+1)")) + xlab("Gene") + geom_hline(yintercept = c(0), linetype = "dotted")

      short_reads = F
      if (is.data.frame(non_reliable_alleles_text)) {
        p <-
          p + geom_text(
            data = non_reliable_alleles_text,
            aes_string(label = "text", x = "gene", y = "pos"),
            angle = 0,
            size = non_reliable_alleles_text$size
          )
        non_reliable_alleles_text$count2 <-
          count.df$count2[count.df$alleles == "NRA"]
        non_reliable_alleles_text <-
          non_reliable_alleles_text[non_reliable_alleles_text$count2 != 0,]
        non_reliable_alleles_text$hjust <-
          ifelse(non_reliable_alleles_text$count2 >= 0, 0, 1)
        p2 <-
          p2 + geom_text(
            data = non_reliable_alleles_text,
            aes_string(
              label = "text",
              x = "gene",
              hjust = "hjust"
            ),
            angle = 0,
            size = 2.25
          )
        short_reads = T
      }
      ########################################################################################################

      ### Plot All panels

      if (html_output) {
        #options(warn = -1)
        ## Prepare panels for html plot

        p = p + theme(axis.title.x = element_blank())
        p.l <-
          ggplotly(p,
                   height = 800,
                   width = 400,
                   tooltip = "text") %>% plotly::layout(showlegend = FALSE)

        pk = pk + theme(axis.title = element_blank())
        pk <-
          pk + scale_fill_manual(
            name = "log<sub>10</sub>(lK)",
            values = c('#FFFFFF', RColorBrewer::brewer.pal(9, 'Blues')),
            drop = FALSE
          )
        pk.l <-
          ggplotly(pk,
                   height = 800,
                   width = 400,
                   tooltip = "text") %>% plotly::layout(showlegend = TRUE)
        pk.l$x$layout$annotations[[1]]$text = p.l$x$layout$annotations[[1]]$text
        pk.l$x$layout$annotations[[2]]$text = p.l$x$layout$annotations[[2]]$text

        p2 <- p2 + ylab("log<sub>10</sub>(Count+1)")
        p2.l <-
          ggplotly(p2,
                   height = 1000,
                   width = 700,
                   tooltip = "text") %>% plotly::layout(
                     margin = list(b = 50),
                     yaxis = list(title = paste0(
                       c(
                         rep("&nbsp;", 3),
                         "Gene",
                         rep("&nbsp;", 3),
                         rep("\n&nbsp;", 1)
                       ), collapse = ""
                     )),
                     showlegend = TRUE
                   )

        p2.l$x$layout$xaxis$ticktext = c(
          lapply(p2.l$x$layout$xaxis$ticktext[1:match("0", p2.l$x$layout$xaxis$ticktext) - 1], function(x)
            paste0("-",
                   x)),
          p2.l$x$layout$xaxis$ticktext[match("0", p2.l$x$layout$xaxis$ticktext):length(p2.l$x$layout$xaxis$ticktext)]
        )

        p2.l$x$data[[grep("dot", p2.l$x$data)]]$y[2] = p2.l$x$layout$yaxis$range[2]

        add_border <- function(p_l) {
          p_l$x$attrs <- lapply(p_l$x$attrs,
                                function(x) {
                                  x$mode <- "markers"
                                  x
                                })

          for (i in 1:length(p_l$x$data)) {
            p_l$x$data[[i]]$marker$line$width <- suppressMessages(0.2)
            p_l$x$data[[i]]$marker$line$color <-
              ifelse(
                p_l$x$data[[i]]$marker$line$color == 'transparent',
                'black',
                p_l$x$data[[i]]$marker$line$color
              )
          }
          return(p_l)
        }

        p.l <- add_border(p.l)
        pk.l <- add_border(pk.l)
        mgsub <- function(pattern, replacement, x, ...) {
          if (length(pattern) != length(replacement)) {
            stop("pattern and replacement do not have the same length.")
          }
          result <- x
          for (i in 1:length(pattern)) {
            result <- gsub(pattern[i], replacement[i], result, ...)
          }
          result
        }


        text_for_hovertext <- function(labels, count.df) {
          for (i in 1:length(labels)) {
            label <- labels[i]
            gene <-
              strsplit(strsplit(label, "<")[[1]][1], " ")[[1]][2]
            allele <- strsplit(label, "Allele: ")[[1]][2]
            if (!is.na(NA)) {
              count <-
                strsplit(strsplit(label, "<br />Count: ")[[1]][2], "<")[[1]][1]
              if (count == "NA")
                next
              count <- as.numeric(count)
              if (count %% 1 != 0)
                count <-
                  count.df %>% filter(
                    .data$gene == gene &
                      .data$alleles == allele &
                      round(.data$count3, nchar(as.character(count)) - 2) == count
                  ) %>% select(.data$count)
              else
                count <-
                  count.df %>% filter(.data$gene == gene &
                                        .data$alleles == allele &
                                        .data$count3 == count) %>% select(.data$count)
              labels[i] <-
                paste0("Gene: ",
                       gene,
                       "<br />Allele: ",
                       allele,
                       "<br />Count: ",
                       count[1,])
            }
          }
          return(labels)
        }

        text_for_hovertext_non_reliable <-
          function(labels, text, annot) {
            for (i in 1:length(labels)) {
              label <- labels[i]
              label.tmp <- toupper(label)
              gene <-
                strsplit(strsplit(label.tmp, "GENE: ")[[1]][2], '[<]')[[1]][1]
              allele <-
                gsub("text: ", "", strsplit(grep(gene, text, value = T), "<")[[1]][1])

              labels[i] <-
                paste0("Gene: ", gene, "<br />Allele: ", annot[allele])
            }
            return(labels)
          }

        count.df$count3 <- abs(count.df$count2)

        change_hovertext <- function(pp, annot) {
          ind <- grep('text: ', pp$x$data)
          for (i in ind) {
            pp$x$data[[i]]$type <-  suppressWarnings("text")
            pp$x$data[[i]]$hoveron <- "fill"
            pp$x$data[[i]]$hoverinfo <- "skip"
          }
          return(pp)
        }

        bottom_annot <- c()
        if (short_reads) {
          bottom_annot <- unique(non_reliable_alleles_text$text_bottom)
          names(bottom_annot) <-
            unique(non_reliable_alleles_text$text)
          p.l <- change_hovertext(p.l, bottom_annot)
          p2.l <- change_hovertext(p2.l, bottom_annot)
        }

        # for (i in 1:length(p2.l$x$data)) {
        #   p2.l$x$data[[i]]$text <- mgsub(c("border: ","white|black","^[<]br [/][>]",gene, "count2", "alleles","factor[(]alleles[,] levels [=] AlleleCol[)]", "yintercept: 0"),
        #                                  c("","","","Gene", "Count","Allele","Allele", ""), p2.l$x$data[[i]]$text)
        #   p2.l$x$data[[i]]$text <- text_for_hovertext(p2.l$x$data[[i]]$text, count.df)
        # }
        #
        # for (i in 1:length(p.l$x$data)) {
        #     p.l$x$data[[i]]$text <- mgsub(c("count\\: [0-9]\\.[0-9]","^[<]br [/][>]",gene, "alleles","factor[(]alleles[,] levels [=] AlleleCol[)]"),
        #                                   c("","","Gene", "Allele", "Allele"), p.l$x$data[[i]]$text)
        # }
        #
        # for (i in 1:length(pk.l$x$data)) {
        #     pk.l$x$data[[i]]$text <- mgsub(c("count\\: [0-9]","^[<]br [/][>]",gene, "K[_]GROUPED"),
        #                                      c("","","Gene", "K"), pk.l$x$data[[i]]$text)
        # }


        p.l.c <-
          suppressWarnings(
            subplot(
              p2.l,
              p.l,
              pk.l,
              widths = c(0.4, 0.2, 0.2),
              shareY = T,
              titleX = TRUE,
              margin = 0.01,
              which_layout = 1
            ) %>%
              plotly::layout(
                title = sample_name,
                font = list(size = 16),
                margin = list(t = 45)
              )
          )


        for (i in 1:length(p2.l$x$data)) {
          p.l.c$x$data[[i]]$showlegend <- FALSE
        }

        for (i in 1:(length(p2.l$x$data) + length(p.l$x$data))) {
          p.l.c$x$data[[i]]$name <-
            gsub("[()|,]|white|black", "", p.l.c$x$data[[i]]$name)
        }


        # p.l.c$x$layout$annotations[[6]]$text = "log<sub>10</sub>(lK)----"
        # p.l.c$x$layout$annotations[[6]]$xanchor = "center"
        # p.l.c$x$layout$annotations[[6]]$legendtitle = TRUE
        # p.l.c$x$layout$annotations[[6]]$y = 1 - 0.033 * (length(grep(
        #   '[[]', grep('TRUE', p.l.c$x$data, value = T), invert = T
        # ))) #(length(AlleleCol) + 2.4)  #0.52
        # p.l.c$x$layout$annotations[[6]]$x = 0.985
        # p.l.c$x$layout$annotations[[6]]$xref = 'paper'
        # p.l.c$x$layout$annotations[[6]]$yref = 'paper'
        # p.l.c$x$layout$annotations[[6]]$font$size = 16
        #
        #
        # p.l.c$x$layout$annotations[[3]] <-
        #   p.l.c$x$layout$annotations[[6]]
        # p.l.c$x$layout$annotations[[3]]$text = "Alleles----"
        # p.l.c$x$layout$annotations[[3]]$y = 0.99
        # p.l.c$x$layout$annotations[[3]]$x = 0.98
        # p.l.c$x$layout$annotations[[3]]$legendTitle = FALSE
        # p.l.c$x$layout$annotations[[3]]$font$size = 16


        if (short_reads) {
          bottom_annot_collapsed <- ifelse(
            length(bottom_annot) > 1,
            paste0(sapply(split(bottom_annot, ceiling(seq_along(
              bottom_annot
            ) / 1)),
            function(x)
              paste0(x, collapse = '\t')), collapse = '\n'),
            paste0(bottom_annot, collapse = '\t')
          )

          # p.l.c$x$layout$annotations[[5]] <-
          #   p.l.c$x$layout$annotations[[4]]
          # p.l.c$x$layout$annotations[[5]]$text = bottom_annot_collapsed
          # p.l.c$x$layout$annotations[[5]]$y = p.l.c$x$layout$annotations[[6]]$y - 0.05 * (length(grep(
          #   '[[]', grep('TRUE', p.l.c$x$data, value = T), invert = T
          # )))
          # p.l.c$x$layout$annotations[[7]]$x = 1.1
          # p.l.c$x$layout$annotations[[7]]$legendTitle = FALSE
          # p.l.c$x$layout$annotations[[7]]$font$size = 12
          #
          y = 1 - 0.037 * (length(grep(
            '[[]', grep('TRUE', p.l.c$x$data, value = T), invert = T
          )))
          y_short = y - 0.05 * (length(grep(
            '[[]', grep('TRUE', p.l.c$x$data, value = T), invert = T
          )))

          p.l.c <- p.l.c %>% plotly::add_annotations(
            x = c(1.1, 1.1),
            y = c(1, y),
            text = c("Alleles-----", "log<sub>10</sub>(lK)----"),
            font = list(size = 16),
            xref = "paper",
            yref = "paper",
            showarrow = FALSE
          ) %>% plotly::add_annotations(
            x = c(1.15),
            y = c(y_short),
            text = c(bottom_annot_collapsed),
            font = list(size = 12),
            xref = "paper",
            yref = "paper",
            showarrow = FALSE
          )
        } else{
          y = 1 - 0.037 * (length(grep(
            '[[]', grep('TRUE', p.l.c$x$data, value = T), invert = T
          )))
          p.l.c <- p.l.c %>% plotly::add_annotations(
            x = c(1.1, 1.1),
            y = c(1, y),
            text = c("Alleles-----", "log<sub>10</sub>(lK)----"),
            font = list(size = 16),
            xref = "paper",
            yref = "paper",
            showarrow = FALSE
          )
        }


        plot_list[[sample_name]] <- p.l.c

      } else {
        p.legend <-
          get_legend(p + theme(legend.key = element_rect("#DCDCDC")))
        p = p + theme(legend.position = "none", axis.title.x = element_blank())

        pk = pk + scale_fill_manual(
          name = expression("log"[10] * "(lK)"),
          values = c('#FFFFFF', RColorBrewer::brewer.pal(9, 'Blues')),
          drop = FALSE
        )
        pk.legend <-
          get_legend(pk + theme(legend.key = element_rect("gray")))
        pk = pk + theme(legend.position = "none", axis.title = element_blank())

        p.legends <-
          plot_grid(pk.legend, p.legend, ncol = 1, align = "hv")

        p1 <-
          plot_grid(
            p2,
            p,
            pk,
            nrow = 1,
            rel_widths = c(0.35, 0.15, 0.05),
            align = "hv",
            axis = "b"
          )

        p <-
          plot_grid(p1,
                    p.legends,
                    ncol = 2,
                    rel_widths = c(1, 0.1))

        if (short_reads) {
          bottom_annot <- unique(non_reliable_alleles_text$text_bottom)
          # Create text for annotating the short reads labels
          lab <- grid::textGrob(
            ifelse(
              length(bottom_annot) > 10,
              paste0(sapply(split(bottom_annot, ceiling(seq_along(
                bottom_annot
              ) / 10)),
              function(x)
                paste0(x, collapse = '\t')), collapse = '\n'),
              paste0(bottom_annot, collapse = '\t')
            ),
            x = unit(.1, "npc"),
            just = c("left"),
            gp = grid::gpar(fontsize = 10, col = "black")
          )

          gp <- ggplotGrob(p)
          # Add a row below the 2nd from the bottom
          gp <-
            gtable::gtable_add_rows(gp, unit(2, "grobheight", lab),-2)

          # Add 'lab' grob to that row, under the plot panel
          gp <-
            gtable::gtable_add_grob(gp, lab, t = -2, l = gp$layout[gp$layout$name == "panel", ]$l)

          plot_list[[sample_name]] <- gp
        } else{
          plot_list[[sample_name]] <- p
        }

      }

    }
    if (length(plot_list) != 1) {
      if (!missing(dir)) {
        dir <- file.path(dir, "haplotype_output")
        dir.create(dir)

      } else{
        dir <- tempdir()
      }

      if (html_output) {
        for (sample_name in names(plot_list)) {
          htmlwidgets::saveWidget(plot_list[[sample_name]],
                                  paste0(dir, '/', sample_name, ".html"),
                                  selfcontained = F)
        }
      } else {
        pdf(paste0(dir, "/haplotype_output.pdf"),
            height = 20,
            width = 15)
        for (sample_name in names(plot_list)) {
          title <- ggdraw() + draw_label(sample_name, fontface = "bold")
          plot(plot_grid(
            title,
            plot_list[[sample_name]],
            ncol = 1,
            rel_heights = c(0.05, 1)
          ))
        }

        dev.off()
      }

    } else if (html_output) {
      return(plot_list[[1]])
    } else{
      title <- ggdraw() + draw_label(sample_name, fontface = "bold")
      plot(plot_grid(
        title,
        plot_list[[1]],
        ncol = 1,
        rel_heights = c(0.05, 1)
      ))
    }
  }

########################################################################################################
#' Graphical output of alleles division by chromosome
#'
#' The \code{hapHeatmap} function generates a graphical output of the alleles per gene in multiple samples.
#'
#'
#' @param    hap_table            haplotype summary table. See details.
#' @param    chain                the IG chain: IGH,IGK,IGL. Default is IGH.
#' @param    genes_order           A vector of the genes by the desired order. Default is by GENE.loc
#' @param    removeIGH            if TRUE, 'IGH'\'IGK'\'IGL'\'TRB' prefix is removed from gene names.
#' @param    lk_cutoff            the lK cutoff value to be considered low for texture layer. Default is lK<1.
#' @param    mark_low_lk          if TRUE, a texture is add for low lK values. Default is TRUE.
#' @param    size_annot           size of bottom annotation text. Default is 1.5 .
#' @param    color_y              named list of the colors for y axis labels.
#' @param    order_subject        order subject by a vector.
#' @param    file                 file path for rendering the plot to pdf. If non is supplied than the plot is returned as object. Default is NULL.
#' @param    size_text            text size for annotations.
#' @param    ylabel_size          text size for y axis labels.
#'
#' @return
#'
#' A list with the following:
#'
#' \itemize{
#'   \item \code{'p'}:        heat-map visualization of the haplotype inference for multiple samples.
#'   \item \code{'width'}:    Optimal width value for rendering plot.
#'   \item \code{'height'}:   Optimal width value for rendering plot.
#' }
#'
#' When a file is supplied the graph is also rendered to pdf.
#'
#' @details
#'
#' A \code{data.frame} created by \code{createFullHaplotype}.
#'
#' @examples
#' # Plotting haplotpe heatmap
#' p <- hapHeatmap(samplesHaplotype)
#' p$p
#' @export
hapHeatmap <-
  function(hap_table,
           chain = c("IGH", "IGK", "IGL",  "TRB", "TRA"),
           genes_order = NULL,
           removeIGH = TRUE,
           lk_cutoff = 1,
           mark_low_lk = TRUE,
           size_annot = 1.5,
           color_y = NULL,
           order_subject = NULL ,
           file = NULL,
           size_text = NULL,
           ylabel_size = 1) {
    '.' <- list
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))
    subject <- gene <- alleles <- NULL

    if (missing(chain)) {
      chain = "IGH"
    }
    chain <- match.arg(chain)

    if (any(!grepl(paste0('^', chain), hap_table$gene)))
      stop(warning("The chain input does not match the genes"))

    if (is.null(genes_order)) {
      genes_order <-
        GENE.loc[[chain]][!GENE.loc[[chain]] %in% PSEUDO[[chain]]]
    } else{
      if (any(!grepl(paste0('^', chain), genes_order)))
        stop(warning("The chain input does not match the genes_order"))
    }

    lk_cutoff = as.numeric(lk_cutoff)

    id_GENE_col <- which(names(hap_table) == "gene")
    hapBy_col_id <- c(id_GENE_col + 1, id_GENE_col + 2)
    hapBy_cols <- names(hap_table)[hapBy_col_id]
    hapBy_alleles <- gsub("_", "*", hapBy_cols)
    samples <- unique(hap_table$subject)

    k_assign <- function(x) {
      # getting the lk value for each allele
      # input haplotype row
      # output lk value
      if (x[5] %fin% c("Unk", "Del", "NR")) {
        k_m <- paste0('k', 1:4)
      } else{
        k_m <- paste0('k', which(x[-c(1:10)] == x[5], arr.ind = T))
      }
      if (all(as.numeric(x[k_m]) == Inf))
        return(Inf)
      else
        return(min(as.numeric(x[k_m]), na.rm = T))
    }

    ### filter columns

    hap_table <-
      hap_table[, c("subject", "gene", hapBy_cols, "alleles", paste0("k", 1:4))]

    # check that all samples have all genes. Those who are missing adding as unk

    hap_table <-
      setDT(hap_table)[CJ(subject = subject,
                          gene = gene,
                          unique = TRUE), on = c("subject", "gene")]
    hap_table[is.na(alleles) , c(hapBy_cols, "alleles", paste0("k", 1:4)) := list("Unk",
                                                                                  "Unk",
                                                                                  NA_character_,
                                                                                  NA_integer_,
                                                                                  NA_integer_,
                                                                                  NA_integer_,
                                                                                  NA_integer_)]

    # sort the data
    hap_table$order <-
      fastmatch::fmatch(hap_table$gene, genes_order)
    hap_table <- stats::na.omit(hap_table, cols = "order")
    hap_table <- hap_table[order(order), ]
    if (removeIGH) {
      hap_table$gene <- gsub(chain, "", hap_table$gene)
      hap_table$gene <-
        factor(hap_table$gene, levels = gsub(chain, "", genes_order))
    } else{
      hap_table$gene <-
        factor(hap_table$gene, levels = genes_order)
    }

    # rename genes to numbers
    gene_loc <-
      1:length(unique(hap_table$gene)[order(match(unique(hap_table$gene), levels(hap_table$gene)))])
    names(gene_loc) <-
      unique(hap_table$gene)[order(match(unique(hap_table$gene), levels(hap_table$gene)))]
    hap_table$gene_LOC <- gene_loc[as.character(hap_table$gene)]
    # fix na in K columns
    hap_table[is.na(hap_table)] <- Inf
    # melt haplotype columns to one
    panels <-
      reshape2::melt(
        hap_table,
        measure.vars = hapBy_cols,
        variable.name = "hapBy",
        value.name = 'hapBy_alleles'
      )

    # separating the allele column
    panels <-
      splitstackshape::cSplit(
        panels[, c(
          'subject',
          'gene',
          'gene_LOC',
          "hapBy",
          "hapBy_alleles",
          "alleles",
          paste0("k", 1:4)
        )],
        'alleles',
        sep = ",",
        fixed = T,
        type.convert = F,
        drop = F
      )
    # separating the hap alleles
    panels <-
      splitstackshape::cSplit(
        panels,
        'hapBy_alleles',
        sep = ",",
        direction = "long",
        fixed = T,
        type.convert = F
      )
    # getting the lk value
    panels[, "k" := apply(panels, 1, k_assign)]
    # clean lk inf
    invisible(lapply(names(panels), function(.name)
      data.table::set(panels, which(is.infinite(panels[[.name]])), j = .name, value =
                        NA)))
    # remove unnecessary columns
    cols <-
      c("subject", "gene", "gene_LOC", "hapBy", "hapBy_alleles", "k")
    panels <- panels[, .SD, .SDcols = cols]

    ######sort the heatmap for plotting
    panels_m <-
      panels[, "n" :=  .N, by = c("subject", "gene", "hapBy")][] # count number of alleles for group
    panels_m$alleles_G <- panels_m$hapBy_alleles # for grouping
    panels_m$text <- ''
    panels_m$text_bottom <- panels_m$hapBy_alleles
    # change ambiguous hapBy_alleles call
    id_nra <-
      grepl("[0-9]_[0-9]+$|[0-9]_[0-9]+[.]+", panels_m$hapBy_alleles) &
      !grepl('^[0-9]+[_][0-9]+[A-Z]+[0-9]+', panels_m$hapBy_alleles)
    nra <- F
    if (any(id_nra)) {
      # number ambiguous hapBy_alleles
      num_text <-
        paste0('[*', 1:length(unique(panels_m$hapBy_alleles[id_nra])), ']')
      names(num_text) <- unique(panels_m$hapBy_alleles[id_nra])
      # text for plot
      panels_m$text[id_nra] <-
        num_text[panels_m$hapBy_alleles[id_nra]]
      # text for legend
      panels_m$text_bottom[id_nra] <-
        paste(num_text[panels_m$hapBy_alleles[id_nra]], panels_m$hapBy_alleles[id_nra])
      # change allele to NRA - non reliable allele
      panels_m$hapBy_alleles[id_nra] <- "NRA"
      # indicates that nra exists
      nra <- T
    }
    # create allele palette
    allele_palette <- alleleHapPalette(panels_m$hapBy_alleles)

    # sort novel allele calls for plot
    val_novel <-
      grep(
        '^[0-9]+[_][A-Z][0-9]+[A-Z]|^[0-9]+[_][0-9]+[A-Z]+[0-9]+',
        panels_m$hapBy_alleles,
        value = T
      )
    novel <- F
    novel_allele_text <- c()
    novel_symbol <- "\u005E"
    if (length(val_novel) != 0) {
      # sort the palettle colors for novel alleles
      id <-
        grep(
          '^[0-9]+[_][A-Z][0-9]+[A-Z]|^[0-9]+[_][0-9]+[A-Z]+[0-9]+',
          names(allele_palette$transper)
        )
      allele_palette$transper[id] <- 1
      # cerate code index for novel allele
      code_allele <- paste0(novel_symbol, 1:length(id))
      names(code_allele) <- allele_palette$AlleleCol[id]
      new_allele <-
        paste0(novel_symbol,
               1:length(id),
               '-',
               allele_palette$AlleleCol[id])
      names(new_allele) <- allele_palette$AlleleCol[id]
      # change the text for plot
      ids <- panels_m$hapBy_alleles %fin% names(new_allele)
      rep <- new_allele[panels_m$hapBy_alleles[ids]]
      rep2 <- code_allele[panels_m$hapBy_alleles[ids]]
      # add new allele code to data
      panels_m[ids, c("hapBy_alleles", "text_bottom", "text") := list(rep, rep, rep2)]
      # change annotation in legend colors
      allele_palette$AlleleCol[id] <- new_allele
      names(allele_palette$transper)[id] <- new_allele
      # indicates that novel exists
      novel <- T
    }

    panels_m$hapBy_alleles <-
      factor(panels_m$hapBy_alleles, levels = allele_palette$AlleleCol)

    # samples names and number
    samples <- unique(panels_m$subject)
    samples_n <- length(samples)
    # genes names and number
    genes <- unique(panels_m$gene)
    genes_n <- length(genes)

    # order the data by gene loc
    setorderv(panels_m, c("subject", "gene_LOC"))
    setkey(panels_m, "subject")

    # sort data for matrix
    panels_m[, "line" := 12 / panels_m$n]
    allele_code <- 1:length(allele_palette$AlleleCol)
    names(allele_code) <-
      gsub(paste0("\\^", "[0-9]+[-]"), "", allele_palette$AlleleCol)
    # sort the alleles in gene box
    panels_m[, "A_CODE" := allele_code[hapBy_alleles] + 1]
    panels_m[grep("[0-9]_[0-9]+$|[0-9]_[0-9]+[.]+",
                  panels_m$hapBy_alleles,
                  perl = T), "A_CODE" :=
               allele_code["NRA"]]
    setorderv(panels_m, c("subject", "gene_LOC", "A_CODE"))

    # duplicate the data by 12 box to gene
    panels_m[, "id" := 1:.N, by = c("subject", "gene", "hapBy")]
    panels_f = panels_m[, c("n_line" = 1:get("line")), by = c("subject",
                                                              "hapBy",
                                                              "gene",
                                                              "gene_LOC",
                                                              "alleles_G",
                                                              "A_CODE",
                                                              "text_bottom",
                                                              "k"), nomatch = 0]

    if (!is.null(order_subject))
      panels_f <-
      panels_f[order(match(panels_f$subject, order_subject))]

    # transform allele codes to matrix, 12 box for each gene. each row is an individual
    upper_m <-
      matrix(
        panels_f[panels_f$hapBy == hapBy_cols[1]][[6]],
        ncol = 12 * genes_n,
        byrow = T,
        dimnames = list(unique(panels_f[panels_f$hapBy == hapBy_cols[1]][[1]]), panels_f[panels_f$hapBy ==
                                                                                           hapBy_cols[1]][[3]][1:(12 * genes_n)])
      )
    lower_m <-
      matrix(
        panels_f[panels_f$hapBy == hapBy_cols[2]][[6]],
        ncol = 12 * genes_n,
        byrow = T,
        dimnames = list(unique(panels_f[panels_f$hapBy == hapBy_cols[2]][[1]]), panels_f[panels_f$hapBy ==
                                                                                           hapBy_cols[2]][[3]][1:(12 * genes_n)])
      )

    #### sort legend
    # fit width of legend column text by the longest allele
    longest_allele <- max(nchar(allele_palette$AlleleCol)) * 3 + 40
    # legend length
    leg_length <- next_divisor(longest_allele, genes_n * 12)
    # create legend values for matrix
    leg <-
      function(x, allele_text_length)
        c(rep(x, 4), rep(0, allele_text_length))
    seqs <-
      lapply(allele_code + 1, leg, allele_text_length = leg_length - 4)
    # add values for legend to complete the matrix, white boxes
    add = ceiling(length(unlist(seqs)) / (genes_n * 12)) * (genes_n * 12) - length(unlist(seqs))
    add = ifelse(add < 0, 0, add)
    # legend matrix
    m2 <-
      matrix(c(unlist(seqs), rep(0, add)), ncol = genes_n * 12, byrow = T)

    # start and end values for plot parts
    matrix_p <- c(1:3)
    matrix_r <- 3
    matrix_heights <- c(2, 2, 0.5)
    size = 1
    short_reads_rows = 0
    ## add short read text annotation at the bottom
    if (nra) {
      bottom_annot <-
        unique(grep(
          "[0-9]_[0-9]+$|[0-9]_[0-9]+[.]+",
          panels_f$text_bottom ,
          value = T
        ))
      bottom_annot <- unique(grep(
        '^[0-9]+[_][0-9]+[A-Z]+[0-9]+',
        bottom_annot ,
        value = T,
        invert = T
      ))

      # Create text for annotating the short reads labels
      annot <-
        splitlines(bottom_annot, genes_n * 12 / (4 * size_annot))
      annot <- annot[!is.na(dplyr::na_if(annot, ""))]
      x_annot = max(nchar(annot)) / (12 * genes_n)
      short_reads_rows = length(annot)
      annot <- paste0(annot, collapse = '\n')
      # add the start and value for the third part
      matrix_p <- c(1:4)
      matrix_r <- 4
      matrix_heights <- c(2, 2, 0.8, 0.5)

    }

    # set the height and width of plot
    height <-
      samples_n * 0.2 + 10 + nrow(m2) * 0.2 + short_reads_rows * 0.4 # number of samples, number of rows in legend, number of rows in bottom annotation
    width <- genes_n * 0.15 + 5 # numer of genes
    size_text = if (is.null(size_text))
      nrow(upper_m) / (height * width) + 1 # text size for heatmap annoations
    size_text_leg = ncol(m2) / (width * longest_allele) + 1 # text size for legend annotations

    if (!is.null(file)) {
      message(paste0("rendering to pdf: ", file))
      pdf(
        file,
        onefile = F,
        width = width,
        height = height,
        family = "serif"
      )
    } else{
      pdf(NULL)
      dev.control(displaylist = "enable")
    }
    # plot layout
    layout.matrix <- matrix(matrix_p, nrow = matrix_r, ncol = 1)
    graphics::layout(mat = layout.matrix,
                     heights = matrix_heights) # Heights of the rows
    # heatmap upper plot
    par(mar = c(2, 6, 8, 6))
    image(
      t(upper_m),
      col = names(allele_palette$AlleleCol),
      breaks = 1:(length(allele_palette$AlleleCol) + 1),
      axes = F
    )
    # add grid lines for genes
    grid(
      lwd = 1,
      nx = genes_n,
      ny = 0,
      col = "white",
      lty = 1
    )
    # add axis annotations
    axis(3, (0:(genes_n - 1)) / genes_n + 6 / (12 * genes_n), names(gene_loc), las =
           3) # top
    axis(1, (0:(genes_n - 1)) / genes_n + 6 / (12 * genes_n), names(gene_loc), las =
           3) # bottom
    graphics::title(gsub('_', '*', hapBy_cols[1]), adj = 0.5, line = 5)
    # color y tick labels if supplied
    colors <- "black"
    if (!is.null(color_y))
      colors <- color_y[rownames(upper_m)]
    samples_loc_n <-
      ifelse(samples_n == 1, 0, samples_n - 1)
    if (samples_loc_n == 0)
      samples_loc <-
      0
    else
      samples_loc <-
      (0:(samples_loc_n)) / (samples_loc_n)
    Map(
      axis,
      side = 2,
      at = samples_loc,
      col.axis = colors,
      labels = rownames(upper_m),
      lwd = 0,
      las = 1,
      cex.axis = ylabel_size
    ) #left
    axis(2, at = samples_loc, labels = FALSE)


    # draw lines for low lk values
    sub_geno = panels_m[panels_m$hapBy == hapBy_cols[1] &
                          panels_m$k < lk_cutoff, ]
    if (nrow(sub_geno) > 0) {
      NR = samples_n
      NC = genes_n * 12
      apply(sub_geno, 1, function(x) {
        I = which(x["subject"] == samples) - 1    # row index
        J = (as.numeric(x["gene_LOC"]) - 1) * 12            # column index
        draw_segment(NR, NC, I, J, lwd = 1, col = "white")
      })
    }

    # ad text annotations
    ids_text <-
      !grepl('^[0-9]|Del|Unk', panels_m$text_bottom)
    if (any(ids_text)) {
      sub_geno = panels_m[panels_m$hapBy == hapBy_cols[1] & ids_text, ]

      NR = samples_n
      NC = genes_n * 12
      apply(sub_geno, 1, function(x) {
        I = which(x["subject"] == samples) - 1    # row index
        J = (as.numeric(x["gene_LOC"]) - 1) * 12            # column index
        ALLELE =  as.numeric(x["id"])                   # allele index
        N_alleles = as.numeric(x["n"])                  # number of alleles
        TEXT =  x["text"]                   # text
        Write_text(NR, NC, I, J, ALLELE, N_alleles, TEXT, cex = size_text)
      })
    }

    # heatmap lower plot
    par(mar = c(2, 6, 8, 6))
    image(
      t(lower_m),
      col = names(allele_palette$AlleleCol),
      breaks = 1:(length(allele_palette$AlleleCol) + 1),
      axes = F
    )
    # add grid lines for genes
    grid(
      lwd = 1,
      nx = genes_n,
      ny = 0,
      col = "white",
      lty = 1
    )
    # add axis annotations
    axis(3, (0:(genes_n - 1)) / genes_n + 6 / (12 * genes_n), names(gene_loc), las =
           3) # top
    axis(1, (0:(genes_n - 1)) / genes_n + 6 / (12 * genes_n), names(gene_loc), las =
           3) # bottom
    graphics::title(gsub('_', '*', hapBy_cols[2]), adj = 0.5, line = 5)
    # color y tick labels if supplied
    colors <- "black"
    if (!is.null(color_y))
      colors <- color_y[rownames(lower_m)]
    Map(
      axis,
      side = 2,
      at = samples_loc,
      col.axis = colors,
      labels = rownames(lower_m),
      lwd = 0,
      las = 1,
      cex.axis = ylabel_size
    ) #left
    axis(2, at = samples_loc, labels = FALSE)

    # draw lines for low lk values

    sub_geno = panels_m[panels_m$hapBy == hapBy_cols[2] &
                          panels_m$k < lk_cutoff, ]
    if (nrow(sub_geno) > 0) {
      NR = samples_n
      NC = genes_n * 12
      apply(sub_geno, 1, function(x) {
        I = which(x["subject"] == samples) - 1    # row index
        J = (as.numeric(x["gene_LOC"]) - 1) * 12            # column index
        draw_segment(NR, NC, I, J, lwd = 1, col = "white")
      })
    }

    # ad text annotations
    sub_geno = panels_m[panels_m$hapBy == hapBy_cols[2] &
                          ids_text, ]
    if (nrow(sub_geno) > 0) {
      NR = samples_n
      NC = genes_n * 12
      apply(sub_geno, 1, function(x) {
        I = which(x["subject"] == samples) - 1    # row index
        J = (as.numeric(x["gene_LOC"]) - 1) * 12            # column index
        ALLELE =  as.numeric(x["id"])                   # allele index
        N_alleles = as.numeric(x["n"])                  # number of alleles
        TEXT =  x["text"]                   # text
        Write_text(NR, NC, I, J, ALLELE, N_alleles, TEXT, cex = size_text)
      })
    }

    # legend plot
    par(mar = c(3, 6, 2, 6))
    image(
      t(m2),
      col = names(allele_palette$AlleleCol),
      breaks = 1:(length(allele_palette$AlleleCol) + 1),
      axes = F
    )
    # add grid lines for each row
    grid(
      lwd = 1,
      ny = nrow(m2),
      nx = 0,
      col = "black",
      lty = 2
    )
    # add text anotation for legend
    NR = nrow(m2)
    NC = genes_n * 12
    names(allele_code) <- allele_palette$AlleleCol
    invisible(tapply(allele_code, names(allele_code), function(x) {
      ii = which(m2 == x + 1, arr.ind = T)[1, ]
      I = ii[[1]] - 1              # row index
      J = ii[[2]]                # column index
      ALLELE =  1                # allele index
      N_alleles = 1              # number of alleles
      TEXT =  names(x)            # text
      STEP_X <- 1 / (NC - 1)
      STEP_Y <-
        ifelse(1 / (NR - 1) == Inf, 0, 1 / (NR - 1))
      text(STEP_X * J + STEP_X * leg_length * 0.5,
           STEP_Y * I,
           TEXT, cex = size_text_leg)
    }))
    # bottom text for nra, only if exists
    if (nra) {
      # add the text at the bottom
      par(mar = c(1, 6, 1, 6))
      plot(
        c(0, 1),
        c(0, 1),
        ann = F,
        bty = 'n',
        type = 'n',
        xaxt = 'n',
        yaxt = 'n'
      )
      text(
        x = x_annot,
        y = 0.5,
        annot,
        cex = size_annot,
        col = "black",
        pos = 4
      )
    }

    if (is.null(file)) {
      p1.base <- recordPlot()
      invisible(dev.off())
      return(list(
        p = p1.base,
        width = width,
        height = height
      ))
    } else{
      dev.off()
      # embed the fonts to file
      #embedFonts(file)
    }
  }


########################################################################################################
#' Hierarchical clustering of haplotypes graphical output
#'
#' The \code{hapDendo} function generates a graphical output of an hierarchical clustering based on the Jaccard distance between multiple samples' haplotypes.
#'
#'
#' @param    hap_table            haplotype summary table. See details.
#' @param    chain                the IG/TR chain: IGH,IGK,IGL,TRB. Default is IGH.
#' @param    genes_order           A vector of the genes by the desired order. Default is by GENE.loc
#' @param    removeIGH            if TRUE, 'IGH'\'IGK'\'IGL' prefix is removed from gene names. Default is TRUE.
#' @param    mark_low_lk          if TRUE, a texture is add for low lK values. Default is TRUE.
#' @param    lk_cutoff            the lK cutoff value to be considerd low for texture layer. Default is lK<1.
#'
#' @return
#'
#' A multitple samples visualization of the distances between haplotypes.
#'
#' @details
#'
#' A \code{data.frame} created by \code{createFullHaplotype}.
#'
#' @examples
#' # Plotting haplotype hierarchical clustering based on the Jaccard distance
#' \donttest{hapDendo(samplesHaplotype)}
#'
#' @export
hapDendo <-
  function(hap_table,
           chain = c("IGH", "IGK", "IGL",  "TRB", "TRA"),
           genes_order = NULL,
           removeIGH = TRUE,
           mark_low_lk = TRUE,
           lk_cutoff = 1) {
    if (missing(chain)) {
      chain = "IGH"
    }
    chain <- match.arg(chain)

    if (is.null(genes_order)) {
      genes_order <- GENE.loc[[chain]]
    }

    lk_cutoff = as.numeric(lk_cutoff)

    id_GENE_col <- which(names(hap_table) == "gene")
    hapBy_col_id <- c(id_GENE_col + 1, id_GENE_col + 2)
    hapBy_cols <- names(hap_table)[hapBy_col_id]
    hapBy_alleles <- gsub("_", "*", hapBy_cols)
    samples <- unique(hap_table$subject)
    if (length(samples) < 2)
      stop("hapDendo function requires at least two samples")
    # creating the distance matrix for clustering
    mat <-
      matrix(NA, nrow = length(samples), ncol = length(samples))
    for (i in 2:length(samples)) {
      for (j in 1:(i - 1)) {
        hap_merge <-
          merge(hap_table[hap_table$subject == samples[i], c("gene", hapBy_cols[1], hapBy_cols[2])], hap_table[hap_table$subject == samples[j],
                                                                                                               c("gene", hapBy_cols[1], hapBy_cols[2])], by = "gene")

        mat[i, j] <-
          calcJacc(
            vec1A = hap_merge[, 2],
            vec1B = hap_merge[, 3],
            vec2A = hap_merge[, 4],
            vec2B = hap_merge[, 5],
            method = "geneByGene"
          )

      }
    }
    colnames(mat) <- samples
    rownames(mat) <- samples

    # finding the hierarchical clustering
    fit <- hclust(as.dist(mat), method = "ward.D")
    samples <- samples[fit$order]

    # Preparing the hclust data for plotting
    dend <- as.dendrogram(hclust(as.dist(mat), method = "ward.D"))
    dend_data <- ggdendro::dendro_data(dend)
    segment_data <-
      with(ggdendro::segment(dend_data),
           data.frame(
             x = y,
             y = x,
             xend = yend,
             yend = xend
           ))

    # Using the dendrogram label data to position the samples labels
    samples_pos_table <-
      with(dend_data$labels,
           data.frame(
             y_center = x,
             gene = as.character(label),
             height = 1
           ))
    samples_axis_limits <-
      with(samples_pos_table, c(min(y_center - 0.5 * height), max(y_center + 0.5 * height))) + 0.1 * c(-1, 1)

    plt_dendr <-
      ggplot(segment_data) + geom_segment(aes_string(
        x = "x",
        y = "y",
        xend = "xend",
        yend = "yend"
      )) + scale_x_continuous(expand = c(0, 0.01)) + scale_y_continuous(
        breaks = samples_pos_table$y_center,
        labels = samples_pos_table$gene,
        limits = samples_axis_limits,
        expand = c(0, 0)
      ) + labs(
        x = "Jaccard distance",
        y = "",
        colour = "",
        size = ""
      ) +
      theme_bw() + theme(
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.text = element_text(size = 14, colour = "black"),
        axis.title.x = element_text(size = 14, colour = "black")
      ) + ylab('')

    # Creating the haploype db for ploting
    haplo_db_clust <- c()
    for (sample_name in samples) {
      haplo.db <-
        parseHapTab(
          hap_table[hap_table$subject == sample_name,],
          chain = chain,
          count_df = F,
          sample_name = sample_name,
          hapBy_cols = hapBy_cols,
          hapBy_alleles = hapBy_alleles
        )
      geno.df <-
        sortDFByGene(
          haplo.db$geno.df,
          chain = chain,
          genes_order = genes_order,
          removeIGH = removeIGH
        )
      kval.df <-
        sortDFByGene(
          haplo.db$kval.df,
          chain = chain,
          genes_order = genes_order,
          removeIGH = removeIGH
        )
      geno.df$k <-
        apply(geno.df[, c("gene", "hapBy")], 1, function(x) {
          asNum(kval.df$k[kval.df$gene == x[[1]] &
                            kval.df$hapBy == x[[2]]], na.strings = "NA")
        })
      haplo_db_clust <- rbind(haplo_db_clust, geno.df)
    }
    allele_palette <- alleleHapPalette(haplo_db_clust$alleles)

    # Formating the data to fit heatmap plot
    haplo_db_clust <-
      haplo_db_clust %>% group_by(.data$subject, .data$hapBy, .data$gene) %>% dplyr::mutate(n = dplyr::n())
    haplo_db_clust$freq <-
      ifelse(haplo_db_clust$n == 2,
             0.5,
             ifelse(haplo_db_clust$n != 1, 0.25, 1))
    haplo_db_clust$gene <-
      factor(haplo_db_clust$gene, levels = gsub(chain, "", genes_order))
    haplo_db_clust$grouper_x <- "Gene"
    haplo_db_clust$grouper_y <-
      sapply(1:nrow(haplo_db_clust), function(i)
        paste0(haplo_db_clust$subject[i], " ", haplo_db_clust$hapBy[i]))

    haplo_db_clust_texture <-
      setNames(
        data.frame(matrix(ncol = 13, nrow = 0), stringsAsFactors = F),
        c(
          'subject',
          'gene',
          'alleles',
          'hapBy',
          'k',
          'n',
          'freq',
          'grouper_x',
          'grouper_y',
          'points',
          'yend',
          'x',
          'xend'
        )
      )
    loc <- 1:length(levels(droplevels(haplo_db_clust$gene)))
    names(loc) <- levels(droplevels(haplo_db_clust$gene))
    for (i in 1:nrow(haplo_db_clust)) {
      if (haplo_db_clust$k[i] < lk_cutoff &&
          !haplo_db_clust$alleles[i] %in% c("Unk", "Del", "NR")) {
        tmp_point <-
          haplo_db_clust[i,] %>% slice(rep(1, each = ifelse(length(samples) < 4, 15, 8))) %>%
          mutate(
            points = seq(0, 0.9, length.out = ifelse(length(samples) < 4, 15, 8)),
            yend = seq(0, 0.9, length.out = ifelse(length(samples) < 4, 15, 8)) + 0.1,
            x = loc[as.character(.data$gene)] - 0.49,
            xend = loc[as.character(.data$gene)] + 0.49
          )
        haplo_db_clust_texture <-
          plyr::rbind.fill(haplo_db_clust_texture, tmp_point)
      }
    }


    allele_cols <- gsub(chain, "", gsub("_", "*", hapBy_cols))
    # Adding white space to plot
    heatmap.df <- c()
    samples_order <- c()
    samples_label <- c()
    for (i in 1:length(samples)) {
      samp = samples[i]
      sub <- haplo_db_clust[haplo_db_clust$subject == samp,]
      sub2 <- sub[sub$hapBy == allele_cols[1],]
      sub2$freq <- 0
      sub2$grouper_y <- paste0(sub2$subject[1], " NA")
      if (i != length(samples)) {
        sub <- rbind(sub, sub2)
        heatmap.df <- rbind(heatmap.df, sub)
        samples_order <- c(samples_order, unique(sub$grouper_y))
        tmp_l <- c(unique(sub$hapBy), "")
        names(tmp_l) <- unique(sub$grouper_y)
        samples_label <- c(samples_label, tmp_l)
      } else {
        heatmap.df <- rbind(heatmap.df, sub)
        samples_order <- c(samples_order, unique(sub$grouper_y))
        tmp_l <- unique(sub$hapBy)
        names(tmp_l) <- unique(sub$grouper_y)
        samples_label <- c(samples_label, tmp_l)
      }
    }

    heatmap.df$grouper_y <-
      factor(heatmap.df$grouper_y, levels = samples_order)
    if (length(grep("[0-9][0-9]_[0-9][0-9]", heatmap.df$alleles)) != 0) {
      non_reliable_alleles_text <-
        nonReliableAllelesText(heatmap.df[!grepl("NA", heatmap.df$grouper_y) &
                                            grepl("[0-9][0-9]_[0-9][0-9]", heatmap.df$alleles),], size = 3)
      non_reliable_alleles_text$grouper_y <-
        factor(non_reliable_alleles_text$grouper_y, levels = samples_order)
      heatmap.df$alleles[grep("[0-9][0-9]_[0-9][0-9]", heatmap.df$alleles)] <-
        "NRA"
      allele_palette <- alleleHapPalette(heatmap.df$alleles)
      non_reliable_alleles_text$alleles <-
        factor(non_reliable_alleles_text$alleles, levels = allele_palette$AlleleCol)
    } else {
      non_reliable_alleles_text <- c()
      allele_palette <-
        alleleHapPalette(heatmap.df$alleles, NRA = FALSE)
    }

    heatmap.df$alleles <-
      factor(heatmap.df$alleles, levels = allele_palette$AlleleCol)

    heatmap.df$gene_LOC <-
      sapply(heatmap.df$gene, function(i) {
        loc[as.character(i)]
      })

    hap_plot <-
      ggplot() + geom_col(
        data = heatmap.df,
        mapping = aes_string(x = "gene_LOC", y = "freq", fill = "alleles"),
        position = "fill",
        width = 0.95,
        na.rm = T
      ) + scale_fill_manual(
        values = alpha(names(allele_palette$AlleleCol), allele_palette$transper),
        name = "Alleles",
        drop = FALSE
      ) + facet_grid(
        grouper_y ~
          grouper_x,
        as.table = FALSE,
        switch = "y",
        labeller = labeller(grouper_y = samples_label),
        drop = FALSE
      ) + scale_y_continuous(expand = c(0, 0)) + # scale_x_discrete(expand = c(0,0))
      theme(
        axis.text.x = element_text(
          angle = 90,
          vjust = 0.5,
          hjust = 1,
          size = 14,
          colour = "black"
        ),
        strip.text.x = element_blank(),
        strip.text.y = element_text(angle = 180,
                                    size = 14),
        panel.grid = element_blank(),
        strip.placement = "outside",
        axis.ticks.y = element_line(colour = "white"),
        axis.line.y.left = element_blank(),
        axis.text.y = element_blank(),
        strip.background.y = element_blank(),
        strip.background.x = element_blank(),
        panel.spacing.y = unit(0.9, "pt"),
        legend.position = "bottom",
        axis.title.x = element_text(size = 15, colour = "black"),
        legend.justification = "center"
      ) + labs(y = "", x = "Gene") + guides(fill = guide_legend(
        nrow = round(length(allele_palette$AlleleCol) / 9),
        order = 1,
        override.aes = list(color = "#DCDCDC")
      )) + scale_x_continuous(
        expand = c(0, 0),
        breaks = 1:length(unique(heatmap.df$gene)[order(match(unique(heatmap.df$gene), levels(heatmap.df$gene)))]),
        labels = unique(heatmap.df$gene)[order(match(unique(heatmap.df$gene), levels(heatmap.df$gene)))],
        sec.axis = dup_axis(name = "")
      )


    if (mark_low_lk & nrow(haplo_db_clust_texture) != 0) {
      haplo_db_clust_texture$grouper_y <-
        factor(haplo_db_clust_texture$grouper_y, levels = samples_order)
      haplo_db_clust_texture <-
        haplo_db_clust_texture[!duplicated(haplo_db_clust_texture[, c("gene", "alleles", "k", "points", "subject")]),]
      haplo_db_clust_texture$col <- paste0("lk<", lk_cutoff)
      # Get Allele legend
      gt1 = ggplotGrob(hap_plot)

      haplo_db_clust_texture$alleles <-
        factor(haplo_db_clust_texture$alleles, levels = allele_palette$AlleleCol)

      hap_plot <-
        hap_plot + geom_segment(
          data = haplo_db_clust_texture,
          mapping = aes_string(
            x = "x",
            xend = "xend",
            y = "points",
            yend = "yend",
            color = "col"
          ),
          colour = "white"
        )  +
        guides(color = "none", fill = "none")

      # Get lK legend
      gt2 = getDigLegend(unique(haplo_db_clust_texture$col))

      leg1 = gtable::gtable_filter(gt1, "guide-box")
      leg2 = gtable::gtable_filter(gt2, "guide-box")
      # Combine the legends
      leg <-
        cbind(leg1[["grobs"]][[1]], leg2[["grobs"]][[1]], size = "first")
      # Insert legend into g1 (or g2)
      gt1$grobs[gt1$layout$name == "guide-box"][[1]] <- leg
      gt1$grobs[gt1$layout$name == "guide-box"][[1]]$layout[3, c("t", "b")] <-
        gt1$grobs[gt1$layout$name == "guide-box"][[1]]$layout[1, c("t", "b")]
      gt1$grobs[gt1$layout$name == "guide-box"][[1]]$layout[3, c("l", "r")] <-
        gt1$grobs[gt1$layout$name == "guide-box"][[1]]$layout[1, c("l", "r")] +
        2
      legend <- get_legend(gt1)
    } else
      legend <- get_legend(hap_plot)

    if (is.data.frame(non_reliable_alleles_text)) {
      non_reliable_alleles_text$gene_LOC <-
        sapply(non_reliable_alleles_text$gene, function(i) {
          loc[as.character(i)]
        })
      hap_plot <-
        hap_plot + geom_text(
          data = non_reliable_alleles_text,
          aes_string(label = "text", x = "gene_LOC", y = "pos"),
          angle = 90,
          size = non_reliable_alleles_text$size
        )
    }

    dend_hap <-
      plot_grid(
        hap_plot + theme(
          legend.position = "none",
          plot.background = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(fill = "transparent", colour = NA),
          axis.line.x = element_line(color = "black")
        ),
        plt_dendr,
        nrow = 1,
        align = "h",
        axis = "bt",
        rel_widths = c(1, 0.25)
      )
    plot(plot_grid(
      dend_hap,
      legend,
      ncol = 1,
      rel_heights = c(1, 0.1)
    ))
  }

########################################################################################################
#' Graphical output of double chromosome deletions
#'
#' The \code{plotDeletionsByBinom} function generates a graphical output of the double chromosome deletions in multiple samples.
#'
#'
#' @param    GENE.usage.df        double chromosome deletion summary table. See details.
#' @param    chain                the IG chain: IGH,IGK,IGL. Default is IGH.
#' @param    genes.low.cer        a vector of IGH genes known to be with low certantiny in the binomial test. Default is IGHV3-43 and IGHV3-20
#' @param    genes.dup            a vector of IGH genes known to have a duplicated gene. Default is IGHD4-11 that his duplicate is IGHD4-4 and IGHD5-18 that his duplicate is IGHD5-5
#' @param    genes_order           A vector of the genes by the desired order. Default is by GENE.loc
#'
#' @return
#'
#' A double chromosome deletion visualization.
#'
#' @details
#'
#' A \code{data.frame} created by \code{binom_test_deletion}.
#'
#' @examples
#'
#' # Load example data and germlines
#' data(samples_db)
#'
#' # Infering haplotype
#' deletions_db = deletionsByBinom(samples_db);
#' plotDeletionsByBinom(deletions_db)
#'
#, chain = c("IGH", "IGK", "IGL",  "TRB", "TRA")
#' @export
plotDeletionsByBinom <-
  function(GENE.usage.df,
           chain = c("IGH", "IGK", "IGL",  "TRB", "TRA"),
           genes.low.cer = c("IGHV3-43", "IGHV3-20"),
           genes.dup = c("IGHD4-11", "IGHD5-18"),
           genes_order = NULL) {
    if (missing(chain)) {
      chain = "IGH"
    }
    chain <- match.arg(chain)

    if (chain != "IGH") {
      genes.low.cer = ""
      genes.dup = ""
    }
    if (!("subject" %in% names(GENE.usage.df))) {
      GENE.usage.df$subject <- rep("S1", nrow(GENE.usage.df))
    }

    if (is.null(genes_order)) {
      genes_order <- GENE.loc[[chain]]
    }

    genes_hap <- unique(substr(GENE.usage.df$gene, 4, 4))
    GENE.loc.tmp <-
      genes_order[genes_order %in% GENE.usage.df$gene]

    GENE.usage.df$gene2 <-
      factor(gsub(chain, "", GENE.usage.df$gene),
             levels = gsub(chain, "", GENE.loc.tmp))

    colvec <-
      ifelse(
        GENE.loc.tmp %in% genes.low.cer,
        "red",
        ifelse(GENE.loc.tmp %in% genes.dup, "purple", "black")
      )

    ### gene usage with deletions in population according to binom test
    p.del <-
      ggplot(
        GENE.usage.df %>% filter(.data$deletion != "Non reliable"),
        aes_string(x = "gene2", y = "frac")
      ) + geom_boxplot(outlier.colour = NA) + geom_jitter(aes_string(x = "gene2",
                                                                     color = "deletion"),
                                                          width = 0.25,
                                                          size = 0.5) + theme(
                                                            axis.text.y = element_text(size = 16),
                                                            axis.title = element_text(size = 16),
                                                            axis.text.x = element_text(
                                                              size = 14,
                                                              angle = 90,
                                                              hjust = 1,
                                                              vjust = 0.5,
                                                              color = colvec
                                                            ),
                                                            legend.text = element_text(size = 16),
                                                            legend.position = "none"
                                                          ) + ylab("Fraction") + xlab("") +
      scale_color_manual(
        name = "",
        labels = c("Deletion", "No Deletion", "NA"),
        values = c("#6d6d6d", "black", "#dedede"),
        drop = T
      ) + guides(color = guide_legend(override.aes = list(size = 5)))

    ### heat map of deletions in population according to binom test
    GENE.usage.df$deletion <-
      factor(GENE.usage.df$deletion, levels = levels(GENE.usage.df$deletion))
    if (length(levels(GENE.usage.df$deletion)) < 4)
      lab = c("Deletion", "No Deletion", "NA")
    else
      lab = c("Deletion", "No Deletion", "NA", "Non reliable")
    if (length(levels(GENE.usage.df$deletion)) < 4)
      col_val = c("#6d6d6d", "#ffffff", "#dedede")
    else
      col_val = c("#6d6d6d", "#ffffff", "#dedede", "#ffefd5")
    heatmap.plot <-
      ggplot(data = GENE.usage.df, aes_string(x = "gene2", y = "subject")) + geom_tile(aes_string(fill = "deletion")) + scale_fill_manual(
        name = "",
        labels = lab,
        values = col_val,
        drop = F
      ) + scale_x_discrete(drop = FALSE) + ylab("subject") + xlab("Gene") + theme(
        axis.text = element_text(size = 12, colour = "black"),
        axis.title.y = element_text(size = 18),
        axis.text.x = element_text(
          angle = 90,
          vjust = 0.5,
          hjust = 1,
          size = 14,
          colour = "black"
        ),
        legend.key = element_rect(
          colour = "black",
          size = 0.5,
          linetype = "solid"
        ),
        legend.text = element_text(size = 14),
        legend.direction = "horizontal",
        legend.justification = "center",
        legend.box.just = "bottom"
      )

    legend <- cowplot::get_legend(heatmap.plot)
    heatmap.plot <-
      heatmap.plot + theme(
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")
      )
    p.del <-
      p.del + theme(
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")
      )

    comb <-
      plot_grid(
        p.del,
        heatmap.plot,
        ncol = 1,
        rel_heights = c(0.15, 0.3),
        align = "hv"
      )
    plot(plot_grid(
      comb,
      legend,
      nrow = 2,
      rel_heights = c(1, 0.2)
    ))
  }

########################################################################################################
#' Graphical output of single chromosome deletions
#'
#' The \code{deletionHeatmap} function generates a graphical output of the single chromosome deletions in multiple samples.
#'
#'
#' @param    hap_table            haplotype summary table. See details.
#' @param    chain                the IG chain: IGH,IGK,IGL. Default is IGH.
#' @param    kThreshDel           the minimum lK (log10 of the Bayes factor) used in \code{createFullHaplotype} to call a deletion. Indicates the color for strong deletion. Default is 3.
#' @param    genes_order           A vector of the genes by the desired order. Default is by GENE.loc
#' @param    html_output          If TRUE, a html5 interactive graph is outputed insteaed of the normal plot. Default is FALSE
#' @return
#'
#' A single chromosome deletion visualization.
#'
#' @details
#'
#' A \code{data.frame} created by \code{createFullHaplotype}.
#'
#' @examples
#' # Plotting single choromosme deletion from haplotype inference
#' deletionHeatmap(samplesHaplotype)
#' @export
# Not in use,
deletionHeatmap <-
  function(hap_table,
           chain = c("IGH", "IGK", "IGL",  "TRB", "TRA"),
           kThreshDel = 3,
           genes_order = NULL,
           html_output = FALSE) {
    if (missing(chain)) {
      chain = "IGH"
    }
    chain <- match.arg(chain)

    if (!("subject" %in% names(hap_table))) {
      hap_table$subject <- rep("S1", nrow(hap_table))
    }

    if (is.null(genes_order)) {
      genes_order <- GENE.loc[[chain]]
    }

    hapBy_cols = names(hap_table)[c(3, 4)]

    genes_hap <- unique(substr(hap_table$gene, 4, 4))

    GENE.loc.tmp <-
      genes_order[grep(paste0(genes_hap, collapse = "|"), genes_order)]

    GENE.loc.tmp <-
      GENE.loc.tmp[GENE.loc.tmp %in% unique(hap_table$gene)]

    ALLELE_01_col = hapBy_cols[1]
    ALLELE_02_col = hapBy_cols[2]

    ALLELE_01_num = strsplit(ALLELE_01_col, "_")[[1]][2]
    ALLELE_02_num = strsplit(ALLELE_02_col, "_")[[1]][2]

    ### create deletion streches heatmap
    hap_table$k1[is.na(hap_table$k1)] <- 0
    hap_table$k2[is.na(hap_table$k2)] <- 0

    hap_table.del.heatmap <-
      hap_table %>% rowwise %>% dplyr::mutate(k = max(as.numeric(.data$k1),
                                                      as.numeric(.data$k2), na.rm = T)) %>%
      select(!!as.name("subject"), !!as.name("gene"),!!as.name(ALLELE_01_col), !!as.name("k"))

    hap_table.del.heatmap$HapBy <-
      rep(ALLELE_01_num, nrow(hap_table.del.heatmap))
    names(hap_table.del.heatmap)[3] <- ALLELE_02_col

    hap_table.del.heatmap <-
      rbind(
        hap_table.del.heatmap,
        data.frame(
          hap_table %>% rowwise %>% dplyr::mutate(k = max(
            as.numeric(.data$k1), as.numeric(.data$k2), na.rm = T
          )) %>%
            select(!!as.name("subject"), !!as.name("gene"),!!as.name(ALLELE_02_col), !!as.name("k")),
          HapBy = ALLELE_02_num
        )
      )

    names(hap_table.del.heatmap)[3] <- "allele"

    hap_table.del.heatmap$k[hap_table.del.heatmap$k == Inf] <- 0
    hap_table.del.heatmap$k[hap_table.del.heatmap$k == -Inf] <- 0

    hap_table.del.heatmap$del <-
      ifelse(hap_table.del.heatmap$allele == "Del" &
               hap_table.del.heatmap$k >= kThreshDel,
             3,
             0)
    hap_table.del.heatmap$del[(!hap_table.del.heatmap$allele %in% c("Del", "Unk", "NA")) &
                                hap_table.del.heatmap$k < 3] <- 1
    hap_table.del.heatmap$del[hap_table.del.heatmap$allele == "Del" &
                                hap_table.del.heatmap$k < kThreshDel] <-
      2
    hap_table.del.heatmap$del[hap_table.del.heatmap$allele == "NA"] <-
      4



    # manual reshape
    hap_table.del.heatmap.02 <-
      hap_table.del.heatmap[hap_table.del.heatmap$HapBy == ALLELE_01_num,]
    hap_table.del.heatmap.03 <-
      hap_table.del.heatmap[hap_table.del.heatmap$HapBy == ALLELE_02_num,]
    hap_table.del.heatmap.02$gene2 <-
      gsub(chain, "", hap_table.del.heatmap.02$gene)
    hap_table.del.heatmap.03$gene2 <-
      gsub(chain, "", hap_table.del.heatmap.03$gene)

    hap_table.del.heatmap.02$subject <-
      factor(x = hap_table.del.heatmap.02$subject,
             levels = unique(hap_table.del.heatmap.02$subject))

    hap_table.del.heatmap.02$gene2 <-
      factor(x = hap_table.del.heatmap.02$gene2,
             levels = gsub(chain, "", GENE.loc.tmp))

    hap_table.del.heatmap.03$subject <-
      factor(x = hap_table.del.heatmap.03$subject,
             levels = unique(hap_table.del.heatmap.03$subject))

    hap_table.del.heatmap.03$gene2 <-
      factor(x = hap_table.del.heatmap.03$gene2,
             levels = gsub(chain, "", GENE.loc.tmp))
    heatmap.df <-
      rbind(hap_table.del.heatmap.02, hap_table.del.heatmap.03)

    ALLELE_01_col = gsub("_", "*", gsub(chain, "", ALLELE_01_col))
    ALLELE_02_col = gsub("_", "*", gsub(chain, "", ALLELE_02_col))
    heatmap.df$del <- factor(heatmap.df$del, levels = c(0:4))
    heatmap.df$HapBy <-
      ifelse(heatmap.df$HapBy == ALLELE_01_num,
             ALLELE_01_col,
             ALLELE_02_col)
    # del.df.heatmap <- heatmap.df
    # del.df.heatmap <- del.df.heatmap %>% filter(.data$ALLELE == "Del")

    del.df.heatmap.cnt <-
      heatmap.df %>% dplyr::ungroup() %>% dplyr::group_by(.data$subject, .data$gene2, .data$del) %>% dplyr::mutate(n = dplyr::n()) %>% dplyr::slice(1)
    del.df.heatmap.cnt$n[del.df.heatmap.cnt$del == 0] <- 0
    del.df.heatmap.cnt$n[del.df.heatmap.cnt$del == 1] <- 0
    del.df.heatmap.cnt$HapBy[del.df.heatmap.cnt$n == 2] <- "Both"
    del.df.heatmap.cnt <-
      del.df.heatmap.cnt %>% dplyr::ungroup() %>% dplyr::group_by(.data$gene2, .data$HapBy, .data$del) %>% dplyr::count_(wt = "n")
    #names(del.df.heatmap.cnt)[4] <- 'n'
    del.df.heatmap.cnt$n[del.df.heatmap.cnt$del == 0] <- 0
    del.df.heatmap.cnt$n[del.df.heatmap.cnt$del == 1] <- 0
    del.df.heatmap.cnt$HapBy <-
      factor(del.df.heatmap.cnt$HapBy,
             levels = c(ALLELE_01_col, ALLELE_02_col, "Both"))
    if (!html_output) {
      heatmap.plot <-
        ggplot(data = heatmap.df, aes_string(x = "gene2", y = "subject")) +
        theme_bw() + geom_tile(aes_string(fill = "del")) +
        facet_wrap( ~ HapBy, nrow = 2) + scale_x_discrete(drop = FALSE) +
        scale_fill_manual(
          name = "lK",
          labels = c(
            "No deletion (lK>=3)",
            "No deletion (lK<3)",
            paste0("Deletion (lK<", kThreshDel, ")"),
            paste0("Deletion (lK>=", kThreshDel, ")"),
            "NA"
          ),
          values = c("white", "#ffb6c1", "lightblue", "#6d6d6d", "#dedede"),
          drop = FALSE
        ) +
        ylab("subject") + xlab("Gene") +
        theme(
          strip.text = element_text(size = 18),
          strip.background = element_rect(fill = "seashell2"),
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 14, colour = "black"),
          axis.text.x = element_text(
            angle = 90,
            vjust = 0.5,
            hjust = 1
          ),
          plot.margin = margin(b = 12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          legend.direction = "horizontal",
          legend.justification = "center",
          legend.position = "bottom",
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14),
          legend.key = element_rect(fill = "white", colour = "black"),
          axis.line = element_line(colour = "black")
        )

      pdel <-
        ggplot(del.df.heatmap.cnt,
               aes_string(x = "gene2", y = "n", fill = "HapBy")) + theme_bw() + geom_bar(stat = "identity",
                                                                                         position = "stack",
                                                                                         na.rm = T) +
        theme(
          strip.background = element_blank(),
          axis.text = element_text(size = 14, colour = "black"),
          axis.text.x = element_text(
            angle = 90,
            vjust = 0.5,
            hjust = 1
          ),
          plot.margin = margin(0, 8, 0, 7, "pt"),
          axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          legend.background = element_blank(),
          panel.background = element_blank()
        ) + ylab("Number of individuals\nwith a deletion") +
        scale_fill_manual(
          name = "Chromosome",
          values = c("darksalmon", "deepskyblue", "darkolivegreen3", "grey50")
        ) + scale_x_discrete(drop = FALSE) + xlab("")

      pdel <-
        pdel + theme(
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.justification = "center",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.key = element_rect(fill = "white",
                                    colour = "black"),
          legend.margin = margin(0, 0, 0, 0),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14)
        )
      p <-
        plot(plot_grid(
          pdel,
          heatmap.plot,
          ncol = 1,
          rel_heights = c(0.15, 0.35),
          align = "hv",
          axis = "b"
        ))
    } else{
      lk_labels <- setNames(c(
        "No deletion (lK>=3)",
        "No deletion (lK<3)",
        paste0("Deletion (lK<", kThreshDel, ")"),
        paste0("Deletion (lK>=", kThreshDel, ")"),
        "NA"
      ),
      0:4)
      heatmap.df$text <- paste0(
        "Individual: ",
        heatmap.df$subject,
        '<br />',
        "Gene: ",
        heatmap.df$gene2,
        '<br />',
        "Deletion: ",
        lk_labels[heatmap.df$del]
      )

      lk_labels <- setNames(paste0("lK - ", lk_labels), 0:4)
      heatmap.plot <-
        ggplot(data = heatmap.df, aes_string(x = "gene2", y = "subject", text = "text")) +
        theme_bw() + geom_tile(aes_string(fill = "del")) +
        facet_wrap( ~ HapBy, nrow = 2) + scale_x_discrete(drop = FALSE) +
        scale_fill_manual(
          name = "",
          labels = lk_labels,
          values = c("white", "#ffb6c1", "lightblue", "#6d6d6d", "#dedede"),
          drop = FALSE
        ) +
        ylab("subject") + xlab("Gene") +
        theme(
          strip.text = element_text(size = 18),
          strip.background = element_rect(fill = "seashell2"),
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 14, colour = "black"),
          axis.text.x = element_text(
            angle = 90,
            vjust = 0.5,
            hjust = 1
          ),
          plot.margin = margin(b = 12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.spacing = unit(2, "lines"),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14),
          legend.key = element_rect(fill = "white", colour = "black"),
          axis.line = element_line(colour = "black")
        )

      del.df.heatmap.cnt$text <-
        paste0(
          "Chromosome: ",
          del.df.heatmap.cnt$HapBy,
          '<br />',
          "Gene: ",
          del.df.heatmap.cnt$gene2,
          '<br />',
          "Number of deletions: ",
          del.df.heatmap.cnt$n
        )
      del.df.heatmap.cnt$HapBy2 <-
        paste0("Chromosome - ", del.df.heatmap.cnt$HapBy)
      pdel <-
        ggplot(del.df.heatmap.cnt,
               aes_string(
                 x = "gene2",
                 y = "n",
                 fill = "HapBy2",
                 text = "text"
               )) + theme_bw() +
        geom_bar(stat = "identity",
                 position = "stack",
                 na.rm = T) +
        theme(
          strip.background = element_blank(),
          axis.text = element_text(size = 14, colour = "black"),
          axis.text.x = element_text(
            angle = 90,
            vjust = 0.5,
            hjust = 1
          ),
          plot.margin = margin(0, 8, 0, 7, "pt"),
          axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          legend.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          legend.key = element_rect(fill = "white", colour = "black"),
          legend.margin = margin(0, 0, 0, 0),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14)
        ) +
        ylab("Number of individuals\nwith a deletion") +
        scale_fill_manual(
          name = "",
          values = c("darksalmon", "deepskyblue", "darkolivegreen3", "grey50")
        ) + scale_x_discrete(drop = FALSE) + xlab("")

      #a start for html output
      pdel.l <-
        plotly::ggplotly(pdel,
                         height = 400,
                         width = 1500,
                         tooltip = "text")

      heatmap.plot.l <-
        plotly::ggplotly(
          heatmap.plot,
          height = 900,
          width = 1500,
          tooltip = "text"
        )
      for (i in 1:length(heatmap.plot.l$x$data)) {
        heatmap.plot.l$x$data[[i]]$name <-
          lk_labels[heatmap.plot.l$x$data[[i]]$name]
        heatmap.plot.l$x$data[[i]]$legendgroup <-
          lk_labels[heatmap.plot.l$x$data[[i]]$legendgroup]
      }
      l <- list(
        font = list(
          family = "sans-serif",
          size = 12,
          color = "#000"
        ),
        bgcolor = "#eae5e5",
        bordercolor = "#FFFFFF",
        borderwidth = 2
      )
      heatmap.bar.l <-
        plotly::subplot(
          pdel.l,
          heatmap.plot.l %>% plotly::layout(
            xaxis = list(title = "Gene"),
            yaxis = list(title = "subject"),
            yaxis2 = list(title = "subject")
          ),
          nrows = 2 ,
          titleY = TRUE,
          titleX = TRUE,
          shareX = F,
          shareY = F,
          heights = c(0.2, 0.7),
          margin = 0.08
        ) %>% plotly::layout(legend = l)

      return(heatmap.bar.l)
    }
  }

##########################################################################
#' Graphical output for single chromosome D or J gene deletions according to V pooled method
#'
#' The \code{plotDeletionsByVpooled} function generates a graphical output for single chromosome D or J gene deletions (for heavy chain only).
#'
#'
#' @param  del.df   a \code{data.frame} created by \code{deletionsByVpooled}
#' @param  chain    the IG chain: IGH,IGK,IGL. Default is IGH..
#' @param  K_ranges vector of one or two integers for log(K) certainty level thresholds
#'
#' @return
#'
#' A single chromosome deletion visualization.
#'
#' @details
#'
#' A \code{data.frame} created by \code{deletionsByVpooled}.
#'
#' @examples
#' \donttest{
#' # Load example data and germlines
#' data(samples_db)
#' del_db <- deletionsByVpooled(samples_db)
#' plotDeletionsByVpooled(del_db)
#' }
#' @export
plotDeletionsByVpooled <-
  function(del.df,
           chain = c("IGH", "IGK", "IGL",  "TRB", "TRA"),
           K_ranges = c(3, 7)) {
    if (!("subject" %in% names(del.df))) {
      del.df$subject <- rep("S1", nrow(del.df))
    }

    if (missing(chain)) {
      chain = "IGH"
    }
    chain <- match.arg(chain)

    if (length(K_ranges) == 2) {
      del.df$EVENT <- unlist(sapply(1:nrow(del.df), function(i) {
        if (del.df$deletion[i] > 0 & del.df$k[i] < K_ranges[1])
          return(1)
        if (del.df$deletion[i] > 0 &
            del.df$k[i] >= K_ranges[1] & del.df$k[i] < K_ranges[2])
          return(2)
        if (del.df$deletion[i] > 0 & del.df$k[i] > K_ranges[2])
          return(3)
        if (del.df$deletion[i] == 0 & del.df$k[i] < K_ranges[1])
          return(4)
        if (del.df$deletion[i] == 0 &
            del.df$k[i] >= K_ranges[1] & del.df$k[i] < K_ranges[2])
          return(5)
        if (del.df$deletion[i] == 0 & del.df$k[i] > K_ranges[2])
          return(6)
      }))


      del.df$EVENT <- factor(del.df$EVENT, levels = 1:6)
      del.df$gene2 <-
        factor(gsub(chain, "", del.df$gene), levels = gsub(chain, "", GENE.loc[[chain]]))
      labels1 = c(
        paste0("Deletion lK<", K_ranges[1]),
        paste0("Deletion ", K_ranges[1], "<=lK<", K_ranges[2]),
        paste0("Deletion lK>=", K_ranges[2]),
        paste0("No deletion lK<",
               K_ranges[1]),
        paste0("No deletion ", K_ranges[1], "<=lK<", K_ranges[2]),
        paste0("No deletion lK>=", K_ranges[2])
      )
      values1 = c("lightblue",
                  "cornflowerblue",
                  "#6d6d6d",
                  "lightpink",
                  "lightcoral",
                  "white")
    }

    if (length(K_ranges) == 1) {
      del.df$EVENT <- unlist(sapply(1:nrow(del.df), function(i) {
        if (del.df$deletion[i] > 0 & del.df$k[i] < K_ranges[1])
          return(1)
        if (del.df$deletion[i] > 0 & del.df$k[i] >= K_ranges[1])
          return(2)
        if (del.df$deletion[i] == 0 & del.df$k[i] < K_ranges[1])
          return(3)
        if (del.df$deletion[i] == 0 &
            del.df$k[i] >= K_ranges[1])
          return(4)
      }))


      del.df$EVENT <- factor(del.df$EVENT, levels = 1:4)
      del.df$gene2 <-
        factor(gsub(chain, "", del.df$gene), levels = gsub(chain, "", GENE.loc[[chain]]))
      labels1 = c(
        paste0("Deletion lK<", K_ranges[1]),
        paste0("Deletion lK>=", K_ranges[1]),
        paste0("No deletion lK<", K_ranges[1]),
        paste0("No deletion lK>=",
               K_ranges[1])
      )
      values1 = c("lightblue", "#6d6d6d", "lightpink", "lightgrey")
    }

    heatmap.plot <-
      ggplot(data = del.df, aes_string(x = "gene2", y = "subject")) + geom_tile(aes_string(fill = "EVENT")) + scale_fill_manual(
        name = "",
        labels = labels1,
        values = values1,
        drop = F
      ) + ylab("subject") + xlab("Gene") + theme(
        strip.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12, colour = "black"),
        legend.position = "bottom",
        legend.justification = "center",
        axis.text.x = element_text(
          angle = 90,
          vjust = 0.5,
          hjust = 1
        ),
        legend.key = element_rect(
          colour = "black",
          size = 0.5,
          linetype = "solid"
        ),
        legend.text = element_text(size = 14),
        axis.line.x.bottom = element_line(colour = "black", inherit.blank = F),
        axis.line.y.left = element_line(colour = "black", inherit.blank = F),
        axis.line = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank()
      )



    plot(heatmap.plot)

  }
