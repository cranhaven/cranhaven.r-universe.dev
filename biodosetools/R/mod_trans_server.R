#' Translocations Chromosome Hottable Server Module
#'
#' @param id Internal parameter for {shiny}.
#'
#' @import shiny rhandsontable
#' @importFrom rlang .data
#' @noRd
mod_trans_chromosome_hot_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    table <- reactive({
      input$button_upd_chrom_table

      isolate({
        chromosome_list <- input$trans_chromosome_select
        color_scheme <- input$trans_m_fish_scheme
      })

      # Modify table rendering depending on stain method
      if (color_scheme) {
        data <- data.frame(
          Chromosome = chromosome_list
        )
      } else {
        color_list <- input$trans_color_select

        data <- matrix(
          nrow = length(chromosome_list),
          ncol = length(color_list) + 1
        ) %>%
          as.data.frame() %>%
          `colnames<-`(c("Chromosome", color_list))

        data[["Chromosome"]] <- chromosome_list
      }

      return(data)
    })

    # Output ----
    output$chromosome_table <- renderRHandsontable({
      if (input$button_upd_chrom_table <= 0) {
        return(NULL)
      }

      num_cols <- as.numeric(ncol(table()))

      hot <- table() %>%
        rhandsontable(
          width = (80 + num_cols * 85),
          height = "100%"
        ) %>%
        hot_col(1, colWidths = 115, readOnly = TRUE) %>%
        hot_cols(halign = "htCenter")

      if (num_cols > 1) {
        hot <- hot %>%
          hot_col(2:num_cols, colWidths = 85)
      }

      hot$x$contextMenu <- list(items = c("remove_row", "---------", "undo", "redo"))

      return(hot)
    })
  })
}

#' Translocations Fraction to Full Genome Server Module
#'
#' @param id Internal parameter for {shiny}.
#'
#' @import shiny rhandsontable
#' @importFrom rlang .data
#' @noRd
mod_trans_fraction_to_full_genome_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Calculate genomic fraction ----

    genome_factor <- reactive({
      # Create button dependency for updating dimensions
      input$button_calc_genome_factor

      isolate({
        dna_table <- biodosetools::dna_content_fractions_morton
        color_scheme <- input$trans_m_fish_scheme
        chromosome_table <- hot_to_r(input$chromosome_table)
        sex <- input$trans_sex
      })

      # Modify pairs of chromosome/stain color depending on stain method
      if (color_scheme) {
        chromosomes <- as.character(chromosome_table[["Chromosome"]])
        colors <- paste("M-Fish", chromosomes)
      } else {
        chromosome_table_melt <- chromosome_table %>%
          tidyr::pivot_longer(
            cols = -"Chromosome",
            names_to = "Stain",
            values_to = "Bool"
          )
        chromosome_table_clean <- chromosome_table_melt %>%
          dplyr::filter(.data$Bool == TRUE)

        chromosomes <- as.character(chromosome_table_clean[["Chromosome"]])
        colors <- chromosome_table_clean[["Stain"]]
      }

      genome_factor <- calculate_genome_factor(dna_table, chromosomes, colors, sex)

      return(genome_factor)
    })

    # Output ----
    output$genome_factor <- renderUI({
      if (input$button_calc_genome_factor <= 0) {
        return(NULL)
      }

      genome_factor_value <- genome_factor()
      genome_factor_text <- paste0(
        "The genomic conversion factor to full genome is ",
        as.character(round(genome_factor_value, 3)),
        "."
      )

      return(genome_factor_text)
    })

    return(
      list(genome_factor = reactive(genome_factor()))
    )
  })
}
