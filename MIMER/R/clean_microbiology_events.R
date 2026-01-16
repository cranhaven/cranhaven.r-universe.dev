
#' Cleanse organism names in Urine samples
#'
#' Function to preprocess organism names in urine samples.
#' Removes specified strings, maps certain values to standard ones, and filters out unwanted values.
#'
#' @param data The dataframe containing urine sample data.
#' @param column_name The name of the column containing organism names.
#' @param strings_to_remove A character vector of strings to be removed from the organism names.
#' @param standard_mapping A named character vector specifying mappings of values to standard ones.
#' @param filter_values A character vector of values to be filtered out from the organism names.
#'
#' @return The preprocessed dataframe.
#'
#' @examples
#' data <- data.frame(org_name = c("PRESUMPTIVELY Streptococcus",
#'               "MODERATE Escherichia coli",
#'               "S. AUREUS POSITIVE",
#'               "CANCELLED Influenza A"))
#' data <- cleanse_urine_organism_names(data,
#'                                    column_name = "org_name",
#'                                    strings_to_remove = c("POSITIVE FOR",
#'                                                         "PRESUMPTIVELY","PRESUMPTIVE",
#'                                                         "PROBABLE", "IDENTIFICATION",
#'                                                         "RESEMBLING", "SEEN",
#'                                                         "MODERATE", "FEW", "BETA",
#'                                                          "METHICILLIN RESISTANT",
#'                                                          "NUTRITIONALLY VARIANT",
#'                                                          "NOT C. PERFRINGENS OR C. SEPTICUM",
#'                                                         "-LACTAMASE POSITIVE",
#'                                                         "-LACTAMASE NEGATIVE",
#'                                                          "VIRAL ANTIGEN",
#'                                                          "CANDIDA INCONSPICUA",
#'                                                         "/POSADASII",
#'                                                         "NOT FUMIGATUS, FLAVUS OR NIGER",
#'                                                         "MRSA POSITIVE", "MRSA NEGATIVE",
#'                                                         "HISTOLYTICA/DISPAR"),
#'                                    standard_mapping = c(
#'                                                         "NON-FERMENTER" = "STREPTOCOCCUS",
#'                                                         "ABIOTROPHIA/GRANULICATELLA" =
#'                                                                             "STREPTOCOCCUS",
#'                                                         "S. AUREUS POSITIVE" =
#'                                                                            "STAPHYLOCOCCUS AUREUS",
#'                                                         "ASPERGILLUS FUMIGATUS COMPLEX" =
#'                                                                            "ASPERGILLUS FUMIGATUS",
#'                                                         "(CRYPTOSPORIDIUM PARVUM OOCYSTS|
#'                                                         CUNNINGHAMELLA BERTHOLLETIAE|
#'                                                         EPIDERMOPHYTON FLOCCOSUM|
#'                                                         EXOPHIALA JEANSELMEI COMPLEX|
#'                                                         SCEDOSPORIUM|
#'                                                         NEOASCOCHYTA DESMAZIERI|
#'                                                         NEOSCYTALIDIUM DIMIDIATUM|
#'                                                         LOMENTOSPORA|NEUROSPORA|
#'                                                         PERONEUTYPA SCOPARIA|
#'                                                         SPOROTHRIX SCHENCKII COMPLEX|
#'                                                         ZYGOSACCHAROMYCES FERMENTATI)" =
#'                                                                             "UNKNOWN FUNGUS"
#'                                                         ),
#'                                    filter_values = c('CANCELLED|VIRUS|SIMPLEX|PARAINFLUENZA|
#'                                                      INFLUENZA A|INFLUENZA B|TICK|
#'                                                      AFB GROWN|GRAM VARIABLE RODS|HYMENOLEPIS'))
#'
#' @export
cleanse_urine_organism_names <- function(data,
                                       column_name="org_name",
                                       strings_to_remove=NULL,
                                       standard_mapping=NULL,
                                       filter_values=NULL
                                   ){
      stopifnot(is.data.frame(data))

      if(length(strings_to_remove) > 0){
        data[[column_name]] <- stringr::str_replace_all(data[[column_name]], paste(strings_to_remove, collapse = "|"), "")
      }

      if(length(standard_mapping) > 0){
        for (mapping_key in names(standard_mapping)) {
          data[[column_name]] <- ifelse(grepl(names(standard_mapping[mapping_key]),data[[column_name]]),
                                        unname(standard_mapping[mapping_key]) ,data[[column_name]] )
        }
      }

      if(length(filter_values) > 0){
        data <- data %>% dplyr::filter(!grepl(paste(filter_values, collapse = "|"), data[[column_name]]))
      }

      return(data)
}
