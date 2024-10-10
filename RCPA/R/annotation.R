#' @title Get Entrez annotation
#' @description This function gets Entrez annotation.
#' @param entrezIds A vector of Entrez IDs.
#' @return A data frame with Entrez annotation. The columns are ID (Entrez ID), Symbol, Description, OtherDesignations, OtherAliases and Chromosome.
#' @examples
#' 
#' library(RCPA)
#' geneAnno <- getEntrezAnnotation(c("77267466", "77267467"))
#' 
#' @importFrom httr POST content
#' @importFrom dplyr %>%
#' @importFrom rlang interrupt
#' @export
getEntrezAnnotation <- function(entrezIds) {

    if (!.requirePackage("XML")){
        return(NULL)
    }

    url <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi"
    query <- list(
        db = "gene",
        id = as.character(entrezIds) %>%
            unique() %>%
            paste0(collapse = ",")
    )
    # res <- POST(url, query = query) %>% content(as = "text")
    # xml <- XML::xmlParse(res)
    xml <- try({
      res <- POST(url, query = query) %>% content(as = "text")
      XML::xmlParse(res)
    }, silent = TRUE)
    
    if (inherits(xml, "try-error")) {
      # df <- data.frame(matrix(ncol = 6, nrow = 0))
      # colnames(df) <- c("ID", "Symbol", "Description", "OtherDesignations", "OtherAliases", "Chromosome")
      # df
      
      warning("Data could not be retrieved. Please try it again later or contact the maintainer(s) to solve this issue.")
      rlang::interrupt()
      
    } else {
      xpath <- "//DocumentSummary"
      XML::xpathApply(xml, xpath, function(xmlDoc) {
        listDat <- XML::xmlToList(xmlDoc)
        data.frame(
          ID = listDat$.attrs[["uid"]],
          Symbol = ifelse(is.null(listDat$Name), NA, listDat$Name),
          Description = ifelse(is.null(listDat$Description), NA, listDat$Description),
          OtherDesignations = ifelse(is.null(listDat$OtherDesignations), NA, listDat$OtherDesignations),
          OtherAliases = ifelse(is.null(listDat$OtherAliases), NA, listDat$OtherAliases),
          Chromosome = ifelse(is.null(listDat$Chromosome), NA, listDat$Chromosome),
          stringsAsFactors = FALSE
        )
      }) %>%
        do.call(what = rbind) %>%
        `rownames<-`(.$ID)
    }

    # xpath <- "//DocumentSummary"
    # XML::xpathApply(xml, xpath, function(xmlDoc) {
    #     listDat <- XML::xmlToList(xmlDoc)
    #     data.frame(
    #         ID = listDat$.attrs[["uid"]],
    #         Symbol = ifelse(is.null(listDat$Name), NA, listDat$Name),
    #         Description = ifelse(is.null(listDat$Description), NA, listDat$Description),
    #         OtherDesignations = ifelse(is.null(listDat$OtherDesignations), NA, listDat$OtherDesignations),
    #         OtherAliases = ifelse(is.null(listDat$OtherAliases), NA, listDat$OtherAliases),
    #         Chromosome = ifelse(is.null(listDat$Chromosome), NA, listDat$Chromosome),
    #         stringsAsFactors = FALSE
    #     )
    # }) %>%
    #     do.call(what = rbind) %>%
    #     `rownames<-`(.$ID)
}
