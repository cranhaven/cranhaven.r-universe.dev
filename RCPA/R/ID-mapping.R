#' @title Get supported platforms
#' @description Get supported platforms
#' @return A named list of supported platforms, where the names are the platform IDs and the values are the corresponding annotation packages from Bioconductor.
#' @examples
#' 
#' library(RCPA)
#' supportedPlatforms <- getSupportedPlatforms()
#' 
#' @export
getSupportedPlatforms <- function(){
    list(
      GPL96 = "hgu133a.db",
      GPL97 = "hgu133b.db",
      GPL570 = "hgu133plus2.db",
      GPL6244 = "hugene10sttranscriptcluster.db",
      GPL4866 = "hgu133plus2.db",
      GPL16311 = "hgu133plus2.db",
      GPL571 = "hgu133a2.db",
      GPL10739 = "hugene10sttranscriptcluster.db",
      GPL201 = "hgfocus.db",
      GPL8300 = "hgu95av2.db",
      GPL80 = "hu6800.db",
      GPL1261 = "mouse4302.db",
      GPL6246 = "mogene10sttranscriptcluster.db",
      GPL81 = "mgu74a.db",
      GPL339 = "moe430a.db",
      GPL10740 = "mogene10sttranscriptcluster.db",
      GPL16570 = "mogene20sttranscriptcluster.db",
      GPL11044 = "mouse4302.db",
      GPL6193 = "moex10sttranscriptcluster.db",
      GPL13667 = "hgu219.db",
      GPL23126 = "clariomdhumantranscriptcluster.db",
      GPL6102 = "illuminaHumanv2.db",
      GPL6947 = "illuminaHumanv3.db",
      GPL71 = "ag.db",
      GPL198 = "ath1121501.db",
      GPL3738 = "canine2.db",
      GPL200 = "celegans.db",
      GPL3213 = "chicken.db",
      GPL72 = "drosgenome1.db",
      GPL1322 = "drosophila2.db",
      GPL74 = "hcg110.db",
      GPL91 = "hgu95av2.db",
      GPL92 = "hgu95b.db",
      GPL93 = "hgu95c.db",
      GPL94 = "hgu95d.db",
      GPL95 = "hgu95e.db",
      GPL3921 = "hthgu133a.db",
      GPL98 = "hu35ksuba.db",
      GPL99 = "hu35ksubb.db",
      GPL100 = "hu35ksubc.db",
      GPL101= "hu35ksubd.db",
      GPL32 = "mgu74a.db",
      GPL33 = "mgu74b.db",
      GPL82 = "mgu74bv2.db",
      GPL34 = "mgu74c.db",
      GPL83 = "mgu74cv2.db",
      GPL340 = "mouse4302.db",
      GPL75 = "mu11ksuba.db",
      GPL76 = "mu11ksubb.db",
      GPL77 = "mu19ksuba.db",
      GPL78 = "mu19ksubb.db",
      GPL79 = "mu19ksubc.db",
      GPL3533 = "porcine.db",
      GPL341 = "rae230a.db",
      GPL342 = "rae230b.db",
      GPL1355 = "rat2302.db",
      GPL85 = "rgu34a.db",
      GPL86 = "rgu34b.db",
      GPL87 = "rgu34c.db",
      GPL88 = "rnu34.db",
      GPL89 = "rtu34.db",
      GPL1352 = "u133x3p.db",
      GPL2529 = "yeast2.db",
      GPL90 = "ygs98.db",
      GPL1319 = "zebrafish.db"
    )
}

#' @title Get ID mapping annotation from GEO platform
#' @description This function gets ID mapping annotation from GEO platform.
#' This function is used internally by runDEAnalysis.
#' @param platform GEO platform ID. E.g., GPL570
#' @return A data frame with ID mapping annotation and two columns: FROM and TO.
#' The first column is the probe ID and the second column is the entrez ID.
#' @importFrom AnnotationDbi keys
#' @importFrom dplyr %>%
#' @importFrom tidyr drop_na
#' @noRd
.getIDMappingAnnotation <- function(platform) {

    annotations <- getSupportedPlatforms()

    if (grep("GPL", platform)) {
        if (!is.null(annotations[[platform]])) {
            anno <- annotations[[platform]]
            if (!.requirePackage(anno)){
                return(NULL)
            }
            annotation <- get(anno)
        } else {
            stop(paste0("Platform ", platform, " is not supported. Please pass an AnnotationDbi object instead"))
        }
    }

    suppressMessages({
        AnnotationDbi::select(annotation, keys = AnnotationDbi::keys(annotation, keytype = "PROBEID"), columns = c("PROBEID", "ENTREZID"), keytype = "PROBEID")
    }) %>%
        `colnames<-`(c("FROM", "TO")) %>%
        drop_na()

}

#' @title Map probe IDs to gene symbols
#' @description This function maps probe IDs to gene symbols.
#' This function is used internally by runDEAnalysis.
#' @param exprs Expression matrix. Rows are genes and columns are samples.
#' @param annotation Annotation data frame with mapping between probe IDs and entrez IDs.
#' The data frame must have two columns: FROM and TO. The first column is the probe ID and the second column is the entrez ID.
#' @param DEResults DE analysis results data frame. Must have a column named ID and p.value.
#' @return A list with two elements:
#' \itemize{
#'  \item exprs: Expression matrix with probe IDs mapped to gene symbols. Rows are genes and columns are samples.
#'  \item DEResults: DE analysis results data frame with probe IDs mapped to gene symbols.
#' }
#' @importFrom dplyr %>% arrange select group_by mutate first rename left_join summarize_all
#' @importFrom tidyr drop_na
#' @noRd
.mapIDs <- function(exprs, annotation, DEResults) {

    if (is.null(annotation)) {
        return(list(
            exprs = exprs,
            DEResults = DEResults,
            mapping = data.frame(ID = rownames(exprs), TO = rownames(exprs))
        ))
    }

    if (is.null(DEResults)) {
        stop("DE results are required for mapping probe IDs to entrez IDs")
    }

    top <- DEResults %>% arrange(.data$p.value)

    mapping <- top[, "ID", drop = F] %>%
        left_join(annotation, by = c("ID" = "FROM")) %>%
        group_by(.data$ID) %>%
        mutate(TO.FIRST = first(.data$TO)) %>%
        `[`(c("ID", "TO.FIRST")) %>%
        unique() %>%
        group_by(.data$TO.FIRST) %>%
        mutate(ID.FIRST = first(.data$ID)) %>%
        `[`(c("ID.FIRST", "TO.FIRST")) %>%
        unique() %>%
        mutate(ID = .data$ID.FIRST, TO = .data$TO.FIRST) %>%
        `[`(c("ID", "TO")) %>%
        drop_na()

    mappedExprs <- exprs[mapping$ID,] %>% `rownames<-`(mapping$TO)

    mappedDEResults <- DEResults %>%
        left_join(mapping, by = "ID") %>%
        mutate(ID = .$TO) %>%
        `[`(!is.na(.$ID), -ncol(.)) %>%
        `rownames<-`(.$ID) %>%
        `[`(mapping$TO, )

    list(
        exprs = mappedExprs,
        DEResults = mappedDEResults,
        mapping = mapping
    )
}
