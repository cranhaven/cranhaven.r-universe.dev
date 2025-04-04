#' @title Get KEGG gene sets names
#' @description This function retrieves KEGG gene sets names for a given organism.
#' @param org The organism abbreviation. E.g, hsa, mmu, dme, etc.
#' To see the full list of supported organisms, visit https://www.genome.jp/kegg/catalog/org_list.html.
#' @return A named vector with KEGG gene sets names.
#' @examples
#' \donttest{
#' .getKEGGPathwayNames("hsa")
#' }
#' @importFrom stringr str_split
#' @importFrom dplyr %>%
#' @importFrom stats setNames
#' @importFrom utils read.table
#' @noRd
.getKEGGPathwayNames <- function(org = "hsa") {
    # gsNames <- read.table(paste0("https://rest.kegg.jp/list/pathway/", org), sep = "\t", header = FALSE, stringsAsFactors = FALSE)
    toUseURL <- paste0("https://rest.kegg.jp/list/pathway/", org)
    
    .checkURLAvailable(toUseURL)
    
    gsNames <- read.table(toUseURL, sep = "\t", header = FALSE, stringsAsFactors = FALSE)
    
    gsNames[, 2] %>%
        str_split(" - ") %>%
        sapply(function(x) paste0(x[1:(length(x) - 1)], collapse = " - ")) %>%
        setNames(paste0("path:", gsNames[, 1]))
}

#' @title Get KEGG gene sets
#' @description This function retrieves KEGG gene sets for a given organism.
#' @param org The organism abbreviation. E.g, hsa, mmu, dme, etc.
#' To see the full list of supported organisms, visit https://www.genome.jp/kegg/catalog/org_list.html.
#' @return A named list with three elements: database, genesets and names.
#' @examples
#' \donttest{
#' .getKEGGGeneSets("hsa")
#' }
#' @importFrom stringr str_split
#' @importFrom dplyr %>% left_join group_by group_split mutate select
#' @importFrom tidyr drop_na
#' @importFrom stats setNames
#' @noRd
.getKEGGGeneSets <- function(org = "hsa") {
    # geneLink <- read.table(paste0("https://rest.kegg.jp/link/", org, "/pathway"), sep = "\t", header = FALSE, stringsAsFactors = FALSE) %>%
    #     `colnames<-`(c("geneset", "gene"))

    toUseURL <- paste0("https://rest.kegg.jp/link/", org, "/pathway")
    
    .checkURLAvailable(toUseURL)
    
    geneLink <- read.table(toUseURL, sep = "\t", header = FALSE, stringsAsFactors = FALSE) %>%
        `colnames<-`(c("geneset", "gene"))
    
    gensets <- geneLink %>%
        group_by(.$geneset) %>%
        group_split() %>%
        lapply(function(df) {
            list(
                name = df$geneset[1] %>% as.character(),
                gene = df$gene %>% as.character() %>% str_split(":") %>% sapply(function(x) x[2])
            )
        }) %>%
        `names<-`(lapply(., function(gl) gl$name)) %>%
        lapply(function(gl) gl$gene)

    list(
        database = "KEGG",
        genesets = gensets,
        names = .getKEGGPathwayNames(org)[names(gensets)]
    )
}

#' @title Get GO terms names
#' @description This function retrieves GO terms for a given organism.
#' @param namespace The namespace of the GO terms. E.g, biological_process, molecular_function, cellular_component.
#' @return A named vector with GO terms names.
#' @importFrom stringr str_split_1 str_starts str_detect str_match
#' @importFrom dplyr filter
#' @importFrom stats setNames
#' @noRd
.getGOTermNames <- function(namespace = c("biological_process", "molecular_function", "cellular_component")) {
    namespace <- match.arg(namespace)
    
    toUseURL <- "http://purl.obolibrary.org/obo/go.obo"
    
    .checkURLAvailable(toUseURL)
    
    # con <- url("http://purl.obolibrary.org/obo/go.obo")
    con <- url(toUseURL)
    rawText <- readLines(con) %>% paste(collapse = "\n")
    close(con)

    terms <- str_split_1(rawText, "\n\n") %>%
        `[`(which(sapply(., function(x) str_starts(x, "\\[Term\\]") && !str_detect(x, "is_obsolete")))) %>%
        lapply(function(x) {
            data.frame(
                id = str_match(x, "\nid: (.*)")[, 2],
                name = str_match(x, "\nname: (.*)")[, 2],
                ns = str_match(x, "\nnamespace: (.*)")[, 2],
                stringsAsFactors = FALSE
            )
        }) %>%
        do.call(what = rbind) %>%
        filter(.$ns == namespace)

    terms$name %>% setNames(terms$id)
}

#' @title Get GO terms
#' @description This function retrieves GO terms for a given organism.
#' @param taxid The NCBI taxonomy ID of the organism.
#' @param namespace The namespace of the GO terms. E.g, biological_process, molecular_function, cellular_component.
#' @return A named list with three elements: database, genesets and names.
#' @importFrom utils read.table download.file
#' @importFrom dplyr %>% filter group_by group_split rename select
#' @importFrom stats setNames
#' @noRd
.getGOTerms <- function(taxid = 9606, namespace = c("biological_process", "molecular_function", "cellular_component")) {

    namespace <- match.arg(namespace)
    
    toUseURL <- "https://ftp.ncbi.nih.gov/gene/DATA/gene2go.gz"
    
    .checkURLAvailable(toUseURL)
    
    tmpTarget <- tempfile(fileext = ".gz")
    on.exit({
      unlink(tmpTarget)
    })
    
    oldTimeout <- options("timeout")
    on.exit({options(timeout = oldTimeout)})
    options(timeout = 3600)
    # download.file("https://ftp.ncbi.nih.gov/gene/DATA/gene2go.gz", tmpTarget)
    download.file(toUseURL, tmpTarget)
    
    cat <- switch(namespace,
                  biological_process = "Process",
                  molecular_function = "Function",
                  cellular_component = "Component")
    
    gzCon <- gzfile(tmpTarget, open = "r")
    on.exit({
      close(gzCon)
    })
    headers <- readLines(gzCon, 1) %>%
      stringr::str_split("\t") %>%
      unlist() %>%
      make.names()
    
    genesets <- list()
    
    while (TRUE) {
      lines <- readLines(gzCon, 100000)
      
      if (length(lines) == 0) {
        break
      }
      
      genesetsTmp <- read.table(textConnection(lines), sep = "\t", header = FALSE, stringsAsFactors = FALSE, fill = TRUE, comment.char = "!", quote = "") %>%
        `colnames<-`(headers) %>%
        rename(tax_id = "X.tax_id") %>%
        filter(.$tax_id == taxid & .$Category == cat) %>%
        group_by(.$GO_ID) %>%
        group_split() %>%
        lapply(function(df) {
          list(
            name = df$GO_ID[1] %>% as.character(),
            geneIds = df$GeneID %>% as.character()
          )
        }) %>%
        setNames(lapply(., function(gl) gl$name)) %>%
        lapply(function(gl) gl$geneIds)
      
      for (gsId in names(genesetsTmp)) {
        if (is.null(genesets[[gsId]])) {
          genesets[[gsId]] <- genesetsTmp[[gsId]]
        } else {
          genesets[[gsId]] <- union(genesets[[gsId]], genesetsTmp[[gsId]])
        }
      }
    }
    
    if (length(genesets) == 0) {
      stop("No GO terms found for the given organism and namespace.")
    }
    
    goTermNames <- .getGOTermNames(namespace)
    
    list(
      database = "GO",
      genesets = genesets,
      names = goTermNames[names(genesets)]
    )

}

#' @title Get gene sets
#' @description This function retrieves gene sets for a given organism.
#' @param database The database of the gene sets. E.g, KEGG, GO.
#' @param org The organism abbreviation. E.g, hsa, mmu, dme, etc.
#' To see the full list of supported organisms, visit https://www.genome.jp/kegg/catalog/org_list.html.
#' This parameter is only used when database is KEGG.
#' @param taxid The NCBI taxonomy ID of the organism.
#' This parameter is only used when database is GO.
#' @param namespace The namespace of the GO terms. E.g, biological_process, molecular_function, cellular_component.
#' @param minSize The minimum size of the gene sets.
#' @param maxSize The maximum size of the gene sets.
#' @param useCache A boolean parameter specifying if using pre-saved downloaded geneset database. It is FALSE by default.
#' @return A named list with three elements: database, genesets and names.
#' @examples
#' \donttest{
#'
#' library(RCPA)
#'
#' KEGGgenesets <- getGeneSets("KEGG", org = "hsa", 
#'                               minSize = 10, maxSize = 1000, useCache = TRUE)
#' 
#' GOterms <- getGeneSets("GO", taxid = 9606, 
#'                         namespace = "biological_process", 
#'                         minSize = 10, maxSize = 1000, useCache = TRUE)
#'
#' }
#' @export
getGeneSets <- function(database = c("KEGG", "GO"), org = "hsa", taxid = 9606, namespace = c("biological_process", "molecular_function", "cellular_component"), minSize = 1, maxSize = 1000, useCache = FALSE) {
    database <- match.arg(database)

    if (database == "KEGG") {
        if (is.null(org)) {
            stop("Organism must be specified")
        }
      
        if (useCache) {
          oldTimeout <- options("timeout")
          on.exit({options(timeout = oldTimeout)})
          options(timeout = 3600)
          name <- paste0(database, "_", org)
          data <- try({load(gzcon(url(paste0("https://raw.githubusercontent.com/tinnlab/RCPA/main/genesets/", name, ".rda"))))}, silent = TRUE)
          if (inherits(data, "try-error")) {
            gs <- .getKEGGGeneSets(org)
          } else {
            gs <- get(data)
          }
        } else {
          gs <- .getKEGGGeneSets(org)
        }

        # gs <- .getKEGGGeneSets(org)
    } else if (database == "GO") {

        if (is.null(taxid)) {
            stop("Taxonomy ID must be specified")
        }

        namespace <- match.arg(namespace)
        
        if (useCache) {
          oldTimeout <- options("timeout")
          on.exit({options(timeout = oldTimeout)})
          options(timeout = 3600)
          name <- paste0(database, "_", taxid, "_", namespace)
          data <- try({load(gzcon(url(paste0("https://raw.githubusercontent.com/tinnlab/RCPA/main/genesets/", name, ".rda"))))}, silent = TRUE)
          if (inherits(data, "try-error")) {
            gs <- .getGOTerms(taxid, namespace)
          } else {
            gs <- get(data)
          }
        } else {
          gs <- .getGOTerms(taxid, namespace)
        }

        # gs <- .getGOTerms(taxid, namespace)
    } else {
        stop("Database not supported")
    }

    gsLength <- sapply(gs$genesets, length)
    keeps <- gsLength >= minSize & gsLength <= maxSize
    gs$genesets <- gs$genesets[keeps]
    gs$names <- gs$names[keeps]

    gs
}
