## --------------- metosite.R ---------------- ##
#                                               #
#   meto.search                                 #
#   meto.scan                                   #
#   meto.list                                   #
#                                               #
## ------------------------------------------- ##

## ----------------------------------------------------------------- ##
#                 meto.search <- function(...)                        #
## ----------------------------------------------------------------- ##
#' Search for Specific MetO Sites
#' @description Searches for specific MetO sites filtering MetOSite according to the selected criteria.
#' @usage meto.search(highthroughput.group = TRUE,
#'                    bodyguard.group = TRUE,
#'                    regulatory.group = TRUE,
#'                    gain.activity = 2, loss.activity = 2, gain.ppi = 2,
#'                    loss.ppi = 2, change.stability = 2, change.location = 2,
#'                    organism = -1, oxidant = -1)
#' @param highthroughput.group logical, when FALSE the sites described in a high-throughput study (unknown effect) are filtered out.
#' @param bodyguard.group logical, when FALSE the sites postulated to function as ROS sink (because when oxidized no apparent effect can be detected) are filtered out.
#' @param regulatory.group logical, when FALSE the sites whose oxidation affect the properties of the protein (and therefore may be involved in regulation) are filtered out.
#' @param gain.activity introduce 1 or 0 to indicate whether the oxidation of the selected sites implies a gain of activity or not, respectively. If we do not wish to use this property to filter, introduce 2.
#' @param loss.activity introduce 1 or 0 to indicate whether or not the oxidation of the selected sites implies a loss of activity or not, respectively. If we do not wish to use this property to filter, introduce 2.
#' @param gain.ppi introduce 1 or 0 to indicate whether the oxidation of the selected sites implies a gain of protein-protein interaction or not, respectively. If we do not wish to use this property to filter, introduce 2.
#' @param loss.ppi introduce 1 or 0 to indicate whether or not the oxidation of the selected sites implies a loss of protein-protein interaction or not, respectively. If we do not wish to use this property to filter, introduce 2.
#' @param change.stability introduce 1 or 0 to indicate whether the oxidation of the selected sites leads to a change in the protein stability or not, respectively. If we do not wish to use this property to filter, introduce 2.
#' @param change.location introduce 1 or 0 to indicate whether or not the oxidation of the selected sites implies a change of localization or not, respectively. If we do not wish to use this property to filter, introduce 2.
#' @param organism a character string indicating the scientific name of the species of interest, or -1 if we do not wish to filter by species.
#' @param oxidant a character string indicating the oxidant, or -1 if we do not wish to filter by oxidants.
#' @details Note that all the arguments of this function are optional. We only pass an argument to the function when we want to use that parameter to filter. Thus, meto.search() will return all the MetO sites found in the database MetOSite.
#' @return This function returns a dataframe with a line per MetO site.
#' @author Juan Carlos Aledo
#' @examples meto.search(organism = 'Homo sapiens', oxidant = 'HClO')
#' @references Valverde et al. 2019. Bioinformatics 35:4849-4850 (PMID: 31197322)
#' @seealso meto.scan(), meto.list()
#' @importFrom jsonlite fromJSON
#' @export

meto.search <-  function(highthroughput.group = TRUE,
                         bodyguard.group = TRUE,
                         regulatory.group = TRUE,
                         gain.activity = 2,
                         loss.activity = 2,
                         gain.ppi = 2,
                         loss.ppi = 2,
                         change.stability = 2,
                         change.location = 2,
                         organism = -1,
                         oxidant = -1)
{

  groups <- paste(sum(highthroughput.group),
                  sum(bodyguard.group),
                  sum(regulatory.group), sep="")

  ## ------------------ Mapping properties to FCs --------------------- ##
  affected.properties <- paste(gain.activity, loss.activity,
                               gain.ppi, loss.ppi,
                               change.stability, change.location, sep ="")
  call <- paste('https://metosite.uma.es/api/sites/mapping/',
                groups, '/', affected.properties, sep = "")

  FCs <- gracefully_fail(call) # Functional Categories

  if (is.null(FCs)){
    message("Call to MetOSite failed")
    return(NULL)
  }
  # response <- httr::GET(call)
  # FCs <-  httr::content(response, 'text') # Functional Categories

  ## ----------------- Formatting FCs for query ---------------------- ##
  formatted.fc <- gsub("\\[|\\]", "", FCs)
  formatted.fc <- gsub(",", "&", formatted.fc)

  ## --------------- Formatting organism for query ------------------- ##
  if (organism != -1){
    organism <- gsub(" ", "%20", organism)
  }

  ## ---------------- Formatting oxidant for query ------------------- ##
  if (oxidant != -1){
    oxidant <- gsub(" ", "%20", oxidant)
  }

  ## -------------------- Quering MetOSite --------------------------- ##
  call <- paste('https://metosite.uma.es/api/sites/search/',
                formatted.fc, '/', organism, '/', oxidant, sep = "")

  entries <- gracefully_fail(call)

  if (is.null(entries)){
    message("Query to MetOSite failed")
    return(NULL)
  } else {
    df.entries <- jsonlite::fromJSON(entries, flatten = TRUE)
    return(df.entries)
  }
}

## ----------------------------------------------------------------- ##
#       meto.scan <- function(up_id, report)                          #
## ----------------------------------------------------------------- ##
#' Scans a Protein in Search of  MetO Sites
#' @description Scans a given protein in search of MetO sites.
#' @usage meto.scan(up_id, report = 1)
#' @param up_id a character string corresponding to the UniProt ID.
#' @param report it should be a natural number between 1 and 3.
#' @details When the 'report' parameter has been set to 1, this function returns a brief report providing the position,  the function category and literature references concerning the residues detected as MetO, if any. If we wish to obtain a more detailed report, the option should be: report = 2. Finally, If we want a detailed and printable report (saved in the current directory), we should set report = 3
#' @return This function returns a report regarding the MetO sites found, if any, in the protein of interest.
#' @author Juan Carlos Aledo
#' @examples meto.scan('P01009')
#' @references Valverde et al. 2019. Bioinformatics 35:4849-4850 (PMID: 31197322)
#' @seealso meto.search(), meto.list()
#' @importFrom jsonlite fromJSON
#' @export

meto.scan <- function(up_id, report = 1){

  call <- paste('https://metosite.uma.es/api/proteins/scan/', up_id,  sep = "")
  output <- gracefully_fail(call)
  if (is.null(output)){
    message("MetOSite couldn't return an answer")
    return(NULL)
  } else {
    output <- jsonlite::fromJSON(output, flatten = TRUE)
  }

  if (report == 1){

    brief_output <- list()
    brief_output$Metosites <- output$Metosites
    brief_output$Note <- output$prot_note
    return(brief_output)

  } else if (report == 2){

    long_output <- output
    return(long_output)

  } else if (report == 3){

    if (requireNamespace("markdown", quietly = TRUE)){
      markobj <- c('---',
                   'title: "Scan Report"',
                   'output: word_document',
                   '---',
                   '',
                   '```{r, out.width = "50px", fig.align="left", echo = FALSE}',
                   'knitr::include_graphics("/Users/JCA/Dropbox/Investigacion/R_ptm/metosite_logo.png")',
                   '```',
                   '',
                   '# Scan Report',
                   '```{r, echo = FALSE}',
                   'cat(paste("Protein name:  ", output$prot_name, sep = ""))',
                   'cat(paste("UniProt ID:  ", output$prot_id, sep = ""))',
                   'cat(paste("Nickname:  ", output$prot_nickname, sep = ""))',
                   'cat(paste("Gene name:  ", output$gene_name, sep = ""))',
                   'cat(paste("Species:  ", output$prot_sp, sep = ""))',
                   'cat(paste("Protein location:  ", output$prot_sub, sep = ""))',
                   'cat(paste("PDB ID:  ", output$prot_pdb, sep = ""))',
                   '```',

                   '```{r, echo = FALSE}',
                   'cat(paste("Protein sequence:  ", output$prot_seq, sep =""))',
                   'cat(paste("Note:  ", output$prot_note, sep = ""))',
                   '```',

                   '```{r, echo = FALSE}',
                   'cat(" ------------------ MetO sites found in this protein ----------------- ##")',
                   'print(output$Metosites)',
                   'cat(" --------------------------------------------------------------------- ##")',
                   '```',
                   '',
                   '',
                   '```{r, echo = FALSE}',
                   'cat("Phosphorylation:")',
                   'print(output$Phosphorylation)',
                   'cat("Acetylation:")',
                   'print(output$Acetylation)',
                   'cat("Methylation")',
                   'print(output$Methylation)',
                   'cat("Ubiquitination")',
                   'print(output$Ubiquitination)',
                   'cat("Sumoylation")',
                   'print(output$Sumoylation)',
                   'cat("OGlcNAc")',
                   'print(output$OGlcNAc)',
                   'cat("RegPTM")',
                   'print(output$RegPTM)',
                   'cat("Disease")',
                   'print(output$Disease)',
                   '```'
      )

      destfile <- paste('report_scan_', up_id, '.txt', sep = "")
      markdown::markdownToHTML(text = knitr::knit(text = markobj), output = destfile)
      # markdown::markdownToHTML(text = knitr::knit(text = markobj), output ='report.html')
      # browseURL("report.html")
    } else {
      warning("To get a full printable report you should install the package 'markdown'")
    }

    brief_output <- list()
    brief_output$Metosites <- output$Metosites
    brief_output$Note <- output$prot_note
    return(brief_output)

  } else {
    stop("A proper report's mode should be provided")
  }
}


## ----------------------------------------------------------------- ##
#           meto.list <- function(keyword)                            #
## ----------------------------------------------------------------- ##
#' List Proteins Found in MetOSite Matching a Keyword
#' @description Lists proteins found in MetOSite with names matching the keyword.
#' @usage meto.list(keyword)
#' @param keyword a character string corresponding to the keyword
#' @return This function returns a dataframe with the uniprot id, the protein name and the species, for those proteins present into MetOSite whose name contains the keyword.
#' @author Juan Carlos Aledo
#' @examples meto.list('inhibitor')
#' @references Valverde et al. 2019. Bioinformatics 35:4849-4850 (PMID: 31197322)
#' @seealso meto.search(), meto.scan()
#' @importFrom jsonlite fromJSON
#' @export

meto.list <- function(keyword){

  keyword <- gsub(" ", '%20', keyword) # HTML URL Encoding

  call <- paste('https://metosite.uma.es/api/proteins/pname/',
                keyword, sep = "")

  entries <- gracefully_fail(call)

  if (is.null(entries)){
    message("MetOSite's API failed")
    return(NULL)
  } else {
    df.entries <- jsonlite::fromJSON(entries, flatten = TRUE)
    return(df.entries)
  }
}

