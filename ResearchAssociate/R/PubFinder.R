#' Retrieve all publications related to user-defined search query from PubMed database
#'
#' @param Searchquery input a text query for example "Bioinformatics", "Bioinformatics or machine learning", "Cancer and drug targets"
#'
#' @param Publicationinfo information to be returned from pubmed such paper title, last author, journal or number of citations
#'
#' @param Output path to save file containig results returened by the function.
#'
#' @author Mohmed Soudy \email{Mohamed.soudy@57357.com} and Ali Mostafa \email{ali.mo.anwar@std.agr.cu.edu.eg}
#'
#' @examples Publications <- Get.Publication.info(Searchquery = "physics and deep learning and proteomics")
#'
#' @export

Get.Publication.info <-function(Searchquery,
           Publicationinfo = c("title",
                               "source",
                               "lastauthor",
                               "pubtype",
                               "pubdate",
                               "pmcrefcount"),Output = NULL)
  {
    #Get pubmed ids
    Query_result <- entrez_search(db = "pubmed", term = Searchquery)
    Query_result <- entrez_search(db = "pubmed", term = Searchquery,
                    retmax = Query_result$count)

    #Get punmed ids

    PubmedIds <- Query_result$ids
    Publication.data <-
      as.data.frame(matrix(ncol = length(Publicationinfo), nrow = 1))
    colnames(Publication.data) <- Publicationinfo
    Counter <- 1
    for (Id in PubmedIds)
    {
      taxize_summ <- tryCatch({
        entrez_summary(db = "pubmed", id = Id)
      }, error = function(cond) {
        message(paste0("ID ", Id, " cause an error."))
      })

      print(paste0(Counter, " Finished out of ", length(PubmedIds)))
      Counter <- Counter + 1


      Publication.info <- NULL
      for (Paperinfo in Publicationinfo)
      {
        Publication.info <- c(Publication.info, paste0(taxize_summ[[Paperinfo]], collapse = " "))
      }

      Publication.data <- rbind(Publication.data, Publication.info)
    }
    Publication.data <- na.omit(Publication.data)
    if (!is.null(Output))
    {
      write.csv(Publication.data, paste0(Output, "/Publications.csv"))
    }
    return(Publication.data)
  }
