#' Get Abstracts of user-defined search query
#'
#' @param Searchquery input a text query for example "Bioinformatics", "Bioinformatics or machine learning", "Cancer and drug targets"
#'
#' @param Output path to save file containig results returened by the function.
#'
#' @author Mohmed Soudy \email{Mohamed.soudy@57357.com} and Ali Mostafa \email{ali.mo.anwar@std.agr.cu.edu.eg}
#'
#' @examples Abstracts <- Get.Abstracts(Searchquery = "physics and deep learning and proteomics")
#'
#' @export
Get.Abstracts <- function(Searchquery, Output = NULL)
{
  #Get pubmed ids
  Query_result <- entrez_search(db = "pubmed", term = Searchquery)
  Query_result <- entrez_search(db = "pubmed", term = Searchquery,
                                retmax = Query_result$count)

  #Get punmed ids
  PubmedIds <- Query_result$ids
  Counter <- 1
  Abstract.data <- as.data.frame(matrix(ncol = 1, nrow = 1))

  for (Id in PubmedIds)
  {
    taxize_summ <- tryCatch({
      entrez_fetch(db="pubmed", id=Id, rettype = "xml", parsed = T)
    }, error = function(cond) {
      message(paste0("ID ", Id, "cause an error."))
    })
    if (is.null(taxize_summ))
    {
      message(paste0("ID ", Id, "cause a NULL error."))
      next
    }
    abstract <- xpathSApply(taxize_summ, "//AbstractText", xmlValue)
    abstract <- ifelse(length(abstract)!= 0, abstract, "Not available")

    Abstract.data <- rbind(Abstract.data, abstract)
    print(paste0(Counter, " Finished out of ", length(PubmedIds)))
    Counter <- Counter + 1
  }
  Abstract.data <- Abstract.data[-1,]
  if(!is.null(Output))
  {
    write.csv(x = Abstract.data, file = paste0(Output, "/", "Abstracts.csv"))
  }
  return(Abstract.data)
}
