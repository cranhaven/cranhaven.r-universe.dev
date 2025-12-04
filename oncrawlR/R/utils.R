#' Transform a character array of URLs into JSON file for OnCrawl platform
#'
#' @param list_urls your urls
#' @param namefile the filename for the JSON export
#' @param pathfile string. Optional. If not specified, the intermediate files are created under \code{TEMPDIR}, with the assumption that directory is granted for written permission.
#'
#' @examples
#' mylist <- c("/cat/domain","/cat/")
#' oncrawlCreateSegmentation(mylist,"test.json")
#'
#' @return JSON file
#' @author Vincent Terrasi
#' @export
oncrawlCreateSegmentation <- function(list_urls, namefile, pathfile=tempdir()) {

  if (!is.character(list_urls)) stop("the first argument for 'dataset' must be a character array")
  if (!file.exists(pathfile)) stop("the path for 'pathfile' - ", pathfile, " does not exist")

  # limit to 15 segments
  if (length(list_urls)>15)
    list_urls <- list_urls[1:15]

  newlist <- list()
  max <- length(list_urls)

  colors <- c('#e6194b', '#3cb44b', '#ffe119', '#4363d8', '#f58231', '#911eb4', '#46f0f0', '#f032e6', '#bcf60c', '#fabebe', '#008080', '#e6beff', '#9a6324', '#fffac8', '#800000', '#aaffc3', '#808000', '#ffd8b1', '#000075', '#808080', '#ffffff', '#000000')

  i <- 1

  for ( cat in list_urls ) {

    candidates <- grep(cat,list_urls)
    nbcandidates <- length(candidates)

    if (nbcandidates==1) {
      temp <- list(
        color=colors[i],
        name=cat,
        oql=list(field= c("urlpath","startswith",cat))
      )
    }
    else {

      listCandidates <- list(list(field= c("urlpath","startswith",cat)))

      # iterate with each candidates
      for (j in candidates) {

        if (cat!=list_urls[j]) {
          listCandidates <- rlist::list.append(listCandidates, list(field= c("urlpath","not_startswith",list_urls[j])))
        }

      }

      temp <- list(
        color=colors[i],
        name=cat,
        # use with AND
        oql=list(and=listCandidates)
      )

    }

    newlist <- rlist::list.append(newlist,temp)
    i <- i+1
  }

  json <- jsonlite::toJSON(newlist)
  filepath <- file.path(pathfile, namefile)
  readr::write_lines(json,filepath)

  message(paste0("your json file is generated - ",filepath))

}


splitURL <- function(url) {

  list <- unlist(strsplit(url, "/"))
  str <- list()

  while (length(list) > 1) {
    # get size
    max <- length(list)
    # disassemble URL
    str <- append(str, paste(list[1:max],collapse='/'))
    # prepare next
    max <- max - 1
    list <- list[1:max]
  }

  str
}


#' Split URLs
#'
#' @param list_urls your urls
#' @param limit the maximum of URLS you want
#'
#' @examples
#' mylist <- c("/cat/domain/web/","/cat/","/cat/domain/")
#' oncrawlSplitURL(mylist, 2)
#'
#' @return data.frame
#' @author Vincent Terrasi
#' @export
oncrawlSplitURL <- function(list_urls, limit=15) {

  uniqueUrlPath <- unique(as.character(list_urls))

  # split URLs
  allUrlPath <- lapply(uniqueUrlPath, splitURL)
  allUrlPath <- unlist(allUrlPath)

  # Method by frequency
  allUrlPath <- sort(table(allUrlPath), decreasing = TRUE)
  allUrlPath <- as.data.frame(allUrlPath, stringsAsFactors=F)
  colnames(allUrlPath) <- c("url","freq")

  # respect your limit
  top <- dplyr::top_n(allUrlPath, limit)

  return(top)

}
