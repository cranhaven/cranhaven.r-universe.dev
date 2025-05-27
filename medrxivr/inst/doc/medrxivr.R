## ----echo=FALSE, message=FALSE,warning=FALSE----------------------------------
# Delete when done
library(medrxivr)
library(dplyr)

knitr::opts_chunk$set(
  collapse = TRUE,
  eval = FALSE,
  comment = "#>"
)


## -----------------------------------------------------------------------------
#  devtools::install_github("mcguinlu/medrxivr")
#  library(medrxivr)

## -----------------------------------------------------------------------------
#  # Get a copy of the database from the live medRxiv API endpoint
#  preprint_data <- mx_api_content()

## -----------------------------------------------------------------------------
#  # Get a copy of the database from the daily snapshot
#  preprint_data <- mx_snapshot()

## ----eval = TRUE, echo = FALSE, out.width = "500px", out.height = "400px"-----

knitr::include_graphics("data_sources.png")

## -----------------------------------------------------------------------------
#  # Get a copy of the database from the live bioRxiv API endpoint
#  preprint_data <- mx_api_content(server = "biorxiv")

## -----------------------------------------------------------------------------
#  
#  # Perform a simple search
#  results <- mx_search(data = preprint_data,
#                       query ="dementia")
#  
#  # Perform an advanced search
#  topic1  <- c("dementia","vascular","alzheimer's")  # Combined with Boolean OR
#  topic2  <- c("lipids","statins","cholesterol")     # Combined with Boolean OR
#  myquery <- list(topic1, topic2)                    # Combined with Boolean AND
#  
#  results <- mx_search(data = preprint_data,
#                       query = myquery)
#  

## ----eval = TRUE, echo = FALSE------------------------------------------------

mx_variables <-
  data.frame(
    Variable = c(
         "ID"      ,
         "title"   ,
         "abstract",
         "authors" ,
         "date"    ,
         "category",
         "doi"     ,
         "version" ,
         "author_corresponding",
         "author_corresponding_institution",
         "link_page",
         "link_pdf" ,
         "license"  ,
         "published"
    ),
    Description = c(
      "Unique identifier",
      "Preprint title",
      "Preprint abstract",
      "Author list in the format 'LastName, InitalOfFirstName.' (e.g. McGuinness, L.). Authors are seperated by a semi-colon.",
      "Date the preprint was posted, in the format YYYYMMDD.",
      "On submission, medRxiv asks authors to classify their preprint into one of a set number of subject categories.",
      "Preprint Digital Object Identifier.",
      "Preprint version number. As authors can update their preprint at any time, this indicates which version of a given preprint the record refers to.", 
      "Corresponding authors name.",
      "Corresponding author's institution.",
      "Link to preprint webpage. The \"?versioned=TRUE\" is required, as otherwise, the URL will resolve to the most recent version of the article (assuming there is >1 version available).",
      "Link to preprint PDF. This is used by `mx_download()` to download a copy of the PDF for that preprint.",
      "Preprint license",
      "If the preprint was subsequently published in a peer-reviewed journal, this variable contains the DOI of the published version."
    )
  )


knitr::kable(mx_variables, format = "html") %>%
  kableExtra::kable_styling(full_width = F) %>%
  kableExtra::column_spec(1, bold = T, border_right = T) %>%
  kableExtra::column_spec(2, width = "30em")

## ----eval = FALSE-------------------------------------------------------------
#  
#  mx_export(data = mx_results,
#            file = tempfile(fileext = ".bib"))
#  

## ----eval = FALSE-------------------------------------------------------------
#  
#  mx_download(results,        # Object returned by mx_search
#              tempdir(),      # Temporary directory to save PDFs to
#              create = TRUE)  # Create the directory if it doesn't exist
#  

