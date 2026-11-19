## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----glossary, echo=FALSE, message=FALSE--------------------------------------
library(CopernicusMarine)
glossary <- cms_glossary()
glossary[] <- lapply(glossary, \(x) {
  x <- gsub("\\[(.*?)\\]\\((.*?)\\)", "<a href='\\2'>\\1</a>", x)
  x[is.na(x)] <- "-"
  x
})

if (requireNamespace("DT")) {
  DT::datatable(
    glossary,
    filter  = "top",
    escape  = FALSE,
    options = list(
      pageLength = 5,
      rownames   = FALSE,
      autoWidth  = TRUE,
      dom        = "tpi",
      columnDefs = list(
        list(targets = 0, visible = FALSE)
      )
    )
  )
} else {
  knitr::kable(glossary)
}

