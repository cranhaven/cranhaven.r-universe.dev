## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=TRUE----------------------------------------------------------------
googleSheetsURL <-
  "https://docs.google.com/spreadsheets/d/1WMO15xroy4a0RfpuZ8GhT-NfDoxwS34w9PrWp8rGjjk";

## ----echo=TRUE, message=FALSE, results='hide', eval=FALSE---------------------
# behaviorchange::complecs(googleSheetsURL);

## ----echo=FALSE, results='asis', message=FALSE--------------------------------

cat(
  behaviorchange::complecs(
    readRDS(
      system.file(package="behaviorchange",
        "extdata",
        "COMPLECS-spec-example.Rds"
      )
    ),
    returnSvgOnly = TRUE
  )
);

# cat(
#   behaviorchange::complecs(
#     system.file(package="behaviorchange",
#       "extdata",
#       "COMPLECS-spec-example.xlsx"
#     ),
#     returnSvgOnly = TRUE
#   )
# );

#cat(behaviorchange::complecs(googleSheetsURL,
#                             returnSvgOnly = TRUE));

## ----eval=FALSE, echo=TRUE----------------------------------------------------
# behaviorchange::complecs(googleSheetsURL,
#                          outputFile = "~/complecs-overview-example.pdf");

