## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval=FALSE--------------------------------------------------------------
#  preregr::form_to_rmd_template(
#    "OSFprereg_v1",
#    file = "C:/Path/To/A/File.Rmd"
#  );

## -----------------------------------------------------------------------------
rmdTemplate_example <-
  preregr::form_to_rmd_template(
    "OSFprereg_v1"
  );

### Show the fragment
cat(rmdTemplate_example[45:88], sep = "\n");

## -----------------------------------------------------------------------------
cat(rmdTemplate_example[835:850], sep = "\n");

## ---- eval=FALSE--------------------------------------------------------------
#  preregr::form_to_rmd_template(
#    "OSFprereg_v1",
#    file = "C:/Path/To/Child-document.Rmd",
#    includeYAML = FALSE
#  );

## ---- echo=FALSE, comment=""--------------------------------------------------
cat('```{r including-the-child, child = "C:/Path/To/Child-document.Rmd"}\n');
cat('```');

## ----preregistration-export, echo=FALSE, comment=""---------------------------
cat(
  '```{r, preregistration-export}',
  'preregr::prereg_spec_to_pdf(',
  '  preregrObject,',
  '  file = here::here("prereg", "registration-1---preregistration.pdf"),',
  '  author = rmarkdown::metadata$author',
  ');',
  '```',
  sep="\n"
);

