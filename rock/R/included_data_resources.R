### Easily update everything
updateEverything <- FALSE;

#' An very rudimentary example codebook specification
#'
#' This is a simple and relatively short codebook
#' specification.
#'
#' @format An example of a codebook specification
#'
"exampleCodebook_1"

### Inclusive General-Purpose Registration Form
if (exists("updateEverything") && updateEverything) {

  gSheet_url <-
    "https://docs.google.com/spreadsheets/d/1gVx5uhYzqcTH6Jq7AYmsLvHSBaYaT-23c7ZhZF4jmps";

  localBackupFile <-
    here::here(
      "inst", "extdata", "exampleCodebook_1.xlsx"
    );

  yamlFile <-
    here::here(
      "inst", "extdata", "exampleCodebook_1.yml"
    );

  exampleCodebook_1 <-
    rock::codebook_fromSpreadsheet(
      gSheet_url,
      localBackup = localBackupFile,
      silent=FALSE
    );

  usethis::use_data(exampleCodebook_1, overwrite=TRUE);

  # ### We also store a completed version of this form; also in in YAML and JSON
  # examplePrereg_1 <-
  #   preregr::prereg_initialize(
  #     form_generalPurpose_v1
  #   );
  #
  # examplePrereg_1 <-
  #   preregr::prereg_specify(
  #     examplePrereg_1,
  #     discipline = "Thaumatology",
  #     title = "Cabbages cast in moderation: a comparative analysis",
  #     authors = "Littlebottom, C., Dibbler, C., & Aching, T."
  #   );
  #
  # examplePrereg_1_asYAML <-
  #   preregr::prereg_spec_to_yaml(
  #     examplePrereg_1
  #   );
  #
  # #tools::showNonASCII(paste0(examplePrereg_1_asYAML, collapse=""));
  #
  # preregr::prereg_spec_to_yaml(
  #   examplePrereg_1,
  #   file = yamlFile
  # );

  #tools::showNonASCIIfile(yamlFile);

  # usethis::use_data(examplePrereg_1, overwrite=TRUE);

}
