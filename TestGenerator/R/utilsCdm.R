emptyCDM <- function(conn, cdm) {

  for (table_name in names(cdm)) {
    if (table_name %in% c("person",
                          "observation_period",
                          "visit_occurrence",
                          "visit_detail",
                          "condition_occurrence",
                          "drug_exposure",
                          "procedure_occurrence",
                          "device_exposure",
                          "measurement",
                          "observation",
                          "death",
                          "note",
                          "note_nlp",
                          "specimen",
                          "fact_relationship",
                          "location",
                          "care_site",
                          "provider",
                          "payer_plan_period",
                          "cost",
                          "drug_era",
                          "dose_era",
                          "condition_era",
                          "metadata",
                          "cdm_source")) {
      DBI::dbExecute(conn, glue::glue("TRUNCATE TABLE {table_name}"))
    }
  }
  return(cdm)
}
