
mnis_id_type_util <- function(ref_dods) {
  if (ref_dods == TRUE) {
    id_type <- "refDods="
  } else {
    id_type <- "id="
  }

  id_type
}
