mpactr <- R6Class("mpactr",
  public = list(
    # Properties

    # Constructor
    initialize = function(peak_table, meta_data) {
      stopifnot(any(class(peak_table$raw_table) == "data.table"))
      stopifnot(any(class(peak_table$peak_table) == "data.table"))
      stopifnot(any(class(meta_data) == "data.table"))
      private$raw_peak_table <- data.table(peak_table$raw_table)
      private$peak_table <- data.table(peak_table$peak_table)
      private$meta_data <- data.table(meta_data)
    },
    isMultipleTechReps = function() {
      any(private$meta_data[, .N, by = Sample_Code][["N"]] > 1)
    }
  ),
  private = list(
    peak_table = NA,
    meta_data = NA,
    raw_peak_table = NA
  )
)
