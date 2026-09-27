# update_design() warns when weight column changes on calibrated design [B-6]

    Code
      suppressMessages(update_design(d, weights = wt2))
    Condition
      Warning:
      ! Weight column changed on a calibrated design.
      i @calibration was built from the previous weight column.
      v Re-run calibration or set `design@calibration <- NULL` before analysing.

