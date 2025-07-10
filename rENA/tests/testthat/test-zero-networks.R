codeNames = c("Data","Technical.Constraints","Performance.Parameters","Client.and.Consultant.Requests","Design.Reasoning","Collaboration")

accum = ena.accumulate.data(
  units = RS.data[,c("Condition","UserName")],
  conversation = RS.data[,c("Condition","GroupName")],
  metadata = RS.data[,c("CONFIDENCE.Change","CONFIDENCE.Pre","CONFIDENCE.Post","C.Change")],
  codes = RS.data[,codeNames],
  model = "EndPoint",
  window.size.back = 4
);
zero_units_rows <- c(1,3,5);
### Set 3 zero networks for units 1, 3, 5
zero_units <- accum$connection.counts[zero_units_rows,]$ENA_UNIT
accum$connection.counts[ENA_UNIT %in% zero_units, 8:22] = 0

test_that("testing zero networks", {
  ### create ENA set WITHOUT center alignment
  set_F = ena.make.set(enadata = accum, center.align.to.origin = FALSE);

  # sapply(as.matrix(set_F$points[ENA_UNIT %in% zero_units,]), all.equal)

  # Verify the dimensions are the same across all dimensions for the zeroed units
  testthat::expect_true(all(apply(as.matrix(set_F$points[ENA_UNIT %in% zero_units,]), 2, var) == 0))
  testthat::expect_true(all(as.matrix(set_F$model$centroids[unit %in% zero_units]) == 0))

  mean_centroid_F = colMeans(set_F$model$centroids[,2:ncol(set_F$model$centroids)])
  testthat::expect_true(all(mean_centroid_F != 0))

  set_T = ena.make.set(
    enadata = accum,
    center.align.to.origin = TRUE
  );

  ### We see that the points for the zero network units 1, 3, 5 are now at origin
  testthat::expect_equal(sum(as.matrix(set_T$points[zero_units_rows])), 0);

  ### And we see that the centroids of the zero network units 1, 3, 5 are also zero.
  testthat::expect_equal(sum(colMeans(as.matrix(set_T$model$centroids[zero_units_rows]))), 0);

  ### And the mean centroid is also zero
  mean_centroid_T = colMeans(as.matrix(set_T$model$centroids))
  testthat::expect_equal(sum(mean_centroid_T), 0)

  centroid_compare <- set_T$model$centroids == set_F$model$centroids
  testthat::expect_true(all(centroid_compare[zero_units_rows,]))
  testthat::expect_true(all(centroid_compare[,1]))
  testthat::expect_true(all(centroid_compare[-zero_units_rows,-1] == FALSE))
  testthat::expect_true(all((set_T$rotation$nodes == set_F$rotation$nodes)[,-1] == FALSE))
})
