test_that("D-score replicated", {
  data("raw_data") # import data
  iat_cleandata <- clean_iat(raw_data, sbj_id = "Participant",
                             block_id = "blockcode",
                             mapA_practice = "practice.iat.Milkbad",
                             mapA_test = "test.iat.Milkbad",
                             mapB_practice = "practice.iat.Milkgood",
                             mapB_test = "test.iat.Milkgood",
                             latency_id = "latency",
                             accuracy_id = "correct",
                             trial_id = "trialcode",
                             trial_eliminate = c("reminder", "reminder1"))
  data <- iat_cleandata[[1]]
  # compute all algorithms from the data frame
  label_d <- paste0("d", 1:6)
  scores <- list()
  for(i in 1:length(label_d)) {
    scores[[i]] <- compute_iat(data, Dscore = label_d[i])
  }
  dscores <- data.frame(participant = scores[[1]]$participant)
  for (i in 1:length(scores)){
    name_col <- gsub("d", "dscore_d", label_d)
    dscores[, name_col[i]] <- scores[[i]][, name_col[[i]]]
  }
  # upload the iat dscore compute_iat on 07/09/2020
  data("iatdscores")
  long_old <- reshape(iatdscores, idvar = "participant", v.names = "old_scores",
                      times = colnames(iatdscores)[-1],
                      timevar = "labels",
                      varying = list(names(iatdscores)[-1]), direction = "long")
  new_long <- reshape(dscores, idvar = "participant", v.names = "new_scores",
                      times = colnames(dscores)[-1],
                      timevar = "labels",
                      varying = list(names(dscores)[-1]), direction = "long")
  comparison <- merge(long_old, new_long, by = c("participant", "labels"))
  # check that all the columns of the two dataset

    expect_true(all(comparison$old_scores == comparison$new_scores) == TRUE)
})
