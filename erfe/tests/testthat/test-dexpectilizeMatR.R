test_that("de-expectilize a matrix works", {
  out <- dexpectilizeMatR(ymat=cbind(sim_panel_data$pred1,
                                     sim_panel_data$pred2),
                          aweight=0 * sim_panel_data$resp + 0.5, 
                               panSizeVec=unname(unlist(lapply(
                                 split(sim_panel_data$id, 
                                       sim_panel_data$id),
                                 function(x) length(x)))))
  expect_type(out, "double")
  expect_equal(nrow(out), nrow(cbind(sim_panel_data$pred1,
                                     sim_panel_data$pred2)))
  expect_equal(ncol(out), ncol(cbind(sim_panel_data$pred1,
                                     sim_panel_data$pred2)))
})
