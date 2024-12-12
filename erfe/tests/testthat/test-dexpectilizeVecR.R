test_that("de-expectilize a vector works", {
  out <- dexpectilizeVecR(yvec=sim_panel_data$pred1,
                          aweight = 0 * sim_panel_data$resp
                          + 0.5, 
                          panSizeVec=unname(unlist(
                            lapply(split(
                              sim_panel_data$id,
                              sim_panel_data$id),
                              function(x) 
                                length(x)))))
  expect_vector(out,
                size=length(sim_panel_data$pred1
                            ))
  expect_equal(sum(is.na(out)), 0)
                
})
