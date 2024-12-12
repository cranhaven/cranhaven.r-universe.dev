test_that("Output are not missing and standard errore is positif", {
  out <- erfeVecR(xmat=cbind(sim_panel_data$pred1, sim_panel_data$pred2),
                  yvec=sim_panel_data$resp, 
                  panSizeVec=unname(unlist(lapply(split(sim_panel_data$id,
                                                        sim_panel_data$id), 
                                                  function(x) length(x)))), 
                  asym=0.5, id=sim_panel_data$id)
  expect_equal(out$asymPoint, 0.5)
  expect_equal(sum(is.na(out$coefEst)), 0)
  expect_equal(sum(is.na(out$standardError)), 0)

  expect_true(all(out$standardError>0))
  
})
