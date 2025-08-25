test_that("elimination_rate", {
  
  skip_on_cran()
  
  # # small test to run fast
  # data("Male_Gammarus_Single")
  # modelData_MGS_ke <- modelData(Male_Gammarus_Single, time_accumulation = 4)
  # fit_MGS_ke <- fitTK(modelData_MGS_ke, iter = 1000, chains = 2)
  # plot(fit_MGS_ke)
  # plot_PriorPost(fit_MGS_ke)
  # 
  # 
  # modelData_MGS_ke0 <- modelData(Male_Gammarus_Single, time_accumulation = 4, elimination_rate = 1e-9)
  # fit_MGS_ke0 <- fitTK(modelData_MGS_ke0, iter = 1000, chains = 2)
  # plot(fit_MGS_ke0)
  # ppc(fit_MGS_ke0)
  # plot_PriorPost(fit_MGS_ke0)
  # 
  # data("Male_Gammarus_seanine_growth")
  # modelData_MGSG_ke <- modelData(Male_Gammarus_seanine_growth, time_accumulation = 1.417)
  # fit_MGSG_ke <- fitTK(modelData_MGSG_ke, iter = 1000, chains=2)
  # plot(fit_MGSG_ke)
  # modelData_MGSG_ke0 <- modelData(Male_Gammarus_seanine_growth, time_accumulation = 1.417, elimination_rate = 1e-9)
  # fit_MGSG_ke0 <- fitTK(modelData_MGSG_ke0, iter = 1000, chains=2)
  # plot(fit_MGSG_ke0)
  # ppc(fit_MGSG_ke0)
  # 
  # data("Chiro_Creuzot")
  # Chiro_Creuzot <- Chiro_Creuzot[Chiro_Creuzot$replicate == 1,]
  # modelData_CC_ke <- modelData(Chiro_Creuzot, time_accumulation = 1.0, elimination_rate = 1e-9)
  # fit_CC_ke <- fitTK(modelData_CC_ke, iter = 1000, chains=2)
  # plot(fit_CC_ke)
  # modelData_CC_ke0 <- modelData(Chiro_Creuzot, time_accumulation = 1.0, elimination_rate = 1e-9)
  # fit_CC_ke0 <- fitTK(modelData_CC_ke0, iter = 1000, chains=2)
  # plot(fit_CC_ke0)
  # ppc(fit_CC_ke0)
  
})
