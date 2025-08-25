test_that("PriorPost",{
  
  # Very small fit to run super fast
  data("Male_Gammarus_Single")
  modelData_MGS <- modelData(Male_Gammarus_Single, time_accumulation = 4)
  fit_MGS <- fitTK(modelData_MGS, iter = 10, chains = 2)
  data("Male_Gammarus_seanine_growth")
  modelData_MGSG <- modelData(Male_Gammarus_seanine_growth, time_accumulation = 1.417)
  fit_MGSG <- fitTK(modelData_MGSG, iter = 10, chains=2)
  data("Chiro_Creuzot")
  modelData_CC <- modelData(Chiro_Creuzot[Chiro_Creuzot$replicate == 1,], time_accumulation = 1.0)
  fit_CC <- fitTK(modelData_CC, iter = 10, chains = 2)
  
  test_that("df_PriorPost", {
    
    df_MGS <- df_PriorPost(fit_MGS)
    df_MGSG <- df_PriorPost(fit_MGSG)
    df_CC <- df_PriorPost(fit_CC)
    
    # Check class
    expect_true(all(class(df_MGS) == c("df_PP", "data.frame")))
    expect_true(all(class(df_MGSG) == c("df_PP", "data.frame")))
    expect_true(all(class(df_CC) == c("df_PP", "data.frame")))
    
    # Check column names 
    expect_true(all(colnames(df_MGS) == c("parameter", "type", "value")))
    expect_true(all(colnames(df_MGSG) == c("parameter", "type", "value")))
    expect_true(all(colnames(df_CC) == c("parameter", "type", "value")))
    
    # Check parameter of prior and posterior are equals
    df_post_MGS = df_MGS[df_MGS$type == "posterior",]
    df_prior_MGS = df_MGS[df_MGS$type == "prior",]
    expect_true(all(df_post_MGS$parameter == df_prior_MGS$parameter))
    
  })
  
  test_that("plot_PriorPost", {
    
    plt_MGS <- plot_PriorPost(fit_MGS)
    plt_MGSG <- plot_PriorPost(fit_MGSG)
    plt_CC <- plot_PriorPost(fit_CC)
    
    # Check class
    expect_true(all(class(plt_MGS) == c("gg", "ggplot")))
    expect_true(all(class(plt_MGSG) == c("gg", "ggplot")))
    
  })
  test_that("plot_PriorPost replace df", {

    df_MGS <- df_PriorPost(fit_MGS)
    plt_MGS_df <- plot_PriorPost(df_MGS)
    
    dfr_MGS <- df_MGS
    dfr_MGS$parameter <- replace_(df_MGS$parameter,"ku","kuw")
    plt_MGS_dfr <- plot_PriorPost(dfr_MGS)

    df_CC <- df_PriorPost(fit_CC)
    plt_CC_df <- plot_PriorPost(df_CC)
    
    dfr_CC <- df_CC
    dfr_CC$parameter <- replace_(df_CC$parameter, c("ku1","ku2","ku3"), c("kuw","kus","kupw"))
    plt_CC_dfr <- plot_PriorPost(dfr_CC)
    
    # Check class
    expect_true(all(class(plt_CC_df) == c("gg", "ggplot")))
    expect_true(all(class(plt_CC_dfr) == c("gg", "ggplot")))
    
  })
  
})


