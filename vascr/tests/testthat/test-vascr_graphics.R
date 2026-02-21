library(vdiffr)

test_that("vascr plot line works", {
  testthat::skip_on_ci()
  expect_doppelganger("Plot line wells", 
                        growth.df %>% vascr_subset(unit = "R", frequency = 1000) %>% vascr_plot_line()
                      )
  expect_doppelganger("Plot line wells without text label", 
                      growth.df %>% vascr_subset(unit = "R", frequency = 1000) %>% vascr_plot_line(text_labels = FALSE)
  )
  
  
})


test_that("vascr plot line works for experiments", {
  testthat::skip_on_ci()
expect_doppelganger("Plot line experiments", 
                    growth.df %>% 
                      vascr_subset(unit = "R", frequency = 1000) %>% 
                      vascr_summarise(level = "experiments") %>%
                      vascr_plot_line()
                    
                    )
  
                     })



key_points = tribble(~title, ~start, ~end, ~color,
                     "test", 50, 100, "red")

test_that("vascr plot line works for experiments", {
  testthat::skip_on_ci()
  expect_doppelganger("highlight keyrange", 
                      growth.df %>% vascr_subset(unit = "R", frequency = 1000) %>% 
                        vascr_summarise(level = "summary") %>%
                        vascr_plot_line() %>%
                        vascr_plot_keyrange(key_points)
                      
  )
})
  
  
  test_that("vascr plot line works for experiments", {
    testthat::skip_on_ci()
    expect_doppelganger("Labeled keyrange", 
                        growth.df %>% vascr_subset(unit = "R", frequency = 1000) %>% 
                          vascr_summarise(level = "summary") %>%
                          vascr_plot_line() %>%
                          vascr_plot_keyrange_labeled(key_points)
                        
    )
    
  })
    
    test_that("vascr plot line works for experiments", {
      testthat::skip_on_ci()
      expect_doppelganger("labeled key points", 
                          growth.df %>% vascr_subset(unit = "R", frequency = 1000) %>% 
                            vascr_summarise(level = "summary") %>%
                            vascr_plot_line() %>%
                            vascr_plot_keyrange_labeled(key_points)
                          
      )
    })
      
      
      test_that("vascr plot line works for experiments", {
        testthat::skip_on_ci()
        expect_doppelganger("labeled key lines", 
                            growth.df %>% vascr_subset(unit = "R", frequency = 1000) %>% 
                              vascr_summarise(level = "summary") %>%
                              vascr_plot_line() %>%
                              vascr_plot_keylines(key_points))
        
        key_points_2 = tribble(~title, ~time,
                             "test", 50)
        
        expect_doppelganger("labeled key lines 2", 
                            growth.df %>% vascr_subset(unit = "R", frequency = 1000) %>% 
                              vascr_summarise(level = "summary") %>%
                              vascr_plot_line() %>%
                              vascr_plot_keylines(key_points_2))
  
})
      
      test_that("vascr_labeling_works", {
        testthat::skip_on_ci()
        expect_doppelganger("labeled key lines with arrow", 
                            growth.df %>% vascr_subset(unit = "R", frequency = 1000) %>% 
                              vascr_summarise(level = "summary") %>%
                              vascr_plot_line() %>%
                              vascr_plot_keyarrows(key_points)
                            
        )
        
      })
      
      
      
      
      test_that("vascr plot line works for experiments", {
        testthat::skip_on_ci()
        expect_doppelganger("Vascr plot line panel", 
                            growth.df %>% vascr_subset(unit = c("R", "C"), frequency = 1000) %>% 
                              vascr_summarise(level = "summary") %>%
                              vascr_plot_line_panel(key_points)
                            
        )
        
      })
      
      
      test_that("vascr_plot_grid works",{
        testthat::skip_on_ci()
        expect_doppelganger("Vascr plot grid", 
                            growth.df %>% vascr_subset(unit = "R", frequency = 4000) %>% 
                              vascr_plot_grid()
                            
        )
        
      })
      
      