# Tests for calculate_residuals() function

test_that("calculate_residuals computes residuals precisely", {
  data <- data.frame(
    id        = rep(1:4, each = 6),
    time      = rep(2000:2005, times = 4),
    cohort    = c(rep(2002,6), rep(2001,6), rep(2005,6), rep(2000,6)),
    instrument= c(0,0,1,1,1,1,  
                  0,1,1,1,1,1,  
                  0,0,0,0,0,1,  
                  0,0,0,1,1,1),
    control1 = c(100,120,140,160,180,200,   
                 150,160,170,180,190,200,   
                 80,90,100,200,250,300,     
                 200,190,180,170,160,150),  
    control2 = c(10,15,20,25,30,35,         
                 20,22,24,26,28,30,         
                 5,10,15,40,45,50,          
                 30,28,26,24,22,20)          
  )

  result <- calculate_residuals(data, c("control1", "control2"))

  fit_control1  <- lm(control1 ~ factor(id) + factor(time), data = data)
  fit_control2  <- lm(control2 ~ factor(id) + factor(time), data = data)

  expect_true(all(c("residual_control1", "residual_control2") %in% names(result)))

  expect_equal(result$residual_control1, unname(residuals(fit_control1)))
  expect_equal(result$residual_control2, unname(residuals(fit_control2)))
})