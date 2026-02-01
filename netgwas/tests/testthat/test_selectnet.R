context("selectnet")

test_that("Model selection works fine ", 
          {
            D <- simgeno(p=10, n=30, k= 3, adjacent = 3, alpha = 0.06 , beta = 0.06)$data
            out  <-  netsnp(D, n.rho= 4, method="npn", ncores= 1)
            sel.ebic <- selectnet(out, criteria = "ebic")$opt.index 
            
            expect_equal(  sel.ebic, 4 )  
          })
