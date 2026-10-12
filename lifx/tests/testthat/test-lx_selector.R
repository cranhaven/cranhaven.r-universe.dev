context("lx_selector")

testthat::test_that(desc = "selector string correct",
                    {
                      expect_true(lx_selector(id = "A")=="id:A")
                      expect_true(lx_selector(id = "A",group = "B")=="id:A,group:B")
                      expect_true(lx_selector(id = "A",group = "B", zones = c(1,2))=="id:A|1|2,group:B|1|2")
                    })
