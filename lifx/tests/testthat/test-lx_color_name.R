context("lx_color_name")

testthat::test_that(desc = "selector string correct",
                    {
                      expect_true(lx_color_name(hue = 255,brightness = 0,saturation = 0.5,check = FALSE)==
                                    " hue:255 saturation:0.5 brightness:0")
                      expect_true(lx_color_name(color_name = "#00FF00",brightness = 0, check = FALSE)==
                                    "#00FF00 brightness:0")
                      expect_error(lx_color_name(hue = 255,token = "NOT A GOOD TOKEN"),regexp = "bad token - could not check color")
                    })
