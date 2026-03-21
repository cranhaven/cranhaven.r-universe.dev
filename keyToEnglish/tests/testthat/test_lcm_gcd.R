test_that('lcm_check_1',
          expect_equal(
            LCM(2,3,4,5,6), 60
          )
)

test_that('lcm_check_2',
          expect_equal(
            LCM(2,256,256,2048), 2048
          )
)

test_that('gcd_check_1',
          expect_equal(
            GCD(2:6), 1
          )
)

test_that('gcd_check_2',
          expect_equal(
            GCD(256,256,2048), 256
          )
)
