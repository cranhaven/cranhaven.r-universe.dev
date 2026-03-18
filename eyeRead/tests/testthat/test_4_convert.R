context( "convert" )

test_that( "specFile is found", {
  expect_true( T )
} )

test_that( "size is correctly converted to degrees, different distances and single values", {
  expect_equal( size2deg( x = 2, dist = 30 ), 3.818304866 )
  expect_equal( size2deg( x = 2, dist = 60 ), 1.909682508 )
  expect_equal( size2deg( x = 2, dist = 15 ), 7.628149669 )
  expect_equal( size2deg( x = 0.5, dist = 30 ), 0.954907555 )
  expect_equal( size2deg( x = 0.5, dist = 60 ), 0.477462066 )
  expect_equal( size2deg( x = 0.5, dist = 15 ), 1.909682508 )
  expect_equal( size2deg( x = 7, dist = 30 ), 13.30885009 )
  expect_equal( size2deg( x = 7, dist = 60 ), 6.676941008 )
  expect_equal( size2deg( x = 7, dist = 15 ), 26.26804461 )
  expect_equal( size2deg( x = 20, dist = 30 ), 36.86989765 )
  expect_equal( size2deg( x = 20, dist = 60 ), 18.92464442 )
  expect_equal( size2deg( x = 20, dist = 15 ), 67.38013505 )
} )

test_that( "size is correctly converted to degrees, different distances and multiple values", {
  expect_equal( size2deg( x = c( 2, 0.5, 7, 2, 20, 0.5 ),
                          dist = c( 30, 30, 60, 15, 30, 15 ) ),
                c( 3.818304866, 0.954907555, 6.676941008, 7.628149669, 36.86989765, 1.909682508 ) )
} )

test_that( "size is correctly converted to degrees; single distances and multiple values", {
  expect_equal( size2deg( x = c( 2, 0.5, 7, 10, 16.5, 100 ),
                          dist = 30 ),
                c( 3.818304866, 0.954907555, 13.30885009, 18.92464442, 30.7525025, 118.0724869 ) )
} )

test_that( "pixels is correctly converted to degrees; different distances, different resolutions, different screen widths and single values", {
  expect_equal( px2deg( x = 2, dist = 30, res = 1024, screenW = 32 ), 0.119366164 )
  expect_equal( px2deg( x = 2, dist = 60, res = 1024, screenW = 32 ), 0.059683098 )
  expect_equal( px2deg( x = 2, dist = 15, res = 1024, screenW = 32 ), 0.238732069 )
  expect_equal( px2deg( x = 2, dist = 30, res = 300, screenW = 32 ), 0.407434937 )
  expect_equal( px2deg( x = 2, dist = 30, res = 2050, screenW = 32 ), 0.059624871 )
  expect_equal( px2deg( x = 2, dist = 30, res = 65, screenW = 32 ), 1.880308091 )
  expect_equal( px2deg( x = 2, dist = 30, res = 1024, screenW = 15 ), 0.055952905 )
  expect_equal( px2deg( x = 2, dist = 30, res = 1024, screenW = 60 ), 0.223811354 )
  expect_equal( px2deg( x = 2, dist = 30, res = 1024, screenW = 88 ), 0.328256172 )
} )

test_that( "pixels is correctly converted to degrees; different distances, different resolutions, different screen widths and multiple values", {
  expect_equal( px2deg( x = c( 2, 8, 100 ), dist = 30, res = 1024, screenW = 32 ),
                c( 0.119366164, 0.477462066, 5.96292244 ) )
  expect_equal( px2deg( x = c( 2, 8, 100 ), dist = 60, res = 1024, screenW = 32 ),
                c( 0.059683098, 0.238732069, 2.983480871 ) )
  expect_equal( px2deg( x = c( 2, 8, 100 ), dist = 15, res = 1024, screenW = 32 ),
                c( 0.238732069, 0.954907555, 11.89372611 ) )
  expect_equal( px2deg( x = c( 2, 8, 100 ), dist = 30, res = 300, screenW = 32 ),
                c( 0.407434937, 1.629636747, 20.1611958 ) )
  expect_equal( px2deg( x = c( 2, 8, 100 ), dist = 30, res = 2050, screenW = 32 ),
                c( 0.059624871, 0.238499161, 2.980571471 ) )
  expect_equal( px2deg( x = c( 2, 8, 100 ), dist = 30, res = 1024, screenW = 15 ),
                c( 0.055952905, 0.223811354, 2.797089841 ) )
  expect_equal( px2deg( x = c( 2, 8, 100 ), dist = 30, res = 1024, screenW = 88 ),
                c( 0.328256172, 1.312970821, 16.3019807 ) )
} )

test_that( "degrees is correctly converted to size; different distances and single values", {
  expect_equal( deg2size( x = 2, dist = 30 ), 1.047303896 )
  expect_equal( deg2size( x = 2, dist = 60 ), 2.094607791 )
  expect_equal( deg2size( x = 2, dist = 15 ), 0.523651948 )
} )

test_that( "degrees is correctly converted to size; different distances and multiple values", {
  expect_equal( deg2size( x = c( 2, 8, 100 ), dist = 30 ),
                c( 1.047303896, 4.195608717, 71.50521556 ) )
  expect_equal( deg2size( x = c( 2, 8, 100 ), dist = 60 ),
                c( 2.094607791, 8.391217433, 143.0104311 ) )
  expect_equal( deg2size( x = c( 2, 8, 100 ), dist = 15 ),
                c( 0.523651948, 2.097804358, 35.75260778 ) )
} )
################################
test_that( "degrees is correctly converted to pixels; different degrees, different resolutions, different screen widths and single values", {
  expect_equal( deg2px( x = 0.119366164, dist = 30, res = 1024, screenW = 32 ), 2 )
  expect_equal( deg2px( x = 0.059683098, dist = 60, res = 1024, screenW = 32 ), 2 )
  expect_equal( deg2px( x = 0.238732069, dist = 15, res = 1024, screenW = 32 ), 2 )
  expect_equal( deg2px( x = 0.407434937, dist = 30, res = 300, screenW = 32 ), 2 )
  expect_equal( deg2px( x = 0.059624871, dist = 30, res = 2050, screenW = 32 ), 2 )
  expect_equal( deg2px( x = 1.8803080912, dist = 30, res = 65, screenW = 32 ), 2 )
  expect_equal( deg2px( x = 0.055952905, dist = 30, res = 1024, screenW = 15 ), 2 )
  expect_equal( deg2px( x = 0.223811354, dist = 30, res = 1024, screenW = 60 ), 2 )
  expect_equal( deg2px( x = 0.328256172, dist = 30, res = 1024, screenW = 88 ), 2 )
} )

test_that( "pixels is correctly converted to degrees; different distances, different resolutions, different screen widths and multiple values", {
  expect_equal( deg2px( x = c( 0.119366164, 0.477462066, 5.96292244 ), dist = 30, res = 1024, screenW = 32 ),
                c( 2, 8, 100 ) )
  expect_equal( deg2px( x = c( 0.059683098, 0.238732069, 2.983480871 ), dist = 60, res = 1024, screenW = 32 ),
                c( 2, 8, 100 ) )
  expect_equal( deg2px( x = c( 0.238732069, 0.954907555, 11.89372611 ), dist = 15, res = 1024, screenW = 32 ),
                c( 2, 8, 100 ) )
  expect_equal( deg2px( x = c( 0.407434937, 1.629636747, 20.1611958 ), dist = 30, res = 300, screenW = 32 ),
                c( 2, 8, 100 ) )
  expect_equal( deg2px( x = c( 0.059624871, 0.238499161, 2.980571471 ), dist = 30, res = 2050, screenW = 32 ),
                c( 2, 8, 100 ) )
  expect_equal( deg2px( x = c( 0.055952905, 0.223811354, 2.797089841 ), dist = 30, res = 1024, screenW = 15 ),
                c( 2, 8, 100 ) )
  expect_equal( deg2px( x = c( 0.328256172, 1.312970821, 16.3019807 ), dist = 30, res = 1024, screenW = 88 ),
                c( 2, 8, 100 ) )
} )