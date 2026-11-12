test_that("A valid boundary and valid areanames returns a list", {
  expect_type(get_boundaries_areaname(
    boundary = "Local_Authority_Districts_December_2022_UK_BGC_V2", 
    col_name_var = "LAD22NM", 
    chosen_constituency_list = c("Derbyshire Dales", "Harrogate", "West Northamptonshire")), "list")
})

test_that("A valid boundary and valid areanames returns a list when there are > 30 constituencies", {
  expect_type(get_boundaries_areaname(
    boundary = "Local_Authority_Districts_December_2022_UK_BGC_V2",
    col_name_var = "LAD22NM", 
    chosen_constituency_list = c(
      "Derbyshire Dales", "Harrogate", "West Northamptonshire", "Aldershot", "Aldridge-Brownhills", 
      "Altrincham and Sale West", "Amber Valley", "Arundel and South Downs", "Ashfield", "Ashford",
      "Ashton-under-Lyne", "Aylesbury", "Banbury", "Barking", "Barnsley North", 
      "Barnsley South", "Barrow and Furness", "Basildon and Billericay", "Basingstoke", "Bassetlaw", 
      "Bath", "Battersea", "Beaconsfield", "Beckenham and Penge", "Bedford",
      "Bermondsey and Old Southwark", "Bethnal Green and Stepney", "Beverley and Holderness", "Bexhill and Battle", "Bexleyheath and Crayford", 
      "Bicester and Woodstock", "Birkenhead", "Birmingham Edgbaston", "Birmingham Erdington", "Birmingham Hall Green and Moseley")),
  "list")
})

