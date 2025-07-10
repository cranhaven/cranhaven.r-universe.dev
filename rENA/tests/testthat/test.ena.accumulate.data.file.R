suppressMessages(library(rENA, quietly = T, verbose = F))
context("Test accumulating data file");

test_that("NULL sent to accumulate", {
  df.accum <- expect_error(rENA:::ena.accumulate.data.file(
    NULL, units.by = c("Name"), conversations.by = c("Day"),
    codes = c("c1", "c2", "c3")))
})

# test_that("Simple data.frame to accumulate", {
#   fake.codes.len = 10;
#   fake.codes <- function(x) sample(0:1,fake.codes.len, replace=T)
#
#   codes = paste("Codes",LETTERS[1:fake.codes.len],sep="-");
#
#   df = data.frame(
#     Name=c("J","Z"),
#     Day=c(1,1,1,1,1,1,2,2,2,2,2,2),
#     c1=c(1,1,1,1,1,0,0,1,1,0,0,1),
#     c2=c(1,1,1,0,0,1,0,1,0,1,0,0),
#     c3=c(0,0,1,0,1,0,1,0,0,0,1,0),
#     c4=c(1,1,1,0,0,1,0,1,0,1,0,0)
#   );
#
#   df.accum = ena.accumulate.data.file(df, units.by = c("Name"), conversations.by = c("Day"), codes = c("c1","c2","c3"));
#   df.accum.weighted = ena.accumulate.data.file(df, units.by = c("Name"), conversations.by = c("Day"), codes = c("c1","c2","c3"), weight.by = "weighted");
# });
test_that("Accumulate using conversation model", {
  fake.codes.len = 10;
  fake.codes <- function(x) sample(0:1,fake.codes.len, replace=T)

  codes = paste("Codes",LETTERS[1:fake.codes.len],sep="-");

  df = data.frame(
    Name=c("J","Z"),
    Day=c(1,1,1,1,1,1,2,2,2,2,2,2),
    c1=c(1,1,1,1,1,0,0,1,1,0,0,1),
    c2=c(1,1,1,0,0,1,0,1,0,1,0,0),
    c3=c(0,0,1,0,1,0,1,0,0,0,1,0),
    c4=c(1,1,1,0,0,1,0,1,0,1,0,0)
  );

  df.accum = ena.accumulate.data.file(
    df,
    units.by = c("Name"),
    conversations.by = c("Day"),
    #weight.by = function(x) { return(x) },
    codes = c("c1","c2","c3"),
    window = "Conversation"
  );

  # Check co-occurrences for unit `J` in conversation `1`
  expected.sums = colSums(df[df$Name=="J"&df$Day==1, df.accum$rotation$codes]);
  expected.sums[expected.sums > 1] = 1
  expected = tcrossprod(expected.sums);
  expected.co = expected[upper.tri(expected)]
  actual.co = as.numeric(as.matrix(df.accum$model$row.connection.counts[Day == 1 & Name == 'J']))
  testthat::expect_equal(
    label = "Verify the co-occurences for unit J in conversation 1",
    object=actual.co,
    expected=expected.co
  )
})
test_that("Accumulate weighted data.", {
  testdata = runif(24, 0, 1)
  testmat = matrix(testdata, 4, dimnames=list(NULL,LETTERS[1:6]))
  testmeta = data.frame(tr=1:4, unit=rep(1, 4))
  testdf = cbind(testmeta, testmat)

  x.normal = rENA:::ena.accumulate.data.file(testdf,
                               units.by='unit',
                               conversations.by='tr',
                               codes=LETTERS[1:6],
                               window.size.back=4,
                               weight.by = "binary")

  x.prod = rENA:::ena.accumulate.data.file(testdf,
                               units.by='unit',
                               conversations.by='tr',
                               codes=LETTERS[1:6],
                               window.size.back=4,
                               weight.by = "product")
  x.sqrt = rENA:::ena.accumulate.data.file(testdf,
                               units.by='unit',
                               conversations.by='tr',
                               codes=LETTERS[1:6],
                               window.size.back=4,
                               weight.by = sqrt)
  x.log = rENA:::ena.accumulate.data.file(testdf,
                               units.by='unit',
                               conversations.by='tr',
                               codes=LETTERS[1:6],
                               window.size.back=4,
                               weight.by = function(x) { log(x + 1) })

  testthat::expect_false(identical(
    as.matrix(x.normal$connection.counts),
    as.matrix(x.prod$connection.counts)
  ))
  testthat::expect_false(identical(
    as.matrix(x.normal$connection.counts),
    as.matrix(x.sqrt$connection.counts)
  ))
  testthat::expect_false(identical(
    as.matrix(x.normal$connection.counts),
    as.matrix(x.log$connection.counts)
  ))
  testthat::expect_false(identical(
    as.matrix(x.prod$connection.counts),
    as.matrix(x.sqrt$connection.counts)
  ))
  testthat::expect_false(identical(
    as.matrix(x.prod$connection.counts),
    as.matrix(x.log$connection.counts)
  ))
  testthat::expect_false(identical(
    as.matrix(x.sqrt$connection.counts),
    as.matrix(x.log$connection.counts)
  ))
})
test_that("Corrected adjacency.vectors equals manually corrected raw data (correction = log)", {
  testdata = runif(24, 0, 1)
  testmat = matrix(testdata, 4, dimnames=list(NULL,LETTERS[1:6]))
  testmeta = data.frame(tr=1:4, unit=rep(1, 4))
  testdf = cbind(testmeta, testmat)

  x = rENA:::ena.accumulate.data.file(testdf,
                               units.by='unit',
                               conversations.by='tr',
                               #units='1',
                               codes=LETTERS[1:6],
                               window.size.back=4,
                               weight.by = log)

  x_binary = rENA:::ena.accumulate.data.file(testdf,
                               units.by='unit',
                               conversations.by='tr',
                               #units='1',
                               codes=LETTERS[1:6],
                               window.size.back=4)

  testthat::expect_null(x$model$unweighted.connection.counts)
  testthat::expect_false(all(x$connection.counts == x_binary$connection.counts))
})
test_that("Simple forwarded metadata", {
  fake.codes.len = 10;
  fake.codes <- function(x) sample(0:1,fake.codes.len, replace=T)

  codes = paste("Codes",LETTERS[1:fake.codes.len],sep="-");

  df = data.frame(
    Name=c("J","Z"),
    Day=c(1,1,1,1,1,1,2,2,2,2,2,2),
    c1=c(1,1,1,1,1,0,0,1,1,0,0,1),
    c2=c(1,1,1,0,0,1,0,1,0,1,0,0),
    c3=c(0,0,1,0,1,0,1,0,0,0,1,0),
    m1=c(1,2),
    m2=c(1,2,3,4)
  );

  df.accum = rENA:::ena.accumulate.data.file(df, units.by = c("Name"), conversations.by = c("Day"), codes = c("c1","c2","c3"), window.size.back = 4, weight.by = 'product');

  testthat::expect_true("m1" %in% colnames(df.accum$meta.data));

  row_3_win_mat <- tcrossprod(apply((df[1:3, 3:5]), 2, sum)) - tcrossprod(apply((df[1:2, 3:5]), 2, sum))
  all.equal(row_3_win_mat[upper.tri(row_3_win_mat)], as.numeric(as.matrix(df.accum$model$row.connection.counts)[3,]))
2});
test_that("Test trajectories", {
  fake.codes.len = 10;
  fake.codes <- function(x) sample(0:1,fake.codes.len, replace=T)

  codes = paste("Codes",LETTERS[1:fake.codes.len],sep="-");

  df = data.frame(
    Name=c("J","Z"),
    Day=c(1,1,1,1,1,1,2,2,2,2,2,2),
    ActivityNumber=c(1,1,1,1,2,2,2,2,3,3,3,3),
    c1=c(1,1,1,1,1,0,0,1,1,0,0,1),
    c2=c(1,1,1,0,0,1,0,0,0,0,0,1),
    c3=c(0,0,1,0,1,0,1,0,0,0,1,0)
  );

  df.accum = rENA:::ena.accumulate.data.file(
    df, units.by = c("Name"), conversations.by = c("Day", "ActivityNumber"), codes = c("c1","c2","c3"),
    model = "AccumulatedTrajectory"
  );
  df.non.accum = rENA:::ena.accumulate.data.file(
    df, units.by = c("Name"), conversations.by = c("Day", "ActivityNumber"), codes = c("c1","c2","c3"),
    model = "SeparateTrajectory"
  );

  # Test for expected accumulated value
  testthat::expect_equal(
    as.numeric(df.accum$connection.counts[df.accum$trajectories$ENA_UNIT == "J" & df.accum$trajectories$ActivityNumber == 1, "c1 & c2"]),
    df.accum$model$row.connection.counts[Name == "J" & ActivityNumber == 1, sum(.SD), .SDcols = c("c1 & c2")]
  );

  # Test for a value of 1 in the first accumulation of the trajectory of code 1
  testthat::expect_true(sum(df.accum$model$row.connection.counts[Name == "Z" & ActivityNumber == 1,  c("c1 & c2"), ]) == 1);

  # Test for a value of 0 in the second accumulation of the trajectory of code 1
  testthat::expect_true(all(df.accum$model$row.connection.counts[Name == "Z" & ActivityNumber == 2, c("c1 & c2")] == 0));

  # Test that the first summed trajectory is 1
  testthat::expect_equal(
    as.numeric(df.accum$connection.counts[df.accum$trajectories$ENA_UNIT == "Z" & df.accum$trajectories$ActivityNumber == 1, c("c1 & c2")])
    ,1
  );

  # Test that the second summed trajectory is 1, even thought it had a zero accumulation for it's conversations
  testthat::expect_true(all(
    as.matrix(df.accum$connection.counts[df.accum$trajectories$ENA_UNIT == "J" & df.accum$trajectories$ActivityNumber==3,]) == c(2,2,1)
  ));


# Test that non-accumulation is properly leaving second trajectory group 0 (different than the previous test)
  testthat::expect_true(all(
    as.matrix(df.non.accum$connection.counts[df.non.accum$trajectories$Name == "J" & df.non.accum$trajectories$ActivityNumber==3,])
      == c(0,0,0)
  ));
})
# test_that("Test accumulation with data.frame and matrix", {
#   # #df.file <- system.file("extdata", "rs.data.csv", package="rENA")
#   #
#   # codeNames = c('Data','Technical.Constraints','Performance.Parameters','Client.and.Consultant.Requests','Design.Reasoning','Collaboration');
#   # df.csv = RS.data; # read.csv(df.file)
#   #
#   # df.accum = ena.accumulate.data.file(df.csv, units.by = c("UserName","Condition"), conversations.by = c("ActivityNumber","GroupName"), codes = codeNames);
#   # df.accum2 = ena.accumulate.data.file(df.file, units.by = c("UserName","Condition"), conversations.by = c("ActivityNumber","GroupName"), codes = codeNames);
#   #
#   # testthat::expect_is(df.csv, "data.frame")
#   # testthat::expect_is(df.accum, "ENAdata")
#   # testthat::expect_is(df.accum2, "ENAdata")
#   #
#   # ## Test with file reported in #5
#   # pn.file <- system.file("extdata", "sample-data", "PinterestMock2.csv", package="rENA")
#   # pn.csv = read.csv(pn.file)
#   # pn.accum = ena.accumulate.data.file(pn.csv, units.by =  c("Teacher"), conversations.by = c("Board"), codes = c("Kinesthetic", "Algorithmic"))
#   #
#   # testthat::expect_is(pn.accum, "ENAdata")
# })
# test_that("Test accumulation with dplyr::tbl_df", {
#   # pn.file = system.file("extdata", "sample-data", "PinterestMock2.csv", package="rENA")
#   # PinterestMock2 <- readr::read_csv(pn.file)
#   # pn.accum = ena.accumulate.data.file(PinterestMock2, units.by =  c("Teacher"), conversations.by = c("Board"), codes = c("Kinesthetic", "Algorithmic"))
#   #
#   # testthat::expect_is(pn.accum, "ENAdata")
# })

# test_that("Test accumulation output JSON", {
#   pn.file = system.file("extdata", "sample-data", "PinterestMock2.csv", package="rENA")
#   pn.accum = ena.accumulate.data.file(pn.file, units.by =  c("Teacher"), conversations.by = c("Board"), codes = c("Kinesthetic", "Algorithmic"), output = "json")
#
#   pn.accum.less = ena.accumulate.data.file(pn.file, units.by =  c("Teacher"), conversations.by = c("Board"), codes = c("Kinesthetic", "Algorithmic"), output = "json", output.fields = c("metadata"))
#
#   testthat::expect_is(pn.accum, "list")
#   testthat::expect_is(pn.accum$accumulated.adjacency.vectors, "data.frame")
#   testthat::expect_is(pn.accum.less, "list")
#   testthat::expect_null(pn.accum.less$accumulated.adjacency.vectors)
# })
