test_that("tempInstance works", {
  t = tempInstance()
  expect_true(dir.exists(t))
  expect_true(length(list.files(t, pattern = "html")) == 1)
  expect_true(length(list.files(t, pattern = "original")) == 1)
  expect_true(length(list.files(t, pattern = "index")) == 2)
  # TODO check version against
  # version = jsonlite::parse_json(
  #   readLines(file.path(build.dir, "package.json")))$dependencies['@tgve/tgvejs'][[1]]
  # version = sub(".", "", version)
  # names(version) <- "version"
  # How do we get the build version? It is not there by default
  # clues for future https://stackoverflow.com/a/58421941/11101153
})

test_that("is_valid_url works", {
  urls = is_valid_url(c("//localhost",
                        "https://bbc.com:80",
                        "https://foo.com/blah_blah/",
                        "127.0.0.1"))
  expect_true(all(urls))
  # from https://cran.r-project.org/web/packages/rex/vignettes/url_parsing.html
  good = c("http://foo.com/blah_blah",
           "http://foo.com/blah_blah/",
           "http://foo.com/blah_blah_(wikipedia)",
           "http://foo.com/blah_blah_(wikipedia)_(again)",
           "http://www.example.com/wpstyle/?p=364",
           "https://www.example.com/foo/?bar=baz&inga=42&quux",
           "http://✪df.ws/123",
           "http://userid:password@example.com:8080",
           "http://userid:password@example.com:8080/",
           "http://userid@example.com",
           "http://userid@example.com/",
           "http://userid@example.com:8080",
           "http://userid@example.com:8080/",
           "http://userid:password@example.com",
           "http://userid:password@example.com/",
           "http://foo.com/blah_(wikipedia)#cite-1",
           "http://foo.com/blah_(wikipedia)_blah#cite-1",
           "http://foo.com/unicode_(✪)_in_parens",
           "http://foo.com/(something)?after=parens",
           "http://☺.damowmow.com/",
           "http://code.google.com/events/#&product=browser",
           "http://j.mp",
           "ftp://foo.bar/baz",
           "http://foo.bar/?q=Test%20URL-encoded%20stuff",
           "http://مثال.إختبار",
           "http://例子.测试",
           "http://-.~_!$&'()*+,;=:%40:80%2f::::::@example.com",
           "http://1337.net",
           "http://a.b-c.de")
  expect_true(all(is_valid_url(good)))
  bad =  c(
    "http://",
    "http://.",
    "http://..",
    "http://../",
    "http://?",
    "http://??",
    "http://??/",
    "http://#",
    "http://##",
    "http://##/",
    "http://foo.bar?q=Spaces should be encoded",
    "//",
    "//a",
    "///a",
    "///",
    "http:///a",
    "foo.com",
    "rdar://1234",
    "h://test",
    "http:// shouldfail.com",
    ":// should fail",
    "http://foo.bar/foo(bar)baz quux",
    "ftps://foo.bar/",
    "http://-error-.invalid/",
    "http://-a.b.co",
    "http://a.b-.co",
    "http://0.0.0.0",
    "http://3628126748",
    "http://.www.foo.bar/",
    "http://www.foo.bar./",
    "http://.www.foo.bar./")
  expect_true(all(is_valid_url(bad) == FALSE))
})
