test_that("response", {
  
  # library(testthat); library(webqueue)
  
  resp <- expect_silent(response(body = list(name = unbox('Peter'), pi = pi)))
  
  expect_identical(resp$status, 200L)
  expect_identical(resp$body, '{"name":"Peter","pi":[3.1416]}')
  expect_length(resp$headers, 1)
  expect_identical(resp$headers[['Content-Type']], 'application/json; charset=utf-8')
  
  
  hdr  <- expect_silent(header(name = 'Location', value = '/index.html'))
  cke  <- expect_silent(cookie(xyz = 123, max_age = Inf, http_only = TRUE))
  resp <- expect_silent(response(body = hdr, status = cke))
  
  expect_identical(resp$status, 200L)
  expect_identical(resp$body, 'OK')
  expect_length(resp$headers, 2)
  expect_identical(resp$headers[['Location']],   '/index.html')
  expect_identical(resp$headers[['Set-Cookie']], 'xyz=123; Max-Age=34560000; HttpOnly')
  
  
  cke  <- expect_silent(cookie(xyz = 123, secure = TRUE))
  uid  <- header('x-user-id'    = 100, expose = TRUE)
  sid  <- header('x-session-id' = 303, expose = TRUE)
  resp <- expect_silent(response(list(updates = js_obj(list())), 307L, Location = '/new/file.html', cke, uid, sid))
  
  expect_identical(resp$status, 307L)
  expect_identical(resp$body, '{"updates":{}}')
  expect_length(resp$headers, 6)
  expect_identical(resp$headers[['Set-Cookie']],                    'xyz=123; Secure')
  expect_identical(resp$headers[['x-user-id']],                     '100')
  expect_identical(resp$headers[['Location']],                      '/new/file.html')
  expect_identical(resp$headers[['x-session-id']],                  '303')
  expect_identical(resp$headers[['Content-Type']],                  'application/json; charset=utf-8')
  expect_identical(resp$headers[['Access-Control-Expose-Headers']], 'x-user-id, x-session-id')
  
  expect_error(response(body = matrix(1:10, nrow = 2)))
  
  resp <- expect_silent(response(404L))
  resp <- expect_silent(response(ls.str(list(x = 123))))
  
  json <- list(result = rep('A long string of text to break over multiple lines.', 50))
  resp <- expect_silent(response(body = json))
  expect_no_error(capture.output(print(resp)))
  
  expect_no_error(capture.output(print(hdr)))
  
})
