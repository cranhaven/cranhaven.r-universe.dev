
test_that("HDF5 smoke test", {
  
  c_file  <- test_path("../src/smoke_test.c")
  o_file  <- test_path("../src/smoke_test.o")
  so_file <- paste0(test_path("../src/smoke_test"), .Platform$dynlib.ext)
  
  # Set environment variables for the R CMD SHLIB call
  Sys.setenv(PKG_CPPFLAGS = c_flags(), PKG_LIBS = ld_flags())
  on.exit(Sys.unsetenv(c("PKG_CPPFLAGS", "PKG_LIBS")), add = TRUE)

  R_EXE <- file.path(R.home("bin"), "R")
  R_EXE <- normalizePath(R_EXE, mustWork = FALSE)

  compile_cmd <- sprintf(
    '%s CMD SHLIB %s',
    shQuote(R_EXE),
    shQuote(c_file) )

  message(paste0(
    "\nSetting environment variables:\n", 
    "  PKG_CPPFLAGS = ", Sys.getenv('PKG_CPPFLAGS'), "\n", 
    "  PKG_LIBS     = ", Sys.getenv('PKG_LIBS'),     "\n",
    "Compiling with command:\n",
    compile_cmd, "\n"))
  
  tryCatch(
    expr  = {
      system(compile_cmd)
      succeed("Compiled successfully.")
    }, 
    error = function (e) {
      fail(paste("Could not compile:", e$message))
    })
  
  if (file.exists(so_file)) {
    succeed("Shared object created successfully.")
    on.exit(file.remove(o_file, so_file), add = TRUE)
  } else {
    fail(paste0("Shared object file not created:", so_file))
  }
  
  expect_silent(dyn.load(so_file))
  
  tmp_file <- tempfile(fileext = ".h5")
  tmp_file <- normalizePath(tmp_file, winslash = "/", mustWork = FALSE)
  
  # Call C the function
  version_str <- tryCatch({
    version_str <- .Call("C_smoke_test", tmp_file)
    succeed(".Call('C_smoke_test') ran successfully")
    version_str
  }, error = function(e) {
    fail(paste("Error during .Call('C_smoke_test'):", e$message))
  })
  
  if (file.exists(tmp_file)) {
    succeed("Output H5 file created successfully.")
    on.exit(file.remove(tmp_file), add = TRUE)
  } else {
    fail(paste0("H5 output file not created:\n", tmp_file))
  }
  
  expect_type(version_str, "character")
  expect_length(version_str, 1)
  expect_match(version_str, "^[0-9]+\\.[0-9]+\\.[0-9]+$")
})
