## This is only for rxode2
.in <- suppressWarnings(readLines("src/Makevars.in"))

if (.Platform$OS.type == "windows" && !file.exists("src/Makevars.win")) {
  .in <- gsub("@CXX14STD@", "-std=c++1y", .in)
  file.out <- file("src/Makevars.win", "wb")
  writeLines(gsub("@ISYSTEM@", "I", .in),
             file.out)
  close(file.out)
} else {
  .in <- gsub("@CXX14STD@", "-std=gnu++14", .in)
  file.out <- file("src/Makevars", "wb")
  writeLines(gsub("@ISYSTEM@", "isystem", .in),
             file.out)
  close(file.out)
}

if (file.exists("man/reexports.Rd")) {
  l <- readLines("man/reexports.Rd")
  if (!any(regexpr("[\\]value", l) != -1)) {
    l <- c(l, "\\value{ Inherited from parent routine }")
    file.out <- file("man/reexports.Rd", "wb")
    writeLines(l, file.out)
    close(file.out)
  }
}


unlink("R/rxode2_md5.R")

cpp <- list.files("src", pattern = ".(c|h|cpp|f)$")
#Rfiles <- list.files("R/", pattern = ".R")

cmd <- file.path(R.home("bin"), "R")
args <- c("CMD", "config")

md5 <- digest::digest(c(lapply(c(paste0("src/", cpp)#,
                                 #paste0("inst/include/", include)#,
                                 #paste0("R/", Rfiles)
                                 ), digest::digest, file = TRUE),
                        ## vapply(c("BLAS_LIBS", "CC",  "CFLAGS", "CPICFLAGS",
                        ##          "CXX", "CXXFLAGS", "CXXPICFLAGS",
                        ##          "CXX11", "CXX11STD", "CXX11FLAGS", "CXX11PICFLAGS",
                        ##          "CXX14", "CXX14STD", "CXX14FLAGS", "CXX14PICFLAGS",
                        ##          "CXX17", "CXX17STD", "CXX17FLAGS", "CXX17PICFLAGS",
                        ##          "CXX20", "CXX20STD", "CXX20FLAGS", "CXX20PICFLAGS",
                        ##          "FC", "FFLAGS", "FCFLAGS",  "FPICFLAGS"),
                        ##        function(cfg) {
                        ##          rawToChar(sys::exec_internal(cmd, c(args, cfg))$stdout)
                        ##        }, character(1)
                        ##       ),
                        ""
                        ))
unlink("R/rxode2et_md5.R")
md5file <- file("R/rxode2et_md5.R", "wb")
writeLines(sprintf("rxode2et.md5 <- \"%s\"\n", md5), md5file)
close(md5file)

l <- readLines(file.path(system.file(package="rxode2random"), "include", "rxode2random_as.h"))
l <- gsub("qtest", "_rxode2et_qtest", l)

rxode2et_as.h <- file("src/rxode2et_as.h", "wb")
writeLines(l, rxode2et_as.h)
close(rxode2et_as.h)
