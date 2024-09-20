cpp <- list.files("src", pattern = ".(c|h|cpp|f)$")
include <- list.files("inst/include")

md5 <- digest::digest(c(lapply(c(paste0("src/", cpp),
                                 paste0("inst/include/", include)#,
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
unlink("R/rxode2random_md5.R")
md5file <- file("R/rxode2random_md5.R", "wb")
writeLines(sprintf("rxode2random.md5 <- \"%s\"\n", md5), md5file)
close(md5file)

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

