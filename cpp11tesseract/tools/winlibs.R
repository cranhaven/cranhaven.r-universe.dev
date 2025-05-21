if (!file.exists("../windows/tesseract/include/tesseract/baseapi.h")) {
  unlink("../windows/tesseract", recursive = TRUE)
  url <- if (grepl("aarch", R.version$platform)) {
    "https://github.com/pachadotdev/cpp11bundles/releases/download/tesseract-5.3.2/tesseract-ocr-5.3.2-clang-aarch64.tar.xz"
  } else if (grepl("clang", Sys.getenv("R_COMPILED_BY"))) {
    "https://github.com/pachadotdev/cpp11bundles/releases/download/tesseract-5.3.2/tesseract-ocr-5.3.2-clang-x86_64.tar.xz"
  } else if(getRversion() >= "4.3") {
    "https://github.com/pachadotdev/cpp11bundles/releases/download/tesseract-5.3.2/tesseract-ocr-5.3.2-ucrt-x86_64.tar.xz"
  } else {
    "https://github.com/pachadotdev/cpp11bundles/releases/download/tesseract-oldR-5.3.2/tesseract-oldR-5.3.2.tar.gz"
  }
  download.file(url, basename(url), quiet = TRUE)
  untar(basename(url), exdir = "../windows", tar = "internal")
  file.rename(file.path("../windows", dir("../windows", pattern = "tesseract-")), "../windows/tesseract")
  unlink(basename(url))
  setwd("../windows")
}

# Also download the english training data
if (!file.exists("../windows/tessdata/eng.traineddata")) {
  message("Downloading eng.traineddata...")
  dir.create("../windows/tessdata", showWarnings = FALSE)
  download.file("https://github.com/tesseract-ocr/tessdata_fast/raw/4.1.0/eng.traineddata",
    "../windows/tessdata/eng.traineddata",
    mode = "wb", quiet = TRUE
  )
}

# This is base training data for Orientation and Script Detection
if (!file.exists("../windows/tessdata/osd.traineddata")) {
  message("Downloading osd.traineddata...")
  download.file("https://github.com/tesseract-ocr/tessdata_fast/raw/4.1.0/osd.traineddata",
    "../windows/tessdata/osd.traineddata",
    mode = "wb", quiet = TRUE
  )
}
