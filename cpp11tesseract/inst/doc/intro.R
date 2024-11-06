## ----echo = FALSE, message = FALSE--------------------------------------------
library(tibble)
# knitr::opts_chunk$set(comment = "")
has_chi_sim <- "chi_sim" %in% cpp11tesseract::tesseract_info()$available
if (identical(Sys.info()[["user"]], "pacha")) stopifnot(has_chi_sim)
if (grepl("tesseract.Rcheck", getwd())) {
  Sys.sleep(10) # workaround for CPU time check
}

## -----------------------------------------------------------------------------
library(cpp11tesseract)
file <- system.file("examples", "testocr.png", package = "cpp11tesseract")
eng <- tesseract("eng")
text <- ocr(file, engine = eng)
cat(text)

## -----------------------------------------------------------------------------
results <- ocr_data(file, engine = eng)
results

## -----------------------------------------------------------------------------
tesseract_info()

## ----eval=FALSE---------------------------------------------------------------
#  # Only need to do download once:
#  tesseract_download("chi_sim")

## ----eval = has_chi_sim-------------------------------------------------------
# Now load the dictionary
(simplified_chinese <- tesseract("chi_sim"))
file <- system.file("examples", "chinese.jpg", package = "cpp11tesseract")
text <- ocr(file, engine = simplified_chinese)
cat(text)

## -----------------------------------------------------------------------------
text2 <- readLines(
  system.file("examples", "chinese.txt", package = "cpp11tesseract")
)

cat(text2)

## -----------------------------------------------------------------------------
library(magick)
file <- system.file("examples", "wilde.jpg", package = "cpp11tesseract")
input <- image_read(file)

text <- input %>%
  image_resize("2000x") %>%
  image_convert(type = "Grayscale") %>%
  image_trim(fuzz = 40) %>%
  image_write(format = "png", density = "300x300") %>%
  ocr()

cat(text)

## ----eval=require(pdftools)---------------------------------------------------
file <- system.file("examples", "ocrscan.pdf", package = "cpp11tesseract")
pngfile <- pdftools::pdf_convert(file, dpi = 600)
text <- ocr(pngfile)
cat(text)

## -----------------------------------------------------------------------------
# List all parameters with *colour* in name or description
tesseract_params("colour")

## -----------------------------------------------------------------------------
tesseract_info()["version"]

## -----------------------------------------------------------------------------
file <- system.file("examples", "receipt.jpg", package = "cpp11tesseract")
numbers <- tesseract(options = list(tessedit_char_whitelist = "-$.0123456789"))
cat(ocr(file, engine = numbers))

## -----------------------------------------------------------------------------
cat(ocr(file, engine = eng))

## -----------------------------------------------------------------------------
file <- system.file("examples", "mrduke.jpg", package = "cpp11tesseract")
text <- ocr(file, engine = eng)
cat(text)

## ----eval = FALSE-------------------------------------------------------------
#  file <- system.file("examples", "chinese.jpg", package = "cpp11tesseract")
#  
#  # download the best model (vertical script download is to avoid a warning)
#  dir <- tempdir()
#  tesseract_download("chi_sim_vert", datapath = dir, model = "best")
#  tesseract_download("chi_sim", datapath = dir, model = "best")
#  
#  # compare the results: fast (text1) vs best (text2)
#  text1 <- ocr(file, engine = tesseract("chi_sim"))
#  text2 <- ocr(file, engine = tesseract("chi_sim", datapath = dir))
#  
#  cat(text1)
#  cat(text2)

## ----eval = FALSE-------------------------------------------------------------
#  file <- system.file("examples", "polytonicgreek.png", package = "cpp11tesseract")
#  
#  # download the best models
#  dir <- tempdir()
#  tesseract_download("grc", datapath = dir, model = "best")
#  tesseract_contributed_download("grc_hist", datapath = dir, model = "best")
#  
#  # compare the results: grc (text1) vs grc_hist (text2)
#  text1 <- ocr(file, engine = tesseract("grc", datapath = dir))
#  text2 <- ocr(file, engine = tesseract("grc_hist", datapath = dir))
#  
#  cat(text1)
#  cat(text2)

## -----------------------------------------------------------------------------
file <- system.file("examples", "tealbook.png", package = "cpp11tesseract")
text <- ocr(file)

cat(text)

## -----------------------------------------------------------------------------
text <- strsplit(text, "\n")[[1]]
text <- text[6:length(text)]

for (i in seq_along(text)) {
  firstdigit <- regexpr("[0-9]", text[i])[1]

  variable <- trimws(substr(text[i], 1, firstdigit - 1))

  values <- strsplit(substr(text[i], firstdigit, nchar(text[i])), " ")[[1]]
  values <- trimws(gsub(",", ".", values))
  values <- suppressWarnings(as.numeric(gsub("\\.$", "", values)))

  if (length(values[!is.na(values)]) < 1) {
    next
  }

  res <- c(variable, values)

  names(res) <- c(
    "variable", "y1964", "y1965est", "y1965q1",
    "y1965q2", "y1965q3", "y1965q4est", "y1966q1pro"
  )

  if (i == 1) {
    df <- as.data.frame(t(res))
  } else {
    df <- rbind(df, as.data.frame(t(res)))
  }
}

head(df)

