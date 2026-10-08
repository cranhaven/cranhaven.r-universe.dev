library(AV1R)

input  <- "/mnt/Data2/DS_projects/AV_test/mitosis.tif"
output <- "/mnt/Data2/DS_projects/AV_test/mitosis_av1.mp4"

# Check what GPU backend is available
bk <- detect_backend()
cat("Detected backend:", bk, "\n")

# Encode TIFF stack
convert_to_av1(
  input  = input,
  output = output,
  options = av1r_options(
    crf     = 3,
    backend = "auto"
  )
)

# Compare file sizes
size_in  <- file.info(input)$size  / 1024^2
size_out <- file.info(output)$size / 1024^2
cat(sprintf("\nOriginal:  %.1f MB\nAV1 (GPU): %.1f MB\nRatio:     %.2fx smaller\n",
            size_in, size_out, size_in / size_out))
