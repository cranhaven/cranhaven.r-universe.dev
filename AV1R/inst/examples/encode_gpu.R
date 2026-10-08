library(AV1R)

input <- "/mnt/Data2/DS_projects/AV_test/test.avi"
output     <- "/mnt/Data2/DS_projects/AV_test/test_av1_gpu.mp4"




#convert_folder("/mnt/Data2/Share/Утиные Истории - Duck Tales/", file.path(tempdir(), "av1_output"))
#input <- "/mnt/Data2/Share/Утиные Истории - Duck Tales/"
#output     <- "/mnt/Data2/DS_projects/AV_test/test_av1_gpu.mp4"

# Check what GPU backend is available
bk <- detect_backend()
cat("Detected backend:", bk, "\n")

# Show GPU devices if Vulkan is available
if (vulkan_available()) {
  cat("Vulkan devices:\n")
  cat(paste(" ", vulkan_devices()), sep = "\n")
}

# Encode using best available GPU backend
convert_to_av1(
  input  = input,
  output = output,
  options = av1r_options(
    crf     = 28,      # quality
    backend = "auto"   # auto: vulkan > vaapi > cpu
  )
)

# Compare file sizes
size_in  <- file.info(input)$size  / 1024^2
size_out <- file.info(output)$size / 1024^2
cat(sprintf("\nOriginal:  %.1f MB\nAV1 (GPU): %.1f MB\nRatio:     %.2fx smaller\n",
            size_in, size_out, size_in / size_out))
