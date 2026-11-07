## ----set-defaults, echo=FALSE, results=FALSE, message=FALSE-------------------
knitr::opts_chunk$set(
    fig.dim = c(5, 5), # Size of stored figures in inches
    fig.show = "hold", # Render images as inline elements
    out.height = "auto", # (1)
    eval = FALSE,
    echo = FALSE,
    results = FALSE,
    message = FALSE
    # (1) Either out.width or out.heigt must be set of rmarkdown will not put
    # a div.figure around the individual img elements
)

## ----chunk-deconvolute, echo=TRUE---------------------------------------------
# sim_dir <- metabodecon::metabodecon_file("bruker/sim")
# sim <- metabodecon::read_spectra(sim_dir)
# deconvoluted_spectra <- metabodecon::deconvolute(
#     sim,                 # The object containing spectra
#     sfr = c(3.35, 3.55), # Borders of signal free region (SFR) in ppm
#     smopts = c(2, 5),    # Smoothing parameters
#     verbose = FALSE,     # Disable verbose output
#     ask = TRUE           # Enable interactive prompting
# )

## ----fig-deconvolute, eval=TRUE-----------------------------------------------
metabodecon::evalwith(
    answers = list(sameParams = "y", adjNo="1", sfrOk = "y", wsOk = "y"),
    pars = list(mar = c(4, 4, 2, 2)),
    expr = {
        sim_dir <- metabodecon::metabodecon_file("bruker/sim")
        sim <- metabodecon::read_spectra(sim_dir)
        deconvoluted_spectra <- metabodecon::deconvolute(
            sim,                 # The object containing spectra
            sfr = c(3.35, 3.55), # Borders of signal free region (SFR) in ppm
            smopts = c(2, 5),    # Smoothing parameters
            verbose = FALSE,     # Disable verbose output
            ask = TRUE           # Enable interactive prompting
        )
    }
)

## ----chunk-plot-spectrum, echo=TRUE-------------------------------------------
# # Visualize the first spectrum.
# metabodecon::plot_spectrum(deconvoluted_spectra[[1]])
# 
# # Visualize the second spectrum, this time without the legend.
# metabodecon::plot_spectrum(deconvoluted_spectra[[1]], lgd = FALSE)
# 
# # Visualize all spectra and save them to a pdf file
# pdfpath <- tempfile(fileext = ".pdf")
# pdf(pdfpath)
# for (x in deconvoluted_spectra) {
#     metabodecon::plot_spectrum(x, main = x$filename)
# }
# dev.off()
# cat("Plots saved to", pdfpath, "\n")

## ----fig-plot-spectrum, eval=TRUE---------------------------------------------
# Visualize the first spectrum.
metabodecon::plot_spectrum(deconvoluted_spectra[[1]])

# Visualize the second spectrum, this time without the legend.
metabodecon::plot_spectrum(deconvoluted_spectra[[1]], lgd = FALSE)

# Visualize all spectra and save them to a pdf file
pdfpath <- tempfile(fileext = ".pdf")
pdf(pdfpath)
for (x in deconvoluted_spectra) {
    metabodecon::plot_spectrum(x, main = x$filename)
}
dev.off()
cat("Plots saved to", pdfpath, "\n")

## ----chunk-align, echo=TRUE---------------------------------------------------
# # Plot spectra before alignment. Only show spectra 1-8 for clarity.
# metabodecon::plot_spectra(deconvoluted_spectra[1:8], lgd = FALSE)
# 
# # Align spectra and plot again.
# aligned_spectra <- try(metabodecon::align(deconvoluted_spectra)) # (1)
# metabodecon::plot_spectra(aligned_spectra[1:8])
# 
# # (1) The call to align() is wrapped in try() because the function may fail
# # if speaq's Bioconductor dependencies (MassSpecWavelet, impute) are missing
# # and the code runs in a non-interactive R session (e.g., during vignette
# # creation). In interactive sessions, try() is not needed, as the user will
# # be prompted to install missing dependencies automatically.

## ----fig-align, eval=TRUE-----------------------------------------------------
# Plot spectra before alignment. Only show spectra 1-8 for clarity.
metabodecon::plot_spectra(deconvoluted_spectra[1:8], lgd = FALSE)

# Align spectra and plot again.
aligned_spectra <- try(
    metabodecon::align(deconvoluted_spectra, install_deps = FALSE)
    # We must wrap the call to align() in try() because the function
    # may fail if speaq's Bioconductor dependencies (MassSpecWavelet,
    # impute) are missing during vignette creation
)
if (!inherits(aligned_spectra, "try-error")) {
    metabodecon::plot_spectra(aligned_spectra[1:8])
}

