################################################################
## script_01_density_figures.R
##
## Generates Figures 1(a), 1(b), 2(a), 2(b) of the paper:
## EVBS and log-EVBS densities. Saves to PNG at 500 dpi.
##
## Usage (after installing the package):
##   library(evbsreg)
##   source(system.file("scripts/script_01_density_figures.R",
##                       package = "evbsreg"))
################################################################

library(evbsreg)

outdir <- tempdir()

cat("Generating Figure 1(a): EVBS densities, varying alpha...\n")
p1 <- plot_evbs_alpha()
ggplot2::ggsave(file.path(outdir, "plot1.png"), plot = p1,
                width = 13, height = 11, units = "cm", dpi = 500)

cat("Generating Figure 1(b): EVBS densities, varying gama...\n")
p2 <- plot_evbs_gama()
ggplot2::ggsave(file.path(outdir, "plot2.png"), plot = p2,
                width = 13, height = 11, units = "cm", dpi = 500)

cat("Generating Figure 2(a): log-EVBS densities, varying alpha...\n")
p3 <- plot_logevbs_alpha()
ggplot2::ggsave(file.path(outdir, "plot3.png"), plot = p3,
                width = 13, height = 11, units = "cm", dpi = 500)

cat("Generating Figure 2(b): log-EVBS densities, varying gama...\n")
p4 <- plot_logevbs_gama()
ggplot2::ggsave(file.path(outdir, "plot4.png"), plot = p4,
                width = 13, height = 11, units = "cm", dpi = 500)

cat(sprintf("\nAll density figures saved to: %s\n", outdir))
