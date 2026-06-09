# r <- conventional_lens_image()$Blue
# jpeg(filename = "man/figures/twocorner_method.jpg", width=400, height=400, quality = 100)
# thr_twocorner(r[], diagnose = TRUE, sigma = 2, slope_reduction = FALSE,
#               adjust_par = TRUE)
# dev.off()
r <- conventional_lens_image()$Blue
png(filename = "man/figures/twocorner_method.png", width=400, height=400)
thr_twocorner(r[], diagnose = TRUE, sigma = 2, slope_reduction = FALSE,
              adjust_par = TRUE)
dev.off()
# r <- conventional_lens_image()$Blue
# jpeg(filename = "man/figures/twocorner_method.jpg", width=800, height=800)
# par(mar = c(0,0,1,0))
# ma <- matrix(c(1, 1, 2, 3), nrow = 2, byrow = TRUE)
# layout(ma)
# plot(r, col = grey((0:255)/255), axes = FALSE)
# thr_twocorner(r[], diagnose = TRUE, window_length = 3, slope_reduction = FALSE,
#               adjust_par = FALSE)
# for (i in 1:100) {
#   text(jitter(0.1*255), jitter(0.9*255),
#        "Original Rosin's construction", pos = 4, cex = 2, col = "white")
# }
# text(0.1*255, 0.9*255, "Original Rosin's construction", pos = 4, cex = 2)
# thr_twocorner(r[], diagnose = TRUE, window_length = 3, slope_reduction = TRUE,
#               adjust_par = FALSE)
# for (i in 1:100) {
#   text(jitter(0.1*255), jitter(0.9*255),
#        "Macfarlane's slope reduction", pos = 4, cex = 2, col = "white")
# }
# text(0.1*255, 0.9*255, "Macfarlane's slope reduction", pos = 4, cex = 2)
# dev.off()
