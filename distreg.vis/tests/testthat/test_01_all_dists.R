###################################################
#######             distreg.vis            ########
#######         Distributional tests       ########
###################################################

########   ---    Preliminaries   ---    ########

## Skip on Cran
testthat::skip_on_cran()

## Libraries
library("distreg.vis")
library("bamlss")
library("gamlss")
library("gamlss.dist")
library("testthat")
library("ggplot2")
library("gridExtra")
library("betareg")

## Delete everything
rm(list = ls())

## Context
testthat::context("Test all dists")

##### Complete function #####
test_core <- function(fam_name) {

  ## Global environment, due to GAMLSS
  fam_name <<- fam_name

  ## Cat
  cat(paste0("Distribution Name: ", fam_name, "\n"))

  ########   ---    Creating data   ---    ########
  art_data <- model_fam_data(fam_name = fam_name)

  ########   ---    Creating models   ---    ########

  ## BAMLSS
  if (distreg.vis:::is.bamlss(fam_name)) {
    fam_called <- do.call(get(paste0(fam_name, "_bamlss"),
                              envir = as.environment("package:bamlss")),
                          args = list())

    # Different formulas depending on number of parameters
    form_list <- list(as.formula(paste0(fam_name, "~ norm2 + binomial1")))
    if (length(fam_called$names) > 1)
      form_list[[2]] <- ~ norm2 + binomial1

    # Create model
    model <- bamlss(form_list, data = art_data, family = fam_called, verbose = FALSE)
  }

  ## GAMLSS
  if (distreg.vis:::is.gamlss(fam_name)) {
    form <- as.formula(paste0(fam_name, "~ norm2 + binomial1"))
    model <- gamlss(form, sigma.formula = ~ .,
                    data = art_data, family = eval(fam_name), trace = FALSE)
  }

  if (distreg.vis:::is.betareg(fam_name)) {
    model <- betareg(betareg ~ norm2 + binomial1 | norm2 + binomial1,
                     data = art_data)
  }

  ########   ---    plot_dist()   ---    ########

  ## Predict parameters and plot distribution
  ndata <- art_data[sample(seq_len(nrow(art_data)), 5),
                    !colnames(art_data) %in% fam_name]
  pred_params <- preds(model, newdata = ndata)
  plots_dist <- plot_dist(model, pred_params, rug = TRUE) # pdf
  if (fam_name != "multinomial")
    plots_dist <- arrangeGrob(plots_dist,
                              plot_dist(model, pred_params, type = "cdf", rug = TRUE),
                              ncol = 2,
                              nrow = 1) # cdf

  ## Save the plots
  fileloc <- tempfile(pattern = paste0("plot_", fam_name, "_dist"),
                      fileext = ".png")

  ggsave(filename = fileloc, height = 12, width = 24, plot = plots_dist,
         units = "cm", device = "png", scale = 2)

  ########   ---    moments()   ---    ########

  ## Get the moments
  if (fam_name != "multinomial") {
    if (distreg.vis:::has.moments(fam_name)) {
      moms <- moments(pred_params, fam_name)
    } else {
      expect_error(moms <- moments(pred_params, fam_name))
    }
  }

  ########   ---    plot_moments()   ---    ########

  ## Create plots and save them if available
  if (distreg.vis:::has.moments(fam_name)) {

    # Not specifying an external function
    if (!fam_name %in% c("LOGNO", "gamma")) {
      # With pred_data
      plots_moments <- arrangeGrob(
        plot_moments(model, "norm2", pred_data = ndata),
        plot_moments(model, "binomial1", pred_data = ndata),
        ncol = 2,
        nrow = 1
      )
      # Without pred_data
      plots_moments_free <- arrangeGrob(
        plot_moments(model, "norm2"),
        plot_moments(model, "binomial1"),
        ncol = 2,
        nrow = 1
      )
    }

    # Specifying an external function
    if (fam_name == "LOGNO") {
      ineq <<- function(par) {
        2 * pnorm((par[["sigma"]] / 2) * sqrt(2)) - 1
      }
      # With pred_data
      plots_moments <- arrangeGrob(
        plot_moments(model, "norm2", pred_data = ndata, ex_fun = "ineq"),
        plot_moments(model, "binomial1", pred_data = ndata, ex_fun = "ineq"),
        ncol = 2,
        nrow = 1
      )
      # Without pred_data
      plots_moments_free <- arrangeGrob(
        plot_moments(model, "norm2", ex_fun = "ineq"),
        plot_moments(model, "binomial1", ex_fun = "ineq"),
        ncol = 2,
        nrow = 1
      )
    }

    # Obtaining samples and uncertainty (only one dist because otherwise test would be too long)
    if (fam_name == "gamma") {
      # With specifying pred_data
      plots_moments <- arrangeGrob(
        plot_moments(model, "norm2", pred_data = ndata,
                     samples = TRUE, uncertainty = TRUE),
        plot_moments(model, "binomial1", pred_data = ndata,
                     samples = TRUE, uncertainty = TRUE),
        ncol = 2,
        nrow = 1
      )
      # Without specifying pred_data
      plots_moments_free <- arrangeGrob(
        plot_moments(model, "norm2",
                     samples = TRUE, uncertainty = TRUE),
        plot_moments(model, "binomial1",
                     samples = TRUE, uncertainty = TRUE),
        ncol = 2,
        nrow = 1
      )
    }

    # Save
    fileloc <- tempfile(pattern = paste0("plot_", fam_name, "_moments"),
                        fileext = ".png")
    fileloc_free <- tempfile(pattern = paste0("plot_", fam_name, "_moments_free"),
                             fileext = ".png")
    ggsave(filename = fileloc, height = 7, width = 14, plot = plots_moments,
           units = "cm", device = "png")
    ggsave(filename = fileloc_free, height = 7, width = 14, plot = plots_moments_free,
           units = "cm", device = "png")
  } else {
    expect_error(plot_moments(model, "norm2", pred_data = ndata))
  }
}

## Now test the function with all implemented distributions
families <- dists[dists$implemented, "dist_name"]
for (fam in families)
  test_core(fam_name = fam)

## Delete empty Rplots.pdf file if exists
if (file.exists("Rplots.pdf"))
  file.remove("Rplots.pdf")
