#' @keywords internal
.perf_plot <- function(BTFit_object,
                      best_iter,
                      out_of_bag_curve,
                      overlay,
                      method,
                      main) {
  # Check inputs
  .check_if_BT_fit(BTFit_object)
  if (!is.logical(overlay) ||
      (length(overlay)) > 1 || is.na(overlay))
    stop("overlay must be a logical - excluding NA")

  if (!is.logical(out_of_bag_curve) ||
      (length(out_of_bag_curve)) > 1 || is.na(out_of_bag_curve))
    stop("out_of_bag_curve must be a logical - excluding NA")

  #Restore old parameters on exit.
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))

  par(mar = c(5, 4, 4, 4) + .1)

  # Get y-axis label and limits
  ylab <-
    paste("Tweedie deviance (power=",
          BTFit_object$distribution,
          ")",
          sep = "")
  if (!.has_train_validation_split(BTFit_object)) {
    ylim <- switch(
      method,
      cv = range(
        .iteration_error(BTFit_object, 'train'),
        .iteration_error(BTFit_object, 'cv')
      ),
      validation = range(
        .iteration_error(BTFit_object, 'train'),
        .iteration_error(BTFit_object, 'validation')
      ),
      OOB = range(.iteration_error(BTFit_object, 'train'))
    ) # Those are the only 3 possibilities allowed by the main BT_callPerformance function, no further test needed.
  } else {
    ylim <- range(
      .iteration_error(BTFit_object, 'train'),
      .iteration_error(BTFit_object, 'validation')
    )
  }

  # Initial plot
  plot(
    .iteration_error(BTFit_object, 'train'),
    ylim = ylim,
    type = "l",
    xlab = "Iteration",
    ylab = ylab,
    main = main
  )

  if (.has_train_validation_split(BTFit_object)) {
    lines(.iteration_error(BTFit_object, 'validation'), col = "red")
  }
  if (method == "cv") {
    lines(.iteration_error(BTFit_object, 'cv'), col = "green")
  }
  if (!is.na(best_iter))
    abline(
      v = best_iter,
      col = "blue",
      lwd = 2,
      lty = 2
    )

  # Plot out of bag curve
  if (out_of_bag_curve) {
    if (BTFit_object$BTParams$bag.fraction == 1)
      stop("Cannot compute OOB estimate or the OOB curve when bag.fraction=1")
    if (all(!is.finite(BTFit_object$BTErrors$oob.improvement)))
      stop("Cannot compute OOB estimate or the OOB curve. No finite OOB estimates of improvement")

    .plot_oobag(BTFit_object, best_iter, overlay, ylab)
  }
}

#' @keywords internal
.plot_oobag <- function(BTFit_object, best_iter, overlay, ylab) {
  # Get smoother
  smoother <- .generate_smoother_oobag(BTFit_object)

  # Plot smoothed out of bag improvement
  if (overlay) {
    #Restore old parameters on exit.
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))

    par(new = TRUE)
    plot(
      smoother$x,
      cumsum(smoother$y),
      col = "blue",
      type = "l",
      xlab = "",
      ylab = "",
      axes = FALSE
    )
    axis(4, srt = 0)
    at <- mean(range(smoother$y))
    mtext(
      paste("OOB improvement in", ylab),
      side = 4,
      srt = 270,
      line = 2
    )
    abline(h = 0,
           col = "blue",
           lwd = 2)
  }

  # Plot original out of bag improvement
  plot(
    BTFit_object$BTErrors$oob.improvement,
    type = "l",
    xlab = "Iteration",
    ylab = paste("OOB change in", ylab)
  )
  lines(smoother, col = "red", lwd = 2)
  abline(h = 0,
         col = "blue",
         lwd = 1)
  abline(v = best_iter,
         col = "blue",
         lwd = 1)
}
