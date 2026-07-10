

plot.inet <- function(x,
                      labels = NULL,
                      order = FALSE,
                      subset = NULL,
                      cex.labels = 0.8,
                      cex.axis = 0.75,
                      ...) {



  # ---- Fill in defaults -----

  if("lasso" %in% class(x)) stop("Only available for estimation methods that produce CIs.")

  if(missing(labels)) labels <- NULL
  if(missing(order)) order <- FALSE
  if(missing(subset)) subset <- NULL
  if(missing(cex.labels)) cex.labels <- 0.8
  if(missing(cex.axis)) cex.axis <- 0.75


  # ---- Make sure par-settings are restored for users after exiting function -----
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))


  # ---- Get basic info -----

  p <- ncol(x$est)
  npar <- p*(p-1)/2

  if(is.null(labels)) labels <- 1:p

  # Subsetting? And if, how?
  subset_ind <- 0
  if(!is.null(subset)) {
    subset_ind <- 1 # Vector specification
    if(!is.null(dim(subset)[2])) subset_ind <- 2 # Matrix specification
  }


  # ----- Rearrange into par x 3 matrix -----

  m_CI <- matrix(NA, nrow = npar, ncol = 4)
  m_labels <- matrix(NA, nrow = npar, ncol = 2)
  co <- 1

  for(i in 1:p) {
    for(j in i:p) {
      if(i!=j) {
        # Indicators
        m_labels[co, 1] <- labels[i]
        m_labels[co, 2] <- labels[j]
        # Point estimate
        m_CI[co, 1] <- x$est[i,j]
        # CIs
        m_CI[co, 2] <- x$ci.lower[i,j] # lower
        m_CI[co, 3] <- x$ci.upper[i,j] # upper
        # Significance
        m_CI[co, 4] <- x$signif[i,j]
        # Set counter
        co <- co + 1
      }
    }
  }


  # ----- Plotting -----

  # Setup plotting area
  lmat <- matrix(1:2, nrow=1)
  lo <- layout(lmat, widths = c(.3, 1))

  # Plot Labels
  # Generate label vector
  if(is.null(labels)) {
    label_vec <- paste0(m_labels[, 1], " - ", m_labels[, 2])
  } else {
    tar_mat_label <- m_labels[ ,1:2]
    tar_mat_label <- apply(tar_mat_label, 1:2, as.character)
    for(i in 1:p) tar_mat_label[tar_mat_label == i] <- labels[i]
    label_vec <- paste0(tar_mat_label[, 1], " - ", tar_mat_label[, 2])
  }

  # Apply ordering, if applicable
  if(order) {
    ord <- order(abs(m_CI[, 1]), decreasing=TRUE)

    m_CI <- m_CI[ord, ]

    label_vec <- label_vec[ord]
  }

  # --- Apply subsetting, if applicable ---

  # If there is no subsetting, we got them all
  subset_label <- 1:npar

  # Version 1
  if(subset_ind == 1) {
    m_CI <- m_CI[subset, ]
    npar <- length(subset)
    subset_label <- subset
  } #  end if: subsetting 2

  # Version 2
  if(subset_ind == 2) {

    # Get rows specified by "subset"-matrix
    tarm_n <- apply(tar_mat_label, 2, as.numeric)
    # Indicator vector: should that edge be included?
    v_ind_ss <- rep(0, npar)
    n_spec <- nrow(subset)

    # Loop through pairwise comparisons
    for(i in 1:npar) {
      for(j in 1:n_spec) {
        if(subset[j,1] == tarm_n[i, 1] & subset[j,2] == tarm_n[i, 2]) v_ind_ss[i] <- 1
      }
    }

    subset_which <- which(v_ind_ss==1)

    m_CI <- m_CI[subset_which, ]
    npar <- length(subset_which)
    subset_label <- subset_which

  } # end if: subsetting 2



  # Define margins
  mar <- c(2,0,2,0)

  # Define reasonable xlim
  if(min(m_CI[, 2])>-0.5) xlim <- c(-0.5, 1) else xlim <- c(-1, 1)

  # Plot
  ylim <- c(0, 1)
  plot_y <- seq(ylim[2], ylim[1], length = npar)

  # par(mar=margins)
  par(mar=mar)
  plot.new()
  plot.window(xlim = c(-1, 1), ylim = ylim)
  text(0, plot_y, label_vec[subset_label], cex = cex.labels)

  # Plot Data
  par(mar=mar)
  plot.new()
  plot.window(xlim = xlim, ylim = ylim)

  # Zero line
  abline(v=0, lty=2)

  # Visual aid
  abline(h=plot_y, col="lightgrey", lty=2)

  # Compute pch based on significance
  pch <- c(1,20)[m_CI[, 4]+1]

  # Data
  points(as.numeric(m_CI[, 1]), plot_y, pch=pch)
  segments(m_CI[, 2], plot_y, m_CI[, 3], plot_y)

  # axis
  axis(3, cex.axis=cex.axis)
  axis(1, cex.axis=cex.axis)

  # ----- Return Outlist -----

  outlist <- list("CI_table" = m_CI)

  return(outlist)


} # eoF
