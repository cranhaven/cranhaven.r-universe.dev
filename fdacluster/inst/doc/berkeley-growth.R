## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

load("../R/sysdata.rda")

library(fdacluster)
library(ggplot2)

unnest_string <- function(x, cols) {
  times <- lengths(x[[cols[1]]])
  if (length(cols) > 1) {
    for (i in 2:length(cols)) {
      if (!all(lengths(x[[cols[i]]]) == times)) {
        cli::cli_abort("All columns in the dataset must have the same length.")
      }
    }
  }
  
  base_names <- names(x)[!(names(x) %in% cols)]
  out <- lapply(base_names, \(.base_name) {
    do.call(c, mapply(\(.x, .times) {
      is_factor <- is.factor(.x)
      if (is_factor) {
        lvls <- levels(.x)
        .x <- as.character(.x)
      }
      if (is.character(.x)) {
        if (length(.x) == 1) {
          out <- rep(.x, .times)
          if (is_factor) {
            out <- factor(out, levels = lvls)
          }
          return(out)
        }
        out <- replicate(.times, .x, simplify = FALSE)
        if (is_factor) {
          out <- lapply(out, \(.out) factor(.out, levels = lvls))
        }
        return(out)
      }
      mat <- tcrossprod(rep(1, .times), .x)
      if (ncol(mat) == 1) {
        return(rep(.x, .times))
      }
      out <- lapply(seq_len(nrow(mat)), \(i) mat[i, ])
    }, x[[.base_name]], times, SIMPLIFY = FALSE))
  })
  names(out) <- base_names
  out <- tibble::as_tibble(out)
  for (col in cols) {
    out[[col]] <- unlist(x[[col]])
  }
  out
}

unnest <- function(x, ...) {
  cols <- rlang::enquos(...)
  cols <- sapply(cols, \(x) rlang::as_name(x))
  unnest_string(x, cols)
}

matrix_tree <- function(x, margin = 1) {
  if (margin == 1) {
    return(lapply(seq_len(nrow(x)), \(i) x[i, ]))
  }
  lapply(seq_len(ncol(x)), \(i) x[, i])
}

## ----fig.width=7, fig.retina=2------------------------------------------------
growth <- fda::growth
mb <- as.factor(c(
  rep("male", dim(growth$hgtm)[2]), 
  rep("female", dim(growth$hgtf)[2])
))
N <- length(mb)
x <- growth$age
M <- length(x)
y0 <- cbind(growth$hgtm, growth$hgtf)
tibble::tibble(
  Age = replicate(N, x, simplify = FALSE),
  Height = matrix_tree(y0, margin = 2),
  Gender = mb,
  CurveID = 1:N
) |> 
  unnest(Age, Height) |> 
  ggplot(aes(Age, Height, color = Gender, group = CurveID)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(
    title = "Heights of 39 boys and 54 girls from age 1 to 18",
    x = "Age (years)",
    y = "Height (cm)"
  )

## -----------------------------------------------------------------------------
basisobj <- fda::create.bspline.basis(rangeval = range(x), nbasis = 15)
fd_vals <- lapply(1:N, \(n) {
  yobs <- y0[, n]
  result <- fda::smooth.basis(x, yobs, basisobj)
  yfd <- result$fd
  cost <- function(lam) {
    yfdPar <- fda::fdPar(yfd, 2, lam)
    out <- fda::smooth.basis(x, yobs, yfdPar)
    out$gcv
  }
  lambda_opt <- stats::optimise(cost, c(1e-8, 1))$minimum
  if (lambda_opt <= 1e-8)
    cli::cli_alert_warning("The optimal penalty has reached the lower bound (1e-8) for curve #{n}.")
  if (lambda_opt >= 1)
    cli::cli_alert_warning("The optimal penalty has reached the upper bound (1) for curve #{n}.")
  yfdPar <- fda::fdPar(yfd, 2, lambda_opt)
  fda::smooth.fd(yfd, yfdPar)
})
fd <- fda::fd(
  coef = fd_vals |>
    lapply(\(.x) .x$coefs) |>
    do.call(cbind, args = _),
  basisobj = basisobj
)

## ----fig.width=7, fig.retina=2------------------------------------------------
y0 <- fda::eval.fd(x, fd, 0)
tibble::tibble(
  Age = replicate(N, x, simplify = FALSE),
  Height = matrix_tree(y0, margin = 2),
  Gender = mb,
  CurveID = 1:N
) |> 
  unnest(Age, Height) |> 
  ggplot(aes(Age, Height, color = Gender, group = CurveID)) + 
  geom_point() + 
  geom_line() +
  theme_bw() + 
  labs(
    title = "Heights of 39 boys and 54 girls from age 1 to 18", 
    x = "Age (years)", 
    y = "Height (cm)"
  )

## ----fig.width=7, fig.retina=2------------------------------------------------
y1 <- fda::eval.fd(x, fd, 1)
tibble::tibble(
  Age = replicate(N, x, simplify = FALSE),
  Height = matrix_tree(y1, margin = 2),
  Gender = mb,
  CurveID = 1:N
) |>
  unnest(Age, Height) |>
  ggplot(aes(Age, Height, color = Gender, group = CurveID)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(
    title = "Growth velocity of 39 boys and 54 girls from age 1 to 18",
    x = "Age (years)",
    y = "Growth velocity (cm/year)"
  )

## ----eval=FALSE---------------------------------------------------------------
# growth_mcaps <- compare_caps(
#   x = x,
#   y = t(y1),
#   n_clusters = 2,
#   metric = "normalized_l2",
#   clustering_method = c(
#     "kmeans",
#     "hclust-complete",
#     "hclust-average",
#     "hclust-single",
#     "dbscan"
#   ),
#   warping_class = "affine",
#   centroid_type = "mean",
#   cluster_on_phase = TRUE
# )

## ----fig.width=7, fig.retina=2------------------------------------------------
plot(growth_mcaps, validation_criterion = "wss", what = "distribution")

## ----fig.width=7, fig.retina=2------------------------------------------------
plot(growth_mcaps, validation_criterion = "silhouette", what = "distribution")

## ----eval=FALSE---------------------------------------------------------------
# growth_caps <- fdahclust(
#   x = x,
#   y = t(y1),
#   n_clusters = 2,
#   metric = "normalized_l2",
#   warping_class = "affine",
#   centroid_type = "mean",
#   cluster_on_phase = TRUE
# )

## ----fig.width=7, fig.retina=2, fig.height=5----------------------------------
plot(growth_caps, type = "amplitude")

## ----fig.width=7, fig.retina=2------------------------------------------------
plot(growth_caps, type = "phase")

## ----fig.width=7, fig.retina=2------------------------------------------------
diagnostic_plot(growth_caps)

## -----------------------------------------------------------------------------
table(growth_caps$memberships, mb) |> 
  `rownames<-`(c("Group 1", "Group 2")) |> 
  knitr::kable()

