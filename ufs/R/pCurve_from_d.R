pCurve_from_d <- function(d,
                          n = NULL,
                          power = NULL,
                          pVals = c(0.01, 0.02, 0.03, 0.04, 0.05)) {

  input <- as.list(environment());

  if (is.null(power) && is.null(n)) {

    stop("Supply either power or n (currently, both are NULL).");

  } else if (is.null(power)) {

    power <- pwr::pwr.t.test(n=n/2, d=d);

  } else if (is.null(n)) {

    n <- 2 * pwr::pwr.t.test(power=power, d=d)$n;

  }

  pVals <- sort(pVals, decreasing = FALSE);

  dVals <-
    unlist(lapply(pVals / 2,
                  ufs::qCohensd,
                  df = n - 2));

  dVals_pos <- abs(dVals);
  dVals_neg <- -1 * dVals_pos;

  pProps_lower_cumulative <-
    unlist(lapply(dVals_neg,
                  ufs::pd,
                  df = n - 2,
                  populationD = d,
                  lower.tail = TRUE));

  pProps_upper_cumulative <-
    unlist(lapply(dVals_pos,
                  ufs::pd,
                  df = n - 2,
                  populationD = d,
                  lower.tail = FALSE));

  pProps_total_cumulative <-
    pProps_lower_cumulative + pProps_upper_cumulative;

  # pProps_higher_cumulative <-
  #   unlist(lapply(dVals,
  #                 ufs::pd,
  #                 n = n,
  #                 populationD = d,
  #                 lower.tail = TRUE));

  # pProps_cumulative <-
  #   unlist(lapply(dVals,
  #                 ufs::pdExtreme,
  #                 n = n,
  #                 populationD = d));

  pProps <-
    pProps_total_cumulative -
    c(0, pProps_total_cumulative[seq_along(pProps_total_cumulative) - 1]);

  pProps_rel_sum <-
    sum(pProps);
  pProps_rel <-
    100 * (pProps / pProps_rel_sum);

  df <-
    data.frame(pVals = pVals,
               dVals = dVals,
               dVals_neg = dVals_neg,
               dVals_pos = dVals_pos,
               pProps_lower_cumulative = pProps_lower_cumulative,
               pProps_upper_cumulative = pProps_upper_cumulative,
               pProps_total_cumulative = pProps_total_cumulative,
               pProps = pProps,
               pProps_rel = pProps_rel)

  plot <-
    ggplot2::ggplot(data = df,
                    mapping = ggplot2::aes_string(x = 'pVals',
                                                  y = 'pProps_rel')) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = paste0("P-curve for power of ", round(100*power), "%"),
                  subtitle = paste0("This corresponds to n=", floor(n), " when aiming to detect d=", d),
                  x = "p",
                  y = "Proportion of p-values (out of p values < .05)");

  res <-
    list(input = input,
         df = df,
         plot = plot);

  return(res);

}
