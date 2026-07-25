epi.format <- function(ratio.tab, label = "", names = rep("", nrow(ratio.tab)), first.val = 1) {
  ratio.tab <- t(ratio.tab)
  
  ratio.tab <- rbind(
    c(first.val, NA, NA, NA),
    ratio.tab
  )
  
  ratio.format <- matrix("", ncol = 5, nrow = nrow(ratio.tab))
  
  ratio.format[, 1] <- names
  
  ratio.format[, 3] <- ifelse(
    is.na(ratio.tab[, "ci.lwr"]) | is.na(ratio.tab[, "ci.upr"]),
    "-",
    sprintf("(%.2f, %.2f)", ratio.tab[, "ci.lwr"], ratio.tab[, "ci.upr"])
  )
  
  ratio.format[, 4] <- ifelse(
    is.na(ratio.tab[, "p"]),
    "-",
    sprintf("%.3f", ratio.tab[, "p"])
  )
  
  ratio.format[, 2] <- ifelse(
    is.na(ratio.tab[, "estimate"]),
    "-",
    sprintf("%.2f", ratio.tab[, "estimate"])
  )
  
  ratio.format[, 5] <- ifelse(
    is.na(ratio.tab[, "estimate"]),
    sprintf("(%s cannot be estimated)", label),
    ""
  )
  
  ratio.format <- rbind(
    c("", label, "95% CI", "p-value", ""),
    ratio.format
  )
  
  ratio.format <- apply(ratio.format, 2, function(x) format(x, justify = "right"))
  ratio.format <- apply(ratio.format, MARGIN = 1, paste0, collapse = "   ")
  ratio.format <- paste0("   ", ratio.format)
  
  ratio.format
}

calculate_or <- function(table, conf.level = 0.95) {
  if (!all(dim(table) == 2)) {
    stop("Table should be 2x2")
  }
  
  odds <- c(
    table[2, 2] / table[2, 1],
    table[1, 2] / table[1, 1]
  )
  est <- odds[1] / odds[2]
  
  ## Only calculate CI/p-value if RR is defined
  if (is.finite(est)) {
    est.var <- sum(1 / table)
    
    ci <- exp(
      log(est) + c(-1, 1) * qnorm((1 - conf.level)/2) * sqrt(est.var)
    )
    ci <- sort(ci)
    
    ci.lwr <- ci[1]
    ci.upr <- ci[2]
    p <- fisher.test(table, conf.level = conf.level)$p.value
  } else {
    est    <- NA
    ci.lwr <- NA
    ci.upr <- NA
    p      <- NA
  }
  
  c(
    estimate = est, 
    ci.lwr = ci.lwr,
    ci.upr = ci.upr,
    p = p
  )
}

calculate_rr <- function(table, conf.level = 0.95) {
  if (!all(dim(table) == 2)) {
    stop("Table should be 2x2")
  }
  
  subtab.s <- rowSums(table)
  subtab.p <- prop.table(table, margin = 1)
  est <- subtab.p[2, 2] / subtab.p[1, 2]
  
  ## Only calculate CI/p-value if RR is defined
  if (is.finite(est)) {
    log.est <- log(est)
    log.est.se <- sqrt(
      (1/table[2, 2]) - (1/subtab.s[2]) + (1/table[1, 2]) - (1/subtab.s[1])
    )
    
    ci <- exp(log.est + c(-1, 1) * qnorm((1 - conf.level)/2) * log.est.se)
    ci <- sort(ci)
    
    ci.lwr <- ci[1]
    ci.upr <- ci[2]
    p <- fisher.test(table, conf.level = conf.level)$p.value
  } else {
    est    <- NA
    ci.lwr <- NA
    ci.upr <- NA
    p      <- NA
  }
  
  c(
    estimate = est, 
    ci.lwr = ci.lwr,
    ci.upr = ci.upr,
    p = p
  )
}

calculate_rd <- function(table, conf.level = 0.95) {
  if (!all(dim(table) == 2)) {
    stop("Table should be 2x2")
  }
  
  subtab.s <- rowSums(table)
  subtab.p <- prop.table(table, margin = 1)
  est <- subtab.p[2, 2] - subtab.p[1, 2]
  
  ## Only calculate CI/p-value if RR is defined
  if (is.finite(est)) {
    var1 <- (subtab.p[2,2] * subtab.p[2,1])/subtab.s[2]
    var2 <- (subtab.p[1,2] * subtab.p[1,1])/subtab.s[1]
    se <- sqrt(var1 + var2)

    ci <- est + c(-1, 1) * qnorm((1 - conf.level)/2) * se
    ci <- sort(ci)
    
    ci.lwr <- ci[1]
    ci.upr <- ci[2]
    
    chi.stat <- est / se
    p <- unname(2 * (1 - pnorm(abs(chi.stat))))
  }
  # else {
  #   est    <- NA
  #   ci.lwr <- NA
  #   ci.upr <- NA
  #   p      <- NA
  # }
  
  c(
    estimate = est, 
    ci.lwr = ci.lwr,
    ci.upr = ci.upr,
    p = p
  )
}
