#' Make unique pairs of level names for correlations
#'
#' @param v a vector of level names or a number of levels
#'
#' @return a vector of all unique pairs
#' @export
#'
#' @examples
#' unique_pairs(c("O", "C", "E", "A", "N"))
#' unique_pairs(3)
unique_pairs <- function(v) {
  if (is.numeric(v) & length(v) == 1) { v <- LETTERS[1:v] }
  if (length(v) < 2) { stop("There must be at least 2 levels") }
  if (duplicated(v) %>% sum()) { stop("You have duplicate levels") }
  v <- factor(v, levels = v)
  
  all <- expand.grid(a = v, b = v)
  diff <- all[all$a != all$b, ]
  a_less <- as.integer(diff$a) < as.integer(diff$b)
  a <- as.character(diff$a)
  b <- as.character(diff$b)
  pairs <- ifelse(a_less, paste0(a, "-", b), paste0(b, "-", a))
  unique(pairs)
}


#' Set design interactively
#' 
#' @param output what type of design to output (faux)
#' @param plot whether to show a plot of the design
#'
#' @return list
#' @export
#'
#' @examples
#' if(interactive()){ des <- interactive_design() }
interactive_design <- function(output = c("faux"),
                               plot = faux_options("plot")) {
  # within factors ----
  wn <- readline_check("How many within-subject factors do you have?: ", "integer")
  within <- list()
  if (wn > 0) {
    for (i in 1:wn) {
      p <- paste0("Name of within-subject factor ", i, ": ")
      name <- readline_check(prompt=p, "length", min = 1)
      
      p <- paste0("How many levels of ", name, ": ")
      nlevels <- readline_check(p, "integer", min = 2)
      
      levels <- c()
      for (j in 1:nlevels) {
        p <- paste0("Name of factor ", name, ", level ", j, ": ")
        levels[j] <- readline_check(prompt=p, "length", min = 1)
      }
      
      within[[name]] <- levels
    }
  }
  
  # between factors ----
  bn <- readline_check("How many between-subject factors do you have?: ", "integer")
  between <- list()
  if (bn > 0) {
    for (i in 1:bn) {
      p <- paste0("Name of between-subject factor ", i, ": ")
      name <- readline_check(prompt=p, "length", min = 1)
      
      p <- paste0("How many levels of ", name, ": ")
      nlevels <- readline_check(p, "integer", min = 2)
      
      levels <- c()
      for (j in 1:nlevels) {
        p <- paste0("Name of factor ", name, ", level ", j, ": ")
        levels[j] <- readline_check(prompt=p, "length", min = 1)
      }
      between[[name]] <- levels
    }
  }
  
  # dv and id ----
  dv <- readline_check("DV column name (default is y): ", "length", min = 1)
  id <- readline_check("ID column name (default is id): ", "length", min = 1)
  
  within <- lapply(within, fix_name_labels)
  between <- lapply(between, fix_name_labels)
  
  cells_w <- cell_combos(within, dv)
  cells_b <- cell_combos(between, dv)
  
  # ask for N ----
  blist <- paste(cells_b, collapse = ", ")
  int_pattern <- paste0("^\\s?(\\d+\\s?,\\s?){0,", (length(cells_b)-1), "}\\s?(\\d+)\\s?$")
  
  if (length(between)) {
    p <- paste0("Number of subjects in conditions ", blist, ": ")
  } else {
    p <- "Number of subjects: "
  }
  n <- readline_check(p, "grep", pattern = int_pattern, perl = TRUE) %>%
    strsplit("\\s?,\\s?") %>% 
    unlist() %>% as.integer() 
  if (length(n) == 1) n <- rep(n, length(cells_b))
  names(n) <- cells_b
  n <- as.list(n)
  
  wlist <- paste(cells_w, collapse = ", ")
  pattern <- paste0("^\\s?(-?\\d*\\.?\\d?\\s?,\\s?){0,", (length(cells_w)-1), "}\\s?(-?\\d*\\.?\\d?)\\s?$")
  
  # ask for mu ----
  mu <- lapply(cells_b, function(b) {
    p <- paste0("Means of ", wlist, " in condition ", b, ": ")
    input <- readline_check(p, "grep", pattern = pattern, perl = TRUE) %>%
      strsplit("\\s?,\\s?") %>% unlist()
    if (length(input) == 1) input <- rep(input, length(cells_w))
    input
  }) %>% unlist() %>% as.double() %>%
    matrix(nrow = length(cells_w), dimnames = list(cells_w, cells_b)) %>%
    as.data.frame()

  # ask for SD ----
  sd <- lapply(cells_b, function(b) {
    p <- paste0("SDs of ", wlist, " in condition ", b, ": ")
    input <- readline_check(p, "grep", pattern = pattern, perl = TRUE) %>%
      strsplit("\\s?,\\s?") %>% unlist()
    if (length(input) == 1) input <- rep(input, length(cells_w))
    input
  }) %>% unlist() %>% as.double() %>%
    matrix(nrow = length(cells_w), dimnames = list(cells_w, cells_b)) %>%
    as.data.frame()
  
  # ask for r ----
  if (length(cells_w)> 1) {
    up <- unique_pairs(cells_w)
    uplist <- paste(up, collapse = ", ")
    pattern <- paste0("^\\s?(-?\\d*\\.?\\d?\\s?,\\s?){0,", (length(up)-1), "}\\s?(-?\\d*\\.?\\d?)\\s?$")
    r <- lapply(cells_b, function(b) {
      p <- paste0("Cors (r) of ", uplist, " in condition ", b, ": ")
      input <- readline_check(p, "grep", pattern = pattern, perl = TRUE) %>%
        strsplit("\\s?,\\s?") %>% unlist()
      tri_n <- length(cells_w) * (length(cells_w)-1) / 2
      if (length(input) == 1) input <- rep(input, tri_n)
      input %>% as.double()
    })
    names(r) <- cells_b
  } else {
    r <- 0 # no within subject factors
  }

  design <- check_design(within = within, between = between, 
                         n = n, mu = mu, sd = sd, r = r, dv = dv, id = id, plot = plot)
  
  message("\033[32mYour design is ready!\033[39m")
  
  output <- match.arg(output)
  #if (output == "ANOVApower") {
  #  faux2ANOVA_design(design)
  #} else {
    design
  #}
}
