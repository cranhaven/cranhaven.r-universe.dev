#' @title  Ground-State spectral term
#' @param  l the l:0,1,2,3,4,5,...
#' @param  n the number of electrons.
#' @description Use such a function to calculate the spectral term,
#'  and  it can show the number of the spectral term.
#' @author Chai
#' @importFrom MASS as.fractions
#' @return  It is a display of the results of the calculation of
#'  the spectral term. For more explanation, please
#'  refer to the structural chemistry.
#'   The ground_state() function will tell you
#'  the ground state that spectral term of  equal electrons.
#'  The state_1() and state_2() will tell you the
#'  spectral term of equal electrons.
#' @rdname  ground_state
#' @export ground_state
ground_state <- function(x) {
  orbital <- c("S", "P", "D", "F", "G", "H", "I", "K", "L", "M", "N", "O")
  l <- which(substr(x, 1, 1) == tolower(orbital))

  n <- substring(x, 2) %>% as.numeric()
  kh(l, n) -> m
  k <- MASS::as.fractions(m[3])
  paste0(m[1], "^", orbital[m[2] + 1], "_", k) %>% noquote()
}


#' @rdname  ground_state
#' @export   state_1
#' @references
#' The method of  state_1() and state_2() function is from:
#' DOI:10.14159/j.cnki.0441-3776.1985.11.020
#' And the url is:
#' \url{https://t.cnki.net/kcms/detail?v=3uoqIhG8C44YLTlOAiTRKqd0WnNPv0wTDjtDUwHroNz8ZoQZVLjnVKa9t-3R3F8t9DSW1fOAkwcAF8JZi9EnOU8ccrbY3bNC&uniplatform=NZKPT}
#' You can get more details from this essay.
state_1 <- function(l, n) FG2(FG(l, n))
#' @examples
#' ground_state("p2")
#' ground_state("f3")
#' state_2("p2")
#' state_2("d3")
#' @rdname ground_state
#' @param  x 'p2','p3','d2','d5','f2',...
#' @export state_2
state_2 <- function(x) FG2(FG_1(x))

NULL
kh <- function(l, n) {
  n1 <- (2 * l - 1)
  n1 # orbital
  S <- 0.5 * n + ifelse(n > n1, -(n - n1), 0) # Ms
  f1 <- function(x) c(seq(x - 1, -(x - 1), by = -1), seq(-(x - 1), x - 1, by = 1)) # l order
  b1 <- f1(l)
  L <- abs(sum(b1[1:n]))
  ###### J
  j1 <- seq(L + S, abs(L - S), by = -1)
  if (n > n1) {
    J <- max(j1)
  } else if (n == n1) {
    J <- S
  } else {
    J <- min(j1)
  }
  return(c(2 * S + 1, L, J))
}


## for example: P2, l = 1; N = 2
N_ab <- function(l, N) {
  if (N %% 2 == 0) {
    S <- seq(N / 2, 0, by = -1)
  } else {
    S <- seq(N / 2, 0.5, by = -1)
  }
  Na <- S + N / 2
  Nb <- N - Na
  cbind(S, Na, Nb)
}

Na_La <- function(Na, l) {
  Ma <- sum(l + 1 - 1:Na)
  if (Na == 0 | Na == 2 * l + 1) {
    La <- 0
  } else if (Na == 1 | Na == 2 * l) {
    La <- l
  } else if (Ma %% 2 != 0) {
    La <- seq(max(Ma), 1, by = -2)
  } else {
    mm <- seq(max(Ma), 0, by = -2)
    La <- c(mm, mean(mm))
  }
  return(La)
}

######
###### La and Lb -> L
ab_L <- function(a, b) {
  seq(a + b, abs(a - b), by = -1)
}
#####
##### Lab_L -> vector!
L_vector <- function(a, b) {
  g1 <- c()
  for (i in seq_along(a)) {
    for (j in seq_along(b)) {
      r <- ab_L(a[i], b[j])
      g1 <- c(g1, r)
    }
  }
  return(g1)
}
####
####
L_list <- function(x, y) {
  g2 <- paste0("x", 1:length(x))
  for (i in seq_along(x)) {
    r2 <- L_vector(x[[i]], y[[i]])
    assign(g2[i], r2)
  }
  a1 <- lapply(g2, function(x) get(x))
  do.call(list, a1) # list object
}
####
#### the max  of list
list_max <- function(y) max(unlist(y))

y_ymax <- function(y, ymax) c(y, 0:ymax)

##### table
#####
list_table <- function(k) {
  ymax <- list_max(k)
  lapply(k, y_ymax, ymax = ymax) -> m1
  sapply(m1, table) - 1
}

## Calculates the column-by-column difference and
##      assigns it to the current column
##############
list_diff <- function(k) {
  m <- ncol(k)
  t(apply(k, 1, cumsum)) -> m2
  b1 <- cbind(0, m2[, 1:(m - 1)])
  k - b1
}
##############
FG <- function(l, n) {
  if (n == 1 || n >= 4 * l + 1) {
    return("n = 1 or >= 4 *l +1,stop!")
  } else if (2 * l + 1 < n && n < 4 * l + 2) {
    n <- 4 * l + 2 - n
  } else {
    N_ab(l, n) -> k1 # return S, Na,Nb
    lapply(k1[, 2], Na_La, l = l) -> q1 # Na ->  La
    lapply(k1[, 3], Na_La, l = l) -> q2 # Nb -> Lb
    L_list(q1, q2) -> k2 # based on La,Lb ->  L
    list_table(k2) -> k3 # S -> table
    list_diff(k3) -> k4 # list -> diff
    colnames(k4) <- as.numeric(k1[, 1])
    nrow(k4) -> list_len
    m2 <- c(
      "S", "P", "D", "F", "G", "H", "I", "K",
      "L", "M", "N", "O", "P", "Q", "R", "S", "T"
    )
    noquote(cbind(m2[1:list_len], k4))
  }
}
############
FG_1 <- function(x) {
  m2 <- c(
    "S", "P", "D", "F", "G", "H", "I", "K",
    "L", "M", "N", "O"
  )
  mm <- tolower(m2)
  l <- which(substr(x, 1, 1) == mm) - 1
  n <- substring(x, 2) %>% as.numeric()
  FG(l, n)
}
################
FG2 <- function(x) {
  if (is.matrix(x) == TRUE) {
    d0 <- colnames(x)
    d1 <- as.numeric(d0)
    d2 <- 2 * d1 + 1
    d2[1] <- "L/2S+1"
    colnames(x) <- d2
    return(x)
  } else {
    x
  }
}
