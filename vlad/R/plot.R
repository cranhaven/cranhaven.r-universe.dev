#' @name VMASK3
#' @title Vmask3
#' @description Helper function to compute truncated symeterical/asymetrical vmask
#'
#' @param A ...
#' @param B ...
#' @param d1 Double. For the XYZ CUSUM Distance d from vertex of V-Mask. d=h/k
#' @param d2 Double. For the XYZ CUSUM Distance d from vertex of V-Mask. d=h/k
#' @param theta1 Double. Angle ...
#' @param theta2 Double. Angle ...
#' @param Sn ...
#' @param seg Logical. ...
#'
#' @return ...
#'
#' @importFrom tidyr gather

#' @author Philipp Wittenberg
#' @export
VMASK3 <- function(A, B, d1, d2, theta1, theta2, Sn, seg) {
  number <- n <- NULL
  theta1 <- (theta1*pi)/180
  theta2 <- (theta2*pi)/180
  ## helpers
  last.n <- B
  last.Sn <- Sn[last.n]
  upper <- function(i) last.Sn + tan(theta1) * (last.n - i + d1)
  lower <- function(i) last.Sn - tan(theta2) * (last.n - i + d2)
  if(seg==TRUE) { ## vertical segments only
    mask <- data.frame(cbind(n  =c(A, last.n),
                             up =c(upper(c(A, (last.n+d1)))),
                             low=c(lower(c(A, (last.n+d2))))),
                       number=rep(B, 2) )
    tidyr::gather(mask, key="group", value=value, c(-n, -number))
  } else { ## truncated V-mask
    mask <- data.frame(cbind(n  =c(A, last.n),
                             up =c(upper(c(A, (last.n)))),
                             low=c(lower(c(A, (last.n))))),
                       number=rep(B, 2) )
    tidyr::gather(mask, key="group", value=value, c(-n, -number))
  }
}

#' @name compute_vmask
#' @title Compute V-Masks arms, nose and alarm points
#' @description Function for plotting truncated symeterical/asymetrical vmask

#' @param z Numeric Vector. ...
#' @param d1 Double. For the XYZ CUSUM Distance d from vertex of V-Mask. d=h/k
#' @param d2 Double. For the XYZ CUSUM Distance d from vertex of V-Mask. d=h/k
#' @param theta1 Double. Angle ...
#' @param theta2 Double. Angle ...

#' @return ...

#' @importFrom dplyr bind_rows filter mutate select
#' @importFrom magrittr "%>%"

#' @author Philipp Wittenberg
#' @export
compute_vmask <- function(z, d1, d2, theta1, theta2) {
  Signal <- Clow <- Cup <- n <- k1 <- k2 <- h1 <- h2 <- NULL
  k1 <- tan(pi*theta1/180)
  k2 <- tan(pi*theta2/180)
  h1 <- k1*d1
  h2 <- k2*d2
  Sn <- cumsum(z)
  cv <- eocusum_scores(z=z, k1=k1, k2=k2, reset=TRUE, h1=h1, h2=h2)
  a  <- data.frame(cbind("n"=1:length(cv$s1), "Cup"=cv$s1, "Clow"=cv$s1l, "h2"=h2, "h1"=h1)) %>%
    dplyr::mutate(Signal=c(ifelse(Cup > h1, "Alarm U", ifelse(Clow < -h2, "Alarm L", "No Alarm")))) %>%
    dplyr::filter(Signal!="No Alarm") %>% dplyr::select(n)
  g <- nrow(a)
  ## arms of vmask
  DD1 <- dplyr::bind_rows(
    ## first Vmask
    VMASK3(1, a[1,], d1, d2, theta1, theta2, Sn, FALSE),
    ## Vmasks in between
    lapply(2:g, function(i) VMASK3(a[i-1,], a[i,], d1, d2, theta1, theta2, Sn, FALSE)),
    ## last Vmask
    VMASK3(a[g,], length(Sn), d1, d2, theta1, theta2, Sn, FALSE),
    .id="Masks"
  )
  ## vertical segments of truncated vmask
  DD2 <- dplyr::bind_rows(
    VMASK3(a[1,], a[1,], d1, d2, theta1, theta2, Sn, TRUE),
    lapply(2:g, function(i) VMASK3(a[i,], a[i,], d1, d2, theta1, theta2, Sn, TRUE)),
    VMASK3(length(Sn), length(Sn), d1, d2, theta1, theta2, Sn, TRUE),
    .id="Masks"
  )
  ## alarm points
  DD3 <- cbind(Masks=as.factor(1:g), n=a, value=Sn[a[, 1]])
  return(list("arms"=DD1, "nose"=DD2, "alarms"=DD3))
}
