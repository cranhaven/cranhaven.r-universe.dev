\name{apc-internal}
\alias{apc.internal.function.detrend}
\alias{apc.internal.function.date.2.character}
\alias{function.coin}
\alias{function.coin.array}
\title{Internal apc Functions}
\description{
  Internal apc functions
}
\details{
  These are not to be called by the user.

  \code{function.coin} is used the coin problem that is relevant for mixed arrays.
  Given coins of value G,H, it returns the Frobenius number, the Sylvester number
  and the number of Non-Representable amounts.

  \code{function.coin.array} is used the coin problem that is relevant for mixed arrays.
  Given r.g G-coins and r.h H-coins, it returns an array of all possible amounts that can be paid.
}
\author{Bent Nielsen <bent.nielsen@nuffield.ox.ac.uk> 25 Jun 2025 (1 Feb 2016)}