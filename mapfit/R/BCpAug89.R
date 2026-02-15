#' Packet Trace Data
#'
#' The data contains packet arrivals seen on an Ethernet at the Bellcore
#' Morristown Research and Engineering facility. Two of the traces are LAN
#' traffic (with a small portion of transit WAN traffic), and two are WAN
#' traffic. The original trace BC-pAug89 began at 11:25 on August 29, 1989,
#' and ran for about 3142.82 seconds (until 1,000,000 packets had been captured).
#' The trace BC-pOct89 began at 11:00 on October 5, 1989, and ran for about
#' 1759.62 seconds. These two traces captured all Ethernet packets.
#' The number of arrivals in the original trace is one million.
#' 
#' @docType data
#' @name BCpAug89
#' @format
#' \code{BCpAug89} is a vector for the inter-arrival time in seconds for 1000 arrivals.
#' @source 
#' The original trace data are published in http://ita.ee.lbl.gov/html/contrib/BC.html.
#' @keywords datasets
NULL

