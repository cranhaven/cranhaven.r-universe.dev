# Define simple function to convert two-tailed p-values to one-tailed, 
#  based on means of comparison groups
# - this assumes null: Mean2=Mean1 and alt: Mean2>Mean1, and
#   diff = Mean2-Mean1
p2.p1 <- function(p,diff)
{
  p1 <- rep(NA,length(p))
  t <- diff >=0
  p1[t] <- p[t]/2
  p1[!t] <- 1-p[!t]/2
  return(p1)
}
