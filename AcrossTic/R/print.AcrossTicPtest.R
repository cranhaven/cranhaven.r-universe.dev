print.AcrossTicPtest <- function (x, ...) 
{
cat ("Observed cross-match statistic", x$observed, "\n")
exceed.pct <- sprintf ("%2.1f", 100 * x$p.value)
cat (paste0 ("This is >= the simulated ones ", 
         exceed.pct, "% of the time (p = ", sprintf ("%0.3f", x$p.value), ")\n"))
}
