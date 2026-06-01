# Test function for the Direct Problem
# F(x) = At^2+Bt+C
#
A<-1
B<-3
C<-50
t_min <- 0
t_max<-100
t_passo<-5
.GlobalEnv$results <- seq(from=t_min, to=t_max, by=t_passo)
t <- t_min
i <- 1
while(t<=t_max)
 {
 f <- A*t*t+B*t+C
 t <- t+t_passo
 .GlobalEnv$results[i] <- f
 i <- i+1
 }
if (isitR==FALSE)
{folder <- getwd()
 setwd(.GlobalEnv$result_folder)
 write.table(results, file = "result.dat", append = FALSE, quote = TRUE, sep = " ",eol = "\n", na = "NA", dec = ".", row.names = FALSE,col.names = FALSE, qmethod = c("escape", "double"),fileEncoding = "")
 setwd(folder)
}
