# Test function for the Direct Problem
# F(x) = At^2+Bt+C
#
my_data <- readLines('input_ln.dat')
A<-as.numeric(my_data[1])
B<-as.numeric(my_data[2])
C<-as.numeric(my_data[3])
t_min <- 0
t_max<-100
t_passo<-5
result <- seq(from=t_min, to=t_max, by=t_passo)
t <- t_min
i <- 1
while(t<=t_max)
 {
 f <- A*t*t+B*t+C
 t <- t+t_passo
 result[i] <- f
 i <- i+1
 }
write.csv(result,row.names=FALSE,'result.dat')
