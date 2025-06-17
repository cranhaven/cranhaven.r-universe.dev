skewness <-
function(x) sqrt(length(x))*sum((x - mean(x))^3)/(sum((x - mean(x))^2)^(3/2))
