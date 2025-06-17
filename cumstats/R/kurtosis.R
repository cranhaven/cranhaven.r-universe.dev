kurtosis <-
function(x) length(x)*sum((x - mean(x))^4)/(sum((x - mean(x))^2)^2)
