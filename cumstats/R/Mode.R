Mode <-
function(x){    
    counts <- tabulate(match(x, unique(x)))    
    list(Values = unique(x)[counts == max(counts)], Frequency = max(counts))
}
