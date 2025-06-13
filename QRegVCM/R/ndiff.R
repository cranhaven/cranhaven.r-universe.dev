ndiff <-
function(n, d = 1){

if (d == 1)
    {D <- diff(diag(n))}
else
    {D <- diff(ndiff(n, d - 1))}
D
}
