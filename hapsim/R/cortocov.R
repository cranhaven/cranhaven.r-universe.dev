"cortocov" <-
function(cor.mat, sd){
 dc <- diag(cor.mat)
 ds <- diag(sd)
 Mo <- ds %*% cor.mat %*% ds
 Mo <- Mo - 0.5 * (Mo-t(Mo)) # Correct any round-off error
 Mo <- Mo - diag(diag(Mo)-sd * sd)
 return(Mo)
}

