VERBFINAL <-
function(proposition){
	target=proposition[[length(proposition)]]
	proposition=proposition[-length(proposition)]
	proposition=proposition[c(names(proposition)[names(proposition)!='verb'], 'verb')]
	proposition$target=target
proposition
}
