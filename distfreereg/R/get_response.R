get_response <-
  function(formula){
    all.vars(update(formula(formula), . ~ 0))
  }
