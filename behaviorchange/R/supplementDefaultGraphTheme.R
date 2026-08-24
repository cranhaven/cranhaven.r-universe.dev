supplementDefaultGraphTheme <- function(graphTheme,
                                        defaultGraphTheme =
                                          behaviorchange::opts$get(
                                            'defaultGraphTheme'
                                          )) {

  if (is.null(graphTheme)) {
    return(defaultGraphTheme);
  }

  names(defaultGraphTheme) <-
    unlist(
      lapply(
        defaultGraphTheme,
        function(x) paste0(x[1], "_", x[3])
      )
    );

  names(graphTheme) <-
    unlist(
      lapply(
        graphTheme,
        function(x) paste0(x[1], "_", x[3])
      )
    );

  ### Complement with default settings that were not overridden
  graphTheme <-
    unname(
      c(graphTheme,
        defaultGraphTheme[
          setdiff(
            names(defaultGraphTheme),
            names(graphTheme)
          )
        ]
      )
    );

  return(graphTheme);

}