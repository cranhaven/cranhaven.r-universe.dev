copulas <- function(theta,cop){
  switch (cop,
          "frank" = frankCopula(param = theta, dim = 2),
          "gaussian" = normalCopula(param = theta, dim = 2),
          "clayton" = claytonCopula(param = theta, dim = 2),
          "joe" = joeCopula(param = theta, dim = 2),
          "gumbel" = gumbelCopula(param = theta, dim = 2),
          "amh" = amhCopula(param = theta, dim = 2)
  )
}
