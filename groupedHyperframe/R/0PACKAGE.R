

#' @import cli
#' @import patchwork
#' 
#' @import stats
#' @import survival
'_PACKAGE'

if (FALSE) {
  # collision between
  ?patchwork::area
  ?spatstat.geom::area
  # and we even have
  ?MASS::area
  # one bandage fix is to import 
  spatstat.geom::area.owin
}


# citation(auto = packageDescription('groupedHyperframe'))
# requires installed package from CRAN..
# ?utils::citation does *not* need to Imports the package!!


.onAttach <- function(libname, pkgname) {
  
  '\n' |>
    cli_inform(class = 'packageStartupMessage')
  
  sprintf(
    fmt = 'Welcome to R package {.href [groupedHyperframe](https://cran.r-project.org/package=groupedHyperframe)}, an %s project by {.href [%s](https://github.com/tingtingzhan/groupedHyperframe)}',
    'entertaining-but-useless' |> bg_br_yellow(),
    'Tingting Zhan' |> col_red() |> style_bold()
  ) |>
    cli_inform(class = 'packageStartupMessage')
  
  '\n' |>
    cli_inform(class = 'packageStartupMessage')
  
  sprintf(
    fmt = 'Please read the {.href [%s](https://tingtingzhan.quarto.pub/groupedhyperframe/)} for details.',
    'vignettes' |> col_blue() |> style_bold()
  ) |>
    cli_inform(class = 'packageStartupMessage')
  
  '\n' |>
    cli_inform(class = 'packageStartupMessage')
  
}



